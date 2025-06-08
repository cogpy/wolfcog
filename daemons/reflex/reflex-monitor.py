#!/usr/bin/env python3
"""
Reflex Daemon - Reactive monitoring for WolfCog AGI-OS
Monitors shells and self-modifying symbols for reactive responses
"""

import time
import threading
import json
import hashlib
from pathlib import Path
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

class ReflexEventHandler(FileSystemEventHandler):
    def __init__(self, reflex_daemon):
        self.reflex_daemon = reflex_daemon
    
    def on_modified(self, event):
        if not event.is_directory:
            self.reflex_daemon.handle_file_change(event.src_path)
    
    def on_created(self, event):
        if not event.is_directory:
            self.reflex_daemon.handle_file_creation(event.src_path)
    
    def on_deleted(self, event):
        if not event.is_directory:
            self.reflex_daemon.handle_file_deletion(event.src_path)

class ReflexDaemon:
    def __init__(self):
        self.running = False
        self.observers = []
        self.watch_paths = [
            "spaces/",
            "kernels/",
            "/tmp/ecron_tasks",
            "agents/"
        ]
        self.reactions = []
        self.file_states = {}
        self.response_queue = []
        
    def start(self):
        """Start the reflex daemon"""
        print("⚡ Starting Reflex Daemon...")
        self.running = True
        
        # Start file system monitoring
        self.start_monitoring()
        
        # Start reactive response thread
        response_thread = threading.Thread(target=self.process_responses)
        response_thread.daemon = True
        response_thread.start()
        
        # Start symbolic monitoring thread
        symbolic_thread = threading.Thread(target=self.monitor_symbolic_changes)
        symbolic_thread.daemon = True
        symbolic_thread.start()
        
        print("👁️ Reflex Daemon monitoring for reactive responses...")
    
    def start_monitoring(self):
        """Start file system monitoring"""
        event_handler = ReflexEventHandler(self)
        
        for watch_path in self.watch_paths:
            path = Path(watch_path)
            if path.exists():
                observer = Observer()
                observer.schedule(event_handler, str(path), recursive=True)
                observer.start()
                self.observers.append(observer)
                print(f"👀 Monitoring: {watch_path}")
    
    def handle_file_change(self, file_path):
        """Handle file modification events"""
        print(f"📝 File modified: {file_path}")
        
        # Calculate file hash to detect actual changes
        current_hash = self.calculate_file_hash(file_path)
        previous_hash = self.file_states.get(file_path)
        
        if current_hash != previous_hash:
            self.file_states[file_path] = current_hash
            
            reaction = {
                "type": "file_modified",
                "path": file_path,
                "timestamp": time.time(),
                "hash": current_hash,
                "previous_hash": previous_hash
            }
            
            self.trigger_reaction(reaction)
    
    def handle_file_creation(self, file_path):
        """Handle file creation events"""
        print(f"✨ File created: {file_path}")
        
        reaction = {
            "type": "file_created",
            "path": file_path,
            "timestamp": time.time(),
            "hash": self.calculate_file_hash(file_path)
        }
        
        self.trigger_reaction(reaction)
    
    def handle_file_deletion(self, file_path):
        """Handle file deletion events"""
        print(f"🗑️ File deleted: {file_path}")
        
        reaction = {
            "type": "file_deleted",
            "path": file_path,
            "timestamp": time.time(),
            "previous_hash": self.file_states.get(file_path)
        }
        
        # Remove from tracked states
        if file_path in self.file_states:
            del self.file_states[file_path]
        
        self.trigger_reaction(reaction)
    
    def calculate_file_hash(self, file_path):
        """Calculate hash of file contents"""
        try:
            with open(file_path, 'rb') as f:
                return hashlib.md5(f.read()).hexdigest()
        except Exception:
            return None
    
    def trigger_reaction(self, reaction):
        """Trigger a reactive response"""
        self.reactions.append(reaction)
        
        # Determine response based on reaction type and context
        response = self.determine_response(reaction)
        if response:
            self.response_queue.append(response)
            print(f"⚡ Triggered reaction: {reaction['type']} -> {response['action']}")
    
    def determine_response(self, reaction):
        """Determine appropriate response to a reaction"""
        file_path = reaction["path"]
        reaction_type = reaction["type"]
        
        # Response rules based on file type and location
        if "kernels/" in file_path:
            return self.handle_kernel_change(reaction)
        elif "spaces/" in file_path:
            return self.handle_space_change(reaction)
        elif "ecron_tasks" in file_path:
            return self.handle_task_change(reaction)
        elif "agents/" in file_path:
            return self.handle_agent_change(reaction)
        
        return None
    
    def handle_kernel_change(self, reaction):
        """Handle changes to kernel files"""
        if reaction["type"] == "file_modified":
            return {
                "action": "reload_kernel",
                "target": reaction["path"],
                "priority": 1,
                "timestamp": time.time()
            }
        return None
    
    def handle_space_change(self, reaction):
        """Handle changes to symbolic spaces"""
        if reaction["type"] == "file_created":
            return {
                "action": "index_memory",
                "target": reaction["path"],
                "priority": 2,
                "timestamp": time.time()
            }
        elif reaction["type"] == "file_modified":
            return {
                "action": "update_memory",
                "target": reaction["path"],
                "priority": 2,
                "timestamp": time.time()
            }
        return None
    
    def handle_task_change(self, reaction):
        """Handle changes to task files"""
        if reaction["type"] == "file_created":
            return {
                "action": "notify_scheduler",
                "target": reaction["path"],
                "priority": 1,
                "timestamp": time.time()
            }
        return None
    
    def handle_agent_change(self, reaction):
        """Handle changes to agent files"""
        if reaction["type"] == "file_modified":
            return {
                "action": "restart_agent",
                "target": reaction["path"],
                "priority": 2,
                "timestamp": time.time()
            }
        return None
    
    def process_responses(self):
        """Process reactive responses"""
        while self.running:
            try:
                if self.response_queue:
                    # Sort by priority (lower number = higher priority)
                    self.response_queue.sort(key=lambda x: x["priority"])
                    response = self.response_queue.pop(0)
                    
                    self.execute_response(response)
                
            except Exception as e:
                print(f"❌ Response processing error: {e}")
            
            time.sleep(1)  # Process responses every second
    
    def execute_response(self, response):
        """Execute a reactive response"""
        action = response["action"]
        target = response["target"]
        
        print(f"🎯 Executing reflex response: {action} on {target}")
        
        if action == "reload_kernel":
            self.reload_kernel(target)
        elif action == "index_memory":
            self.index_memory(target)
        elif action == "update_memory":
            self.update_memory(target)
        elif action == "notify_scheduler":
            self.notify_scheduler(target)
        elif action == "restart_agent":
            self.restart_agent(target)
    
    def reload_kernel(self, kernel_path):
        """Reload a modified kernel"""
        print(f"🔄 Reloading kernel: {kernel_path}")
        # Placeholder for kernel reload logic
    
    def index_memory(self, memory_path):
        """Index new memory structure"""
        print(f"📚 Indexing memory: {memory_path}")
        # Placeholder for memory indexing
    
    def update_memory(self, memory_path):
        """Update existing memory structure"""
        print(f"🔄 Updating memory: {memory_path}")
        # Placeholder for memory update
    
    def notify_scheduler(self, task_path):
        """Notify scheduler of new task"""
        print(f"📨 Notifying scheduler: {task_path}")
        # Placeholder for scheduler notification
    
    def restart_agent(self, agent_path):
        """Restart modified agent"""
        print(f"🔄 Restarting agent: {agent_path}")
        # Placeholder for agent restart
    
    def monitor_symbolic_changes(self):
        """Monitor symbolic changes in the system"""
        while self.running:
            try:
                # Monitor for symbolic self-modifications
                self.check_symbolic_mutations()
                
                # Monitor for recursive changes
                self.check_recursive_modifications()
                
            except Exception as e:
                print(f"❌ Symbolic monitoring error: {e}")
            
            time.sleep(5)  # Symbolic monitoring every 5 seconds
    
    def check_symbolic_mutations(self):
        """Check for symbolic mutations in the system"""
        # Placeholder for symbolic mutation detection
        pass
    
    def check_recursive_modifications(self):
        """Check for recursive self-modifications"""
        # Placeholder for recursive modification detection
        pass
    
    def get_status(self):
        """Get reflex daemon status"""
        return {
            "running": self.running,
            "monitored_paths": len(self.watch_paths),
            "tracked_files": len(self.file_states),
            "pending_responses": len(self.response_queue),
            "total_reactions": len(self.reactions)
        }
    
    def stop(self):
        """Stop the reflex daemon"""
        print("🛑 Stopping Reflex Daemon...")
        self.running = False
        
        # Stop all observers
        for observer in self.observers:
            observer.stop()
            observer.join()

if __name__ == "__main__":
    # Install watchdog if not available
    try:
        from watchdog.observers import Observer
        from watchdog.events import FileSystemEventHandler
    except ImportError:
        print("❌ watchdog package required. Install with: pip install watchdog")
        exit(1)
    
    daemon = ReflexDaemon()
    try:
        daemon.start()
        # Keep daemon running
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        daemon.stop()
        print("👋 Reflex Daemon stopped.")