#!/usr/bin/env python3
"""
Enhanced Ecron Task Daemon - AGI-OS Task Dispatcher with Parallel Processing

Listens to /kernels/ecron/ for symbolic task specifications
Parses them and sends commands to CogServer via telnet or IPC socket
Supports parallel task processing for improved performance
"""

import os
import time
import json
import socket
import threading
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import queue


class EcronTaskDaemon:
    def __init__(self, ecron_path="/tmp/ecron_tasks", cog_host="localhost", 
                 cog_port=17001, max_workers=4):
        self.ecron_path = Path(ecron_path)
        self.cog_host = cog_host
        self.cog_port = cog_port
        self.running = False
        self.feedback_queue = []
        self.processed_tasks = []
        self.max_workers = max_workers
        self.task_queue = queue.Queue()
        self.executor = None
        self.stats = {
            "tasks_processed": 0,
            "tasks_failed": 0,
            "start_time": time.time()
        }
        
        # Ensure ecron task directory exists
        self.ecron_path.mkdir(parents=True, exist_ok=True)
        
    def start(self):
        """Start the task daemon with parallel processing"""
        print("🚀 Starting Enhanced Ecron Task Daemon...")
        print(f"⚡ Parallel processing with {self.max_workers} workers")
        self.running = True
        
        # Initialize thread pool executor
        self.executor = ThreadPoolExecutor(max_workers=self.max_workers)
        
        # Start file watcher thread
        watcher_thread = threading.Thread(target=self.watch_ecron_files)
        watcher_thread.daemon = True
        watcher_thread.start()
        
        print(f"📁 Watching {self.ecron_path} for task specifications")
        
    def watch_ecron_files(self):
        """Watch for new ecron task files and queue them for processing"""
        while self.running:
            try:
                if self.ecron_path.exists():
                    task_files = list(self.ecron_path.glob("*.json"))
                    
                    # Process multiple files in parallel
                    if task_files:
                        # Submit tasks to thread pool
                        futures = []
                        for task_file in task_files:
                            future = self.executor.submit(self.process_task_file, task_file)
                            futures.append(future)
                        
                        # Wait for completion with timeout
                        for future in futures:
                            try:
                                future.result(timeout=30)  # 30 second timeout per task
                            except Exception as e:
                                print(f"❌ Task processing timeout or error: {e}")
                                
            except Exception as e:
                print(f"❌ Error watching files: {e}")
            time.sleep(0.5)  # Faster polling for better responsiveness
            
    def process_task_file(self, task_file):
        """Process a symbolic task specification file"""
        try:
            print(f"📋 Processing task file: {task_file}")
            
            # Read and validate task
            with open(task_file, 'r') as f:
                task_spec = json.load(f)
            
            # Validate task specification
            if not self.validate_task_spec(task_spec):
                print(f"❌ Invalid task specification in {task_file}")
                self.archive_invalid_task(task_file, "Invalid task specification")
                self.stats["tasks_failed"] += 1
                return
            
            # Extract task information
            flow = task_spec.get('flow', 'unknown')
            space = task_spec.get('space', 'e')
            symbolic_expr = task_spec.get('symbolic', '')
            action = task_spec.get('action', 'evaluate')
            
            print(f"🎯 Processing {flow} in {space} space")
            print(f"🔮 Symbolic expression: {symbolic_expr}")
            
            # Process based on space
            result = self.process_by_space(flow, space)
            
            # Send to CogServer
            self.send_to_cogserver(symbolic_expr, f"{action}_in_{space}")
            
            # Store processed task and create feedback
            task_result = {
                'task': task_spec,
                'result': result,
                'timestamp': time.time(),
                'space': space,
                'processing_time': time.time()
            }
            
            self.processed_tasks.append(task_result)
            
            # Generate feedback
            self.generate_feedback(task_spec, result)
            
            # Archive processed file
            task_file.rename(task_file.with_suffix('.processed'))
            
            self.stats["tasks_processed"] += 1
            
            # Print performance stats periodically
            if self.stats["tasks_processed"] % 10 == 0:
                self.print_performance_stats()
            
        except Exception as e:
            print(f"❌ Error processing {task_file}: {e}")
            self.archive_invalid_task(task_file, f"Processing error: {e}")
            self.stats["tasks_failed"] += 1
    
    def print_performance_stats(self):
        """Print current performance statistics"""
        runtime = time.time() - self.stats["start_time"]
        processed = self.stats["tasks_processed"]
        failed = self.stats["tasks_failed"]
        rate = processed / runtime if runtime > 0 else 0
        
        print(f"📊 Performance: {processed} processed, {failed} failed, "
              f"{rate:.2f} tasks/sec")
    
    def validate_task_spec(self, task_spec):
        """Validate task specification format and content"""
        required_fields = ['flow', 'space', 'action']
        
        # Check required fields
        for field in required_fields:
            if field not in task_spec:
                print(f"❌ Missing required field: {field}")
                return False
        
        # Validate space
        valid_spaces = ['u', 'e', 's']
        if task_spec['space'] not in valid_spaces:
            print(f"❌ Invalid space: {task_spec['space']}. "
                  f"Must be one of {valid_spaces}")
            return False
        
        # Validate flow name
        flow = task_spec['flow']
        if not isinstance(flow, str) or len(flow) == 0:
            print(f"❌ Invalid flow name: {flow}")
            return False
        
        # Validate action
        valid_actions = ['evaluate', 'evolve', 'optimize', 'test', 'meta_evolve', 'process']
        if task_spec['action'] not in valid_actions:
            print(f"❌ Invalid action: {task_spec['action']}. "
                  f"Must be one of {valid_actions}")
            return False
        
        # Validate symbolic expression if present
        if 'symbolic' in task_spec:
            symbolic = task_spec['symbolic']
            if not isinstance(symbolic, str):
                print(f"❌ Invalid symbolic expression type: {type(symbolic)}")
                return False
            
            # Check for basic syntax errors in symbolic expressions
            if not self.validate_symbolic_syntax(symbolic):
                print(f"❌ Invalid symbolic syntax: {symbolic}")
                return False
        else:
            # For most actions, symbolic expression should be present
            if task_spec['action'] in ['evaluate', 'process']:
                print(f"❌ Missing symbolic expression for action: {task_spec['action']}")
                return False
        
        return True
    
    def validate_symbolic_syntax(self, symbolic):
        """Basic validation of symbolic expression syntax"""
        if not symbolic or len(symbolic.strip()) == 0:
            return False
        
        # Check for unmatched brackets
        stack = []
        bracket_pairs = {'(': ')', '[': ']', '{': '}'}
        
        for char in symbolic:
            if char in bracket_pairs:
                stack.append(char)
            elif char in bracket_pairs.values():
                if not stack:
                    return False
                expected_open = None
                for open_bracket, close_bracket in bracket_pairs.items():
                    if close_bracket == char:
                        expected_open = open_bracket
                        break
                if stack.pop() != expected_open:
                    return False
        
        # Should have no unmatched opening brackets
        return len(stack) == 0
    
    def archive_invalid_task(self, task_file, reason):
        """Archive invalid task with error information"""
        try:
            error_info = {
                "original_file": str(task_file),
                "error_reason": reason,
                "timestamp": time.time()
            }
            
            # Save error info
            error_file = task_file.with_suffix('.error')
            with open(error_file, 'w') as f:
                json.dump(error_info, f, indent=2)
            
            # Remove original file
            task_file.unlink()
            print(f"📁 Archived invalid task to {error_file}")
            
        except Exception as e:
            print(f"❌ Error archiving invalid task: {e}")
    
    def process_by_space(self, flow, space):
        """Process task according to symbolic space"""
        if space == "u":
            return self.process_user_space(flow)
        elif space == "e":
            return self.process_execution_space(flow)
        elif space == "s":
            return self.process_system_space(flow)
        else:
            return self.process_default(flow)
    
    def process_user_space(self, flow):
        """Process user space symbolic flow"""
        print("👤 User space processing")
        return {"space": "user", "processed": flow, "interactive": True}
    
    def process_execution_space(self, flow):
        """Process execution space symbolic flow"""
        print("⚡ Execution space processing")
        return {"space": "execution", "processed": flow, "runtime": True}
    
    def process_system_space(self, flow):
        """Process system space symbolic flow"""
        print("🔧 System space processing")
        return {"space": "system", "processed": flow, "meta": True}
    
    def process_default(self, flow):
        """Default processing"""
        print("🔄 Default processing")
        return {"space": "default", "processed": flow}
    
    def generate_feedback(self, task_data, result):
        """Generate feedback for the symbolic memory system"""
        feedback = {
            "type": "task_completion",
            "original_task": task_data,
            "result": result,
            "timestamp": time.time(),
            "feedback_id": len(self.feedback_queue)
        }
        
        self.feedback_queue.append(feedback)
        print(f"🔄 Generated feedback: {feedback['feedback_id']}")
        
        # Limit feedback queue size
        if len(self.feedback_queue) > 100:
            self.feedback_queue = self.feedback_queue[-50:]
            
    def send_to_cogserver(self, expr, action):
        """Send command to CogServer via telnet/socket"""
        try:
            print(f"📡 Sending to CogServer: {action}({expr})")
            
            # TODO: Implement actual CogServer connection
            # For now, simulate the connection with improved response
            response_time = 0.1  # Simulate faster response
            time.sleep(response_time)
            print(f"🧠 CogServer response: processed {action}")
            
        except Exception as e:
            print(f"❌ CogServer connection error: {e}")
    
    def get_stats(self):
        """Get current processing statistics"""
        runtime = time.time() - self.stats["start_time"]
        return {
            **self.stats,
            "runtime": runtime,
            "rate": self.stats["tasks_processed"] / runtime if runtime > 0 else 0
        }
            
    def stop(self):
        """Stop the daemon"""
        print("🛑 Stopping Enhanced Ecron Task Daemon...")
        self.running = False
        
        if self.executor:
            print("⏳ Waiting for tasks to complete...")
            self.executor.shutdown(wait=True, timeout=10)
            
        # Print final stats
        self.print_performance_stats()


if __name__ == "__main__":
    daemon = EcronTaskDaemon()
    try:
        daemon.start()
        # Keep daemon running
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        daemon.stop()
        print("👋 Enhanced Ecron Task Daemon stopped.")
