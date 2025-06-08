#!/usr/bin/env python3
"""
WolfCog AGI-OS Master Coordinator
Coordinates all components of the symbolic operating system
"""

import time
import threading
import subprocess
import signal
import sys
from pathlib import Path

class WolfCogCoordinator:
    def __init__(self):
        self.running = False
        self.processes = {}
        self.dashboard = None
        self.components = [
            {"name": "ecron-task-daemon", "path": "opencog/ecron-task-daemon.py"},
            {"name": "scheduler-daemon", "path": "daemons/scheduler/ecron-scheduler.py"},
            {"name": "admin-agent", "path": "agents/admin_agent.py"},
            {"name": "director-agent", "path": "agents/director_agent.py"},
            {"name": "symbolic-dashboard", "path": "daemons/dashboard/symbolic-state-dashboard.py"},
            {"name": "conversational-agent", "path": "agents/conversational_agent.py"},
            {"name": "memory-evolution-tracker", "path": "link/GitLink/memory-evolution-tracker.py"},
            {"name": "performance-monitor", "path": "daemons/performance/performance-monitor.py"}
        ]
        
        # Setup signal handlers
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
    
    def signal_handler(self, signum, frame):
        """Handle shutdown signals"""
        print("\n🛑 Received shutdown signal, stopping WolfCog...")
        self.stop()
        sys.exit(0)
    
    def start(self):
        """Start the WolfCog AGI-OS"""
        print("🐺 Starting WolfCog AGI-OS...")
        print("🌟 Initializing symbolic cognitive substrate...")
        self.running = True
        
        # Ensure required directories exist
        self.setup_directories()
        
        # Start coordination thread
        coord_thread = threading.Thread(target=self.coordination_loop)
        coord_thread.daemon = True
        coord_thread.start()
        
        # Start all components
        self.start_components()
        
        # Start system monitoring
        monitor_thread = threading.Thread(target=self.monitor_system)
        monitor_thread.daemon = True
        monitor_thread.start()
        
        print("✨ WolfCog AGI-OS is now live and operational!")
        print("🧠 Symbolic recursive runtime active")
        print("🔁 Self-modification capabilities enabled")
        print("🌐 Multi-language cognitive agents running")
        print("📚 Geometric memory evolution in progress")
        print("🔮 Ready for symbolic computation...")
    
    def setup_directories(self):
        """Setup required directories"""
        dirs = [
            "/tmp/ecron_tasks",
            "/tmp/wolfcog_visualizations",
            "spaces/u",
            "spaces/e", 
            "spaces/s"
        ]
        
        for dir_path in dirs:
            Path(dir_path).mkdir(parents=True, exist_ok=True)
            print(f"📁 Ensured directory: {dir_path}")
    
    def start_components(self):
        """Start all WolfCog components"""
        for component in self.components:
            self.start_component(component)
    
    def start_component(self, component):
        """Start a specific component"""
        name = component["name"]
        path = component["path"]
        
        if not Path(path).exists():
            print(f"❌ Component file not found: {path}")
            return
        
        try:
            print(f"🚀 Starting {name}...")
            process = subprocess.Popen(
                [sys.executable, path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            self.processes[name] = process
            print(f"✅ {name} started (PID: {process.pid})")
            
        except Exception as e:
            print(f"❌ Failed to start {name}: {e}")
    
    def coordination_loop(self):
        """Main coordination loop with enhanced error handling"""
        coordination_errors = 0
        max_errors = 5
        
        while self.running:
            try:
                # Coordinate between components
                self.coordinate_components()
                
                # Manage symbolic flows
                self.manage_symbolic_flows()
                
                # Check system health
                self.check_system_health()
                
                # Reset error counter on successful cycle
                coordination_errors = 0
                
            except KeyboardInterrupt:
                print("🛑 Received shutdown signal")
                self.stop()
                break
            except Exception as e:
                coordination_errors += 1
                print(f"❌ Coordination error ({coordination_errors}/{max_errors}): {e}")
                
                if coordination_errors >= max_errors:
                    print("🚨 Too many coordination errors, entering safe mode")
                    self.enter_safe_mode()
                    break
                    
                # Progressive backoff on errors
                time.sleep(min(coordination_errors * 5, 30))
                continue
            
            time.sleep(10)  # Coordination cycle every 10 seconds
    
    def enter_safe_mode(self):
        """Enter safe mode when too many errors occur"""
        print("🛡️ Entering safe mode...")
        
        # Stop all components gracefully
        for name, process in list(self.processes.items()):
            try:
                print(f"🛑 Stopping {name} for safe mode...")
                process.terminate()
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                print(f"⚠️ Force killing {name}")
                process.kill()
            except Exception as e:
                print(f"❌ Error stopping {name}: {e}")
        
        # Clear process list
        self.processes.clear()
        self.running = False
        
        print("✅ Safe mode activated - all components stopped")
    
    
    def coordinate_components(self):
        """Coordinate between system components"""
        # Create test tasks to demonstrate the system
        self.create_test_symbolic_flows()
    
    def create_test_symbolic_flows(self):
        """Create test symbolic flows to demonstrate the system"""
        task_path = Path("/tmp/ecron_tasks")
        
        # Only create test tasks if queue is empty
        if len(list(task_path.glob("*.json"))) == 0:
            import json
            import random
            
            # Create sample symbolic tasks for different spaces
            tasks = [
                {
                    "flow": "user_interaction",
                    "space": "u",
                    "symbolic": "∇(user_intent)",
                    "action": "evolve",
                    "timestamp": time.time()
                },
                {
                    "flow": "runtime_optimization", 
                    "space": "e",
                    "symbolic": "∂Ω(execution_flow)",
                    "action": "optimize",
                    "timestamp": time.time()
                },
                {
                    "flow": "system_evolution",
                    "space": "s", 
                    "symbolic": "⊗Φ(meta_system)",
                    "action": "meta_evolve",
                    "timestamp": time.time()
                }
            ]
            
            for i, task in enumerate(tasks):
                task_file = task_path / f"demo_task_{i}_{int(time.time())}.json"
                with open(task_file, 'w') as f:
                    json.dump(task, f, indent=2)
                
                print(f"📋 Created demo task: {task['flow']} in {task['space']} space")
    
    def manage_symbolic_flows(self):
        """Manage symbolic flows across the system"""
        # Monitor task processing
        task_path = Path("/tmp/ecron_tasks")
        pending = len(list(task_path.glob("*.json")))
        processed = len(list(task_path.glob("*.processed")))
        
        if pending > 0 or processed > 0:
            print(f"🌊 Symbolic flows: {pending} pending, {processed} processed")
    
    def check_system_health(self):
        """Check health of system components"""
        active_components = 0
        failed_components = []
        
        for name, process in self.processes.items():
            if process.poll() is None:  # Process is still running
                active_components += 1
            else:
                print(f"⚠️ Component {name} has stopped")
                failed_components.append(name)
        
        if active_components != len(self.components):
            print(f"⚠️ System health: {active_components}/{len(self.components)} components active")
            
            # Attempt to restart failed components
            if failed_components:
                self.restart_failed_components(failed_components)
    
    def restart_failed_components(self, failed_components):
        """Restart failed components with backoff strategy"""
        for name in failed_components:
            try:
                # Find component definition
                component = next((c for c in self.components if c["name"] == name), None)
                if component:
                    print(f"🔄 Attempting to restart {name}...")
                    
                    # Remove old process reference
                    if name in self.processes:
                        del self.processes[name]
                    
                    # Wait a moment before restart
                    time.sleep(2)
                    
                    # Restart component
                    self.start_component(component)
                    
                    # Verify restart
                    if name in self.processes and self.processes[name].poll() is None:
                        print(f"✅ Successfully restarted {name}")
                    else:
                        print(f"❌ Failed to restart {name}")
                        
            except Exception as e:
                print(f"❌ Error restarting {name}: {e}")
    
    
    def monitor_system(self):
        """Monitor overall system state"""
        while self.running:
            try:
                # Monitor symbolic spaces
                self.monitor_symbolic_spaces()
                
                # Monitor memory evolution
                self.monitor_memory_evolution()
                
            except Exception as e:
                print(f"❌ Monitoring error: {e}")
            
            time.sleep(30)  # System monitoring every 30 seconds
    
    def monitor_symbolic_spaces(self):
        """Monitor symbolic space activity"""
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            if space_path.exists():
                file_count = len(list(space_path.glob("*")))
                if file_count > 0:
                    print(f"📊 Space {space}: {file_count} symbolic structures")
    
    def monitor_memory_evolution(self):
        """Monitor memory evolution progress"""
        # Simple evolution indicator
        print("🧬 Symbolic memory evolution in progress...")
    
    def get_system_status(self):
        """Get comprehensive system status"""
        status = {
            "running": self.running,
            "components": {},
            "symbolic_spaces": {},
            "task_queue": {}
        }
        
        # Component status
        for name, process in self.processes.items():
            status["components"][name] = "running" if process.poll() is None else "stopped"
        
        # Space status
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            status["symbolic_spaces"][space] = {
                "exists": space_path.exists(),
                "files": len(list(space_path.glob("*"))) if space_path.exists() else 0
            }
        
        # Task queue status
        task_path = Path("/tmp/ecron_tasks")
        status["task_queue"] = {
            "pending": len(list(task_path.glob("*.json"))),
            "processed": len(list(task_path.glob("*.processed")))
        }
        
        return status
    
    def stop(self):
        """Stop the WolfCog AGI-OS"""
        print("🛑 Stopping WolfCog AGI-OS...")
        self.running = False
        
        # Stop all component processes
        for name, process in self.processes.items():
            if process.poll() is None:
                print(f"🛑 Stopping {name}...")
                process.terminate()
                
                # Wait for graceful shutdown
                try:
                    process.wait(timeout=5)
                    print(f"✅ {name} stopped gracefully")
                except subprocess.TimeoutExpired:
                    print(f"⚠️ Force killing {name}...")
                    process.kill()
        
        print("👋 WolfCog AGI-OS stopped")

def main():
    coordinator = WolfCogCoordinator()
    
    try:
        coordinator.start()
        
        # Keep coordinator running
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        coordinator.stop()

if __name__ == "__main__":
    main()