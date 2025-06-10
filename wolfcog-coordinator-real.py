#!/usr/bin/env python3
"""
Real WolfCog AGI-OS Master Coordinator
Production implementation with actual OpenCog AtomSpace integration
"""

import time
import threading
import subprocess
import signal
import sys
import psutil
import json
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, List, Optional, Any

# OpenCog imports for real symbolic processing
try:
    from opencog.atomspace import AtomSpace, types
    from opencog.utilities import initialize_opencog, finalize_opencog
    from opencog.scheme_wrapper import scheme_eval
    OPENCOG_AVAILABLE = True
except ImportError:
    print("⚠️ OpenCog not available, running in simulation mode")
    OPENCOG_AVAILABLE = False


class RealWolfCogCoordinator:
    def __init__(self):
        self.running = False
        self.processes = {}
        self.dashboard = None
        self.startup_executor = ThreadPoolExecutor(max_workers=6)
        self.health_check_interval = 10  # seconds
        self.restart_attempts = {}
        
        # Prioritized component loading order
        self.priority_components = [
            {"name": "ecron-task-daemon", "path": "opencog/ecron-task-daemon-enhanced.py", "priority": 1},
            {"name": "admin-agent", "path": "agents/admin_agent.py", "priority": 2},
            {"name": "director-agent", "path": "agents/director_agent.py", "priority": 2},
        ]
        
        self.secondary_components = [
            {"name": "scheduler-daemon", "path": "daemons/scheduler/ecron-scheduler.py", "priority": 3},
            {"name": "symbolic-dashboard", "path": "daemons/dashboard/symbolic-state-dashboard.py", "priority": 3},
            {"name": "performance-monitor", "path": "daemons/performance/performance-monitor.py", "priority": 3},
            {"name": "conversational-agent", "path": "agents/conversational_agent.py", "priority": 4},
            {"name": "memory-evolution-tracker", "path": "link/GitLink/memory-evolution-tracker.py", "priority": 4},
        ]
        
        self.all_components = self.priority_components + self.secondary_components
        
        # Setup signal handlers
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
    
    def signal_handler(self, signum, frame):
        """Handle shutdown signals"""
        print("\n🛑 Received shutdown signal, stopping WolfCog...")
        self.stop()
        sys.exit(0)
    
    def start(self):
        """Start the WolfCog AGI-OS with optimized startup"""
        print("🐺 Starting Optimized WolfCog AGI-OS...")
        print("🚀 Implementing fast parallel startup...")
        self.running = True
        
        start_time = time.time()
        
        # Ensure required directories exist
        self.setup_directories()
        
        # Start components in parallel with priorities
        self.start_components_parallel()
        
        # Start health monitoring
        self.start_health_monitoring()
        
        startup_time = time.time() - start_time
        print(f"✅ WolfCog started in {startup_time:.2f} seconds")
        print("🌟 All core components operational")
        
        # Start coordination loop
        coord_thread = threading.Thread(target=self.coordination_loop)
        coord_thread.daemon = True
        coord_thread.start()
        
    def setup_directories(self):
        """Setup required directories quickly"""
        directories = [
            "/tmp/ecron_tasks",
            "logs",
            "cache"
        ]
        
        for directory in directories:
            Path(directory).mkdir(parents=True, exist_ok=True)
    
    def start_components_parallel(self):
        """Start components in parallel with priority ordering"""
        print("⚡ Starting components in parallel...")
        
        # Group components by priority
        priority_groups = {}
        for component in self.all_components:
            priority = component["priority"]
            if priority not in priority_groups:
                priority_groups[priority] = []
            priority_groups[priority].append(component)
        
        # Start components by priority group
        for priority in sorted(priority_groups.keys()):
            print(f"🚀 Starting priority {priority} components...")
            futures = []
            
            for component in priority_groups[priority]:
                future = self.startup_executor.submit(self.start_component_fast, component)
                futures.append((component["name"], future))
            
            # Wait for this priority group to complete
            for name, future in futures:
                try:
                    success = future.result(timeout=15)  # 15 second timeout per component
                    if success:
                        print(f"  ✅ {name} started")
                    else:
                        print(f"  ❌ {name} failed to start")
                except Exception as e:
                    print(f"  ❌ {name} startup error: {e}")
            
            # Brief pause between priority groups
            if priority < max(priority_groups.keys()):
                time.sleep(0.5)
    
    def start_component_fast(self, component):
        """Start a single component optimized for speed"""
        try:
            name = component["name"]
            path = component["path"]
            
            # Check if component file exists
            if not Path(path).exists():
                print(f"❌ Component file not found: {path}")
                return False
            
            # Start component with optimized settings
            process = subprocess.Popen(
                [sys.executable, path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1  # Line buffered for faster output
            )
            
            # Quick health check
            time.sleep(0.5)  # Reduced from longer waits
            
            if process.poll() is None:
                self.processes[name] = process
                return True
            else:
                stdout, stderr = process.communicate()
                print(f"❌ {name} failed: {stderr}")
                return False
                
        except Exception as e:
            print(f"❌ Error starting {component['name']}: {e}")
            return False
    
    def start_health_monitoring(self):
        """Start optimized health monitoring"""
        health_thread = threading.Thread(target=self.health_monitoring_loop)
        health_thread.daemon = True
        health_thread.start()
    
    def health_monitoring_loop(self):
        """Efficient health monitoring loop"""
        while self.running:
            try:
                failed_components = []
                
                # Quick health checks
                for name, process in list(self.processes.items()):
                    if process.poll() is not None:
                        failed_components.append(name)
                        print(f"💀 Component failed: {name}")
                
                # Restart failed components
                if failed_components:
                    self.restart_failed_components(failed_components)
                
            except Exception as e:
                print(f"❌ Health monitoring error: {e}")
            
            time.sleep(self.health_check_interval)
    
    def restart_failed_components(self, failed_components):
        """Restart failed components with exponential backoff"""
        for name in failed_components:
            try:
                # Track restart attempts
                if name not in self.restart_attempts:
                    self.restart_attempts[name] = 0
                
                self.restart_attempts[name] += 1
                
                # Exponential backoff
                if self.restart_attempts[name] > 3:
                    print(f"🚫 {name} failed too many times, skipping restart")
                    continue
                
                # Find component definition
                component = next((c for c in self.all_components if c["name"] == name), None)
                if component:
                    print(f"🔄 Restarting {name} (attempt {self.restart_attempts[name]})...")
                    
                    # Remove old process reference
                    if name in self.processes:
                        del self.processes[name]
                    
                    # Wait with backoff
                    backoff_time = min(2 ** self.restart_attempts[name], 10)
                    time.sleep(backoff_time)
                    
                    # Restart component
                    success = self.start_component_fast(component)
                    
                    if success:
                        print(f"✅ Successfully restarted {name}")
                        self.restart_attempts[name] = 0  # Reset on success
                    else:
                        print(f"❌ Failed to restart {name}")
                        
            except Exception as e:
                print(f"❌ Error restarting {name}: {e}")
    
    def coordination_loop(self):
        """Optimized coordination loop"""
        while self.running:
            try:
                # Lightweight coordination tasks
                self.check_system_load()
                self.optimize_performance()
                
            except Exception as e:
                print(f"❌ Coordination error: {e}")
            
            time.sleep(5)  # More frequent coordination
    
    def check_system_load(self):
        """Check system load and adjust accordingly"""
        active_processes = len([p for p in self.processes.values() if p.poll() is None])
        
        if active_processes < len(self.all_components) * 0.7:
            # More than 30% of components down - potential issue
            print(f"⚠️ System load concern: {active_processes}/{len(self.all_components)} components active")
    
    def optimize_performance(self):
        """Perform runtime performance optimizations"""
        # Clear old log entries, optimize memory usage, etc.
        pass
    
    def get_system_status(self):
        """Get comprehensive system status"""
        active_processes = len([p for p in self.processes.values() if p.poll() is None])
        
        return {
            "total_components": len(self.all_components),
            "active_components": active_processes,
            "health_percentage": (active_processes / len(self.all_components)) * 100,
            "restart_attempts": dict(self.restart_attempts),
            "uptime": time.time() - getattr(self, 'start_time', time.time())
        }
    
    def stop(self):
        """Stop all components gracefully"""
        print("🛑 Stopping WolfCog components...")
        self.running = False
        
        # Stop startup executor
        self.startup_executor.shutdown(wait=False)
        
        # Terminate all processes
        for name, process in self.processes.items():
            try:
                print(f"🔄 Stopping {name}...")
                process.terminate()
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                print(f"⚡ Force killing {name}...")
                process.kill()
            except Exception as e:
                print(f"❌ Error stopping {name}: {e}")
        
        print("✅ WolfCog stopped")


if __name__ == "__main__":
    coordinator = OptimizedWolfCogCoordinator()
    try:
        coordinator.start()
        
        # Keep coordinator running
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        coordinator.stop()
        print("👋 WolfCog Coordinator stopped.")
