#!/usr/bin/env python3
"""
WolfCog Symbolic State Dashboard - Live monitoring interface
Provides real-time monitoring of WolfCog AGI-OS system state
"""

import time
import json
import subprocess
import threading
import os
import glob
from pathlib import Path
from datetime import datetime

class SymbolicStateDashboard:
    def __init__(self):
        self.running = False
        self.metrics = {}
        self.update_interval = 5  # seconds
        self.dashboard_file = "/tmp/wolfcog_visualizations/live-dashboard.json"
        self.ensure_directories()
        
    def ensure_directories(self):
        """Ensure required directories exist"""
        Path("/tmp/wolfcog_visualizations").mkdir(exist_ok=True)
        
    def collect_system_metrics(self):
        """Collect comprehensive system metrics"""
        timestamp = datetime.now().isoformat()
        
        # Collect space statistics
        spaces = {}
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            file_count = len(list(space_path.glob("*"))) if space_path.exists() else 0
            
            spaces[space] = {
                "files": file_count,
                "activity": "active" if file_count > 0 else "idle",
                "memory_usage": self.get_space_memory_usage(space),
                "last_modified": self.get_last_modified_time(space_path)
            }
        
        # Collect task queue statistics  
        task_dir = Path("/tmp/ecron_tasks")
        task_stats = {
            "pending": len(list(task_dir.glob("*.json"))) if task_dir.exists() else 0,
            "processed": len(list(task_dir.glob("*.processed"))) if task_dir.exists() else 0,
            "failed": len(list(task_dir.glob("*.failed"))) if task_dir.exists() else 0
        }
        
        # Collect agent statistics
        agent_stats = self.collect_agent_statistics()
        
        # Collect shell depth and recursion info
        shell_info = self.collect_shell_information()
        
        # Collect memory topology data
        memory_topology = self.collect_memory_topology()
        
        # Collect recent symbolic mutations
        recent_mutations = self.collect_recent_mutations()
        
        # System performance metrics (simplified without psutil)
        system_perf = {
            "load_average": self.get_load_average(),
            "process_count": self.get_process_count(),
            "uptime": self.get_system_uptime()
        }
        
        metrics = {
            "timestamp": timestamp,
            "spaces": spaces,
            "tasks": task_stats,
            "agents": agent_stats,
            "shell": shell_info,
            "memory_topology": memory_topology,
            "recent_mutations": recent_mutations,
            "system_performance": system_perf,
            "uptime": self.get_system_uptime()
        }
        
        self.metrics = metrics
        return metrics
        
    def get_space_memory_usage(self, space):
        """Get memory usage for a specific space"""
        space_path = Path(f"spaces/{space}")
        if not space_path.exists():
            return 0
            
        total_size = 0
        for file_path in space_path.rglob("*"):
            if file_path.is_file():
                total_size += file_path.stat().st_size
        return total_size
        
    def get_last_modified_time(self, path):
        """Get last modified time for a path"""
        if not path.exists():
            return None
            
        latest_time = 0
        for item in path.rglob("*"):
            if item.is_file():
                latest_time = max(latest_time, item.stat().st_mtime)
                
        return datetime.fromtimestamp(latest_time).isoformat() if latest_time > 0 else None
        
    def get_load_average(self):
        """Get system load average (Linux only)"""
        try:
            with open('/proc/loadavg', 'r') as f:
                return f.read().split()[0]
        except:
            return "0.0"
            
    def get_process_count(self):
        """Get number of running processes"""
        try:
            return len(os.listdir('/proc')) - len([x for x in os.listdir('/proc') if not x.isdigit()])
        except:
            return 0
            
    def collect_agent_statistics(self):
        """Collect statistics about running agents"""
        agents = {}
        
        # Check for running processes using simple ps command
        agent_processes = ["admin_agent.py", "director_agent.py", "ecron-scheduler.py", "reflex-monitor.py", "symbolic-state-dashboard.py"]
        
        for agent_script in agent_processes:
            agent_name = agent_script.replace(".py", "").replace("-", "_")
            agents[agent_name] = {
                "status": "running" if self.is_process_running(agent_script) else "stopped",
                "pid": self.get_process_pid(agent_script)
            }
            
        return agents
        
    def is_process_running(self, script_name):
        """Check if a process is running using ps command"""
        try:
            result = subprocess.run(['ps', 'aux'], capture_output=True, text=True)
            return script_name in result.stdout
        except:
            return False
        
    def get_process_pid(self, script_name):
        """Get PID of a process using ps command"""
        try:
            result = subprocess.run(['ps', 'aux'], capture_output=True, text=True)
            for line in result.stdout.split('\n'):
                if script_name in line:
                    return line.split()[1]
        except:
            pass
        return None
        
    def collect_shell_information(self):
        """Collect shell depth and recursion information"""
        # In a real implementation, this would interface with meta-shellwalker
        # For now, we'll simulate or read from a state file
        shell_state_file = Path("/tmp/wolfcog_shell_state.json")
        
        if shell_state_file.exists():
            try:
                with open(shell_state_file, 'r') as f:
                    shell_data = json.load(f)
                return shell_data
            except json.JSONDecodeError:
                pass
                
        # Default shell information
        return {
            "depth": 0,
            "recursions": 0,
            "current_space": "u",
            "context_stack": [],
            "active_shells": 1
        }
        
    def collect_memory_topology(self):
        """Collect memory topology information"""
        # Analyze symbolic memory structures
        topology = {
            "nodes": 0,
            "connections": 0,
            "complexity": 0.0,
            "evolution_steps": 0
        }
        
        # Count files across all spaces as nodes
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            if space_path.exists():
                topology["nodes"] += len(list(space_path.glob("*")))
                
        # Simulate topology metrics
        topology["connections"] = topology["nodes"] * 2
        topology["complexity"] = min(1.0, topology["nodes"] / 10.0)
        topology["evolution_steps"] = topology["nodes"] // 3
        
        return topology
        
    def collect_recent_mutations(self):
        """Collect recent symbolic mutations"""
        mutations = []
        
        # Check for recent file changes in spaces
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            if space_path.exists():
                for file_path in space_path.rglob("*"):
                    if file_path.is_file():
                        mod_time = datetime.fromtimestamp(file_path.stat().st_mtime)
                        if (datetime.now() - mod_time).total_seconds() < 300:  # Last 5 minutes
                            mutations.append({
                                "type": "file_mutation",
                                "location": str(file_path),
                                "timestamp": mod_time.isoformat(),
                                "space": space
                            })
                            
        # Keep only the 10 most recent mutations
        mutations.sort(key=lambda x: x["timestamp"], reverse=True)
        return mutations[:10]
        
    def get_system_uptime(self):
        """Get system uptime in seconds"""
        try:
            with open('/proc/uptime', 'r') as f:
                return float(f.read().split()[0])
        except:
            return 0
            
    def save_metrics(self):
        """Save current metrics to file"""
        with open(self.dashboard_file, 'w') as f:
            json.dump(self.metrics, f, indent=2)
            
    def print_dashboard_summary(self):
        """Print a text summary of the dashboard"""
        if not self.metrics:
            return
            
        print("\n" + "="*60)
        print("🐺 WolfCog Symbolic State Dashboard")
        print("="*60)
        print(f"⏰ Last Update: {self.metrics['timestamp']}")
        print(f"🔄 System Uptime: {self.metrics['uptime']:.0f} seconds")
        print()
        
        # Spaces summary
        print("📁 Symbolic Spaces:")
        for space, data in self.metrics['spaces'].items():
            status_emoji = "🟢" if data['activity'] == 'active' else "🔴"
            print(f"  {status_emoji} /{space}/ - {data['files']} files, {data['memory_usage']} bytes")
        print()
        
        # Tasks summary
        tasks = self.metrics['tasks']
        print("📋 Task Queue:")
        print(f"  ⏳ Pending: {tasks['pending']}")
        print(f"  ✅ Processed: {tasks['processed']}")
        print(f"  ❌ Failed: {tasks['failed']}")
        print()
        
        # Agents summary
        print("🤖 Agents:")
        for agent, data in self.metrics['agents'].items():
            status_emoji = "🟢" if data['status'] == 'running' else "🔴"
            print(f"  {status_emoji} {agent}: {data['status']} (PID: {data['pid']})")
        print()
        
        # Shell information
        shell = self.metrics['shell']
        print("🚶 Shell State:")
        print(f"  📏 Depth: {shell['depth']}")
        print(f"  🔄 Recursions: {shell['recursions']}")
        print(f"  📍 Current Space: {shell['current_space']}")
        print()
        
        # Memory topology
        topology = self.metrics['memory_topology']
        print("🗺️ Memory Topology:")
        print(f"  🔵 Nodes: {topology['nodes']}")
        print(f"  🔗 Connections: {topology['connections']}")
        print(f"  📊 Complexity: {topology['complexity']:.2f}")
        print()
        
        # Recent mutations
        mutations = self.metrics['recent_mutations']
        print(f"🧬 Recent Mutations ({len(mutations)}):")
        for mutation in mutations[:3]:  # Show only first 3
            print(f"  🔹 {mutation['type']} in {mutation['space']} at {mutation['timestamp']}")
        print()
        
        print("="*60)
        
    def monitoring_loop(self):
        """Main monitoring loop"""
        print("🔄 Starting symbolic state monitoring...")
        
        while self.running:
            try:
                # Collect metrics
                self.collect_system_metrics()
                
                # Save metrics
                self.save_metrics()
                
                # Print summary
                self.print_dashboard_summary()
                
                # Wait for next update
                time.sleep(self.update_interval)
                
            except KeyboardInterrupt:
                print("\n🛑 Dashboard monitoring stopped by user")
                break
            except Exception as e:
                print(f"❌ Error in monitoring loop: {e}")
                time.sleep(self.update_interval)
                
    def start(self):
        """Start the dashboard monitoring"""
        print("📊 Starting WolfCog Symbolic State Dashboard...")
        self.running = True
        
        # Start monitoring in a separate thread
        monitor_thread = threading.Thread(target=self.monitoring_loop)
        monitor_thread.daemon = True
        monitor_thread.start()
        
        print("✨ Dashboard monitoring active!")
        return monitor_thread
        
    def stop(self):
        """Stop the dashboard monitoring"""
        print("🛑 Stopping dashboard monitoring...")
        self.running = False
        
    def get_metrics(self):
        """Get current metrics"""
        return self.metrics

def main():
    """Main function for standalone execution"""
    dashboard = SymbolicStateDashboard()
    
    try:
        # Start monitoring
        monitor_thread = dashboard.start()
        
        # Keep running until interrupted
        while dashboard.running:
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\n🛑 Shutting down dashboard...")
        dashboard.stop()
        
if __name__ == "__main__":
    main()