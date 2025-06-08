#!/usr/bin/env python3
"""
Performance Monitor - Monitor WolfCog system performance and resource usage
"""

import time
import json
import threading
from pathlib import Path
from datetime import datetime

class PerformanceMonitor:
    def __init__(self, report_file="performance_metrics.json"):
        self.report_file = report_file
        self.running = False
        self.metrics = {}
        self.start_time = time.time()
        
    def start(self):
        """Start performance monitoring"""
        print("📊 Starting Performance Monitor...")
        self.running = True
        
        # Start monitoring thread
        monitor_thread = threading.Thread(target=self.monitor_loop)
        monitor_thread.daemon = True
        monitor_thread.start()
        
    def monitor_loop(self):
        """Main monitoring loop"""
        while self.running:
            try:
                # Collect system metrics (simplified without psutil)
                self.collect_system_metrics()
                
                # Collect WolfCog specific metrics
                self.collect_wolfcog_metrics()
                
                # Save metrics
                self.save_metrics()
                
                # Check for performance issues
                self.check_performance_issues()
                
            except Exception as e:
                print(f"❌ Performance monitoring error: {e}")
            
            time.sleep(30)  # Monitor every 30 seconds
    
    def collect_system_metrics(self):
        """Collect basic system performance metrics"""
        try:
            # Get load average (Linux/Unix)
            try:
                with open('/proc/loadavg', 'r') as f:
                    load_avg = f.read().split()[:3]
                    load_avg = [float(x) for x in load_avg]
            except:
                load_avg = [0.0, 0.0, 0.0]
            
            # Get uptime
            try:
                with open('/proc/uptime', 'r') as f:
                    uptime = float(f.read().split()[0])
            except:
                uptime = 0
            
            # Basic memory info
            try:
                with open('/proc/meminfo', 'r') as f:
                    meminfo = {}
                    for line in f:
                        key, value = line.split(':')
                        meminfo[key.strip()] = int(value.split()[0]) * 1024  # Convert to bytes
                
                memory_total = meminfo.get('MemTotal', 0)
                memory_free = meminfo.get('MemFree', 0)
                memory_available = meminfo.get('MemAvailable', memory_free)
                memory_used = memory_total - memory_available
                memory_percent = (memory_used / memory_total * 100) if memory_total > 0 else 0
            except:
                memory_total = memory_used = memory_available = memory_percent = 0
            
            system_metrics = {
                "timestamp": datetime.now().isoformat(),
                "load_avg": load_avg,
                "uptime": uptime,
                "memory": {
                    "total": memory_total,
                    "used": memory_used,
                    "available": memory_available,
                    "percent": memory_percent
                }
            }
            
            self.metrics["system"] = system_metrics
            
        except Exception as e:
            print(f"❌ Error collecting system metrics: {e}")
            self.metrics["system"] = {"timestamp": datetime.now().isoformat(), "error": str(e)}
    
    def collect_wolfcog_metrics(self):
        """Collect WolfCog specific performance metrics"""
        try:
            # Task processing metrics
            task_metrics = self.get_task_processing_metrics()
            
            # Space activity metrics
            space_metrics = self.get_space_activity_metrics()
            
            # Component health metrics
            component_metrics = self.get_component_health_metrics()
            
            wolfcog_metrics = {
                "uptime": time.time() - self.start_time,
                "tasks": task_metrics,
                "spaces": space_metrics,
                "components": component_metrics
            }
            
            self.metrics["wolfcog"] = wolfcog_metrics
            
        except Exception as e:
            print(f"❌ Error collecting WolfCog metrics: {e}")
    
    def get_task_processing_metrics(self):
        """Get task processing performance metrics"""
        task_dir = Path("/tmp/ecron_tasks")
        
        if not task_dir.exists():
            return {"pending": 0, "processed": 0, "errors": 0, "throughput": 0}
        
        pending = len(list(task_dir.glob("*.json")))
        processed = len(list(task_dir.glob("*.processed")))
        errors = len(list(task_dir.glob("*.error")))
        
        return {
            "pending": pending,
            "processed": processed,
            "errors": errors,
            "throughput": self.calculate_task_throughput()
        }
    
    def calculate_task_throughput(self):
        """Calculate task processing throughput"""
        try:
            task_dir = Path("/tmp/ecron_tasks")
            processed_files = list(task_dir.glob("*.processed"))
            
            if not processed_files:
                return 0
            
            # Get recent processed files (last 5 minutes)
            recent_time = time.time() - 300
            recent_tasks = 0
            
            for file in processed_files:
                if file.stat().st_mtime > recent_time:
                    recent_tasks += 1
            
            # Tasks per minute
            return recent_tasks / 5.0
            
        except Exception:
            return 0
    
    def get_space_activity_metrics(self):
        """Get symbolic space activity metrics"""
        spaces = {}
        
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            
            if space_path.exists():
                files = list(space_path.glob("*"))
                total_size = sum(f.stat().st_size for f in files if f.is_file())
                
                spaces[space] = {
                    "files": len(files),
                    "total_size": total_size,
                    "last_modified": max((f.stat().st_mtime for f in files if f.is_file()), default=0)
                }
            else:
                spaces[space] = {"files": 0, "total_size": 0, "last_modified": 0}
        
        return spaces
    
    def get_component_health_metrics(self):
        """Get component health metrics (simplified)"""
        try:
            import subprocess
            import os
            
            # Count processes that might be WolfCog related
            result = subprocess.run(['pgrep', '-f', 'python.*wolfcog|python.*ecron|python.*agent'], 
                                  capture_output=True, text=True)
            
            if result.returncode == 0:
                pids = result.stdout.strip().split('\n') if result.stdout.strip() else []
                active_processes = len([pid for pid in pids if pid])
            else:
                active_processes = 0
            
            return {
                "active_processes": active_processes,
                "estimated_total_memory_mb": active_processes * 50,  # Rough estimate
                "health_status": "good" if active_processes > 0 else "no_processes"
            }
            
        except Exception as e:
            print(f"❌ Error getting component metrics: {e}")
            return {"active_processes": 0, "health_status": "unknown"}
    
    def check_performance_issues(self):
        """Check for performance issues and alert"""
        if "system" not in self.metrics or "wolfcog" not in self.metrics:
            return
        
        system = self.metrics["system"]
        wolfcog = self.metrics["wolfcog"]
        
        issues = []
        
        # Check load average
        if len(system.get("load_avg", [])) > 0 and system["load_avg"][0] > 2.0:
            issues.append(f"High load average: {system['load_avg'][0]:.2f}")
        
        # Check memory usage
        memory = system.get("memory", {})
        if memory.get("percent", 0) > 85:
            issues.append(f"High memory usage: {memory['percent']:.1f}%")
        
        # Check task processing
        tasks = wolfcog.get("tasks", {})
        if tasks.get("errors", 0) > 5:
            issues.append(f"High task error rate: {tasks['errors']} errors")
        
        if tasks.get("throughput", 0) < 0.1 and tasks.get("processed", 0) > 0:
            issues.append("Low task processing throughput")
        
        # Check component health
        components = wolfcog.get("components", {})
        if components.get("active_processes", 0) == 0:
            issues.append("No active WolfCog processes detected")
        
        # Report issues
        if issues:
            print("⚠️ Performance issues detected:")
            for issue in issues:
                print(f"  • {issue}")
        else:
            print("✅ System performance: Normal")
    
    def save_metrics(self):
        """Save metrics to file"""
        try:
            with open(self.report_file, 'w') as f:
                json.dump(self.metrics, f, indent=2)
        except Exception as e:
            print(f"❌ Error saving metrics: {e}")
    
    def get_metrics(self):
        """Get current metrics"""
        return self.metrics.copy()
    
    def stop(self):
        """Stop performance monitoring"""
        print("🛑 Stopping Performance Monitor...")
        self.running = False

if __name__ == "__main__":
    monitor = PerformanceMonitor()
    try:
        monitor.start()
        # Keep monitor running
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        monitor.stop()
        print("👋 Performance Monitor stopped.")