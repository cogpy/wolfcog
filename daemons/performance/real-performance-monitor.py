#!/usr/bin/env python3
"""
Real WolfCog Performance Monitor
Production implementation with actual system metrics
Monitors real CPU, memory, I/O, and symbolic computation performance
"""

import time
import json
import threading
import os
from pathlib import Path
from typing import Dict, List, Optional

class RealPerformanceMonitor:
    """Real performance monitor with actual system metrics"""
    
    def __init__(self, metrics_file="performance_metrics.json"):
        self.running = False
        self.metrics_file = Path(metrics_file)
        self.collection_interval = 30  # seconds
        
        # Real metrics storage
        self.metrics_history = []
        self.current_metrics = {
            "timestamp": 0,
            "system": {
                "cpu_usage": 0.0,
                "memory_usage": 0.0,
                "memory_total": 0,
                "memory_free": 0,
                "load_average": 0.0,
                "disk_usage": 0.0
            },
            "wolfcog": {
                "processes": 0,
                "tasks_processed": 0,
                "symbolic_operations": 0,
                "atomspace_size": 0,
                "component_health": 0.0
            },
            "performance": {
                "task_throughput": 0.0,
                "average_task_time": 0.0,
                "symbolic_ops_per_second": 0.0,
                "memory_efficiency": 0.0
            }
        }
        
        print("📊 Real Performance Monitor initialized")
    
    def start(self):
        """Start real performance monitoring"""
        print("🚀 Starting Real Performance Monitor...")
        self.running = True
        
        # Start metrics collection thread
        collection_thread = threading.Thread(target=self.collect_real_metrics)
        collection_thread.daemon = True
        collection_thread.start()
        
        # Start metrics analysis thread
        analysis_thread = threading.Thread(target=self.analyze_performance)
        analysis_thread.daemon = True
        analysis_thread.start()
        
        print("✅ Real performance monitoring started")
    
    def collect_real_metrics(self):
        """Collect actual system metrics"""
        while self.running:
            try:
                # Collect real system metrics
                system_metrics = self.get_real_system_metrics()
                wolfcog_metrics = self.get_real_wolfcog_metrics()
                performance_metrics = self.calculate_real_performance_metrics()
                
                # Update current metrics
                self.current_metrics.update({
                    "timestamp": time.time(),
                    "system": system_metrics,
                    "wolfcog": wolfcog_metrics,
                    "performance": performance_metrics
                })
                
                # Add to history
                self.metrics_history.append(self.current_metrics.copy())
                
                # Keep only last 100 entries
                if len(self.metrics_history) > 100:
                    self.metrics_history = self.metrics_history[-100:]
                
                # Save to file
                self.save_metrics()
                
                # Log summary every 10 collections
                if len(self.metrics_history) % 10 == 0:
                    self.log_metrics_summary()
                
            except Exception as e:
                print(f"❌ Error collecting metrics: {e}")
            
            time.sleep(self.collection_interval)
    
    def get_real_system_metrics(self) -> Dict:
        """Get actual system metrics from /proc"""
        try:
            # CPU load average
            with open('/proc/loadavg', 'r') as f:
                load_avg = float(f.read().split()[0])
            
            # Memory information
            with open('/proc/meminfo', 'r') as f:
                meminfo = f.read()
                
            mem_total = int([line for line in meminfo.split('\n') if 'MemTotal' in line][0].split()[1])
            mem_free = int([line for line in meminfo.split('\n') if 'MemFree' in line][0].split()[1])
            mem_available = int([line for line in meminfo.split('\n') if 'MemAvailable' in line][0].split()[1])
            
            memory_usage = ((mem_total - mem_available) / mem_total) * 100
            
            # Disk usage for root
            stat = os.statvfs('/')
            disk_total = stat.f_blocks * stat.f_frsize
            disk_free = stat.f_available * stat.f_frsize
            disk_usage = ((disk_total - disk_free) / disk_total) * 100
            
            return {
                "cpu_usage": min(load_avg * 100, 100),  # Approximate CPU usage
                "memory_usage": memory_usage,
                "memory_total": mem_total * 1024,  # Convert to bytes
                "memory_free": mem_free * 1024,
                "load_average": load_avg,
                "disk_usage": disk_usage
            }
            
        except Exception as e:
            print(f"❌ Error getting system metrics: {e}")
            return {
                "cpu_usage": 0.0,
                "memory_usage": 0.0,
                "memory_total": 0,
                "memory_free": 0,
                "load_average": 0.0,
                "disk_usage": 0.0
            }
    
    def get_real_wolfcog_metrics(self) -> Dict:
        """Get real WolfCog system metrics"""
        try:
            # Count running WolfCog processes
            processes = 0
            try:
                result = os.popen("ps aux | grep -E '(wolfcog|ecron|agent)' | grep -v grep | wc -l").read()
                processes = int(result.strip())
            except:
                processes = 0
            
            # Get task metrics
            tasks_processed = 0
            task_path = Path("/tmp/ecron_tasks")
            if task_path.exists():
                tasks_processed = len(list(task_path.glob("*.processed")))
            
            # Get symbolic operations from metrics files
            symbolic_operations = 0
            atomspace_size = 0
            
            try:
                # Look for task processor metrics
                for result_file in task_path.glob("*.result"):
                    with open(result_file, 'r') as f:
                        result_data = json.load(f)
                        if 'result' in result_data and 'computation' in result_data['result']:
                            symbolic_operations += 1
                        if 'atomspace_size' in result_data:
                            atomspace_size = max(atomspace_size, result_data['atomspace_size'])
            except:
                pass
            
            # Calculate component health
            expected_components = 6  # Based on real coordinator
            component_health = min(processes / expected_components, 1.0) * 100
            
            return {
                "processes": processes,
                "tasks_processed": tasks_processed,
                "symbolic_operations": symbolic_operations,
                "atomspace_size": atomspace_size,
                "component_health": component_health
            }
            
        except Exception as e:
            print(f"❌ Error getting WolfCog metrics: {e}")
            return {
                "processes": 0,
                "tasks_processed": 0,
                "symbolic_operations": 0,
                "atomspace_size": 0,
                "component_health": 0.0
            }
    
    def calculate_real_performance_metrics(self) -> Dict:
        """Calculate real performance metrics"""
        try:
            # Calculate metrics from history
            if len(self.metrics_history) < 2:
                return {
                    "task_throughput": 0.0,
                    "average_task_time": 0.0,
                    "symbolic_ops_per_second": 0.0,
                    "memory_efficiency": 0.0
                }
            
            # Get recent metrics
            current = self.current_metrics
            previous = self.metrics_history[-1] if self.metrics_history else current
            
            # Calculate throughput
            time_diff = current["timestamp"] - previous["timestamp"]
            if time_diff > 0:
                task_diff = current["wolfcog"]["tasks_processed"] - previous["wolfcog"]["tasks_processed"]
                task_throughput = task_diff / time_diff
                
                ops_diff = current["wolfcog"]["symbolic_operations"] - previous["wolfcog"]["symbolic_operations"]
                ops_per_second = ops_diff / time_diff
            else:
                task_throughput = 0.0
                ops_per_second = 0.0
            
            # Calculate memory efficiency
            memory_used = current["system"]["memory_usage"]
            atomspace_size = current["wolfcog"]["atomspace_size"]
            
            if memory_used > 0 and atomspace_size > 0:
                memory_efficiency = atomspace_size / memory_used
            else:
                memory_efficiency = 0.0
            
            # Estimate average task time
            if task_throughput > 0:
                average_task_time = 1.0 / task_throughput
            else:
                average_task_time = 0.0
            
            return {
                "task_throughput": task_throughput,
                "average_task_time": average_task_time,
                "symbolic_ops_per_second": ops_per_second,
                "memory_efficiency": memory_efficiency
            }
            
        except Exception as e:
            print(f"❌ Error calculating performance metrics: {e}")
            return {
                "task_throughput": 0.0,
                "average_task_time": 0.0,
                "symbolic_ops_per_second": 0.0,
                "memory_efficiency": 0.0
            }
    
    def analyze_performance(self):
        """Analyze performance trends and issues"""
        while self.running:
            try:
                if len(self.metrics_history) >= 5:
                    self.detect_performance_issues()
                    self.generate_performance_insights()
                
            except Exception as e:
                print(f"❌ Error analyzing performance: {e}")
            
            time.sleep(60)  # Analyze every minute
    
    def detect_performance_issues(self):
        """Detect real performance issues"""
        current = self.current_metrics
        
        # Check for high resource usage
        if current["system"]["cpu_usage"] > 80:
            print("⚠️ High CPU usage detected: {:.1f}%".format(current["system"]["cpu_usage"]))
        
        if current["system"]["memory_usage"] > 85:
            print("⚠️ High memory usage detected: {:.1f}%".format(current["system"]["memory_usage"]))
        
        if current["system"]["disk_usage"] > 90:
            print("⚠️ High disk usage detected: {:.1f}%".format(current["system"]["disk_usage"]))
        
        # Check for low component health
        if current["wolfcog"]["component_health"] < 70:
            print("⚠️ Low component health: {:.1f}%".format(current["wolfcog"]["component_health"]))
        
        # Check for low throughput
        if current["performance"]["task_throughput"] < 0.1 and len(self.metrics_history) > 10:
            print("⚠️ Low task throughput detected: {:.3f} tasks/second".format(
                current["performance"]["task_throughput"]))
    
    def generate_performance_insights(self):
        """Generate real performance insights"""
        if len(self.metrics_history) < 10:
            return
        
        # Calculate trends
        recent_metrics = self.metrics_history[-10:]
        
        # CPU trend
        cpu_values = [m["system"]["cpu_usage"] for m in recent_metrics]
        cpu_trend = "increasing" if cpu_values[-1] > cpu_values[0] else "decreasing"
        
        # Memory trend
        mem_values = [m["system"]["memory_usage"] for m in recent_metrics]
        mem_trend = "increasing" if mem_values[-1] > mem_values[0] else "decreasing"
        
        # Throughput trend
        throughput_values = [m["performance"]["task_throughput"] for m in recent_metrics]
        throughput_avg = sum(throughput_values) / len(throughput_values)
        
        # Log insights occasionally
        if len(self.metrics_history) % 20 == 0:
            print(f"📈 Performance Insights:")
            print(f"   CPU trend: {cpu_trend}")
            print(f"   Memory trend: {mem_trend}")
            print(f"   Average throughput: {throughput_avg:.3f} tasks/second")
    
    def save_metrics(self):
        """Save metrics to file"""
        try:
            metrics_data = {
                "last_updated": time.time(),
                "current_metrics": self.current_metrics,
                "history_size": len(self.metrics_history),
                "collection_interval": self.collection_interval
            }
            
            with open(self.metrics_file, 'w') as f:
                json.dump(metrics_data, f, indent=2)
                
        except Exception as e:
            print(f"❌ Error saving metrics: {e}")
    
    def log_metrics_summary(self):
        """Log real metrics summary"""
        current = self.current_metrics
        
        print("📊 Real Performance Summary:")
        print(f"   System - CPU: {current['system']['cpu_usage']:.1f}%, "
              f"Memory: {current['system']['memory_usage']:.1f}%, "
              f"Load: {current['system']['load_average']:.2f}")
        print(f"   WolfCog - Processes: {current['wolfcog']['processes']}, "
              f"Health: {current['wolfcog']['component_health']:.1f}%")
        print(f"   Performance - Throughput: {current['performance']['task_throughput']:.3f} tasks/s, "
              f"AtomSpace: {current['wolfcog']['atomspace_size']} atoms")
    
    def get_current_metrics(self) -> Dict:
        """Get current metrics"""
        return self.current_metrics.copy()
    
    def get_metrics_history(self) -> List[Dict]:
        """Get metrics history"""
        return self.metrics_history.copy()
    
    def stop(self):
        """Stop performance monitoring"""
        print("🛑 Stopping Real Performance Monitor...")
        self.running = False
        
        # Save final metrics
        self.save_metrics()
        
        print("✅ Real Performance Monitor stopped")


def main():
    """Main function for standalone execution"""
    monitor = RealPerformanceMonitor()
    
    try:
        monitor.start()
        
        print("📊 Real Performance Monitor running...")
        print(f"📁 Metrics file: {monitor.metrics_file}")
        print("Press Ctrl+C to stop")
        
        # Keep monitor running
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        monitor.stop()
        print("👋 Real Performance Monitor stopped.")


if __name__ == "__main__":
    main()
