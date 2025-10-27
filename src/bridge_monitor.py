#!/usr/bin/env python3
"""
Bridge Monitor
Monitors health, performance, and statistics of the Wolfram-OpenCog bridge
"""

import time
import threading
import queue
from typing import Dict, Any, List, Optional
from collections import deque
import json


class BridgeMetrics:
    """Container for bridge metrics"""
    
    def __init__(self):
        self.total_computations = 0
        self.successful_computations = 0
        self.failed_computations = 0
        self.total_execution_time = 0.0
        self.wolfram_calls = 0
        self.atomspace_operations = 0
        self.kernel_restarts = 0
        self.errors = []
        self.start_time = time.time()
        
        # Performance tracking
        self.execution_times = deque(maxlen=100)  # Last 100 execution times
        self.throughput_samples = deque(maxlen=60)  # Last 60 seconds
    
    def record_computation(self, success: bool, execution_time: float):
        """Record a computation attempt"""
        self.total_computations += 1
        if success:
            self.successful_computations += 1
        else:
            self.failed_computations += 1
        
        self.total_execution_time += execution_time
        self.execution_times.append(execution_time)
    
    def record_wolfram_call(self):
        """Record a Wolfram Language call"""
        self.wolfram_calls += 1
    
    def record_atomspace_operation(self):
        """Record an AtomSpace operation"""
        self.atomspace_operations += 1
    
    def record_kernel_restart(self):
        """Record a kernel restart"""
        self.kernel_restarts += 1
    
    def record_error(self, error_type: str, error_message: str):
        """Record an error"""
        error_entry = {
            "timestamp": time.time(),
            "type": error_type,
            "message": error_message
        }
        self.errors.append(error_entry)
        # Keep only last 100 errors
        if len(self.errors) > 100:
            self.errors.pop(0)
    
    def get_average_execution_time(self) -> float:
        """Get average execution time"""
        if not self.execution_times:
            return 0.0
        return sum(self.execution_times) / len(self.execution_times)
    
    def get_success_rate(self) -> float:
        """Get computation success rate"""
        if self.total_computations == 0:
            return 0.0
        return (self.successful_computations / self.total_computations) * 100
    
    def get_uptime(self) -> float:
        """Get bridge uptime in seconds"""
        return time.time() - self.start_time
    
    def get_throughput(self) -> float:
        """Get current throughput (computations per second)"""
        uptime = self.get_uptime()
        if uptime == 0:
            return 0.0
        return self.total_computations / uptime
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert metrics to dictionary"""
        return {
            "total_computations": self.total_computations,
            "successful_computations": self.successful_computations,
            "failed_computations": self.failed_computations,
            "success_rate": self.get_success_rate(),
            "average_execution_time": self.get_average_execution_time(),
            "total_execution_time": self.total_execution_time,
            "wolfram_calls": self.wolfram_calls,
            "atomspace_operations": self.atomspace_operations,
            "kernel_restarts": self.kernel_restarts,
            "uptime_seconds": self.get_uptime(),
            "throughput": self.get_throughput(),
            "error_count": len(self.errors),
            "recent_errors": self.errors[-5:]  # Last 5 errors
        }


class BridgeHealthStatus:
    """Health status of the bridge"""
    
    HEALTHY = "healthy"
    DEGRADED = "degraded"
    UNHEALTHY = "unhealthy"
    
    def __init__(self):
        self.status = self.HEALTHY
        self.issues = []
        self.last_check = time.time()
    
    def update_status(self, status: str, issues: List[str] = None):
        """Update health status"""
        self.status = status
        self.issues = issues or []
        self.last_check = time.time()
    
    def is_healthy(self) -> bool:
        """Check if bridge is healthy"""
        return self.status == self.HEALTHY
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert health status to dictionary"""
        return {
            "status": self.status,
            "issues": self.issues,
            "last_check": self.last_check,
            "is_healthy": self.is_healthy()
        }


class BridgeMonitor:
    """Monitor for Wolfram-OpenCog bridge"""
    
    def __init__(self):
        self.metrics = BridgeMetrics()
        self.health = BridgeHealthStatus()
        self.monitoring = False
        self.monitor_thread = None
        self.alert_queue = queue.Queue()
        
        # Health check thresholds
        self.config = {
            "max_error_rate": 0.2,  # 20% error rate triggers degraded status
            "max_avg_execution_time": 10.0,  # 10 seconds average triggers warning
            "kernel_restart_threshold": 5,  # 5 restarts triggers warning
            "check_interval": 60  # Check every 60 seconds
        }
    
    def start_monitoring(self):
        """Start monitoring"""
        if self.monitoring:
            return
        
        self.monitoring = True
        self.monitor_thread = threading.Thread(target=self._monitoring_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()
        print("🔍 Bridge monitoring started")
    
    def stop_monitoring(self):
        """Stop monitoring"""
        self.monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=5)
        print("🛑 Bridge monitoring stopped")
    
    def _monitoring_loop(self):
        """Main monitoring loop"""
        while self.monitoring:
            self.perform_health_check()
            time.sleep(self.config["check_interval"])
    
    def perform_health_check(self):
        """Perform health check"""
        issues = []
        status = BridgeHealthStatus.HEALTHY
        
        # Check error rate
        error_rate = 1.0 - (self.metrics.get_success_rate() / 100.0)
        if error_rate > self.config["max_error_rate"]:
            issues.append(f"High error rate: {error_rate*100:.1f}%")
            status = BridgeHealthStatus.DEGRADED
        
        # Check average execution time
        avg_time = self.metrics.get_average_execution_time()
        if avg_time > self.config["max_avg_execution_time"]:
            issues.append(f"High average execution time: {avg_time:.2f}s")
            status = BridgeHealthStatus.DEGRADED
        
        # Check kernel restarts
        if self.metrics.kernel_restarts >= self.config["kernel_restart_threshold"]:
            issues.append(f"Multiple kernel restarts: {self.metrics.kernel_restarts}")
            status = BridgeHealthStatus.UNHEALTHY
        
        # Update health status
        self.health.update_status(status, issues)
        
        # Generate alerts for issues
        if issues:
            for issue in issues:
                self.alert_queue.put({
                    "timestamp": time.time(),
                    "severity": "warning" if status == BridgeHealthStatus.DEGRADED else "critical",
                    "message": issue
                })
    
    def record_computation_start(self) -> float:
        """Record start of computation and return start time"""
        return time.time()
    
    def record_computation_end(self, start_time: float, success: bool):
        """Record end of computation"""
        execution_time = time.time() - start_time
        self.metrics.record_computation(success, execution_time)
    
    def record_wolfram_call(self):
        """Record a Wolfram call"""
        self.metrics.record_wolfram_call()
    
    def record_atomspace_operation(self):
        """Record an AtomSpace operation"""
        self.metrics.record_atomspace_operation()
    
    def record_kernel_restart(self):
        """Record a kernel restart"""
        self.metrics.record_kernel_restart()
    
    def record_error(self, error_type: str, error_message: str):
        """Record an error"""
        self.metrics.record_error(error_type, error_message)
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get current metrics"""
        return self.metrics.to_dict()
    
    def get_health_status(self) -> Dict[str, Any]:
        """Get current health status"""
        return self.health.to_dict()
    
    def get_alerts(self) -> List[Dict[str, Any]]:
        """Get pending alerts"""
        alerts = []
        while not self.alert_queue.empty():
            try:
                alerts.append(self.alert_queue.get_nowait())
            except queue.Empty:
                break
        return alerts
    
    def get_full_report(self) -> Dict[str, Any]:
        """Get full monitoring report"""
        return {
            "timestamp": time.time(),
            "metrics": self.get_metrics(),
            "health": self.get_health_status(),
            "alerts": self.get_alerts()
        }
    
    def export_metrics(self, filepath: str):
        """Export metrics to JSON file"""
        report = self.get_full_report()
        with open(filepath, 'w') as f:
            json.dump(report, f, indent=2)
    
    def reset_metrics(self):
        """Reset metrics counters"""
        self.metrics = BridgeMetrics()
        print("📊 Metrics reset")


# Singleton instance
_bridge_monitor = None

def get_bridge_monitor() -> BridgeMonitor:
    """Get the global bridge monitor instance"""
    global _bridge_monitor
    if _bridge_monitor is None:
        _bridge_monitor = BridgeMonitor()
    return _bridge_monitor


if __name__ == "__main__":
    # Test bridge monitor
    print("🧪 Testing Bridge Monitor...")
    
    monitor = get_bridge_monitor()
    
    # Test recording computations
    print("\n1. Recording test computations:")
    for i in range(10):
        start_time = monitor.record_computation_start()
        time.sleep(0.01)  # Simulate computation
        success = i % 8 != 0  # 12.5% failure rate
        monitor.record_computation_end(start_time, success)
        monitor.record_wolfram_call()
        monitor.record_atomspace_operation()
    
    # Get metrics
    print("\n2. Current metrics:")
    metrics = monitor.get_metrics()
    for key, value in metrics.items():
        if key != "recent_errors":
            print(f"  {key}: {value}")
    
    # Perform health check
    print("\n3. Health check:")
    monitor.perform_health_check()
    health = monitor.get_health_status()
    print(f"  Status: {health['status']}")
    print(f"  Healthy: {health['is_healthy']}")
    if health['issues']:
        print(f"  Issues: {health['issues']}")
    
    # Test error recording
    print("\n4. Recording test error:")
    monitor.record_error("test_error", "This is a test error")
    
    # Get full report
    print("\n5. Full report:")
    report = monitor.get_full_report()
    print(f"  Report timestamp: {report['timestamp']}")
    print(f"  Metrics keys: {list(report['metrics'].keys())}")
    print(f"  Health status: {report['health']['status']}")
    
    print("\n✅ Bridge Monitor tests completed")
