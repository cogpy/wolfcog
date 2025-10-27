#!/usr/bin/env python3
"""
Wolfram-OpenCog Integration Manager
Manages the complete integration between Wolfram Language and OpenCog
Coordinates kernel pools, bridges, and symbolic computation
"""

import threading
import time
import json
import queue
import subprocess
import sys
from pathlib import Path
from typing import Dict, Any, List, Optional

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.symbolic_processor import RealSymbolicProcessor
from src.wolfram_opencog_bridge import WolframOpenCogBridge
from src.type_registry import get_type_registry
from src.symbolic_patterns import get_pattern_registry
from src.bridge_monitor import get_bridge_monitor


class WolframOpenCogIntegration:
    """Complete integration manager for Wolfram-OpenCog bridge"""
    
    def __init__(self):
        self.running = False
        self.symbolic_processor = None
        self.wolfram_bridge = None
        self.guile_bridge = None
        
        # Integration queues
        self.computation_queue = queue.Queue()
        self.result_queue = queue.Queue()
        
        # Enhanced components
        self.type_registry = get_type_registry()
        self.pattern_registry = get_pattern_registry()
        self.monitor = get_bridge_monitor()
        
        # Statistics
        self.stats = {
            "computations_completed": 0,
            "wolfram_calls": 0,
            "opencog_operations": 0,
            "bridge_errors": 0,
            "start_time": None
        }
    
    def initialize(self):
        """Initialize all integration components"""
        print("🚀 Initializing Wolfram-OpenCog Integration...")
        
        # Initialize symbolic processor
        self.symbolic_processor = RealSymbolicProcessor()
        self.symbolic_processor.start()
        print("✅ Symbolic processor initialized")
        
        # Initialize Wolfram bridge
        self.wolfram_bridge = WolframOpenCogBridge()
        if self.wolfram_bridge.start_kernel_pool(3):
            print("✅ Wolfram bridge initialized")
        else:
            print("⚠️ Wolfram bridge using simulation mode")
        
        # Initialize Guile bridge
        if self.initialize_guile_bridge():
            print("✅ Guile bridge initialized")
        else:
            print("⚠️ Guile bridge not available")
        
        self.stats["start_time"] = time.time()
        return True
    
    def initialize_guile_bridge(self):
        """Initialize Guile Scheme bridge"""
        try:
            # Test if Guile is available and load our bridge
            result = subprocess.run([
                "guile", "-l", "kernels/wolfram-opencog-bridge.scm",
                "-c", "(init-wolfram-bridge)"
            ], capture_output=True, text=True, timeout=10)
            
            if result.returncode == 0:
                self.guile_bridge = True
                return True
            else:
                print(f"Guile bridge error: {result.stderr}")
                return False
                
        except Exception as e:
            print(f"Failed to initialize Guile bridge: {e}")
            return False
    
    def start(self):
        """Start the integration system"""
        if not self.initialize():
            return False
        
        print("🔄 Starting integration system...")
        self.running = True
        
        # Start monitoring
        self.monitor.start_monitoring()
        
        # Start processing thread
        process_thread = threading.Thread(target=self.processing_loop)
        process_thread.daemon = True
        process_thread.start()
        
        print("✅ Wolfram-OpenCog Integration running")
        return True
    
    def processing_loop(self):
        """Main processing loop"""
        while self.running:
            try:
                # Get computation request
                try:
                    request = self.computation_queue.get(timeout=1.0)
                    
                    # Record computation start
                    start_time = self.monitor.record_computation_start()
                    
                    # Process request
                    result = self.process_integration_request(request)
                    
                    # Record computation end
                    success = result.get("success", False)
                    self.monitor.record_computation_end(start_time, success)
                    
                    self.result_queue.put(result)
                    self.stats["computations_completed"] += 1
                    
                except queue.Empty:
                    continue
                    
            except Exception as e:
                print(f"❌ Integration processing error: {e}")
                self.stats["bridge_errors"] += 1
                self.monitor.record_error("processing_error", str(e))
    
    def process_integration_request(self, request):
        """Process an integration request using pattern registry"""
        pattern = request.get("pattern", "symbolic_solve")
        data = request.get("data", {})
        
        # Use pattern registry for execution
        result = self.pattern_registry.execute_pattern(pattern, self, data)
        
        return result
    
    def submit_computation(self, pattern, data):
        """Submit a computation request"""
        request = {"pattern": pattern, "data": data}
        self.computation_queue.put(request)
    
    def get_result(self, timeout=5.0):
        """Get a computation result"""
        try:
            return self.result_queue.get(timeout=timeout)
        except queue.Empty:
            return None
    
    def get_integration_stats(self):
        """Get integration statistics"""
        current_time = time.time()
        uptime = current_time - (self.stats["start_time"] or current_time)
        
        stats = {
            **self.stats,
            "uptime_seconds": uptime,
            "running": self.running,
            "computation_queue_size": self.computation_queue.qsize(),
            "result_queue_size": self.result_queue.qsize(),
        }
        
        # Add component stats
        if self.symbolic_processor:
            stats["symbolic_processor"] = self.symbolic_processor.get_stats()
        
        if self.wolfram_bridge:
            stats["wolfram_bridge"] = self.wolfram_bridge.get_statistics()
        
        # Add monitor metrics and health
        stats["monitor_metrics"] = self.monitor.get_metrics()
        stats["health_status"] = self.monitor.get_health_status()
        
        # Add available patterns
        stats["available_patterns"] = self.pattern_registry.list_patterns()
        
        # Add type mappings count
        type_mappings = self.type_registry.get_all_mappings()
        stats["type_mappings_count"] = {
            "wolfram_to_atomspace": len(type_mappings["wolfram_to_atomspace"]),
            "atomspace_to_wolfram": len(type_mappings["atomspace_to_wolfram"])
        }
        
        return stats
    
    def test_integration(self):
        """Test the complete integration"""
        print("🧪 Testing Wolfram-OpenCog Integration...")
        
        test_cases = [
            {
                "name": "Symbolic Solve",
                "pattern": "symbolic_solve",
                "data": {"equation": "x^2 - 9 == 0", "variable": "x"}
            },
            {
                "name": "Pattern Match", 
                "pattern": "pattern_match",
                "data": {"pattern": "Dog->*", "context": "animals"}
            },
            {
                "name": "Inference Chain",
                "pattern": "inference_chain", 
                "data": {"premises": ["A isa B", "B isa C"], "goal": "A isa C"}
            },
            {
                "name": "Optimization",
                "pattern": "optimization",
                "data": {"objective": "x^2 + y^2", "variables": ["x", "y"]}
            },
            {
                "name": "Knowledge Extraction",
                "pattern": "knowledge_extraction",
                "data": {"text": "The cat is on the mat. Cats are animals.", "domain": "pets"}
            }
        ]
        
        results = {}
        for test_case in test_cases:
            print(f"  Testing {test_case['name']}...")
            self.submit_computation(test_case["pattern"], test_case["data"])
            result = self.get_result(timeout=10.0)
            results[test_case["name"]] = result
            
            if result and result.get("success", False):
                print(f"    ✅ {test_case['name']} passed")
            else:
                print(f"    ❌ {test_case['name']} failed")
        
        # Print summary
        passed = sum(1 for r in results.values() if r and r.get("success", False))
        total = len(test_cases)
        print(f"🏆 Test Results: {passed}/{total} passed")
        
        return results
    
    def stop(self):
        """Stop the integration system"""
        print("🛑 Stopping Wolfram-OpenCog Integration...")
        self.running = False
        
        # Stop monitoring
        self.monitor.stop_monitoring()
        
        if self.symbolic_processor:
            self.symbolic_processor.stop()
        
        if self.wolfram_bridge:
            self.wolfram_bridge.stop()
        
        print("✅ Integration stopped")


def main():
    """Test the integration system"""
    integration = WolframOpenCogIntegration()
    
    if integration.start():
        try:
            # Run integration tests
            results = integration.test_integration()
            
            # Show statistics
            stats = integration.get_integration_stats()
            print(f"\n📊 Integration Statistics:")
            for key, value in stats.items():
                if not isinstance(value, dict):
                    print(f"  {key}: {value}")
            
            print("\n🚀 Integration system ready for use")
            print("Use integration.submit_computation(pattern, data) to submit requests")
            
            # Keep running for interactive use
            input("Press Enter to stop the integration system...")
            
        except KeyboardInterrupt:
            print("\n🛑 Interrupted by user")
        finally:
            integration.stop()
    else:
        print("❌ Failed to start integration system")


if __name__ == "__main__":
    main()
