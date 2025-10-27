#!/usr/bin/env python3
"""
Comprehensive Test Suite for Enhanced Wolfram-OpenCog Bridge
Tests all components of the foundational architecture
"""

import sys
import time
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from src.type_registry import get_type_registry, TypeRegistry
from src.symbolic_patterns import get_pattern_registry, PatternRegistry
from src.bridge_monitor import get_bridge_monitor, BridgeMonitor
from src.wolfram_opencog_integration import WolframOpenCogIntegration


class TestResults:
    """Track test results"""
    
    def __init__(self):
        self.total = 0
        self.passed = 0
        self.failed = 0
        self.errors = []
    
    def record_pass(self, test_name: str):
        """Record a passing test"""
        self.total += 1
        self.passed += 1
        print(f"    ✅ {test_name}")
    
    def record_fail(self, test_name: str, error: str = ""):
        """Record a failing test"""
        self.total += 1
        self.failed += 1
        self.errors.append((test_name, error))
        print(f"    ❌ {test_name}: {error}")
    
    def print_summary(self):
        """Print test summary"""
        print(f"\n📊 Test Summary:")
        print(f"  Total tests: {self.total}")
        print(f"  Passed: {self.passed} ({self.passed/self.total*100:.1f}%)")
        print(f"  Failed: {self.failed} ({self.failed/self.total*100:.1f}%)")
        
        if self.errors:
            print(f"\n❌ Failed tests:")
            for test_name, error in self.errors:
                print(f"  - {test_name}: {error}")
        
        return self.failed == 0


def test_type_registry():
    """Test type registry functionality"""
    print("\n🧪 Testing Type Registry...")
    results = TestResults()
    
    registry = get_type_registry()
    
    # Test 1: Basic type mapping
    try:
        atomspace_type = registry.get_atomspace_type("Concept")
        if atomspace_type == "ConceptNode":
            results.record_pass("Basic type mapping (Wolfram -> AtomSpace)")
        else:
            results.record_fail("Basic type mapping", f"Expected ConceptNode, got {atomspace_type}")
    except Exception as e:
        results.record_fail("Basic type mapping", str(e))
    
    # Test 2: Reverse mapping
    try:
        wolfram_type = registry.get_wolfram_type("ConceptNode")
        if wolfram_type == "Concept":
            results.record_pass("Reverse type mapping (AtomSpace -> Wolfram)")
        else:
            results.record_fail("Reverse type mapping", f"Expected Concept, got {wolfram_type}")
    except Exception as e:
        results.record_fail("Reverse type mapping", str(e))
    
    # Test 3: Wolfram to AtomSpace conversion
    try:
        wolfram_expr = {"type": "Concept", "name": "TestConcept"}
        atomspace_repr = registry.convert_wolfram_to_atomspace(wolfram_expr)
        if atomspace_repr["type"] == "ConceptNode" and atomspace_repr["name"] == "TestConcept":
            results.record_pass("Wolfram to AtomSpace conversion")
        else:
            results.record_fail("Wolfram to AtomSpace conversion", f"Unexpected result: {atomspace_repr}")
    except Exception as e:
        results.record_fail("Wolfram to AtomSpace conversion", str(e))
    
    # Test 4: AtomSpace to Wolfram conversion
    try:
        atomspace_atom = {"type": "ConceptNode", "name": "TestNode"}
        wolfram_repr = registry.convert_atomspace_to_wolfram(atomspace_atom)
        if wolfram_repr["type"] == "Concept" and wolfram_repr["name"] == "TestNode":
            results.record_pass("AtomSpace to Wolfram conversion")
        else:
            results.record_fail("AtomSpace to Wolfram conversion", f"Unexpected result: {wolfram_repr}")
    except Exception as e:
        results.record_fail("AtomSpace to Wolfram conversion", str(e))
    
    # Test 5: Wolfram expression formatting
    try:
        wolfram_data = {"type": "Concept", "name": "FormattedConcept"}
        formatted = registry.format_wolfram_expression(wolfram_data)
        if formatted == 'Concept["FormattedConcept"]':
            results.record_pass("Wolfram expression formatting")
        else:
            results.record_fail("Wolfram expression formatting", f"Expected Concept[\"FormattedConcept\"], got {formatted}")
    except Exception as e:
        results.record_fail("Wolfram expression formatting", str(e))
    
    # Test 6: List formatting
    try:
        list_data = {"type": "List", "elements": [
            {"type": "Number", "value": 1},
            {"type": "Number", "value": 2}
        ]}
        formatted = registry.format_wolfram_expression(list_data)
        if "List[" in formatted:
            results.record_pass("List expression formatting")
        else:
            results.record_fail("List expression formatting", f"Unexpected format: {formatted}")
    except Exception as e:
        results.record_fail("List expression formatting", str(e))
    
    # Test 7: All mappings
    try:
        mappings = registry.get_all_mappings()
        if len(mappings["wolfram_to_atomspace"]) >= 25:
            results.record_pass(f"Type mappings count ({len(mappings['wolfram_to_atomspace'])} types)")
        else:
            results.record_fail("Type mappings count", f"Expected >= 25, got {len(mappings['wolfram_to_atomspace'])}")
    except Exception as e:
        results.record_fail("Type mappings count", str(e))
    
    return results.print_summary()


def test_symbolic_patterns():
    """Test symbolic patterns functionality"""
    print("\n🧪 Testing Symbolic Patterns...")
    results = TestResults()
    
    registry = get_pattern_registry()
    
    # Test 1: Pattern listing
    try:
        patterns = registry.list_patterns()
        if len(patterns) == 8:
            results.record_pass(f"Pattern registry ({len(patterns)} patterns)")
        else:
            results.record_fail("Pattern registry", f"Expected 8 patterns, got {len(patterns)}")
    except Exception as e:
        results.record_fail("Pattern registry", str(e))
    
    # Test 2: Get pattern
    try:
        pattern = registry.get_pattern("symbolic_solve")
        if pattern and pattern.name == "symbolic_solve":
            results.record_pass("Get symbolic_solve pattern")
        else:
            results.record_fail("Get symbolic_solve pattern", "Pattern not found or incorrect")
    except Exception as e:
        results.record_fail("Get symbolic_solve pattern", str(e))
    
    # Test 3: Pattern validation
    try:
        solve_pattern = registry.get_pattern("symbolic_solve")
        valid_data = {"equation": "x^2 - 4 == 0", "variable": "x"}
        invalid_data = {"variable": "x"}
        
        if solve_pattern.validate_input(valid_data) and not solve_pattern.validate_input(invalid_data):
            results.record_pass("Pattern input validation")
        else:
            results.record_fail("Pattern input validation", "Validation logic incorrect")
    except Exception as e:
        results.record_fail("Pattern input validation", str(e))
    
    # Test 4: Pattern descriptions
    try:
        all_have_descriptions = True
        for pattern_name in registry.list_patterns():
            pattern = registry.get_pattern(pattern_name)
            if not pattern.description:
                all_have_descriptions = False
                break
        
        if all_have_descriptions:
            results.record_pass("Pattern descriptions")
        else:
            results.record_fail("Pattern descriptions", "Some patterns missing descriptions")
    except Exception as e:
        results.record_fail("Pattern descriptions", str(e))
    
    # Test 5: Required patterns exist
    try:
        required = ["symbolic_solve", "pattern_matching", "inference_chain", 
                   "optimization", "differentiation", "integration"]
        patterns = registry.list_patterns()
        all_present = all(p in patterns for p in required)
        
        if all_present:
            results.record_pass("Required patterns present")
        else:
            missing = [p for p in required if p not in patterns]
            results.record_fail("Required patterns present", f"Missing: {missing}")
    except Exception as e:
        results.record_fail("Required patterns present", str(e))
    
    return results.print_summary()


def test_bridge_monitor():
    """Test bridge monitor functionality"""
    print("\n🧪 Testing Bridge Monitor...")
    results = TestResults()
    
    # Create fresh monitor for testing
    monitor = BridgeMonitor()
    
    # Test 1: Record computations
    try:
        for i in range(10):
            start_time = monitor.record_computation_start()
            time.sleep(0.001)
            success = i % 5 != 0  # 80% success rate (8 out of 10 succeed)
            monitor.record_computation_end(start_time, success)
        
        metrics = monitor.get_metrics()
        if metrics["total_computations"] == 10:
            results.record_pass("Record computations")
        else:
            results.record_fail("Record computations", f"Expected 10, got {metrics['total_computations']}")
    except Exception as e:
        results.record_fail("Record computations", str(e))
    
    # Test 2: Success rate calculation
    try:
        metrics = monitor.get_metrics()
        expected_rate = 80.0  # 8/10 = 80%
        if abs(metrics["success_rate"] - expected_rate) < 0.1:
            results.record_pass("Success rate calculation")
        else:
            results.record_fail("Success rate calculation", f"Expected {expected_rate}, got {metrics['success_rate']}")
    except Exception as e:
        results.record_fail("Success rate calculation", str(e))
    
    # Test 3: Average execution time
    try:
        metrics = monitor.get_metrics()
        avg_time = metrics["average_execution_time"]
        if avg_time > 0 and avg_time < 1:  # Should be in milliseconds range
            results.record_pass("Average execution time")
        else:
            results.record_fail("Average execution time", f"Unexpected value: {avg_time}")
    except Exception as e:
        results.record_fail("Average execution time", str(e))
    
    # Test 4: Health check
    try:
        monitor.perform_health_check()
        health = monitor.get_health_status()
        if "status" in health and "is_healthy" in health:
            results.record_pass("Health check execution")
        else:
            results.record_fail("Health check execution", "Missing health fields")
    except Exception as e:
        results.record_fail("Health check execution", str(e))
    
    # Test 5: Error recording
    try:
        monitor.record_error("test_error", "This is a test error")
        metrics = monitor.get_metrics()
        if metrics["error_count"] == 1:
            results.record_pass("Error recording")
        else:
            results.record_fail("Error recording", f"Expected 1 error, got {metrics['error_count']}")
    except Exception as e:
        results.record_fail("Error recording", str(e))
    
    # Test 6: Wolfram call tracking
    try:
        monitor.record_wolfram_call()
        monitor.record_wolfram_call()
        metrics = monitor.get_metrics()
        if metrics["wolfram_calls"] == 2:
            results.record_pass("Wolfram call tracking")
        else:
            results.record_fail("Wolfram call tracking", f"Expected 2, got {metrics['wolfram_calls']}")
    except Exception as e:
        results.record_fail("Wolfram call tracking", str(e))
    
    # Test 7: Throughput calculation
    try:
        metrics = monitor.get_metrics()
        throughput = metrics["throughput"]
        if throughput > 0:
            results.record_pass("Throughput calculation")
        else:
            results.record_fail("Throughput calculation", f"Unexpected value: {throughput}")
    except Exception as e:
        results.record_fail("Throughput calculation", str(e))
    
    return results.print_summary()


def test_integration():
    """Test complete integration"""
    print("\n🧪 Testing Wolfram-OpenCog Integration...")
    results = TestResults()
    
    integration = WolframOpenCogIntegration()
    
    # Test 1: Initialization
    try:
        if integration.initialize():
            results.record_pass("Integration initialization")
        else:
            results.record_fail("Integration initialization", "Initialization returned False")
    except Exception as e:
        results.record_fail("Integration initialization", str(e))
    
    # Test 2: Start integration
    try:
        if integration.start():
            results.record_pass("Integration start")
            time.sleep(0.5)  # Let it start
        else:
            results.record_fail("Integration start", "Start returned False")
    except Exception as e:
        results.record_fail("Integration start", str(e))
    
    # Test 3: Submit computation
    try:
        integration.submit_computation("symbolic_solve", {
            "equation": "x^2 - 16 == 0",
            "variable": "x"
        })
        time.sleep(1.0)  # Give processing thread time to handle it
        result = integration.get_result(timeout=5.0)
        if result:
            results.record_pass("Submit and retrieve computation")
        else:
            results.record_fail("Submit and retrieve computation", "No result received (check async processing)")
    except Exception as e:
        results.record_fail("Submit and retrieve computation", str(e))
    
    # Test 4: Get statistics
    try:
        stats = integration.get_integration_stats()
        required_keys = ["computations_completed", "running", "available_patterns", 
                        "monitor_metrics", "health_status"]
        all_present = all(key in stats for key in required_keys)
        
        if all_present:
            results.record_pass("Integration statistics")
        else:
            missing = [key for key in required_keys if key not in stats]
            results.record_fail("Integration statistics", f"Missing keys: {missing}")
    except Exception as e:
        results.record_fail("Integration statistics", str(e))
    
    # Test 5: Check monitor integration
    try:
        stats = integration.get_integration_stats()
        monitor_metrics = stats.get("monitor_metrics", {})
        if "total_computations" in monitor_metrics:
            results.record_pass("Monitor integration")
        else:
            results.record_fail("Monitor integration", "Monitor metrics not available")
    except Exception as e:
        results.record_fail("Monitor integration", str(e))
    
    # Test 6: Pattern registry integration
    try:
        stats = integration.get_integration_stats()
        patterns = stats.get("available_patterns", [])
        if len(patterns) >= 8:
            results.record_pass(f"Pattern registry integration ({len(patterns)} patterns)")
        else:
            results.record_fail("Pattern registry integration", f"Expected >= 8 patterns, got {len(patterns)}")
    except Exception as e:
        results.record_fail("Pattern registry integration", str(e))
    
    # Test 7: Type registry integration
    try:
        stats = integration.get_integration_stats()
        type_counts = stats.get("type_mappings_count", {})
        if type_counts.get("wolfram_to_atomspace", 0) >= 25:
            results.record_pass("Type registry integration")
        else:
            results.record_fail("Type registry integration", f"Unexpected type count: {type_counts}")
    except Exception as e:
        results.record_fail("Type registry integration", str(e))
    
    # Stop integration
    try:
        integration.stop()
        time.sleep(0.5)  # Let it stop
        results.record_pass("Integration stop")
    except Exception as e:
        results.record_fail("Integration stop", str(e))
    
    return results.print_summary()


def main():
    """Run all tests"""
    print("🐺 WolfCog Enhanced Bridge Test Suite")
    print("=" * 60)
    print("Testing foundational architecture components...")
    print("=" * 60)
    
    all_passed = True
    
    # Run test suites
    all_passed &= test_type_registry()
    all_passed &= test_symbolic_patterns()
    all_passed &= test_bridge_monitor()
    all_passed &= test_integration()
    
    # Final summary
    print("\n" + "=" * 60)
    print("🎯 FINAL RESULTS")
    print("=" * 60)
    
    if all_passed:
        print("🎉 ALL TESTS PASSED!")
        print("✅ WolfCog foundational architecture is complete and operational")
        print("✅ Type registry: 25+ type mappings")
        print("✅ Pattern registry: 8 symbolic patterns")
        print("✅ Bridge monitor: Health tracking and metrics")
        print("✅ Integration: All components working together")
        return 0
    else:
        print("❌ SOME TESTS FAILED")
        print("Please review the failed tests above and fix the issues")
        return 1


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)
