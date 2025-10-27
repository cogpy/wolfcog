#!/usr/bin/env python3
"""
WolfCog Enhanced Bridge - Quick Start Example
Demonstrates how to use the completed foundational architecture
"""

import sys
import time
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from src.wolfram_opencog_integration import WolframOpenCogIntegration
from src.type_registry import get_type_registry
from src.symbolic_patterns import get_pattern_registry
from src.bridge_monitor import get_bridge_monitor


def main():
    """Quick start example"""
    print("🐺 WolfCog Enhanced Bridge - Quick Start")
    print("=" * 60)
    
    # Create and start the integration system
    print("\n1. Initializing WolfCog integration...")
    integration = WolframOpenCogIntegration()
    
    if not integration.start():
        print("❌ Failed to start integration")
        return
    
    print("✅ Integration system started")
    time.sleep(0.5)  # Let it initialize
    
    # Show available components
    print("\n2. Available Components:")
    
    # Type registry
    type_registry = get_type_registry()
    mappings = type_registry.get_all_mappings()
    print(f"   📋 Type Registry: {len(mappings['wolfram_to_atomspace'])} type mappings")
    
    # Pattern registry
    pattern_registry = get_pattern_registry()
    patterns = pattern_registry.list_patterns()
    print(f"   🔧 Pattern Registry: {len(patterns)} computation patterns")
    for pattern in patterns:
        p = pattern_registry.get_pattern(pattern)
        print(f"      - {pattern}: {p.description[:50]}...")
    
    # Monitor
    monitor = get_bridge_monitor()
    print(f"   📊 Bridge Monitor: Tracking performance and health")
    
    # Example 1: Solve an equation
    print("\n3. Example 1: Solve Symbolic Equation")
    integration.submit_computation("symbolic_solve", {
        "equation": "x^2 - 9 == 0",
        "variable": "x"
    })
    time.sleep(0.5)  # Brief wait for async processing
    result = integration.get_result(timeout=5.0)
    if result:
        print(f"   Input: x^2 - 9 == 0")
        print(f"   Status: {'✅ Success' if result.get('success') else '❌ Failed'}")
        if result.get("success"):
            print(f"   Solution: {result.get('solution', 'N/A')}")
    
    # Example 2: Differentiate a function
    print("\n4. Example 2: Symbolic Differentiation")
    integration.submit_computation("differentiation", {
        "expression": "x^3 + 2*x^2 - 5*x + 1",
        "variable": "x",
        "order": 1
    })
    time.sleep(0.5)  # Brief wait for async processing
    result = integration.get_result(timeout=5.0)
    if result:
        print(f"   Input: x^3 + 2*x^2 - 5*x + 1")
        print(f"   Status: {'✅ Success' if result.get('success') else '❌ Failed'}")
        if result.get("success"):
            print(f"   Derivative: {result.get('derivative', 'N/A')}")
    
    # Example 3: Optimize a function
    print("\n5. Example 3: Symbolic Optimization")
    integration.submit_computation("optimization", {
        "objective": "x^2 + y^2",
        "variables": ["x", "y"]
    })
    time.sleep(0.5)  # Brief wait for async processing
    result = integration.get_result(timeout=5.0)
    if result:
        print(f"   Objective: x^2 + y^2")
        print(f"   Status: {'✅ Success' if result.get('success') else '❌ Failed'}")
        if result.get("success"):
            print(f"   Result: {result.get('result', 'N/A')}")
    
    # Show system statistics
    print("\n6. System Statistics:")
    stats = integration.get_integration_stats()
    
    print(f"   Computations completed: {stats['computations_completed']}")
    print(f"   Queue size: {stats['computation_queue_size']}")
    print(f"   Uptime: {stats['uptime_seconds']:.1f} seconds")
    
    # Monitor metrics
    monitor_metrics = stats.get("monitor_metrics", {})
    if monitor_metrics:
        print(f"\n7. Performance Metrics:")
        print(f"   Success rate: {monitor_metrics.get('success_rate', 0):.1f}%")
        print(f"   Avg execution time: {monitor_metrics.get('average_execution_time', 0):.3f}s")
        print(f"   Throughput: {monitor_metrics.get('throughput', 0):.2f} ops/sec")
    
    # Health status
    health = stats.get("health_status", {})
    if health:
        print(f"\n8. System Health:")
        status = health.get("status", "unknown")
        is_healthy = health.get("is_healthy", False)
        print(f"   Status: {status} {'✅' if is_healthy else '⚠️'}")
        if health.get("issues"):
            print(f"   Issues: {health['issues']}")
    
    # Type conversion example
    print("\n9. Type Conversion Example:")
    wolfram_expr = {"type": "Concept", "name": "ExampleConcept"}
    atomspace_repr = type_registry.convert_wolfram_to_atomspace(wolfram_expr)
    formatted = type_registry.format_wolfram_expression(wolfram_expr)
    print(f"   Wolfram: {wolfram_expr}")
    print(f"   AtomSpace: {atomspace_repr}")
    print(f"   Formatted: {formatted}")
    
    # Clean shutdown
    print("\n10. Shutting down...")
    integration.stop()
    time.sleep(0.5)
    
    print("\n✅ Quick start completed successfully!")
    print("\n📚 For more information:")
    print("   - API Documentation: docs/enhanced-bridge-api.md")
    print("   - Test Suite: python test_enhanced_bridge.py")
    print("   - Integration Tests: python src/wolfram_opencog_integration.py")


if __name__ == "__main__":
    main()
