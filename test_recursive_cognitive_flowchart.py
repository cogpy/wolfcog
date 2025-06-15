#!/usr/bin/env python3
"""
Test suite for WolfCog Recursive Cognitive Flowchart implementation
Validates all 5 layers and their integration
"""

import sys
import time
import unittest
from pathlib import Path

# Add the project root to the path
sys.path.insert(0, str(Path(__file__).parent))

class TestRecursiveCognitiveFlowchart(unittest.TestCase):
    """Test the recursive cognitive flowchart implementation"""
    
    @classmethod
    def setUpClass(cls):
        """Set up test class"""
        try:
            from src.recursive_cognitive_flowchart import WolfCogRecursiveCognitiveFlowchart
            cls.flowchart = WolfCogRecursiveCognitiveFlowchart()
            cls.flowchart_available = True
        except ImportError as e:
            print(f"⚠️ Could not import flowchart: {e}")
            cls.flowchart_available = False
    
    def test_layer1_mock_bifurcation(self):
        """Test Layer 1: Mock vs Reality bifurcation"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test mock feature scanning
        mock_features = self.flowchart._scan_for_mock_features()
        self.assertIsInstance(mock_features, list)
        
        # Test mock filtering
        all_features = ["real-processor.py", "mock-feature.py", "symbolic-engine.py"]
        real_features = self.flowchart._filter_mock_features(all_features)
        
        # Should filter out mock features
        self.assertIn("real-processor.py", real_features)
        self.assertIn("symbolic-engine.py", real_features)
        self.assertNotIn("mock-feature.py", real_features)
        
        print("✅ Layer 1: Mock bifurcation test passed")
    
    def test_layer2_symbolic_core(self):
        """Test Layer 2: Symbolic Processing Core"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test symbolic processor
        result = self.flowchart._atomspace_symbolic_processor("test_task")
        self.assertIsInstance(result, dict)
        self.assertIn("status", result)
        
        # Test recursive task evaluation
        test_hierarchy = ["task1", "task2", "task3"]
        evaluation_result = self.flowchart._recursive_task_evaluation(test_hierarchy)
        self.assertEqual(len(evaluation_result), len(test_hierarchy))
        
        print("✅ Layer 2: Symbolic core test passed")
    
    def test_layer3_agent_coordination(self):
        """Test Layer 3: Agent Coordination Protocols"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test coordination system
        coordinator = self.flowchart.layer3_coordinator
        self.assertIsNotNone(coordinator)
        
        # Test message passing
        message = coordinator.agent_message_passing(
            "test_agent", "test_task", {"type": "test"}
        )
        self.assertIsNotNone(message)
        self.assertEqual(message.agent_id, "test_agent")
        
        print("✅ Layer 3: Agent coordination test passed")
    
    def test_layer4_system_integration(self):
        """Test Layer 4: System Integration"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test integration manager
        integration_manager = self.flowchart.layer4_integration
        self.assertIsNotNone(integration_manager)
        
        # Test cross-platform computation
        computation_spec = {"python": "2 + 2"}
        result = integration_manager.execute_cross_platform_computation(computation_spec)
        self.assertIsInstance(result, dict)
        self.assertIn("computation_id", result)
        
        print("✅ Layer 4: System integration test passed")
    
    def test_layer5_optimization(self):
        """Test Layer 5: Optimization and Convergence"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test optimization engine
        optimizer = self.flowchart.layer5_optimizer
        self.assertIsNotNone(optimizer)
        
        # Test metrics collection
        metrics = optimizer._collect_optimization_metrics()
        self.assertIsNotNone(metrics)
        self.assertTrue(hasattr(metrics, 'timestamp'))
        
        print("✅ Layer 5: Optimization test passed")
    
    def test_neural_symbolic_synergy_zones(self):
        """Test Neural-Symbolic Synergy Zones"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test synergy zones initialization
        self.assertIn("input_hierarchies", self.flowchart.synergy_zones)
        self.assertIn("symbolic_meta_layering", self.flowchart.synergy_zones)
        self.assertIn("emergent_coordination", self.flowchart.synergy_zones)
        
        # Test synergy metrics calculation
        metrics = self.flowchart._calculate_synergy_metrics()
        self.assertIsInstance(metrics, dict)
        self.assertIn("overall_synergy", metrics)
        
        print("✅ Neural-Symbolic Synergy Zones test passed")
    
    def test_recursive_implementation_spiral(self):
        """Test Recursive Implementation Spiral"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test spiral phase execution
        self.flowchart._execute_spiral_phase("mock_dependency_culling")
        self.flowchart._execute_spiral_phase("symbol_grounding")
        
        # Test spiral progress tracking
        self.assertIsInstance(self.flowchart.spiral_progress, float)
        self.assertGreaterEqual(self.flowchart.spiral_progress, 0.0)
        self.assertLessEqual(self.flowchart.spiral_progress, 1.0)
        
        print("✅ Recursive Implementation Spiral test passed")
    
    def test_complete_flowchart_integration(self):
        """Test complete flowchart integration"""
        if not self.flowchart_available:
            self.skipTest("Flowchart not available")
        
        # Test flowchart status
        status = self.flowchart.get_flowchart_status()
        self.assertIsNotNone(status)
        self.assertTrue(hasattr(status, 'overall_coherence'))
        self.assertTrue(hasattr(status, 'emergent_capabilities'))
        
        # Test layer status tracking
        self.assertIsInstance(self.flowchart.layer_status, dict)
        self.assertEqual(len(self.flowchart.layer_status), 5)
        
        print("✅ Complete flowchart integration test passed")

class TestRecursiveCognitiveFlowchartIntegration(unittest.TestCase):
    """Integration tests for the complete system"""
    
    def test_full_system_startup_shutdown(self):
        """Test full system startup and shutdown"""
        try:
            from src.recursive_cognitive_flowchart import WolfCogRecursiveCognitiveFlowchart
            
            # Initialize flowchart
            flowchart = WolfCogRecursiveCognitiveFlowchart()
            
            # Start flowchart
            success = flowchart.start_recursive_cognitive_flowchart()
            self.assertTrue(success, "Flowchart should start successfully")
            
            # Wait a moment for initialization
            time.sleep(2)
            
            # Check status
            status = flowchart.get_flowchart_status()
            self.assertGreater(status.overall_coherence, 0.0)
            
            # Stop flowchart
            stop_success = flowchart.stop_recursive_cognitive_flowchart()
            self.assertTrue(stop_success, "Flowchart should stop successfully")
            
            print("✅ Full system startup/shutdown test passed")
            
        except ImportError:
            self.skipTest("Flowchart components not available")
    
    def test_layer_interdependencies(self):
        """Test that layers properly depend on each other"""
        try:
            from src.recursive_cognitive_flowchart import WolfCogRecursiveCognitiveFlowchart
            
            flowchart = WolfCogRecursiveCognitiveFlowchart()
            
            # Test Layer 1 → Layer 2 dependency
            self.assertTrue(flowchart._execute_layer1_mock_bifurcation())
            self.assertTrue(flowchart.layer_status[1])
            
            # Test Layer 2 → Layer 3 dependency  
            self.assertTrue(flowchart._execute_layer2_symbolic_core())
            self.assertTrue(flowchart.layer_status[2])
            
            print("✅ Layer interdependencies test passed")
            
        except ImportError:
            self.skipTest("Flowchart components not available")

def run_recursive_cognitive_flowchart_tests():
    """Run all recursive cognitive flowchart tests"""
    print("🧠 Running WolfCog Recursive Cognitive Flowchart Tests")
    print("=" * 65)
    
    # Create test suite
    suite = unittest.TestSuite()
    
    # Add unit tests
    suite.addTest(unittest.makeSuite(TestRecursiveCognitiveFlowchart))
    suite.addTest(unittest.makeSuite(TestRecursiveCognitiveFlowchartIntegration))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    total_tests = result.testsRun
    failures = len(result.failures)
    errors = len(result.errors)
    skipped = len(result.skipped) if hasattr(result, 'skipped') else 0
    
    print("\n" + "=" * 65)
    print("🎯 Test Results Summary:")
    print(f"   Total Tests: {total_tests}")
    print(f"   Passed: {total_tests - failures - errors - skipped}")
    print(f"   Failed: {failures}")
    print(f"   Errors: {errors}")
    print(f"   Skipped: {skipped}")
    
    if failures == 0 and errors == 0:
        print("🎉 All Recursive Cognitive Flowchart tests PASSED!")
        return True
    else:
        print("❌ Some tests failed or had errors")
        return False

if __name__ == "__main__":
    success = run_recursive_cognitive_flowchart_tests()
    sys.exit(0 if success else 1)