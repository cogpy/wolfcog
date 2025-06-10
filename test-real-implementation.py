#!/usr/bin/env python3
"""
Test Real WolfCog Implementation
Comprehensive test of the actual implementation components
"""

import sys
import time
import subprocess
from pathlib import Path

def test_real_components():
    """Test the real implementation components"""
    print("🧪 Testing Real WolfCog Implementation Components")
    print("=" * 60)
    
    results = {}
    
    # Test 1: Symbolic Processor
    print("\n1. Testing Symbolic Processor...")
    try:
        result = subprocess.run([
            sys.executable, "src/symbolic_processor.py"
        ], capture_output=True, text=True, timeout=30)
        
        if result.returncode == 0:
            print("  ✅ Symbolic processor working")
            results["symbolic_processor"] = True
        else:
            print(f"  ❌ Symbolic processor failed: {result.stderr}")
            results["symbolic_processor"] = False
    except Exception as e:
        print(f"  ❌ Symbolic processor test error: {e}")
        results["symbolic_processor"] = False
    
    # Test 2: Task Manager
    print("\n2. Testing Task Manager...")
    try:
        result = subprocess.run([
            sys.executable, "src/task_manager.py"
        ], capture_output=True, text=True, timeout=30)
        
        if result.returncode == 0:
            print("  ✅ Task manager working")
            results["task_manager"] = True
        else:
            print(f"  ❌ Task manager failed: {result.stderr}")
            results["task_manager"] = False
    except Exception as e:
        print(f"  ❌ Task manager test error: {e}")
        results["task_manager"] = False
    
    # Test 3: Agent Coordinator
    print("\n3. Testing Agent Coordinator...")
    try:
        result = subprocess.run([
            sys.executable, "src/agent_coordinator.py"
        ], capture_output=True, text=True, timeout=30)
        
        if result.returncode == 0:
            print("  ✅ Agent coordinator working")
            results["agent_coordinator"] = True
        else:
            print(f"  ❌ Agent coordinator failed: {result.stderr}")
            results["agent_coordinator"] = False
    except Exception as e:
        print(f"  ❌ Agent coordinator test error: {e}")
        results["agent_coordinator"] = False
    
    # Test 4: Real Coordinator (quick test)
    print("\n4. Testing Real Coordinator (quick start/stop)...")
    try:
        # Import and test basic functionality
        sys.path.append('.')
        
        # Import with proper module name
        import importlib.util
        spec = importlib.util.spec_from_file_location("wolfcog_coordinator_real", "wolfcog-coordinator-real.py")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        
        coordinator = module.RealWolfCogCoordinator()
        
        # Test initialization without starting full system
        coordinator.create_symbolic_space_simulation()
        status = coordinator.get_system_status()
        
        if isinstance(status, dict) and "timestamp" in status:
            print("  ✅ Real coordinator basic functionality working")
            results["real_coordinator"] = True
        else:
            print(f"  ❌ Real coordinator status check failed: {status}")
            results["real_coordinator"] = False
            
    except Exception as e:
        print(f"  ❌ Real coordinator test error: {e}")
        results["real_coordinator"] = False
    
    # Test 5: Integration Test
    print("\n5. Testing Component Integration...")
    try:
        # Test importing all components together
        from src.symbolic_processor import RealSymbolicProcessor
        from src.task_manager import RealTaskManager
        from src.agent_coordinator import RealAgentCoordinator
        
        # Quick instantiation test
        processor = RealSymbolicProcessor()
        manager = RealTaskManager(max_workers=1)
        coordinator = RealAgentCoordinator()
        
        print("  ✅ All components can be imported and instantiated")
        results["integration"] = True
        
    except Exception as e:
        print(f"  ❌ Integration test error: {e}")
        results["integration"] = False
    
    # Summary
    print(f"\n📊 Test Results Summary:")
    total_tests = len(results)
    passed_tests = sum(results.values())
    
    for component, passed in results.items():
        status = "✅ PASS" if passed else "❌ FAIL"
        print(f"  {component}: {status}")
    
    print(f"\nOverall: {passed_tests}/{total_tests} tests passed")
    
    if passed_tests == total_tests:
        print("🎉 All real implementation components are working!")
        return True
    else:
        print("⚠️ Some components need attention")
        return False


def test_real_coordinator_startup():
    """Test full coordinator startup"""
    print("\n🚀 Testing Real Coordinator Full Startup...")
    
    try:
        # Start coordinator in separate process
        process = subprocess.Popen([
            sys.executable, "wolfcog-coordinator-real.py"
        ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        
        # Let it run for a few seconds
        time.sleep(5)
        
        # Check if still running
        if process.poll() is None:
            print("✅ Real coordinator started successfully")
            
            # Terminate cleanly
            process.terminate()
            process.wait(timeout=5)
            print("✅ Real coordinator stopped cleanly")
            return True
        else:
            stdout, stderr = process.communicate()
            print(f"❌ Real coordinator failed to start:")
            print(f"  stdout: {stdout.decode()}")
            print(f"  stderr: {stderr.decode()}")
            return False
            
    except Exception as e:
        print(f"❌ Coordinator startup test error: {e}")
        return False


def check_dependencies():
    """Check for required dependencies"""
    print("🔍 Checking Dependencies...")
    
    dependencies = {
        "python": True,  # Already running
        "guile": False,
        "opencog": False
    }
    
    # Check Guile
    try:
        result = subprocess.run(["guile", "--version"], 
                              capture_output=True, text=True, timeout=5)
        if result.returncode == 0:
            dependencies["guile"] = True
            print(f"  ✅ Guile: {result.stdout.split()[2]}")
        else:
            print("  ❌ Guile not found")
    except:
        print("  ❌ Guile not available")
    
    # Check OpenCog
    try:
        import opencog
        dependencies["opencog"] = True
        print("  ✅ OpenCog Python bindings available")
    except ImportError:
        print("  ⚠️ OpenCog not available (will use simulation)")
    
    return dependencies


def main():
    """Main test function"""
    print("🐺 WolfCog Real Implementation Test Suite")
    print("=" * 60)
    
    # Check dependencies
    deps = check_dependencies()
    
    # Test components
    component_tests_passed = test_real_components()
    
    # Test full startup if components pass
    if component_tests_passed:
        startup_test_passed = test_real_coordinator_startup()
    else:
        startup_test_passed = False
    
    # Final summary
    print(f"\n{'='*60}")
    print("🎯 FINAL TEST SUMMARY")
    print(f"{'='*60}")
    
    if component_tests_passed and startup_test_passed:
        print("🎉 SUCCESS: Real WolfCog implementation is working!")
        print("✅ All core components are functional")
        print("✅ Full system startup/shutdown works")
        
        if deps["guile"] and deps["opencog"]:
            print("✅ Full symbolic processing capabilities available")
        else:
            print("⚠️ Running with simulation mode (OpenCog/Guile not fully available)")
            
        print("\n🚀 Ready to run: python wolfcog-coordinator-real.py")
        return True
        
    else:
        print("❌ FAILURE: Implementation needs work")
        if not component_tests_passed:
            print("❌ Component tests failed")
        if not startup_test_passed:
            print("❌ Startup test failed")
        
        print("\n🔧 Check the error messages above and fix issues")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
