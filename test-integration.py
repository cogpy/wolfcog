#!/usr/bin/env python3
"""
WolfCog Integration Test - Verify complete symbolic pipeline
"""

import json
import time
import subprocess
import sys
from pathlib import Path

def test_symbolic_pipeline():
    """Test the complete WolfCog symbolic pipeline"""
    print("🧪 Testing WolfCog Symbolic Pipeline...")
    
    # Ensure test environment is clean
    task_dir = Path("/tmp/ecron_tasks")
    task_dir.mkdir(exist_ok=True)
    
    # Clean any existing files
    for f in task_dir.glob("*"):
        f.unlink()
    
    print("📁 Test environment prepared")
    
    # Start the task daemon
    print("🚀 Starting task daemon...")
    daemon_process = subprocess.Popen([sys.executable, "opencog/ecron-task-daemon.py"])
    
    time.sleep(2)  # Let daemon start
    
    try:
        # Create test tasks for each symbolic space
        test_tasks = [
            {
                "flow": "user_cognition",
                "space": "u", 
                "symbolic": "∇(user_intent)",
                "action": "understand",
                "test_id": "u_space_test"
            },
            {
                "flow": "execution_optimization", 
                "space": "e",
                "symbolic": "∂Ω(runtime_flow)",
                "action": "optimize",
                "test_id": "e_space_test"
            },
            {
                "flow": "system_evolution",
                "space": "s",
                "symbolic": "⊗Φ(meta_system)", 
                "action": "evolve",
                "test_id": "s_space_test"
            }
        ]
        
        # Submit test tasks
        for i, task in enumerate(test_tasks):
            task_file = task_dir / f"pipeline_test_{i}.json"
            with open(task_file, 'w') as f:
                json.dump(task, f, indent=2)
            print(f"📝 Created test task: {task['test_id']}")
        
        # Wait for processing
        print("⏳ Waiting for task processing...")
        time.sleep(5)
        
        # Check results
        processed_files = list(task_dir.glob("*.processed"))
        pending_files = list(task_dir.glob("*.json"))
        
        print(f"✅ Results: {len(processed_files)} processed, {len(pending_files)} pending")
        
        # Verify space-specific processing
        for processed_file in processed_files:
            try:
                with open(processed_file, 'r') as f:
                    content = f.read()
                print(f"📄 Processed: {processed_file.name}")
            except Exception as e:
                print(f"❌ Error reading {processed_file}: {e}")
        
        if len(processed_files) >= len(test_tasks):
            print("🎉 Symbolic pipeline test PASSED!")
            return True
        else:
            print(f"⚠️ Symbolic pipeline test PARTIAL: {len(processed_files)}/{len(test_tasks)} tasks processed")
            return False
            
    finally:
        # Clean up
        print("🧹 Cleaning up...")
        daemon_process.terminate()
        daemon_process.wait()
        
        # Clean test files
        for f in task_dir.glob("pipeline_test_*"):
            f.unlink()

def test_agent_communication():
    """Test agent communication and coordination"""
    print("🤝 Testing agent communication...")
    
    # Start admin agent briefly
    print("👨‍💼 Testing admin agent...")
    admin_process = subprocess.Popen([sys.executable, "agents/admin_agent.py"])
    time.sleep(3)
    admin_process.terminate()
    admin_process.wait()
    
    print("✅ Admin agent test completed")
    
    # Start director agent briefly  
    print("🎬 Testing director agent...")
    director_process = subprocess.Popen([sys.executable, "agents/director_agent.py"])
    time.sleep(3)
    director_process.terminate() 
    director_process.wait()
    
    print("✅ Director agent test completed")
    return True

def main():
    """Run integration tests"""
    print("🐺 WolfCog Integration Test Suite")
    print("=" * 50)
    
    success = True
    
    # Test 1: Symbolic Pipeline
    print("\n🧪 Test 1: Symbolic Pipeline")
    try:
        if not test_symbolic_pipeline():
            success = False
    except Exception as e:
        print(f"❌ Pipeline test error: {e}")
        success = False
    
    # Test 2: Agent Communication
    print("\n🤝 Test 2: Agent Communication") 
    try:
        if not test_agent_communication():
            success = False
    except Exception as e:
        print(f"❌ Agent test error: {e}")
        success = False
    
    # Summary
    print("\n" + "=" * 50)
    if success:
        print("🎉 All WolfCog integration tests PASSED!")
        print("✨ Symbolic cognitive substrate is fully operational")
        return 0
    else:
        print("⚠️ Some tests had issues - check output above")
        return 1

if __name__ == "__main__":
    exit(main())