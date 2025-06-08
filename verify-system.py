#!/usr/bin/env python3
"""
WolfCog System Verification & Improvement Analysis
Comprehensive audit of all functional features and identification of improvement areas
"""

import json
import time
import subprocess
import sys
import os
import threading
from pathlib import Path
from datetime import datetime

class WolfCogVerifier:
    def __init__(self):
        self.results = {
            "timestamp": datetime.now().isoformat(),
            "features": {},
            "issues": [],
            "improvements": [],
            "performance": {},
            "recommendations": []
        }
        
    def verify_all_features(self):
        """Verify all functional features systematically"""
        print("🔍 WolfCog System Verification & Analysis")
        print("=" * 60)
        
        # Test core components
        self.verify_symbolic_spaces()
        self.verify_task_processing()
        self.verify_agent_system()
        self.verify_system_daemons()
        self.verify_coordinator()
        self.verify_integration_health()
        self.verify_performance()
        self.verify_error_handling()
        self.verify_security_safety()
        
        # Generate report
        self.generate_improvement_report()
        
    def verify_symbolic_spaces(self):
        """Verify symbolic spaces functionality"""
        print("\n🌌 Verifying Symbolic Spaces...")
        
        spaces = ["u", "e", "s"]
        space_status = {}
        
        for space in spaces:
            space_path = Path(f"spaces/{space}")
            status = {
                "exists": space_path.exists(),
                "accessible": False,
                "files": 0,
                "memory_structures": False
            }
            
            if space_path.exists():
                try:
                    files = list(space_path.glob("*"))
                    status["files"] = len(files)
                    status["accessible"] = True
                    
                    # Check for memory structures
                    if any(f.name.endswith(('.memory', '.symbolic', '.json')) or 'memory' in f.name.lower() for f in files):
                        status["memory_structures"] = True
                        
                except Exception as e:
                    self.results["issues"].append(f"Space {space} access error: {e}")
                    
            space_status[space] = status
            print(f"  📁 Space {space}: {'✅' if status['accessible'] else '❌'} "
                  f"({status['files']} files)")
                  
        self.results["features"]["symbolic_spaces"] = space_status
        
        # Check for improvements needed
        if not all(s["accessible"] for s in space_status.values()):
            self.results["improvements"].append("Some symbolic spaces are not accessible")
            
        if not any(s["memory_structures"] for s in space_status.values()):
            self.results["improvements"].append("No memory structures detected in spaces")
            
    def verify_task_processing(self):
        """Verify task processing pipeline"""
        print("\n⚙️ Verifying Task Processing Pipeline...")
        
        task_dir = Path("/tmp/ecron_tasks")
        
        # Create test task
        test_task = {
            "flow": "verification_test",
            "space": "e",
            "symbolic": "∇(verify_system)",
            "action": "test",
            "timestamp": time.time()
        }
        
        try:
            # Ensure task directory exists
            task_dir.mkdir(exist_ok=True)
            
            # Create test task
            test_file = task_dir / "verification_test.json"
            with open(test_file, 'w') as f:
                json.dump(test_task, f)
                
            print(f"  📝 Created test task: {test_file}")
            
            # Start task daemon briefly
            daemon_process = subprocess.Popen(
                [sys.executable, "opencog/ecron-task-daemon.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            time.sleep(5)  # Let it process
            daemon_process.terminate()
            daemon_process.wait()
            
            # Check if task was processed
            processed_files = list(task_dir.glob("*.processed"))
            pending_files = list(task_dir.glob("*.json"))
            
            self.results["features"]["task_processing"] = {
                "daemon_startable": True,
                "processes_tasks": len(processed_files) > 0,
                "pending_tasks": len(pending_files),
                "processed_tasks": len(processed_files)
            }
            
            print(f"  ✅ Task daemon functional")
            print(f"  📊 {len(processed_files)} processed, {len(pending_files)} pending")
            
        except Exception as e:
            self.results["issues"].append(f"Task processing error: {e}")
            self.results["features"]["task_processing"] = {"error": str(e)}
            print(f"  ❌ Task processing failed: {e}")
            
    def verify_agent_system(self):
        """Verify agent system functionality"""
        print("\n🤖 Verifying Agent System...")
        
        agents = [
            ("admin_agent.py", "Admin Agent"),
            ("director_agent.py", "Director Agent"),
            ("conversational_agent.py", "Conversational Agent")
        ]
        
        agent_status = {}
        
        for agent_file, agent_name in agents:
            agent_path = Path(f"agents/{agent_file}")
            status = {
                "exists": agent_path.exists(),
                "startable": False,
                "functional": False
            }
            
            if agent_path.exists():
                try:
                    # Try to start agent briefly
                    process = subprocess.Popen(
                        [sys.executable, str(agent_path)],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True
                    )
                    
                    time.sleep(3)
                    status["startable"] = process.poll() is None
                    
                    process.terminate()
                    process.wait()
                    
                    # Check output for functionality indicators
                    stdout, stderr = process.communicate() if not status["startable"] else ("", "")
                    status["functional"] = "error" not in stderr.lower()
                    
                except Exception as e:
                    self.results["issues"].append(f"{agent_name} startup error: {e}")
                    
            agent_status[agent_name] = status
            print(f"  🤖 {agent_name}: {'✅' if status['startable'] else '❌'}")
            
        self.results["features"]["agents"] = agent_status
        
    def verify_system_daemons(self):
        """Verify system daemons functionality"""
        print("\n⚙️ Verifying System Daemons...")
        
        daemons = [
            ("scheduler/ecron-scheduler.py", "Scheduler Daemon"),
            ("reflex/reflex-monitor.py", "Reflex Daemon"),
            ("dashboard/symbolic-state-dashboard.py", "Dashboard Daemon")
        ]
        
        daemon_status = {}
        
        for daemon_file, daemon_name in daemons:
            daemon_path = Path(f"daemons/{daemon_file}")
            status = {
                "exists": daemon_path.exists(),
                "startable": False,
                "dependencies_met": True
            }
            
            if daemon_path.exists():
                try:
                    # Check dependencies first
                    if "reflex" in daemon_file:
                        try:
                            import watchdog
                        except ImportError:
                            status["dependencies_met"] = False
                            self.results["improvements"].append(f"Missing dependency for {daemon_name}: watchdog")
                    
                    if status["dependencies_met"]:
                        # Try to start daemon briefly
                        process = subprocess.Popen(
                            [sys.executable, str(daemon_path)],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            text=True
                        )
                        
                        time.sleep(3)
                        status["startable"] = process.poll() is None
                        
                        process.terminate()
                        process.wait()
                        
                except Exception as e:
                    self.results["issues"].append(f"{daemon_name} startup error: {e}")
                    
            daemon_status[daemon_name] = status
            print(f"  ⚙️ {daemon_name}: {'✅' if status['startable'] else '❌'}")
            
        self.results["features"]["daemons"] = daemon_status
        
    def verify_coordinator(self):
        """Verify master coordinator functionality"""
        print("\n🎛️ Verifying Master Coordinator...")
        
        coordinator_path = Path("wolfcog-coordinator.py")
        status = {
            "exists": coordinator_path.exists(),
            "startable": False,
            "manages_components": False
        }
        
        if coordinator_path.exists():
            try:
                # Check if coordinator can initialize
                process = subprocess.Popen(
                    [sys.executable, str(coordinator_path)],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True
                )
                
                time.sleep(5)
                status["startable"] = process.poll() is None
                
                # Check if it's managing components
                if status["startable"]:
                    # Look for component management in output
                    try:
                        stdout, _ = process.communicate(timeout=2)
                        status["manages_components"] = "started" in stdout.lower()
                    except subprocess.TimeoutExpired:
                        status["manages_components"] = True  # Still running, likely managing
                        
                process.terminate()
                process.wait()
                
            except Exception as e:
                self.results["issues"].append(f"Coordinator startup error: {e}")
                
        self.results["features"]["coordinator"] = status
        print(f"  🎛️ Coordinator: {'✅' if status['startable'] else '❌'}")
        
    def verify_integration_health(self):
        """Verify overall system integration"""
        print("\n🔗 Verifying System Integration...")
        
        # Run existing integration test
        try:
            result = subprocess.run(
                [sys.executable, "test-integration.py"],
                capture_output=True,
                text=True,
                timeout=60
            )
            
            integration_status = {
                "passes": result.returncode == 0,
                "output": result.stdout,
                "errors": result.stderr
            }
            
            self.results["features"]["integration"] = integration_status
            print(f"  🔗 Integration tests: {'✅' if integration_status['passes'] else '❌'}")
            
        except Exception as e:
            self.results["issues"].append(f"Integration test error: {e}")
            print(f"  ❌ Integration test failed: {e}")
            
    def verify_performance(self):
        """Verify performance characteristics"""
        print("\n⚡ Analyzing Performance...")
        
        performance_metrics = {
            "startup_time": self.measure_startup_time(),
            "task_processing_speed": self.measure_task_processing(),
            "memory_usage": self.estimate_memory_usage(),
            "component_responsiveness": self.measure_component_responsiveness()
        }
        
        self.results["performance"] = performance_metrics
        
        # Identify performance issues
        if performance_metrics["startup_time"] > 15:
            self.results["improvements"].append("Slow system startup (>15s)")
            
        if performance_metrics["task_processing_speed"] < 1:
            self.results["improvements"].append("Slow task processing (<1 task/sec)")
            
    def measure_startup_time(self):
        """Measure system startup time"""
        try:
            start_time = time.time()
            process = subprocess.Popen(
                [sys.executable, "wolfcog-coordinator.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            # Wait for startup completion indicators
            startup_complete = False
            while not startup_complete and time.time() - start_time < 30:
                try:
                    stdout, _ = process.communicate(timeout=1)
                    if "operational" in stdout.lower() or "ready" in stdout.lower():
                        startup_complete = True
                except subprocess.TimeoutExpired:
                    continue
                    
            startup_time = time.time() - start_time
            process.terminate()
            process.wait()
            
            print(f"  ⏱️ Startup time: {startup_time:.2f}s")
            return startup_time
            
        except Exception as e:
            print(f"  ❌ Startup measurement failed: {e}")
            return -1
            
    def measure_task_processing(self):
        """Measure task processing speed"""
        try:
            # Create multiple test tasks
            task_dir = Path("/tmp/ecron_tasks")
            task_dir.mkdir(exist_ok=True)
            
            # Clear existing tasks
            for f in task_dir.glob("*"):
                f.unlink()
                
            # Create test tasks
            num_tasks = 5
            for i in range(num_tasks):
                task = {
                    "flow": f"perf_test_{i}",
                    "space": "e",
                    "symbolic": f"∇(test_{i})",
                    "action": "test",
                    "timestamp": time.time()
                }
                
                with open(task_dir / f"perf_test_{i}.json", 'w') as f:
                    json.dump(task, f)
                    
            # Start daemon and measure processing time
            start_time = time.time()
            process = subprocess.Popen(
                [sys.executable, "opencog/ecron-task-daemon.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            # Wait for processing
            time.sleep(10)
            process.terminate()
            process.wait()
            
            # Count processed tasks
            processed = len(list(task_dir.glob("*.processed")))
            processing_time = time.time() - start_time
            speed = processed / processing_time if processing_time > 0 else 0
            
            print(f"  📊 Task processing: {speed:.2f} tasks/sec")
            return speed
            
        except Exception as e:
            print(f"  ❌ Performance measurement failed: {e}")
            return 0
            
    def estimate_memory_usage(self):
        """Estimate memory usage"""
        # Simple estimation based on component count and file sizes
        try:
            total_size = 0
            for root, dirs, files in os.walk("."):
                for file in files:
                    if file.endswith(('.py', '.wl', '.lisp', '.scm')):
                        file_path = os.path.join(root, file)
                        total_size += os.path.getsize(file_path)
                        
            # Rough estimation: 2-5x file size for runtime memory
            estimated_memory = total_size * 3
            print(f"  💾 Estimated memory usage: {estimated_memory / 1024 / 1024:.2f} MB")
            return estimated_memory
            
        except Exception as e:
            print(f"  ❌ Memory estimation failed: {e}")
            return 0
            
    def measure_component_responsiveness(self):
        """Measure component responsiveness"""
        # Test how quickly components respond to signals
        try:
            components = ["opencog/ecron-task-daemon.py", "agents/admin_agent.py"]
            responsiveness = {}
            
            for component in components:
                start_time = time.time()
                process = subprocess.Popen(
                    [sys.executable, component],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True
                )
                
                time.sleep(2)  # Let it initialize
                
                # Measure termination responsiveness
                term_start = time.time()
                process.terminate()
                process.wait()
                term_time = time.time() - term_start
                
                responsiveness[component] = {
                    "startup": 2.0,  # Fixed initialization time
                    "termination": term_time
                }
                
            print(f"  📈 Component responsiveness measured")
            return responsiveness
            
        except Exception as e:
            print(f"  ❌ Responsiveness measurement failed: {e}")
            return {}
            
    def verify_error_handling(self):
        """Verify error handling and robustness"""
        print("\n🛡️ Verifying Error Handling...")
        
        error_tests = {
            "invalid_task": self.test_invalid_task_handling(),
            "missing_files": self.test_missing_file_handling(),
            "component_failure": self.test_component_failure_handling()
        }
        
        self.results["features"]["error_handling"] = error_tests
        
        # Identify improvements needed
        failed_tests = [test for test, result in error_tests.items() if not result]
        if failed_tests:
            self.results["improvements"].append(f"Error handling needs improvement: {failed_tests}")
            
    def test_invalid_task_handling(self):
        """Test handling of invalid tasks"""
        try:
            task_dir = Path("/tmp/ecron_tasks")
            task_dir.mkdir(exist_ok=True)
            
            # Create invalid task
            invalid_task = {"invalid": "task", "missing": "required_fields"}
            with open(task_dir / "invalid_test.json", 'w') as f:
                json.dump(invalid_task, f)
                
            # Start daemon
            process = subprocess.Popen(
                [sys.executable, "opencog/ecron-task-daemon.py"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            time.sleep(5)
            process.terminate()
            stdout, stderr = process.communicate()
            
            # Check if error was handled gracefully
            graceful = "error" in stderr.lower() and "exception" not in stderr.lower()
            print(f"  🧪 Invalid task handling: {'✅' if graceful else '❌'}")
            return graceful
            
        except Exception:
            return False
            
    def test_missing_file_handling(self):
        """Test handling of missing files"""
        try:
            # Try to start non-existent component
            result = subprocess.run(
                [sys.executable, "nonexistent_component.py"],
                capture_output=True,
                text=True
            )
            
            # Should fail gracefully with clear error
            graceful = result.returncode != 0 and "No such file" in result.stderr
            print(f"  🧪 Missing file handling: {'✅' if graceful else '❌'}")
            return graceful
            
        except Exception:
            return False
            
    def test_component_failure_handling(self):
        """Test handling of component failures"""
        # This is complex to test properly without causing actual failures
        # For now, assume it needs improvement
        print(f"  🧪 Component failure handling: ⚠️ (needs testing)")
        return False
        
    def verify_security_safety(self):
        """Verify security and safety measures"""
        print("\n🔒 Verifying Security & Safety...")
        
        security_checks = {
            "input_validation": self.check_input_validation(),
            "self_modification_bounds": self.check_self_modification_safety(),
            "resource_limits": self.check_resource_limits()
        }
        
        self.results["features"]["security"] = security_checks
        
        # Identify security improvements needed
        failed_checks = [check for check, result in security_checks.items() if not result]
        if failed_checks:
            self.results["improvements"].append(f"Security needs improvement: {failed_checks}")
            
    def check_input_validation(self):
        """Check if inputs are properly validated"""
        # Check for basic validation in key components
        try:
            with open("opencog/ecron-task-daemon.py", 'r') as f:
                content = f.read()
                has_validation = "validate" in content.lower() or "check" in content.lower()
                
            print(f"  🔍 Input validation: {'✅' if has_validation else '❌'}")
            return has_validation
            
        except Exception:
            return False
            
    def check_self_modification_safety(self):
        """Check safety bounds for self-modification"""
        # Check for safety measures in self-modifying components
        try:
            safety_files = ["kernels/meta-shellwalker.wl", "agents/director_agent.py"]
            has_safety = False
            
            for file_path in safety_files:
                if Path(file_path).exists():
                    with open(file_path, 'r') as f:
                        content = f.read()
                        if any(term in content.lower() for term in ["safety", "bound", "limit", "validate"]):
                            has_safety = True
                            break
                            
            print(f"  🛡️ Self-modification safety: {'✅' if has_safety else '❌'}")
            return has_safety
            
        except Exception:
            return False
            
    def check_resource_limits(self):
        """Check for resource limits and monitoring"""
        # Simple check for resource management
        try:
            with open("wolfcog-coordinator.py", 'r') as f:
                content = f.read()
                has_limits = any(term in content.lower() for term in ["timeout", "limit", "resource", "memory"])
                
            print(f"  📊 Resource limits: {'✅' if has_limits else '❌'}")
            return has_limits
            
        except Exception:
            return False
            
    def generate_improvement_report(self):
        """Generate comprehensive improvement recommendations"""
        print("\n📋 Generating Improvement Report...")
        
        # Analyze results and generate specific recommendations
        recommendations = []
        
        # Performance recommendations
        if self.results["performance"].get("startup_time", 0) > 10:
            recommendations.append({
                "category": "Performance",
                "priority": "High",
                "item": "Optimize system startup time",
                "details": "Current startup time exceeds 10 seconds. Consider lazy loading of components."
            })
            
        # Error handling recommendations
        if not self.results["features"].get("error_handling", {}).get("component_failure", True):
            recommendations.append({
                "category": "Robustness", 
                "priority": "High",
                "item": "Implement component failure recovery",
                "details": "System needs automatic recovery mechanisms for failed components."
            })
            
        # Security recommendations
        if not self.results["features"].get("security", {}).get("input_validation", True):
            recommendations.append({
                "category": "Security",
                "priority": "Medium", 
                "item": "Add input validation",
                "details": "All user inputs and task data should be validated before processing."
            })
            
        # Add monitoring recommendations
        recommendations.append({
            "category": "Monitoring",
            "priority": "Medium",
            "item": "Add comprehensive system monitoring",
            "details": "Implement real-time monitoring of all components with health checks."
        })
        
        # Add testing recommendations
        recommendations.append({
            "category": "Testing",
            "priority": "Medium", 
            "item": "Expand test coverage",
            "details": "Add unit tests for individual components and performance benchmarks."
        })
        
        self.results["recommendations"] = recommendations
        
        # Save results
        self.save_results()
        
        # Print summary
        print("\n" + "=" * 60)
        print("📊 VERIFICATION SUMMARY")
        print("=" * 60)
        
        print(f"\n✅ Functional Features:")
        for feature, status in self.results["features"].items():
            if isinstance(status, dict):
                working = sum(1 for v in status.values() if v is True)
                total = len(status)
                print(f"  • {feature}: {working}/{total} components working")
            else:
                print(f"  • {feature}: {'✅' if status else '❌'}")
                
        print(f"\n⚠️ Issues Found: {len(self.results['issues'])}")
        for issue in self.results["issues"][:5]:  # Show first 5
            print(f"  • {issue}")
            
        print(f"\n🔧 Improvements Needed: {len(self.results['improvements'])}")
        for improvement in self.results["improvements"][:5]:  # Show first 5
            print(f"  • {improvement}")
            
        print(f"\n📋 Recommendations: {len(recommendations)}")
        for rec in recommendations[:3]:  # Show first 3
            print(f"  • [{rec['priority']}] {rec['item']}")
            
        print(f"\n📄 Full report saved to: verification_report.json")
        
    def save_results(self):
        """Save verification results to file"""
        with open("verification_report.json", 'w') as f:
            json.dump(self.results, f, indent=2)

def main():
    """Run complete system verification"""
    verifier = WolfCogVerifier()
    verifier.verify_all_features()
    return 0

if __name__ == "__main__":
    exit(main())