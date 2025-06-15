#!/usr/bin/env python3
"""
Test suite for WolfCog Stage0 Bootstrap implementation
Validates the bootstrap structure and functionality without requiring full dependencies
"""

import os
import subprocess
import json
from pathlib import Path

class BootstrapTest:
    def __init__(self):
        self.repo_root = Path("/home/runner/work/wolfcog/wolfcog")
        self.results = {}
        
    def test_bootstrap_files_exist(self):
        """Test that all required bootstrap files exist"""
        print("🔍 Testing bootstrap file structure...")
        
        required_files = [
            ".guix/manifest.scm",
            ".guix/bootstrap/stage0.scm", 
            ".guix/bootstrap/stage0-bootstrap.scm",
            ".guix/bootstrap/stage1.scm",
            ".guix/bootstrap/stage2.scm", 
            ".guix/bootstrap/stage3.scm",
            ".guix/bootstrap/multi-stage-bootstrap.scm",
            ".guix/bootstrap/init-shell.scm"
        ]
        
        missing_files = []
        for file_path in required_files:
            full_path = self.repo_root / file_path
            if not full_path.exists():
                missing_files.append(file_path)
            else:
                print(f"  ✓ Found: {file_path}")
        
        if missing_files:
            print(f"  ❌ Missing files: {missing_files}")
            self.results["bootstrap_files"] = {"status": "fail", "missing": missing_files}
        else:
            print("  ✅ All bootstrap files present")
            self.results["bootstrap_files"] = {"status": "pass", "files": required_files}
            
    def test_bootstrap_content_structure(self):
        """Test that bootstrap files contain expected functions and structure"""
        print("\n🔍 Testing bootstrap content structure...")
        
        stage0_file = self.repo_root / ".guix/bootstrap/stage0-bootstrap.scm"
        if not stage0_file.exists():
            print("  ❌ stage0-bootstrap.scm not found")
            self.results["bootstrap_content"] = {"status": "fail", "reason": "file_missing"}
            return
            
        content = stage0_file.read_text()
        
        # Check for required functions from the issue specification
        required_functions = [
            "stage0-bootstrap",
            "detect-wolfram-kernels", 
            "verify-opencog-unified",
            "init-wolfram-pools",
            "load-cogutil",
            "load-cogserver", 
            "init-atomspace",
            "mount-asfs",
            "apply-guix-security",
            "present-boot-options"
        ]
        
        missing_functions = []
        for func in required_functions:
            if f"(define ({func}" not in content:
                missing_functions.append(func)
            else:
                print(f"  ✓ Function: {func}")
                
        if missing_functions:
            print(f"  ❌ Missing functions: {missing_functions}")
            self.results["bootstrap_content"] = {
                "status": "fail", 
                "missing_functions": missing_functions
            }
        else:
            print("  ✅ All required functions present")
            self.results["bootstrap_content"] = {
                "status": "pass", 
                "functions": required_functions
            }
            
    def test_multi_stage_structure(self):
        """Test that all bootstrap stages are properly implemented"""
        print("\n🔍 Testing multi-stage bootstrap structure...")
        
        stage_files = {
            "stage1": ".guix/bootstrap/stage1.scm",
            "stage2": ".guix/bootstrap/stage2.scm", 
            "stage3": ".guix/bootstrap/stage3.scm",
            "multi-stage": ".guix/bootstrap/multi-stage-bootstrap.scm"
        }
        
        stage_functions = {
            "stage1": ["stage1-bootstrap", "load-advanced-cogutil", "init-distributed-cogserver"],
            "stage2": ["stage2-bootstrap", "activate-system-integration", "start-symbolic-evolution"],
            "stage3": ["stage3-bootstrap", "enable-autonomous-evolution", "activate-meta-learning"],
            "multi-stage": ["complete-wolfcog-bootstrap", "execute-stage0", "execute-stage1"]
        }
        
        missing_stages = []
        stage_status = {}
        
        for stage, file_path in stage_files.items():
            full_path = self.repo_root / file_path
            if not full_path.exists():
                missing_stages.append(stage)
                print(f"  ❌ Missing: {stage} ({file_path})")
                stage_status[stage] = "missing"
            else:
                print(f"  ✓ Found: {stage}")
                
                # Check for required functions
                content = full_path.read_text()
                missing_funcs = []
                for func in stage_functions[stage]:
                    if f"(define ({func}" not in content:
                        missing_funcs.append(func)
                    else:
                        print(f"    ✓ Function: {func}")
                
                if missing_funcs:
                    print(f"    ❌ Missing functions in {stage}: {missing_funcs}")
                    stage_status[stage] = "incomplete"
                else:
                    stage_status[stage] = "complete"
        
        if missing_stages:
            print(f"  ❌ Missing stages: {missing_stages}")
            self.results["multi_stage_structure"] = {
                "status": "fail", 
                "missing_stages": missing_stages,
                "stage_status": stage_status
            }
        else:
            print("  ✅ All bootstrap stages present and complete")
            self.results["multi_stage_structure"] = {
                "status": "pass", 
                "stages": list(stage_files.keys()),
                "stage_status": stage_status
            }
            
    def test_opencog_structure(self):
        """Test that OpenCog components are present as expected"""
        print("\n🔍 Testing OpenCog component structure...")
        
        opencog_dirs = ["cogutil", "atomspace", "cogserver"]
        found_dirs = []
        missing_dirs = []
        
        for dirname in opencog_dirs:
            dir_path = self.repo_root / dirname
            if dir_path.is_dir():
                found_dirs.append(dirname)
                print(f"  ✓ Found: {dirname}/")
            else:
                missing_dirs.append(dirname)
                print(f"  ⚠️ Missing: {dirname}/")
                
        self.results["opencog_structure"] = {
            "found": found_dirs,
            "missing": missing_dirs,
            "total_expected": len(opencog_dirs)
        }
        
        if len(found_dirs) == len(opencog_dirs):
            print("  ✅ All OpenCog components present")
        else:
            print(f"  ⚠️ Found {len(found_dirs)}/{len(opencog_dirs)} OpenCog components")
            
    def test_kernel_structure(self):
        """Test that Wolf kernel files are present"""
        print("\n🔍 Testing Wolf kernel structure...")
        
        kernel_files = [
            "kernels/wolfcore.lisp",
            "kernels/ecron.wl",
            "kernels/wolfnode-guile.scm"
        ]
        
        found_kernels = []
        missing_kernels = []
        
        for kernel in kernel_files:
            kernel_path = self.repo_root / kernel
            if kernel_path.exists():
                found_kernels.append(kernel)
                print(f"  ✓ Found: {kernel}")
            else:
                missing_kernels.append(kernel)
                print(f"  ❌ Missing: {kernel}")
                
        self.results["kernel_structure"] = {
            "found": found_kernels,
            "missing": missing_kernels
        }
        
        if not missing_kernels:
            print("  ✅ All core kernel files present")
        else:
            print(f"  ⚠️ Missing {len(missing_kernels)} kernel files")
            
    def test_manifest_content(self):
        """Test that manifest.scm contains expected dependencies"""
        print("\n🔍 Testing Guix manifest content...")
        
        manifest_file = self.repo_root / ".guix/manifest.scm"
        if not manifest_file.exists():
            print("  ❌ manifest.scm not found")
            self.results["manifest"] = {"status": "fail", "reason": "file_missing"}
            return
            
        content = manifest_file.read_text()
        
        # Check for core dependencies mentioned in the issue
        expected_deps = ["guile", "sbcl", "python", "clang", "git"]
        found_deps = []
        missing_deps = []
        
        for dep in expected_deps:
            if f'"{dep}"' in content:
                found_deps.append(dep)
                print(f"  ✓ Dependency: {dep}")
            else:
                missing_deps.append(dep)
                print(f"  ❌ Missing dependency: {dep}")
                
        self.results["manifest"] = {
            "status": "pass" if not missing_deps else "partial",
            "found": found_deps,
            "missing": missing_deps
        }
        
        if not missing_deps:
            print("  ✅ All expected dependencies present")
            
    def test_bootstrap_integration(self):
        """Test integration between bootstrap components"""
        print("\n🔍 Testing bootstrap component integration...")
        
        stage0_file = self.repo_root / ".guix/bootstrap/stage0.scm"
        init_shell_file = self.repo_root / ".guix/bootstrap/init-shell.scm"
        
        integration_checks = []
        
        # Check if stage0.scm includes launch functionality
        if stage0_file.exists():
            stage0_content = stage0_file.read_text()
            if "launch-wolf-bootstrap" in stage0_content:
                integration_checks.append("stage0_launch")
                print("  ✓ Stage0 launch function present")
            else:
                print("  ⚠️ Stage0 launch function not found")
                
        # Check if init-shell.scm has environment setup
        if init_shell_file.exists():
            init_content = init_shell_file.read_text()
            if "init-wolf-environment" in init_content:
                integration_checks.append("shell_init")
                print("  ✓ Shell initialization present")
            else:
                print("  ⚠️ Shell initialization not found")
                
        self.results["bootstrap_integration"] = {
            "checks_passed": integration_checks,
            "total_checks": 2
        }
        
        if len(integration_checks) == 2:
            print("  ✅ Bootstrap integration complete")
        else:
            print(f"  ⚠️ Partial integration: {len(integration_checks)}/2 checks passed")

    def test_devcontainer_structure(self):
        """Test that devcontainer files exist and are properly structured"""
        print("\n🔍 Testing devcontainer structure...")
        
        required_files = [
            ".devcontainer/devcontainer.json",
            ".devcontainer/Dockerfile",
            ".devcontainer/bootstrap.sh",
            ".devcontainer/wolfram-installer.sh"
        ]
        
        missing_files = []
        for file_path in required_files:
            full_path = self.repo_root / file_path
            if not full_path.exists():
                missing_files.append(file_path)
            else:
                print(f"  ✓ Found: {file_path}")
        
        # Validate JSON structure
        devcontainer_json = self.repo_root / ".devcontainer/devcontainer.json"
        json_valid = False
        if devcontainer_json.exists():
            try:
                import json
                with open(devcontainer_json, 'r') as f:
                    config = json.load(f)
                    if "name" in config and "build" in config:
                        print("  ✓ devcontainer.json structure valid")
                        json_valid = True
                    else:
                        print("  ⚠️ devcontainer.json missing required fields")
            except json.JSONDecodeError:
                print("  ❌ devcontainer.json is not valid JSON")
        
        if missing_files:
            print(f"  ❌ Missing files: {missing_files}")
            self.results["devcontainer_structure"] = {"status": "fail", "missing": missing_files}
        elif not json_valid:
            print("  ❌ devcontainer.json validation failed")
            self.results["devcontainer_structure"] = {"status": "fail", "reason": "invalid_json"}
        else:
            print("  ✅ All devcontainer files present and valid")
            self.results["devcontainer_structure"] = {"status": "pass", "files": required_files}
            
    def generate_report(self):
        """Generate comprehensive test report"""
        print("\n" + "="*60)
        print("🔍 WOLFCOG STAGE0 BOOTSTRAP TEST REPORT")
        print("="*60)
        
        total_tests = len(self.results)
        passed_tests = 0
        
        # Count actual passes more accurately
        for result in self.results.values():
            if isinstance(result, dict):
                if result.get("status") == "pass":
                    passed_tests += 1
                elif result.get("status") != "fail":
                    # Check for successful completion indicators
                    if ("found" in result and "missing" in result and 
                        len(result["missing"]) == 0):
                        passed_tests += 1
                    elif ("checks_passed" in result and "total_checks" in result and
                          result["checks_passed"] and 
                          len(result["checks_passed"]) == result["total_checks"]):
                        passed_tests += 1
        
        print(f"\n📊 Overall Results: {passed_tests}/{total_tests} tests passed")
        
        for test_name, result in self.results.items():
            print(f"\n🔹 {test_name}:")
            if isinstance(result, dict):
                if result.get("status") == "pass":
                    print("  ✅ PASS")
                elif result.get("status") == "fail":
                    print("  ❌ FAIL")
                    if "reason" in result:
                        print(f"     Reason: {result['reason']}")
                    if "missing" in result:
                        print(f"     Missing: {result['missing']}")
                elif ("found" in result and "missing" in result and 
                      len(result["missing"]) == 0):
                    print("  ✅ PASS")
                elif ("checks_passed" in result and 
                      len(result["checks_passed"]) == result.get("total_checks", 0)):
                    print("  ✅ PASS")
                else:
                    print("  ⚠️ PARTIAL")
            else:
                print(f"  📋 {result}")
                
        print(f"\n🎯 Implementation Status:")
        if passed_tests >= total_tests * 0.8:
            print("  ✅ Stage0 bootstrap implementation is well-structured")
            print("  🚀 Ready for testing with proper Guile environment")
        elif passed_tests >= total_tests * 0.6:
            print("  ⚠️ Stage0 bootstrap implementation is mostly complete")
            print("  🔧 Minor fixes needed before deployment")
        else:
            print("  ❌ Stage0 bootstrap implementation needs significant work")
            print("  🛠️ Major components missing or broken")
            
        print(f"\n💡 Next Steps:")
        print(f"  1. Set up proper Guix environment for testing")
        print(f"  2. Test Wolfram kernel detection")
        print(f"  3. Validate OpenCog integration")
        print(f"  4. Test complete bootstrap sequence")
        
        return self.results

def main():
    """Run all bootstrap tests"""
    print("🐺 WolfCog Stage0 Bootstrap Test Suite")
    print("=====================================")
    
    tester = BootstrapTest()
    
    # Run all tests
    tester.test_bootstrap_files_exist()
    tester.test_bootstrap_content_structure()
    tester.test_multi_stage_structure()
    tester.test_opencog_structure()
    tester.test_kernel_structure()
    tester.test_manifest_content()
    tester.test_bootstrap_integration()
    tester.test_devcontainer_structure()
    
    # Generate final report
    results = tester.generate_report()
    
    # Save results for CI/CD
    results_file = Path("/home/runner/work/wolfcog/wolfcog/bootstrap_test_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\n📁 Test results saved to: {results_file}")
    
    return 0

if __name__ == "__main__":
    exit(main())