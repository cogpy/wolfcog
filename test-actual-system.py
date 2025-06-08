#!/usr/bin/env python3
"""
WolfCog Actual System Test
Test the REAL components: Guile shell, OpenCog integration, and Wolfram kernels
No fake symbolic processing or Flask nonsense!
"""

import subprocess
import sys
import time
from pathlib import Path

class WolfCogRealityCheck:
    def __init__(self):
        self.results = {}
        
    def test_guile_shell(self):
        """Test if Guile shell is working properly"""
        print("🐧 Testing Guile Shell...")
        
        try:
            # Test basic Guile functionality
            result = subprocess.run(
                ["guile", "-c", "(display \"Guile is working!\\n\")"], 
                capture_output=True, text=True, timeout=10
            )
            
            guile_working = result.returncode == 0 and "Guile is working!" in result.stdout
            
            if guile_working:
                print("  ✅ Guile shell is functional")
                
                # Test WolfNode Guile module
                wolfnode_test = subprocess.run(
                    ["guile", "-l", "kernels/wolfnode-guile.scm", "-c", "(wolfnode-eval 'test)"],
                    capture_output=True, text=True, timeout=10, cwd="/workspaces/wolfcog"
                )
                
                wolfnode_working = wolfnode_test.returncode == 0
                print(f"  {'✅' if wolfnode_working else '❌'} WolfNode Guile module: {'Working' if wolfnode_working else 'Failed'}")
                
                self.results["guile"] = {
                    "basic": True,
                    "wolfnode": wolfnode_working,
                    "output": result.stdout.strip()
                }
            else:
                print(f"  ❌ Guile shell failed: {result.stderr}")
                self.results["guile"] = {"basic": False, "error": result.stderr}
                
        except Exception as e:
            print(f"  ❌ Guile test failed: {e}")
            self.results["guile"] = {"basic": False, "error": str(e)}
            
    def test_opencog_integration(self):
        """Test OpenCog integration"""
        print("\n🧠 Testing OpenCog Integration...")
        
        try:
            # Check if OpenCog tools are available
            cog_check = subprocess.run(
                ["which", "cogserver"], 
                capture_output=True, text=True
            )
            
            if cog_check.returncode == 0:
                print("  ✅ CogServer found in PATH")
                
                # Test the wolf-to-cog adapter
                adapter_test = subprocess.run(
                    ["guile", "-l", "opencog/wolf-to-cog-adapter.scm", "-c", "(init-cog-bridge)"],
                    capture_output=True, text=True, timeout=10, cwd="/workspaces/wolfcog"
                )
                
                adapter_working = adapter_test.returncode == 0
                print(f"  {'✅' if adapter_working else '❌'} Wolf-to-Cog adapter: {'Working' if adapter_working else 'Failed'}")
                
                self.results["opencog"] = {
                    "cogserver": True,
                    "adapter": adapter_working,
                    "adapter_output": adapter_test.stdout
                }
            else:
                print("  ⚠️ CogServer not found - OpenCog may not be installed")
                self.results["opencog"] = {"cogserver": False, "note": "Not installed or not in PATH"}
                
        except Exception as e:
            print(f"  ❌ OpenCog test failed: {e}")
            self.results["opencog"] = {"error": str(e)}
            
    def test_wolfram_kernels(self):
        """Test Wolfram Language kernel integration"""
        print("\n🺸 Testing Wolfram Kernels...")
        
        try:
            # Check if Wolfram Engine/Mathematica is available
            wolfram_check = subprocess.run(
                ["which", "wolframscript"], 
                capture_output=True, text=True
            )
            
            if wolfram_check.returncode == 0:
                print("  ✅ Wolfram Engine found")
                
                # Test basic Wolfram functionality
                basic_test = subprocess.run(
                    ["wolframscript", "-c", "Print[\"Wolfram is working!\"]"],
                    capture_output=True, text=True, timeout=15
                )
                
                if basic_test.returncode == 0:
                    print("  ✅ Basic Wolfram execution working")
                    
                    # Test ecron.wl kernel
                    ecron_test = subprocess.run(
                        ["wolframscript", "-f", "kernels/ecron.wl"],
                        capture_output=True, text=True, timeout=15, cwd="/workspaces/wolfcog"
                    )
                    
                    ecron_working = ecron_test.returncode == 0
                    print(f"  {'✅' if ecron_working else '❌'} Ecron kernel: {'Working' if ecron_working else 'Failed'}")
                    
                    self.results["wolfram"] = {
                        "engine": True,
                        "basic": True,
                        "ecron": ecron_working,
                        "ecron_output": ecron_test.stdout
                    }
                else:
                    print(f"  ❌ Basic Wolfram test failed: {basic_test.stderr}")
                    self.results["wolfram"] = {"engine": True, "basic": False, "error": basic_test.stderr}
            else:
                print("  ⚠️ Wolfram Engine not found")
                
                # Check for alternative: Mathematica
                math_check = subprocess.run(["which", "math"], capture_output=True, text=True)
                if math_check.returncode == 0:
                    print("  ✅ Mathematica found as alternative")
                    self.results["wolfram"] = {"engine": False, "mathematica": True}
                else:
                    print("  ❌ No Wolfram Language kernel found")
                    self.results["wolfram"] = {"engine": False, "mathematica": False}
                    
        except Exception as e:
            print(f"  ❌ Wolfram test failed: {e}")
            self.results["wolfram"] = {"error": str(e)}
            
    def test_integration_bridges(self):
        """Test bridges between components"""
        print("\n🌉 Testing Integration Bridges...")
        
        # Test if components can communicate
        guile_ok = self.results.get("guile", {}).get("basic", False)
        opencog_ok = self.results.get("opencog", {}).get("adapter", False)
        wolfram_ok = self.results.get("wolfram", {}).get("basic", False)
        
        integration_score = sum([guile_ok, opencog_ok, wolfram_ok])
        
        print(f"  📊 Component Integration Score: {integration_score}/3")
        
        if integration_score == 3:
            print("  🌟 Full integration possible!")
        elif integration_score == 2:
            print("  ✅ Partial integration available")
        elif integration_score == 1:
            print("  ⚠️ Limited integration - missing components")
        else:
            print("  ❌ No integration possible - core components missing")
            
        self.results["integration"] = {
            "score": integration_score,
            "guile": guile_ok,
            "opencog": opencog_ok, 
            "wolfram": wolfram_ok
        }
        
    def test_actual_task_processing(self):
        """Test the actual task processing pipeline"""
        print("\n⚙️ Testing Actual Task Processing...")
        
        try:
            # Test the enhanced task daemon (this one actually exists)
            daemon_test = subprocess.run(
                [sys.executable, "opencog/ecron-task-daemon-enhanced.py", "--test"],
                capture_output=True, text=True, timeout=10, cwd="/workspaces/wolfcog"
            )
            
            # Even if it fails, we can see what the actual error is
            daemon_working = daemon_test.returncode == 0
            
            print(f"  {'✅' if daemon_working else '⚠️'} Task daemon: {'Working' if daemon_working else 'Has issues'}")
            
            if not daemon_working:
                print(f"    Error: {daemon_test.stderr}")
                
            self.results["task_processing"] = {
                "daemon": daemon_working,
                "output": daemon_test.stdout,
                "error": daemon_test.stderr
            }
            
        except Exception as e:
            print(f"  ❌ Task processing test failed: {e}")
            self.results["task_processing"] = {"error": str(e)}
            
    def generate_reality_report(self):
        """Generate an honest report about what's actually working"""
        print("\n" + "=" * 60)
        print("🔍 WOLFCOG REALITY CHECK REPORT")
        print("=" * 60)
        
        print(f"\n🎯 Component Status:")
        
        # Guile Status
        guile_status = self.results.get("guile", {})
        if guile_status.get("basic"):
            print(f"  🐧 Guile: ✅ Working")
            if guile_status.get("wolfnode"):
                print(f"    🔧 WolfNode module: ✅ Loaded")
            else:
                print(f"    🔧 WolfNode module: ⚠️ Issues")
        else:
            print(f"  🐧 Guile: ❌ Not working")
            
        # OpenCog Status  
        opencog_status = self.results.get("opencog", {})
        if opencog_status.get("cogserver"):
            print(f"  🧠 OpenCog: ✅ Available")
            if opencog_status.get("adapter"):
                print(f"    🌉 Wolf-Cog adapter: ✅ Working")
            else:
                print(f"    🌉 Wolf-Cog adapter: ⚠️ Issues")
        else:
            print(f"  🧠 OpenCog: ❌ Not installed")
            
        # Wolfram Status
        wolfram_status = self.results.get("wolfram", {})
        if wolfram_status.get("engine") or wolfram_status.get("mathematica"):
            print(f"  🺸 Wolfram: ✅ Available")
            if wolfram_status.get("ecron"):
                print(f"    ⏰ Ecron kernel: ✅ Working")
            else:
                print(f"    ⏰ Ecron kernel: ⚠️ Issues")
        else:
            print(f"  🺸 Wolfram: ❌ Not available")
            
        # Integration Assessment
        integration = self.results.get("integration", {})
        score = integration.get("score", 0)
        
        print(f"\n🔗 Integration Assessment:")
        print(f"  Score: {score}/3 components working")
        
        if score == 3:
            print(f"  ✨ Ready for kernel pool integration!")
        elif score >= 2:
            print(f"  🔧 Almost ready - fix remaining component")
        else:
            print(f"  ⚠️ Need to fix core components first")
            
        print(f"\n💡 Next Steps:")
        if not self.results.get("wolfram", {}).get("engine"):
            print(f"  1. Install Wolfram Engine or Mathematica")
        if not self.results.get("opencog", {}).get("cogserver"):
            print(f"  2. Install OpenCog (cogserver)")
        if score >= 2:
            print(f"  3. Set up Wolfram kernel pools")
            print(f"  4. Create proper symbolic bridges")
            
def main():
    print("🔍 WolfCog Reality Check - Testing ACTUAL Components")
    print("=" * 60)
    
    tester = WolfCogRealityCheck()
    
    tester.test_guile_shell()
    tester.test_opencog_integration() 
    tester.test_wolfram_kernels()
    tester.test_integration_bridges()
    tester.test_actual_task_processing()
    
    tester.generate_reality_report()

if __name__ == "__main__":
    main()
