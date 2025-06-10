#!/usr/bin/env python3
"""
Wolfram-OpenCog Bridge
Integrates Wolfram Language kernels with OpenCog AtomSpace
Enables symbolic computation exchange between the two systems
"""

import subprocess
import json
import time
import threading
import queue
import tempfile
import os
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
import re


class WolframOpenCogBridge:
    """Bridge between Wolfram Language and OpenCog AtomSpace"""
    
    def __init__(self, wolfram_executable="wolframscript"):
        self.wolfram_executable = wolfram_executable
        self.kernel_pool = []
        self.available_kernels = queue.Queue()
        self.atomspace = None
        self.running = False
        
        # Translation mappings
        self.wolfram_to_atomspace = {
            "Concept": "ConceptNode",
            "Rule": "ImplicationLink", 
            "List": "ListLink",
            "Set": "SetLink",
            "Function": "LambdaLink",
            "Equal": "EqualLink"
        }
        
        self.atomspace_to_wolfram = {v: k for k, v in self.wolfram_to_atomspace.items()}
        
        # Initialize components
        self.initialize_opencog()
        
    def initialize_opencog(self):
        """Initialize OpenCog AtomSpace"""
        try:
            from opencog.atomspace import AtomSpace
            from opencog.utilities import initialize_opencog
            
            self.atomspace = AtomSpace()
            initialize_opencog(self.atomspace)
            self.opencog_available = True
            print("✅ OpenCog AtomSpace initialized")
            
        except ImportError:
            print("⚠️ OpenCog not available, using simulation mode")
            self.opencog_available = False
            self.atomspace_simulation = {
                "atoms": {},
                "concepts": {},
                "links": {}
            }
    
    def start_kernel_pool(self, pool_size=3):
        """Start a pool of Wolfram kernels"""
        print(f"🔧 Starting Wolfram kernel pool (size: {pool_size})...")
        
        # Check if Wolfram is available
        if not self.check_wolfram_availability():
            print("❌ Wolfram Language not available")
            return False
        
        # Start kernels
        for i in range(pool_size):
            kernel = self.create_wolfram_kernel(f"kernel-{i}")
            if kernel:
                self.kernel_pool.append(kernel)
                self.available_kernels.put(kernel)
                print(f"  ✅ Started kernel {i+1}/{pool_size}")
            else:
                print(f"  ❌ Failed to start kernel {i+1}")
        
        self.running = True
        print(f"✅ Kernel pool started with {len(self.kernel_pool)} kernels")
        return len(self.kernel_pool) > 0
    
    def check_wolfram_availability(self):
        """Check if Wolfram Language is available"""
        try:
            result = subprocess.run([
                self.wolfram_executable, "-code", "Print[\"Wolfram Available\"]"
            ], capture_output=True, text=True, timeout=10)
            
            return result.returncode == 0 and "Wolfram Available" in result.stdout
            
        except (subprocess.TimeoutExpired, FileNotFoundError):
            print("⚠️ Wolfram Language not found, creating simulation")
            self.create_wolfram_simulation()
            return True
    
    def create_wolfram_simulation(self):
        """Create Wolfram Language simulation for development"""
        self.wolfram_simulation = {
            "symbolic_expressions": {},
            "rules": {},
            "functions": {},
            "variables": {}
        }
        print("📝 Wolfram simulation created")
    
    def create_wolfram_kernel(self, kernel_id):
        """Create a single Wolfram kernel"""
        try:
            # Create initialization script
            init_script = """
(* Initialize kernel for OpenCog bridge *)
$HistoryLength = 0;
Print["Kernel initialized: """ + kernel_id + """"];

(* Define OpenCog bridge functions *)
OpenCogConceptNode[name_String] := Concept[name];
OpenCogListLink[elements_List] := List @@ elements;
OpenCogImplicationLink[premise_, conclusion_] := Rule[premise, conclusion];

(* Export to AtomSpace format *)
ToAtomSpace[expr_] := Module[{result},
  result = Replace[expr, {
    Concept[name_] :> {"type": "ConceptNode", "name": name},
    List[elements___] :> {"type": "ListLink", "elements": {elements}},
    Rule[p_, c_] :> {"type": "ImplicationLink", "premise": p, "conclusion": c}
  }, {0, Infinity}];
  ExportString[result, "JSON"]
];

(* Ready signal *)
Print["KERNEL_READY"];
"""
            
            # Write script to temp file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.wls', delete=False) as f:
                f.write(init_script)
                script_path = f.name
            
            # Start kernel process
            process = subprocess.Popen([
                self.wolfram_executable, "-script", script_path
            ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE, text=True, bufsize=1)
            
            # Wait for initialization
            output = ""
            start_time = time.time()
            while "KERNEL_READY" not in output and time.time() - start_time < 30:
                line = process.stdout.readline()
                if line:
                    output += line
                    if "KERNEL_READY" in line:
                        break
                time.sleep(0.1)
            
            if "KERNEL_READY" in output:
                kernel_info = {
                    "id": kernel_id,
                    "process": process,
                    "script_path": script_path,
                    "available": True
                }
                return kernel_info
            else:
                print(f"❌ Kernel {kernel_id} failed to initialize")
                process.terminate()
                return None
                
        except Exception as e:
            print(f"❌ Error creating kernel {kernel_id}: {e}")
            return None
    
    def get_kernel(self, timeout=5.0):
        """Get an available kernel from the pool"""
        try:
            return self.available_kernels.get(timeout=timeout)
        except queue.Empty:
            print("⚠️ No kernels available")
            return None
    
    def return_kernel(self, kernel):
        """Return a kernel to the pool"""
        if kernel and kernel["available"]:
            self.available_kernels.put(kernel)
    
    def wolfram_to_atomspace(self, wolfram_expr):
        """Convert Wolfram expression to AtomSpace representation"""
        if self.opencog_available:
            return self.wolfram_to_atomspace_real(wolfram_expr)
        else:
            return self.wolfram_to_atomspace_simulation(wolfram_expr)
    
    def wolfram_to_atomspace_real(self, wolfram_expr):
        """Convert using real AtomSpace"""
        try:
            from opencog.type_constructors import ConceptNode, ListLink, ImplicationLink
            
            # Parse Wolfram expression and convert
            if isinstance(wolfram_expr, dict):
                expr_type = wolfram_expr.get("type")
                
                if expr_type == "Concept":
                    atom = ConceptNode(wolfram_expr["name"])
                    return self.atomspace.add_atom(atom)
                    
                elif expr_type == "List":
                    elements = [self.wolfram_to_atomspace_real(elem) 
                              for elem in wolfram_expr["elements"]]
                    atom = ListLink(*elements)
                    return self.atomspace.add_atom(atom)
                    
                elif expr_type == "Rule":
                    premise = self.wolfram_to_atomspace_real(wolfram_expr["premise"])
                    conclusion = self.wolfram_to_atomspace_real(wolfram_expr["conclusion"])
                    atom = ImplicationLink(premise, conclusion)
                    return self.atomspace.add_atom(atom)
            
            # Default: create concept node
            atom = ConceptNode(str(wolfram_expr))
            return self.atomspace.add_atom(atom)
            
        except Exception as e:
            print(f"❌ Error converting to AtomSpace: {e}")
            return None
    
    def wolfram_to_atomspace_simulation(self, wolfram_expr):
        """Convert using AtomSpace simulation"""
        atom_id = f"atom-{len(self.atomspace_simulation['atoms'])}"
        
        atom_data = {
            "id": atom_id,
            "wolfram_expr": wolfram_expr,
            "type": "ConceptNode",
            "created_at": time.time()
        }
        
        self.atomspace_simulation["atoms"][atom_id] = atom_data
        return atom_data
    
    def atomspace_to_wolfram(self, atom):
        """Convert AtomSpace atom to Wolfram expression"""
        if self.opencog_available:
            return self.atomspace_to_wolfram_real(atom)
        else:
            return self.atomspace_to_wolfram_simulation(atom)
    
    def atomspace_to_wolfram_real(self, atom):
        """Convert real AtomSpace atom to Wolfram"""
        try:
            atom_type = atom.type_name
            
            if atom_type == "ConceptNode":
                return f"Concept[\"{atom.name}\"]"
            elif atom_type == "ListLink":
                elements = [self.atomspace_to_wolfram_real(elem) for elem in atom.out]
                return f"List[{', '.join(elements)}]"
            elif atom_type == "ImplicationLink":
                premise = self.atomspace_to_wolfram_real(atom.out[0])
                conclusion = self.atomspace_to_wolfram_real(atom.out[1])
                return f"Rule[{premise}, {conclusion}]"
            else:
                return f"Unknown[\"{str(atom)}\"]"
                
        except Exception as e:
            print(f"❌ Error converting from AtomSpace: {e}")
            return str(atom)
    
    def atomspace_to_wolfram_simulation(self, atom_data):
        """Convert simulated atom to Wolfram"""
        if isinstance(atom_data, dict):
            return f"Concept[\"{atom_data.get('id', 'unknown')}\"]"
        return str(atom_data)
    
    def execute_wolfram_code(self, code, return_format="json"):
        """Execute Wolfram code and return result"""
        kernel = self.get_kernel()
        if not kernel:
            return self.execute_wolfram_simulation(code)
        
        try:
            # Prepare code for execution
            if return_format == "json":
                wrapped_code = f"ToAtomSpace[{code}]"
            else:
                wrapped_code = code
            
            # Send to kernel
            kernel["process"].stdin.write(wrapped_code + "\n")
            kernel["process"].stdin.flush()
            
            # Read result
            result = ""
            start_time = time.time()
            while time.time() - start_time < 10:  # 10 second timeout
                line = kernel["process"].stdout.readline()
                if line.strip():
                    result = line.strip()
                    break
                time.sleep(0.01)
            
            return result
            
        except Exception as e:
            print(f"❌ Error executing Wolfram code: {e}")
            return None
        finally:
            self.return_kernel(kernel)
    
    def execute_wolfram_simulation(self, code):
        """Simulate Wolfram code execution"""
        # Simple pattern matching for common operations
        patterns = {
            r'Concept\["(.+?)"\]': lambda m: {"type": "ConceptNode", "name": m.group(1)},
            r'List\[(.+)\]': lambda m: {"type": "ListLink", "elements": m.group(1).split(",")},
            r'(\d+)\s*\+\s*(\d+)': lambda m: str(int(m.group(1)) + int(m.group(2))),
            r'Solve\[(.+)\]': lambda m: f"Solution[{m.group(1)}]"
        }
        
        for pattern, handler in patterns.items():
            match = re.search(pattern, code)
            if match:
                return handler(match)
        
        return f"SimulatedResult[{code}]"
    
    def symbolic_computation(self, atomspace_query, wolfram_computation):
        """Perform symbolic computation bridging both systems"""
        print(f"🔄 Performing symbolic computation...")
        
        # Convert AtomSpace query to Wolfram
        wolfram_expr = self.atomspace_to_wolfram(atomspace_query)
        
        # Execute computation in Wolfram
        computation_code = f"{wolfram_computation}[{wolfram_expr}]"
        wolfram_result = self.execute_wolfram_code(computation_code)
        
        # Convert result back to AtomSpace
        if wolfram_result:
            try:
                if isinstance(wolfram_result, str) and wolfram_result.startswith("{"):
                    # Parse JSON result
                    result_data = json.loads(wolfram_result)
                    atomspace_result = self.wolfram_to_atomspace(result_data)
                else:
                    # Simple string result
                    atomspace_result = self.wolfram_to_atomspace({"type": "Concept", "name": str(wolfram_result)})
                
                return atomspace_result
            except Exception as e:
                print(f"❌ Error processing result: {e}")
                return None
        
        return None
    
    def solve_symbolic_equation(self, equation):
        """Solve symbolic equation using Wolfram and store in AtomSpace"""
        print(f"🔢 Solving equation: {equation}")
        
        # Execute in Wolfram
        wolfram_code = f"Solve[{equation}, x]"
        solution = self.execute_wolfram_code(wolfram_code)
        
        if solution:
            # Create AtomSpace representation
            equation_concept = self.wolfram_to_atomspace({
                "type": "Concept", 
                "name": f"Equation_{equation}"
            })
            
            solution_concept = self.wolfram_to_atomspace({
                "type": "Concept",
                "name": f"Solution_{solution}"
            })
            
            # Link equation to solution
            if self.opencog_available:
                from opencog.type_constructors import EvaluationLink, PredicateNode, ListLink
                eval_link = EvaluationLink(
                    PredicateNode("has_solution"),
                    ListLink(equation_concept, solution_concept)
                )
                self.atomspace.add_atom(eval_link)
            
            return solution
        
        return None
    
    def pattern_match_with_wolfram(self, pattern):
        """Use Wolfram for advanced pattern matching"""
        print(f"🔍 Pattern matching: {pattern}")
        
        # Convert pattern to Wolfram
        wolfram_pattern = self.atomspace_to_wolfram(pattern)
        
        # Execute pattern matching
        wolfram_code = f"Cases[AllAtoms[], {wolfram_pattern}]"
        matches = self.execute_wolfram_code(wolfram_code)
        
        if matches:
            # Convert matches back to AtomSpace
            result_atoms = []
            if isinstance(matches, list):
                for match in matches:
                    atom = self.wolfram_to_atomspace(match)
                    if atom:
                        result_atoms.append(atom)
            
            return result_atoms
        
        return []
    
    def get_statistics(self):
        """Get bridge statistics"""
        stats = {
            "running": self.running,
            "kernel_pool_size": len(self.kernel_pool),
            "available_kernels": self.available_kernels.qsize(),
            "opencog_available": self.opencog_available
        }
        
        if self.opencog_available and self.atomspace:
            stats["atomspace_size"] = len(self.atomspace)
        elif hasattr(self, 'atomspace_simulation'):
            stats["simulated_atoms"] = len(self.atomspace_simulation["atoms"])
        
        return stats
    
    def stop(self):
        """Stop the bridge and all kernels"""
        print("🛑 Stopping Wolfram-OpenCog bridge...")
        self.running = False
        
        for kernel in self.kernel_pool:
            try:
                kernel["process"].terminate()
                kernel["process"].wait(timeout=5)
                os.unlink(kernel["script_path"])  # Clean up temp script
            except Exception as e:
                print(f"⚠️ Error stopping kernel {kernel['id']}: {e}")
        
        print("✅ Bridge stopped")


def main():
    """Test the Wolfram-OpenCog bridge"""
    bridge = WolframOpenCogBridge()
    
    if bridge.start_kernel_pool(2):
        print("🧪 Testing Wolfram-OpenCog integration...")
        
        # Test 1: Basic symbolic computation
        test_concept = {"type": "Concept", "name": "TestConcept"}
        atomspace_atom = bridge.wolfram_to_atomspace(test_concept)
        print(f"Test 1 - AtomSpace atom: {atomspace_atom}")
        
        # Test 2: Solve equation
        solution = bridge.solve_symbolic_equation("x^2 - 4 == 0")
        print(f"Test 2 - Equation solution: {solution}")
        
        # Test 3: Execute Wolfram code
        result = bridge.execute_wolfram_code("2 + 2")
        print(f"Test 3 - Wolfram computation: {result}")
        
        # Test 4: Get statistics
        stats = bridge.get_statistics()
        print(f"Test 4 - Bridge stats: {stats}")
        
        bridge.stop()
    else:
        print("❌ Failed to start bridge")


if __name__ == "__main__":
    main()
