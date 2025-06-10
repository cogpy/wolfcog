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
from pathlib import Path
from typing import Dict, Any, List, Optional

from symbolic_processor import RealSymbolicProcessor
from wolfram_opencog_bridge import WolframOpenCogBridge


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
        
        # Statistics
        self.stats = {
            "computations_completed": 0,
            "wolfram_calls": 0,
            "opencog_operations": 0,
            "bridge_errors": 0,
            "start_time": None
        }
        
        # Integration patterns
        self.integration_patterns = {
            "symbolic_solve": self.pattern_symbolic_solve,
            "pattern_match": self.pattern_pattern_match,
            "inference_chain": self.pattern_inference_chain,
            "optimization": self.pattern_optimization,
            "knowledge_extraction": self.pattern_knowledge_extraction
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
                    result = self.process_integration_request(request)
                    self.result_queue.put(result)
                    self.stats["computations_completed"] += 1
                except queue.Empty:
                    continue
                    
            except Exception as e:
                print(f"❌ Integration processing error: {e}")
                self.stats["bridge_errors"] += 1
    
    def process_integration_request(self, request):
        """Process an integration request"""
        pattern = request.get("pattern", "symbolic_solve")
        data = request.get("data", {})
        
        if pattern in self.integration_patterns:
            return self.integration_patterns[pattern](data)
        else:
            return {"error": f"Unknown pattern: {pattern}"}
    
    def pattern_symbolic_solve(self, data):
        """Pattern: Solve symbolic equation using both systems"""
        equation = data.get("equation", "x^2 - 4 == 0")
        variable = data.get("variable", "x")
        
        print(f"🔢 Solving: {equation}")
        
        # Step 1: Use Wolfram to solve
        wolfram_result = self.wolfram_bridge.solve_symbolic_equation(equation)
        self.stats["wolfram_calls"] += 1
        
        # Step 2: Store result in AtomSpace
        if wolfram_result:
            atomspace_task = {
                "type": "add_concept",
                "data": {
                    "name": f"Solution_{equation}",
                    "properties": {"solution": wolfram_result, "equation": equation}
                }
            }
            self.symbolic_processor.submit_task(atomspace_task)
            time.sleep(0.1)  # Allow processing
            
            result = self.symbolic_processor.get_result()
            self.stats["opencog_operations"] += 1
            
            return {
                "pattern": "symbolic_solve",
                "equation": equation,
                "wolfram_solution": wolfram_result,
                "atomspace_result": result,
                "success": True
            }
        
        return {"pattern": "symbolic_solve", "success": False, "error": "No solution found"}
    
    def pattern_pattern_match(self, data):
        """Pattern: Advanced pattern matching using both systems"""
        pattern = data.get("pattern", "X->Y")
        context = data.get("context", "general")
        
        print(f"🔍 Pattern matching: {pattern}")
        
        # Step 1: Use AtomSpace for local pattern matching
        atomspace_task = {
            "type": "pattern_match",
            "data": {"pattern": pattern}
        }
        self.symbolic_processor.submit_task(atomspace_task)
        time.sleep(0.1)
        
        atomspace_matches = self.symbolic_processor.get_result()
        self.stats["opencog_operations"] += 1
        
        # Step 2: Use Wolfram for advanced pattern analysis
        if atomspace_matches and "matches" in atomspace_matches:
            wolfram_analysis = self.wolfram_bridge.pattern_match_with_wolfram({
                "type": "pattern", 
                "expression": pattern
            })
            self.stats["wolfram_calls"] += 1
            
            return {
                "pattern": "pattern_match",
                "query_pattern": pattern,
                "atomspace_matches": atomspace_matches,
                "wolfram_analysis": wolfram_analysis,
                "success": True
            }
        
        return {"pattern": "pattern_match", "success": False, "error": "No matches found"}
    
    def pattern_inference_chain(self, data):
        """Pattern: Multi-step inference using both systems"""
        premises = data.get("premises", [])
        goal = data.get("goal", "conclusion")
        
        print(f"🧠 Inference chain: {premises} -> {goal}")
        
        # Step 1: Use AtomSpace for logical inference
        atomspace_task = {
            "type": "inference",
            "data": {"premises": premises}
        }
        self.symbolic_processor.submit_task(atomspace_task)
        time.sleep(0.1)
        
        atomspace_conclusions = self.symbolic_processor.get_result()
        self.stats["opencog_operations"] += 1
        
        # Step 2: Use Wolfram for verification and expansion
        if atomspace_conclusions and "conclusions" in atomspace_conclusions:
            conclusions = atomspace_conclusions["conclusions"]
            
            # Verify each conclusion with Wolfram
            verified_conclusions = []
            for conclusion in conclusions:
                verification = self.wolfram_bridge.execute_wolfram_code(
                    f"TrueQ[{conclusion}]"
                )
                if verification and "True" in str(verification):
                    verified_conclusions.append(conclusion)
            
            self.stats["wolfram_calls"] += len(conclusions)
            
            return {
                "pattern": "inference_chain",
                "premises": premises,
                "atomspace_conclusions": conclusions,
                "verified_conclusions": verified_conclusions,
                "success": True
            }
        
        return {"pattern": "inference_chain", "success": False, "error": "No conclusions"}
    
    def pattern_optimization(self, data):
        """Pattern: Optimization using Wolfram with AtomSpace storage"""
        objective = data.get("objective", "x^2 + y^2")
        variables = data.get("variables", ["x", "y"])
        constraints = data.get("constraints", [])
        
        print(f"⚡ Optimizing: {objective}")
        
        # Use Wolfram for optimization
        wolfram_code = f"NMinimize[{objective}, {{{', '.join(variables)}}}]"
        optimization_result = self.wolfram_bridge.execute_wolfram_code(wolfram_code)
        self.stats["wolfram_calls"] += 1
        
        if optimization_result:
            # Store optimization result in AtomSpace
            atomspace_task = {
                "type": "add_concept",
                "data": {
                    "name": f"Optimization_{objective}",
                    "properties": {
                        "objective": objective,
                        "variables": variables,
                        "result": optimization_result,
                        "method": "wolfram_nminimize"
                    }
                }
            }
            self.symbolic_processor.submit_task(atomspace_task)
            time.sleep(0.1)
            
            storage_result = self.symbolic_processor.get_result()
            self.stats["opencog_operations"] += 1
            
            return {
                "pattern": "optimization",
                "objective": objective,
                "wolfram_result": optimization_result,
                "atomspace_storage": storage_result,
                "success": True
            }
        
        return {"pattern": "optimization", "success": False, "error": "Optimization failed"}
    
    def pattern_knowledge_extraction(self, data):
        """Pattern: Extract knowledge from text using both systems"""
        text = data.get("text", "")
        domain = data.get("domain", "general")
        
        print(f"📚 Extracting knowledge from text...")
        
        # Use Wolfram for text analysis
        wolfram_analysis = self.wolfram_bridge.execute_wolfram_code(
            f'TextAnalyze["{text}"]'
        )
        self.stats["wolfram_calls"] += 1
        
        if wolfram_analysis:
            # Extract concepts and relations
            concepts = self.extract_concepts_from_analysis(wolfram_analysis)
            relations = self.extract_relations_from_analysis(wolfram_analysis)
            
            # Store in AtomSpace
            for concept in concepts:
                atomspace_task = {
                    "type": "add_concept",
                    "data": {"name": concept, "properties": {"domain": domain, "source": "text"}}
                }
                self.symbolic_processor.submit_task(atomspace_task)
                time.sleep(0.01)
            
            for relation in relations:
                atomspace_task = {
                    "type": "add_relation",
                    "data": {
                        "source": relation["source"],
                        "target": relation["target"],
                        "relation_type": relation["type"]
                    }
                }
                self.symbolic_processor.submit_task(atomspace_task)
                time.sleep(0.01)
            
            self.stats["opencog_operations"] += len(concepts) + len(relations)
            
            return {
                "pattern": "knowledge_extraction",
                "text_length": len(text),
                "concepts_extracted": len(concepts),
                "relations_extracted": len(relations),
                "wolfram_analysis": wolfram_analysis,
                "success": True
            }
        
        return {"pattern": "knowledge_extraction", "success": False, "error": "Analysis failed"}
    
    def extract_concepts_from_analysis(self, analysis):
        """Extract concepts from Wolfram text analysis"""
        # Simplified concept extraction
        if isinstance(analysis, str):
            words = analysis.split()
            concepts = [word for word in words if len(word) > 3 and word.isalpha()]
            return concepts[:10]  # Limit to 10 concepts
        return []
    
    def extract_relations_from_analysis(self, analysis):
        """Extract relations from Wolfram text analysis"""
        # Simplified relation extraction
        relations = []
        if isinstance(analysis, str):
            # Look for simple patterns like "X is Y" or "X has Y"
            import re
            patterns = [
                r'(\w+) is (\w+)',
                r'(\w+) has (\w+)',
                r'(\w+) and (\w+)'
            ]
            
            for pattern in patterns:
                matches = re.findall(pattern, analysis)
                for match in matches:
                    relations.append({
                        "source": match[0],
                        "target": match[1],
                        "type": "relates_to"
                    })
        
        return relations[:5]  # Limit to 5 relations
    
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
