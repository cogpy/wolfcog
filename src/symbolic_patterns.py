#!/usr/bin/env python3
"""
Symbolic Computation Patterns
Advanced symbolic computation patterns integrating Wolfram Language and OpenCog
"""

from typing import Dict, Any, List, Optional, Callable
import json
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.type_registry import get_type_registry


class SymbolicPattern:
    """Base class for symbolic computation patterns"""
    
    def __init__(self, name: str, description: str = ""):
        self.name = name
        self.description = description
        self.type_registry = get_type_registry()
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute the pattern - to be overridden by subclasses"""
        raise NotImplementedError(f"Pattern {self.name} must implement execute()")
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        """Validate input data - to be overridden by subclasses"""
        return True


class SymbolicSolvePattern(SymbolicPattern):
    """Pattern for solving symbolic equations"""
    
    def __init__(self):
        super().__init__(
            "symbolic_solve",
            "Solve symbolic equations using Wolfram and store results in AtomSpace"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "equation" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute symbolic equation solving"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: equation required"}
        
        equation = data.get("equation", "")
        variable = data.get("variable", "x")
        
        # Use bridge to solve equation
        wolfram_code = f"Solve[{equation}, {variable}]"
        solution = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        if solution:
            # Create AtomSpace representation
            atomspace_task = {
                "type": "add_concept",
                "data": {
                    "name": f"Solution_{equation}",
                    "properties": {
                        "equation": equation,
                        "variable": variable,
                        "solution": solution
                    }
                }
            }
            
            # Store in AtomSpace via symbolic processor
            bridge.symbolic_processor.submit_task(atomspace_task)
            
            return {
                "success": True,
                "pattern": self.name,
                "equation": equation,
                "variable": variable,
                "solution": solution
            }
        
        return {"success": False, "error": "Failed to solve equation"}


class PatternMatchingPattern(SymbolicPattern):
    """Pattern for advanced pattern matching"""
    
    def __init__(self):
        super().__init__(
            "pattern_matching",
            "Advanced pattern matching using both Wolfram and OpenCog"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "pattern" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute pattern matching"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: pattern required"}
        
        pattern = data.get("pattern", "")
        context = data.get("context", "general")
        
        # First, search in AtomSpace
        atomspace_task = {
            "type": "pattern_match",
            "data": {"pattern": pattern}
        }
        bridge.symbolic_processor.submit_task(atomspace_task)
        
        # Then use Wolfram for advanced analysis
        wolfram_pattern = self.type_registry.format_wolfram_expression({
            "type": "Concept",
            "name": pattern
        })
        
        wolfram_code = f"Cases[AllConcepts[], {wolfram_pattern}]"
        wolfram_matches = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        return {
            "success": True,
            "pattern": self.name,
            "query_pattern": pattern,
            "context": context,
            "wolfram_matches": wolfram_matches
        }


class InferenceChainPattern(SymbolicPattern):
    """Pattern for multi-step logical inference"""
    
    def __init__(self):
        super().__init__(
            "inference_chain",
            "Multi-step inference using logical rules"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "premises" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute inference chain"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: premises required"}
        
        premises = data.get("premises", [])
        goal = data.get("goal", None)
        
        # Use AtomSpace for inference
        atomspace_task = {
            "type": "inference",
            "data": {"premises": premises, "goal": goal}
        }
        bridge.symbolic_processor.submit_task(atomspace_task)
        
        # Build Wolfram representation for verification
        wolfram_premises = [self._premise_to_wolfram(p) for p in premises]
        
        return {
            "success": True,
            "pattern": self.name,
            "premises": premises,
            "goal": goal,
            "wolfram_premises": wolfram_premises
        }
    
    def _premise_to_wolfram(self, premise: str) -> str:
        """Convert premise to Wolfram format"""
        # Simple conversion - can be enhanced
        if "=>" in premise:
            parts = premise.split("=>")
            return f"Implies[{parts[0].strip()}, {parts[1].strip()}]"
        return premise


class OptimizationPattern(SymbolicPattern):
    """Pattern for symbolic optimization"""
    
    def __init__(self):
        super().__init__(
            "optimization",
            "Optimize symbolic expressions using Wolfram"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "objective" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute optimization"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: objective required"}
        
        objective = data.get("objective", "")
        variables = data.get("variables", ["x"])
        constraints = data.get("constraints", [])
        method = data.get("method", "NMinimize")
        
        # Build Wolfram optimization code
        var_list = "{" + ", ".join(variables) + "}"
        
        if constraints:
            constraint_str = " && ".join(constraints)
            wolfram_code = f"{method}[{{{objective}, {constraint_str}}}, {var_list}]"
        else:
            wolfram_code = f"{method}[{objective}, {var_list}]"
        
        # Execute optimization
        result = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        if result:
            # Store result in AtomSpace
            atomspace_task = {
                "type": "add_concept",
                "data": {
                    "name": f"Optimization_{objective}",
                    "properties": {
                        "objective": objective,
                        "variables": variables,
                        "method": method,
                        "result": result
                    }
                }
            }
            bridge.symbolic_processor.submit_task(atomspace_task)
            
            return {
                "success": True,
                "pattern": self.name,
                "objective": objective,
                "variables": variables,
                "result": result
            }
        
        return {"success": False, "error": "Optimization failed"}


class SymbolicSimplificationPattern(SymbolicPattern):
    """Pattern for symbolic simplification"""
    
    def __init__(self):
        super().__init__(
            "simplification",
            "Simplify symbolic expressions"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "expression" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute simplification"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: expression required"}
        
        expression = data.get("expression", "")
        assumptions = data.get("assumptions", [])
        
        # Build Wolfram simplification code
        if assumptions:
            assumption_str = ", ".join(assumptions)
            wolfram_code = f"Simplify[{expression}, {assumption_str}]"
        else:
            wolfram_code = f"Simplify[{expression}]"
        
        # Execute simplification
        result = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        if result:
            return {
                "success": True,
                "pattern": self.name,
                "original": expression,
                "simplified": result,
                "assumptions": assumptions
            }
        
        return {"success": False, "error": "Simplification failed"}


class SymbolicDifferentiationPattern(SymbolicPattern):
    """Pattern for symbolic differentiation"""
    
    def __init__(self):
        super().__init__(
            "differentiation",
            "Compute symbolic derivatives"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "expression" in data and "variable" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute differentiation"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: expression and variable required"}
        
        expression = data.get("expression", "")
        variable = data.get("variable", "x")
        order = data.get("order", 1)
        
        # Build Wolfram differentiation code
        if order == 1:
            wolfram_code = f"D[{expression}, {variable}]"
        else:
            wolfram_code = f"D[{expression}, {{{variable}, {order}}}]"
        
        # Execute differentiation
        result = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        if result:
            return {
                "success": True,
                "pattern": self.name,
                "expression": expression,
                "variable": variable,
                "order": order,
                "derivative": result
            }
        
        return {"success": False, "error": "Differentiation failed"}


class SymbolicIntegrationPattern(SymbolicPattern):
    """Pattern for symbolic integration"""
    
    def __init__(self):
        super().__init__(
            "integration",
            "Compute symbolic integrals"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "expression" in data and "variable" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute integration"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: expression and variable required"}
        
        expression = data.get("expression", "")
        variable = data.get("variable", "x")
        bounds = data.get("bounds", None)  # For definite integrals
        
        # Build Wolfram integration code
        if bounds and len(bounds) == 2:
            wolfram_code = f"Integrate[{expression}, {{{variable}, {bounds[0]}, {bounds[1]}}}]"
        else:
            wolfram_code = f"Integrate[{expression}, {variable}]"
        
        # Execute integration
        result = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        if result:
            return {
                "success": True,
                "pattern": self.name,
                "expression": expression,
                "variable": variable,
                "bounds": bounds,
                "integral": result
            }
        
        return {"success": False, "error": "Integration failed"}


class KnowledgeExtractionPattern(SymbolicPattern):
    """Pattern for extracting knowledge from text"""
    
    def __init__(self):
        super().__init__(
            "knowledge_extraction",
            "Extract semantic knowledge from text"
        )
    
    def validate_input(self, data: Dict[str, Any]) -> bool:
        return "text" in data
    
    def execute(self, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute knowledge extraction"""
        if not self.validate_input(data):
            return {"success": False, "error": "Invalid input: text required"}
        
        text = data.get("text", "")
        domain = data.get("domain", "general")
        
        # Use Wolfram for text analysis
        wolfram_code = f'TextAnalyze["{text}"]'
        analysis = bridge.wolfram_bridge.execute_wolfram_code(wolfram_code)
        
        # Extract concepts and relations (simplified)
        concepts = self._extract_concepts(text)
        relations = self._extract_relations(text)
        
        # Store in AtomSpace
        for concept in concepts:
            atomspace_task = {
                "type": "add_concept",
                "data": {
                    "name": concept,
                    "properties": {"domain": domain, "source": "text"}
                }
            }
            bridge.symbolic_processor.submit_task(atomspace_task)
        
        return {
            "success": True,
            "pattern": self.name,
            "text_length": len(text),
            "concepts": concepts,
            "relations": relations,
            "domain": domain
        }
    
    def _extract_concepts(self, text: str) -> List[str]:
        """Extract concepts from text (simplified)"""
        # Simple word extraction - can be enhanced with NLP
        words = text.split()
        concepts = [word.strip('.,!?;:') for word in words if len(word) > 4]
        return list(set(concepts))[:10]  # Limit to 10 unique concepts
    
    def _extract_relations(self, text: str) -> List[Dict[str, str]]:
        """Extract relations from text (simplified)"""
        # Simple pattern matching - can be enhanced
        relations = []
        
        # Look for "X is Y" patterns
        import re
        patterns = [
            (r'(\w+) is (\w+)', 'is_a'),
            (r'(\w+) has (\w+)', 'has'),
            (r'(\w+) and (\w+)', 'related_to')
        ]
        
        for pattern, relation_type in patterns:
            matches = re.findall(pattern, text.lower())
            for match in matches[:3]:  # Limit to 3 per pattern
                relations.append({
                    "source": match[0],
                    "target": match[1],
                    "type": relation_type
                })
        
        return relations


class PatternRegistry:
    """Registry for managing symbolic computation patterns"""
    
    def __init__(self):
        self.patterns: Dict[str, SymbolicPattern] = {}
        self._register_default_patterns()
    
    def _register_default_patterns(self):
        """Register default patterns"""
        self.register_pattern(SymbolicSolvePattern())
        self.register_pattern(PatternMatchingPattern())
        self.register_pattern(InferenceChainPattern())
        self.register_pattern(OptimizationPattern())
        self.register_pattern(SymbolicSimplificationPattern())
        self.register_pattern(SymbolicDifferentiationPattern())
        self.register_pattern(SymbolicIntegrationPattern())
        self.register_pattern(KnowledgeExtractionPattern())
    
    def register_pattern(self, pattern: SymbolicPattern):
        """Register a new pattern"""
        self.patterns[pattern.name] = pattern
    
    def get_pattern(self, name: str) -> Optional[SymbolicPattern]:
        """Get a pattern by name"""
        return self.patterns.get(name)
    
    def list_patterns(self) -> List[str]:
        """List all registered pattern names"""
        return list(self.patterns.keys())
    
    def execute_pattern(self, name: str, bridge, data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a pattern by name"""
        pattern = self.get_pattern(name)
        if pattern:
            return pattern.execute(bridge, data)
        else:
            return {"success": False, "error": f"Pattern '{name}' not found"}


# Singleton instance
_pattern_registry = None

def get_pattern_registry() -> PatternRegistry:
    """Get the global pattern registry instance"""
    global _pattern_registry
    if _pattern_registry is None:
        _pattern_registry = PatternRegistry()
    return _pattern_registry


if __name__ == "__main__":
    # Test pattern registry
    print("🧪 Testing Symbolic Patterns...")
    
    registry = get_pattern_registry()
    
    # List all patterns
    print("\n1. Available patterns:")
    for pattern_name in registry.list_patterns():
        pattern = registry.get_pattern(pattern_name)
        print(f"  - {pattern_name}: {pattern.description}")
    
    # Test pattern validation
    print("\n2. Testing pattern validation:")
    solve_pattern = registry.get_pattern("symbolic_solve")
    valid_data = {"equation": "x^2 - 4 == 0", "variable": "x"}
    invalid_data = {"variable": "x"}
    
    print(f"  Valid data: {solve_pattern.validate_input(valid_data)}")
    print(f"  Invalid data: {solve_pattern.validate_input(invalid_data)}")
    
    print("\n✅ Symbolic Patterns tests completed")
