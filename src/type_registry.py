#!/usr/bin/env python3
"""
Type Registry for Wolfram-OpenCog Bridge
Manages bidirectional type mappings and conversions between Wolfram Language and OpenCog AtomSpace
"""

from typing import Dict, Any, Optional, Callable, Tuple
import json
import re


class TypeRegistry:
    """Registry for managing type mappings between Wolfram and OpenCog"""
    
    def __init__(self):
        self.wolfram_to_atomspace: Dict[str, str] = {}
        self.atomspace_to_wolfram: Dict[str, str] = {}
        self.conversion_handlers: Dict[str, Callable] = {}
        self.reverse_conversion_handlers: Dict[str, Callable] = {}
        
        # Initialize default mappings
        self._initialize_default_mappings()
    
    def _initialize_default_mappings(self):
        """Initialize default type mappings"""
        # Basic atom types
        self.register_type_mapping("Concept", "ConceptNode")
        self.register_type_mapping("Predicate", "PredicateNode")
        self.register_type_mapping("Variable", "VariableNode")
        self.register_type_mapping("Number", "NumberNode")
        self.register_type_mapping("String", "StringNode")
        
        # Link types
        self.register_type_mapping("List", "ListLink")
        self.register_type_mapping("Set", "SetLink")
        self.register_type_mapping("Rule", "ImplicationLink")
        self.register_type_mapping("Equal", "EqualLink")
        self.register_type_mapping("And", "AndLink")
        self.register_type_mapping("Or", "OrLink")
        self.register_type_mapping("Not", "NotLink")
        self.register_type_mapping("Function", "LambdaLink")
        self.register_type_mapping("Evaluation", "EvaluationLink")
        self.register_type_mapping("Execution", "ExecutionLink")
        
        # Advanced types
        self.register_type_mapping("Implication", "ImplicationLink")
        self.register_type_mapping("Inheritance", "InheritanceLink")
        self.register_type_mapping("Similarity", "SimilarityLink")
        self.register_type_mapping("Member", "MemberLink")
        self.register_type_mapping("Subset", "SubsetLink")
        
        # Mathematical constructs
        self.register_type_mapping("Plus", "PlusLink")
        self.register_type_mapping("Times", "TimesLink")
        self.register_type_mapping("Power", "PowerLink")
        
        # Truth value types
        self.register_type_mapping("TruthValue", "SimpleTruthValue")
        self.register_type_mapping("Probability", "ProbabilisticTruthValue")
    
    def register_type_mapping(self, wolfram_type: str, atomspace_type: str):
        """Register a bidirectional type mapping"""
        self.wolfram_to_atomspace[wolfram_type] = atomspace_type
        self.atomspace_to_wolfram[atomspace_type] = wolfram_type
    
    def register_conversion_handler(
        self,
        wolfram_type: str,
        to_atomspace: Callable,
        from_atomspace: Optional[Callable] = None
    ):
        """Register custom conversion handlers for complex types"""
        self.conversion_handlers[wolfram_type] = to_atomspace
        if from_atomspace:
            atomspace_type = self.wolfram_to_atomspace.get(wolfram_type)
            if atomspace_type:
                self.reverse_conversion_handlers[atomspace_type] = from_atomspace
    
    def get_atomspace_type(self, wolfram_type: str) -> Optional[str]:
        """Get AtomSpace type for a Wolfram type"""
        return self.wolfram_to_atomspace.get(wolfram_type)
    
    def get_wolfram_type(self, atomspace_type: str) -> Optional[str]:
        """Get Wolfram type for an AtomSpace type"""
        return self.atomspace_to_wolfram.get(atomspace_type)
    
    def has_conversion_handler(self, wolfram_type: str) -> bool:
        """Check if a custom conversion handler exists"""
        return wolfram_type in self.conversion_handlers
    
    def get_conversion_handler(self, wolfram_type: str) -> Optional[Callable]:
        """Get conversion handler for Wolfram type"""
        return self.conversion_handlers.get(wolfram_type)
    
    def get_reverse_conversion_handler(self, atomspace_type: str) -> Optional[Callable]:
        """Get reverse conversion handler for AtomSpace type"""
        return self.reverse_conversion_handlers.get(atomspace_type)
    
    def convert_wolfram_to_atomspace(self, wolfram_expr: Dict[str, Any]) -> Dict[str, Any]:
        """Convert Wolfram expression to AtomSpace representation"""
        if not isinstance(wolfram_expr, dict):
            return {"type": "ConceptNode", "name": str(wolfram_expr)}
        
        wolfram_type = wolfram_expr.get("type", "Unknown")
        
        # Check for custom handler
        handler = self.get_conversion_handler(wolfram_type)
        if handler:
            return handler(wolfram_expr)
        
        # Use default mapping
        atomspace_type = self.get_atomspace_type(wolfram_type)
        if atomspace_type:
            result = {"type": atomspace_type}
            
            # Copy relevant fields
            if "name" in wolfram_expr:
                result["name"] = wolfram_expr["name"]
            if "elements" in wolfram_expr:
                result["outgoing"] = wolfram_expr["elements"]
            if "value" in wolfram_expr:
                result["value"] = wolfram_expr["value"]
            if "truthvalue" in wolfram_expr:
                result["tv"] = wolfram_expr["truthvalue"]
            
            return result
        
        # Fallback to concept node
        return {
            "type": "ConceptNode",
            "name": f"Wolfram_{wolfram_type}_{wolfram_expr.get('name', 'Unknown')}"
        }
    
    def convert_atomspace_to_wolfram(self, atom_data: Dict[str, Any]) -> Dict[str, Any]:
        """Convert AtomSpace atom to Wolfram representation"""
        if not isinstance(atom_data, dict):
            return {"type": "Concept", "name": str(atom_data)}
        
        atomspace_type = atom_data.get("type", "Unknown")
        
        # Check for custom reverse handler
        handler = self.get_reverse_conversion_handler(atomspace_type)
        if handler:
            return handler(atom_data)
        
        # Use default mapping
        wolfram_type = self.get_wolfram_type(atomspace_type)
        if wolfram_type:
            result = {"type": wolfram_type}
            
            # Copy relevant fields
            if "name" in atom_data:
                result["name"] = atom_data["name"]
            if "outgoing" in atom_data:
                result["elements"] = atom_data["outgoing"]
            if "value" in atom_data:
                result["value"] = atom_data["value"]
            if "tv" in atom_data:
                result["truthvalue"] = atom_data["tv"]
            
            return result
        
        # Fallback to concept
        return {
            "type": "Concept",
            "name": f"AtomSpace_{atomspace_type}_{atom_data.get('name', 'Unknown')}"
        }
    
    def format_wolfram_expression(self, wolfram_data: Dict[str, Any]) -> str:
        """Format Wolfram data as Wolfram Language code"""
        wl_type = wolfram_data.get("type", "Unknown")
        
        if wl_type == "Concept":
            name = wolfram_data.get("name", "Unknown")
            return f'Concept["{name}"]'
        
        elif wl_type == "List":
            elements = wolfram_data.get("elements", [])
            if isinstance(elements, list):
                formatted_elements = [self.format_wolfram_expression(e) if isinstance(e, dict) else str(e) 
                                    for e in elements]
                return f"List[{', '.join(formatted_elements)}]"
            return "List[]"
        
        elif wl_type == "Rule":
            premise = wolfram_data.get("premise", {})
            conclusion = wolfram_data.get("conclusion", {})
            premise_str = self.format_wolfram_expression(premise) if isinstance(premise, dict) else str(premise)
            conclusion_str = self.format_wolfram_expression(conclusion) if isinstance(conclusion, dict) else str(conclusion)
            return f"Rule[{premise_str}, {conclusion_str}]"
        
        elif wl_type == "Equal":
            lhs = wolfram_data.get("lhs", {})
            rhs = wolfram_data.get("rhs", {})
            lhs_str = self.format_wolfram_expression(lhs) if isinstance(lhs, dict) else str(lhs)
            rhs_str = self.format_wolfram_expression(rhs) if isinstance(rhs, dict) else str(rhs)
            return f"Equal[{lhs_str}, {rhs_str}]"
        
        elif wl_type == "Number":
            value = wolfram_data.get("value", 0)
            return str(value)
        
        elif wl_type == "String":
            value = wolfram_data.get("value", "")
            return f'"{value}"'
        
        elif wl_type == "Variable":
            name = wolfram_data.get("name", "x")
            return name
        
        else:
            # Generic format
            name = wolfram_data.get("name", "Unknown")
            return f'{wl_type}["{name}"]'
    
    def parse_wolfram_expression(self, wolfram_code: str) -> Dict[str, Any]:
        """Parse Wolfram Language code to structured data"""
        wolfram_code = wolfram_code.strip()
        
        # Simple parsing for basic types
        # Concept["name"]
        if wolfram_code.startswith("Concept["):
            match = re.match(r'Concept\["(.+?)"\]', wolfram_code)
            if match:
                return {"type": "Concept", "name": match.group(1)}
        
        # List[...]
        elif wolfram_code.startswith("List["):
            # Simplified list parsing
            inner = wolfram_code[5:-1]  # Remove "List[" and "]"
            # This is simplified - real parsing would need proper bracket matching
            return {"type": "List", "elements": [inner]}
        
        # Rule[premise, conclusion]
        elif wolfram_code.startswith("Rule["):
            # Simplified rule parsing
            return {"type": "Rule", "raw": wolfram_code}
        
        # Number
        elif wolfram_code.isdigit() or (wolfram_code.replace('.', '', 1).isdigit()):
            return {"type": "Number", "value": float(wolfram_code)}
        
        # String
        elif wolfram_code.startswith('"') and wolfram_code.endswith('"'):
            return {"type": "String", "value": wolfram_code[1:-1]}
        
        # Default: treat as concept
        return {"type": "Concept", "name": wolfram_code}
    
    def get_all_mappings(self) -> Dict[str, Dict[str, str]]:
        """Get all registered type mappings"""
        return {
            "wolfram_to_atomspace": self.wolfram_to_atomspace.copy(),
            "atomspace_to_wolfram": self.atomspace_to_wolfram.copy()
        }
    
    def export_mappings(self, filepath: str):
        """Export type mappings to JSON file"""
        mappings = self.get_all_mappings()
        with open(filepath, 'w') as f:
            json.dump(mappings, f, indent=2)
    
    def import_mappings(self, filepath: str):
        """Import type mappings from JSON file"""
        with open(filepath, 'r') as f:
            mappings = json.load(f)
        
        if "wolfram_to_atomspace" in mappings:
            self.wolfram_to_atomspace.update(mappings["wolfram_to_atomspace"])
        
        if "atomspace_to_wolfram" in mappings:
            self.atomspace_to_wolfram.update(mappings["atomspace_to_wolfram"])


# Singleton instance
_type_registry = None

def get_type_registry() -> TypeRegistry:
    """Get the global type registry instance"""
    global _type_registry
    if _type_registry is None:
        _type_registry = TypeRegistry()
    return _type_registry


if __name__ == "__main__":
    # Test type registry
    print("🧪 Testing Type Registry...")
    
    registry = get_type_registry()
    
    # Test basic mappings
    print("\n1. Testing basic type mappings:")
    wolfram_type = "Concept"
    atomspace_type = registry.get_atomspace_type(wolfram_type)
    print(f"  {wolfram_type} -> {atomspace_type}")
    
    reverse_type = registry.get_wolfram_type(atomspace_type)
    print(f"  {atomspace_type} -> {reverse_type}")
    
    # Test conversion
    print("\n2. Testing Wolfram to AtomSpace conversion:")
    wolfram_expr = {"type": "Concept", "name": "TestConcept"}
    atomspace_repr = registry.convert_wolfram_to_atomspace(wolfram_expr)
    print(f"  Wolfram: {wolfram_expr}")
    print(f"  AtomSpace: {atomspace_repr}")
    
    # Test reverse conversion
    print("\n3. Testing AtomSpace to Wolfram conversion:")
    atomspace_atom = {"type": "ConceptNode", "name": "TestNode"}
    wolfram_repr = registry.convert_atomspace_to_wolfram(atomspace_atom)
    print(f"  AtomSpace: {atomspace_atom}")
    print(f"  Wolfram: {wolfram_repr}")
    
    # Test formatting
    print("\n4. Testing Wolfram expression formatting:")
    formatted = registry.format_wolfram_expression(wolfram_expr)
    print(f"  Formatted: {formatted}")
    
    # Test parsing
    print("\n5. Testing Wolfram expression parsing:")
    parsed = registry.parse_wolfram_expression('Concept["ParsedConcept"]')
    print(f"  Parsed: {parsed}")
    
    # Show all mappings
    print("\n6. All registered type mappings:")
    mappings = registry.get_all_mappings()
    print(f"  Total Wolfram types: {len(mappings['wolfram_to_atomspace'])}")
    print(f"  Total AtomSpace types: {len(mappings['atomspace_to_wolfram'])}")
    
    print("\n✅ Type Registry tests completed")
