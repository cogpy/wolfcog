#!/usr/bin/env python3
"""
WolfCog Architecture Documentation Validation Test
Validates the comprehensive architecture documentation and Mermaid diagrams
"""

import os
import re
from pathlib import Path

def validate_mermaid_diagrams():
    """Validate Mermaid diagram syntax and completeness"""
    docs_dir = Path("docs")
    results = {
        "total_files": 0,
        "total_diagrams": 0,
        "diagram_types": {},
        "validation_errors": []
    }
    
    # Mermaid diagram types to validate
    expected_types = [
        "graph TD", "graph LR", "graph TB", "flowchart",
        "sequenceDiagram", "stateDiagram-v2", "stateDiagram"
    ]
    
    for md_file in docs_dir.glob("*.md"):
        if md_file.name in ["README.md", "architecture.md", "component-architecture.md", 
                           "integration-pathways.md", "emergent-patterns.md"]:
            results["total_files"] += 1
            content = md_file.read_text()
            
            # Find all Mermaid diagrams
            mermaid_blocks = re.findall(r'```mermaid\n(.*?)\n```', content, re.DOTALL)
            results["total_diagrams"] += len(mermaid_blocks)
            
            for block in mermaid_blocks:
                # Identify diagram type
                first_line = block.split('\n')[0].strip()
                diagram_type = None
                for expected in expected_types:
                    if first_line.startswith(expected):
                        diagram_type = expected
                        break
                
                if diagram_type:
                    results["diagram_types"][diagram_type] = results["diagram_types"].get(diagram_type, 0) + 1
                else:
                    results["validation_errors"].append(f"Unknown diagram type in {md_file.name}: {first_line}")
    
    return results

def validate_documentation_structure():
    """Validate documentation structure and cross-references"""
    docs_dir = Path("docs")
    structure_results = {
        "required_files": [],
        "missing_files": [],
        "cross_references": 0,
        "hypergraph_annotations": 0
    }
    
    required_files = [
        "README.md", "architecture.md", "component-architecture.md",
        "integration-pathways.md", "emergent-patterns.md"
    ]
    
    for file in required_files:
        file_path = docs_dir / file
        if file_path.exists():
            structure_results["required_files"].append(file)
            content = file_path.read_text()
            
            # Count cross-references
            structure_results["cross_references"] += len(re.findall(r'\[.*?\]\(.*?\.md\)', content))
            
            # Count hypergraph annotations
            structure_results["hypergraph_annotations"] += content.count("⟨") + content.count("⟩")
        else:
            structure_results["missing_files"].append(file)
    
    return structure_results

def main():
    print("🐺 WolfCog Architecture Documentation Validation")
    print("=" * 50)
    
    # Validate Mermaid diagrams
    print("🔍 Validating Mermaid diagrams...")
    mermaid_results = validate_mermaid_diagrams()
    
    print(f"  📊 Files processed: {mermaid_results['total_files']}")
    print(f"  📈 Total diagrams: {mermaid_results['total_diagrams']}")
    print(f"  🎯 Diagram types found:")
    for diagram_type, count in mermaid_results['diagram_types'].items():
        print(f"    - {diagram_type}: {count}")
    
    if mermaid_results['validation_errors']:
        print(f"  ⚠️  Validation errors:")
        for error in mermaid_results['validation_errors']:
            print(f"    - {error}")
    else:
        print("  ✅ All Mermaid diagrams validated successfully")
    
    # Validate documentation structure
    print("\n🔍 Validating documentation structure...")
    structure_results = validate_documentation_structure()
    
    print(f"  📁 Required files found: {len(structure_results['required_files'])}/5")
    for file in structure_results['required_files']:
        print(f"    ✅ {file}")
    
    if structure_results['missing_files']:
        print(f"  ❌ Missing files:")
        for file in structure_results['missing_files']:
            print(f"    - {file}")
    
    print(f"  🔗 Cross-references: {structure_results['cross_references']}")
    print(f"  ⟨⟩ Hypergraph annotations: {structure_results['hypergraph_annotations']}")
    
    # Overall validation summary
    print("\n📊 Validation Summary:")
    total_score = 0
    max_score = 100
    
    # Mermaid diagrams score (40 points)
    if mermaid_results['total_diagrams'] >= 40:
        diagram_score = 40
    else:
        diagram_score = (mermaid_results['total_diagrams'] / 40) * 40
    total_score += diagram_score
    print(f"  📈 Mermaid diagrams: {diagram_score:.1f}/40 ({mermaid_results['total_diagrams']} diagrams)")
    
    # Documentation structure score (30 points)
    structure_score = (len(structure_results['required_files']) / 5) * 30
    total_score += structure_score
    print(f"  📁 Documentation structure: {structure_score:.1f}/30")
    
    # Cross-references score (20 points)
    if structure_results['cross_references'] >= 10:
        ref_score = 20
    else:
        ref_score = (structure_results['cross_references'] / 10) * 20
    total_score += ref_score
    print(f"  🔗 Cross-references: {ref_score:.1f}/20 ({structure_results['cross_references']} refs)")
    
    # Hypergraph annotations score (10 points)
    if structure_results['hypergraph_annotations'] >= 50:
        hyper_score = 10
    else:
        hyper_score = (structure_results['hypergraph_annotations'] / 50) * 10
    total_score += hyper_score
    print(f"  ⟨⟩ Hypergraph annotations: {hyper_score:.1f}/10 ({structure_results['hypergraph_annotations']} annotations)")
    
    print(f"\n🎯 Total Score: {total_score:.1f}/{max_score}")
    
    if total_score >= 90:
        print("🌟 EXCELLENT: Comprehensive architecture documentation achieved!")
    elif total_score >= 80:
        print("✅ GOOD: Strong architecture documentation with minor improvements needed")
    elif total_score >= 70:
        print("⚠️  ADEQUATE: Basic requirements met but significant improvements needed")
    else:
        print("❌ INSUFFICIENT: Major improvements required")
    
    return total_score >= 80

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)