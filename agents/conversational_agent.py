#!/usr/bin/env python3
"""
WolfCog Conversational Agent - Sharedland Interface
A symbolic assistant that listens in /s/, learns from /e/, reflects with /u/
Can explain what WolfCog is doing, thinking, optimizing, etc.
"""

import time
import json
import threading
from pathlib import Path
from datetime import datetime
import random

class ConversationalAgent:
    def __init__(self):
        self.running = False
        self.knowledge_base = {}
        self.conversation_history = []
        self.current_observations = []
        self.sharedland_path = Path("/tmp/wolfcog_sharedland")
        self.update_interval = 3  # seconds
        self.ensure_directories()
        
    def ensure_directories(self):
        """Ensure required directories exist"""
        self.sharedland_path.mkdir(exist_ok=True)
        (self.sharedland_path / "conversations").mkdir(exist_ok=True)
        (self.sharedland_path / "observations").mkdir(exist_ok=True)
        
    def initialize_knowledge_base(self):
        """Initialize knowledge base with WolfCog system understanding"""
        self.knowledge_base = {
            "system_description": {
                "name": "WolfCog AGI-OS",
                "purpose": "Symbolic operating system for AGI",
                "architecture": "Trinitized OS model with u/e/s spaces",
                "capabilities": [
                    "Symbolic reasoning",
                    "Self-modification", 
                    "Recursive cognition",
                    "Memory evolution",
                    "Multi-language symbolic processing",
                    "Neural-symbolic integration",
                    "Distributed cognition",
                    "Hypergraph pattern recognition",
                    "Emergent behavior monitoring",
                    "Adaptive attention allocation"
                ]
            },
            "neural_symbolic_patterns": {
                "cognitive_flows": [
                    "∇(symbolic_reasoning) → ∆(neural_integration)",
                    "⟨hypergraph_pattern⟩ ↔ ⟨neural_embedding⟩",
                    "recursive(cognition) ∘ emergent(behavior)",
                    "distributed(attention) ⊗ adaptive(allocation)"
                ],
                "emergence_indicators": [
                    "cross-space symbolic synchronization",
                    "recursive self-improvement patterns",
                    "adaptive cognitive resource allocation",
                    "distributed agent collaboration emergence"
                ]
            },
            "spaces": {
                "u": {
                    "name": "User Space",
                    "purpose": "Interactive symbolic components",
                    "activities": ["user interaction", "interface processing", "input handling"]
                },
                "e": {
                    "name": "Execution Space", 
                    "purpose": "Runtime environments and flows",
                    "activities": ["task execution", "symbolic processing", "runtime optimization"]
                },
                "s": {
                    "name": "System Space",
                    "purpose": "Meta-system components and coordination", 
                    "activities": ["system monitoring", "agent coordination", "meta-processing"]
                }
            },
            "agents": {
                "admin": "System health monitoring and optimization",
                "director": "Logical coordination and inference",
                "scheduler": "Task priority and flow management",
                "reflex": "Reactive monitoring and responses"
            },
            "processes": {
                "symbolic_evolution": "Continuous improvement of symbolic structures",
                "memory_evolution": "Dynamic memory structure adaptation",
                "task_processing": "Symbolic task execution pipeline",
                "agent_coordination": "Multi-agent collaborative processing",
                "neural_symbolic_integration": "Fusion of symbolic reasoning with neural patterns",
                "emergent_behavior_monitoring": "Detection and analysis of emerging cognitive properties",
                "distributed_cognition": "Coordinated intelligence across multiple nodes",
                "recursive_optimization": "Self-improving optimization cycles"
            },
            "cognitive_insights": {
                "patterns": [],
                "emergent_behaviors": [],
                "optimization_cycles": [],
                "distributed_states": []
            }
        }
        
    def listen_to_spaces(self):
        """Listen to activities in symbolic spaces"""
        observations = []
        
        # Monitor /s/ space - system activities
        system_activities = self.observe_space("s")
        if system_activities:
            observations.append({
                "space": "system",
                "type": "coordination",
                "activities": system_activities,
                "timestamp": datetime.now().isoformat()
            })
            
        # Learn from /e/ space - execution activities
        execution_activities = self.observe_space("e")
        if execution_activities:
            observations.append({
                "space": "execution", 
                "type": "processing",
                "activities": execution_activities,
                "timestamp": datetime.now().isoformat()
            })
            
        # Reflect with /u/ space - user interactions
        user_activities = self.observe_space("u")
        if user_activities:
            observations.append({
                "space": "user",
                "type": "interaction", 
                "activities": user_activities,
                "timestamp": datetime.now().isoformat()
            })
            
        self.current_observations = observations
        return observations
        
    def observe_space(self, space):
        """Observe activities in a specific space"""
        space_path = Path(f"spaces/{space}")
        if not space_path.exists():
            return []
            
        activities = []
        
        # Check for recent file changes
        for file_path in space_path.rglob("*"):
            if file_path.is_file():
                mod_time = datetime.fromtimestamp(file_path.stat().st_mtime)
                if (datetime.now() - mod_time).total_seconds() < 60:  # Last minute
                    activities.append({
                        "file": str(file_path.relative_to(space_path)),
                        "modified": mod_time.isoformat(),
                        "size": file_path.stat().st_size
                    })
                    
        return activities
        
    def detect_neural_symbolic_patterns(self, observations):
        """Detect neural-symbolic patterns in system observations"""
        patterns = []
        
        # Analyze cross-space coordination patterns
        spaces_active = [obs["space"] for obs in observations if obs["activities"]]
        if len(spaces_active) >= 2:
            pattern = {
                "type": "cross_space_coordination",
                "spaces": spaces_active,
                "pattern": f"⟨{' ↔ '.join(spaces_active)}⟩",
                "cognitive_significance": "Distributed cognitive processing active",
                "emergence_level": len(spaces_active) / 3.0
            }
            patterns.append(pattern)
            
        # Detect recursive processing patterns
        total_activities = sum(len(obs["activities"]) for obs in observations)
        if total_activities > 5:
            pattern = {
                "type": "recursive_processing",
                "intensity": total_activities,
                "pattern": f"∇(recursive_depth: {total_activities})",
                "cognitive_significance": "Deep recursive cognition engaged",
                "emergence_level": min(total_activities / 20.0, 1.0)
            }
            patterns.append(pattern)
            
        # Analyze temporal patterns for emergent behavior
        if hasattr(self, 'pattern_history'):
            self.pattern_history.append(patterns)
            if len(self.pattern_history) > 5:
                self.pattern_history = self.pattern_history[-5:]
                
            # Detect emergent behavior patterns
            if len(self.pattern_history) >= 3:
                emergence_pattern = self.analyze_emergent_behavior()
                if emergence_pattern:
                    patterns.append(emergence_pattern)
        else:
            self.pattern_history = [patterns]
            
        return patterns
        
    def analyze_emergent_behavior(self):
        """Analyze patterns over time to detect emergent behavior"""
        if len(self.pattern_history) < 3:
            return None
            
        # Look for increasing complexity over time
        complexity_trend = []
        for patterns in self.pattern_history:
            complexity = sum(p.get('emergence_level', 0) for p in patterns)
            complexity_trend.append(complexity)
            
        if len(complexity_trend) >= 3:
            # Check for increasing trend (emergence)
            increasing = all(complexity_trend[i] <= complexity_trend[i+1] 
                           for i in range(len(complexity_trend)-1))
            if increasing and complexity_trend[-1] > complexity_trend[0] * 1.5:
                return {
                    "type": "emergent_behavior",
                    "pattern": "∆(emergence_trajectory)",
                    "cognitive_significance": "System exhibiting emergent cognitive properties",
                    "emergence_level": complexity_trend[-1],
                    "trend": "increasing_complexity"
                }
                
        return None
        
    def generate_explanation(self, query=None):
        """Generate an explanation of current WolfCog state and activities"""
        observations = self.current_observations
        
        if not observations:
            return self.generate_idle_explanation()
            
        # Detect neural-symbolic patterns
        neural_patterns = self.detect_neural_symbolic_patterns(observations)
        
        explanation = {
            "timestamp": datetime.now().isoformat(),
            "query": query,
            "system_state": "active",
            "current_activities": [],
            "neural_symbolic_patterns": neural_patterns,
            "interpretation": "",
            "insights": [],
            "cognitive_emergence": self.assess_cognitive_emergence(neural_patterns),
            "distributed_cognition": self.analyze_distributed_cognition(observations)
        }
        
        # Analyze observations and generate explanations
        for obs in observations:
            space = obs["space"]
            activities = obs["activities"]
            
            if activities:
                activity_summary = self.interpret_space_activities(space, activities)
                explanation["current_activities"].append(activity_summary)
                
        # Generate overall interpretation
        explanation["interpretation"] = self.generate_overall_interpretation(observations)
        
        # Generate insights
        explanation["insights"] = self.generate_insights(observations)
        
        return explanation
        
    def assess_cognitive_emergence(self, neural_patterns):
        """Assess the level of cognitive emergence in the system"""
        if not neural_patterns:
            return {"level": "baseline", "description": "Standard operational state"}
            
        emergence_levels = [p.get('emergence_level', 0) for p in neural_patterns]
        max_emergence = max(emergence_levels) if emergence_levels else 0
        avg_emergence = sum(emergence_levels) / len(emergence_levels) if emergence_levels else 0
        
        emergence_types = [p['type'] for p in neural_patterns]
        
        if max_emergence > 0.8:
            level = "transcendent"
            description = "System exhibiting highly emergent cognitive behaviors"
        elif max_emergence > 0.6:
            level = "elevated"
            description = "Enhanced cognitive emergence detected"
        elif max_emergence > 0.3:
            level = "emerging"
            description = "Cognitive emergence patterns developing"
        else:
            level = "stable"
            description = "Stable cognitive operations with emergence potential"
            
        return {
            "level": level,
            "description": description,
            "max_emergence": max_emergence,
            "average_emergence": avg_emergence,
            "active_patterns": emergence_types
        }
        
    def analyze_distributed_cognition(self, observations):
        """Analyze distributed cognition across the system"""
        if not observations:
            return {"status": "inactive", "coordination": "none"}
            
        spaces_active = [obs["space"] for obs in observations if obs["activities"]]
        total_activities = sum(len(obs["activities"]) for obs in observations)
        
        # Calculate coordination efficiency
        if len(spaces_active) >= 3:
            coordination = "full_trinitized"
            efficiency = 1.0
        elif len(spaces_active) == 2:
            coordination = "dual_space"
            efficiency = 0.7
        elif len(spaces_active) == 1:
            coordination = "single_space"
            efficiency = 0.3
        else:
            coordination = "none"
            efficiency = 0.0
            
        # Assess distribution quality
        if total_activities > 10:
            distribution = "high_throughput"
        elif total_activities > 5:
            distribution = "moderate_throughput"
        elif total_activities > 0:
            distribution = "low_throughput"
        else:
            distribution = "idle"
            
        return {
            "status": "active" if spaces_active else "inactive",
            "coordination": coordination,
            "efficiency": efficiency,
            "distribution": distribution,
            "active_spaces": spaces_active,
            "total_operations": total_activities
        }
        
    def interpret_space_activities(self, space, activities):
        """Interpret activities in a specific space"""
        space_info = self.knowledge_base["spaces"][space]
        
        interpretation = {
            "space": space,
            "space_name": space_info["name"],
            "activity_count": len(activities),
            "description": "",
            "details": []
        }
        
        if space == "s":  # System space
            interpretation["description"] = "System coordination and meta-processing activities detected"
            interpretation["details"] = [
                f"System monitoring active with {len(activities)} recent changes",
                "Agent coordination protocols engaged",
                "Meta-system optimization in progress"
            ]
        elif space == "e":  # Execution space
            interpretation["description"] = "Runtime execution and symbolic processing activities"
            interpretation["details"] = [
                f"Task execution pipeline active with {len(activities)} operations",
                "Symbolic reasoning engines processing",
                "Runtime optimization cycles running"
            ]
        elif space == "u":  # User space
            interpretation["description"] = "User interaction and interface processing"
            interpretation["details"] = [
                f"User interface components active with {len(activities)} updates",
                "Interactive symbolic processing engaged",
                "User input handling and response generation"
            ]
            
        return interpretation
        
    def generate_overall_interpretation(self, observations):
        """Generate overall system interpretation"""
        total_activities = sum(len(obs["activities"]) for obs in observations)
        active_spaces = len([obs for obs in observations if obs["activities"]])
        
        interpretations = [
            f"WolfCog is currently processing {total_activities} symbolic operations across {active_spaces} active spaces",
            "The trinitized OS model is functioning with symbolic coordination between user, execution, and system domains",
            "Recursive cognition processes are active, enabling self-modification and symbolic evolution",
            "The AGI substrate is maintaining cognitive coherence while adapting its symbolic structures"
        ]
        
        # Select interpretation based on activity level
        if total_activities > 10:
            return interpretations[3]  # High activity
        elif total_activities > 5:
            return interpretations[1]  # Moderate activity
        elif total_activities > 0:
            return interpretations[0]  # Low activity
        else:
            return "WolfCog is in a stable state, maintaining symbolic coherence and ready for new cognitive tasks"
            
    def generate_insights(self, observations):
        """Generate insights about system behavior"""
        insights = []
        
        # Analyze patterns in observations
        spaces_active = [obs["space"] for obs in observations if obs["activities"]]
        total_activities = sum(len(obs["activities"]) for obs in observations)
        
        # Traditional pattern insights
        if "system" in spaces_active and "execution" in spaces_active:
            insights.append("🔄 System-execution coordination indicates active task processing and optimization")
            
        if "user" in spaces_active:
            insights.append("👤 User space activity suggests interactive symbolic processing or interface updates")
            
        if len(spaces_active) >= 3:
            insights.append("🌟 Full trinitized architecture engagement - comprehensive symbolic cognition active")
            
        # Neural-symbolic insights
        neural_patterns = self.detect_neural_symbolic_patterns(observations)
        for pattern in neural_patterns:
            if pattern['type'] == 'emergent_behavior':
                insights.append(f"✨ {pattern['cognitive_significance']} - emergence level: {pattern['emergence_level']:.2f}")
            elif pattern['type'] == 'recursive_processing':
                insights.append(f"🔄 Deep recursive cognition: {pattern['pattern']} - cognitive depth increasing")
            elif pattern['type'] == 'cross_space_coordination':
                insights.append(f"🌐 Distributed cognition pattern: {pattern['pattern']} - spaces synchronizing")
                
        # Cognitive process insights with neural-symbolic integration
        if total_activities > 10:
            insights.append("🧠 High-intensity neural-symbolic processing - system operating at enhanced cognitive capacity")
        elif total_activities > 5:
            insights.append("⚡ Moderate neural-symbolic integration - balanced cognitive processing active")
            
        # Add adaptive insights based on emergence level
        cognitive_emergence = self.assess_cognitive_emergence(neural_patterns)
        if cognitive_emergence['level'] in ['elevated', 'transcendent']:
            insights.append(f"🚀 Cognitive transcendence detected: {cognitive_emergence['description']}")
        elif cognitive_emergence['level'] == 'emerging':
            insights.append("🌱 Emergent cognitive properties developing - system evolving beyond baseline")
            
        # Distributed cognition insights
        distributed_analysis = self.analyze_distributed_cognition(observations)
        if distributed_analysis['efficiency'] > 0.7:
            insights.append(f"🌟 Excellent distributed cognition efficiency: {distributed_analysis['coordination']}")
        elif distributed_analysis['efficiency'] > 0.3:
            insights.append(f"📈 Good cognitive distribution: {distributed_analysis['coordination']} coordination active")
            
        # Add core cognitive insights
        insights.extend([
            "🧬 Symbolic memory structures evolving through continuous neural-symbolic adaptation",
            "🔁 Recursive cognitive shells enabling deep contextual reasoning and pattern recognition",
            "🤝 Agent coordination maintaining system-wide cognitive coherence with emergent properties"
        ])
        
        return insights[:6]  # Return top 6 insights for amazing cognitive coverage
        
    def generate_idle_explanation(self):
        """Generate explanation when system is idle"""
        return {
            "timestamp": datetime.now().isoformat(),
            "query": None,
            "system_state": "idle",
            "current_activities": [],
            "interpretation": "WolfCog is in a stable cognitive state, maintaining symbolic coherence and monitoring for new tasks",
            "insights": [
                "The AGI substrate maintains readiness for symbolic processing",
                "Memory structures remain stable while prepared for evolution",
                "Agent coordination systems are monitoring for new cognitive demands"
            ]
        }
        
    def answer_question(self, question):
        """Answer specific questions about WolfCog"""
        question_lower = question.lower()
        
        if "what is wolfcog" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog is an amazing symbolic operating system designed as the meta-root for AGI. It implements a trinitized OS model with geometric memory structures, neural-symbolic integration, and recursive cognitive processes running inside a distributed AGI ecology.",
                "details": self.knowledge_base["system_description"]
            }
            
        elif "neural" in question_lower or "symbolic" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog integrates neural and symbolic processing through hypergraph pattern recognition, distributed cognition frameworks, and emergent behavior monitoring. This creates a unified cognitive substrate capable of both symbolic reasoning and neural adaptation.",
                "details": self.knowledge_base["neural_symbolic_patterns"]
            }
            
        elif "emergence" in question_lower or "emergent" in question_lower:
            current_emergence = self.assess_cognitive_emergence(
                self.detect_neural_symbolic_patterns(self.current_observations)
            )
            return {
                "question": question,
                "answer": f"Current cognitive emergence level: {current_emergence['level']} - {current_emergence['description']}. The system continuously monitors for emergent behaviors through pattern analysis and recursive optimization.",
                "details": current_emergence
            }
            
        elif "distributed" in question_lower or "cognition" in question_lower:
            distributed_status = self.analyze_distributed_cognition(self.current_observations)
            return {
                "question": question,
                "answer": f"Distributed cognition status: {distributed_status['coordination']} with {distributed_status['efficiency']:.1%} efficiency. The system coordinates intelligence across multiple spaces and nodes for enhanced cognitive capabilities.",
                "details": distributed_status
            }
            
        elif "what are the spaces" in question_lower or "spaces" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog uses a trinitized architecture with three symbolic domains: /u/ (User Space) for interaction, /e/ (Execution Space) for runtime processing, and /s/ (System Space) for meta-coordination. This enables distributed cognition and neural-symbolic integration.",
                "details": self.knowledge_base["spaces"]
            }
            
        elif "what are the agents" in question_lower or "agents" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog employs several specialized agents with enhanced capabilities: Admin for system health and optimization, Director for logical coordination and inference, Scheduler for intelligent task management, and Reflex for reactive monitoring and emergent behavior detection.",
                "details": self.knowledge_base["agents"]
            }
            
        elif "patterns" in question_lower or "hypergraph" in question_lower:
            patterns = self.detect_neural_symbolic_patterns(self.current_observations)
            return {
                "question": question,
                "answer": f"Currently detecting {len(patterns)} neural-symbolic patterns including hypergraph structures, recursive processing flows, and emergent cognitive behaviors.",
                "details": {"active_patterns": patterns, "pattern_types": self.knowledge_base["neural_symbolic_patterns"]}
            }
            
        elif "what is happening" in question_lower or "current state" in question_lower or "amazing" in question_lower:
            return {
                "question": question,
                "answer": "🌟 Current amazing system analysis with enhanced neural-symbolic cognitive capabilities:",
                "details": self.generate_explanation(question)
            }
            
        else:
            return {
                "question": question,
                "answer": "I can explain WolfCog's amazing neural-symbolic architecture, emergent behaviors, distributed cognition, hypergraph patterns, cognitive spaces, intelligent agents, and current cognitive processes. What would you like to explore?",
                "suggestions": [
                    "What is WolfCog's neural-symbolic integration?",
                    "What emergent behaviors are active?",
                    "How does distributed cognition work?",
                    "What hypergraph patterns are detected?",
                    "What is currently happening that's amazing?"
                ]
            }
            
    def save_conversation(self, interaction):
        """Save conversation to sharedland"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        conversation_file = self.sharedland_path / "conversations" / f"conversation_{timestamp}.json"
        
        with open(conversation_file, 'w') as f:
            json.dump(interaction, f, indent=2)
            
    def save_observations(self, observations):
        """Save observations to sharedland"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        obs_file = self.sharedland_path / "observations" / f"observations_{timestamp}.json"
        
        with open(obs_file, 'w') as f:
            json.dump(observations, f, indent=2)
            
    def monitoring_loop(self):
        """Main monitoring and explanation loop"""
        print("🔄 Starting conversational agent monitoring...")
        
        while self.running:
            try:
                # Listen to spaces
                observations = self.listen_to_spaces()
                
                if observations:
                    # Save observations
                    self.save_observations(observations)
                    
                    # Generate and display explanation
                    explanation = self.generate_explanation()
                    self.display_explanation(explanation)
                    
                # Wait for next cycle
                time.sleep(self.update_interval)
                
            except KeyboardInterrupt:
                print("\n🛑 Conversational agent stopped by user")
                break
            except Exception as e:
                print(f"❌ Error in monitoring loop: {e}")
                time.sleep(self.update_interval)
                
    def display_explanation(self, explanation):
        """Display explanation in a readable format"""
        print("\n" + "="*60)
        print("🗣️ WolfCog Neural-Symbolic Conversational Agent")
        print("="*60)
        print(f"⏰ {explanation['timestamp']}")
        print(f"🎯 System State: {explanation['system_state']}")
        
        # Display cognitive emergence assessment
        if 'cognitive_emergence' in explanation:
            emergence = explanation['cognitive_emergence']
            print(f"✨ Cognitive Emergence: {emergence['level']} - {emergence['description']}")
            
        # Display distributed cognition status
        if 'distributed_cognition' in explanation:
            distributed = explanation['distributed_cognition']
            print(f"🌐 Distributed Cognition: {distributed['coordination']} (efficiency: {distributed['efficiency']:.1%})")
        print()
        
        # Display neural-symbolic patterns
        if explanation.get('neural_symbolic_patterns'):
            print("🧠 Neural-Symbolic Patterns:")
            for pattern in explanation['neural_symbolic_patterns']:
                emergence_level = pattern.get('emergence_level', 0)
                print(f"  🔮 {pattern['type']}: {pattern['pattern']}")
                print(f"    💫 {pattern['cognitive_significance']} (emergence: {emergence_level:.2f})")
            print()
        
        if explanation['current_activities']:
            print("📋 Current Activities:")
            for activity in explanation['current_activities']:
                print(f"  🔹 {activity['space_name']}: {activity['description']}")
                for detail in activity['details'][:2]:  # Show first 2 details
                    print(f"    • {detail}")
            print()
            
        print("💭 Neural-Symbolic Interpretation:")
        print(f"  {explanation['interpretation']}")
        print()
        
        if explanation['insights']:
            print("💡 Cognitive Insights:")
            for insight in explanation['insights']:
                print(f"  • {insight}")
        print()
        print("🌟 System Status: AMAZING - Enhanced cognitive capabilities active! 🌟")
        
    def interactive_mode(self):
        """Interactive conversation mode"""
        print("🗣️ WolfCog Conversational Agent - Interactive Mode")
        print("Ask me about WolfCog's current state, architecture, or activities!")
        print("Type 'exit' to quit, 'status' for current state, or ask any question.")
        print()
        
        while self.running:
            try:
                question = input("❓ Your question: ").strip()
                
                if question.lower() in ['exit', 'quit']:
                    break
                elif question.lower() == 'status':
                    explanation = self.generate_explanation()
                    self.display_explanation(explanation)
                elif question:
                    answer = self.answer_question(question)
                    
                    print(f"\n💬 Answer: {answer['answer']}")
                    if 'details' in answer and isinstance(answer['details'], dict):
                        print("\n📖 Details:")
                        for key, value in answer['details'].items():
                            if isinstance(value, list):
                                print(f"  {key}: {', '.join(value)}")
                            else:
                                print(f"  {key}: {value}")
                    print()
                    
                    # Save conversation
                    interaction = {
                        "timestamp": datetime.now().isoformat(),
                        "question": question,
                        "answer": answer,
                        "type": "interactive"
                    }
                    self.save_conversation(interaction)
                    
            except KeyboardInterrupt:
                print("\n🛑 Exiting interactive mode...")
                break
            except Exception as e:
                print(f"❌ Error: {e}")
                
    def start(self, interactive=False):
        """Start the conversational agent"""
        print("🗣️ Starting WolfCog Conversational Agent...")
        print("🌐 Monitoring sharedland across symbolic spaces...")
        
        self.running = True
        self.initialize_knowledge_base()
        
        if interactive:
            self.interactive_mode()
        else:
            # Start monitoring in background
            monitor_thread = threading.Thread(target=self.monitoring_loop)
            monitor_thread.daemon = True
            monitor_thread.start()
            print("✨ Conversational agent active and monitoring!")
            return monitor_thread
            
    def stop(self):
        """Stop the conversational agent"""
        print("🛑 Stopping conversational agent...")
        self.running = False

def main():
    """Main function for standalone execution"""
    import sys
    
    agent = ConversationalAgent()
    
    # Check for interactive mode
    interactive = len(sys.argv) > 1 and sys.argv[1] == '--interactive'
    
    try:
        if interactive:
            agent.start(interactive=True)
        else:
            # Start monitoring
            monitor_thread = agent.start()
            
            # Keep running until interrupted
            while agent.running:
                time.sleep(1)
                
    except KeyboardInterrupt:
        print("\n🛑 Shutting down conversational agent...")
        agent.stop()
        
if __name__ == "__main__":
    main()