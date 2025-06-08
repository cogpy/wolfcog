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
                    "Multi-language symbolic processing"
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
                "agent_coordination": "Multi-agent collaborative processing"
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
        
    def generate_explanation(self, query=None):
        """Generate an explanation of current WolfCog state and activities"""
        observations = self.current_observations
        
        if not observations:
            return self.generate_idle_explanation()
            
        explanation = {
            "timestamp": datetime.now().isoformat(),
            "query": query,
            "system_state": "active",
            "current_activities": [],
            "interpretation": "",
            "insights": []
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
        
        if "system" in spaces_active and "execution" in spaces_active:
            insights.append("System-execution coordination indicates active task processing and optimization")
            
        if "user" in spaces_active:
            insights.append("User space activity suggests interactive symbolic processing or interface updates")
            
        if len(spaces_active) >= 3:
            insights.append("Full trinitized architecture engagement - comprehensive symbolic cognition active")
            
        # Add cognitive process insights
        insights.extend([
            "Symbolic memory structures are evolving through continuous adaptation",
            "Recursive shell processes enable deep contextual reasoning",
            "Agent coordination maintains system-wide cognitive coherence"
        ])
        
        return insights[:3]  # Return top 3 insights
        
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
                "answer": "WolfCog is a symbolic operating system designed as the meta-root for AGI. It implements a trinitized OS model with geometric memory structures and contextual grammars running inside a recursive AGI ecology.",
                "details": self.knowledge_base["system_description"]
            }
            
        elif "what are the spaces" in question_lower or "spaces" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog uses a trinitized architecture with three symbolic domains: /u/ (User Space) for interaction, /e/ (Execution Space) for runtime processing, and /s/ (System Space) for meta-coordination.",
                "details": self.knowledge_base["spaces"]
            }
            
        elif "what are the agents" in question_lower or "agents" in question_lower:
            return {
                "question": question,
                "answer": "WolfCog employs several specialized agents: Admin for system health, Director for logical coordination, Scheduler for task management, and Reflex for reactive monitoring.",
                "details": self.knowledge_base["agents"]
            }
            
        elif "what is happening" in question_lower or "current state" in question_lower:
            return {
                "question": question,
                "answer": "Current system analysis:",
                "details": self.generate_explanation(question)
            }
            
        else:
            return {
                "question": question,
                "answer": "I can explain WolfCog's architecture, current activities, symbolic spaces, agents, and cognitive processes. What would you like to know?",
                "suggestions": [
                    "What is WolfCog?",
                    "What are the symbolic spaces?",
                    "What agents are active?",
                    "What is currently happening?"
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
        print("\n" + "="*50)
        print("🗣️ WolfCog Conversational Agent")
        print("="*50)
        print(f"⏰ {explanation['timestamp']}")
        print(f"🎯 System State: {explanation['system_state']}")
        print()
        
        if explanation['current_activities']:
            print("📋 Current Activities:")
            for activity in explanation['current_activities']:
                print(f"  🔹 {activity['space_name']}: {activity['description']}")
                for detail in activity['details'][:2]:  # Show first 2 details
                    print(f"    • {detail}")
            print()
            
        print("💭 Interpretation:")
        print(f"  {explanation['interpretation']}")
        print()
        
        if explanation['insights']:
            print("💡 Insights:")
            for insight in explanation['insights']:
                print(f"  • {insight}")
        print()
        
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