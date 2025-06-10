#!/usr/bin/env python3
"""
Real WolfCog Conversational Agent
Production implementation with actual AtomSpace-based communication
Monitors system state and provides real insights without mock features
"""

import time
import json
import threading
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional

# OpenCog imports for real agent communication
try:
    from opencog.atomspace import AtomSpace, types
    from opencog.scheme_wrapper import scheme_eval
    OPENCOG_AVAILABLE = True
except ImportError:
    print("⚠️ OpenCog not available, running in simulation mode")
    OPENCOG_AVAILABLE = False


class RealConversationalAgent:
    """Real conversational agent with AtomSpace-based communication"""
    
    def __init__(self):
        self.running = False
        self.sharedland_path = Path("/tmp/wolfcog_sharedland")
        self.update_interval = 5  # seconds
        
        # Real AtomSpace for agent communication
        if OPENCOG_AVAILABLE:
            self.atomspace = AtomSpace()
            print("✅ Real conversational agent AtomSpace initialized")
        else:
            self.atomspace = None
        
        # Real system knowledge (not mock)
        self.knowledge_base = {
            "system_description": {
                "name": "WolfCog AGI-OS",
                "purpose": "Symbolic operating system for AGI research",
                "architecture": "OpenCog AtomSpace with symbolic processing",
                "capabilities": [
                    "Symbolic reasoning via AtomSpace",
                    "Task processing pipeline",
                    "Agent coordination",
                    "Real-time monitoring"
                ]
            },
            "spaces": {
                "u": {"name": "User Space", "purpose": "User interaction"},
                "e": {"name": "Execution Space", "purpose": "Task processing"},
                "s": {"name": "System Space", "purpose": "System coordination"}
            }
        }
        
        # Current observations
        self.current_observations = []
        
        # Ensure directories exist
        self.ensure_directories()
    
    def ensure_directories(self):
        """Ensure required directories exist"""
        self.sharedland_path.mkdir(exist_ok=True)
        (self.sharedland_path / "conversations").mkdir(exist_ok=True)
        (self.sharedland_path / "observations").mkdir(exist_ok=True)
    
    def start(self, interactive=False):
        """Start the real conversational agent"""
        print("🗣️ Starting Real WolfCog Conversational Agent...")
        print("🔧 Focus: Actual system monitoring and AtomSpace communication")
        self.running = True
        
        if interactive:
            self.interactive_mode()
        else:
            # Start real monitoring
            monitor_thread = threading.Thread(target=self.real_monitoring_loop)
            monitor_thread.daemon = True
            monitor_thread.start()
            print("✅ Real conversational agent active")
            return monitor_thread
    
    def real_monitoring_loop(self):
        """Real monitoring loop without mock features"""
        while self.running:
            try:
                # Monitor actual system state
                observations = self.monitor_real_system()
                
                if observations:
                    self.current_observations = observations
                    self.save_real_observations(observations)
                    
                    # Generate real explanation
                    explanation = self.generate_real_explanation()
                    self.display_real_explanation(explanation)
                
                time.sleep(self.update_interval)
                
            except KeyboardInterrupt:
                print("\n🛑 Real conversational agent stopped")
                break
            except Exception as e:
                print(f"❌ Error in monitoring: {e}")
                time.sleep(self.update_interval)
    
    def monitor_real_system(self) -> List[Dict]:
        """Monitor actual system without mock features"""
        observations = []
        
        # Monitor symbolic spaces for real activity
        for space in ["u", "e", "s"]:
            space_path = Path(f"spaces/{space}")
            if space_path.exists():
                recent_files = []
                cutoff_time = time.time() - 60  # Last minute
                
                for file_path in space_path.rglob("*"):
                    if file_path.is_file() and file_path.stat().st_mtime > cutoff_time:
                        recent_files.append({
                            "file": str(file_path.relative_to(space_path)),
                            "modified": datetime.fromtimestamp(file_path.stat().st_mtime).isoformat(),
                            "size": file_path.stat().st_size
                        })
                
                if recent_files:
                    observations.append({
                        "space": space,
                        "activity_type": "file_updates",
                        "count": len(recent_files),
                        "files": recent_files[:5],  # Limit to 5 files
                        "timestamp": datetime.now().isoformat()
                    })
        
        # Monitor task processing
        task_path = Path("/tmp/ecron_tasks")
        if task_path.exists():
            pending_tasks = len(list(task_path.glob("*.json")))
            processed_tasks = len(list(task_path.glob("*.processed")))
            
            if pending_tasks > 0 or processed_tasks > 0:
                observations.append({
                    "component": "task_processor",
                    "activity_type": "task_processing",
                    "pending": pending_tasks,
                    "processed": processed_tasks,
                    "timestamp": datetime.now().isoformat()
                })
        
        # Monitor AtomSpace if available
        if self.atomspace:
            atom_count = len(self.atomspace.get_atoms_by_type(types.Atom))
            if atom_count > 0:
                observations.append({
                    "component": "atomspace",
                    "activity_type": "symbolic_processing",
                    "atom_count": atom_count,
                    "timestamp": datetime.now().isoformat()
                })
        
        return observations
    
    def generate_real_explanation(self) -> Dict:
        """Generate real explanation without mock features"""
        observations = self.current_observations
        
        if not observations:
            return {
                "timestamp": datetime.now().isoformat(),
                "system_state": "idle",
                "observations": [],
                "summary": "WolfCog system is idle, no recent activity detected",
                "details": []
            }
        
        # Analyze real observations
        space_activities = [obs for obs in observations if "space" in obs]
        task_activities = [obs for obs in observations if obs.get("component") == "task_processor"]
        atomspace_activities = [obs for obs in observations if obs.get("component") == "atomspace"]
        
        summary_parts = []
        details = []
        
        # Real space analysis
        if space_activities:
            active_spaces = [obs["space"] for obs in space_activities]
            total_files = sum(obs["count"] for obs in space_activities)
            summary_parts.append(f"File activity in {len(active_spaces)} spaces ({total_files} files)")
            
            for obs in space_activities:
                space_info = self.knowledge_base["spaces"][obs["space"]]
                details.append(f"Space {obs['space']} ({space_info['name']}): {obs['count']} file updates")
        
        # Real task analysis
        if task_activities:
            task_obs = task_activities[0]
            summary_parts.append(f"Task processing: {task_obs['pending']} pending, {task_obs['processed']} processed")
            details.append(f"Task pipeline active with {task_obs['pending'] + task_obs['processed']} total tasks")
        
        # Real AtomSpace analysis
        if atomspace_activities:
            atom_obs = atomspace_activities[0]
            summary_parts.append(f"Symbolic processing: {atom_obs['atom_count']} atoms in AtomSpace")
            details.append(f"AtomSpace contains {atom_obs['atom_count']} symbolic structures")
        
        return {
            "timestamp": datetime.now().isoformat(),
            "system_state": "active",
            "observations": observations,
            "summary": "; ".join(summary_parts) if summary_parts else "System activity detected",
            "details": details
        }
    
    def display_real_explanation(self, explanation: Dict):
        """Display real explanation without mock features"""
        print("\n" + "="*60)
        print("🗣️ Real WolfCog System Report")
        print("="*60)
        print(f"⏰ {explanation['timestamp']}")
        print(f"🎯 State: {explanation['system_state']}")
        print(f"📋 Summary: {explanation['summary']}")
        
        if explanation['details']:
            print("\n📊 Details:")
            for detail in explanation['details']:
                print(f"  • {detail}")
        
        print("="*60)
    
    def save_real_observations(self, observations: List[Dict]):
        """Save real observations without mock data"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        obs_file = self.sharedland_path / "observations" / f"real_observations_{timestamp}.json"
        
        with open(obs_file, 'w') as f:
            json.dump({
                "timestamp": datetime.now().isoformat(),
                "agent": "RealConversationalAgent",
                "observations": observations,
                "observation_count": len(observations)
            }, f, indent=2)
    
    def interactive_mode(self):
        """Real interactive mode"""
        print("🗣️ Real WolfCog Conversational Agent - Interactive Mode")
        print("Ask about current system state, components, or task processing.")
        print("Type 'exit' to quit, 'status' for current state.")
        print()
        
        while self.running:
            try:
                question = input("❓ Your question: ").strip()
                
                if question.lower() in ['exit', 'quit']:
                    break
                elif question.lower() == 'status':
                    explanation = self.generate_real_explanation()
                    self.display_real_explanation(explanation)
                elif question:
                    answer = self.answer_real_question(question)
                    print(f"\n💬 {answer}")
                    
                    # Save real conversation
                    self.save_real_conversation(question, answer)
                    
            except KeyboardInterrupt:
                print("\n🛑 Exiting interactive mode...")
                break
            except Exception as e:
                print(f"❌ Error: {e}")
    
    def answer_real_question(self, question: str) -> str:
        """Answer questions about real system state"""
        question_lower = question.lower()
        
        if "what is wolfcog" in question_lower:
            return ("WolfCog is a symbolic operating system for AGI research, built on OpenCog AtomSpace. "
                   "It provides task processing, agent coordination, and symbolic reasoning capabilities.")
        
        elif "spaces" in question_lower:
            return ("WolfCog uses three symbolic spaces: u/ (User Space) for interaction, "
                   "e/ (Execution Space) for processing, and s/ (System Space) for coordination.")
        
        elif "tasks" in question_lower:
            observations = self.current_observations
            task_obs = [obs for obs in observations if obs.get("component") == "task_processor"]
            if task_obs:
                obs = task_obs[0]
                return f"Task system has {obs['pending']} pending tasks and {obs['processed']} processed tasks."
            else:
                return "No recent task processing activity detected."
        
        elif "atomspace" in question_lower:
            if self.atomspace:
                atom_count = len(self.atomspace.get_atoms_by_type(types.Atom))
                return f"AtomSpace is active with {atom_count} symbolic structures."
            else:
                return "AtomSpace is not available in this configuration."
        
        elif "status" in question_lower or "state" in question_lower:
            explanation = self.generate_real_explanation()
            return f"System state: {explanation['system_state']}. {explanation['summary']}"
        
        else:
            return ("I can provide information about WolfCog's current state, symbolic spaces, "
                   "task processing, and AtomSpace contents. What would you like to know?")
    
    def save_real_conversation(self, question: str, answer: str):
        """Save real conversation without mock data"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        conv_file = self.sharedland_path / "conversations" / f"real_conversation_{timestamp}.json"
        
        with open(conv_file, 'w') as f:
            json.dump({
                "timestamp": datetime.now().isoformat(),
                "agent": "RealConversationalAgent",
                "question": question,
                "answer": answer,
                "interaction_type": "real_conversation"
            }, f, indent=2)
    
    def communicate_via_atomspace(self, message: str, recipient: str = "system"):
        """Real communication via AtomSpace"""
        if not self.atomspace:
            return False
        
        try:
            # Create communication atoms
            sender_node = self.atomspace.add_node(types.ConceptNode, "conversational_agent")
            recipient_node = self.atomspace.add_node(types.ConceptNode, recipient)
            message_node = self.atomspace.add_node(types.ConceptNode, message)
            
            # Create communication link
            comm_link = self.atomspace.add_link(types.EvaluationLink, [
                self.atomspace.add_node(types.PredicateNode, "communicates"),
                self.atomspace.add_link(types.ListLink, [sender_node, recipient_node, message_node])
            ])
            
            print(f"📡 Sent message via AtomSpace: {message[:50]}...")
            return True
            
        except Exception as e:
            print(f"❌ Error in AtomSpace communication: {e}")
            return False
    
    def get_real_status(self) -> Dict:
        """Get real agent status"""
        return {
            "agent": "RealConversationalAgent",
            "running": self.running,
            "opencog_available": OPENCOG_AVAILABLE,
            "atomspace_size": len(self.atomspace.get_atoms_by_type(types.Atom)) if self.atomspace else 0,
            "recent_observations": len(self.current_observations),
            "monitoring_interval": self.update_interval
        }
    
    def stop(self):
        """Stop the real conversational agent"""
        print("🛑 Stopping Real Conversational Agent...")
        self.running = False
        print("✅ Real Conversational Agent stopped")


def main():
    """Main function for standalone execution"""
    import sys
    
    agent = RealConversationalAgent()
    
    # Check for interactive mode
    interactive = len(sys.argv) > 1 and sys.argv[1] == '--interactive'
    
    try:
        if interactive:
            agent.start(interactive=True)
        else:
            # Start monitoring
            monitor_thread = agent.start()
            
            print("🗣️ Real Conversational Agent running...")
            print("🔧 Monitoring: Actual system state")
            print("📡 Communication: Via AtomSpace")
            print("Press Ctrl+C to stop")
            
            # Keep running until interrupted
            while agent.running:
                time.sleep(1)
                
    except KeyboardInterrupt:
        print("\n🛑 Shutting down real conversational agent...")
        agent.stop()


if __name__ == "__main__":
    main()
