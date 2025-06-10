#!/usr/bin/env python3
"""
Real Agent Coordinator
Coordinates multiple agents using actual symbolic communication
No mock features - only real agent coordination
"""

import time
import json
import threading
import queue
import uuid
from pathlib import Path
from typing import Dict, Any, List, Optional, Set
from dataclasses import dataclass, asdict
from enum import Enum


class AgentStatus(Enum):
    INITIALIZING = "initializing"
    ACTIVE = "active"
    IDLE = "idle"
    BUSY = "busy"
    ERROR = "error"
    STOPPED = "stopped"


@dataclass
class Agent:
    """Real agent representation"""
    id: str
    name: str
    type: str
    status: AgentStatus
    capabilities: List[str]
    current_task: Optional[str] = None
    message_queue: Optional[queue.Queue] = None
    last_heartbeat: float = 0.0
    created_at: float = 0.0
    task_count: int = 0
    error_count: int = 0


@dataclass
class Message:
    """Inter-agent message"""
    id: str
    sender_id: str
    recipient_id: str
    message_type: str
    content: Dict[str, Any]
    timestamp: float
    processed: bool = False


class RealAgentCoordinator:
    """Real coordinator for symbolic agents"""
    
    def __init__(self):
        self.running = False
        self.agents: Dict[str, Agent] = {}
        self.message_queues: Dict[str, queue.Queue] = {}
        self.global_message_queue = queue.Queue()
        self.coordination_patterns = {}
        
        # Symbolic communication space
        self.symbolic_space = {
            "shared_concepts": {},
            "active_goals": {},
            "coordination_state": {},
            "agent_knowledge": {}
        }
        
        # Coordination statistics
        self.stats = {
            "agents_registered": 0,
            "messages_sent": 0,
            "tasks_coordinated": 0,
            "coordination_cycles": 0,
            "errors": 0
        }
        
        # Storage for persistence
        self.storage_path = Path("/tmp/wolfcog_coordination")
        self.storage_path.mkdir(exist_ok=True)
    
    def start(self):
        """Start the agent coordinator"""
        print("🤝 Starting Real Agent Coordinator...")
        self.running = True
        
        # Start coordination loop
        coord_thread = threading.Thread(target=self.coordination_loop)
        coord_thread.daemon = True
        coord_thread.start()
        
        # Start message processing
        msg_thread = threading.Thread(target=self.message_processing_loop)
        msg_thread.daemon = True
        msg_thread.start()
        
        # Start health monitoring
        health_thread = threading.Thread(target=self.health_monitoring_loop)
        health_thread.daemon = True
        health_thread.start()
        
        print("✅ Agent coordinator started")
    
    def register_agent(self, name: str, agent_type: str, capabilities: List[str]) -> str:
        """Register a new agent"""
        agent_id = str(uuid.uuid4())
        
        # Create message queue for agent
        message_queue = queue.Queue()
        self.message_queues[agent_id] = message_queue
        
        # Create agent record
        agent = Agent(
            id=agent_id,
            name=name,
            type=agent_type,
            status=AgentStatus.INITIALIZING,
            capabilities=capabilities,
            message_queue=message_queue,
            last_heartbeat=time.time(),
            created_at=time.time()
        )
        
        self.agents[agent_id] = agent
        self.stats["agents_registered"] += 1
        
        # Initialize agent's knowledge space
        self.symbolic_space["agent_knowledge"][agent_id] = {
            "local_concepts": {},
            "active_intentions": [],
            "collaboration_state": {}
        }
        
        print(f"🤖 Registered agent {name} ({agent_type}) with ID {agent_id}")
        
        # Send welcome message
        self.send_message(
            sender_id="coordinator",
            recipient_id=agent_id,
            message_type="welcome",
            content={"agent_id": agent_id, "capabilities": capabilities}
        )
        
        return agent_id
    
    def send_message(self, sender_id: str, recipient_id: str, message_type: str, content: Dict[str, Any]) -> str:
        """Send a message between agents"""
        message_id = str(uuid.uuid4())
        
        message = Message(
            id=message_id,
            sender_id=sender_id,
            recipient_id=recipient_id,
            message_type=message_type,
            content=content,
            timestamp=time.time()
        )
        
        # Route message
        if recipient_id == "broadcast":
            # Broadcast to all agents
            for agent_id in self.agents.keys():
                if agent_id != sender_id:
                    self.message_queues[agent_id].put(message)
        elif recipient_id in self.message_queues:
            # Send to specific agent
            self.message_queues[recipient_id].put(message)
        else:
            print(f"⚠️ Unknown recipient: {recipient_id}")
            return ""
        
        self.stats["messages_sent"] += 1
        self.global_message_queue.put(message)  # For logging/monitoring
        
        print(f"📨 Message {message_type} sent from {sender_id} to {recipient_id}")
        return message_id
    
    def coordinate_task(self, task_type: str, task_data: Dict[str, Any], required_capabilities: List[str]) -> Optional[str]:
        """Coordinate a task among suitable agents"""
        # Find capable agents
        capable_agents = []
        for agent_id, agent in self.agents.items():
            if (agent.status in [AgentStatus.ACTIVE, AgentStatus.IDLE] and
                any(cap in agent.capabilities for cap in required_capabilities)):
                capable_agents.append(agent_id)
        
        if not capable_agents:
            print(f"⚠️ No capable agents found for task {task_type}")
            return None
        
        # Select best agent (simple selection: least busy)
        selected_agent_id = min(capable_agents, key=lambda aid: self.agents[aid].task_count)
        selected_agent = self.agents[selected_agent_id]
        
        # Assign task
        task_id = str(uuid.uuid4())
        selected_agent.current_task = task_id
        selected_agent.status = AgentStatus.BUSY
        selected_agent.task_count += 1
        
        # Send task assignment
        self.send_message(
            sender_id="coordinator",
            recipient_id=selected_agent_id,
            message_type="task_assignment",
            content={
                "task_id": task_id,
                "task_type": task_type,
                "task_data": task_data,
                "required_capabilities": required_capabilities
            }
        )
        
        self.stats["tasks_coordinated"] += 1
        print(f"📋 Task {task_type} assigned to agent {selected_agent.name}")
        
        return task_id
    
    def coordination_loop(self):
        """Main coordination loop"""
        print("🔄 Starting coordination loop...")
        
        while self.running:
            try:
                # Update coordination state
                self.update_coordination_state()
                
                # Process global goals
                self.process_global_goals()
                
                # Check for collaboration opportunities
                self.check_collaboration_opportunities()
                
                # Update symbolic space
                self.update_symbolic_space()
                
                self.stats["coordination_cycles"] += 1
                time.sleep(1)  # Coordination cycle interval
                
            except Exception as e:
                print(f"❌ Coordination error: {e}")
                self.stats["errors"] += 1
    
    def update_coordination_state(self):
        """Update the global coordination state"""
        active_agents = sum(1 for agent in self.agents.values() 
                          if agent.status in [AgentStatus.ACTIVE, AgentStatus.BUSY])
        
        busy_agents = sum(1 for agent in self.agents.values() 
                         if agent.status == AgentStatus.BUSY)
        
        self.symbolic_space["coordination_state"] = {
            "active_agents": active_agents,
            "busy_agents": busy_agents,
            "load_factor": busy_agents / max(active_agents, 1),
            "last_updated": time.time()
        }
    
    def process_global_goals(self):
        """Process system-wide goals that require coordination"""
        # Example: Load balancing
        coord_state = self.symbolic_space["coordination_state"]
        
        if coord_state.get("load_factor", 0) > 0.8:  # High load
            # Try to redistribute work or spawn new agents
            self.suggest_load_balancing()
        
        # Example: Knowledge sharing
        if len(self.agents) > 1:
            self.facilitate_knowledge_sharing()
    
    def suggest_load_balancing(self):
        """Suggest load balancing among agents"""
        # Find overloaded agents
        overloaded = [agent for agent in self.agents.values() 
                     if agent.task_count > 5]  # Threshold
        
        if overloaded:
            # Send load balancing suggestions
            for agent in overloaded:
                self.send_message(
                    sender_id="coordinator",
                    recipient_id=agent.id,
                    message_type="load_suggestion",
                    content={"suggestion": "consider_task_delegation"}
                )
    
    def facilitate_knowledge_sharing(self):
        """Facilitate knowledge sharing between agents"""
        # Simple knowledge sharing: broadcast interesting concepts
        for agent_id, knowledge in self.symbolic_space["agent_knowledge"].items():
            if knowledge.get("local_concepts"):
                # Share some concepts with other agents
                self.send_message(
                    sender_id=agent_id,
                    recipient_id="broadcast",
                    message_type="knowledge_share",
                    content={"concepts": list(knowledge["local_concepts"].keys())[:3]}
                )
    
    def check_collaboration_opportunities(self):
        """Check for opportunities for agent collaboration"""
        # Find agents with complementary capabilities
        agent_list = list(self.agents.values())
        
        for i, agent1 in enumerate(agent_list):
            for agent2 in agent_list[i+1:]:
                if (agent1.status == AgentStatus.ACTIVE and 
                    agent2.status == AgentStatus.ACTIVE):
                    
                    # Check for complementary capabilities
                    complementary = set(agent1.capabilities) & set(agent2.capabilities)
                    if not complementary and len(agent1.capabilities) + len(agent2.capabilities) > 3:
                        # Suggest collaboration
                        self.suggest_collaboration(agent1.id, agent2.id)
    
    def suggest_collaboration(self, agent1_id: str, agent2_id: str):
        """Suggest collaboration between two agents"""
        collaboration_id = f"collab-{int(time.time())}"
        
        # Send collaboration suggestion to both agents
        for agent_id in [agent1_id, agent2_id]:
            self.send_message(
                sender_id="coordinator",
                recipient_id=agent_id,
                message_type="collaboration_suggestion",
                content={
                    "collaboration_id": collaboration_id,
                    "partner_id": agent2_id if agent_id == agent1_id else agent1_id,
                    "suggested_task": "knowledge_integration"
                }
            )
    
    def update_symbolic_space(self):
        """Update the shared symbolic space"""
        # Aggregate agent knowledge
        all_concepts = set()
        for knowledge in self.symbolic_space["agent_knowledge"].values():
            all_concepts.update(knowledge.get("local_concepts", {}).keys())
        
        self.symbolic_space["shared_concepts"] = {
            concept: {"shared_by": [], "last_updated": time.time()}
            for concept in all_concepts
        }
    
    def message_processing_loop(self):
        """Process global messages for monitoring"""
        while self.running:
            try:
                message = self.global_message_queue.get(timeout=1.0)
                self.process_global_message(message)
            except queue.Empty:
                continue
            except Exception as e:
                print(f"❌ Message processing error: {e}")
    
    def process_global_message(self, message: Message):
        """Process a message for global coordination"""
        # Log important messages
        if message.message_type in ["task_completion", "error", "collaboration_request"]:
            print(f"📨 Global message: {message.message_type} from {message.sender_id}")
        
        # Update agent status based on messages
        if message.message_type == "task_completion" and message.sender_id in self.agents:
            agent = self.agents[message.sender_id]
            agent.status = AgentStatus.ACTIVE
            agent.current_task = None
        
        elif message.message_type == "error" and message.sender_id in self.agents:
            agent = self.agents[message.sender_id]
            agent.error_count += 1
            if agent.error_count > 5:  # Threshold
                agent.status = AgentStatus.ERROR
    
    def health_monitoring_loop(self):
        """Monitor agent health and coordination system health"""
        while self.running:
            try:
                current_time = time.time()
                
                # Check agent heartbeats
                for agent_id, agent in self.agents.items():
                    if current_time - agent.last_heartbeat > 30:  # 30 second timeout
                        if agent.status != AgentStatus.ERROR:
                            print(f"⚠️ Agent {agent.name} appears unresponsive")
                            agent.status = AgentStatus.ERROR
                
                # System health check
                healthy_agents = sum(1 for agent in self.agents.values() 
                                   if agent.status not in [AgentStatus.ERROR, AgentStatus.STOPPED])
                
                if len(self.agents) > 0 and healthy_agents / len(self.agents) < 0.5:
                    print("⚠️ System health warning: >50% agents unhealthy")
                
                time.sleep(10)  # Health check every 10 seconds
                
            except Exception as e:
                print(f"❌ Health monitoring error: {e}")
    
    def agent_heartbeat(self, agent_id: str):
        """Record agent heartbeat"""
        if agent_id in self.agents:
            self.agents[agent_id].last_heartbeat = time.time()
            if self.agents[agent_id].status == AgentStatus.ERROR:
                self.agents[agent_id].status = AgentStatus.ACTIVE
                print(f"✅ Agent {self.agents[agent_id].name} recovered")
    
    def get_agent_messages(self, agent_id: str, timeout: float = 0.1) -> List[Message]:
        """Get pending messages for an agent"""
        messages = []
        if agent_id in self.message_queues:
            queue_obj = self.message_queues[agent_id]
            while True:
                try:
                    message = queue_obj.get(timeout=timeout)
                    messages.append(message)
                except queue.Empty:
                    break
        return messages
    
    def get_coordination_status(self) -> Dict[str, Any]:
        """Get coordination system status"""
        agent_summary = {}
        for agent_id, agent in self.agents.items():
            agent_summary[agent_id] = {
                "name": agent.name,
                "type": agent.type,
                "status": agent.status.value,
                "task_count": agent.task_count,
                "error_count": agent.error_count,
                "capabilities": agent.capabilities
            }
        
        return {
            "running": self.running,
            "agent_count": len(self.agents),
            "agents": agent_summary,
            "coordination_state": self.symbolic_space["coordination_state"],
            "shared_concepts": len(self.symbolic_space["shared_concepts"]),
            "statistics": self.stats
        }
    
    def stop(self):
        """Stop the agent coordinator"""
        print("🛑 Stopping agent coordinator...")
        self.running = False
        
        # Notify all agents
        for agent_id in self.agents.keys():
            self.send_message(
                sender_id="coordinator",
                recipient_id=agent_id,
                message_type="shutdown",
                content={"reason": "coordinator_stopping"}
            )
        
        print("✅ Agent coordinator stopped")


# Simple agent implementation for testing
class TestAgent:
    """Simple test agent"""
    
    def __init__(self, coordinator: RealAgentCoordinator, name: str, agent_type: str, capabilities: List[str]):
        self.coordinator = coordinator
        self.name = name
        self.agent_type = agent_type
        self.capabilities = capabilities
        self.agent_id = None
        self.running = False
    
    def start(self):
        """Start the test agent"""
        self.agent_id = self.coordinator.register_agent(self.name, self.agent_type, self.capabilities)
        self.running = True
        
        # Start message processing loop
        thread = threading.Thread(target=self.message_loop)
        thread.daemon = True
        thread.start()
        
        # Mark as active
        time.sleep(0.1)
        if self.agent_id in self.coordinator.agents:
            self.coordinator.agents[self.agent_id].status = AgentStatus.ACTIVE
    
    def message_loop(self):
        """Process messages"""
        while self.running:
            try:
                messages = self.coordinator.get_agent_messages(self.agent_id, timeout=1.0)
                for message in messages:
                    self.process_message(message)
                
                # Send heartbeat
                self.coordinator.agent_heartbeat(self.agent_id)
                
            except Exception as e:
                print(f"❌ Agent {self.name} error: {e}")
    
    def process_message(self, message: Message):
        """Process a received message"""
        print(f"🤖 Agent {self.name} received {message.message_type}")
        
        if message.message_type == "task_assignment":
            # Simulate task processing
            time.sleep(1)  # Simulate work
            
            # Send completion
            self.coordinator.send_message(
                sender_id=self.agent_id,
                recipient_id="coordinator", 
                message_type="task_completion",
                content={"task_id": message.content.get("task_id"), "result": "completed"}
            )


def main():
    """Test the agent coordinator"""
    coordinator = RealAgentCoordinator()
    coordinator.start()
    
    # Create test agents
    agents = [
        TestAgent(coordinator, "Agent-Alpha", "symbolic_processor", ["pattern_matching", "inference"]),
        TestAgent(coordinator, "Agent-Beta", "knowledge_manager", ["knowledge_query", "learning"]),
        TestAgent(coordinator, "Agent-Gamma", "task_executor", ["task_execution", "planning"])
    ]
    
    print("🤖 Starting test agents...")
    for agent in agents:
        agent.start()
        time.sleep(0.5)
    
    # Submit test coordination tasks
    time.sleep(2)
    print("\n📋 Testing task coordination...")
    
    coordinator.coordinate_task("pattern_analysis", {"pattern": "test"}, ["pattern_matching"])
    coordinator.coordinate_task("knowledge_lookup", {"query": "concept"}, ["knowledge_query"])
    coordinator.coordinate_task("task_planning", {"goal": "optimize"}, ["planning"])
    
    # Let system run
    time.sleep(5)
    
    # Check status
    print(f"\n📊 Coordination Status:")
    status = coordinator.get_coordination_status()
    print(f"  Agents: {status['agent_count']}")
    print(f"  Messages sent: {status['statistics']['messages_sent']}")
    print(f"  Tasks coordinated: {status['statistics']['tasks_coordinated']}")
    
    coordinator.stop()


if __name__ == "__main__":
    main()
