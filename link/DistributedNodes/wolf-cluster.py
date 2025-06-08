#!/usr/bin/env python3
"""
WolfCog Distributed Nodes - Multi-node deployment capability
Deploy /e/ on remote systems, enable task sharding across nodes
Implement symbolic inter-node messaging via meta-shellwalker
"""

import json
import socket
import threading
import subprocess
import time
from pathlib import Path
from datetime import datetime
import hashlib

class WolfNode:
    """Individual WolfCog node for distributed processing"""
    
    def __init__(self, node_id, node_type="execution", port=None):
        self.node_id = node_id
        self.node_type = node_type  # "execution", "coordination", "storage"
        self.port = port or self.get_available_port()
        self.running = False
        self.connected_nodes = {}
        self.task_queue = []
        self.symbolic_memory = {}
        self.message_handlers = {}
        self.setup_message_handlers()
        
    def get_available_port(self):
        """Get an available port for the node"""
        sock = socket.socket()
        sock.bind(('', 0))
        port = sock.getsockname()[1]
        sock.close()
        return port
        
    def setup_message_handlers(self):
        """Setup handlers for different message types"""
        self.message_handlers = {
            "task_shard": self.handle_task_shard,
            "symbolic_sync": self.handle_symbolic_sync,
            "node_discovery": self.handle_node_discovery,
            "health_check": self.handle_health_check,
            "memory_sync": self.handle_memory_sync
        }
        
    def start(self):
        """Start the WolfNode"""
        print(f"🐺 Starting WolfNode {self.node_id} ({self.node_type}) on port {self.port}")
        self.running = True
        
        # Start message listener
        listener_thread = threading.Thread(target=self.message_listener)
        listener_thread.daemon = True
        listener_thread.start()
        
        # Start task processor
        processor_thread = threading.Thread(target=self.task_processor)
        processor_thread.daemon = True
        processor_thread.start()
        
        print(f"✅ WolfNode {self.node_id} started and ready for distributed processing")
        
    def stop(self):
        """Stop the WolfNode"""
        print(f"🛑 Stopping WolfNode {self.node_id}")
        self.running = False
        
    def message_listener(self):
        """Listen for incoming messages from other nodes"""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            sock.bind(('localhost', self.port))
            sock.listen(5)
            sock.settimeout(1)  # Non-blocking with timeout
            
            while self.running:
                try:
                    conn, addr = sock.accept()
                    message_thread = threading.Thread(target=self.handle_connection, args=(conn, addr))
                    message_thread.daemon = True
                    message_thread.start()
                except socket.timeout:
                    continue
                except Exception as e:
                    if self.running:
                        print(f"❌ Error in message listener: {e}")
                        
        except Exception as e:
            print(f"❌ Failed to start message listener: {e}")
            
    def handle_connection(self, conn, addr):
        """Handle incoming connection"""
        try:
            data = conn.recv(4096).decode('utf-8')
            if data:
                message = json.loads(data)
                response = self.process_message(message)
                
                response_data = json.dumps(response).encode('utf-8')
                conn.send(response_data)
                
        except Exception as e:
            print(f"❌ Error handling connection from {addr}: {e}")
        finally:
            conn.close()
            
    def process_message(self, message):
        """Process incoming message"""
        msg_type = message.get("type", "unknown")
        
        if msg_type in self.message_handlers:
            return self.message_handlers[msg_type](message)
        else:
            return {"status": "error", "message": f"Unknown message type: {msg_type}"}
            
    def send_message(self, target_node, message):
        """Send message to another node"""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(('localhost', target_node['port']))
            
            message_data = json.dumps(message).encode('utf-8')
            sock.send(message_data)
            
            response_data = sock.recv(4096).decode('utf-8')
            response = json.loads(response_data)
            
            sock.close()
            return response
            
        except Exception as e:
            print(f"❌ Error sending message to {target_node['id']}: {e}")
            return {"status": "error", "message": str(e)}
            
    def handle_task_shard(self, message):
        """Handle incoming task shard"""
        task_shard = message.get("task_shard", {})
        
        print(f"📋 Received task shard: {task_shard.get('shard_id', 'unknown')}")
        
        # Add to task queue
        self.task_queue.append({
            "shard": task_shard,
            "received": datetime.now().isoformat(),
            "status": "queued"
        })
        
        return {"status": "accepted", "node_id": self.node_id}
        
    def handle_symbolic_sync(self, message):
        """Handle symbolic memory synchronization"""
        sync_data = message.get("symbolic_data", {})
        
        print(f"🔄 Syncing symbolic data from node {message.get('from_node', 'unknown')}")
        
        # Merge symbolic memory
        for key, value in sync_data.items():
            self.symbolic_memory[key] = value
            
        return {"status": "synced", "node_id": self.node_id, "memory_size": len(self.symbolic_memory)}
        
    def handle_node_discovery(self, message):
        """Handle node discovery request"""
        requesting_node = message.get("node_info", {})
        
        print(f"🔍 Node discovery from {requesting_node.get('id', 'unknown')}")
        
        # Add to connected nodes
        if requesting_node.get('id'):
            self.connected_nodes[requesting_node['id']] = requesting_node
            
        return {
            "status": "discovered",
            "node_info": {
                "id": self.node_id,
                "type": self.node_type,
                "port": self.port,
                "capabilities": self.get_capabilities()
            }
        }
        
    def handle_health_check(self, message):
        """Handle health check request"""
        return {
            "status": "healthy",
            "node_id": self.node_id,
            "timestamp": datetime.now().isoformat(),
            "task_queue_size": len(self.task_queue),
            "memory_size": len(self.symbolic_memory)
        }
        
    def handle_memory_sync(self, message):
        """Handle memory synchronization request"""
        return {
            "status": "sync_ready",
            "symbolic_memory": self.symbolic_memory,
            "node_id": self.node_id
        }
        
    def get_capabilities(self):
        """Get node capabilities"""
        return {
            "task_processing": True,
            "symbolic_memory": True,
            "inter_node_messaging": True,
            "shard_distribution": self.node_type == "coordination"
        }
        
    def task_processor(self):
        """Process tasks from the queue"""
        while self.running:
            if self.task_queue:
                task = self.task_queue.pop(0)
                self.process_task_shard(task)
            else:
                time.sleep(1)
                
    def process_task_shard(self, task):
        """Process a task shard"""
        shard = task["shard"]
        print(f"⚡ Processing task shard: {shard.get('shard_id', 'unknown')}")
        
        # Simulate processing
        time.sleep(0.5)
        
        # Mark as complete
        task["status"] = "completed"
        task["completed"] = datetime.now().isoformat()
        
        print(f"✅ Completed task shard: {shard.get('shard_id', 'unknown')}")

class DistributedWolfCog:
    """Distributed WolfCog coordinator for multi-node deployment"""
    
    def __init__(self):
        self.nodes = {}
        self.running = False
        self.coordination_port = 9000
        self.node_counter = 0
        
    def create_node(self, node_type="execution"):
        """Create a new WolfNode"""
        self.node_counter += 1
        node_id = f"wolf_{node_type}_{self.node_counter}"
        
        node = WolfNode(node_id, node_type)
        self.nodes[node_id] = node
        
        print(f"🏗️ Created {node_type} node: {node_id}")
        return node
        
    def start_cluster(self, execution_nodes=2, coordination_nodes=1):
        """Start a distributed WolfCog cluster"""
        print(f"🚀 Starting distributed WolfCog cluster...")
        print(f"   Execution nodes: {execution_nodes}")
        print(f"   Coordination nodes: {coordination_nodes}")
        
        self.running = True
        
        # Create coordination nodes
        for _ in range(coordination_nodes):
            coord_node = self.create_node("coordination")
            coord_node.start()
            
        # Create execution nodes
        for _ in range(execution_nodes):
            exec_node = self.create_node("execution")
            exec_node.start()
            
        # Allow nodes to discover each other
        time.sleep(1)
        self.perform_node_discovery()
        
        print(f"✅ Distributed cluster started with {len(self.nodes)} nodes")
        
    def perform_node_discovery(self):
        """Perform node discovery across the cluster"""
        print("🔍 Performing node discovery...")
        
        node_list = list(self.nodes.values())
        
        for node in node_list:
            for other_node in node_list:
                if node.node_id != other_node.node_id:
                    discovery_message = {
                        "type": "node_discovery",
                        "node_info": {
                            "id": node.node_id,
                            "type": node.node_type,
                            "port": node.port
                        }
                    }
                    
                    response = node.send_message(
                        {"id": other_node.node_id, "port": other_node.port},
                        discovery_message
                    )
                    
                    if response.get("status") == "discovered":
                        print(f"🤝 {node.node_id} discovered {other_node.node_id}")
                        
    def distribute_task(self, task, shard_count=None):
        """Distribute a task across execution nodes"""
        execution_nodes = [node for node in self.nodes.values() if node.node_type == "execution"]
        
        if not execution_nodes:
            print("❌ No execution nodes available for task distribution")
            return False
            
        shard_count = shard_count or len(execution_nodes)
        
        print(f"📋 Distributing task across {shard_count} shards...")
        
        # Create task shards
        shards = self.create_task_shards(task, shard_count)
        
        # Distribute shards to nodes
        for i, shard in enumerate(shards):
            target_node = execution_nodes[i % len(execution_nodes)]
            
            shard_message = {
                "type": "task_shard",
                "task_shard": shard
            }
            
            response = target_node.send_message(
                {"id": target_node.node_id, "port": target_node.port},
                shard_message
            )
            
            if response.get("status") == "accepted":
                print(f"✅ Shard {shard['shard_id']} assigned to {target_node.node_id}")
            else:
                print(f"❌ Failed to assign shard {shard['shard_id']} to {target_node.node_id}")
                
        return True
        
    def create_task_shards(self, task, shard_count):
        """Create task shards for distribution"""
        shards = []
        
        for i in range(shard_count):
            shard = {
                "shard_id": f"{task.get('id', 'unknown')}_shard_{i+1}",
                "parent_task": task.get('id', 'unknown'),
                "shard_index": i,
                "total_shards": shard_count,
                "symbolic_operation": task.get('symbolic', 'process'),
                "space": task.get('space', 'e'),
                "data_subset": f"subset_{i+1}_of_{shard_count}",
                "created": datetime.now().isoformat()
            }
            shards.append(shard)
            
        return shards
        
    def synchronize_symbolic_memory(self):
        """Synchronize symbolic memory across nodes"""
        print("🔄 Synchronizing symbolic memory across cluster...")
        
        # Collect memory from all nodes
        all_memory = {}
        
        for node in self.nodes.values():
            memory_message = {"type": "memory_sync"}
            response = node.send_message(
                {"id": node.node_id, "port": node.port},
                memory_message
            )
            
            if response.get("status") == "sync_ready":
                node_memory = response.get("symbolic_memory", {})
                all_memory.update(node_memory)
                
        # Distribute unified memory to all nodes
        for node in self.nodes.values():
            sync_message = {
                "type": "symbolic_sync",
                "symbolic_data": all_memory,
                "from_node": "coordinator"
            }
            
            response = node.send_message(
                {"id": node.node_id, "port": node.port},
                sync_message
            )
            
            if response.get("status") == "synced":
                print(f"✅ Memory synced to {node.node_id}")
                
    def get_cluster_status(self):
        """Get status of the distributed cluster"""
        status = {
            "total_nodes": len(self.nodes),
            "execution_nodes": len([n for n in self.nodes.values() if n.node_type == "execution"]),
            "coordination_nodes": len([n for n in self.nodes.values() if n.node_type == "coordination"]),
            "healthy_nodes": 0,
            "total_task_queue": 0,
            "nodes": {}
        }
        
        for node in self.nodes.values():
            health_message = {"type": "health_check"}
            response = node.send_message(
                {"id": node.node_id, "port": node.port},
                health_message
            )
            
            if response.get("status") == "healthy":
                status["healthy_nodes"] += 1
                status["total_task_queue"] += response.get("task_queue_size", 0)
                
            status["nodes"][node.node_id] = {
                "type": node.node_type,
                "port": node.port,
                "health": response.get("status", "unknown"),
                "task_queue": response.get("task_queue_size", 0)
            }
            
        return status
        
    def stop_cluster(self):
        """Stop the distributed cluster"""
        print("🛑 Stopping distributed WolfCog cluster...")
        
        for node in self.nodes.values():
            node.stop()
            
        self.running = False
        print("✅ Cluster stopped")

def main():
    """Main function for standalone execution"""
    distributed_wolfcog = DistributedWolfCog()
    
    try:
        # Start cluster
        distributed_wolfcog.start_cluster(execution_nodes=3, coordination_nodes=1)
        
        time.sleep(2)
        
        # Show cluster status
        print("\n📊 Cluster Status:")
        status = distributed_wolfcog.get_cluster_status()
        for key, value in status.items():
            if key != "nodes":
                print(f"  {key}: {value}")
        print()
        
        # Distribute a test task
        test_task = {
            "id": "symbolic_processing_001",
            "symbolic": "∇(cognitive_pattern)",
            "space": "e",
            "action": "evolve"
        }
        
        distributed_wolfcog.distribute_task(test_task, shard_count=3)
        
        # Wait for processing
        time.sleep(3)
        
        # Synchronize memory
        distributed_wolfcog.synchronize_symbolic_memory()
        
        # Final status
        print("\n📊 Final Cluster Status:")
        status = distributed_wolfcog.get_cluster_status()
        for key, value in status.items():
            if key != "nodes":
                print(f"  {key}: {value}")
        print()
        
        print("✨ Distributed WolfCog demonstration complete!")
        
        # Keep running for a bit
        time.sleep(2)
        
    except KeyboardInterrupt:
        print("\n🛑 Shutting down cluster...")
    finally:
        distributed_wolfcog.stop_cluster()

if __name__ == "__main__":
    main()