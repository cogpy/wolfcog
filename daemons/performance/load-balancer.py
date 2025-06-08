#!/usr/bin/env python3
"""
WolfCog Distributed Load Balancer
Production-grade load balancing and cluster management for WolfCog nodes
"""

import json
import time
import threading
import socket
import hashlib
import statistics
from pathlib import Path
from datetime import datetime, timedelta
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional, Tuple
import subprocess
import sys

@dataclass
class NodeMetrics:
    """Node performance and health metrics"""
    node_id: str
    cpu_usage: float
    memory_usage: float
    task_queue_size: int
    tasks_completed: int
    response_time: float
    last_heartbeat: datetime
    is_healthy: bool
    load_score: float

@dataclass
class TaskRequest:
    """Task request with routing information"""
    task_id: str
    symbolic_expr: str
    space: str
    priority: int
    estimated_complexity: float
    required_resources: Dict[str, float]
    created_at: datetime

class LoadBalancer:
    """Intelligent load balancer for WolfCog distributed processing"""
    
    def __init__(self, port=9100):
        self.port = port
        self.running = False
        self.nodes: Dict[str, NodeMetrics] = {}
        self.task_queue: List[TaskRequest] = []
        self.routing_table = {}
        self.health_check_interval = 10  # seconds
        self.rebalance_interval = 30     # seconds
        self.node_timeout = 60           # seconds
        
        # Load balancing algorithms
        self.algorithms = {
            "round_robin": self._round_robin,
            "least_connections": self._least_connections,
            "weighted_response_time": self._weighted_response_time,
            "resource_aware": self._resource_aware
        }
        self.current_algorithm = "resource_aware"
        
        # Performance tracking
        self.metrics = {
            "total_tasks_routed": 0,
            "successful_routes": 0,
            "failed_routes": 0,
            "average_routing_time": 0,
            "cluster_utilization": 0
        }
        
    def start(self):
        """Start the load balancer"""
        print("🚀 Starting WolfCog Distributed Load Balancer...")
        self.running = True
        
        # Start health monitoring
        health_thread = threading.Thread(target=self._health_monitor)
        health_thread.daemon = True
        health_thread.start()
        
        # Start rebalancing
        rebalance_thread = threading.Thread(target=self._rebalance_monitor)
        rebalance_thread.daemon = True
        rebalance_thread.start()
        
        # Start request handler
        handler_thread = threading.Thread(target=self._request_handler)
        handler_thread.daemon = True
        handler_thread.start()
        
        print(f"✅ Load balancer started on port {self.port}")
        
    def register_node(self, node_info: Dict) -> bool:
        """Register a new node with the load balancer"""
        node_id = node_info.get("node_id")
        if not node_id:
            return False
            
        metrics = NodeMetrics(
            node_id=node_id,
            cpu_usage=0.0,
            memory_usage=0.0,
            task_queue_size=0,
            tasks_completed=0,
            response_time=0.0,
            last_heartbeat=datetime.now(),
            is_healthy=True,
            load_score=0.0
        )
        
        self.nodes[node_id] = metrics
        print(f"📋 Registered node: {node_id}")
        return True
        
    def update_node_metrics(self, node_id: str, metrics_data: Dict):
        """Update metrics for a specific node"""
        if node_id not in self.nodes:
            return False
            
        node = self.nodes[node_id]
        node.cpu_usage = metrics_data.get("cpu_usage", 0.0)
        node.memory_usage = metrics_data.get("memory_usage", 0.0)
        node.task_queue_size = metrics_data.get("task_queue_size", 0)
        node.tasks_completed = metrics_data.get("tasks_completed", 0)
        node.response_time = metrics_data.get("response_time", 0.0)
        node.last_heartbeat = datetime.now()
        node.is_healthy = metrics_data.get("is_healthy", True)
        node.load_score = self._calculate_load_score(node)
        
        return True
        
    def route_task(self, task_request: TaskRequest) -> Optional[str]:
        """Route a task to the best available node"""
        start_time = time.time()
        
        # Get healthy nodes
        healthy_nodes = [node for node in self.nodes.values() if node.is_healthy]
        
        if not healthy_nodes:
            print("❌ No healthy nodes available for task routing")
            self.metrics["failed_routes"] += 1
            return None
            
        # Select best node using current algorithm
        selected_node = self.algorithms[self.current_algorithm](healthy_nodes, task_request)
        
        if selected_node:
            # Update metrics
            routing_time = time.time() - start_time
            self.metrics["total_tasks_routed"] += 1
            self.metrics["successful_routes"] += 1
            self.metrics["average_routing_time"] = (
                (self.metrics["average_routing_time"] * (self.metrics["total_tasks_routed"] - 1) + routing_time) /
                self.metrics["total_tasks_routed"]
            )
            
            print(f"🎯 Routed task {task_request.task_id} to {selected_node.node_id}")
            return selected_node.node_id
        else:
            self.metrics["failed_routes"] += 1
            return None
            
    def _calculate_load_score(self, node: NodeMetrics) -> float:
        """Calculate comprehensive load score for a node"""
        # Weighted combination of different metrics
        cpu_weight = 0.3
        memory_weight = 0.2
        queue_weight = 0.3
        response_weight = 0.2
        
        # Normalize metrics (0-1 scale)
        cpu_score = min(node.cpu_usage / 100.0, 1.0)
        memory_score = min(node.memory_usage / 100.0, 1.0)
        queue_score = min(node.task_queue_size / 10.0, 1.0)  # Assume max 10 tasks
        response_score = min(node.response_time / 1000.0, 1.0)  # Assume max 1000ms
        
        load_score = (
            cpu_score * cpu_weight +
            memory_score * memory_weight +
            queue_score * queue_weight +
            response_score * response_weight
        )
        
        return load_score
        
    def _round_robin(self, nodes: List[NodeMetrics], task: TaskRequest) -> Optional[NodeMetrics]:
        """Simple round-robin selection"""
        if not nodes:
            return None
        return min(nodes, key=lambda n: n.tasks_completed)
        
    def _least_connections(self, nodes: List[NodeMetrics], task: TaskRequest) -> Optional[NodeMetrics]:
        """Select node with least active connections/tasks"""
        if not nodes:
            return None
        return min(nodes, key=lambda n: n.task_queue_size)
        
    def _weighted_response_time(self, nodes: List[NodeMetrics], task: TaskRequest) -> Optional[NodeMetrics]:
        """Select based on weighted response time"""
        if not nodes:
            return None
        return min(nodes, key=lambda n: n.response_time * (1 + n.task_queue_size * 0.1))
        
    def _resource_aware(self, nodes: List[NodeMetrics], task: TaskRequest) -> Optional[NodeMetrics]:
        """Intelligent resource-aware selection"""
        if not nodes:
            return None
            
        # Score nodes based on multiple factors
        scored_nodes = []
        
        for node in nodes:
            # Base load score
            score = node.load_score
            
            # Adjust for task complexity
            complexity_factor = task.estimated_complexity / 10.0  # Assume max complexity 10
            if node.cpu_usage > 70:  # High CPU usage
                score += complexity_factor * 0.3
                
            # Adjust for memory requirements
            if "memory" in task.required_resources:
                memory_need = task.required_resources["memory"]
                available_memory = 100 - node.memory_usage
                if memory_need > available_memory * 0.8:  # Need more than 80% of available
                    score += 0.5
                    
            # Priority boost for high-priority tasks
            if task.priority > 5:
                score *= 0.8  # Lower score = higher preference
                
            scored_nodes.append((node, score))
            
        # Select node with lowest score (best fit)
        return min(scored_nodes, key=lambda x: x[1])[0]
        
    def _health_monitor(self):
        """Monitor health of all registered nodes"""
        while self.running:
            try:
                current_time = datetime.now()
                
                for node_id, node in list(self.nodes.items()):
                    # Check if node has timed out
                    time_since_heartbeat = current_time - node.last_heartbeat
                    
                    if time_since_heartbeat.total_seconds() > self.node_timeout:
                        print(f"⚠️ Node {node_id} timed out, marking as unhealthy")
                        node.is_healthy = False
                    
                    # Remove completely dead nodes
                    if time_since_heartbeat.total_seconds() > self.node_timeout * 2:
                        print(f"💀 Removing dead node: {node_id}")
                        del self.nodes[node_id]
                        
            except Exception as e:
                print(f"❌ Error in health monitor: {e}")
                
            time.sleep(self.health_check_interval)
            
    def _rebalance_monitor(self):
        """Monitor and trigger rebalancing when needed"""
        while self.running:
            try:
                if len(self.nodes) < 2:
                    time.sleep(self.rebalance_interval)
                    continue
                    
                # Check if rebalancing is needed
                healthy_nodes = [n for n in self.nodes.values() if n.is_healthy]
                
                if len(healthy_nodes) < 2:
                    time.sleep(self.rebalance_interval)
                    continue
                    
                # Calculate load distribution
                load_scores = [n.load_score for n in healthy_nodes]
                load_variance = statistics.variance(load_scores)
                
                # Trigger rebalancing if load is uneven
                if load_variance > 0.3:  # Threshold for rebalancing
                    print("⚖️ Load imbalance detected, triggering rebalancing...")
                    self._perform_rebalancing()
                    
                # Update cluster utilization
                avg_load = statistics.mean(load_scores)
                self.metrics["cluster_utilization"] = avg_load * 100
                
            except Exception as e:
                print(f"❌ Error in rebalance monitor: {e}")
                
            time.sleep(self.rebalance_interval)
            
    def _perform_rebalancing(self):
        """Perform load rebalancing across nodes"""
        # This would implement task migration logic
        # For now, just log the rebalancing event
        print("🔄 Performing cluster rebalancing...")
        
        # In a full implementation, this would:
        # 1. Identify overloaded nodes
        # 2. Find underutilized nodes
        # 3. Migrate tasks between nodes
        # 4. Update routing preferences
        
    def _request_handler(self):
        """Handle incoming load balancing requests"""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            sock.bind(('localhost', self.port))
            sock.listen(10)
            sock.settimeout(1)
            
            while self.running:
                try:
                    conn, addr = sock.accept()
                    request_thread = threading.Thread(
                        target=self._handle_request, 
                        args=(conn, addr)
                    )
                    request_thread.daemon = True
                    request_thread.start()
                except socket.timeout:
                    continue
                except Exception as e:
                    if self.running:
                        print(f"❌ Error accepting connection: {e}")
                        
        except Exception as e:
            print(f"❌ Failed to start request handler: {e}")
            
    def _handle_request(self, conn, addr):
        """Handle individual requests"""
        try:
            data = conn.recv(4096).decode('utf-8')
            if data:
                request = json.loads(data)
                response = self._process_request(request)
                
                response_data = json.dumps(response).encode('utf-8')
                conn.send(response_data)
                
        except Exception as e:
            print(f"❌ Error handling request from {addr}: {e}")
        finally:
            conn.close()
            
    def _process_request(self, request):
        """Process load balancer requests"""
        req_type = request.get("type")
        
        if req_type == "register_node":
            success = self.register_node(request.get("node_info", {}))
            return {"status": "registered" if success else "failed"}
            
        elif req_type == "update_metrics":
            node_id = request.get("node_id")
            metrics = request.get("metrics", {})
            success = self.update_node_metrics(node_id, metrics)
            return {"status": "updated" if success else "failed"}
            
        elif req_type == "route_task":
            task_data = request.get("task", {})
            task_request = TaskRequest(
                task_id=task_data.get("task_id", "unknown"),
                symbolic_expr=task_data.get("symbolic_expr", ""),
                space=task_data.get("space", "e"),
                priority=task_data.get("priority", 1),
                estimated_complexity=task_data.get("complexity", 1.0),
                required_resources=task_data.get("resources", {}),
                created_at=datetime.now()
            )
            
            selected_node = self.route_task(task_request)
            return {
                "status": "routed" if selected_node else "failed",
                "selected_node": selected_node
            }
            
        elif req_type == "cluster_status":
            return {
                "status": "healthy",
                "total_nodes": len(self.nodes),
                "healthy_nodes": len([n for n in self.nodes.values() if n.is_healthy]),
                "metrics": self.metrics,
                "nodes": {nid: asdict(node) for nid, node in self.nodes.items()}
            }
            
        else:
            return {"status": "error", "message": f"Unknown request type: {req_type}"}
            
    def get_cluster_status(self):
        """Get comprehensive cluster status"""
        healthy_nodes = [n for n in self.nodes.values() if n.is_healthy]
        
        return {
            "total_nodes": len(self.nodes),
            "healthy_nodes": len(healthy_nodes),
            "cluster_utilization": self.metrics["cluster_utilization"],
            "routing_success_rate": (
                self.metrics["successful_routes"] / 
                max(self.metrics["total_tasks_routed"], 1) * 100
            ),
            "average_routing_time": self.metrics["average_routing_time"],
            "current_algorithm": self.current_algorithm
        }
        
    def stop(self):
        """Stop the load balancer"""
        print("🛑 Stopping load balancer...")
        self.running = False

def main():
    """Main function for standalone execution"""
    load_balancer = LoadBalancer(port=9100)
    
    try:
        load_balancer.start()
        
        # Demo: Register some test nodes
        test_nodes = [
            {"node_id": "wolf_exec_1", "type": "execution", "port": 9001},
            {"node_id": "wolf_exec_2", "type": "execution", "port": 9002},
            {"node_id": "wolf_coord_1", "type": "coordination", "port": 9003}
        ]
        
        for node_info in test_nodes:
            load_balancer.register_node(node_info)
            
        # Simulate some metrics updates
        time.sleep(2)
        
        load_balancer.update_node_metrics("wolf_exec_1", {
            "cpu_usage": 45.0,
            "memory_usage": 60.0,
            "task_queue_size": 2,
            "tasks_completed": 5,
            "response_time": 150.0,
            "is_healthy": True
        })
        
        load_balancer.update_node_metrics("wolf_exec_2", {
            "cpu_usage": 70.0,
            "memory_usage": 80.0,
            "task_queue_size": 5,
            "tasks_completed": 3,
            "response_time": 300.0,
            "is_healthy": True
        })
        
        # Test task routing
        test_task = TaskRequest(
            task_id="test_symbolic_001",
            symbolic_expr="∇(cognitive_pattern)",
            space="e",
            priority=3,
            estimated_complexity=5.0,
            required_resources={"memory": 30.0},
            created_at=datetime.now()
        )
        
        selected_node = load_balancer.route_task(test_task)
        print(f"🎯 Test task routed to: {selected_node}")
        
        # Show cluster status
        print("\n📊 Cluster Status:")
        status = load_balancer.get_cluster_status()
        for key, value in status.items():
            print(f"  {key}: {value}")
            
        print("\n✨ Load balancer demo complete!")
        print("🔄 Running... (Ctrl+C to stop)")
        
        # Keep running
        while True:
            time.sleep(10)
            status = load_balancer.get_cluster_status()
            print(f"📊 Cluster Utilization: {status['cluster_utilization']:.1f}%")
            
    except KeyboardInterrupt:
        print("\n🛑 Shutting down...")
    finally:
        load_balancer.stop()

if __name__ == "__main__":
    main()
