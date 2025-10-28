#!/usr/bin/env python3
"""
Master Node Coordinator for Wolfram EPC

Manages all core services, licensing, and coordinates distributed compute nodes.
The central control point for the Enterprise Private Cloud.
"""

import json
import time
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass


@dataclass
class ComputeNode:
    """Information about a compute node"""
    node_id: str
    host: str
    port: int
    max_kernels: int
    current_load: float = 0.0
    status: str = "online"
    last_heartbeat: float = 0.0


class MasterNode:
    """
    Master Node Coordinator for EPC
    
    The main, all-encompassing node that contains:
    - Core service management
    - License management
    - Compute node coordination
    - Load balancing
    - System monitoring
    """
    
    def __init__(self, config_path: Optional[Path] = None):
        """
        Initialize master node
        
        Args:
            config_path: Path to configuration file
        """
        self.config_path = config_path or Path("/tmp/epc_master_config.json")
        self.data_dir = Path("/tmp/epc_master")
        self.data_dir.mkdir(exist_ok=True, parents=True)
        
        # Core services
        self.compute_engine = None
        self.api_interface = None
        self.auth_system = None
        self.web_interface = None
        self.license_manager = None
        
        # Distributed compute nodes
        self.compute_nodes: Dict[str, ComputeNode] = {}
        
        # System status
        self.status = "initializing"
        self.start_time = time.time()
        
        # Metrics
        self.metrics = {
            "total_requests": 0,
            "active_kernels": 0,
            "total_compute_nodes": 0,
            "system_uptime": 0.0
        }
        
        # Load configuration
        self._load_config()
    
    def _load_config(self):
        """Load master node configuration"""
        if self.config_path.exists():
            try:
                with open(self.config_path) as f:
                    self.config = json.load(f)
            except Exception as e:
                print(f"Warning: Could not load config: {e}")
                self.config = self._get_default_config()
        else:
            self.config = self._get_default_config()
            self._save_config()
    
    def _save_config(self):
        """Save configuration"""
        try:
            with open(self.config_path, 'w') as f:
                json.dump(self.config, f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save config: {e}")
    
    def _get_default_config(self) -> Dict:
        """Get default configuration"""
        return {
            "master_node": {
                "host": "0.0.0.0",
                "port": 8080,
                "max_kernels": 4
            },
            "api_interface": {
                "host": "0.0.0.0",
                "port": 5000
            },
            "web_interface": {
                "host": "0.0.0.0",
                "port": 8888
            },
            "authentication": {
                "require_auth": True,
                "allow_self_provisioning": True
            },
            "licensing": {
                "license_server": "localhost",
                "check_interval": 3600
            },
            "compute_nodes": {
                "auto_discovery": True,
                "heartbeat_interval": 30
            }
        }
    
    def initialize(self, compute_engine, api_interface, auth_system, 
                  web_interface=None, license_manager=None):
        """
        Initialize master node with core services
        
        Args:
            compute_engine: ComputeEngine instance
            api_interface: APIInterface instance
            auth_system: AuthenticationSystem instance
            web_interface: Optional WebInterface instance
            license_manager: Optional LicenseManager instance
        """
        self.compute_engine = compute_engine
        self.api_interface = api_interface
        self.auth_system = auth_system
        self.web_interface = web_interface
        self.license_manager = license_manager
        
        self.status = "initialized"
        
        print("🎯 Master Node initialized")
        print(f"   • Compute Engine: {self.compute_engine.max_kernels} max kernels")
        print(f"   • API Interface: http://{self.api_interface.host}:{self.api_interface.port}")
        print(f"   • Authentication: {len(self.auth_system.users)} users")
    
    def start(self):
        """Start the master node and all services"""
        print("🚀 Starting Master Node...")
        
        self.status = "starting"
        
        # Start compute engine kernels
        if self.compute_engine:
            initial_kernels = min(2, self.compute_engine.max_kernels)
            for i in range(initial_kernels):
                kernel_id = self.compute_engine.start_kernel()
                print(f"   ✓ Started kernel: {kernel_id}")
        
        self.status = "running"
        self.start_time = time.time()
        
        print("✅ Master Node running")
    
    def register_compute_node(self, node_id: str, host: str, port: int, 
                            max_kernels: int) -> ComputeNode:
        """
        Register a distributed compute node
        
        Args:
            node_id: Unique node identifier
            host: Node hostname/IP
            port: Node port
            max_kernels: Maximum kernels for this node
            
        Returns:
            ComputeNode instance
        """
        node = ComputeNode(
            node_id=node_id,
            host=host,
            port=port,
            max_kernels=max_kernels,
            last_heartbeat=time.time()
        )
        
        self.compute_nodes[node_id] = node
        print(f"📡 Registered compute node: {node_id} at {host}:{port}")
        
        return node
    
    def update_node_heartbeat(self, node_id: str, load: float):
        """Update compute node heartbeat and load"""
        if node_id not in self.compute_nodes:
            raise ValueError(f"Unknown compute node: {node_id}")
        
        node = self.compute_nodes[node_id]
        node.last_heartbeat = time.time()
        node.current_load = load
        node.status = "online"
    
    def check_node_health(self):
        """Check health of all compute nodes"""
        current_time = time.time()
        timeout = self.config["compute_nodes"]["heartbeat_interval"] * 2
        
        for node_id, node in self.compute_nodes.items():
            if current_time - node.last_heartbeat > timeout:
                node.status = "offline"
                print(f"⚠️  Compute node offline: {node_id}")
    
    def select_compute_node(self) -> Optional[ComputeNode]:
        """
        Select best compute node for work distribution
        
        Returns:
            ComputeNode with lowest load, or None if none available
        """
        online_nodes = [
            node for node in self.compute_nodes.values()
            if node.status == "online"
        ]
        
        if not online_nodes:
            return None
        
        # Return node with lowest load
        return min(online_nodes, key=lambda n: n.current_load)
    
    def get_system_status(self) -> Dict:
        """Get comprehensive system status"""
        uptime = time.time() - self.start_time if self.start_time else 0
        
        status = {
            "master_node": {
                "status": self.status,
                "uptime": uptime,
                "uptime_formatted": self._format_uptime(uptime)
            },
            "compute_engine": {
                "active_kernels": len(self.compute_engine.kernels) if self.compute_engine else 0,
                "max_kernels": self.compute_engine.max_kernels if self.compute_engine else 0
            },
            "compute_nodes": {
                "total": len(self.compute_nodes),
                "online": sum(1 for n in self.compute_nodes.values() if n.status == "online"),
                "offline": sum(1 for n in self.compute_nodes.values() if n.status == "offline"),
                "nodes": [
                    {
                        "id": node.node_id,
                        "host": node.host,
                        "status": node.status,
                        "load": node.current_load,
                        "max_kernels": node.max_kernels
                    }
                    for node in self.compute_nodes.values()
                ]
            },
            "api_interface": {
                "endpoints": len(self.api_interface.endpoints) if self.api_interface else 0,
                "total_requests": self.api_interface.metrics["total_requests"] if self.api_interface else 0
            },
            "authentication": {
                "total_users": len(self.auth_system.users) if self.auth_system else 0,
                "active_sessions": len(self.auth_system.sessions) if self.auth_system else 0
            },
            "licensing": {
                "status": "valid" if self.license_manager else "not_configured"
            }
        }
        
        return status
    
    def _format_uptime(self, seconds: float) -> str:
        """Format uptime in human-readable form"""
        hours = int(seconds // 3600)
        minutes = int((seconds % 3600) // 60)
        secs = int(seconds % 60)
        
        if hours > 0:
            return f"{hours}h {minutes}m {secs}s"
        elif minutes > 0:
            return f"{minutes}m {secs}s"
        else:
            return f"{secs}s"
    
    def shutdown(self):
        """Shutdown master node and all services"""
        print("🛑 Shutting down Master Node...")
        
        self.status = "shutting_down"
        
        # Shutdown compute engine
        if self.compute_engine:
            self.compute_engine.shutdown()
            print("   ✓ Compute engine shutdown")
        
        # Notify compute nodes
        for node_id in list(self.compute_nodes.keys()):
            print(f"   ✓ Disconnected compute node: {node_id}")
            del self.compute_nodes[node_id]
        
        self.status = "stopped"
        print("✅ Master Node stopped")
