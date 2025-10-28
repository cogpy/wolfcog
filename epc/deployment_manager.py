#!/usr/bin/env python3
"""
Deployment Manager for Wolfram EPC

Manages different deployment options:
- Application host (direct to user)
- Computation center
- Embedded applications (via another service)
- Hosted reporting
"""

import json
import time
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum


class DeploymentType(Enum):
    """Types of deployments supported by EPC"""
    APPLICATION_HOST = "application_host"
    COMPUTATION_CENTER = "computation_center"
    EMBEDDED_APPLICATION = "embedded_application"
    HOSTED_REPORTING = "hosted_reporting"


@dataclass
class Deployment:
    """Deployment configuration"""
    deployment_id: str
    name: str
    deployment_type: DeploymentType
    config: Dict[str, Any]
    status: str = "inactive"
    created_at: float = 0.0
    url: Optional[str] = None


class DeploymentManager:
    """
    EPC Deployment Manager
    
    Handles multiple deployment scenarios:
    1. Application Host - Direct user-facing applications
    2. Computation Center - Backend processing hub
    3. Embedded Application - Integration into other services
    4. Hosted Reporting - Scheduled/triggered reporting
    """
    
    def __init__(self, master_node, data_dir: Optional[Path] = None):
        """
        Initialize deployment manager
        
        Args:
            master_node: MasterNode instance
            data_dir: Directory for deployment data
        """
        self.master_node = master_node
        self.data_dir = data_dir or Path("/tmp/epc_deployments")
        self.data_dir.mkdir(exist_ok=True, parents=True)
        
        self.deployments: Dict[str, Deployment] = {}
        self.deployment_counter = 0
        
        # Load existing deployments
        self._load_deployments()
    
    def _load_deployments(self):
        """Load deployments from storage"""
        deployments_file = self.data_dir / "deployments.json"
        if deployments_file.exists():
            try:
                with open(deployments_file) as f:
                    data = json.load(f)
                    for dep_id, dep_data in data.items():
                        self.deployments[dep_id] = Deployment(
                            deployment_id=dep_data["deployment_id"],
                            name=dep_data["name"],
                            deployment_type=DeploymentType(dep_data["deployment_type"]),
                            config=dep_data["config"],
                            status=dep_data.get("status", "inactive"),
                            created_at=dep_data.get("created_at", time.time()),
                            url=dep_data.get("url")
                        )
            except Exception as e:
                print(f"Warning: Could not load deployments: {e}")
    
    def _save_deployments(self):
        """Save deployments to storage"""
        deployments_file = self.data_dir / "deployments.json"
        try:
            data = {
                dep_id: {
                    "deployment_id": dep.deployment_id,
                    "name": dep.name,
                    "deployment_type": dep.deployment_type.value,
                    "config": dep.config,
                    "status": dep.status,
                    "created_at": dep.created_at,
                    "url": dep.url
                }
                for dep_id, dep in self.deployments.items()
            }
            with open(deployments_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save deployments: {e}")
    
    def deploy_application(self, name: str, notebook_path: str,
                          form_based: bool = False,
                          linguistic_interface: bool = False) -> Deployment:
        """
        Deploy application host (direct to user)
        
        Deploy and distribute a Wolfram Language application direct to any 
        audience at any scale.
        
        Args:
            name: Application name
            notebook_path: Path to Wolfram notebook
            form_based: Enable form-based interface
            linguistic_interface: Enable linguistic interface
            
        Returns:
            Deployment instance
        """
        deployment_id = f"app_{self.deployment_counter}"
        self.deployment_counter += 1
        
        config = {
            "notebook_path": notebook_path,
            "form_based": form_based,
            "linguistic_interface": linguistic_interface,
            "interactive_3d": True,
            "public_access": True
        }
        
        deployment = Deployment(
            deployment_id=deployment_id,
            name=name,
            deployment_type=DeploymentType.APPLICATION_HOST,
            config=config,
            created_at=time.time(),
            url=f"/app/{deployment_id}"
        )
        
        self.deployments[deployment_id] = deployment
        self._save_deployments()
        
        # Deploy the application
        self._activate_deployment(deployment)
        
        return deployment
    
    def deploy_computation_center(self, name: str, 
                                 api_endpoints: List[str]) -> Deployment:
        """
        Deploy computation center
        
        Act as a computational processing hub for data and computations 
        receiving and sending via the Wolfram Language API.
        
        Args:
            name: Center name
            api_endpoints: List of API endpoint names to expose
            
        Returns:
            Deployment instance
        """
        deployment_id = f"compute_{self.deployment_counter}"
        self.deployment_counter += 1
        
        config = {
            "api_endpoints": api_endpoints,
            "processing_mode": "batch",
            "queue_enabled": True,
            "max_concurrent": 10
        }
        
        deployment = Deployment(
            deployment_id=deployment_id,
            name=name,
            deployment_type=DeploymentType.COMPUTATION_CENTER,
            config=config,
            created_at=time.time(),
            url=f"/compute/{deployment_id}"
        )
        
        self.deployments[deployment_id] = deployment
        self._save_deployments()
        
        self._activate_deployment(deployment)
        
        return deployment
    
    def deploy_embedded_application(self, name: str, target_service: str,
                                   iframe_url: Optional[str] = None) -> Deployment:
        """
        Deploy embedded application
        
        Embed a deployed Wolfram Language application into another web 
        environment to incorporate computational and analytical power.
        
        Args:
            name: Application name
            target_service: Target web service
            iframe_url: Optional iframe embedding URL
            
        Returns:
            Deployment instance
        """
        deployment_id = f"embed_{self.deployment_counter}"
        self.deployment_counter += 1
        
        config = {
            "target_service": target_service,
            "iframe_url": iframe_url,
            "cors_enabled": True,
            "embed_mode": "iframe"
        }
        
        deployment = Deployment(
            deployment_id=deployment_id,
            name=name,
            deployment_type=DeploymentType.EMBEDDED_APPLICATION,
            config=config,
            created_at=time.time(),
            url=f"/embed/{deployment_id}"
        )
        
        self.deployments[deployment_id] = deployment
        self._save_deployments()
        
        self._activate_deployment(deployment)
        
        return deployment
    
    def deploy_hosted_reporting(self, name: str, report_notebook: str,
                               schedule: Optional[str] = None,
                               triggers: Optional[List[str]] = None) -> Deployment:
        """
        Deploy hosted reporting
        
        Ad hoc, scheduled or triggered interactive reporting delivered 
        online or through client services.
        
        Args:
            name: Report name
            report_notebook: Path to report notebook
            schedule: Cron-style schedule (e.g., "0 0 * * *")
            triggers: List of trigger conditions
            
        Returns:
            Deployment instance
        """
        deployment_id = f"report_{self.deployment_counter}"
        self.deployment_counter += 1
        
        config = {
            "report_notebook": report_notebook,
            "schedule": schedule,
            "triggers": triggers or [],
            "output_formats": ["pdf", "html", "notebook"],
            "delivery_methods": ["email", "web"]
        }
        
        deployment = Deployment(
            deployment_id=deployment_id,
            name=name,
            deployment_type=DeploymentType.HOSTED_REPORTING,
            config=config,
            created_at=time.time(),
            url=f"/reports/{deployment_id}"
        )
        
        self.deployments[deployment_id] = deployment
        self._save_deployments()
        
        self._activate_deployment(deployment)
        
        return deployment
    
    def _activate_deployment(self, deployment: Deployment):
        """Activate a deployment"""
        print(f"🚀 Activating {deployment.deployment_type.value}: {deployment.name}")
        
        # In production, this would:
        # 1. Create necessary web routes
        # 2. Configure authentication
        # 3. Set up monitoring
        # 4. Initialize resources
        
        deployment.status = "active"
        
        print(f"   ✓ Deployment active at: {deployment.url}")
    
    def stop_deployment(self, deployment_id: str):
        """Stop a deployment"""
        if deployment_id not in self.deployments:
            raise ValueError(f"Unknown deployment: {deployment_id}")
        
        deployment = self.deployments[deployment_id]
        deployment.status = "inactive"
        self._save_deployments()
        
        print(f"🛑 Stopped deployment: {deployment.name}")
    
    def get_deployment_status(self, deployment_id: str) -> Dict:
        """Get deployment status"""
        if deployment_id not in self.deployments:
            raise ValueError(f"Unknown deployment: {deployment_id}")
        
        deployment = self.deployments[deployment_id]
        
        return {
            "deployment_id": deployment.deployment_id,
            "name": deployment.name,
            "type": deployment.deployment_type.value,
            "status": deployment.status,
            "url": deployment.url,
            "created_at": deployment.created_at,
            "uptime": time.time() - deployment.created_at if deployment.status == "active" else 0
        }
    
    def list_deployments(self, deployment_type: Optional[DeploymentType] = None) -> List[Dict]:
        """List all deployments"""
        deployments = self.deployments.values()
        
        if deployment_type:
            deployments = [d for d in deployments if d.deployment_type == deployment_type]
        
        return [
            {
                "deployment_id": d.deployment_id,
                "name": d.name,
                "type": d.deployment_type.value,
                "status": d.status,
                "url": d.url
            }
            for d in deployments
        ]
