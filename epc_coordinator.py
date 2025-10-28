#!/usr/bin/env python3
"""
Wolfram Enterprise Private Cloud Coordinator

Main entry point for EPC infrastructure. Coordinates all components:
- Compute Engine
- API Interface
- Authentication System
- Master Node
- Deployment Manager
- Web Interface
"""

import sys
import time
import signal
from pathlib import Path
from typing import Optional

# Import EPC components
from epc.compute_engine import ComputeEngine
from epc.api_interface import APIInterface
from epc.authentication import AuthenticationSystem
from epc.master_node import MasterNode
from epc.deployment_manager import DeploymentManager


class EPCCoordinator:
    """
    Main coordinator for Wolfram Enterprise Private Cloud
    
    Manages the complete EPC infrastructure including:
    - Compute engine with kernel pool
    - API deployment system
    - User authentication
    - Master node coordination
    - Deployment options
    """
    
    def __init__(self, config_path: Optional[Path] = None):
        """
        Initialize EPC coordinator
        
        Args:
            config_path: Path to configuration file
        """
        self.config_path = config_path
        self.running = False
        
        # Core components
        self.compute_engine = None
        self.api_interface = None
        self.auth_system = None
        self.master_node = None
        self.deployment_manager = None
        
        # Setup signal handlers
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
    
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals"""
        print("\n🛑 Received shutdown signal...")
        self.shutdown()
        sys.exit(0)
    
    def initialize(self, max_kernels: int = 4):
        """
        Initialize all EPC components
        
        Args:
            max_kernels: Maximum number of Wolfram kernels
        """
        print("=" * 60)
        print("🐺 Wolfram Enterprise Private Cloud for WolfCog")
        print("=" * 60)
        print()
        
        # Initialize compute engine
        print("🔧 Initializing Compute Engine...")
        self.compute_engine = ComputeEngine(max_kernels=max_kernels)
        print(f"   ✓ Compute Engine ready (max {max_kernels} kernels)")
        
        # Initialize authentication system
        print("🔐 Initializing Authentication System...")
        self.auth_system = AuthenticationSystem()
        
        # Create default admin user if no users exist
        if not self.auth_system.users:
            self.auth_system.register_user(
                username="admin",
                email="admin@wolfcog.local",
                password="admin123",
                subdomains=["*"],
                roles=["admin"]
            )
            print("   ✓ Created default admin user (admin/admin123)")
        print(f"   ✓ Authentication System ready ({len(self.auth_system.users)} users)")
        
        # Initialize API interface
        print("📡 Initializing API Interface...")
        self.api_interface = APIInterface(
            compute_engine=self.compute_engine,
            host="0.0.0.0",
            port=5000
        )
        print("   ✓ API Interface ready")
        
        # Initialize master node
        print("🎯 Initializing Master Node...")
        self.master_node = MasterNode(config_path=self.config_path)
        self.master_node.initialize(
            compute_engine=self.compute_engine,
            api_interface=self.api_interface,
            auth_system=self.auth_system
        )
        print("   ✓ Master Node ready")
        
        # Initialize deployment manager
        print("🚀 Initializing Deployment Manager...")
        self.deployment_manager = DeploymentManager(
            master_node=self.master_node
        )
        print("   ✓ Deployment Manager ready")
        
        print()
        print("✅ EPC Infrastructure initialized successfully")
        print()
    
    def start(self):
        """Start the EPC infrastructure"""
        print("🚀 Starting Wolfram Enterprise Private Cloud...")
        print()
        
        # Start master node
        self.master_node.start()
        
        self.running = True
        
        # Display access information
        self._display_access_info()
    
    def _display_access_info(self):
        """Display access information"""
        print()
        print("=" * 60)
        print("📱 Access Information")
        print("=" * 60)
        print(f"API Interface:     http://localhost:{self.api_interface.port}")
        print(f"API Endpoints:     http://localhost:{self.api_interface.port}/api/endpoints")
        print(f"API Metrics:       http://localhost:{self.api_interface.port}/api/metrics")
        print(f"Health Check:      http://localhost:{self.api_interface.port}/api/health")
        print()
        print("Authentication:")
        print(f"  Default admin:   admin / admin123")
        print(f"  Total users:     {len(self.auth_system.users)}")
        print()
        print("=" * 60)
        print()
    
    def deploy_example_apis(self):
        """Deploy example Wolfram Language APIs"""
        print("📚 Deploying example APIs...")
        
        # Deploy factorial API
        self.api_interface.deploy_api(
            name="factorial",
            wolfram_function="Factorial[#]&",
            description="Compute factorial of a number",
            parameters={"n": "integer"}
        )
        print("   ✓ Deployed: /api/factorial")
        
        # Deploy prime check API
        self.api_interface.deploy_api(
            name="isprime",
            wolfram_function="PrimeQ[#]&",
            description="Check if a number is prime",
            parameters={"n": "integer"}
        )
        print("   ✓ Deployed: /api/isprime")
        
        # Deploy solve equation API
        self.api_interface.deploy_api(
            name="solve",
            wolfram_function="Solve[#[[1]] == #[[2]], x]&",
            description="Solve algebraic equation",
            parameters={"lhs": "expression", "rhs": "expression"}
        )
        print("   ✓ Deployed: /api/solve")
        
        print()
    
    def demo_deployment_options(self):
        """Demonstrate deployment options"""
        print("🎨 Demonstrating Deployment Options...")
        
        # Application Host
        app_deployment = self.deployment_manager.deploy_application(
            name="Interactive Calculator",
            notebook_path="/tmp/calculator.nb",
            form_based=True
        )
        print(f"   ✓ Application Host: {app_deployment.url}")
        
        # Computation Center
        compute_deployment = self.deployment_manager.deploy_computation_center(
            name="Math Processing Hub",
            api_endpoints=["factorial", "isprime", "solve"]
        )
        print(f"   ✓ Computation Center: {compute_deployment.url}")
        
        # Embedded Application
        embed_deployment = self.deployment_manager.deploy_embedded_application(
            name="Math Widget",
            target_service="external-website"
        )
        print(f"   ✓ Embedded Application: {embed_deployment.url}")
        
        # Hosted Reporting
        report_deployment = self.deployment_manager.deploy_hosted_reporting(
            name="Daily Analytics Report",
            report_notebook="/tmp/analytics.nb",
            schedule="0 0 * * *"
        )
        print(f"   ✓ Hosted Reporting: {report_deployment.url}")
        
        print()
    
    def run_interactive_demo(self):
        """Run interactive demonstration"""
        print("🎮 Interactive EPC Demo")
        print()
        
        while self.running:
            print("Options:")
            print("  1. Execute Wolfram code")
            print("  2. View system status")
            print("  3. List deployments")
            print("  4. Test API")
            print("  5. Exit")
            print()
            
            try:
                choice = input("Select option (1-5): ").strip()
                
                if choice == "1":
                    code = input("Enter Wolfram code: ").strip()
                    if code:
                        result = self.compute_engine.execute(code)
                        print(f"\nResult: {result['output']}")
                        if result.get('error'):
                            print(f"Error: {result['error']}")
                
                elif choice == "2":
                    status = self.master_node.get_system_status()
                    print("\n📊 System Status:")
                    print(f"  Master Node: {status['master_node']['status']}")
                    print(f"  Uptime: {status['master_node']['uptime_formatted']}")
                    print(f"  Active Kernels: {status['compute_engine']['active_kernels']}")
                    print(f"  API Endpoints: {status['api_interface']['endpoints']}")
                    print(f"  Total Users: {status['authentication']['total_users']}")
                
                elif choice == "3":
                    deployments = self.deployment_manager.list_deployments()
                    print("\n🚀 Deployments:")
                    for dep in deployments:
                        print(f"  • {dep['name']} ({dep['type']}): {dep['url']}")
                
                elif choice == "4":
                    print("\nTesting /api/factorial...")
                    result = self.compute_engine.execute("Factorial[5]")
                    print(f"Result: {result['output']}")
                
                elif choice == "5":
                    break
                
                else:
                    print("Invalid option")
                
                print()
                
            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"Error: {e}")
                print()
    
    def shutdown(self):
        """Shutdown EPC infrastructure"""
        if not self.running:
            return
        
        print("\n🛑 Shutting down EPC infrastructure...")
        
        self.running = False
        
        # Shutdown master node
        if self.master_node:
            self.master_node.shutdown()
        
        print("✅ EPC shutdown complete")


def main():
    """Main entry point"""
    print()
    
    # Create and initialize EPC
    epc = EPCCoordinator()
    
    try:
        # Initialize with 4 max kernels
        epc.initialize(max_kernels=4)
        
        # Start the infrastructure
        epc.start()
        
        # Deploy example APIs
        epc.deploy_example_apis()
        
        # Demonstrate deployment options
        epc.demo_deployment_options()
        
        # Display system status
        print("📊 System Status:")
        status = epc.master_node.get_system_status()
        print(f"  • Master Node: {status['master_node']['status']}")
        print(f"  • Active Kernels: {status['compute_engine']['active_kernels']}")
        print(f"  • API Endpoints: {status['api_interface']['endpoints']}")
        print(f"  • Deployments: {len(epc.deployment_manager.deployments)}")
        print()
        
        # Run interactive demo
        print("🎮 Starting interactive demo...")
        print("   Press Ctrl+C to exit")
        print()
        
        epc.run_interactive_demo()
        
    except KeyboardInterrupt:
        print("\n")
    finally:
        epc.shutdown()


if __name__ == "__main__":
    main()
