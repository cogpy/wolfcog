#!/usr/bin/env python3
"""
Quick Demo of Wolfram Enterprise Private Cloud Infrastructure

This script demonstrates the key features of the EPC implementation.
"""

import sys
from pathlib import Path

# Import EPC components
from epc.compute_engine import ComputeEngine
from epc.api_interface import APIInterface
from epc.authentication import AuthenticationSystem
from epc.master_node import MasterNode
from epc.deployment_manager import DeploymentManager


def demo_compute_engine():
    """Demonstrate Compute Engine features"""
    print("\n" + "=" * 60)
    print("DEMO 1: Compute Engine - Wolfram Kernel Management")
    print("=" * 60)
    
    engine = ComputeEngine(max_kernels=4)
    
    # Start kernels
    print("\n📦 Starting Wolfram kernels...")
    k1 = engine.start_kernel()
    k2 = engine.start_kernel()
    print(f"   ✓ Started {k1}")
    print(f"   ✓ Started {k2}")
    
    # Execute code
    print("\n🔢 Executing Wolfram code...")
    result = engine.execute("Factorial[10]")
    print(f"   Factorial[10] = {result['output'].strip()}")
    
    # Parallel execution
    print("\n⚡ Parallel map execution...")
    results = engine.parallel_map("Prime", [1, 2, 3, 4, 5])
    print(f"   Computed primes for indices 1-5")
    
    # Get status
    print("\n📊 Kernel status:")
    status = engine.get_kernel_status()
    print(f"   Active kernels: {status['total_kernels']}/{status['max_kernels']}")
    
    engine.shutdown()
    print("\n✅ Compute Engine demo complete")
    
    return engine


def demo_api_interface():
    """Demonstrate API Interface features"""
    print("\n" + "=" * 60)
    print("DEMO 2: API Interface - REST API Deployment")
    print("=" * 60)
    
    engine = ComputeEngine(max_kernels=2)
    engine.start_kernel()
    
    api = APIInterface(compute_engine=engine, port=5000)
    
    # Deploy APIs
    print("\n📡 Deploying Wolfram Language APIs...")
    
    api.deploy_api(
        name="factorial",
        wolfram_function="Factorial[#]&",
        description="Compute factorial of a number"
    )
    print("   ✓ Deployed: /api/factorial")
    
    api.deploy_api(
        name="isprime",
        wolfram_function="PrimeQ[#]&",
        description="Check if number is prime"
    )
    print("   ✓ Deployed: /api/isprime")
    
    api.deploy_api(
        name="fibonacci",
        wolfram_function="Fibonacci[#]&",
        description="Compute Fibonacci number"
    )
    print("   ✓ Deployed: /api/fibonacci")
    
    # Show Excel integration
    print("\n📊 Excel connector for factorial API:")
    formula = api.excel_connector("factorial")
    print(f"   {formula.split('Or with')[0].strip()}")
    
    # Show OpenAPI spec
    print("\n📖 OpenAPI specification generated:")
    spec = api.get_openapi_spec()
    print(f"   Version: {spec['openapi']}")
    print(f"   Endpoints: {len(spec['paths'])}")
    
    engine.shutdown()
    print("\n✅ API Interface demo complete")


def demo_authentication():
    """Demonstrate Authentication System features"""
    print("\n" + "=" * 60)
    print("DEMO 3: Authentication - User Provisioning & Access Control")
    print("=" * 60)
    
    auth = AuthenticationSystem(data_dir=Path("/tmp/demo_auth"))
    
    # Register users
    print("\n👥 Registering users...")
    
    admin = auth.register_user(
        username="admin",
        email="admin@wolfcog.local",
        password="admin123",
        roles=["admin"]
    )
    print(f"   ✓ Admin user: {admin.username}")
    
    dev = auth.register_user(
        username="developer",
        email="dev@example.com",
        password="dev123",
        subdomains=["analytics", "reports"],
        roles=["developer"]
    )
    print(f"   ✓ Developer: {dev.username} (Subdomains: {', '.join(dev.subdomains)})")
    
    # Authenticate
    print("\n🔐 Authentication...")
    session = auth.authenticate("developer", "dev123")
    print(f"   ✓ Session token: {session[:20]}...")
    
    # Check access
    print("\n🔒 Subdomain access control:")
    has_analytics = auth.check_subdomain_access("developer", "analytics")
    has_admin = auth.check_subdomain_access("developer", "admin")
    print(f"   developer → analytics: {'✓ Allowed' if has_analytics else '✗ Denied'}")
    print(f"   developer → admin: {'✓ Allowed' if has_admin else '✗ Denied'}")
    
    # Admin access
    admin_has_all = auth.check_subdomain_access("admin", "any_subdomain")
    print(f"   admin → any_subdomain: {'✓ Allowed' if admin_has_all else '✗ Denied'} (admin role)")
    
    print("\n✅ Authentication demo complete")


def demo_deployments():
    """Demonstrate Deployment Manager features"""
    print("\n" + "=" * 60)
    print("DEMO 4: Deployment Manager - Multiple Deployment Scenarios")
    print("=" * 60)
    
    engine = ComputeEngine(max_kernels=2)
    auth = AuthenticationSystem(data_dir=Path("/tmp/demo_auth2"))
    api = APIInterface(compute_engine=engine, port=5001)
    
    master = MasterNode()
    master.initialize(engine, api, auth)
    
    deployer = DeploymentManager(master_node=master, data_dir=Path("/tmp/demo_deploy"))
    
    # Application Host
    print("\n🎨 Deploying Application Host...")
    app = deployer.deploy_application(
        name="Data Visualizer",
        notebook_path="/tmp/visualizer.nb",
        form_based=True
    )
    print(f"   ✓ {app.name}")
    print(f"   • Type: {app.deployment_type.value}")
    print(f"   • URL: {app.url}")
    print(f"   • Status: {app.status}")
    
    # Computation Center
    print("\n🖥️  Deploying Computation Center...")
    compute = deployer.deploy_computation_center(
        name="Analytics Hub",
        api_endpoints=["factorial", "isprime", "fibonacci"]
    )
    print(f"   ✓ {compute.name}")
    print(f"   • Type: {compute.deployment_type.value}")
    print(f"   • URL: {compute.url}")
    print(f"   • APIs: {len(compute.config['api_endpoints'])}")
    
    # Embedded Application
    print("\n📦 Deploying Embedded Application...")
    embed = deployer.deploy_embedded_application(
        name="Math Widget",
        target_service="company-portal"
    )
    print(f"   ✓ {embed.name}")
    print(f"   • Type: {embed.deployment_type.value}")
    print(f"   • URL: {embed.url}")
    print(f"   • Target: {embed.config['target_service']}")
    
    # Hosted Reporting
    print("\n📊 Deploying Hosted Reporting...")
    report = deployer.deploy_hosted_reporting(
        name="Weekly Analytics",
        report_notebook="/tmp/weekly_report.nb",
        schedule="0 0 * * 0"
    )
    print(f"   ✓ {report.name}")
    print(f"   • Type: {report.deployment_type.value}")
    print(f"   • URL: {report.url}")
    print(f"   • Schedule: {report.config['schedule']}")
    
    # List all deployments
    print("\n📋 All Deployments:")
    deployments = deployer.list_deployments()
    for i, dep in enumerate(deployments, 1):
        print(f"   {i}. {dep['name']} ({dep['type']}) → {dep['url']}")
    
    master.shutdown()
    print("\n✅ Deployment Manager demo complete")


def demo_master_node():
    """Demonstrate Master Node features"""
    print("\n" + "=" * 60)
    print("DEMO 5: Master Node - System Coordination")
    print("=" * 60)
    
    engine = ComputeEngine(max_kernels=4)
    auth = AuthenticationSystem(data_dir=Path("/tmp/demo_auth3"))
    api = APIInterface(compute_engine=engine, port=5002)
    
    master = MasterNode()
    master.initialize(engine, api, auth)
    
    print("\n🎯 Starting Master Node...")
    master.start()
    
    # Register compute nodes
    print("\n📡 Registering distributed compute nodes...")
    node1 = master.register_compute_node("node1", "192.168.1.100", 8080, 8)
    master.update_node_heartbeat("node1", 0.3)
    print(f"   ✓ {node1.node_id}: {node1.host}:{node1.port} ({node1.max_kernels} kernels, load: {node1.current_load})")
    
    node2 = master.register_compute_node("node2", "192.168.1.101", 8080, 8)
    master.update_node_heartbeat("node2", 0.7)
    print(f"   ✓ {node2.node_id}: {node2.host}:{node2.port} ({node2.max_kernels} kernels, load: {node2.current_load})")
    
    # Select best node
    print("\n⚖️  Load balancing - selecting best node...")
    best = master.select_compute_node()
    print(f"   ✓ Selected: {best.node_id} (load: {best.current_load})")
    
    # Get system status
    print("\n📊 System Status:")
    status = master.get_system_status()
    print(f"   • Master: {status['master_node']['status']}")
    print(f"   • Uptime: {status['master_node']['uptime_formatted']}")
    print(f"   • Kernels: {status['compute_engine']['active_kernels']}/{status['compute_engine']['max_kernels']}")
    print(f"   • Compute Nodes: {status['compute_nodes']['online']}/{status['compute_nodes']['total']} online")
    print(f"   • Users: {status['authentication']['total_users']}")
    
    master.shutdown()
    print("\n✅ Master Node demo complete")


def main():
    """Run all demos"""
    print("\n" + "=" * 60)
    print("🐺 Wolfram Enterprise Private Cloud for WolfCog")
    print("   Feature Demonstration Suite")
    print("=" * 60)
    
    demos = [
        demo_compute_engine,
        demo_api_interface,
        demo_authentication,
        demo_deployments,
        demo_master_node
    ]
    
    for demo in demos:
        try:
            demo()
        except Exception as e:
            print(f"\n❌ Error in demo: {e}")
            import traceback
            traceback.print_exc()
    
    print("\n" + "=" * 60)
    print("✅ All Demonstrations Complete!")
    print("=" * 60)
    print()
    print("For more information:")
    print("  • Documentation: docs/epc-documentation.md")
    print("  • Tests: python3 test_epc_infrastructure.py")
    print("  • Interactive: python3 epc_coordinator.py")
    print()


if __name__ == "__main__":
    main()
