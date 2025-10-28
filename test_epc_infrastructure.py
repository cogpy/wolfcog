#!/usr/bin/env python3
"""
Tests for Wolfram Enterprise Private Cloud Infrastructure

Tests all EPC components:
- Compute Engine
- API Interface
- Authentication System
- Master Node
- Deployment Manager
"""

import unittest
import sys
import time
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from epc.compute_engine import ComputeEngine, KernelStatus
from epc.api_interface import APIInterface
from epc.authentication import AuthenticationSystem
from epc.master_node import MasterNode
from epc.deployment_manager import DeploymentManager, DeploymentType


class TestComputeEngine(unittest.TestCase):
    """Test Compute Engine functionality"""
    
    def setUp(self):
        """Setup test compute engine"""
        self.engine = ComputeEngine(max_kernels=2)
    
    def tearDown(self):
        """Cleanup"""
        self.engine.shutdown()
    
    def test_start_kernel(self):
        """Test starting a kernel"""
        kernel_id = self.engine.start_kernel()
        self.assertIsNotNone(kernel_id)
        self.assertIn(kernel_id, self.engine.kernels)
        
        kernel = self.engine.kernels[kernel_id]
        self.assertEqual(kernel.status, KernelStatus.IDLE)
    
    def test_max_kernels_limit(self):
        """Test maximum kernel limit"""
        # Start max kernels
        for i in range(self.engine.max_kernels):
            self.engine.start_kernel()
        
        # Try to start one more
        with self.assertRaises(RuntimeError):
            self.engine.start_kernel()
    
    def test_execute_code(self):
        """Test code execution"""
        kernel_id = self.engine.start_kernel()
        
        result = self.engine.execute("2 + 2", kernel_id=kernel_id)
        
        self.assertIsNotNone(result)
        self.assertIn("output", result)
        # Result will be mock execution unless Wolfram is installed
    
    def test_kernel_selection(self):
        """Test automatic kernel selection"""
        kernel_id = self.engine.start_kernel()
        
        # Execute without specifying kernel
        result = self.engine.execute("1 + 1")
        
        self.assertEqual(result["kernel_id"], kernel_id)
    
    def test_parallel_map(self):
        """Test parallel map execution"""
        self.engine.start_kernel()
        
        data = [1, 2, 3, 4, 5]
        results = self.engine.parallel_map("Prime", data)
        
        self.assertEqual(len(results), len(data))
    
    def test_get_kernel_status(self):
        """Test getting kernel status"""
        kernel_id = self.engine.start_kernel()
        
        status = self.engine.get_kernel_status(kernel_id)
        
        self.assertIn("kernel_id", status)
        self.assertIn("status", status)
        self.assertIn("tasks_processed", status)
    
    def test_wolfcog_space_integration(self):
        """Test WolfCog space integration"""
        self.engine.start_kernel()
        
        result = self.engine.integrate_with_wolfcog_space(
            space="e",
            operation="1 + 1"
        )
        
        self.assertIsNotNone(result)


class TestAPIInterface(unittest.TestCase):
    """Test API Interface functionality"""
    
    def setUp(self):
        """Setup test API interface"""
        self.engine = ComputeEngine(max_kernels=2)
        self.engine.start_kernel()
        self.api = APIInterface(compute_engine=self.engine, port=5001)
    
    def tearDown(self):
        """Cleanup"""
        self.engine.shutdown()
    
    def test_deploy_api(self):
        """Test API deployment"""
        endpoint = self.api.deploy_api(
            name="test_factorial",
            wolfram_function="Factorial[#]&",
            description="Test factorial API"
        )
        
        self.assertEqual(endpoint.name, "test_factorial")
        self.assertEqual(endpoint.path, "/api/test_factorial")
        self.assertIn("test_factorial", self.api.endpoints)
    
    def test_multiple_api_deployment(self):
        """Test deploying multiple APIs"""
        self.api.deploy_api("api1", "Function1")
        self.api.deploy_api("api2", "Function2")
        self.api.deploy_api("api3", "Function3")
        
        self.assertEqual(len(self.api.endpoints), 3)
    
    def test_api_metrics(self):
        """Test API metrics tracking"""
        self.api.deploy_api("test", "TestFunction")
        
        self.assertIn("test", self.api.metrics["endpoints"])
        metrics = self.api.metrics["endpoints"]["test"]
        
        self.assertEqual(metrics["requests"], 0)
        self.assertEqual(metrics["successes"], 0)
    
    def test_excel_connector(self):
        """Test Excel connector formula generation"""
        self.api.deploy_api("factorial", "Factorial[#]&")
        
        formula = self.api.excel_connector("factorial")
        
        self.assertIn("WEBSERVICE", formula)
        self.assertIn("/api/factorial", formula)
    
    def test_openapi_spec(self):
        """Test OpenAPI specification generation"""
        self.api.deploy_api("test", "TestFunc", description="Test API")
        
        spec = self.api.get_openapi_spec()
        
        self.assertEqual(spec["openapi"], "3.0.0")
        self.assertIn("paths", spec)
        self.assertIn("/api/test", spec["paths"])


class TestAuthenticationSystem(unittest.TestCase):
    """Test Authentication System"""
    
    def setUp(self):
        """Setup test authentication system"""
        self.auth = AuthenticationSystem(data_dir=Path("/tmp/test_epc_auth"))
    
    def tearDown(self):
        """Cleanup"""
        # Clean up test data
        import shutil
        if Path("/tmp/test_epc_auth").exists():
            shutil.rmtree("/tmp/test_epc_auth")
    
    def test_register_user(self):
        """Test user registration"""
        user = self.auth.register_user(
            username="testuser",
            email="test@example.com",
            password="testpass123",
            subdomains=["test"],
            roles=["user"]
        )
        
        self.assertEqual(user.username, "testuser")
        self.assertIn("testuser", self.auth.users)
        self.assertTrue(user.api_key)
    
    def test_duplicate_username(self):
        """Test duplicate username rejection"""
        self.auth.register_user("user1", "email1@example.com", "pass1")
        
        with self.assertRaises(ValueError):
            self.auth.register_user("user1", "email2@example.com", "pass2")
    
    def test_authenticate(self):
        """Test user authentication"""
        self.auth.register_user("testuser", "test@example.com", "testpass")
        
        session_token = self.auth.authenticate("testuser", "testpass")
        
        self.assertIsNotNone(session_token)
        self.assertIn(session_token, self.auth.sessions)
    
    def test_invalid_credentials(self):
        """Test invalid credentials"""
        self.auth.register_user("testuser", "test@example.com", "correctpass")
        
        session_token = self.auth.authenticate("testuser", "wrongpass")
        
        self.assertIsNone(session_token)
    
    def test_api_key_authentication(self):
        """Test API key authentication"""
        user = self.auth.register_user("testuser", "test@example.com", "pass")
        
        username = self.auth.authenticate_api_key(user.api_key)
        
        self.assertEqual(username, "testuser")
    
    def test_session_validation(self):
        """Test session validation"""
        self.auth.register_user("testuser", "test@example.com", "pass")
        session_token = self.auth.authenticate("testuser", "pass")
        
        username = self.auth.validate_session(session_token)
        
        self.assertEqual(username, "testuser")
    
    def test_subdomain_access(self):
        """Test subdomain access control"""
        self.auth.register_user(
            "testuser",
            "test@example.com",
            "pass",
            subdomains=["analytics"]
        )
        
        self.assertTrue(self.auth.check_subdomain_access("testuser", "analytics"))
        self.assertFalse(self.auth.check_subdomain_access("testuser", "admin"))
    
    def test_admin_subdomain_access(self):
        """Test admin access to all subdomains"""
        self.auth.register_user(
            "admin",
            "admin@example.com",
            "pass",
            roles=["admin"]
        )
        
        self.assertTrue(self.auth.check_subdomain_access("admin", "anything"))


class TestMasterNode(unittest.TestCase):
    """Test Master Node functionality"""
    
    def setUp(self):
        """Setup test master node"""
        self.engine = ComputeEngine(max_kernels=2)
        self.auth = AuthenticationSystem(data_dir=Path("/tmp/test_epc_auth"))
        self.api = APIInterface(compute_engine=self.engine, port=5002)
        
        self.master = MasterNode(config_path=Path("/tmp/test_master_config.json"))
        self.master.initialize(
            compute_engine=self.engine,
            api_interface=self.api,
            auth_system=self.auth
        )
    
    def tearDown(self):
        """Cleanup"""
        self.master.shutdown()
    
    def test_initialization(self):
        """Test master node initialization"""
        self.assertEqual(self.master.status, "initialized")
        self.assertIsNotNone(self.master.compute_engine)
        self.assertIsNotNone(self.master.api_interface)
    
    def test_start(self):
        """Test master node start"""
        self.master.start()
        
        self.assertEqual(self.master.status, "running")
        self.assertGreater(self.master.start_time, 0)
    
    def test_register_compute_node(self):
        """Test compute node registration"""
        node = self.master.register_compute_node(
            node_id="node1",
            host="localhost",
            port=8080,
            max_kernels=4
        )
        
        self.assertIn("node1", self.master.compute_nodes)
        self.assertEqual(node.host, "localhost")
    
    def test_node_heartbeat(self):
        """Test node heartbeat update"""
        self.master.register_compute_node("node1", "localhost", 8080, 4)
        
        self.master.update_node_heartbeat("node1", load=0.5)
        
        node = self.master.compute_nodes["node1"]
        self.assertEqual(node.current_load, 0.5)
        self.assertEqual(node.status, "online")
    
    def test_select_compute_node(self):
        """Test compute node selection"""
        self.master.register_compute_node("node1", "localhost", 8080, 4)
        self.master.update_node_heartbeat("node1", 0.3)
        
        self.master.register_compute_node("node2", "localhost", 8081, 4)
        self.master.update_node_heartbeat("node2", 0.7)
        
        selected = self.master.select_compute_node()
        
        self.assertEqual(selected.node_id, "node1")  # Lower load
    
    def test_get_system_status(self):
        """Test system status retrieval"""
        self.master.start()
        
        status = self.master.get_system_status()
        
        self.assertIn("master_node", status)
        self.assertIn("compute_engine", status)
        self.assertIn("api_interface", status)


class TestDeploymentManager(unittest.TestCase):
    """Test Deployment Manager functionality"""
    
    def setUp(self):
        """Setup test deployment manager"""
        self.engine = ComputeEngine(max_kernels=2)
        self.auth = AuthenticationSystem(data_dir=Path("/tmp/test_epc_auth"))
        self.api = APIInterface(compute_engine=self.engine, port=5003)
        
        self.master = MasterNode()
        self.master.initialize(self.engine, self.api, self.auth)
        
        self.deployer = DeploymentManager(
            master_node=self.master,
            data_dir=Path("/tmp/test_deployments")
        )
    
    def tearDown(self):
        """Cleanup"""
        import shutil
        if Path("/tmp/test_deployments").exists():
            shutil.rmtree("/tmp/test_deployments")
    
    def test_deploy_application(self):
        """Test application deployment"""
        deployment = self.deployer.deploy_application(
            name="Test App",
            notebook_path="/tmp/test.nb"
        )
        
        self.assertEqual(deployment.deployment_type, DeploymentType.APPLICATION_HOST)
        self.assertEqual(deployment.status, "active")
        self.assertIn(deployment.deployment_id, self.deployer.deployments)
    
    def test_deploy_computation_center(self):
        """Test computation center deployment"""
        deployment = self.deployer.deploy_computation_center(
            name="Test Center",
            api_endpoints=["api1", "api2"]
        )
        
        self.assertEqual(deployment.deployment_type, DeploymentType.COMPUTATION_CENTER)
        self.assertIn("api_endpoints", deployment.config)
    
    def test_deploy_embedded_application(self):
        """Test embedded application deployment"""
        deployment = self.deployer.deploy_embedded_application(
            name="Test Widget",
            target_service="external"
        )
        
        self.assertEqual(deployment.deployment_type, DeploymentType.EMBEDDED_APPLICATION)
    
    def test_deploy_hosted_reporting(self):
        """Test hosted reporting deployment"""
        deployment = self.deployer.deploy_hosted_reporting(
            name="Test Report",
            report_notebook="/tmp/report.nb",
            schedule="0 0 * * *"
        )
        
        self.assertEqual(deployment.deployment_type, DeploymentType.HOSTED_REPORTING)
        self.assertEqual(deployment.config["schedule"], "0 0 * * *")
    
    def test_list_deployments(self):
        """Test listing deployments"""
        self.deployer.deploy_application("App1", "/tmp/app1.nb")
        self.deployer.deploy_computation_center("Center1", ["api1"])
        
        deployments = self.deployer.list_deployments()
        
        self.assertEqual(len(deployments), 2)
    
    def test_filter_deployments_by_type(self):
        """Test filtering deployments by type"""
        self.deployer.deploy_application("App1", "/tmp/app1.nb")
        self.deployer.deploy_computation_center("Center1", ["api1"])
        
        apps = self.deployer.list_deployments(DeploymentType.APPLICATION_HOST)
        
        self.assertEqual(len(apps), 1)
        self.assertEqual(apps[0]["name"], "App1")


def run_tests():
    """Run all tests"""
    print("=" * 60)
    print("Testing Wolfram Enterprise Private Cloud Infrastructure")
    print("=" * 60)
    print()
    
    # Create test suite
    suite = unittest.TestLoader().loadTestsFromModule(sys.modules[__name__])
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print()
    print("=" * 60)
    print("Test Summary")
    print("=" * 60)
    print(f"Tests run: {result.testsRun}")
    print(f"Successes: {result.testsRun - len(result.failures) - len(result.errors)}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print()
    
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(run_tests())
