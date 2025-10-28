# Wolfram Enterprise Private Cloud for WolfCog

Complete implementation of Wolfram Enterprise Private Cloud (EPC) infrastructure integrated with the WolfCog AGI-OS.

## Overview

This implementation brings the power of Wolfram Enterprise Private Cloud to WolfCog, providing:

- **Compute Engine**: Managed Wolfram Engine kernel pool with parallelization
- **API Interface**: Single-command API deployment from Wolfram Language functions
- **Web Interface**: Interactive web-based applications and notebooks
- **Authentication**: Self-provisioning users with subdomain-based restrictions
- **Master Node**: Centralized coordination of all services
- **Deployment Options**: Multiple deployment scenarios for different use cases

## Architecture

```
epc/
├── __init__.py              # EPC module initialization
├── compute_engine.py        # Wolfram kernel management
├── api_interface.py         # REST API deployment
├── authentication.py        # User authentication & provisioning
├── master_node.py          # Master node coordination
├── deployment_manager.py    # Deployment orchestration
└── setup-notebook.wl       # Configuration notebook
```

## Quick Start

### 1. Initialize EPC Infrastructure

```bash
# Start the EPC coordinator
python3 epc_coordinator.py
```

### 2. Access Services

- **API Interface**: http://localhost:5000
- **API Endpoints List**: http://localhost:5000/api/endpoints
- **Health Check**: http://localhost:5000/api/health
- **Metrics**: http://localhost:5000/api/metrics

### 3. Default Credentials

- **Username**: admin
- **Password**: admin123

## Components

### Compute Engine

The Wolfram Compute Engine manages a pool of Wolfram Language kernels:

```python
from epc.compute_engine import ComputeEngine

# Create engine with 4 max kernels
engine = ComputeEngine(max_kernels=4)

# Start a kernel
kernel_id = engine.start_kernel()

# Execute code
result = engine.execute("Factorial[10]")
print(result["output"])

# Parallel execution
results = engine.parallel_map("Prime", [1, 2, 3, 4, 5])
```

**Features:**
- Dynamic kernel pooling
- Automatic kernel selection
- Parallel computation support
- Integration with WolfCog symbolic spaces

### API Interface

Deploy Wolfram Language functions as REST APIs with a single command:

```python
from epc.api_interface import APIInterface

# Initialize with compute engine
api = APIInterface(compute_engine=engine)

# Deploy API from Wolfram function
api.deploy_api(
    name="factorial",
    wolfram_function="Factorial[#]&",
    description="Compute factorial",
    parameters={"n": "integer"}
)

# Start API server
api.run()
```

**Access the API:**
```bash
curl -X POST http://localhost:5000/api/factorial \
  -H "Content-Type: application/json" \
  -d '{"n": 5}'
```

**Features:**
- Single-command deployment
- Automatic parameter validation
- Rate limiting
- Authentication support
- OpenAPI specification generation
- Excel connector formulas

### Authentication System

Self-provisioning user accounts with subdomain restrictions:

```python
from epc.authentication import AuthenticationSystem

# Initialize auth system
auth = AuthenticationSystem()

# Register user
user = auth.register_user(
    username="developer",
    email="dev@example.com",
    password="secure_password",
    subdomains=["analytics", "reporting"],
    roles=["developer"]
)

# Authenticate
session_token = auth.authenticate("developer", "secure_password")

# Check subdomain access
has_access = auth.check_subdomain_access("developer", "analytics")
```

**Features:**
- Self-provisioning accounts
- Subdomain-based restrictions
- Role-based access control (RBAC)
- API key management
- Session management

### Master Node

Centralized coordination and management:

```python
from epc.master_node import MasterNode

# Initialize master node
master = MasterNode()

# Initialize with services
master.initialize(
    compute_engine=engine,
    api_interface=api,
    auth_system=auth
)

# Start services
master.start()

# Get system status
status = master.get_system_status()
print(status)
```

**Features:**
- Service orchestration
- Compute node registration
- Load balancing
- Health monitoring
- System metrics

### Deployment Manager

Multiple deployment scenarios:

```python
from epc.deployment_manager import DeploymentManager

# Initialize deployment manager
deployer = DeploymentManager(master_node=master)

# 1. Application Host - Direct user applications
app = deployer.deploy_application(
    name="Interactive Calculator",
    notebook_path="/path/to/notebook.nb",
    form_based=True
)

# 2. Computation Center - Processing hub
compute = deployer.deploy_computation_center(
    name="Math Hub",
    api_endpoints=["factorial", "prime", "solve"]
)

# 3. Embedded Application - Integration into other services
embed = deployer.deploy_embedded_application(
    name="Math Widget",
    target_service="external-website"
)

# 4. Hosted Reporting - Scheduled/triggered reports
report = deployer.deploy_hosted_reporting(
    name="Daily Analytics",
    report_notebook="/path/to/report.nb",
    schedule="0 0 * * *"  # Daily at midnight
)
```

## Deployment Options

### 1. Application Host (Direct to User)

Deploy and distribute Wolfram Language applications directly to users:

- Form-based interfaces
- Notebook interfaces
- Linguistic interfaces
- Interactive 3D graphics
- Public or authenticated access

**Example:**
```python
deployment = deployer.deploy_application(
    name="Data Analyzer",
    notebook_path="/apps/analyzer.nb",
    form_based=True,
    linguistic_interface=True
)
# Access at: /app/{deployment_id}
```

### 2. Computation Center

Act as a computational processing hub:

- Batch processing
- Queue management
- High-throughput computation
- API-based communication

**Example:**
```python
deployment = deployer.deploy_computation_center(
    name="Processing Hub",
    api_endpoints=["analyze", "transform", "predict"]
)
# Access at: /compute/{deployment_id}
```

### 3. Embedded Application

Embed into another web environment:

- iframe embedding
- CORS support
- Cross-origin integration
- Widget deployment

**Example:**
```python
deployment = deployer.deploy_embedded_application(
    name="Calculator Widget",
    target_service="company-portal",
    iframe_url="/embed/calculator"
)
```

### 4. Hosted Reporting

Scheduled or triggered reporting:

- Cron-based scheduling
- Event-triggered reports
- Multiple output formats (PDF, HTML, Notebook)
- Email delivery

**Example:**
```python
deployment = deployer.deploy_hosted_reporting(
    name="Weekly Report",
    report_notebook="/reports/weekly.nb",
    schedule="0 0 * * 0",  # Weekly on Sunday
    triggers=["data_update", "manual_request"]
)
```

## Platform Integrations

### Development Platform

Traditional code development environment:

- Full Wolfram Language IDE
- Package development
- Version control integration
- Collaborative editing

### Mathematica Online

Familiar Mathematica interface:

- Notebook-based workflow
- Educational use cases
- Exploration and prototyping
- Desktop Mathematica compatibility

### Mobile Interface

iOS and Android access:

- Mobile-optimized UI
- Touch-based interaction
- Offline capabilities
- Push notifications

### Desktop Interface

Direct desktop access:

- Wolfram Desktop integration
- Cross-platform (Windows, Mac, Linux)
- Local computation with cloud sync
- High-performance graphics

## Excel Integration

Access deployed APIs from Excel:

```python
# Get Excel formula for API
formula = api.excel_connector("factorial")
print(formula)
```

Use in Excel:
```excel
=WEBSERVICE("http://localhost:5000/api/factorial?n=5")
```

Or with Power Query for POST requests.

## Configuration

### Setup Notebook

Run the configuration notebook to set up all components:

```bash
wolframscript -file epc/setup-notebook.wl
```

The notebook configures:
- Compute engine settings
- API deployments
- Authentication
- Web interfaces
- License management
- Integration with WolfCog

### Python Configuration

Edit `epc_coordinator.py` to customize:

```python
# Maximum kernels
epc.initialize(max_kernels=8)

# Custom API port
api = APIInterface(compute_engine, port=8080)

# Custom authentication
auth = AuthenticationSystem(data_dir="/custom/path")
```

## Integration with WolfCog

The EPC infrastructure integrates seamlessly with WolfCog's symbolic spaces:

```python
# Execute in WolfCog space context
result = engine.integrate_with_wolfcog_space(
    space="e",  # Execution space
    operation="Factorial[10]"
)
```

**Symbolic Space Integration:**
- **u-space (User)**: User-facing applications and interfaces
- **e-space (Execution)**: Computational processing
- **s-space (System)**: System-level operations

## Security

### Authentication

- Password hashing (SHA-256 with salt)
- Session token management
- API key authentication
- Subdomain restrictions

### Access Control

- Role-based access control (RBAC)
- Subdomain-based permissions
- API rate limiting
- Request validation

## Monitoring & Metrics

### System Status

```python
status = master.get_system_status()
```

Returns:
- Master node status and uptime
- Active kernel count
- Compute node health
- API metrics
- User sessions

### API Metrics

```bash
curl http://localhost:5000/api/metrics
```

Returns:
- Total requests
- Success/failure rates
- Per-endpoint metrics
- Average response times

## Troubleshooting

### Wolfram Kernel Not Found

If WolframKernel is not found, specify the path:

```python
engine = ComputeEngine(
    wolfram_path="/path/to/WolframKernel"
)
```

### Port Already in Use

Change the API port:

```python
api = APIInterface(compute_engine, port=5001)
```

### License Issues

Check license status:

```wolfram
$LicenseExpirationDate
$MaxLicenseProcesses
```

## Advanced Features

### Distributed Compute Nodes

Register additional compute nodes:

```python
master.register_compute_node(
    node_id="node1",
    host="192.168.1.100",
    port=8080,
    max_kernels=8
)
```

### Custom API Functions

Deploy complex functions:

```python
api.deploy_api(
    name="custom_analysis",
    wolfram_function="""
    Module[{data, result},
        data = #data;
        result = Analyze[data, Method -> "Advanced"];
        ExportString[result, "JSON"]
    ]&
    """,
    parameters={"data": "list"}
)
```

### Notebook Deployment

Deploy from Wolfram Notebook:

```python
api.deploy_from_notebook("/path/to/apis.nb")
```

## References

- [Wolfram Enterprise Private Cloud Overview](https://www.wolfram.com/enterprise-private-cloud/)
- [EPC Elements](https://www.wolfram.com/enterprise-private-cloud/elements/)
- [Wolfram Engine Documentation](https://www.wolfram.com/engine/)
- [WolfCog Documentation](../docs/README.md)

## Support

For issues and questions:
- GitHub Issues: [wolfcog/issues](https://github.com/cogpy/wolfcog/issues)
- Documentation: See `docs/` directory
- Configuration help: Run setup notebook

---

**Note**: This implementation provides the infrastructure layer. For production deployments, additional security hardening, monitoring, and scalability features should be implemented based on specific requirements.
