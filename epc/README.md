# Wolfram Enterprise Private Cloud (EPC) Module

Enterprise-grade private cloud infrastructure for WolfCog, providing Wolfram Language computation capabilities.

## Components

### Core Modules

- **`compute_engine.py`** - Wolfram kernel pool management with parallelization support
- **`api_interface.py`** - REST API deployment from Wolfram Language functions
- **`authentication.py`** - User provisioning and subdomain-based access control
- **`master_node.py`** - Central coordination of all EPC services
- **`deployment_manager.py`** - Multiple deployment scenario orchestration

### Configuration

- **`setup-notebook.wl`** - Wolfram Language configuration notebook
- **`__init__.py`** - Module initialization and exports

## Quick Start

```python
# Import EPC coordinator
from epc_coordinator import EPCCoordinator

# Create and initialize
epc = EPCCoordinator()
epc.initialize(max_kernels=4)

# Start infrastructure
epc.start()

# Deploy example APIs
epc.deploy_example_apis()

# Run interactive demo
epc.run_interactive_demo()
```

## Features

### Compute Engine
- Dynamic kernel pool (1-N simultaneous kernels)
- Automatic kernel selection and load balancing
- Parallel computation distribution
- WolfCog symbolic space integration

### API Interface
- Single-command API deployment: `deploy_api("name", "WolframFunction")`
- Automatic REST endpoint generation
- Excel connector formula generation
- OpenAPI specification support
- Request metrics and monitoring

### Authentication
- Self-provisioning user accounts
- Subdomain-based restrictions
- Role-based access control (admin, developer, user)
- API key authentication
- Session management

### Deployment Options
1. **Application Host** - Direct user-facing applications
2. **Computation Center** - Backend processing hub
3. **Embedded Applications** - Integration into other services
4. **Hosted Reporting** - Scheduled/triggered reports

## Testing

Run the comprehensive test suite:

```bash
python3 test_epc_infrastructure.py
```

Tests cover:
- Compute engine kernel management
- API deployment and execution
- User authentication and access control
- Master node coordination
- All deployment scenarios

## Documentation

See `/docs/epc-documentation.md` for complete documentation including:
- Architecture overview
- Component details
- API reference
- Deployment examples
- Integration guide
- Troubleshooting

## Integration with WolfCog

The EPC infrastructure integrates seamlessly with WolfCog's symbolic spaces:

- **u-space**: User-facing applications
- **e-space**: Computational execution
- **s-space**: System operations

## Requirements

- Python 3.7+
- Flask >= 2.0.0
- flask-socketio >= 5.0.0
- Wolfram Engine (optional, falls back to mock execution)

## License

See main WolfCog LICENSE file.
