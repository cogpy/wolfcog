# WolfCog AGI-OS

🐺 **WolfCog** is a symbolic operating system designed as the meta-root for AGI (Artificial General Intelligence). Built on a Guix/Guile foundation, it implements a trinitized OS model with geometric memory structures and contextual grammars running inside a recursive AGI ecology.

Perfect for assembling the **meta-root of a symbolic OS** using **Guix/Guile** as the bootstrap layer—a **fully reproducible, Lisp-native substrate** that integrates cleanly into a symbolic AGI runtime.


## Key Features

🔧 **Guix + Guile-based symbolic bootstrap** - Fully reproducible environment
🐺 **WolfKernels and symbolic services** - Multi-kernel symbolic architecture  
🔗 **LibraryLinkUtils, GitLink, CascadeLink** - Advanced integration layers
🧠 **OpenCog interfacing** - AtomSpace symbolic memory integration
🧬 **Symbolic evolution engine** - Self-modifying and self-improving capabilities
☁️ **Wolfram Enterprise Private Cloud** - Full EPC infrastructure with API deployment

## Architecture

```
wolfcog/
├── .guix/                 # Guix bootstrap and environment
│   ├── manifest.scm       # Package dependencies
│   └── bootstrap/         # Bootstrap stages
├── kernels/               # Wolf kernel system
│   ├── wolfcore.lisp      # Central symbolic microkernel
│   ├── wolfnode-guile.scm # Guile execution nodes
│   ├── ecron.wl          # Symbolic tensor scheduler
│   └── meta-shellwalker.wl# Self-modifying shell
├── epc/                   # Wolfram Enterprise Private Cloud
│   ├── compute_engine.py  # Kernel pool management
│   ├── api_interface.py   # REST API deployment
│   ├── authentication.py  # User provisioning
│   ├── master_node.py     # Service coordination
│   └── deployment_manager.py # Deployment orchestration
├── link/                 # Integration layers
│   ├── LibraryLinkUtils/ # Wolfram-C++ bridges
│   ├── GitLink/          # Self-modifying code mgmt
│   └── CascadeLink/      # Multi-layer evaluation
├── opencog/              # OpenCog integration
│   ├── atomspace-mirror/ # Local AtomSpace cache
│   ├── wolf-to-cog-adapter.scm
│   └── ecron-task-daemon.py
├── daemons/              # System services
│   ├── scheduler/        # Symbolic flow coordination
│   ├── reflex/          # Reactive monitoring
│   └── services/        # Core system services
└── docs/                # Design documentation
```

## Quick Start

### 1. Amazing Setup (Recommended)

```bash
# One-command setup that makes everything amazing
./setup-amazing.sh
```

This amazing setup script will:
- Check all dependencies automatically
- Install Python requirements
- Initialize OpenCog submodules
- Test the complete system
- Show you how to get started

### 2. Using the WolfCog CLI

```bash
# Test the system
./wolfcog test

# Create cognitive tasks
./wolfcog task "my_flow" u understand --symbolic "∇(cognitive_pattern)"

# List all tasks
./wolfcog list

# Check system status  
./wolfcog status

# Start the full system
./wolfcog start
```

### 4. Containerized Deployment (Amazing)

```bash
# Deploy the entire cognitive architecture in containers
docker-compose up -d

# View container status
docker-compose ps

# View logs from cognitive components
docker-compose logs cogserver
```

The containerized deployment provides:
- Isolated cognitive components (cogutil, atomspace, cogserver)  
- Automated dependency management
- Network configuration for component communication
- Volume mounts for persistent cognitive data

### 5. Manual Bootstrap (Advanced)

```bash
# Enter the WolfCog AGI-OS environment
guix shell -m .guix/manifest.scm

# Run Stage0 AGI-OS bootstrap sequence
guile .guix/bootstrap/stage0.scm

# Or interactive bootstrap shell
guile .guix/bootstrap/init-shell.scm
```

The Stage0 bootstrap implements:
- **Wolfram Kernel Pool Detection**: Automatic discovery and configuration
- **OpenCog Unified Integration**: CogUtil, AtomSpace, CogServer loading
- **ASFS Initialization**: AtomSpace FileSystem mounting
- **Security Enforcement**: Guix-inspired declarative constraints  
- **Adaptive Attention**: Cognitive resource allocation

## Amazing Features

🎯 **Smart CLI Interface**: Intuitive command-line interface for all operations
🚀 **One-Command Setup**: Automated installation and configuration  
🔧 **Enhanced Actions**: Extended cognitive action vocabulary (understand, analyze, synthesize)
🐳 **Container Ready**: Full Docker Compose support for deployment
✅ **Robust Testing**: Comprehensive integration tests with detailed feedback
🔄 **Error Recovery**: Enhanced error handling and graceful failure management
📊 **Real-time Status**: Live system monitoring and task tracking

### 6. Launch Complete System

```bash
# Start the full WolfCog AGI-OS with all components
python3 wolfcog-coordinator.py
```

This will start:
- Symbolic spaces (u/e/s) with memory structures
- Ecron task scheduler with feedback loops
- Admin and Director persistent agents
- Scheduler and Reflex daemons
- Task processing pipeline

### 7. Test Symbolic Operations

```bash
# Create symbolic tasks for processing
echo '{"flow": "test_cognition", "space": "e", "symbolic": "∇(cognitive_pattern)", "action": "evolve"}' > /tmp/ecron_tasks/test.json

# Monitor task processing
ls -la /tmp/ecron_tasks/
```

## Core Components

### Enhanced Cognitive Actions

WolfCog now supports an extended set of cognitive actions:

- **evaluate**: Assess cognitive patterns and symbolic expressions
- **evolve**: Drive evolutionary improvement of cognitive structures  
- **optimize**: Enhance performance and efficiency of cognitive processes
- **test**: Validate cognitive operations and system integrity
- **meta_evolve**: Recursive self-improvement of the cognitive architecture
- **understand**: Deep comprehension and interpretation of symbolic content
- **analyze**: Systematic examination and decomposition of cognitive elements
- **synthesize**: Creative combination and integration of cognitive components

### Wolf Kernels

- **WolfCore**: Central symbolic microkernel with macro evaluation and state management
- **WolfNode**: Guile-based execution nodes for distributed symbolic processing  
- **Ecron**: Tensor-based symbolic scheduler for complex task flows
- **Meta Shell Walker**: Self-modifying shell system with recursive capabilities

### Integration Layers

- **LibraryLinkUtils**: Connects Wolfram Engine to C/C++ codebases and AtomSpace memory
- **GitLink**: Enables AGI self-modification through automated code repository management
- **CascadeLink**: Multi-layered evaluations with rollback/fork capabilities

### System Services

- **Scheduler Daemon**: Runs Ecron and coordinates symbolic flows across spaces
- **Reflex Daemon**: Monitors shells and self-modifying symbols for reactive responses  
- **Services Daemon**: Handles GitLink, Cascade operations, and runtime triggers
- **Admin Agent**: Persistent monitoring agent for system health and optimization
- **Director Agent**: Logical reasoning agent for system coordination and inference

### Wolfram Enterprise Private Cloud

- **Compute Engine**: Managed pool of Wolfram Language kernels with parallelization
- **API Interface**: Single-command deployment of Wolfram functions as REST APIs
- **Authentication System**: Self-provisioning users with subdomain-based access control
- **Master Node**: Centralized coordination of all EPC services and compute nodes
- **Deployment Manager**: Multiple deployment scenarios (apps, computation center, embedded, reporting)
- **Platform Support**: Development Platform, Mathematica Online, Mobile/Desktop interfaces

**Quick Start:**
```bash
# Start the EPC infrastructure
python3 epc_coordinator.py

# Access APIs at http://localhost:5000
# Deploy Wolfram functions as REST endpoints
# Manage users and deployments
```

See [EPC Documentation](docs/epc-documentation.md) for complete details.

## Design Philosophy

WolfCog implements a cognitive architecture where:

- **Code is Cognition**: Programs embody understanding and reasoning patterns
- **Tasks are Manifold Flows**: Computational tasks flow on mathematical manifolds
- **Memory is Geometric**: Data structures organized as navigable geometric spaces
- **Evolution is Recursive**: System continuously improves through self-modification

## Documentation

📚 **[Complete Architecture Documentation Index](docs/README.md)** - Start here for comprehensive system documentation

### Core Architecture
- [Comprehensive Architecture](docs/architecture.md) - **Complete system architecture with Mermaid diagrams**
- [Component Architecture](docs/component-architecture.md) - **Detailed component interactions and cognitive flows**
- [Integration Pathways](docs/integration-pathways.md) - **Neural-symbolic integration and adaptive attention mechanisms**
- [Emergent Patterns](docs/emergent-patterns.md) - **Recursive documentation evolution and cognitive pattern emergence**

### System Design
- [Design Overview](docs/design.md) - Complete architectural design
- [Guix Bootstrap Guide](docs/guix-bootstrap.md) - Environment setup and reproducibility
- [Kernel Specification](docs/kernel-spec.md) - Detailed kernel documentation
- [Symbolic Evolution](docs/symbolic-evolution.md) - Self-modification and improvement
- [Next Steps Implementation](docs/next-steps-implementation.md) - Recently implemented cognitive capabilities
- [EPC Documentation](docs/epc-documentation.md) - **Wolfram Enterprise Private Cloud infrastructure**

## Development

WolfCog is designed for:

- **Symbolic AI Research**: Pure symbolic reasoning and processing
- **Cognitive Architecture Development**: AGI system experimentation
- **Self-Modifying Systems**: Recursive improvement and adaptation
- **Geometric Computing**: Manifold-based computational approaches

## Getting Started

1. **Install Guix**: Ensure GNU Guix is installed on your system
2. **Clone Repository**: `git clone https://github.com/HyperCogWizard/wolfcog.git`
3. **Bootstrap Environment**: Follow the Quick Start guide above
4. **Explore Documentation**: Read the design docs to understand the architecture
5. **Experiment**: Start with simple symbolic operations and explore capabilities

---

This is where **WolfCog** is born - a symbolic operating system for the AGI age.