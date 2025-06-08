# WolfCog AGI-OS

🐺 **WolfCog** is a symbolic operating system designed as the meta-root for AGI (Artificial General Intelligence). Built on a Guix/Guile foundation, it implements a trinitized OS model with geometric memory structures and contextual grammars running inside a recursive AGI ecology.

Perfect for assembling the **meta-root of a symbolic OS** using **Guix/Guile** as the bootstrap layer—a **fully reproducible, Lisp-native substrate** that integrates cleanly into a symbolic AGI runtime.


## Key Features

🔧 **Guix + Guile-based symbolic bootstrap** - Fully reproducible environment
🐺 **WolfKernels and symbolic services** - Multi-kernel symbolic architecture  
🔗 **LibraryLinkUtils, GitLink, CascadeLink** - Advanced integration layers
🧠 **OpenCog interfacing** - AtomSpace symbolic memory integration
🧬 **Symbolic evolution engine** - Self-modifying and self-improving capabilities

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

### 1. Bootstrap the Environment

```bash
# Enter the WolfCog environment
guix shell -m .guix/manifest.scm

# Initialize bootstrap shell
guile .guix/bootstrap/stage0.scm
```

### 2. Launch Complete System

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

### 3. Test Symbolic Operations

```bash
# Create symbolic tasks for processing
echo '{"flow": "test_cognition", "space": "e", "symbolic": "∇(cognitive_pattern)", "action": "evolve"}' > /tmp/ecron_tasks/test.json

# Monitor task processing
ls -la /tmp/ecron_tasks/
```

## Core Components

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

## Design Philosophy

WolfCog implements a cognitive architecture where:

- **Code is Cognition**: Programs embody understanding and reasoning patterns
- **Tasks are Manifold Flows**: Computational tasks flow on mathematical manifolds
- **Memory is Geometric**: Data structures organized as navigable geometric spaces
- **Evolution is Recursive**: System continuously improves through self-modification

## Documentation

- [Design Overview](docs/design.md) - Complete architectural design
- [Guix Bootstrap Guide](docs/guix-bootstrap.md) - Environment setup and reproducibility
- [Kernel Specification](docs/kernel-spec.md) - Detailed kernel documentation
- [Symbolic Evolution](docs/symbolic-evolution.md) - Self-modification and improvement
- [Next Steps Implementation](docs/next-steps-implementation.md) - Recently implemented cognitive capabilities

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