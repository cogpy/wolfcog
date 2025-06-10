# Guix Bootstrap Guide

## Overview

WolfCog uses GNU Guix as its foundation for reproducible, deterministic system bootstrapping. This guide covers the setup and usage of the Guix-based environment.

## Prerequisites

- GNU Guix installation
- Basic familiarity with Scheme/Guile
- Understanding of functional package management

## Bootstrap Process

### 1. Environment Setup

```bash
# Enter the WolfCog environment
guix shell -m .guix/manifest.scm

# Initialize the bootstrap shell (interactive)
guile .guix/bootstrap/init-shell.scm

# Or run full Stage0 bootstrap sequence
guile .guix/bootstrap/stage0.scm
```

### 2. Stage0 AGI-OS Bootstrap

The enhanced Stage0 bootstrap implements the complete cognitive flowchart:

```scheme
;; Load the bootstrap module
(load ".guix/bootstrap/stage0-bootstrap.scm")

;; Run complete bootstrap sequence
(stage0-bootstrap)
```

This executes the recursive implementation pathway:
- **Stage0** → Guile package load → Configuration UI → Kernel pool selection
- **OpenCog** services bootstrap → ASFS initialization  
- **Security** enforcement → Adaptive feedback/attention allocation

### 2. Manifest Configuration

The `.guix/manifest.scm` file defines the exact package versions and dependencies:

```scheme
(specifications->manifest
  '("guile"
    "guix" 
    "make"
    "sbcl"
    "python"
    "gfortran"
    "emacs"
    "openjdk"
    "clang"
    "git"))
```

### 3. Bootstrap Stages

#### Stage 0: AGI-OS Initialization
- Initializes Guile environment with core packages
- Detects and configures Wolfram kernel pools
- Verifies OpenCog Unified System components
- Presents bootloader configuration interface
- Applies Guix-inspired security mechanisms

#### Stage 1: Cognitive Platform Loading
- Loads CogUtil (core utilities, logging, pattern-matching)
- Initializes CogServer (distributed server, RPC, message passing)  
- Sets up AtomSpace (hypergraph memory)
- Mounts ASFS (AtomSpace FileSystem)
- Establishes inter-component bridges

#### Stage 2: Adaptive System Activation
- Registers CogServer with AtomSpace and ASFS
- Starts adaptive attention allocation system
- Activates symbolic evolution engine
- Enables self-modification capabilities
- Launches cognitive feedback loops

## Reproducibility

### Package Pinning

Guix ensures reproducibility through:

```bash
# Generate exact package specification
guix describe --format=channels > .guix/channels.scm

# Lock dependencies
guix time-machine -C .guix/channels.scm -- shell -m .guix/manifest.scm
```

### Environment Isolation

Each WolfCog instance runs in a pure environment:

```bash
# Pure environment (no external dependencies)
guix shell -m .guix/manifest.scm --pure

# Container isolation
guix shell -m .guix/manifest.scm --container
```

## Dependency Management

### Core Dependencies

1. **Guile**: Scheme interpreter and symbolic processing
2. **SBCL**: Steel Bank Common Lisp for WolfCore
3. **Python**: For daemon systems and OpenCog integration
4. **Clang**: For LibraryLinkUtils compilation
5. **Git**: For GitLink self-modification

### AGI-OS Bootstrap Dependencies

6. **Wolfram Engine/Mathematica**: For symbolic computation kernels
7. **OpenCog Suite**: CogUtil, AtomSpace, CogServer for cognitive architecture
8. **Guix**: For reproducible environment management

### Automatic Detection

The Stage0 bootstrap automatically detects:
- Available Wolfram kernels (WolframScript, Mathematica)
- OpenCog components (cogutil, atomspace, cogserver)
- System capabilities and fallback options

### Optional Dependencies

- **Emacs**: Development environment
- **OpenJDK**: For Java-based components
- **GFortran**: For numerical computing

## Development Workflow

### 1. Environment Entry

```bash
cd /path/to/wolfcog
guix shell -m .guix/manifest.scm
```

### 2. Interactive Development

```scheme
;; Load bootstrap
(load ".guix/bootstrap/init-shell.scm")

;; Interactive commands available:
(stage0-bootstrap)      ; Run full Stage0 bootstrap
(detect-wolfram)        ; Detect Wolfram kernels
(verify-opencog)        ; Check OpenCog components
(wolf-shell-info)       ; Show available commands

;; Start kernel manually
(load "kernels/wolfcore.lisp")
(wolf-kernel)
```

### 3. AGI-OS Bootstrap Testing

```bash
# Test bootstrap implementation
python3 test-bootstrap.py

# Validate specific components
python3 -c "
from test_bootstrap import BootstrapTest
tester = BootstrapTest()
tester.test_bootstrap_content_structure()
"
```

### 3. Dependency Updates

```bash
# Update manifest
editor .guix/manifest.scm

# Test new environment
guix shell -m .guix/manifest.scm --check

# Commit changes
git add .guix/manifest.scm
git commit -m "Update dependencies"
```

## Troubleshooting

### Common Issues

1. **Package Not Found**
   ```bash
   guix search <package-name>
   guix show <package-name>
   ```

2. **Environment Conflicts**
   ```bash
   guix shell -m .guix/manifest.scm --pure
   ```

3. **Reproducibility Problems**
   ```bash
   guix describe
   guix time-machine -C .guix/channels.scm
   ```

### Debug Mode

```bash
# Verbose output
guix shell -m .guix/manifest.scm --verbosity=3

# Development mode
guix shell -m .guix/manifest.scm --development
```

## Best Practices

### 1. Version Control

- Always commit `.guix/manifest.scm` changes
- Use `guix describe` output for releases
- Tag stable configurations

### 2. Environment Hygiene

- Use `--pure` for reproducible builds
- Avoid system-wide package pollution
- Test in containers when possible

### 3. Dependency Discipline

- Minimize external dependencies
- Prefer Guix packages over manual installs
- Document non-Guix dependencies clearly

## Integration with WolfCog

The Guix environment integrates seamlessly with WolfCog components:

- **Kernels**: Load in isolated Guile environment
- **Daemons**: Python processes in managed environment
- **Links**: Compiled libraries with reproducible toolchain
- **OpenCog**: Integration through standardized Python environment

This ensures that the entire WolfCog AGI system is reproducible, verifiable, and can be exactly replicated across different systems and time periods.

## Stage0 AGI-OS Bootstrap

The enhanced Stage0 bootstrap implements a comprehensive AGI operating system initialization:

### Cognitive Architecture Integration

```scheme
;; Complete bootstrap following cognitive flowchart
(stage0-bootstrap) 
; → Guile packages → Bootloader UI → Kernel pools
; → OpenCog services → ASFS → Security → Attention
```

### Component Discovery and Integration

- **Automatic Detection**: Wolfram kernels, OpenCog components
- **Fallback Mechanisms**: Graceful degradation when components missing  
- **Security Enforcement**: Guix-inspired declarative constraints
- **Symbolic Storage**: ASFS (AtomSpace FileSystem) integration

### Adaptive Attention Allocation

The bootstrap establishes an adaptive attention system:

- **Primary Focus**: Robust, reproducible Guile-based bootstrap
- **Secondary Focus**: Kernel orchestration and service integration  
- **Dynamic Monitoring**: Emergent behavior in AtomSpace
- **Feedback Loops**: Continuous system optimization

### Bootstrap Validation

```bash
# Comprehensive bootstrap testing
python3 test-bootstrap.py

# Results show component status:
# ✅ Bootstrap files: All present
# ✅ Function structure: Complete  
# ✅ OpenCog integration: Ready
# ✅ Kernel availability: Detected
# ✅ Security mechanisms: Active
```

This creates a foundation for the recursive AGI system described in the project vision.