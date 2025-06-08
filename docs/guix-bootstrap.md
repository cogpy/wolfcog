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

# Initialize the bootstrap shell
guile .guix/bootstrap/stage0.scm
```

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

#### Stage 0: Basic Shell
- Initializes Guile environment
- Sets up readline interface
- Prepares for kernel loading

#### Stage 1: Kernel Loading
- Loads WolfCore symbolic kernel
- Initializes memory structures
- Connects to daemon system

#### Stage 2: Full System
- Activates all Wolf kernels
- Starts symbolic scheduler
- Enables self-modification

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

;; Start kernel
(load "kernels/wolfcore.lisp")
(wolf-kernel)
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