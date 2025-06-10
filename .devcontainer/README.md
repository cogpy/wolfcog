# Devcontainer Bootstrap Guide

## Overview

This devcontainer configuration implements the AGI-OS cognitive flowchart for WolfCog development, providing a reproducible, containerized workspace that integrates:

- **Guix Environment**: Declarative package management with Stage0 bootstrapping
- **Stage0 Bootloader**: Automated Guile boot process with Wolfram kernel configuration
- **Wolfram Integration**: Optional Wolfram Engine support for symbolic computation
- **OpenCog Components**: Automatic detection and building of cogutil, cogserver, atomspace
- **Adaptive Attention**: Entry-point scripts for recursive system bring-up

## Files

### `.devcontainer/devcontainer.json`
Main devcontainer configuration with:
- Debian bookworm-slim base with Guix installation
- Required capabilities and security options for AGI-OS
- Port forwarding for OpenCog services (17001-17003)
- Automatic bootstrap execution on container creation

### `.devcontainer/Dockerfile` 
Multi-stage container build:
1. **Base Setup**: Debian with build tools and dependencies
2. **Guix Installation**: Binary Guix installation with proper permissions
3. **Environment Config**: Guile path setup and profile initialization
4. **Wolfram Support**: Optional Wolfram Engine installation hooks

### `.devcontainer/bootstrap.sh`
Orchestrates the complete AGI-OS initialization:
1. **Guix Profile**: Initialize and install manifest packages
2. **Wolf Environment**: Configure environment variables and paths
3. **OpenCog Verification**: Detect and prepare cogutil/cogserver/atomspace
4. **Kernel Detection**: Verify Wolf kernel components
5. **ASFS Setup**: Create symbolic filesystem mount points
6. **Stage0 Ready**: Display available commands and startup options

### `.devcontainer/wolfram-installer.sh`
Wolfram Engine integration helper:
- Detects existing Wolfram installations
- Provides installation guidance for free Wolfram Engine
- Creates symbolic links for system integration
- Handles fallback modes without Wolfram kernels

## Usage

### 1. Open in VS Code

```bash
# Open repository in VS Code with devcontainer
code .
# Select "Reopen in Container" when prompted
```

### 2. Automatic Bootstrap

The container automatically runs `.devcontainer/bootstrap.sh` which:
- Sets up the complete AGI-OS environment
- Verifies all components
- Displays available commands

### 3. Start Stage0 Bootstrap

```bash
# Inside the devcontainer
guile .guix/bootstrap/init-shell.scm
```

This launches the Wolf Shell with:
- Complete Stage0 bootstrap environment
- Wolfram kernel detection and pooling
- OpenCog component integration
- Adaptive attention allocation system

### 4. Run System Tests

```bash
# Validate the complete bootstrap system
python3 test-bootstrap.py

# Start the full WolfCog AGI-OS
python3 wolfcog-coordinator.py
```

## Integration with Existing System

The devcontainer leverages the existing WolfCog infrastructure:

- **Manifest Integration**: Uses `.guix/manifest.scm` for package dependencies
- **Stage0 Bootstrap**: Integrates with `.guix/bootstrap/stage0.scm` implementation
- **Wolf Environment**: Uses `.guix/bootstrap/init-shell.scm` for environment setup
- **Component Detection**: Leverages existing OpenCog and kernel verification
- **Test Framework**: Extends `test-bootstrap.py` with devcontainer validation

## Cognitive Flowchart Implementation

The devcontainer implements the complete AGI-OS cognitive flowchart:

1. **Guix Environment Initialization** ✓
   - Declarative package environment via manifest.scm
   - Guile and Stage0 artifact installation
   - Essential build tools and shell access

2. **Stage0 Bootloader Configuration** ✓  
   - Automated Stage0 Guile boot process
   - Wolfram kernel configuration options presentation
   - OpenCog unified system integration

3. **Wolfram Kernel Integration** ✓
   - Optional Wolfram Engine installation
   - Environment variables and pool configuration
   - Fallback modes for development without licenses

4. **OpenCog Component Initialization** ✓
   - Automatic detection of cogutil/, cogserver/, atomspace/
   - Build preparation with autogen.sh and configure
   - Stage0 and bootloader linking to OpenCog services

5. **Startup Scripts & Adaptive Attention** ✓
   - Entry-point script for recursive system bring-up
   - Logging and emergent boot anomaly detection
   - Complete system status reporting

## Troubleshooting

### Build Issues

#### Guix Binary Installation Freeze
**Problem**: Container build freezes during Guix binary installation at various steps.

**Solution**: The Dockerfile has been updated to handle this issue with comprehensive improvements:
- **Granular Step Separation**: Split the complex RUN command into 5 separate steps for precise debugging
- **Step-by-step Progress Tracking**: Added echo statements before and after each major operation
- **Reduced Timeout**: Changed key authorization timeout from 60 to 5 seconds to prevent hanging
- **Network Resilience**: Added fallback mirror for Guix binary download
- **Robust Error Handling**: Added existence checks before each file operation
- **Specific Cleanup**: Changed from `rm -rf /tmp/*` to `rm -rf /tmp/guix* /tmp/gnu*` for safety
- **Graceful Fallback**: Continues build even if individual steps fail with warning messages

**Technical Details**: The previous implementation used a single complex RUN command that could hang at any step without clear indication. The new approach breaks down the process into:
1. **Network/Archive Access**: Download with mirror fallback and completion verification
2. **Filesystem Permissions**: Directory existence checks before moving with sudo
3. **Symlink Creation**: Target existence verification before symlink creation  
4. **Key Authorization**: Reduced timeout with enhanced progress feedback
5. **Cleanup**: Specific file pattern cleanup to avoid affecting other processes

Each step provides clear progress indicators and continues gracefully if optional operations fail.

#### Bootstrap Script Hangs
**Problem**: The bootstrap script hangs during `guix pull` or package installation.

**Solution**: Added timeout protection:
- `guix pull --bootstrap`: 5-minute timeout
- Package installation: 10-minute timeout  
- Better error messages for debugging

#### Network Issues
**Problem**: Cannot download Guix binary or packages due to network restrictions.

**Solutions**:
- Check firewall settings and proxy configuration
- Use alternative Guix mirrors if available
- Build container on a system with better internet access

### Runtime Issues

#### Guix Commands Not Found
**Problem**: `guix` command not available after container startup.

**Solutions**:
- Restart the container to ensure all environment variables are loaded
- Source the Guix profile manually: `source ~/.guix-profile/etc/profile`
- Check if `/usr/local/bin/guix` symlink exists

#### Wolfram Engine Not Found
**Problem**: Wolfram kernels not available in the development environment.

**Solutions**:
- This is expected behavior when no Wolfram license is available
- The system will operate in fallback mode using Guile and OpenCog
- To add Wolfram support, obtain a free Wolfram Engine license and follow the installer guidance

#### OpenCog Build Failures
**Problem**: OpenCog components fail to build during bootstrap.

**Solutions**:
- Check build dependencies are available via the Guix manifest
- Review build logs for specific error messages
- Some build failures are expected and handled gracefully with `|| true`

## Benefits

- **Reproducible Environment**: Exact dependency versions via Guix
- **Isolated Development**: Container isolation prevents system pollution  
- **Automatic Setup**: Zero-configuration AGI-OS development environment
- **Component Integration**: Seamless OpenCog and Wolfram kernel coordination
- **Stage0 Bootstrap**: Complete cognitive architecture initialization
- **Adaptive Attention**: Monitoring and reactive system responses
- **Robust Error Handling**: Timeout protection and graceful degradation

This devcontainer provides a complete, reproducible AGI-OS development nexus that implements the cognitive synergy between Guix, Guile, Wolfram kernels, and OpenCog components as specified in the original cognitive flowchart.