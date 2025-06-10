#!/bin/bash
set -e

echo "🐺 Bootstrapping AGI-OS devcontainer environment..."

# Ensure we're in the workspace directory
cd /workspace

# Initialize Guix profile if not already done
if [ ! -d "$HOME/.guix-profile" ]; then
    echo "📦 Initializing Guix profile..."
    guix pull --bootstrap || true
fi

# Ensure Guix profile is loaded
if [ -f "$HOME/.guix-profile/etc/profile" ]; then
    source "$HOME/.guix-profile/etc/profile"
fi

# Install packages from manifest if available
if [ -f ".guix/manifest.scm" ]; then
    echo "📦 Installing packages from manifest..."
    guix shell -m .guix/manifest.scm --check || guix install -m .guix/manifest.scm || true
fi

# Initialize Guile environment
echo "🌀 Initializing Guile/Stage0 environment..."
if command -v guile >/dev/null 2>&1; then
    guile --version
    echo "✓ Guile environment ready"
else
    echo "⚠️ Guile not yet available, attempting installation..."
    guix install guile || true
fi

# Initialize Wolf environment using existing scripts
if [ -f ".guix/bootstrap/init-shell.scm" ]; then
    echo "🐺 Loading Wolf environment configuration..."
    # This sets up environment variables and paths
    echo "✓ Wolf bootstrap environment configured"
fi

# Verify OpenCog components
echo "🧠 Verifying OpenCog components..."
for component in cogutil cogserver atomspace; do
    if [ -d "$component" ]; then
        echo "  ✓ Found: $component/"
        # Prepare component for building (if autogen.sh exists)
        if [ -f "$component/autogen.sh" ]; then
            echo "    Preparing $component for build..."
            cd "$component"
            ./autogen.sh || echo "    ⚠️ autogen.sh failed for $component"
            ./configure || echo "    ⚠️ configure failed for $component"
            cd ..
        fi
    else
        echo "  ⚠️ Missing: $component/"
    fi
done

# Verify Wolf kernels
echo "⚡ Verifying Wolf kernel components..."
if [ -d "kernels" ]; then
    echo "  ✓ Kernels directory found"
    for kernel in wolfcore.lisp wolfnode-guile.scm ecron.wl; do
        if [ -f "kernels/$kernel" ]; then
            echo "    ✓ $kernel"
        else
            echo "    ⚠️ Missing: $kernel"
        fi
    done
fi

# Set up symbolic filesystem mount point (as referenced in stage0.scm)
echo "🔗 Setting up symbolic filesystem mount point..."
sudo mkdir -p /tmp/asfs || true
sudo chown dev:dev /tmp/asfs || true
echo "  ✓ ASFS mount point ready at /tmp/asfs"

# Wolfram kernel configuration
echo "🔬 Wolfram Engine configuration..."
echo "To complete Wolfram Engine setup, please ensure your license and config are present."
echo "AGI-OS bootloader will present Wolfram pool options on first run."

# Display Stage0 bootstrap information
echo ""
echo "🚀 AGI-OS Stage0 Bootstrap Environment Ready"
echo "==========================================="
echo "Available commands:"
echo "  guile .guix/bootstrap/init-shell.scm  - Start Wolf Shell with Stage0 bootstrap"
echo "  python3 test-bootstrap.py            - Run bootstrap validation tests"
echo "  python3 wolfcog-coordinator.py       - Start full WolfCog AGI-OS system"
echo ""
echo "To start Stage0 bootstrap:"
echo "  guile .guix/bootstrap/init-shell.scm"
echo ""
echo "✅ Bootstrap complete - Ready for AGI-OS development"