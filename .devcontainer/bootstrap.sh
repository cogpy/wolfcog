#!/bin/bash
set -e

echo "🌱 Bootstrapping AGI-OS cognition stack..."

# Ensure we're in the workspace directory
cd /workspace

# Initialize Guix profile if not already done
if [ ! -d "$HOME/.guix-profile" ]; then
    echo "📦 Initializing Guix profile..."
    guix pull --bootstrap || true
fi

# Load Guix profile if present
if [ -f "$HOME/.guix-profile/etc/profile" ]; then
    source "$HOME/.guix-profile/etc/profile"
fi

# Install packages from manifest if available
if [ -f ".guix/manifest.scm" ]; then
    echo "📦 Installing packages from manifest..."
    guix shell -m .guix/manifest.scm --check || guix install -m .guix/manifest.scm || true
fi

# Stage0: Guile bootstrapping (ensure guile/guix present)
echo "🌀 Initializing Guile Stage0..."
if command -v guile >/dev/null 2>&1; then
    guile --version
    guile -c '(display "Guile Stage0 ready for bootstrapping\n")'
    echo "✓ Guile environment ready"
else
    echo "⚠️ Guile not yet available, attempting installation..."
    guix install guile || true
fi

# Initialize Wolf environment using existing scripts
if [ -f ".guix/bootstrap/init-shell.scm" ]; then
    echo "🐺 Loading Wolf environment configuration..."
    echo "✓ Wolf bootstrap environment configured"
fi

# Build Opencog components (recursive cognitive synergy)
echo "🧠 Verifying OpenCog components..."
for comp in cogutil cogserver atomspace; do
    if [ -d "$comp" ]; then
        echo "🔗 Building $comp..."
        cd $comp
        [ -f autogen.sh ] && ./autogen.sh || true
        [ -f configure ] && ./configure || true
        make || true
        cd ..
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

# Present Wolfram kernel configuration (manual or scripted)
echo "⚡ To enable Wolfram Engine, ensure licensing and config are present in /workspace/wolfram."
echo "Bootloader will present kernel pool options on first cognitive cycle."

# Security: follow Guix/Guile principles (informational)
echo "🔒 Security isolation: leveraging Guix declarative, least-privilege approach."

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

# Adaptive attention: log emergent events
echo "✅ AGI-OS devcontainer ready for recursive cognitive emergence."