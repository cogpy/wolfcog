#!/bin/bash
# Wolfram Engine Installer for AGI-OS Devcontainer
# This is a placeholder script for Wolfram Engine installation

echo "🔬 Wolfram Engine Installer"
echo "=========================="

# Check if WolframScript or Mathematica are available
if command -v wolframscript >/dev/null 2>&1; then
    echo "✓ WolframScript already available"
    wolframscript --version
    exit 0
fi

if command -v mathematica >/dev/null 2>&1; then
    echo "✓ Mathematica already available"
    mathematica --version
    exit 0
fi

# Check for Wolfram Engine installation files
if [ -f "/tmp/WolframEngine.sh" ]; then
    echo "📦 Installing Wolfram Engine from provided installer..."
    bash /tmp/WolframEngine.sh -auto -verbose || echo "⚠️ Wolfram Engine installation failed"
elif [ -f "/opt/Wolfram/WolframEngine" ] || [ -f "/usr/local/Wolfram/WolframEngine" ]; then
    echo "✓ Wolfram Engine found in system paths"
else
    echo "⚠️ Wolfram Engine not found"
    echo ""
    echo "To install Wolfram Engine:"
    echo "1. Download the free Wolfram Engine from https://wolfram.com/engine/"
    echo "2. Place the installer as /tmp/WolframEngine.sh"
    echo "3. Rebuild the devcontainer"
    echo ""
    echo "Alternative: Install Mathematica if you have a license"
    echo ""
    echo "The AGI-OS system will operate in fallback mode without Wolfram kernels."
fi

# Create symbolic link if Wolfram is installed but not in PATH
for wolfram_path in /opt/Wolfram/WolframEngine/*/Executables /usr/local/Wolfram/WolframEngine/*/Executables; do
    if [ -d "$wolfram_path" ] && [ -f "$wolfram_path/wolframscript" ]; then
        echo "🔗 Creating symbolic link for WolframScript..."
        sudo ln -sf "$wolfram_path/wolframscript" /usr/local/bin/wolframscript || true
        break
    fi
done

echo "✓ Wolfram installer script completed"