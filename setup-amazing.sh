#!/bin/bash
# WolfCog Amazing Setup Script
# Makes everything amazing by automating the setup process

set -e

echo "✨ WolfCog AGI-OS Amazing Setup"
echo "================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check dependencies
check_dependencies() {
    log_info "Checking system dependencies..."
    
    # Check Python
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 --version 2>&1 | cut -d' ' -f2)
        log_success "Python 3 found: $PYTHON_VERSION"
    else
        log_error "Python 3 is required but not found"
        exit 1
    fi
    
    # Check cmake (for OpenCog builds)
    if command -v cmake &> /dev/null; then
        CMAKE_VERSION=$(cmake --version | head -1 | cut -d' ' -f3)
        log_success "CMake found: $CMAKE_VERSION"
    else
        log_warning "CMake not found - needed for building OpenCog components"
    fi
    
    # Check Docker (optional but recommended)
    if command -v docker &> /dev/null; then
        DOCKER_VERSION=$(docker --version | cut -d' ' -f3 | tr -d ',')
        log_success "Docker found: $DOCKER_VERSION"
    else
        log_warning "Docker not found - containerized deployment won't be available"
    fi
    
    # Check Git
    if command -v git &> /dev/null; then
        GIT_VERSION=$(git --version | cut -d' ' -f3)
        log_success "Git found: $GIT_VERSION"
    else
        log_error "Git is required but not found"
        exit 1
    fi
}

# Setup directories
setup_directories() {
    log_info "Setting up WolfCog directories..."
    
    # Create essential directories
    mkdir -p /tmp/ecron_tasks
    mkdir -p logs
    mkdir -p data
    
    log_success "Directories created"
}

# Install Python dependencies
install_python_deps() {
    log_info "Installing Python dependencies..."
    
    if [ -f "requirements.txt" ]; then
        python3 -m pip install --user -r requirements.txt
        log_success "Python dependencies installed"
    else
        log_warning "No requirements.txt found, skipping Python dependencies"
    fi
}

# Initialize submodules
init_submodules() {
    log_info "Initializing OpenCog submodules..."
    
    if [ -d ".git" ]; then
        git submodule update --init --recursive
        log_success "Submodules initialized"
    else
        log_warning "Not a git repository, skipping submodule initialization"
    fi
}

# Test system
test_system() {
    log_info "Testing WolfCog system..."
    
    if python3 test-integration.py > /dev/null 2>&1; then
        log_success "All tests passed! System is ready"
        return 0
    else
        log_warning "Some tests failed, but system should still be functional"
        return 1
    fi
}

# Show usage instructions
show_usage() {
    echo ""
    echo "🎉 WolfCog setup complete!"
    echo ""
    echo "💡 Quick Start Commands:"
    echo "   ./wolfcog test      # Run system tests"
    echo "   ./wolfcog start     # Start the AGI-OS"
    echo "   ./wolfcog status    # Check system status"
    echo "   ./wolfcog task flow_name u evaluate  # Create a task"
    echo ""
    echo "📚 Advanced Usage:"
    echo "   python3 wolfcog-coordinator.py     # Direct coordinator start"
    echo "   docker-compose up                  # Containerized deployment"
    echo ""
    echo "🧠 Everything is now amazing! Welcome to WolfCog AGI-OS."
}

# Main setup flow
main() {
    check_dependencies
    setup_directories
    install_python_deps
    init_submodules
    
    echo ""
    log_info "Running system tests..."
    if test_system; then
        echo ""
        show_usage
    else
        echo ""
        log_warning "Setup completed with some test failures"
        echo "You can still use the system, but some features might not work correctly."
        show_usage
    fi
}

# Handle script interruption
trap 'echo ""; log_error "Setup interrupted"; exit 1' INT

# Check if script is being sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi