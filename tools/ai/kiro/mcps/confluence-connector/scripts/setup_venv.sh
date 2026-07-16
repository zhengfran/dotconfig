#!/bin/bash
# Setup script for Confluence MCP Server (Unix/Linux/macOS)
# This script creates a virtual environment and installs dependencies

set -e

echo "========================================"
echo "Confluence MCP Server - Setup"
echo "========================================"

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python 3 is not installed or not in PATH"
    exit 1
fi

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    python3 -m venv venv
    echo "Virtual environment created successfully"
else
    echo "Virtual environment already exists"
fi

# Activate virtual environment
echo "Activating virtual environment..."
source venv/bin/activate

# Upgrade pip
echo "Upgrading pip..."
python -m pip install --upgrade pip

# Install runtime dependencies
echo "Installing runtime dependencies..."
pip install -r requirements.txt

# Install development dependencies
echo "Installing development dependencies..."
pip install -r requirements-dev.txt

echo "========================================"
echo "Setup complete!"
echo "========================================"
echo ""
echo "To activate the virtual environment, run:"
echo "  source venv/bin/activate"
echo ""
echo "To run the server:"
echo "  export CONFLUENCE_BASE_URL=https://central.confluence.automotive.cloud"
echo "  export CONFLUENCE_PAT=your-personal-access-token"
echo "  python src/server.py"
echo ""
