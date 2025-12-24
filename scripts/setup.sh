#!/usr/bin/env bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Setting up configuration files..."
bash "$SCRIPT_DIR/setup-config.sh"

echo "Installing packages..."
bash "$SCRIPT_DIR/setup-packages.sh"

echo "Setup complete!"