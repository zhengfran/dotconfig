#!/usr/bin/env bash
# Launch an MCP server from the co-located mcps/ directory.
# Usage: run-mcp.sh <server-name>
#   e.g. run-mcp.sh jira-connector

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "$0")")" && pwd)"
MCPS_DIR="$SCRIPT_DIR/../mcps"
VENV_DIR="$MCPS_DIR/.venv"
SERVER="${1:-}"

if [ -z "$SERVER" ]; then
    echo "Usage: run-mcp.sh <server-name>" >&2
    exit 1
fi

# Auto-create venv and install all server deps on first run
if [ ! -d "$VENV_DIR" ]; then
    python3 -m venv "$VENV_DIR"
    for req in "$MCPS_DIR"/*/requirements.txt; do
        "$VENV_DIR/bin/pip" install -q -r "$req"
    done
fi

exec "$VENV_DIR/bin/python" "$MCPS_DIR/$SERVER/src/server.py"
