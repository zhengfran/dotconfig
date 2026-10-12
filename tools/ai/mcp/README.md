# MCP servers

One definition of MCP servers for every coding harness.

| File | Role |
|---|---|
| `servers.json` | Canonical `mcpServers`. No secrets, no internal URLs. |
| `bin/mcp-run` | Launcher. Reads each server's credentials from `~/.config/mcp/secrets.env` and starts it from `~/.kiro/mcps` (override with `MCP_SECRETS_FILE` / `MCP_SERVERS_DIR`). |
| `secrets.env.example` | Template for the untracked secrets file. |
| `sync-mcp.sh` | Merges `servers.json` into each harness's user config, keeping other entries. |

Harness targets:

| Harness | File written |
|---|---|
| Pi (built-in MCP) | `~/.pi/agent/mcp.json` |
| Claude Code | user scope, via `claude mcp add-json -s user` |
| Kiro | `~/.kiro/settings/mcp.json` (also gets `autoApprove`) |

## New machine

1. Install the work connectors to `~/.kiro/mcps` (internal setup; the code is not in this repo).
2. `cp secrets.env.example ~/.config/mcp/secrets.env && chmod 600 ~/.config/mcp/secrets.env`, then fill it in.
3. `./sync-mcp.sh` (or `scripts/setup-config.sh`, which calls it).

Servers marked `"secrets": true` are skipped on machines without the secrets
file, so personal machines get no failing entries.

## Adding a server

Add it to `servers.json`. If it needs credentials, set `"command": "mcp-run"`,
add a `case` arm in `bin/mcp-run` listing its variables, and add those
variables to `secrets.env.example`. Then re-run `./sync-mcp.sh`.
