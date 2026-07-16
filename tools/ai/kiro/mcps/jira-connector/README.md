# Jira MCP Server

A Model Context Protocol (MCP) server that enables AI assistants to interact with Jira Data Center instances. Designed for local use with [Kiro](https://kiro.dev).

## Features

- **Dynamic Configuration**: Add, remove, and switch between multiple Jira instances at runtime
- **Issue Management**: Create, update, search, and transition Jira issues
- **Issue Linking**: Create, view, and delete links between issues
- **Comments & Attachments**: Add comments and upload files to issues
- **Time Tracking**: Log work and view work logs on issues
- **Custom Fields**: Retrieve field mappings and allowed values for any field
- **Bearer Token Authentication**: Uses Personal Access Token (PAT) for secure Jira Data Center authentication

## Prerequisites

- Python 3.10 or higher
- Access to a Jira Data Center instance
- A Personal Access Token (PAT) for Jira authentication

### Getting a Personal Access Token (PAT)

1. Log in to your Jira Data Center instance
2. Go to **Profile** → **Personal Access Tokens**
3. Click **Create token**
4. Give it a name and set an expiration
5. Copy the generated token (you won't see it again!)

## Installation

### Quick Install (Recommended)

The GUI installer handles everything: virtual environment, dependencies, and Kiro configuration.

1. Download or clone this repository
2. Double-click `install.bat` (or run `python install.py` from a terminal)
3. In the installer window:
   - Choose an install location (or keep the default `~/.jira-mcp-server`)
   - Enter your Jira base URL and Personal Access Token
   - Click **Install**
4. Restart Kiro

The `jira-connector` MCP server should now appear in Kiro's MCP panel.

**Note:** If you choose a different install location than the current directory, the installer copies the server files there. This means you can safely delete the original download afterwards. If you install in the current directory, don't move or delete the folder — Kiro depends on it.

### Manual Install

If you prefer to set things up yourself:

#### 1. Clone and set up the environment

```bash
git clone <repository-url>
cd jira-mcp-server
python -m venv venv
```

Activate the virtual environment:

**Windows (PowerShell):**
```powershell
venv\Scripts\Activate.ps1
```

**Linux/macOS:**
```bash
source venv/bin/activate
```

Install dependencies:
```bash
pip install -r requirements.txt
```

#### 2. Configure Kiro

Create or edit `~/.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "jira-connector": {
      "command": "/full/path/to/jira-mcp-server/venv/Scripts/python.exe",
      "args": ["/full/path/to/jira-mcp-server/src/server.py"],
      "env": {
        "JIRA_BASE_URL": "https://jira.example.com",
        "JIRA_PAT": "your-personal-access-token"
      },
      "disabled": false,
      "autoApprove": ["list_jira_configs", "get_active_jira"]
    }
  }
}
```

**Important:** Use the full absolute path to the Python executable inside the venv. This avoids "connection closed" issues.

#### 3. Restart Kiro

The `jira-connector` MCP server should now appear in Kiro's MCP panel.

## Configuration

### Default Instance (via environment)

The `env` block in your `mcp.json` sets up a default Jira instance that is automatically activated on startup. This is the simplest approach for most users.

### Multiple Instances (at runtime)

You can add and switch between multiple Jira instances at runtime using the configuration tools:

```
1. add_jira_config(name="production", base_url="https://jira.example.com", pat="your-token")
2. set_active_jira(name="production")
3. test_jira_connection()
4. Start using Jira tools!
```

You can also pre-configure multiple instances via environment variables using the pattern `JIRA_<NAME>_URL` and `JIRA_<NAME>_PAT`:

```json
{
  "mcpServers": {
    "jira-connector": {
      "command": "/path/to/venv/Scripts/python.exe",
      "args": ["/path/to/src/server.py"],
      "env": {
        "JIRA_LOCAL_URL": "https://jira-local.example.com",
        "JIRA_LOCAL_PAT": "your-local-pat",
        "JIRA_PROD_URL": "https://jira-prod.example.com",
        "JIRA_PROD_PAT": "your-prod-pat"
      },
      "disabled": false
    }
  }
}
```

Then switch at runtime:
```
set_active_jira(name="local")
set_active_jira(name="prod")
```

## Available Tools

### Configuration Management

| Tool | Description |
|------|-------------|
| `add_jira_config` | Add or update a Jira configuration profile |
| `remove_jira_config` | Remove a Jira configuration profile |
| `list_jira_configs` | List all configured profiles (PAT masked) |
| `set_active_jira` | Switch the active Jira instance |
| `get_active_jira` | Show the currently active configuration |
| `test_jira_connection` | Test connectivity to the active instance |

### Issue Management

| Tool | Description |
|------|-------------|
| `get_ticket_by_key` | Fetch a ticket by key (e.g., PROJ-123) |
| `get_custom_field_mappings` | Map custom field IDs to human-readable names |
| `get_create_metadata` | Discover required fields before creating an issue |
| `get_field_options` | Get allowed values for a field (e.g., Team names) |
| `create_issue` | Create a Story, Epic, Task, Bug, etc. |
| `update_issue` | Update an existing issue's fields |
| `get_transitions` | List available status transitions |
| `transition_issue` | Move an issue to a new status |
| `search_issues` | Search using JQL queries |

### Comments & Attachments

| Tool | Description |
|------|-------------|
| `add_comment` | Add a comment to an issue |
| `add_attachment` | Upload a file to an issue |

### Issue Linking

| Tool | Description |
|------|-------------|
| `get_link_types` | List available link types |
| `create_issue_link` | Link two issues together |
| `get_issue_links` | Get all links for an issue |
| `delete_issue_link` | Remove a link between issues |

### Time Tracking

| Tool | Description |
|------|-------------|
| `log_work` | Log time spent on an issue |
| `get_worklogs` | View work log history for an issue |

### Filters

| Tool | Description |
|------|-------------|
| `search_filters` | Get the authenticated user's favourite filters |
| `get_filter_by_id` | Get a specific filter's details and JQL |

## Project Structure

```
jira-mcp-server/
├── src/
│   ├── server.py          # MCP server and tool definitions
│   └── jira_client.py     # Jira REST API client + ConfigManager
├── tests/                 # Unit and property-based tests
├── scripts/
│   ├── debug_createmeta.py  # Debug script for create metadata API
│   ├── test_auth.py         # Authentication test script
│   └── test_mcp_tools.py    # MCP tools integration test
├── install.py             # GUI installer for Kiro
├── install.bat            # Windows launcher for the installer
├── pyproject.toml         # Project configuration
├── requirements.txt       # Dependencies
└── README.md              # This file
```

## Development

### Running Tests

```bash
# Activate virtual environment first
pip install -r requirements.txt  # includes dev dependencies
pytest tests/ -v
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "No active Jira configuration" | Use `add_jira_config` and `set_active_jira` first |
| "Invalid URL" error | Check base_url includes `https://` |
| "Authentication failed" | Verify PAT is valid and not expired |
| "Connection closed" in Kiro | Use the full venv Python path in MCP config |
| Server crashes on start | Check all dependencies are installed |

Server logs are written to `output/server.log`.

## License

MIT License
