# Confluence MCP Server

A Model Context Protocol (MCP) server that enables AI assistants to interact with Confluence Data Center instances. Designed for local use with [Kiro](https://kiro.dev).

## Features

- **Page Retrieval**: Fetch page content by numeric page ID or tiny link
- **Page Creation**: Create new pages as children of existing pages
- **Page Editing**: Update existing page content (full storage format support, including complex templates)
- **Search**: Search for pages by title or content using CQL
- **Space Browsing**: List pages within a specific Confluence space
- **Bearer Token Authentication**: Uses Personal Access Token (PAT) for secure Confluence Data Center authentication

## Prerequisites

- Python 3.10 or higher
- Access to a Confluence Data Center instance
- A Personal Access Token (PAT) for Confluence authentication

### Getting a Personal Access Token (PAT)

1. Log in to your Confluence Data Center instance
2. Go to your profile → **Personal Access Tokens**
   (or visit `<your-confluence-url>/plugins/personalaccesstokens/usertokens.action`)
3. Click **Create token**
4. Give it a name and set an expiration
5. Copy the generated token (you won't see it again!)

## Installation

### Quick Install (Recommended — tailored for Kiro)

The GUI installer handles everything: virtual environment, dependencies, and Kiro MCP configuration.

1. Download or clone this repository
2. Double-click `install.bat` (or run `python install.py` from a terminal)
3. In the installer window:
   - Choose an install location (or click "Use current directory")
   - Enter your Confluence base URL and Personal Access Token
   - Click **Install**
4. Restart Kiro

The `confluence-connector` MCP server should now appear in Kiro's MCP panel.

**Note:** If you choose a different install location than the current directory, the installer copies the server files there. This means you can safely delete the original download afterwards. If you install in the current directory, don't move or delete the folder — Kiro depends on it.

### Manual Install

If you prefer to set things up yourself:

#### 1. Clone and set up the environment

```bash
git clone <repository-url>
cd mcp.confluence_connector
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
    "confluence-connector": {
      "command": "/full/path/to/venv/Scripts/python.exe",
      "args": ["/full/path/to/src/server.py"],
      "env": {
        "CONFLUENCE_BASE_URL": "https://central.confluence.automotive.cloud",
        "CONFLUENCE_PAT": "your-personal-access-token"
      },
      "disabled": false,
      "autoApprove": []
    }
  }
}
```

**Important:** Use the full absolute path to the Python executable inside the venv.

#### 3. Restart Kiro

The `confluence-connector` MCP server should now appear in Kiro's MCP panel.

## Available Tools

| Tool | Description |
|------|-------------|
| `get_page_by_id` | Fetch a page by its numeric ID |
| `get_page_by_tiny_link` | Resolve a tiny link (`/x/...`) and fetch the page |
| `search_pages` | Search for pages by title or content (CQL) |
| `list_space_pages` | List pages within a Confluence space |
| `create_page` | Create a new child page under an existing page |
| `update_page` | Update an existing page's content |

## Project Structure

```
mcp.confluence_connector/
├── src/
│   ├── server.py              # MCP server and tool definitions
│   ├── confluence_client.py   # Confluence REST API client
│   └── models.py              # Data models
├── tests/                     # Unit and property-based tests
├── scripts/
│   └── test_auth.py           # Authentication test script
├── install.py                 # GUI installer for Kiro
├── install.bat                # Windows launcher for the installer
├── pyproject.toml             # Project configuration
├── requirements.txt           # Dependencies
└── README.md                  # This file
```

## Development

### Running Tests

```bash
# Activate virtual environment first
pip install -r requirements-dev.txt
pytest tests/ -v
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Authentication failed" | Verify PAT is valid and not expired |
| "Connection closed" in Kiro | Use the full venv Python path in MCP config |
| "Resource not found" | Check the page ID or tiny link is correct |
| Server crashes on start | Check all dependencies are installed |
