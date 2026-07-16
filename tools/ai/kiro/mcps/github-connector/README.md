# GitHub MCP Server

A Model Context Protocol (MCP) server that enables AI assistants to interact with GitHub Enterprise instances. Designed for local use with [Kiro](https://kiro.dev).

## Features

- **Multi-instance support**: Configure and switch between multiple GitHub Enterprise instances
- **Repository browsing**: List repos, get details, read file content
- **Pull Requests**: List and inspect PRs with reviews and changed files
- **Branches & Commits**: Browse branches and commit history
- **Issues**: List and filter issues
- **GitHub Actions**: List workflows and their runs
- **Bearer Token Authentication**: Uses Personal Access Token (PAT) for secure authentication

## Prerequisites

- Python 3.10 or higher
- Access to a GitHub Enterprise instance
- A Personal Access Token (PAT)

### Getting a Personal Access Token (PAT)

1. Go to GitHub Enterprise → **Settings** → **Developer settings** → **Personal access tokens** → **Fine-grained**
2. Click **Generate new token**
3. Set permissions: `Contents: read/write`, `Pull requests: read/write`, `Metadata: read`
4. Copy the generated token

## Installation

### Environment Variables (Recommended)

Set these environment variables:

```bash
export GITHUB_URL="https://github-ix.int.automotive-wan.com/api/v3"
export GITHUB_TOKEN="your_pat_here"
export GITHUB_ORG="your-default-org"
```

### Kiro MCP Configuration

Add to `~/.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "github-connector": {
      "command": "/path/to/venv/bin/python3",
      "args": ["/path/to/mcp.github_connector/src/server.py"],
      "env": {
        "GITHUB_URL": "https://github-ix.int.automotive-wan.com/api/v3",
        "GITHUB_TOKEN": "your_pat_here",
        "GITHUB_ORG": "your-default-org"
      },
      "disabled": false,
      "autoApprove": ["list_github_instances", "get_active_github", "list_repos", "list_pull_requests", "list_branches", "list_commits", "list_issues", "list_workflows", "get_file_content"]
    }
  }
}
```

## Available Tools

| Tool | Description |
|------|-------------|
| `list_github_instances` | List all configured GitHub instances |
| `set_active_github` | Switch the active instance |
| `get_active_github` | Show the currently active instance |
| `test_github_connection` | Test connectivity |
| `list_repos` | List all repos in an organization |
| `get_repo` | Get details of a specific repository |
| `get_file_content` | Read a file from a repository |
| `list_pull_requests` | List PRs (open/closed/all) |
| `get_pull_request` | Get PR details with reviews and changed files |
| `create_pull_request` | Create a new pull request |
| `list_branches` | List all branches |
| `list_commits` | List recent commits |
| `list_issues` | List issues (open/closed/all) |
| `list_workflows` | List GitHub Actions workflows |
| `list_workflow_runs` | List runs for a workflow |

## Project Structure

```
mcp.github_connector/
├── src/
│   ├── server.py           # MCP server and tool definitions
│   └── github_client.py    # GitHub REST API client
├── .env.example            # Documents required environment variables
├── pyproject.toml          # Project configuration
├── requirements.txt        # Dependencies
└── README.md
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "No active GitHub instance set" | Set GITHUB_URL and GITHUB_TOKEN env vars |
| "Authentication failed" | Verify PAT is valid and not expired |
| "Connection closed" in Kiro | Use the full venv Python path in MCP config |
| 403 Forbidden | PAT missing required permissions |
| Connection timeout | Check VPN connectivity |

## License

MIT License
