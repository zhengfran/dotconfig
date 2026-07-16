"""
GitHub MCP Server
A Model Context Protocol server for GitHub / GitHub Enterprise integration.
Designed for local use with Kiro.
"""
import json
import os
from pathlib import Path
from mcp.server.fastmcp import FastMCP
from github_client import GitHubClient

mcp = FastMCP("github-mcp")

active_github_name = None
github_clients = {}


def _load_mcp_env_file() -> None:
    """Load env vars from mcp.env file if it exists."""
    candidates = [
        Path.home() / ".kiro" / "settings" / "mcp.env",
        Path(__file__).parent.parent.parent.parent / "settings" / "mcp.env",
    ]
    userprofile = os.environ.get("USERPROFILE", "")
    if userprofile:
        candidates.append(Path(userprofile) / ".kiro" / "settings" / "mcp.env")
    for env_file in candidates:
        if env_file.is_file():
            with open(env_file) as f:
                for line in f:
                    line = line.strip()
                    if not line or line.startswith("#"):
                        continue
                    if line.startswith("export "):
                        line = line[7:]
                    key, _, value = line.partition("=")
                    if key and value:
                        value = value.strip().strip('"').strip("'")
                        os.environ.setdefault(key.strip(), value)
            return


def load_config():
    """Load GitHub instance configurations.

    Priority:
    1. Environment variables (GITHUB_URL, GITHUB_TOKEN, GITHUB_ORG)
    2. mcp.env file
    3. config/github-instances.json file
    """
    _load_mcp_env_file()
    # Try environment variables first
    github_url = os.environ.get("GITHUB_URL")
    github_token = os.environ.get("GITHUB_TOKEN")
    github_org = os.environ.get("GITHUB_ORG", "")
    github_instance_name = os.environ.get("GITHUB_INSTANCE_NAME", "github")

    if github_url and github_token:
        return {
            "instances": [
                {
                    "name": github_instance_name,
                    "organization": github_org,
                    "server_type": "github-enterprise",
                    "url": github_url,
                    "token": github_token
                }
            ],
            "default_active": github_instance_name
        }

    # Fall back to config file
    config_path = Path(__file__).parent.parent / "config" / "github-instances.json"
    if not config_path.exists():
        raise FileNotFoundError(
            f"Config not found at {config_path} and no GITHUB_* environment "
            "variables set. Either set GITHUB_URL and GITHUB_TOKEN env vars, "
            "or create config/github-instances.json."
        )
    with open(config_path, 'r') as f:
        return json.load(f)


try:
    config = load_config()
    for instance in config.get("instances", []):
        github_clients[instance["name"]] = GitHubClient(
            url=instance["url"],
            token=instance["token"]
        )
    active_github_name = config.get("default_active")
except (FileNotFoundError, Exception) as e:
    import sys
    print(f"Warning: GitHub config not loaded: {e}", file=sys.stderr)
    config = {"instances": []}


# ── Instance management ────────────────────────────────────────────────────

@mcp.tool()
def list_github_instances() -> dict:
    """List all configured GitHub instances"""
    instances = []
    for name, client in github_clients.items():
        cfg = next((i for i in config.get("instances", []) if i["name"] == name), None)
        instances.append({
            "name": name,
            "url": client.url,
            "organization": cfg.get("organization") if cfg else None,
            "server_type": cfg.get("server_type") if cfg else None,
            "is_active": name == active_github_name
        })
    return {"success": True, "instances": instances, "active": active_github_name}


@mcp.tool()
def set_active_github(name: str) -> dict:
    """Set the active GitHub instance"""
    global active_github_name
    if name not in github_clients:
        return {"success": False, "error": f"Instance '{name}' not found. Available: {list(github_clients.keys())}"}
    active_github_name = name
    return {"success": True, "message": f"Active GitHub instance set to '{name}'"}


@mcp.tool()
def get_active_github() -> dict:
    """Get the currently active GitHub instance"""
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    return {"success": True, "name": active_github_name, "url": github_clients[active_github_name].url}


@mcp.tool()
def test_github_connection() -> dict:
    """Test connection to the active GitHub instance"""
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].test_connection()
    result["instance"] = active_github_name
    return result


# ── Repos ──────────────────────────────────────────────────────────────────

@mcp.tool()
def list_repos(org: str) -> dict:
    """List all repositories for an organization

    Args:
        org: Organization name (e.g., "my-org")
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_repos(org)
    result["instance"] = active_github_name
    return result


@mcp.tool()
def get_repo(owner: str, repo: str) -> dict:
    """Get details of a specific repository

    Args:
        owner: Owner or organization name
        repo: Repository name
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].get_repo(owner, repo)
    result["instance"] = active_github_name
    return result


# ── Pull Requests ──────────────────────────────────────────────────────────

@mcp.tool()
def list_pull_requests(owner: str, repo: str, state: str = "open") -> dict:
    """List pull requests for a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
        state: PR state — "open", "closed", or "all" (default: "open")
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_pull_requests(owner, repo, state)
    result["instance"] = active_github_name
    return result


@mcp.tool()
def get_pull_request(owner: str, repo: str, pr_number: int) -> dict:
    """Get detailed info about a specific pull request

    Args:
        owner: Owner or organization name
        repo: Repository name
        pr_number: PR number
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].get_pull_request(owner, repo, pr_number)
    result["instance"] = active_github_name
    return result


@mcp.tool()
def create_pull_request(owner: str, repo: str, title: str, head: str,
                        base: str = "main", body: str = "") -> dict:
    """Create a pull request

    Args:
        owner: Owner or organization name
        repo: Repository name
        title: PR title (should reference subtask Jira key, e.g. "IIP-12346: Add feature")
        head: Branch containing the changes (e.g. "feature/IIP-12346")
        base: Branch to merge into (default: "main")
        body: PR description in Markdown (optional)
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].create_pull_request(owner, repo, title, head, base, body)
    result["instance"] = active_github_name
    return result


# ── Branches & Commits ─────────────────────────────────────────────────────

@mcp.tool()
def list_branches(owner: str, repo: str) -> dict:
    """List all branches of a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_branches(owner, repo)
    result["instance"] = active_github_name
    return result


@mcp.tool()
def list_commits(owner: str, repo: str, branch: str = None, limit: int = 20) -> dict:
    """List recent commits for a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
        branch: Branch name (default: default branch)
        limit: Number of commits to return (default: 20)
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_commits(owner, repo, branch, limit)
    result["instance"] = active_github_name
    return result


# ── Issues ─────────────────────────────────────────────────────────────────

@mcp.tool()
def list_issues(owner: str, repo: str, state: str = "open") -> dict:
    """List issues for a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
        state: Issue state — "open", "closed", or "all" (default: "open")
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_issues(owner, repo, state)
    result["instance"] = active_github_name
    return result


# ── GitHub Actions ─────────────────────────────────────────────────────────

@mcp.tool()
def list_workflows(owner: str, repo: str) -> dict:
    """List GitHub Actions workflows for a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_workflows(owner, repo)
    result["instance"] = active_github_name
    return result


@mcp.tool()
def list_workflow_runs(owner: str, repo: str, workflow_id: str, status: str = None) -> dict:
    """List runs for a specific GitHub Actions workflow

    Args:
        owner: Owner or organization name
        repo: Repository name
        workflow_id: Workflow ID or filename (e.g., "ci.yml")
        status: Filter by status — "completed", "in_progress", "queued" (optional)
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].list_workflow_runs(owner, repo, workflow_id, status)
    result["instance"] = active_github_name
    return result


# ── File content ───────────────────────────────────────────────────────────

@mcp.tool()
def get_file_content(owner: str, repo: str, path: str, ref: str = None) -> dict:
    """Get the content of a file from a repository

    Args:
        owner: Owner or organization name
        repo: Repository name
        path: File path in the repo (e.g., "Jenkinsfile" or "src/main.py")
        ref: Branch, tag, or commit SHA (default: default branch)
    """
    if not active_github_name:
        return {"success": False, "error": "No active GitHub instance set"}
    result = github_clients[active_github_name].get_file_content(owner, repo, path, ref)
    result["instance"] = active_github_name
    return result


if __name__ == "__main__":
    mcp.run()
