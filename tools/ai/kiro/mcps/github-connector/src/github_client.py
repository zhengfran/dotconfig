"""
GitHub API Client
Handles connections and operations for GitHub / GitHub Enterprise servers
"""
import requests
from typing import Dict, List, Any, Optional


class GitHubClient:
    """Client for interacting with GitHub REST API"""

    def __init__(self, url: str, token: str):
        self.url = url.rstrip('/')
        self.token = token
        self.timeout = (5, 30)
        self.session = requests.Session()
        self.session.headers.update({
            "Authorization": f"token {token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28"
        })

    def _get(self, endpoint: str, params: Optional[Dict] = None) -> Any:
        url = f"{self.url}/{endpoint.lstrip('/')}"
        response = self.session.get(url, params=params, timeout=self.timeout)
        response.raise_for_status()
        return response.json() if response.content else {}

    def _post(self, endpoint: str, json: Optional[Dict] = None) -> Any:
        url = f"{self.url}/{endpoint.lstrip('/')}"
        response = self.session.post(url, json=json, timeout=self.timeout)
        response.raise_for_status()
        return response.json() if response.content else {}

    def _patch(self, endpoint: str, json: Optional[Dict] = None) -> Any:
        url = f"{self.url}/{endpoint.lstrip('/')}"
        response = self.session.patch(url, json=json, timeout=self.timeout)
        response.raise_for_status()
        return response.json() if response.content else {}

    def test_connection(self) -> Dict[str, Any]:
        try:
            data = self._get("user")
            return {"success": True, "user": data.get("login"), "url": self.url}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Repos ──────────────────────────────────────────────────────────────

    def list_repos(self, org: str) -> Dict[str, Any]:
        try:
            data = self._get(f"orgs/{org}/repos", params={"per_page": 100, "type": "all"})
            repos = [{"name": r["name"], "full_name": r["full_name"],
                      "default_branch": r["default_branch"],
                      "url": r["html_url"], "private": r["private"]} for r in data]
            return {"success": True, "repos": repos, "count": len(repos)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def get_repo(self, owner: str, repo: str) -> Dict[str, Any]:
        try:
            data = self._get(f"repos/{owner}/{repo}")
            return {"success": True, "repo": {
                "name": data["name"], "full_name": data["full_name"],
                "description": data.get("description"),
                "default_branch": data["default_branch"],
                "open_issues": data["open_issues_count"],
                "url": data["html_url"]
            }}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Pull Requests ──────────────────────────────────────────────────────

    def list_pull_requests(self, owner: str, repo: str, state: str = "open") -> Dict[str, Any]:
        try:
            data = self._get(f"repos/{owner}/{repo}/pulls",
                             params={"state": state, "per_page": 50})
            prs = [{"number": pr["number"], "title": pr["title"],
                    "state": pr["state"], "author": pr["user"]["login"],
                    "branch": pr["head"]["ref"], "base": pr["base"]["ref"],
                    "url": pr["html_url"], "created_at": pr["created_at"]} for pr in data]
            return {"success": True, "pull_requests": prs, "count": len(prs)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def get_pull_request(self, owner: str, repo: str, pr_number: int) -> Dict[str, Any]:
        try:
            pr = self._get(f"repos/{owner}/{repo}/pulls/{pr_number}")
            reviews = self._get(f"repos/{owner}/{repo}/pulls/{pr_number}/reviews")
            files = self._get(f"repos/{owner}/{repo}/pulls/{pr_number}/files")
            return {"success": True, "pr": {
                "number": pr["number"], "title": pr["title"],
                "state": pr["state"], "author": pr["user"]["login"],
                "branch": pr["head"]["ref"], "base": pr["base"]["ref"],
                "mergeable": pr.get("mergeable"),
                "commits": pr["commits"], "additions": pr["additions"],
                "deletions": pr["deletions"], "changed_files": pr["changed_files"],
                "url": pr["html_url"],
                "reviews": [{"user": r["user"]["login"], "state": r["state"]} for r in reviews],
                "files": [f["filename"] for f in files]
            }}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def create_pull_request(self, owner: str, repo: str, title: str, head: str,
                            base: str = "main", body: str = "") -> Dict[str, Any]:
        """Create a pull request.

        @param owner: Owner or organization name.
        @param repo: Repository name.
        @param title: PR title.
        @param head: Branch containing the changes.
        @param base: Branch to merge into (default: main).
        @param body: PR description in Markdown.
        @return: Dict with PR details or error.
        """
        try:
            payload = {
                "title": title,
                "head": head,
                "base": base,
                "body": body.replace("\\n", "\n")
            }
            pr = self._post(f"repos/{owner}/{repo}/pulls", json=payload)
            return {"success": True, "pr": {
                "number": pr["number"], "title": pr["title"],
                "state": pr["state"], "author": pr["user"]["login"],
                "branch": pr["head"]["ref"], "base": pr["base"]["ref"],
                "url": pr["html_url"], "created_at": pr["created_at"]
            }}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Branches ───────────────────────────────────────────────────────────

    def list_branches(self, owner: str, repo: str) -> Dict[str, Any]:
        try:
            data = self._get(f"repos/{owner}/{repo}/branches", params={"per_page": 100})
            branches = [{"name": b["name"], "sha": b["commit"]["sha"]} for b in data]
            return {"success": True, "branches": branches, "count": len(branches)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Commits ────────────────────────────────────────────────────────────

    def list_commits(self, owner: str, repo: str, branch: str = None, limit: int = 20) -> Dict[str, Any]:
        try:
            params = {"per_page": limit}
            if branch:
                params["sha"] = branch
            data = self._get(f"repos/{owner}/{repo}/commits", params=params)
            commits = [{"sha": c["sha"][:8], "message": c["commit"]["message"].split("\n")[0],
                        "author": c["commit"]["author"]["name"],
                        "date": c["commit"]["author"]["date"]} for c in data]
            return {"success": True, "commits": commits, "count": len(commits)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Issues ─────────────────────────────────────────────────────────────

    def list_issues(self, owner: str, repo: str, state: str = "open") -> Dict[str, Any]:
        try:
            data = self._get(f"repos/{owner}/{repo}/issues",
                             params={"state": state, "per_page": 50})
            issues = [{"number": i["number"], "title": i["title"],
                       "state": i["state"], "author": i["user"]["login"],
                       "labels": [l["name"] for l in i["labels"]],
                       "url": i["html_url"]} for i in data if "pull_request" not in i]
            return {"success": True, "issues": issues, "count": len(issues)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── Workflows (Actions) ────────────────────────────────────────────────

    def list_workflows(self, owner: str, repo: str) -> Dict[str, Any]:
        try:
            data = self._get(f"repos/{owner}/{repo}/actions/workflows")
            workflows = [{"id": w["id"], "name": w["name"],
                          "state": w["state"], "path": w["path"]} for w in data.get("workflows", [])]
            return {"success": True, "workflows": workflows, "count": len(workflows)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def list_workflow_runs(self, owner: str, repo: str, workflow_id: str,
                           status: str = None) -> Dict[str, Any]:
        try:
            params = {"per_page": 20}
            if status:
                params["status"] = status
            data = self._get(f"repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
                             params=params)
            runs = [{"id": r["id"], "name": r["name"], "status": r["status"],
                     "conclusion": r["conclusion"], "branch": r["head_branch"],
                     "sha": r["head_sha"][:8], "created_at": r["created_at"],
                     "url": r["html_url"]} for r in data.get("workflow_runs", [])]
            return {"success": True, "runs": runs, "count": len(runs)}
        except Exception as e:
            return {"success": False, "error": str(e)}

    # ── File content ───────────────────────────────────────────────────────

    def get_file_content(self, owner: str, repo: str, path: str, ref: str = None) -> Dict[str, Any]:
        try:
            params = {}
            if ref:
                params["ref"] = ref
            data = self._get(f"repos/{owner}/{repo}/contents/{path}", params=params)
            import base64
            content = base64.b64decode(data["content"]).decode("utf-8")
            return {"success": True, "path": path, "content": content,
                    "sha": data["sha"], "size": data["size"]}
        except Exception as e:
            return {"success": False, "error": str(e)}
