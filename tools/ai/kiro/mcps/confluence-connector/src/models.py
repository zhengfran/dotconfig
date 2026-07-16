"""Data models for Confluence MCP Server."""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional
import os
import logging

logger = logging.getLogger(__name__)


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


@dataclass
class ToolResponse:
    """Standardized response structure for MCP tools.
    
    Attributes:
        success: Whether the operation completed successfully
        data: The response data (if successful)
        error: Error message (if unsuccessful)
    """
    success: bool
    data: Any = None
    error: Optional[str] = None
    
    def to_dict(self) -> dict:
        """Convert response to dictionary for serialization.
        
        Returns:
            Dictionary representation of the response
        """
        result = {"success": self.success}
        if self.data is not None:
            result["data"] = self.data
        if self.error is not None:
            result["error"] = self.error
        return result
    
    @classmethod
    def success_response(cls, data: Any) -> "ToolResponse":
        """Create a successful response.
        
        Args:
            data: The response data
            
        Returns:
            ToolResponse with success=True
        """
        return cls(success=True, data=data)
    
    @classmethod
    def error_response(cls, error: str) -> "ToolResponse":
        """Create an error response.
        
        Args:
            error: The error message
            
        Returns:
            ToolResponse with success=False
        """
        return cls(success=False, error=error)


@dataclass
class ConfluenceConfig:
    """Configuration for Confluence API connection.
    
    Attributes:
        base_url: Base URL of the Confluence instance
        pat: Personal Access Token for authentication
        timeout: Request timeout in seconds (default: 30)
    """
    base_url: str
    pat: str
    timeout: int = 30
    
    @classmethod
    def from_environment(cls) -> "ConfluenceConfig":
        """Load configuration from environment variables or config file.
        
        Priority:
            1. Environment variables (CONFLUENCE_BASE_URL, CONFLUENCE_PAT)
            2. Config file at ../config/confluence.json (relative to src/)
        
        Returns:
            ConfluenceConfig instance
            
        Raises:
            ValueError: If required configuration is missing from all sources
        """
        base_url = os.environ.get("CONFLUENCE_BASE_URL")
        pat = os.environ.get("CONFLUENCE_PAT")
        timeout = int(os.environ.get("CONFLUENCE_TIMEOUT", "30"))
        
        # Fallback to mcp.env file if env vars are missing
        if not base_url or not pat:
            _load_mcp_env_file()
            base_url = base_url or os.environ.get("CONFLUENCE_BASE_URL")
            pat = pat or os.environ.get("CONFLUENCE_PAT")
        
        # Fallback to config file if env vars are missing
        if not base_url or not pat:
            config_path = os.path.join(
                os.path.dirname(os.path.abspath(__file__)), 
                "..", "config", "confluence.json"
            )
            if os.path.exists(config_path):
                import json
                with open(config_path, 'r') as f:
                    file_config = json.load(f)
                base_url = base_url or file_config.get("base_url", "")
                pat = pat or file_config.get("pat", "")
                timeout = int(file_config.get("timeout", timeout))
                logger.info(f"Loaded config from file: {config_path}")
        
        missing = []
        if not base_url:
            missing.append("CONFLUENCE_BASE_URL")
            logger.warning("Missing required configuration: CONFLUENCE_BASE_URL")
        if not pat:
            missing.append("CONFLUENCE_PAT")
            logger.warning("Missing required configuration: CONFLUENCE_PAT")
        
        if missing:
            raise ValueError(f"Missing required configuration: {', '.join(missing)}")
        
        # Remove trailing slash from base_url
        base_url = base_url.rstrip("/")
        
        return cls(base_url=base_url, pat=pat, timeout=timeout)


@dataclass
class PageContent:
    """Confluence page content structure.
    
    Attributes:
        id: Page ID
        title: Page title
        space_key: Space key (e.g., "DEV")
        version: Version number
        last_modified: ISO timestamp of last modification
        body_storage: HTML storage format content
        body_plain_text: Extracted plain text content
        url: Web UI URL for the page
    """
    id: str
    title: str
    space_key: str
    version: int
    last_modified: str
    body_storage: str
    body_plain_text: str
    url: str
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization."""
        return {
            "id": self.id,
            "title": self.title,
            "space_key": self.space_key,
            "version": self.version,
            "last_modified": self.last_modified,
            "body_storage": self.body_storage,
            "body_plain_text": self.body_plain_text,
            "url": self.url,
        }


@dataclass
class SearchResult:
    """Search result item structure.
    
    Attributes:
        id: Page ID
        title: Page title
        space_key: Space key
        excerpt: Search result excerpt with highlights
        url: Web UI URL for the page
    """
    id: str
    title: str
    space_key: str
    excerpt: str
    url: str
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization."""
        return {
            "id": self.id,
            "title": self.title,
            "space_key": self.space_key,
            "excerpt": self.excerpt,
            "url": self.url,
        }


@dataclass
class PageSummary:
    """Page summary for space listing.
    
    Attributes:
        id: Page ID
        title: Page title
    """
    id: str
    title: str
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization."""
        return {
            "id": self.id,
            "title": self.title,
        }


@dataclass
class PageTreeNode:
    """A node in a page tree representing a page and its children.
    
    Attributes:
        id: Page ID
        title: Page title
        url: Web UI URL for the page
        children: List of child PageTreeNode instances
    """
    id: str
    title: str
    url: str
    children: list = field(default_factory=list)
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization (recursive)."""
        return {
            "id": self.id,
            "title": self.title,
            "url": self.url,
            "children": [child.to_dict() for child in self.children],
        }
