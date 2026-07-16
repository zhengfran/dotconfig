"""
Jira REST API client module.

Handles HTTP communication with Jira Data Center using Personal Access Token (PAT) authentication.
"""
import logging
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

import requests
from requests.exceptions import ConnectionError, Timeout

logger = logging.getLogger(__name__)

# Default timeout for HTTP requests (seconds)
DEFAULT_TIMEOUT = 30


@dataclass
class JiraConfig:
    """Configuration for Jira API connection."""
    base_url: str  # e.g., "https://jira-ibs.eu.agileci.automotive.cloud"
    pat: str       # Personal Access Token

    @classmethod
    def from_environment(cls) -> "JiraConfig":
        """
        Load configuration from environment variables.
        
        Reads JIRA_BASE_URL and JIRA_PAT from environment.
        Logs warnings for missing variables.
        """
        base_url = os.environ.get("JIRA_BASE_URL", "")
        pat = os.environ.get("JIRA_PAT", "")

        if not base_url:
            logger.warning("Missing required configuration: JIRA_BASE_URL")
        if not pat:
            logger.warning("Missing required configuration: JIRA_PAT")
        
        # Remove trailing slash to avoid double slashes in URLs
        base_url = base_url.rstrip("/")
        
        if base_url:
            logger.info(f"Configured Jira base URL: {base_url}")

        return cls(base_url=base_url, pat=pat)


@dataclass
class ToolResponse:
    """Standard response structure for MCP tools."""
    success: bool
    data: Any = None
    error: Optional[str] = None

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization."""
        result = {"success": self.success}
        if self.data is not None:
            result["data"] = self.data
        if self.error is not None:
            result["error"] = self.error
        return result


@dataclass
class JiraConfigProfile:
    """A named Jira configuration profile for multi-instance support."""
    name: str      # Profile identifier (e.g., "local", "vm", "production")
    base_url: str  # Jira server base URL
    pat: str       # Personal Access Token
    
    def to_config(self) -> JiraConfig:
        """Convert to JiraConfig for client creation."""
        return JiraConfig(base_url=self.base_url, pat=self.pat)
    
    def to_display_dict(self) -> dict:
        """Convert to dict with masked PAT for display."""
        masked_pat = self.pat[:4] + "****" + self.pat[-4:] if len(self.pat) > 8 else "****"
        return {
            "name": self.name,
            "base_url": self.base_url,
            "pat": masked_pat
        }


class ConfigManager:
    """
    Manages multiple Jira configuration profiles.
    
    Supports adding, removing, listing, and switching between profiles.
    Optionally loads default configuration from environment variables.
    """
    
    def __init__(self, load_from_env: bool = True):
        """
        Initialize ConfigManager.
        
        Args:
            load_from_env: If True, load default config from JIRA_BASE_URL/JIRA_PAT
        """
        self._profiles: dict[str, JiraConfigProfile] = {}
        self._active_name: str | None = None
        
        if load_from_env:
            self._load_from_environment()
    
    @staticmethod
    def _load_mcp_env_file() -> None:
        """Load env vars from mcp.env file if it exists (fallback for missing env vars)."""
        # Search common locations for mcp.env
        candidates = [
            Path.home() / ".kiro" / "settings" / "mcp.env",
            Path(__file__).parent.parent.parent.parent / "settings" / "mcp.env",
        ]
        # Also check WSL path if running on Windows
        userprofile = os.environ.get("USERPROFILE", "")
        if userprofile:
            candidates.append(Path(userprofile) / ".kiro" / "settings" / "mcp.env")
        for env_file in candidates:
            if env_file.is_file():
                logger.info(f"Loading env vars from {env_file}")
                with open(env_file) as f:
                    for line in f:
                        line = line.strip()
                        if not line or line.startswith("#"):
                            continue
                        # Strip 'export ' prefix if present
                        if line.startswith("export "):
                            line = line[7:]
                        key, _, value = line.partition("=")
                        if key and value:
                            # Strip surrounding quotes
                            value = value.strip().strip('"').strip("'")
                            os.environ.setdefault(key.strip(), value)
                return
    
    def _load_from_environment(self) -> None:
        """
        Load configurations from environment variables.
        
        Supports two patterns:
        1. Legacy: JIRA_BASE_URL + JIRA_PAT -> creates "default" profile
        2. Multi-instance: JIRA_<NAME>_URL + JIRA_<NAME>_PAT -> creates "<name>" profile
           Example: JIRA_LOCAL_URL + JIRA_LOCAL_PAT -> creates "local" profile
                    JIRA_VM_URL + JIRA_VM_PAT -> creates "vm" profile
        """
        # If env vars are missing, try loading from mcp.env file
        if not os.environ.get("JIRA_BASE_URL"):
            self._load_mcp_env_file()

        # Pattern 1: Legacy single config (JIRA_BASE_URL + JIRA_PAT)
        base_url = os.environ.get("JIRA_BASE_URL", "").rstrip("/")
        pat = os.environ.get("JIRA_PAT", "")
        
        first_profile_name = None
        
        if base_url and pat:
            self._profiles["default"] = JiraConfigProfile(
                name="default",
                base_url=base_url,
                pat=pat
            )
            first_profile_name = "default"
            logger.info(f"Loaded default Jira config from environment: {base_url}")
        
        # Pattern 2: Multi-instance configs (JIRA_<NAME>_URL + JIRA_<NAME>_PAT)
        # Scan environment for JIRA_*_URL patterns
        url_suffix = "_URL"
        pat_suffix = "_PAT"
        prefix = "JIRA_"
        
        for key in os.environ:
            if key.startswith(prefix) and key.endswith(url_suffix):
                # Extract profile name: JIRA_LOCAL_URL -> LOCAL -> local
                name_part = key[len(prefix):-len(url_suffix)]
                if name_part and name_part not in ("BASE",):  # Skip JIRA_BASE_URL
                    profile_name = name_part.lower()
                    pat_key = f"{prefix}{name_part}{pat_suffix}"
                    
                    url_value = os.environ.get(key, "").rstrip("/")
                    pat_value = os.environ.get(pat_key, "")
                    
                    if url_value and pat_value:
                        self._profiles[profile_name] = JiraConfigProfile(
                            name=profile_name,
                            base_url=url_value,
                            pat=pat_value
                        )
                        if first_profile_name is None:
                            first_profile_name = profile_name
                        logger.info(f"Loaded '{profile_name}' Jira config from environment: {url_value}")
        
        # Set first loaded profile as active
        if first_profile_name and self._active_name is None:
            self._active_name = first_profile_name
            logger.info(f"Set '{first_profile_name}' as active Jira configuration")
    
    def add_config(self, name: str, base_url: str, pat: str) -> ToolResponse:
        """
        Add or update a configuration profile.
        
        Args:
            name: Profile name (e.g., "local", "vm", "production")
            base_url: Jira base URL
            pat: Personal Access Token
            
        Returns:
            ToolResponse with success status
        """
        if not name or not name.strip():
            return ToolResponse(success=False, error="Profile name is required")
        if not base_url or not base_url.strip():
            return ToolResponse(success=False, error="Base URL is required")
        if not pat or not pat.strip():
            return ToolResponse(success=False, error="PAT is required")
        
        name = name.strip()
        base_url = base_url.strip().rstrip("/")
        pat = pat.strip()
        
        is_update = name in self._profiles
        self._profiles[name] = JiraConfigProfile(name=name, base_url=base_url, pat=pat)
        
        action = "updated" if is_update else "added"
        logger.info(f"Configuration profile '{name}' {action}: {base_url}")
        
        return ToolResponse(success=True, data={
            "name": name,
            "base_url": base_url,
            "action": action,
            "message": f"Configuration '{name}' {action} successfully"
        })
    
    def remove_config(self, name: str) -> ToolResponse:
        """
        Remove a configuration profile.
        
        Args:
            name: Profile name to remove
            
        Returns:
            ToolResponse with success status
        """
        if not name or name not in self._profiles:
            available = list(self._profiles.keys())
            return ToolResponse(
                success=False,
                error=f"Configuration '{name}' not found. Available: {available}"
            )
        
        del self._profiles[name]
        
        # Clear active if we removed the active config
        if self._active_name == name:
            self._active_name = None
            logger.info(f"Removed active configuration '{name}', no active config now")
        else:
            logger.info(f"Removed configuration '{name}'")
        
        return ToolResponse(success=True, data={
            "name": name,
            "message": f"Configuration '{name}' removed successfully",
            "active_cleared": self._active_name is None
        })
    
    def list_configs(self) -> ToolResponse:
        """
        List all configuration profiles with masked PATs.
        
        Returns:
            ToolResponse with list of profiles
        """
        profiles = [p.to_display_dict() for p in self._profiles.values()]
        
        # Mark the active one
        for p in profiles:
            p["is_active"] = p["name"] == self._active_name
        
        return ToolResponse(success=True, data={
            "profiles": profiles,
            "active": self._active_name,
            "count": len(profiles)
        })
    
    def set_active(self, name: str) -> ToolResponse:
        """
        Set the active configuration by name.
        
        Args:
            name: Profile name to activate
            
        Returns:
            ToolResponse with success status
        """
        if not name or name not in self._profiles:
            available = list(self._profiles.keys())
            return ToolResponse(
                success=False,
                error=f"Configuration '{name}' not found. Available: {available}"
            )
        
        self._active_name = name
        profile = self._profiles[name]
        logger.info(f"Set active configuration to '{name}': {profile.base_url}")
        
        return ToolResponse(success=True, data={
            "name": name,
            "base_url": profile.base_url,
            "message": f"Active configuration set to '{name}'"
        })
    
    def get_active(self) -> ToolResponse:
        """
        Get the currently active configuration info.
        
        Returns:
            ToolResponse with active config info or indication of no active config
        """
        if self._active_name is None or self._active_name not in self._profiles:
            return ToolResponse(success=True, data={
                "active": None,
                "message": "No active configuration. Use add_jira_config and set_active_jira first."
            })
        
        profile = self._profiles[self._active_name]
        return ToolResponse(success=True, data={
            "active": profile.to_display_dict(),
            "message": f"Active configuration: {self._active_name}"
        })
    
    def get_active_config(self) -> JiraConfigProfile | None:
        """
        Get the active configuration object (internal use).
        
        Returns:
            JiraConfigProfile or None if no active config
        """
        if self._active_name is None:
            return None
        return self._profiles.get(self._active_name)


class JiraClient:
    """
    Client for Jira REST API v2.
    
    Uses Bearer token authentication with Personal Access Token (PAT).
    """

    def __init__(self, config: JiraConfig):
        """
        Initialize JiraClient with configuration.
        
        Args:
            config: JiraConfig with base_url and pat
        """
        self.config = config
        self._session = requests.Session()
        self._session.headers.update({
            "Authorization": f"Bearer {config.pat}",
            "Content-Type": "application/json",
            "Accept": "application/json",
        })

    def _handle_http_error(self, response: requests.Response, context: str = "") -> str:
        """
        Generate appropriate error message for HTTP error status codes.
        
        Args:
            response: The HTTP response object
            context: Additional context for the error message
            
        Returns:
            Formatted error message string
        """
        status = response.status_code
        
        if status == 400:
            return f"Bad request: {response.text}"
        elif status == 401:
            return "Authentication failed: Invalid credentials"
        elif status == 403:
            return "Access denied: Insufficient permissions"
        elif status == 404:
            if context:
                return f"{context} not found"
            return "Resource not found"
        elif status == 429:
            retry_after = response.headers.get("Retry-After", "unknown")
            return f"Rate limited: Retry after {retry_after} seconds"
        elif 500 <= status < 600:
            return f"Jira server error: {status}"
        else:
            return f"HTTP error {status}: {response.text}"

    def get_issue(self, issue_key: str, timeout: int = DEFAULT_TIMEOUT) -> ToolResponse:
        """
        Fetch a Jira issue by its key.
        
        Args:
            issue_key: The issue key (e.g., "PROJ-123")
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with issue data or error
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                return ToolResponse(success=True, data=response.json())
            else:
                error_msg = self._handle_http_error(response, f"Issue {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_create_metadata(
        self, 
        project_key: str, 
        issue_type_name: str = "Story",
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get metadata for creating an issue, including required and optional fields.
        
        Uses the new Jira 9.0+ createmeta API endpoints.
        
        Args:
            project_key: The project key (e.g., "DGR")
            issue_type_name: The issue type name (e.g., "Story", "Task", "Bug")
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with create metadata including fields and their requirements
        """
        try:
            # Step 1: Get issue types for the project
            url_types = f"{self.config.base_url}/rest/api/2/issue/createmeta/{project_key}/issuetypes"
            response_types = self._session.get(url_types, timeout=timeout)
            
            if not response_types.ok:
                error_msg = self._handle_http_error(response_types, f"Project {project_key}")
                return ToolResponse(success=False, error=error_msg)
            
            types_data = response_types.json()
            issue_types = types_data.get("values", [])
            
            # Find the requested issue type
            target_type = None
            for it in issue_types:
                if it.get("name", "").lower() == issue_type_name.lower():
                    target_type = it
                    break
            
            if not target_type:
                available = [it.get("name") for it in issue_types[:10]]
                return ToolResponse(
                    success=False,
                    error=f"Issue type '{issue_type_name}' not found. Available: {available}"
                )
            
            issue_type_id = target_type.get("id")
            
            # Step 2: Get fields for the issue type
            url_fields = f"{self.config.base_url}/rest/api/2/issue/createmeta/{project_key}/issuetypes/{issue_type_id}"
            response_fields = self._session.get(url_fields, timeout=timeout)
            
            if not response_fields.ok:
                error_msg = self._handle_http_error(response_fields, "Fields metadata")
                return ToolResponse(success=False, error=error_msg)
            
            fields_data = response_fields.json()
            fields_list = fields_data.get("values", [])
            
            # Convert to dict format for easier consumption
            field_info = {}
            for field in fields_list:
                field_id = field.get("fieldId")
                field_info[field_id] = {
                    "name": field.get("name", field_id),
                    "required": field.get("required", False),
                    "schema": field.get("schema", {}),
                    "has_allowed_values": "allowedValues" in field,
                    "allowed_values_count": len(field.get("allowedValues", []))
                }
            
            return ToolResponse(success=True, data={
                "project": {"key": project_key},
                "issue_type": {"name": target_type.get("name"), "id": issue_type_id},
                "fields": field_info
            })
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_field_options(
        self,
        project_key: str,
        issue_type_name: str,
        field_id: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get allowed values for a specific field in a project/issue type context.
        
        Uses the new Jira 9.0+ createmeta API endpoints.
        
        Args:
            project_key: The project key (e.g., "DGR")
            issue_type_name: The issue type name (e.g., "Story")
            field_id: The field ID (e.g., "customfield_21844") or field name
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of allowed values for the field
        """
        try:
            # Step 1: Get issue types for the project
            url_types = f"{self.config.base_url}/rest/api/2/issue/createmeta/{project_key}/issuetypes"
            response_types = self._session.get(url_types, timeout=timeout)
            
            if not response_types.ok:
                error_msg = self._handle_http_error(response_types, f"Project {project_key}")
                return ToolResponse(success=False, error=error_msg)
            
            types_data = response_types.json()
            issue_types = types_data.get("values", [])
            
            # Find the requested issue type
            target_type = None
            for it in issue_types:
                if it.get("name", "").lower() == issue_type_name.lower():
                    target_type = it
                    break
            
            if not target_type:
                return ToolResponse(
                    success=False,
                    error=f"Issue type '{issue_type_name}' not found"
                )
            
            issue_type_id = target_type.get("id")
            
            # Step 2: Get fields for the issue type
            url_fields = f"{self.config.base_url}/rest/api/2/issue/createmeta/{project_key}/issuetypes/{issue_type_id}"
            response_fields = self._session.get(url_fields, timeout=timeout)
            
            if not response_fields.ok:
                error_msg = self._handle_http_error(response_fields, "Fields metadata")
                return ToolResponse(success=False, error=error_msg)
            
            fields_data = response_fields.json()
            fields_list = fields_data.get("values", [])
            
            # Find field by ID or by name (case-insensitive)
            target_field = None
            target_field_id = None
            field_id_lower = field_id.lower()
            
            for field in fields_list:
                fid = field.get("fieldId", "")
                fname = field.get("name", "").lower()
                
                # Exact ID match
                if fid == field_id:
                    target_field = field
                    target_field_id = fid
                    break
                # Name match (case-insensitive, partial match)
                elif field_id_lower in fname or fname in field_id_lower:
                    target_field = field
                    target_field_id = fid
                    # Don't break - continue looking for exact match
            
            if not target_field:
                available_fields = [f"{f.get('fieldId')}: {f.get('name')}" for f in fields_list[:20]]
                return ToolResponse(
                    success=False,
                    error=f"Field '{field_id}' not found. Available fields: {available_fields}"
                )
            
            allowed_values = target_field.get("allowedValues", [])
            
            # Simplify the allowed values
            simplified_values = []
            for val in allowed_values:
                if isinstance(val, dict):
                    simplified_values.append({
                        "id": val.get("id"),
                        "value": val.get("value") or val.get("name"),
                        "name": val.get("name")
                    })
                else:
                    simplified_values.append({"value": val})
            
            return ToolResponse(success=True, data={
                "field_id": target_field_id,
                "field_name": target_field.get("name"),
                "required": target_field.get("required", False),
                "allowed_values": simplified_values
            })
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def create_issue(
        self,
        project_key: str,
        issue_type: str,
        summary: str,
        description: str = "",
        fields: dict = None,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Create a new Jira issue.
        
        Args:
            project_key: The project key (e.g., "DGR")
            issue_type: The issue type name (e.g., "Story", "Task")
            summary: Issue summary/title
            description: Issue description (optional)
            fields: Additional fields as dict of field_id -> value (optional)
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with created issue key and URL
        """
        url = f"{self.config.base_url}/rest/api/2/issue"
        
        # Build the payload
        payload = {
            "fields": {
                "project": {"key": project_key},
                "issuetype": {"name": issue_type},
                "summary": summary,
            }
        }
        
        if description:
            payload["fields"]["description"] = description
        
        # Add any additional fields
        if fields:
            for field_id, value in fields.items():
                payload["fields"][field_id] = value
        
        try:
            response = self._session.post(url, json=payload, timeout=timeout)
            
            if response.ok:
                data = response.json()
                issue_key = data.get("key")
                return ToolResponse(success=True, data={
                    "key": issue_key,
                    "id": data.get("id"),
                    "url": f"{self.config.base_url}/browse/{issue_key}"
                })
            else:
                error_msg = self._handle_http_error(response, "Create issue")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )
    def update_issue(
        self,
        issue_key: str,
        summary: str = "",
        description: str = "",
        fields: dict = None,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Update an existing Jira issue.
        
        Args:
            issue_key: The issue key (e.g., "DGR-12345")
            summary: New summary/title (optional, only updated if provided)
            description: New description (optional, only updated if provided)
            fields: Additional fields as dict of field_id -> value (optional)
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with success status
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}"
        
        # Build the payload with only provided fields
        payload = {"fields": {}}
        
        if summary:
            payload["fields"]["summary"] = summary
        
        if description:
            payload["fields"]["description"] = description
        
        # Add any additional fields
        if fields:
            for field_id, value in fields.items():
                payload["fields"][field_id] = value
        
        # Don't make request if no fields to update
        if not payload["fields"]:
            return ToolResponse(
                success=False,
                error="No fields provided to update"
            )
        
        try:
            response = self._session.put(url, json=payload, timeout=timeout)
            
            if response.ok:
                return ToolResponse(success=True, data={
                    "key": issue_key,
                    "url": f"{self.config.base_url}/browse/{issue_key}",
                    "message": "Issue updated successfully"
                })
            else:
                error_msg = self._handle_http_error(response, f"Update issue {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_transitions(
        self,
        issue_key: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get available transitions for an issue.
        
        Args:
            issue_key: The issue key (e.g., "DGR-12345")
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of available transitions
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/transitions"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                data = response.json()
                transitions = data.get("transitions", [])
                # Simplify the response
                simplified = []
                for t in transitions:
                    simplified.append({
                        "id": t.get("id"),
                        "name": t.get("name"),
                        "to_status": t.get("to", {}).get("name"),
                        "to_status_category": t.get("to", {}).get("statusCategory", {}).get("name")
                    })
                return ToolResponse(success=True, data=simplified)
            else:
                error_msg = self._handle_http_error(response, f"Transitions for {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def transition_issue(
        self,
        issue_key: str,
        transition_id: str,
        comment: str = "",
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Transition an issue to a new status.
        
        Args:
            issue_key: The issue key (e.g., "DGR-12345")
            transition_id: The transition ID to execute
            comment: Optional comment to add during transition
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with success status
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/transitions"
        
        payload = {
            "transition": {"id": transition_id}
        }
        
        # Add comment if provided
        if comment:
            payload["update"] = {
                "comment": [{"add": {"body": comment}}]
            }
        
        try:
            response = self._session.post(url, json=payload, timeout=timeout)
            
            if response.ok:
                return ToolResponse(success=True, data={
                    "key": issue_key,
                    "url": f"{self.config.base_url}/browse/{issue_key}",
                    "message": "Issue transitioned successfully"
                })
            else:
                error_msg = self._handle_http_error(response, f"Transition issue {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def search_issues(
        self,
        jql: str,
        max_results: int = 50,
        fields: str = "key,summary,status,issuetype,assignee,reporter,created,updated",
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Search for issues using JQL (Jira Query Language).
        
        Args:
            jql: JQL query string (e.g., 'project = DGR AND status = Open')
            max_results: Maximum number of results to return (default 50, max 100)
            fields: Comma-separated list of fields to return
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of matching issues
        """
        url = f"{self.config.base_url}/rest/api/2/search"
        
        # Cap max_results at 100 for safety
        max_results = min(max_results, 100)
        
        params = {
            "jql": jql,
            "maxResults": max_results,
            "fields": fields
        }
        
        try:
            response = self._session.get(url, params=params, timeout=timeout)
            
            if response.ok:
                data = response.json()
                issues = data.get("issues", [])
                
                # Simplify the response
                simplified = []
                for issue in issues:
                    fields_data = issue.get("fields", {})
                    simplified.append({
                        "key": issue.get("key"),
                        "summary": fields_data.get("summary"),
                        "status": fields_data.get("status", {}).get("name") if fields_data.get("status") else None,
                        "issuetype": fields_data.get("issuetype", {}).get("name") if fields_data.get("issuetype") else None,
                        "assignee": fields_data.get("assignee", {}).get("displayName") if fields_data.get("assignee") else None,
                        "reporter": fields_data.get("reporter", {}).get("displayName") if fields_data.get("reporter") else None,
                        "created": fields_data.get("created"),
                        "updated": fields_data.get("updated"),
                    })
                
                return ToolResponse(success=True, data={
                    "total": data.get("total", 0),
                    "max_results": max_results,
                    "issues": simplified
                })
            else:
                error_msg = self._handle_http_error(response, "Search")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_fields(self, timeout: int = DEFAULT_TIMEOUT) -> ToolResponse:
        """
        Fetch all field definitions from Jira.
        
        Returns:
            ToolResponse with list of field definitions or error
        """
        url = f"{self.config.base_url}/rest/api/2/field"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                return ToolResponse(success=True, data=response.json())
            else:
                error_msg = self._handle_http_error(response, "Fields")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def create_comment(
        self,
        issue_key: str,
        body: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Add a comment to a Jira issue.
        
        Args:
            issue_key: The issue key (e.g., "PROJ-123")
            body: Comment text (supports Jira wiki markup)
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with comment ID and URL or error
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/comment"
        
        payload = {"body": body}
        
        try:
            response = self._session.post(url, json=payload, timeout=timeout)
            
            if response.ok:
                data = response.json()
                comment_id = data.get("id")
                return ToolResponse(success=True, data={
                    "id": comment_id,
                    "issue_key": issue_key,
                    "url": f"{self.config.base_url}/browse/{issue_key}?focusedCommentId={comment_id}"
                })
            else:
                error_msg = self._handle_http_error(response, f"Comment on {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def create_attachment(
        self,
        issue_key: str,
        file_path: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Add an attachment to a Jira issue.
        
        Args:
            issue_key: The issue key (e.g., "PROJ-123")
            file_path: Path to the file to upload
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with attachment ID, filename, and URL or error
        """
        import os
        from pathlib import Path
        
        # Validate file exists
        if not os.path.exists(file_path):
            return ToolResponse(
                success=False,
                error=f"File not found: {file_path}"
            )
        
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/attachments"
        
        # Get filename from path
        filename = Path(file_path).name
        
        try:
            # Read file content
            with open(file_path, 'rb') as f:
                file_content = f.read()
            
            # Prepare multipart form data
            files = {'file': (filename, file_content)}
            
            # Need special headers for attachment upload
            # Remove Content-Type (requests will set it for multipart)
            # Add X-Atlassian-Token to bypass XSRF check
            headers = {
                "Authorization": f"Bearer {self.config.pat}",
                "Accept": "application/json",
                "X-Atlassian-Token": "no-check"
            }
            
            response = requests.post(
                url,
                files=files,
                headers=headers,
                timeout=timeout
            )
            
            if response.ok:
                data = response.json()
                # Jira returns a list of attachments (usually just one)
                if data and len(data) > 0:
                    attachment = data[0]
                    return ToolResponse(success=True, data={
                        "id": attachment.get("id"),
                        "filename": attachment.get("filename"),
                        "issue_key": issue_key,
                        "size": attachment.get("size"),
                        "mime_type": attachment.get("mimeType"),
                        "url": attachment.get("content")
                    })
                else:
                    return ToolResponse(success=True, data={
                        "issue_key": issue_key,
                        "message": "Attachment uploaded but no details returned"
                    })
            else:
                error_msg = self._handle_http_error(response, f"Attachment on {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except IOError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to read file: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_link_types(self, timeout: int = DEFAULT_TIMEOUT) -> ToolResponse:
        """
        Get all available issue link types from Jira.
        
        Args:
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of link types, each containing:
            - id: Link type ID
            - name: Link type name (e.g., "Blocks", "Duplicate")
            - inward: Inward description (e.g., "is blocked by")
            - outward: Outward description (e.g., "blocks")
        """
        url = f"{self.config.base_url}/rest/api/2/issueLinkType"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                data = response.json()
                link_types = data.get("issueLinkTypes", [])
                
                # Transform to simplified format
                simplified = []
                for lt in link_types:
                    simplified.append({
                        "id": lt.get("id"),
                        "name": lt.get("name"),
                        "inward": lt.get("inward"),
                        "outward": lt.get("outward")
                    })
                
                return ToolResponse(success=True, data=simplified)
            else:
                error_msg = self._handle_http_error(response, "Link types")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def create_link(
        self,
        inward_issue_key: str,
        outward_issue_key: str,
        link_type: str,
        comment: str = "",
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Create a link between two issues.
        
        The link direction matters:
        - inward_issue_key receives the "inward" description
        - outward_issue_key receives the "outward" description
        
        Example: For "blocks/is blocked by" link type:
        - create_link("PROJ-2", "PROJ-1", "Blocks")
        - Result: PROJ-1 blocks PROJ-2 (PROJ-2 is blocked by PROJ-1)
        
        Args:
            inward_issue_key: The issue that receives the inward description
            outward_issue_key: The issue that receives the outward description
            link_type: Link type name or ID
            comment: Optional comment to add with the link
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with success status and link details
        """
        url = f"{self.config.base_url}/rest/api/2/issueLink"
        
        # Build the payload
        payload = {
            "type": {
                "name": link_type
            },
            "inwardIssue": {
                "key": inward_issue_key
            },
            "outwardIssue": {
                "key": outward_issue_key
            }
        }
        
        # Add comment if provided
        if comment:
            payload["comment"] = {
                "body": comment
            }
        
        try:
            response = self._session.post(url, json=payload, timeout=timeout)
            
            if response.ok:
                # Jira returns 201 Created with no body on success
                return ToolResponse(success=True, data={
                    "inward_issue": inward_issue_key,
                    "outward_issue": outward_issue_key,
                    "link_type": link_type,
                    "message": f"Link created: {outward_issue_key} -> {inward_issue_key}"
                })
            else:
                error_msg = self._handle_http_error(response, "Create link")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_issue_links(
        self,
        issue_key: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get all links for a specific issue.
        
        Args:
            issue_key: The issue key to get links for
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of links, each containing:
            - id: Link ID (for deletion)
            - type: Link type name
            - direction: "inward" or "outward"
            - description: The link description for this direction
            - linked_issue: Key and summary of linked issue
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}"
        params = {"fields": "issuelinks"}
        
        try:
            response = self._session.get(url, params=params, timeout=timeout)
            
            if response.ok:
                data = response.json()
                fields = data.get("fields", {})
                issue_links = fields.get("issuelinks", [])
                
                # Transform to simplified format
                links = []
                for link in issue_links:
                    link_type = link.get("type", {})
                    
                    # Determine direction and get linked issue
                    if "inwardIssue" in link:
                        direction = "inward"
                        description = link_type.get("inward", "")
                        linked_issue = link.get("inwardIssue", {})
                    elif "outwardIssue" in link:
                        direction = "outward"
                        description = link_type.get("outward", "")
                        linked_issue = link.get("outwardIssue", {})
                    else:
                        continue  # Skip malformed links
                    
                    links.append({
                        "id": link.get("id"),
                        "type": link_type.get("name"),
                        "direction": direction,
                        "description": description,
                        "linked_issue": {
                            "key": linked_issue.get("key"),
                            "summary": linked_issue.get("fields", {}).get("summary")
                        }
                    })
                
                return ToolResponse(success=True, data={
                    "issue_key": issue_key,
                    "links": links
                })
            else:
                error_msg = self._handle_http_error(response, f"Issue {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def delete_link(
        self,
        link_id: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Delete an issue link by ID.
        
        Args:
            link_id: The link ID to delete (from get_issue_links)
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with success status
        """
        url = f"{self.config.base_url}/rest/api/2/issueLink/{link_id}"
        
        try:
            response = self._session.delete(url, timeout=timeout)
            
            if response.ok:
                # Jira returns 204 No Content on success
                return ToolResponse(success=True, data={
                    "link_id": link_id,
                    "message": f"Link {link_id} deleted successfully"
                })
            else:
                error_msg = self._handle_http_error(response, f"Link {link_id}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def log_work(
        self,
        issue_key: str,
        time_spent: str,
        comment: str = "",
        started: str = "",
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Log work (time tracking) on a Jira issue.
        
        Args:
            issue_key: The issue key (e.g., "PROJ-123")
            time_spent: Time spent in Jira format (e.g., "1h", "30m", "1h 30m", "1d 2h")
            comment: Optional work description/comment
            started: Optional start datetime in ISO format (e.g., "2025-02-06T09:00:00.000+0000")
                    If not provided, defaults to current time
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with worklog ID and details or error
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/worklog"
        
        payload = {
            "timeSpent": time_spent
        }
        
        if comment:
            payload["comment"] = comment
        
        if started:
            payload["started"] = started
        
        try:
            response = self._session.post(url, json=payload, timeout=timeout)
            
            if response.ok:
                data = response.json()
                return ToolResponse(success=True, data={
                    "id": data.get("id"),
                    "issue_key": issue_key,
                    "time_spent": data.get("timeSpent"),
                    "time_spent_seconds": data.get("timeSpentSeconds"),
                    "author": data.get("author", {}).get("displayName"),
                    "started": data.get("started"),
                    "comment": data.get("comment"),
                    "url": f"{self.config.base_url}/browse/{issue_key}?focusedWorklogId={data.get('id')}"
                })
            else:
                error_msg = self._handle_http_error(response, f"Log work on {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def get_worklogs(
        self,
        issue_key: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get all worklogs for a Jira issue.
        
        Args:
            issue_key: The issue key (e.g., "PROJ-123")
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with list of worklogs or error
        """
        url = f"{self.config.base_url}/rest/api/2/issue/{issue_key}/worklog"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                data = response.json()
                worklogs = data.get("worklogs", [])
                
                # Simplify the response
                simplified = []
                for wl in worklogs:
                    simplified.append({
                        "id": wl.get("id"),
                        "author": wl.get("author", {}).get("displayName"),
                        "time_spent": wl.get("timeSpent"),
                        "time_spent_seconds": wl.get("timeSpentSeconds"),
                        "started": wl.get("started"),
                        "comment": wl.get("comment"),
                        "created": wl.get("created"),
                        "updated": wl.get("updated")
                    })
                
                return ToolResponse(success=True, data={
                    "issue_key": issue_key,
                    "total": data.get("total", len(simplified)),
                    "worklogs": simplified
                })
            else:
                error_msg = self._handle_http_error(response, f"Worklogs for {issue_key}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(
                success=False,
                error=f"Request timed out after {timeout} seconds"
            )
        except ConnectionError as e:
            return ToolResponse(
                success=False,
                error=f"Failed to connect to Jira: {str(e)}"
            )
        except Exception as e:
            return ToolResponse(
                success=False,
                error=f"Unexpected error: {str(e)}"
            )

    def _simplify_filter(self, f: dict) -> dict:
        """Extract relevant fields from a raw Jira filter response."""
        return {
            "id": f.get("id"),
            "name": f.get("name"),
            "owner": f.get("owner", {}).get("displayName") if f.get("owner") else None,
            "owner_username": f.get("owner", {}).get("name") if f.get("owner") else None,
            "jql": f.get("jql"),
            "favourite": f.get("favourite", False),
            "url": f"{self.config.base_url}/issues/?filter={f.get('id')}"
        }

    def get_favourite_filters(
        self,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get the authenticated user's favourite filters.

        Args:
            timeout: Request timeout in seconds

        Returns:
            ToolResponse with list of favourite filters
        """
        url = f"{self.config.base_url}/rest/api/2/filter/favourite"

        try:
            response = self._session.get(url, timeout=timeout)

            if response.ok:
                raw_filters = response.json()
                filters = [self._simplify_filter(f) for f in raw_filters]
                return ToolResponse(success=True, data={
                    "total": len(filters),
                    "filters": filters
                })
            else:
                error_msg = self._handle_http_error(response, "Favourite filters")
                return ToolResponse(success=False, error=error_msg)

        except Timeout:
            return ToolResponse(success=False, error=f"Request timed out after {timeout} seconds")
        except ConnectionError as e:
            return ToolResponse(success=False, error=f"Failed to connect to Jira: {str(e)}")
        except Exception as e:
            return ToolResponse(success=False, error=f"Unexpected error: {str(e)}")


    def get_filter(
        self,
        filter_id: str,
        timeout: int = DEFAULT_TIMEOUT
    ) -> ToolResponse:
        """
        Get a specific filter by ID.
        
        Args:
            filter_id: The filter ID
            timeout: Request timeout in seconds
            
        Returns:
            ToolResponse with filter details including JQL
        """
        url = f"{self.config.base_url}/rest/api/2/filter/{filter_id}"
        
        try:
            response = self._session.get(url, timeout=timeout)
            
            if response.ok:
                f = response.json()
                data = self._simplify_filter(f)
                data["description"] = f.get("description")
                return ToolResponse(success=True, data=data)
            else:
                error_msg = self._handle_http_error(response, f"Filter {filter_id}")
                return ToolResponse(success=False, error=error_msg)
                
        except Timeout:
            return ToolResponse(success=False, error=f"Request timed out after {timeout} seconds")
        except ConnectionError as e:
            return ToolResponse(success=False, error=f"Failed to connect to Jira: {str(e)}")
        except Exception as e:
            return ToolResponse(success=False, error=f"Unexpected error: {str(e)}")
