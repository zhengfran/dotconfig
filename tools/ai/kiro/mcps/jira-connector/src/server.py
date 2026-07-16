"""
Jira MCP Server - FastMCP server with tools for Jira Data Center integration.

Runs in stdio mode for local MCP client integration (e.g., Kiro).
"""
import logging
import os
import re
from pathlib import Path

from mcp.server.fastmcp import FastMCP

from jira_client import JiraClient, JiraConfig, ConfigManager, ToolResponse

# Configure output directory for logs
OUTPUT_DIR = Path(__file__).parent.parent / "output"
OUTPUT_DIR.mkdir(exist_ok=True)
LOG_FILE = OUTPUT_DIR / "server.log"

# Jira ticket key pattern: PROJECT-NUMBER (e.g., PROJ-123, VWICAS23-334534)
TICKET_KEY_PATTERN = re.compile(r"^[A-Z][A-Z0-9]+-\d+$")

# Global ConfigManager instance (manages multiple Jira configurations)
_config_manager: ConfigManager | None = None

# Global MCP server instance (created in main with proper config)
mcp: FastMCP | None = None


def get_config_manager() -> ConfigManager:
    """Get the global ConfigManager instance."""
    global _config_manager
    if _config_manager is None:
        _config_manager = ConfigManager(load_from_env=True)
    return _config_manager


def setup_logging() -> None:
    """Configure logging to file only (stdio mode must not write to stdout/stderr)."""
    file_handler = logging.FileHandler(LOG_FILE)
    file_handler.setFormatter(
        logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    )
    logging.basicConfig(
        level=logging.INFO,
        handlers=[file_handler],
    )


def get_jira_client() -> JiraClient:
    """
    Get a JiraClient for the active configuration.
    
    Returns:
        JiraClient configured with the active Jira instance
        
    Raises:
        Returns error response if no active configuration is set
    """
    manager = get_config_manager()
    config = manager.get_active_config()
    
    if config is None:
        raise ValueError(
            "No active Jira configuration. Use add_jira_config and set_active_jira first."
        )
    
    return JiraClient(config.to_config())


# =============================================================================
# Configuration Management Tools
# =============================================================================

def add_jira_config(name: str, base_url: str, pat: str) -> dict:
    """
    Add or update a Jira configuration profile.
    
    Use this to configure connections to different Jira instances.
    After adding a config, use set_active_jira to activate it.
    
    Args:
        name: Profile name (e.g., "local", "vm", "production")
        base_url: Jira base URL (e.g., "https://jira.example.com")
        pat: Personal Access Token for authentication
        
    Returns:
        ToolResponse with success status and config details
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Adding Jira config: {name}")
    
    manager = get_config_manager()
    response = manager.add_config(name, base_url, pat)
    return response.to_dict()


def remove_jira_config(name: str) -> dict:
    """
    Remove a Jira configuration profile.
    
    If the removed config was active, no config will be active afterwards.
    
    Args:
        name: Profile name to remove
        
    Returns:
        ToolResponse with success status
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Removing Jira config: {name}")
    
    manager = get_config_manager()
    response = manager.remove_config(name)
    return response.to_dict()


def list_jira_configs() -> dict:
    """
    List all configured Jira profiles.
    
    Returns all profiles with their names and URLs (PAT is masked for security).
    Shows which profile is currently active.
    
    Returns:
        ToolResponse with list of profiles and active indicator
    """
    logger = logging.getLogger(__name__)
    logger.info("Listing Jira configs")
    
    manager = get_config_manager()
    response = manager.list_configs()
    return response.to_dict()


def set_active_jira(name: str) -> dict:
    """
    Set the active Jira configuration by name.
    
    All subsequent Jira operations will use this configuration.
    
    Args:
        name: Profile name to activate (must exist)
        
    Returns:
        ToolResponse with success status and active config info
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Setting active Jira config: {name}")
    
    manager = get_config_manager()
    response = manager.set_active(name)
    return response.to_dict()


def get_active_jira() -> dict:
    """
    Get the currently active Jira configuration.
    
    Returns:
        ToolResponse with active config info (name, URL, masked PAT)
        or indication that no config is active
    """
    logger = logging.getLogger(__name__)
    logger.info("Getting active Jira config")
    
    manager = get_config_manager()
    response = manager.get_active()
    return response.to_dict()


def test_jira_connection() -> dict:
    """
    Test connectivity to the active Jira instance.
    
    Attempts to connect to the Jira server and retrieve server info.
    Use this to verify your configuration is correct.
    
    Returns:
        ToolResponse with success status and server info or error details
    """
    logger = logging.getLogger(__name__)
    logger.info("Testing Jira connection")
    
    manager = get_config_manager()
    config = manager.get_active_config()
    
    if config is None:
        return ToolResponse(
            success=False,
            error="No active Jira configuration. Use add_jira_config and set_active_jira first."
        ).to_dict()
    
    try:
        client = JiraClient(config.to_config())
        # Try to get server info as a connectivity test
        import requests
        url = f"{config.base_url}/rest/api/2/serverInfo"
        response = client._session.get(url, timeout=10)
        
        if response.ok:
            data = response.json()
            return ToolResponse(success=True, data={
                "connected": True,
                "base_url": config.base_url,
                "server_title": data.get("serverTitle", "Unknown"),
                "version": data.get("version", "Unknown"),
                "build_number": data.get("buildNumber", "Unknown"),
            }).to_dict()
        else:
            return ToolResponse(
                success=False,
                error=f"Connection failed: HTTP {response.status_code}"
            ).to_dict()
    except Exception as e:
        return ToolResponse(
            success=False,
            error=f"Connection failed: {str(e)}"
        ).to_dict()


# =============================================================================
# Jira Issue Tools (use active configuration)
# =============================================================================

def _get_client_or_error() -> JiraClient | dict:
    """
    Get JiraClient or return error dict if no active config.
    
    Returns:
        JiraClient if active config exists, or error dict
    """
    try:
        return get_jira_client()
    except ValueError as e:
        return ToolResponse(success=False, error=str(e)).to_dict()


def validate_ticket_key(key: str) -> tuple[bool, str]:
    """
    Validate Jira ticket key format.
    
    Args:
        key: The ticket key to validate
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if not key:
        return False, "Invalid ticket key format: empty key. Expected format: PROJECT-NUMBER"
    
    if not TICKET_KEY_PATTERN.match(key):
        return False, f"Invalid ticket key format: {key}. Expected format: PROJECT-NUMBER"
    
    return True, ""


def get_ticket_by_key(ticket_key: str) -> dict:
    """
    Fetch a Jira ticket by its key (e.g., PROJ-123).
    
    Returns complete ticket information including all standard and custom fields.
    Custom field IDs (e.g., customfield_10001) can be correlated with field names
    using the get_custom_field_mappings tool.
    
    Args:
        ticket_key: The Jira ticket key (e.g., PROJ-123, VWICAS23-334534)
        
    Returns:
        ToolResponse dict with success status and ticket data or error message
    """
    logger = logging.getLogger(__name__)
    
    # Validate ticket key format
    is_valid, error_msg = validate_ticket_key(ticket_key)
    if not is_valid:
        logger.warning(f"Invalid ticket key: {ticket_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Fetching ticket: {ticket_key}")
    response = client.get_issue(ticket_key)
    return response.to_dict()


def get_custom_field_mappings() -> dict:
    """
    Get mappings between custom field IDs and their human-readable names.
    
    Returns a list of custom fields with their ID (e.g., customfield_10001),
    name (e.g., "Scrum Team"), and schema type. Use this to understand
    custom field values returned by get_ticket_by_key.
    
    Returns:
        ToolResponse dict with success status and list of field mappings or error
    """
    logger = logging.getLogger(__name__)
    logger.info("Fetching custom field mappings")
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.get_fields()
    
    if not response.success:
        return response.to_dict()
    
    # Filter to custom fields only and extract relevant info
    custom_fields = []
    for field in response.data or []:
        field_id = field.get("id", "")
        if field_id.startswith("customfield_"):
            schema = field.get("schema", {})
            custom_fields.append({
                "id": field_id,
                "name": field.get("name", ""),
                "schema_type": schema.get("type", "") if schema else "",
            })
    
    logger.info(f"Found {len(custom_fields)} custom fields")
    return ToolResponse(success=True, data=custom_fields).to_dict()


def get_create_metadata(project_key: str, issue_type: str = "Story") -> dict:
    """
    IMPORTANT: Call this FIRST before creating any issue to discover required fields.
    
    Gets metadata about what fields are required/available when creating an issue
    in a specific project. This tells you which fields are mandatory and which
    custom fields exist (like Team, Sprint, Epic Link, etc.).
    
    Workflow for creating issues:
    1. Call get_create_metadata to discover required fields
    2. Call get_field_options for any field where you need to match a value (e.g., Team)
    3. Call create_issue with the discovered field IDs and resolved values
    
    Args:
        project_key: The project key (e.g., "DGR", "PROJ")
        issue_type: The issue type name (default: "Story"). Common types: Story, Task, Bug, Epic
        
    Returns:
        ToolResponse with:
        - project: Project info (key, name)
        - issue_type: Issue type info (name, id)
        - fields: Dict of field_id -> {name, required, schema, has_allowed_values}
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Fetching create metadata for {project_key}/{issue_type}")
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.get_create_metadata(project_key, issue_type)
    return response.to_dict()


def get_field_options(project_key: str, issue_type: str, field: str) -> dict:
    """
    Get allowed values for a specific field. Use this to resolve user-friendly names
    to actual Jira values.
    
    For example, if user says "Team Muppets", use this to find the actual value
    "AN_PL1_DevOps_Muppets" and its ID.
    
    The field parameter can be:
    - A field ID (e.g., "customfield_21844")
    - A field name or partial name (e.g., "Team", "Responsible Team")
    
    Args:
        project_key: The project key (e.g., "DGR")
        issue_type: The issue type name (e.g., "Story")
        field: Field ID or name to get options for
        
    Returns:
        ToolResponse with:
        - field_id: The actual field ID
        - field_name: Human-readable field name
        - required: Whether the field is required
        - allowed_values: List of {id, value, name} for each allowed option
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Fetching field options for {field} in {project_key}/{issue_type}")
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.get_field_options(project_key, issue_type, field)
    return response.to_dict()


def create_issue(
    project_key: str,
    issue_type: str,
    summary: str,
    description: str = "",
    labels: str = "",
    epic_link: str = "",
    epic_name: str = "",
    custom_fields: str = ""
) -> dict:
    """
    Create a single Jira issue. Only ONE issue can be created per call for safety.
    
    Supports creating Stories, Epics, Tasks, Bugs, etc.
    - For Epics: use epic_name parameter (required for Epics)
    - For Stories: optionally use epic_link to link to a parent Epic
    - Stories can also be created independently without an Epic
    
    IMPORTANT: Before calling this, you should:
    1. Call get_create_metadata to discover required fields for the project
    2. Call get_field_options to resolve any custom field values (like Team names)
    
    Args:
        project_key: The project key (e.g., "DGR")
        issue_type: The issue type (e.g., "Story", "Task", "Bug", "Epic")
        summary: Issue title/summary (required)
        description: Issue description in Jira wiki markup (optional)
        labels: Comma-separated list of labels (optional, e.g., "AI_&_Data,PI25.5")
        epic_link: For Stories/Tasks - Epic issue key to link to (optional, e.g., "DGR-1234")
        epic_name: For Epics only - the short Epic label/name (required when creating Epics)
        custom_fields: JSON string of additional fields as {"field_id": value} (optional)
                      For select fields, use {"customfield_XXX": {"id": "option_id"}}
                      
    Returns:
        ToolResponse with:
        - key: The created issue key (e.g., "DGR-12345")
        - id: The issue ID
        - url: Direct URL to the issue
        
    Examples:
        # Create an Epic
        create_issue(project_key="DGR", issue_type="Epic", 
                    summary="My Epic Title", epic_name="My Epic Label", ...)
        
        # Create a Story linked to an Epic
        create_issue(project_key="DGR", issue_type="Story",
                    summary="My Story", epic_link="DGR-12345", ...)
        
        # Create a standalone Story (no Epic)
        create_issue(project_key="DGR", issue_type="Story",
                    summary="Independent Story", ...)
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Creating {issue_type} in {project_key}: {summary[:50]}...")
    
    # Validate Epic-specific requirements
    if issue_type.lower() == "epic" and not epic_name:
        return ToolResponse(
            success=False,
            error="epic_name is required when creating an Epic"
        ).to_dict()
    
    # Parse custom fields JSON if provided
    extra_fields = {}
    if custom_fields:
        try:
            import json
            extra_fields = json.loads(custom_fields)
        except json.JSONDecodeError as e:
            return ToolResponse(
                success=False,
                error=f"Invalid custom_fields JSON: {str(e)}"
            ).to_dict()
    
    # Add labels if provided
    if labels:
        label_list = [l.strip() for l in labels.split(",") if l.strip()]
        if label_list:
            extra_fields["labels"] = label_list
    
    # Add epic link if provided (for Stories/Tasks linking to an Epic)
    if epic_link:
        extra_fields["customfield_10006"] = epic_link
    
    # Add epic name if provided (required for Epic creation)
    if epic_name:
        extra_fields["customfield_10007"] = epic_name
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.create_issue(
        project_key=project_key,
        issue_type=issue_type,
        summary=summary,
        description=description,
        fields=extra_fields
    )
    return response.to_dict()


def update_issue(
    issue_key: str,
    summary: str = "",
    description: str = "",
    labels: str = "",
    custom_fields: str = ""
) -> dict:
    """
    Update an existing Jira issue.
    
    Only provided fields will be updated - omitted fields remain unchanged.
    
    Args:
        issue_key: The issue key to update (e.g., "DGR-12345")
        summary: New summary/title (optional)
        description: New description in Jira wiki markup (optional)
        labels: Comma-separated list of labels to SET (optional, replaces existing labels)
        custom_fields: JSON string of additional fields as {"field_id": value} (optional)
                      For select fields, use {"customfield_XXX": {"id": "option_id"}}
                      
    Returns:
        ToolResponse with:
        - key: The updated issue key
        - url: Direct URL to the issue
        - message: Success message
        
    Examples:
        # Update just the description
        update_issue(issue_key="DGR-12345", description="New description...")
        
        # Update summary and add labels
        update_issue(issue_key="DGR-12345", summary="New Title", labels="label1,label2")
        
        # Update custom fields
        update_issue(issue_key="DGR-12345", custom_fields='{"customfield_21844": {"id": "55516"}}')
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Updating issue: {issue_key}")
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Parse custom fields JSON if provided
    extra_fields = {}
    if custom_fields:
        try:
            import json
            extra_fields = json.loads(custom_fields)
        except json.JSONDecodeError as e:
            return ToolResponse(
                success=False,
                error=f"Invalid custom_fields JSON: {str(e)}"
            ).to_dict()
    
    # Add labels if provided
    if labels:
        label_list = [l.strip() for l in labels.split(",") if l.strip()]
        extra_fields["labels"] = label_list
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.update_issue(
        issue_key=issue_key,
        summary=summary,
        description=description,
        fields=extra_fields if extra_fields else None
    )
    return response.to_dict()


def get_transitions(issue_key: str) -> dict:
    """
    Get available transitions for an issue.
    
    Use this to discover what status changes are possible for an issue.
    The transition IDs returned can be used with transition_issue.
    
    Args:
        issue_key: The issue key (e.g., "DGR-12345")
        
    Returns:
        ToolResponse with list of available transitions, each containing:
        - id: Transition ID (use this with transition_issue)
        - name: Transition name (e.g., "Start Progress", "Close")
        - to_status: Target status name
        - to_status_category: Status category (To Do, In Progress, Done)
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Getting transitions for: {issue_key}")
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.get_transitions(issue_key)
    return response.to_dict()


def transition_issue(
    issue_key: str,
    transition_id: str,
    comment: str = ""
) -> dict:
    """
    Transition an issue to a new status.
    
    Use get_transitions first to discover available transitions and their IDs.
    
    Args:
        issue_key: The issue key (e.g., "DGR-12345")
        transition_id: The transition ID to execute (from get_transitions)
        comment: Optional comment to add during the transition
        
    Returns:
        ToolResponse with success status
        
    Example workflow:
        1. get_transitions("DGR-12345") -> returns available transitions
        2. Find the transition you want (e.g., "Start Progress" with id "21")
        3. transition_issue("DGR-12345", "21", "Starting work on this")
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Transitioning issue: {issue_key} with transition {transition_id}")
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.transition_issue(issue_key, transition_id, comment)
    return response.to_dict()


def search_issues(
    jql: str,
    max_results: int = 50
) -> dict:
    """
    Search for issues using JQL (Jira Query Language).
    
    Use this to find issues matching specific criteria, such as:
    - Stories linked to an Epic: '"Epic Link" = DGR-12304'
    - Open issues in a project: 'project = DGR AND status != Closed'
    - Issues assigned to someone: 'assignee = currentUser()'
    - Recently updated: 'updated >= -7d'
    
    Args:
        jql: JQL query string
        max_results: Maximum results to return (default 50, max 100)
        
    Returns:
        ToolResponse with:
        - total: Total matching issues
        - max_results: Results returned
        - issues: List of simplified issue data (key, summary, status, type, assignee, etc.)
        
    Common JQL examples:
        - Find stories in Epic: '"Epic Link" = DGR-12304'
        - Find by team: '"Responsible Team" = "AN_PL1_DevOps_Muppets"'
        - Find open bugs: 'project = DGR AND issuetype = Bug AND status != Closed'
        - Find my issues: 'assignee = currentUser() ORDER BY updated DESC'
    """
    logger = logging.getLogger(__name__)
    logger.info(f"Searching issues with JQL: {jql[:100]}...")
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.search_issues(jql, max_results)
    return response.to_dict()


def add_comment(issue_key: str, body: str) -> dict:
    """
    Add a comment to a Jira issue.
    
    Args:
        issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
        body: Comment text (supports Jira wiki markup)
        
    Returns:
        ToolResponse dict with:
        - id: The comment ID
        - issue_key: The issue the comment was added to
        - url: Direct URL to the comment
    """
    logger = logging.getLogger(__name__)
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Validate comment body is not empty/whitespace
    if not body or not body.strip():
        logger.warning("Empty comment body provided")
        return ToolResponse(
            success=False,
            error="Comment body is required and cannot be empty"
        ).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Adding comment to issue: {issue_key}")
    response = client.create_comment(issue_key, body)
    return response.to_dict()


def add_attachment(issue_key: str, file_path: str) -> dict:
    """
    Add an attachment to a Jira issue.
    
    Args:
        issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
        file_path: Path to the file to upload
        
    Returns:
        ToolResponse dict with:
        - id: The attachment ID
        - filename: The uploaded filename
        - issue_key: The issue the attachment was added to
        - size: File size in bytes
        - mime_type: MIME type of the file
        - url: Direct URL to download the attachment
    """
    import os
    logger = logging.getLogger(__name__)
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Validate file exists
    if not os.path.exists(file_path):
        logger.warning(f"File not found: {file_path}")
        return ToolResponse(
            success=False,
            error=f"File not found: {file_path}"
        ).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Adding attachment to issue: {issue_key}, file: {file_path}")
    response = client.create_attachment(issue_key, file_path)
    return response.to_dict()


# =============================================================================
# Issue Linking Tools
# =============================================================================

def get_link_types() -> dict:
    """
    Get all available issue link types from Jira.
    
    Use this to discover what link types are available before creating links.
    Each link type has an inward and outward description that defines the
    relationship direction.
    
    Returns:
        ToolResponse dict with list of link types, each containing:
        - id: Link type ID
        - name: Link type name (e.g., "Blocks", "Duplicate")
        - inward: Inward description (e.g., "is blocked by")
        - outward: Outward description (e.g., "blocks")
    """
    logger = logging.getLogger(__name__)
    logger.info("Fetching available link types")
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    response = client.get_link_types()
    return response.to_dict()


def create_issue_link(
    inward_issue: str,
    outward_issue: str,
    link_type: str,
    comment: str = ""
) -> dict:
    """
    Create a link between two Jira issues.
    
    The link direction matters:
    - inward_issue receives the "inward" description
    - outward_issue receives the "outward" description
    
    Example: For "blocks/is blocked by" link type:
    - create_issue_link("PROJ-2", "PROJ-1", "Blocks")
    - Result: PROJ-1 blocks PROJ-2 (PROJ-2 is blocked by PROJ-1)
    
    Use get_link_types first to discover available link types.
    
    Args:
        inward_issue: Issue key that receives inward description (e.g., "DGR-12534")
        outward_issue: Issue key that receives outward description (e.g., "DGR-12539")
        link_type: Link type name (e.g., "Blocks", "Duplicate", "Epic-Story Link")
        comment: Optional comment to add with the link
        
    Returns:
        ToolResponse dict with:
        - inward_issue: The inward issue key
        - outward_issue: The outward issue key
        - link_type: The link type used
        - message: Success message
    """
    logger = logging.getLogger(__name__)
    
    # Validate both issue keys
    is_valid, error_msg = validate_ticket_key(inward_issue)
    if not is_valid:
        logger.warning(f"Invalid inward issue key: {inward_issue}")
        return ToolResponse(success=False, error=f"Invalid inward issue: {error_msg}").to_dict()
    
    is_valid, error_msg = validate_ticket_key(outward_issue)
    if not is_valid:
        logger.warning(f"Invalid outward issue key: {outward_issue}")
        return ToolResponse(success=False, error=f"Invalid outward issue: {error_msg}").to_dict()
    
    # Validate link type is not empty
    if not link_type or not link_type.strip():
        logger.warning("Empty link type provided")
        return ToolResponse(
            success=False,
            error="Link type is required and cannot be empty"
        ).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Creating link: {outward_issue} -> {inward_issue} ({link_type})")
    response = client.create_link(inward_issue, outward_issue, link_type.strip(), comment)
    return response.to_dict()


def get_issue_links(issue_key: str) -> dict:
    """
    Get all links for a Jira issue.
    
    Returns both inward and outward links with their types and linked issue information.
    Use this to understand an issue's relationships with other issues.
    
    Args:
        issue_key: The issue key (e.g., "DGR-12345")
        
    Returns:
        ToolResponse dict with:
        - issue_key: The requested issue key
        - links: List of links, each containing:
          - id: Link ID (use for deletion)
          - type: Link type name
          - direction: "inward" or "outward"
          - description: The link description for this direction
          - linked_issue: Key and summary of the linked issue
    """
    logger = logging.getLogger(__name__)
    
    # Validate issue key
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Fetching links for issue: {issue_key}")
    response = client.get_issue_links(issue_key)
    return response.to_dict()


def delete_issue_link(link_id: str) -> dict:
    """
    Delete an issue link by its ID.
    
    Use get_issue_links first to find the link ID you want to delete.
    
    Args:
        link_id: The link ID to delete (from get_issue_links response)
        
    Returns:
        ToolResponse dict with:
        - link_id: The deleted link ID
        - message: Success message
    """
    logger = logging.getLogger(__name__)
    
    # Validate link ID is not empty
    if not link_id or not link_id.strip():
        logger.warning("Empty link ID provided")
        return ToolResponse(
            success=False,
            error="Link ID is required and cannot be empty"
        ).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Deleting link: {link_id}")
    response = client.delete_link(link_id.strip())
    return response.to_dict()


# =============================================================================
# Time Tracking / Work Log Tools
# =============================================================================

def log_work(
    issue_key: str,
    time_spent: str,
    comment: str = "",
    started: str = ""
) -> dict:
    """
    Log work (time tracking) on a Jira issue.
    
    Use this to record time spent working on an issue. The time is added to the
    issue's time tracking and appears in the work log.
    
    Args:
        issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
        time_spent: Time spent in Jira format. Examples:
                   - "1h" (1 hour)
                   - "30m" (30 minutes)
                   - "1h 30m" (1 hour 30 minutes)
                   - "1d" (1 day, typically 8 hours)
                   - "1d 2h 30m" (1 day, 2 hours, 30 minutes)
        comment: Optional work description explaining what was done
        started: Optional start datetime in ISO format (e.g., "2025-02-06T09:00:00.000+0000")
                If not provided, defaults to current time
        
    Returns:
        ToolResponse dict with:
        - id: The worklog entry ID
        - issue_key: The issue the work was logged on
        - time_spent: Human-readable time spent
        - time_spent_seconds: Time spent in seconds
        - author: Who logged the work
        - started: When the work started
        - comment: Work description
        - url: Direct URL to the worklog
    """
    logger = logging.getLogger(__name__)
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Validate time_spent is not empty
    if not time_spent or not time_spent.strip():
        logger.warning("Empty time_spent provided")
        return ToolResponse(
            success=False,
            error="time_spent is required (e.g., '1h', '30m', '1h 30m')"
        ).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Logging work on issue: {issue_key}, time: {time_spent}")
    response = client.log_work(issue_key, time_spent.strip(), comment, started)
    return response.to_dict()


def get_worklogs(issue_key: str) -> dict:
    """
    Get all work log entries for a Jira issue.
    
    Use this to see the time tracking history for an issue, including who worked
    on it, when, and for how long.
    
    Args:
        issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
        
    Returns:
        ToolResponse dict with:
        - issue_key: The requested issue key
        - total: Total number of worklog entries
        - worklogs: List of worklog entries, each containing:
          - id: Worklog entry ID
          - author: Who logged the work
          - time_spent: Human-readable time spent
          - time_spent_seconds: Time spent in seconds
          - started: When the work started
          - comment: Work description
          - created: When the entry was created
          - updated: When the entry was last updated
    """
    logger = logging.getLogger(__name__)
    
    # Validate issue key format
    is_valid, error_msg = validate_ticket_key(issue_key)
    if not is_valid:
        logger.warning(f"Invalid issue key: {issue_key}")
        return ToolResponse(success=False, error=error_msg).to_dict()
    
    # Get client (may return error if no active config)
    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error
    
    logger.info(f"Fetching worklogs for issue: {issue_key}")
    response = client.get_worklogs(issue_key)
    return response.to_dict()


def search_filters(
    owner: str = "",
    name: str = "",
    max_results: int = 50
) -> dict:
    """
    Get the authenticated user's favourite Jira filters.

    Returns all filters the authenticated user has marked as favourite.
    Optionally filter the results by owner username or name substring.

    Args:
        owner: Filter results by owner username (optional, client-side filtering)
        name: Filter results by name substring (optional, client-side filtering)
        max_results: Maximum results to return (default 50)

    Returns:
        ToolResponse dict with:
        - total: Total number of matching filters
        - filters: List of filters, each containing:
          - id: Filter ID
          - name: Filter name
          - owner: Owner display name
          - owner_username: Owner username
          - jql: The JQL query behind the filter
          - favourite: Whether the filter is a favourite
          - url: Direct URL to view the filter results in Jira
    """
    logger = logging.getLogger(__name__)

    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error

    logger.info("Fetching favourite filters")
    response = client.get_favourite_filters()

    # Client-side filtering if requested
    if response.success and (owner or name):
        filters = response.data.get("filters", [])
        if owner:
            filters = [f for f in filters if f.get("owner_username") == owner]
        if name:
            name_lower = name.lower()
            filters = [f for f in filters if name_lower in (f.get("name") or "").lower()]
        response.data["filters"] = filters[:max_results]
        response.data["total"] = len(filters)

    return response.to_dict()


def get_filter_by_id(filter_id: str) -> dict:
    """
    Get a specific Jira filter by its ID.

    Returns full filter details including the JQL query, owner, and a direct URL.

    Args:
        filter_id: The filter ID (e.g., "12345")

    Returns:
        ToolResponse dict with:
        - id: Filter ID
        - name: Filter name
        - description: Filter description
        - owner: Owner display name
        - owner_username: Owner username
        - jql: The JQL query behind the filter
        - favourite: Whether the filter is a favourite
        - url: Direct URL to view the filter results in Jira
    """
    logger = logging.getLogger(__name__)

    if not filter_id or not filter_id.strip():
        return ToolResponse(success=False, error="filter_id is required").to_dict()

    client_or_error = _get_client_or_error()
    if isinstance(client_or_error, dict):
        return client_or_error
    client = client_or_error

    logger.info(f"Fetching filter by ID: {filter_id}")
    response = client.get_filter(filter_id.strip())
    return response.to_dict()


def create_mcp_server() -> FastMCP:
    """Create and configure the MCP server with tools."""
    server = FastMCP("jira-connector")
    
    # ==========================================================================
    # Configuration Management Tools
    # ==========================================================================
    
    server.add_tool(
        add_jira_config,
        name="add_jira_config",
        description="""Add or update a Jira configuration profile.

Use this to configure connections to different Jira instances.
After adding a config, use set_active_jira to activate it.

Args:
    name: Profile name (e.g., "local", "production")
    base_url: Jira base URL (e.g., "https://jira.example.com")
    pat: Personal Access Token for authentication"""
    )
    
    server.add_tool(
        remove_jira_config,
        name="remove_jira_config",
        description="""Remove a Jira configuration profile.

If the removed config was active, no config will be active afterwards.

Args:
    name: Profile name to remove"""
    )
    
    server.add_tool(
        list_jira_configs,
        name="list_jira_configs",
        description="""List all configured Jira profiles.

Returns all profiles with their names and URLs (PAT is masked for security).
Shows which profile is currently active."""
    )
    
    server.add_tool(
        set_active_jira,
        name="set_active_jira",
        description="""Set the active Jira configuration by name.

All subsequent Jira operations will use this configuration.

Args:
    name: Profile name to activate (must exist)"""
    )
    
    server.add_tool(
        get_active_jira,
        name="get_active_jira",
        description="""Get the currently active Jira configuration.

Returns active config info (name, URL, masked PAT) or indication that no config is active."""
    )
    
    server.add_tool(
        test_jira_connection,
        name="test_jira_connection",
        description="""Test connectivity to the active Jira instance.

Attempts to connect to the Jira server and retrieve server info.
Use this to verify your configuration is correct."""
    )
    
    # ==========================================================================
    # Jira Issue Tools
    # ==========================================================================
    
    server.add_tool(
        get_ticket_by_key,
        name="get_ticket_by_key",
        description="""Fetch a Jira ticket by its key (e.g., PROJ-123).
    
Returns complete ticket information including all standard and custom fields.
Custom field IDs (e.g., customfield_10001) can be correlated with field names
using the get_custom_field_mappings tool.

Args:
    ticket_key: The Jira ticket key (e.g., PROJ-123, VWICAS23-334534)
    
Returns:
    ToolResponse dict with success status and ticket data or error message"""
    )
    
    server.add_tool(
        get_custom_field_mappings,
        name="get_custom_field_mappings",
        description="""Get mappings between custom field IDs and their human-readable names.

Returns a list of custom fields with their ID (e.g., customfield_10001),
name (e.g., "Scrum Team"), and schema type. Use this to understand
custom field values returned by get_ticket_by_key.

Returns:
    ToolResponse dict with success status and list of field mappings or error"""
    )
    
    server.add_tool(
        get_create_metadata,
        name="get_create_metadata",
        description="""IMPORTANT: Call this FIRST before creating any issue to discover required fields.

Gets metadata about what fields are required/available when creating an issue
in a specific project. This tells you which fields are mandatory and which
custom fields exist (like Team, Sprint, Epic Link, etc.).

Workflow for creating issues:
1. Call get_create_metadata to discover required fields
2. Call get_field_options for any field where you need to match a value (e.g., Team)
3. Call create_issue with the discovered field IDs and resolved values

Args:
    project_key: The project key (e.g., "DGR", "PROJ")
    issue_type: The issue type name (default: "Story"). Common types: Story, Task, Bug, Epic"""
    )
    
    server.add_tool(
        get_field_options,
        name="get_field_options",
        description="""Get allowed values for a specific field. Use this to resolve user-friendly 
names to actual Jira values.

For example, if user says "Team Muppets", use this to find the actual value
"AN_PL1_DevOps_Muppets" and its ID.

The field parameter can be a field ID (e.g., "customfield_21844") or 
a field name/partial name (e.g., "Team", "Responsible Team").

Args:
    project_key: The project key (e.g., "DGR")
    issue_type: The issue type name (e.g., "Story")
    field: Field ID or name to get options for"""
    )
    
    server.add_tool(
        create_issue,
        name="create_issue",
        description="""Create a single Jira issue. Only ONE issue can be created per call for safety.

Supports creating Stories, Epics, Tasks, Bugs, etc.
- For Epics: use epic_name parameter (required for Epics)
- For Stories: optionally use epic_link to link to a parent Epic
- Stories can also be created independently without an Epic

IMPORTANT: Before calling this, you should:
1. Call get_create_metadata to discover required fields for the project
2. Call get_field_options to resolve any custom field values (like Team names)

Args:
    project_key: The project key (e.g., "DGR")
    issue_type: The issue type (e.g., "Story", "Task", "Bug", "Epic")
    summary: Issue title/summary (required)
    description: Issue description in Jira wiki markup (optional)
    labels: Comma-separated list of labels (optional, e.g., "AI_&_Data,PI25.5")
    epic_link: For Stories/Tasks - Epic issue key to link to (optional, e.g., "DGR-1234")
    epic_name: For Epics only - the short Epic label/name (required when creating Epics)
    custom_fields: JSON string of additional fields as {"field_id": value} (optional)
                  For select fields, use {"customfield_XXX": {"id": "option_id"}}"""
    )
    
    server.add_tool(
        update_issue,
        name="update_issue",
        description="""Update an existing Jira issue.

Only provided fields will be updated - omitted fields remain unchanged.

Args:
    issue_key: The issue key to update (e.g., "DGR-12345")
    summary: New summary/title (optional)
    description: New description in Jira wiki markup (optional)
    labels: Comma-separated list of labels to SET (optional, replaces existing labels)
    custom_fields: JSON string of additional fields as {"field_id": value} (optional)
                  For select fields, use {"customfield_XXX": {"id": "option_id"}}"""
    )
    
    server.add_tool(
        get_transitions,
        name="get_transitions",
        description="""Get available transitions for an issue.

Use this to discover what status changes are possible for an issue.
The transition IDs returned can be used with transition_issue.

Args:
    issue_key: The issue key (e.g., "DGR-12345")
    
Returns list of available transitions with id, name, to_status, to_status_category."""
    )
    
    server.add_tool(
        transition_issue,
        name="transition_issue",
        description="""Transition an issue to a new status.

Use get_transitions first to discover available transitions and their IDs.

Args:
    issue_key: The issue key (e.g., "DGR-12345")
    transition_id: The transition ID to execute (from get_transitions)
    comment: Optional comment to add during the transition
    
Example workflow:
    1. get_transitions("DGR-12345") -> returns available transitions
    2. Find the transition you want (e.g., "Start Progress" with id "21")
    3. transition_issue("DGR-12345", "21", "Starting work on this")"""
    )
    
    server.add_tool(
        search_issues,
        name="search_issues",
        description="""Search for issues using JQL (Jira Query Language).

Use this to find issues matching specific criteria, such as:
- Stories linked to an Epic: '"Epic Link" = DGR-12304'
- Open issues in a project: 'project = DGR AND status != Closed'
- Issues assigned to someone: 'assignee = currentUser()'

Args:
    jql: JQL query string
    max_results: Maximum results to return (default 50, max 100)

Common JQL examples:
    - Find stories in Epic: '"Epic Link" = DGR-12304'
    - Find by team: '"Responsible Team" = "AN_PL1_DevOps_Muppets"'
    - Find open bugs: 'project = DGR AND issuetype = Bug AND status != Closed'"""
    )
    
    server.add_tool(
        add_comment,
        name="add_comment",
        description="""Add a comment to a Jira issue.

Use this to add notes, updates, or communicate with team members on an issue.
Supports Jira wiki markup for formatting.

Args:
    issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
    body: Comment text (supports Jira wiki markup)

Returns:
    - id: The comment ID
    - issue_key: The issue the comment was added to
    - url: Direct URL to the comment"""
    )
    
    server.add_tool(
        add_attachment,
        name="add_attachment",
        description="""Add an attachment to a Jira issue.

Use this to upload files such as logs, screenshots, or documents to an issue.

Args:
    issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
    file_path: Path to the file to upload

Returns:
    - id: The attachment ID
    - filename: The uploaded filename
    - issue_key: The issue the attachment was added to
    - size: File size in bytes
    - mime_type: MIME type of the file
    - url: Direct URL to download the attachment"""
    )
    
    # ==========================================================================
    # Issue Linking Tools
    # ==========================================================================
    
    server.add_tool(
        get_link_types,
        name="get_link_types",
        description="""Get all available issue link types from Jira.

Use this to discover what link types are available before creating links.
Each link type has an inward and outward description that defines the
relationship direction.

Returns:
    - List of link types, each with: id, name, inward, outward"""
    )
    
    server.add_tool(
        create_issue_link,
        name="create_issue_link",
        description="""Create a link between two Jira issues.

The link direction matters:
- inward_issue receives the "inward" description
- outward_issue receives the "outward" description

Example: For "blocks/is blocked by" link type:
- create_issue_link("PROJ-2", "PROJ-1", "Blocks")
- Result: PROJ-1 blocks PROJ-2 (PROJ-2 is blocked by PROJ-1)

Use get_link_types first to discover available link types.

Args:
    inward_issue: Issue key that receives inward description
    outward_issue: Issue key that receives outward description
    link_type: Link type name (e.g., "Blocks", "Duplicate")
    comment: Optional comment to add with the link"""
    )
    
    server.add_tool(
        get_issue_links,
        name="get_issue_links",
        description="""Get all links for a Jira issue.

Returns both inward and outward links with their types and linked issue information.
Use this to understand an issue's relationships with other issues.

Args:
    issue_key: The issue key (e.g., "DGR-12345")

Returns:
    - issue_key: The requested issue key
    - links: List of links with id, type, direction, description, linked_issue"""
    )
    
    server.add_tool(
        delete_issue_link,
        name="delete_issue_link",
        description="""Delete an issue link by its ID.

Use get_issue_links first to find the link ID you want to delete.

Args:
    link_id: The link ID to delete (from get_issue_links response)"""
    )
    
    # ==========================================================================
    # Time Tracking / Work Log Tools
    # ==========================================================================
    
    server.add_tool(
        log_work,
        name="log_work",
        description="""Log work (time tracking) on a Jira issue.

Use this to record time spent working on an issue. The time is added to the
issue's time tracking and appears in the work log.

Args:
    issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")
    time_spent: Time spent in Jira format. Examples:
               - "1h" (1 hour)
               - "30m" (30 minutes)
               - "1h 30m" (1 hour 30 minutes)
               - "1d" (1 day, typically 8 hours)
               - "1d 2h 30m" (1 day, 2 hours, 30 minutes)
    comment: Optional work description explaining what was done
    started: Optional start datetime in ISO format (e.g., "2025-02-06T09:00:00.000+0000")
            If not provided, defaults to current time

Returns:
    - id: The worklog entry ID
    - issue_key: The issue the work was logged on
    - time_spent: Human-readable time spent
    - time_spent_seconds: Time spent in seconds
    - author: Who logged the work
    - url: Direct URL to the worklog"""
    )
    
    server.add_tool(
        get_worklogs,
        name="get_worklogs",
        description="""Get all work log entries for a Jira issue.

Use this to see the time tracking history for an issue, including who worked
on it, when, and for how long.

Args:
    issue_key: The issue key (e.g., "PROJ-123", "DGR-12345")

Returns:
    - issue_key: The requested issue key
    - total: Total number of worklog entries
    - worklogs: List of worklog entries with id, author, time_spent, started, comment"""
    )
    
    # ==========================================================================
    # Filter Tools
    # ==========================================================================
    
    server.add_tool(
        search_filters,
        name="search_filters",
        description="""Get the authenticated user's favourite Jira filters.

Returns all filters the current user has marked as favourite in Jira.
Optionally filter the results client-side by owner username or name substring.

To look up a specific filter by ID, use get_filter_by_id instead.

Args:
    owner: Filter results by owner username (optional, e.g., "uia67906")
    name: Filter results by name substring (optional, e.g., "Sprint")
    max_results: Maximum results to return (default 50)

Returns:
    - total: Total number of matching filters
    - filters: List of filters with id, name, owner, owner_username, jql, favourite, url"""
    )
    
    server.add_tool(
        get_filter_by_id,
        name="get_filter_by_id",
        description="""Get a specific Jira filter by its ID.

Returns full filter details including the JQL query, owner, and a direct URL
to view the filter results in Jira.

Use this after search_filters to get more details about a specific filter,
or when you already know the filter ID.

Args:
    filter_id: The filter ID (e.g., "12345")

Returns:
    - id: Filter ID
    - name: Filter name
    - description: Filter description
    - owner: Owner display name
    - owner_username: Owner username
    - jql: The JQL query behind the filter
    - favourite: Whether the filter is a favourite
    - url: Direct URL to view the filter results in Jira"""
    )
    
    return server


def main() -> None:
    """Main entry point for the MCP server (stdio mode)."""
    global mcp

    setup_logging()
    logger = logging.getLogger(__name__)

    # Initialize ConfigManager (loads from env vars if present)
    manager = get_config_manager()
    active = manager.get_active()
    if active.data.get("active"):
        logger.info(f"Loaded default config from environment: {active.data['active']['base_url']}")
    else:
        logger.info("No default Jira config found. Use add_jira_config to configure.")

    # Create and run MCP server in stdio mode
    mcp = create_mcp_server()
    logger.info("Starting Jira MCP Server in stdio mode")
    mcp.run(transport="stdio")


if __name__ == "__main__":
    main()
