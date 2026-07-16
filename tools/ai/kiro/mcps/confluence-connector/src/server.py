"""Confluence MCP Server - FastMCP implementation.

This module provides the MCP server that exposes Confluence tools to AI assistants.
"""

import argparse
import logging
import re
import sys
import os

from mcp.server.fastmcp import FastMCP

# Add src directory to path for imports when running directly
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from models import ConfluenceConfig, ToolResponse
from confluence_client import (
    ConfluenceClient,
    ConfluenceError,
    ConfluenceAuthError,
    ConfluenceNotFoundError,
    ConfluenceValidationError
)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Initialize FastMCP server
mcp = FastMCP("confluence-connector")

# Global client instance (initialized on startup)
_client: ConfluenceClient = None


def get_client() -> ConfluenceClient:
    """Get or create the Confluence client instance."""
    global _client
    if _client is None:
        try:
            config = ConfluenceConfig.from_environment()
            _client = ConfluenceClient(config)
            logger.info(f"Confluence client initialized for {config.base_url}")
        except ValueError as e:
            logger.error(f"Failed to initialize Confluence client: {e}")
            raise
    return _client


@mcp.tool()
def get_page_by_id(page_id: str) -> dict:
    """Fetch a Confluence page by its numeric ID.
    
    Retrieves the complete page content including title, body (HTML and plain text),
    space key, version number, and last modified date.
    
    Args:
        page_id: The numeric page ID (e.g., "123456")
        
    Returns:
        ToolResponse dict with page content or error message
    """
    try:
        client = get_client()
        page = client.get_page(page_id)
        
        logger.info(f"Successfully retrieved page {page_id}: {page.title}")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error for page {page_id}: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {page_id}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error for page {page_id}: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error retrieving page {page_id}")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def get_page_by_tiny_link(tiny_link: str) -> dict:
    """Fetch a Confluence page by resolving a tiny link.
    
    Resolves shortened URLs (e.g., /x/hgoFYg) to the actual page and retrieves
    the complete page content.
    
    Args:
        tiny_link: The tiny link URL (e.g., "/x/hgoFYg" or full URL containing /x/)
        
    Returns:
        ToolResponse dict with page content or error message
    """
    try:
        client = get_client()
        
        # Resolve tiny link to page ID
        page_id = client.resolve_tiny_link(tiny_link)
        logger.info(f"Resolved tiny link {tiny_link} to page ID {page_id}")
        
        # Get page content
        page = client.get_page(page_id)
        
        logger.info(f"Successfully retrieved page via tiny link: {page.title}")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error for tiny link {tiny_link}: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Could not resolve tiny link: {tiny_link}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error for tiny link {tiny_link}: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error resolving tiny link {tiny_link}")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def search_pages(query: str, limit: int = 10) -> dict:
    """Search for Confluence pages by title or content.
    
    Uses Confluence Query Language (CQL) to search for pages matching the query.
    
    Args:
        query: Search query text
        limit: Maximum number of results to return (default: 10)
        
    Returns:
        ToolResponse dict with search results or error message
    """
    try:
        client = get_client()
        result = client.search(query, limit=limit)
        
        logger.info(f"Search for '{query}' returned {len(result['results'])} results")
        return ToolResponse.success_response(result).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error for search query: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error during search: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error during search")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def list_space_pages(space_key: str, limit: int = 25, start: int = 0) -> dict:
    """List pages within a Confluence space.
    
    Retrieves a paginated list of pages in the specified space.
    
    Args:
        space_key: The space key (e.g., "DEV", "DOCS")
        limit: Maximum number of results to return (default: 25)
        start: Starting index for pagination (default: 0)
        
    Returns:
        ToolResponse dict with page list or error message
    """
    try:
        client = get_client()
        result = client.list_space_pages(space_key, limit=limit, start=start)
        
        logger.info(f"Listed {len(result['pages'])} pages in space {space_key}")
        return ToolResponse.success_response(result).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error for space key: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Space not found: {space_key}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error listing space {space_key}: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error listing space {space_key}")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def get_page_tree(page_link: str, max_depth: int = 10) -> dict:
    """Get the full page tree (children, grandchildren, etc.) starting from a given page.
    
    Recursively fetches all descendant pages and returns them as a nested tree structure.
    Useful for discovering all sub-pages under a parent page.
    
    The page can be specified by:
    - A numeric page ID (e.g., "4012276902")
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A full Confluence page URL containing /pages/<id>/
    - A full Confluence page URL with pageId parameter
    
    Args:
        page_link: Link or ID of the root page to start from
        max_depth: Maximum depth to traverse (default: 10, use lower values for large trees)
        
    Returns:
        ToolResponse dict with nested page tree or error message
    """
    try:
        client = get_client()
        
        # Resolve to page ID
        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()
        
        tree = client.get_page_tree(page_id, max_depth=max_depth)
        
        # Count total pages in tree
        def count_nodes(node):
            return 1 + sum(count_nodes(c) for c in node.children)
        
        total = count_nodes(tree)
        logger.info(f"Built page tree for {page_id}: {total} pages, max_depth={max_depth}")
        
        return ToolResponse.success_response({
            "root": tree.to_dict(),
            "total_pages": total,
        }).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error for page tree: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found for tree: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error building page tree: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error building page tree")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


def _resolve_page_id(client: ConfluenceClient, page_link: str) -> str:
    """Resolve a page link/URL/ID to a numeric page ID.
    
    Supports:
    - Numeric page ID (e.g., "4012276902")
    - Tiny link (e.g., "/x/hgoFYg")
    - Full URL with /pages/<id>/ path segment
    - Full URL with pageId query parameter
    
    Returns:
        The resolved page ID string, or None if it cannot be resolved.
    """
    page_id = page_link.strip()
    
    if page_id.isdigit():
        return page_id
    
    if "/x/" in page_id:
        return client.resolve_tiny_link(page_id)
    
    # Try to extract from /pages/<id>/ in URL path
    match = re.search(r'/pages/(\d+)/', page_id)
    if match:
        return match.group(1)
    
    if "pageId=" in page_id:
        from urllib.parse import urlparse, parse_qs
        parsed = urlparse(page_id)
        params = parse_qs(parsed.query)
        if "pageId" in params:
            return params["pageId"][0]
    
    return None


@mcp.tool()
def rename_page(page_link: str, new_title: str) -> dict:
    """Rename a Confluence page (title only, body is preserved exactly).
    
    This is the safe way to change a page title without risking body content loss.
    Fetches the current page, keeps the body unchanged, and updates only the title.
    
    The page can be specified by:
    - A numeric page ID (e.g., "4012276902")
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A full Confluence page URL containing /pages/<id>/
    - A full Confluence page URL with pageId parameter
    
    Args:
        page_link: Link or ID of the page to rename
        new_title: The new title for the page
        
    Returns:
        ToolResponse dict with renamed page details or error message
    """
    try:
        client = get_client()
        
        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()
        
        page = client.rename_page(page_id, new_title)
        
        logger.info(f"Renamed page {page_id} to '{new_title}'")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceError as e:
        logger.error(f"Confluence error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except Exception as e:
        logger.exception(f"Unexpected error renaming page")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def create_page(
    parent_page_link: str,
    title: str,
    body: str
) -> dict:
    """Create a new Confluence page as a child of an existing page.
    
    The parent page can be specified by:
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A numeric page ID (e.g., "123456")
    - A full Confluence page URL (e.g., "https://confluence.example.com/display/SPACE/Page+Title")
    
    The body should be provided in Confluence storage format (XHTML).
    Simple HTML like <p>, <h1>, <ul>, <li>, <table>, <b>, <i> works fine.
    
    Args:
        parent_page_link: Link or ID of the parent page to create the child under
        title: Title for the new page
        body: Page content in Confluence storage format (XHTML)
        
    Returns:
        ToolResponse dict with created page details or error message
    """
    try:
        client = get_client()
        
        # Resolve parent to a page ID
        parent_id = parent_page_link.strip()
        if parent_id.isdigit():
            # Already a numeric page ID
            pass
        elif "/x/" in parent_id:
            # Tiny link
            parent_id = client.resolve_tiny_link(parent_id)
        elif "pageId=" in parent_id:
            # URL with pageId param
            from urllib.parse import urlparse, parse_qs
            parsed = urlparse(parent_id)
            params = parse_qs(parsed.query)
            if "pageId" in params:
                parent_id = params["pageId"][0]
            else:
                return ToolResponse.error_response(
                    "Could not extract page ID from URL"
                ).to_dict()
        else:
            return ToolResponse.error_response(
                "Could not determine parent page. Provide a tiny link (/x/...), "
                "a numeric page ID, or a URL with pageId parameter."
            ).to_dict()
        
        # Get space key from parent page
        space_key = client.get_page_space_key(parent_id)
        if not space_key:
            return ToolResponse.error_response(
                f"Could not determine space key for parent page {parent_id}"
            ).to_dict()
        
        page = client.create_page(
            title=title,
            body_storage=body,
            space_key=space_key,
            parent_id=parent_id
        )
        
        logger.info(f"Created page '{title}' (ID: {page.id}) under parent {parent_id}")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error creating page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Parent page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error creating page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error creating page")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def update_page(
    page_link: str,
    body: str,
    title: str = "",
    version_message: str = ""
) -> dict:
    """Update an existing Confluence page's content.
    
    Fetches the current page first to get the version number and title,
    then applies the update. You must provide the FULL body content in
    Confluence storage format (XHTML) — this is a full replacement, not a patch.
    
    Workflow:
    1. Use get_page_by_id or get_page_by_tiny_link to read the current page
    2. Modify the body_storage content as needed
    3. Call update_page with the modified body
    
    The page can be specified by:
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A numeric page ID (e.g., "123456")
    - A full Confluence page URL with pageId parameter
    
    Args:
        page_link: Link or ID of the page to update
        body: Full page content in Confluence storage format (XHTML).
              This replaces the entire page body.
        title: New page title (optional — keeps existing title if empty)
        version_message: Optional version comment (shown in page history)
        
    Returns:
        ToolResponse dict with updated page details or error message
    """
    try:
        client = get_client()
        
        # Resolve to page ID
        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()
        
        # Fetch current page to get version and title
        current_page = client.get_page(page_id)
        
        page_title = title.strip() if title and title.strip() else current_page.title
        
        page = client.update_page(
            page_id=page_id,
            title=page_title,
            body_storage=body,
            version_number=current_page.version,
            version_message=version_message
        )
        
        logger.info(f"Updated page '{page_title}' (ID: {page.id}, version: {page.version})")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error updating page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except ConfluenceError as e:
        logger.error(f"Confluence error updating page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
        
    except Exception as e:
        logger.exception(f"Unexpected error updating page")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def patch_page(
    page_link: str,
    find: str,
    replace: str,
    occurrence: int = 1,
    version_message: str = ""
) -> dict:
    """Partially edit a Confluence page using search-and-replace.

    Instead of sending the full page body, provide a ``find`` snippet and its
    ``replace`` ment.  The server fetches the current body, performs the
    substitution, and pushes the result back — all in one call.

    This is the recommended way to make surgical edits to large pages.

    The page can be specified by:
    - A numeric page ID (e.g., "4012276902")
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A full Confluence page URL containing /pages/<id>/
    - A full Confluence page URL with pageId parameter

    Args:
        page_link: Link or ID of the page to edit
        find: XHTML snippet to locate in the page body (exact match).
              Tip: copy the snippet from the body_storage field returned by
              get_page_by_id to ensure an exact match (watch out for
              non-breaking spaces \\xa0 and HTML entities).
        replace: XHTML snippet to substitute in place of ``find``
        occurrence: Which occurrence to replace (1-based, default 1).
                    Use 0 to replace **all** occurrences.
        version_message: Optional version comment (shown in page history)

    Returns:
        ToolResponse dict with updated page details or error message
    """
    try:
        client = get_client()

        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()

        page = client.patch_page(
            page_id=page_id,
            find=find,
            replace=replace,
            occurrence=occurrence,
            version_message=version_message
        )

        logger.info(f"Patched page '{page.title}' (ID: {page.id}, version: {page.version})")
        return ToolResponse.success_response(page.to_dict()).to_dict()

    except ConfluenceValidationError as e:
        logger.warning(f"Validation error patching page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceError as e:
        logger.error(f"Confluence error patching page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except Exception as e:
        logger.exception(f"Unexpected error patching page")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def insert_into_page(
    page_link: str,
    anchor: str,
    content: str,
    position: str = "after",
    occurrence: int = 1,
    version_message: str = ""
) -> dict:
    """Insert XHTML content before or after an anchor snippet in a Confluence page.

    Locates the ``anchor`` snippet in the page body and inserts ``content``
    immediately before or after it.  Useful for appending rows to tables,
    adding sections, etc.

    The page can be specified by:
    - A numeric page ID (e.g., "4012276902")
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A full Confluence page URL containing /pages/<id>/
    - A full Confluence page URL with pageId parameter

    Args:
        page_link: Link or ID of the page to edit
        anchor: XHTML snippet to locate the insertion point
        content: XHTML content to insert
        position: "before" or "after" (default "after")
        occurrence: Which occurrence of the anchor to target (1-based, default 1).
                    Use 0 to insert at every occurrence.
        version_message: Optional version comment (shown in page history)

    Returns:
        ToolResponse dict with updated page details or error message
    """
    try:
        client = get_client()

        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()

        page = client.insert_into_page(
            page_id=page_id,
            anchor=anchor,
            content=content,
            position=position,
            occurrence=occurrence,
            version_message=version_message
        )

        logger.info(f"Inserted content into page '{page.title}' (ID: {page.id}, version: {page.version})")
        return ToolResponse.success_response(page.to_dict()).to_dict()

    except ConfluenceValidationError as e:
        logger.warning(f"Validation error inserting into page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceError as e:
        logger.error(f"Confluence error inserting into page: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except Exception as e:
        logger.exception(f"Unexpected error inserting into page")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def download_page_body(page_link: str, output_path: str) -> dict:
    """Download a Confluence page's body (storage format XHTML) to a local file.
    
    Fetches the page and writes its body_storage content to the specified file path.
    This is useful for large pages where editing the body in-context is impractical.
    The file can then be edited locally and re-uploaded with update_page_from_file.
    
    A metadata comment is prepended to the file with the page ID, title, and version
    so that update_page_from_file can auto-detect which page to update.
    
    The page can be specified by:
    - A numeric page ID (e.g., "4012276902")
    - A tiny link (e.g., "/x/hgoFYg" or full URL containing /x/)
    - A full Confluence page URL containing /pages/<id>/
    - A full Confluence page URL with pageId parameter
    
    Args:
        page_link: Link or ID of the page to download
        output_path: File path to write the body content to.
                     Accepts absolute paths (e.g., "C:/workspace/page_body.xhtml")
                     or relative paths resolved against the current working directory.
        
    Returns:
        ToolResponse dict with file path and page metadata or error message
    """
    try:
        client = get_client()
        
        output_path = output_path.strip()
        if not os.path.isabs(output_path):
            output_path = os.path.abspath(output_path)
        
        page_id = _resolve_page_id(client, page_link)
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine page. Provide a tiny link (/x/...), "
                "a numeric page ID, a URL with /pages/<id>/, or a URL with pageId parameter."
            ).to_dict()
        
        page = client.get_page(page_id)
        
        # Write body to file with metadata header
        metadata_comment = (
            f"<!-- confluence-metadata\n"
            f"     page_id: {page.id}\n"
            f"     title: {page.title}\n"
            f"     space_key: {page.space_key}\n"
            f"     version: {page.version}\n"
            f"     url: {page.url}\n"
            f"-->\n"
        )
        
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        with open(output_path, "w", encoding="utf-8") as f:
            f.write(metadata_comment)
            f.write(page.body_storage)
        
        logger.info(f"Downloaded page {page.id} body to {output_path}")
        return ToolResponse.success_response({
            "file_path": output_path,
            "page_id": page.id,
            "title": page.title,
            "version": page.version,
            "body_size": len(page.body_storage),
        }).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceError as e:
        logger.error(f"Confluence error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except OSError as e:
        logger.error(f"File write error: {e}")
        return ToolResponse.error_response(f"Failed to write file: {e}").to_dict()
    except Exception as e:
        logger.exception(f"Unexpected error downloading page body")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


@mcp.tool()
def update_page_from_file(
    file_path: str,
    page_link: str = "",
    title: str = "",
    version_message: str = ""
) -> dict:
    """Update a Confluence page using body content read from a local file.
    
    Reads the XHTML body from a local file and uses it to update the page.
    If the file was created by download_page_body, the page ID is auto-detected
    from the metadata comment — no need to provide page_link separately.
    
    The metadata comment (<!-- confluence-metadata ... -->) is automatically
    stripped before uploading so it doesn't end up in the page content.
    
    Args:
        file_path: Path to the local file containing the XHTML body.
                   Accepts absolute or relative paths (resolved against cwd).
        page_link: Link or ID of the page to update (optional if file has metadata)
        title: New page title (optional — keeps existing title if empty)
        version_message: Optional version comment (shown in page history)
        
    Returns:
        ToolResponse dict with updated page details or error message
    """
    try:
        client = get_client()
        
        file_path = file_path.strip()
        if not os.path.isabs(file_path):
            file_path = os.path.abspath(file_path)
        
        if not os.path.isfile(file_path):
            return ToolResponse.error_response(f"File not found: {file_path}").to_dict()
        
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()
        
        # Try to extract metadata from the file header
        metadata_match = re.search(
            r'<!--\s*confluence-metadata\s*\n(.*?)-->',
            content,
            re.DOTALL
        )
        
        file_page_id = None
        if metadata_match:
            metadata_block = metadata_match.group(1)
            id_match = re.search(r'page_id:\s*(\d+)', metadata_block)
            if id_match:
                file_page_id = id_match.group(1)
            # Strip the metadata comment from the body
            content = content[metadata_match.end():].lstrip("\n")
        
        # Determine which page to update
        page_id = None
        if page_link and page_link.strip():
            page_id = _resolve_page_id(client, page_link)
        
        if page_id is None:
            page_id = file_page_id
        
        if page_id is None:
            return ToolResponse.error_response(
                "Could not determine which page to update. Either provide page_link "
                "or use a file created by download_page_body (contains metadata)."
            ).to_dict()
        
        # Fetch current page for version and title
        current_page = client.get_page(page_id)
        page_title = title.strip() if title and title.strip() else current_page.title
        
        page = client.update_page(
            page_id=page_id,
            title=page_title,
            body_storage=content,
            version_number=current_page.version,
            version_message=version_message or "Updated from local file"
        )
        
        logger.info(f"Updated page '{page_title}' (ID: {page.id}) from file {file_path}")
        return ToolResponse.success_response(page.to_dict()).to_dict()
        
    except ConfluenceValidationError as e:
        logger.warning(f"Validation error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceNotFoundError as e:
        logger.warning(f"Page not found: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceAuthError as e:
        logger.error(f"Authentication error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except ConfluenceError as e:
        logger.error(f"Confluence error: {e}")
        return ToolResponse.error_response(str(e)).to_dict()
    except OSError as e:
        logger.error(f"File read error: {e}")
        return ToolResponse.error_response(f"Failed to read file: {e}").to_dict()
    except Exception as e:
        logger.exception(f"Unexpected error updating page from file")
        return ToolResponse.error_response(f"Unexpected error: {e}").to_dict()


def parse_args():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Confluence MCP Server - Read-Only Connector"
    )
    parser.add_argument(
        "--sse",
        action="store_true",
        help="Run in SSE (HTTP) transport mode instead of stdio"
    )
    parser.add_argument(
        "--host",
        default="localhost",
        help="Host for SSE mode (default: localhost)"
    )
    parser.add_argument(
        "--port",
        type=int,
        default=8000,
        help="Port for SSE mode (default: 8000)"
    )
    return parser.parse_args()


def main():
    """Main entry point for the server."""
    args = parse_args()
    
    # Log startup information
    if args.sse:
        logger.info(f"Starting Confluence MCP Server in SSE mode on {args.host}:{args.port}")
    else:
        logger.info("Starting Confluence MCP Server in stdio mode")
    
    # Try to initialize client, but don't crash if config is missing
    # (tools will return errors when called without valid config)
    try:
        get_client()
    except ValueError as e:
        logger.warning(f"Configuration not available at startup: {e}")
        logger.warning("Server will start but tools will fail until config is provided.")
    
    # Run server
    if args.sse:
        mcp.run(transport="sse", host=args.host, port=args.port)
    else:
        mcp.run()


if __name__ == "__main__":
    main()
