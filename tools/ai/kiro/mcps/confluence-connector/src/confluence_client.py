"""Confluence REST API client."""

import re
import logging
from typing import Optional
from urllib.parse import urljoin, urlparse, parse_qs

import requests
from bs4 import BeautifulSoup

from models import ConfluenceConfig, PageContent, SearchResult, PageSummary, PageTreeNode, ToolResponse

logger = logging.getLogger(__name__)


class ConfluenceError(Exception):
    """Base exception for Confluence client errors."""
    pass


class ConfluenceAuthError(ConfluenceError):
    """Authentication error."""
    pass


class ConfluenceNotFoundError(ConfluenceError):
    """Resource not found error."""
    pass


class ConfluenceValidationError(ConfluenceError):
    """Input validation error."""
    pass


class ConfluenceClient:
    """HTTP client for Confluence REST API.
    
    Handles all communication with Confluence Data Center using PAT authentication.
    """
    
    def __init__(self, config: ConfluenceConfig):
        """Initialize the Confluence client.
        
        Args:
            config: Configuration with base_url and PAT
        """
        self.config = config
        self.session = requests.Session()
        self.session.headers.update({
            "Authorization": f"Bearer {config.pat}",
            "Content-Type": "application/json",
            "Accept": "application/json"
        })
    
    def _make_request(self, method: str, endpoint: str, **kwargs) -> dict:
        """Make an HTTP request to the Confluence API.
        
        Args:
            method: HTTP method (GET, POST, etc.)
            endpoint: API endpoint path
            **kwargs: Additional arguments for requests
            
        Returns:
            JSON response as dictionary
            
        Raises:
            ConfluenceAuthError: For 401/403 errors
            ConfluenceNotFoundError: For 404 errors
            ConfluenceError: For other HTTP errors
        """
        url = urljoin(self.config.base_url, endpoint)
        kwargs.setdefault("timeout", self.config.timeout)
        
        try:
            response = self.session.request(method, url, **kwargs)
            
            if response.status_code == 401:
                logger.error("Authentication failed: Invalid or expired PAT")
                raise ConfluenceAuthError("Authentication failed: Invalid or expired PAT")
            
            if response.status_code == 403:
                logger.error(f"Access denied: Insufficient permissions for {endpoint}")
                raise ConfluenceAuthError(f"Access denied: Insufficient permissions for {endpoint}")
            
            if response.status_code == 404:
                logger.warning(f"Resource not found: {endpoint}")
                raise ConfluenceNotFoundError(f"Resource not found (HTTP 404)")
            
            if response.status_code == 429:
                retry_after = response.headers.get("Retry-After", "unknown")
                logger.warning(f"Rate limited: Retry after {retry_after} seconds")
                raise ConfluenceError(f"Rate limited: Retry after {retry_after} seconds")
            
            if response.status_code >= 500:
                logger.error(f"Confluence server error: {response.status_code}")
                raise ConfluenceError(f"Confluence server error: {response.status_code}")
            
            if response.status_code >= 400:
                logger.error(f"Bad request: {response.text}")
                raise ConfluenceError(f"Bad request: {response.text}")
            
            return response.json()
            
        except requests.exceptions.Timeout:
            logger.error(f"Request timed out after {self.config.timeout} seconds")
            raise ConfluenceError(f"Request timed out after {self.config.timeout} seconds")
        
        except requests.exceptions.ConnectionError as e:
            logger.error(f"Failed to connect to Confluence: {e}")
            raise ConfluenceError(f"Failed to connect to Confluence: {e}")
    
    def _validate_page_id(self, page_id: str) -> None:
        """Validate page ID format.
        
        Args:
            page_id: The page ID to validate
            
        Raises:
            ConfluenceValidationError: If page ID is invalid
        """
        if not page_id or not page_id.strip():
            raise ConfluenceValidationError("Invalid page ID: empty value")
        
        if not page_id.isdigit():
            raise ConfluenceValidationError(
                f"Invalid page ID: {page_id}. Expected numeric value"
            )
    
    def _validate_tiny_link(self, tiny_link: str) -> str:
        """Validate and normalize tiny link format.
        
        Args:
            tiny_link: The tiny link to validate
            
        Returns:
            Normalized tiny link path (e.g., /x/hgoFYg)
            
        Raises:
            ConfluenceValidationError: If tiny link format is invalid
        """
        if not tiny_link or not tiny_link.strip():
            raise ConfluenceValidationError("Invalid tiny link format: empty value")
        
        # Extract path from full URL if provided
        if tiny_link.startswith("http"):
            parsed = urlparse(tiny_link)
            tiny_link = parsed.path
        
        # Normalize to /x/... format
        if not tiny_link.startswith("/"):
            tiny_link = "/" + tiny_link
        
        # Validate format
        if not re.match(r"^/x/[A-Za-z0-9_-]+$", tiny_link):
            raise ConfluenceValidationError(
                f"Invalid tiny link format: {tiny_link}. Expected /x/... format"
            )
        
        return tiny_link
    
    def _validate_search_query(self, query: str) -> None:
        """Validate search query.
        
        Args:
            query: The search query to validate
            
        Raises:
            ConfluenceValidationError: If query is empty
        """
        if not query or not query.strip():
            raise ConfluenceValidationError("Search query is required")
    
    def extract_plain_text(self, html_content: str) -> str:
        """Extract plain text from HTML storage format.
        
        Args:
            html_content: HTML content from Confluence storage format
            
        Returns:
            Plain text with HTML tags removed
        """
        if not html_content:
            return ""
        
        try:
            soup = BeautifulSoup(html_content, "html.parser")
            
            # Remove script and style elements
            for element in soup(["script", "style"]):
                element.decompose()
            
            # Get text and normalize whitespace
            text = soup.get_text(separator=" ")
            text = " ".join(text.split())
            
            return text
        except Exception as e:
            logger.warning(f"Failed to extract plain text: {e}")
            return html_content
    
    def get_page(self, page_id: str) -> PageContent:
        """Fetch a page by its ID.
        
        Args:
            page_id: The numeric page ID
            
        Returns:
            PageContent with page details
            
        Raises:
            ConfluenceValidationError: If page_id is invalid
            ConfluenceNotFoundError: If page doesn't exist
            ConfluenceError: For other errors
        """
        self._validate_page_id(page_id)
        
        endpoint = f"/rest/api/content/{page_id}"
        params = {
            "expand": "body.storage,version,space,history.lastUpdated"
        }
        
        try:
            data = self._make_request("GET", endpoint, params=params)
        except ConfluenceNotFoundError:
            raise ConfluenceNotFoundError(f"Page {page_id} not found (HTTP 404)")
        
        # Extract fields
        body_storage = data.get("body", {}).get("storage", {}).get("value", "")
        body_plain_text = self.extract_plain_text(body_storage)
        
        last_modified = (
            data.get("history", {})
            .get("lastUpdated", {})
            .get("when", "")
        )
        
        web_ui = data.get("_links", {}).get("webui", "")
        url = urljoin(self.config.base_url, web_ui) if web_ui else ""
        
        return PageContent(
            id=data.get("id", page_id),
            title=data.get("title", ""),
            space_key=data.get("space", {}).get("key", ""),
            version=data.get("version", {}).get("number", 0),
            last_modified=last_modified,
            body_storage=body_storage,
            body_plain_text=body_plain_text,
            url=url
        )
    
    def resolve_tiny_link(self, tiny_link: str) -> str:
        """Resolve a tiny link to a page ID.
        
        Args:
            tiny_link: The tiny link (e.g., /x/hgoFYg or full URL)
            
        Returns:
            The resolved page ID
            
        Raises:
            ConfluenceValidationError: If tiny link format is invalid
            ConfluenceNotFoundError: If link cannot be resolved
        """
        path = self._validate_tiny_link(tiny_link)
        
        # Make request and follow redirects
        url = urljoin(self.config.base_url, path)
        
        try:
            response = self.session.get(
                url,
                allow_redirects=True,
                timeout=self.config.timeout
            )
            
            if response.status_code == 404:
                raise ConfluenceNotFoundError(
                    f"Could not resolve tiny link: {tiny_link}"
                )
            
            if response.status_code >= 400:
                raise ConfluenceError(
                    f"Failed to resolve tiny link: HTTP {response.status_code}"
                )
            
            # Extract page ID from final URL
            final_url = response.url
            
            # Try viewpage.action?pageId=...
            if "pageId=" in final_url:
                parsed = urlparse(final_url)
                params = parse_qs(parsed.query)
                if "pageId" in params:
                    return params["pageId"][0]
            
            # Try /pages/viewpage.action pattern
            match = re.search(r"/pages/(\d+)", final_url)
            if match:
                return match.group(1)
            
            # Try /display/SPACE/... pattern - need to look up by title
            # For now, raise an error
            raise ConfluenceNotFoundError(
                f"Could not extract page ID from resolved URL: {final_url}"
            )
            
        except requests.exceptions.Timeout:
            raise ConfluenceError(
                f"Request timed out while resolving tiny link"
            )
        except requests.exceptions.ConnectionError as e:
            raise ConfluenceError(f"Failed to connect to Confluence: {e}")
    
    def search(self, query: str, limit: int = 10, start: int = 0) -> dict:
        """Search for pages using CQL.
        
        Args:
            query: Search query text
            limit: Maximum results to return (default: 10)
            start: Starting index for pagination (default: 0)
            
        Returns:
            Dictionary with results list and total count
            
        Raises:
            ConfluenceValidationError: If query is empty
        """
        self._validate_search_query(query)
        
        endpoint = "/rest/api/content/search"
        params = {
            "cql": f'text~"{query}"',
            "limit": limit,
            "start": start
        }
        
        data = self._make_request("GET", endpoint, params=params)
        
        results = []
        for item in data.get("results", []):
            content = item.get("content", item)
            
            web_ui = content.get("_links", {}).get("webui", "")
            url = urljoin(self.config.base_url, web_ui) if web_ui else ""
            
            results.append(SearchResult(
                id=content.get("id", ""),
                title=content.get("title", ""),
                space_key=content.get("space", {}).get("key", ""),
                excerpt=item.get("excerpt", ""),
                url=url
            ))
        
        return {
            "results": [r.to_dict() for r in results],
            "total": data.get("totalSize", len(results)),
            "limit": limit,
            "start": start
        }
    
    def get_page_space_key(self, page_id: str) -> str:
        """Get the space key for a page by its ID.
        
        Args:
            page_id: The numeric page ID
            
        Returns:
            The space key string
        """
        self._validate_page_id(page_id)
        endpoint = f"/rest/api/content/{page_id}"
        params = {"expand": "space"}
        data = self._make_request("GET", endpoint, params=params)
        return data.get("space", {}).get("key", "")

    def update_page(
        self,
        page_id: str,
        title: str,
        body_storage: str,
        version_number: int,
        version_message: str = ""
    ) -> PageContent:
        """Update an existing Confluence page.
        
        Args:
            page_id: The numeric page ID
            title: Page title (required by API even if unchanged)
            body_storage: Full page body in Confluence storage format (XHTML)
            version_number: Current version number (will be incremented by 1)
            version_message: Optional version comment
            
        Returns:
            PageContent of the updated page
            
        Raises:
            ConfluenceValidationError: If required fields are missing
            ConfluenceError: For API errors
        """
        self._validate_page_id(page_id)
        if not title or not title.strip():
            raise ConfluenceValidationError("Page title is required")

        payload = {
            "id": page_id,
            "type": "page",
            "title": title,
            "body": {
                "storage": {
                    "value": body_storage,
                    "representation": "storage"
                }
            },
            "version": {
                "number": version_number + 1
            }
        }

        if version_message:
            payload["version"]["message"] = version_message

        data = self._make_request("PUT", f"/rest/api/content/{page_id}", json=payload)

        body = data.get("body", {}).get("storage", {}).get("value", "")
        web_ui = data.get("_links", {}).get("webui", "")
        base_link = data.get("_links", {}).get("base", self.config.base_url)
        url = urljoin(base_link, web_ui) if web_ui else ""
        space_key = data.get("space", {}).get("key", "")

        return PageContent(
            id=data.get("id", page_id),
            title=data.get("title", title),
            space_key=space_key,
            version=data.get("version", {}).get("number", version_number + 1),
            last_modified=data.get("version", {}).get("when", ""),
            body_storage=body,
            body_plain_text=self.extract_plain_text(body),
            url=url
        )


    def patch_page(
        self,
        page_id: str,
        find: str,
        replace: str,
        occurrence: int = 1,
        version_message: str = ""
    ) -> PageContent:
        """Perform a search-and-replace edit on a page without requiring the full body.

        Fetches the current page body, locates the ``find`` snippet, replaces the
        chosen occurrence with ``replace``, and pushes the updated body back.

        Args:
            page_id: The numeric page ID
            find: XHTML snippet to locate in the page body (exact match)
            replace: XHTML snippet to substitute in place of ``find``
            occurrence: Which occurrence to replace (1-based). Use 0 to replace **all**.
            version_message: Optional version comment

        Returns:
            PageContent of the updated page

        Raises:
            ConfluenceValidationError: If inputs are invalid or ``find`` is not found
            ConfluenceError: For API errors
        """
        self._validate_page_id(page_id)
        if not find:
            raise ConfluenceValidationError("'find' string must not be empty")

        current = self.get_page(page_id)
        body = current.body_storage

        count = body.count(find)
        if count == 0:
            raise ConfluenceValidationError(
                f"The 'find' string was not found in the page body (page {page_id}). "
                "Make sure the snippet matches the storage-format XHTML exactly, "
                "including non-breaking spaces (\\xa0) and entity encoding."
            )

        if occurrence == 0:
            # Replace all occurrences
            new_body = body.replace(find, replace)
        else:
            if occurrence < 0 or occurrence > count:
                raise ConfluenceValidationError(
                    f"Requested occurrence {occurrence} but only {count} "
                    f"occurrence(s) found in the page body."
                )
            # Replace the N-th occurrence (1-based)
            parts = body.split(find)
            # parts has (count + 1) elements; rejoin replacing only the N-th split
            new_body = (
                find.join(parts[:occurrence])
                + replace
                + find.join(parts[occurrence:])
            )

        return self.update_page(
            page_id=page_id,
            title=current.title,
            body_storage=new_body,
            version_number=current.version,
            version_message=version_message or "Patched via search-and-replace"
        )

    def insert_into_page(
        self,
        page_id: str,
        anchor: str,
        content: str,
        position: str = "after",
        occurrence: int = 1,
        version_message: str = ""
    ) -> PageContent:
        """Insert XHTML content before or after an anchor snippet in a page.

        Fetches the current page body, locates the ``anchor`` snippet, and inserts
        ``content`` immediately before or after it.

        Args:
            page_id: The numeric page ID
            anchor: XHTML snippet to locate the insertion point
            content: XHTML content to insert
            position: ``"before"`` or ``"after"`` (default ``"after"``)
            occurrence: Which occurrence of the anchor to target (1-based). 0 = all.
            version_message: Optional version comment

        Returns:
            PageContent of the updated page

        Raises:
            ConfluenceValidationError: If inputs are invalid or anchor is not found
        """
        self._validate_page_id(page_id)
        if not anchor:
            raise ConfluenceValidationError("'anchor' string must not be empty")
        if position not in ("before", "after"):
            raise ConfluenceValidationError("'position' must be 'before' or 'after'")

        if position == "before":
            replacement = content + anchor
        else:
            replacement = anchor + content

        return self.patch_page(
            page_id=page_id,
            find=anchor,
            replace=replacement,
            occurrence=occurrence,
            version_message=version_message or f"Inserted content {position} anchor"
        )

    def rename_page(self, page_id: str, new_title: str, version_message: str = "") -> PageContent:
        """Rename a page by updating only its title, preserving the body exactly.

        Args:
            page_id: The numeric page ID
            new_title: The new title for the page
            version_message: Optional version comment

        Returns:
            PageContent of the renamed page
        """
        self._validate_page_id(page_id)
        if not new_title or not new_title.strip():
            raise ConfluenceValidationError("New title is required")

        # Fetch current page to get body and version
        current = self.get_page(page_id)

        return self.update_page(
            page_id=page_id,
            title=new_title.strip(),
            body_storage=current.body_storage,
            version_number=current.version,
            version_message=version_message or f"Renamed from '{current.title}' to '{new_title.strip()}'"
        )


    def create_page(
        self,
        title: str,
        body_storage: str,
        space_key: str,
        parent_id: str = ""
    ) -> PageContent:
        """Create a new Confluence page.
        
        Args:
            title: Page title
            body_storage: Page body in Confluence storage format (XHTML)
            space_key: The space key to create the page in
            parent_id: Optional parent page ID (creates as child page)
            
        Returns:
            PageContent of the newly created page
            
        Raises:
            ConfluenceValidationError: If required fields are missing
            ConfluenceError: For API errors
        """
        if not title or not title.strip():
            raise ConfluenceValidationError("Page title is required")
        if not space_key or not space_key.strip():
            raise ConfluenceValidationError("Space key is required")

        payload = {
            "type": "page",
            "title": title,
            "space": {"key": space_key},
            "body": {
                "storage": {
                    "value": body_storage,
                    "representation": "storage"
                }
            }
        }

        if parent_id:
            self._validate_page_id(parent_id)
            payload["ancestors"] = [{"id": parent_id}]

        data = self._make_request("POST", "/rest/api/content", json=payload)

        # Build PageContent from response
        body = data.get("body", {}).get("storage", {}).get("value", "")
        web_ui = data.get("_links", {}).get("webui", "")
        url = urljoin(self.config.base_url, web_ui) if web_ui else ""

        return PageContent(
            id=data.get("id", ""),
            title=data.get("title", title),
            space_key=space_key,
            version=data.get("version", {}).get("number", 1),
            last_modified="",
            body_storage=body,
            body_plain_text=self.extract_plain_text(body),
            url=url
        )

    def list_space_pages(
        self,
        space_key: str,
        limit: int = 25,
        start: int = 0
    ) -> dict:
        """List pages in a space.
        
        Args:
            space_key: The space key (e.g., "DEV")
            limit: Maximum results to return (default: 25)
            start: Starting index for pagination (default: 0)
            
        Returns:
            Dictionary with pages list and pagination info
            
        Raises:
            ConfluenceNotFoundError: If space doesn't exist
        """
        if not space_key or not space_key.strip():
            raise ConfluenceValidationError("Space key is required")
        
        endpoint = "/rest/api/content"
        params = {
            "spaceKey": space_key,
            "type": "page",
            "limit": limit,
            "start": start
        }
        
        try:
            data = self._make_request("GET", endpoint, params=params)
        except ConfluenceNotFoundError:
            raise ConfluenceNotFoundError(f"Space {space_key} not found")
        
        # Check if results are empty (might indicate invalid space)
        results = data.get("results", [])
        
        pages = [
            PageSummary(
                id=page.get("id", ""),
                title=page.get("title", "")
            ).to_dict()
            for page in results
        ]
        
        return {
            "pages": pages,
            "total": data.get("size", len(pages)),
            "limit": limit,
            "start": start
        }


    def get_child_pages(self, page_id: str, limit: int = 100, start: int = 0) -> list:
        """Get direct child pages of a given page.

        Args:
            page_id: The parent page ID
            limit: Maximum results per request (default: 100)
            start: Starting index for pagination (default: 0)

        Returns:
            List of dicts with id, title, and url for each child page
        """
        self._validate_page_id(page_id)

        all_children = []
        current_start = start

        while True:
            endpoint = f"/rest/api/content/{page_id}/child/page"
            params = {
                "limit": limit,
                "start": current_start,
            }

            data = self._make_request("GET", endpoint, params=params)
            results = data.get("results", [])

            for child in results:
                web_ui = child.get("_links", {}).get("webui", "")
                url = urljoin(self.config.base_url, web_ui) if web_ui else ""
                all_children.append({
                    "id": child.get("id", ""),
                    "title": child.get("title", ""),
                    "url": url,
                })

            # Check if there are more pages
            size = data.get("size", 0)
            if size < limit:
                break
            current_start += size

        return all_children

    def get_page_tree(self, page_id: str, max_depth: int = 10, _current_depth: int = 0) -> PageTreeNode:
        """Recursively build the full page tree starting from a given page.

        Args:
            page_id: The root page ID
            max_depth: Maximum recursion depth to prevent runaway traversal (default: 10)
            _current_depth: Internal counter for current recursion depth

        Returns:
            PageTreeNode representing the root with nested children

        Raises:
            ConfluenceValidationError: If page_id is invalid
            ConfluenceNotFoundError: If the root page doesn't exist
        """
        self._validate_page_id(page_id)

        # Fetch the root page info
        page = self.get_page(page_id)

        root = PageTreeNode(
            id=page.id,
            title=page.title,
            url=page.url,
        )

        if _current_depth >= max_depth:
            logger.warning(f"Max depth {max_depth} reached at page {page_id}, stopping recursion")
            return root

        # Get direct children
        children = self.get_child_pages(page_id)

        for child in children:
            child_node = self.get_page_tree(
                child["id"],
                max_depth=max_depth,
                _current_depth=_current_depth + 1,
            )
            root.children.append(child_node)

        return root

