"""Tests for ConfluenceClient.

Property-based tests for Confluence API client functionality.
"""

import pytest
import os
import sys
import responses
from hypothesis import given, strategies as st, settings, assume

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from models import ConfluenceConfig
from confluence_client import (
    ConfluenceClient,
    ConfluenceError,
    ConfluenceAuthError,
    ConfluenceNotFoundError,
    ConfluenceValidationError
)


@pytest.fixture
def client(mock_env_vars):
    """Create a ConfluenceClient with mock config."""
    config = ConfluenceConfig.from_environment()
    return ConfluenceClient(config)


class TestBearerTokenHeader:
    """Tests for Bearer token authentication.
    
    **Feature: confluence-mcp-server, Property 9: Bearer token in Authorization header**
    **Validates: Requirements 4.4**
    """
    
    @given(pat=st.text(min_size=1, alphabet=st.characters(whitelist_categories=('L', 'N'))).filter(lambda x: x.strip()))
    @settings(max_examples=100)
    def test_bearer_token_format(self, pat, monkeypatch):
        """Property 9: For any HTTP request made by ConfluenceClient, the Authorization 
        header SHALL be present and formatted as 'Bearer {PAT}'.
        
        **Feature: confluence-mcp-server, Property 9: Bearer token in Authorization header**
        **Validates: Requirements 4.4**
        """
        monkeypatch.setenv("CONFLUENCE_BASE_URL", "https://test.example.com")
        monkeypatch.setenv("CONFLUENCE_PAT", pat)
        
        config = ConfluenceConfig.from_environment()
        client = ConfluenceClient(config)
        
        # Verify header is set correctly
        auth_header = client.session.headers.get("Authorization")
        assert auth_header is not None
        assert auth_header == f"Bearer {pat}"
        assert auth_header.startswith("Bearer ")


class TestPageRetrieval:
    """Tests for page retrieval functionality.
    
    **Feature: confluence-mcp-server, Property 1: Page retrieval returns complete data**
    **Validates: Requirements 1.1, 1.3, 1.4**
    """
    
    @responses.activate
    def test_get_page_success(self, client, sample_page_response):
        """Test successful page retrieval."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json=sample_page_response,
            status=200
        )
        
        page = client.get_page("123456")
        
        assert page.id == "123456"
        assert page.title == "Test Page"
        assert page.space_key == "TEST"
        assert page.version == 5
        assert page.body_storage == "<p>This is <strong>test</strong> content.</p>"
        assert "test content" in page.body_plain_text.lower()
    
    @responses.activate
    @given(
        page_id=st.text(alphabet=st.characters(whitelist_categories=('Nd',)), min_size=1, max_size=10),
        title=st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
        space_key=st.text(alphabet=st.characters(whitelist_categories=('Lu',)), min_size=2, max_size=10),
        version=st.integers(min_value=1, max_value=1000),
        body_content=st.text(min_size=0, max_size=500)
    )
    @settings(max_examples=50)
    def test_page_retrieval_completeness(self, client, page_id, title, space_key, version, body_content):
        """Property 1: For any valid page ID and successful API response, the get_page_by_id 
        tool SHALL return a ToolResponse where the data object contains non-empty id, title, 
        space_key, version, last_modified, body_storage, and body_plain_text fields.
        
        **Feature: confluence-mcp-server, Property 1: Page retrieval returns complete data**
        **Validates: Requirements 1.1, 1.3, 1.4**
        """
        assume(page_id.strip())
        assume(title.strip())
        assume(space_key.strip())
        
        responses.reset()
        
        mock_response = {
            "id": page_id,
            "title": title,
            "space": {"key": space_key},
            "version": {"number": version},
            "history": {"lastUpdated": {"when": "2024-01-15T10:30:00.000Z"}},
            "body": {"storage": {"value": f"<p>{body_content}</p>"}},
            "_links": {"webui": f"/pages/viewpage.action?pageId={page_id}"}
        }
        
        responses.add(
            responses.GET,
            f"https://test.confluence.example.com/rest/api/content/{page_id}",
            json=mock_response,
            status=200
        )
        
        page = client.get_page(page_id)
        
        # Verify all required fields are present and non-empty where expected
        assert page.id == page_id
        assert page.title == title
        assert page.space_key == space_key
        assert page.version == version
        assert page.last_modified  # Non-empty
        assert page.body_storage is not None
        assert page.body_plain_text is not None


class TestTinyLinkResolution:
    """Tests for tiny link resolution.
    
    **Feature: confluence-mcp-server, Property 3: Tiny link resolution equivalence**
    **Validates: Requirements 2.1, 2.2**
    """
    
    @responses.activate
    def test_resolve_tiny_link_success(self, client):
        """Test successful tiny link resolution."""
        # Mock the redirect
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/x/hgoFYg",
            status=302,
            headers={"Location": "/pages/viewpage.action?pageId=123456"}
        )
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/pages/viewpage.action?pageId=123456",
            body="<html></html>",
            status=200
        )
        
        page_id = client.resolve_tiny_link("/x/hgoFYg")
        assert page_id == "123456"
    
    def test_invalid_tiny_link_format(self, client):
        """Test validation of invalid tiny link format."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_tiny_link("/invalid/format")
        assert "Invalid tiny link format" in str(exc_info.value)
    
    @given(
        code=st.text(alphabet=st.characters(whitelist_categories=('L', 'N')), min_size=4, max_size=10)
    )
    @settings(max_examples=50)
    def test_tiny_link_validation(self, client, code):
        """Property 3: Tiny link validation accepts valid formats.
        
        **Feature: confluence-mcp-server, Property 3: Tiny link resolution equivalence**
        **Validates: Requirements 2.1, 2.2**
        """
        assume(code.strip())
        assume(code.isalnum())
        
        # Valid format should pass validation
        tiny_link = f"/x/{code}"
        result = client._validate_tiny_link(tiny_link)
        assert result == tiny_link


class TestSearchResults:
    """Tests for search functionality.
    
    **Feature: confluence-mcp-server, Property 5: Search results contain required fields**
    **Feature: confluence-mcp-server, Property 6: Search respects limit parameter**
    **Validates: Requirements 3.1, 3.2**
    """
    
    @responses.activate
    def test_search_success(self, client, sample_search_response):
        """Test successful search."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/search",
            json=sample_search_response,
            status=200
        )
        
        result = client.search("test query")
        
        assert "results" in result
        assert "total" in result
        assert result["total"] == 42
        assert len(result["results"]) == 1
    
    @responses.activate
    @given(
        num_results=st.integers(min_value=0, max_value=20),
        limit=st.integers(min_value=1, max_value=50)
    )
    @settings(max_examples=50)
    def test_search_limit_respected(self, client, num_results, limit):
        """Property 6: For any search query with a specified limit, the number of 
        results returned SHALL be less than or equal to the limit value.
        
        **Feature: confluence-mcp-server, Property 6: Search respects limit parameter**
        **Validates: Requirements 3.2**
        """
        responses.reset()
        
        # Generate mock results up to the limit
        actual_results = min(num_results, limit)
        mock_results = [
            {
                "content": {
                    "id": str(i),
                    "title": f"Page {i}",
                    "space": {"key": "TEST"},
                    "_links": {"webui": f"/pages/{i}"}
                },
                "excerpt": f"Excerpt {i}"
            }
            for i in range(actual_results)
        ]
        
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/search",
            json={
                "results": mock_results,
                "totalSize": num_results,
                "start": 0,
                "limit": limit
            },
            status=200
        )
        
        result = client.search("test", limit=limit)
        
        # Verify limit is respected
        assert len(result["results"]) <= limit
    
    @responses.activate
    @given(
        page_id=st.text(alphabet=st.characters(whitelist_categories=('Nd',)), min_size=1, max_size=10),
        title=st.text(min_size=1, max_size=50).filter(lambda x: x.strip()),
        space_key=st.text(alphabet=st.characters(whitelist_categories=('Lu',)), min_size=2, max_size=5)
    )
    @settings(max_examples=50)
    def test_search_result_structure(self, client, page_id, title, space_key):
        """Property 5: For any search query and non-empty result set, each result 
        in the list SHALL contain non-empty id, title, space_key, and url fields.
        
        **Feature: confluence-mcp-server, Property 5: Search results contain required fields**
        **Validates: Requirements 3.1**
        """
        assume(page_id.strip())
        assume(title.strip())
        assume(space_key.strip())
        
        responses.reset()
        
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/search",
            json={
                "results": [{
                    "content": {
                        "id": page_id,
                        "title": title,
                        "space": {"key": space_key},
                        "_links": {"webui": f"/pages/{page_id}"}
                    },
                    "excerpt": "test excerpt"
                }],
                "totalSize": 1,
                "start": 0,
                "limit": 10
            },
            status=200
        )
        
        result = client.search("test")
        
        assert len(result["results"]) > 0
        for item in result["results"]:
            assert item["id"]
            assert item["title"]
            assert item["space_key"]
            assert item["url"]


class TestSpaceListing:
    """Tests for space page listing.
    
    **Feature: confluence-mcp-server, Property 12: Space page listing returns required fields**
    **Feature: confluence-mcp-server, Property 13: Space listing respects pagination**
    **Validates: Requirements 7.1, 7.2**
    """
    
    @responses.activate
    def test_list_space_pages_success(self, client, sample_space_pages_response):
        """Test successful space page listing."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content",
            json=sample_space_pages_response,
            status=200
        )
        
        result = client.list_space_pages("TEST")
        
        assert "pages" in result
        assert len(result["pages"]) == 3
    
    @responses.activate
    @given(
        num_pages=st.integers(min_value=0, max_value=30),
        limit=st.integers(min_value=1, max_value=50),
        start=st.integers(min_value=0, max_value=100)
    )
    @settings(max_examples=50)
    def test_pagination_respected(self, client, num_pages, limit, start):
        """Property 13: For any list_space_pages call with specified limit and start 
        parameters, the number of results SHALL be less than or equal to the limit.
        
        **Feature: confluence-mcp-server, Property 13: Space listing respects pagination**
        **Validates: Requirements 7.2**
        """
        responses.reset()
        
        actual_pages = min(num_pages, limit)
        mock_pages = [
            {"id": str(i), "title": f"Page {i}"}
            for i in range(actual_pages)
        ]
        
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content",
            json={
                "results": mock_pages,
                "start": start,
                "limit": limit,
                "size": actual_pages
            },
            status=200
        )
        
        result = client.list_space_pages("TEST", limit=limit, start=start)
        
        assert len(result["pages"]) <= limit
        assert result["limit"] == limit
        assert result["start"] == start
    
    @responses.activate
    @given(
        page_id=st.text(alphabet=st.characters(whitelist_categories=('Nd',)), min_size=1, max_size=10),
        title=st.text(min_size=1, max_size=50).filter(lambda x: x.strip())
    )
    @settings(max_examples=50)
    def test_space_listing_structure(self, client, page_id, title):
        """Property 12: For any valid space key and successful API response, each 
        page in the list SHALL contain non-empty id and title fields.
        
        **Feature: confluence-mcp-server, Property 12: Space page listing returns required fields**
        **Validates: Requirements 7.1**
        """
        assume(page_id.strip())
        assume(title.strip())
        
        responses.reset()
        
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content",
            json={
                "results": [{"id": page_id, "title": title}],
                "start": 0,
                "limit": 25,
                "size": 1
            },
            status=200
        )
        
        result = client.list_space_pages("TEST")
        
        for page in result["pages"]:
            assert page["id"]
            assert page["title"]


class TestTextExtraction:
    """Tests for HTML to plain text extraction."""
    
    def test_extract_plain_text_basic(self, client):
        """Test basic HTML to text extraction."""
        html = "<p>Hello <strong>World</strong></p>"
        text = client.extract_plain_text(html)
        assert "Hello" in text
        assert "World" in text
        assert "<" not in text
    
    def test_extract_plain_text_empty(self, client):
        """Test extraction from empty content."""
        assert client.extract_plain_text("") == ""
        assert client.extract_plain_text(None) == ""
    
    def test_extract_plain_text_removes_scripts(self, client):
        """Test that script tags are removed."""
        html = "<p>Text</p><script>alert('xss')</script>"
        text = client.extract_plain_text(html)
        assert "alert" not in text
        assert "Text" in text
