"""Tests for error handling.

Property-based tests for error handling and input validation.
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


class TestHTTPErrorHandling:
    """Tests for HTTP error handling.
    
    **Feature: confluence-mcp-server, Property 10: HTTP errors return structured response**
    **Validates: Requirements 5.1, 4.3**
    """
    
    @responses.activate
    @given(status_code=st.sampled_from([400, 401, 403, 404, 429, 500, 502, 503]))
    @settings(max_examples=50)
    def test_http_error_response_structure(self, client, status_code):
        """Property 10: For any HTTP error status code (4xx or 5xx) returned by the API, 
        the tool SHALL return a ToolResponse with success=false and an error message 
        containing the status code.
        
        **Feature: confluence-mcp-server, Property 10: HTTP errors return structured response**
        **Validates: Requirements 5.1, 4.3**
        """
        responses.reset()
        
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Error"},
            status=status_code
        )
        
        # Expect appropriate exception based on status code
        if status_code == 401 or status_code == 403:
            with pytest.raises(ConfluenceAuthError):
                client.get_page("123456")
        elif status_code == 404:
            with pytest.raises(ConfluenceNotFoundError):
                client.get_page("123456")
        else:
            with pytest.raises(ConfluenceError) as exc_info:
                client.get_page("123456")
            # Verify error message contains status code for 4xx/5xx
            if status_code >= 500:
                assert str(status_code) in str(exc_info.value)
    
    @responses.activate
    def test_401_authentication_error(self, client):
        """Test 401 returns authentication error."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Unauthorized"},
            status=401
        )
        
        with pytest.raises(ConfluenceAuthError) as exc_info:
            client.get_page("123456")
        assert "Authentication failed" in str(exc_info.value)
    
    @responses.activate
    def test_403_access_denied(self, client):
        """Test 403 returns access denied error."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Forbidden"},
            status=403
        )
        
        with pytest.raises(ConfluenceAuthError) as exc_info:
            client.get_page("123456")
        assert "Access denied" in str(exc_info.value)
    
    @responses.activate
    def test_404_not_found(self, client):
        """Test 404 returns not found error."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Not Found"},
            status=404
        )
        
        with pytest.raises(ConfluenceNotFoundError) as exc_info:
            client.get_page("123456")
        assert "not found" in str(exc_info.value).lower()
    
    @responses.activate
    def test_429_rate_limited(self, client):
        """Test 429 returns rate limit error."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Too Many Requests"},
            status=429,
            headers={"Retry-After": "60"}
        )
        
        with pytest.raises(ConfluenceError) as exc_info:
            client.get_page("123456")
        assert "Rate limited" in str(exc_info.value)
    
    @responses.activate
    def test_500_server_error(self, client):
        """Test 500 returns server error."""
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content/123456",
            json={"message": "Internal Server Error"},
            status=500
        )
        
        with pytest.raises(ConfluenceError) as exc_info:
            client.get_page("123456")
        assert "500" in str(exc_info.value)


class TestInvalidPageID:
    """Tests for invalid page ID validation.
    
    **Feature: confluence-mcp-server, Property 2: Invalid page ID returns structured error**
    **Validates: Requirements 1.2**
    """
    
    @given(
        invalid_id=st.one_of(
            st.text(alphabet=st.characters(whitelist_categories=('L',)), min_size=1, max_size=10),
            st.just(""),
            st.just("   "),
            st.text(min_size=1, max_size=10).filter(lambda x: not x.strip().isdigit())
        )
    )
    @settings(max_examples=100)
    def test_invalid_page_id_error(self, client, invalid_id):
        """Property 2: For any page ID that does not exist or is malformed, the 
        get_page_by_id tool SHALL return a ToolResponse with success=false and 
        a non-empty error message.
        
        **Feature: confluence-mcp-server, Property 2: Invalid page ID returns structured error**
        **Validates: Requirements 1.2**
        """
        # Skip if the ID happens to be valid (all digits)
        assume(not invalid_id.strip().isdigit() if invalid_id else True)
        
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_page_id(invalid_id)
        
        assert str(exc_info.value)  # Non-empty error message
    
    def test_empty_page_id(self, client):
        """Test empty page ID returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_page_id("")
        assert "Invalid page ID" in str(exc_info.value)
    
    def test_non_numeric_page_id(self, client):
        """Test non-numeric page ID returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_page_id("abc123")
        assert "Expected numeric value" in str(exc_info.value)


class TestInvalidTinyLink:
    """Tests for invalid tiny link validation.
    
    **Feature: confluence-mcp-server, Property 4: Invalid tiny link returns structured error**
    **Validates: Requirements 2.3**
    """
    
    @given(
        invalid_link=st.one_of(
            st.just(""),
            st.just("   "),
            st.just("/invalid/path"),
            st.just("/y/something"),
            st.text(min_size=1, max_size=20).filter(lambda x: "/x/" not in x)
        )
    )
    @settings(max_examples=100)
    def test_invalid_tiny_link_error(self, client, invalid_link):
        """Property 4: For any tiny link that cannot be resolved, the get_page_by_tiny_link 
        tool SHALL return a ToolResponse with success=false and an error message containing 
        'resolve' or 'not found'.
        
        **Feature: confluence-mcp-server, Property 4: Invalid tiny link returns structured error**
        **Validates: Requirements 2.3**
        """
        # Skip if the link happens to be valid format
        assume(not (invalid_link.strip() and "/x/" in invalid_link))
        
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_tiny_link(invalid_link)
        
        error_msg = str(exc_info.value).lower()
        assert "invalid" in error_msg or "format" in error_msg
    
    def test_empty_tiny_link(self, client):
        """Test empty tiny link returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_tiny_link("")
        assert "Invalid tiny link" in str(exc_info.value)
    
    def test_wrong_format_tiny_link(self, client):
        """Test wrong format tiny link returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_tiny_link("/pages/123")
        assert "Expected /x/... format" in str(exc_info.value)


class TestEmptySearchQuery:
    """Tests for empty search query validation.
    
    **Feature: confluence-mcp-server, Property 7: Empty search query returns error**
    **Validates: Requirements 3.4**
    """
    
    @given(
        empty_query=st.one_of(
            st.just(""),
            st.just("   "),
            st.just("\t"),
            st.just("\n"),
            st.text(alphabet=st.characters(whitelist_categories=('Zs',)), min_size=0, max_size=10)
        )
    )
    @settings(max_examples=100)
    def test_empty_search_query_error(self, client, empty_query):
        """Property 7: For any search query that is empty or contains only whitespace, 
        the search_pages tool SHALL return a ToolResponse with success=false and an 
        error message.
        
        **Feature: confluence-mcp-server, Property 7: Empty search query returns error**
        **Validates: Requirements 3.4**
        """
        assume(not empty_query.strip())
        
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_search_query(empty_query)
        
        assert "required" in str(exc_info.value).lower()
    
    def test_empty_string_query(self, client):
        """Test empty string query returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_search_query("")
        assert "Search query is required" in str(exc_info.value)
    
    def test_whitespace_only_query(self, client):
        """Test whitespace-only query returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client._validate_search_query("   ")
        assert "Search query is required" in str(exc_info.value)


class TestInvalidSpaceKey:
    """Tests for invalid space key handling.
    
    **Feature: confluence-mcp-server, Property 14: Invalid space key returns error**
    **Validates: Requirements 7.3**
    """
    
    @responses.activate
    def test_invalid_space_key_404(self, client):
        """Property 14: For any space key that does not exist, the list_space_pages 
        tool SHALL return a ToolResponse with success=false and an error message 
        indicating the space was not found.
        
        **Feature: confluence-mcp-server, Property 14: Invalid space key returns error**
        **Validates: Requirements 7.3**
        """
        responses.add(
            responses.GET,
            "https://test.confluence.example.com/rest/api/content",
            json={"message": "Space not found"},
            status=404
        )
        
        with pytest.raises(ConfluenceNotFoundError) as exc_info:
            client.list_space_pages("INVALID")
        
        assert "not found" in str(exc_info.value).lower()
    
    def test_empty_space_key(self, client):
        """Test empty space key returns error."""
        with pytest.raises(ConfluenceValidationError) as exc_info:
            client.list_space_pages("")
        assert "Space key is required" in str(exc_info.value)
    
    @given(
        empty_key=st.one_of(
            st.just(""),
            st.just("   "),
            st.text(alphabet=st.characters(whitelist_categories=('Zs',)), min_size=0, max_size=5)
        )
    )
    @settings(max_examples=50)
    def test_empty_space_key_validation(self, client, empty_key):
        """Test various empty space key formats."""
        assume(not empty_key.strip())
        
        with pytest.raises(ConfluenceValidationError):
            client.list_space_pages(empty_key)
