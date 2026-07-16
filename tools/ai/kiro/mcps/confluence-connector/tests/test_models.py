"""Tests for data models.

**Feature: confluence-mcp-server, Property 8: Configuration loading from environment**
**Validates: Requirements 4.1**
"""

import pytest
import os
from hypothesis import given, strategies as st, settings

# Import from src
import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from models import ToolResponse, ConfluenceConfig, PageContent, SearchResult, PageSummary


class TestToolResponse:
    """Tests for ToolResponse dataclass."""
    
    def test_success_response(self):
        """Test creating a successful response."""
        response = ToolResponse.success_response({"key": "value"})
        assert response.success is True
        assert response.data == {"key": "value"}
        assert response.error is None
    
    def test_error_response(self):
        """Test creating an error response."""
        response = ToolResponse.error_response("Something went wrong")
        assert response.success is False
        assert response.data is None
        assert response.error == "Something went wrong"
    
    def test_to_dict_success(self):
        """Test serialization of successful response."""
        response = ToolResponse(success=True, data={"id": "123"})
        result = response.to_dict()
        assert result == {"success": True, "data": {"id": "123"}}
    
    def test_to_dict_error(self):
        """Test serialization of error response."""
        response = ToolResponse(success=False, error="Error message")
        result = response.to_dict()
        assert result == {"success": False, "error": "Error message"}
    
    def test_to_dict_excludes_none(self):
        """Test that None values are excluded from serialization."""
        response = ToolResponse(success=True)
        result = response.to_dict()
        assert "data" not in result
        assert "error" not in result


class TestConfluenceConfig:
    """Tests for ConfluenceConfig dataclass."""
    
    def test_from_environment_success(self, mock_env_vars):
        """Test loading config from environment variables."""
        config = ConfluenceConfig.from_environment()
        assert config.base_url == mock_env_vars["base_url"]
        assert config.pat == mock_env_vars["pat"]
        assert config.timeout == 30
    
    def test_from_environment_custom_timeout(self, mock_env_vars, monkeypatch):
        """Test loading config with custom timeout."""
        monkeypatch.setenv("CONFLUENCE_TIMEOUT", "60")
        config = ConfluenceConfig.from_environment()
        assert config.timeout == 60
    
    def test_from_environment_missing_base_url(self, monkeypatch):
        """Test error when CONFLUENCE_BASE_URL is missing."""
        monkeypatch.setenv("CONFLUENCE_PAT", "test-token")
        monkeypatch.delenv("CONFLUENCE_BASE_URL", raising=False)
        
        with pytest.raises(ValueError) as exc_info:
            ConfluenceConfig.from_environment()
        assert "CONFLUENCE_BASE_URL" in str(exc_info.value)
    
    def test_from_environment_missing_pat(self, monkeypatch):
        """Test error when CONFLUENCE_PAT is missing."""
        monkeypatch.setenv("CONFLUENCE_BASE_URL", "https://test.example.com")
        monkeypatch.delenv("CONFLUENCE_PAT", raising=False)
        
        with pytest.raises(ValueError) as exc_info:
            ConfluenceConfig.from_environment()
        assert "CONFLUENCE_PAT" in str(exc_info.value)
    
    def test_from_environment_strips_trailing_slash(self, monkeypatch):
        """Test that trailing slash is removed from base_url."""
        monkeypatch.setenv("CONFLUENCE_BASE_URL", "https://test.example.com/")
        monkeypatch.setenv("CONFLUENCE_PAT", "test-token")
        
        config = ConfluenceConfig.from_environment()
        assert config.base_url == "https://test.example.com"


class TestConfluenceConfigProperty:
    """Property-based tests for ConfluenceConfig.
    
    **Feature: confluence-mcp-server, Property 8: Configuration loading from environment**
    **Validates: Requirements 4.1**
    """
    
    @given(
        base_url=st.text(min_size=1, alphabet=st.characters(whitelist_categories=('L', 'N', 'P', 'S'))).filter(lambda x: x.strip()),
        pat=st.text(min_size=1, alphabet=st.characters(whitelist_categories=('L', 'N', 'P', 'S'))).filter(lambda x: x.strip())
    )
    @settings(max_examples=100)
    def test_config_loading_preserves_values(self, base_url, pat, monkeypatch):
        """Property 8: For any set of environment variables, the ConfluenceConfig 
        object SHALL contain values matching CONFLUENCE_BASE_URL and CONFLUENCE_PAT 
        when present.
        
        **Feature: confluence-mcp-server, Property 8: Configuration loading from environment**
        **Validates: Requirements 4.1**
        """
        # Set environment variables
        monkeypatch.setenv("CONFLUENCE_BASE_URL", base_url)
        monkeypatch.setenv("CONFLUENCE_PAT", pat)
        
        # Load config
        config = ConfluenceConfig.from_environment()
        
        # Verify values match (base_url has trailing slash stripped)
        assert config.base_url == base_url.rstrip("/")
        assert config.pat == pat


class TestPageContent:
    """Tests for PageContent dataclass."""
    
    def test_to_dict(self):
        """Test serialization of PageContent."""
        page = PageContent(
            id="123",
            title="Test",
            space_key="TEST",
            version=1,
            last_modified="2024-01-01T00:00:00Z",
            body_storage="<p>Content</p>",
            body_plain_text="Content",
            url="/pages/viewpage.action?pageId=123"
        )
        result = page.to_dict()
        assert result["id"] == "123"
        assert result["title"] == "Test"
        assert result["space_key"] == "TEST"


class TestSearchResult:
    """Tests for SearchResult dataclass."""
    
    def test_to_dict(self):
        """Test serialization of SearchResult."""
        result = SearchResult(
            id="123",
            title="Test",
            space_key="TEST",
            excerpt="...test...",
            url="/pages/viewpage.action?pageId=123"
        )
        d = result.to_dict()
        assert d["id"] == "123"
        assert d["excerpt"] == "...test..."


class TestPageSummary:
    """Tests for PageSummary dataclass."""
    
    def test_to_dict(self):
        """Test serialization of PageSummary."""
        summary = PageSummary(id="123", title="Test Page")
        result = summary.to_dict()
        assert result == {"id": "123", "title": "Test Page"}
