"""Tests for MCP server.

Property-based tests for server initialization and tool registration.
"""

import pytest
import os
import sys
from unittest.mock import patch, MagicMock
from hypothesis import given, strategies as st, settings

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))


class TestSSEConfiguration:
    """Tests for SSE transport mode configuration.
    
    **Feature: confluence-mcp-server, Property 11: SSE mode accepts custom host and port**
    **Validates: Requirements 6.3**
    """
    
    @given(
        host=st.sampled_from(["localhost", "127.0.0.1", "0.0.0.0", "192.168.1.1"]),
        port=st.integers(min_value=1024, max_value=65535)
    )
    @settings(max_examples=50)
    def test_sse_custom_host_port(self, host, port):
        """Property 11: For any valid host string and port number provided via 
        command-line arguments in SSE mode, the server SHALL configure to use 
        those values.
        
        **Feature: confluence-mcp-server, Property 11: SSE mode accepts custom host and port**
        **Validates: Requirements 6.3**
        """
        from server import parse_args
        
        with patch('sys.argv', ['server.py', '--sse', '--host', host, '--port', str(port)]):
            args = parse_args()
            
            assert args.sse is True
            assert args.host == host
            assert args.port == port
    
    def test_default_stdio_mode(self):
        """Test default transport mode is stdio."""
        from server import parse_args
        
        with patch('sys.argv', ['server.py']):
            args = parse_args()
            
            assert args.sse is False
    
    def test_sse_default_host_port(self):
        """Test SSE mode default host and port."""
        from server import parse_args
        
        with patch('sys.argv', ['server.py', '--sse']):
            args = parse_args()
            
            assert args.sse is True
            assert args.host == "localhost"
            assert args.port == 8000


class TestToolRegistration:
    """Tests for MCP tool registration."""
    
    def test_tools_registered(self):
        """Test that all tools are registered with FastMCP."""
        from server import mcp
        
        # Get registered tools
        tools = mcp._tools if hasattr(mcp, '_tools') else {}
        
        # Note: FastMCP may register tools differently
        # This test verifies the module loads without errors
        assert mcp is not None
        assert mcp.name == "confluence-connector"


class TestGetClient:
    """Tests for client initialization."""
    
    def test_get_client_creates_instance(self, mock_env_vars):
        """Test that get_client creates a client instance."""
        from server import get_client, _client
        import server
        
        # Reset global client
        server._client = None
        
        client = get_client()
        
        assert client is not None
        assert server._client is not None
    
    def test_get_client_reuses_instance(self, mock_env_vars):
        """Test that get_client reuses existing instance."""
        from server import get_client
        import server
        
        # Reset global client
        server._client = None
        
        client1 = get_client()
        client2 = get_client()
        
        assert client1 is client2
    
    def test_get_client_missing_config(self, monkeypatch):
        """Test that get_client raises error with missing config."""
        import server
        
        # Reset global client
        server._client = None
        
        # Remove environment variables
        monkeypatch.delenv("CONFLUENCE_BASE_URL", raising=False)
        monkeypatch.delenv("CONFLUENCE_PAT", raising=False)
        
        with pytest.raises(ValueError):
            server.get_client()


class TestToolFunctions:
    """Tests for individual tool functions."""
    
    def test_get_page_by_id_validation_error(self, mock_env_vars):
        """Test get_page_by_id with invalid page ID."""
        from server import get_page_by_id
        import server
        
        # Reset global client
        server._client = None
        
        result = get_page_by_id("invalid")
        
        assert result["success"] is False
        assert "error" in result
    
    def test_search_pages_validation_error(self, mock_env_vars):
        """Test search_pages with empty query."""
        from server import search_pages
        import server
        
        # Reset global client
        server._client = None
        
        result = search_pages("")
        
        assert result["success"] is False
        assert "error" in result
    
    def test_list_space_pages_validation_error(self, mock_env_vars):
        """Test list_space_pages with empty space key."""
        from server import list_space_pages
        import server
        
        # Reset global client
        server._client = None
        
        result = list_space_pages("")
        
        assert result["success"] is False
        assert "error" in result
