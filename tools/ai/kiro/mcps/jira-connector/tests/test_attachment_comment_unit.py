"""
Unit tests for Jira MCP Server attachment and comment features.

Tests the JiraClient methods with mocked HTTP responses.

**Feature: jira-attachment-comment**
"""
import os
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

import pytest
import requests

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from jira_client import JiraClient, JiraConfig, ToolResponse


@pytest.fixture
def jira_client():
    """Create a JiraClient with test configuration."""
    config = JiraConfig(
        base_url="https://test-jira.example.com",
        pat="test-token-12345"
    )
    return JiraClient(config)


class TestCreateComment:
    """
    Unit tests for JiraClient.create_comment method.
    
    **Validates: Requirements 1.1, 1.2**
    """

    def test_create_comment_success(self, jira_client):
        """Test successful comment creation."""
        mock_response = MagicMock()
        mock_response.ok = True
        mock_response.json.return_value = {
            "id": "12345",
            "body": "Test comment",
            "author": {"displayName": "Test User"}
        }
        
        with patch.object(jira_client._session, 'post', return_value=mock_response) as mock_post:
            result = jira_client.create_comment("TEST-123", "Test comment")
            
            assert result.success is True
            assert result.data["id"] == "12345"
            assert result.data["issue_key"] == "TEST-123"
            assert "url" in result.data
            
            # Verify the correct URL was called
            mock_post.assert_called_once()
            call_args = mock_post.call_args
            assert "TEST-123/comment" in call_args[0][0]

    def test_create_comment_issue_not_found(self, jira_client):
        """Test comment creation on non-existent issue returns 404 error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 404
        mock_response.text = "Issue Does Not Exist"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.create_comment("NONEXISTENT-999", "Test comment")
            
            assert result.success is False
            assert "not found" in result.error.lower()

    def test_create_comment_unauthorized(self, jira_client):
        """Test comment creation with invalid credentials returns 401 error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 401
        mock_response.text = "Unauthorized"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.create_comment("TEST-123", "Test comment")
            
            assert result.success is False
            assert "authentication" in result.error.lower() or "invalid" in result.error.lower()

    def test_create_comment_forbidden(self, jira_client):
        """Test comment creation without permission returns 403 error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 403
        mock_response.text = "Forbidden"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.create_comment("TEST-123", "Test comment")
            
            assert result.success is False
            assert "denied" in result.error.lower() or "permission" in result.error.lower()

    def test_create_comment_timeout(self, jira_client):
        """Test comment creation handles timeout gracefully."""
        with patch.object(jira_client._session, 'post', side_effect=requests.exceptions.Timeout()):
            result = jira_client.create_comment("TEST-123", "Test comment")
            
            assert result.success is False
            assert "timed out" in result.error.lower() or "timeout" in result.error.lower()

    def test_create_comment_connection_error(self, jira_client):
        """Test comment creation handles connection error gracefully."""
        with patch.object(jira_client._session, 'post', side_effect=requests.exceptions.ConnectionError("Connection refused")):
            result = jira_client.create_comment("TEST-123", "Test comment")
            
            assert result.success is False
            assert "connect" in result.error.lower()


class TestCreateAttachment:
    """
    Unit tests for JiraClient.create_attachment method.
    
    **Validates: Requirements 2.1, 2.2, 2.6**
    """

    def test_create_attachment_success(self, jira_client):
        """Test successful attachment upload."""
        mock_response = MagicMock()
        mock_response.ok = True
        mock_response.json.return_value = [{
            "id": "67890",
            "filename": "test.txt",
            "size": 1024,
            "mimeType": "text/plain",
            "content": "https://test-jira.example.com/secure/attachment/67890/test.txt"
        }]
        
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', return_value=mock_response) as mock_post:
                result = jira_client.create_attachment("TEST-123", temp_file)
                
                assert result.success is True
                assert result.data["id"] == "67890"
                assert result.data["filename"] == "test.txt"
                assert result.data["issue_key"] == "TEST-123"
                assert result.data["size"] == 1024
                assert result.data["mime_type"] == "text/plain"
                
                # Verify the correct URL was called
                mock_post.assert_called_once()
                call_args = mock_post.call_args
                # Check positional args or keyword args for URL
                url_found = False
                if call_args.args:
                    url_found = "TEST-123/attachments" in str(call_args.args[0])
                if not url_found and call_args.kwargs:
                    url_found = "TEST-123/attachments" in str(call_args.kwargs)
                assert url_found or "TEST-123/attachments" in str(call_args)
        finally:
            os.unlink(temp_file)

    def test_create_attachment_file_not_found(self, jira_client):
        """Test attachment upload with non-existent file."""
        result = jira_client.create_attachment("TEST-123", "/nonexistent/file.txt")
        
        assert result.success is False
        assert "not found" in result.error.lower()

    def test_create_attachment_issue_not_found(self, jira_client):
        """Test attachment upload to non-existent issue returns 404 error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 404
        mock_response.text = "Issue Does Not Exist"
        
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', return_value=mock_response):
                result = jira_client.create_attachment("NONEXISTENT-999", temp_file)
                
                assert result.success is False
                assert "not found" in result.error.lower()
        finally:
            os.unlink(temp_file)

    def test_create_attachment_unauthorized(self, jira_client):
        """Test attachment upload with invalid credentials returns 401 error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 401
        mock_response.text = "Unauthorized"
        
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', return_value=mock_response):
                result = jira_client.create_attachment("TEST-123", temp_file)
                
                assert result.success is False
                assert "authentication" in result.error.lower() or "invalid" in result.error.lower()
        finally:
            os.unlink(temp_file)

    def test_create_attachment_file_too_large(self, jira_client):
        """Test attachment upload with file exceeding size limit returns error."""
        mock_response = MagicMock()
        mock_response.ok = False
        mock_response.status_code = 400
        mock_response.text = "The file is too large"
        
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', return_value=mock_response):
                result = jira_client.create_attachment("TEST-123", temp_file)
                
                assert result.success is False
                assert "bad request" in result.error.lower() or "too large" in result.error.lower()
        finally:
            os.unlink(temp_file)

    def test_create_attachment_timeout(self, jira_client):
        """Test attachment upload handles timeout gracefully."""
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', side_effect=requests.exceptions.Timeout()):
                result = jira_client.create_attachment("TEST-123", temp_file)
                
                assert result.success is False
                assert "timed out" in result.error.lower() or "timeout" in result.error.lower()
        finally:
            os.unlink(temp_file)

    def test_create_attachment_connection_error(self, jira_client):
        """Test attachment upload handles connection error gracefully."""
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', side_effect=requests.exceptions.ConnectionError("Connection refused")):
                result = jira_client.create_attachment("TEST-123", temp_file)
                
                assert result.success is False
                assert "connect" in result.error.lower()
        finally:
            os.unlink(temp_file)

    def test_create_attachment_includes_xsrf_header(self, jira_client):
        """Test that attachment upload includes X-Atlassian-Token header."""
        mock_response = MagicMock()
        mock_response.ok = True
        mock_response.json.return_value = [{
            "id": "67890",
            "filename": "test.txt",
            "size": 1024,
            "mimeType": "text/plain",
            "content": "https://test-jira.example.com/secure/attachment/67890/test.txt"
        }]
        
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("Test content")
            temp_file = f.name
        
        try:
            with patch('requests.post', return_value=mock_response) as mock_post:
                jira_client.create_attachment("TEST-123", temp_file)
                
                # Verify X-Atlassian-Token header was included
                call_args = mock_post.call_args
                headers = call_args[1].get("headers", {})
                assert headers.get("X-Atlassian-Token") == "no-check"
        finally:
            os.unlink(temp_file)
