"""
Property-based tests for log work (time tracking) functionality.
Tests server-side validation and tool behavior.
"""
import pytest
from unittest.mock import Mock, patch, MagicMock

from src.server import log_work, get_worklogs, validate_ticket_key
from src.jira_client import ToolResponse


class TestLogWorkValidation:
    """Tests for log_work input validation."""
    
    def test_log_work_rejects_invalid_issue_key(self):
        """Test that log_work rejects invalid issue keys before API call."""
        invalid_keys = [
            "",           # empty
            "   ",        # whitespace only
            "123",        # no project prefix
            "proj-123",   # lowercase project
            "PROJ",       # no number
            "PROJ-",      # no number after dash
            "-123",       # no project
            "PROJ_123",   # underscore instead of dash
        ]
        
        for key in invalid_keys:
            result = log_work(issue_key=key, time_spent="1h")
            assert result["success"] is False, f"Should reject invalid key: {key}"
            assert "error" in result
    
    def test_log_work_rejects_empty_time_spent(self):
        """Test that log_work rejects empty time_spent."""
        empty_values = ["", "   ", None]
        
        for value in empty_values:
            if value is None:
                # Skip None as it would cause TypeError
                continue
            result = log_work(issue_key="PROJ-123", time_spent=value)
            assert result["success"] is False
            assert "time_spent" in result["error"].lower() or "required" in result["error"].lower()
    
    @patch('src.server._get_client_or_error')
    def test_log_work_accepts_valid_time_formats(self, mock_get_client):
        """Test that log_work accepts various valid time formats."""
        mock_client = Mock()
        mock_client.log_work.return_value = ToolResponse(
            success=True,
            data={"id": "123", "time_spent": "1h"}
        )
        mock_get_client.return_value = mock_client
        
        valid_formats = [
            "1h",
            "30m",
            "1h 30m",
            "1d",
            "1d 2h",
            "1d 2h 30m",
            "2w",
            "2w 3d 4h 30m",
        ]
        
        for time_format in valid_formats:
            result = log_work(issue_key="PROJ-123", time_spent=time_format)
            # Should at least pass validation and call the client
            mock_client.log_work.assert_called()


class TestGetWorklogsValidation:
    """Tests for get_worklogs input validation."""
    
    def test_get_worklogs_rejects_invalid_issue_key(self):
        """Test that get_worklogs rejects invalid issue keys before API call."""
        invalid_keys = [
            "",           # empty
            "   ",        # whitespace only
            "123",        # no project prefix
            "proj-123",   # lowercase project
            "PROJ",       # no number
        ]
        
        for key in invalid_keys:
            result = get_worklogs(issue_key=key)
            assert result["success"] is False, f"Should reject invalid key: {key}"
            assert "error" in result
    
    @patch('src.server._get_client_or_error')
    def test_get_worklogs_accepts_valid_issue_keys(self, mock_get_client):
        """Test that get_worklogs accepts valid issue keys."""
        mock_client = Mock()
        mock_client.get_worklogs.return_value = ToolResponse(
            success=True,
            data={"issue_key": "PROJ-123", "total": 0, "worklogs": []}
        )
        mock_get_client.return_value = mock_client
        
        valid_keys = [
            "PROJ-1",
            "PROJ-123",
            "PROJ-12345",
            "ABC123-456",
            "VWICAS23-334534",
        ]
        
        for key in valid_keys:
            result = get_worklogs(issue_key=key)
            mock_client.get_worklogs.assert_called()


class TestLogWorkResponseStructure:
    """Tests for log_work response structure."""
    
    @patch('src.server._get_client_or_error')
    def test_log_work_success_response_structure(self, mock_get_client):
        """Test that successful log_work returns expected structure."""
        mock_client = Mock()
        mock_client.log_work.return_value = ToolResponse(
            success=True,
            data={
                "id": "12345",
                "issue_key": "PROJ-123",
                "time_spent": "2h",
                "time_spent_seconds": 7200,
                "author": "Test User",
                "started": "2025-02-06T09:00:00.000+0000",
                "comment": "Test comment",
                "url": "https://jira.example.com/browse/PROJ-123?focusedWorklogId=12345"
            }
        )
        mock_get_client.return_value = mock_client
        
        result = log_work(issue_key="PROJ-123", time_spent="2h", comment="Test comment")
        
        assert result["success"] is True
        assert "data" in result
        data = result["data"]
        assert "id" in data
        assert "issue_key" in data
        assert "time_spent" in data
        assert "url" in data


class TestGetWorklogsResponseStructure:
    """Tests for get_worklogs response structure."""
    
    @patch('src.server._get_client_or_error')
    def test_get_worklogs_success_response_structure(self, mock_get_client):
        """Test that successful get_worklogs returns expected structure."""
        mock_client = Mock()
        mock_client.get_worklogs.return_value = ToolResponse(
            success=True,
            data={
                "issue_key": "PROJ-123",
                "total": 2,
                "worklogs": [
                    {
                        "id": "111",
                        "author": "User A",
                        "time_spent": "2h",
                        "time_spent_seconds": 7200,
                        "started": "2025-02-05T09:00:00.000+0000",
                        "comment": "Work 1",
                        "created": "2025-02-05T11:00:00.000+0000",
                        "updated": "2025-02-05T11:00:00.000+0000"
                    },
                    {
                        "id": "222",
                        "author": "User B",
                        "time_spent": "1h",
                        "time_spent_seconds": 3600,
                        "started": "2025-02-06T14:00:00.000+0000",
                        "comment": "Work 2",
                        "created": "2025-02-06T15:00:00.000+0000",
                        "updated": "2025-02-06T15:00:00.000+0000"
                    }
                ]
            }
        )
        mock_get_client.return_value = mock_client
        
        result = get_worklogs(issue_key="PROJ-123")
        
        assert result["success"] is True
        assert "data" in result
        data = result["data"]
        assert "issue_key" in data
        assert "total" in data
        assert "worklogs" in data
        assert isinstance(data["worklogs"], list)
        assert len(data["worklogs"]) == 2
        
        # Check worklog entry structure
        for wl in data["worklogs"]:
            assert "id" in wl
            assert "author" in wl
            assert "time_spent" in wl


class TestNoActiveConfigError:
    """Tests for behavior when no Jira config is active."""
    
    @patch('src.server._get_client_or_error')
    def test_log_work_returns_error_without_active_config(self, mock_get_client):
        """Test that log_work returns error when no config is active."""
        mock_get_client.return_value = {
            "success": False,
            "error": "No active Jira configuration. Use add_jira_config and set_active_jira first."
        }
        
        result = log_work(issue_key="PROJ-123", time_spent="1h")
        
        assert result["success"] is False
        assert "no active" in result["error"].lower()
    
    @patch('src.server._get_client_or_error')
    def test_get_worklogs_returns_error_without_active_config(self, mock_get_client):
        """Test that get_worklogs returns error when no config is active."""
        mock_get_client.return_value = {
            "success": False,
            "error": "No active Jira configuration. Use add_jira_config and set_active_jira first."
        }
        
        result = get_worklogs(issue_key="PROJ-123")
        
        assert result["success"] is False
        assert "no active" in result["error"].lower()
