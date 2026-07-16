"""
Unit tests for log work (time tracking) functionality.
"""
import pytest
from unittest.mock import Mock, patch, MagicMock
from requests.exceptions import Timeout, ConnectionError

from src.jira_client import JiraClient, JiraConfig, ToolResponse


@pytest.fixture
def jira_config():
    """Create a test JiraConfig."""
    return JiraConfig(
        base_url="https://jira.example.com",
        pat="test-pat-token"
    )


@pytest.fixture
def jira_client(jira_config):
    """Create a JiraClient with mocked session."""
    client = JiraClient(jira_config)
    return client


class TestLogWork:
    """Tests for the log_work method."""
    
    def test_log_work_success(self, jira_client):
        """Test successful work logging."""
        mock_response = Mock()
        mock_response.ok = True
        mock_response.json.return_value = {
            "id": "12345",
            "timeSpent": "2h",
            "timeSpentSeconds": 7200,
            "author": {"displayName": "Test User"},
            "started": "2025-02-06T09:00:00.000+0000",
            "comment": "Worked on feature implementation"
        }
        
        with patch.object(jira_client._session, 'post', return_value=mock_response) as mock_post:
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="2h",
                comment="Worked on feature implementation"
            )
            
            assert result.success is True
            assert result.data["id"] == "12345"
            assert result.data["time_spent"] == "2h"
            assert result.data["time_spent_seconds"] == 7200
            assert result.data["author"] == "Test User"
            assert result.data["issue_key"] == "PROJ-123"
            
            # Verify the API was called correctly
            mock_post.assert_called_once()
            call_args = mock_post.call_args
            assert "PROJ-123/worklog" in call_args[0][0]
            assert call_args[1]["json"]["timeSpent"] == "2h"
            assert call_args[1]["json"]["comment"] == "Worked on feature implementation"
    
    def test_log_work_with_started_time(self, jira_client):
        """Test work logging with custom start time."""
        mock_response = Mock()
        mock_response.ok = True
        mock_response.json.return_value = {
            "id": "12346",
            "timeSpent": "1h 30m",
            "timeSpentSeconds": 5400,
            "author": {"displayName": "Test User"},
            "started": "2025-02-05T14:00:00.000+0000",
            "comment": ""
        }
        
        with patch.object(jira_client._session, 'post', return_value=mock_response) as mock_post:
            result = jira_client.log_work(
                issue_key="PROJ-456",
                time_spent="1h 30m",
                started="2025-02-05T14:00:00.000+0000"
            )
            
            assert result.success is True
            assert result.data["started"] == "2025-02-05T14:00:00.000+0000"
            
            # Verify started was passed to API
            call_args = mock_post.call_args
            assert call_args[1]["json"]["started"] == "2025-02-05T14:00:00.000+0000"
    
    def test_log_work_issue_not_found(self, jira_client):
        """Test work logging on non-existent issue."""
        mock_response = Mock()
        mock_response.ok = False
        mock_response.status_code = 404
        mock_response.text = "Issue Does Not Exist"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.log_work(
                issue_key="PROJ-999",
                time_spent="1h"
            )
            
            assert result.success is False
            assert "not found" in result.error.lower()
    
    def test_log_work_unauthorized(self, jira_client):
        """Test work logging with invalid credentials."""
        mock_response = Mock()
        mock_response.ok = False
        mock_response.status_code = 401
        mock_response.text = "Unauthorized"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="1h"
            )
            
            assert result.success is False
            assert "authentication" in result.error.lower() or "credentials" in result.error.lower()
    
    def test_log_work_forbidden(self, jira_client):
        """Test work logging without permission."""
        mock_response = Mock()
        mock_response.ok = False
        mock_response.status_code = 403
        mock_response.text = "Forbidden"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="1h"
            )
            
            assert result.success is False
            assert "denied" in result.error.lower() or "permission" in result.error.lower()
    
    def test_log_work_invalid_time_format(self, jira_client):
        """Test work logging with invalid time format."""
        mock_response = Mock()
        mock_response.ok = False
        mock_response.status_code = 400
        mock_response.text = "Invalid time format"
        
        with patch.object(jira_client._session, 'post', return_value=mock_response):
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="invalid"
            )
            
            assert result.success is False
            assert "bad request" in result.error.lower() or "invalid" in result.error.lower()
    
    def test_log_work_timeout(self, jira_client):
        """Test work logging timeout handling."""
        with patch.object(jira_client._session, 'post', side_effect=Timeout()):
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="1h"
            )
            
            assert result.success is False
            assert "timed out" in result.error.lower()
    
    def test_log_work_connection_error(self, jira_client):
        """Test work logging connection error handling."""
        with patch.object(jira_client._session, 'post', side_effect=ConnectionError("Connection refused")):
            result = jira_client.log_work(
                issue_key="PROJ-123",
                time_spent="1h"
            )
            
            assert result.success is False
            assert "connect" in result.error.lower()


class TestGetWorklogs:
    """Tests for the get_worklogs method."""
    
    def test_get_worklogs_success(self, jira_client):
        """Test successful worklog retrieval."""
        mock_response = Mock()
        mock_response.ok = True
        mock_response.json.return_value = {
            "total": 2,
            "worklogs": [
                {
                    "id": "111",
                    "author": {"displayName": "User A"},
                    "timeSpent": "2h",
                    "timeSpentSeconds": 7200,
                    "started": "2025-02-05T09:00:00.000+0000",
                    "comment": "Initial work",
                    "created": "2025-02-05T11:00:00.000+0000",
                    "updated": "2025-02-05T11:00:00.000+0000"
                },
                {
                    "id": "222",
                    "author": {"displayName": "User B"},
                    "timeSpent": "1h 30m",
                    "timeSpentSeconds": 5400,
                    "started": "2025-02-06T14:00:00.000+0000",
                    "comment": "Follow-up work",
                    "created": "2025-02-06T15:30:00.000+0000",
                    "updated": "2025-02-06T15:30:00.000+0000"
                }
            ]
        }
        
        with patch.object(jira_client._session, 'get', return_value=mock_response) as mock_get:
            result = jira_client.get_worklogs("PROJ-123")
            
            assert result.success is True
            assert result.data["issue_key"] == "PROJ-123"
            assert result.data["total"] == 2
            assert len(result.data["worklogs"]) == 2
            
            # Check first worklog
            wl1 = result.data["worklogs"][0]
            assert wl1["id"] == "111"
            assert wl1["author"] == "User A"
            assert wl1["time_spent"] == "2h"
            assert wl1["time_spent_seconds"] == 7200
            
            # Check second worklog
            wl2 = result.data["worklogs"][1]
            assert wl2["id"] == "222"
            assert wl2["author"] == "User B"
            
            # Verify API call
            mock_get.assert_called_once()
            assert "PROJ-123/worklog" in mock_get.call_args[0][0]
    
    def test_get_worklogs_empty(self, jira_client):
        """Test worklog retrieval for issue with no worklogs."""
        mock_response = Mock()
        mock_response.ok = True
        mock_response.json.return_value = {
            "total": 0,
            "worklogs": []
        }
        
        with patch.object(jira_client._session, 'get', return_value=mock_response):
            result = jira_client.get_worklogs("PROJ-123")
            
            assert result.success is True
            assert result.data["total"] == 0
            assert result.data["worklogs"] == []
    
    def test_get_worklogs_issue_not_found(self, jira_client):
        """Test worklog retrieval for non-existent issue."""
        mock_response = Mock()
        mock_response.ok = False
        mock_response.status_code = 404
        mock_response.text = "Issue Does Not Exist"
        
        with patch.object(jira_client._session, 'get', return_value=mock_response):
            result = jira_client.get_worklogs("PROJ-999")
            
            assert result.success is False
            assert "not found" in result.error.lower()
    
    def test_get_worklogs_timeout(self, jira_client):
        """Test worklog retrieval timeout handling."""
        with patch.object(jira_client._session, 'get', side_effect=Timeout()):
            result = jira_client.get_worklogs("PROJ-123")
            
            assert result.success is False
            assert "timed out" in result.error.lower()
    
    def test_get_worklogs_connection_error(self, jira_client):
        """Test worklog retrieval connection error handling."""
        with patch.object(jira_client._session, 'get', side_effect=ConnectionError("Connection refused")):
            result = jira_client.get_worklogs("PROJ-123")
            
            assert result.success is False
            assert "connect" in result.error.lower()
