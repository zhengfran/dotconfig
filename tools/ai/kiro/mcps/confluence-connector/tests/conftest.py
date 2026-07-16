"""Shared fixtures and configuration for tests."""

import pytest
import os
import sys

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))


@pytest.fixture
def mock_env_vars(monkeypatch):
    """Fixture to set up mock environment variables."""
    monkeypatch.setenv("CONFLUENCE_BASE_URL", "https://test.confluence.example.com")
    monkeypatch.setenv("CONFLUENCE_PAT", "test-pat-token-12345")
    return {
        "base_url": "https://test.confluence.example.com",
        "pat": "test-pat-token-12345"
    }


@pytest.fixture
def sample_page_response():
    """Sample Confluence API page response."""
    return {
        "id": "123456",
        "title": "Test Page",
        "space": {"key": "TEST"},
        "version": {"number": 5},
        "history": {"lastUpdated": {"when": "2024-01-15T10:30:00.000Z"}},
        "body": {
            "storage": {
                "value": "<p>This is <strong>test</strong> content.</p>"
            }
        },
        "_links": {
            "webui": "/pages/viewpage.action?pageId=123456"
        }
    }


@pytest.fixture
def sample_search_response():
    """Sample Confluence API search response."""
    return {
        "results": [
            {
                "content": {
                    "id": "123456",
                    "title": "Test Page",
                    "space": {"key": "TEST"},
                    "_links": {"webui": "/pages/viewpage.action?pageId=123456"}
                },
                "excerpt": "...test <strong>content</strong>..."
            }
        ],
        "totalSize": 42,
        "start": 0,
        "limit": 10
    }


@pytest.fixture
def sample_space_pages_response():
    """Sample Confluence API space pages response."""
    return {
        "results": [
            {"id": "123456", "title": "Page One"},
            {"id": "123457", "title": "Page Two"},
            {"id": "123458", "title": "Page Three"}
        ],
        "start": 0,
        "limit": 25,
        "size": 3
    }
