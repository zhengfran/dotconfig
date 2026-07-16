"""
Shared pytest fixtures and configuration for Jira MCP Server tests.
"""
import os
import sys
from pathlib import Path

import pytest

# Add src directory to Python path for imports
src_path = Path(__file__).parent.parent / "src"
sys.path.insert(0, str(src_path))

# Set dummy environment variables for testing
os.environ.setdefault("JIRA_BASE_URL", "https://test-jira.example.com")
os.environ.setdefault("JIRA_PAT", "test-token-12345")
