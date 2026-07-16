"""
Property-based tests for Jira MCP Server attachment and comment features.

Uses Hypothesis for property-based testing as specified in the design document.

**Feature: jira-attachment-comment**
"""
import os
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest
from hypothesis import given, settings, strategies as st, assume

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from jira_client import ToolResponse


# =============================================================================
# Strategies for generating test data
# =============================================================================

@st.composite
def whitespace_only_string(draw):
    """
    Generate strings composed entirely of whitespace characters.
    Includes empty string, spaces, tabs, newlines, and combinations.
    """
    whitespace_chars = ' \t\n\r\v\f'
    length = draw(st.integers(min_value=0, max_value=50))
    if length == 0:
        return ""
    return draw(st.text(alphabet=whitespace_chars, min_size=length, max_size=length))


@st.composite
def invalid_issue_key_string(draw):
    """
    Generate strings that do NOT match the valid Jira issue key pattern.
    Valid pattern: ^[A-Z][A-Z0-9]+-\\d+$
    """
    strategy = st.one_of(
        # Empty string
        st.just(""),
        # Lowercase letters
        st.text(alphabet="abcdefghijklmnopqrstuvwxyz", min_size=1, max_size=10),
        # Numbers only
        st.text(alphabet="0123456789", min_size=1, max_size=10),
        # Missing hyphen
        st.text(alphabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", min_size=2, max_size=10)
            .filter(lambda x: "-" not in x),
        # Special characters
        st.text(alphabet="!@#$%^&*()_+=[]{}|;':\",./<>?", min_size=1, max_size=10),
        # Hyphen at wrong position
        st.just("-123"),
        st.just("ABC-"),
        st.just("-ABC-123"),
        # Lowercase project key
        st.just("abc-123"),
        # Space in key
        st.just("ABC 123"),
        st.just("ABC- 123"),
    )
    result = draw(strategy)
    # Ensure it doesn't accidentally match the valid pattern
    import re
    assume(not re.match(r"^[A-Z][A-Z0-9]+-\d+$", result))
    return result


@st.composite
def nonexistent_file_path(draw):
    """
    Generate file paths that are guaranteed not to exist.
    """
    # Generate random path components
    random_name = draw(st.text(
        alphabet="abcdefghijklmnopqrstuvwxyz0123456789_-",
        min_size=5,
        max_size=20
    ).filter(lambda x: len(x) >= 5))
    
    random_ext = draw(st.sampled_from([".txt", ".log", ".pdf", ".png", ".doc", ""]))
    
    # Use a non-existent directory structure
    base_paths = [
        "/nonexistent_dir_xyz123/",
        "C:\\nonexistent_dir_xyz123\\",
        "./nonexistent_subdir_abc789/",
        "../nonexistent_parent_def456/",
    ]
    base = draw(st.sampled_from(base_paths))
    
    path = f"{base}{random_name}{random_ext}"
    
    # Ensure the path doesn't exist
    assume(not os.path.exists(path))
    
    return path


# =============================================================================
# Property Tests
# =============================================================================

class TestEmptyCommentRejection:
    """
    **Feature: jira-attachment-comment, Property 2: Empty comment rejection**
    
    *For any* string composed entirely of whitespace characters (including empty string),
    `add_comment` SHALL return an error response with success=False and an error message
    indicating that comment text is required.
    
    **Validates: Requirements 1.3**
    """

    @given(empty_comment=whitespace_only_string())
    @settings(max_examples=100, deadline=None)
    def test_empty_comment_rejection(self, empty_comment):
        """
        **Feature: jira-attachment-comment, Property 2: Empty comment rejection**
        **Validates: Requirements 1.3**
        
        Property: For any whitespace-only string, add_comment SHALL reject it.
        """
        from server import add_comment
        
        # Use a valid issue key format
        valid_issue_key = "TEST-123"
        
        # Call add_comment with the whitespace-only body
        result = add_comment(issue_key=valid_issue_key, body=empty_comment)
        
        # Property: Must return error (success=False)
        assert result["success"] is False, \
            f"Expected rejection for whitespace-only comment: '{repr(empty_comment)}'"
        
        # Property: Error message must indicate comment is required
        assert "error" in result, "Error response must contain 'error' field"
        error_msg = result["error"].lower()
        assert "comment" in error_msg or "body" in error_msg or "empty" in error_msg or "required" in error_msg, \
            f"Error message should mention comment/body/empty/required: {result['error']}"


class TestNonExistentFileRejection:
    """
    **Feature: jira-attachment-comment, Property 3: Non-existent file rejection**
    
    *For any* file path that does not exist on the filesystem,
    `add_attachment` SHALL return an error response with success=False and an error message
    indicating the file was not found.
    
    **Validates: Requirements 2.3**
    """

    @given(nonexistent_path=nonexistent_file_path())
    @settings(max_examples=100, deadline=None)
    def test_nonexistent_file_rejection(self, nonexistent_path):
        """
        **Feature: jira-attachment-comment, Property 3: Non-existent file rejection**
        **Validates: Requirements 2.3**
        
        Property: For any non-existent file path, add_attachment SHALL reject it.
        """
        from server import add_attachment
        
        # Use a valid issue key format
        valid_issue_key = "TEST-123"
        
        # Call add_attachment with the non-existent file path
        result = add_attachment(issue_key=valid_issue_key, file_path=nonexistent_path)
        
        # Property: Must return error (success=False)
        assert result["success"] is False, \
            f"Expected rejection for non-existent file: '{nonexistent_path}'"
        
        # Property: Error message must indicate file not found
        assert "error" in result, "Error response must contain 'error' field"
        error_msg = result["error"].lower()
        assert "file" in error_msg and ("not found" in error_msg or "not exist" in error_msg or "does not exist" in error_msg), \
            f"Error message should mention file not found: {result['error']}"


class TestInvalidIssueKeyRejection:
    """
    **Feature: jira-attachment-comment, Property 1: Invalid issue key format rejection**
    
    *For any* string that does not match the pattern ^[A-Z][A-Z0-9]+-\\d+$,
    both `add_comment` and `add_attachment` SHALL return an error response with
    success=False and an error message containing "Invalid ticket key format".
    
    **Validates: Requirements 1.4, 2.4**
    """

    @given(invalid_key=invalid_issue_key_string())
    @settings(max_examples=100, deadline=None)
    def test_add_comment_rejects_invalid_issue_key(self, invalid_key):
        """
        **Feature: jira-attachment-comment, Property 1: Invalid issue key format rejection**
        **Validates: Requirements 1.4**
        
        Property: For any invalid issue key, add_comment SHALL reject it.
        """
        from server import add_comment
        
        # Use a valid comment body
        valid_body = "This is a test comment"
        
        # Call add_comment with the invalid issue key
        result = add_comment(issue_key=invalid_key, body=valid_body)
        
        # Property: Must return error (success=False)
        assert result["success"] is False, \
            f"Expected rejection for invalid issue key: '{invalid_key}'"
        
        # Property: Error message must indicate invalid format
        assert "error" in result, "Error response must contain 'error' field"
        error_msg = result["error"].lower()
        assert "invalid" in error_msg and ("key" in error_msg or "format" in error_msg), \
            f"Error message should mention invalid key/format: {result['error']}"

    @given(invalid_key=invalid_issue_key_string())
    @settings(max_examples=100, deadline=None)
    def test_add_attachment_rejects_invalid_issue_key(self, invalid_key):
        """
        **Feature: jira-attachment-comment, Property 1: Invalid issue key format rejection**
        **Validates: Requirements 2.4**
        
        Property: For any invalid issue key, add_attachment SHALL reject it.
        """
        from server import add_attachment
        
        # Use a file path (doesn't matter if it exists, key validation happens first)
        file_path = "/some/file.txt"
        
        # Call add_attachment with the invalid issue key
        result = add_attachment(issue_key=invalid_key, file_path=file_path)
        
        # Property: Must return error (success=False)
        assert result["success"] is False, \
            f"Expected rejection for invalid issue key: '{invalid_key}'"
        
        # Property: Error message must indicate invalid format
        assert "error" in result, "Error response must contain 'error' field"
        error_msg = result["error"].lower()
        assert "invalid" in error_msg and ("key" in error_msg or "format" in error_msg), \
            f"Error message should mention invalid key/format: {result['error']}"
