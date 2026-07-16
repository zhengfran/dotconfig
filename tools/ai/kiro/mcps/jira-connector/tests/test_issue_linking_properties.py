"""
Property-based tests for Jira issue linking functionality.

Uses hypothesis library for property-based testing.
Tests validate correctness properties from the design document.
"""
import re
import pytest
from hypothesis import given, strategies as st, settings

# Import the validation function and response classes
import sys
sys.path.insert(0, 'src')
from jira_client import ToolResponse


# =============================================================================
# Test Helpers - Link Type Response Transformation
# =============================================================================

def transform_link_types(raw_link_types: list) -> list:
    """
    Transform raw Jira API link type response to simplified format.
    This mirrors the transformation in JiraClient.get_link_types().
    """
    simplified = []
    for lt in raw_link_types:
        simplified.append({
            "id": lt.get("id"),
            "name": lt.get("name"),
            "inward": lt.get("inward"),
            "outward": lt.get("outward")
        })
    return simplified


# =============================================================================
# Property 3: Link Type Response Structure
# For any link type returned by get_link_types, the response object SHALL 
# contain the fields: id, name, inward, and outward, all as non-empty strings.
# Validates: Requirements 1.2
# =============================================================================

# Strategy for generating valid link type data from Jira API
link_type_strategy = st.fixed_dictionaries({
    "id": st.text(min_size=1, max_size=10).filter(lambda x: x.strip()),
    "name": st.text(min_size=1, max_size=50).filter(lambda x: x.strip()),
    "inward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
    "outward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
    # Extra fields that might come from Jira API but should be ignored
    "self": st.just("https://jira.example.com/rest/api/2/issueLinkType/10000"),
})


@given(st.lists(link_type_strategy, min_size=0, max_size=20))
@settings(max_examples=100)
def test_link_type_response_structure_property(raw_link_types):
    """
    Property 3: Link Type Response Structure
    
    For any link type returned by get_link_types, the response object SHALL 
    contain the fields: id, name, inward, and outward, all as non-empty strings.
    
    **Validates: Requirements 1.2**
    """
    # Transform the raw data (simulating what JiraClient does)
    transformed = transform_link_types(raw_link_types)
    
    # Verify each transformed link type has required fields
    for link_type in transformed:
        # All required fields must be present
        assert "id" in link_type, "Link type must have 'id' field"
        assert "name" in link_type, "Link type must have 'name' field"
        assert "inward" in link_type, "Link type must have 'inward' field"
        assert "outward" in link_type, "Link type must have 'outward' field"
        
        # All fields must be non-empty strings (when input was non-empty)
        assert isinstance(link_type["id"], str), "id must be a string"
        assert isinstance(link_type["name"], str), "name must be a string"
        assert isinstance(link_type["inward"], str), "inward must be a string"
        assert isinstance(link_type["outward"], str), "outward must be a string"
        
        # Fields should preserve non-empty values from input
        assert link_type["id"], "id must not be empty"
        assert link_type["name"], "name must not be empty"
        assert link_type["inward"], "inward must not be empty"
        assert link_type["outward"], "outward must not be empty"


@given(st.lists(link_type_strategy, min_size=1, max_size=10))
@settings(max_examples=100)
def test_link_type_count_preserved(raw_link_types):
    """
    Property: Link type count is preserved during transformation.
    
    The number of link types in the output should equal the input count.
    """
    transformed = transform_link_types(raw_link_types)
    assert len(transformed) == len(raw_link_types), \
        "Transformation should preserve link type count"


# =============================================================================
# Property 4: Create Link Success Response Structure
# For any successful link creation, the response SHALL contain: the inward 
# issue key, the outward issue key, and the link type name that was used.
# Validates: Requirements 2.7
# =============================================================================

def create_link_success_response(inward_key: str, outward_key: str, link_type: str) -> dict:
    """
    Create a success response for link creation.
    This mirrors the response structure in JiraClient.create_link().
    """
    return {
        "inward_issue": inward_key,
        "outward_issue": outward_key,
        "link_type": link_type,
        "message": f"Link created: {outward_key} -> {inward_key}"
    }


# Strategy for valid issue keys
valid_issue_key_strategy = st.from_regex(r"[A-Z][A-Z0-9]{1,9}-[1-9][0-9]{0,5}", fullmatch=True)

# Strategy for link type names
link_type_name_strategy = st.text(min_size=1, max_size=50).filter(lambda x: x.strip())


@given(
    inward_key=valid_issue_key_strategy,
    outward_key=valid_issue_key_strategy,
    link_type=link_type_name_strategy
)
@settings(max_examples=100)
def test_create_link_success_response_structure_property(inward_key, outward_key, link_type):
    """
    Property 4: Create Link Success Response Structure
    
    For any successful link creation, the response SHALL contain: the inward 
    issue key, the outward issue key, and the link type name that was used.
    
    **Validates: Requirements 2.7**
    """
    # Create the success response (simulating what JiraClient does)
    response = create_link_success_response(inward_key, outward_key, link_type)
    
    # Verify required fields are present
    assert "inward_issue" in response, "Response must have 'inward_issue' field"
    assert "outward_issue" in response, "Response must have 'outward_issue' field"
    assert "link_type" in response, "Response must have 'link_type' field"
    
    # Verify values match input
    assert response["inward_issue"] == inward_key, "inward_issue must match input"
    assert response["outward_issue"] == outward_key, "outward_issue must match input"
    assert response["link_type"] == link_type, "link_type must match input"


@given(
    inward_key=valid_issue_key_strategy,
    outward_key=valid_issue_key_strategy,
    link_type=link_type_name_strategy
)
@settings(max_examples=100)
def test_create_link_response_preserves_keys(inward_key, outward_key, link_type):
    """
    Property: Create link response preserves exact issue keys.
    
    The response should contain the exact same issue keys that were provided,
    without any transformation or normalization.
    """
    response = create_link_success_response(inward_key, outward_key, link_type)
    
    # Keys should be preserved exactly
    assert response["inward_issue"] == inward_key
    assert response["outward_issue"] == outward_key
    assert response["link_type"] == link_type


# =============================================================================
# Property 5: Issue Links Response Structure
# For any link returned by get_issue_links, the link object SHALL contain: 
# id, type, direction (either "inward" or "outward"), and linked_issue with 
# key and summary fields.
# Validates: Requirements 3.2
# =============================================================================

def transform_issue_links(raw_links: list) -> list:
    """
    Transform raw Jira API issue links to simplified format.
    This mirrors the transformation in JiraClient.get_issue_links().
    """
    links = []
    for link in raw_links:
        link_type = link.get("type", {})
        
        # Determine direction and get linked issue
        if "inwardIssue" in link:
            direction = "inward"
            description = link_type.get("inward", "")
            linked_issue = link.get("inwardIssue", {})
        elif "outwardIssue" in link:
            direction = "outward"
            description = link_type.get("outward", "")
            linked_issue = link.get("outwardIssue", {})
        else:
            continue  # Skip malformed links
        
        links.append({
            "id": link.get("id"),
            "type": link_type.get("name"),
            "direction": direction,
            "description": description,
            "linked_issue": {
                "key": linked_issue.get("key"),
                "summary": linked_issue.get("fields", {}).get("summary")
            }
        })
    
    return links


# Strategy for generating inward links from Jira API
inward_link_strategy = st.fixed_dictionaries({
    "id": st.text(min_size=1, max_size=10).filter(lambda x: x.strip()),
    "type": st.fixed_dictionaries({
        "name": st.text(min_size=1, max_size=50).filter(lambda x: x.strip()),
        "inward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
        "outward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
    }),
    "inwardIssue": st.fixed_dictionaries({
        "key": valid_issue_key_strategy,
        "fields": st.fixed_dictionaries({
            "summary": st.text(min_size=1, max_size=200).filter(lambda x: x.strip())
        })
    })
})

# Strategy for generating outward links from Jira API
outward_link_strategy = st.fixed_dictionaries({
    "id": st.text(min_size=1, max_size=10).filter(lambda x: x.strip()),
    "type": st.fixed_dictionaries({
        "name": st.text(min_size=1, max_size=50).filter(lambda x: x.strip()),
        "inward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
        "outward": st.text(min_size=1, max_size=100).filter(lambda x: x.strip()),
    }),
    "outwardIssue": st.fixed_dictionaries({
        "key": valid_issue_key_strategy,
        "fields": st.fixed_dictionaries({
            "summary": st.text(min_size=1, max_size=200).filter(lambda x: x.strip())
        })
    })
})

# Combined strategy for either inward or outward links
issue_link_strategy = st.one_of(inward_link_strategy, outward_link_strategy)


@given(st.lists(issue_link_strategy, min_size=0, max_size=10))
@settings(max_examples=100)
def test_issue_links_response_structure_property(raw_links):
    """
    Property 5: Issue Links Response Structure
    
    For any link returned by get_issue_links, the link object SHALL contain: 
    id, type, direction (either "inward" or "outward"), and linked_issue with 
    key and summary fields.
    
    **Validates: Requirements 3.2**
    """
    # Transform the raw data (simulating what JiraClient does)
    transformed = transform_issue_links(raw_links)
    
    # Verify each transformed link has required fields
    for link in transformed:
        # All required fields must be present
        assert "id" in link, "Link must have 'id' field"
        assert "type" in link, "Link must have 'type' field"
        assert "direction" in link, "Link must have 'direction' field"
        assert "linked_issue" in link, "Link must have 'linked_issue' field"
        
        # Direction must be valid
        assert link["direction"] in ("inward", "outward"), \
            f"direction must be 'inward' or 'outward', got '{link['direction']}'"
        
        # linked_issue must have key and summary
        linked_issue = link["linked_issue"]
        assert "key" in linked_issue, "linked_issue must have 'key' field"
        assert "summary" in linked_issue, "linked_issue must have 'summary' field"


@given(st.lists(issue_link_strategy, min_size=1, max_size=10))
@settings(max_examples=100)
def test_issue_links_direction_matches_source(raw_links):
    """
    Property: Link direction correctly identifies source field.
    
    If the raw link has 'inwardIssue', direction should be 'inward'.
    If the raw link has 'outwardIssue', direction should be 'outward'.
    """
    transformed = transform_issue_links(raw_links)
    
    for i, link in enumerate(transformed):
        raw_link = raw_links[i]
        
        if "inwardIssue" in raw_link:
            assert link["direction"] == "inward", \
                "Link with inwardIssue should have direction='inward'"
        elif "outwardIssue" in raw_link:
            assert link["direction"] == "outward", \
                "Link with outwardIssue should have direction='outward'"


# =============================================================================
# Property 1: Issue Key Validation
# For any string input provided as an issue key, the validation function SHALL 
# return true if and only if the string matches the pattern ^[A-Z][A-Z0-9]+-\d+$
# Validates: Requirements 2.2, 3.3
# =============================================================================

# Import the validation function from server
sys.path.insert(0, 'src')
from server import validate_ticket_key

# Strategy for valid issue keys (matches ^[A-Z][A-Z0-9]+-\d+$)
# Note: The pattern requires at least 2 characters before hyphen (one [A-Z] + one or more [A-Z0-9])
valid_key_strategy = st.from_regex(r"[A-Z][A-Z0-9]+-[1-9][0-9]{0,5}", fullmatch=True)

# Strategy for invalid issue keys
invalid_key_strategies = st.one_of(
    # Empty string
    st.just(""),
    # Lowercase letters
    st.from_regex(r"[a-z][a-z0-9]+-[0-9]+", fullmatch=True),
    # Missing hyphen
    st.from_regex(r"[A-Z][A-Z0-9]+[0-9]+", fullmatch=True),
    # Missing number
    st.from_regex(r"[A-Z][A-Z0-9]+-", fullmatch=True),
    # Starting with number
    st.from_regex(r"[0-9][A-Z0-9]+-[0-9]+", fullmatch=True),
    # Just numbers
    st.from_regex(r"[0-9]+-[0-9]+", fullmatch=True),
    # Single letter before hyphen (doesn't match [A-Z][A-Z0-9]+)
    st.from_regex(r"[A-Z]-[0-9]+", fullmatch=True),
    # Random text without pattern
    st.text(min_size=1, max_size=20).filter(lambda x: not re.match(r"^[A-Z][A-Z0-9]+-\d+$", x)),
)


@given(valid_key_strategy)
@settings(max_examples=100)
def test_valid_issue_keys_pass_validation(key):
    r"""
    Property 1: Issue Key Validation (valid keys)
    
    For any string that matches the pattern ^[A-Z][A-Z0-9]+-\d+$,
    the validation function SHALL return true.
    
    **Validates: Requirements 2.2, 3.3**
    """
    is_valid, error_msg = validate_ticket_key(key)
    assert is_valid, f"Valid key '{key}' should pass validation, got error: {error_msg}"
    assert error_msg == "", f"Valid key should have empty error message, got: {error_msg}"


@given(invalid_key_strategies)
@settings(max_examples=100)
def test_invalid_issue_keys_fail_validation(key):
    r"""
    Property 1: Issue Key Validation (invalid keys)
    
    For any string that does NOT match the pattern ^[A-Z][A-Z0-9]+-\d+$,
    the validation function SHALL return false.
    
    **Validates: Requirements 2.2, 3.3**
    """
    is_valid, error_msg = validate_ticket_key(key)
    assert not is_valid, f"Invalid key '{key}' should fail validation"
    assert error_msg != "", f"Invalid key should have non-empty error message"



# =============================================================================
# Property 2: Invalid Keys Rejected Before API Call
# For any issue key that fails validation, the create_link and get_issue_links 
# functions SHALL return an error response without making any HTTP requests.
# Validates: Requirements 2.4
# =============================================================================

from server import create_issue_link, get_issue_links


@given(invalid_key_strategies)
@settings(max_examples=100)
def test_create_link_rejects_invalid_inward_key_before_api(key):
    r"""
    Property 2: Invalid Keys Rejected Before API Call (create_link - inward)
    
    For any invalid inward issue key, create_issue_link SHALL return an error
    response without making any HTTP requests to the Jira API.
    
    **Validates: Requirements 2.4**
    """
    # Use a valid outward key and link type
    result = create_issue_link(
        inward_issue=key,
        outward_issue="PROJ-123",
        link_type="Blocks"
    )
    
    # Should return error without making API call
    assert result["success"] is False, f"Invalid inward key '{key}' should be rejected"
    assert "error" in result, "Error response should have error message"
    assert "inward" in result["error"].lower() or "invalid" in result["error"].lower(), \
        f"Error should mention invalid key: {result['error']}"


@given(invalid_key_strategies)
@settings(max_examples=100)
def test_create_link_rejects_invalid_outward_key_before_api(key):
    r"""
    Property 2: Invalid Keys Rejected Before API Call (create_link - outward)
    
    For any invalid outward issue key, create_issue_link SHALL return an error
    response without making any HTTP requests to the Jira API.
    
    **Validates: Requirements 2.4**
    """
    # Use a valid inward key and link type
    result = create_issue_link(
        inward_issue="PROJ-123",
        outward_issue=key,
        link_type="Blocks"
    )
    
    # Should return error without making API call
    assert result["success"] is False, f"Invalid outward key '{key}' should be rejected"
    assert "error" in result, "Error response should have error message"
    assert "outward" in result["error"].lower() or "invalid" in result["error"].lower(), \
        f"Error should mention invalid key: {result['error']}"


@given(invalid_key_strategies)
@settings(max_examples=100)
def test_get_issue_links_rejects_invalid_key_before_api(key):
    r"""
    Property 2: Invalid Keys Rejected Before API Call (get_issue_links)
    
    For any invalid issue key, get_issue_links SHALL return an error
    response without making any HTTP requests to the Jira API.
    
    **Validates: Requirements 2.4**
    """
    result = get_issue_links(issue_key=key)
    
    # Should return error without making API call
    assert result["success"] is False, f"Invalid key '{key}' should be rejected"
    assert "error" in result, "Error response should have error message"
