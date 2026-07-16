"""
Property-based tests for Jira MCP Server.

Uses Hypothesis for property-based testing as specified in the design document.
"""
import sys
from pathlib import Path
from unittest.mock import patch

import pytest
from hypothesis import given, settings, strategies as st

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from jira_client import ToolResponse


# Strategy for generating valid Jira custom field data from API
@st.composite
def jira_custom_field_strategy(draw):
    """
    Generate mock Jira field API responses that represent custom fields.
    
    This simulates what the Jira REST API returns for custom fields.
    """
    field_num = draw(st.integers(min_value=10000, max_value=99999))
    field_id = f"customfield_{field_num}"
    
    # Generate a non-empty name (Jira always returns names for fields)
    name = draw(st.text(min_size=1, max_size=100, alphabet=st.characters(
        whitelist_categories=('L', 'N', 'P', 'S'),
        whitelist_characters=' -_'
    )).filter(lambda x: x.strip()))
    
    # Schema types that Jira uses
    schema_types = ["string", "number", "array", "option", "user", "date", "datetime", "any"]
    schema_type = draw(st.sampled_from(schema_types))
    
    return {
        "id": field_id,
        "name": name,
        "schema": {"type": schema_type}
    }


@st.composite
def jira_fields_response_strategy(draw):
    """
    Generate a list of Jira field API responses including custom fields.
    
    Simulates the response from GET /rest/api/2/field
    """
    # Generate some standard fields (non-custom)
    standard_fields = [
        {"id": "summary", "name": "Summary", "schema": {"type": "string"}},
        {"id": "description", "name": "Description", "schema": {"type": "string"}},
        {"id": "status", "name": "Status", "schema": {"type": "status"}},
    ]
    
    # Generate 0-10 custom fields
    num_custom = draw(st.integers(min_value=0, max_value=10))
    custom_fields = [draw(jira_custom_field_strategy()) for _ in range(num_custom)]
    
    return standard_fields + custom_fields


class TestFieldMappingStructure:
    """
    **Feature: jira-mcp-server, Property 3: Field mapping structure completeness**
    
    *For any* custom field returned by get_custom_field_mappings, the field object 
    SHALL contain non-empty "id", "name", and "schema_type" attributes.
    
    **Validates: Requirements 2.1**
    """

    @given(fields_response=jira_fields_response_strategy())
    @settings(max_examples=100, deadline=None)
    def test_field_mapping_structure_completeness(self, fields_response):
        """
        **Feature: jira-mcp-server, Property 3: Field mapping structure completeness**
        **Validates: Requirements 2.1**
        
        Property: For any custom field returned by get_custom_field_mappings,
        the field object SHALL contain non-empty "id", "name", and "schema_type" attributes.
        """
        # Import here to avoid issues with module loading
        from server import get_custom_field_mappings
        
        # Mock the JiraClient.get_fields to return our generated response
        mock_response = ToolResponse(success=True, data=fields_response)
        
        with patch('server.get_jira_client') as mock_get_client:
            mock_client = mock_get_client.return_value
            mock_client.get_fields.return_value = mock_response
            
            # Call the tool
            result = get_custom_field_mappings()
            
            # Verify success
            assert result["success"] is True
            
            # Verify each custom field in the result has required attributes
            for field in result.get("data", []):
                # Property: id must be non-empty
                assert "id" in field, "Field missing 'id' attribute"
                assert field["id"], "Field 'id' is empty"
                assert isinstance(field["id"], str), "Field 'id' must be a string"
                
                # Property: name must be non-empty
                assert "name" in field, "Field missing 'name' attribute"
                assert field["name"], "Field 'name' is empty"
                assert isinstance(field["name"], str), "Field 'name' must be a string"
                
                # Property: schema_type must be present (can be empty string if no schema)
                assert "schema_type" in field, "Field missing 'schema_type' attribute"
                assert isinstance(field["schema_type"], str), "Field 'schema_type' must be a string"
                
                # Verify it's actually a custom field
                assert field["id"].startswith("customfield_"), \
                    f"Non-custom field in results: {field['id']}"

