#!/usr/bin/env python3
"""Debug script to test the createmeta API endpoint."""
import os
import sys
import json

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from jira_client import JiraConfig, JiraClient

def main():
    config = JiraConfig.from_environment()
    client = JiraClient(config)
    
    project_key = "DGR"
    issue_type = "Story"
    
    print(f"Base URL: {config.base_url}")
    print()
    
    # Test get_create_metadata
    print("=" * 60)
    print("Test: get_create_metadata")
    result = client.get_create_metadata(project_key, issue_type)
    print(f"Success: {result.success}")
    if result.success:
        data = result.data
        print(f"Project: {data.get('project')}")
        print(f"Issue Type: {data.get('issue_type')}")
        print(f"Fields count: {len(data.get('fields', {}))}")
        
        # Show some fields
        fields = data.get('fields', {})
        print("\nSample fields:")
        for i, (fid, fdata) in enumerate(list(fields.items())[:10]):
            print(f"  {fid}: {fdata.get('name')} (required: {fdata.get('required')}, has_values: {fdata.get('has_allowed_values')})")
    else:
        print(f"Error: {result.error}")
    
    print()
    
    # Test get_field_options for Responsible Team
    print("=" * 60)
    print("Test: get_field_options for 'Responsible Team'")
    result2 = client.get_field_options(project_key, issue_type, "Responsible Team")
    print(f"Success: {result2.success}")
    if result2.success:
        data2 = result2.data
        print(f"Field ID: {data2.get('field_id')}")
        print(f"Field Name: {data2.get('field_name')}")
        print(f"Required: {data2.get('required')}")
        print(f"Allowed values count: {len(data2.get('allowed_values', []))}")
        
        # Show some values
        values = data2.get('allowed_values', [])
        print("\nSample values (looking for 'Foo'):")
        for v in values:
            name = v.get('value') or v.get('name') or ''
            if 'foo' in name.lower() or 'muppet' in name.lower():
                print(f"  id={v.get('id')}, value={name}")
    else:
        print(f"Error: {result2.error}")

if __name__ == "__main__":
    main()
