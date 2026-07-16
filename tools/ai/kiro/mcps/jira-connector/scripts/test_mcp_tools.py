"""
Quick test script to verify MCP server tools work with real Jira data.

Usage:
    set JIRA_BASE_URL=https://jira-ibs.eu.agileci.automotive.cloud
    set JIRA_PAT=your-personal-access-token
    python scripts/test_mcp_tools.py VWICAS23-334534
"""
import json
import os
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from server import get_ticket_by_key, get_custom_field_mappings


def main():
    # Check environment
    base_url = os.environ.get("JIRA_BASE_URL")
    pat = os.environ.get("JIRA_PAT")
    
    if not base_url or not pat:
        print("ERROR: Set JIRA_BASE_URL and JIRA_PAT environment variables")
        print("\nExample:")
        print("  set JIRA_BASE_URL=https://jira-ibs.eu.agileci.automotive.cloud")
        print("  set JIRA_PAT=your-token-here")
        sys.exit(1)
    
    print(f"Using Jira: {base_url}")
    print("-" * 60)
    
    # Get ticket key from args or use default
    ticket_key = sys.argv[1] if len(sys.argv) > 1 else "VWICAS23-334534"
    
    # Test 1: Get ticket by key
    print(f"\n1. Testing get_ticket_by_key('{ticket_key}')...")
    result = get_ticket_by_key(ticket_key)
    
    if result["success"]:
        data = result["data"]
        print(f"   ✓ Success!")
        print(f"   Key: {data.get('key')}")
        fields = data.get("fields", {})
        print(f"   Summary: {fields.get('summary', 'N/A')}")
        print(f"   Status: {fields.get('status', {}).get('name', 'N/A')}")
        print(f"   Issue Type: {fields.get('issuetype', {}).get('name', 'N/A')}")
        
        # Count custom fields
        custom_fields = [k for k in fields.keys() if k.startswith("customfield_")]
        print(f"   Custom fields found: {len(custom_fields)}")
    else:
        print(f"   ✗ Error: {result.get('error')}")
    
    # Test 2: Get custom field mappings
    print(f"\n2. Testing get_custom_field_mappings()...")
    result = get_custom_field_mappings()
    
    if result["success"]:
        mappings = result["data"]
        print(f"   ✓ Success! Found {len(mappings)} custom fields")
        
        # Show first 5 mappings
        if mappings:
            print("   Sample mappings:")
            for field in mappings[:5]:
                print(f"     - {field['id']}: {field['name']} ({field['schema_type']})")
            if len(mappings) > 5:
                print(f"     ... and {len(mappings) - 5} more")
    else:
        print(f"   ✗ Error: {result.get('error')}")
    
    print("\n" + "-" * 60)
    print("Test complete!")


if __name__ == "__main__":
    main()
