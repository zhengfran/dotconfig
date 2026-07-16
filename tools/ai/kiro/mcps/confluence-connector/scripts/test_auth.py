#!/usr/bin/env python
"""
Minimal script to test Confluence PAT authentication.

This script verifies that your Confluence credentials are working correctly
before running the full MCP server.

Usage:
    Windows:
        set CONFLUENCE_BASE_URL=https://central.confluence.automotive.cloud
        set CONFLUENCE_PAT=your-personal-access-token
        python scripts/test_auth.py [page_id]
    
    Unix/Linux:
        export CONFLUENCE_BASE_URL=https://central.confluence.automotive.cloud
        export CONFLUENCE_PAT=your-personal-access-token
        python scripts/test_auth.py [page_id]

Arguments:
    page_id: Optional page ID to test retrieval (default: tests API connectivity only)
"""

import os
import sys
import requests


def test_auth(page_id: str = None):
    """Test Confluence PAT authentication.
    
    Args:
        page_id: Optional page ID to retrieve for testing
    """
    base_url = os.environ.get("CONFLUENCE_BASE_URL")
    pat = os.environ.get("CONFLUENCE_PAT")
    
    print("=" * 60)
    print("Confluence Authentication Test")
    print("=" * 60)
    
    # Check environment variables
    if not base_url:
        print("ERROR: CONFLUENCE_BASE_URL environment variable is not set")
        print("\nSet it using:")
        print("  Windows: set CONFLUENCE_BASE_URL=https://your-confluence-server")
        print("  Unix:    export CONFLUENCE_BASE_URL=https://your-confluence-server")
        sys.exit(1)
    
    if not pat:
        print("ERROR: CONFLUENCE_PAT environment variable is not set")
        print("\nSet it using:")
        print("  Windows: set CONFLUENCE_PAT=your-personal-access-token")
        print("  Unix:    export CONFLUENCE_PAT=your-personal-access-token")
        sys.exit(1)
    
    print(f"Base URL: {base_url}")
    print(f"PAT: {pat[:8]}...{pat[-4:] if len(pat) > 12 else '****'}")
    print()
    
    # Set up headers
    headers = {
        "Authorization": f"Bearer {pat}",
        "Content-Type": "application/json",
        "Accept": "application/json"
    }
    
    # Test 1: Check server connectivity
    print("Test 1: Server Connectivity")
    print("-" * 40)
    
    try:
        # Try to get server info or a simple endpoint
        url = f"{base_url}/rest/api/space"
        print(f"Testing: GET {url}")
        
        response = requests.get(url, headers=headers, timeout=30, params={"limit": 1})
        
        print(f"Status: {response.status_code}")
        
        if response.status_code == 200:
            print("✓ Server connectivity: OK")
            data = response.json()
            if data.get("results"):
                print(f"  Found {data.get('size', 0)} space(s)")
        elif response.status_code == 401:
            print("✗ Authentication failed: Invalid or expired PAT")
            print("  Please check your Personal Access Token")
            sys.exit(1)
        elif response.status_code == 403:
            print("✗ Access denied: Insufficient permissions")
            print("  Your PAT may not have the required permissions")
            sys.exit(1)
        else:
            print(f"✗ Unexpected status code: {response.status_code}")
            print(f"  Response: {response.text[:200]}")
            sys.exit(1)
            
    except requests.exceptions.Timeout:
        print("✗ Connection timed out")
        print("  Check your network connection and base URL")
        sys.exit(1)
    except requests.exceptions.ConnectionError as e:
        print(f"✗ Connection failed: {e}")
        print("  Check your network connection and base URL")
        sys.exit(1)
    
    print()
    
    # Test 2: Retrieve a specific page (if page_id provided)
    if page_id:
        print("Test 2: Page Retrieval")
        print("-" * 40)
        
        url = f"{base_url}/rest/api/content/{page_id}"
        params = {"expand": "body.storage,version,space"}
        
        print(f"Testing: GET {url}")
        
        try:
            response = requests.get(url, headers=headers, params=params, timeout=30)
            
            print(f"Status: {response.status_code}")
            
            if response.status_code == 200:
                data = response.json()
                print("✓ Page retrieval: OK")
                print(f"  Title: {data.get('title', 'N/A')}")
                print(f"  Space: {data.get('space', {}).get('key', 'N/A')}")
                print(f"  Version: {data.get('version', {}).get('number', 'N/A')}")
                
                body = data.get("body", {}).get("storage", {}).get("value", "")
                print(f"  Body length: {len(body)} characters")
            elif response.status_code == 404:
                print(f"✗ Page not found: {page_id}")
                print("  The page ID may be incorrect or you don't have access")
            else:
                print(f"✗ Failed to retrieve page: {response.status_code}")
                print(f"  Response: {response.text[:200]}")
                
        except Exception as e:
            print(f"✗ Error retrieving page: {e}")
    
    print()
    print("=" * 60)
    print("Authentication test completed successfully!")
    print("=" * 60)
    print()
    print("You can now run the MCP server:")
    print("  python src/server.py")
    print()


def main():
    """Main entry point."""
    page_id = sys.argv[1] if len(sys.argv) > 1 else None
    test_auth(page_id)


if __name__ == "__main__":
    main()
