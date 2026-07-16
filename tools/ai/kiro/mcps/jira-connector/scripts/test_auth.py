"""
Minimal script to test Jira PAT authentication.

Usage:
    set JIRA_BASE_URL=https://jira-ibs.eu.agileci.automotive.cloud
    set JIRA_PAT=your-personal-access-token
    python scripts/test_auth.py VWICAS23-334534
"""
import os
import sys
import requests


def test_auth(ticket_key: str) -> None:
    """Test Jira PAT authentication by fetching a ticket."""
    base_url = os.environ.get("JIRA_BASE_URL")
    pat = os.environ.get("JIRA_PAT")

    if not base_url or not pat:
        print("ERROR: Set JIRA_BASE_URL and JIRA_PAT environment variables")
        sys.exit(1)

    url = f"{base_url}/rest/api/2/issue/{ticket_key}"
    headers = {"Authorization": f"Bearer {pat}"}

    print(f"Testing: GET {url}")
    
    try:
        response = requests.get(url, headers=headers, timeout=30)
        print(f"Status: {response.status_code}")
        
        if response.ok:
            data = response.json()
            print(f"Success! Ticket: {data.get('key')}")
            print(f"Summary: {data.get('fields', {}).get('summary')}")
        else:
            print(f"Error: {response.text}")
            sys.exit(1)
            
    except requests.exceptions.Timeout:
        print("ERROR: Request timed out after 30 seconds")
        sys.exit(1)
    except requests.exceptions.ConnectionError as e:
        print(f"ERROR: Failed to connect to Jira: {e}")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"ERROR: Request failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    ticket = sys.argv[1] if len(sys.argv) > 1 else "VWICAS23-334534"
    test_auth(ticket)
