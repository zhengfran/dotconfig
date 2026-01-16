# Emacs Bookmark Capture via Raycast
# Opens Emacs frame to capture URL bookmark with clipboard detection
# Keyword: 'bc' (bookmark capture)
#
# Usage:
#   Via Raycast: Alt+Space -> type 'bc'
#   Via command: powershell -File emacs-bookmark-capture.ps1
#   Best practice: Copy URL to clipboard before running
#
# Requirements:
#   - Emacs with modular config running (server auto-starts)
#   - emacsclient installed (via Scoop)
#   - Bookmark file: ~/org/bookmarks.org
#
# Features:
#   - Auto-detects URL from clipboard
#   - Checks for duplicate URLs
#   - Extracts domain as default description
#   - Saves to Inbox heading in bookmarks.org
#
# Author: Assistant
# Date: 2026-01-16

# Error handling
$ErrorActionPreference = "Stop"

# Load Windows Forms for message boxes
Add-Type -AssemblyName System.Windows.Forms

try {
    # 1. Check emacsclient exists
    $emacsClientPath = "C:\Users\uie41442\scoop\shims\emacsclient.exe"
    if (-not (Test-Path $emacsClientPath)) {
        throw "emacsclient.exe not found at: $emacsClientPath`n`nPlease install Emacs via Scoop:`n  scoop install emacs"
    }
    
    # 2. Check Emacs server is running
    $serverFile = "$env:USERPROFILE\.config\emacs\modular\var\server\auth\server"
    if (-not (Test-Path $serverFile)) {
        throw "Emacs server not running.`n`nPlease start Emacs first. The server starts automatically with the modular config."
    }
    
    # 3. Call emacsclient to open bookmark capture frame
    # Note: Uses -c to create frame, my/global-bookmark-capture handles frame lifecycle
    $evalCommand = "(my/global-bookmark-capture)"
    $arguments = "-f", "`"$serverFile`"", "-c", "-e", "`"$evalCommand`""
    
    # Start emacsclient (opens frame with prompts)
    Start-Process -FilePath $emacsClientPath `
                  -ArgumentList $arguments `
                  -NoNewWindow
    
    # Success - no output needed
    # Frame will appear with prompts:
    #   1. URL detection from clipboard (or prompt if not found)
    #   2. Duplicate check (aborts if URL already exists)
    #   3. Description prompt (with domain as default suggestion)
    #   4. Saves to ~/org/bookmarks.org under Inbox heading
    #   5. Frame closes automatically
    
} catch {
    # Show error in Windows message box
    [System.Windows.Forms.MessageBox]::Show(
        "Failed to open bookmark capture:`n`n$($_.Exception.Message)",
        "Emacs Bookmark Capture Error",
        [System.Windows.Forms.MessageBoxButtons]::OK,
        [System.Windows.Forms.MessageBoxIcon]::Error
    )
    exit 1
}
