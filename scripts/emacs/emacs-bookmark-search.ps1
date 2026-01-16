# Emacs Bookmark Search via Raycast
# Opens Emacs bookmark search frame to select and open URL bookmarks
# Keyword: 'bs' (bookmark search)
#
# Usage:
#   Via Raycast: Alt+Space -> type 'bs'
#   Via command: powershell -File emacs-bookmark-search.ps1
#
# Requirements:
#   - Emacs with modular config running (server auto-starts)
#   - emacsclient installed (via Scoop)
#   - Bookmarks in ~/org/bookmarks.org
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
    
    # 3. Call emacsclient to run bookmark search
    # Note: Uses -c -e to create frame (emacsclient creates, function configures)
    $evalCommand = "(my/global-bookmark-search)"
    $arguments = "-f", "`"$serverFile`"", "-c", "-e", "`"$evalCommand`""
    
    # Start emacsclient (opens frame, searches bookmarks, opens in browser, closes frame)
    Start-Process -FilePath $emacsClientPath `
                  -ArgumentList $arguments `
                  -NoNewWindow
    
    # Success - no output needed
    # Frame will appear, user selects bookmark, browser opens, frame closes automatically
    
} catch {
    # Show error in Windows message box
    [System.Windows.Forms.MessageBox]::Show(
        "Failed to open bookmark search:`n`n$($_.Exception.Message)",
        "Emacs Bookmark Search Error",
        [System.Windows.Forms.MessageBoxButtons]::OK,
        [System.Windows.Forms.MessageBoxIcon]::Error
    )
    exit 1
}
