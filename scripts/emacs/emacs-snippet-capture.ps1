# Emacs Snippet Capture via Raycast
# Opens Emacs frame to create new yasnippet interactively
# Keyword: 'sc' (snippet capture)
#
# Usage:
#   Via Raycast: Alt+Space -> type 'sc'
#   Via command: powershell -File emacs-snippet-capture.ps1
#
# Requirements:
#   - Emacs with modular config running (server auto-starts)
#   - emacsclient installed (via Scoop)
#   - Snippet capture functions loaded (in snippets.el)
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
    
    # 3. Call emacsclient to open capture frame
    $evalCommand = "(my/global-snippet-capture)"
    $arguments = "-f", "`"$serverFile`"", "-c", "-e", "`"$evalCommand`""
    
    # Start emacsclient (opens frame, returns immediately)
    Start-Process -FilePath $emacsClientPath `
                  -ArgumentList $arguments `
                  -NoNewWindow
    
    # Success - no output needed (frame will appear)
    # Frame stays open until user presses C-c C-c (save) or C-c C-k (cancel)
    
} catch {
    # Show error in Windows message box
    [System.Windows.Forms.MessageBox]::Show(
        "Failed to open snippet capture:`n`n$($_.Exception.Message)",
        "Emacs Snippet Capture Error",
        [System.Windows.Forms.MessageBoxButtons]::OK,
        [System.Windows.Forms.MessageBoxIcon]::Error
    )
    exit 1
}
