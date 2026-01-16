# emacs-snippet-search.ps1
# Triggers Emacs snippet search frame via emacsclient
# For use with Raycast on Windows (keyword: ss)
#
# Requirements:
#   - Emacs server must be running (auto-starts with modular config)
#   - emacsclient.exe must be in PATH (via Scoop)
#
# Usage:
#   Raycast: Alt+Space -> type "ss" -> Enter
#   Direct: powershell -File emacs-snippet-search.ps1

# Configuration
$EmacsclientPath = "C:\Users\uie41442\scoop\shims\emacsclient.exe"
$ServerAuthFile = "C:\Users\uie41442\.config\emacs\modular\var\server\auth\server"

# Verify emacsclient exists
if (-not (Test-Path $EmacsclientPath)) {
    Write-Error "Error: emacsclient not found at: $EmacsclientPath"
    Write-Error "Please ensure Emacs is installed via Scoop"
    exit 1
}

# Verify Emacs server is running
if (-not (Test-Path $ServerAuthFile)) {
    Write-Error "Error: Emacs server is not running!"
    Write-Error "Server auth file not found at: $ServerAuthFile"
    Write-Error ""
    Write-Error "Please start Emacs with modular config:"
    Write-Error "  emacs --init-directory C:\Users\uie41442\dotconfig\basic\editor\emacs\modular"
    
    # Show Windows message box
    Add-Type -AssemblyName System.Windows.Forms
    [System.Windows.Forms.MessageBox]::Show(
        "Emacs server is not running!`n`nPlease start Emacs first.`n`nThe server will auto-start when you open Emacs.",
        "Emacs Server Error",
        [System.Windows.Forms.MessageBoxButtons]::OK,
        [System.Windows.Forms.MessageBoxIcon]::Warning
    )
    exit 1
}

# Launch emacsclient with snippet search
# -f: Specify server auth file
# -c: Create new frame
# -e: Evaluate Emacs Lisp expression
try {
    $arguments = @(
        '-f', "`"$ServerAuthFile`"",
        '-c',
        '-e', '"(my/global-snippet-search)"'
    )
    
    Start-Process -FilePath $EmacsclientPath `
                  -ArgumentList $arguments `
                  -WindowStyle Hidden `
                  -ErrorAction Stop
    
    Write-Host "Success: Emacs snippet search launched"
}
catch {
    $errorMsg = $_.Exception.Message
    Write-Error "Error launching emacsclient: $errorMsg"
    exit 1
}
