# setup-config.ps1 — Symlink dotconfig files on Windows
# Run as Administrator (needed for New-Item -ItemType SymbolicLink on some systems)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

Write-Host "[INFO] Starting setup-config.ps1..." -ForegroundColor Cyan

$dotconfig = "$HOME\dotconfig"

# Clone if not present
if (-not (Test-Path $dotconfig)) {
    Write-Host "[INFO] Cloning dotconfig repository..."
    git clone "git@github.com:zhengfran/dotconfig.git" $dotconfig
} else {
    Write-Host "[INFO] dotconfig directory already exists."
}

# Helper: create a symlink if the target doesn't already exist
function New-ConfigLink {
    param(
        [string]$Source,
        [string]$Target,
        [switch]$IsDirectory
    )

    if (-not (Test-Path $Source)) {
        Write-Host "[WARN] Source not found: $Source"
        return
    }
    if (Test-Path $Target) {
        Write-Host "[INFO] $Target already exists. Skipping."
        return
    }

    # Ensure parent directory exists
    $parent = Split-Path $Target -Parent
    if (-not (Test-Path $parent)) {
        New-Item -ItemType Directory -Path $parent -Force | Out-Null
    }

    if ($IsDirectory) {
        New-Item -ItemType SymbolicLink -Path $Target -Target $Source | Out-Null
    } else {
        New-Item -ItemType SymbolicLink -Path $Target -Target $Source | Out-Null
    }
    Write-Host "[INFO] Symlinked: $Source -> $Target"
}

# --- PowerShell profile ---
$pwshProfile = "$dotconfig\basic\shell\powershell\Microsoft.PowerShell_profile.ps1"
if (Test-Path $pwshProfile) {
    $profileDir = Split-Path $PROFILE -Parent
    New-ConfigLink -Source $pwshProfile -Target $PROFILE
} else {
    Write-Host "[WARN] PowerShell profile not found in dotconfig."
}

# --- yazi ---
New-ConfigLink -Source "$dotconfig\tools\yazi" `
               -Target "$env:APPDATA\yazi\config" -IsDirectory

# --- emacs ---
# chemacs2
if (-not (Test-Path "$HOME\.emacs.d")) {
    Write-Host "[INFO] Cloning chemacs2 into ~/.emacs.d..."
    git clone --depth 1 "https://github.com/plexus/chemacs2.git" "$HOME\.emacs.d"
} else {
    Write-Host "[INFO] ~/.emacs.d already exists. Skipping chemacs2 clone."
}

New-ConfigLink -Source "$dotconfig\basic\editor\emacs" `
               -Target "$HOME\.config\emacs" -IsDirectory

$emacsProfiles = "$dotconfig\basic\editor\emacs\chemacs\.emacs-profiles.el"
if (Test-Path $emacsProfiles) {
    New-ConfigLink -Source $emacsProfiles -Target "$HOME\.emacs-profiles.el"
}

# --- nvim (LazyVim starter) ---
$nvimTarget = "$env:LOCALAPPDATA\nvim"
if (-not (Test-Path $nvimTarget)) {
    Write-Host "[INFO] Cloning LazyVim starter into $nvimTarget..."
    git clone "https://github.com/LazyVim/starter" $nvimTarget
} else {
    Write-Host "[INFO] $nvimTarget already exists. Skipping."
}

# --- AI tools ---
# opencode
New-ConfigLink -Source "$dotconfig\tools\ai\opencode" `
               -Target "$HOME\.config\opencode" -IsDirectory

# agents
New-ConfigLink -Source "$dotconfig\tools\ai\agents" `
               -Target "$HOME\.agents" -IsDirectory

# Claude Code settings
$claudeSettings = "$dotconfig\tools\ai\claude\settings.json"
if (Test-Path $claudeSettings) {
    $claudeDir = "$HOME\.claude"
    if (-not (Test-Path $claudeDir)) {
        New-Item -ItemType Directory -Path $claudeDir -Force | Out-Null
    }
    New-ConfigLink -Source $claudeSettings -Target "$claudeDir\settings.json"
}

# Claude Code skills
$claudeSkills = "$dotconfig\tools\ai\agents\skills"
if (Test-Path $claudeSkills) {
    $claudeDir = "$HOME\.claude"
    if (-not (Test-Path $claudeDir)) {
        New-Item -ItemType Directory -Path $claudeDir -Force | Out-Null
    }
    New-ConfigLink -Source $claudeSkills -Target "$claudeDir\skills" -IsDirectory
}

# --- common shell config ---
New-ConfigLink -Source "$dotconfig\basic\shell\common" `
               -Target "$HOME\.config\common" -IsDirectory

Write-Host "[INFO] setup-config.ps1 completed." -ForegroundColor Cyan
