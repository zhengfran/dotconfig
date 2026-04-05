# setup.ps1 — Full setup for Windows (symlink configs + install packages)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "Setting up configuration files..."
& "$ScriptDir\setup-config.ps1"

Write-Host ""
Write-Host "Installing packages..."
& "$ScriptDir\setup-packages.ps1"

Write-Host ""
Write-Host "Setup complete!" -ForegroundColor Green
