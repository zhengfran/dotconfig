#!/usr/bin/env pwsh
# Ensure a herdr tab labeled "wsl-vpn" exists and runs wsl-vpnkit.
# Waits for the herdr server to come up, then creates the tab if missing.

$Label   = 'wsl-vpn'
$Command = 'wsl -d wsl-vpnkit --cd /app wsl-vpnkit'

# Wait for the herdr server (started by the interactive `herdr` in the sibling tab).
$deadline = (Get-Date).AddSeconds(30)
while ((Get-Date) -lt $deadline) {
    $status = herdr status server 2>$null
    if ($LASTEXITCODE -eq 0 -and ($status -join "`n") -match 'status:\s*running') { break }
    Start-Sleep -Milliseconds 500
}

$tabsJson = herdr tab list 2>$null | ConvertFrom-Json
if (-not $tabsJson) { exit 1 }

$existing = $tabsJson.result.tabs | Where-Object { $_.label -eq $Label }
if ($existing) { exit 0 }

$created = herdr tab create --label $Label --no-focus | ConvertFrom-Json
$tabId       = $created.result.tab.tab_id
$workspaceId = $created.result.tab.workspace_id

$panes = (herdr pane list --workspace $workspaceId | ConvertFrom-Json).result.panes
$pane  = $panes | Where-Object { $_.tab_id -eq $tabId } | Select-Object -First 1
if (-not $pane) { exit 1 }

herdr pane run $pane.pane_id $Command | Out-Null
