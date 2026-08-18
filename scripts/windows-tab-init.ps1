#!/usr/bin/env pwsh
# Windows tab init: start herdr server, then launch the VPN helper in background.

Start-Process powershell -WindowStyle Hidden -ArgumentList `
    '-NoProfile', '-File', "$PSScriptRoot\ensure-herdr-wsl-vpn.ps1"

herdr
