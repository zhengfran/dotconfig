$log = Join-Path $env:APPDATA 'komorebi\komorebi-launch.log'
New-Item -ItemType Directory -Force -Path (Split-Path $log) | Out-Null
Start-Transcript -Path $log -Force | Out-Null
trap { Write-Host "ERROR: $_"; Stop-Transcript | Out-Null; continue }

$komorebiConfig = Join-Path $HOME '.config\komorebi'
$env:KOMOREBI_CONFIG_HOME = $komorebiConfig

# WMI monitor enumeration can return empty right after login; give it a beat.
Start-Sleep -Seconds 2

$configFile = & (Join-Path $komorebiConfig 'select-profile.ps1')
$ahkScript  = Join-Path $komorebiConfig 'komorebi.ahk'
$watcher    = Join-Path $komorebiConfig 'watch-displays.ps1'
$stateFile  = Join-Path $env:TEMP 'komorebi-active-profile.txt'
Set-Content -Path $stateFile -Value $configFile
Write-Host "Selected profile: $configFile"

# Kill any lingering processes from a previous session.
Get-Process komorebi,komorebi-bar -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
Get-CimInstance Win32_Process -Filter "Name='powershell.exe'" |
    Where-Object { $_.CommandLine -like "*watch-displays.ps1*" } |
    ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }

# Full paths — scoop shims may not be on PATH at first-login Startup.
$komorebiExe = "$HOME\scoop\apps\komorebi\current\komorebi.exe"
$komorebiBar = "$HOME\scoop\apps\komorebi\current\komorebi-bar.exe"
$ahkExe      = "$HOME\scoop\apps\autohotkey\current\v2\AutoHotkey.exe"

# komorebic v0.1.41's `start --config` mis-quotes args to komorebi.exe; launch the exe directly.
Start-Process -FilePath $komorebiExe -ArgumentList @('--clean-state', '--config', $configFile) -WindowStyle Hidden
Start-Sleep -Seconds 2  # let komorebi's socket come up before the bar attaches
Start-Process -FilePath $komorebiBar -WindowStyle Hidden
Start-Process -FilePath $ahkExe      -ArgumentList "`"$ahkScript`""
Start-Process -FilePath 'powershell.exe' -ArgumentList @('-NoProfile', '-WindowStyle', 'Hidden', '-ExecutionPolicy', 'Bypass', '-File', $watcher) -WindowStyle Hidden

Stop-Transcript | Out-Null
