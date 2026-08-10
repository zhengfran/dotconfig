$komorebiConfig = Join-Path $HOME '.config\komorebi'
$env:KOMOREBI_CONFIG_HOME = $komorebiConfig

$configFile = & (Join-Path $komorebiConfig 'select-profile.ps1')
$ahkScript  = Join-Path $komorebiConfig 'komorebi.ahk'
$watcher    = Join-Path $komorebiConfig 'watch-displays.ps1'
$stateFile  = Join-Path $env:TEMP 'komorebi-active-profile.txt'

Set-Content -Path $stateFile -Value $configFile

# Kill any lingering watcher from a previous run before starting a new one.
Get-CimInstance Win32_Process -Filter "Name='powershell.exe'" |
    Where-Object { $_.CommandLine -like "*watch-displays.ps1*" } |
    ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }

# komorebic v0.1.41's `start --config` mis-quotes args to komorebi.exe; launch the exe directly.
$komorebiExe = "$HOME\scoop\apps\komorebi\current\komorebi.exe"
Start-Process -FilePath $komorebiExe    -ArgumentList @('--clean-state', '--config', $configFile) -WindowStyle Hidden
Start-Process -FilePath 'autohotkey.exe' -ArgumentList "`"$ahkScript`""
Start-Process -FilePath 'powershell.exe' -ArgumentList @('-NoProfile', '-WindowStyle', 'Hidden', '-ExecutionPolicy', 'Bypass', '-File', $watcher) -WindowStyle Hidden
