$logDir = Join-Path $env:APPDATA 'komorebi'
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
$log       = Join-Path $logDir 'komorebi-launch.log'
$komErrLog = Join-Path $logDir 'komorebi.stderr.log'
$barErrLog = Join-Path $logDir 'komorebi-bar.stderr.log'
$komOutLog = Join-Path $logDir 'komorebi.stdout.log'
$barOutLog = Join-Path $logDir 'komorebi-bar.stdout.log'
Start-Transcript -Path $log -Force | Out-Null
trap { Write-Host "ERROR: $_"; Stop-Transcript | Out-Null; continue }

Write-Host "Boot uptime (min): $(((Get-Date) - (Get-CimInstance Win32_OperatingSystem).LastBootUpTime).TotalMinutes)"

$komorebiConfig = Join-Path $env:USERPROFILE '.config\komorebi'
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
$komorebiExe = Join-Path $env:USERPROFILE 'scoop\apps\komorebi\current\komorebi.exe'
$komorebiBar = Join-Path $env:USERPROFILE 'scoop\apps\komorebi\current\komorebi-bar.exe'
$ahkExe      = Join-Path $env:USERPROFILE 'scoop\apps\autohotkey\current\v2\AutoHotkey.exe'

function Start-AndVerify($name, $exe, $args, $errLog, $outLog) {
    if ($exe -match '[\\/]' -and -not (Test-Path $exe)) { Write-Host "[$name] MISSING: $exe"; return }
    $sp = @{ FilePath = $exe; PassThru = $true; WindowStyle = 'Hidden' }
    if ($args) { $sp['ArgumentList'] = $args }
    if ($errLog) { $sp['RedirectStandardError']  = $errLog }
    if ($outLog) { $sp['RedirectStandardOutput'] = $outLog }
    $p = Start-Process @sp
    Start-Sleep -Seconds 3
    $alive = $null -ne (Get-Process -Id $p.Id -ErrorAction SilentlyContinue)
    Write-Host "[$name] PID=$($p.Id) alive-after-3s=$alive"
}

# komorebic v0.1.41's `start --config` mis-quotes args to komorebi.exe; launch the exe directly.
Start-AndVerify 'komorebi'     $komorebiExe @('--clean-state', '--config', $configFile) $komErrLog $komOutLog
Start-Sleep -Seconds 2  # let komorebi's socket come up before the bar attaches
Start-AndVerify 'komorebi-bar' $komorebiBar $null $barErrLog $barOutLog
Start-AndVerify 'autohotkey'   $ahkExe      @("`"$ahkScript`"") $null $null
Start-AndVerify 'watcher'      'powershell.exe' @('-NoProfile', '-WindowStyle', 'Hidden', '-ExecutionPolicy', 'Bypass', '-File', $watcher) $null $null

Stop-Transcript | Out-Null
