$logDir = Join-Path $env:APPDATA 'komorebi'
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
Start-Transcript -Path (Join-Path $logDir 'komorebi-launch.log') -Force | Out-Null
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
$komorebiExe  = Join-Path $env:USERPROFILE 'scoop\apps\komorebi\current\komorebi.exe'
$komorebiBar  = Join-Path $env:USERPROFILE 'scoop\apps\komorebi\current\komorebi-bar.exe'
$komorebicExe = Join-Path $env:USERPROFILE 'scoop\apps\komorebi\current\komorebic.exe'
$ahkExe       = Join-Path $env:USERPROFILE 'scoop\apps\autohotkey\current\v2\AutoHotkey.exe'
$barBaseConf  = Join-Path $komorebiConfig 'komorebi.bar.json'

# Start-Process -WindowStyle Hidden breaks komorebi's AllowSetForegroundWindow
# call, so use ShellExecute-based hidden launch via .NET ProcessStartInfo.
# ShellExecute mode disallows stream redirection — worth it since komorebi/bar
# have their own log files under $TEMP anyway.
function Start-Hidden($name, $exe, $argString, $verifySeconds = 3) {
    if ($exe -match '[\\/]' -and -not (Test-Path $exe)) { Write-Host "[$name] MISSING: $exe"; return $null }
    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName        = $exe
    $psi.Arguments       = $argString
    $psi.UseShellExecute = $true
    $psi.WindowStyle     = 'Hidden'
    $p = [System.Diagnostics.Process]::Start($psi)
    Start-Sleep -Seconds $verifySeconds
    $alive = $null -ne (Get-Process -Id $p.Id -ErrorAction SilentlyContinue)
    Write-Host "[$name] PID=$($p.Id) alive-after-${verifySeconds}s=$alive"
    if ($alive) { return $p } else { return $null }
}

# komorebi's early AllowSetForegroundWindow call can fail during Windows startup
# (subsystem not ready). Retry with backoff — first attempt uses --clean-state,
# subsequent attempts don't, so we don't repeatedly wipe recovered state.
# komorebic v0.1.41's `start --config` mis-quotes args, so launch the exe directly.
$komorebiProc = $null
$maxAttempts  = 8
for ($attempt = 1; $attempt -le $maxAttempts; $attempt++) {
    $prefix = if ($attempt -eq 1) { '--clean-state ' } else { '' }
    $komorebiProc = Start-Hidden "komorebi (attempt $attempt/$maxAttempts)" $komorebiExe "$prefix--config `"$configFile`""
    if ($komorebiProc) { break }
    Start-Sleep -Seconds 5
}

if ($komorebiProc) {
    Start-Sleep -Seconds 2  # let komorebi's socket come up before the bar attaches

    # One komorebi-bar process per monitor — the bar renders on a single monitor
    # (default index 0), so multi-monitor coverage requires N processes with N
    # per-monitor configs overriding `monitor.index`.
    $baseBar = Get-Content $barBaseConf -Raw | ConvertFrom-Json
    try {
        $state = & $komorebicExe state | ConvertFrom-Json
        $monitorCount = @($state.monitors.elements).Count
    } catch {
        Write-Host "komorebic state failed ($_); falling back to 1 bar."
        $monitorCount = 1
    }
    Write-Host "Launching bars for $monitorCount monitor(s)."

    for ($i = 0; $i -lt $monitorCount; $i++) {
        $perMonitor = $baseBar.PSObject.Copy()
        $perMonitor | Add-Member -NotePropertyName monitor -NotePropertyValue @{ index = $i } -Force
        $perMonitorPath = Join-Path $env:TEMP "komorebi.bar.$i.json"
        # Write without BOM — Set-Content -Encoding UTF8 emits a BOM that serde_json rejects.
        [IO.File]::WriteAllText($perMonitorPath, ($perMonitor | ConvertTo-Json -Depth 32), (New-Object System.Text.UTF8Encoding $false))
        Start-Hidden "komorebi-bar[$i]" $komorebiBar "--config `"$perMonitorPath`"" | Out-Null
    }
} else {
    Write-Host "[komorebi] FAILED after $maxAttempts attempts; skipping bar."
}

Start-Hidden 'autohotkey' $ahkExe "`"$ahkScript`"" | Out-Null
Start-Hidden 'watcher'    'powershell.exe' "-NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File `"$watcher`"" | Out-Null

Stop-Transcript | Out-Null
