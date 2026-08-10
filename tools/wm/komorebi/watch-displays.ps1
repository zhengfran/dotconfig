Add-Type -AssemblyName System.Windows.Forms

$script:configDir = Join-Path $HOME '.config\komorebi'
$script:selector  = Join-Path $script:configDir 'select-profile.ps1'
$script:stateFile = Join-Path $env:TEMP 'komorebi-active-profile.txt'
$script:komorebic = "$HOME\scoop\shims\komorebic.exe"

function Apply-Profile {
    $chosen = & $script:selector
    if (-not $chosen -or -not (Test-Path $chosen)) { return }
    $last = ''
    if (Test-Path $script:stateFile) { $last = (Get-Content $script:stateFile -Raw).Trim() }
    if ($chosen -eq $last) { return }
    & $script:komorebic replace-configuration $chosen | Out-Null
    Set-Content -Path $script:stateFile -Value $chosen
}

# WMI monitor enumeration lags a bit behind DisplaySettingsChanged on some docks;
# 2s debounce catches the settle-down after a hotplug burst.
$script:debounce = New-Object System.Windows.Forms.Timer
$script:debounce.Interval = 2000
$script:debounce.Add_Tick({ $script:debounce.Stop(); Apply-Profile })

[Microsoft.Win32.SystemEvents]::add_DisplaySettingsChanged({
    $script:debounce.Stop()
    $script:debounce.Start()
})

[System.Windows.Forms.Application]::Run()
