$profilesDir = Join-Path $PSScriptRoot 'profiles'
$fallback = Join-Path $profilesDir 'laptop-only.json'

$connected = @(Get-CimInstance -Namespace root/wmi -ClassName WmiMonitorID -ErrorAction SilentlyContinue | ForEach-Object {
    $m = [System.Text.Encoding]::ASCII.GetString($_.ManufacturerName).TrimEnd([char]0)
    $p = [System.Text.Encoding]::ASCII.GetString($_.ProductCodeID).TrimEnd([char]0)
    "$m$p"
})

$best = $null
$bestScore = -1
Get-ChildItem $profilesDir -Filter '*.json' | ForEach-Object {
    $cfg = Get-Content $_.FullName -Raw | ConvertFrom-Json
    $expected = @()
    if ($cfg.display_index_preferences) {
        $cfg.display_index_preferences.PSObject.Properties | ForEach-Object {
            $expected += ($_.Value -split '-')[0]
        }
    }
    $missing = @($expected | Where-Object { $connected -notcontains $_ })
    if ($missing.Count -eq 0 -and $expected.Count -gt $bestScore) {
        $bestScore = $expected.Count
        $best = $_.FullName
    }
}

if ($best) { Write-Output $best } else { Write-Output $fallback }
