Import-Module ZLocation

# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Autocompletion for arrow keys
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# auto suggestions
Import-Module PSReadLine
Set-PSReadLineOption -PredictionSource History

Set-Alias -Name ll -Value ls
Set-Alias -Name lg -Value lazygit
Set-Alias -Name c -Value clear
Set-Alias -Name p -Value pwd
Set-Alias -Name nm -Value neovim
Set-Alias -Name vi -Value neovide

# 让powershell 支持linux 下的快捷键
if ($host.Name -eq 'ConsoleHost')
{
    Import-Module PSReadLine
    Set-PSReadLineOption -EditMode Emacs
}

function y {
    $tmp = [System.IO.Path]::GetTempFileName()
    yazi $args --cwd-file="$tmp"
    $cwd = Get-Content -Path $tmp
    if (-not [String]::IsNullOrEmpty($cwd) -and $cwd -ne $PWD.Path) {
        Set-Location -LiteralPath $cwd
    }
    Remove-Item -Path $tmp
}

# for komorebic
$Env:KOMOREBI_CONFIG_HOME = Join-Path -Path $HOME -ChildPath '.config\komorebi'
function Start-OrSwitch-KomorebiWorkspace {
    param(
        [Parameter(Mandatory=$true)]
        [string]$ProcessName,

        [Parameter(Mandatory=$false)]
        [int]$MonitorId = $null
    )

    # Get the current state of komorebi
    $state = komorebic state | ConvertFrom-Json
    
    # Variables to store workspace and monitor info
    $workspaceId = $null
    $monitorId = $null

    # Loop through monitors and workspaces to find the running process
    foreach ($monitor in $state.monitors.elements) {
        foreach ($workspace in $monitor.workspaces.elements) {
            Write-Host "$workspace"
            foreach ($container in $workspace.containers.elements) {
                foreach ($window in $container.windows.elements) {
                    if ($window.exe -ieq $ProcessName) {
                        
                        $workspaceId = $workspace.name
                        $monitorId = $monitor.id
                        break
                    }
                }
                if ($workspaceId -ne $null) { break }
            }
            if ($workspaceId -ne $null) { break }
        }
        if ($workspaceId -ne $null) { break }
    }

    Write-Host "$workspaceId"
    if ($workspaceId -ne $null) {
        Write-Host "$ProcessName is already running on Workspace $workspaceId on Monitor $monitorId."
        Write-Host "Switching to workspace $workspaceId on Monitor $monitorId..."
        
        # Focus the monitor first, then the workspace
        komorebic focus-monitor $monitorId
        Start-Sleep -Milliseconds 300
        komorebic focus-workspace $workspaceId
    }
    else {
        Write-Host "$ProcessName is not running. Starting a new instance..."
        
        # If MonitorId is provided, focus that monitor
        if ($MonitorId -ne $null) {
            Write-Host "Focusing on Monitor $MonitorId..."
            komorebic focus-monitor $MonitorId
            Start-Sleep -Milliseconds 300
        }
        else {
            Write-Host "No MonitorId provided. Using the currently focused monitor."
        }

        # Cycle to the next workspace on the focused monitor
        komorebic new-workspace
        
        # Start the process in the new workspace on the specified or focused monitor
        Start-Process $ProcessName
        Write-Host "New instance of $ProcessName started in the new workspace."
    }
}



#zoxide init
Invoke-Expression (& { (zoxide init powershell | Out-String) })