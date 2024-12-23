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
