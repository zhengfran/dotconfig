#!/usr/bin/env pwsh
# Open 3 Windows Terminal tabs, each with its own name, color, and command.

wt.exe `
    new-tab --title "windows"  --tabColor "#0000FF" powershell -NoExit -File "C:\Users\uie41442\dotconfig\scripts\windows-tab-init.ps1" `; `
    new-tab --title "wsl"      --tabColor "#00FF00" wsl zsh -ic 'herdr && exec zsh' `; `
    new-tab --title "igas116x" --tabColor "#FF0000" ssh -t uie41442@igas116x 'herdr && exec $SHELL -l'
