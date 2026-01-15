#Requires AutoHotkey v2.0
#SingleInstance Force
SetWorkingDir(A_ScriptDir)

; =============================================================================
; EMACS GLOBAL SNIPPET SEARCH
; =============================================================================
; Hotkey: Ctrl+Shift+Space
; Opens Emacs popup frame for snippet search from any Windows application
; Selected snippet is copied to clipboard for immediate pasting
; 
; IMPORTANT: Emacs server must be running first!
; Start Emacs with: runemacs.exe (server-start is in init.el)
; Or manually run: runemacs --daemon
; 
; NOTE: Server file location changed due to no-littering package
; =============================================================================

; Configuration
EmacsclientPath := "C:\Users\uie41442\scoop\shims\emacsclient.exe"
ServerFile := "C:\Users\uie41442\.config\emacs\default\var\server\auth\default"

; Ctrl+Shift+Space - Global Snippet Search
^+Space:: {
    ; Try to open snippet search
    ; If server not running, emacsclient will fail and we show error
    ; NOTE: -c flag creates frame, my/global-snippet-search configures and closes it
    try {
        Run(EmacsclientPath . ' -f "' . ServerFile . '" -c -e "(my/global-snippet-search)"')
    } catch {
        MsgBox("Emacs server is not running!`n`nPlease start Emacs first.`n`nThe server will auto-start when you open Emacs.", "Emacs Server Error", "Icon!")
    }
}
