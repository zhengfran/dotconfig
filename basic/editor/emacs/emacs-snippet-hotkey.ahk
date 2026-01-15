#SingleInstance Force
#NoEnv
SetWorkingDir %A_ScriptDir%

; =============================================================================
; EMACS GLOBAL SNIPPET SEARCH
; =============================================================================
; Hotkey: Ctrl+Shift+Space
; Opens Emacs popup frame for snippet search from any Windows application
; Selected snippet is copied to clipboard for immediate pasting
; =============================================================================

; Configuration
EmacsclientPath := "C:\Users\uie41442\scoop\shims\emacsclient.exe"
EmacsPath := "C:\Users\uie41442\scoop\shims\runemacs.exe"

; Ctrl+Shift+Space - Global Snippet Search
^+Space::
{
    ; Check if Emacs server is running
    ; emacsclient returns 0 if server is running, non-zero otherwise
    RunWait, %ComSpec% /c "%EmacsclientPath%" -e "(+ 1 1)" > nul 2>&1, , Hide, ServerCheckResult
    
    ; If server not running, start Emacs daemon
    if (ErrorLevel != 0)
    {
        ToolTip, Starting Emacs daemon...
        RunWait, %EmacsPath% --daemon, , Hide
        Sleep, 3000  ; Wait for daemon to initialize
        ToolTip  ; Clear tooltip
    }
    
    ; Open snippet search in new frame
    ; -c creates new frame, -e evaluates expression
    Run, %EmacsclientPath% -c -e "(my/global-snippet-search)", , Hide
    
    return
}

; Optional: Ctrl+Shift+Alt+Space - Kill Emacs server (for debugging)
^+!Space::
{
    Run, %EmacsclientPath% -e "(kill-emacs)", , Hide
    ToolTip, Emacs server stopped
    Sleep, 1500
    ToolTip
    return
}
