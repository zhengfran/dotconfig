#Requires AutoHotkey v2.0
#SingleInstance Force

; ============================================================================
; smart-launcher.ahk
;
; Two layers of bindings (independent prefixes — never conflict):
;
;   HYPER = real Ctrl + Alt + Shift + Win modifier combo.
;           CapsLock physically emits all 4 modifiers when held (caps toggle
;           disabled, Win-tap suppressed via vk07 trick on release).
;           -> smart app launcher (Hyper + X = launch / focus / cycle X)
;
;   META  = Ctrl + Alt + Shift (no Win — strictly distinct from Hyper)
;           -> window ops (snap quarters/halves/2-3, max toggle, monitor switch)
;
; SmartLaunch behavior (Hyper):
;   - app not running       -> Run(exe)
;   - 1 window              -> WinActivate
;   - N windows + focused   -> cycle to next (wraps around)
;   - N windows + unfocused -> activate most-recently-active
;
; All actions show a brief orange border flash around the target window.
;
; Add new bindings: copy a line in the appropriate section near the bottom
; and change the key + action.
; ============================================================================

; ============================================================================
; CapsLock = real Hyper key (Ctrl + Alt + Shift + Win)
;   Down  -> hold all 4 modifiers
;   Up    -> release them, but FIRST send a dummy vk07 key. This absorbs
;            the LWin "tap" (down+up with nothing in between) which would
;            otherwise trigger the Windows Start Menu.
; ============================================================================
SetCapsLockState("AlwaysOff")
*CapsLock::Send "{Blind}{LCtrl Down}{LShift Down}{LAlt Down}{LWin Down}"
*CapsLock Up::Send "{Blind}{vk07}{LCtrl Up}{LShift Up}{LAlt Up}{LWin Up}"

; ============================================================================
; SmartLaunch
;   - not running           -> Run(runCmd or exeName)
;   - 1 window              -> activate it (with flash)
;   - N windows + focused   -> cycle to next window with wrap (with flash)
;   - N windows + unfocused -> activate most-recently-active (with flash)
; ============================================================================
SmartLaunch(exeName, runCmd := "") {
    rawWindows := WinGetList("ahk_exe " exeName)

    ; CYCLE STABILITY:
    ; WinGetList returns Z-order (most-recent-first). Each WinActivate moves
    ; the activated window to the top, so naive z-order cycling re-shuffles
    ; the list every press — bouncing between only the 2 MRU windows forever.
    ;
    ; Fix: cycle in stable HWND-sorted order. Use Z-order ONLY for the
    ; initial jump (so first press from outside the app lands on MRU).
    hwndStr := ""
    for h in rawWindows
        hwndStr .= h "`n"
    sorted := Sort(RTrim(hwndStr, "`n"), "N")

    windows := []
    if (sorted != "") {
        for line in StrSplit(sorted, "`n") {
            if (line != "")
                windows.Push(Integer(line))
        }
    }

    if (windows.Length = 0) {
        Run(runCmd != "" ? runCmd : exeName)
        return
    }

    activeId := WinExist("A")
    activeIdx := 0
    Loop windows.Length {
        if (windows[A_Index] = activeId) {
            activeIdx := A_Index
            break
        }
    }

    if (activeIdx = 0)
        targetHwnd := rawWindows[1]                                ; MRU (z-order)
    else
        targetHwnd := windows[Mod(activeIdx, windows.Length) + 1]  ; sorted cycle

    WinActivate("ahk_id " targetHwnd)
    FlashBorder(targetHwnd)
}

; ============================================================================
; SmartLaunchByTitle: same launch / focus / cycle behavior as SmartLaunch but
; matches windows by an AHK WinTitle query (substring match) instead of
; ahk_exe. Mainly for WSL GUI apps via WSLg — they all share ahk_exe=msrdc.exe
; so we can't distinguish by exe. Title + ahk_class RAIL_WINDOW works:
;   RAIL_WINDOW = "Remote Application Integrated Locally" — the RDP class
;   that WSLg uses for every Linux GUI window. Combined with a title fragment
;   (e.g. "GNU Emacs"), the query uniquely matches that one Linux app.
; ============================================================================
SmartLaunchByTitle(titleQuery, runCmd) {
    prevMode := A_TitleMatchMode
    SetTitleMatchMode(2)   ; substring match for the title portion
    try {
        rawWindows := WinGetList(titleQuery)

        ; Same HWND-sorted stable cycle as SmartLaunch.
        hwndStr := ""
        for h in rawWindows
            hwndStr .= h "`n"
        sorted := Sort(RTrim(hwndStr, "`n"), "N")

        windows := []
        if (sorted != "") {
            for line in StrSplit(sorted, "`n") {
                if (line != "")
                    windows.Push(Integer(line))
            }
        }

        if (windows.Length = 0) {
            Run(runCmd)
            return
        }

        activeId := WinExist("A")
        activeIdx := 0
        Loop windows.Length {
            if (windows[A_Index] = activeId) {
                activeIdx := A_Index
                break
            }
        }

        if (activeIdx = 0)
            targetHwnd := rawWindows[1]
        else
            targetHwnd := windows[Mod(activeIdx, windows.Length) + 1]

        WinActivate("ahk_id " targetHwnd)
        FlashBorder(targetHwnd)
    } finally {
        SetTitleMatchMode(prevMode)
    }
}

; ============================================================================
; Window Ops: Snap / Maximize toggle / Move to next/prev monitor
; ============================================================================

; Snap window into a fractional rect of its current monitor's work area.
; Fractions in [0, 1]: e.g. (0, 0, 0.5, 0.5) = top-left quarter.
SnapWindow(hwnd, fracX, fracY, fracW, fracH) {
    if (!hwnd)
        return
    ; Restore first — WinMove silently no-ops on maximized windows
    if (WinGetMinMax("ahk_id " hwnd) = 1)
        WinRestore("ahk_id " hwnd)

    WinGetPos(&wx, &wy, &ww, &wh, "ahk_id " hwnd)
    mon := GetWorkAreaForPoint(wx + ww // 2, wy + wh // 2)
    mw := mon.right - mon.left
    mh := mon.bottom - mon.top

    ; Target VISIBLE rect (where the user expects the window edges to land).
    visX := mon.left + Round(mw * fracX)
    visY := mon.top  + Round(mh * fracY)
    visW := Round(mw * fracW)
    visH := Round(mh * fracH)

    ; Compensate for the ghost frame: WinGetPos / WinMove operate on the
    ; "outer" rect which on Win10+ includes the invisible drop shadow (~8px
    ; on left/right/bottom). DWM gives the visible rect; the diff per-side
    ; is the padding we add back when calling WinMove so the user's visible
    ; window edges fall exactly where we want.
    ;
    ; SKIP for RAIL_WINDOW (WSLg) — the RDP proxy doesn't follow native
    ; Win frame conventions. Our DWM-based compensation produces wildly
    ; wrong values for them, AND WinMove itself often doesn't fully
    ; propagate to the Linux side anyway. Plain WinMove gets us as close
    ; as possible; the Linux app may still not resize beyond what its WM
    ; agrees to.
    skipCompensation := WinGetClass("ahk_id " hwnd) = "RAIL_WINDOW"

    if (!skipCompensation && GetVisibleRect(hwnd, &vx, &vy, &vw, &vh)) {
        leftPad   := vx - wx
        topPad    := vy - wy
        rightPad  := (wx + ww) - (vx + vw)
        bottomPad := (wy + wh) - (vy + vh)
        WinMove(
            visX - leftPad,
            visY - topPad,
            visW + leftPad + rightPad,
            visH + topPad + bottomPad,
            "ahk_id " hwnd
        )
    } else if (skipCompensation) {
        ; RAIL_WINDOW: Win-side container resizes immediately, but the
        ; Linux side (GTK / X server) often doesn't follow — leaves the
        ; app rendered at its old size with surrounding black inside the
        ; container. Aggressive sequence to push the resize through:
        ;   1. WinMove sets the Win container size
        ;   2. SetWindowPos with SWP_FRAMECHANGED triggers WM_NCCALCSIZE
        ;      which can force the RAIL agent to re-sync with the X server
        ;   3. Brief sleep so async events can flow
        ;   4. Second WinMove catches any silent rejection
        WinMove(visX, visY, visW, visH, "ahk_id " hwnd)
        DllCall("SetWindowPos",
            "Ptr", hwnd, "Ptr", 0,
            "Int", visX, "Int", visY, "Int", visW, "Int", visH,
            "UInt", 0x0034)   ; SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED
        Sleep(80)
        WinMove(visX, visY, visW, visH, "ahk_id " hwnd)
    } else {
        ; DWM failed (rare) — direct WinMove without compensation.
        WinMove(visX, visY, visW, visH, "ahk_id " hwnd)
    }

    FlashBorder(hwnd)
}

SnapActive(fracX, fracY, fracW, fracH) {
    SnapWindow(WinExist("A"), fracX, fracY, fracW, fracH)
}

; Maximize <-> Restore toggle. Press Meta+M once to maximize, again to restore.
MaxWindow(hwnd) {
    if (!hwnd)
        return
    if (WinGetMinMax("ahk_id " hwnd) = 1)
        WinRestore("ahk_id " hwnd)
    else
        WinMaximize("ahk_id " hwnd)
    FlashBorder(hwnd)
}

MaxActive() {
    MaxWindow(WinExist("A"))
}

; Move window to prev/next monitor (direction = -1 or +1).
; Preserves relative position/size proportionally; if maximized, stays maximized.
MoveToMonitor(hwnd, direction) {
    if (!hwnd)
        return
    count := MonitorGetCount()
    if (count <= 1) {
        FlashBorder(hwnd)   ; single-monitor: no-op but acknowledge
        return
    }

    WinGetPos(&wx, &wy, &ww, &wh, "ahk_id " hwnd)
    cur := GetWorkAreaForPoint(wx + ww // 2, wy + wh // 2)
    targetIdx := Mod(cur.idx - 1 + direction + count, count) + 1
    MonitorGetWorkArea(targetIdx, &tLeft, &tTop, &tRight, &tBottom)

    wasMax := WinGetMinMax("ahk_id " hwnd) = 1
    if (wasMax) {
        WinRestore("ahk_id " hwnd)
        WinMove(tLeft, tTop, tRight - tLeft, tBottom - tTop, "ahk_id " hwnd)
        WinMaximize("ahk_id " hwnd)
    } else {
        curW := cur.right - cur.left
        curH := cur.bottom - cur.top
        targetW := tRight - tLeft
        targetH := tBottom - tTop
        relX := (wx - cur.left) / curW
        relY := (wy - cur.top)  / curH
        relW := ww / curW
        relH := wh / curH
        WinMove(
            tLeft + Round(relX * targetW),
            tTop  + Round(relY * targetH),
            Round(relW * targetW),
            Round(relH * targetH),
            "ahk_id " hwnd
        )
    }

    FlashBorder(hwnd)
}

MoveActiveToMonitor(direction) {
    MoveToMonitor(WinExist("A"), direction)
}

; ============================================================================
; FlashBorder: brief colored border as visual feedback after activation.
; Renders 4 thin GUIs (top/bottom/left/right) at the window edges, then
; destroys them after `duration` ms via a one-shot timer.
; ============================================================================
FlashBorder(hwnd, color := "FFA500", thickness := 6, duration := 250) {
    if (!WinExist("ahk_id " hwnd))
        return

    WinGetPos(&x, &y, &w, &h, "ahk_id " hwnd)
    mon := GetWorkAreaForPoint(x + w // 2, y + h // 2)

    ; Clip to monitor work area. Maximized windows on Win10/11 have a rect
    ; that extends ~8px beyond the visible screen on every side (hidden
    ; resize border), so unclipped borders draw entirely off-screen.
    right  := Min(x + w, mon.right)
    bottom := Min(y + h, mon.bottom)
    x := Max(x, mon.left)
    y := Max(y, mon.top)
    w := right - x
    h := bottom - y

    if (w <= 0 || h <= 0)
        return

    borders := []
    coords := [
        [x,                 y,                 w,         thickness],   ; top
        [x,                 y + h - thickness, w,         thickness],   ; bottom
        [x,                 y,                 thickness, h],           ; left
        [x + w - thickness, y,                 thickness, h]            ; right
    ]

    for c in coords {
        g := Gui("+AlwaysOnTop -Caption +ToolWindow")
        g.BackColor := color
        g.Show("x" c[1] " y" c[2] " w" c[3] " h" c[4] " NoActivate")
        borders.Push(g)
    }

    SetTimer(DestroyBorders.Bind(borders), -duration)
}

DestroyBorders(arr, *) {
    for g in arr {
        try g.Destroy()
    }
}

; ============================================================================
; GetWorkAreaForPoint: return the monitor work-area rect (taskbar excluded)
; containing point (cx, cy). Falls back to primary monitor.
; Used by FlashBorder + SnapWindow + MoveToMonitor.
; ============================================================================
GetWorkAreaForPoint(cx, cy) {
    Loop MonitorGetCount() {
        MonitorGetWorkArea(A_Index, &mL, &mT, &mR, &mB)
        if (cx >= mL && cx < mR && cy >= mT && cy < mB)
            return { left: mL, top: mT, right: mR, bottom: mB, idx: A_Index }
    }
    pri := MonitorGetPrimary()
    MonitorGetWorkArea(pri, &mL, &mT, &mR, &mB)
    return { left: mL, top: mT, right: mR, bottom: mB, idx: pri }
}

; ============================================================================
; GetVisibleRect: actual VISIBLE rect of a window (excludes the invisible
; drop-shadow / ghost frame that Win10+ adds, typically ~8px each side).
; SnapWindow uses this to compensate so WinMove lands the visible edges
; exactly on the requested rect — without it WSLg / RAIL_WINDOW snaps look
; offset and "not aligned".
; Returns true on success, false if DWM call fails.
; ============================================================================
GetVisibleRect(hwnd, &x, &y, &w, &h) {
    rect := Buffer(16, 0)
    ; DWMWA_EXTENDED_FRAME_BOUNDS = 9
    if (DllCall("dwmapi\DwmGetWindowAttribute",
            "Ptr", hwnd, "UInt", 9, "Ptr", rect, "UInt", 16, "UInt") != 0)
        return false
    x := NumGet(rect, 0, "Int")
    y := NumGet(rect, 4, "Int")
    r := NumGet(rect, 8, "Int")
    b := NumGet(rect, 12, "Int")
    w := r - x
    h := b - y
    return true
}

; ============================================================================
; Hyper bindings (Ctrl+Alt+Shift+Win + X) — Smart launcher
; Physically: hold CapsLock then press X (CapsLock emits the 4 modifiers).
; Hotkey syntax: ^=Ctrl  !=Alt  +=Shift  #=Win  →  ^!+# = Hyper
; If launch fails, run AHK Window Spy on the running app to verify exe name.
; ============================================================================

^!+#o::SmartLaunch("Obsidian.exe")                               ; Obsidian
^!+#t::SmartLaunch("WindowsTerminal.exe", "wt.exe")              ; Windows Terminal
^!+#v::SmartLaunch("Code.exe")                                   ; VS Code
^!+#m::SmartLaunch("olk.exe")                                    ; Outlook (new); classic = "OUTLOOK.EXE"
^!+#c::SmartLaunch("ms-teams.exe")                               ; Teams (new); classic = "Teams.exe"
^!+#z::SmartLaunch("zotero.exe")                                 ; Zotero
^!+#a::SmartLaunch("ChatGPT.exe")                                ; ChatGPT desktop
^!+#b::SmartLaunch("Arc.exe")                                    ; Arc browser
^!+#e::SmartLaunch("msedge.exe")                                 ; Microsoft Edge
^!+#g::SmartLaunch("project-graph.exe", EnvGet("USERPROFILE") "\scoop\apps\project-graph\current\project-graph.exe")  ; project-graph (scoop)
^!+#x::SmartLaunchByTitle("GNU Emacs ahk_class RAIL_WINDOW", "wsl emacs")  ; WSL Emacs (Ubuntu-24.04)

; ============================================================================
; Meta bindings (Ctrl+Shift+Alt + X) — Window ops
;   Quarters / halves / 2/3 strips / maximize toggle / monitor switch
; ============================================================================

; Quarters (1/4 of work area)
^!+q::SnapActive(0,    0,    0.5, 0.5)        ; top-left
^!+w::SnapActive(0.5,  0,    0.5, 0.5)        ; top-right
^!+a::SnapActive(0,    0.5,  0.5, 0.5)        ; bottom-left
^!+s::SnapActive(0.5,  0.5,  0.5, 0.5)        ; bottom-right

; Two-thirds (vertical strips, full height)
^!+y::SnapActive(0,    0,    2/3, 1)          ; left 2/3
^!+u::SnapActive(1/6,  0,    2/3, 1)          ; center 2/3
^!+i::SnapActive(1/3,  0,    2/3, 1)          ; right 2/3

; Halves
^!+h::SnapActive(0,    0,    0.5, 1)          ; left half
^!+l::SnapActive(0.5,  0,    0.5, 1)          ; right half
^!+j::SnapActive(0,    0,    1,   0.5)        ; top half
^!+k::SnapActive(0,    0.5,  1,   0.5)        ; bottom half

; Maximize toggle (M -> max, M again -> restore)
^!+m::MaxActive()

; Monitor switching
^!+p::MoveActiveToMonitor(-1)                 ; previous monitor
^!+n::MoveActiveToMonitor(1)                  ; next monitor
