# yank.ps1 — Copy input to clipboard (Windows equivalent of yank)
#
# Usage: yank.ps1 [FILE...]
#   Copies the contents of given files (or stdin) to the Windows clipboard.
#   Also emits an OSC 52 escape sequence for terminal clipboard support.

param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Files
)

# Read input from files or stdin
if ($Files -and $Files.Count -gt 0) {
    $buf = ($Files | ForEach-Object { Get-Content $_ -Raw }) -join ""
} else {
    $buf = [Console]::In.ReadToEnd()
}

# Copy to Windows clipboard
Set-Clipboard -Value $buf

# Also emit OSC 52 for terminal passthrough (e.g. SSH sessions)
$max = 74994
if ($buf.Length -gt $max) {
    Write-Warning "Input is $($buf.Length - $max) bytes too long for OSC 52"
    $buf52 = $buf.Substring(0, $max)
} else {
    $buf52 = $buf
}

$b64 = [Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($buf52))
$esc = "`e]52;c;${b64}`a"

# If inside tmux, wrap in DCS passthrough
if ($env:TMUX) {
    $esc = "`ePtmux;`e${esc}`e\"
}

[Console]::Write($esc)
