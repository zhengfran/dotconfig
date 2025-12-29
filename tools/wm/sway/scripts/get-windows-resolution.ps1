# Get primary monitor resolution from Windows (minus taskbar)
Add-Type -AssemblyName System.Windows.Forms
$screen = [System.Windows.Forms.Screen]::PrimaryScreen

# Use WorkingArea instead of Bounds to exclude taskbar
$width = $screen.WorkingArea.Width
$height = $screen.WorkingArea.Height

Write-Output "${width}x${height}"
