# setup-packages.ps1 — Install packages on Windows via winget and scoop

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# --- winget packages (available from Microsoft Store / winget repos) ---
$wingetPackages = @(
    "Git.Git"
    "sharkdp.fd"
    "BurntSushi.ripgrep.MSVC"
    "junegunn.fzf"
    "jqlang.jq"
    "7zip.7zip"
    "ImageMagick.ImageMagick"
    "Graphviz.Graphviz"
    "FFmpeg (Gyan.dev).FFmpeg"       # ffmpeg
    "ajeetdsouza.zoxide"
    "Neovim.Neovim"
)

Write-Host "==> Installing winget packages..." -ForegroundColor Cyan
foreach ($pkg in $wingetPackages) {
    $installed = winget list --id $pkg 2>$null | Select-String $pkg
    if ($installed) {
        Write-Host "  [OK] $pkg already installed"
    } else {
        Write-Host "  [INSTALL] $pkg"
        winget install --id $pkg --accept-source-agreements --accept-package-agreements -e
    }
}

# --- scoop packages (for tools not easily available via winget) ---
# Install scoop if not present
if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
    Write-Host "==> Installing Scoop..." -ForegroundColor Cyan
    Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression
}

$scoopPackages = @(
    "yazi"
    "lazygit"
    "shfmt"
    "poppler"
)

Write-Host "==> Installing scoop packages..." -ForegroundColor Cyan
foreach ($pkg in $scoopPackages) {
    $installed = scoop list $pkg 2>$null | Select-String $pkg
    if ($installed) {
        Write-Host "  [OK] $pkg already installed"
    } else {
        Write-Host "  [INSTALL] $pkg"
        scoop install $pkg
    }
}

Write-Host ""
Write-Host "Package installation complete!" -ForegroundColor Green
