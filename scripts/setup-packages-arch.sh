#!/usr/bin/env bash

set -e

echo "Setting up packages for Arch Linux/MSYS2..."

# Check if running on MSYS2 or Arch Linux
if [[ -f /etc/os-release ]]; then
	. /etc/os-release
	IS_MSYS2=false
	[[ "$ID" == "msys" ]] && IS_MSYS2=true
else
	IS_MSYS2=true
fi

# Install paru if not present (AUR helper)
if ! command -v paru >/dev/null 2>&1 && [[ "$IS_MSYS2" == false ]]; then
	echo "Installing paru (AUR helper)..."
	sudo pacman -S --needed --noconfirm base-devel git
	git clone https://aur.archlinux.org/paru.git /tmp/paru
	cd /tmp/paru
	makepkg -si --noconfirm
	cd -
	rm -rf /tmp/paru
fi

# Core packages available in official repos
PACMAN_PACKAGES=(
	git
	zsh
	ffmpeg
	p7zip
	jq
	poppler
	zoxide
	imagemagick
	graphviz
	fzf
	fd
	ripgrep
	neovim
	lazygit
	shfmt
)

# MSYS2 specific packages
if [[ "$IS_MSYS2" == true ]]; then
	echo "Installing MSYS2 packages..."
	pacman -S --needed --noconfirm "${PACMAN_PACKAGES[@]}"
else
	echo "Installing Arch Linux packages..."
	sudo pacman -S --needed --noconfirm "${PACMAN_PACKAGES[@]}"

	# wl-clipboard for Wayland (Arch Linux only)
	sudo pacman -S --needed --noconfirm wl-clipboard

	# zenity (GUI dialogs)
	sudo pacman -S --needed --noconfirm zenity
fi

# AUR packages (Arch Linux only, using paru)
if [[ "$IS_MSYS2" == false ]] && command -v paru >/dev/null 2>&1; then
	echo "Installing AUR packages..."
	AUR_PACKAGES=(
		yazi
	)

	for pkg in "${AUR_PACKAGES[@]}"; do
		if ! pacman -Q "$pkg" >/dev/null 2>&1; then
			paru -S --noconfirm "$pkg"
		fi
	done
fi

# For MSYS2, some packages might need manual installation or alternatives
if [[ "$IS_MSYS2" == true ]]; then
	echo ""
	echo "Note: Some packages may not be available in MSYS2 repos:"
	echo "  - yazi: Install manually from GitHub releases"
	echo "  - zenity: Not available in MSYS2"
	echo "  - wl-clipboard: Use Windows clipboard instead"
fi

echo ""
echo "Package installation complete!"
