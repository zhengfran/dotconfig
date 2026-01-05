#!/usr/bin/env bash

# Install devtools
sudo apt install -y shfmt 

# Install debian/ubuntu packages
sudo apt install -y git wl-clipboard zsh ffmpeg 7zip jq poppler-utils zoxide imagemagick graphviz

# Install Homebrew if not present, certain debian packages are outdated
if ! command -v brew >/dev/null 2>&1; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
else
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# Install packages only if not present
for pkg in fzf fd ripgrep yazi lazygit nvim zenity; do
    if ! brew list --formula | grep -qw "$pkg"; then
        brew install "$pkg"
    fi
done
