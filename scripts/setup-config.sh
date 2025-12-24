#!/usr/bin/env bash

echo "[INFO] Starting setup-config.sh..."

# Fetch config if not present and set them up
if [ ! -d ~/dotconfig ]; then
    echo "[INFO] Cloning dotconfig repository..."
    git clone -b Rebirth git@github.com:zhengfran/dotconfig.git ~/dotconfig
else
    echo "[INFO] dotconfig directory already exists."
fi

# Symlink config files
## tmux
tmux_conf_path=$(find ~/dotconfig -type f -name ".tmux.conf" | head -n 1)
if [ -n "$tmux_conf_path" ] && [ ! -e ~/.tmux.conf ]; then
    echo "[INFO] Symlinking tmux config: $tmux_conf_path -> ~/.tmux.conf"
    ln -s "$tmux_conf_path" ~/.tmux.conf
elif [ -e ~/.tmux.conf ]; then
    echo "[INFO] ~/.tmux.conf already exists. Skipping."
else
    echo "[WARN] .tmux.conf not found in dotconfig."
fi

## zsh
zshrc_path=$(find ~/dotconfig -type f -name "zshrc" | head -n 1)
if [ -n "$zshrc_path" ] && [ ! -e ~/.zshrc ]; then
    echo "[INFO] Symlinking zshrc: $zshrc_path -> ~/.zshrc"
    ln -s "$zshrc_path" ~/.zshrc
elif [ -e ~/.zshrc ]; then
    echo "[INFO] ~/.zshrc already exists. Skipping."
else
    echo "[WARN] zshrc not found in dotconfig."
fi

zimrc_path=$(find ~/dotconfig -type f -name "zimrc" | head -n 1)
if [ -n "$zimrc_path" ] && [ ! -e ~/.zimrc ]; then
    echo "[INFO] Symlinking zimrc: $zimrc_path -> ~/.zimrc"
    ln -s "$zimrc_path" ~/.zimrc
elif [ -e ~/.zimrc ]; then
    echo "[INFO] ~/.zimrc already exists. Skipping."
else
    echo "[WARN] zimrc not found in dotconfig."
fi

shell_dir_path=$(find ~/dotconfig -type d -name "common" | head -n 1)
if [ -n "$shell_dir_path" ] && [ ! -e ~/.config/common ]; then
    echo "[INFO] Symlinking common dir: $shell_dir_path -> ~/.config/common"
    ln -s "$shell_dir_path" ~/.config/common
elif [ -e ~/.config/common ]; then
    echo "[INFO] ~/.config/common already exists. Skipping."
else
    echo "[WARN] common directory not found in dotconfig."
fi

zsh_dir_path=$(find ~/dotconfig -type d -name "zsh" | head -n 1)
if [ -n "$zsh_dir_path" ] && [ ! -e ~/.config/zsh ]; then
    echo "[INFO] Symlinking zsh dir: $zsh_dir_path -> ~/.config/zsh"
    ln -s "$zsh_dir_path" ~/.config/zsh
elif [ -e ~/.config/zsh ]; then
    echo "[INFO] ~/.config/zsh already exists. Skipping."
else
    echo "[WARN] zsh directory not found in dotconfig."
fi

## yazi
yazi_dir_path=$(find ~/dotconfig -type d -name "yazi" | head -n 1)
if [ -n "$yazi_dir_path" ] && [ ! -e ~/.config/yazi ]; then
    echo "[INFO] Symlinking yazi dir: $yazi_dir_path -> ~/.config/yazi"
    ln -s "$yazi_dir_path" ~/.config/yazi
elif [ -e ~/.config/yazi ]; then
    echo "[INFO] ~/.config/yazi already exists. Skipping."
else
    echo "[WARN] yazi directory not found in dotconfig."
fi

## nvim
nvim_dir_path=$(find ~/dotconfig -type d -name "lazy" | head -n 1)
if [ -n "$nvim_dir_path" ] && [ ! -e ~/.config/nvim ]; then
    echo "[INFO] Symlinking nvim dir: $nvim_dir_path -> ~/.config/nvim"
    ln -s "$nvim_dir_path" ~/.config/nvim
elif [ -e ~/.config/nvim ]; then
    echo "[INFO] ~/.config/nvim already exists. Skipping."
else
    echo "[WARN] lazy directory for nvim not found in dotconfig."
fi

echo "[INFO] setup-config.sh completed."
