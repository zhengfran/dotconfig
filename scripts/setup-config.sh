#!/usr/bin/env bash

# Fetch config if not present and set them up
if [ ! -d ~/dotconfig ]; then
    git clone -b Rebirth git@github.com:zhengfran/dotconfig.git ~/dotconfig
fi

# Symlink config files
## tmux
tmux_conf_path=$(find ~/dotconfig -type f -name ".tmux.conf" | head -n 1)
if [ -n "$tmux_conf_path" ]; then
    ln -sf "$tmux_conf_path" ~/.tmux.conf
fi

## zsh
zshrc_path=$(find ~/dotconfig -type f -name "zshrc" | head -n 1)
if [ -n "$zshrc_path" ]; then
    ln -sf "$zshrc_path" ~/.zshrc
fi
zimrc_path=$(find ~/dotconfig -type f -name "zimrc" | head -n 1)
if [ -n "$zimrc_path" ]; then
    ln -sf "$zimrc_path" ~/.zimrc
fi
shell_dir_path=$(find ~/dotconfig -type d -name "common" | head -n 1)
if [ -n "$shell_dir_path" ]; then
    ln -sf "$shell_dir_path" ~/.config/common
fi
zsh_dir_path=$(find ~/dotconfig -type d -name "zsh" | head -n 1)
if [ -n "$zsh_dir_path" ]; then
    ln -sf "$zsh_dir_path" ~/.config/zsh
fi

## yazi
yazi_dir_path=$(find ~/dotconfig -type d -name "yazi" | head -n 1)
if [ -n "$yazi_dir_path" ]; then
    ln -sf "$yazi_dir_path" ~/.config/yazi
fi

## nvim
nvim_dir_path=$(find ~/dotconfig -type d -name "lazy" | head -n 1)
if [ -n "$nvim_dir_path" ]; then
    ln -sf "$nvim_dir_path" ~/.config/nvim
fi
