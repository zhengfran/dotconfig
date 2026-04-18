#!/usr/bin/env bash

echo "[INFO] Starting setup-config.sh..."

# Fetch config if not present and set them up
if [ ! -d ~/dotconfig ]; then
    echo "[INFO] Cloning dotconfig repository..."
    git clone git@github.com:zhengfran/dotconfig.git ~/dotconfig
else
    echo "[INFO] dotconfig directory already exists."
fi

# Helper: symlink src -> dest, prompting if dest already exists
symlink_config() {
    local label="$1"
    local src="$2"
    local dest="$3"

    if [ -z "$src" ]; then
        echo "[WARN] $label not found in dotconfig."
        return
    fi

    if [ ! -e "$dest" ] && [ ! -L "$dest" ]; then
        echo "[INFO] Symlinking $label: $src -> $dest"
        ln -s "$src" "$dest"
    else
        read -r -p "[PROMPT] $dest already exists. Replace? [y/N] " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            rm -rf "$dest"
            echo "[INFO] Symlinking $label: $src -> $dest"
            ln -s "$src" "$dest"
        else
            echo "[INFO] Skipping $label."
        fi
    fi
}

# Symlink config files
## tmux
tmux_conf_path=$(find ~/dotconfig -type f -name ".tmux.conf" | head -n 1)
symlink_config "tmux config" "$tmux_conf_path" ~/.tmux.conf

## zsh
zshrc_path=$(find ~/dotconfig -type f -name "zshrc" | head -n 1)
symlink_config "zshrc" "$zshrc_path" ~/.zshrc

zimrc_path=$(find ~/dotconfig -type f -name "zimrc" | head -n 1)
symlink_config "zimrc" "$zimrc_path" ~/.zimrc

shell_dir_path=$(find ~/dotconfig -type d -name "common" | head -n 1)
symlink_config "common shell dir" "$shell_dir_path" ~/.config/common

zsh_dir_path=$(find ~/dotconfig -type d -name "zsh" | head -n 1)
symlink_config "zsh dir" "$zsh_dir_path" ~/.config/zsh

## yazi
yazi_dir_path=$(find ~/dotconfig -type d -name "yazi" | head -n 1)
symlink_config "yazi dir" "$yazi_dir_path" ~/.config/yazi

## nvim
nvim_dir_path=$(find ~/dotconfig -type d -name "lazy" | head -n 1)
if [ -n "$nvim_dir_path" ]; then
    if [ ! -e ~/.config/nvim ] && [ ! -L ~/.config/nvim ]; then
        echo "[INFO] Cloning LazyVim starter into ~/.config/nvim"
        git clone https://github.com/LazyVim/starter ~/.config/nvim
    else
        read -r -p "[PROMPT] ~/.config/nvim already exists. Replace? [y/N] " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            rm -rf ~/.config/nvim
            echo "[INFO] Cloning LazyVim starter into ~/.config/nvim"
            git clone https://github.com/LazyVim/starter ~/.config/nvim
        else
            echo "[INFO] Skipping nvim."
        fi
    fi
else
    echo "[WARN] lazy directory for nvim not found in dotconfig."
fi

## emacs
if [ ! -e ~/.emacs.d ] && [ ! -L ~/.emacs.d ]; then
    echo "[INFO] Cloning chemacs2 into ~/.emacs.d..."
    git clone --depth 1 https://github.com/plexus/chemacs2.git ~/.emacs.d
else
    read -r -p "[PROMPT] ~/.emacs.d already exists. Replace with chemacs2? [y/N] " answer
    if [[ "$answer" =~ ^[Yy]$ ]]; then
        rm -rf ~/.emacs.d
        echo "[INFO] Cloning chemacs2 into ~/.emacs.d..."
        git clone --depth 1 https://github.com/plexus/chemacs2.git ~/.emacs.d
    else
        echo "[INFO] Skipping chemacs2 clone."
    fi
fi

emacs_dir_path=$(find ~/dotconfig -type d -name "emacs" | head -n 1)
if [ -n "$emacs_dir_path" ]; then
    symlink_config "emacs dir" "$emacs_dir_path" ~/.config/emacs
    symlink_config "emacs-profiles.el" "$emacs_dir_path/chemacs/.emacs-profiles.el" ~/.emacs-profiles.el
else
    echo "[WARN] emacs directory not found in dotconfig."
fi

## sway
sway_dir_path=$(find ~/dotconfig -type d -name "sway" | head -n 1)
if [ -n "$sway_dir_path" ]; then
    symlink_config "sway dir" "$sway_dir_path" ~/.config/sway

    sway_scripts_path="$sway_dir_path/scripts"
    if [ -d "$sway_scripts_path" ]; then
        mkdir -p ~/.local/bin
        for script in "$sway_scripts_path"/*; do
            if [ -f "$script" ]; then
                script_name=$(basename "$script")
                symlink_config "sway script $script_name" "$script" "$HOME/.local/bin/$script_name"
            fi
        done
    else
        echo "[WARN] scripts directory not found under sway."
    fi
else
    echo "[WARN] sway directory not found in dotconfig."
fi

## AI
opencode_dir_path=$(find ~/dotconfig -type d -name "opencode" | head -n 1)
symlink_config "opencode dir" "$opencode_dir_path" ~/.config/opencode

agents_dir_path=$(find ~/dotconfig -type d -path "*/tools/ai/agents" | head -n 1)
symlink_config "agents dir" "$agents_dir_path" ~/.agents

claude_settings_path=$(find ~/dotconfig -type f -path "*/tools/ai/claude/settings.json" | head -n 1)
if [ -n "$claude_settings_path" ]; then
    mkdir -p ~/.claude
    symlink_config "Claude Code settings" "$claude_settings_path" ~/.claude/settings.json
else
    echo "[WARN] Claude Code settings.json not found in dotconfig."
fi

claude_statusline_path=$(find ~/dotconfig -type f -path "*/tools/ai/claude/statusline-command.sh" | head -n 1)
if [ -n "$claude_statusline_path" ]; then
    mkdir -p ~/.claude
    symlink_config "Claude Code statusline" "$claude_statusline_path" ~/.claude/statusline-command.sh
else
    echo "[WARN] Claude Code statusline-command.sh not found in dotconfig."
fi

claude_skills_path=$(find ~/dotconfig -type d -path "*/tools/ai/agents/skills" | head -n 1)
if [ -n "$claude_skills_path" ]; then
    mkdir -p ~/.claude
    symlink_config "Claude Code skills" "$claude_skills_path" ~/.claude/skills
else
    echo "[WARN] Claude Code skills directory not found in dotconfig."
fi

## aerospace (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
    aerospace_dir_path=$(find ~/dotconfig -type d -path "*/tools/wm/aerospace" | head -n 1)
    symlink_config "aerospace dir" "$aerospace_dir_path" ~/.config/aerospace
fi

echo "[INFO] setup-config.sh completed."
