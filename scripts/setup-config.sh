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

## herdr
herdr_conf_path=$(find ~/dotconfig -type f -path "*/tools/herdr/config.toml" | head -n 1)
if [ -n "$herdr_conf_path" ]; then
    mkdir -p ~/.config/herdr
    symlink_config "herdr config" "$herdr_conf_path" ~/.config/herdr/config.toml
fi

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
emacs_dir_path=$(find ~/dotconfig -type d -name "emacs" | head -n 1)
if [ -n "$emacs_dir_path" ]; then
    symlink_config "emacs dir" "$emacs_dir_path" ~/.config/emacs
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

# NOTE: legacy ~/.agents/skills flat dump removed — skills now live under
# tools/ai/skills/ (vendored + assembled) and are distributed per-agent below.

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

## AI skills — distribute the assembled global pack into every agent (single source of truth).
## Per-project packs (notes -> ~/org, coding -> a repo) are installed via
## tools/ai/skills/scripts/skills-install; global skills go everywhere.
skills_sync=$(find ~/dotconfig -type f -path "*/tools/ai/skills/scripts/skills-sync" | head -n 1)
[ -n "$skills_sync" ] && bash "$skills_sync" >/dev/null 2>&1 || echo "[WARN] skills-sync did not run"
assembled_global=$(find ~/dotconfig -type d -path "*/tools/ai/skills/assembled/global" | head -n 1)
if [ -n "$assembled_global" ]; then
    hermes_extra=$(find ~/dotconfig -type d -path "*/tools/ai/skills/assembled/hermes-only" | head -n 1)
    link_global_pack() {
        # $1 = destination skills dir, $2 = agent label
        local dest="$1" label="$2"
        [ -L "$dest" ] && rm -f "$dest"
        mkdir -p "$dest"
        find "$dest" -mindepth 1 -maxdepth 1 -exec rm -rf {} +   # clear stale entries
        for s in "$assembled_global"/*; do ln -sfn "$s" "$dest/$(basename "$s")"; done
        echo "[OK] $label skills -> assembled/global ($(ls "$dest" | wc -l))"
    }
    mkdir -p ~/.claude ~/.kiro ~/.pi/agent ~/.hermes
    link_global_pack ~/.claude/skills "Claude"
    link_global_pack ~/.kiro/skills "kiro"
    link_global_pack ~/.pi/agent/skills "pi"
    link_global_pack ~/.hermes/skills "hermes"
    if [ -n "$hermes_extra" ]; then   # hermes-only extras (e.g. garmin-runcoach)
        for s in "$hermes_extra"/*; do ln -sfn "$s" ~/.hermes/skills/"$(basename "$s")"; done
        echo "[OK] hermes extras -> $(ls "$hermes_extra" | wc -l)"
    fi
else
    echo "[WARN] assembled/global not found — run tools/ai/skills/scripts/skills-sync first."
fi

## kiro
kiro_dir_path=$(find ~/dotconfig -type d -path "*/tools/ai/kiro" | head -n 1)
if [ -n "$kiro_dir_path" ]; then
    mkdir -p ~/.kiro
    symlink_config "kiro settings" "$kiro_dir_path/settings/cli.json" ~/.kiro/settings/cli.json
    symlink_config "kiro mcp.json" "$kiro_dir_path/settings/mcp.json" ~/.kiro/settings/mcp.json
    symlink_config "kiro agents" "$kiro_dir_path/agents" ~/.kiro/agents
    symlink_config "kiro scripts" "$kiro_dir_path/scripts" ~/.kiro/scripts
    # kiro skills handled by the per-agent global distribution above.
fi

## aerospace (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
    aerospace_dir_path=$(find ~/dotconfig -type d -path "*/tools/wm/aerospace" | head -n 1)
    symlink_config "aerospace dir" "$aerospace_dir_path" ~/.config/aerospace
fi

echo "[INFO] setup-config.sh completed."
