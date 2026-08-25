#!/usr/bin/env bash

echo "[INFO] Starting setup-config.sh..."

# Fetch config if not present and set them up
if [ ! -d ~/dotconfig ]; then
    echo "[INFO] Cloning dotconfig repository and submodules..."
    git clone --recurse-submodules git@github.com:zhengfran/dotconfig.git ~/dotconfig
else
    echo "[INFO] dotconfig directory already exists."
    echo "[INFO] Initializing dotconfig submodules..."
    git -C ~/dotconfig submodule update --init --recursive || \
        echo "[WARN] Failed to initialize one or more dotconfig submodules."
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

## pi (standalone config repo, attached at tools/ai/pi as a submodule)
pi_dir_path=$(find ~/dotconfig -type d -path "*/tools/ai/pi" | head -n 1)
if [ -n "$pi_dir_path" ]; then
    if command -v npm >/dev/null 2>&1; then
        echo "[INFO] Installing Pi config dependencies..."
        npm --prefix "$pi_dir_path" run install:all || \
            echo "[WARN] Failed to install one or more Pi config dependencies."
    else
        echo "[WARN] npm not found; Pi local extensions may be missing dependencies."
    fi
    mkdir -p ~/.pi
    symlink_config "pi agent dir" "$pi_dir_path" ~/.pi/agent
fi

## nvim (LazyVim config tracked in dotconfig)
nvim_dir_path=$(find ~/dotconfig -type d -path "*/basic/editor/nvim" | head -n 1)
symlink_config "nvim dir" "$nvim_dir_path" ~/.config/nvim

## emacs
# Single modular config loaded directly via XDG ~/.config/emacs (no chemacs).
emacs_modular_path=$(find ~/dotconfig -type d -path "*editor/emacs/modular" | head -n 1)
if [ -n "$emacs_modular_path" ]; then
    symlink_config "emacs config" "$emacs_modular_path" ~/.config/emacs
else
    echo "[WARN] emacs modular config not found in dotconfig."
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

# NOTE: legacy ~/.agents/skills flat dump removed. Skills were later restructured
# into tools/ai/skills/ (vendored + assembled), extracted on 2026-08-10 into the
# standalone github.com/zhengfran/zzc-skills repo, and re-attached here as a git
# submodule at tools/ai/skills. That repo still owns its own distribution
# (`skills-sync && skills-install global`) — dotconfig no longer touches agent
# skill dirs itself.

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

## herdr agent integrations — herdr generates and owns these hook/plugin files,
## so they are installed rather than symlinked. Claude's settings.json ships a
## SessionStart hook pointing at ~/.claude/hooks/herdr-agent-state.sh, so the
## install has to run on every machine or that hook fails each session.
## (opencode lands inside ~/.config/opencode -> dotconfig, i.e. in this repo.)
if command -v herdr >/dev/null 2>&1; then
    for herdr_agent in claude codex opencode; do
        if herdr integration install "$herdr_agent" >/dev/null 2>&1; then
            echo "[OK] herdr integration: $herdr_agent"
        else
            echo "[WARN] herdr integration install $herdr_agent failed."
        fi
    done
else
    echo "[WARN] herdr not on PATH — skipping agent integrations; run 'herdr integration install claude' once installed."
fi

## AI skills — github.com/zhengfran/zzc-skills, vendored as a submodule at
## tools/ai/skills (populated by `git submodule update --init`, or by cloning
## dotconfig with --recurse-submodules). Its scripts/ dir is on PATH via
## basic/shell/common/env. That repo owns its own distribution: assembled/ is
## gitignored there, so a freshly-initialised submodule needs
##   skills-sync && skills-install global
## run once before any agent sees the skills.

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

## komorebi (Windows only)
# git-bash's `ln -s` copies instead of linking on Windows, so use PowerShell
# to create a real SYMLINKD that komorebic can follow.
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    komorebi_dir_path=$(find ~/dotconfig -type d -path "*/tools/wm/komorebi" | head -n 1)
    if [ -n "$komorebi_dir_path" ]; then
        komorebi_dest="$HOME/.config/komorebi"
        install_komorebi=1
        if [ -e "$komorebi_dest" ] || [ -L "$komorebi_dest" ]; then
            read -r -p "[PROMPT] $komorebi_dest already exists. Replace? [y/N] " answer
            [[ "$answer" =~ ^[Yy]$ ]] || install_komorebi=0
        fi
        if [ "$install_komorebi" = "1" ]; then
            mkdir -p "$HOME/.config"
            powershell.exe -NoProfile -Command "Remove-Item -Recurse -Force '$HOME\\.config\\komorebi' -ErrorAction SilentlyContinue; New-Item -ItemType SymbolicLink -Path '$HOME\\.config\\komorebi' -Target '$HOME\\dotconfig\\tools\\wm\\komorebi' | Out-Null" \
                && echo "[INFO] Symlinked komorebi config: $HOME/dotconfig/tools/wm/komorebi -> $komorebi_dest" \
                || echo "[WARN] Failed to create komorebi symlink (developer mode / admin may be required)."
        else
            echo "[INFO] Skipping komorebi config."
        fi
        # komorebic reads config from $KOMOREBI_CONFIG_HOME; set it as a persistent user env var
        powershell.exe -NoProfile -Command "[Environment]::SetEnvironmentVariable('KOMOREBI_CONFIG_HOME', '$HOME\\.config\\komorebi', 'User')" \
            && echo "[INFO] Set KOMOREBI_CONFIG_HOME=$HOME/.config/komorebi (User)"
    fi
fi

echo "[INFO] setup-config.sh completed."
