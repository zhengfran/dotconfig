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
    git clone https://github.com/LazyVim/starter ~/.config/nvim
elif [ -e ~/.config/nvim ]; then
    echo "[INFO] ~/.config/nvim already exists. Skipping."
else
    echo "[WARN] lazy directory for nvim not found in dotconfig."
fi

## emacs
if [ ! -e ~/.emacs.d ]; then
    echo "[INFO] Cloning chemacs2 into ~/.emacs.d..."
    git clone --depth 1 https://github.com/plexus/chemacs2.git ~/.emacs.d
else
    echo "[INFO] ~/.emacs.d already exists. Skipping chemacs2 clone."
fi

emacs_dir_path=$(find ~/dotconfig -type d -name "emacs" | head -n 1)
if [ -n "$emacs_dir_path" ] && [ ! -e ~/.config/emacs ]; then
    echo "[INFO] Symlinking emacs dir: $emacs_dir_path -> ~/.config/emacs"
    ln -s "$emacs_dir_path" ~/.config/emacs
    ln -s "$emacs_dir_path/chemacs/.emacs-profiles.el" ~/.emacs-profiles.el
elif [ -e ~/.config/emacs ]; then
    echo "[INFO] ~/.config/emacs already exists. Skipping."
else
    echo "[WARN] emacs directory not found in dotconfig."
fi

## sway
sway_dir_path=$(find ~/dotconfig -type d -name "sway" | head -n 1)
if [ -n "$sway_dir_path" ] && [ ! -e ~/.config/sway ]; then
    echo "[INFO] Symlinking sway dir: $sway_dir_path -> ~/.config/sway"
    ln -s "$sway_dir_path" ~/.config/sway

    # Symlink individual scripts if scripts folder exists
    sway_scripts_path="$sway_dir_path/scripts"
    if [ -d "$sway_scripts_path" ]; then
        mkdir -p ~/.local/bin
        for script in "$sway_scripts_path"/*; do
            if [ -f "$script" ]; then
                script_name=$(basename "$script")
                target="$HOME/.local/bin/$script_name"
                if [ ! -e "$target" ]; then
                    echo "[INFO] Symlinking script: $script -> $target"
                    ln -s "$script" "$target"
                else
                    echo "[INFO] $target already exists. Skipping."
                fi
            fi
        done
    else
        echo "[WARN] scripts directory not found under sway."
    fi
elif [ -e ~/.config/sway ]; then
    echo "[INFO] ~/.config/sway already exists. Skipping."
else
    echo "[WARN] sway directory not found in dotconfig."
fi


##AI
opencode_dir_path=$(find ~/dotconfig -type d -name "opencode" | head -n 1)
if [ -n "$opencode_dir_path" ] && [ ! -e ~/.config/opencode ]; then
    echo "[INFO] Symlinking opencode dir: $opencode_dir_path -> ~/.config/opencode"
    ln -s "$opencode_dir_path" ~/.config/opencode
elif [ -e ~/.config/opencode ]; then
    echo "[INFO] ~/.config/opencode already exists. Skipping."
else
    echo "[WARN] opencode directory not found in dotconfig."
fi

agents_dir_path=$(find ~/dotconfig -type d -path "*/tools/ai/agents" | head -n 1)
if [ -n "$agents_dir_path" ] && [ ! -e ~/.agents ]; then
    echo "[INFO] Symlinking agents dir: $agents_dir_path -> ~/.agents"
    ln -s "$agents_dir_path" ~/.agents
elif [ -e ~/.agents ]; then
    echo "[INFO] ~/.agents already exists. Skipping."
else
    echo "[WARN] agents directory not found in dotconfig."
fi

## aerospace (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
    aerospace_dir_path=$(find ~/dotconfig -type d -path "*/tools/wm/aerospace" | head -n 1)
    if [ -n "$aerospace_dir_path" ] && [ ! -e ~/.config/aerospace ]; then
        echo "[INFO] Symlinking aerospace dir: $aerospace_dir_path -> ~/.config/aerospace"
        ln -s "$aerospace_dir_path" ~/.config/aerospace
    elif [ -e ~/.config/aerospace ]; then
        echo "[INFO] ~/.config/aerospace already exists. Skipping."
    else
        echo "[WARN] aerospace directory not found in dotconfig."
    fi
fi

echo "[INFO] setup-config.sh completed."
