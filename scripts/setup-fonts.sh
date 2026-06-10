#!/usr/bin/env bash
# Fonts required by the lijigang Doom Emacs config (basic/editor/emacs/doom-lijigang).
# Idempotent: safe to re-run.
set -e

# apt-provided fonts: Symbola (org bullets/symbols), Noto CJK + Color Emoji
# (PingFang SC/Apple Color Emoji fallbacks), LXGW WenKai (CJK).
if command -v apt-get >/dev/null 2>&1; then
    sudo apt-get install -y \
        fonts-symbola \
        fonts-noto-cjk \
        fonts-noto-color-emoji \
        fonts-lxgw-wenkai
fi

# Roboto Mono is not packaged with the Mono family in apt; fetch from Google Fonts.
ROBOTO_DIR="$HOME/.local/share/fonts/RobotoMono"
if ! fc-list | grep -qi "roboto mono"; then
    mkdir -p "$ROBOTO_DIR"
    base="https://raw.githubusercontent.com/googlefonts/RobotoMono/main/fonts/ttf"
    for f in RobotoMono-Regular.ttf RobotoMono-Bold.ttf RobotoMono-Italic.ttf RobotoMono-BoldItalic.ttf; do
        curl -fsSL "$base/$f" -o "$ROBOTO_DIR/$f"
    done
fi

fc-cache -f >/dev/null 2>&1
echo "Emacs fonts installed."
