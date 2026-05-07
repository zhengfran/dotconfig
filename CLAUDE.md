## What This Is

Personal dotfiles repository (`~/dotconfig`) for a cross-platform development environment (WSL2/Linux primary, macOS secondary, Windows tertiary). Configs are symlinked into place by `scripts/setup-config.sh`.

## Repository Structure

- **basic/shell/** — Shell configs: zsh (primary, with zim framework), bash, nushell, powershell. Common aliases/functions/env shared across shells via `basic/shell/common/`.
- **basic/editor/emacs/** — Modular Emacs configuration (see `basic/editor/emacs/CLAUDE.md` for detailed architecture). This is the most actively developed part of the repo.
- **tools/** — Application configs: tmux, wezterm, yazi (file manager), window managers (sway, aerospace), AI tools (Claude Code settings, opencode).
- **rime/** — Rime IME (Chinese input method) configuration with wanxiang schema.
- **dev/** — Language-specific dev environment configs (C, Rust, Python, shell). Currently placeholder directories.
- **scripts/** — Setup and utility scripts.
- **ssh/** — SSH client config (no keys).
- **.commonprofile** — Shared shell functions sourced by zshrc/bashrc. Contains WSL helpers and work-specific (Continental automotive) build functions.

## Setup & Deployment

```bash
# Full setup (clone, symlink configs, install packages)
scripts/setup.sh

# Just symlink configs (no package install)
scripts/setup-config.sh

# Build Emacs from source (Wayland/pgtk, native-comp, tree-sitter)
scripts/build-emacs.sh [-v emacs-30.2] [-s ~/emacs-source-code]

# Install system packages (Arch)
scripts/setup-packages-arch.sh
```

The setup script symlinks configs to standard locations:
- `basic/shell/zsh/zshrc` → `~/.zshrc`
- `basic/shell/zsh/zimrc` → `~/.zimrc`
- `basic/editor/emacs/` → `~/.config/emacs`
- `tools/yazi/` → `~/.config/yazi`
- `tools/wm/sway/` → `~/.config/sway`
- `tools/ai/opencode/` → `~/.config/opencode`

## Key Conventions

- **Git branch**: Active development is on `main`.
- **Emacs config** is the primary focus. Always read `basic/editor/emacs/CLAUDE.md` before modifying any `.el` files — it documents module load order, dependencies, and key variables.
- **Emacs Lisp edits**: After editing `.el` files, delete any corresponding `.elc` files to avoid stale byte-compiled code. Validate changes with `emacs --batch --init-directory ~/dotconfig/basic/editor/emacs/modular/ -l init.el 2>&1`.
- **WSL awareness**: Many configs handle WSL path differences. Check `my/is-WSL` (Emacs) or `is_wsl()` (.commonprofile) for platform-conditional logic.
- **Symlink-based deployment**: Editing files here directly affects the live environment. Changes take effect immediately for shell configs (after re-sourcing) and on next Emacs restart.
- **Git commits**: Follow [Conventional Commits](https://www.conventionalcommits.org/) format — `type(scope): description`, e.g. `feat(ahk): add hyper key layer`. Allowed types: `feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert`. Use the `conventional-commit` skill to format. Do **not** add `Co-Authored-By: Claude...` (or any AI assistant attribution) trailer.

## Emacs Quick Reference

The Emacs config lives at `basic/editor/emacs/modular/` with 22 modules loaded in dependency order from `init.el`. Key points:

- Package manager: `straight.el` with `use-package`
- Keybinding system: Evil mode + `general.el` with SPC leader via `zzc/leader-keys`
- Notes system: Denote (file-based, no database) in `~/org/notes/`
- Before proposing fixes to Emacs packages, read the package source code first — do not guess at APIs
