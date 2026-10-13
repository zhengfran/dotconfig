## What This Is

Personal dotfiles repository (`~/dotconfig`) for a cross-platform development environment (WSL2/Linux primary, macOS secondary, Windows tertiary). Configs are symlinked into place by `scripts/setup-config.sh`. `AGENTS.md` is a symlink to this file, so every coding harness (Claude Code, Pi, Codex, Kiro) reads the same instructions.

## Repository Structure

- **basic/shell/** — Shell configs: zsh (primary, with zim framework), bash, nushell, powershell. Common aliases/functions/env shared across shells via `basic/shell/common/`.
- **basic/git/** — Git config. `gitconfig` (no identity) and `gitconfig-personal` are symlinked to `~/.gitconfig` / `~/.gitconfig-personal`; the work identity, credential helpers and machine-specific `safe.directory` entries live in the untracked `~/.gitconfig-local`. Identity is chosen per-repo by `[includeIf "hasconfig:remote.*.url:..."]` rules — this repo is public, so never commit the work email.
- **basic/editor/emacs/** — Modular Emacs configuration (see `basic/editor/emacs/CLAUDE.md` for detailed architecture). This is the most actively developed part of the repo.
- **basic/editor/nvim/** — Neovim config.
- **tools/** — Application configs: tmux, wezterm, yazi, herdr, rime (Rime IME, wanxiang schema), window managers (sway, omniwm, komorebi, yasb), automation (AutoHotkey).
- **tools/ai/** — AI harness configs: `claude/` (Claude Code settings, statusline), `dsh/` (DeepSeek Harness profiles), `shared/` (global `AGENTS.md` for every harness).
  - **tools/ai/pi/** — git submodule of [zhengfran/pi-config](https://github.com/zhengfran/pi-config): Pi settings, extensions, themes, agent routing. Edit and commit in the submodule, push it, then bump the pointer here.
  - **tools/ai/skills/** — git submodule of [zhengfran/zzc-skills](https://github.com/zhengfran/zzc-skills). Edit and commit skills in that repo, then bump the pointer here. The orchestrator runtime lives in the private [zhengfran/zzc-orchestrator](https://github.com/zhengfran/zzc-orchestrator) (on hold, not a submodule).
  - **tools/ai/mcp/** — MCP servers shared by every harness (Pi, Claude Code, Kiro). `servers.json` is secret-free; `bin/mcp-run` reads credentials from the untracked `~/.config/mcp/secrets.env`; `sync-mcp.sh` merges the servers into each harness's user config.
- **scripts/** — Setup and utility scripts (`.sh` for Linux/macOS, `.ps1` for Windows).
- **archive/** — Configs for retired tools, kept for history and never linked by setup. `archive/README.md` lists each tool, when it was used and what replaced it. Retire a tool by `git mv`-ing it here and adding a row, not by deleting it.
- **ssh/** — SSH client config (no keys).
- **.commonprofile** — Shared shell functions sourced by zshrc/bashrc. Contains WSL helpers and work-specific (Continental automotive) build functions.

## Setup & Deployment

```bash
# Full setup (clone, symlink configs, install packages)
scripts/setup.sh

# Just symlink configs (no package install); also syncs MCP servers
scripts/setup-config.sh

# Build Emacs from source (Wayland/pgtk, native-comp, tree-sitter)
scripts/build-emacs.sh [-v emacs-30.2] [-s ~/emacs-source-code]

# Install system packages (Arch)
scripts/setup-packages-arch.sh
```

`setup-config.sh` symlinks, among others:
- `basic/shell/zsh/zshrc` → `~/.zshrc`, `basic/shell/zsh/zimrc` → `~/.zimrc`
- `basic/editor/emacs/modular/` → `~/.config/emacs` (loaded directly via XDG; no chemacs)
- `basic/editor/nvim/` → `~/.config/nvim`
- `basic/git/gitconfig` → `~/.gitconfig`
- `tools/yazi/` → `~/.config/yazi`, `tools/wm/sway/` → `~/.config/sway`, `tools/herdr/config.toml` → `~/.config/herdr/config.toml`
- `tools/ai/claude/settings.json` → `~/.claude/settings.json`
- `tools/ai/pi/{settings.json,extensions,themes}` → `~/.pi/agent/`
- `tools/ai/shared/AGENTS.md` → `~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md`, `~/.pi/agent/AGENTS.md`

The script is the source of truth for the full list.

## Key Conventions

- **Git branch**: Active development is on `main`.
- **Submodules**: `tools/ai/pi` and `tools/ai/skills` (see `.gitmodules`), both tracking `main`. Pull with `git pull --recurse-submodules` and check `git submodule status` before running `git submodule update`, which checks out the *recorded* commit and can roll a submodule back if its pointer hasn't been bumped yet.
- **Emacs config** is the primary focus. Read `basic/editor/emacs/CLAUDE.md` before modifying any `.el` files — it documents module load order, dependencies, and key variables.
- **Emacs Lisp edits**: After editing `.el` files, delete any corresponding `.elc` files to avoid stale byte-compiled code.
- **WSL awareness**: Many configs handle WSL path differences. Check `my/is-WSL` (Emacs) or `is_wsl()` (.commonprofile) for platform-conditional logic.
- **Symlink-based deployment**: Editing files here directly changes the live environment. Shell configs apply after re-sourcing, Emacs on next restart, harness configs on next session.
- **Public repo, no secrets**: Tokens, work email, and internal hostnames go in untracked files (`~/.gitconfig-local`, `~/.config/mcp/secrets.env`), never in tracked ones. Check `git diff` for them before committing.
- **Git commits**: [Conventional Commits](https://www.conventionalcommits.org/) — `type(scope): description`, e.g. `feat(ahk): add hyper key layer`. Allowed types: `feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert`. Do not add a `Co-Authored-By` trailer or any other AI-assistant attribution.

## How to Work Here

**When to keep going and when to stop.** When a step doesn't need my input, keep going, and put status notes in the same message as your next action. Stop and ask only when you can't continue without me, or before anything destructive or outward-facing:
- deleting or overwriting files outside this repo, including untracked live files under `~` (`~/.gitconfig-local`, `~/.config/mcp/secrets.env`, `~/.claude.json`, harness state dirs)
- committing, pushing, force-pushing, or bumping a submodule pointer (`tools/ai/pi`, `tools/ai/skills`), unless I asked for it
- anything that would put a secret, work email, or internal hostname into a tracked file

**Done means verified.** Finish with the check that fits what changed, and say if you could not run it:
- Emacs: `emacs --batch --init-directory ~/dotconfig/basic/editor/emacs/modular/ -l init.el 2>&1` loads with no new errors.
- Shell: `bash -n` / `zsh -n` on edited scripts; source the file in a subshell when it is safe to.
- Pi extensions: `npm run check` in `tools/ai/pi`.
- JSON/TOML configs: parse them (`jq . file`).
- MCP: `tools/ai/mcp/sync-mcp.sh`, then `pi mcp list` or `claude mcp list` shows the servers connected.

**Read before guessing.** Before proposing fixes to an Emacs package, a harness, or any third-party tool, read its source or docs — do not guess at APIs. Mark anything you couldn't confirm and say where you looked.

**Final report.** End a multi-step run with three short parts, in this order: **Needs from me** (decisions or actions only I can take, or "nothing"), **Changed** (files and behaviour), **Found** (anything surprising, stale, or broken that you noticed but did not fix).

## Emacs Quick Reference

The Emacs config lives at `basic/editor/emacs/modular/` with 23 modules in `modules/`, loaded in dependency order from `init.el`. Key points:

- Package manager: `straight.el` with `use-package`
- Keybinding system: Evil mode + `general.el` with SPC leader via `zzc/leader-keys`
- Notes system: Denote (file-based, no database) in `~/org/notes/`
