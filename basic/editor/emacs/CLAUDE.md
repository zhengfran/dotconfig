# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a modular Emacs configuration built with `straight.el` package manager and `use-package`. The configuration is split into 20+ feature-based modules for maintainability and clear dependency management.

**Key Philosophy**: Evil mode (Vim emulation) + SPC leader key bindings + Org-mode focused workflow with denote note-taking system.

## Starting Emacs with This Configuration

```bash
emacs --init-directory ~/dotconfig/basic/editor/emacs/modular/
```

For faster startup profiling:
```elisp
M-x esup  ; Emacs Start Up Profiler
```

## Architecture

### Directory Structure

```
modular/
├── early-init.el          # Pre-GUI initialization (fonts, performance)
├── init.el                # Bootstrap & module loader (~150 lines)
├── modules/               # 20+ feature modules (see below)
│   ├── core.el           # System detection, paths, file management
│   ├── keybindings.el    # which-key, general.el, SPC leader setup
│   ├── ui.el             # Themes, modeline, icons
│   ├── completion.el     # Vertico, consult, corfu
│   ├── evil.el           # Vim emulation
│   ├── org-*.el          # Org-mode ecosystem
│   └── ...
├── snippets/              # Yasnippet templates (org-mode, fundamental-mode)
├── straight/              # Package manager (straight.el)
└── var/                   # Runtime state (no-littering)
```

### Module Dependency Graph

**Critical**: Modules MUST be loaded in dependency order (as defined in `init.el`):

1. **core** → System detection, paths (defines `my/org-base-dir`, `my/is-*` vars)
2. **keybindings** → `zzc/leader-keys` definer (used by ~10 modules)
3. **ui** → Themes, modeline
4. **editor** → Basic editor settings
5. **evil** → Evil mode (required by navigation, window, etc.)
6. **window** → Window management
7. **completion** → Vertico/consult (required by bookmarks, denote)
8. **bookmarks** → Uses consult
9. **workspace** → Tab-bar, project, desktop sessions
10. **navigation** → Avy, treemacs, multiple-cursors
11. **snippets** → Yasnippet + global snippet search
12. **org-base** → Core org-mode
13. **org-agenda-config** → Defines `org-agenda-files`
14. **denote-config** → Uses `org-agenda-files`
15. **org-babel** → Code execution in org
16. **buffer-tabs** → Buffer tab-line
17. **programming** → LSP, language modes
18. **ai** → GPTel AI integration
19. **chinese** → Input methods, formatting
20. **terminal** → vterm (non-Windows only)

### Key Global Variables

Defined in `core.el`:
- `my/org-base-dir` - Denote notes directory (`~/org/notes/`)
- `my/is-windows`, `my/is-linux`, `my/is-mac`, `my/is-WSL` - Platform detection
- `org_notes_dir`, `zot_bib`, `zot_pdf` - Zotero integration paths
- `my/font-size` - Font size in 1/10 pt units (160 = 16pt)
- `my/preferred-fonts` - Font family mappings

Defined in `keybindings.el`:
- `zzc/leader-keys` - General.el leader key definer (SPC in normal/visual, C-SPC globally)

## Common Development Tasks

### Modifying Configuration

1. **Edit a module**: Open `modular/modules/<feature>.el`
2. **Reload config**: `SPC r r` or `M-x my/reload-emacs-config`
3. **Profile startup**: `M-x esup`

### Adding a New Module

1. Create `modular/modules/your-feature.el`
2. Add header with Commentary section documenting:
   - DEPENDENCIES: Which modules it requires
   - USED BY: Which modules depend on it
3. End with `(provide 'your-feature)`
4. Add `(require 'your-feature)` to `init.el` in correct dependency order
5. Use `zzc/leader-keys` for keybindings (see examples in existing modules)

### Code Editing Best Practices

**CRITICAL**: Always validate Elisp syntax after making changes to configuration files.

```bash
# Validate syntax of a module file
emacs --batch --eval "(condition-case err (progn (load-file \"/path/to/file.el\") (message \"Syntax OK\")) (error (message \"Error: %S\" err)))"
```

Common syntax errors to avoid:
- **Parentheses mismatch**: Count opening and closing parens carefully when adding nested forms
- **Missing quote**: Symbols and lists need proper quoting (`'symbol`, `'(list)`)
- **Malformed let bindings**: `(let ((var value)) body)` not `(let (var value) body)`
- **Unbalanced strings**: Check for missing closing quotes in strings and docstrings

When editing complex nested forms:
1. Make the edit in your tool
2. Run syntax validation before committing
3. If syntax error occurs, use `git diff` to see exact changes and identify the issue
4. Fix and re-validate before proceeding

### Keybinding Conventions

All keybindings use `zzc/leader-keys` with SPC prefix:

```elisp
(zzc/leader-keys
  "p"   '(:ignore t :which-key "project")
  "p p" '(project-switch-project :which-key "switch project")
  "p f" '(project-find-file :which-key "find file"))
```

Common prefixes:
- `SPC r` - Reload config
- `SPC p` - Project commands
- `SPC s` - Snippet commands
- `SPC o` - Org-mode commands
- `SPC n` - Denote notes

### Denote Structure

Directory layout under `~/org/notes/`:
```
notes/
├── journal/                           # Daily notes (migrated from daily/)
├── ref/                              # Reference/literature notes
├── trades/                           # Trade logs (custom capture)
└── YYYYMMDDTHHMMSS--title__keywords.org  # Files in denote format
    Examples:
    - 20240131T120000--my-project__project.org
    - 20240131T000000--2024-01-31-thu__journal.org
```

Denote naming convention:
- `YYYYMMDDTHHMMSS` - Timestamp identifier
- `--title` - Slugified title
- `__keywords` - Underscore-separated keywords (project, journal, trade, ref)

Key functions:
- `my/denote-ensure-journal-note-exists` - Create today's journal note
- `my/copy-completed-task-to-journal` - Auto-copy DONE tasks to journal notes
- `my/denote-capture-trade` - Trade log capture with templates
- `my/denote-refresh-agenda-list` - Add project files to agenda

### Snippet System

Snippets live in `~/org/snippets/<mode-name>/`:
- Global search: `M-x my/snippet-search`
- Insert: `SPC s i`
- Create new: `SPC s n` (with mode selection)
- Eval expressions: Use `(elisp-code)` in snippet templates

## Module-Specific Notes

### completion.el
- **Corfu**: In-buffer completion (TAB/S-TAB navigation, RET insert)
- **Vertico**: Minibuffer completion
- **Consult**: Enhanced search commands (`consult-ripgrep`, `consult-buffer`)
- **Cape**: Completion-at-point extensions (file, dabbrev, emoji, yasnippet)

### evil.el
- Uses `evil-collection` for consistent bindings
- Evil escape: `jk` → normal mode
- Evil commentary: `gc` → comment
- Evil surround: `ys`, `cs`, `ds` → surround operations
- Text objects: `evil-textobj-anyblock` for `ib`/`ab`

### workspace.el
- **Tab-bar workspaces**: Named workspaces with buffer isolation
- **Desktop sessions**: Auto-save every 5 minutes (`desktop-auto-save-timeout`)
- **Project markers**: `.project`, `*.csproj` recognized as project roots

### bookmarks.el (543 lines - largest module)
- URL bookmarks with denote integration
- Bookmark search via consult (`consult-bookmark`)
- URL capture from clipboard → denote notes

### snippets.el (786 lines - second largest)
- Comprehensive snippet capture system
- Multi-monitor aware popup frames
- Raycast integration (external trigger: Alt+Space → 'ss')
- Expression evaluation in snippets
- Mode detection and creation workflow

### denote-config.el (~400 lines)
- Trade log capture with template system (`my/trade-template-dir`)
- Task completion automation (copies DONE/CANCEL to journal notes)
- Backlink creation for completed tasks
- consult-denote integration for search and navigation
- Org-noter PDF annotation integration
- File-based linking (no database required)

## Platform-Specific Behavior

### Windows
- Terminal module (`terminal.el`) is NOT loaded
- Shell detection: Falls back to bash/zsh if available
- No vterm support

### WSL (Windows Subsystem for Linux)
- Browser opens via `/mnt/c/Windows/System32/cmd.exe /c start`
- Detected via `uname -r` containing "Microsoft"

### macOS/Linux
- Exec path loaded from shell via `exec-path-from-shell`
- Vterm terminal emulation available

## Performance Optimizations

### Early-init.el
- Font configuration before GUI render
- `inhibit-compacting-font-caches t` - Prevent font cache GC
- `auto-window-vscroll nil` - Disable variable scroll

### Init.el
- GC threshold: 50MB (`gc-cons-threshold`)
- Straight.el modification checks: `check-on-save` + `find-when-checking`
- Native compilation enabled (Emacs 29+)
- Org recipe: Direct GitHub clone (bypasses org-elpa pre-build issues)

### No-littering
- Auto-save, backup, and state files centralized in `var/` directory
- `recentf` excludes temporary directories
- Clean user-emacs-directory

## Common Patterns

### use-package with straight.el

```elisp
(use-package package-name
  :custom
  (setting-var value)
  :config
  (mode-enable))
```

All packages use `straight.el` by default (`:straight t` implicit).

### Module Structure Template

```elisp
;;; feature.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;; Feature description
;;
;; DEPENDENCIES: other-module (var-name), another-module (function-name)
;; USED BY: dependent-module

;;; Code:

;; ============================================================================
;; SECTION NAME
;; ============================================================================

(use-package ...)

(provide 'feature)
;;; feature.el ends here
```

## Testing & Validation

### Manual Testing
1. Start Emacs with config: `emacs --init-directory ~/dotconfig/basic/editor/emacs/modular/`
2. Check `*Messages*` buffer for errors
3. Verify startup time: "*** Emacs loaded in X.XX seconds"

### Common Issues
- **Module load order**: Ensure dependencies loaded first
- **Missing directories**: Check `org-roam-directory` and subdirectories exist
- **Font fallback**: Fonts gracefully fall back if not found (see `my/get-font-or-default`)
- **Native comp warnings**: Ignore async compilation warnings on first startup

## File Size Guidelines

Most modules: 100-200 lines
- Large modules are acceptable if cohesive:
  - `snippets.el` (786 lines) - Comprehensive snippet system
  - `bookmarks.el` (543 lines) - URL + org-roam integration
  - `org-roam-config.el` (569 lines) - Complex knowledge management

When editing, maintain:
- Clear section separators (`============`)
- Dependency documentation in Commentary
- `use-package` declarations for each package
- Keybindings via `zzc/leader-keys`
