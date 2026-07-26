;;; terminal-config.el --- Terminal Configuration (vterm, eee) -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal emulator configuration using vterm and eee
;; Only loads on non-Windows systems (vterm doesn't work on Windows)
;;
;; DEPENDENCIES: core (system detection vars), evil (evil-set-initial-state),
;;               keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; VTERM - Non-Windows only (requires POSIX pty)
;; ============================================================================

(unless (eq system-type 'windows-nt)
  ;; Vterm - full-featured terminal emulator
  (use-package vterm
    :commands vterm
    :config
    (setq vterm-max-scrollback 10000)
    (setq vterm-buffer-name-string "%s")
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))
    (evil-set-initial-state 'vterm-mode 'emacs)
    ;; evil-collection-vterm sets its own initial state (normal); re-assert
    ;; emacs state on buffer creation so vterm always starts in editing mode.
    (add-hook 'vterm-mode-hook #'evil-emacs-state)
    (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
    (define-key vterm-mode-map (kbd "M-<left>") #'vterm-send-left)
    (define-key vterm-mode-map (kbd "M-<right>") #'vterm-send-right)
    (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode)
    ;; ── Desktop save/restore for vterm ──────────────────────────────────
    ;; NOTE: a live zsh process CANNOT survive an Emacs restart — the pty
    ;; closes and the OS kills the child. What we persist is the *set* of
    ;; vterm buffers and their working directories; on restart we respawn
    ;; FRESH zsh shells in those same dirs (no scrollback / no running cmd).
    (defun my/vterm-desktop-save (_desktop-dirname)
      "Persist the vterm's working directory for desktop restore."
      `((default-directory . ,default-directory)
        (buffer-name . ,(buffer-name))))
    (add-hook 'vterm-mode-hook
              (lambda ()
                ;; Mark this buffer as desktop-saveable via our handler.
                (setq-local desktop-save-buffer #'my/vterm-desktop-save)))
    (defun my/vterm-desktop-restore (_file-name buffer-name misc)
      "Recreate a vterm in the saved directory MISC on desktop restore."
      (let ((default-directory (or (cdr (assq 'default-directory misc))
                                   default-directory)))
        (vterm buffer-name)        ; spawn a fresh shell in that dir
        (current-buffer)))
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-buffer-mode-handlers
                   '(vterm-mode . my/vterm-desktop-restore))
      ;; Make sure vterm-mode is allowed to be saved/restored.
      (setq desktop-modes-not-to-save
            (delq 'vterm-mode desktop-modes-not-to-save)))
    :custom
    (vterm-shell (or (executable-find "zsh") (getenv "SHELL") "/bin/bash"))
    (vterm-kill-buffer-on-exit t)
    (vterm-clear-scrollback-when-clearing t))

  (use-package multi-vterm
    :commands (multi-vterm multi-vterm-project)
    :config
    (setq multi-vterm-program (or (executable-find "zsh") (getenv "SHELL") "/bin/bash"))
    (setq multi-vterm-dedicated-window-height-percent 30)
    :bind
    (:map vterm-mode-map
          ("C-c C-n" . multi-vterm-next)
          ("C-c C-p" . multi-vterm-prev)))

  (use-package vterm-toggle
    ;; NOTE: do NOT use ":after vterm" here. That would wrap this whole
    ;; declaration (including :bind) in (with-eval-after-load 'vterm ...),
    ;; and since vterm is itself lazy, "C-c t" would stay unbound until
    ;; vterm was loaded once. Autoload vterm-toggle on the keypress instead;
    ;; it pulls in vterm on demand.
    :commands (vterm-toggle)
    :config
    ;; Never take over the whole frame; always a split.
    (setq vterm-toggle-fullscreen-p nil)
    ;; Always pop the vterm buffer as a bottom window spanning the full
    ;; frame width, sized to 1/3 of the frame height.
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window
                    display-buffer-reuse-mode-window
                    display-buffer-at-bottom)
                   (mode . vterm-mode)
                   (reusable-frames . visible)
                   (window-height . 0.33)))
    :bind
    ;; Single toggle entry point.
    (("C-c t" . vterm-toggle)))

  (zzc/leader-keys
    "v"  '(:ignore t :which-key "vterm")
    "vv"  '(vterm :which-key "open vterm")
    "vn"  '(multi-vterm :which-key "new vterm")
    "vp"  '(multi-vterm-project :which-key "vterm in project")))

;; ============================================================================
;; GHOSTEL - Non-Windows trial, side-by-side with vterm (libghostty-vt engine)
;; ============================================================================
;; Trial run to evaluate replacing vterm.  Kept fully additive: the vterm block
;; above is untouched and keeps C-c t / SPC v, so both engines can be compared.
;; Ghostel lives on its own bindings (C-x m) per its README.  The native module
;; (x86_64-linux prebuilt) auto-downloads on first use to a stable path OUTSIDE
;; the repo.  See https://github.com/dakra/ghostel

(unless (eq system-type 'windows-nt)
  (use-package ghostel
    ;; Monorepo: elisp is under lisp/ (covered by :defaults) and the shell
    ;; integration + bundled terminfo under etc/ must ship in the build.
    :straight (ghostel :type git :host github :repo "dakra/ghostel"
                       :files (:defaults "etc"))
    :commands (ghostel ghostel-project ghostel-project-list-buffers ghostel-other)
    :bind (:map ghostel-semi-char-mode-map
                ("C-s" . consult-line)
                ;; Walk shell history with M-p/M-n by sending C-p/C-n to the pty.
                ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
                ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
    ;; NOTE: the README also binds M-<backspace> to `ghostel-backward-kill-word',
    ;; which does not exist in ghostel.el; M-<backspace> already falls through to
    ;; the global `backward-kill-word', so that binding is intentionally omitted.
    :custom
    ;; A straight rebuild can invalidate a module loaded from the package tree,
    ;; and an in-tree binary would show up as an untracked blob in the dotfiles
    ;; repo (~/.config/emacs is symlinked here).  Pin it outside the repo.
    (ghostel-module-directory (expand-file-name "~/.cache/emacs/ghostel/"))
    (ghostel-module-auto-install 'download)  ; silent prebuilt fetch on first use
    (ghostel-shell (or (executable-find "zsh") (getenv "SHELL") "/bin/bash"))
    :config
    (make-directory ghostel-module-directory t))

  (use-package evil-ghostel
    :straight (evil-ghostel :type git :host github :repo "dakra/ghostel"
                            :files ("extensions/evil-ghostel/evil-ghostel.el"))
    :after (ghostel evil)
    :hook (ghostel-mode . evil-ghostel-mode))

  ;; ── Bottom drawer, mirroring the vterm-toggle display rule ───────────────
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (with-current-buffer (get-buffer buffer-or-name)
                     (derived-mode-p 'ghostel-mode)))
                 (display-buffer-reuse-window
                  display-buffer-reuse-mode-window
                  display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.33)))

  ;; ── Toggle entry point paralleling vterm's "C-c t" ───────────────────────
  (defun my/ghostel-toggle (&optional arg)
    "Summon or dismiss the ghostel drawer.
With prefix ARG, always create a NEW ghostel buffer (ghostel's own
prefix-arg convention)."
    (interactive "P")
    (if arg
        (ghostel arg)
      (let ((win (seq-find
                  (lambda (w)
                    (with-current-buffer (window-buffer w)
                      (derived-mode-p 'ghostel-mode)))
                  (window-list))))
        (if (and win (not (one-window-p)))
            (delete-window win)      ; dismiss the visible drawer
          (ghostel)))))              ; summon / reuse an existing terminal
  (global-set-key (kbd "C-x m") #'my/ghostel-toggle)  ; shadows `compose-mail'

  ;; ── Project integration ──────────────────────────────────────────────────
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'ghostel-project)
    (define-key project-prefix-map "M" #'ghostel-project-list-buffers)
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
    (add-to-list 'project-switch-commands
                 '(ghostel-project-list-buffers "Ghostel buffers") t))

  ;; ── Desktop save/restore ─────────────────────────────────────────────────
  ;; Registered at top level (NOT in :config) so the handler is present when
  ;; `desktop-save-mode' replays buffers on `after-init-hook' — ghostel stays
  ;; lazily loaded and is pulled in by the autoloaded `ghostel' call inside the
  ;; restore handler.  As with vterm, a live shell cannot survive a restart, so
  ;; we persist each terminal's cwd and respawn a FRESH shell there.  Buffer
  ;; names are title-derived, so restore keys off the directory, not the name.
  (defun my/ghostel-desktop-save (_desktop-dirname)
    "Persist the ghostel's working directory for desktop restore."
    `((default-directory . ,default-directory)
      (buffer-name . ,(buffer-name))))
  (add-hook 'ghostel-mode-hook
            (lambda ()
              (setq-local desktop-save-buffer #'my/ghostel-desktop-save)))
  (defun my/ghostel-desktop-restore (_file-name buffer-name misc)
    "Recreate a ghostel terminal in the saved directory from MISC."
    (let ((default-directory (or (cdr (assq 'default-directory misc))
                                 default-directory))
          (ghostel-buffer-name buffer-name))
      (save-window-excursion (ghostel))))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-buffer-mode-handlers
                 '(ghostel-mode . my/ghostel-desktop-restore))
    (setq desktop-modes-not-to-save
          (delq 'ghostel-mode desktop-modes-not-to-save))))

;; ============================================================================
;; EAT - Non-Windows only (requires POSIX pty: /usr/bin/env, stty, sh)
;; ============================================================================

(unless (eq system-type 'windows-nt)
  (use-package eat
    :config
    (evil-set-initial-state 'eat-mode 'emacs)))

;; ============================================================================
;; EEE - All platforms (eat backend on non-Windows, mintty on Windows)
;; ============================================================================

(use-package eee
  :straight '(eee :type git :host github :repo "eval-exec/eee.el"
                  :files (:defaults "*.el" "*.sh"))
  :config
  (if (eq system-type 'windows-nt) 
      ;; Windows: use alacritty with Git Bash
      (progn
        (setq ee-terminal-command "alacritty")
        ;; Advise ee-run to convert Windows paths to MSYS (/c/...) and
        ;; call bash explicitly (same as running the script manually in Git Bash)
        (advice-add 'ee-run :around
		    (lambda (orig name dir command &rest args)
		      (let ((to-msys (lambda (s)
				       (replace-regexp-in-string
					"^\\([a-zA-Z]\\):/" "/\\1/" s))))
			(apply orig name
			       (funcall to-msys dir)
			       (concat "bash " (funcall to-msys command))
			       args)))))
    ;; Non-Windows: use eat as embedded terminal backend
    ;; (setq ee-start-terminal-function #'ee-eat-start-terminal)
    (setq ee-terminal-command "st"))
  (zzc/leader-keys
    "tf"  '(ee-find :which-key "find")
    "tr"  '(ee-rg :which-key "rg")
    "tg"  '(ee-lazygit :which-key "lazygit")
    "ty"  '(ee-yazi :which-key "yazi")))

(provide 'terminal-config)
;;; terminal-config.el ends here

