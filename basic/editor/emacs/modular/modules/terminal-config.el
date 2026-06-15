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
                   (display-buffer-reuse-window display-buffer-at-bottom)
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

