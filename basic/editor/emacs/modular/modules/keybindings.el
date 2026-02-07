;;; keybindings.el --- Keybinding framework setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up which-key and general.el framework for leader key bindings.
;; Individual modules will add their specific keybindings using zzc/leader-keys.
;; Also includes config reload functions.
;;
;; DEPENDENCIES: None
;; USED BY: ui, editor, bookmarks, workspace, navigation, snippets,
;;          org-agenda, denote, org-babel, ai, terminal (zzc/leader-keys)

;;; Code:

;; ============================================================================
;; WHICH-KEY
;; ============================================================================

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  ;; Use minibuffer to avoid conflict with global modeline side-window
  (setq which-key-popup-type 'minibuffer))

;; ============================================================================
;; GENERAL.EL - LEADER KEY FRAMEWORK
;; ============================================================================

(use-package general
  :demand t
  :config
  (general-create-definer zzc/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; ============================================================================
;; CONFIG RELOAD - RESTART EMACS
;; ============================================================================

(use-package restart-emacs
  :commands restart-emacs)

(defun my/reload-emacs-config ()
  "Restart Emacs to reload configuration.
Restarting is more reliable than in-place reloading."
  (interactive)
  (when (yes-or-no-p "Restart Emacs to reload configuration? ")
    (restart-emacs)))

;; Keybindings for config reload
(zzc/leader-keys
  "r"  '(:ignore t :which-key "reload")
  "rr" '(my/reload-emacs-config :which-key "restart & reload"))

(provide 'keybindings)
;;; keybindings.el ends here
