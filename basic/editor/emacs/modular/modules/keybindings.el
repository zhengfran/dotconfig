;;; keybindings.el --- Keybinding framework setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up which-key and general.el framework for leader key bindings.
;; Individual modules will add their specific keybindings using zzc/leader-keys.
;; Also includes config reload functions.
;;
;; DEPENDENCIES: None
;; USED BY: ui, editor, bookmarks, workspace, navigation, snippets, 
;;          org-agenda, org-roam, org-babel, ai, terminal (zzc/leader-keys)

;;; Code:

;; ============================================================================
;; WHICH-KEY
;; ============================================================================

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; ============================================================================
;; GENERAL.EL - LEADER KEY FRAMEWORK
;; ============================================================================

(use-package general
  :config
  (general-create-definer zzc/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; ============================================================================
;; CONFIG RELOAD FUNCTIONS
;; ============================================================================

(defun my/reload-emacs-config ()
    "Reload Emacs configuration by loading init.el.
This is useful after making changes to config.org and tangling it."
    (interactive)
    (message "Reloading Emacs configuration...")
    (load-file (expand-file-name "init.el" user-emacs-directory))
    (message "Emacs configuration reloaded successfully!"))

(defun my/tangle-and-reload-config ()
  "Tangle config.org and then reload the configuration.
This is useful when you've edited config.org and want to apply changes immediately."
  (interactive)
  (if (and (buffer-file-name)
           (string-match-p "config\\.org$" (buffer-file-name)))
      (progn
        (message "Tangling config.org...")
        (org-babel-tangle)
        (message "Config tangled successfully!")
        (my/reload-emacs-config))
    (message "Current buffer is not config.org. Reloading init.el anyway...")
    (my/reload-emacs-config)))

;; Keybindings for config reload
(zzc/leader-keys
  "r"  '(:ignore t :which-key "reload")
  "rr" '(my/reload-emacs-config :which-key "reload config")
  "rt" '(my/tangle-and-reload-config :which-key "tangle & reload"))

(provide 'keybindings)
;;; keybindings.el ends here
