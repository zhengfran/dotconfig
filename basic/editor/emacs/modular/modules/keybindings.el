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
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

;; ============================================================================
;; CONFIG RELOAD - RESTART EMACS
;; ============================================================================

(use-package restart-emacs
  :commands restart-emacs)

(defun my/reload-emacs-config ()
  "Restart Emacs to reload configuration.
Restarting is more reliable than in-place reloading."
  (interactive)
  (when (y-or-n-p "Restart Emacs to reload configuration? ")
    (restart-emacs)))

;; Modules in the same dependency order as init.el. Keep in sync if init.el
;; changes. Used by `my/reload-config-in-place'.
(defvar my/config-modules
  '(core keybindings ui editor evil window-config completion bookmarks
    workspace navigation snippets org-base org-agenda-config denote-config
    habit-tracker blog org-babel buffer-tabs programming ai beancount
    rime-config terminal-config)
  "Ordered list of config module features for in-place reload.")

(defun my/reload-config-in-place ()
  "Re-load all config modules WITHOUT restarting Emacs.
Re-evaluates init.el's custom file and every module in dependency order.
Faster than `my/reload-emacs-config' for iterating on settings, but note:
some changes (removed bindings/hooks, package :init, native modules) only
take full effect after a real restart (`SPC r r')."
  (interactive)
  ;; Reload custom.el first (mirrors init.el).
  (when (and (boundp 'custom-file) custom-file (file-exists-p custom-file))
    (load custom-file 'noerror 'nomessage))
  (let ((ok 0) (fail 0))
    (dolist (mod my/config-modules)
      (let ((file (locate-file (symbol-name mod)
                               load-path '(".el" ".el.gz"))))
        (condition-case err
            (progn
              ;; `load' re-evaluates even if already provided, unlike `require'.
              (if file
                  (load file nil 'nomessage)
                (require mod))
              (setq ok (1+ ok)))
          (error
           (setq fail (1+ fail))
           (message "Reload error in %s: %s" mod (error-message-string err))))))
    (message "Config reloaded in place: %d modules ok, %d failed%s"
             ok fail
             (if (> fail 0) " (see *Messages*; SPC r r for a clean restart)" ""))))

;; Keybindings for config reload
(defun my/yank-file-path ()
  "Copy current file's full path to kill ring and clipboard."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (progn
        (kill-new file)
        (message "Copied: %s" file))
    (message "No file associated with current buffer")))

(zzc/leader-keys
  "f"  '(:ignore t :which-key "file")
  "fy" '(my/yank-file-path :which-key "yank path"))

(zzc/leader-keys
  "r"  '(:ignore t :which-key "reload")
  "rr" '(my/reload-emacs-config :which-key "restart & reload")
  "rc" '(my/reload-config-in-place :which-key "reload config (no restart)"))

(provide 'keybindings)
;;; keybindings.el ends here
