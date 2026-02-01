;;; terminal-config.el --- Terminal Configuration (vterm, eee) -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal emulator configuration using vterm and eee
;; Only loads on non-Windows systems (vterm doesn't work on Windows)
;;
;; DEPENDENCIES: core (system detection vars), evil (evil-set-initial-state),
;;               keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

(unless (eq system-type 'windows-nt)
  ;; Vterm - full-featured terminal emulator
  (use-package vterm
    :commands vterm
    :config
    ;; Speed up vterm
    (setq vterm-max-scrollback 10000)
    (setq vterm-buffer-name-string "vterm %s")
    
    ;; Disable line numbers in vterm
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))
    
    ;; Fix evil mode integration
    (evil-set-initial-state 'vterm-mode 'emacs)
    
    ;; Keybindings for better navigation
    (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
    (define-key vterm-mode-map (kbd "M-<left>") #'vterm-send-left)
    (define-key vterm-mode-map (kbd "M-<right>") #'vterm-send-right)
    
    ;; Better copy mode
    (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode)
    
    ;; Custom settings
    :custom
    (vterm-shell (or (executable-find "zsh") (getenv "SHELL") "/bin/bash"))
    (vterm-kill-buffer-on-exit t)
    (vterm-clear-scrollback-when-clearing t))

  ;; Multi-vterm for managing multiple vterm buffers
  (use-package multi-vterm
    :commands (multi-vterm multi-vterm-project)
    :config
    ;; Set the default shell
    (setq multi-vterm-program (or (executable-find "zsh") (getenv "SHELL") "/bin/bash"))
    
    ;; Dedicated vterm window
    (setq multi-vterm-dedicated-window-height-percent 30)
    
    :bind
    (:map vterm-mode-map
          ("C-c C-n" . multi-vterm-next)
          ("C-c C-p" . multi-vterm-prev)))

  ;; vterm-toggle for quick terminal popup
  (use-package vterm-toggle
    :after vterm
    :config
    ;; Show vterm buffer in bottom side window
    (setq vterm-toggle-fullscreen-p nil)
    (add-to-list 'display-buffer-alist
                 '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))
    
    :bind
    (("C-`" . vterm-toggle)
     ("C-~" . vterm-toggle-cd)))

  ;; Key bindings with leader key
  (zzc/leader-keys
    "v"  '(:ignore t :which-key "vterm")
    "vv"  '(vterm :which-key "open vterm")
    "vn"  '(multi-vterm :which-key "new vterm")
    "vp"  '(multi-vterm-project :which-key "vterm in project")
    "vt"  '(vterm-toggle :which-key "toggle vterm")
    "vd"  '(vterm-toggle-cd :which-key "toggle vterm in current dir"))

  ;; EEE - External terminal launcher (kitty, wezterm, etc.)
  (use-package eee
    :straight '(eee :type git :host github :repo "eval-exec/eee.el"
                    :files (:defaults "*.el" "*.sh"))
    :config
    ;; Set terminal command based on system
    (when my/is-WSL
      (setq ee-terminal-command "kitty")
      (setq ee-terminal-options
          '(("kitty" . "--class scratchterm"))))
    (when my/is-mac
      (setq ee-terminal-command "/Applications/Ghostty.app/Contents/MacOS/ghostty")
      (setq ee-terminal-options
          '(("ghostty" . "--title=ee-ghostty --window-decoration=none"))))
    
    ;; Set custom kitty options with --class scratchterm (overwrite default)
    
    ;; Keybindings
    (zzc/leader-keys
      "tf"  '(ee-find :which-key "find")
      "tr"  '(ee-rg :which-key "rg")
      "tg"  '(ee-lazygit :which-key "lazygit")
      "ty"  '(ee-yazi :which-key "yazi"))))  ; closes use-package eee AND unless block

(provide 'terminal-config)
;;; terminal-config.el ends here
