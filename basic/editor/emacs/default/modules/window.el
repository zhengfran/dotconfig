;;; window.el --- Window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window management: splits, winner-mode, winum, golden-ratio,
;; font size adjustment, toggle-one-window
;;
;; DEPENDENCIES: evil (for evil-window-* commands)
;; USED BY: None

;;; Code:

;; ============================================================================
;; WINDOW SPLIT FUNCTIONS
;; ============================================================================

(defun split-window-right-and-focus ()
  "Split the window vertically and move focus to the new one."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-and-focus ()
  "Split the window horizontally and move focus to the new one."
  (interactive)
  (split-window-below)
  (other-window 1))

(defadvice split-window-right (after split-window-right-and-focus activate)
  (other-window 1))

(defadvice split-window-below (after split-window-below-and-focus activate)
  (other-window 1))

;; ============================================================================
;; WINNER MODE
;; ============================================================================

(winner-mode 1)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

;; ============================================================================
;; FONT SIZE ADJUSTMENT
;; ============================================================================

;; Font size adjustment keybindings
(global-set-key (kbd "C-=") 'text-scale-increase)  ; Ctrl + = (same key as +)
(global-set-key (kbd "C-+") 'text-scale-increase)  ; Ctrl + + (explicit)
(global-set-key (kbd "C--") 'text-scale-decrease)  ; Ctrl + -
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))  ; Ctrl + 0 to reset

;; ============================================================================
;; WINUM
;; ============================================================================

(use-package winum
  :config
  (winum-mode))

;; ============================================================================
;; GOLDEN RATIO
;; ============================================================================

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  ;; Widescreen support - auto-scale on wide monitors
  (setq golden-ratio-auto-scale t)
  
  ;; Exclude special modes from golden-ratio resizing
  (setq golden-ratio-exclude-modes
        '(ediff-mode
          eshell-mode
          dired-mode
          term-mode
          vterm-mode
          compilation-mode
          ibuffer-mode
          treemacs-mode
          pdf-view-mode
          doc-view-mode
          magit-mode
          magit-status-mode
          magit-log-mode
          magit-diff-mode))
  
  ;; Exclude special buffer names from resizing
  (setq golden-ratio-exclude-buffer-names
        '("*Org Agenda*"
          "*Org Select*"
          "*which-key*"
          "*Treemacs*"
          " *NeoTree*"
          "*Messages*"
          "*Warnings*"))
  
  ;; Exclude buffer name patterns (regex)
  (setq golden-ratio-exclude-buffer-regexp
        '("^\\*helm.*"
          "^\\*Flycheck.*"
          "^\\*Warnings.*"
          "^CAPTURE.*\\.org$"))
  
  ;; Integration with window commands - CRITICAL for golden-ratio to work
  (add-to-list 'golden-ratio-extra-commands 'other-window)
  (add-to-list 'golden-ratio-extra-commands 'select-window-1)
  (add-to-list 'golden-ratio-extra-commands 'select-window-2)
  (add-to-list 'golden-ratio-extra-commands 'select-window-3)
  (add-to-list 'golden-ratio-extra-commands 'select-window-4)
  (add-to-list 'golden-ratio-extra-commands 'select-window-5)
  (add-to-list 'golden-ratio-extra-commands 'select-window-6)
  (add-to-list 'golden-ratio-extra-commands 'select-window-7)
  (add-to-list 'golden-ratio-extra-commands 'select-window-8)
  (add-to-list 'golden-ratio-extra-commands 'select-window-9)
  
  ;; Integration with winum - golden-ratio triggers on window selection
  (dolist (n (number-sequence 1 9))
    (add-to-list 'golden-ratio-extra-commands 
                 (intern (format "winum-select-window-%d" n))))
  
  ;; Integrate with other window navigation commands
  (add-to-list 'golden-ratio-extra-commands 'windmove-left)
  (add-to-list 'golden-ratio-extra-commands 'windmove-right)
  (add-to-list 'golden-ratio-extra-commands 'windmove-up)
  (add-to-list 'golden-ratio-extra-commands 'windmove-down)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  
  ;; Only enable golden-ratio when there are more than 2 windows
  (defun my/golden-ratio-inhibit-few-windows ()
    "Inhibit golden-ratio when there are 2 or fewer windows.
This prevents golden-ratio from activating in simple window layouts."
    (let ((window-count (length (cl-remove-if #'window-dedicated-p (window-list)))))
      (<= window-count 2)))
  
  (add-to-list 'golden-ratio-inhibit-functions 
               'my/golden-ratio-inhibit-few-windows)
  
  ;; Enable golden-ratio globally - MUST be last
  (golden-ratio-mode 1))

;; ============================================================================
;; TOGGLE ONE WINDOW
;; ============================================================================

(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")

(defun toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
      (if toggle-one-window-window-configuration
          (progn
            (set-window-configuration toggle-one-window-window-configuration)
            (setq toggle-one-window-window-configuration nil))
        (message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))

(general-define-key
 :prefix "C-c"
 "m" 'toggle-one-window)

(provide 'window)
;;; window.el ends here
