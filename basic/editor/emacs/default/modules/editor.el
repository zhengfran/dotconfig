;;; editor.el --- Basic editor behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Core editor settings: line numbers, visual settings, encoding, 
;; basic modes, and helper functions
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; STARTUP AND FRAME SETTINGS
;; ============================================================================

(setq inhibit-startup-message t)
(setq frame-resize-pixelwise t)
(scroll-bar-mode -1) ;;disable visusal scroll bar
(tool-bar-mode -1) ;;disable tool bar
(tooltip-mode -1) ;;disable tool tips
(menu-bar-mode -1) ;;disable menu bar
(set-fringe-mode 10) ;;Give some breathing room

;; ============================================================================
;; BASIC MODES
;; ============================================================================

(column-number-mode)
(global-hl-line-mode)
(global-visual-line-mode)

;; ============================================================================
;; LINE NUMBERS
;; ============================================================================

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width-start t)

;; Disable line numbers for some modes
(dolist (mode
         '(org-mode-hook
           term-mode-hook
           eshell-mode-hook
           dired-mode-hook
           helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(defun doom/toggle-line-numbers ()
  "Toggle line numbers.
  Cycles through regular, relative and no line numbers. The order depends on what
  `display-line-numbers-type' is set to. If you're using Emacs 26+, and
  visual-line-mode is on, this skips relative and uses visual instead.
  See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles
          `(t
            ,(if visual-line-mode
                 'visual
               'relative)
            nil))
         (order
          (cons
           display-line-numbers-type
           (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next
          (if (= (length queue) 1)
              (car order)
            (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (setq display-line-numbers-width-start t)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

(zzc/leader-keys
 "tl"
 '(doom/toggle-line-numbers :which-key "toggle line numbers"))

;; ============================================================================
;; ENCODING
;; ============================================================================

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
   (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

(provide 'editor)
;;; editor.el ends here
