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
(fringe-mode '(0 . 0)) ;;no fringe (lijigang-style)

;; ============================================================================
;; BASIC MODES
;; ============================================================================

(column-number-mode)
(global-hl-line-mode)
(global-visual-line-mode)

;; ============================================================================
;; LINE NUMBERS
;; ============================================================================

;; Lijigang-style: no line numbers globally; use SPC t l to toggle per-buffer.
(setq display-line-numbers-type nil)
(setq-default display-line-numbers-width-start t)

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

(defun my/delete-current-file ()
  "Delete the file associated with the current buffer, then kill the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "Buffer is not visiting a file"))
    (when (y-or-n-p (format "Delete %s? " file))
      (delete-file file)
      (kill-buffer))))

(defun my/rename-current-file (new-name)
  "Rename the file visited by the current buffer to NEW-NAME.
Move the file on disk and switch the buffer to visit the new path."
  (interactive
   (let ((file (or (buffer-file-name)
                   (error "Buffer is not visiting a file"))))
     (list (read-file-name "Rename to: " (file-name-directory file)
                           nil nil (file-name-nondirectory file)))))
  (let ((old-name (buffer-file-name)))
    (when (and (file-exists-p new-name)
               (not (string= old-name new-name)))
      (unless (y-or-n-p (format "%s exists; overwrite? " new-name))
        (error "Rename aborted")))
    (rename-file old-name new-name 1)
    (set-visited-file-name new-name t t)
    (message "Renamed to %s" new-name)))

(zzc/leader-keys
 "f"  '(:ignore t :which-key "file")
 "fd" '(my/delete-current-file :which-key "delete file")
 "fr" '(my/rename-current-file :which-key "rename file"))

(provide 'editor)
;;; editor.el ends here
