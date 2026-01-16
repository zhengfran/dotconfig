;;; buffer-tabs.el --- Buffer tab navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tab-line mode for per-window buffer tabs with:
;; - Evil H/L navigation in normal mode
;; - Org-mode #+TITLE: extraction for tab names
;; - Exclusion of special buffers
;; - Desktop session integration
;;
;; DEPENDENCIES: evil (for keybindings), org-base (org-mode)
;; USED BY: None

;;; Code:

;; ============================================================================
;; TAB-LINE CONFIGURATION
;; ============================================================================

(use-package tab-line
  :init
  (setq tab-line-close-button-show nil)        ; No close buttons
  (setq tab-line-new-button-show nil)          ; No new tab button
  (setq tab-line-separator "")                 ; Separator between tabs
  (setq tab-line-tab-name-truncated-max 20)    ; Max 20 chars per tab
  
  ;; Exclude special modes/buffers
  (setq tab-line-exclude-modes
        '(completion-list-mode
          help-mode
          helpful-mode
          Info-mode
          magit-mode
          magit-status-mode
          magit-log-mode
          magit-diff-mode
          treemacs-mode
          vterm-mode
          eshell-mode
          shell-mode
          term-mode
          compilation-mode
          messages-buffer-mode))
  
  :config
  ;; Exclude special buffers by name pattern
  (defun my/tab-line-exclude-buffer ()
    "Exclude special buffers from showing tab-line."
    (let ((name (buffer-name)))
      (or (string-prefix-p " " name)           ; Invisible buffers
          (string-prefix-p "*scratch" name)
          (string-prefix-p "*Messages" name)
          (string-prefix-p "*Warnings" name)
          (string-prefix-p "*Compile-Log" name)
          (string-prefix-p "*Help" name)
          (string-prefix-p "*Completions" name)
          (string-prefix-p "*Backtrace" name)
          (string-prefix-p "*Flycheck" name)
          (string-prefix-p "*which-key" name)
          (string-prefix-p "*Treemacs" name))))
  
  ;; Set exclusion function
  (setq tab-line-exclude-function 'my/tab-line-exclude-buffer)
  
  ;; Global enable
  (global-tab-line-mode 1))

;; ============================================================================
;; ORG-MODE TITLE EXTRACTION
;; ============================================================================

(defvar my/org-title-cache (make-hash-table :test 'equal)
  "Cache for org-mode buffer titles to avoid repeated parsing.")

(defun my/clear-org-title-cache ()
  "Clear the org title cache for current buffer."
  (remhash (buffer-file-name) my/org-title-cache))

(defun my/extract-org-title ()
  "Extract #+TITLE: property from current org buffer.
Returns title string or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun my/get-org-buffer-name (buffer)
  "Get display name for org-mode BUFFER.
Uses #+TITLE: if present, otherwise buffer name.
Caches results for performance."
  (if (with-current-buffer buffer (derived-mode-p 'org-mode))
      (let* ((file (buffer-file-name buffer))
             (cached (when file (gethash file my/org-title-cache))))
        (if cached
            cached
          (with-current-buffer buffer
            (let ((title (my/extract-org-title)))
              (when (and title file)
                (puthash file title my/org-title-cache))
              (or title (buffer-name buffer))))))
    (buffer-name buffer)))

;; Clear cache when org buffer is modified and saved
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/clear-org-title-cache nil t)))

;; ============================================================================
;; CUSTOM TAB NAME FORMATTER
;; ============================================================================

(defun my/tab-line-name-buffer (buffer &optional _buffers)
  "Format tab name for BUFFER.
Shows org-mode titles, modified indicator, and truncates to 20 chars."
  (let* ((name (my/get-org-buffer-name buffer))
         (modified (if (buffer-modified-p buffer) "* " ""))
         (full-name (concat modified name))
         (max-len tab-line-tab-name-truncated-max))
    (if (> (length full-name) max-len)
        (concat (substring full-name 0 (- max-len 3)) "...")
      full-name)))

(setq tab-line-tab-name-function #'my/tab-line-name-buffer)

;; ============================================================================
;; EVIL KEYBINDINGS (H/L NAVIGATION)
;; ============================================================================

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "H") 'tab-line-switch-to-prev-tab)
  (define-key evil-normal-state-map (kbd "L") 'tab-line-switch-to-next-tab))

;; ============================================================================
;; DESKTOP SESSION INTEGRATION
;; ============================================================================

;; Tab-line state is automatically saved with desktop.el
;; Buffers are restored, and tab-line reconstructs tabs automatically
;; No additional configuration needed

(provide 'buffer-tabs)
;;; buffer-tabs.el ends here
