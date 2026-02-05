;;; buffer-tabs.el --- Buffer tab navigation with centaur-tabs -*- lexical-binding: t; -*-

;;; Commentary:
;; Centaur-tabs configuration with:
;; - Project-based tab grouping
;; - Org-mode #+TITLE: extraction for tab names
;; - Evil keybindings (H/L/K for navigation)
;;
;; DEPENDENCIES: evil
;; USED BY: None

;;; Code:

;; ============================================================================
;; ORG TITLE CACHE (preserved from tab-line config)
;; ============================================================================

(defvar my/org-title-cache (make-hash-table :test 'equal)
  "Cache for org-mode buffer titles.")

(defun my/clear-org-title-cache ()
  "Clear cached org title for current buffer."
  (when (eq major-mode 'org-mode)
    (remhash (buffer-file-name) my/org-title-cache)))

(add-hook 'after-save-hook #'my/clear-org-title-cache)

(defun my/extract-org-title ()
  "Extract #+TITLE: property from current org buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun my/get-org-buffer-name (buffer)
  "Get display name for org-mode BUFFER using #+TITLE: if available."
  (with-current-buffer buffer
    (if (not (eq major-mode 'org-mode))
        (buffer-name buffer)
      (let* ((file (buffer-file-name))
             (cached (and file (gethash file my/org-title-cache))))
        (or cached
            (let ((title (my/extract-org-title)))
              (when (and title file)
                (puthash file title my/org-title-cache))
              (or title (buffer-name buffer))))))))

;; ============================================================================
;; CENTAUR-TABS CONFIGURATION
;; ============================================================================

;; Suppress "tab-line-format set outside of tab-line-mode" warning
;; Centaur-tabs uses the tab-line display area but manages it independently
(add-to-list 'warning-suppress-types '(tab-line))
(add-to-list 'warning-suppress-log-types '(tab-line))

(use-package centaur-tabs
  :demand t
  :custom
  ;; Appearance
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-height 32)
  (centaur-tabs-set-close-button t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-set-bar 'under)

  ;; Modified indicator
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")

  ;; Icons disabled (set to t if you have all-the-icons fonts installed)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)

  :config
  (centaur-tabs-mode t)

  ;; No grouping - all tabs in single flat list
  (defun my/centaur-tabs-buffer-groups ()
    "Put all buffers in a single group (no grouping)."
    '("All"))

  (setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)

  ;; Custom tab label function with org-title support
  (defun my/centaur-tabs-label (tab)
    "Return a label for TAB with org-title support and truncation."
    (let* ((buf (car tab))
           (name (my/get-org-buffer-name buf))
           (max-len 20))
      (if (> (length name) max-len)
          (concat (substring name 0 (- max-len 3)) "...")
        name)))

  (setq centaur-tabs-tab-label-function #'my/centaur-tabs-label)

  ;; Exclude buffers
  (setq centaur-tabs-excluded-prefixes
        '(" *" "*scratch" "*Messages" "*Warnings" "*Compile-Log"
          "*Help" "*Completions" "*Backtrace" "*Flycheck"
          "*which-key" "*Treemacs" "*lsp" "*eglot"))

  ;; Hide tabs in certain modes
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'helpful-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'help-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'magit-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'treemacs-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'vterm-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'eshell-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'term-mode-hook 'centaur-tabs-local-mode))

;; ============================================================================
;; EVIL KEYBINDINGS
;; ============================================================================

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "H") 'centaur-tabs-backward)
  (define-key evil-normal-state-map (kbd "L") 'centaur-tabs-forward)
  (define-key evil-normal-state-map (kbd "K") 'kill-current-buffer))

(provide 'buffer-tabs)
;;; buffer-tabs.el ends here
