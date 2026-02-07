;;; buffer-tabs.el --- Frame-level buffer tabs -*- lexical-binding: t; -*-

;;; Commentary:
;; Frame-level buffer tabs using Emacs' built-in tab-bar (spans full width).
;; Project-aware: only shows buffers in the current project.
;; Org-mode #+TITLE: extraction for tab names.
;; Evil keybindings (H/L for tab navigation, K for close).
;;
;; DEPENDENCIES: evil, workspace (project)
;; USED BY: None

;;; Code:

;; ============================================================================
;; ORG TITLE CACHE
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
;; BUFFER FILTERING
;; ============================================================================

(defun my/buffer-tab-hidden-p (buffer)
  "Return non-nil if BUFFER should be hidden from tabs."
  (let ((name (buffer-name buffer)))
    (or
     ;; Invisible buffers (space prefix)
     (string-prefix-p " " name)
     ;; Special buffers
     (string-prefix-p "*scratch" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p "*Warnings" name)
     (string-prefix-p "*Compile-Log" name)
     (string-prefix-p "*Async-native-compile-log" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*helpful" name)
     (string-prefix-p "*Completions" name)
     (string-prefix-p "*Backtrace" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*which-key" name)
     (string-prefix-p "*Treemacs" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*eglot" name)
     (string-prefix-p "*vterm" name)
     (string-prefix-p "*eshell" name)
     (string-prefix-p "magit" name)
     (string-prefix-p "*fava" name)
     (string-prefix-p "*gptel" name)
     ;; Modes to exclude
     (with-current-buffer buffer
       (derived-mode-p 'dired-mode
                       'help-mode
                       'helpful-mode
                       'magit-mode
                       'treemacs-mode
                       'vterm-mode
                       'eshell-mode
                       'term-mode)))))

(defun my/buffer-in-current-project-p (buffer)
  "Return non-nil if BUFFER belongs to the current project."
  (when-let* ((current-project (project-current))
              (current-root (file-truename (expand-file-name (project-root current-project))))
              (buf-file (buffer-file-name buffer)))
    (string-prefix-p current-root (file-truename buf-file))))

(defun my/debug-buffer-project ()
  "Show debug info about current buffer's project status."
  (interactive)
  (let* ((buf (current-buffer))
         (buf-file (buffer-file-name buf))
         (buf-file-true (and buf-file (file-truename buf-file)))
         (current-project (project-current))
         (current-root (and current-project (project-root current-project)))
         (current-root-expanded (and current-root (file-truename (expand-file-name current-root))))
         (in-project (my/buffer-in-current-project-p buf)))
    (message "Buffer: %s\nFile: %s\nFile (truename): %s\nProject root: %s\nProject root (expanded): %s\nIn project: %s"
             (buffer-name buf)
             (or buf-file "No file")
             (or buf-file-true "No file")
             (or current-root "Not in a project")
             (or current-root-expanded "Not in a project")
             (if in-project "YES" "NO"))))

(defun my/visible-buffer-list ()
  "Return list of buffers visible in tabs, filtered by current project.
Only shows buffers that belong to the current project. If not in a project,
shows all non-project buffers."
  (let* ((current-project (project-current))
         (all-buffers (buffer-list))
         (filtered-buffers
          (if current-project
              ;; In a project: show only buffers in this project
              (seq-filter
               (lambda (buf)
                 (and (not (my/buffer-tab-hidden-p buf))
                      (my/buffer-in-current-project-p buf)))
               all-buffers)
            ;; Not in a project: show all non-hidden buffers
            (seq-filter
             (lambda (buf) (not (my/buffer-tab-hidden-p buf)))
             all-buffers))))
    (sort filtered-buffers
          (lambda (a b)
            (string< (buffer-name a) (buffer-name b))))))

;; ============================================================================
;; TAB-BAR BUFFER TABS (FRAME-LEVEL)
;; ============================================================================

(defun my/tab-bar-buffer-tabs ()
  "Generate tab-bar items showing open file buffers.
Each buffer becomes a clickable tab in the frame-level tab-bar."
  (let ((current-buf (window-buffer (selected-window))))
    (mapcar
     (lambda (buf)
       (let* ((name (my/get-org-buffer-name buf))
              (truncated (if (> (length name) 20)
                            (concat (substring name 0 17) "...")
                          name))
              (is-current (eq buf current-buf))
              (is-modified (buffer-modified-p buf))
              (label (concat " "
                             (when is-modified "*")
                             truncated
                             " "))
              (b buf))
         `(,(intern (format "buf-tab-%s" (sxhash buf)))
           menu-item
           ,(propertize label 'face (if is-current
                                        'tab-bar-tab
                                      'tab-bar-tab-inactive))
           ,(lambda () (interactive) (switch-to-buffer b))
           :help ,(buffer-name buf))))
     (my/visible-buffer-list))))

;; Override workspace.el tab-bar settings: show buffer tabs in tab-bar
(setq tab-bar-format '(my/tab-bar-buffer-tabs))
(setq tab-bar-show t)
(setq tab-bar-auto-width nil)
;; Force tab-bar visible (workspace.el set tab-bar-lines to 0)
(add-to-list 'default-frame-alist '(tab-bar-lines . 1))
(set-frame-parameter nil 'tab-bar-lines 1)

;; Ensure tab-bar stays visible after tab switches
(defun my/ensure-tab-bar-visible (&rest _)
  "Ensure tab-bar remains visible after tab operations."
  (setq tab-bar-show t)
  (set-frame-parameter nil 'tab-bar-lines 1))

(advice-add 'tab-bar-select-tab :after #'my/ensure-tab-bar-visible)
(advice-add 'tab-bar-switch-to-tab :after #'my/ensure-tab-bar-visible)
(advice-add 'tab-bar-new-tab :after #'my/ensure-tab-bar-visible)
(advice-add 'tab-bar-close-tab :after #'my/ensure-tab-bar-visible)

;; ============================================================================
;; BUFFER NAVIGATION
;; ============================================================================

(defun my/next-buffer-tab ()
  "Switch to next buffer in tab list."
  (interactive)
  (let* ((bufs (my/visible-buffer-list))
         (len (length bufs))
         (pos (seq-position bufs (current-buffer))))
    (when (and pos (> len 1))
      (switch-to-buffer (nth (mod (1+ pos) len) bufs)))))

(defun my/prev-buffer-tab ()
  "Switch to previous buffer in tab list."
  (interactive)
  (let* ((bufs (my/visible-buffer-list))
         (len (length bufs))
         (pos (seq-position bufs (current-buffer))))
    (when (and pos (> len 1))
      (switch-to-buffer (nth (mod (1- pos) len) bufs)))))

;; ============================================================================
;; EVIL KEYBINDINGS
;; ============================================================================

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "H") 'my/prev-buffer-tab)
  (define-key evil-normal-state-map (kbd "L") 'my/next-buffer-tab)
  (define-key evil-normal-state-map (kbd "K") 'kill-current-buffer))

(provide 'buffer-tabs)
;;; buffer-tabs.el ends here
