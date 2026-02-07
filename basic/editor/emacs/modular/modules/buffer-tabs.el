;;; buffer-tabs.el --- Frame-level buffer tabs and mode-line -*- lexical-binding: t; -*-

;;; Commentary:
;; Frame-level buffer tabs using Emacs' built-in tab-bar (spans full width).
;; Frame-level mode-line using a bottom side-window (doesn't split).
;; Org-mode #+TITLE: extraction for tab names.
;; Evil keybindings (H/L for tab navigation, K for close).
;;
;; DEPENDENCIES: evil, ui (doom-modeline)
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

(defvar my/global-modeline-buffer-name " *global-modeline*"
  "Buffer name for the global mode-line side window.")

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

(defun my/visible-buffer-list ()
  "Return list of buffers visible in tabs, sorted by name."
  (sort (seq-filter (lambda (buf) (not (my/buffer-tab-hidden-p buf)))
                    (buffer-list))
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

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

;; ============================================================================
;; GLOBAL MODE-LINE (BOTTOM SIDE WINDOW)
;; ============================================================================

;; Allow 1-line windows (needed for the mode-line side window)
(setq window-min-height 1)

(defun my/global-modeline-setup ()
  "Create a bottom side-window that acts as a frame-level mode-line.
Side windows are not affected by regular split commands."
  (unless (get-buffer-window my/global-modeline-buffer-name t)
    (let ((buf (get-buffer-create my/global-modeline-buffer-name)))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (setq-local truncate-lines t)
        (setq-local cursor-type nil)
        (setq-local mode-line-format nil)
        ;; Remap default face to mode-line so background matches
        (setq-local face-remapping-alist '((default mode-line))))
      (display-buffer-in-side-window buf
        '((side . bottom)
          (slot . 1)
          (window-height . 1)
          (window-parameters
           (mode-line-format . none)
           (header-line-format . none)
           (tab-line-format . none)
           (no-other-window . t)
           (no-delete-other-windows . t)))))))

;; Ensure doom-modeline's selected-window var is treated as dynamic (special)
(defvar doom-modeline--selected-window)

(defun my/global-modeline-update ()
  "Update the global mode-line content from the active window."
  (when-let* ((ml-buf (get-buffer my/global-modeline-buffer-name))
              (ml-win (get-buffer-window ml-buf t))
              (sel-win (if (minibuffer-window-active-p (minibuffer-window))
                           (minibuffer-selected-window)
                         (selected-window)))
              (sel-buf (window-buffer sel-win)))
    ;; Don't update if active window is the modeline itself
    (unless (eq sel-buf ml-buf)
      ;; Use ml-win (full-width side window) for format-mode-line so
      ;; doom-modeline right-aligns to the full frame width, not the
      ;; split window width.  Bind doom-modeline--selected-window to
      ;; ml-win so it still renders in "active" style.
      (let* ((doom-modeline--selected-window ml-win)
             (ml-text (format-mode-line
                       (buffer-local-value 'mode-line-format sel-buf)
                       'mode-line ml-win sel-buf))
             (win-width (window-body-width ml-win)))
        (with-current-buffer ml-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert ml-text)
            ;; Pad to fill full width with mode-line background
            (let ((padding (max 0 (- win-width (current-column)))))
              (insert (propertize (make-string padding ?\s)
                                  'face 'mode-line)))))))))

;; Hide mode-line in ALL regular windows (side window takes over)
(defun my/hide-all-window-modelines ()
  "Hide per-window mode-lines; the global side-window shows mode-line instead."
  ;; Ensure the side window exists
  (my/global-modeline-setup)
  (walk-windows
   (lambda (win)
     (unless (or (window-minibuffer-p win)
                 (eq (window-buffer win)
                     (get-buffer my/global-modeline-buffer-name)))
       (set-window-parameter win 'mode-line-format 'none)))
   nil nil))

(add-hook 'window-configuration-change-hook #'my/hide-all-window-modelines)
(add-hook 'post-command-hook #'my/global-modeline-update)

;; Initialize after frame is ready
(add-hook 'after-init-hook #'my/global-modeline-setup)
;; Also handle daemon/client frames
(add-hook 'server-after-make-frame-hook #'my/global-modeline-setup)

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
