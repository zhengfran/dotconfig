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

(defun my/tab-terminal-p (&optional buffer)
  "Return non-nil if BUFFER (default current) is a terminal buffer.
Terminals (vterm/eshell/term) form their own tab group rather than
being hidden, so they can be navigated like iTerm-style tabs."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'vterm-mode 'eshell-mode 'term-mode)))

(defun my/buffer-tab-hidden-p (buffer)
  "Return non-nil if BUFFER should be hidden from tabs entirely.
Note: terminal buffers are NOT hidden here; they are handled as a
separate group (see `my/tab-terminal-p')."
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
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*eglot" name)
     (string-prefix-p "magit" name)
     (string-prefix-p "*fava" name)
     (string-prefix-p "*gptel" name)
     ;; Modes to exclude
     (with-current-buffer buffer
       (derived-mode-p 'dired-mode
                       'help-mode
                       'helpful-mode
                       'magit-mode)))))

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

(defvar my/tab-buffer-order nil
  "Ordered list of buffers as they first appeared on the tab-line.
Used to keep a stable tab order: existing buffers keep their position,
newly-seen buffers are appended to the end, dead buffers are dropped.")

(defun my/tab-stable-order (buffers)
  "Return BUFFERS in a stable tab order.
Known buffers keep their recorded position in `my/tab-buffer-order';
buffers not yet recorded are appended (in BUFFERS' order).  The order
list only ever drops dead buffers, so switching project/group does not
reshuffle existing tabs, and the normal/terminal groups don't clobber
each other's positions."
  ;; Record any newly-seen buffers at the end.
  (dolist (buf buffers)
    (unless (memq buf my/tab-buffer-order)
      (setq my/tab-buffer-order (append my/tab-buffer-order (list buf)))))
  ;; Forget buffers that have been killed.
  (setq my/tab-buffer-order
        (seq-filter #'buffer-live-p my/tab-buffer-order))
  ;; Project BUFFERS onto the recorded order.
  (seq-filter (lambda (b) (memq b buffers)) my/tab-buffer-order))

(defun my/visible-buffer-list ()
  "Return list of NORMAL buffers visible in tabs, filtered by current project.
Excludes hidden buffers and terminal buffers (terminals are their own
group, see `my/tab-terminal-p').  In a project, only buffers belonging to
that project are shown; otherwise all non-hidden, non-terminal buffers."
  (let* ((current-project (project-current))
         (all-buffers (buffer-list))
         (filtered-buffers
          (if current-project
              ;; In a project: show only buffers in this project
              (seq-filter
               (lambda (buf)
                 (and (not (my/buffer-tab-hidden-p buf))
                      (not (my/tab-terminal-p buf))
                      (my/buffer-in-current-project-p buf)))
               all-buffers)
            ;; Not in a project: show all non-hidden, non-terminal buffers
            (seq-filter
             (lambda (buf)
               (and (not (my/buffer-tab-hidden-p buf))
                    (not (my/tab-terminal-p buf))))
             all-buffers))))
    ;; Stable order: existing tabs stay put, new buffers append to the end.
    (my/tab-stable-order filtered-buffers)))

;; ============================================================================
;; TWO-LINE TABS: tab-bar = WORKSPACES, tab-line = BUFFERS
;; ============================================================================

;; ── Line 1: tab-bar shows WORKSPACES (native tabs) ──────────────────────────
;; Restore workspace tabs in the frame-level tab-bar (workspace.el hid it).
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-show t)
(setq tab-bar-auto-width nil)
;; Force tab-bar visible (workspace.el set tab-bar-lines to 0)
(add-to-list 'default-frame-alist '(tab-bar-lines . 1))
(set-frame-parameter nil 'tab-bar-lines 1)

;; ── Line 2: tab-line shows BUFFERS in the current workspace ─────────────────
;; Reuse the project-aware buffer list and org #+TITLE: naming from above.

;; Per-workspace terminals: tag each terminal buffer with the tab-bar
;; workspace it was created in, then show only this workspace's terminals
;; in the terminal tab group.
(defvar-local my/tab-terminal-workspace nil
  "Name of the tab-bar workspace a terminal buffer belongs to.")

(defun my/current-workspace-name ()
  "Return the current tab-bar workspace (tab) name, or nil."
  (when (fboundp 'tab-bar-tab-name-current)
    (ignore-errors (tab-bar-tab-name-current))))

(defun my/tag-terminal-workspace ()
  "Tag the current terminal buffer with the active workspace name.
Added to terminal mode hooks so each terminal remembers where it was born."
  (when (my/tab-terminal-p)
    (setq my/tab-terminal-workspace (my/current-workspace-name))))

(add-hook 'vterm-mode-hook  #'my/tag-terminal-workspace)
(add-hook 'eshell-mode-hook #'my/tag-terminal-workspace)
(add-hook 'term-mode-hook   #'my/tag-terminal-workspace)

(defun my/terminal-in-current-workspace-p (buffer)
  "Return non-nil if terminal BUFFER belongs to the current workspace.
Untagged terminals (no recorded workspace) are shown everywhere so they
never silently disappear."
  (let ((tag (buffer-local-value 'my/tab-terminal-workspace buffer)))
    (or (null tag)
        (equal tag (my/current-workspace-name)))))

(defun my/terminal-buffer-list ()
  "Return live terminal buffers for the current workspace, in stable order."
  (my/tab-stable-order
   (seq-filter (lambda (b)
                 (and (my/tab-terminal-p b)
                      (my/terminal-in-current-workspace-p b)))
               (buffer-list))))

(defun my/tab-line-buffers ()
  "Return buffers for the tab-line, grouped by the current buffer's type.
- In a terminal buffer: show only terminal buffers (iTerm-style group).
- Otherwise: show the project-filtered normal buffer list.
The current buffer is always included even if filtering would drop it."
  (let ((bufs (if (my/tab-terminal-p)
                  (my/terminal-buffer-list)
                (my/visible-buffer-list))))
    (if (memq (current-buffer) bufs)
        bufs
      (cons (current-buffer) bufs))))

(defun my/tab-line-buffer-name (buffer &optional _buffers)
  "Display name for BUFFER on the tab-line (uses org #+TITLE: when available)."
  (let ((name (my/get-org-buffer-name buffer)))
    (concat " "
            (when (buffer-modified-p buffer) "*")
            (if (> (length name) 20)
                (concat (substring name 0 17) "...")
              name)
            " ")))

(use-package tab-line
  :init
  (setq tab-line-tabs-function #'my/tab-line-buffers)
  (setq tab-line-tab-name-function #'my/tab-line-buffer-name)
  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  (setq tab-line-separator " | ")
  :config
  (global-tab-line-mode 1))

;; ============================================================================
;; BUFFER NAVIGATION
;; ============================================================================

(defun my/next-buffer-tab ()
  "Switch to the next buffer following the tab-line display order.
Stays within the current tab group (normal vs terminal)."
  (interactive)
  (let* ((bufs (tab-line-tabs-window-buffers))
         (len (length bufs))
         (pos (seq-position bufs (current-buffer))))
    (when (and pos (> len 1))
      (tab-line-select-tab-buffer (nth (mod (1+ pos) len) bufs)
                                  (selected-window)))))

(defun my/prev-buffer-tab ()
  "Switch to the previous buffer following the tab-line display order.
Stays within the current tab group (normal vs terminal)."
  (interactive)
  (let* ((bufs (tab-line-tabs-window-buffers))
         (len (length bufs))
         (pos (seq-position bufs (current-buffer))))
    (when (and pos (> len 1))
      (tab-line-select-tab-buffer (nth (mod (1- pos) len) bufs)
                                  (selected-window)))))

;; ============================================================================
;; SKIP BORING BUFFERS IN NATIVE next-buffer / previous-buffer
;; ============================================================================
;; Make C-x <left>/<right> (and any switch-to-prev/next-buffer caller) skip
;; the same buffers that are hidden from tabs, and keep terminals separate
;; from normal buffers. Reuses the predicates above for a single source
;; of truth.

(defun my/switch-to-prev-buffer-skip (_win buf _bury-or-kill)
  "Predicate for `switch-to-prev-buffer-skip'.
Skip BUF when navigating so that:
- from a terminal, only other terminals are reachable;
- from a normal buffer, terminals and hidden buffers are skipped."
  (if (my/tab-terminal-p)
      (not (my/tab-terminal-p buf))
    (or (my/buffer-tab-hidden-p buf)
        (my/tab-terminal-p buf))))

(setq switch-to-prev-buffer-skip #'my/switch-to-prev-buffer-skip)

;; ============================================================================
;; EVIL KEYBINDINGS
;; ============================================================================

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "H") 'my/prev-buffer-tab)
  (define-key evil-normal-state-map (kbd "L") 'my/next-buffer-tab)
  (define-key evil-normal-state-map (kbd "K") 'kill-current-buffer))

(provide 'buffer-tabs)
;;; buffer-tabs.el ends here
