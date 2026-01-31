;;; workspace.el --- Workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Tab-bar workspaces, desktop session save/restore, project management,
;; buffer management
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys), completion (consult-buffer)
;; USED BY: None

;;; Code:

;; ============================================================================
;; PROJECT CONFIGURATION
;; ============================================================================

(use-package project
    ;; Cannot use :hook because 'project-find-functions does not end in -hook
    ;; Cannot use :init (must use :config) because otherwise
    ;; project-find-functions is not yet initialized.
    :config
    (setq project-vc-extra-root-markers '(".project" "*.csproj")))

;; Project leader key shortcuts
(zzc/leader-keys
  "p"   '(:ignore t :which-key "project")
  "p p" '(project-switch-project :which-key "switch project")
  "p f" '(project-find-file :which-key "find file")
  "p d" '(project-find-dir :which-key "find directory")
  "p b" '(project-switch-to-buffer :which-key "switch buffer")
  "p k" '(project-kill-buffers :which-key "kill buffers")
  "p c" '(project-compile :which-key "compile")
  "p s" '(project-shell :which-key "shell")
  "p r" '(project-query-replace-regexp :which-key "replace regexp")
  "p g" '(consult-ripgrep :which-key "find regexp")
  "p D" '(project-dired :which-key "dired root")
  "p !" '(project-shell-command :which-key "shell command")
  "p &" '(project-async-shell-command :which-key "async shell command"))

;; ============================================================================
;; DESKTOP SESSION MANAGEMENT
;; ============================================================================

(use-package desktop
  :init
  ;; Desktop file location
  (setq desktop-path (list user-emacs-directory))
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name "emacs-desktop")
  (setq desktop-base-lock-name "emacs-desktop.lock")

  ;; Restore all buffers immediately (per user preference)
  (setq desktop-restore-eager t)

  ;; Allow loading locked desktop (safe for single-user systems)
  (setq desktop-load-locked-desktop t)

  ;; Auto-save every 5 minutes
  (setq desktop-auto-save-timeout 300)

  ;; Always save desktop on exit without asking
  (setq desktop-save t)
  
  ;; Disable frame restoration to avoid reloading tab workspaces
  (setq desktop-restore-frames nil)
  (setq desktop-restore-in-current-display t)
  (setq desktop-restore-forces-onscreen t)
  
  :config
  ;; Only enable desktop-save-mode in non-daemon mode
  (unless (daemonp)
    (desktop-save-mode 1))
  
  ;; Don't save certain buffer types
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\|\\*helm.*\\*\\|\\*Compile-Log\\*\\|\\*Warnings\\*"
                "\\|\\*scratch\\*"
                "\\)$"))
  
  ;; Files to exclude from desktop save
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

;; ============================================================================
;; TAB-BAR WORKSPACES
;; ============================================================================

(use-package tab-bar
  :init
  ;; Hide tab bar (workspaces managed in background)
  (setq tab-bar-show nil)
  
  ;; Clean appearance - no extra buttons
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  
  ;; Tab behavior
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-close-tab-select 'recent)
  
  ;; Show tab numbers for quick switching
  (setq tab-bar-tab-hints nil)
  
  ;; Tab bar format
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  
  ;; Preserve window configurations when switching tabs
  (setq tab-bar-new-tab-group nil)
  
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)  ; Enable undo/redo for closed tabs
  
  ;; Helper function: Name tab after project
  (defun my/tab-bar-name-from-project ()
    "Name tab after current project root or directory."
    (if-let* ((project (project-current)))
        (file-name-nondirectory
         (directory-file-name
          (project-root project)))
      (file-name-nondirectory
       (directory-file-name default-directory))))
  
  ;; Helper function: Create new tab for project
  (defun my/open-project-in-new-tab ()
    "Open a project in a new tab and name it after the project."
    (interactive)
    (tab-bar-new-tab)
    (call-interactively 'project-switch-project)
    (tab-bar-rename-tab (my/tab-bar-name-from-project))
    ;; Store project root in tab configuration
    (when-let* ((project (project-current)))
      (set-frame-parameter nil 
                          (intern (format "tab-project-%s" (tab-bar--current-tab-index)))
                          (project-root project))))
  
  ;; Get the project root associated with current workspace
  (defun my/workspace-project-root ()
    "Get the project root for the current workspace."
    (frame-parameter nil (intern (format "tab-project-%s" (tab-bar--current-tab-index)))))
  
  ;; Project-aware consult-buffer for workspaces
  (defun my/consult-project-buffer ()
    "Show buffers filtered by current workspace's project."
    (interactive)
    (if-let* ((project-root (my/workspace-project-root)))
        (let ((default-directory project-root)
              (consult-buffer-sources '(consult--source-project-buffer-hidden
                                       consult--source-project-buffer
                                       consult--source-project-recent-file)))
          (consult-buffer))
      (consult-buffer)))
  
  ;; Desktop save/load helpers without prompting
  (defun my/desktop-save-default ()
    "Save desktop to default directory without prompting."
    (interactive)
    (desktop-save desktop-dirname))
  
  (defun my/desktop-load-default ()
    "Load desktop from default directory without prompting."
    (interactive)
    (desktop-read desktop-dirname))
  
  ;; Key bindings with SPC w prefix
  (zzc/leader-keys
    "w"   '(:ignore t :which-key "workspace")
    "w w" '(tab-bar-switch-to-tab :which-key "switch workspace")
    "w n"   '(tab-bar-new-tab :which-key "new workspace")
    "w c"   '(tab-bar-close-tab :which-key "close workspace")
    "w C"   '(tab-bar-close-other-tabs :which-key "close other workspaces")
    "w r"   '(tab-bar-rename-tab :which-key "rename workspace")
    "w ]"   '(tab-bar-switch-to-next-tab :which-key "next workspace")
    "w ["   '(tab-bar-switch-to-prev-tab :which-key "previous workspace")
    "w u"   '(tab-bar-undo-close-tab :which-key "undo close workspace")
    "w ."   '(tab-bar-switch-to-recent-tab :which-key "recent workspace")
    "w p"   '(my/open-project-in-new-tab :which-key "open project in new tab")
    "w R"   '(tab-bar-rename-tab-by-name :which-key "rename workspace by name")
    "w s"   '(my/desktop-save-default :which-key "save session")
    "w S"   '(desktop-save-in-desktop-dir :which-key "save session now")
    "w l"   '(my/desktop-load-default :which-key "load session")
    "w k"   '(desktop-clear :which-key "clear session")))

;; Workspace number selection functions
(defun my/tab-bar-select-tab-1 () (interactive) (tab-bar-select-tab 1))
(defun my/tab-bar-select-tab-2 () (interactive) (tab-bar-select-tab 2))
(defun my/tab-bar-select-tab-3 () (interactive) (tab-bar-select-tab 3))
(defun my/tab-bar-select-tab-4 () (interactive) (tab-bar-select-tab 4))
(defun my/tab-bar-select-tab-5 () (interactive) (tab-bar-select-tab 5))
(defun my/tab-bar-select-tab-6 () (interactive) (tab-bar-select-tab 6))
(defun my/tab-bar-select-tab-7 () (interactive) (tab-bar-select-tab 7))
(defun my/tab-bar-select-tab-8 () (interactive) (tab-bar-select-tab 8))
(defun my/tab-bar-select-tab-9 () (interactive) (tab-bar-select-tab 9))

(zzc/leader-keys
  "w 1" '(my/tab-bar-select-tab-1 :which-key "workspace 1")
  "w 2" '(my/tab-bar-select-tab-2 :which-key "workspace 2")
  "w 3" '(my/tab-bar-select-tab-3 :which-key "workspace 3")
  "w 4" '(my/tab-bar-select-tab-4 :which-key "workspace 4")
  "w 5" '(my/tab-bar-select-tab-5 :which-key "workspace 5")
  "w 6" '(my/tab-bar-select-tab-6 :which-key "workspace 6")
  "w 7" '(my/tab-bar-select-tab-7 :which-key "workspace 7")
  "w 8" '(my/tab-bar-select-tab-8 :which-key "workspace 8")
  "w 9" '(my/tab-bar-select-tab-9 :which-key "workspace 9"))

;; ============================================================================
;; BUFFER MANAGEMENT
;; ============================================================================

(defun my-scratch-buffer-no-save ()
  "Prevent any buffer with *scratch* in its name from being marked as modified."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*scratch\\*" (buffer-name buf))
      (with-current-buffer buf
        (set-buffer-modified-p nil)))))

(add-hook 'after-change-functions (lambda (&rest _) (my-scratch-buffer-no-save)))

(zzc/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bp"  '(switch-to-prev-buffer :which-key "previous buffer")
  "bn"  '(switch-to-next-buffer :which-key "next buffer")
  "bb"  '(switch-to-buffer :which-key "list buffers")
  "bB"  '(ibuffer-list-buffers :which-key "list ibuffers")
  "bk"  '(kill-current-buffer :which-key "kill current buffer")
  "bs"  '(save-buffer :which-key "save buffer"))

(provide 'workspace)
;;; workspace.el ends here
