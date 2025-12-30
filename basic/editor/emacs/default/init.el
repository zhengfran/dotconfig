;; -*- lexical-binding: t; -*-
;;show show errors
(setq warning-minimum-level :error)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;disable bell
(setq visible-bell 1)
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

(defvar native-comp-deferred-compilation-deny-list nil)

(defvar bootstrap-version)
;; ?? Emacs 29 ??? native-compile ??????? bug
(unless (version<= emacs-version "28.2")
  (setq straight-repository-branch "develop"))
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; make sure shell PATH is same as emacs PATH 
(use-package exec-path-from-shell
  :config
  (setq shell-file-name "/usr/bin/zsh")
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package esup
  :config
  (setq esup-depth 0))

;; Centralized path configuration
(defvar my/org-base-dir (expand-file-name "~/org/notes/")
  "Base directory for all org-roam notes.")

(setq org_notes_dir my/org-base-dir
      zot_bib "~/Nutstore/1/Nutstore/Zotero-Library/Main.bib"; Zotero .bib 文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 同步文件
      org_notes (expand-file-name "ref/" my/org-base-dir)) ; org-roam 文献笔记目录

(unless (file-exists-p org_notes_dir) (setq org_notes_dir nil))
(unless (file-exists-p zot_bib) (setq zot_bib nil))
(unless (file-exists-p zot_pdf) (setq zot_pdf nil))
(unless (file-exists-p org_notes) (setq org_notes nil)) ; 防止文件不存在报错

;; Create org-roam subdirectories if they don't exist
(dolist (subdir '("daily" "projects" "meetings" "concepts" "lit" "ref" "templates"))
  (let ((dir (expand-file-name subdir my/org-base-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(setq my/is-windows (eq system-type 'windows-nt)) ; Windows 
(setq my/is-linux (eq system-type 'gnu/linux)) ; Linux
(setq my/is-mac (eq system-type 'darwin)) ; mac
(setq my/is-WSL
      (if (and (eq system-type 'gnu/linux)
               (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
          t
        nil)) ; WSL
(setq my/is-terminal (not window-system)) ;GUI

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(use-package no-littering
  :demand t
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))) ; 设置自动保存文件目录
(use-package recentf
:after no-littering
:demand t 
:custom
(recentf-exclude '(no-littering-var-directory
                   no-littering-etc-directory)) ; 屏蔽临时文件
(recentf-max-menu-items 25)
(recentf-max-saved-items 25)
:bind ("C-x C-r" . 'recentf-open-files)
:config
(recentf-mode 1))

(use-package saveplace
  :defer 1
  :config
    (save-place-mode 1))
(use-package savehist
  :defer 1
  :config (savehist-mode))
(use-package super-save
  :defer 1
  :custom
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers nil)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer zzc/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun my/reload-emacs-config ()
    "Reload Emacs configuration by loading init.el.
This is useful after making changes to config.org and tangling it."
    (interactive)
    (message "Reloading Emacs configuration...")
    (load-file (expand-file-name "init.el" user-emacs-directory))
    (message "Emacs configuration reloaded successfully!"))

  (defun my/tangle-and-reload-config ()
    "Tangle config.org and then reload the configuration.
This is useful when you've edited config.org and want to apply changes immediately."
    (interactive)
    (if (and (buffer-file-name)
             (string-match-p "config\\.org$" (buffer-file-name)))
        (progn
          (message "Tangling config.org...")
          (org-babel-tangle)
          (message "Config tangled successfully!")
          (my/reload-emacs-config))
      (message "Current buffer is not config.org. Reloading init.el anyway...")
      (my/reload-emacs-config)))

  ;; Keybindings for config reload
  (zzc/leader-keys
    "r"  '(:ignore t :which-key "reload")
    "rr" '(my/reload-emacs-config :which-key "reload config")
    "rt" '(my/tangle-and-reload-config :which-key "tangle & reload"))

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-symbol-word-search t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; Bind Enter to project-aware consult-buffer in normal mode
  (define-key evil-normal-state-map (kbd "RET") 'my/consult-project-buffer)
  ;; Bind t to ace-window in normal mode
  (define-key evil-normal-state-map (kbd "t") 'ace-window))
  
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode))
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(use-package evil-escape
  :init (evil-escape-mode)
  :after evil
  :config
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.3))
(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key
   :states '(normal motion)
   :keymaps 'org-mode-map
   "zd" 'org-fold-hide-drawer-toggle))
(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))
;; save file very time after quit insert mode
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(setq
 display-buffer-alist
 '(;; Help windows
   ("^\\*[Hh]elp"                            ;正则匹配 buffer name
    (display-buffer-reuse-window
   ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些 alist
     display-buffer-in-side-window)
    (side . right)                        ;参数 alist 从这里开始。这个 side 会被 display-buffer-in-side-window 使用
    (window-width . 0.5)                     ;emacs 会自动把这个设置到 window-parameter 里
    (window-height . 0.33)                   ;同上
    (slot . 1)                               ;这个会被 display-buffer-in-side-window 使用，控制 window 位置
    (reusable-frames . visible)              ;这个参数看第三个链接的 display-buffer
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26 及以上会自动把下面的设置到 window-parameter 里
     (select . t)                            ;自定义的 param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line-format . none)               ;emacs version > 25， none 会隐藏 mode line，nil 会显示...
     (no-other-window . t)                   ;随你设置其他的 window-parameter，看文档
     ))))

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

(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (setq project-vc-extra-root-markers '(".project" "*.csproj")))

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
  
  ;; Restore frames and displays
  (setq desktop-restore-frames t)
  (setq desktop-restore-in-current-display t)
  (setq desktop-restore-forces-onscreen t)
  
  :config
  (desktop-save-mode 1)
  
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
    (if-let ((project (project-current)))
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
    (when-let ((project (project-current)))
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
    (if-let ((project-root (my/workspace-project-root)))
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
  "bd"  '(kill-current-buffer :which-key "kill current buffer")
  "bs"  '(save-buffer :which-key "save buffer")
  )

(winner-mode 1)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

(use-package winum
  :config
  (winum-mode))

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

;; save bookmark on change
(setq bookmark-save-flag 1)
(require 'bookmark)
;; set bookmark file to sync across difference device
(setq bookmark-default-file "~/.config/emacs/default/bookmarks")
(zzc/leader-keys
  "bm"  '(:ignore t :which-key "bookmark")
  "bmm"  '(bookmark-set :which-key "Add current file/dir to bookmark")
  "bml"  '(consult-bookmark :which-key "Open Bookmark List"))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))
(defun treemacs-adjust-width-to-fit ()
  "Adjust Treemacs window width to fit the longest filename."
  (let ((max-length (apply 'max
                           (mapcar 'string-width
                                   (treemacs--get-children-of (treemacs-current-root)))))
        (treemacs-default-width 30)) ;; Default width if there are no entries
    (treemacs-resize-to-width (max 30 (+ 5 max-length))))) ;; Add 5 to accommodate icons

;; Hook into window selection to auto-adjust width
(add-hook 'treemacs-select-window-hook 'treemacs-adjust-width-to-fit)

(use-package company
  :hook ((org-mode LaTeX-mode prog-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 4)
  (company-idle-delay 0.3)
  (company-tootip-idle-delay 0.5)
  (company-tooltip-offset-display 'line)
  (company-tooltip-align-annotation t)
  (company-show-quick-access t)
  (company-backends
   '((company-capf :with company-dabbrev-code company-keywords)
     (company-dabbrev)
     (company-ispell)
     (company-files)))
  (company-dabbrev-ignore-case nil) 
  (company-dabbrev-downcase nil)
  (company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  (company-show-quick-access 'left)
  :bind
  (:map company-active-map 
        ("M-/" . company-complete)
        ("<tab>" . company-indent-or-complete-common)
        ("C-c C-/" . company-other-backend))
  :config
  (set-face-attribute 'company-tooltip nil :inherit 'fixed-pitch))

(defun my/minibuffer-backward-kill (arg)
   "When minibuffer is completing a file name delete up to parent
   folder, otherwise delete a word"
   (interactive "p")
   (if minibuffer-completing-file-name
       ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
       (if (string-match-p "/." (minibuffer-contents))
           (zap-up-to-char (- arg) ?/)
         (delete-minibuffer-contents))
     (delete-word (- arg))))
 (setq completion-ignore-case 't) ; minibuffer ignore case
 (use-package vertico
   :defer 1
   :custom
   (verticle-cycle t)
   :config
   (vertico-mode)
   :bind (:map minibuffer-local-map
               ("M-h" .  my/minibuffer-backward-kill)))
;; (use-package vertico-posframe
;;    :init
;;    (vertico-posframe-mode)
;;    :config
;;    (setq vertico-posframe-poshandler 'posframe-poshandler-point-window-center)
;;    )

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :defer 1
  :config
  (marginalia-mode))

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
   :bind
   ( "C-;" . 'embark-act))
 (use-package consult
   :defer 1
   :bind
   ( "C-s" . 'consult-line)
   ;; Enable automatic preview at point in the *Completions* buffer. This is
   ;; relevant when you use the default completion UI.
   :hook (completion-list-mode . consult-preview-at-point-mode)

   ;; The :init configuration is always executed (Not lazy)
   :init
   ;; Optionally configure the register formatting. This improves the register
   ;; preview for `consult-register', `consult-register-load',
   ;; `consult-register-store' and the Emacs built-ins.
   (setq register-preview-delay 0.5
         register-preview-function #'consult-register-format)

   ;; Optionally tweak the register preview window.
   ;; This adds thin lines, sorting and hides the mode line of the window.
   (advice-add #'register-preview :override #'consult-register-window)

   ;; Use Consult to select xref locations with preview
   (setq xref-show-xrefs-function #'consult-xref
         xref-show-definitions-function #'consult-xref)
   ;; Configure other variables and modes in the :config section,
   ;; after lazily loading the package.
   :config

   ;; Optionally configure preview. The default value
   ;; is 'any, such that any key triggers the preview.
   ;; (setq consult-preview-key 'any)
   ;; (setq consult-preview-key "M-.")
   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
   ;; For some commands and buffer sources it is useful to configure the
   ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))

   ;; Optionally configure the narrowing key.
   ;; Both < and C-+ work reasonably well.
   (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
    )
(use-package embark-consult)

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(zzc/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package avy
  :demand 1
  :after general
  :config
  (zzc/leader-keys
    "j" '(:ignore t :which-key "jump")
    "jj" '(avy-goto-char :which-key "jump to char")
    "jw" '(avy-goto-word-0 :which-key "jump to word")
    "jl" '(avy-goto-line :which-key "jump to line")))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines))
  :config
  ;; Evil mode integration
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g z") 'mc/mark-next-like-this)
    (define-key evil-normal-state-map (kbd "g Z") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "g z") 'mc/mark-all-like-this))
  
  ;; Leader key bindings
  (zzc/leader-keys
    "m"  '(:ignore t :which-key "multiple-cursors")
    "mn" '(mc/mark-next-like-this :which-key "mark next")
    "mp" '(mc/mark-previous-like-this :which-key "mark previous")
    "ma" '(mc/mark-all-like-this :which-key "mark all")
    "ml" '(mc/edit-lines :which-key "edit lines")
    "mr" '(mc/mark-all-in-region :which-key "mark all in region")
    "ms" '(mc/skip-to-next-like-this :which-key "skip to next")
    "mu" '(mc/unmark-next-like-this :which-key "unmark next")))

(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
   (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

(use-package yasnippet
  :init
  (add-hook 'yas-minor-mode-hook (lambda()
				       (yas-activate-extra-mode 'fundamental-mode)))
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")))
(yas-global-mode 1)
(zzc/leader-keys
  "s"  '(:ignore t :which-key "snippet")
  "sc"  '(yas-new-snippet :which-key "create new snippet")
  "si"  '(yas-insert-snippet :which-key "insert snippet"))

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;;disable visusal scroll bar
(tool-bar-mode -1) ;;disable tool bar
(tooltip-mode -1) ;;disable tool tips
(menu-bar-mode -1) ;;disable menu bar
(set-fringe-mode 10) ;;Give some breathing room
(column-number-mode)
(global-hl-line-mode)
(global-visual-line-mode)
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width-start t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.
  Cycles through regular, relative and no line numbers. The order depends on what
  `display-line-numbers-type' is set to. If you're using Emacs 26+, and
  visual-line-mode is on, this skips relative and uses visual instead.
  See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
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
  "tl" '(doom/toggle-line-numbers :which-key "toggle line numbers"))

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(defvar my/font-height 200)
(defvar my/latex-preview-scale 1.3)
(defvar my/mm-char-height 3.2) ;4.2mm
;; 当字体高度为 4.2 mm 时, 对应的字体大小 1080p: 15.5; 2K: 18; 4K: 22
(defun my/get-font-height (&optional frame)
  (let* ((attrs (frame-monitor-attributes frame))
	 (geometry (alist-get 'geometry attrs)) 
	 (size (alist-get 'mm-size attrs)) 
	 (pixel-width (caddr geometry)) ; ????????
	 (mm-width  (car size))
	 (round (* 10 (/ pixel-width  (/ mm-width my/mm-char-height)))))))

(defun my/set-font-size ()
  (interactive)
  (let* ((font-size (my/get-font-height)))
    (message "font size: %s" font-size)
    (setq my/font-height font-size)
    (setq my/latex-preview-scale
	  (/ font-size 80.0))))

(defun my/set-font (font-height &optional frame)
  (interactive)
  ;; Ensure font-height is a valid integer
  (unless (and (integerp font-height) (> font-height 0))
    (setq font-height 200)) ; Fallback to default if invalid
  ;; 系统默认字体
  (setq my/system-default-font (font-get-system-normal-font))
  ;; Emacs 默认字体
  (setq my/default-font "Iosevka")
  (unless (find-font (font-spec :name my/default-font))
    (message (format "cannot find %s for the default font" my/default-font))
    (setq my/default-font my/system-default-font))

  ;; LaTeX 默认字体
  (setq my/math-font "Latin Modern Math")
  (unless (find-font (font-spec :name my/math-font))
    (message (format "cannot find %s for the math font. Use system default instead"  my/math-font))
    (setq my/math-font my/system-default-font))

  ;; 中文字体
  (setq my/chinese-font "LXGW WenKai")
  (unless (find-font (font-spec :name my/chinese-font))
    (message (format "cannot find %s for the chinese font. Use system default instead"  my/chinese-font))
    (setq my/chinese-font my/system-default-font))

  (setq my/variable-pitch-font "Cantarell")
  (unless (find-font (font-spec :name my/variable-pitch-font))
    (message (format "cannot find %s for the variable-pitch font. Use system default instead"  my/variable-pitch-font))
    (setq my/variable-pitch-font my/system-default-font))

  ;; 等宽字体
  (setq my/fixed-pitch-font "JetBrains Mono Nerd Font") ; fonts-jetbrains-mono (ubuntu) ; ttf-jetbrains-mono (manjaro)
  (unless (find-font (font-spec :name my/fixed-pitch-font))
    (message (format "cannot find %s for the fixed-pitch font. Use system default instead"  my/fixed-pitch-font))
    (setq my/fixed-pitch-font my/system-default-font))

  (set-face-attribute 'default frame :font my/default-font :height font-height)  ; 默认字体 字号
  (set-face-attribute 'variable-pitch frame :font my/variable-pitch-font :height font-height) ; 比例字体
  (set-face-attribute 'fixed-pitch frame :font my/fixed-pitch-font :height font-height) ; 等宽体
  (set-face-attribute 'bold nil :foreground "Salmon")

  (set-fontset-font "fontset-default" 'mathematical my/math-font) 
  (set-fontset-font "fontset-default" 'han my/chinese-font) 
  (set-fontset-font "fontset-default" 'unicode my/chinese-font) 
  (setq inhibit-compacting-font-caches t) 
  (setq auto-window-vscroll nil))

(defun my/set-font-current-frame ()
  (interactive)
  (my/set-font (my/get-font-height) (selected-frame)))
(global-set-key (kbd "C-x 9") #'my/set-font-current-frame)
(add-hook 'window-setup-hook #'my/set-font-current-frame)

(custom-set-faces
 '(region ((t (:background "yellow" :foreground "black" :weight bold)))))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(defun my/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
					(->> (custom-available-themes)
					     (-map #'symbol-name)
					     (--select (string-prefix-p "doom-" it)))))))
  (my/switch-theme theme)
  (set-face-foreground 'org-indent (face-background 'default)))

(defun my/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
					(->> (custom-available-themes)
					     (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t) ; ????
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (doom-themes-org-config))
(zzc/leader-keys
  "t"  '(:ignore t :which-key "toggle")
  "tt" '(my/load-doom-theme :which-kei "themes")
)

(use-package all-the-icons
  :if (display-graphic-p)) ;M-x all-the-icon-install-fonts.
(use-package minions
  :hook (doom-modeline-mode . minions-mode))
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-unicode-fallback t)
  ;; Show tab-bar workspace name in modeline
  (doom-modeline-workspace-name t)
  :config
  ;; Ensure workspace segment is visible
  (setq doom-modeline-workspace-name t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern 
  :custom
  (org-modern-hide-stars nil) 
  (org-modern-table t)
  (org-modern-list 
   '((?- . "•")
     (?* . "•")
     (?+ . "•")))
  :init
  (global-org-modern-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(defun my/set-org-font ()
  (interactive)
  ;; org 字体美化
  (require 'org-faces)
  ;; 标题字体大小优化
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.2)
  (dolist (face '((org-level-1 . 1.15)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground 'unspecified' :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :foreground 'unspecified' :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-block-end-line nil :foreground 'unspecified' :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-property-value nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-fontify-quote-and-verse-blocks t) ; 启用 org-qoute 变量为 quote 设置不同的字体
  (set-face-attribute 'org-quote nil :inherit 'fixed-pitch)
  (require 'org-indent) ;; 开启 org-indent 并设设置缩进字体
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

(defun my/org-mode-visual-fill ()
(interactive)
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill ))

(defun my/org-download-method (link) 
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          (dirname (concat "~/org/notes/images/" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
      (setq org-download-image-dir dirname)
      (make-directory dirname t)
      (expand-file-name (funcall org-download-file-format-function filename) dirname)))

(defun my/org-download-clipboard-wsl ()
  (interactive)
  (let* ((image-name (read-string "enter image name (without extension): "))
         (filename (expand-file-name (concat image-name ".png") "/tmp/"))
         (powershell-path "/mnt/c/windows/system32/windowspowershell/v1.0/powershell.exe"))
    ;; use full path to powershell
    (shell-command-to-string 
     (format "%s -command \"(get-clipboard -format image).save('$(wslpath -w %s)')\"" powershell-path filename))
    (when (file-exists-p filename)
      (org-download-image filename)
      (delete-file filename))))

(defun my/org-download-clipboard ()
  (interactive)
  (cond (my/is-windows (my/org-download-clipboard-windows))
        (my/is-WSL (my/org-download-clipboard-wsl))
        (t (org-download-clipboard)))) ; for linux and mac system

(setq org-image-actual-width nil)
(use-package org-download
  :custom
  (org-download-heading-lvl 1)
  (org-download-method #'my/org-download-method)
  :after org
  :bind (:map org-mode-map
              ("C-c i y" . org-download-yank)
              ("C-c i d" . org-download-delete)
              ("C-c i e" . org-download-edit)
              ("C-M-y" . my/org-download-clipboard)))

(defun my-org-hook ()
  (org-indent-mode) ; 自动缩进
  (variable-pitch-mode 1) ; 比例字体
  (visual-line-mode 1))

(defun my/follow-link-at-current-window () 
  (interactive)
  (let ((org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                      (vm-imap . vm-visit-imap-folder-other-frame)
                                      (gnus . gnus)
                                      (file . find-file)
                                      (wl . wl-other-frame)))))

    (org-open-at-point)))
(defun my/follow-link-at-current-window-mouse (event)
  (interactive (list last-command-event))
  (posn-set-point (event-end event))
  (let ((org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                      (vm-imap . vm-visit-imap-folder-other-frame)
                                      (gnus . gnus)
                                      (file . find-file)
                                      (wl . wl-other-frame)))))
    (org-open-at-point)))
  (use-package org
    :defer 10
    :custom
    (org-m-ret-may-split-line t)
    (org-priority-highest ?A) ; org-agenda 的最高优先级设为 A
    (org-priority-lowest ?C) ; org-agenda 的优先级设为 A-E
    (org-priority-default ?C) ; org-agenda 的默认优先级设为 D
    ;; (org-startup-with-latex-preview t) ; 设为 t 则创建新笔记时会出错.
    :bind
    (:map org-mode-map
          ("C-c n" . nil) ; 用于 org-roam 快捷键
          ("C-c o" . my/follow-link-at-current-window) ; 在当前窗口打开 org 文件
          ("C-<down-mouse-1>" . my/follow-link-at-current-window-mouse) ; ctrl+鼠标点击时, 在当前窗口打开 org 文件
          ("C-<drag-mouse-1>" . my/follow-link-at-current-window-mouse))
    :config
    (require 'org-download)
    (setq org-ellipsis " ▾"); 用小箭头代替...表示折叠
    (setq org-startup-folded 'content) ; 开启时折叠大纲

    (my/set-org-font)
    (add-hook 'org-mode-hook 'my-org-hook)
    (add-to-list 'org-babel-load-languages '(shell . t)))

;; (use-package org-jira
;;   :config
;;   (setq org-jira-working-dir "~/Documents/org/jira/")
;;   (setq jiralib-url "https://jira.vni.agileci.conti.de")
;;   (setq jiralib-token
;;     (cons "Authorization"
;;       (concat "Bearer " (auth-source-pick-first-password
;; 			 :host "jira.vni.agileci.conti.de"))))
;;   (setq org-jira-use-status-as-todo nil)
;;   (setq org-jira-jira-status-to-org-keyword-alist 
;;      '(("Working" . "ONGOING")
;;        ("New" . "TODO")
;;        ("Ready" . "TODO")
;;        ("Closed" . "DONE")
;;        ("Verifying" . "DONE"))))

(setq org-agenda-dir "~/org/jira/")
(setq org-agenda-files (directory-files-recursively org-agenda-dir "\\.org$"))

;; Log timestamp when TODO state changes
(setq org-log-done 'time)              ; Log timestamp when marking DONE
(setq org-log-into-drawer t)           ; Store state changes in :LOGBOOK: drawer
(setq org-log-state-notes-insert-after-drawers nil)  ; Insert state changes after properties

;; TODO keywords with logging:
;; ! = log timestamp when entering state
;; @ = prompt for note when entering state
;; Example: "DONE(d!)" logs timestamp when entering DONE
;;          "CANCEL(n@)" prompts for cancellation reason
(setq org-todo-keywords
   '((sequence "TODO(t)" "ONGOING(o)" "|" "CANCEL(c@)" "DONE(d!)")))
    ;; configure custom agenda views
    (setq org-agenda-custom-commands
     '(("d" "dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "ongoing"
  	((org-agenda-overriding-header "next tasks")))
          (tags-todo "agenda/active" ((org-agenda-overriding-header "active projects")))))
       ("n" "ongoing tasks"
        ((todo "next"
   	((org-agenda-overriding-header "next tasks")))))))

      ;; do not display done items in org-agenda
      (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo '("DONE" "CANCEL")))
      ;;key-binds
    (general-define-key
     :prefix "C-c"
     "a" 'org-agenda)
    (add-hook 'org-agenda-mode-hook
    	  (lambda ()
    	    (local-set-key (kbd "k") 'org-agenda-previous-item)
                (local-set-key (kbd "j") 'org-agenda-next-item)))
    ;; save all org files after change todo
    (defmacro η (fnc)
      "return function that ignores its arguments and invokes fnc."
      `(lambda (&rest _rest)
         (funcall ,fnc)))
    (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
    (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
    (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
    (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
    (advice-add 'org-priority       :after (η #'org-save-all-org-buffers))

(defun my/org-roam-get-daily-note-file ()
    "Get the file path for today's daily note."
    (let* ((time (current-time))
           (dailies-dir (expand-file-name org-roam-dailies-directory org-roam-directory))
           (filename (format-time-string "%Y-%m-%d.org" time)))
      (expand-file-name filename dailies-dir)))

  (defun my/org-roam-ensure-daily-note-exists ()
    "Ensure today's daily note exists, creating it if necessary."
    (let ((daily-file (my/org-roam-get-daily-note-file)))
      (unless (file-exists-p daily-file)
        ;; Create the daily note using org-roam-dailies
        (save-window-excursion
          (org-roam-dailies-goto-today)))
      daily-file))

  (defun my/refile-completed-task-to-daily ()
    "Refile completed task to today's daily note under Tasks > Completed heading.
This function is called automatically when a task is marked as DONE."
    (when (and (eq major-mode 'org-mode)
               (member (org-get-todo-state) '("DONE" "CANCEL")))
      (let* ((daily-file (my/org-roam-ensure-daily-note-exists))
             (task-heading (org-get-heading t t t t))
             (task-body (save-excursion
                          (org-back-to-heading t)
                          (let ((start (point)))
                            (org-end-of-subtree t t)
                            (buffer-substring-no-properties start (point))))))
        ;; Only refile if not already in daily note
        (unless (string= (buffer-file-name) daily-file)
          (save-excursion
            ;; Find and append to Tasks > Completed in daily note
            (with-current-buffer (find-file-noselect daily-file)
              (goto-char (point-min))
              ;; Find "Tasks" heading
              (if (re-search-forward "^\\* Tasks" nil t)
                  (progn
                    ;; Find or create "Completed" subheading
                    (let ((tasks-end (save-excursion
                                       (org-end-of-subtree t t)
                                       (point))))
                      (if (re-search-forward "^\\*\\* Completed" tasks-end t)
                          (progn
                            ;; Go to end of Completed section
                            (org-end-of-subtree t t)
                            (insert "\n" task-body))
                        ;; Create Completed subheading
                        (org-end-of-subtree t t)
                        (insert "\n** Completed\n" task-body)))
                    (save-buffer))
                (message "Warning: Could not find 'Tasks' heading in daily note"))))
          ;; Delete the original task
          (org-back-to-heading t)
          (org-cut-subtree)
          (save-buffer)
          (message "Task refiled to daily note: %s" task-heading)))))

  ;; Hook to auto-refile when TODO state changes to DONE/CANCEL
  (add-hook 'org-after-todo-state-change-hook 'my/refile-completed-task-to-daily)

(use-package org-pomodoro)
(setq org-pomodoro-audio-player "mpv"
      org-pomodoro-ticking-sound-p t
      org-pomodoro-ticking-sound-states '(:pomodoro)
      org-pomodoro-finished-sound-p t
      org-pomodoro-short-break-length 5
      org-pomodoro-finished-sound-args "--volume=50"
      org-pomodoro-long-break-sound-args "--volume=50"
      org-pomodoro-short-break-sound-args "--volume=50"
      org-pomodoro-ticking-sound-args "--volume=60")

;;key-binds
(zzc/leader-keys
  "c"  '(:ignore t :which-key "clock")
  "ci" '(org-clock-in :which-key "clock-in")
  "co" '(org-clock-out :which-key "clock-out")
  "cq" '(org-clock-cancel :which-key "clock-cancel")
  "cr" '(org-clock-report :which-key "clock-report")
  "cp" '(org-pomodoro :which-key "clock-pomodoro")
  "cd" '(org-clock-display :which-key "clock-display"))

(defun my/org-roam-daily-note-header ()
  "Generate daily note header with links and template content."
  (concat "#+title: " (format-time-string "%Y-%m-%d %a") "\n\n"
          "[[roam:" (format-time-string "%Y-w%W") "]]\n\n"
          "[[roam:" (format-time-string "%Y-%B") "]]\n\n"
          (with-temp-buffer
            (insert-file-contents "~/org/templates/daily.org")
            (buffer-string))))

(setq my/daily-note-filename "%<%Y-%m-%d>.org")

(use-package org-roam
    :custom
    (org-roam-directory my/org-base-dir)
    (org-roam-completion-everywhere t)
    (org-roam-completion-system 'default)
    (org-roam-node-display-template 
     (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-gc-threshold most-positive-fixnum)
    (org-roam-db-update-on-save t)
    (org-roam-db-update-idle-seconds 2.0)
    (org-roam-dailies-directory "daily/")
    (org-roam-dailies-capture-templates
    `(("d" "default" entry "* %?"
       :if-new (file+head ,my/daily-note-filename
                          ,(my/org-roam-daily-note-header)))
      ))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n c" . org-roam-capture)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n I" . org-roam-node-insert-immediate)
           ("C-c n t" . my/org-roam-capture-task)
           ("C-c n P" . my/org-roam-insert-new-project)
           ("C-c n p" . my/org-roam-find-project)
           ("C-c n u" . org-roam-ui-mode)
           ("C-c n g" . org-roam-graph)
           ("C-c n a" . org-roam-alias-add)
           ("C-c n T" . org-roam-tag-add)
           ("C-c n D" . org-roam-tag-remove)
           ("C-c n m" . org-roam-buffer-display-dedicated)
           ("C-c n s" . org-roam-db-sync)
           ("C-c n S" . my/org-roam-db-diagnose)
           ("C-c n Y" . my/org-roam-goto-year)
           ("C-c n M" . my/org-roam-goto-month)
           ("C-c n C" . my/org-roam-dailies-capture-date)
           :map org-mode-map
           ("C-M-i". completion-at-point))
    :config
    (define-key org-roam-mode-map [mouse-1] (kbd "C-u <return>"))
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
             :unnarrowed t)
            
            ("l" "literature" plain
             "* Source\n\n%?\n\n* Summary\n\n* Key Points\n- \n\n* Personal Notes\n\n* Related\n"
             :target (file+head "lit/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :literature:\n#+date: %U\n\n")
             :unnarrowed t)
            
            ("m" "meeting" plain
             "* Attendees\n- %?\n\n* Agenda\n- \n\n* Discussion\n\n* Action Items\n- [ ] \n\n* Follow-up\n"
             :target (file+head "meetings/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :meeting:\n#+date: %U\n\n")
             :unnarrowed t)
            
            ("c" "concept" plain
             "* Definition\n\n%?\n\n* Examples\n- \n\n* Properties\n- \n\n* Related Concepts\n- \n\n* References\n"
             :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :concept:\n#+date: %U\n\n")
             :unnarrowed t)
            
            ("r" "reference" plain
             "* Key Ideas\n\n%?\n\n* Quotes\n\n* Questions\n\n* Next Steps\n"
             :target (file+head "ref/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :reference:\n#+date: %U\n\n")
             :unnarrowed t)))
    (org-roam-db-autosync-mode) 
    (my/org-roam-refresh-agenda-list)
    (add-to-list 'org-after-todo-state-change-hook 
                 (lambda ()
                   (when (or (equal org-state "DONE")
  			   (equal org-state "COMPLETED"))
                     (my/org-roam-copy-todo-to-today)))))
  (add-hook 'org-roam-mode-hook 'visual-line-mode) ; 自动换行

(defun my/org-roam-filter-by-tag (tag-name)
  "Return a predicate function that filters org-roam nodes by TAG-NAME (case-insensitive).
The returned lambda checks if TAG-NAME is present in a node's tags list."
  (lambda (node)
    (cl-some (lambda (node-tag)
               (string-equal-ignore-case tag-name node-tag))
             (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Return a list of file paths for all org-roam nodes tagged with TAG-NAME (case-insensitive)."
  (mapcar #'org-roam-node-file
          (seq-filter (my/org-roam-filter-by-tag tag-name)
                      (org-roam-node-list))))

(defun my/org-roam-filter-by-tags (wanted unwanted)
  "Return a predicate that filters nodes having any tag in WANTED but none in UNWANTED (case-insensitive).
WANTED and UNWANTED should be lists of tag strings."
  (lambda (node)
    (let ((node-tags (org-roam-node-tags node)))
      (and (cl-some (lambda (tag) 
                      (cl-some (lambda (node-tag)
                                 (string-equal-ignore-case tag node-tag))
                               node-tags))
                    wanted)
           (not (cl-some (lambda (tag)
                           (cl-some (lambda (node-tag)
                                      (string-equal-ignore-case tag node-tag))
                                    node-tags))
                         unwanted))))))

(defun my/org-roam-refresh-agenda-list ()
  "Add all org-roam files tagged with 'Project' to `org-agenda-files' (case-insensitive).
This allows project files to appear in the org-agenda view."
  (interactive)
  (when (featurep 'org-roam)
    (setq org-agenda-files
          (delete-dups (append org-agenda-files
                               (my/org-roam-list-notes-by-tag "Project"))))
    (message "Refreshed agenda list: %d project files found" 
             (length (my/org-roam-list-notes-by-tag "Project")))))

(defun my/org-roam-db-diagnose ()
  "Clear and rebuild org-roam database to fix issues.
Prompts for confirmation before clearing the database."
  (interactive)
  (when (yes-or-no-p "Clear and rebuild org-roam database? This may take a few minutes. Continue? ")
    (message "Clearing org-roam database...")
    (org-roam-db-clear-all)
    (message "Rebuilding database from %s..." org-roam-directory)
    (org-roam-db-sync)
    (message "Org-roam database rebuilt successfully! Found %d nodes." 
             (caar (org-roam-db-query "SELECT COUNT(*) FROM nodes")))))

(defun my/org-roam-dailies-capture-date ()
  "Capture to a daily note on a specific date selected from calendar.
Prompts for date using org-mode's date picker."
  (interactive)
  (let ((time (org-read-date nil t nil "Daily note date:")))
    (org-roam-dailies-capture-date time)))

;; Initial setup: run after org-roam is loaded
(with-eval-after-load 'org-roam
  (my/org-roam-refresh-agenda-list))

;; Automatic refresh hooks
(defun my/org-roam-auto-refresh-agenda ()
  "Automatically refresh org-agenda-files when org-roam database updates."
  (when (and (featurep 'org-roam)
             (buffer-file-name)
             (string-prefix-p (expand-file-name org-roam-directory)
                              (file-truename (buffer-file-name))))
    (my/org-roam-refresh-agenda-list)))

;; Hook into org-roam database sync
(add-hook 'org-roam-db-autosync-mode-hook 'my/org-roam-refresh-agenda-list)

;; Hook into after saving org-roam files
(add-hook 'after-save-hook 'my/org-roam-auto-refresh-agenda)

;; Hook into after org-roam capture finalize
(add-hook 'org-roam-capture-new-node-hook 'my/org-roam-refresh-agenda-list)

(defun my/org-roam-goto-month ()
  "Navigate to or create a monthly planning note for the current month.
Creates a note in the root directory with format YYYY-MMMM.org."
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: :project:\n")
                                   :unnarrowed t))))

(defun my/org-roam-goto-year ()
  "Navigate to or create a yearly planning note for the current year.
Creates a note in the root directory with format YYYY.org."
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: :project:\n")
                                   :unnarrowed t))))

(defun my/set-orui-latex-macros ()
  "Configure LaTeX macros for org-roam-ui mathematical notation rendering.
Sets up commonly used mathematical symbols and operators."
  (setq org-roam-ui-latex-macros
        '(("\\C" . "\\mathbb{C}")
          ("\\fc" . "\\mathcal{f}")
          ("\\nc" . "\\mathcal{n}")
          ("\\ps" . "\\mathsf{p}")
          ("\\ppbf" . "\\mathbf{p}")
          ("\\ppbb" . "\\mathbb{p}")
          ("\\esf" . "\\mathsf{e}")
          ("\\eebf" . "\\mathbf{e}")
          ("\\eebb" . "\\mathbb{e}")
          ("\\one" . "\\mathbf{1}")
          ("\\R" . "\\mathbb{R}")
          ("\\Z" . "\\mathbb{Z}")
          ("\\Q" . "\\mathbb{Q}")
          ("\\N" . "\\mathbb{N}")
          ("\\eps" . "\\varepsilon")
          ("\\det" . "\\mathop{det}"))))
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :config
  (my/set-orui-latex-macros))

(use-package consult-org-roam
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n L" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

;; Org-roam leader key bindings under SPC n prefix
(zzc/leader-keys
  "n" '(:ignore t :which-key "notes")
  "n f" '(org-roam-node-find :which-key "find node")
  "n i" '(org-roam-node-insert :which-key "insert node")
  "n c" '(org-roam-capture :which-key "capture")
  "n j" '(org-roam-dailies-capture-today :which-key "daily today")
  "n d" '(org-roam-dailies-goto-today :which-key "goto today")
  "n y" '(org-roam-dailies-goto-yesterday :which-key "goto yesterday")
  "n t" '(org-roam-dailies-goto-tomorrow :which-key "goto tomorrow")
  "n D" '(org-roam-dailies-goto-date :which-key "goto date")
  "n p" '(my/org-roam-find-project :which-key "find project")
  "n P" '(my/org-roam-insert-new-project :which-key "new project")
  "n T" '(my/org-roam-capture-task :which-key "capture task")
  "n Y" '(my/org-roam-goto-year :which-key "goto year")
  "n M" '(my/org-roam-goto-month :which-key "goto month")
  "n u" '(org-roam-ui-open :which-key "open ui")
  "n s" '(org-roam-db-sync :which-key "sync db"))

;; org-roam capture 与 *Org-Select* 默认右侧打开
(add-to-list 'display-buffer-alist '("\\(^CAPTURE.*\.org$\\|\\*Org.*Select\\*$\\)"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (slot . 0)
                                     (window-width . 60)))

(use-package org-noter
  :bind
  (("C-c n n" . org-noter)
   :map org-noter-doc-mode-map
   ("M-e" . org-noter-insert-precise-note))
  :custom
  (org-noter-highlight-selected-text t)
  (org-noter-notes-search-path (list (expand-file-name "ref/" my/org-base-dir)))
  (org-noter-auto-save-last-location t))

(use-package org-anki
  :after org
  :config
  (setq org-anki-default-deck "Mega"))

(defvar my/org-roam-project-template 
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+date: %U\n#+category: ${title}\n#+filetags: :project:\n"
                           ("Tasks"))))

(defun my/org-roam-project-finalize-hook ()
  "adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
  ;; remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-insert-new-project ()
  (interactive)
  ;; add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; select a project file to open, creating it if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates (list my/org-roam-project-template)))

(defun my/org-roam-find-project ()
  (interactive)
  ;; add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tags '("Project") '("Archived"))))

(defun my/org-roam-capture-task ()
(interactive)
;; update org-agenda list after adding projects
(add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;; new todo
(org-roam-capture- :node (org-roam-node-read
                          nil
                          (my/org-roam-filter-by-tag "Project"))
                   :templates (list my/org-roam-project-template)))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (unless (or (string= (buffer-name) "*habit*") ; do nothing in habit buffer
          (string= (org-entry-get nil "STYLE") "habit")) ; skip if the task is a habit
    (let ((org-refile-keep t) ; set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%d-%d>\n" ("Done")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))
      ;; only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Done" today-file nil pos))))))

(defun my/org-refile-update-targets ()
  "Update `org-refile-targets` to match `org-agenda-files`."
  (setq org-refile-targets
        (mapcar (lambda (file) (cons file '(:maxlevel . 3))) org-agenda-files)))

;; Run once on startup
(my/org-refile-update-targets)

;; Update targets when project finalize hook runs (only when projects change)
(advice-add 'my/org-roam-project-finalize-hook :after #'my/org-refile-update-targets)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
    (python . t)
    (shell . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org source block editing behavior
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0  ; No extra indentation
      org-src-tab-acts-natively t          ; TAB works in native mode
      org-src-fontify-natively t           ; Syntax highlighting
      org-src-window-setup 'current-window) ; Edit in current window

(defun my/org-src-format-buffer ()
  "Format code in org-src-edit buffer based on major mode."
  (when (and (derived-mode-p 'prog-mode)
             (not org-src-preserve-indentation))
    (condition-case err
        (cond
         ;; Emacs Lisp
         ((eq major-mode 'emacs-lisp-mode)
          (require 'elisp-autofmt)
          (elisp-autofmt-buffer))
         ;; Shell scripts
         ((or (eq major-mode 'sh-mode) (eq major-mode 'shell-script-mode))
          (when (executable-find "shfmt")
            (shell-command-on-region (point-min) (point-max) 
                                     "shfmt" 
                                     (current-buffer) t 
                                     "*shfmt errors*" t)))
         ;; Fallback for other modes
         (t (indent-region (point-min) (point-max))))
      (error 
       (message "Formatting failed: %s, using indent-region" err)
       (indent-region (point-min) (point-max))))))

;; Add formatting hook to org-src-mode
(add-hook 'org-src-mode-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (add-hook 'before-save-hook #'my/org-src-format-buffer nil t))))

;; Keybindings for formatting code blocks
(with-eval-after-load 'org
  ;; Format entire code block from org-mode (C-c f b)
  (define-key org-mode-map (kbd "C-c f b") 
    (lambda () 
      (interactive)
      (if (org-in-src-block-p)
          (progn
            (org-edit-special)
            (my/org-src-format-buffer)
            (org-edit-src-exit))
        (message "Not in a source block"))))
  
  ;; Format region in code block (C-c f r)
  (define-key org-mode-map (kbd "C-c f r")
    (lambda ()
      (interactive)
      (when (org-in-src-block-p)
        (org-edit-special)
        (indent-region (region-beginning) (region-end))
        (org-edit-src-exit)))))

;; Keybindings when editing code blocks with C-c '
(with-eval-after-load 'org-src
  (define-key org-src-mode-map (kbd "C-c C-f") 
    (lambda ()
      (interactive)
      (my/org-src-format-buffer)))
  (define-key org-src-mode-map (kbd "C-c C-r")
    (lambda ()
      (interactive)
      (when (use-region-p)
        (indent-region (region-beginning) (region-end))))))

;; Add to leader key (SPC f)
(with-eval-after-load 'general
  (zzc/leader-keys
    "f"  '(:ignore t :which-key "format")
    "fb" '((lambda () 
             (interactive)
             (if (and (eq major-mode 'org-mode) (org-in-src-block-p))
                 (progn
                   (org-edit-special)
                   (my/org-src-format-buffer)
                   (org-edit-src-exit))
               (indent-region (point-min) (point-max))))
           :which-key "format buffer")
    "fr" '((lambda ()
             (interactive)
             (when (use-region-p)
               (indent-region (region-beginning) (region-end))))
           :which-key "format region")))

;; automatically tangle our emacs.org config file when we save it
(defun zzc/org-babel-tangle-config ()
  (when (string-equal (file-truename (buffer-file-name))
		      (file-truename (expand-file-name "~/dotconfig/basic/editor/emacs/default/config.org")))
    ;; dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook 
          (lambda () 
            (add-hook 'after-save-hook #'zzc/org-babel-tangle-config nil t)))

;; (use-package flycheck
;;   :init (global-flycheck-mode))

;;(use-package cmake-mode)

(use-package elisp-autofmt
  :straight t
  :demand t
  :config
  (setq elisp-autofmt-style 'native)
  (setq elisp-autofmt-format-quoted t)
  (setq elisp-autofmt-empty-line-max 2))

(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))

;; (use-package rust-mode
;;   :ensure t)
;;(use-package rustic
;;  :ensure t
;;  :config
;;  (setq rustic-format-on-save nil)
;;  :custom
;;  (rustic-cargo-use-last-stored-arguments t))

;;(use-package lua-mode)

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
   ("c-~" . VTERM-Toggle-cd)))

;; Key bindings with leader key
(zzc/leader-keys
  "v"  '(:ignore t :which-key "vterm")
  "vv"  '(vterm :which-key "open vterm")
  "vn"  '(multi-vterm :which-key "new vterm")
  "vp"  '(multi-vterm-project :which-key "vterm in project")
  "vt"  '(vterm-toggle :which-key "toggle vterm")
  "vd"  '(vterm-toggle-cd :which-key "toggle vterm in current dir"))

(use-package eee
   :straight '(eee :type git :host github :repo "eval-exec/eee.el"
               :files (:defaults "*.el" "*.sh"))
   :config
  ;; Set terminal command based on system
  (when my/is-WSL
    (setq ee-terminal-command "kitty"))
  (when my/is-mac
    (setq ee-terminal-command "wezterm"))
  
  ;; Set custom kitty options with --class scratchterm (overwrite default)
  (setq ee-terminal-options
        '(("kitty" . "--class scratchterm")))
  
  ;; Keybindings
  (zzc/leader-keys
    "tf"  '(ee-find :which-key "find")
    "tr"  '(ee-rg :which-key "rg")
    "tg"  '(ee-lazygit :which-key "lazygit")
    "ty"  '(ee-yazi :which-key "yazi")))

;; (require 'posframe)
;; (setq rime-show-candidate 'posframe)
(use-package rime
:straight (rime :type git
                :host github
                :repo "doglooksgood/emacs-rime"
                :files ("*.el" "Makefile" "lib.c"))
:custom
(default-input-method "rime")
(rime-show-candidate 'posframe)
(rime-user-data-dir "~/.config/rime")
(rime-disable-predicates '(rime-predicate-evil-mode-p
                            rime-predicate-after-ascii-char-p
                            rime-predicate-hydra-p
                            rime-predicate-tex-math-or-command-p
                            rime-predicate-prog-in-code-p))
:config
;; prevent rime crash
(defun rime-lib-finalize() nil)
(add-hook 'kill-emacs-hook #'rime-lib-finalize))

(cond
 ((and my/is-windows (not my/is-WSL)) ; Only Windows, not WSL
  (set-clipboard-coding-system 'utf-8))
 (my/is-WSL ; Specifically WSL
  (set-clipboard-coding-system 'utf-8)))

(use-package pangu-spacing)
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)
