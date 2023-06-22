;; -*- lexical-binding: t; -*-
;;disable warnings temporarily
(setq warning-minimum-level :emergency)
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

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
(package-refresh-contents))
;;Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;;(setq use-package-always-ensure t)

;; make sure shell PATH is same as emacs PATH 
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package no-littering)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
        ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))
(use-package evil-escape
  :init (evil-escape-mode)
  :after evil
  :config
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-delay 0.2))
;; save file very time after quit inder mode
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
	    (call-interactively #'save-buffer)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer zzc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC"))

(setq
 display-buffer-alist
 '(("^\\*[Hh]elp"                            ;正则匹配buffer name
    (display-buffer-reuse-window
     ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些alist
     display-buffer-in-side-window)
    (side . right)                        ;参数alist从这里开始。这个side会被display-buffer-in-side-window使用
    (window-width . 0.5)                     ;emacs会自动把这个设置到window-parameter里
    (window-height . 0.33)                   ;同上
    (slot . 1)                               ;这个会被display-buffer-in-side-window使用，控制window位置
    (reusable-frames . visible)              ;这个参数看第三个链接的display-buffer
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26及以上会自动把下面的设置到window-parameter里
     (select . t)                            ;自定义的param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line-format . none)               ;emacs version > 25， none会隐藏mode line，nil会显示...
     (no-other-window . t)                   ;随你设置其他的window-parameter，看文档
     ))))

(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (setq project-vc-extra-root-markers '(".project" "*.csproj")))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))  ; pick your own prefix key here
  :config
  (setq persp-state-default-file "~/.emacs.d/persp-session")
  (add-hook 'kill-emacs-hook #'persp-state-save)
  :init
  (persp-mode))

(zzc/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bp"  '(switch-to-prev-buffer :which-key "previous buffer")
  "bn"  '(switch-to-next-buffer :which-key "next buffer")
  "bb"  '(switch-to-buffer :which-key "list buffers")
  "bB"  '(ibuffer-list-buffers :which-key "list ibuffers")
  "bk"  '(kill-current-buffer :which-key "kill current buffer")
  "bs"  '(save-buffer :which-key "save buffer")
  )

(winner-mode 1)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

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
 ;; bind "C-c a" to 'org-agenda
 "m" 'toggle-one-window)

;; save bookmark on change
(setq bookmark-save-flag 1)
;; open bookmark on start-up
(setq inhibit-splash-screen t)
(require 'bookmark)
(list-bookmarks)
(switch-to-buffer "*Bookmark List*")
;; set bookmark file to sync across difference device
(setq bookmark-default-file "~/dotconfig/emacs/bookmarks")
(zzc/leader-keys
  "bm"  '(:ignore t :which-key "bookmark")
  "bmm"  '(bookmark-set :which-key "Add current file/dir to bookmark")
  "bml"  '(list-bookmarks :which-key "Open Bookmark List"))

(zzc/leader-keys
  "."  '(find-file :which-key "find file")
)

;; (use-package ivy
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          ("C-q" . ivy-immediate-done)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))
;; (use-package counsel
;;     :bind (("M-x" . counsel-M-x)
;;            ("C-x b" . counsel-ibuffer)
;;            ("C-x C-f" . counsel-find-file)
;;            :map minibuffer-local-map
;;            ("C-r" . 'counsel-minibuffer-history)))
;; (use-package ivy-posframe
;;       :config 
;;      (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))) 
;;      (ivy-posframe-mode 1))
;; (use-package ivy-rich
;;     :config
;;     (ivy-rich-mode 1))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode))
(use-package vertico-posframe
    :init
    (vertico-posframe-mode)
    :config
    (setq vertico-posframe-poshandler 'posframe-poshandler-point-window-center)
)
;;save history
(use-package savehist
  :init
  (savehist-mode))
(use-package orderless
  :config
  (setq completion-styles '(orderless)))
(use-package marginalia
  :config
  (marginalia-mode))
(use-package embark
  :bind
  ( "C-;" . 'embark-act))
(use-package consult
  :bind
  ( "C-s" . 'consult-line))

;;  (use-package helpful
;;    :custom
;;    (counsel-describe-function-function #'helpful-callable)
;;    (counsel-describe-variable-function #'helpful-variable)
;;    :bind
;;    ([remap describe-function] . counsel-describe-function)
;;    ([remap describe-command] . helpful-command)
;;    ([remap describe-variable] . counsel-describe-variable)
;;    ([remap describe-key] . helpful-key))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(zzc/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package posframe)
;; (use-package rime
    ;;   :custom
    ;;   (rime-show-candidate 'posframe)
    ;;   (rime-user-data-dir "~/.config/Rime")
    ;;   (default-input-method "rime")
    ;;   (rime-posframe-properties
    ;;    (list :background-color "#333333"
    ;;          :foreground-color "#dcdccc"
    ;;          ;; :font "WenQuanYi Zen Hei"
    ;;          :internal-border-width 10))
    ;;   (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
    ;;   (rime-librime-root "~/dotconfig/emacs/librime/dist")
    ;;   (rime-disable-predicates
    ;;        '(rime-predicate-evil-mode-p
    ;;          rime-predicate-after-alphabet-char-p
    ;;          rime-predicate-prog-in-code-p))
    ;; )
(use-package pyim
  :ensure nil
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  ;;(setq ivy-re-builders-alist
  ;;      '((t . pyim-cregexp-ivy)))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

    ;; 开启拼音
  ;; 搜索功能
  ;; (pyim-isearch-mode 1)

  ;; 使用 posframe 来绘制选词框 
  (require 'posframe)
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method))

;; comment line helper
(
 defun zzc/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(
 zzc/leader-keys
  "zz"  '(zzc/comment-or-uncomment-region-or-line :which-key "quick comment/uncomment"))

(zzc/leader-keys
  "="  '(:ignore t :which-key "open")
  "=i" '((lambda () (interactive) (find-file "~/dotconfig/emacs/init.el")) :which-key "open init.el")
  "=c" '((lambda () (interactive) (find-file "~/dotconfig/emacs/config.org")) :which-key "open config file"))

(use-package format-all 
  :hook
  (
   (python-mode . format-all-mode)
   (emacs-lisp-mode . format-all-mode)
   (ledger-mode . format-all-mode)
   (format-all-mode-hook . format-all-ensure-formatter)
  )
  :config
  (custom-set-variables
   '(format-all-formatters (quote (("Python" black) ("ledger" ledger-mode))))
  )
)
  ;; (use-package doom-format
  ;;   :after format-all
  ;;   :load-path "~/emacs-scratch/lisp/format")
  ;;   (setq +format-with-lsp nil)

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
  "ll" '(doom/toggle-line-numbers :which-key "toggle line numbers"))

(defun centaur-setup-fonts ()
  (set-face-attribute 'default nil :font (font-spec :family "Fira Code" :size 16))
  (set-fontset-font t 'unicode (font-spec :family "Noto Color Emoji" :size 14))
  (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Sarasa Mono Slab SC" :size 18 :weight 'bold))
)

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
;;M-X run all-the-icons-install-fonts
(use-package all-the-icons) ;;needed by doom-modeline

(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-org-blocks  'tinted-background
	modus-themes-region '(bg-only))
  (setq modus-themes-headings
	'((1 . (rainbow background 1.5))
	  (2 . (rainbow background 1.4))
	  (3 . (rainbow bold 1.3))
	  (t . (semilight 1.05))))

  (setq modus-themes-scale-headings t)
  (setq modus-themes-org-agenda
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale bold-today))
        (scheduled . uniform)
        (habit . simplified)))

  ;; Maybe define some palette overrides, such as by using our presets
   (setq modus-themes-common-palette-overrides
         modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org
  :config
  (setq org-directory "~/Documents/org"))

(use-package org-modern
  :config
   (setq
    ;; Edit settings
    org-auto-align-tags nil
    org-tags-column 0
    org-catch-invisible-edits 'show-and-error
    org-insert-heading-respect-content t

    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers t
    org-ellipsis "…"

    ;; Agenda styling
    org-agenda-tags-column 0
    org-agenda-time-grid
    '((daily today require-timed)
      (800 1000 1200 1400 1600 1800 2000)
      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    org-agenda-current-time-string
    "⭠ now ─────────────────────────────────────────────────")
  (with-eval-after-load 'org (global-org-modern-mode)))

;; (defun zzc/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 80
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . zzc/org-mode-visual-fill))

(setq org-indent-indentation-per-level 4)
(org-indent-mode 1)
(zzc/leader-keys
  "l"  '(:ignore t :which-key "line/link")
  "li" '(org-insert-link :which-key "Inser Link")
  "ls" '(org-store-link :which-key "Generate Link"))

(setq org-agenda-dir "~/Documents/org/notes/journal"
	org-agenda-files (list org-agenda-dir))

  (setq org-todo-keywords
    '((sequence "TODO(t)" "ONGOING(o)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c!)" "CANC(k@)")))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "ONGOING"
	((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "ONGOING Tasks"
     ((todo "NEXT"
	((org-agenda-overriding-header "Next Tasks")))))
    
    ("w" "Work Tasks" tags-todo "+work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("W" "Workflow Status"

     ((todo "WAIT"
	    ((org-agenda-overriding-header "Waiting on External")
	     (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
	    ((org-agenda-overriding-header "In Review")
	     (org-agenda-files org-agenda-files)))
      (todo "PLAN"
	    ((org-agenda-overriding-header "In Planning")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
	    ((org-agenda-overriding-header "Project Backlog")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "READY"
	    ((org-agenda-overriding-header "Ready for Work")
	     (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
	    ((org-agenda-overriding-header "Active Projects")
	     (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
	    ((org-agenda-overriding-header "Completed Projects")
	     (org-agenda-files org-agenda-files)))
      (todo "CANC"
	    ((org-agenda-overriding-header "Cancelled Projects")
	     (org-agenda-files org-agenda-files)))))))

  ;; Do not display Done items in org-agenda
  (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo '("DONE" "COMPLETED" "CANC")))
  ;;key-binds
  (zzc/leader-keys
    "n"  '(:ignore t :which-key "notes")
    "na" '(org-agenda :which-key "org agenda")
    "nt" '(org-todo :which-key "org todo"))
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (local-set-key (kbd "k") 'org-agenda-previous-item)
            (local-set-key (kbd "j") 'org-agenda-next-item)))
;; save all org files after change todo
(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))
(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
(advice-add 'org-priority       :after (η #'org-save-all-org-buffers))

;;key-binds
(zzc/leader-keys
  "nc"  '(:ignore t :which-key "clock")
  "nci" '(org-clock-in :which-key "clock-in")
  "nco" '(org-clock-out :which-key "clock-out")
  "ncq" '(org-clock-cancel :which-key "clock-cancel")
  "ncr" '(org-clock-report :which-key "clock-report")
  "ncd" '(org-clock-display :which-key "clock-display"))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
     (python . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
	:from tags
	:left-join nodes
	:on (= tags:node-id nodes:id)
	:where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

;; (add-hook 'find-file-hook #'vulpea-project-update-tag)
;; (add-hook 'before-save-hook #'vulpea-project-update-tag)

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

(use-package emacsql-sqlite-module)
(use-package emacsql-sqlite-builtin)
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :after org
  :custom
  (org-roam-directory "~/Documents/org/notes")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  ;; use emacs 29 built in sql
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "work-project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Docs\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project work")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Conclusion\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
      :unnarrowed t)))
(org-roam-dailies-capture-templates
   '(("d" "Journal" plain 
      "* 流水帐%?\n\n* Memorable Points\n** Good Stuff\n** Bad Stuff\n"
      :if-new (file+head "%<%Y%m%d>.org" "#+title: %<%Y%m%d>\n#+filetags: daily\n#+startup: overview"))))

  :bind (:map org-mode-map
	 ("C-M-q" . completion-at-point))
  :config
  (setq org-id-link-to-org-use-id t)
  (org-roam-setup) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(zzc/leader-keys
  "nr"  '(:ignore t :which-key "roam")
  "nrf"  '(org-roam-node-find :which-key "find roam node")
  "nrl"  '(org-roam-buffer-toggle :which-key "list roam backlinks")
  "nri"  '(org-roam-node-insert :which-key "insert roam node")
  "nrs"  '(org-roam-db-sync :which-key "sync roam database")
  )

(zzc/leader-keys
    "nd"  '(:ignore t :which-key "daily")
    "ndn"  '(org-roam-dailies-capture-today :which-key "capture for today")
    "ndd"  '(org-roam-dailies-goto-today :which-key "goto for today")
    "ndy"  '(org-roam-dailies-capture-yesterday :which-key "capture for yesterday")
    "ndt"  '(org-roam-dailies-capture-tomorrow :which-key "capture for tomorrow")
    "ndY"  '(org-roam-dailies-goto-yesterday :which-key "goto for yesterday")
    "ndT"  '(org-roam-dailies-goto-tomorrow :which-key "goto for tomorrow")
    "ndb"  '(org-roam-dailies-goto-next-note :which-key "goto next day")
    "ndp"  '(org-roam-dailies-goto-previous-note :which-key "goto previous day")
    "ndv"  '(org-roam-dailies-goto-date :which-key "goto next day")
)

(use-package websocket
  :after org-roam)
(use-package simple-httpd
  :after org-roam)
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(zzc/leader-keys
  "nr"  '(:ignore t :which-key "roam")
  "nru"  '(org-roam-ui-open :which-key "open org roam ui"))

(setq org-roam-dailies-files (file-expand-wildcards(concat org-directory  "/notes/journal/*.org")))
;; (setq org-roam-today-journal (file-expand-wildcards(concat org-directory  (format-time-string "/notes/journal/%Y%m%d.org"))))
(setq org-refile-targets '((org-roam-dailies-files :maxlevel . 2)))
;; save org mode after refiling
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-src-preserve-indentation nil
    org-edit-src-content-indentation 0)

;; Automatically tangle our Emacs.org config file when we save it
(defun zzc/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotconfig/emacs/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'zzc/org-babel-tangle-config)))

;; (use-package eaf
;;    :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;    :custom
;;    ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;    (eaf-browser-continue-where-left-off t)
;;    (eaf-browser-enable-adblocker t)
;;    (browse-url-browser-function 'eaf-open-browser)
;;    ;;enter insert mode in eaf
;;    (eval-after-load "evil"
;;      '(progn
;;         (defvar last-focus-buffer nil
;;           "Buffer currently in focus.")
;;         (defun buffer-focus-handler ()
;;           (interactive)
;;           (when (not (buffer-live-p last-focus-buffer))
;;             (setq last-focus-buffer nil))
;;           (when (and (eq (window-buffer (selected-window))
;;                          (current-buffer))
;;                      (not (eq last-focus-buffer (current-buffer))))
;;             (setq last-focus-buffer (current-buffer))
;;             (when (derived-mode-p 'eaf-mode)
;;               (evil-insert-state))))
;;         (add-hook 'buffer-list-update-hook #'buffer-focus-handler)))
;;    :config
;;    (defalias 'browse-web #'eaf-open-browser))
;;  (require 'eaf)
;;  (require 'eaf-pdf-viewer)
;;  (require 'eaf-browser)
;;  (zzc/leader-keys
;;    "e"  '(:ignore t :which-key "eaf")
;;    "eb"  '(:ignore t :which-key "eaf browser")
;;    "ebb"  '(eaf-open-browser :which-key "eaf open browser")
;;    "ebh"  '(eaf-open-browser-with-history :which-key "eaf open browser with history")
;;    "ebp"  '(eaf-open-pdf-from-history :which-key "eaf open pdf from history")
;; )

(use-package yasnippet
  :init
  (add-hook 'yas-minor-mode-hook (lambda()
				       (yas-activate-extra-mode 'fundamental-mode)))
  :config
  (setq yas-snippet-dirs '("~/dotconfig/emacs/snippets"))
  (yas-global-mode 1))

(zzc/leader-keys
  "s"  '(:ignore t :which-key "snippet")
  "sc"  '(yas-new-snippet :which-key "Create new snippet")
  "si"  '(yas-insert-snippet :which-key "Insert snippet"))

(setq org-plantuml-jar-path (expand-file-name "~/dotconfig/plantuml/plantuml-1.2023.9.jar"))

;;(straight-use-package
;; '(lsp-bridge :host github
;;              :repo "manateelazycat/lsp-bridge"
;;              :files ("*.el" "*.py" "acm" "core" "langserver"
;;                      "multiserver" "resources")))
;;(use-package lsp-bridge
;;   :commands lsp-bridge-mode
;;   :load-path ""
;;   :ensure nil
;;   :init
;;   (use-package markdown-mode)
;;   (use-package posframe)
;;
;;   :config
;;   (setq lsp-bridge-enable-auto-format-code t)
;;
;;   (global-lsp-bridge-mode))
;; (add-to-list 'load-path "~/.emacs.d/lsp-bridge")

;; (use-package markdown-mode)
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

(use-package vterm
    :ensure t)
(zzc/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tv" '(vterm :which-key "open vterm"))



(use-package org-noter
  :after org-pdftools
  :config
  ;; Your org-noter config ........
)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;;(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)
) ; if you are using yasnippet and want `ai` snippets

(zzc/leader-keys
  "oa"  '(:ignore t :which-key "org ai")
  "oar"  '(org-ai-on-region :which-key "Ask AI about selected text")
  "oas"  '(org-ai-summarize :which-key "Summarize selected text")
  "oac"  '(org-ai-refactor-code :which-key "Refactor selected code")
  "oap"  '(org-ai-prompt :which-key "Prompt user for a text and then print AI's reponse")
  "oa$"  '(org-ai-open-account-usage-page :which-key "Check how much money burned")
)

;; (straight-use-package
;;    '(mind-wave :host github
;;                :repo "manateelazycat/mind-wave"
;;                :files ("*.el" "*.md" "*.py"))
;; (add-to-list 'load-path "~/.emacs.d/straight/repos/mind-wave")
;; (require 'mind-wave)

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))
