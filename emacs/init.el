;; -*- lexical-binding: t; -*-
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
(setq use-package-always-ensure t)

;; make sure shell PATH is same as emacs PATH 
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package no-littering)
(setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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
    :global-prefix " M-SPC"))

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

(use-package ivy
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           ("C-q" . ivy-immediate-done)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history)))
(use-package ivy-posframe
      :config 
     (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))) 
     (ivy-posframe-mode 1))
(use-package ivy-rich
    :config
    (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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

    ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
    ;; 我自己使用的中英文动态切换规则是：
    ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;; 2. 光标前是汉字字符时，才能输入中文。
    ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    ;; (setq-default pyim-english-input-switch-functions
    ;;               '(pyim-probe-dynamic-english
    ;;                 pyim-probe-program-mode
    ;;                 pyim-probe-org-structure-template))

    ;; (setq-default pyim-punctuation-half-width-functions
    ;;               '(pyim-probe-punctuation-line-beginning
    ;;                 pyim-probe-punctuation-after-punctuation))

    ;; 开启拼音
    ;; 搜索功能
    ;; (pyim-isearch-mode 1)

    ;; 使用 pupup-el 来绘制选词框
    (setq pyim-page-tooltip 'popup)

    ;; 选词框显示5个候选词
    (setq pyim-page-length 5)

    ;; 让 Emacs 启动时自动加载 pyim 词库
    (add-hook 'emacs-startup-hook
              #'(lambda () (pyim-restart-1 t)))
    :bind
    (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
     ("C-;" . pyim-delete-word-from-personal-buffer)))
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)

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
  "=b" '((lambda () (interactive) (find-file "~/Documents/org/finance/bills.org")) :which-key "open bill")
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

;;(set-face-attribute 'default nil :font "MesloLGS NF" :height 160)
    ;; Set the fixed pitch face
    ;;(set-face-attribute 'fixed-pitch nil :font "MesloLGS NF" :height 160)
    ;; Set the variable pitch face
    ;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 200 :weight 'regular)
    (defvar meomacs-font-size 16
      "Current font size.")

  (defvar meomacs-fonts '((default . "MesloLGS NF")
			  (cjk . "Unifont")
			  (symbol . "Unifont")
			  (fixed . "MesloLGS NF")
			  (fixed-serif . "Dejavu Serif")
			  (variable . "Cantarell")
			  (wide . "MesloLGS NF")
			  (tall . "MesloLGS NF"))
    "Fonts to use.")
(defun meomacs--get-font-family (key)
  (let ((font (alist-get key meomacs-fonts)))
    (if (string-empty-p font)
	(alist-get 'default meomacs-fonts)
      font)))

(defun meomacs-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s"
			      (meomacs--get-font-family 'default)
			      meomacs-font-size)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun meomacs-load-face-font ()
  "Load face font configuration."
  (let ((variable-font (meomacs--get-font-family 'variable))
	(fixed-font (meomacs--get-font-family 'fixed))
	(fixed-serif-font (meomacs--get-font-family 'fixed-serif)))
    (set-face-attribute 'variable-pitch nil :family variable-font)
    (set-face-attribute 'fixed-pitch nil :family fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

(defun meomacs-load-charset-font (&optional font)
  "Load charset font configuration."
  (let ((default-font (or font (format "%s-%s"
				       (meomacs--get-font-family 'default)
				       meomacs-font-size)))
	(cjk-font (meomacs--get-font-family 'cjk))
	(symbol-font (meomacs--get-font-family 'symbol)))
    (set-frame-font default-font)
    (let ((fontset (create-fontset-from-ascii-font default-font)))
      ;; Fonts for charsets
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
	(set-fontset-font fontset charset cjk-font))
      (set-fontset-font fontset 'symbol symbol-font)
      ;; Apply fontset
      (set-frame-parameter nil 'font fontset)
      (add-to-list 'default-frame-alist (cons 'font fontset)))))

(meomacs-load-default-font)
(meomacs-load-face-font)

;; Run after startup
(add-hook 'after-init-hook
	  (lambda ()
	    (when window-system
	      (meomacs-load-charset-font))))

(use-package doom-themes
  :init (load-theme 'doom-nord t))
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
;;M-X run all-the-icons-install-fonts
(use-package all-the-icons) ;;needed by doom-modeline

(zzc/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "te" '(counsel-load-theme :which-key "choose theme"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package awesome-tab
    :config
    (awesome-tab-mode t))
(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))

(zzc/leader-keys
  "tt" '(awesome-fast-switch/body :which-key "tab switch")
  "tj" '(awesome-tab-forward-group :which-key "forward tab group")
  "tk" '(awesome-tab-backward-group :which-key "backward tab group")
  "tl" '(awesome-tab-forward-tab :which-key "backward tab group")
  "th" '(awesome-tab-backward-tab :which-key "backward tab group")
  "tg" '(awesome-tab-ace-jump :which-key "tab ace-jump")
  "tc" '(:ignore t :which-key "close tabs")
  "tco" '(awesome-tab-kill-other-buffers-in-current-group :which-key "close all other tabs in current group")
  "tca" '(awesome-tab-kill-other-buffers-in-current-group :which-key "close all tabs in current group")
  )

(defun zzc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (setq org-src-preserve-indentation nil 
      org-edit-src-content-indentation 0)
  (visual-line-mode 1))

(defun zzc/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))

  (use-package org
    :hook (org-mode . zzc/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    (setq org-directory "~/Documents/org")
    (zzc/org-font-setup))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  (defun zzc/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . zzc/org-mode-visual-fill))

  (zzc/leader-keys
    "l"  '(:ignore t :which-key "line/link")
    "li" '(org-insert-link :which-key "Inser Link")
    "ls" '(org-store-link :which-key "Generate Link"))

(setq org-agenda-dir "~/Documents/org/notes/journal"
      org-agenda-files (list org-agenda-dir))

(setq org-todo-keywords
  '((sequence "TODO(t)" "ONGOING(o)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c!)" "CANC(k@)")))

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

;;key-binds
(zzc/leader-keys
  "nc"  '(:ignore t :which-key "clock")
  "nci" '(org-clock-in :which-key "clock-in")
  "nco" '(org-clock-out :which-key "clock-out")
  "ncq" '(org-clock-cancel :which-key "clock-cancel")
  "ncr" '(org-clock-report :which-key "clock-report")
  "ncd" '(org-clock-display :which-key "clock-display"))

(straight-use-package
   '(ob-ledger :host github
               :repo "overtone/emacs-live"
               :files ("packs/stable/org-pack/lib/org-mode/lisp/ob-ledger.el")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
     (ledger . t)
     (python . t)))
(setq org-confirm-babel-evaluate nil)

;; (defun vulpea-project-files ()
;;     "Return a list of note files containing 'project' tag." ;
;;     (seq-uniq
;;      (seq-map
;;       #'car
;;       (org-roam-db-query
;;        [:select [nodes:file]
;;         :from tags
;;         :left-join nodes
;;         :on (= tags:node-id nodes:id)
;;         :where (like tag (quote "%\"project\"%"))]))))

;; (defun vulpea-agenda-files-update (&rest _)
;;   "Update the value of `org-agenda-files'."
;;   (setq org-agenda-files (vulpea-project-files)))

;; ;; (add-hook 'find-file-hook #'vulpea-project-update-tag)
;; ;; (add-hook 'before-save-hook #'vulpea-project-update-tag)

;; (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
;; (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

 (use-package emacsql-sqlite-module)
 (use-package emacsql-sqlite-builtin)
 (use-package org-roam
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
    '(("d" "default" plain
       "* Tasks\n\n%?\n\n* Flashes\n\n* Summary\n\n"
       :if-new (file+head "%<%Y%m%d>.org" "#+title: %<%Y%m%d>\n"))))
   :bind (:map org-mode-map
          ("C-M-q" . completion-at-point))
   :config
   (org-roam-setup)
   (require 'org-roam-dailies) ;; Ensure the keymap is available
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

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("cc" . "src c"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ai" . "ai")) ;;for org-ai
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
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

;; insert date and time
(defvar current-date-time-format "%Y %b %d %H:%M:%S %a %Z"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun zzc/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n"))

(defun zzc/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n"))

(zzc/leader-keys
  "i"  '(:ignore t :which-key "insert")
  "id"  '(zzc/insert-current-date-time :which-key "Inser current date and time")
  "it"  '(zzc/insert-current-time :which-key "Insert current time"))

;;(use-package ledger-mode)

;;(straight-use-package
;; '(lsp-bridge :host github
;;              :repo "manateelazycat/lsp-bridge"
;;              :files ("*.el" "*.py" "acm" "core" "langserver"
;;                      "multiserver" "resources")))
;;(unless (package-installed-p 'yasnippet)
;;  (package-install 'yasnippet))
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;
;;(require 'lsp-bridge)
;;(global-lsp-bridge-mode)

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (setq org-ai-openai-api-token "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")
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;;(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

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
