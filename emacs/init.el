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
(setq use-package-always-ensure t)

;; make sure shell PATH is same as emacs PATH 
(use-package exec-path-from-shell
  :config
  (setq shell-file-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package esup
  :config
  (setq esup-depth 0))

(setq org_notes_dir "~/Documents/org/notes" ; org notes location
      zot_bib "~/Nutstore/1/Nutstore/Zotero-Library/Main.bib"; Zotero .bib 文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 同步文件
      org_notes "~/Documents/org/notes/ref/") ; org-roam 文献笔记目录

(unless (file-exists-p org_notes_dir) (setq org_notes_dir nil))
(unless (file-exists-p zot_bib) (setq zot_bib nil))
(unless (file-exists-p zot_pdf) (setq zot_pdf nil))
(unless (file-exists-p org_notes) (setq org_notes nil)) ; 防止文件不存在报错

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer zzc/leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

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

;; save file very time after quit inder mode
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(defun minibuffer-next-line ()
  "Move to the next line in the minibuffer history."
  (interactive)
  (if (eq last-command 'next-history-element)
      (next-history-element 1)
    (next-history-element 0)))

(defun minibuffer-previous-line ()
  "Move to the previous line in the minibuffer history."
  (interactive)
  (if (eq last-command 'previous-history-element)
      (previous-history-element 1)
    (previous-history-element 0)))

(define-key minibuffer-local-map (kbd "C-j") 'minibuffer-next-line)
(define-key minibuffer-local-map (kbd "C-k") 'minibuffer-previous-line)

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

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))  ; pick your own prefix key here
  :config
  (setq persp-state-default-file (expand-file-name ".persp-save" user-emacs-directory))
  ;; Automatically save perspectives when Emacs quits
  (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; Automatically load perspectives at startup
  ;; (add-hook 'emacs-startup-hook #'persp-state-load)
  :init
  (persp-mode))

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
  :ensure t
  :config
  (winum-mode))

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
(setq bookmark-default-file "~/.config/emacs/bookmarks")
(zzc/leader-keys
  "bm"  '(:ignore t :which-key "bookmark")
  "bmm"  '(bookmark-set :which-key "Add current file/dir to bookmark")
  "bml"  '(list-bookmarks :which-key "Open Bookmark List"))

(zzc/leader-keys
  "="  '(:ignore t :which-key "open")
  "=h" '((lambda () (interactive) (find-file "~/Documents/notes/20241004160632-habit_tracking.org")) :which-key "open habit.org")
  "=c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :which-key "open config file"))

(zzc/leader-keys
  "."  '(find-file :which-key "find file")
  )

(use-package treemacs
  :ensure t
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
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
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
(use-package vertico-posframe
  :init
  (vertico-posframe-mode)
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-point-window-center)
  )

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
(use-package consult)
(use-package embark-consult)
(zzc/leader-keys
  "sb" '(consult-line :which-key "search line"))

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

(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

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
(add-hook 'after-init-hook #'my/set-font-current-frame)

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

(setq display-time-day-and-date t)
(display-time-mode 1)

(use-package all-the-icons
  :if (display-graphic-p)) ;M-x all-the-icon-install-fonts.
(use-package minions
  :hook (doom-modeline-mode . minions-mode))
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (setq doom-modeline-height 25) ;; Adjust height for better appearance
  (setq doom-modeline-bar-width 3) ;; Optional: Adjust bar width
  (custom-set-faces
   '(mode-line ((t (:family "JetBrains Mono Nerd Font" :height 120))))
   '(mode-line-inactive ((t (:family "JetBrains Mono Nerd Font" :height 120)))))
  (doom-modeline-unicode-fallback t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern 
  :custom
  (org-modern-hide-stars nil) 
  (org-modern-table nil)
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
          (dirname (concat "~/Documents/notes/images/" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
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

(use-package org
  :defer 10
  :custom
  (org-m-ret-may-split-line t)
  (org-priority-lowest ?e) ; org-agenda 的优先级设为a-e
  (org-priority-default ?d) ; org-agenda 的默认优先级设为d
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
  (if t ; my/enable-folding
      (setq org-startup-folded 'content) ; 开启时折叠大纲
    (setq org-startup-folded 'showeverything))

  (my/set-org-font)
  (add-hook 'org-mode-hook 'my-org-hook)
  (add-to-list 'org-babel-load-languages '(shell . t)))

(setq org-agenda-dir "~/Documents/org/notes/journal"
	org-agenda-files (list org-agenda-dir))

  (setq org-todo-keywords
    '((sequence "TODO(t)" "ONGOING(o)" "|" "LOGGED(n@)" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c!)" "CANC(k@)")))

  ;; configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "ongoing"
	((org-agenda-overriding-header "next tasks")))
      (tags-todo "agenda/active" ((org-agenda-overriding-header "active projects")))))


    ("n" "ongoing tasks"
     ((todo "next"
	((org-agenda-overriding-header "next tasks")))))
    
    ("w" "work tasks" tags-todo "+work")

    ("w" "workflow status"
     ((todo "wait"
	python -m pip install -u aider-chat    ((org-agenda-overriding-header "waiting on external")
	     (org-agenda-files org-agenda-files)))
      (todo "review"
	    ((org-agenda-overriding-header "in review")
	     (org-agenda-files org-agenda-files)))
      (todo "plan"
	    ((org-agenda-overriding-header "in planning")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "backlog"
	    ((org-agenda-overriding-header "project backlog")
	     (org-agenda-todo-list-sublevels nil)
	     (org-agenda-files org-agenda-files)))
      (todo "ready"
	    ((org-agenda-overriding-header "ready for work")
	     (org-agenda-files org-agenda-files)))
      (todo "active"
	    ((org-agenda-overriding-header "active projects")
	     (org-agenda-files org-agenda-files)))
      (todo "completed"
	    ((org-agenda-overriding-header "completed projects")
	     (org-agenda-files org-agenda-files)))
      (todo "canc"
	    ((org-agenda-overriding-header "cancelled projects")
	     (org-agenda-files org-agenda-files)))))))

  ;; do not display done items in org-agenda
  (setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo '("done" "completed" "canc")))
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

(use-package org-pomodoro)
(setq org-pomodoro-audio-player "mpv")
;;key-binds
(zzc/leader-keys
  "nc"  '(:ignore t :which-key "clock")
  "nci" '(org-clock-in :which-key "clock-in")
  "nco" '(org-clock-out :which-key "clock-out")
  "ncq" '(org-clock-cancel :which-key "clock-cancel")
  "ncr" '(org-clock-report :which-key "clock-report")
  "ncp" '(org-pomodoro :which-key "clock-pomodoro")
  "ncd" '(org-clock-display :which-key "clock-display"))

;; org-ref
(use-package org-ref
  :bind (:map org-mode-map
              ("C-c (". org-ref-insert-label-link)
              ("C-c )". org-ref-insert-ref-link)))
;; org-transclusion
(use-package org-transclusion)

(setq my/daily-note-filename "%<%Y-%m-%d>.org" 
      my/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-w%W>]]\n\n[[roam:%<%Y-%B>]]\n\n")

(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/org/notes/") 
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template 
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-dailies-directory "daily/") 
  (org-roam-dailies-capture-templates 
   `(("d" "default" entry "* %?" 
      :target (file+head ,my/daily-note-filename
                         ,my/daily-note-header))
     ("t" "task" entry "* TODO %?\n  %U\n  %a\n  %i" 
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                            ("tasks"))
      :empty-lines 1) 
     ("j" "journal" entry "* %<%I:%M %P> - journal  :journal:\n\n%?\n\n" 
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("log")))
     ("f" "闪念" entry "* %<%I:%M %P> - 闪念  :journal:\n\n%?\n\n" 
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("log")))
     ("m" "meeting" entry "* %<%I:%M %P> - meeting with %^{whom}  :meetings:\n\n%?\n\n" 
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("meeting")))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-actions)
         ("C-c n d" . my/org-roam-jump-menu/body)
         ("C-c n P" . my/org-roam-insert-new-project)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n u" . org-roam-ui-mode)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (define-key org-roam-mode-map [mouse-1] (kbd "C-u <return>")) ; org-roam-buffer ???????c-u <return>
  (setq org-roam-capture-templates  ; org-roam
        '(("d" "default" plain "%?" ; 
           :target
           (file+head "%<%y%m%d%h%m%s>-${slug}.org" "#+title: ${title} \n")
           :unnarrowed t)
	  ))
  (require 'org-roam-dailies) 
  (org-roam-db-autosync-mode) 
  (my/org-roam-refresh-agenda-list) 
  (add-to-list 'org-after-todo-state-change-hook 
               (lambda ()
                 (when (or (equal org-state "DONE")
			   (equal org-state "COMPLETED"))
                   (my/org-roam-copy-todo-to-today)))))
(add-hook 'org-roam-mode-hook 'visual-line-mode) ; 自动换行

(defun my/set-orui-latex-macros ()
  (setq org-roam-ui-latex-macros
        '(("\\c" . "\\mathbb{c}")
          ("\\fc" . "\\mathcal{f}")
          ("\\nc" . "\\mathcal{n}")
          ("\\ps" . "\\mathsf{p}")
          ("\\pp" . "\\mathbf{p}")
          ("\\pp" . "\\mathbb{p}")
          ("\\e" . "\\mathsf{e}")
          ("\\ee" . "\\mathbf{e}")
          ("\\ee" . "\\mathbb{e}")
          ("\\one" . "\\mathbf{1}")
          ("\\r" . "\\mathbb{r}")
          ("\\z" . "\\mathbb{z}")
          ("\\q" . "\\mathbb{q}")
          ("\\n" . "\\mathbb{n}")
          ("\\eps" . "\\varepsilon")
          ("\\det" . "\\mathop{det}"))))
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  :config
  (my/set-orui-latex-macros))

(use-package org-noter
  :bind
  (("C-c n n" . org-noter)
   :map org-noter-doc-mode-map
   ("M-e" . org-noter-insert-precise-note))
  :custom
  (org-noter-highlight-selected-text t)
  (org-noter-notes-search-path '("~/Documents/org/notes/ref/"))
  (org-noter-auto-save-last-location t))

(defvar my/org-roam-project-template 
    '("p" "project" plain "** TODO %?"
      :if-new (file+head+olp "%<%Y%m%d%H>-${slug}.org"
                             "#+title: ${title}\n\n#+filetags: Project\n"
                             ("tasks"))))
  (defun my/org-roam-filter-by-tag (tag-name) 
    (lambda (node)
      (member tag-name (org-roam-node-tags node)))) 
  (defun my/org-roam-list-notes-by-tag (tag-name) 
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
(defun my/org-roam-filter-by-tags (wanted unwanted)
(lambda (node)
  (let ((node-tags (org-roam-node-tags node)))
    (and (cl-some (lambda (tag) (member tag node-tags)) wanted)
         (not (cl-some (lambda (tag) (member tag node-tags)) unwanted))))))

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

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(defun my/org-roam-capture-task ()
(interactive)
;; update org-agenda list after adding projects
(add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;; new todo
(org-roam-capture- :node (org-roam-node-read
                          nil
                          (my/org-roam-filter-by-tag "Project"))
                   :templates (list my/org-roam-project-template)))

(defun my/org-roam-dailies-go-to-today ()
  "go to today's daily note if it exists, otherwise trigger capture."
  (interactive)
  (let* ((today (org-roam-dailies--file-for-today)))
    (if (file-exists-p today)
        (find-file today)  ;; open the existing file if it exists.
      (org-roam-dailies-capture-today))))  ;; trigger capture if it doesn't.

(defun my/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: project\n")
                                   :unnarrowed t))))

(defun my/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: project\n")
                                   :unnarrowed t))))
(defhydra my/org-roam-jump-menu (:hint nil)
  "
^dailies^        ^capture^       ^jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" my/org-roam-goto-month)
  ("e" my/org-roam-goto-year)
  ("c" nil "cancel"))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (unless (or (string= (buffer-name) "*habit*") ; do nothing in habit buffer
          (string= (org-entry-get nil "STYLE") "habit")) ; skip if the task is a habit
    (let ((org-refile-keep t) ; set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%d-%d>\n" ("tasks")))))
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
        (org-refile nil nil (list "tasks" today-file nil pos))))))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
    (python . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; automatically tangle our emacs.org config file when we save it
(defun zzc/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotconfig/emacs/config.org"))
    ;; dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'zzc/org-babel-tangle-config)))

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
(use-package vertico-posframe
  :init
  (vertico-posframe-mode)
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-point-window-center)
  )

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
(use-package consult)
(use-package embark-consult)
(zzc/leader-keys
  "sb" '(consult-line :which-key "search line"))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(zzc/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; (use-package flycheck
;;   :init (global-flycheck-mode))

;;(use-package cmake-mode)

;;(use-package rustic
;;  :ensure t
;;  :config
;;  (setq rustic-format-on-save nil)
;;  :custom
;;  (rustic-cargo-use-last-stored-arguments t))

;;(use-package lua-mode)

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

;; (require 'posframe)
;; (use-package rime)
;; (require 'rime)
;; (setq default-input-method "rime")
;; (setq rime-show-candidate 'posframe)
(use-package rime
:straight (rime :type git
                :host github
                :repo "doglooksgood/emacs-rime"
                :files ("*.el" "Makefile" "lib.c"))
:custom
(default-input-method "rime")
(rime-show-candidate 'posframe))
;; prevent rime crash
(defun rime-lib-finalize() nil)
(add-hook 'kill-emacs-hook #'rime-lib-finalize)

;; Conditionally set clipboard coding system for windows, windows clipboard seems not saved in UTF-8
(cond
 ((and my/is-windows (not my/is-WSL)) ; Only Windows, not WSL
  ;; (set-clipboard-coding-system 'euc-cn))
  (set-clipboard-coding-system 'utf-8))
 (my/is-WSL ; Specifically WSL
  ;; (set-clipboard-coding-system 'euc-cn)))
  (set-clipboard-coding-system 'utf-8)))

(use-package pangu-spacing)
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)

;; (use-package vterm)
;; (use-package eee
;;    :straight (:host github :repo "eval-exec/eee.el" :files (:defaults "*.el" "*.sh"))
;;    :config
;;    (setq ee-terminal-command "vterm")
;; )
;; (zzc/leader-keys
;;   "t"  '(:ignore t :which-key "toggle")
;;   "tl"  '(ee-lazygit :which-key "lazygit")
;; )
