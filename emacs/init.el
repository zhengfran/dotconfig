;; -*- lexical-binding: t; -*-
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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
  (setq-default evil-escape-key-sequence "jj"
      evil-escape-delay 0.2))

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
    :global-prefix "C-SPC"))

(zzc/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bp"  '(switch-to-prev-buffer :which-key "previous buffer")
  "bn"  '(switch-to-next-buffer :which-key "next buffer")
  "bb"  '(switch-to-buffer :which-key "list buffers")
  "bk"  '(kill-current-buffer :which-key "kill current buffer")
  "bs"  '(save-buffer :which-key "save buffer")
  )

(zzc/leader-keys
  "."  '(find-file :which-key "find file")
  )

(use-package posframe)
(use-package rime
  :custom
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/.config/Rime")
  (default-input-method "rime")
  (rime-posframe-properties
   (list :background-color "#333333"
         :foreground-color "#dcdccc"
         ;; :font "WenQuanYi Zen Hei"
         :internal-border-width 10))
  (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
  (rime-librime-root "~/.emacs.d/librime/dist"))

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

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
  ("f" nil "finished" :exit t))
(zzc/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; comment line helper
(defun zzc/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(zzc/leader-keys
  "TAB TAB"  '(zzc/comment-or-uncomment-region-or-line :which-key "quick comment/uncomment"))

(zzc/leader-keys
  "="  '(:ignore t :which-key "open")
  "=b" '((lambda () (interactive) (find-file "~/Documents/org/finance/bills.org")) :which-key "open bill")
  "=c" '((lambda () (interactive) (find-file "~/Documents/org/config.org")) :which-key "open config file"))

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
  "tl" '(doom/toggle-line-numbers :which-key "toggle line numbers"))

(set-face-attribute 'default nil :font "MesloLGS NF" :height 160)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MesloLGS NF" :height 160)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 200 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-nord t))
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
;;M-X run all-the-icons-install-fonts
(use-package all-the-icons) ;;needed by doom-modeline

(zzc/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun zzc/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun zzc/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
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

(setq org-agenda-dir "~/Documents/org/"
      ;; define the refile targets
      org-agenda-files nil)

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

;;key-binds
(zzc/leader-keys
  "n"  '(:ignore t :which-key "notes")
  "na" '(org-agenda :which-key "org agenda")
  "nt" '(org-todo :which-key "org todo"))

(use-package ob-ledger
  :load-path "~/emacs-scratch/lisp/ob-ledger")

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
     (ledger . t)
     (python . t)))
(setq org-confirm-babel-evaluate nil)

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

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :after org
  :custom
  (org-roam-directory "~/Nextcloud2/org/notes")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
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
      "* Goals\n\n%?\n\n* Task Accomplished\n\n* Summary\n\n"
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

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("cc" . "src c"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun zzc/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/emacs-scratch/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'zzc/org-babel-tangle-config)))

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
(require 'eaf-rss-reader)
(require 'eaf-pdf-viewer)
(require 'eaf-browser)
(require 'eaf-file-manager)
(require 'eaf-mindmap)
(require 'eaf-org-previewer)
(require 'eaf-system-monitor)

(use-package ledger-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(format-all-formatters '(("Python" black) ("ledger" ledger-mode)) t)
 '(package-selected-packages
   '(org-roam-ui simple-httpd websocket exec-path-from-shell which-key visual-fill-column use-package undo-tree rime rainbow-delimiters org-roam org-bullets no-littering ledger-mode ivy-rich hydra helpful general format-all evil-escape evil-collection doom-themes doom-modeline counsel all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
