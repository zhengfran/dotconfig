;;; org-base.el --- Core org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core org-mode settings: appearance, fonts, org-modern, org-download,
;; visual-fill-column, link following
;;
;; DEPENDENCIES: core (system detection vars)
;; USED BY: org-roam, org-agenda, org-babel

;;; Code:

;; ============================================================================
;; ORG-MODERN (MODERN APPEARANCE)
;; ============================================================================

(use-package org-modern-indent
  :after org
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :init
  ;; Set variables before org-modern loads (in :init block)
  ;; This ensures they're available when org-modern-mode activates
  
  ;; Heading stars - use fold indicators (dynamic bullets showing document structure)
  (setq org-modern-star 'fold)
  (setq org-modern-hide-stars 'leading)  ; Hide leading stars
  
  ;; Fold indicators: (folded . expanded) pairs for each heading level
  (setq org-modern-fold-stars
        '(("▶" . "▼")   ; Level 1: ▶ when folded, ▼ when expanded
          ("▷" . "▽")   ; Level 2
          ("⯈" . "⯆")   ; Level 3  
          ("▹" . "▿")   ; Level 4
          ("▸" . "▾"))) ; Level 5+
  
  ;; List item bullets
  (setq org-modern-list
        '((?- . "•")    ; - becomes •
          (?+ . "◦")    ; + becomes ◦
          (?* . "▸")))  ; * becomes ▸
  
  ;; Hide leading stars in org-mode
  (setq org-hide-leading-stars t))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;; ============================================================================
;; ORG FONT CONFIGURATION
;; ============================================================================

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

;; ============================================================================
;; VISUAL-FILL-COLUMN
;; ============================================================================

(use-package visual-fill-column
  :defer t
  :custom
  ;; Default settings (buffer-local when set via setq-local or in hooks)
  (visual-fill-column-width nil)  ; Use fill-column value by default
  (visual-fill-column-center-text nil)  ; Don't center by default
  (visual-fill-column-enable-sensible-window-split t)  ; Allow vertical splits
  (visual-fill-column-fringes-outside-margins t)  ; Fringes outside margins
  :init
  (defun my/org-mode-visual-fill ()
    "Enable visual-fill-column-mode with custom settings for org-mode."
    (interactive)
    (when (fboundp 'visual-fill-column-mode)  ; Check if function exists
      (setq-local visual-fill-column-width 150
                  visual-fill-column-center-text t)
      (visual-fill-column-mode 1)))
  :config
  ;; Adjust margins when text scale changes
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;; For org-mode specifically, use custom settings
(add-hook 'org-mode-hook #'my/org-mode-visual-fill)

;; ============================================================================
;; ORG-DOWNLOAD (IMAGE INSERTION)
;; ============================================================================

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

;; ============================================================================
;; ORG MODE HOOK
;; ============================================================================

(defun my-org-hook ()
  (org-indent-mode) ; 自动缩进
  (variable-pitch-mode 1) ; 比例字体
  (visual-line-mode 1))

;; ============================================================================
;; LINK FOLLOWING
;; ============================================================================

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

;; ============================================================================
;; ORG MODE CORE SETTINGS
;; ============================================================================

(use-package org
    :defer 10
    :custom
    (org-m-ret-may-split-line t)
    (org-priority-highest ?A) ; org-agenda 的最高优先级设为 A
    (org-priority-lowest ?C) ; org-agenda 的优先级设为 A-C
    (org-priority-default ?C) ; org-agenda 的默认优先级设为 C
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

(provide 'org-base)
;;; org-base.el ends here
