;;; core.el --- Core Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core settings: package management, path configuration, system detection,
;; file management (recentf, saveplace, auto-save, auto-revert)
;;
;; DEPENDENCIES: None (loads first)
;; USED BY: org-base, denote, chinese, terminal (system detection vars)

;;; Code:

;; ============================================================================
;; EXEC PATH FROM SHELL
;; ============================================================================

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq shell-file-name
        (cond
          ((eq system-type 'windows-nt) (or (executable-find "bash")
                                            (executable-find "zsh")
                                            "bash"))
          (t (or (executable-find "zsh") "/bin/zsh"))))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; ============================================================================
;; STARTUP PROFILING
;; ============================================================================

(use-package esup
  :config
  (setq esup-depth 0))

;; ============================================================================
;; PATH CONFIGURATION
;; ============================================================================

;; Centralized path configuration
(defvar my/org-base-dir (expand-file-name "~/org/notes/")
  "Base directory for all denote notes (previously org-roam).")

(setq org_notes_dir my/org-base-dir
      zot_bib "~/Nutstore/1/Nutstore/Zotero-Library/Main.bib"; Zotero .bib 文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 同步文件
      org_notes (expand-file-name "ref/" my/org-base-dir)) ; 文献笔记目录

(unless (file-exists-p org_notes_dir) (setq org_notes_dir nil))
(unless (file-exists-p zot_bib) (setq zot_bib nil))
(unless (file-exists-p zot_pdf) (setq zot_pdf nil))
(unless (file-exists-p org_notes) (setq org_notes nil)) ; 防止文件不存在报错

;; Create denote subdirectories if they don't exist
;; Note: daily/ has been migrated to journal/ for denote
(dolist (subdir '("journal" "ref" "trades"))
  (let ((dir (expand-file-name subdir my/org-base-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

;; ============================================================================
;; SYSTEM DETECTION
;; ============================================================================

(setq my/is-windows (eq system-type 'windows-nt)) ; Windows 
(setq my/is-linux (eq system-type 'gnu/linux)) ; Linux
(setq my/is-mac (eq system-type 'darwin)) ; mac
(setq my/is-WSL
      (if (and (eq system-type 'gnu/linux)
               (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
          t
        nil)) ; WSL
(setq my/is-terminal (not window-system)) ;GUI

;; WSL browser configuration
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

;; ============================================================================
;; FILE MANAGEMENT
;; ============================================================================

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

(provide 'core)
;;; core.el ends here
