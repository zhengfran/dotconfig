;; -*- lexical-binding: t; -*-
;;; init.el --- Modular Emacs Configuration

;;; Commentary:
;; Minimal init.el that loads feature-based modules from modules/ directory.
;; All configuration is organized into 20 separate modules.
;;
;; To use this config:
;;   emacs --init-directory ~/dotconfig/basic/editor/emacs/modular/
;;
;; Architecture:
;;   - init.el: Bootstrap and module loading (this file, ~110 lines)
;;   - early-init.el: Font configuration loaded before GUI
;;   - modules/*.el: 20 feature-based modules
;;   - custom.el: Emacs auto-generated customizations
;;
;; Module Load Order (dependency-aware):
;;   1. core              -> System detection, paths, file management
;;   2. keybindings       -> which-key, general.el, leader key setup
;;   3. ui                -> Themes, modeline, icons
;;   4. editor            -> Basic editor settings
;;   5. evil              -> Evil mode (needed by many modules)
;;   6. window            -> Window management, golden-ratio
;;   7. completion        -> Vertico, consult, corfu
;;   8. bookmarks         -> Bookmark management, URL capture
;;   9. workspace         -> Tab-bar workspaces, desktop sessions
;;   10. navigation       -> Avy, treemacs, multiple-cursors
;;   11. snippets         -> Yasnippet, global snippet search
;;   12. org-base         -> Core org-mode setup
;;   13. org-agenda-config -> Org agenda and TODO management
;;   14. org-roam-config  -> Org-roam notes system
;;   15. org-babel        -> Org babel code execution
;;   16. buffer-tabs      -> Buffer tabs with H/L navigation
;;   17. programming      -> Language support
;;   18. ai               -> GPTel AI integration
;;   19. chinese          -> Chinese input and formatting
;;  20. terminal-config   -> Terminal emulation (non-Windows only)

;;; Code:

;; ============================================================================
;; BASIC SETTINGS
;; ============================================================================

;; Show only errors in warnings buffer
(setq warning-minimum-level :error)

;; Custom file (Emacs auto-generated settings)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Disable bell
(setq visible-bell 1)

;; Increase garbage collection threshold for better startup performance
;; The default is 800 kilobytes. Set to 50MB.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Native compilation settings (Emacs 29+)
(defvar native-comp-deferred-compilation-deny-list nil)

;; ============================================================================
;; WINDOWS EXEC-PATH BOOTSTRAP
;; Must run before straight.el bootstrap since straight needs git
;; ============================================================================

;; Windows: Add Scoop git to exec-path so straight.el can find it
;; git lives in scoop app dir (not shims) at %USERPROFILE%\scoop\apps\git\current\cmd
(when (eq system-type 'windows-nt)
  (let ((git-cmd (expand-file-name "scoop/apps/git/current/cmd" (getenv "USERPROFILE"))))
    (when (file-directory-p git-cmd)
      (add-to-list 'exec-path git-cmd)
      (setenv "PATH" (concat git-cmd path-separator (getenv "PATH")))))
  ;; Disable git SSL verification for corporate proxy (SSL inspection)
  (setenv "GIT_SSL_NO_VERIFY" "true"))

;; ============================================================================
;; BOOTSTRAP STRAIGHT.EL (PACKAGE MANAGER)
;; ============================================================================

(defvar bootstrap-version)

;; Fix for Emacs 29+ native-compile bug
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

;; Disable org-elpa recipe source to avoid pre-build issues on Windows
(setq straight-recipe-repositories (delq 'org-elpa straight-recipe-repositories))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Override org recipe (org-elpa disabled above)
;; Use a simple recipe without problematic pre-build steps
(straight-use-package
 '(org :type git 
       :host github 
       :repo "emacs-straight/org-mode"
       :local-repo "org"
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

;; ============================================================================
;; LOAD MODULES
;; ============================================================================

;; Add modules directory to load-path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load modules in dependency order
;; IMPORTANT: Order matters! Some modules depend on others
(require 'core)            ; 1. System detection, paths, file management
(require 'keybindings)     ; 2. which-key, general.el, leader key setup
(require 'ui)              ; 3. Themes, modeline, icons
(require 'editor)          ; 4. Basic editor settings
(require 'evil)            ; 5. Evil mode (needed by other modules)
(require 'window-config)          ; 6. Window management
(require 'completion)      ; 7. Vertico, consult, corfu (needed by other modules)
(require 'bookmarks)       ; 8. Bookmarks (uses consult)
(require 'workspace)       ; 9. Workspaces, sessions, projects
(require 'navigation)      ; 10. Avy, treemacs, multiple-cursors
(require 'snippets)        ; 11. Yasnippet + global snippet search
(require 'org-base)           ; 12. Core org-mode setup
(require 'org-agenda-config)  ; 13. Org agenda and TODO management (defines org-agenda-files)
(require 'denote-config)           ; 14. Denote notes system (uses org-agenda-files)
(require 'habit-tracker)           ; 14b. Progressive habit tracking (uses denote)
(require 'blog)                    ; 14c. Blog article tracking (uses denote)
(require 'org-babel)          ; 15. Org babel code execution
(require 'buffer-tabs)        ; 16. Buffer tab-line with H/L navigation and org titles
(require 'programming)        ; 17. Programming languages
(require 'ai)                 ; 18. GPTel AI integration
(require 'beancount)          ; 19. Beancount plain-text accounting
(require 'rime-config)        ; 20. Chinese input and formatting (all systems)
(require 'terminal-config)  ; 20. Terminal (vterm, eee) - non-Windows only

;; ============================================================================
;; STARTUP COMPLETE
;; ============================================================================

(message "✓ All modules loaded successfully")

;;; init.el ends here
