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

;; Only needed on real GUI sessions where Emacs does NOT inherit the shell's
;; PATH (macOS, native X).  This is a pgtk build, so under WSLg `window-system'
;; is `pgtk' (not in the list below) and a login shell would be slow and
;; unnecessary anyway; the WSL_DISTRO_NAME guard makes that skip explicit.
(use-package exec-path-from-shell
  :if (and (memq window-system '(mac ns x))
           (not (getenv "WSL_DISTRO_NAME")))
  :config
  (setq shell-file-name
        (cond
          ((eq system-type 'windows-nt) (or (executable-find "bash")
                                            (executable-find "zsh")
                                            "bash"))
          (t (or (executable-find "zsh") "/bin/zsh"))))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Windows: Add Git for Windows Unix tools (grep, xargs, etc.) to exec-path
;; Required by xref-matches-in-files which pipes through xargs
(when (eq system-type 'windows-nt)
  (when-let* ((git-exe (executable-find "git"))
              (git-dir (file-name-directory
                        (directory-file-name
                         (file-name-directory git-exe))))
              (usr-bin (expand-file-name "usr/bin" git-dir)))
    (when (file-directory-p usr-bin)
      (add-to-list 'exec-path usr-bin)
      (setenv "PATH" (concat usr-bin path-separator (getenv "PATH"))))))

;; Windows: Add npm global bin directory to exec-path (Scoop-managed Node.js)
;; Required for agent-shell to find claude-code-acp and other npm-installed agents
;; Use USERPROFILE (C:\Users\<user>) since ~ resolves to AppData\Roaming on Windows
(when (eq system-type 'windows-nt)
  (let ((npm-bin (expand-file-name "scoop/persist/nodejs/bin" (getenv "USERPROFILE"))))
    (when (file-directory-p npm-bin)
      (add-to-list 'exec-path npm-bin)
      (setenv "PATH" (concat npm-bin path-separator (getenv "PATH"))))))

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
(dolist (subdir '("journal" "ref" "trades" "habits"))
  (let ((dir (expand-file-name subdir my/org-base-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

;; ============================================================================
;; SYSTEM DETECTION
;; ============================================================================

(setq my/is-windows (eq system-type 'windows-nt)) ; Windows
(setq my/is-linux (eq system-type 'gnu/linux)) ; Linux
(setq my/is-mac (eq system-type 'darwin)) ; mac
;; WSL detection: read the kernel osrelease from /proc instead of spawning a
;; `uname' subprocess (process spawns are expensive on WSL/9p).
(setq my/is-WSL
      (and (eq system-type 'gnu/linux)
           (with-temp-buffer
             (ignore-errors
               (insert-file-contents "/proc/sys/kernel/osrelease"))
             (goto-char (point-min))
             (and (re-search-forward "[Mm]icrosoft\\|WSL" nil t) t))))
(setq my/is-terminal (not window-system)) ;GUI

;; WSL browser configuration (reuse my/is-WSL; avoids a second subprocess)
(when my/is-WSL
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

;; ============================================================================
;; WSL CLIPBOARD BRIDGE
;; ============================================================================
;; This is a pgtk (GTK/Wayland) build, so under WSLg Emacs is a native Wayland
;; client -- and WSLg does NOT reliably mirror the Wayland selection to the
;; Windows clipboard (its X11 bridge works, the Wayland one does not).  So a
;; kill here never reaches the Windows clipboard.  Bridge it through PowerShell,
;; which is Unicode-safe (Chinese survives intact):
;;   - Cut  (Emacs -> Windows): debounced + async, so a burst of evil kills does
;;     not spawn a PowerShell process each; also sets the Wayland selection, so
;;     copy-to-another-Linux-app keeps working.
;;   - Paste (Windows -> Emacs): synchronous (~0.3s/yank).  Turn it off with
;;     (setq my/wsl-clipboard-sync-paste nil) for instant native Wayland paste
;;     (Emacs -> Windows copy still works).
(when (and my/is-WSL (not my/is-terminal) (executable-find "powershell.exe"))

  (defvar my/wsl-clipboard-sync-paste t
    "When non-nil, yanking pulls from the Windows clipboard via PowerShell.
Set to nil for instant native Wayland paste; Emacs -> Windows copy is unaffected.")

  (defvar my/wsl-clip--pending nil
    "Latest killed text awaiting a debounced push to the Windows clipboard.")
  (defvar my/wsl-clip--timer nil
    "Idle timer that coalesces rapid kills into a single clipboard push.")

  (defun my/wsl-clip--flush ()
    "Push the most recent killed text to the Windows clipboard, once."
    (when my/wsl-clip--pending
      (let ((text my/wsl-clip--pending))
        (setq my/wsl-clip--pending nil)
        (ignore-errors
          (let ((proc (make-process
                       :name "wsl-clip-cut" :buffer nil
                       :connection-type 'pipe :coding 'utf-8-unix :noquery t
                       :command
                       '("powershell.exe" "-NoProfile" "-NonInteractive" "-Command"
                         "[Console]::InputEncoding=[Text.Encoding]::UTF8; Set-Clipboard -Value ([Console]::In.ReadToEnd())"))))
            (process-send-string proc text)
            (process-send-eof proc))))))

  (defun my/wsl-clip-cut (text)
    "`interprogram-cut-function': set the Wayland selection and, debounced, the
Windows clipboard."
    (gui-select-text text)
    (setq my/wsl-clip--pending text)
    (when (timerp my/wsl-clip--timer) (cancel-timer my/wsl-clip--timer))
    (setq my/wsl-clip--timer (run-with-idle-timer 0.2 nil #'my/wsl-clip--flush)))

  (defun my/wsl-clip-paste ()
    "`interprogram-paste-function': return the Windows clipboard contents.
Falls back to the native Wayland selection when `my/wsl-clipboard-sync-paste'
is nil."
    (if (not my/wsl-clipboard-sync-paste)
        (gui-selection-value)
      (let ((s (with-output-to-string
                 (with-current-buffer standard-output
                   (let ((coding-system-for-read 'utf-8-unix))
                     (ignore-errors
                       (call-process "powershell.exe" nil t nil
                                     "-NoProfile" "-NonInteractive" "-Command"
                                     "[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-Clipboard -Raw")))))))
        ;; PowerShell emits CRLF line endings and a trailing newline; normalize.
        (setq s (replace-regexp-in-string "\r" "" s))
        (when (string-suffix-p "\n" s) (setq s (substring s 0 -1)))
        (unless (string-empty-p s) s))))

  (setq interprogram-cut-function   #'my/wsl-clip-cut)
  (setq interprogram-paste-function #'my/wsl-clip-paste))

;; ============================================================================
;; FILE MANAGEMENT
;; ============================================================================

(use-package no-littering
  :demand t
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

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

;; ============================================================================
;; TRAMP-RPC - DISABLED (Not stable on WSL)
;; ============================================================================

;; TRAMP-RPC has issues on WSL:
;; - ControlMaster doesn't work reliably
;; - Large find/fd commands timeout
;; - SSH compatibility issues
;;
;; Use traditional SSH TRAMP instead: /ssh:user@host:/path
;;
;; If you want to try TRAMP-RPC on native Linux/macOS, uncomment below:

(use-package msgpack)

(use-package tramp-rpc
  :straight (tramp-rpc :type git
                       :host github
                       :repo "ArthurHeymans/emacs-tramp-rpc")
  :custom
  (tramp-rpc-deploy-auto-deploy t)
  (tramp-rpc-deploy-local-cache-directory
   (expand-file-name "tramp-rpc/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))
  (tramp-rpc-deploy-remote-directory "~/.cache/tramp-rpc/")
  (tramp-rpc-deploy-prefer-build nil)
  (tramp-rpc-use-controlmaster (not (or my/is-windows my/is-WSL))))

(provide 'core)
;;; core.el ends here
