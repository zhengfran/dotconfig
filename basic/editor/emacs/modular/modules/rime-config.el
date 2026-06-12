;;; rime-config.el --- Chinese Input and Formatting Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Chinese input method (Rime) and pangu-spacing
;; This module loads on ALL systems (Windows, Linux, macOS)
;;
;; DEPENDENCIES: core (system detection vars), evil (rime predicates)
;; USED BY: None

;;; Code:

;; Rime input method - Chinese input
;; Posframe is required for rime candidate display
(use-package posframe)

(use-package rime 
  :straight (rime :type git
                  :host github
                  :repo "doglooksgood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :init
  ;; Set the library root only on macOS
  (when (eq system-type 'darwin)
    (setq rime-librime-root "/opt/homebrew"))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'popup)
  ;; 万象 (rime-wanxiang) is designed to live entirely in the *user* data
  ;; dir: its lua scripts read ./lua/ and create writable userdbs
  ;; (e.g. lua/predict.userdb) relative to user-data-dir. So point
  ;; user-data-dir at ~/dotconfig/Rime where wanxiang + its grammar model
  ;; were installed.
  (rime-user-data-dir (expand-file-name "~/dotconfig/Rime"))
  ;; Shared data: system rime-data provides preset/essential files as a
  ;; fallback. (Wanxiang is self-contained, but this is a safety net and
  ;; keeps Chinese input working on machines without the wanxiang dir.)
  (rime-share-data-dir
   (seq-find #'file-directory-p
             (list "/usr/share/rime-data"
                   "/opt/homebrew/share/rime-data"
                   "/usr/local/share/rime-data")
             "/usr/share/rime-data"))
  (rime-disable-predicates '(rime-predicate-evil-mode-p
                              rime-predicate-after-ascii-char-p
                              rime-predicate-hydra-p
                              rime-predicate-tex-math-or-command-p
                              rime-predicate-prog-in-code-p))
  :config
  ;; prevent rime crash
  (defun rime-lib-finalize() nil)
  (add-hook 'kill-emacs-hook #'rime-lib-finalize))

;; Clipboard encoding for Chinese characters 
(cond
 ((and my/is-windows (not my/is-WSL)) ; Only Windows, not WSL
  (set-clipboard-coding-system 'utf-8))
 (my/is-WSL ; Specifically WSL
  (set-clipboard-coding-system 'utf-8)))

;; Pangu-spacing: Automatic space between Chinese and English/numbers 
;; Improves readability of mixed Chinese-English text
(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t))

(provide 'rime-config)
;;; rime-config.el ends here
