;;; chinese.el --- Chinese Input and Formatting Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Chinese input method (Rime) and pangu-spacing
;; This module loads on ALL systems (Windows, Linux, macOS)
;;
;; DEPENDENCIES: core (system detection vars), evil (rime predicates)
;; USED BY: None

;;; Code:

;; Rime input method - Chinese input
;; Note: Only loads on non-Windows systems due to compilation requirements
(unless (eq system-type 'windows-nt)
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
    (add-hook 'kill-emacs-hook #'rime-lib-finalize)))

;; Clipboard encoding for Chinese characters
(cond
 ((and my/is-windows (not my/is-WSL)) ; Only Windows, not WSL
  (set-clipboard-coding-system 'utf-8))
 (my/is-WSL ; Specifically WSL
  (set-clipboard-coding-system 'utf-8)))

;; Pangu-spacing: Automatic space between Chinese and English/numbers
;; Improves readability of mixed Chinese-English text
(use-package pangu-spacing)
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)

(provide 'chinese)
;;; chinese.el ends here
