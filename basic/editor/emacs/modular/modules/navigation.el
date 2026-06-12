;;; navigation.el --- Navigation tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigation tools: avy (jump)
;;
;; DEPENDENCIES: evil
;; USED BY: None

;;; Code:

;; ============================================================================
;; AVY (JUMP TO CHAR/WORD/LINE)
;; ============================================================================

(use-package avy
  :demand 1
  :after (general evil)
  :config
  ;; Bind via `general' (like the rest of the config) so these survive
  ;; `evil-collection-init' reshaping the `g' prefix. `:bind' on
  ;; evil-normal-state-map can get shadowed, which is why `gl' didn't work.
  (general-define-key
   :states '(normal motion)
   "s"  'avy-goto-char-timer
   "gl" 'avy-goto-line))

;; ============================================================================
;; ACE-PINYIN (JUMP TO CHINESE CHARS VIA PINYIN, ON TOP OF AVY)
;; ============================================================================
;; Makes the existing avy commands (e.g. `s' -> avy-goto-char-timer) able to
;; jump to Chinese characters by typing the pinyin initials. No new keys
;; needed. `ace-pinyin-use-avy' must be set before the global mode is enabled.

(use-package ace-pinyin
  :after avy
  :init
  (setq ace-pinyin-use-avy t)            ; use avy backend (default)
  :config
  (ace-pinyin-global-mode +1))

(provide 'navigation)
;;; navigation.el ends here
