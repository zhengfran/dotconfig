;;; programming.el --- Programming Language Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for programming languages, formatters, and language-specific settings
;;
;; DEPENDENCIES: None
;; USED BY: org-babel (elisp-autofmt)

;;; Code:

;; Emacs Lisp formatter
(use-package elisp-autofmt
  :straight t
  :demand t
  :config
  (setq elisp-autofmt-style 'native)
  (setq elisp-autofmt-format-quoted t)
  (setq elisp-autofmt-empty-line-max 2))

;; Shell script configuration
(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))

;; Commented out language modes (enable as needed)
;; (use-package rust-mode
;;   :ensure t)
;;(use-package rustic
;;  :ensure t
;;  :config
;;  (setq rustic-format-on-save nil)
;;  :custom
;;  (rustic-cargo-use-last-stored-arguments t))

;;(use-package lua-mode)

;;(use-package cmake-mode)

(provide 'programming)
;;; programming.el ends here
