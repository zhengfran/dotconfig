;;; completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion system: corfu (in-buffer), vertico (minibuffer),
;; consult (enhanced commands), cape (completion-at-point),
;; marginalia, orderless, embark
;;
;; DEPENDENCIES: None
;; USED BY: bookmarks, workspace, snippets, denote (consult-* functions)

;;; Code:

;; ============================================================================
;; CORFU (IN-BUFFER COMPLETION)
;; ============================================================================

(use-package
 corfu
 :custom
 (corfu-auto t)
 (corfu-auto-delay 0.1)
 (corfu-auto-prefix 2)
 (corfu-cycle t)
 (corfu-separator ?\s)
 (corfu-preview-current nil)
 (corfu-on-exact-match nil)
 (corfu-left-margin-width 1.0) ;; Fix clipping on left
 (corfu-right-margin-width 1.0) ;; Fix clipping on right
 :config
 (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch)
 (set-face-attribute 'corfu-current nil :inherit '(fixed-pitch highlight))
 ;; Crucial for proper sizing of child frames
 (setq frame-resize-pixelwise t)

 ;; Keybindings - use define-key for reliability
 (define-key corfu-map (kbd "M-/") #'completion-at-point)
 (define-key corfu-map (kbd "<tab>") #'corfu-next)
 (define-key corfu-map (kbd "TAB") #'corfu-next)
 (define-key corfu-map (kbd "<backtab>") #'corfu-previous)
 (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
 (define-key corfu-map (kbd "RET") #'corfu-insert)

 ;; Enable corfu globally (after keybindings are set)
 (global-corfu-mode))

(with-eval-after-load 'corfu
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.5)
  (setq corfu-popupinfo-max-height 20)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle))

;; ============================================================================
;; CAPE (COMPLETION AT POINT EXTENSIONS)
;; ============================================================================

(use-package
 cape
 :init
 ;; Define hook functions (runs at startup, before cape loads)
 (defun my/cape-setup-prog-mode ()
   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
   (add-to-list 'completion-at-point-functions #'cape-keyword)
   (add-to-list 'completion-at-point-functions #'cape-file))

 (defun my/cape-setup-text-mode ()
   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
   (add-to-list 'completion-at-point-functions #'cape-dict)
   (add-to-list 'completion-at-point-functions #'cape-file))

 ;; Register hooks immediately at startup
 (add-hook 'prog-mode-hook #'my/cape-setup-prog-mode)
 (add-hook 'text-mode-hook #'my/cape-setup-text-mode)

 
 :config
 ;; Configuration that needs cape loaded
 (setq cape-dabbrev-check-other-buffers nil)
 (require 'cape-keyword)

 :bind
 (("C-c p d" . cape-dabbrev)
  ("C-c p f" . cape-file)
  ("C-c p w" . cape-dict)
  ("C-c p k" . cape-keyword)
  ("C-c p s" . cape-elisp-symbol)
  ("C-c p a" . cape-abbrev)))

;; ============================================================================
;; MINIBUFFER HELPER
;; ============================================================================

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

;; ============================================================================
;; VERTICO (MINIBUFFER COMPLETION)
;; ============================================================================

(use-package vertico
   :defer 1
   :custom
   (vertico-cycle t)
   :config
   (vertico-mode)
   :bind (:map minibuffer-local-map
               ("M-h" .  my/minibuffer-backward-kill)))

;; (use-package vertico-posframe
;;    :init
;;    (vertico-posframe-mode)
;;    :config
;;    (setq vertico-posframe-poshandler 'posframe-poshandler-point-window-center)
;;    )

;; ============================================================================
;; MARGINALIA (COMPLETION ANNOTATIONS)
;; ============================================================================

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :defer 1
  :config
  (marginalia-mode))

;; ============================================================================
;; ORDERLESS (FUZZY MATCHING)
;; ============================================================================

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; ============================================================================
;; EMBARK (CONTEXT ACTIONS)
;; ============================================================================

(use-package embark
   :bind
   ( "C-;" . 'embark-act))

;; ============================================================================
;; CONSULT (ENHANCED COMMANDS)
;; ============================================================================

(use-package consult
   :defer 1
   :bind
   ( "C-s" . 'consult-line)
   ( "C-x C-r" . 'consult-recent-file)
   ;; Enable automatic preview at point in the *Completions* buffer. This is
   ;; relevant when you use the default completion UI.
   :hook (completion-list-mode . consult-preview-at-point-mode)

   ;; The :init configuration is always executed (Not lazy)
   :init
   ;; Optionally configure the register formatting. This improves the register
   ;; preview for `consult-register', `consult-register-load',
   ;; `consult-register-store' and the Emacs built-ins.
   (setq register-preview-delay 0.5
         register-preview-function #'consult-register-format)

   ;; Optionally tweak the register preview window.
   ;; This adds thin lines, sorting and hides the mode line of the window.
   (advice-add #'register-preview :override #'consult-register-window)

   ;; Use Consult to select xref locations with preview
   (setq xref-show-xrefs-function #'consult-xref
         xref-show-definitions-function #'consult-xref)
   ;; Configure other variables and modes in the :config section,
   ;; after lazily loading the package.
   :config

   ;; Optionally configure preview. The default value
   ;; is 'any, such that any key triggers the preview.
   ;; (setq consult-preview-key 'any)
   ;; (setq consult-preview-key "M-.")
   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
   ;; For some commands and buffer sources it is useful to configure the
   ;; :preview-key on a per-command basis using the `consult-customize' macro.
     (consult-customize
      consult-theme :preview-key '(:debounce 0.2 any)
      consult-ripgrep consult-git-grep consult-grep
      consult-bookmark consult-recent-file consult-xref
      consult-source-bookmark consult-source-file-register
      consult-source-recent-file consult-source-project-recent-file
      ;; :preview-key "M-."
      :preview-key '(:debounce 0.4 any))

   ;; Optionally configure the narrowing key.
   ;; Both < and C-+ work reasonably well.
   (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
    )

;; ============================================================================
;; EMBARK-CONSULT INTEGRATION
;; ============================================================================

(use-package embark-consult)

;; ============================================================================
;; HYDRA (TRANSIENT KEYBINDINGS)
;; ============================================================================

(use-package hydra)

(provide 'completion)
;;; completion.el ends here
