;;; evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil mode and all evil-related packages:
;; evil-collection, evil-commentary, evil-surround, evil-escape,
;; evil-org, evil-goggles, undo-tree integration
;;
;; DEPENDENCIES: None
;; USED BY: window, navigation, terminal, chinese (evil-set-initial-state)

;;; Code:

;; ============================================================================
;; EVIL MODE
;; ============================================================================

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-integration t)
  (setq evil-symbol-word-search t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
  
;; ============================================================================
;; EVIL COLLECTION
;; ============================================================================

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ============================================================================
;; EVIL COMMENTARY
;; ============================================================================

(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode))

;; ============================================================================
;; EVIL SURROUND
;; ============================================================================

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ============================================================================
;; UNDO-TREE
;; ============================================================================

(use-package undo-tree
  :after evil
  :diminish
  :config
  (setq undo-tree-history-directory-alist 
        `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; ============================================================================
;; EVIL ESCAPE
;; ============================================================================

(use-package evil-escape
  :init (evil-escape-mode)
  :after evil
  :config
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.3))

;; ============================================================================
;; EVIL ORG
;; ============================================================================

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key
   :states '(normal motion)
   :keymaps 'org-mode-map
   "zd" 'org-fold-hide-drawer-toggle))

;; ============================================================================
;; EVIL GOGGLES
;; ============================================================================

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-textobj-anyblock
  :after evil
  :config
  (define-key evil-inner-text-objects-map "q"
    #'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "q"
    #'evil-textobj-anyblock-a-block))
;; ============================================================================
;; AUTO-SAVE ON INSERT EXIT
;; ============================================================================

;; save file every time after quit insert mode
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(provide 'evil)
;;; evil.el ends here
