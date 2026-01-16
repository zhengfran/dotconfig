;;; navigation.el --- Navigation tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigation tools: avy (jump), treemacs (file tree), multiple-cursors
;;
;; DEPENDENCIES: evil, keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; TREEMACS (FILE TREE)
;; ============================================================================

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(defun treemacs-adjust-width-to-fit ()
  "Adjust Treemacs window width to fit the longest filename."
  (let ((max-length (apply 'max
                           (mapcar 'string-width
                                   (treemacs--get-children-of (treemacs-current-root)))))
        (treemacs-default-width 30)) ;; Default width if there are no entries
    (treemacs-resize-to-width (max 30 (+ 5 max-length))))) ;; Add 5 to accommodate icons

;; Hook into window selection to auto-adjust width
(add-hook 'treemacs-select-window-hook 'treemacs-adjust-width-to-fit)

;; ============================================================================
;; AVY (JUMP TO CHAR/WORD/LINE)
;; ============================================================================

(use-package avy
  :demand 1
  :after general
  :config
  (zzc/leader-keys
    "j" '(:ignore t :which-key "jump")
    "jj" '(avy-goto-char :which-key "jump to char")
    "jw" '(avy-goto-word-0 :which-key "jump to word")
    "jl" '(avy-goto-line :which-key "jump to line")))

;; ============================================================================
;; MULTIPLE CURSORS
;; ============================================================================

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines))
  :config
  ;; Evil mode integration
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g z") 'mc/mark-next-like-this)
    (define-key evil-normal-state-map (kbd "g Z") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "g z") 'mc/mark-all-like-this))
  
  ;; Leader key bindings
  (zzc/leader-keys
    "m"  '(:ignore t :which-key "multiple-cursors")
    "mn" '(mc/mark-next-like-this :which-key "mark next")
    "mp" '(mc/mark-previous-like-this :which-key "mark previous")
    "ma" '(mc/mark-all-like-this :which-key "mark all")
    "ml" '(mc/edit-lines :which-key "edit lines")
    "mr" '(mc/mark-all-in-region :which-key "mark all in region")
    "ms" '(mc/skip-to-next-like-this :which-key "skip to next")
    "mu" '(mc/unmark-next-like-this :which-key "unmark next")))

(provide 'navigation)
;;; navigation.el ends here
