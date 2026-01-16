;;; org-babel.el --- Org Babel Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for org-babel: code block execution, formatting, and tangling
;;
;; DEPENDENCIES: org-base, keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; Load languages for org-babel execution
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
    (python . t)
    (shell . t)))

(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org source block editing behavior
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0  ; No extra indentation
      org-src-tab-acts-natively t          ; TAB works in native mode
      org-src-fontify-natively t           ; Syntax highlighting
      org-src-window-setup 'current-window) ; Edit in current window

(defun my/org-src-format-buffer ()
  "Format code in org-src-edit buffer based on major mode."
  (when (and (derived-mode-p 'prog-mode)
             (not org-src-preserve-indentation))
    (condition-case err
        (cond
         ;; Emacs Lisp
         ((eq major-mode 'emacs-lisp-mode)
          (require 'elisp-autofmt)
          (elisp-autofmt-buffer))
         ;; Shell scripts
         ((or (eq major-mode 'sh-mode) (eq major-mode 'shell-script-mode))
          (when (executable-find "shfmt")
            (shell-command-on-region (point-min) (point-max) 
                                     "shfmt" 
                                     (current-buffer) t 
                                     "*shfmt errors*" t)))
         ;; Fallback for other modes
         (t (indent-region (point-min) (point-max))))
      (error 
       (message "Formatting failed: %s, using indent-region" err)
       (indent-region (point-min) (point-max))))))

;; Add formatting hook to org-src-mode
(add-hook 'org-src-mode-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (add-hook 'before-save-hook #'my/org-src-format-buffer nil t))))

;; Keybindings for formatting code blocks
(with-eval-after-load 'org
  ;; Format entire code block from org-mode (C-c f b)
  (define-key org-mode-map (kbd "C-c f b") 
    (lambda () 
      (interactive)
      (if (org-in-src-block-p)
          (progn
            (org-edit-special)
            (my/org-src-format-buffer)
            (org-edit-src-exit))
        (message "Not in a source block"))))
  
  ;; Format region in code block (C-c f r)
  (define-key org-mode-map (kbd "C-c f r")
    (lambda ()
      (interactive)
      (when (org-in-src-block-p)
        (org-edit-special)
        (indent-region (region-beginning) (region-end))
        (org-edit-src-exit)))))

;; Keybindings when editing code blocks with C-c '
(with-eval-after-load 'org-src
  (define-key org-src-mode-map (kbd "C-c C-f") 
    (lambda ()
      (interactive)
      (my/org-src-format-buffer)))
  (define-key org-src-mode-map (kbd "C-c C-r")
    (lambda ()
      (interactive)
      (when (use-region-p)
        (indent-region (region-beginning) (region-end))))))

;; Add to leader key (SPC f)
(with-eval-after-load 'general
  (zzc/leader-keys
    "f"  '(:ignore t :which-key "format")
    "fb" '((lambda () 
             (interactive)
             (if (and (eq major-mode 'org-mode) (org-in-src-block-p))
                 (progn
                   (org-edit-special)
                   (my/org-src-format-buffer)
                   (org-edit-src-exit))
               (indent-region (point-min) (point-max))))
           :which-key "format buffer")
    "fr" '((lambda ()
             (interactive)
             (when (use-region-p)
               (indent-region (region-beginning) (region-end))))
           :which-key "format region")))

;; automatically tangle our emacs.org config file when we save it
(defun zzc/org-babel-tangle-config ()
  (when (string-equal (file-truename (buffer-file-name))
		      (file-truename (expand-file-name "~/dotconfig/basic/editor/emacs/default/config.org")))
    ;; dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook 
          (lambda () 
            (add-hook 'after-save-hook #'zzc/org-babel-tangle-config nil t)))

(provide 'org-babel)
;;; org-babel.el ends here
