(setq org_notes_dir "~/Documents/org/notes" ; org notes location
      zot_bib "~/Nutstore/1/Nutstore/Zotero-Library/Main.bib"; Zotero .bib 文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 同步文件
      org_notes "~/Documents/org/notes/ref/") ; org-roam 文献笔记目录

(unless (file-exists-p org_notes_dir) (setq org_notes_dir nil))
(unless (file-exists-p zot_bib) (setq zot_bib nil))
(unless (file-exists-p zot_pdf) (setq zot_pdf nil))
(unless (file-exists-p org_notes) (setq org_notes nil)) ; 防止文件不存在报错

(setq my/is-windows (eq system-type 'windows-nt)) ; Windows
(setq my/is-linux (eq system-type 'gnu/linux)) ; Linux
(setq my/is-mac (eq system-type 'darwin)) ; mac
(setq my/is-WSL
      (if (and (eq system-type 'gnu/linux)
               (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
          t
        nil)) ; WSL
(setq my/is-terminal (not window-system)) ;GUI

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(defvar toggle-one-window-window-configuration nil
  "The window configuration use for `toggle-one-window'.")
(defun toggle-one-window ()
  "Toggle between window layout and one window."
  (interactive)
  (if (equal (length (cl-remove-if #'window-dedicated-p (window-list))) 1)
      (if toggle-one-window-window-configuration
          (progn
            (set-window-configuration toggle-one-window-window-configuration)
            (setq toggle-one-window-window-configuration nil))
        (message "No other windows exist."))
    (setq toggle-one-window-window-configuration (current-window-configuration))
    (delete-other-windows)))
(general-define-key
 :prefix "C-c"
 "m" 'toggle-one-window)

(after! persp-mode
  (setq persp-state-default-file (expand-file-name ".persp-save" doom-cache-dir))
  (defun my/persp-state-save-silent ()
    "Save perspective state without confirmation."
    (let ((persp-state-save-behavior nil)) ; Prevent prompting
      (persp-save-state-to-file persp-state-default-file)))

    ;; Load perspectives without confirmation
  (defun my/persp-state-load-silent ()
      "Load perspective state without confirmation."
      (when (file-exists-p persp-state-default-file)
        (persp-load-state-from-file persp-state-default-file)))
   ;; Automatically save perspectives when Emacs quits
    (add-hook 'kill-emacs-hook #'my/persp-state-save-silent)
    ;; Automatically load perspectives at startup
    (add-hook 'emacs-startup-hook #'my/persp-state-load-silent)
  (persp-mode +1))  ;; Enable persistence mode

(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq doom-font (font-spec :family "JetBrains Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 34))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-modeline-persp-name t) ;; Show workspace name in modeline
(setq doom-modeline-display-default-persp-name t) ;; Display the default workspace name

(setq org-directory "~/Documents/org/")

(after! eee
  (setq ee-terminal-command "st") ; Set terminal command
  (map! :leader
        (:prefix ("t" . "toggle")  ; Prefix for toggle-related commands
         :desc "Lazygit" "z" #'ee-lazygit
         :desc "Yazi" "y" #'ee-yazi)))

(after! org-ai
  (setq org-ai-default-chat-model "gpt-4o") ; Set default chat model

  (add-hook 'org-mode-hook #'org-ai-mode) ; Enable org-ai in org-mode
  (org-ai-global-mode) ; Enable global keybindings

  ;; Install yasnippets if yasnippet is enabled
  (when (featurep! :editor snippets)
    (org-ai-install-yasnippets)))

(after! gptel
  ;; Set API key securely
  (setq gptel-api-key (auth-source-pick-first-password :host "openai.com"))

  ;; Set GPT model
  (setq gptel-model "gpt-4-turbo") ;; Use desired model

  ;; Define custom prompt templates
  (setq gptel-prompt-templates
        '(("Journal Analysis"
           :system "I’d like you to take on the role of a supportive and understanding life coach. For this session, I want to imagine the best life possible across various areas of my life, including relationships, career, health, and mental well-being."
           :user "Analyze the following journal entry and provide actionable advice in Chinese. Output the possible TODO items in Emacs Org TODO format with level 3 heading\n\n{{input}}"))))

;; Function to analyze the current buffer using GPTel
(defun my/gptel-analyze-current-buffer ()
  "Send the content of the current buffer to GPTel using a saved prompt template."
  (interactive)
  (let* ((buffer-content (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (buffer-substring-no-properties (point-min) (point-max))))
         (template (assoc "Journal Analysis" gptel-prompt-templates)) ;; Retrieve the template
         (system-message (plist-get (cdr template) :system))
         (user-prompt (plist-get (cdr template) :user))
         (final-prompt (replace-regexp-in-string "{{input}}" buffer-content user-prompt))) ;; Replace {{input}}
    (gptel-request final-prompt :system system-message)))

(after! rime
  (setq rime-user-data-dir "~/dotconfig/rime")
  (setq default-input-method "rime"
        rime-show-candidate 'posframe
        rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-ascii-char-p
          rime-predicate-hydra-p
          rime-predicate-tex-math-or-command-p
          rime-predicate-prog-in-code-p))
  ;; Prevent rime crash on exit
  (defun rime-lib-finalize () nil)
  (add-hook 'kill-emacs-hook #'rime-lib-finalize))

(after! pangu-spacing
  (setq pangu-spacing-real-insert-separator t) ;; Enable real spacing
  (global-pangu-spacing-mode 1)) ;; Enable globally
