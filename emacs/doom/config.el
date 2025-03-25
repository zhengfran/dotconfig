;; -*- lexical-binding: t; -*-
(setq org_notes_dir "~/Documents/org/notes" ; org notes location
      zot_bib "~/Nutstore/1/Nutstore/Zotero-Library/Main.bib"; Zotero .bib 文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 同步文件
      org_notes "~/Documents/org/notes/ref/") ; org-roam 文献笔记目录
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))
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

(after! evil
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.2)) ;; Adjust delay if needed

(after! super-save
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq super-save-delete-trailing-whitespace t)
(setq auto-save-default nil)
(setq super-save-exclude '(".gpg")))

(defun my/resize-window-up ()
  "Increase window height repeatedly."
  (interactive)
  (enlarge-window 1)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<up>") 'my/resize-window-up)
     map)
   t))

(defun my/resize-window-down ()
  "Decrease window height repeatedly."
  (interactive)
  (shrink-window 1)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<down>") 'my/resize-window-down)
     map)
   t))

(defun my/resize-window-right ()
  "Increase window width repeatedly."
  (interactive)
  (enlarge-window-horizontally 1)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<right>") 'my/resize-window-right)
     map)
   t))

(defun my/resize-window-left ()
  "Decrease window width repeatedly."
  (interactive)
  (shrink-window-horizontally 1)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<left>") 'my/resize-window-left)
     map)
   t))

;; Bind these to SPC + arrow keys
(map! :leader
      :desc "Resize window up"    "<up>"    #'my/resize-window-up
      :desc "Resize window down"  "<down>"  #'my/resize-window-down
      :desc "Resize window right" "<right>" #'my/resize-window-right
      :desc "Resize window left"  "<left>"  #'my/resize-window-left)

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

(require 'sort-tab)
(sort-tab-mode 1)
(defun my/sort-tab-hide-buffer-not-in-current-workspace (buffer)
  "Hide BUFFER if it's not in the current Doom workspace."
  (let ((buf-name (buffer-name buffer)))
    (not (member buf-name (mapcar #'buffer-name (+workspace-buffer-list))))))
(setq sort-tab-hide-function #'my/sort-tab-hide-buffer-not-in-current-workspace)
(map! :n "H" #'sort-tab-select-prev-tab
      :n "L" #'sort-tab-select-next-tab)

;; save doom emacs session every 15 minute
(run-with-timer 900 900 #'doom/quicksave-session)

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
(global-visual-line-mode)

(setq doom-modeline-persp-name t) ;; Show workspace name in modeline
(setq doom-modeline-display-default-persp-name t) ;; Display the default workspace name

(setq org-directory "~/Documents/org/")

;; Set bold text color after Org and theme load
(after! org
  (setq org-hide-emphasis-markers t)
  (custom-set-faces!
    '(org-bold :foreground "#FF5555" :weight bold :inherit nil)))

(after! org
  (setq org-agenda-dir "~/Documents/org/jira/")
  (setq org-agenda-files (directory-files-recursively org-agenda-dir "\\.org$"))
  ;; save all org files after change todo
  (defmacro η (fnc)
     "return function that ignores its arguments and invokes fnc."
     `(lambda (&rest _rest)
      (funcall ,fnc)))
  (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
  (advice-add 'org-priority       :after (η #'org-save-all-org-buffers)))

(after! org
  (defun my/org-refile-update-targets ()
    "Update `org-refile-targets` to match `org-agenda-files`."
    (setq org-refile-targets
          (mapcar (lambda (file) (cons file '(:maxlevel . 3))) org-agenda-files)))

  ;; Run once on startup
  (my/org-refile-update-targets)

  ;; Update targets whenever `org-agenda-files` changes
  (add-hook! 'org-agenda-mode-hook #'my/org-refile-update-targets)
  (add-hook! 'org-mode-hook #'my/org-refile-update-targets))

(after! org-pomodoro
  (setq org-pomodoro-audio-player "mpv"
    org-pomodoro-ticking-sound-p t
    org-pomodoro-ticking-sound-states '(:pomodoro)
    org-pomodoro-finished-sound-p t
    org-pomodoro-short-break-length 5
    org-pomodoro-finished-sound-args "--volume=50"
    org-pomodoro-long-break-sound-args "--volume=50"
    org-pomodoro-short-break-sound-args "--volume=50"
    org-pomodoro-ticking-sound-args "--volume=60"))

(after! org-jira
  (setq org-jira-working-dir "~/Documents/org/jira/")
  (setq jiralib-url "https://jira.vni.agileci.conti.de")
  (setq jiralib-token
    (cons "Authorization"
      (concat "Bearer " (auth-source-pick-first-password
			 :host "jira.vni.agileci.conti.de"))))
  (setq org-jira-use-status-as-todo nil)
  (setq org-jira-jira-status-to-org-keyword-alist
   '(("Working" . "STRT")
     ("New" . "TODO")
     ("Ready" . "TODO")
     ("Closed" . "DONE")
     ("Verifying" . "STRT"))))

(after! org-anki
  (setq org-anki-default-deck "Mega"))

(setq my/daily-note-filename "%<%Y-%m-%d>.org"
      my/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-w%W>]]\n\n[[roam:%<%Y-%B>]]\n\n* Tasks\n** Completed\n** Meeting\n\n* Capture\n** Information\n** Opinions\n** Tools\n** Feelings\n\n* Reflection\n** One thing Good\n** One thing Bad\n** Questions to my self\n*** All the decisions make today, which are by choice, and which are by fear?\n* AI Summary")
(defvar my/org-roam-project-template
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "%<%Y%m%d%H>-${slug}.org"
                           "#+title: ${title}\n\n#+category: ${title}\n#+filetags: Project\n"
                           ("tasks"))))
(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))
(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
    (seq-filter
      (my/org-roam-filter-by-tag tag-name)
      (org-roam-node-list))))
(defun my/org-roam-filter-by-tags (wanted unwanted)
  (lambda (node)
  (let ((node-tags (org-roam-node-tags node)))
    (and (cl-some (lambda (tag) (member tag node-tags)) wanted)
         (not (cl-some (lambda (tag) (member tag node-tags)) unwanted))))))
(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files
        (delete-dups (append org-agenda-files
                             (my/org-roam-list-notes-by-tag "Project")))))
;; for projects
(defun my/org-roam-project-finalize-hook ()
  "adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
  ;; remove the hook since it was addd temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-insert-new-project ()
  (interactive)
  ;; add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; select a project file to open, creating it if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates (list my/org-roam-project-template)))

(defun my/org-roam-find-project ()
  (interactive)
  ;; add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tags '("Project") '("Archived"))))
;; new todo in project
(defun my/org-roam-capture-task ()
(interactive)
;; update org-agenda list after adding projects
(add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;; new todo
(org-roam-capture- :node (org-roam-node-read
                          nil
                          (my/org-roam-filter-by-tag "Project"))
                   :templates (list my/org-roam-project-template)))

(defun my/org-roam-copy-todo-to-today ()
 (interactive)
 (unless (or (string= (buffer-name) "*habit*") ; do nothing in habit buffer
         (string= (org-entry-get nil "STYLE") "habit")) ; skip if the task is a habit
   (let ((org-refile-keep t) ; set this to nil to delete the original!
         (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%d-%d>\n" ("Done")))))
         (org-after-refile-insert-hook #'save-buffer)
         today-file
         pos)
     (save-window-excursion
       (org-roam-dailies--capture (current-time) t)
       (setq today-file (buffer-file-name))
       (setq pos (point)))
     ;; only refile if the target file is different than the current file
     (unless (equal (file-truename today-file)
                    (file-truename (buffer-file-name)))
       (org-refile nil nil (list "Done" today-file nil pos))))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; Org-roam configuration for Doom Emacs
(after! org-roam
  (setq org-roam-directory "~/Documents/org/notes/"
        org-roam-completion-everywhere t
        org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?"
            :if-new (file+head ,my/daily-note-filename
                              ,my/daily-note-header)))
        org-roam-capture-templates
        '(
          ("d" "default" plain "- tag :: \n %?"
           :target
           (file+head "%<%y%m%d%h%m%s>-${slug}.org" "#+title: ${title} \n")
           :unnarrowed t)
          ("h" "Hugo Blog Post" plain
          (file "~/Documents/org/templates/hugo-post.org")
            :target (file+head "%<%y%m%d%h%m%s>-${slug}.org" "")
            :unnarrowed t)
        )
  )
  ;; Keybindings
  (map! :leader
        :desc "Toggle org-roam buffer" "n r l" #'org-roam-buffer-toggle
        :desc "Capture org-roam note" "n r c" #'org-roam-capture
        :desc "Insert org-roam node" "n r i" #'org-roam-node-insert
        :desc "Insert immediate org-roam node" "n r I" #'org-roam-node-insert-immediate
        :desc "Capture org-roam task" "n r t" #'my/org-roam-capture-task
        :desc "Insert new project" "n r P" #'my/org-roam-insert-new-project
        :desc "Find project" "n r p" #'my/org-roam-find-project
        :desc "Toggle org-roam UI" "n r u" #'org-roam-ui-mode)

  ;; Additional keybindings for Org mode
  (map! :map org-mode-map
        "C-M-i" #'completion-at-point)
  (advice-add 'org-agenda :before #'my/org-roam-refresh-agenda-list))

(use-package! org-roam-ui
  :config
    (setq org-roam-ui-sync-theme t)
    (setq org-roam-ui-follow t)
    (setq org-roam-ui-update-on-save t)
    (setq org-roam-ui-open-on-start t))

(use-package! org-media-note
  :init (setq org-media-note-use-org-ref t)
  :hook (org-mode .  org-media-note-mode)
  :config
  (setq org-media-note-screenshot-image-dir "~/Documents/org/notes/images/")  ;; Folder to save screenshot
  (setq org-media-note-use-refcite-first t)  ;; use videocite link instead of video link if possible
  (map! :leader
        (:prefix ("n" . "notes")
         :desc "media note" "m" 'org-media-note-show-interface)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
    (python . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package! eglot-booster
	:after eglot
	:config	(eglot-booster-mode))

(use-package! citre
  :init
  ;; Optional: Enable Citre globally or in specific modes
  (add-hook 'prog-mode-hook #'citre-mode)
  ;; Bind Citre commands to Doom’s leader key
  (map! :leader
        (:prefix "c"  ; SPC c for code commands
         :desc "Jump to definition" "d" #'citre-jump
         :desc "Jump back" "b" #'citre-jump-back
         :desc "Jump back" "u" #'citre-update-this-tags-file
         :desc "Peek definition" "p" #'citre-peek))
  :config
  ;; Enable Citre in supported buffers automatically
  (citre-auto-enable-citre-mode)
  ;; Define tags file names Citre should look for
  (setq citre-default-tags-files '(".tags" "tags" "TAGS"))
  ;; Optional: Integrate with LSP if you use it
  (when (and (modulep! :tools lsp) (featurep 'lsp))
    (citre-lsp-integration)))

(after! eee
  (setq ee-terminal-command "st") ; Set terminal command
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Lazygit" "g" #'ee-lazygit
         :desc "Yazi" "y" #'ee-yazi)))

(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (setq aidermacs-config-file "~/.aider.conf.yml"))

(use-package! gptel
  :config
  ;; Retrieve API key securely
  ;; (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com" :user "apikey"))
  (setq gptel-api-key
        (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter-apikey"))

  ;; Use OpenRouter's API endpoint
  (setq gptel-backend (gptel-make-openai "OpenRouter"
                      :host "openrouter.ai"
                      :endpoint "/api/v1/chat/completions"
                      :key gptel-api-key
                      :models '(deepseek/deepseek-r1:free
                                openai/gpt-4o-2024-11-20
                                anthropic/claude-3.7-sonnet)))
  ;; Keybinding to quickly open `gptel`
  (map! :leader
     :desc "Chat with GPT via OpenRouter"
     "o p" #'gptel))

(use-package! yasnippet
  :init
  (add-hook 'yas-minor-mode-hook (lambda()
				       (yas-activate-extra-mode 'fundamental-mode)))
  :config
  (setq yas-snippet-dirs '("~/dotconfig/emacs/doom/snippets")))

(use-package! rime
  :config
  (setq rime-user-data-dir "~/dotconfig/rime")
  ;; mac needs to manually download librime and set these path for compilation
  (when my/is-mac
    (setq rime-librime-root "~/.emacs.d/librime/dist")
    (setq rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@30/30.1/include"))
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
