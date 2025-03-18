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

(map! :leader
      :repeat t  ;; Enables key repeat for these bindings
      :desc "Resize window left"  "<left>"  #'shrink-window-horizontally
      :desc "Resize window right" "<right>" #'enlarge-window-horizontally
      :desc "Resize window up"    "<up>"    #'shrink-window
      :desc "Resize window down"  "<down>"  #'enlarge-window)

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
(defun my/display-all-workspaces ()
  "Display all workspace names and numbers in the mode line."
  (let ((workspaces (persp-names)))
    (if workspaces
        (mapconcat (lambda (ws) (format "[%s]" ws)) workspaces " ")
      "No Workspaces")))

(setq-default mode-line-format
              (append mode-line-format
                      '((:eval (my/display-all-workspaces)))))
(setq doom-modeline-workspace-name (lambda () (my/display-all-workspaces)))

(setq org-directory "~/Documents/org/")

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
        '(("h" "Hugo Blog Post" plain
          (file "~/Documents/org/templates/hugo-post.org") ;; Uses external template
            :target (file+head "%<%y%m%d%h%m%s>-${slug}.org" "")
            :unnarrowed t)))
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
  (add-hook! 'after-init-hook #'my/org-roam-refresh-agenda-list))

(use-package! org-roam-ui
  :config
    (setq org-roam-ui-sync-theme t)
    (setq org-roam-ui-follow t)
    (setq org-roam-ui-update-on-save t)
    (setq org-roam-ui-open-on-start t))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (plantuml . t)
    (python . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(after! eee
  (setq ee-terminal-command "st") ; Set terminal command
  (map! :leader
        (:prefix ("t" . "toggle")  ; Prefix for toggle-related commands
         :desc "Lazygit" "z" #'ee-lazygit
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
