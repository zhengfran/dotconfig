;; (require 'pyim-greatdict)
;; (pyim-greatdict-enable)
;; (quelpa '(pyim-greatdict :fetcher github :repo "tumashu/pyim-greatdict"))
(map! :leader
      (:prefix ("z" . "Customize")
        :desc "Activate Chinese Input" "a" #'pyim-activate
        :desc "Deactivate Chinese Input" "d" #'pyim-deactivate
        :desc "Toggle between chinese and ascii" "t" #'pyim-toggle-input-ascii))

(setq flyspell-mode nil)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-theme 'doom-acario-light)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(defun my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Fira Code" 15)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
    ))
(defun my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'my|init-font)
  (my/better-font))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))

;; (setq elfeed-feeds (quote
;;                     (("https://www.reddit.com/r/linux.rss" reddit linux)
;;                      ("https://www.reddit.com/r/commandeadlines.rss" linux)
;;                      ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))
;; (require 'elfeed-goodies)
;; (elfeed-goodies/setup)
;; (setq elfeed-goodies/entry-pane-size 0.5)

(setq plantuml-default-exec-mode 'jar)

(defun dt/insert-todays-date (prefix)
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%A, %B %d, %Y")
                 ((equal prefix '(4)) "%m-%d-%Y")
                 ((equal prefix '(16)) "%Y-%m-%d"))))
    (insert (format-time-string format))))

(require 'calendar)
(defun dt/insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(map! :leader
      (:prefix ("i d" . "Insert date")
        :desc "Insert any date" "a" #'dt/insert-any-date
        :desc "Insert todays date" "t" #'dt/insert-todays-date))

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit archive" "a" #'(lambda () (interactive) (find-file "~/Documents/org/archive.org"))
       :desc "Edit bills" "b" #'(lambda () (interactive) (find-file "~/Documents/org/finance/bills.ledge"))
       :desc "Edit habits" "h" #'(lambda () (interactive) (find-file "~/Documents/org/habits.org"))
       :desc "Edit archive" "g" #'(lambda () (interactive) (find-file "~/Documents/org/gtd.org"))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
       :desc "Edit archive" "m" #'(lambda () (interactive) (find-file "~/Documents/org/metrics.org"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el"))))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-directory "~/Documents/org/"

        org-ellipsis " ⤵ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000))

(after! org
  (setq org-agenda-dir "~/Documents/org/"
        ;; define the refile targets
        org-agenda-files nil)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c!)" "CANC(k@)")))

;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("w" "Work Tasks" tags-todo "+work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("W" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files))))))))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! org
    (setq org-capture-templates
        `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Documents/org/gtd.org" "Inbox")
            "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("tn" "Task Without Context" entry (file+olp "~/Documents/org/gtd.org" "Inbox")
            "* TODO %?\n  %U\n  %i" :empty-lines 1)

        ;; ("j" "Journal Entries")
        ;; ("jj" "Journal" entry
        ;;      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
        ;;      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
        ;;      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
        ;;      :clock-in :clock-resume
        ;;      :empty-lines 1)
        ;; ("jm" "Meeting" entry
        ;;      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
        ;;      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
        ;;      :clock-in :clock-resume
        ;;      :empty-lines 1)

        ;; ("w" "Workflows")
        ;; ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
        ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "~/Documents/org/metrics.org" "Weight")
        "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
        ("mp" "Pushup" table-line (file+headline "~/Documents/org/metrics.org" "Pushups")
        "| %U | %^{Pushup} | %^{Notes} |" :kill-buffer t)
        ("ms" "Squat" table-line (file+headline "~/Documents/org/metrics.org" "Squat")
        "| %U | %^{Squat} | %^{Notes} |" :kill-buffer t))))

(setq org-habit-graph-column 60)

(setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("home" . ?h)
       ("work" . ?w)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("note" . ?n)
       ("idea" . ?i)))

(setq org-refile-targets
    '(("~/Documents/org/archive.org" :maxlevel . 2)
      ("~/Documents/org/gtd.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; (defun vulpea-project-p ()
;;   "Return non-nil if current buffer has any todo entry.
;;     TODO entries marked as done are ignored, meaning the this
;;     function returns nil if current buffer contains only completed
;;     tasks."
;;   (seq-find                                 ; (3)
;;    (lambda (type)
;;      (eq type 'todo))
;;    (org-element-map                         ; (2)
;;        (org-element-parse-buffer 'headline) ; (1)
;;        'headline
;;      (lambda (h)
;;        (org-element-property :todo-type h)))))

;; (defun vulpea-project-update-tag ()
;;     "Update PROJECT tag in the current buffer."
;;     (when (and (not (active-minibuffer-window))
;;                (vulpea-buffer-p))
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let* ((tags (vulpea-buffer-tags-get))
;;                (original-tags tags))
;;           (if (vulpea-project-p)
;;               (setq tags (cons "project" tags))
;;             (setq tags (remove "project" tags)))

;;           ;; cleanup duplicates
;;           (setq tags (seq-uniq tags))

;;           ;; update tags if changed
;;           (when (or (seq-difference tags original-tags)
;;                     (seq-difference original-tags tags))
;;             (apply #'vulpea-buffer-tags-set tags))))))

;; (defun vulpea-buffer-p ()
;;   "Return non-nil if the currently visited buffer is a note."
;;   (and buffer-file-name
;;        (string-prefix-p
;;         (expand-file-name (file-name-as-directory org-roam-directory))
;;         (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

;; (add-hook 'find-file-hook #'vulpea-project-update-tag)
;; (add-hook 'before-save-hook #'vulpea-project-update-tag)

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org/notes")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "work-project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Docs\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
      :unnarrowed t)))
  :bind (:map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)

(map! :leader
      (:prefix ("n" . "notes")
       :desc "Insert-Node-Immediate" "r I" #'org-roam-node-insert-immediate)))

(setq org-journal-dir "~/Documents/org/journal/"
      org-journal-file-format "%Y-%m-%d.org")
;;function to create journal based on template
(defun pc/new-buffer-p ()
    (not (file-exists-p (buffer-file-name))))

  (defun pc/insert-journal-template ()
    (let ((template-file (expand-file-name "journal-template.org" org-directory)))
      (when (pc/new-buffer-p)
        (save-excursion
          (goto-char (point-max))
          (insert-file-contents template-file)))))

  (add-hook 'org-journal-after-entry-create-hook #'pc/insert-journal-template)

(setq max-specpdl-size 13000)

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))





(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(projectile-mode t)

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;(setq lsp-bridge-path (concat straight-base-dir "straight/repos/lsp-bridge"))
;;(add-to-list 'load-path lsp-bridge-path)
;; (add-to-list 'load-path "/home/zhicheng/.emacs.d/.local/straight/repos/lsp-bridge")
;; (require 'posframe)
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
;; (setq lsp-bridge-c-lsp-server "ccls"
;;       acm-enable-english-helper nil)

(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)
(map! :leader
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

;; (setq tramp-shell-prompt-pattern  "[-a-z0-9]+{[a-z0-9]+}[0-9]+: *")
(setq tramp-shell-prompt-pattern       "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(setq tramp-terminal-type       "xterm")
(eval-after-load 'tramp '(setenv "SHELL" "/usr/bin/bash"))
(setq tramp-encoding-shell "/usr/bin/bash")
(setq tramp-verbose 10)
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (add-to-list 'tramp-remote-path "/usr/bin")

(defun my-yank-image-from-win-clipboard-through-powershell()
  "to simplify the logic, use c:/Users/Public as temporary directoy, then move it into current directoy

Anyway, if need to modify the file name, please DONT delete or modify file extension \".png\",
otherwise this function don't work and don't know the reason
"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name (format "%s" (read-from-minibuffer "Img Name:" (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))))
         ;; (file-path-powershell (concat "c:/Users/\$env:USERNAME/" file-name))
         (file-path-wsl (concat "./images/" file-name)))
    (if (file-exists-p "./images")
        (ignore)
      (make-directory "./images"))
    ;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
    (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
    (rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)
    (format "%s" file-path-wsl)
    ))

(defun my-yank-image-link-into-org-from-wsl ()
  "call `my-yank-image-from-win-clipboard-through-powershell' and insert image file link with org-mode format"
  (interactive)
  (let* ((file-path (my-yank-image-from-win-clipboard-through-powershell))
         (file-link (format "[[file:%s][%s]]" file-path (file-name-sans-extension (file-name-nondirectory file-path))))
         )
    (insert file-link)
    ))

(map! :leader
      (:prefix ("l" . "wsl")
       :desc "wsl paste image" "p" #'my-yank-image-link-into-org-from-wsl))
