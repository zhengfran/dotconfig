;;; denote-config.el --- Denote note-taking configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Denote knowledge management system: configuration, journal notes,
;; project management, consult-denote integration, org-noter, org-anki
;;
;; DEPENDENCIES: core (my/org-base-dir), org-base, org-agenda-config (org-agenda-files),
;;               completion (consult-denote), keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; TRADE CAPTURE SYSTEM (preserved from org-roam)
;; ============================================================================

(defvar my/trade-template-dir (expand-file-name "~/org/templates/trades/")
  "Base directory for all trade strategy template notes.")

(defun my/trade-list-templates ()
  "Return list of trade template base names without extension."
  (mapcar #'file-name-base
          (directory-files my/trade-template-dir nil "\\.org$")))

(defun my/trade-read-template (name)
  "Read trade template NAME and return contents."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat name ".org") my/trade-template-dir))
    (buffer-string)))

(defun my/denote-capture-trade ()
  "Capture a new trade note using strategy template selection."
  (interactive)
  (let* ((strategy (completing-read
                    "Strategy: "
                    (my/trade-list-templates)
                    nil t))
         (body (my/trade-read-template strategy))
         (title (format-time-string "%Y-%m-%d-%H-%M-%S Trade"))
         (keywords (list "trade" strategy))
         (subdir (expand-file-name "trades" denote-directory)))
    ;; Ensure trades directory exists
    (unless (file-exists-p subdir)
      (make-directory subdir t))
    ;; Create denote note in trades/ subdirectory
    (denote title keywords 'org subdir nil body)))

;; ============================================================================
;; DAILY/JOURNAL NOTE HELPERS
;; ============================================================================

(defun my/denote-get-journal-note-file (&optional date)
  "Get the file path for journal note on DATE (defaults to today).
Returns nil if no journal note exists for that date."
  (let* ((time (or date (current-time)))
         (date-str (format-time-string "%Y%m%d" time))
         (journal-dir (expand-file-name "journal" denote-directory))
         (file (when (file-exists-p journal-dir)
                 (car (directory-files journal-dir nil
                                       (concat "^" date-str "T[0-9]\\{6\\}--.*\\.org$"))))))
    (when file
      (expand-file-name file journal-dir))))

(defun my/denote-ensure-journal-note-exists ()
  "Ensure today's journal note exists, creating it if necessary.
Returns the file path of today's journal note."
  (or (my/denote-get-journal-note-file)
      (my/denote-create-journal-note)
      (my/denote-get-journal-note-file)))

(defun my/denote-create-journal-note (&optional date)
  "Create journal note for DATE (defaults to today) with template.
Uses template from ~/org/templates/daily.org if it exists.
Returns the file path of the created note."
  (interactive)
  (let* ((time (or date (current-time)))
         (title (format-time-string "%Y-%m-%d %a" time))
         (template-file "~/org/templates/daily.org")
         (template (if (file-exists-p template-file)
                       (with-temp-buffer
                         (insert-file-contents template-file)
                         (buffer-string))
                     ;; Default template if file doesn't exist
                                           "* What's done today?\n\n#+BEGIN: tasks-done-today\n#+END:\n\n* Notes\n\n"))
         (subdir (expand-file-name "journal" denote-directory)))
    ;; Ensure journal directory exists
    (unless (file-exists-p subdir)
      (make-directory subdir t))
    ;; Create denote note with specific date
    (denote title '("journal") 'org subdir time template)
    (message "Created journal note for %s" title)
    ;; Return the file path (denote switches to the new buffer)
    (buffer-file-name)))

(defun my/denote-journal-goto-today ()
  "Open or create today's journal note."
  (interactive)
  (let ((journal-file (my/denote-ensure-journal-note-exists)))
    (find-file journal-file)))

(defun my/denote-journal-goto-yesterday ()
  "Open yesterday's journal note, creating if needed."
  (interactive)
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
         (file (or (my/denote-get-journal-note-file yesterday)
                   (my/denote-create-journal-note yesterday))))
    (find-file file)))

(defun my/denote-journal-goto-tomorrow ()
  "Open or create tomorrow's journal note."
  (interactive)
  (let* ((tomorrow (time-add (current-time) (days-to-time 1)))
         (file (or (my/denote-get-journal-note-file tomorrow)
                   (my/denote-create-journal-note tomorrow))))
    (find-file file)))

(defun my/denote-journal-goto-date ()
  "Open or create journal note for a specific date selected from calendar."
  (interactive)
  (let* ((time (org-read-date nil t nil "Journal note date:"))
         (file (or (my/denote-get-journal-note-file time)
                   (my/denote-create-journal-note time))))
    (find-file file)))

;; ============================================================================
;; COMPLETED TASKS DYNAMIC BLOCK
;; ============================================================================

(defun my/org-format-clock-time (minutes)
  "Format MINUTES as H:MM string, or '—' if zero/negative."
  (if (or (null minutes) (<= minutes 0))
      "—"
    (format "%d:%02d" (/ minutes 60) (% minutes 60))))

(defun my/org-get-task-clock-time (file pos)
  "Get total clocked minutes for task at POS in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (let ((org-clock-sum-current-start-time nil)
            (total 0))
        (save-restriction
          (org-narrow-to-subtree)
          (goto-char (point-min))
          (while (re-search-forward org-clock-line-re nil t)
            (when-let* ((ts (match-string 2))
                        (duration (org-duration-to-minutes ts)))
              (cl-incf total duration))))
        total))))

(defun my/org-get-completed-tasks-on-date (date-str)
  "Return list of completed tasks on DATE-STR (YYYY-MM-DD).
Each element is (HEADING CLOCK-MINUTES FILE POS)."
  (let ((results nil)
        (closed-re (format "CLOSED: \\[%s .*?\\]" (regexp-quote date-str))))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward closed-re nil t)
              (when-let* ((pos (save-excursion
                                 (org-back-to-heading t)
                                 (point)))
                          (heading (save-excursion
                                     (goto-char pos)
                                     (org-get-heading t t t t)))
                          (clock-mins (my/org-get-task-clock-time file pos)))
                (push (list heading clock-mins file pos) results)))))))
    (nreverse results)))

(defun my/org-extract-date-from-journal-filename ()
  "Extract date string (YYYY-MM-DD) from current buffer filename.
Expects denote journal filename format: YYYYMMDDTHHMMSS--title.org"
  (let ((filename (buffer-file-name)))
    (when (and filename (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T" filename))
      (format "%s-%s-%s"
              (match-string 1 filename)
              (match-string 2 filename)
              (match-string 3 filename)))))

(defun org-dblock-write:tasks-done-today (params)
  "Dynamic block showing tasks completed on journal date.
PARAMS is ignored (date comes from filename)."
  (let* ((date-str (my/org-extract-date-from-journal-filename))
         (tasks (when date-str
                  (my/org-get-completed-tasks-on-date date-str))))
    (if (null tasks)
        (insert "| No tasks completed on this day |")
      (insert "| Task | Time spent | Link |\n")
      (insert "|------+------------+------|\n")
      (dolist (task tasks)
        (let* ((heading (nth 0 task))
               (mins (nth 1 task))
               (file (nth 2 task))
               (pos (nth 3 task))
               (time-str (my/org-format-clock-time mins))
               (link (format "[[file:%s::*%s][%s]]"
                             file
                             (url-hexify-string heading)
                             (file-name-nondirectory file))))
           (insert (format "| %s | %s | %s |\n" heading time-str link)))))))

;; ============================================================================
;; FILTER FUNCTIONS (denote-based)
;; ============================================================================

(defun my/denote-list-notes-by-keyword (keyword)
  "Return a list of file paths for all denote notes with KEYWORD."
  (denote-directory-files (concat "_" keyword)))

(defun my/denote-list-project-files ()
  "Return list of all project note files."
  (my/denote-list-notes-by-keyword "project"))

;; ============================================================================
;; AGENDA INTEGRATION
;; ============================================================================

;; Agenda integration removed - org-agenda-files now set statically in org-agenda-config.el

;; ============================================================================
;; DENOTE CONFIGURATION
;; ============================================================================

(use-package denote
  :demand t
  :custom
  (denote-directory my/org-base-dir)
  (denote-known-keywords '("project" "journal" "trade" "work" "review" "ref" "archived" "habit"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-eile-type 'org)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  :bind
  (("C-c n l" . denote-backlinks)
   ("C-c n f" . denote-open-or-create)
   ("C-c n c" . denote)
   ("C-c n C" . denote-type)
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-link)
   ("C-c n t" . my/denote-capture-task)
   ("C-c n P" . my/denote-insert-new-project)
   ("C-c n p" . my/denote-find-project)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n a" . denote-keywords-add)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  ;; Ensure denote-backlinks buffer uses UTF-8 encoding for Chinese characters
  (defun my/denote-backlinks-set-encoding ()
    "Set UTF-8 encoding for denote-backlinks buffer to properly display Chinese characters."
    (when (string-match-p "\\*denote-backlinks\\*" (buffer-name))
      (set-buffer-file-coding-system 'utf-8-unix t)
      (setq buffer-file-coding-system 'utf-8-unix)))

  ;; Hook into denote-backlinks-mode if it exists
  (add-hook 'denote-backlinks-mode-hook #'my/denote-backlinks-set-encoding)

  ;; Also set encoding after denote-backlinks command runs
  (advice-add 'denote-backlinks :after
              (lambda (&rest _)
                (when (get-buffer "*denote-backlinks*")
                  (with-current-buffer "*denote-backlinks*"
                    (set-buffer-file-coding-system 'utf-8-unix t)
                    (setq buffer-file-coding-system 'utf-8-unix)))))

  ;; Create subdirectories if they don't exist
  (dolist (subdir '("journal" "ref" "trades" "habits" "projects"))
    (let ((dir (expand-file-name subdir denote-directory)))
      (unless (file-exists-p dir)
        (make-directory dir t))))

  ;; Leader key bindings
  (zzc/leader-keys
    "n" '(:ignore t :which-key "notes")
    "n f" '(denote-open-or-create :which-key "find note")
    "n i" '(denote-link-or-create :which-key "insert link")
    "n c" '(denote :which-key "create note")
    "n j" '(my/denote-create-journal-note :which-key "new journal")
    "n d" '(my/denote-journal-goto-today :which-key "goto today")
    "n y" '(my/denote-journal-goto-yesterday :which-key "goto yesterday")
    "n m" '(my/denote-journal-goto-tomorrow :which-key "goto tomorrow")
    "n D" '(my/denote-journal-goto-date :which-key "goto date")
    "n p" '(my/denote-find-project :which-key "find project")
    "n P" '(my/denote-insert-new-project :which-key "new project")
    "n r" '(my/denote-capture-trade :which-key "capture trade")
    "n t" '(my/denote-capture-task :which-key "capture task")
    "n Y" '(my/denote-goto-year :which-key "goto year")
    "n M" '(my/denote-goto-month :which-key "goto month")
    "n l" '(denote-backlinks :which-key "backlinks")))

;; ============================================================================
;; YEAR/MONTH NAVIGATION
;; ============================================================================

(defun my/denote-goto-month ()
  "Navigate to or create a monthly planning note in journal/ subdirectory."
  (interactive)
  (let* ((month-str (format-time-string "%Y-%B"))
         (regexp (concat ".*--" (regexp-quote (downcase month-str))))
         (existing (car (denote-directory-files regexp)))
         (subdir (expand-file-name "journal" denote-directory)))
    (if existing
        (find-file existing)
      (unless (file-exists-p subdir)
        (make-directory subdir t))
      (denote month-str '("journal") 'org subdir nil
              "\n* Goals\n\n* Summary\n\n"))))

(defun my/denote-goto-year ()
  "Navigate to or create a yearly planning note in journal/ subdirectory."
  (interactive)
  (let* ((year-str (format-time-string "%Y"))
         (regexp (concat ".*--" year-str "$"))
         (existing (car (denote-directory-files regexp)))
         (subdir (expand-file-name "journal" denote-directory)))
    (if existing
        (find-file existing)
      (unless (file-exists-p subdir)
        (make-directory subdir t))
      (denote year-str '("journal") 'org subdir nil
              "\n* Goals\n\n* Summary\n\n"))))

;; ============================================================================
;; CONSULT-DENOTE
;; ============================================================================

(use-package consult-denote
  :after denote
  :config
  (consult-denote-mode 1)
  :bind
  ("C-c n e" . consult-denote-find)
  ("C-c n b" . consult-denote-backlinks)
  ("C-c n g" . consult-denote-grep))

;; ============================================================================
;; ORG-NOTER (unchanged from org-roam)
;; ============================================================================

(use-package org-noter
  :bind
  (("C-c n n" . org-noter)
   :map org-noter-doc-mode-map
   ("M-e" . org-noter-insert-precise-note))
  :custom
  (org-noter-highlight-selected-text t)
  (org-noter-notes-search-path (list (expand-file-name "ref/" my/org-base-dir)))
  (org-noter-auto-save-last-location t))

;; ============================================================================
;; ORG-ANKI (unchanged from org-roam)
;; ============================================================================

(use-package org-anki
  :after org
  :config
  (setq org-anki-default-deck "Mega"))

;; ============================================================================
;; PROJECT MANAGEMENT
;; ============================================================================

(defun my/denote-insert-new-project ()
  "Create a new project note in projects/ subdirectory."
  (interactive)
  (let* ((title (read-string "Project title: "))
         (subdir (expand-file-name "projects" denote-directory)))
    (unless (file-exists-p subdir)
      (make-directory subdir t))
    (denote title '("project") 'org subdir nil
            "* Tasks\n\n")))

(defun my/denote-find-project ()
  "Find and open a project note."
  (interactive)
  (let* ((project-files (my/denote-list-project-files))
         (candidates (mapcar (lambda (f)
                              (cons (denote-retrieve-title-value f 'org) f))
                            project-files)))
    (if (null candidates)
        (message "No project files found. Create one with SPC n P")
      (let* ((selected (completing-read "Project: " candidates nil t))
             (file (cdr (assoc selected candidates))))
        (find-file file)))))

(defun my/denote-capture-task ()
  "Capture a task to an existing project."
  (interactive)
  (let* ((project-files (my/denote-list-project-files))
         (candidates (mapcar (lambda (f)
                              (cons (denote-retrieve-title-value f 'org) f))
                            project-files)))
    (if (null candidates)
        (message "No project files found. Create one with SPC n P")
      (let* ((selected (completing-read "Project: " candidates nil t))
             (file (cdr (assoc selected candidates))))
        (find-file file)
        (goto-char (point-min))
        (if (re-search-forward "^\\* Tasks" nil t)
            (progn
              (org-end-of-subtree t t)
              (insert "** TODO "))
          (goto-char (point-max))
          (insert "\n** TODO "))))))

;; ============================================================================
;; REFILE CONFIGURATION
;; ============================================================================

(defun my/org-refile-update-targets ()
  "Update `org-refile-targets` to match `org-agenda-files`."
  (when (boundp 'org-agenda-files)
    (setq org-refile-targets
          (mapcar (lambda (file) (cons file '(:maxlevel . 3))) org-agenda-files))))

;; Only run if org-agenda-files is already defined
(when (boundp 'org-agenda-files)
  (my/org-refile-update-targets))

(provide 'denote-config)
;;; denote-config.el ends here
