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
  (let ((journal-file (my/denote-get-journal-note-file)))
    (unless journal-file
      (my/denote-create-journal-note)
      (setq journal-file (my/denote-get-journal-note-file)))
    journal-file))

(defun my/denote-create-journal-note (&optional date)
  "Create journal note for DATE (defaults to today) with template.
Uses template from ~/org/templates/daily.org if it exists."
  (interactive)
  (let* ((time (or date (current-time)))
         (title (format-time-string "%Y-%m-%d %a" time))
         (template-file "~/org/templates/daily.org")
         (template (if (file-exists-p template-file)
                       (with-temp-buffer
                         (insert-file-contents template-file)
                         (buffer-string))
                     ;; Default template if file doesn't exist
                     "* What's done today?\n\n* Notes\n\n"))
         (subdir (expand-file-name "journal" denote-directory)))
    ;; Ensure journal directory exists
    (unless (file-exists-p subdir)
      (make-directory subdir t))
    ;; Create denote note with specific date
    (denote title '("journal") 'org subdir time template)
    (message "Created journal note for %s" title)))

(defun my/denote-journal-goto-today ()
  "Open or create today's journal note."
  (interactive)
  (let ((journal-file (my/denote-ensure-journal-note-exists)))
    (find-file journal-file)))

(defun my/denote-journal-goto-yesterday ()
  "Open yesterday's journal note, creating if needed."
  (interactive)
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
         (file (my/denote-get-journal-note-file yesterday)))
    (if file
        (find-file file)
      ;; Create yesterday's note if it doesn't exist
      (my/denote-create-journal-note yesterday)
      (find-file (my/denote-get-journal-note-file yesterday)))))

(defun my/denote-journal-goto-tomorrow ()
  "Open or create tomorrow's journal note."
  (interactive)
  (let ((tomorrow (time-add (current-time) (days-to-time 1))))
    (my/denote-create-journal-note tomorrow)
    (find-file (my/denote-get-journal-note-file tomorrow))))

(defun my/denote-journal-goto-date ()
  "Open or create journal note for a specific date selected from calendar."
  (interactive)
  (let ((time (org-read-date nil t nil "Journal note date:")))
    (let ((file (my/denote-get-journal-note-file time)))
      (if file
          (find-file file)
        (my/denote-create-journal-note time)
        (find-file (my/denote-get-journal-note-file time))))))

;; ============================================================================
;; TASK COMPLETION AUTOMATION
;; ============================================================================

(defun my/copy-completed-task-to-journal ()
  "Copy completed task to today's journal note under 'What's done today?' heading.
The original task remains in its original file. A backlink is added to the copy.
This function is called automatically when a task is marked as DONE or CANCEL."
  (when (and (eq major-mode 'org-mode)
             (member (org-get-todo-state) '("DONE" "CANCEL"))
             (buffer-file-name))
    (let* ((journal-dir (expand-file-name "journal" denote-directory))
           (current-file (file-truename (buffer-file-name))))
      ;; Only process if NOT already in a journal note
      (unless (string-prefix-p (file-truename journal-dir) current-file)
        (let* ((journal-file (my/denote-ensure-journal-note-exists))
               (task-heading (org-get-heading t t t t))
               (original-file (buffer-file-name))
               (original-link
                (format "[[file:%s][%s]]"
                        original-file
                        (file-name-nondirectory original-file)))
               (task-body
                (save-excursion
                  (org-back-to-heading t)
                  (let ((start (point)))
                    (org-end-of-subtree t t)
                    (buffer-substring-no-properties start (point))))))
          (save-excursion
            (with-current-buffer (find-file-noselect journal-file)
              (goto-char (point-min))
              (if (re-search-forward "^\\* What's done today\\?" nil t)
                  (progn
                    (org-end-of-subtree t t)
                    (insert "\n" task-body)
                    (forward-line -1)
                    (org-end-of-line)
                    (insert "\n/Original: " original-link "/")
                    (save-buffer))
                (message "Warning: Could not find 'What's done today?' heading"))))
          (message "Task copied to journal note: %s" task-heading))))))

(add-hook 'org-after-todo-state-change-hook 'my/copy-completed-task-to-journal)

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

(defun my/denote-refresh-agenda-list ()
  "Add all denote files with 'project' keyword to `org-agenda-files'."
  (interactive)
  (let ((project-files (my/denote-list-project-files)))
    (setq org-agenda-files
          (delete-dups (append (or org-agenda-files '())
                               project-files)))
    (message "Refreshed agenda list: %d total files (%d projects)"
             (length org-agenda-files)
             (length project-files))))

(defun my/denote-auto-refresh-agenda ()
  "Automatically refresh org-agenda-files when saving in denote directory."
  (when (and (buffer-file-name)
             (string-prefix-p (expand-file-name denote-directory)
                              (file-truename (buffer-file-name))))
    (my/denote-refresh-agenda-list)))

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
   ("C-c n d" . denote-keywords-remove)
   ("C-c n s" . my/denote-refresh-agenda-list)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  ;; Create subdirectories if they don't exist
  (dolist (subdir '("journal" "ref" "trades" "habits"))
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
    "n l" '(denote-backlinks :which-key "backlinks")
    "n s" '(my/denote-refresh-agenda-list :which-key "refresh agenda"))

  ;; Hook for auto-refresh
  (add-hook 'after-save-hook 'my/denote-auto-refresh-agenda))

;; ============================================================================
;; YEAR/MONTH NAVIGATION
;; ============================================================================

(defun my/denote-goto-month ()
  "Navigate to or create a monthly planning note."
  (interactive)
  (let* ((month-str (format-time-string "%Y-%B"))
         (regexp (concat ".*--" (regexp-quote (downcase month-str))))
         (existing (car (denote-directory-files regexp))))
    (if existing
        (find-file existing)
      (denote month-str '("project") 'org nil nil
              "\n* Goals\n\n* Summary\n\n"))))

(defun my/denote-goto-year ()
  "Navigate to or create a yearly planning note."
  (interactive)
  (let* ((year-str (format-time-string "%Y"))
         (regexp (concat ".*--" year-str "$"))
         (existing (car (denote-directory-files regexp))))
    (if existing
        (find-file existing)
      (denote year-str '("project") 'org nil nil
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

(defvar my/denote-after-new-project-hook nil
  "Hook run after creating a new project note.")

(defun my/denote-project-finalize-hook ()
  "Add the new project file to `org-agenda-files'."
  (remove-hook 'my/denote-after-new-project-hook #'my/denote-project-finalize-hook)
  (add-to-list 'org-agenda-files (buffer-file-name)))

(defun my/denote-insert-new-project ()
  "Create a new project note."
  (interactive)
  (add-hook 'my/denote-after-new-project-hook #'my/denote-project-finalize-hook)
  (let ((title (read-string "Project title: ")))
    (denote title '("project") 'org nil nil
            "* Tasks\n\n")
    ;; Run hooks after creation
    (run-hooks 'my/denote-after-new-project-hook)
    (my/denote-refresh-agenda-list)))

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

(advice-add 'my/denote-project-finalize-hook :after #'my/org-refile-update-targets)

(provide 'denote-config)
;;; denote-config.el ends here
