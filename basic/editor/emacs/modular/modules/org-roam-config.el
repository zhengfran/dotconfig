;;; org-roam.el --- Org-roam configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-roam knowledge management system: configuration, dailies,
;; project management, org-roam-ui, consult-org-roam, org-noter, org-anki
;;
;; DEPENDENCIES: core (my/org-base-dir), org-base, org-agenda-config (org-agenda-files),
;;               completion (consult-org-roam), keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; Trade related capture
(defvar my/trade-template-dir (expand-file-name "~/org/templates/trades/")
  "Base directory for all trade strategy template notes.")
(defun my/trade-list-templates ()
  "Return list of trade template base names without extension."
  (mapcar #'file-name-base
          (directory-files my/trade-template-dir nil "\\.org$")))

;;Read selected template file
(defun my/trade-read-template (name)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat name ".org") my/trade-template-dir))
    (buffer-string)))

;;trade capture function
(defun my/org-roam-capture-trade ()
  (interactive)
  (let* ((strategy (completing-read
                    "Strategy: "
                    (my/trade-list-templates)
                    nil t))
         (body (my/trade-read-template strategy))
         (header (concat
                  "#+title: %<%Y-%m-%d-%H-%M-%S> Trade\n"
                  "#+filetags: :trade:" strategy ":\n\n")))
    (org-roam-capture-
     :node (org-roam-node-create)
     :templates
     `(("t" "trade log" plain
        ,body
        :if-new
        (file+head
         "trades/%<%Y%m%d%H%M%S>.org"
         ,header)
        :unnarrowed t)))))


;; ============================================================================
;; DAILY NOTE HELPERS
;; ============================================================================

(defun my/org-roam-get-daily-note-file ()
  "Get the file path for today's daily note."
  (let* ((time (current-time))
         (dailies-dir
          (expand-file-name org-roam-dailies-directory
                            org-roam-directory))
         (filename (format-time-string "%Y-%m-%d.org" time)))
    (expand-file-name filename dailies-dir)))

(defun my/org-roam-ensure-daily-note-exists ()
  "Ensure today's daily note exists, creating it if necessary."
  (let ((daily-file (my/org-roam-get-daily-note-file)))
    (unless (file-exists-p daily-file)
      (save-window-excursion (org-roam-dailies-goto-today)))
    daily-file))

;; ============================================================================
;; TASK COMPLETION AUTOMATION
;; ============================================================================

(defun my/copy-completed-task-to-daily ()
  "Copy completed task to today's daily note under 'What's done today?' heading.
The original task remains in its original file. A backlink is added to the copy.
This function is called automatically when a task is marked as DONE or CANCEL."
  (when (and (eq major-mode 'org-mode)
             (member (org-get-todo-state) '("DONE" "CANCEL"))
             (buffer-file-name))
    (let* ((dailies-dir
            (expand-file-name org-roam-dailies-directory
                              org-roam-directory))
           (current-file (file-truename (buffer-file-name))))
      ;; Only process if NOT already in a daily note
      (unless (string-prefix-p
               (file-truename dailies-dir) current-file)
        (let* ((daily-file (my/org-roam-ensure-daily-note-exists))
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
            (with-current-buffer (find-file-noselect daily-file)
              (goto-char (point-min))
              ;; Find "What's done today?" heading
              (if (re-search-forward "^\\* What's done today\\?"
                                     nil
                                     t)
                  (progn
                    ;; Go to end of this section (before next top-level heading)
                    (org-end-of-subtree t t)
                    ;; Insert task directly under heading
                    (insert "\n" task-body)
                    ;; Add backlink as visible line after the heading
                    (forward-line -1)
                    (org-end-of-line)
                    (insert "\n/Original: " original-link "/")
                    (save-buffer))
                (message
                 "Warning: Could not find 'What's done today?' heading in daily note"))))
          (message "Task copied to daily note (original kept): %s"
                   task-heading))))))

;; Hook to auto-copy when TODO state changes to DONE/CANCEL
(add-hook
 'org-after-todo-state-change-hook 'my/copy-completed-task-to-daily)

;; ============================================================================
;; DAILY NOTE TEMPLATE
;; ============================================================================

(defun my/org-roam-daily-note-header ()
  "Generate daily note header with links and template content."
  (concat "#+title: " (format-time-string "%Y-%m-%d %a") "\n\n"
          "[[roam:" (format-time-string "%Y-w%W") "]]\n\n"
          "[[roam:" (format-time-string "%Y-%B") "]]\n\n"
          (with-temp-buffer
            (insert-file-contents "~/org/templates/daily.org")
            (buffer-string))))

(setq my/daily-note-filename "%<%Y-%m-%d>.org")

;; ============================================================================
;; FILTER FUNCTIONS
;; ============================================================================
;; These functions require org-roam to be loaded

(with-eval-after-load 'org-roam
  (defun my/org-roam-filter-by-tag (tag-name)
    "Return a predicate function that filters org-roam nodes by TAG-NAME (case-insensitive).
The returned lambda checks if TAG-NAME is present in a node's tags list."
    (let ((target-tag tag-name))
      (lambda (node)
        (let ((node-tags (org-roam-node-tags node)))
          (catch 'found
            (dolist (node-tag node-tags)
              (when (string-equal-ignore-case target-tag node-tag)
                (throw 'found t))))))))

  (defun my/org-roam-list-notes-by-tag (tag-name)
    "Return a list of file paths for all org-roam nodes tagged with TAG-NAME (case-insensitive)."
    (mapcar #'org-roam-node-file
            (seq-filter (my/org-roam-filter-by-tag tag-name)
                        (org-roam-node-list))))

  (defun my/org-roam-filter-by-tags (wanted unwanted)
    "Return a predicate that filters nodes having any tag in WANTED but none in UNWANTED (case-insensitive).
WANTED and UNWANTED should be lists of tag strings."
    (let ((wanted-tags wanted)
          (unwanted-tags unwanted))
      (lambda (node)
        (let ((node-tags (org-roam-node-tags node))
              (has-wanted nil)
              (has-unwanted nil))
          ;; Check for wanted tags
          (catch 'found-wanted
            (dolist (tag wanted-tags)
              (dolist (node-tag node-tags)
                (when (string-equal-ignore-case tag node-tag)
                  (setq has-wanted t)
                  (throw 'found-wanted)))))
          ;; Check for unwanted tags
          (catch 'found-unwanted
            (dolist (tag unwanted-tags)
              (dolist (node-tag node-tags)
                (when (string-equal-ignore-case tag node-tag)
                  (setq has-unwanted t)
                  (throw 'found-unwanted)))))
          ;; Return true only if has wanted and no unwanted
          (and has-wanted (not has-unwanted))))))

;; ============================================================================
;; AGENDA INTEGRATION
;; ============================================================================

(with-eval-after-load 'org-roam
  (defun my/org-roam-refresh-agenda-list ()
    "Add all org-roam files tagged with 'Project' to `org-agenda-files' (case-insensitive).
This allows project files to appear in the org-agenda view."
    (interactive)
    (when (featurep 'org-roam)
      (let ((project-files (my/org-roam-list-notes-by-tag "Project")))
        (setq org-agenda-files
              (delete-dups (append (or org-agenda-files '())
                                   project-files)))
        (message "Refreshed agenda list: %d total files (%d projects)" 
                 (length org-agenda-files)
                 (length project-files)))))))

(defun my/org-roam-db-diagnose ()
  "Clear and rebuild org-roam database to fix issues.
Prompts for confirmation before clearing the database."
  (interactive)
  (when (yes-or-no-p "Clear and rebuild org-roam database? This may take a few minutes. Continue? ")
    (message "Clearing org-roam database...")
    (org-roam-db-clear-all)
    (message "Rebuilding database from %s..." org-roam-directory)
    (org-roam-db-sync)
    (message "Org-roam database rebuilt successfully! Found %d nodes." 
             (caar (org-roam-db-query "SELECT COUNT(*) FROM nodes")))))

(defun my/org-roam-dailies-capture-date ()
  "Capture to a daily note on a specific date selected from calendar.
Prompts for date using org-mode's date picker."
  (interactive)
  (let ((time (org-read-date nil t nil "Daily note date:")))
    (org-roam-dailies-capture-date time)))

;; Automatic refresh hooks
(with-eval-after-load 'org-roam
  (defun my/org-roam-auto-refresh-agenda ()
    "Automatically refresh org-agenda-files when org-roam database updates."
    (when (and (featurep 'org-roam)
               (buffer-file-name)
               (string-prefix-p (expand-file-name org-roam-directory)
                                (file-truename (buffer-file-name))))
      (my/org-roam-refresh-agenda-list))))

;; ============================================================================
;; ORG-ROAM CONFIGURATION
;; ============================================================================
(setq org-capture-delete-aborted-notes t)
;; Ensure emacsql is available (includes sqlite-builtin support)
(use-package emacsql
  :config
  ;; Explicitly load emacsql-sqlite to ensure the interface is available
  ;; This is needed because org-roam requires it internally but the autoloads
  ;; might not be ready on first startup with fresh packages
  (require 'emacsql-sqlite))

(use-package org-roam
    :after emacsql
    :commands (org-roam-node-find
               org-roam-node-insert
               org-roam-capture
               org-roam-dailies-capture-today
               org-roam-dailies-goto-today
               org-roam-dailies-goto-yesterday
               org-roam-dailies-goto-tomorrow
               org-roam-dailies-goto-date
               org-roam-buffer-toggle
               org-roam-db-sync
               org-roam-ui-open)
    :custom
    (org-roam-directory my/org-base-dir)
    (org-roam-completion-everywhere t)
    (org-roam-completion-system 'default)
    (org-roam-node-display-template
     (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-gc-threshold most-positive-fixnum)
    (org-roam-db-update-on-save t)
    (org-roam-db-update-idle-seconds 2.0)
    (org-roam-dailies-directory "daily/")
    (org-roam-dailies-capture-templates
    `(("d" "default" entry "* %?"
       :if-new (file+head ,my/daily-note-filename
                          ,(my/org-roam-daily-note-header)))
      ))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n c" . org-roam-capture)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n I" . org-roam-node-insert-immediate)
           ("C-c n t" . my/org-roam-capture-task)
           ("C-c n P" . my/org-roam-insert-new-project)
           ("C-c n p" . my/org-roam-find-project)
           ("C-c n u" . org-roam-ui-mode)
           ("C-c n g" . org-roam-graph)
           ("C-c n a" . org-roam-alias-add)
           ("C-c n T" . org-roam-tag-add)
           ("C-c n D" . org-roam-tag-remove)
           ("C-c n m" . org-roam-buffer-display-dedicated)
           ("C-c n s" . org-roam-db-sync)
           ("C-c n S" . my/org-roam-db-diagnose)
           ("C-c n Y" . my/org-roam-goto-year)
           ("C-c n M" . my/org-roam-goto-month)
           ("C-c n C" . my/org-roam-dailies-capture-date)
           :map org-mode-map
           ("C-M-i". completion-at-point))
      :config
      ;; Ensure org-roam-directory is set before db-autosync-mode
      (setq org-roam-directory my/org-base-dir)
      (setq org-roam-capture-templates
           '(("d" "default" plain "%?"
              :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                 "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
              :unnarrowed t)

             ("p" "project" plain "** TODO %?\n"
              :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                                 "#+title: ${title}\n#+date: %U\n#+category: ${title}\n#+filetags: :Project:\n\n* Tasks\n")
              :unnarrowed t)))
     (add-hook 'org-roam-mode-hook 'visual-line-mode)
    (org-roam-db-autosync-mode)

     ;; Leader key bindings
     (zzc/leader-keys
       "n" '(:ignore t :which-key "notes")
       "n f" '(org-roam-node-find :which-key "find node")
       "n i" '(org-roam-node-insert :which-key "insert node")
       "n c" '(org-roam-capture :which-key "capture")
       "n j" '(org-roam-dailies-capture-today :which-key "daily today")
       "n d" '(org-roam-dailies-goto-today :which-key "goto today")
       "n y" '(org-roam-dailies-goto-yesterday :which-key "goto yesterday")
       "n m" '(org-roam-dailies-goto-tomorrow :which-key "goto tomorrow")
       "n D" '(org-roam-dailies-goto-date :which-key "goto date")
       "n p" '(my/org-roam-find-project :which-key "find project")
       "n P" '(my/org-roam-insert-new-project :which-key "new project")
       "n r" '(my/org-roam-capture-trade :which-key "capture trade")
       "n t" '(my/org-roam-capture-task :which-key "capture task")
       "n Y" '(my/org-roam-goto-year :which-key "goto year")
       "n M" '(my/org-roam-goto-month :which-key "goto month")
       "n u" '(org-roam-ui-open :which-key "open ui")
       "n s" '(org-roam-db-sync :which-key "sync db"))

    ;; Hook into org-roam database sync
    (add-hook 'org-roam-db-autosync-mode-hook 'my/org-roam-refresh-agenda-list)

    ;; Hook into after org-roam capture finalize
    (add-hook 'org-roam-capture-new-node-hook 'my/org-roam-refresh-agenda-list)

    ;; Display buffer configuration
    (add-to-list 'display-buffer-alist '("\\(^CAPTURE.*\\.org$\\|\\*Org.*Select\\*$\\)"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 0)
                                       (window-width . 60))

    ;; Hook into after saving org-roam files
    (add-hook 'after-save-hook 'my/org-roam-auto-refresh-agenda)))

 ;; ============================================================================
 ;; YEAR/MONTH NAVIGATION
;; ============================================================================

(defun my/org-roam-goto-month ()
  "Navigate to or create a monthly planning note for the current month.
Creates a note in the root directory with format YYYY-MMMM.org."
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: :project:\n")
                                   :unnarrowed t))))

(defun my/org-roam-goto-year ()
  "Navigate to or create a yearly planning note for the current year.
Creates a note in the root directory with format YYYY.org."
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* goals\n\n%?* summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: :project:\n")
                                   :unnarrowed t))))

;; ============================================================================
;; ORG-ROAM-UI
;; ============================================================================

(defun my/set-orui-latex-macros ()
  "Configure LaTeX macros for org-roam-ui mathematical notation rendering.
Sets up commonly used mathematical symbols and operators."
  (setq org-roam-ui-latex-macros
        '(("\\C" . "\\mathbb{C}")
          ("\\fc" . "\\mathcal{f}")
          ("\\nc" . "\\mathcal{n}")
          ("\\ps" . "\\mathsf{p}")
          ("\\ppbf" . "\\mathbf{p}")
          ("\\ppbb" . "\\mathbb{p}")
          ("\\esf" . "\\mathsf{e}")
          ("\\eebf" . "\\mathbf{e}")
          ("\\eebb" . "\\mathbb{e}")
          ("\\one" . "\\mathbf{1}")
          ("\\R" . "\\mathbb{R}")
          ("\\Z" . "\\mathbb{Z}")
          ("\\Q" . "\\mathbb{Q}")
          ("\\N" . "\\mathbb{N}")
          ("\\eps" . "\\varepsilon")
          ("\\det" . "\\mathop{det}"))))

 (use-package org-roam-ui
   :defer t
   :after org-roam
   :custom
   (org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t)
   (org-roam-ui-open-on-start nil)
   :config
   (my/set-orui-latex-macros))

;; ============================================================================
;; CONSULT-ORG-ROAM
;; ============================================================================

(use-package consult-org-roam
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n L" . consult-org-roam-forward-links)
    ("C-c n r" . consult-org-roam-search))

 ;; ============================================================================
 ;; ORG-NOTER
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
;; ORG-ANKI
;; ============================================================================

(use-package org-anki
  :after org
  :config
  (setq org-anki-default-deck "Mega"))

;; ============================================================================
;; PROJECT MANAGEMENT
;; ============================================================================

(with-eval-after-load 'org-roam
  (defun my/org-roam-project-finalize-hook ()
    "adds the captured project file to `org-agenda-files' if the
    capture was not aborted."
    ;; remove the hook since it was added temporarily
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
    (org-roam-capture- :keys "p"
                       :node (org-roam-node-read
                              nil
                              (my/org-roam-filter-by-tag "Project"))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
    ;; select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tags '("Project") '("Archived"))))

  (defun my/org-roam-capture-task ()
    (interactive)
    ;; update org-agenda list after adding projects
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
    ;; new todo
    (org-roam-capture- :keys "p"
                       :node (org-roam-node-read
                              nil
                              (my/org-roam-filter-by-tag "Project")))))

;; ============================================================================
;; REFILE CONFIGURATION
;; ============================================================================

(defun my/org-refile-update-targets ()
  "Update `org-refile-targets` to match `org-agenda-files`."
  (setq org-refile-targets
        (mapcar (lambda (file) (cons file '(:maxlevel . 3))) org-agenda-files)))

;; Run once on startup
(my/org-refile-update-targets)

;; Update targets when project finalize hook runs (only when projects change)
(advice-add 'my/org-roam-project-finalize-hook :after #'my/org-refile-update-targets)

;; ============================================================================
;; MIGRATION HELPER
;; ============================================================================

(defun my/org-roam-migrate-project-files ()
  "Move existing project files from root to projects/ subdirectory.
Migrates all files tagged with :Project: or :project: or :projects:."
  (interactive)
  (let* ((projects-dir (expand-file-name "projects/" org-roam-directory))
         (moved-count 0)
         (skipped-count 0)
         (root-dir (expand-file-name org-roam-directory)))
    
    (unless (file-exists-p projects-dir)
      (make-directory projects-dir t))
    
    ;; Find all project files in root directory
    (dolist (file (directory-files root-dir t "\\.org$"))
      (when (and (file-regular-p file)
                 (not (string-prefix-p "." (file-name-nondirectory file))))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Check for Project tag (case-insensitive)
          (when (re-search-forward "^#\\+filetags:.*:[Pp]rojects?:" nil t)
            (let* ((filename (file-name-nondirectory file))
                   (new-path (expand-file-name filename projects-dir)))
              (if (file-exists-p new-path)
                  (progn
                    (message "Skipped (already exists): %s" filename)
                    (setq skipped-count (1+ skipped-count)))
                (rename-file file new-path)
                (setq moved-count (1+ moved-count))
                (message "Moved: %s -> projects/%s" filename filename)))))))
    
    (message "Migration complete. Moved: %d, Skipped: %d" moved-count skipped-count)
    (when (> moved-count 0)
      (org-roam-db-sync)
      (my/org-roam-refresh-agenda-list))))

(provide 'org-roam-config)
;;; org-roam.el ends here
