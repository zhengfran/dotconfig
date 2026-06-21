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
;; JOURNAL (denote-journal): daily / weekly / monthly / yearly cadences
;; ============================================================================
;; Each cadence is the SAME `denote-journal-new-or-existing-entry' command with
;; the interval, keyword, and title format dynamically bound. Entries all live
;; in the journal/ subdirectory but are distinguished by keyword:
;;   daily -> journal, weekly -> weekly, monthly -> monthly, yearly -> yearly.
;; The denote ID timestamp encodes the date, which the `tasks-done-today'
;; dynamic block (below) parses for daily entries.

(use-package denote-journal
  :after denote
  :commands (denote-journal-new-entry
             denote-journal-new-or-existing-entry
             denote-journal-link-or-create-entry
             denote-journal-calendar-new-or-existing
             denote-journal-calendar-find-file)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :custom
  (denote-journal-directory (expand-file-name "journal" my/org-base-dir))
  (denote-journal-keyword "journal")
  (denote-journal-title-format 'day-date-month-year))

;; These dynamic variables are defined by `denote-journal' (via defcustom).
;; Declare them here so this file byte-compiles cleanly and the `let' bindings
;; in `my/denote-journal-with' are dynamic (special), not lexical.
(defvar denote-journal-interval)
(defvar denote-journal-keyword)
(defvar denote-journal-title-format)

(defmacro my/denote-journal-with (interval keyword title-format &rest body)
  "Run BODY with denote-journal bound to INTERVAL, KEYWORD and TITLE-FORMAT."
  (declare (indent 3))
  `(let ((denote-journal-interval ,interval)
         (denote-journal-keyword ,keyword)
         (denote-journal-title-format ,title-format))
     ,@body))

(defun my/denote-journal-daily (&optional date)
  "Visit or create today's daily journal entry.
With prefix arg, prompt for a DATE."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (my/denote-journal-with 'daily "journal" "%Y-%m-%d %a"
    (denote-journal-new-or-existing-entry date)))

(defun my/denote-journal-weekly (&optional date)
  "Visit or create this week's journal entry.
With prefix arg, prompt for a DATE within the desired week."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (my/denote-journal-with 'weekly "weekly" "Week %V %Y"
    (denote-journal-new-or-existing-entry date)))

(defun my/denote-journal-monthly (&optional date)
  "Visit or create this month's journal entry.
With prefix arg, prompt for a DATE within the desired month."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (my/denote-journal-with 'monthly "monthly" "%Y-%B"
    (denote-journal-new-or-existing-entry date)))

(defun my/denote-journal-yearly (&optional date)
  "Visit or create this year's journal entry.
With prefix arg, prompt for a DATE within the desired year."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (my/denote-journal-with 'yearly "yearly" "%Y"
    (denote-journal-new-or-existing-entry date)))

(defun my/denote-journal-yesterday ()
  "Visit or create yesterday's daily journal entry."
  (interactive)
  (my/denote-journal-daily
   (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))))

(defun my/denote-journal-tomorrow ()
  "Visit or create tomorrow's daily journal entry."
  (interactive)
  (my/denote-journal-daily
   (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 1)))))

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
  (denote-known-keywords '("project" "journal" "weekly" "monthly" "yearly" "trade" "work" "review" "ref" "archived" "habit" "blog"))
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
   ;; Journal / date navigation (denote-journal cadences)
   ("C-c n j" . my/denote-journal-daily)
   ("C-c n d" . my/denote-journal-daily)
   ("C-c n y" . my/denote-journal-yesterday)
   ("C-c n m" . my/denote-journal-tomorrow)
   ("C-c n w" . my/denote-journal-weekly)
   ("C-c n T" . my/denote-capture-trade)
   ("C-c n Y" . my/denote-journal-yearly)
   ("C-c n M" . my/denote-journal-monthly)
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
  (dolist (subdir '("journal" "ref" "trades" "habits" "projects" "blog"))
    (let ((dir (expand-file-name subdir denote-directory)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; ============================================================================
;; CONSULT-DENOTE
;; ============================================================================

(use-package consult-denote
  :after denote
  :config
  (consult-denote-mode 1)
  :bind
  ("C-c n e" . consult-denote-find)
  ("C-c n b" . denote-find-backlink)
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
            "#+status: Active\n\n* Tasks\n\n")))

(defun my/denote-find-project ()
  "Find and open a project note."
  (interactive)
  (let* ((project-files (my/denote-list-project-files))
         (candidates (mapcar (lambda (f)
                              (cons (denote-retrieve-title-value f 'org) f))
                            project-files)))
    (if (null candidates)
        (message "No project files found. Create one with C-c n P")
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
        (message "No project files found. Create one with C-c n P")
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
;; PROJECT TABLE (dynamic block, like SPC h t habit-tracker)
;; ============================================================================

(defun my/project-get-title (file)
  "Return #+title of FILE, or its base name as fallback."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
        (string-trim (match-string 1))
      (file-name-base file))))

(defun my/project-get-status (file)
  "Return the #+status value of FILE (read from its head), or empty string."
  (with-temp-buffer
    (insert-file-contents file nil 0 1024)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+status:\\s-*\\(.+\\)" nil t)
        (string-trim (match-string 1))
      "")))

(defvar my/project-status-order '("Active" "On-Hold" "Archived")
  "Display order for #+status values in the project table.
Projects are grouped/sorted by this order; unknown values sort last.")

(defun my/project-status-rank (status)
  "Return the sort rank of STATUS per `my/project-status-order'."
  (or (seq-position my/project-status-order status
                    (lambda (a b) (string-equal-ignore-case a b)))
      (length my/project-status-order)))

(defun my/project-files-in-folder ()
  "Return all real .org files under the projects subfolder of `denote-directory'.
Excludes Emacs lock files (.#...), other dot-files, and dangling symlinks so
the dynamic block does not error on a project that has unsaved changes."
  (let ((dir (expand-file-name "projects" denote-directory)))
    (when (file-directory-p dir)
      (seq-filter (lambda (f)
                    (and (file-regular-p f)
                         (not (string-prefix-p "." (file-name-nondirectory f)))))
                  (directory-files-recursively dir "\\.org\\'")))))

(defun org-dblock-write:project-table (params)
  "Write a table of projects with their #+status into the dynamic block.
Reads only the #+status keyword of each .org file in the projects folder
\(values: Active / On-Hold / Archived).
PARAMS can include:
  :status - only list projects whose #+status matches (case-insensitive)."
  (let* ((status-filter (plist-get params :status))
         (files (my/project-files-in-folder))
         (rows '()))
    (dolist (file files)
      (let ((status (my/project-get-status file)))
        ;; Skip projects that have no #+status metadata at all.
        (when (and (not (string= status ""))
                   (or (not status-filter)
                       (string-equal-ignore-case status status-filter)))
          (let* ((title (my/project-get-title file))
                 (identifier (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                                                 (file-name-nondirectory file))
                               (match-string 1 (file-name-nondirectory file)))))
            (push (list title (or identifier file) status)
                  rows)))))
    ;; Sort by status order, then title.
    (setq rows (sort rows (lambda (a b)
                            (let ((ra (my/project-status-rank (nth 2 a)))
                                  (rb (my/project-status-rank (nth 2 b))))
                              (if (= ra rb)
                                  (string< (nth 0 a) (nth 0 b))
                                (< ra rb))))))
    (insert "| Project | Status |\n")
    (insert "|---------|--------|\n")
    (if (null rows)
        (insert "| (no projects) | |\n")
      (dolist (r rows)
        (insert (format "| [[denote:%s][%s]] | %s |\n"
                        (nth 1 r) (nth 0 r) (nth 2 r)))))
    (forward-line -1)
    (org-table-align)))

(defun my/project-insert-table-block (&optional active-only)
  "Insert a project-table dynamic block and populate it.
With prefix arg ACTIVE-ONLY, list only projects with #+status: Active."
  (interactive "P")
  (insert (if active-only
              "#+BEGIN: project-table :status \"Active\"\n"
            "#+BEGIN: project-table\n"))
  (insert "#+END:\n")
  (forward-line -1)
  (org-ctrl-c-ctrl-c))

;; ----------------------------------------------------------------------------
;; Kanban-style block: one column per status, projects listed beneath.
;; ----------------------------------------------------------------------------

(defun org-dblock-write:project-kanban (params)
  "Write a kanban board of projects: one column per status.
Each project (with a #+status) appears as a denote link under its status
column; columns are padded to equal height.
PARAMS can include:
  :statuses - list of status column headers (default `my/project-status-order')."
  (let* ((statuses (or (plist-get params :statuses) my/project-status-order))
         (files (my/project-files-in-folder))
         ;; one cons cell per column: (STATUS . list-of (title . cell))
         (cols (mapcar (lambda (s) (cons s nil)) statuses)))
    (dolist (file files)
      (let ((status (my/project-get-status file)))
        (unless (string= status "")
          (let ((col (seq-find (lambda (c) (string-equal-ignore-case (car c) status))
                               cols)))
            (when col
              (let* ((title (my/project-get-title file))
                     (id (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                                             (file-name-nondirectory file))
                           (match-string 1 (file-name-nondirectory file))))
                     (cell (format "[[denote:%s][%s]]" (or id file) title)))
                (setcdr col (cons (cons title cell) (cdr col)))))))))
    ;; Sort each column by project title.
    (dolist (c cols)
      (setcdr c (mapcar #'cdr
                        (sort (cdr c) (lambda (a b) (string< (car a) (car b)))))))
    (let ((height (apply #'max 0 (mapcar (lambda (c) (length (cdr c))) cols))))
      (insert "| " (mapconcat #'car cols " | ") " |\n")
      (insert "|" (mapconcat (lambda (_) "---") cols "+") "|\n")
      (dotimes (i height)
        (insert "| "
                (mapconcat (lambda (c) (or (nth i (cdr c)) "")) cols " | ")
                " |\n"))
      (forward-line -1)
      (org-table-align))))

(defun my/project-insert-kanban-block ()
  "Insert a project-kanban dynamic block and populate it."
  (interactive)
  (insert "#+BEGIN: project-kanban\n#+END:\n")
  (forward-line -1)
  (org-ctrl-c-ctrl-c))

(defun my/project-status-next (status)
  "Return the status after STATUS in `my/project-status-order', wrapping.
If STATUS is unknown or empty, return the first element."
  (let ((pos (seq-position my/project-status-order status
                           (lambda (a b) (string-equal-ignore-case a b)))))
    (if pos
        (nth (% (1+ pos) (length my/project-status-order)) my/project-status-order)
      (car my/project-status-order))))

(defun my/project-set-status (file new-status)
  "Set the #+status keyword of FILE to NEW-STATUS and save the file."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+status:.*$" nil t)
          (replace-match (format "#+status: %s" new-status) t t)
        ;; No #+status line yet: insert after the front-matter keyword block.
        (goto-char (point-min))
        (when (looking-at-p "^[ \t]*:PROPERTIES:")
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (forward-line 1)))
        (let ((insert-point (point)))
          (while (looking-at-p "^#\\+[A-Za-z]")
            (forward-line 1)
            (setq insert-point (point)))
          (goto-char insert-point))
        (insert (format "#+status: %s\n" new-status))))
    (save-buffer)))

(defun my/project-table-row-id ()
  "Return the denote identifier referenced on the current table row, or nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match "denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" line)
      (match-string 1 line))))

(defun my/project-cell-id ()
  "Return the denote identifier in the current table cell, or nil."
  (let ((field (and (org-at-table-p) (org-table-get-field))))
    (when (and field
               (string-match "denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" field))
      (match-string 1 field))))

(defun my/project--file-for-id (id)
  "Return the project file path for denote ID, or nil."
  (and id (or (denote-get-path-by-id id)
              (seq-find (lambda (f)
                          (string-match-p id (file-name-nondirectory f)))
                        (my/project-files-in-folder)))))

(defun my/project--cycle-and-refresh (file id block-name)
  "Cycle FILE's #+status, refresh the BLOCK-NAME dblock, land back on ID."
  (let* ((current (my/project-get-status file))
         (next (my/project-status-next current))
         (blk-start (copy-marker
                     (save-excursion
                       (forward-line 0)
                       (if (re-search-backward
                            (format "^[ \t]*#\\+BEGIN: +%s\\b" (regexp-quote block-name))
                            nil t)
                           (point)
                         (point-min))))))
    (my/project-set-status file next)
    ;; `org-update-dblock' must run with point on the #+BEGIN line.
    (goto-char blk-start)
    (org-update-dblock)
    (when id
      (goto-char blk-start)
      (when (re-search-forward (regexp-quote id) nil t)
        (forward-line 0)))
    (message "%s: %s → %s"
             (my/project-get-title file)
             (if (string-empty-p current) "—" current)
             next)))

(defun my/project-table-cycle-status ()
  "Cycle the #+status of the project on the current project-table row.
Order is `my/project-status-order' (Active -> On-Hold -> Archived -> ...).
Writes the new status back to the original project file and refreshes the
dynamic block, keeping point on the same project."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Point is not on a project-table row"))
  (let ((file (my/project--file-for-id (my/project-table-row-id))))
    (unless (and file (file-exists-p file))
      (user-error "No project file found for this row"))
    (my/project--cycle-and-refresh file (my/project-table-row-id) "project-table")))

(defun my/project-kanban-cycle-status ()
  "Cycle the #+status of the project in the current project-kanban cell.
After the block refreshes, the project moves to the column of its new status."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Point is not on a kanban cell"))
  (let* ((id (my/project-cell-id))
         (file (my/project--file-for-id id)))
    (unless (and file (file-exists-p file))
      (user-error "No project in this cell"))
    (my/project--cycle-and-refresh file id "project-kanban")))

(with-eval-after-load 'general
  (zzc/leader-keys
    "p t" '(my/project-insert-table-block :which-key "insert project table")))

(defun my/project-in-dblock-p (name)
  "Return non-nil if point is inside a dynamic block named NAME."
  (let ((pos (point))
        (case-fold-search t))
    (save-excursion
      (when (re-search-backward
             (format "^[ \t]*#\\+BEGIN: +%s\\b" (regexp-quote name)) nil t)
        (let ((beg (point))
              (end (and (re-search-forward "^[ \t]*#\\+END:" nil t) (point))))
          (and end (<= beg pos) (<= pos end)))))))

(defun my/project-table-on-status-column-p ()
  "Return non-nil when point is in the column headed \"Status\"."
  (let ((col (org-table-current-column)))
    (save-excursion
      (goto-char (org-table-begin))
      (string-equal-ignore-case
       (string-trim (or (org-table-get-field col) ""))
       "Status"))))

(defun my/project-table-ctrl-c-ctrl-c ()
  "Cycle project status when C-c C-c is pressed on a project cell.
Registered on `org-ctrl-c-ctrl-c-hook'.  Handles both the `project-table'
\(on the Status column) and the `project-kanban' (any cell holding a project
link).  Returns non-nil when it handles the command so org skips its default
table action; returns nil otherwise to leave C-c C-c behaving normally."
  (cond
   ;; project-table: a data row, on the Status column.
   ((and (org-at-table-p)
         (my/project-table-row-id)
         (my/project-in-dblock-p "project-table")
         (my/project-table-on-status-column-p))
    (my/project-table-cycle-status)
    t)
   ;; project-kanban: any cell that holds a project link.
   ((and (org-at-table-p)
         (my/project-in-dblock-p "project-kanban")
         (my/project-cell-id))
    (my/project-kanban-cycle-status)
    t)))

;; Hook the conventional C-c C-c: on a project-table status cell it cycles the
;; project status; everywhere else org's default behaviour is untouched.
(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook #'my/project-table-ctrl-c-ctrl-c))

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
