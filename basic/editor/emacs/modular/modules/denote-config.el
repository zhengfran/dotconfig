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
  "Create a new project note in the projects/ subdirectory.
Seeds the richer skeleton used by the `new-org-project' skill: an Overview
heading (Description/Priority/Due drawer, Goal, Scope, Risks) plus a starter
Epic, and the `#+columns:' line that powers the per-project column-view
dashboard (C-c C-x C-c).  Point is left on the Description value to fill in."
  (interactive)
  (let* ((title (read-string "Project title: "))
         (subdir (expand-file-name "projects" denote-directory))
         (template
          (concat
           "#+status:   Active\n"
           "#+category: " title "\n"
           "#+columns:  %40ITEM(Task) %TODO %PRIORITY(Pri) %DEADLINE(Due)\n"
           "#+startup:  content\n\n"
           "* Overview\n"
           ":PROPERTIES:\n"
           ":Description:\n"
           ":Priority:\n"
           ":Due:\n"
           ":END:\n\n"
           "** 🎯 Goal\n\n\n"
           "** 📐 Scope\n"
           "- *In*:\n"
           "- *Out*:\n"
           "- *Success*:\n\n"
           "** ⚠️ Risks & Constraints\n\n\n"
           "* Epic:  [/] :epic:\n"
           "** TODO \n")))
    (unless (file-exists-p subdir)
      (make-directory subdir t))
    (denote title '("project") 'org subdir nil template)
    ;; Land on the Description value so the user can start filling it in.
    (goto-char (point-min))
    (when (re-search-forward "^:Description:" nil t)
      (end-of-line))))

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
  "Capture a Task (a TODO heading) into an existing project note.
Prompts for the project, appends a new TODO under its \"* Tasks\" heading
\(creating that heading if absent), and leaves point ready to type the title.
Issues are not captured by a command: an Issue is created by typing a heading
and setting an ISSUE-sequence keyword (see docs/adr/0001)."
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
            (progn (org-end-of-subtree t t)
                   (unless (bolp) (insert "\n")))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* Tasks\n"))
        (insert "** TODO \n")
        (forward-line -1)
        (end-of-line)))))

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
    "p t" '(my/project-insert-table-block :which-key "insert project table")
    "p d" '(my/project-insert-dashboard :which-key "insert dashboard")
    "p x" '(my/denote-extract-item :which-key "extract item to note")))

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
;; TASK & ISSUE TRACKING
;; ============================================================================
;; Tasks and Issues are ordinary Org headings inside a project note, identified
;; purely by which TODO keyword sequence their keyword belongs to -- never by
;; outline position (see docs/adr/0001).  The four keyword lists below are the
;; single source of truth every piece here consults.
;;   Task lifecycle : TODO -> ONGOING -> DONE / CANCEL
;;   Issue lifecycle: ISSUE -> INVESTIGATING -> RESOLVED / WONTFIX

(defvar my/project-task-keywords '("TODO" "ONGOING" "CANCEL" "DONE")
  "All keywords of the Task lifecycle sequence, in cycle order.")

(defvar my/project-task-open-keywords '("TODO" "ONGOING")
  "Task keywords that count as open (not done or cancelled).")

(defvar my/project-task-done-keywords '("DONE" "CANCEL")
  "Task keywords that close a Task.")

(defvar my/project-issue-keywords '("ISSUE" "INVESTIGATING" "WONTFIX" "RESOLVED")
  "All keywords of the Issue lifecycle sequence, in cycle order.")

(defvar my/project-issue-open-keywords '("ISSUE" "INVESTIGATING")
  "Issue keywords that count as open (not resolved or won't-fixed).")

(defun my/project-item-type (&optional keyword)
  "Return the item type of KEYWORD, or of the heading at point.
Value is `task', `issue', or nil.  With no KEYWORD, read the TODO state of the
heading at point via `org-get-todo-state'."
  (let ((kw (or keyword (org-get-todo-state))))
    (cond
     ((member kw my/project-task-keywords) 'task)
     ((member kw my/project-issue-keywords) 'issue)
     (t nil))))

(defun my/project-item-open-p (keyword)
  "Return non-nil if KEYWORD is an open Task or open Issue state."
  (or (member keyword my/project-task-open-keywords)
      (member keyword my/project-issue-open-keywords)))

(defun my/project-item-next-state (keyword)
  "Return the keyword after KEYWORD within its own sequence, wrapping.
Return nil if KEYWORD belongs to no known sequence.  Unlike `org-todo' with
`right' -- which walks the flat combined keyword list -- this never crosses
from the Task sequence into the Issue sequence."
  (let* ((seq (cond ((member keyword my/project-task-keywords) my/project-task-keywords)
                    ((member keyword my/project-issue-keywords) my/project-issue-keywords)))
         (tail (and seq (cdr (member keyword seq)))))
    (when seq (if tail (car tail) (car seq)))))

;; ----------------------------------------------------------------------------
;; Ancestor helpers
;; ----------------------------------------------------------------------------

(defun my/project--ancestor-task-pos ()
  "Return the buffer position of the nearest ancestor Task heading, or nil.
Point is assumed to be within an entry."
  (save-excursion
    (catch 'found
      (while (org-up-heading-safe)
        (when (eq (my/project-item-type (org-get-todo-state)) 'task)
          (throw 'found (point))))
      nil)))

(defun my/project--ancestor-task-title ()
  "Return the clean title of the nearest ancestor Task heading, or nil."
  (let ((pos (my/project--ancestor-task-pos)))
    (when pos
      (save-excursion (goto-char pos) (org-get-heading t t t t)))))

;; ----------------------------------------------------------------------------
;; Collection -- the query the dashboard tables render
;; ----------------------------------------------------------------------------

(defun my/project--item-at-point ()
  "Return a plist describing the Task/Issue heading at point, or nil.
Keys: :title :state :type :priority :deadline :open :blocks.  :blocks is the
title of the nearest ancestor Task and is set only for Issues."
  (let* ((components (org-heading-components))
         (state (nth 2 components))
         (type (my/project-item-type state)))
    (when type
      (let ((priority (nth 3 components)))
        (list :title (org-get-heading t t t t)
              :state state
              :type type
              :priority (and priority (char-to-string priority))
              :deadline (org-entry-get nil "DEADLINE")
              :open (my/project-item-open-p state)
              :blocks (when (eq type 'issue) (my/project--ancestor-task-title)))))))

(defun my/project-collect-items-in-buffer (&optional type open-only)
  "Return item plists for Task/Issue headings in the current Org buffer.
TYPE, when non-nil (`task' or `issue'), restricts to that type.  When
OPEN-ONLY is non-nil, only open items are returned.  See
`my/project--item-at-point' for the plist shape.  This is the seam the
dashboard dynamic blocks build on and the unit tests exercise directly."
  ;; `org-map-entries' honors `org-agenda-skip-function-global', which this
  ;; config sets to hide done states.  Bind it off so OUR `open-only' argument
  ;; is what decides membership -- otherwise closed items are never seen.
  (org-with-wide-buffer
   (let ((org-agenda-skip-function-global nil)
         items)
     (org-map-entries
      (lambda ()
        (let ((item (my/project--item-at-point)))
          (when (and item
                     (or (null type) (eq (plist-get item :type) type))
                     (or (not open-only) (plist-get item :open)))
            (push item items)))))
     (nreverse items))))

(defun my/project-collect-items (file &optional type open-only)
  "Return item plists from FILE.  See `my/project-collect-items-in-buffer'."
  (with-current-buffer (find-file-noselect file)
    (my/project-collect-items-in-buffer type open-only)))

;; ----------------------------------------------------------------------------
;; Dashboard dynamic blocks: task-table and issue-table
;; ----------------------------------------------------------------------------

(defun my/project--item-link (item)
  "Return an in-file Org link to ITEM's heading, described by its title."
  (let ((title (plist-get item :title)))
    (format "[[*%s][%s]]" title title)))

(defun my/project--write-item-table (type headers row-fn)
  "Insert a table of open items of TYPE from the current buffer.
HEADERS is the list of column header strings.  ROW-FN maps an item plist to a
list of cell strings.  Called from an `org-dblock-write:' writer."
  (let ((items (my/project-collect-items-in-buffer type t)))
    (insert "| " (mapconcat #'identity headers " | ") " |\n")
    (insert "|" (mapconcat (lambda (_) "---") headers "+") "|\n")
    (if (null items)
        (insert "| "
                (mapconcat #'identity
                           (cons (format "(no open %ss)" (symbol-name type))
                                 (make-list (1- (length headers)) ""))
                           " | ")
                " |\n")
      (dolist (item items)
        (insert "| " (mapconcat #'identity (funcall row-fn item) " | ") " |\n")))
    (forward-line -1)
    (org-table-align)))

(defun org-dblock-write:task-table (_params)
  "Dynamic block: a table of the current project's open Tasks.
Columns: Task (link), State, Pri, Deadline."
  (my/project--write-item-table
   'task '("Task" "State" "Pri" "Deadline")
   (lambda (item)
     (list (my/project--item-link item)
           (or (plist-get item :state) "")
           (or (plist-get item :priority) "")
           (or (plist-get item :deadline) "")))))

(defun org-dblock-write:issue-table (_params)
  "Dynamic block: a table of the current project's open Issues.
Columns: Issue (link), State, Blocks (the Task the Issue blocks)."
  (my/project--write-item-table
   'issue '("Issue" "State" "Blocks")
   (lambda (item)
     (list (my/project--item-link item)
           (or (plist-get item :state) "")
           (or (plist-get item :blocks) "")))))

(defun my/project-insert-dashboard ()
  "Insert a \"* Dashboard\" heading with task-table and issue-table blocks."
  (interactive)
  (unless (bolp) (insert "\n"))
  (insert "* Dashboard\n"
          "** Open Tasks\n"
          "#+BEGIN: task-table\n#+END:\n"
          "** Open Issues\n"
          "#+BEGIN: issue-table\n#+END:\n")
  (org-update-all-dblocks))

;; ----------------------------------------------------------------------------
;; C-c C-c on a dashboard row: advance the linked item's state
;; ----------------------------------------------------------------------------

(defun my/project--row-item-title ()
  "Return the item title linked on the current table row, or nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match "\\[\\[\\*\\([^]]+\\)\\]\\[" line)
      (match-string 1 line))))

(defun my/project--goto-heading-by-title (title)
  "Move point to the first heading whose clean title equals TITLE.
Return point on success, nil otherwise.  Searches the whole buffer."
  (goto-char (point-min))
  (let (target)
    (while (and (not target) (re-search-forward org-heading-regexp nil t))
      (save-excursion
        (beginning-of-line)
        (when (equal (org-get-heading t t t t) title)
          (setq target (point)))))
    (when target (goto-char target) target)))

(defun my/project-item-row-cycle ()
  "Advance the state of the Task/Issue linked on the current dashboard row.
Advances within the item's own lifecycle sequence (one step, wrapping), then
refreshes the table and keeps point on the item's row when it is still open."
  (interactive)
  (let ((title (my/project--row-item-title))
        (block-beg (save-excursion
                     (forward-line 0)
                     (when (re-search-backward
                            "^[ \t]*#\\+BEGIN: +\\(?:task-table\\|issue-table\\)\\b"
                            nil t)
                       (point)))))
    (unless title (user-error "No item link on this row"))
    (unless block-beg (user-error "Not inside a task-table or issue-table block"))
    (org-with-wide-buffer
     (when (my/project--goto-heading-by-title title)
       (let ((next (my/project-item-next-state (org-get-todo-state))))
         (when next (org-todo next)))))
    (save-buffer)
    (goto-char block-beg)
    (org-update-dblock)
    (goto-char block-beg)
    (when (re-search-forward (format "\\[\\[\\*%s\\]" (regexp-quote title)) nil t)
      (forward-line 0))))

(defun my/project-item-table-ctrl-c-ctrl-c ()
  "Advance an item's state when C-c C-c is pressed on a dashboard table row.
Registered on `org-ctrl-c-ctrl-c-hook'.  Returns non-nil when it handles the
command so Org skips its default table action; nil otherwise."
  (when (and (org-at-table-p)
             (or (my/project-in-dblock-p "task-table")
                 (my/project-in-dblock-p "issue-table"))
             (my/project--row-item-title))
    (my/project-item-row-cycle)
    t))

(with-eval-after-load 'org
  (add-hook 'org-ctrl-c-ctrl-c-hook #'my/project-item-table-ctrl-c-ctrl-c))

;; ----------------------------------------------------------------------------
;; Blocking: an open child Issue blocks its parent Task from closing
;; ----------------------------------------------------------------------------

(defun my/task-blocked-by-open-issue-p (&optional pom)
  "Return non-nil if the Task at POM owns an open Issue.
An Issue is owned by its nearest ancestor Task, so a child Task never blocks a
parent.  POM defaults to point."
  (org-with-point-at (or pom (point))
    (org-back-to-heading t)
    (let ((org-agenda-skip-function-global nil)
          (task-pos (point))
          (found nil))
      (org-map-entries
       (lambda ()
         (let ((state (org-get-todo-state)))
           (when (and (eq (my/project-item-type state) 'issue)
                      (member state my/project-issue-open-keywords)
                      (equal (my/project--ancestor-task-pos) task-pos))
             (setq found t))))
       nil 'tree)
      found)))

(defun my/project--first-open-issue-heading ()
  "Return the heading of the first open Issue owned by the Task at point."
  (org-back-to-heading t)
  (let ((org-agenda-skip-function-global nil)
        (task-pos (point)) result)
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (and (not result)
                    (eq (my/project-item-type state) 'issue)
                    (member state my/project-issue-open-keywords)
                    (equal (my/project--ancestor-task-pos) task-pos))
           (setq result (org-get-heading t t t t)))))
     nil 'tree)
    result))

(defun my/task-block-on-open-issue (change-plist)
  "Block closing a Task while it owns an open Issue.
For `org-blocker-hook': return t to allow the change, nil to block.  Only
guards transitions into a Task-closing keyword (DONE or CANCEL); the escapes
are to Resolve/Won't-Fix the Issue or refile it out from under the Task."
  (catch 'allow
    (unless (eq (plist-get change-plist :type) 'todo-state-change)
      (throw 'allow t))
    (unless (member (plist-get change-plist :to) my/project-task-done-keywords)
      (throw 'allow t))
    (save-excursion
      (goto-char (plist-get change-plist :position))
      (if (my/task-blocked-by-open-issue-p)
          (progn
            (setq org-block-entry-blocking
                  (or (my/project--first-open-issue-heading) "an open issue"))
            nil)
        t))))

(with-eval-after-load 'org
  (add-hook 'org-blocker-hook #'my/task-block-on-open-issue))

;; ----------------------------------------------------------------------------
;; Extraction: move a long Task/Issue body into its own Detail Note
;; ----------------------------------------------------------------------------

(defun my/denote--item-body-region ()
  "Return (START . END) of the body of the item heading at point.
The body excludes the heading's metadata (planning line, drawers) and any
child headings.  Point must be on the heading."
  (org-back-to-heading t)
  (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
         (start (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion
                (goto-char start)
                (if (re-search-forward org-heading-regexp subtree-end t)
                    (line-beginning-position)
                  subtree-end))))
    (cons start end)))

(defun my/denote--extract-stub-rewrite (detail-id detail-title)
  "Replace the body of the item heading at point with a link to DETAIL-ID.
Keep the heading, its keyword, priority, metadata, and child headings intact.
Return the extracted body string.  Point must be on the heading.  This is the
buffer-rewrite seam, unit-tested independently of Denote file creation."
  (org-back-to-heading t)
  (let* ((region (my/denote--item-body-region))
         (start (car region))
         (end (cdr region))
         (body (string-trim (buffer-substring-no-properties start end))))
    (delete-region start end)
    (goto-char start)
    (unless (bolp) (insert "\n"))
    (insert (format "See detail note: [[denote:%s][%s]]\n" detail-id detail-title))
    body))

(defun my/denote-extract-item ()
  "Extract the body of the Task/Issue at point into its own Detail Note.
Create a Denote note keyworded `task' or `issue' per the item type, move the
heading's body text there, and leave a linked stub -- keeping the heading's
keyword, priority, and child headings -- in the project note.  One-way: there
is no merge-back command."
  (interactive)
  (org-back-to-heading t)
  (let ((type (my/project-item-type (org-get-todo-state))))
    (unless type
      (user-error "Point is not on a Task or Issue heading"))
    (let* ((title (org-get-heading t t t t))
           (region (my/denote--item-body-region))
           (body (string-trim (buffer-substring-no-properties (car region) (cdr region))))
           (project-file (buffer-file-name))
           (project-id (and project-file
                            (denote-retrieve-filename-identifier project-file)))
           (project-title (and project-file (my/project-get-title project-file)))
           (project-buffer (current-buffer))
           (heading-pos (point)))
      (unless project-id
        (user-error "This buffer is not a denote note"))
      (when (string-empty-p body)
        (user-error "This %s has no body to extract" (symbol-name type)))
      (let* ((content (format "Extracted from [[denote:%s][%s]].\n\n%s\n"
                              project-id project-title body))
             (detail-file (denote title (list (symbol-name type)) 'org nil nil content))
             (detail-id (denote-retrieve-filename-identifier detail-file)))
        ;; `denote' leaves the new note in an unsaved buffer; write it so the
        ;; stub's link points at a file that actually exists.
        (when-let* ((buf (find-buffer-visiting detail-file)))
          (with-current-buffer buf (save-buffer)))
        (with-current-buffer project-buffer
          (save-excursion
            (goto-char heading-pos)
            (my/denote--extract-stub-rewrite detail-id title))
          (save-buffer))
        (message "Extracted %s -> %s" (symbol-name type)
                 (file-name-nondirectory detail-file))))))

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
