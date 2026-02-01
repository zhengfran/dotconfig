;;; habit-tracker.el --- Progressive habit tracking system -*- lexical-binding: t; -*-

;;; Commentary:
;; Progressive habit tracking with denote integration.
;; Habits grow over time with configurable progression.
;; Dual streak tracking: participation + target-met.
;; Heat map calendar visualization.
;;
;; DEPENDENCIES: core (my/org-base-dir), denote-config, keybindings (zzc/leader-keys),
;;               completion (consult)
;; USED BY: None

;;; Code:

;; External function declarations (silence byte-compiler)
(declare-function org-back-to-heading "org")
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-end-of-subtree "org")
(declare-function org-table-align "org-table")
(declare-function org-ctrl-c-ctrl-c "org")
(declare-function denote "denote")
(declare-function denote-retrieve-title-value "denote")
(declare-function calendar-last-day-of-month "calendar")
(declare-function calendar-month-name "calendar")
(declare-function calendar-day-of-week "calendar")

;; External variable declaration
(defvar my/org-base-dir)

;; ============================================================================
;; CUSTOM VARIABLES
;; ============================================================================

(defvar my/habit-directory (expand-file-name "habits" my/org-base-dir)
  "Directory for habit notes.")

(defvar my/habit-template-dir (expand-file-name "~/org/templates/habits/")
  "Directory for habit templates.")

(defvar my/habit-heatmap-colors
  '((0 . "#1a1a2e")     ; 0% - dark gray
    (25 . "#16213e")    ; 1-25% - very light
    (50 . "#0f3460")    ; 26-50% - light
    (75 . "#533483")    ; 51-75% - medium
    (100 . "#7209b7")   ; 76-100% - bright (target met)
    (150 . "#f72585"))  ; >100% - extra bright (exceeded)
  "Color scale for heat map based on completion percentage.")

;; ============================================================================
;; DIRECTORY SETUP
;; ============================================================================

(defun my/habit-ensure-directories ()
  "Ensure habit directories exist."
  (unless (file-exists-p my/habit-directory)
    (make-directory my/habit-directory t))
  (unless (file-exists-p my/habit-template-dir)
    (make-directory my/habit-template-dir t)))

(my/habit-ensure-directories)

;; ============================================================================
;; PROPERTY UTILITIES
;; ============================================================================

(defun my/habit-get-property (property)
  "Get PROPERTY value from current habit heading."
  (save-excursion
    (org-back-to-heading t)
    (org-entry-get nil property)))

(defun my/habit-set-property (property value)
  "Set PROPERTY to VALUE on current habit heading."
  (save-excursion
    (org-back-to-heading t)
    (org-entry-put nil property (format "%s" value))))

(defun my/habit-get-property-number (property &optional default)
  "Get PROPERTY as number, returning DEFAULT if not found."
  (let ((val (my/habit-get-property property)))
    (if val (string-to-number val) (or default 0))))

(defun my/habit-increment-property (property &optional amount)
  "Increment PROPERTY by AMOUNT (default 1)."
  (let ((current (my/habit-get-property-number property 0))
        (increment (or amount 1)))
    (my/habit-set-property property (+ current increment))))

;; ============================================================================
;; HABIT CREATION
;; ============================================================================

(defun my/habit-create ()
  "Create a new progressive habit note."
  (interactive)
  (my/habit-ensure-directories)
  (let* ((title (read-string "Habit name: "))
         (unit (read-string "Unit (e.g., reps, minutes, pages): " "reps"))
         (base-value (read-number "Starting target value: " 1))
         (increment (read-number "Increment amount per progression: " 1))
         (interval (read-number "Days of target-met before progression: " 7))
         (habit-id (replace-regexp-in-string "[^a-zA-Z0-9]" "-" (downcase title)))
         (template (my/habit-read-template "progressive"))
         (body (format-spec template
                            `((?i . ,habit-id)
                              (?t . ,title)
                              (?b . ,(number-to-string base-value))
                              (?c . ,(number-to-string base-value))
                              (?n . ,(number-to-string increment))
                              (?v . ,(number-to-string interval))
                              (?u . ,unit)
                              (?d . ,(format-time-string "[%Y-%m-%d %a]"))))))
    (denote title '("habit") 'org my/habit-directory nil body)
    (message "Created habit: %s (target: %d %s)" title base-value unit)))

(defun my/habit-read-template (name)
  "Read habit template NAME and return contents."
  (let ((template-file (expand-file-name (concat name ".org") my/habit-template-dir)))
    (if (file-exists-p template-file)
        (with-temp-buffer
          (insert-file-contents template-file)
          (buffer-string))
      ;; Default template if file doesn't exist
      "* %t
:PROPERTIES:
:HABIT_ID: %i
:HABIT_TYPE: progressive
:HABIT_BASE_VALUE: %b
:HABIT_CURRENT_TARGET: %c
:HABIT_INCREMENT: %n
:HABIT_INCREMENT_INTERVAL: %v
:HABIT_UNIT: %u
:HABIT_STREAK_PARTICIPATION: 0
:HABIT_STREAK_TARGET: 0
:HABIT_CONSECUTIVE_TARGET_MET: 0
:HABIT_LAST_TARGET_INCREASE: %d
:END:

** Log
")))

;; ============================================================================
;; HABIT FINDING
;; ============================================================================

(defun my/habit-list-files ()
  "Return list of all habit note files."
  (when (file-exists-p my/habit-directory)
    (directory-files my/habit-directory t "\\.org$")))

(defun my/habit-get-title (file)
  "Get the title of habit FILE."
  (denote-retrieve-title-value file 'org))

(defun my/habit-find ()
  "Find and open a habit note."
  (interactive)
  (let* ((habit-files (my/habit-list-files))
         (candidates (mapcar (lambda (f)
                               (cons (my/habit-get-title f) f))
                             habit-files)))
    (if (null candidates)
        (if (y-or-n-p "No habits found. Create one? ")
            (my/habit-create)
          (message "No habits found."))
      (let* ((selected (completing-read "Habit: " candidates nil t))
             (file (cdr (assoc selected candidates))))
        (find-file file)))))

(defun my/habit-goto-log-section ()
  "Go to the Log section in current habit buffer, creating if needed."
  (goto-char (point-min))
  (if (re-search-forward "^\\*\\* Log$" nil t)
      (org-end-of-subtree t t)
    ;; Create Log section if it doesn't exist
    (goto-char (point-max))
    (insert "\n** Log\n")))

;; ============================================================================
;; LOGGING SYSTEM
;; ============================================================================

(defun my/habit-log-quick ()
  "Log habit completion with current target value."
  (interactive)
  (if (my/habit-in-habit-buffer-p)
      (let ((target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1)))
        (my/habit-log-entry target))
    (my/habit-log-from-anywhere t)))

(defun my/habit-log-value ()
  "Log habit completion with a specific value."
  (interactive)
  (if (my/habit-in-habit-buffer-p)
      (let* ((target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1))
             (unit (or (my/habit-get-property "HABIT_UNIT") "units"))
             (value (read-number (format "Value completed (%s, target: %d): " unit target))))
        (my/habit-log-entry value))
    (my/habit-log-from-anywhere nil)))

(defun my/habit-log-from-anywhere (&optional quick)
  "Select a habit and log completion. If QUICK is non-nil, use target value."
  (interactive)
  (let* ((habit-files (my/habit-list-files))
         (candidates (mapcar (lambda (f)
                               (cons (my/habit-get-title f) f))
                             habit-files)))
    (if (null candidates)
        (message "No habits found. Create one with SPC h n")
      (let* ((selected (completing-read "Log habit: " candidates nil t))
             (file (cdr (assoc selected candidates))))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^\\* " nil t)
              (let* ((target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1))
                     (unit (or (my/habit-get-property "HABIT_UNIT") "units"))
                     (value (if quick
                                target
                              (read-number (format "Value completed (%s, target: %d): " unit target)))))
                (my/habit-log-entry value)))))))))

(defun my/habit-in-habit-buffer-p ()
  "Return t if current buffer is a habit note."
  (and (buffer-file-name)
       (string-prefix-p (file-truename my/habit-directory)
                        (file-truename (buffer-file-name)))))

(defun my/habit-log-entry (value)
  "Create a log entry with VALUE. Must be in habit buffer at habit heading."
  (let* ((target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1))
         (met-target (>= value target))
         (date-heading (format-time-string "*** [%Y-%m-%d %a]")))
    ;; Create log entry
    (save-excursion
      (my/habit-goto-log-section)
      (insert date-heading "\n")
      (insert ":PROPERTIES:\n")
      (insert (format ":LOG_VALUE: %d\n" value))
      (insert (format ":LOG_TARGET: %d\n" target))
      (insert (format ":LOG_MET_TARGET: %s\n" (if met-target "t" "nil")))
      (insert ":END:\n\n"))
    ;; Update streaks
    (my/habit-update-streaks value target met-target)
    ;; Check for progression
    (my/habit-check-progression)
    ;; Save buffer
    (save-buffer)
    ;; Message
    (let ((unit (or (my/habit-get-property "HABIT_UNIT") "units")))
      (if met-target
          (message "Logged %d %s (target: %d). Target met!" value unit target)
        (message "Logged %d %s (target: %d). Keep going!" value unit target)))))

;; ============================================================================
;; STREAK TRACKING
;; ============================================================================

(defun my/habit-update-streaks (_value _target met-target)
  "Update streak properties based on MET-TARGET.
_VALUE and _TARGET are unused but kept for API consistency."
  ;; Always increment participation streak
  (my/habit-increment-property "HABIT_STREAK_PARTICIPATION")
  ;; Update target streak
  (if met-target
      (progn
        (my/habit-increment-property "HABIT_STREAK_TARGET")
        (my/habit-increment-property "HABIT_CONSECUTIVE_TARGET_MET"))
    ;; Reset consecutive counter on miss
    (my/habit-set-property "HABIT_CONSECUTIVE_TARGET_MET" 0)))

(defun my/habit-parse-log-entries ()
  "Parse all log entries in current habit buffer.
Returns list of (date value target met-target) tuples."
  (save-excursion
    (goto-char (point-min))
    (let ((entries '()))
      (while (re-search-forward "^\\*\\*\\* \\[\\([0-9-]+\\)" nil t)
        (let* ((date (match-string 1))
               (value (save-excursion
                        (when (re-search-forward ":LOG_VALUE: \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1)))))
               (target (save-excursion
                         (when (re-search-forward ":LOG_TARGET: \\([0-9]+\\)" nil t)
                           (string-to-number (match-string 1)))))
               (met (save-excursion
                      (when (re-search-forward ":LOG_MET_TARGET: \\(t\\|nil\\)" nil t)
                        (string= (match-string 1) "t")))))
          (when (and date value target)
            (push (list date value target met) entries))))
      (nreverse entries))))

(defun my/habit-calculate-streaks ()
  "Recalculate all streak values from log entries."
  (interactive)
  (let* ((entries (my/habit-parse-log-entries))
         (participation-count (length entries))
         (target-count (cl-count-if (lambda (e) (nth 3 e)) entries))
         (consecutive 0)
         (done nil))
    ;; Calculate consecutive target-met from end
    (dolist (entry (reverse entries))
      (unless done
        (if (nth 3 entry)
            (cl-incf consecutive)
          (setq done t))))
    ;; Update properties
    (my/habit-set-property "HABIT_STREAK_PARTICIPATION" participation-count)
    (my/habit-set-property "HABIT_STREAK_TARGET" target-count)
    (my/habit-set-property "HABIT_CONSECUTIVE_TARGET_MET" consecutive)
    (message "Streaks updated: %d participation, %d target-met, %d consecutive"
             participation-count target-count consecutive)))

;; ============================================================================
;; AUTO-PROGRESSION
;; ============================================================================

(defun my/habit-check-progression ()
  "Check if habit should progress to next level and apply if ready."
  (let* ((consecutive (my/habit-get-property-number "HABIT_CONSECUTIVE_TARGET_MET" 0))
         (interval (my/habit-get-property-number "HABIT_INCREMENT_INTERVAL" 7)))
    (when (>= consecutive interval)
      (my/habit-progress-target))))

(defun my/habit-progress-target ()
  "Increase current target by increment amount."
  (let* ((current-target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1))
         (increment (my/habit-get-property-number "HABIT_INCREMENT" 1))
         (unit (or (my/habit-get-property "HABIT_UNIT") "units"))
         (new-target (+ current-target increment)))
    ;; Update target
    (my/habit-set-property "HABIT_CURRENT_TARGET" new-target)
    ;; Reset consecutive counter
    (my/habit-set-property "HABIT_CONSECUTIVE_TARGET_MET" 0)
    ;; Record progression date
    (my/habit-set-property "HABIT_LAST_TARGET_INCREASE"
                           (format-time-string "[%Y-%m-%d %a]"))
    (message "Level up! New target: %d %s (was %d)" new-target unit current-target)))

;; ============================================================================
;; HEAT MAP CALENDAR
;; ============================================================================

(defun my/habit-get-color-for-percentage (pct)
  "Get color for completion percentage PCT."
  (cond
   ((= pct 0) (cdr (assoc 0 my/habit-heatmap-colors)))
   ((<= pct 25) (cdr (assoc 25 my/habit-heatmap-colors)))
   ((<= pct 50) (cdr (assoc 50 my/habit-heatmap-colors)))
   ((<= pct 75) (cdr (assoc 75 my/habit-heatmap-colors)))
   ((<= pct 100) (cdr (assoc 100 my/habit-heatmap-colors)))
   (t (cdr (assoc 150 my/habit-heatmap-colors)))))

(defun my/habit-calculate-completion-percentage (value target)
  "Calculate completion percentage from VALUE and TARGET."
  (if (zerop target)
      0
    (round (* 100.0 (/ (float value) target)))))

(defun my/habit-entries-to-hash (entries)
  "Convert ENTRIES list to hash table keyed by date string YYYY-MM-DD."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (puthash (car entry) entry table))
    table))

(defun my/habit-heatmap-month (year month habit-file)
  "Generate heat map data for YEAR MONTH from HABIT-FILE.
Returns list of (day percentage) pairs."
  (with-current-buffer (find-file-noselect habit-file)
    (let* ((entries (my/habit-parse-log-entries))
           (entries-hash (my/habit-entries-to-hash entries))
           (days-in-month (calendar-last-day-of-month month year))
           (result '()))
      (dotimes (d days-in-month)
        (let* ((day (1+ d))
               (date-str (format "%04d-%02d-%02d" year month day))
               (entry (gethash date-str entries-hash))
               (pct (if entry
                        (my/habit-calculate-completion-percentage (nth 1 entry) (nth 2 entry))
                      0)))
          (push (list day pct) result)))
      (nreverse result))))

(defun my/habit-heatmap-view ()
  "Display heat map calendar for a habit."
  (interactive)
  (let* ((habit-files (my/habit-list-files))
         (candidates (mapcar (lambda (f)
                               (cons (my/habit-get-title f) f))
                             habit-files)))
    (if (null candidates)
        (message "No habits found. Create one with SPC h n")
      (let* ((selected (completing-read "Habit for calendar: " candidates nil t))
             (file (cdr (assoc selected candidates)))
             (now (decode-time))
             (year (nth 5 now))
             (month (nth 4 now)))
        (my/habit-display-heatmap file year month)))))

(defun my/habit-display-heatmap (habit-file year month)
  "Display heat map for HABIT-FILE for YEAR MONTH."
  (let* ((buffer-name "*Habit Heat Map*")
         (habit-title (my/habit-get-title habit-file))
         (data (my/habit-heatmap-month year month habit-file))
         (month-name (calendar-month-name month))
         (first-day (calendar-day-of-week (list month 1 year))))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert (propertize (format " %s - %s %d\n\n" habit-title month-name year)
                            'face '(:weight bold :height 1.2)))
        ;; Day headers
        (insert "  Su  Mo  Tu  We  Th  Fr  Sa\n")
        ;; Calendar grid
        ;; Add padding for first week
        (dotimes (_ first-day)
          (insert "    "))
        ;; Days
        (let ((current-weekday first-day))
          (dolist (day-data data)
            (let* ((day (car day-data))
                   (pct (cadr day-data))
                   (color (my/habit-get-color-for-percentage pct))
                   (day-str (format "%3d " day)))
              (insert (propertize day-str 'face `(:background ,color :foreground "white")))
              (cl-incf current-weekday)
              (when (= current-weekday 7)
                (insert "\n")
                (setq current-weekday 0)))))
        (insert "\n\n")
        ;; Legend
        (insert "Legend: ")
        (dolist (level '((0 "0%") (50 "50%") (100 "100%") (150 ">100%")))
          (let ((pct (car level))
                (label (cadr level)))
            (insert (propertize (format " %s " label)
                                'face `(:background ,(my/habit-get-color-for-percentage pct)
                                                    :foreground "white")))
            (insert " ")))
        (insert "\n\n")
        ;; Navigation hint
        (insert "[p] Previous month  [n] Next month  [h] Select habit  [q] Quit\n")
        ;; Set up local keymap
        (setq-local my/habit-heatmap-file habit-file)
        (setq-local my/habit-heatmap-year year)
        (setq-local my/habit-heatmap-month month)
        (use-local-map (let ((map (make-sparse-keymap)))
                         (define-key map "p" #'my/habit-heatmap-prev-month)
                         (define-key map "n" #'my/habit-heatmap-next-month)
                         (define-key map "h" #'my/habit-heatmap-select-habit)
                         (define-key map "q" #'quit-window)
                         map))
        (setq buffer-read-only t)
        (goto-char (point-min))))
    (switch-to-buffer buffer-name)))

(defun my/habit-heatmap-prev-month ()
  "Show previous month in heat map."
  (interactive)
  (let* ((year my/habit-heatmap-year)
         (month my/habit-heatmap-month)
         (file my/habit-heatmap-file))
    (if (= month 1)
        (progn
          (setq year (1- year))
          (setq month 12))
      (setq month (1- month)))
    (my/habit-display-heatmap file year month)))

(defun my/habit-heatmap-next-month ()
  "Show next month in heat map."
  (interactive)
  (let* ((year my/habit-heatmap-year)
         (month my/habit-heatmap-month)
         (file my/habit-heatmap-file))
    (if (= month 12)
        (progn
          (setq year (1+ year))
          (setq month 1))
      (setq month (1+ month)))
    (my/habit-display-heatmap file year month)))

(defun my/habit-heatmap-select-habit ()
  "Select a different habit to display."
  (interactive)
  (let* ((habit-files (my/habit-list-files))
         (candidates (mapcar (lambda (f)
                               (cons (my/habit-get-title f) f))
                             habit-files))
         (selected (completing-read "Habit: " candidates nil t))
         (file (cdr (assoc selected candidates))))
    (my/habit-display-heatmap file my/habit-heatmap-year my/habit-heatmap-month)))

;; ============================================================================
;; DASHBOARD
;; ============================================================================

(defun my/habit-dashboard ()
  "Display dashboard overview of all habits."
  (interactive)
  (let* ((habit-files (my/habit-list-files))
         (buffer-name "*Habit Dashboard*"))
    (if (null habit-files)
        (message "No habits found. Create one with SPC h n")
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize " Habit Dashboard\n\n" 'face '(:weight bold :height 1.3)))
          (insert (format "%-25s %8s %8s %10s %12s\n"
                          "Habit" "Target" "P-Streak" "T-Streak" "Consecutive"))
          (insert (make-string 70 ?-) "\n")
          (dolist (file habit-files)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^\\* " nil t)
                  (let* ((title (my/habit-get-title file))
                         (target (my/habit-get-property-number "HABIT_CURRENT_TARGET" 1))
                         (unit (or (my/habit-get-property "HABIT_UNIT") ""))
                         (p-streak (my/habit-get-property-number "HABIT_STREAK_PARTICIPATION" 0))
                         (t-streak (my/habit-get-property-number "HABIT_STREAK_TARGET" 0))
                         (consecutive (my/habit-get-property-number "HABIT_CONSECUTIVE_TARGET_MET" 0)))
                    (insert (format "%-25s %5d %-3s %8d %10d %12d\n"
                                    (truncate-string-to-width title 25)
                                    target unit p-streak t-streak consecutive)))))))
          (insert "\n")
          ;; Legend
          (insert "P-Streak: Participation days | T-Streak: Target-met days | Consecutive: Current run\n\n")
          ;; Keybindings
          (insert "[l] Log habit  [c] Calendar  [n] New habit  [q] Quit\n")
          (use-local-map (let ((map (make-sparse-keymap)))
                           (define-key map "l" #'my/habit-log-from-anywhere)
                           (define-key map "c" #'my/habit-heatmap-view)
                           (define-key map "n" #'my/habit-create)
                           (define-key map "q" #'quit-window)
                           map))
          (setq buffer-read-only t)
          (goto-char (point-min))))
      (switch-to-buffer buffer-name))))

;; ============================================================================
;; DYNAMIC BLOCK: HABIT TRACKER TABLE
;; ============================================================================

(defun my/habit-get-log-data (file)
  "Get all log entries from FILE as hash table keyed by date (YYYY-MM-DD).
Each value is a plist with :value, :target, :met."
  (let ((data (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Log" nil t)
        (while (re-search-forward "^\\*\\*\\* \\[\\([0-9]+-[0-9]+-[0-9]+\\)" nil t)
          (let ((date-str (match-string 1))
                (entry-end (save-excursion
                             (or (and (re-search-forward "^\\*\\*\\*" nil t) (point))
                                 (point-max))))
                value target met)
            (save-excursion
              (when (re-search-forward ":LOG_VALUE:\\s-*\\([0-9]+\\)" entry-end t)
                (setq value (string-to-number (match-string 1)))))
            (save-excursion
              (when (re-search-forward ":LOG_TARGET:\\s-*\\([0-9]+\\)" entry-end t)
                (setq target (string-to-number (match-string 1)))))
            (save-excursion
              (when (re-search-forward ":LOG_MET_TARGET:\\s-*\\(t\\|nil\\)" entry-end t)
                (setq met (string= (match-string 1) "t"))))
            (when (and value target)
              (puthash date-str (list :value value :target target :met met) data))))))
    data))

(defun my/habit-format-date (time-value)
  "Format TIME-VALUE as 'Mon DD' (e.g., 'Jan 28')."
  (format-time-string "%b %d" time-value))

(defun my/habit-get-title-from-file (file)
  "Extract habit title from FILE using #+title."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
        (string-trim (match-string 1))
      (file-name-base file))))

(defun org-dblock-write:habit-tracker (params)
  "Generate habit tracker table for the last N days.
PARAMS can include:
  :days - Number of days to show (default 7)
  :dir - Habit directory (default `my/habit-directory')
  :show-values - Show value/target columns (default t)"
  (let* ((days (or (plist-get params :days) 7))
         (dir (or (plist-get params :dir) my/habit-directory))
         (show-values (if (plist-member params :show-values)
                          (plist-get params :show-values)
                        t))
         (files (directory-files (expand-file-name dir) t "\\.org$"))
         (today (current-time))
         ;; Generate list of dates from oldest to newest
         (dates (cl-loop for i from (1- days) downto 0
                         collect (time-subtract today (days-to-time i))))
         (date-strings (mapcar (lambda (d) (format-time-string "%Y-%m-%d" d)) dates))
         (date-headers (mapcar #'my/habit-format-date dates))
         ;; Collect habit data: ((title . log-hash) ...)
         (habits (mapcar (lambda (f)
                           (cons (my/habit-get-title-from-file f)
                                 (my/habit-get-log-data f)))
                         files))
         ;; Track totals per day
         (day-totals (make-vector days 0))
         (day-counts (make-vector days 0)))
    ;; Insert table header
    (insert "| Habit |")
    (dolist (header date-headers)
      (insert (format " %s |" header)))
    (when show-values
      (insert " Total |"))
    (insert " Rate |\n")
    ;; Separator
    (insert "|-------|")
    (dotimes (_ days)
      (insert "--------|"))
    (when show-values
      (insert "-------|"))
    (insert "------|\n")
    ;; Insert habit rows
    (dolist (habit habits)
      (let* ((name (car habit))
             (log-data (cdr habit))
             (habit-total 0)
             (habit-met 0))
        (insert (format "| %s |" name))
        (cl-loop for date-str in date-strings
                 for i from 0
                 do (let* ((entry (gethash date-str log-data))
                           (cell-str
                            (if entry
                                (let ((value (plist-get entry :value))
                                      (target (plist-get entry :target))
                                      (met (plist-get entry :met)))
                                  (cl-incf habit-total value)
                                  (when met
                                    (cl-incf habit-met)
                                    (aset day-totals i (1+ (aref day-totals i))))
                                  (aset day-counts i (1+ (aref day-counts i)))
                                  (if show-values
                                      (format "%d/%d %s" value target (if met "✓" "✗"))
                                    (if met "✓" "✗")))
                              "-")))
                      (insert (format " %s |" cell-str))))
        (when show-values
          (insert (format " %d |" habit-total)))
        (insert (format " %d%% |" (if (> days 0)
                                      (round (* 100.0 (/ (float habit-met) days)))
                                    0)))
        (insert "\n")))
    ;; Totals row
    (insert "|-------|")
    (dotimes (_ days)
      (insert "--------|"))
    (when show-values
      (insert "-------|"))
    (insert "------|\n")
    (insert "| *Total* |")
    (let ((grand-total 0))
      (dotimes (i days)
        (let ((day-met (aref day-totals i)))
          (cl-incf grand-total day-met)
          (insert (format " %d/%d |" day-met (length habits)))))
      (when show-values
        (insert (format " %d |" grand-total)))
      (insert (format " %d%% |"
                      (let ((possible (* days (length habits))))
                        (if (> possible 0)
                            (round (* 100.0 (/ (float grand-total) possible)))
                          0)))))
    (insert "\n")
    ;; Align the table
    (forward-line -1)
    (org-table-align)))

(defun my/habit-insert-tracker-block (&optional days)
  "Insert a habit tracker dynamic block with DAYS (default 7)."
  (interactive "P")
  (let ((num-days (or days 7)))
    (insert (format "#+BEGIN: habit-tracker :days %d\n" num-days))
    (insert "#+END:\n"))
  (forward-line -1)
  (org-ctrl-c-ctrl-c))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Declare to silence byte-compiler warnings
(declare-function zzc/leader-keys "keybindings")

(with-eval-after-load 'general
  (zzc/leader-keys
    "h"   '(:ignore t :which-key "habit")
    "h h" '(my/habit-find :which-key "find habit")
    "h n" '(my/habit-create :which-key "new habit")
    "h l" '(my/habit-log-from-anywhere :which-key "log habit")
    "h q" '(my/habit-log-quick :which-key "quick log")
    "h v" '(my/habit-log-value :which-key "log value")
    "h c" '(my/habit-heatmap-view :which-key "calendar")
    "h d" '(my/habit-dashboard :which-key "dashboard")
    "h r" '(my/habit-calculate-streaks :which-key "recalculate streaks")
    "h t" '(my/habit-insert-tracker-block :which-key "insert tracker table")))

(provide 'habit-tracker)
;;; habit-tracker.el ends here
