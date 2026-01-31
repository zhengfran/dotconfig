;;; org-agenda-config.el --- Org agenda and TODO management -*- lexical-binding: t; -*-

;;; Commentary:
;; Org agenda configuration, TODO keywords, custom agenda views,
;; org-pomodoro, task completion automation
;;
;; DEPENDENCIES: org-base, keybindings (zzc/leader-keys)
;; USED BY: denote (org-agenda-files variable)

;;; Code:

;; ============================================================================
;; AGENDA FILE CONFIGURATION
;; ============================================================================

(setq org-agenda-dir "~/org/notes/")
;; Initialize with denote directory
;; Note: my/denote-refresh-agenda-list in denote-config.el adds project files
(setq org-agenda-files (list org-agenda-dir))

;; ============================================================================
;; TODO KEYWORDS AND LOGGING
;; ============================================================================

;; Log timestamp when TODO state changes
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; TODO keywords with logging
(setq org-todo-keywords
   '((sequence "TODO(t)" "ONGOING(o)" "|" "CANCEL(c@)" "DONE(d!)")))

;; ============================================================================
;; AGENDA APPEARANCE
;; ============================================================================

(setq org-agenda-start-with-log-mode t)
(setq org-agenda-span 'week)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo '("DONE" "CANCEL")))
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------"))
(setq org-agenda-current-time-string "← now")
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

;; ============================================================================
;; CUSTOM AGENDA VIEWS
;; ============================================================================

;; Helper function for agenda filtering
(defun my/skip-non-work-tasks ()
  "Skip agenda entries that don't have :work: tag.
Respects tag inheritance from parent headings.
Returns position to skip to if item should be skipped, nil otherwise."
  (let ((tags (org-get-tags)))
    (if (not (member "work" tags))
        (org-entry-end-position)
      nil)))

;; Custom agenda views
(setq org-agenda-custom-commands
 '(("d" "Dashboard"
    ((agenda "" ((org-deadline-warning-days 7)
                 (org-agenda-overriding-header "Week Overview")))
     (todo "ONGOING"
           ((org-agenda-overriding-header "Active Tasks")))
     (tags-todo "+PRIORITY=\"A\""
                ((org-agenda-overriding-header "High Priority")))))
   
   ("o" "Ongoing Tasks"
    ((todo "ONGOING"
           ((org-agenda-overriding-header "Currently Working On")))))
   
   ("p" "Priority View"
    ((tags-todo "+PRIORITY=\"A\""
                ((org-agenda-overriding-header "Priority A")))
     (tags-todo "+PRIORITY=\"B\""
                ((org-agenda-overriding-header "Priority B")))
     (tags-todo "+PRIORITY=\"C\""
                ((org-agenda-overriding-header "Priority C")))))
   
   ("w" "Weekly Review"
    ((agenda "" ((org-agenda-span 7)
                 (org-agenda-start-day "-7d")
                 (org-agenda-overriding-header "Past Week")))
     (agenda "" ((org-agenda-span 7)
                 (org-agenda-start-day "+0d")
                 (org-agenda-overriding-header "Next Week")))))
   
   ("u" "Unscheduled TODO"
    ((todo "TODO"
           ((org-agenda-skip-function
             '(org-agenda-skip-entry-if 'scheduled 'deadline))
            (org-agenda-overriding-header "Unscheduled Tasks")))))
   
   ("k" "Work Dashboard"
    ((agenda ""
             ((org-agenda-span 7)
              (org-agenda-skip-function 'my/skip-non-work-tasks)
              (org-agenda-overriding-header "📅 Work Schedule (This Week)")))
     (tags-todo "+work/+TODO|+ONGOING"
                ((org-agenda-overriding-header "⚡ Active Work Tasks")
                 (org-agenda-sorting-strategy '(priority-down time-up))))))))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Key-binds
(general-define-key
 :prefix "C-c"
 "a" 'org-agenda)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "k") 'org-agenda-previous-item)
            (local-set-key (kbd "j") 'org-agenda-next-item)))

;; ============================================================================
;; AUTO-SAVE ON TODO STATE CHANGE
;; ============================================================================

;; Save all org files after change todo
(defmacro η (fnc)
  "return function that ignores its arguments and invokes fnc."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (η #'org-save-all-org-buffers))
(advice-add 'org-priority       :after (η #'org-save-all-org-buffers))

;; ============================================================================
;; ORG-POMODORO
;; ============================================================================

(use-package org-pomodoro)
(setq org-pomodoro-audio-player "mpv"
      org-pomodoro-ticking-sound-p t
      org-pomodoro-ticking-sound-states '(:pomodoro)
      org-pomodoro-finished-sound-p t
      org-pomodoro-short-break-length 5
      org-pomodoro-finished-sound-args "--volume=50"
      org-pomodoro-long-break-sound-args "--volume=50"
      org-pomodoro-short-break-sound-args "--volume=50"
      org-pomodoro-ticking-sound-args "--volume=60")

;;key-binds
(zzc/leader-keys
  "c"  '(:ignore t :which-key "clock")
  "ci" '(org-clock-in :which-key "clock-in")
  "co" '(org-clock-out :which-key "clock-out")
  "cq" '(org-clock-cancel :which-key "clock-cancel")
  "cr" '(org-clock-report :which-key "clock-report")
  "cp" '(org-pomodoro :which-key "clock-pomodoro")
  "cd" '(org-clock-display :which-key "clock-display"))

(provide 'org-agenda-config)
;;; org-agenda-config.el ends here
