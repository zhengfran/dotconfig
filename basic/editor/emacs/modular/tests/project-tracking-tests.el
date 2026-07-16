;;; project-tracking-tests.el --- Tests for Task/Issue tracking -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the Task & Issue tracking added to denote-config.el.
;;
;; These exercise the highest seams -- the data a function returns or the
;; buffer state it produces -- not Org internals or table-string formatting:
;;   - `my/project-item-type' / `my/project-item-next-state' (pure predicates)
;;   - `my/project-collect-items-in-buffer'  (the dashboard's query)
;;   - `my/task-blocked-by-open-issue-p'      (the blocker predicate)
;;   - `my/denote--extract-stub-rewrite'      (extraction's buffer rewrite,
;;                                             independent of Denote file I/O)
;;
;; Fixtures are inline Org strings (self-contained, less brittle than files).
;; Run headless with tests/run-tests.sh, which loads the full init first so the
;; global `org-todo-keywords' (both sequences) are active.

;;; Code:

(require 'ert)
(require 'org)

(defmacro pt-test-with-org (content &rest body)
  "Run BODY in a temporary Org buffer initialized with CONTENT."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun pt-test--titles (items)
  "Return the sorted :title values of ITEMS."
  (sort (mapcar (lambda (i) (plist-get i :title)) items) #'string<))

;; ----------------------------------------------------------------------------
;; Classification / cycling (pure)
;; ----------------------------------------------------------------------------

(ert-deftest pt-item-type ()
  (should (eq (my/project-item-type "TODO") 'task))
  (should (eq (my/project-item-type "ONGOING") 'task))
  (should (eq (my/project-item-type "DONE") 'task))
  (should (eq (my/project-item-type "ISSUE") 'issue))
  (should (eq (my/project-item-type "INVESTIGATING") 'issue))
  (should (eq (my/project-item-type "RESOLVED") 'issue))
  (should (null (my/project-item-type "MEETING")))
  ;; At-point path: a heading with no TODO keyword classifies as nil.
  (pt-test-with-org "* Just a heading\n"
    (org-back-to-heading t)
    (should (null (my/project-item-type)))))

(ert-deftest pt-next-state-stays-in-sequence ()
  (should (equal (my/project-item-next-state "TODO") "ONGOING"))
  ;; End of the Task sequence wraps to its own start, NOT into the Issue seq.
  (should (equal (my/project-item-next-state "DONE") "TODO"))
  (should (equal (my/project-item-next-state "ISSUE") "INVESTIGATING"))
  (should (equal (my/project-item-next-state "RESOLVED") "ISSUE"))
  (should (null (my/project-item-next-state "NOPE"))))

;; ----------------------------------------------------------------------------
;; Collection
;; ----------------------------------------------------------------------------

(defconst pt-fixture-project "\
* Tasks
** ONGOING Debug deser
*** ISSUE pca9539 panic
*** RESOLVED old glitch
** TODO Raise PR
** DONE Shipped
* Issues
** INVESTIGATING flaky clock
")

(ert-deftest pt-collect-open-tasks ()
  (pt-test-with-org pt-fixture-project
    ;; DONE excluded; ONGOING and TODO kept.
    (should (equal (pt-test--titles (my/project-collect-items-in-buffer 'task t))
                   '("Debug deser" "Raise PR")))))

(ert-deftest pt-collect-open-issues ()
  (pt-test-with-org pt-fixture-project
    ;; RESOLVED excluded; ISSUE and INVESTIGATING kept.
    (should (equal (pt-test--titles (my/project-collect-items-in-buffer 'issue t))
                   '("flaky clock" "pca9539 panic")))))

(ert-deftest pt-collect-issue-blocks-nearest-task ()
  (pt-test-with-org pt-fixture-project
    (let* ((issues (my/project-collect-items-in-buffer 'issue t))
           (panic (seq-find (lambda (i) (equal (plist-get i :title) "pca9539 panic"))
                            issues))
           (flaky (seq-find (lambda (i) (equal (plist-get i :title) "flaky clock"))
                            issues)))
      ;; The nested issue is blocking its nearest ancestor Task.
      (should (equal (plist-get panic :blocks) "Debug deser"))
      ;; A project-level issue blocks no Task.
      (should (null (plist-get flaky :blocks))))))

(ert-deftest pt-collect-type-filter-and-closed ()
  (pt-test-with-org pt-fixture-project
    ;; Without open-only, closed items are included too.
    (should (member "Shipped"
                    (mapcar (lambda (i) (plist-get i :title))
                            (my/project-collect-items-in-buffer 'task nil))))
    ;; Type filter keeps issues out of the task set.
    (should-not (member "pca9539 panic"
                        (mapcar (lambda (i) (plist-get i :title))
                                (my/project-collect-items-in-buffer 'task nil))))))

;; ----------------------------------------------------------------------------
;; Blocking predicate
;; ----------------------------------------------------------------------------

(defun pt-test--blocked-p (title)
  "Return non-nil if the Task titled TITLE is blocked by an open Issue."
  (let (result)
    (org-map-entries
     (lambda ()
       (when (equal (org-get-heading t t t t) title)
         (setq result (my/task-blocked-by-open-issue-p)))))
    result))

(ert-deftest pt-block-open-issue-blocks-parent ()
  (pt-test-with-org "\
* TODO Parent
** ISSUE open problem
"
    (should (pt-test--blocked-p "Parent"))))

(ert-deftest pt-block-resolved-issue-does-not-block ()
  (pt-test-with-org "\
* TODO Parent
** RESOLVED fixed
"
    (should-not (pt-test--blocked-p "Parent"))))

(ert-deftest pt-block-child-task-does-not-block ()
  (pt-test-with-org "\
* TODO Parent
** TODO Child task
"
    (should-not (pt-test--blocked-p "Parent"))))

(ert-deftest pt-block-issue-blocks-its-owner-not-grandparent ()
  ;; Issue under a child Task blocks the child, not the grandparent.
  (pt-test-with-org "\
* TODO Grand
** TODO Child
*** ISSUE deep problem
"
    (should-not (pt-test--blocked-p "Grand"))
    (should (pt-test--blocked-p "Child"))))

;; ----------------------------------------------------------------------------
;; Extraction: stub rewrite (no Denote I/O)
;; ----------------------------------------------------------------------------

(ert-deftest pt-extract-stub-rewrite ()
  (pt-test-with-org "\
* ISSUE pca9539 panic
:PROPERTIES:
:CUSTOM_ID: x
:END:
kernel panic log line 1
line 2
** ISSUE child stays
"
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((body (my/denote--extract-stub-rewrite "20260101T000000" "pca9539 panic")))
      ;; Body was captured for the note.
      (should (string-match-p "kernel panic log line 1" body))
      (should (string-match-p "line 2" body))
      ;; Heading and its keyword survive.
      (goto-char (point-min))
      (should (looking-at-p "\\* ISSUE pca9539 panic"))
      ;; Link to the detail note was inserted.
      (should (save-excursion
                (search-forward "[[denote:20260101T000000][pca9539 panic]]" nil t)))
      ;; Original body text is gone from the stub.
      (should-not (save-excursion (search-forward "kernel panic log line 1" nil t)))
      ;; Property drawer and child heading are preserved.
      (should (save-excursion (search-forward ":CUSTOM_ID: x" nil t)))
      (should (save-excursion (search-forward "** ISSUE child stays" nil t))))))

(provide 'project-tracking-tests)
;;; project-tracking-tests.el ends here
