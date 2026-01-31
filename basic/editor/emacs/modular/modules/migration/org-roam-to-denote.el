;;; org-roam-to-denote.el --- Migrate org-roam notes to denote -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Adapted for modular Emacs config
;; Original Copyright (C) 2022 bitspook <bitspook@proton.me>

;; Author: Adapted from https://github.com/bitspook/notes-migrator
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (denote "1.0.0") (org-roam "2.0.0"))

;;; Commentary:
;; Migrate org-roam notes to denote with support for:
;; - Subdirectory structure (daily/ → journal/, projects/, trades/, ref/)
;; - Dry-run mode for safe testing
;; - Progress reporting
;; - Backlink preservation
;;
;; Adaptations from original:
;; - Handle daily/ subdirectory → journal/ with "journal" keyword
;; - Handle projects/ subdirectory files → root with "project" keyword
;; - Keep trades/ structure with "trade" keyword
;; - Keep ref/ structure with "ref" keyword
;; - Add dry-run capability
;; - Better error handling and reporting

;;; Code:
(require 'denote)
(require 'org-element)
(require 'org-roam)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar my/migration-source-dir "~/org/notes/"
  "Source directory containing org-roam notes.")

(defvar my/migration-dry-run t
  "When non-nil, only preview changes without applying them.")

(defvar my/migration-stats '(:total 0 :migrated 0 :skipped 0 :errors 0)
  "Migration statistics.")

;; ============================================================================
;; HELPER FUNCTIONS FROM ORIGINAL
;; ============================================================================

(defun nm--roam-node-ctime (node)
  "Get create-time of org-roam NODE.
It assumes that date is stored in the filename of NODE in one of
the 3 formats:
- YYYY-MM-DD.org (e.g in case of org-roam-dailies)
- YYYYMMDDHHMMSS.*.org (new org-roam nodes)
- YYYY-MM-DD--HH-MM-SS.*.org (old org-roam nodes)"
  (let* ((fname (file-name-base (org-roam-node-file node)))
         (old-date-rx (rx (group (= 4 num) "-" (= 2 num) "-" (= 2 num))
                          "--" (group (= 2 num)) "-" (group (= 2 num)) "-" (group (= 2 num)) "Z"))
         (new-date-rx (rx (group (= 4 num)) (group (= 2 num)) (group (= 2 num))
                          (group (= 2 num)) (group (= 2 num)) (group (= 2 num)) "-"))
         (dailies-date-rx (rx (= 4 num) "-" (= 2 num) "-" (= 2 num)))
         (time-str (save-match-data
                     (or (and (string-match old-date-rx fname)
                              (concat (match-string 1 fname) "T"
                                      (format "%s:%s:%s" (match-string 2 fname) (match-string 3 fname) (match-string 4 fname))))
                         (and (string-match new-date-rx fname)
                              (format "%s-%s-%sT%s:%s:%s"
                                      (match-string 1 fname) (match-string 2 fname) (match-string 3 fname)
                                      (match-string 4 fname) (match-string 5 fname) (match-string 6 fname)))
                         (and (string-match dailies-date-rx fname)
                              (format "%sT00:00:00" (match-string 0 fname)))))))
    (when (not time-str) (error "Encountered org-roam file with unknown name: %s.org" fname))
    (encode-time (parse-time-string time-str))))

(defun nm--roam-node-denote-id (node)
  "Create denote identifier for org-roam NODE.
It returns creation timestamp of NODE, which is obtained using `nm--roam-node-ctime'."
  (format-time-string denote-id-format (nm--roam-node-ctime node)))

(defun nm--org-element-save-to-buffer (el)
  "Save `org-element' EL back in `current-buffer'.
Make sure EL is obtained from `current-buffer.'"
  (let ((begin (org-element-property :begin el))
        (end (org-element-property :end el)))
    (delete-region begin end)
    (goto-char begin)
    (insert (org-element-interpret-data el))))

(defun nm--add-org-file-tags (tags)
  "Set #+filetags in `current-buffer' to TAGS.
Existing filetags aren't removed, but are converted to :tag:
format."
  (goto-char (point-min))
  (re-search-forward (rx "#+title: ") nil t)
  (end-of-line)

  (when (not (re-search-forward (rx "#+filetags: ") nil t))
    (insert "\n#+filetags: "))

  (let* ((el (org-element-context))
         (old-tags (org-element-property :value el)))
    (setf old-tags (split-string (string-replace " " ":" old-tags) ":" t))
    (org-element-put-property el :value (concat ":" (string-join (seq-concatenate 'list old-tags tags) ":") ":"))
    (nm--org-element-save-to-buffer el)))

(defun nm--roam-node-from-file (fname)
  "Find org-roam node for file with name FNAME."
  (condition-case err
      (with-temp-buffer
        (insert-file fname)
        (goto-char (line-beginning-position 2))
        (org-mode)
        (org-roam-node-from-id (org-element-property :value (org-element-at-point))))
    (error
     (message "Warning: Could not extract node from %s: %s" fname err)
     nil)))

;; ============================================================================
;; ADAPTED FUNCTIONS FOR THIS CONFIG
;; ============================================================================

(defun my/migration-get-subdir-for-file (file)
  "Determine target subdirectory for FILE based on its current location.
Returns cons of (target-subdir . extra-keywords)."
  (let ((relative-path (file-relative-name file (expand-file-name my/migration-source-dir))))
    (cond
     ;; daily/ → journal/ with journal keyword
     ((string-prefix-p "daily/" relative-path)
      (cons "journal/" '("journal")))
     ;; projects/ → root with project keyword
     ((string-prefix-p "projects/" relative-path)
      (cons "" '("project")))
     ;; trades/ → keep in trades/ with trade keyword
     ((string-prefix-p "trades/" relative-path)
      (cons "trades/" '("trade")))
     ;; ref/ → keep in ref/ with ref keyword
     ((string-prefix-p "ref/" relative-path)
      (cons "ref/" '("ref")))
     ;; root files stay in root
     (t (cons "" '())))))

(defun my/migration-denote-filename (node extra-keywords subdir)
  "Generate denote filename for NODE with EXTRA-KEYWORDS in SUBDIR."
  (let* ((id (nm--roam-node-denote-id node))
         (base-tags (mapcar #'downcase (org-roam-node-tags node)))
         (all-tags (delete-dups (append base-tags extra-keywords)))
         (title (or (string-replace "/" "-" (org-roam-node-title node)) "untitled"))
         (filename (concat id "--" (denote-sluggify-title title)
                          (when all-tags
                            (concat "__" (string-join all-tags "_")))
                          ".org")))
    (expand-file-name filename (expand-file-name subdir denote-directory))))

(defun my/migration-convert-roam-links-to-denote (&optional filename)
  "Convert all org-roam links in `current-buffer' to denote links.
If org-roam node for a link is not found, a warning is logged and
the link is not converted.
FILENAME can be optionally provided for debugging in case of
failed link conversions."
  (let ((roam-link-rx (rx "[[id:"))
        (converted 0)
        (failed 0))
    (goto-char (point-min))
    (while (re-search-forward roam-link-rx nil t)
      (let* ((el (org-element-copy (org-element-context)))
             (node-id (org-element-property :path el))
             (node (org-roam-node-from-id node-id)))
        (if (not node)
            (progn
              (setq failed (1+ failed))
              (warn "Failed to convert org-roam link to denote because corresponding org-roam node wasn't found. [id=%s, filename=%s]" node-id filename))
          (let* ((begin (org-element-property :begin el))
                 (end (org-element-property :end el)))
            (replace-string (format "id:%s" node-id)
                          (format "denote:%s" (nm--roam-node-denote-id node))
                          nil begin end)
            (setq converted (1+ converted))))))
    (message "  Links: %d converted, %d failed" converted failed)))

(defun my/migration-migrate-single-node (node)
  "Migrate a single org-roam NODE to denote format.
Returns t on success, nil on failure."
  (condition-case err
      (let* ((file (org-roam-node-file node))
             (subdir-info (my/migration-get-subdir-for-file file))
             (subdir (car subdir-info))
             (extra-keywords (cdr subdir-info))
             (new-name (my/migration-denote-filename node extra-keywords subdir)))

        (if my/migration-dry-run
            (progn
              (message "[DRY-RUN] Would migrate:")
              (message "  From: %s" (file-relative-name file (expand-file-name my/migration-source-dir)))
              (message "  To:   %s" (file-relative-name new-name denote-directory))
              (message "  Keywords: %s" (string-join (append (org-roam-node-tags node) extra-keywords) ", "))
              t)

          ;; Actual migration
          (with-temp-buffer
            (erase-buffer)
            (insert-file-contents file)
            (goto-char (point-min))

            ;; Delete the properties drawer roam inserts on top
            (when (search-forward ":END:" nil t)
              (delete-region (point-min) (1+ (point))))

            (org-mode)
            (my/migration-convert-roam-links-to-denote new-name)

            ;; Add extra keywords if needed
            (when extra-keywords
              (nm--add-org-file-tags extra-keywords))

            ;; Ensure target directory exists
            (let ((target-dir (file-name-directory new-name)))
              (unless (file-exists-p target-dir)
                (make-directory target-dir t)))

            ;; Write file
            (write-file new-name nil)
            (message "Migrated: %s → %s"
                    (file-name-nondirectory file)
                    (file-name-nondirectory new-name))
            t)))
    (error
     (message "ERROR migrating %s: %s" (org-roam-node-file node) err)
     (plist-put my/migration-stats :errors (1+ (plist-get my/migration-stats :errors)))
     nil)))

;; ============================================================================
;; MAIN MIGRATION FUNCTION
;; ============================================================================

;;;###autoload
(defun my/migration-run ()
  "Run the migration for all org-roam files.
Respects `my/migration-dry-run' setting."
  (interactive)
  (unless (and (boundp 'denote-directory) denote-directory)
    (error "denote-directory is not set. Please configure denote first"))

  (unless (file-exists-p (expand-file-name my/migration-source-dir))
    (error "Source directory does not exist: %s" my/migration-source-dir))

  ;; Reset statistics
  (setq my/migration-stats '(:total 0 :migrated 0 :skipped 0 :errors 0))

  (message "========================================")
  (message "org-roam to denote Migration")
  (message "========================================")
  (message "Mode: %s" (if my/migration-dry-run "DRY-RUN (no changes will be made)" "LIVE"))
  (message "Source: %s" my/migration-source-dir)
  (message "Target: %s" denote-directory)
  (message "========================================")

  ;; Get all org-roam files and nodes
  (let* ((all-files (org-roam-list-files))
         (all-nodes (delq nil (mapcar #'nm--roam-node-from-file all-files)))
         (total (length all-nodes)))

    (plist-put my/migration-stats :total total)
    (message "Found %d org-roam files to process\n" total)

    ;; Process each node
    (dolist (node all-nodes)
      (when (my/migration-migrate-single-node node)
        (plist-put my/migration-stats :migrated
                  (1+ (plist-get my/migration-stats :migrated)))))

    ;; Print summary
    (message "\n========================================")
    (message "Migration Summary")
    (message "========================================")
    (message "Total files:    %d" (plist-get my/migration-stats :total))
    (message "Migrated:       %d" (plist-get my/migration-stats :migrated))
    (message "Errors:         %d" (plist-get my/migration-stats :errors))
    (message "========================================")

    (if my/migration-dry-run
        (message "\nThis was a DRY-RUN. Set (setq my/migration-dry-run nil) to perform actual migration.")
      (message "\nMigration complete! Please verify the files in %s" denote-directory))))

;;;###autoload
(defun my/migration-toggle-dry-run ()
  "Toggle dry-run mode for migration."
  (interactive)
  (setq my/migration-dry-run (not my/migration-dry-run))
  (message "Migration dry-run mode: %s" (if my/migration-dry-run "ENABLED" "DISABLED")))

(provide 'org-roam-to-denote)
;;; org-roam-to-denote.el ends here
