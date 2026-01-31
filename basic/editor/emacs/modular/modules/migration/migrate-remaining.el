;;; migrate-remaining.el --- Migrate remaining non-standard files -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple migration for files that don't match standard org-roam naming.
;; Uses file modification time as denote ID.

;;; Code:

(require 'denote)

(defun migrate-remaining-file (file)
  "Migrate a single non-denote FILE using its modification time."
  (let* ((mtime (file-attribute-modification-time (file-attributes file)))
         (id (format-time-string "%Y%m%dT%H%M%S" mtime))
         (basename (file-name-sans-extension (file-name-nondirectory file)))
         (title (replace-regexp-in-string "^[0-9]+-" "" basename))
         (title (replace-regexp-in-string "_" " " title))
         (keywords '())
         (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))

    ;; Extract keywords from #+filetags if present
    (when (string-match "^#\\+filetags:\\s-*\\(.+\\)$" content)
      (setq keywords (split-string (match-string 1 content) ":" t)))

    ;; Clean keywords
    (setq keywords (mapcar #'downcase (delete-dups keywords)))

    ;; Generate new filename
    (let* ((slug (denote-sluggify-title title))
           (keyword-str (if keywords
                           (concat "__" (string-join keywords "_"))
                         ""))
           (new-name (concat id "--" slug keyword-str ".org"))
           (new-path (expand-file-name new-name denote-directory)))

      (message "Migrating: %s → %s" (file-name-nondirectory file) new-name)

      ;; Write file with denote format
      (with-temp-buffer
        (insert content)
        (write-file new-path nil))

      ;; Delete original
      (delete-file file)
      t)))

(defun migrate-all-remaining ()
  "Migrate all non-denote org files in denote-directory."
  (interactive)
  (let* ((all-files (directory-files denote-directory t "\\.org$"))
         (non-denote (cl-remove-if
                     (lambda (f)
                       (or (string-match-p "--.*\\.org$" f)  ; Already denote format
                           (file-directory-p f)))             ; Directory
                     all-files))
         (migrated 0)
         (failed 0))

    (message "Found %d files to migrate" (length non-denote))

    (dolist (file non-denote)
      (condition-case err
          (progn
            (migrate-remaining-file file)
            (setq migrated (1+ migrated)))
        (error
         (message "ERROR: %s - %s" file err)
         (setq failed (1+ failed)))))

    (message "\nMigration complete!")
    (message "  Migrated: %d" migrated)
    (message "  Failed:   %d" failed)))

(provide 'migrate-remaining)
;;; migrate-remaining.el ends here
