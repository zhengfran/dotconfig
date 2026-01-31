;;; batch-migrate.el --- Batch migration from org-roam to denote -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone batch script to migrate org-roam to denote.
;; Run with: emacs --batch -l batch-migrate.el

;;; Code:

(message "========================================")
(message "org-roam → denote Batch Migration")
(message "========================================\n")

;; Set up paths
(defvar user-emacs-directory
  (expand-file-name "~/dotconfig/basic/editor/emacs/modular/")
  "Emacs configuration directory.")

(message "Step 1: Setting up package system...")

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (error "straight.el not found at %s" bootstrap-file))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(message "✓ Package system initialized\n")

;; Set org-roam directory
(setq my/org-base-dir (expand-file-name "~/org/notes/"))

(message "Step 2: Loading org-roam (temporarily)...")

;; Load org-roam
(straight-use-package 'emacsql)
(require 'emacsql-sqlite)
(straight-use-package 'org-roam)
(require 'org-roam)

;; Configure org-roam
(setq org-roam-directory my/org-base-dir)
(setq org-roam-dailies-directory "daily/")
(setq org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory))

;; Initialize database
(org-roam-db-autosync-mode)

(let ((node-count (caar (org-roam-db-query "SELECT COUNT(*) FROM nodes"))))
  (message "✓ org-roam loaded with %d nodes\n" node-count))

(message "Step 3: Loading denote...")

;; Load denote
(straight-use-package 'denote)
(require 'denote)

;; Configure denote
(setq denote-directory my/org-base-dir)
(setq denote-known-keywords '("project" "journal" "trade" "ref" "archived"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'org)

(message "✓ denote loaded, directory: %s\n" denote-directory)

(message "Step 4: Loading migration script...")

;; Load migration script
(let ((migration-script (expand-file-name "modules/migration/org-roam-to-denote.el" user-emacs-directory)))
  (load-file migration-script))

(message "✓ Migration script loaded\n")

;; First: DRY-RUN
(message "========================================")
(message "PHASE 1: DRY-RUN (Preview Only)")
(message "========================================\n")

(setq my/migration-dry-run t)
(my/migration-run)

(message "\n========================================")
(message "PHASE 2: LIVE MIGRATION")
(message "========================================\n")
(message "Starting actual file migration...")
(sit-for 2)

(setq my/migration-dry-run nil)
(my/migration-run)

(message "\n========================================")
(message "✓✓✓ Migration Complete! ✓✓✓")
(message "========================================\n")

(message "Summary:")
(message "  Total files: %d" (plist-get my/migration-stats :total))
(message "  Migrated:    %d" (plist-get my/migration-stats :migrated))
(message "  Errors:      %d" (plist-get my/migration-stats :errors))

(message "\nNext steps:")
(message "  1. Check migrated files in %s" denote-directory)
(message "  2. Review journal/ directory for daily notes")
(message "  3. Verify project files in root with __project keyword")
(message "  4. init.el will be updated to load denote-config")

(message "\n✓ Batch migration completed successfully!")

;;; batch-migrate.el ends here
