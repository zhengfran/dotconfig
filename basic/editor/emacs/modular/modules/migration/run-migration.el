;;; run-migration.el --- Execute org-roam to denote migration -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone script to run the org-roam to denote migration.
;; This script:
;; 1. Loads the old org-roam configuration
;; 2. Loads denote
;; 3. Runs the migration script
;; 4. Reports results
;;
;; Usage:
;;   emacs --batch -l run-migration.el -f run-migration-interactive
;;   OR from within Emacs: M-x load-file RET run-migration.el RET M-x run-migration-interactive

;;; Code:

(defvar migration-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing migration scripts.")

(defun run-migration-setup ()
  "Set up environment for migration."
  (message "=== Migration Setup ===")

  ;; Add modules to load path
  (let ((modules-dir (expand-file-name ".." migration-root-dir)))
    (add-to-list 'load-path modules-dir))

  ;; Load straight.el if not already loaded
  (unless (featurep 'straight)
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (error "straight.el not found. Please run Emacs normally first to install packages."))
      (load bootstrap-file nil 'nomessage)))

  ;; Ensure use-package is available
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)

  (message "✓ Package system initialized"))

(defun run-migration-load-org-roam ()
  "Load org-roam temporarily for migration."
  (message "=== Loading org-roam (temporary) ===")

  ;; Set org-roam directory
  (setq my/org-base-dir (expand-file-name "~/org/notes/"))

  ;; Load org-roam
  (straight-use-package 'emacsql)
  (require 'emacsql-sqlite)
  (straight-use-package 'org-roam)
  (require 'org-roam)

  ;; Configure org-roam
  (setq org-roam-directory my/org-base-dir)
  (setq org-roam-dailies-directory "daily/")

  ;; Initialize org-roam database
  (message "Initializing org-roam database...")
  (org-roam-db-autosync-mode)

  (message "✓ org-roam loaded with %d nodes"
           (caar (org-roam-db-query "SELECT COUNT(*) FROM nodes"))))

(defun run-migration-load-denote ()
  "Load denote for migration target."
  (message "=== Loading denote ===")

  (straight-use-package 'denote)
  (require 'denote)

  ;; Configure denote
  (setq denote-directory my/org-base-dir)
  (setq denote-known-keywords '("project" "journal" "trade" "ref" "archived"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'org)

  (message "✓ denote loaded, directory: %s" denote-directory))

(defun run-migration-execute (&optional dry-run)
  "Execute the migration. If DRY-RUN is non-nil, only preview changes."
  (message "=== Loading Migration Script ===")

  ;; Load migration script
  (let ((migration-script (expand-file-name "org-roam-to-denote.el" migration-root-dir)))
    (load-file migration-script))

  (message "✓ Migration script loaded")

  ;; Set dry-run mode
  (setq my/migration-dry-run dry-run)

  (message "\n========================================")
  (if dry-run
      (message "DRY-RUN MODE: No files will be modified")
    (message "LIVE MODE: Files will be migrated"))
  (message "========================================\n")

  ;; Run migration
  (my/migration-run)

  (message "\n✓ Migration %s" (if dry-run "preview complete" "complete")))

;;;###autoload
(defun run-migration-interactive ()
  "Run migration interactively with user confirmation."
  (interactive)

  (message "\n")
  (message "╔═══════════════════════════════════════════════════╗")
  (message "║   org-roam → denote Migration                     ║")
  (message "╚═══════════════════════════════════════════════════╝")
  (message "\n")

  ;; Setup
  (run-migration-setup)
  (run-migration-load-org-roam)
  (run-migration-load-denote)

  (message "\nReady to migrate!")
  (message "  Source: %s" org-roam-directory)
  (message "  Target: %s" denote-directory)
  (message "  Total org-roam nodes: %d"
           (caar (org-roam-db-query "SELECT COUNT(*) FROM nodes")))

  ;; First, always do dry-run
  (message "\n" "Step 1: DRY-RUN to preview changes...")
  (sit-for 2)
  (run-migration-execute t)

  ;; Ask user if they want to proceed
  (message "\n")
  (when (yes-or-no-p "Dry-run complete. Proceed with actual migration? ")
    (message "\nStep 2: LIVE MIGRATION...")
    (sit-for 1)
    (run-migration-execute nil)
    (message "\n✓✓✓ Migration complete! ✓✓✓")
    (message "\nNext steps:")
    (message "  1. Review migrated files in %s" denote-directory)
    (message "  2. Restart Emacs to load denote-config")
    (message "  3. Run: M-x my/denote-refresh-agenda-list")))

;; For batch mode
;;;###autoload
(defun run-migration-batch ()
  "Run migration in batch mode (non-interactive)."
  (run-migration-setup)
  (run-migration-load-org-roam)
  (run-migration-load-denote)
  (run-migration-execute t)  ; Dry-run first
  (when (getenv "MIGRATION_CONFIRM")
    (run-migration-execute nil)))

(provide 'run-migration)
;;; run-migration.el ends here
