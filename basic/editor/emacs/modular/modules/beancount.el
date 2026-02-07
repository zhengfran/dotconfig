;;; beancount.el --- Plain-text accounting with beancount -*- lexical-binding: t; -*-

;;; Commentary:
;; Beancount plain-text accounting integration for Emacs.
;; Provides syntax highlighting, completion, validation, and fava web UI integration.
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys)
;; USED BY: None
;;
;; External dependencies (install via pip):
;;   pip install beancount fava

;;; Code:

;; ============================================================================
;; PATH CONFIGURATION
;; ============================================================================

(defvar my/beancount-dir (expand-file-name "~/org/finance/")
  "Directory for beancount ledger files.")

(defvar my/beancount-main-file (expand-file-name "main.beancount" my/beancount-dir)
  "Path to main beancount ledger file.")

(defvar my/fava-port 5000
  "Port for fava web server.")

(defvar my/beancount-format-column 60
  "Column to align amounts when formatting beancount files.")

;; Create finance directory if it doesn't exist
(unless (file-exists-p my/beancount-dir)
  (make-directory my/beancount-dir t))

;; ============================================================================
;; BEANCOUNT MODE
;; ============================================================================

(use-package beancount
  :straight (beancount :type git :host github :repo "beancount/beancount-mode")
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.bean\\'" . beancount-mode))
  :hook (beancount-mode . outline-minor-mode)
  :custom
  (beancount-number-alignment-column 52)
  :config
  ;; Enable completion
  (add-hook 'beancount-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'beancount-completion-at-point)))))

;; ============================================================================
;; FAVA INTEGRATION
;; ============================================================================

(defvar my/fava-process nil
  "Process handle for running fava server.")

(defun my/fava-start (&optional file)
  "Start fava web server for FILE (defaults to main ledger)."
  (interactive)
  (let ((ledger-file (or file my/beancount-main-file)))
    (if (not (file-exists-p ledger-file))
        (message "Ledger file not found: %s" ledger-file)
      ;; Kill existing fava process if running
      (when (and my/fava-process (process-live-p my/fava-process))
        (kill-process my/fava-process))
      ;; Start new fava process
      (setq my/fava-process
            (start-process "fava" "*fava*" "fava"
                           "-p" (number-to-string my/fava-port)
                           ledger-file))
      (message "Fava started on http://localhost:%d" my/fava-port))))

(defun my/fava-open ()
  "Open fava web UI in browser, starting server if needed."
  (interactive)
  (unless (and my/fava-process (process-live-p my/fava-process))
    (my/fava-start))
  ;; Wait briefly for server to start
  (run-at-time 1 nil
               (lambda ()
                 (browse-url (format "http://localhost:%d" my/fava-port)))))

(defun my/fava-stop ()
  "Stop fava web server."
  (interactive)
  (when (and my/fava-process (process-live-p my/fava-process))
    (kill-process my/fava-process)
    (setq my/fava-process nil)
    (message "Fava stopped")))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(defun my/beancount-open-main ()
  "Open the main beancount ledger file."
  (interactive)
  (if (file-exists-p my/beancount-main-file)
      (find-file my/beancount-main-file)
    (when (y-or-n-p (format "Create %s? " my/beancount-main-file))
      (find-file my/beancount-main-file)
      (insert ";; -*- mode: beancount -*-\n")
      (insert ";; Main Beancount Ledger\n\n")
      (insert "option \"title\" \"Personal Ledger\"\n")
      (insert "option \"operating_currency\" \"CNY\"\n\n")
      (insert ";; Account definitions\n")
      (insert ";; 资产账户\n")
      (insert "1970-01-01 open Assets:Bank:Checking\n")
      (insert "1970-01-01 open Assets:Cash\n\n")
      (insert ";; 负债账户\n")
      (insert "1970-01-01 open Liabilities:CreditCard\n\n")
      (insert ";; 收入账户\n")
      (insert "1970-01-01 open Income:Salary\n\n")
      (insert ";; 支出账户\n")
      (insert "1970-01-01 open Expenses:Food\n")
      (insert "1970-01-01 open Expenses:Transport\n")
      (insert "1970-01-01 open Expenses:Shopping\n\n")
      (insert ";; 权益账户\n")
      (insert "1970-01-01 open Equity:Opening-Balances\n\n")
      (insert ";; Transactions below\n\n")
      (save-buffer))))

(defun my/beancount-check ()
  "Run bean-check on the current beancount file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (string-match-p "\\.beancount\\'" file))
        (compile (format "bean-check %s" (shell-quote-argument file)))
      (message "Not a beancount file"))))

(defun my/beancount-insert-transaction ()
  "Insert a transaction template at point."
  (interactive)
  (let ((date (format-time-string "%Y-%m-%d")))
    (insert (format "%s * \"\" \"\"\n  Expenses:  CNY\n  Assets:Bank:Checking\n" date))
    (forward-line -2)
    (search-forward "\"\"" nil t)
    (backward-char 1)))

;; ============================================================================
;; FORMATTING
;; ============================================================================

(defun my/beancount-format-buffer ()
  "Format the current beancount buffer using bean-format."
  (interactive)
  (when (eq major-mode 'beancount-mode)
    (let* ((point-before (point))
           (scroll-before (window-start))
           (temp-file (make-temp-file "beancount-" nil ".beancount"))
           (content (buffer-string)))
      (unwind-protect
          (progn
            ;; Write buffer content to temp file
            (with-temp-file temp-file
              (insert content))
            ;; Run bean-format on temp file
            (let ((exit-code (call-process "bean-format" nil nil nil
                                           "-c" (number-to-string my/beancount-format-column)
                                           "-o" temp-file
                                           temp-file)))
              (when (= exit-code 0)
                ;; Read formatted content back
                (erase-buffer)
                (insert-file-contents temp-file)
                (goto-char (min point-before (point-max)))
                (set-window-start nil scroll-before))))
        ;; Cleanup temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(defun my/beancount-format-on-save ()
  "Format beancount buffer before saving."
  (when (eq major-mode 'beancount-mode)
    (my/beancount-format-buffer)))

;; Enable format on save for beancount files
(add-hook 'beancount-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/beancount-format-on-save nil t)))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(zzc/leader-keys
  "F"   '(:ignore t :which-key "finance")
  "F f" '(my/beancount-open-main :which-key "open ledger")
  "F v" '(my/fava-open :which-key "fava web UI")
  "F V" '(my/fava-stop :which-key "stop fava")
  "F c" '(my/beancount-check :which-key "check ledger")
  "F t" '(my/beancount-insert-transaction :which-key "insert transaction")
  "F =" '(my/beancount-format-buffer :which-key "format buffer"))

(provide 'beancount)
;;; beancount.el ends here
