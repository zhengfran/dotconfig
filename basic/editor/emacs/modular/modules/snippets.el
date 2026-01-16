;;; snippets.el --- Yasnippet and global snippet search -*- lexical-binding: t; -*-

;;; Commentary:
;; Yasnippet configuration plus comprehensive global snippet search system:
;; - System-wide snippet access via Raycast (Alt+Space -> type 'ss')
;; - Fuzzy search with consult integration
;; - Expression evaluation in snippets
;; - Clipboard integration
;; - Multi-monitor aware centered popup frames
;;
;; DEPENDENCIES: completion (consult-*), keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; YASNIPPET
;; ============================================================================

(use-package yasnippet
  :init
  (add-hook 'yas-minor-mode-hook (lambda()
(yas-activate-extra-mode 'fundamental-mode)))
  :config
  (setq yas-snippet-dirs (list (expand-file-name "~/org/snippets"))))

(yas-global-mode 1)

(use-package yasnippet-capf
  :init
  (defun my/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions    #'yasnippet-capf))
  (add-hook 'prog-mode-hook #'my/yasnippet-capf-h)
  (add-hook 'text-mode-hook #'my/yasnippet-capf-h))

(zzc/leader-keys
  "s"  '(:ignore t :which-key "snippet")
  "sc"  '(yas-new-snippet :which-key "create new snippet")
  "si"  '(yas-insert-snippet :which-key "insert snippet")
  "sy"  '(yasnippet-capf :which-key "complete snippet")
  "sn"  '(my/snippet-capture-form :which-key "new snippet (capture)"))

;; ============================================================================
;; SNIPPET CAPTURE SYSTEM
;; ============================================================================

(defvar my/snippet-mode-history nil
  "History list for snippet mode selection.
Remembers last-used mode for quick access when creating multiple snippets.")

(defun my/get-snippet-modes ()
  "Get list of existing snippet mode directories.
Scans ~/org/snippets/ and returns list of mode names.
Adds '+ Create new mode' option at the end.
Returns empty list with create option if directory doesn't exist."
  (let* ((snippets-dir (expand-file-name "~/org/snippets"))
         (modes '()))
    (when (file-directory-p snippets-dir)
      (dolist (file (directory-files snippets-dir t "^[^.]"))
        (when (file-directory-p file)
          (push (file-name-nondirectory file) modes))))
    (setq modes (sort modes #'string<))
    (append modes '("+ Create new mode"))))

(defun my/prompt-snippet-mode ()
  "Prompt user to select or create a snippet mode.
Uses completing-read with history to remember last-used mode.
If '+ Create new mode' selected, prompts for new mode name.
Validates mode name: suggests appending '-mode' suffix if missing.
Returns mode string (e.g., 'org-mode', 'python-mode')."
  (let* ((modes (my/get-snippet-modes))
         (selected (completing-read "Mode: "
                                   modes
                                   nil nil nil
                                   'my/snippet-mode-history)))
    (if (string= selected "+ Create new mode")
        ;; Create new mode
        (let ((new-mode (read-string "New mode name: ")))
          (when (string-empty-p new-mode)
            (error "Mode name cannot be empty"))
          (when (string-match-p "\\s-" new-mode)
            (error "Mode name cannot contain spaces"))
          ;; Suggest -mode suffix if missing
          (unless (string-suffix-p "-mode" new-mode)
            (when (y-or-n-p (format "Add '-mode' suffix? (%s-mode) " new-mode))
              (setq new-mode (concat new-mode "-mode"))))
          new-mode)
      ;; Return selected existing mode
      selected)))

(defun my/generate-snippet-filename (name)
  "Generate safe filename from snippet NAME.
Converts to lowercase and replaces spaces/special chars with underscores.
Example: 'Leetcode Notes!' -> 'leetcode_notes'
Returns filename string without extension."
  (let ((filename (downcase name)))
    ;; Replace spaces and special characters with underscores
    (setq filename (replace-regexp-in-string "[^a-z0-9_-]+" "_" filename))
    ;; Remove leading/trailing underscores
    (setq filename (replace-regexp-in-string "^_+\\|_+$" "" filename))
    ;; Collapse multiple underscores
    (setq filename (replace-regexp-in-string "_+" "_" filename))
    filename))

(defun my/snippet-file-exists-p (mode name)
  "Check if snippet with NAME already exists in MODE.
Returns t if file exists, nil otherwise.
Used for duplicate detection before creating new snippets."
  (let* ((snippets-dir (expand-file-name "~/org/snippets"))
         (mode-dir (expand-file-name mode snippets-dir))
         (filename (my/generate-snippet-filename name))
         (file-path (expand-file-name filename mode-dir)))
    (file-exists-p file-path)))

(defun my/write-snippet-file (mode name key content)
  "Write snippet to file in yasnippet format.
Creates MODE directory if it doesn't exist.
Generates filename from NAME.
Returns file path on success, signals error on failure.

Arguments:
  MODE - Mode directory name (e.g., 'org-mode')
  NAME - Display name for snippet
  KEY - Trigger key (can be empty string)
  CONTENT - Snippet body with placeholders"
  (let* ((snippets-dir (expand-file-name "~/org/snippets"))
         (mode-dir (expand-file-name mode snippets-dir))
         (filename (my/generate-snippet-filename name))
         (file-path (expand-file-name filename mode-dir)))
    
    ;; Create mode directory if doesn't exist
    (unless (file-directory-p mode-dir)
      (make-directory mode-dir t)
      (message "Created new mode directory: %s" mode-dir))
    
    ;; Write snippet file
    (with-temp-file file-path
      (insert "# -*- mode: snippet -*-\n")
      (insert (format "# name: %s\n" name))
      (when (and key (not (string-empty-p key)))
        (insert (format "# key: %s\n" key)))
      (insert "# --\n")
      (insert content))
    
    file-path))

(defun my/configure-capture-frame (frame)
  "Configure FRAME for snippet capture with multi-monitor aware centering.
Centers frame on primary monitor and applies styling.
Size: 120x50 characters (taller than search for multiline content).

Parameters:
  FRAME - The frame to configure (created by emacsclient -c)"
  (let* ((frame-width-chars 120)
         (frame-height-chars 50)
         
         ;; Multi-monitor aware centering on PRIMARY monitor
         (monitor-attrs (car (display-monitor-attributes-list)))
         (workarea (assq 'workarea monitor-attrs))
         (monitor-x (nth 1 workarea))
         (monitor-y (nth 2 workarea))
         (monitor-width (nth 3 workarea))
         (monitor-height (nth 4 workarea))
         
         ;; Calculate pixel dimensions
         (char-width (frame-char-width frame))
         (char-height (frame-char-height frame))
         (frame-pixel-width (* frame-width-chars char-width))
         (frame-pixel-height (* frame-height-chars char-height))
         
         ;; Center on primary monitor
         (left (+ monitor-x (max 0 (/ (- monitor-width frame-pixel-width) 2))))
         (top (+ monitor-y (max 0 (/ (- monitor-height frame-pixel-height) 2)))))
    
    ;; Apply frame configuration
    (set-frame-size frame frame-width-chars frame-height-chars)
    (set-frame-position frame left top)
    (set-frame-parameter frame 'name "Snippet Capture")
    (set-frame-parameter frame 'unsplittable t)
    (set-frame-parameter frame 'auto-raise t)
    (set-frame-parameter frame 'tool-bar-lines 0)
    (set-frame-parameter frame 'menu-bar-lines 0)
    (set-frame-parameter frame 'tab-bar-lines 0)
    (set-frame-parameter frame 'vertical-scroll-bars nil)))

;; Widget-based capture form
(require 'wid-edit)

(defvar-local my/snippet-mode-widget nil
  "Widget for mode selection in snippet capture form.")
(defvar-local my/snippet-name-widget nil
  "Widget for snippet name in capture form.")
(defvar-local my/snippet-key-widget nil
  "Widget for snippet key in capture form.")
(defvar-local my/snippet-content-widget nil
  "Widget for snippet content in capture form.")

(define-derived-mode snippet-capture-mode special-mode "Snippet-Capture"
  "Major mode for capturing snippets interactively.
\\{snippet-capture-mode-map}"
  (setq-local buffer-read-only nil))

(define-key snippet-capture-mode-map (kbd "C-c C-c") 'my/snippet-capture-finish)
(define-key snippet-capture-mode-map (kbd "C-c C-k") 'my/snippet-capture-cancel)

(defun my/snippet-capture-form ()
  "Open interactive form to create new snippet.
Displays widget-based form with fields for mode, name, key, and content.
Press C-c C-c to save, C-c C-k to cancel.
Can be called directly in Emacs (SPC s n) or via emacsclient frame."
  (interactive)
  (let ((buffer (get-buffer-create "*Snippet Capture*")))
    (with-current-buffer buffer
      (snippet-capture-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      
      ;; Header
      (widget-insert (propertize "═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════\n" 'face 'bold))
      (widget-insert (propertize "                                          Create New Snippet\n" 'face '(:height 1.2 :weight bold)))
      (widget-insert (propertize "                                  C-c C-c to save  |  C-c C-k to cancel\n" 'face 'italic))
      (widget-insert (propertize "═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════\n\n" 'face 'bold))
      
      ;; Mode field
      (widget-insert "Mode: ")
      (setq my/snippet-mode-widget
            (widget-create 'editable-field
                          :size 80
                          :format "%v"
                          :help-echo "Select mode (with TAB) or type to filter. Choose '+ Create new mode' to create new."
                          ""))
      (widget-insert "\n      (Press TAB to see available modes, or type to filter)\n\n")
      
      ;; Name field
      (widget-insert "Name: ")
      (setq my/snippet-name-widget
            (widget-create 'editable-field
                          :size 80
                          :format "%v"
                          :help-echo "Display name for snippet (e.g., 'Leetcode Notes')"
                          ""))
      (widget-insert "\n\n")
      
      ;; Key field
      (widget-insert "Key:  ")
      (setq my/snippet-key-widget
            (widget-create 'editable-field
                          :size 80
                          :format "%v"
                          :help-echo "Optional trigger key (e.g., '!lc' or '<date')"
                          ""))
      (widget-insert " (optional)\n\n")
      
      ;; Content field
      (widget-insert (propertize "Content:\n" 'face 'bold))
      (widget-insert "┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐\n")
      (setq my/snippet-content-widget
            (widget-create 'text
                          :size 25
                          :format "%v"
                          :help-echo "Snippet body. Use $0 for cursor, $1/$2 for tab stops, ${1:default} for defaults, `(elisp)` for expressions"
                          ""))
      (widget-insert "\n└─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘\n")
      (widget-insert "\n")
      (widget-insert (propertize "Placeholders: " 'face 'bold))
      (widget-insert "$0 (cursor) | $1, $2 (tab stops) | ${1:default} (with default) | `(elisp)` (evaluated)\n")
      
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    
    (switch-to-buffer buffer)))

(defun my/snippet-capture-finish ()
  "Finish snippet capture: validate, save, and close.
Extracts values from form widgets, validates required fields,
checks for duplicates, writes snippet file, reloads snippets,
and closes frame if called via emacsclient."
  (interactive)
  (condition-case err
      (let* ((mode-input (string-trim (widget-value my/snippet-mode-widget)))
             (name (string-trim (widget-value my/snippet-name-widget)))
             (key (string-trim (widget-value my/snippet-key-widget)))
             (content (widget-value my/snippet-content-widget))
             (mode nil))
        
        ;; Handle mode selection/creation
        (cond
         ;; Empty mode - need to prompt
         ((string-empty-p mode-input)
          (setq mode (my/prompt-snippet-mode)))
         ;; User wants to create new mode
         ((string= mode-input "+ Create new mode")
          (setq mode (my/prompt-snippet-mode)))
         ;; Mode provided
         (t
          (setq mode mode-input)))
        
        ;; Validate mode
        (when (string-empty-p mode)
          (error "Mode is required. Press C-c C-k to cancel"))
        
        ;; Validate name
        (when (string-empty-p name)
          (error "Snippet name is required. Press C-c C-k to cancel"))
        
        ;; Check for duplicate
        (when (my/snippet-file-exists-p mode name)
          (error "Snippet '%s' already exists in %s. Choose a different name or press C-c C-k to cancel" name mode))
        
        ;; Write snippet file
        (let ((file-path (my/write-snippet-file mode name key content)))
          ;; Reload snippets
          (yas-reload-all)
          
          ;; Success message
          (message "Snippet '%s' saved to %s" name file-path)
          
          ;; Close frame if this is an emacsclient frame
          (when (frame-parameter (selected-frame) 'client)
            (delete-frame (selected-frame)))
          
          ;; Kill buffer
          (kill-buffer "*Snippet Capture*")))
    
    (error
     (message "Snippet capture error: %s" (error-message-string err)))))

(defun my/snippet-capture-cancel ()
  "Cancel snippet capture and close frame.
Shows cancellation message, closes emacsclient frame if applicable,
and kills the capture buffer."
  (interactive)
  (message "Snippet capture cancelled")
  
  ;; Close frame if this is an emacsclient frame
  (when (frame-parameter (selected-frame) 'client)
    (delete-frame (selected-frame)))
  
  ;; Kill buffer
  (when (get-buffer "*Snippet Capture*")
    (kill-buffer "*Snippet Capture*")))

(defun my/global-snippet-capture ()
  "Global snippet capture with frame management for emacsclient.
This function is designed to be called via 'emacsclient -c' which creates
a frame. It configures that frame (centers on primary monitor, sets size)
and opens the snippet capture form.

Intended use: System-wide via Raycast (Alt+Space -> type 'sc')

The frame stays open until user presses C-c C-c (save) or C-c C-k (cancel)."
  (interactive)
  (let ((client-frame (selected-frame)))
    (condition-case err
        (progn
          ;; Configure the emacsclient frame
          (my/configure-capture-frame client-frame)
          
          ;; Open capture form
          (my/snippet-capture-form))
      
      ;; Handle errors
      (error
       (message "Snippet capture error: %s" (error-message-string err))
       ;; Close frame on error
       (when (frame-live-p client-frame)
         (delete-frame client-frame t))))))

;; ============================================================================
;; GLOBAL SNIPPET SEARCH ENGINE
;; ============================================================================

;;; Global Snippet Search Engine
;; System-wide snippet access via Raycast (Alt+Space -> 'ss')
;;
;; Architecture:
;;   1. Emacs server runs in background (configured at end of init.el)
;;   2. Raycast captures command ('ss' keyword)
;;   3. PowerShell script calls emacsclient to open popup frame (120x40, centered)
;;   4. Fuzzy search with consult/vertico/orderless
;;   5. Selected snippet copied to clipboard (yasnippet variables stripped)
;;   6. Frame auto-closes after selection or cancel
;;
;; Keybindings:
;;   SPC s g           - Test snippet search in Emacs
;;   Alt+Space -> 'ss' - Global snippet search (system-wide via Raycast)
;;
;; Snippet Format:
;;   ~/org/snippets/MODE-NAME/snippet-file
;;   Yasnippet format with # name:, # key:, # --
;;
;; Setup:
;;   1. This config enables Emacs server (see bottom of init.el)
;;   2. Install Raycast for Windows
;;   3. Configure Raycast script command with keyword 'ss'
;;   4. Script location: ~/dotconfig/scripts/emacs/emacs-snippet-search.ps1
;;
;; Implemented Features:
;;   1. Snippet search (SPC s g, or Raycast 'ss')
;;      - Fuzzy search with live preview
;;      - Expression evaluation and variable stripping
;;      - System-wide clipboard integration
;;
;;   2. Snippet capture (SPC s n, or Raycast 'sc')
;;      - Interactive form with mode/name/key/content fields
;;      - Mode history for quick access
;;      - Duplicate detection
;;      - Auto-reload after save
;;
;; Future Enhancements:
;;   - Snippet editing: C-c C-e from search to open file
;;   - Snippet deletion: Delete from search interface
;;   - Tag-based filtering and organization
;;   - Frequently-used snippets tracking
;;   - Cloud sync across machines (Git integration)

(defun my/evaluate-snippet-expressions (text)
  "Evaluate Emacs Lisp expressions in backticks and replace with results.

Searches for patterns like `(elisp-code)` and evaluates the code,
replacing the entire backtick expression with its string result.

This allows dynamic snippet content (dates, times, calculations) to work
when copied via global snippet search, matching yasnippet's normal behavior.

Examples:
  Input:  \"Today is `(format-time-string \\\"%Y-%m-%d\\\")`\"
  Output: \"Today is 2026-01-15\"

  Input:  \"`(+ 1 2)` plus `(* 3 4)` equals `(+ 3 12)`\"
  Output: \"3 plus 12 equals 15\"

If evaluation fails, replaces with error message:
  Input:  \"`(undefined-function)`\"
  Output: \"ERROR: (void-function undefined-function)\"

Note: Only evaluates expressions when snippet is SELECTED, not during
initial search loading, for optimal performance."
  (let ((result text))
    ;; Find all backtick expressions
    (while (string-match "`\\([^`]+\\)`" result)
      (let* ((expr-str (match-string 1 result))
             (value
              (condition-case err
                  ;; Try to evaluate the expression
                  (let* ((expr (read expr-str))
                         (eval-result (eval expr)))
                    ;; Convert result to string
                    (format "%s" eval-result))
                ;; If evaluation fails, show error
                (error
                 (format "ERROR: %s" (error-message-string err))))))
        ;; Replace the backtick expression with the value
        (setq result (replace-match value t t result))))
    result))

(defun my/strip-snippet-variables (text)
  "Remove yasnippet placeholder syntax from TEXT.
Replaces $N and ${N:default} patterns with ___ placeholder.

Note: Backtick expressions are NOT stripped by this function.
They should be evaluated first using `my/evaluate-snippet-expressions'.

Handles:
  $0, $1, $2, etc.          -> ___
  ${1:default}              -> ___
  ${2:$(elisp-expression)}  -> ___

Does NOT strip backtick expressions - those are evaluated separately.

Example:
  Input:  \"Hello $1, your ${2:item} is ready. $0\"
  Output: \"Hello ___, your ___ is ready. ___\""
  (let ((cleaned text))
    ;; Strip ${N:...} patterns (non-greedy to handle nested braces)
    (setq cleaned (replace-regexp-in-string "${[0-9]+:[^}]*}" "___" cleaned))
    ;; Strip $N simple placeholders
    (setq cleaned (replace-regexp-in-string "\\$[0-9]+" "___" cleaned))
    cleaned))

(defun my/parse-snippet-file (file-path)
  "Parse a single yasnippet file at FILE-PATH.
Returns plist with :name, :key, :content, :file.
Returns nil if file cannot be parsed or content is empty.

The parser looks for:
  # name: snippet-name
  # key: trigger-key
  # --
  [content after # --]

Content is stored RAW with backtick expressions and variables intact.
They will be evaluated/stripped when the snippet is selected, not during parsing."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        
        (let ((name nil)
              (key "")
              (content nil))
          
          ;; Extract # name: line
          (when (re-search-forward "^#\\s-*name:\\s-*\\(.+\\)$" nil t)
            (setq name (string-trim (match-string 1))))
          
          ;; If no name found, use filename
          (unless name
            (setq name (file-name-nondirectory file-path)))
          
          ;; Extract # key: line
          (goto-char (point-min))
          (when (re-search-forward "^#\\s-*key:\\s-*\\(.+\\)$" nil t)
            (setq key (string-trim (match-string 1))))
          
          ;; Find # -- separator and extract content after it
          (goto-char (point-min))
          (if (re-search-forward "^#\\s-*--\\s-*$" nil t)
              (progn
                (forward-line 1)
                (setq content (buffer-substring-no-properties (point) (point-max))))
            ;; No separator found, use entire buffer as content
            (setq content (buffer-string)))
          
          ;; Keep content RAW - don't evaluate or strip yet
          ;; That happens on selection for optimal performance
          (when content
            (setq content (string-trim content)))
          
          ;; Only return if we have content
          (if (and content (not (string-empty-p content)))
              (list :name name
                    :key key
                    :content content
                    :file file-path)
            (progn
              (message "Skipping empty snippet: %s" (file-name-nondirectory file-path))
              nil))))
    (error
     (message "Failed to parse snippet file %s: %s"
              (file-name-nondirectory file-path)
              (error-message-string err))
     nil)))

(defun my/collect-all-snippets ()
  "Collect and parse all snippets from ~/org/snippets/.
Returns list of snippet plists with :name, :key, :mode, :content, :file.

Scans all mode subdirectories (fundamental-mode/, org-mode/, etc.) and
parses each snippet file. Skips backup files (ending with ~) and hidden files.

Returns empty list if snippets directory doesn't exist or no snippets found."
  (let* ((snippets-dir (expand-file-name "~/org/snippets"))
         (snippets '()))
    
    (unless (file-directory-p snippets-dir)
      (error "Snippet directory not found: %s" snippets-dir))
    
    ;; Scan each mode subdirectory
    (dolist (mode-dir (directory-files snippets-dir t "^[^.]"))
      (when (file-directory-p mode-dir)
        (let ((mode-name (file-name-nondirectory mode-dir)))
          ;; Parse each snippet file in this mode directory
          (dolist (snippet-file (directory-files mode-dir t "^[^.]"))
            (when (and (file-regular-p snippet-file)
                      (not (string-suffix-p "~" snippet-file)))
              (let ((snippet (my/parse-snippet-file snippet-file)))
                (when snippet
                  ;; Add mode information
                  (plist-put snippet :mode mode-name)
                  (push snippet snippets))))))))
    
    (if (null snippets)
        (message "No snippets found in %s" snippets-dir)
      (message "Loaded %d snippets from %s" (length snippets) snippets-dir))
    
    (nreverse snippets)))

(defun my/consult-snippets ()
  "Fuzzy search for snippets with preview using consult.
Returns snippet content if selected, nil if cancelled (C-g or ESC).

Display format: [mode] name (key)
Preview shows full snippet content as you navigate.

Uses orderless fuzzy matching for flexible search.

When a snippet is selected, backtick expressions are evaluated and
yasnippet variables are stripped before returning the content."
  (let* ((snippets (my/collect-all-snippets))
         (candidates
          (mapcar (lambda (snippet)
                    (let* ((name (plist-get snippet :name))
                           (key (plist-get snippet :key))
                           (mode (plist-get snippet :mode))
                           (content (plist-get snippet :content))
                           ;; Display format: [mode] name (key)
                           (display (format "[%s] %s%s"
                                           mode
                                           name
                                           (if (string-empty-p key)
                                               ""
                                             (format " (%s)" key)))))
                      ;; Return cons: (display . content)
                      (cons display content)))
                  snippets)))
    (if (null candidates)
        (progn
          (message "No snippets found. Add snippets to ~/org/snippets/")
          nil)
      ;; Use consult with preview
      (let ((selected-content (consult--read candidates
                                             :prompt "Search snippets: "
                                             :lookup 'consult--lookup-cdr
                                             :sort nil
                                             :require-match t
                                             :category 'snippet
                                             :history 'my/snippet-search-history
                                             :preview-key 'any)))
        ;; Process selected snippet: evaluate expressions then strip variables
        (when selected-content
          ;; Step 1: Evaluate backtick expressions (e.g., `(format-time-string ...)`)
          (setq selected-content (my/evaluate-snippet-expressions selected-content))
          ;; Step 2: Strip yasnippet variables (e.g., $0, $1, ${2:default})
          (setq selected-content (my/strip-snippet-variables selected-content)))
        selected-content))))

(defun my/copy-to-clipboard (text)
  "Copy TEXT to Windows system clipboard using PowerShell.
Returns t on success, nil on failure.

Uses a temporary file to handle multi-line content and special characters
properly. Shows informative message with character count on success."
  (condition-case err
      (let ((temp-file (make-temp-file "emacs-clipboard-" nil ".txt")))
        (unwind-protect
            (progn
              ;; Write text to temp file (handles special characters)
              (with-temp-file temp-file
                (insert text))
              ;; Use PowerShell to set clipboard from file
              (shell-command
               (format "powershell.exe -command \"Get-Content '%s' -Raw | Set-Clipboard\""
                      temp-file))
              (message "Snippet copied! (%d chars) Paste with Ctrl+V" (length text))
              t)
          ;; Cleanup: always delete temp file
          (when (file-exists-p temp-file)
            (delete-file temp-file))))
    (error
     (message "Clipboard copy failed: %s" (error-message-string err))
     nil)))

(defun my/configure-snippet-frame (frame)
  "Configure FRAME for snippet search with multi-monitor aware centering.
Centers frame on primary monitor and applies styling (size, no toolbars, etc.).

This function handles:
- Multi-monitor setups: always centers on PRIMARY monitor
- Proper sizing (120x40 characters)
- Clean appearance (no toolbars, menu bars, scrollbars)
- Taskbar avoidance (uses workarea instead of full screen dimensions)

Parameters:
  FRAME - The frame to configure (created by emacsclient -c)"
  (let* ((frame-width-chars 120)
         (frame-height-chars 40)
         
         ;; Multi-monitor aware centering on PRIMARY monitor
         (monitor-attrs (car (display-monitor-attributes-list)))
         (workarea (assq 'workarea monitor-attrs))
         (monitor-x (nth 1 workarea))       ; Left edge of primary monitor
         (monitor-y (nth 2 workarea))       ; Top edge of primary monitor
         (monitor-width (nth 3 workarea))   ; Width (excluding taskbar)
         (monitor-height (nth 4 workarea))  ; Height (excluding taskbar)
         
         ;; Calculate pixel dimensions
         (char-width (frame-char-width frame))
         (char-height (frame-char-height frame))
         (frame-pixel-width (* frame-width-chars char-width))
         (frame-pixel-height (* frame-height-chars char-height))
         
         ;; Center on primary monitor
         (left (+ monitor-x (max 0 (/ (- monitor-width frame-pixel-width) 2))))
         (top (+ monitor-y (max 0 (/ (- monitor-height frame-pixel-height) 2)))))
    
    ;; Apply frame configuration
    (set-frame-size frame frame-width-chars frame-height-chars)
    (set-frame-position frame left top)
    (set-frame-parameter frame 'name "Snippet Search")
    (set-frame-parameter frame 'unsplittable t)
    (set-frame-parameter frame 'auto-raise t)
    (set-frame-parameter frame 'tool-bar-lines 0)
    (set-frame-parameter frame 'menu-bar-lines 0)
    (set-frame-parameter frame 'tab-bar-lines 0)
    (set-frame-parameter frame 'vertical-scroll-bars nil)))

(defun my/consult-snippets-inline ()
  "Search and copy snippet in current window (no frame creation).
Opens fuzzy search interface in the current Emacs window, allows
selection of a snippet, and copies it to the system clipboard.

Designed for use within Emacs via 'SPC s g' keybinding.

Workflow:
  1. Opens consult fuzzy search in current window
  2. Shows live preview of snippet content
  3. On selection: evaluates expressions, strips variables, copies to clipboard
  4. On cancel (C-g): shows cancellation message

No frame management - runs in current Emacs window/frame."
  (interactive)
  (condition-case err
      (let ((content (my/consult-snippets)))
        (if content
            (progn
              (my/copy-to-clipboard content)
              (message "Snippet copied to clipboard"))
          (message "Snippet search cancelled")))
    (error
     (message "Snippet search error: %s" (error-message-string err)))))

(defun my/global-snippet-search ()
  "Global snippet search with frame management for emacsclient.
This function is designed to be called via 'emacsclient -c' which creates
a frame. It configures that frame (centers on primary monitor, sets size),
runs snippet search, and closes the frame after selection.

Intended use: System-wide via Raycast (Alt+Space -> type 'ss')

Workflow:
  1. Configures the emacsclient-created frame (centers on primary monitor)
  2. Opens fuzzy search interface with all snippets
  3. Shows live preview of snippet content
  4. On selection: evaluates expressions, strips variables, copies to clipboard
  5. Closes frame (whether selection succeeded, cancelled, or errored)

The frame ALWAYS closes after the search completes, ensuring no leftover
frames remain on screen. Focus returns to the previously active application."
  (interactive)
  (let ((client-frame (selected-frame)))
    (unwind-protect
        (condition-case err
            (progn
              ;; Step 1: Configure the emacsclient frame
              (my/configure-snippet-frame client-frame)
              
              ;; Step 2: Run consult search with preview
              (let ((content (my/consult-snippets)))
                
                ;; Step 3: Copy to clipboard if snippet was selected
                (if content
                    (my/copy-to-clipboard content)
                  (message "Snippet search cancelled"))))
          
          ;; Handle errors gracefully
          (error
           (message "Snippet search error: %s" (error-message-string err))))
      
      ;; Cleanup: ALWAYS close the frame
      (when (frame-live-p client-frame)
        (delete-frame client-frame t)))))

;; Add keybinding: SPC s g for inline snippet search (no frame creation)
(zzc/leader-keys
  "sg" '(my/consult-snippets-inline :which-key "snippet to clipboard"))

;; ============================================================================
;; EMACS SERVER FOR GLOBAL SNIPPET ACCESS
;; ============================================================================

;;; Emacs Server for Global Snippet Access
;; Enables emacsclient to connect for system-wide snippet search
;; Required for Raycast integration (Alt+Space -> type 'ss')
(require 'server)
(unless (server-running-p)
  (server-start)
  (message "Emacs server started for snippet engine"))

(provide 'snippets)
;;; snippets.el ends here
