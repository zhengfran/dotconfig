;;; bookmarks.el --- Bookmark management and URL bookmarks -*- lexical-binding: t; -*-

;;; Commentary:
;; Standard Emacs bookmarks plus custom URL bookmark system:
;; - Parse bookmarks from ~/org/bookmarks.org
;; - Fuzzy search with consult integration
;; - System-wide access via Raycast (Alt+Space -> 'bs' search, 'bc' capture)
;; - Centered popup frame for selection (100x30 for search, 80x20 for capture)
;; - URL capture with clipboard detection and duplicate checking
;; - Windows browser integration via w32-shell-execute
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys), completion (consult-*)
;; USED BY: None

;;; Code:

;; ============================================================================
;; STANDARD BOOKMARKS
;; ============================================================================

;; save bookmark on change
(setq bookmark-save-flag 1)
(require 'bookmark)
;; set bookmark file to sync across difference device
(setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))

(zzc/leader-keys
  "bm"  '(:ignore t :which-key "bookmark")
  "bmm"  '(bookmark-set :which-key "Add current file/dir to bookmark")
  "bml"  '(consult-bookmark :which-key "Open Bookmark List"))

;; ============================================================================
;; URL BOOKMARK SEARCH SYSTEM
;; ============================================================================

;; Custom Bookmark Search - URL Launcher
;; Bookmark search in popup frame with fuzzy matching
;; Opens org-mode bookmarks in Windows default browser
;; Keybinding: SPC b u

(defun my/parse-org-bookmarks ()
  "Extract HTTP(S) links from ~/org/bookmarks.org.
Returns alist of (DESCRIPTION . URL) pairs.

Description priority:
  1. Explicit [Description] in link syntax
  2. Nearest org heading text (preserves tags)
  3. URL itself (fallback)

Only includes HTTP/HTTPS URLs, skips file: and elisp: links.

Examples:
  ** GitHub
  [[https://github.com]]
  → Description: \"GitHub\"

  ** GitHub :work:
  [[https://github.com][My Account]]
  → Description: \"My Account\" (explicit priority)

  ** Development :tag:
  [[https://github.com]]
  → Description: \"Development :tag:\" (tags preserved)"
  (let ((bookmark-file (expand-file-name "~/org/bookmarks.org")))
    ;; Check file exists
    (unless (file-exists-p bookmark-file)
      (error "Bookmark file not found: %s" bookmark-file))
    
    (with-temp-buffer
      (insert-file-contents bookmark-file)
      (goto-char (point-min))
      (let (bookmarks)
        ;; Find all HTTP(S) links
        (while (re-search-forward
                "\\[\\[\\(https?://[^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
                nil t)
          (let ((url (match-string 1))
                (explicit-desc (match-string 2))
                (heading-desc nil))
            
            ;; If no explicit description, search backwards for nearest heading
            (unless explicit-desc
              (save-excursion
                (beginning-of-line)
                ;; Find nearest heading at any level (*, **, ***, etc.)
                ;; Captures heading text including any org tags (:tag:)
                (when (re-search-backward "^\\*+\\s-+\\(.+\\)$" nil t)
                  (setq heading-desc (string-trim (match-string 1))))))
            
            ;; Priority: explicit > heading > URL
            (let ((desc (or explicit-desc heading-desc url)))
              (push (cons desc url) bookmarks))))
        
        ;; Error if no bookmarks found
        (unless bookmarks
          (error "No HTTP(S) bookmarks found in %s" bookmark-file))
        
        (nreverse bookmarks)))))

(defun my/consult-bookmarks ()
  "Fuzzy search bookmarks from ~/org/bookmarks.org.
Returns selected URL or nil if cancelled."
  (interactive)
  (let* ((bookmarks (my/parse-org-bookmarks))
         (selected (consult--read
                    bookmarks
                    :prompt "Bookmark: "
                    :sort nil                           ; Keep org file order
                    :require-match t                    ; Must select valid bookmark
                    :category 'bookmark                 ; For completion annotations
                    :lookup #'consult--lookup-cdr       ; Return URL (cdr), not description
                    :history 'my/bookmark-history)))    ; Remember recent selections
    selected))

(defun my/open-url-in-browser (url)
  "Open URL in Windows default browser.
Uses w32-shell-execute for native Windows."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute "open" url)
    (error "Not running on native Windows. System type: %s" system-type)))

(defun my/bookmark-search-inline ()
  "Search and open bookmark in current window (no frame creation).
Opens fuzzy search interface in the current Emacs window, allows
selection of a bookmark, and opens it in the default browser.

Designed for use within Emacs via 'SPC b u' keybinding.

Workflow:
  1. Opens consult fuzzy search in current window
  2. Shows bookmark descriptions with fuzzy matching
  3. On selection: opens URL in default browser
  4. On cancel (C-g): shows cancellation message

No frame management - runs in current Emacs window/frame."
  (interactive)
  (condition-case err
      (let ((url (my/consult-bookmarks)))
        (if url
            (progn
              (my/open-url-in-browser url)
              (message "Opened: %s" url))
          (message "Bookmark search cancelled")))
    (error
     (message "Bookmark search error: %s" (error-message-string err)))))

(defun my/configure-bookmark-search-frame (frame)
  "Configure FRAME for bookmark search with multi-monitor aware centering.
Size: 100x30 characters (optimized for bookmark descriptions).
Centers on primary monitor, clean appearance.

Parameters:
  FRAME - The frame to configure (created by emacsclient -c)"
  (let* ((frame-width-chars 100)
         (frame-height-chars 30)
         
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
    (set-frame-parameter frame 'name "Bookmark Search")
    (set-frame-parameter frame 'unsplittable t)
    (set-frame-parameter frame 'auto-raise t)
    (set-frame-parameter frame 'tool-bar-lines 0)
    (set-frame-parameter frame 'menu-bar-lines 0)
    (set-frame-parameter frame 'tab-bar-lines 0)
    (set-frame-parameter frame 'vertical-scroll-bars nil)))

(defun my/global-bookmark-search ()
  "Global bookmark search with frame management for emacsclient.
This function is designed to be called via 'emacsclient -c' which creates
a frame. It configures that frame (centers on primary monitor, sets size),
runs bookmark search, and closes the frame after selection.

Intended use: System-wide via Raycast (Alt+Space -> type 'bs')

Workflow:
  1. Configures emacsclient-created frame (100x30, centered)
  2. Opens fuzzy search interface with all bookmarks
  3. Shows bookmark descriptions with fuzzy matching
  4. On selection: opens URL in default browser, closes frame
  5. On cancel (C-g): closes frame
  
The frame ALWAYS closes after search completes, ensuring no leftover
frames remain on screen. Focus returns to the previously active application."
  (interactive)
  (let ((client-frame (selected-frame)))
    (unwind-protect
        (condition-case err
            (progn
              ;; Step 1: Configure the emacsclient frame
              (my/configure-bookmark-search-frame client-frame)
              
              ;; Step 2: Run consult bookmark search
              (let ((url (my/consult-bookmarks)))
                
                ;; Step 3: Open URL if selected (nil if C-g cancelled)
                (when url
                  (my/open-url-in-browser url)
                  (message "Opened: %s" url))))
          
          ;; Handle errors gracefully
          (error
           (message "Bookmark search error: %s" (error-message-string err))))
      
      ;; Cleanup: ALWAYS close the frame
      (when (frame-live-p client-frame)
        (delete-frame client-frame t)))))

;; ============================================================================
;; URL BOOKMARK CAPTURE SYSTEM
;; ============================================================================

;;; URL Bookmark Capture System
;; Keybinding: SPC b c (bookmark → capture)
;; Intelligently captures URLs to ~/org/bookmarks.org under Inbox heading

(defun my/get-clipboard-content ()
  "Get clipboard content on Windows using PowerShell.
Returns trimmed string or nil if clipboard is empty/inaccessible."
  (condition-case nil
      (let ((content (string-trim
                      (shell-command-to-string "powershell.exe -command \"Get-Clipboard\""))))
        (if (string-empty-p content) nil content))
    (error nil)))

(defun my/is-valid-url-p (text)
  "Check if TEXT is a valid HTTP(S) URL.
Basic format check: must start with http:// or https:// and have no whitespace.
Case-sensitive check."
  (and (stringp text)
       (not (string-empty-p text))
       (string-match-p "^https?://[^\s]+$" text)))

(defun my/find-duplicate-bookmark (url)
  "Search ~/org/bookmarks.org for duplicate URL.
Returns (LINE-NUM . HEADING-TEXT) if URL exists, nil otherwise.
Case-sensitive URL comparison."
  (let ((org-file (expand-file-name "~/org/bookmarks.org"))
        (duplicate-info nil))
    (when (file-exists-p org-file)
      (with-temp-buffer
        (insert-file-contents org-file)
        (goto-char (point-min))
        ;; Search for exact [[URL]] pattern (case-sensitive)
        (when (search-forward (format "[[%s]]" url) nil t)
          (let ((line-num (line-number-at-pos))
                (heading-text nil))
            ;; Find nearest heading above the URL
            (save-excursion
              (when (re-search-backward "^\\*+\\s-+\\(.+\\)$" nil t)
                (setq heading-text (string-trim (match-string 1)))))
            (setq duplicate-info (cons line-num heading-text))))))
    duplicate-info))

(defun my/extract-domain-from-url (url)
  "Extract domain name from URL as description suggestion.
Example: https://github.com/user/repo → github.com
Returns domain string or nil if extraction fails."
  (when (string-match "^https?://\\([^/]+\\)" url)
    (match-string 1 url)))

(defun my/append-bookmark-to-inbox (url description)
  "Append bookmark entry to Inbox heading in ~/org/bookmarks.org.
Creates file with proper structure if it doesn't exist.
Creates Inbox heading if missing.
Format: ** DESCRIPTION\\n[[URL]]\\n

Inserts at end of Inbox section (before next top-level heading or EOF)."
  (let ((org-file (expand-file-name "~/org/bookmarks.org")))
    
    ;; Ensure file exists with proper structure
    (unless (file-exists-p org-file)
      (with-temp-file org-file
        (insert "#+TITLE: Linkmarks Bookmarks\n")
        (insert "#+DESCRIPTION: Emacs Linkmarks - Org-mode based bookmarks\n\n")
        (insert "* Inbox\n\n")))
    
    ;; Append bookmark under Inbox
    (with-current-buffer (find-file-noselect org-file)
      (save-excursion
        (goto-char (point-min))
        
        ;; Find or create Inbox heading
        (unless (re-search-forward "^\\* Inbox$" nil t)
          (goto-char (point-min))
          ;; Skip file headers
          (when (re-search-forward "^#\\+.*$" nil t)
            (forward-line 1))
          (insert "\n* Inbox\n"))
        
        ;; Move to end of Inbox section (before next top-level heading or EOF)
        (forward-line 1)
        (if (re-search-forward "^\\* [^*]" nil t)
            (progn
              (beginning-of-line)
              (insert "\n"))
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))
        
        ;; Insert bookmark entry
        (insert (format "** %s\n[[%s]]\n" description url)))
      
      ;; Save file
      (save-buffer))))

(defun my/capture-url-bookmark ()
  "Capture URL bookmark to ~/org/bookmarks.org under Inbox heading.

Workflow:
1. Check clipboard for HTTP(S) URL
   - If valid URL found: prompt for description only
   - If no URL: prompt for URL first, then description
2. Validate URL format (must start with http:// or https://)
3. Check for duplicate URL (case-sensitive)
   - If duplicate exists: show error with location and abort
4. Extract domain name from URL as default description suggestion
5. Prompt for description with domain as default
6. Append to Inbox as subheading: ** DESCRIPTION\\n[[URL]]
7. Show success message with description and URL

Error handling:
- User cancels (C-g): Show \"Bookmark capture cancelled\"
- Invalid URL format: Show error and abort
- Duplicate URL: Show existing location and abort
- Empty description: Show error and abort"
  (interactive)
  (condition-case err
      (let* ((clipboard (my/get-clipboard-content))
             (clipboard-is-url (my/is-valid-url-p clipboard))
             (url nil)
             (description nil)
             (domain-suggestion nil))
        
        ;; Step 1: Get URL (from clipboard or prompt)
        (if clipboard-is-url
            (setq url clipboard)
          ;; Prompt for URL
          (setq url (read-string "URL: "))
          (unless (my/is-valid-url-p url)
            (user-error "Invalid URL format. Must start with http:// or https://")))
        
        ;; Step 2: Check for duplicates (case-sensitive)
        (let ((duplicate (my/find-duplicate-bookmark url)))
          (when duplicate
            (user-error "URL already exists at line %d under heading: %s"
                        (car duplicate)
                        (or (cdr duplicate) "unknown"))))
        
        ;; Step 3: Extract domain for suggestion
        (setq domain-suggestion (my/extract-domain-from-url url))
        
        ;; Step 4: Prompt for description
        (setq description
              (read-string
               (format "Description%s: "
                       (if domain-suggestion
                           (format " (default: %s)" domain-suggestion)
                         ""))
               nil nil domain-suggestion))
        
        ;; Step 5: Validate description
        (when (or (not description) (string-empty-p description))
          (user-error "Description cannot be empty"))
        
        ;; Step 6: Append to bookmarks.org under Inbox
        (my/append-bookmark-to-inbox url description)
        
        ;; Step 7: Show success message with both description and URL
        (message "✓ Captured bookmark: %s → %s" description url))
    
    ;; Error handling
    (quit (message "Bookmark capture cancelled"))
    (error (message "Error capturing bookmark: %s" (error-message-string err)))))

;; ============================================================================
;; RAYCAST INTEGRATION
;; ============================================================================

;;; Raycast Integration for System-Wide Bookmark Access
;; Enables bookmark search and capture from anywhere in Windows via Raycast
;; Keywords: 'bs' (bookmark search), 'bc' (bookmark capture)
;;
;; Architecture:
;;   1. PowerShell scripts call emacsclient with -c flag (creates frame)
;;   2. Search: my/global-bookmark-search (configures frame, runs search, closes)
;;   3. Capture: my/global-bookmark-capture (configures frame, runs capture, closes)
;;   4. Inline functions for Emacs use: my/bookmark-search-inline (SPC b u)
;;
;; Frame Management:
;;   - Search frame: 100x30 chars (fits bookmark descriptions)
;;   - Capture frame: 80x20 chars (only shows prompts)
;;   - Both centered on primary monitor
;;   - emacsclient creates frame, functions configure and close it
;;
;; Setup:
;;   1. Emacs server runs automatically (see init.el)
;;   2. Install Raycast for Windows
;;   3. Configure Raycast script commands with keywords 'bs' and 'bc'
;;   4. Scripts location: ~/dotconfig/scripts/emacs/emacs-bookmark-*.ps1

(defun my/configure-bookmark-capture-frame (frame)
  "Configure FRAME for bookmark capture with centered positioning.
Size: 80x20 characters (smaller than search, only shows prompts).
Centers on primary monitor, clean appearance.

Parameters:
  FRAME - The frame to configure (created by emacsclient -c)"
  (let* ((frame-width-chars 80)
         (frame-height-chars 20)
         
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
    (set-frame-parameter frame 'name "Bookmark Capture")
    (set-frame-parameter frame 'unsplittable t)
    (set-frame-parameter frame 'auto-raise t)
    (set-frame-parameter frame 'tool-bar-lines 0)
    (set-frame-parameter frame 'menu-bar-lines 0)
    (set-frame-parameter frame 'tab-bar-lines 0)
    (set-frame-parameter frame 'vertical-scroll-bars nil)))

(defun my/global-bookmark-capture ()
  "Global bookmark capture with frame management for emacsclient.
This function is designed to be called via 'emacsclient -c' which creates
a frame. It configures that frame (centers on primary monitor, sets size),
runs the bookmark capture with minibuffer prompts, and closes the frame
after completion or cancellation.

Intended use: System-wide via Raycast (Alt+Space -> type 'bc')

Workflow:
  1. Configures emacsclient-created frame (80x20, centered)
  2. Runs my/capture-url-bookmark (clipboard detection, duplicate check, prompts)
  3. Closes frame after save/cancel (whether successful or error)
  
The frame ALWAYS closes after capture completes, ensuring no leftover frames."
  (interactive)
  (let ((client-frame (selected-frame)))
    (unwind-protect
        (condition-case err
            (progn
              ;; Step 1: Configure the emacsclient frame
              (my/configure-bookmark-capture-frame client-frame)
              
              ;; Step 2: Run bookmark capture (blocks until complete)
              ;; Uses minibuffer prompts: URL (if not in clipboard), Description
              ;; Handles: clipboard detection, validation, duplicate check
              (my/capture-url-bookmark))
          
          ;; Handle errors gracefully (show in minibuffer, not message box)
          (error
           (message "Bookmark capture error: %s" (error-message-string err))))
      
      ;; Cleanup: ALWAYS close the frame
      (when (frame-live-p client-frame)
        (delete-frame client-frame t)))))

;; Architecture Notes:
;; - Bookmark search: Two functions for different contexts
;;   * my/bookmark-search-inline: For SPC b u (no frame, current window)
;;   * my/global-bookmark-search: For Raycast bs (popup frame managed by emacsclient)
;; - Bookmark capture: Single function reused by wrapper
;;   * my/capture-url-bookmark: Core capture logic (reused)
;;   * my/global-bookmark-capture: Frame wrapper for Raycast bc
;; - Frame creation: emacsclient -c creates frame, functions configure it
;; - Consistent with snippet system architecture (ss/sc and bs/bc)

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Keybindings:
;;   SPC b u  - URL bookmarks search (inline, current window)
;;   SPC b c  - Capture URL bookmark
;;   Raycast: 'bs' (search with popup frame), 'bc' (capture with popup frame)
(zzc/leader-keys
  "bu" '(my/bookmark-search-inline :which-key "url bookmarks")
  "bc" '(my/capture-url-bookmark :which-key "capture url"))

;; Implemented features:
;;   1. URL bookmark search (SPC b u, or Raycast 'bs')
;;      - Fuzzy search with consult integration
;;      - Popup frame with automatic cleanup
;;      - Opens in Windows default browser
;;      - Preserves org tags in descriptions
;;
;;   2. URL bookmark capture (SPC b c, or Raycast 'bc')
;;      - Clipboard detection for URLs
;;      - Duplicate checking (case-sensitive)
;;      - Domain extraction for default description
;;      - Saves to Inbox heading in bookmarks.org
;;
;; Future enhancements:
;;   - Category management: Choose heading during capture (not always Inbox)
;;   - Bookmark editing: C-c C-e to open bookmarks.org at selected entry
;;   - Preview: Show URL in minibuffer before opening
;;   - Category filtering: Narrow by org heading (Development, Documentation, etc.)
;;   - Frequency tracking: Sort by access frequency/recency
;;   - Multiple files: Search across multiple bookmark files
;;   - Tag support: Search and filter by org tags

(provide 'bookmarks)
;;; bookmarks.el ends here
