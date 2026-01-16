;;; bookmarks.el --- Bookmark management and URL bookmarks -*- lexical-binding: t; -*-

;;; Commentary:
;; Standard Emacs bookmarks plus custom URL bookmark system:
;; - Parse bookmarks from ~/org/bookmarks.org
;; - Fuzzy search with consult
;; - Centered popup frame for selection
;; - URL capture with clipboard detection and duplicate checking
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

(defun my/create-centered-bookmark-frame ()
  "Create a centered popup frame for bookmark search.
Frame size: 100 chars wide × 30 lines tall."
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         ;; Frame dimensions in pixels
         (frame-width-chars 100)
         (frame-height-chars 30)
         (frame-pixel-width (* frame-width-chars char-width))
         (frame-pixel-height (* frame-height-chars char-height))
         ;; Center calculations
         (left (max 0 (/ (- screen-width frame-pixel-width) 2)))
         (top (max 0 (/ (- screen-height frame-pixel-height) 2))))
    
    (make-frame `((name . "Bookmark Search")
                  (width . ,frame-width-chars)
                  (height . ,frame-height-chars)
                  (left . ,left)
                  (top . ,top)
                  (minibuffer . t)           ; Need minibuffer for consult
                  (unsplittable . t)         ; Single window only
                  (auto-raise . t)           ; Bring to front
                  (tool-bar-lines . 0)       ; No toolbar
                  (menu-bar-lines . 0)       ; No menu bar
                  (tab-bar-lines . 0)        ; No tab bar
                  (vertical-scroll-bars . nil))))) ; No scrollbars

(defun my/open-url-in-browser (url)
  "Open URL in Windows default browser.
Uses w32-shell-execute for native Windows."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute "open" url)
    (error "Not running on native Windows. System type: %s" system-type)))

(defun my/search-bookmarks-in-frame ()
  "Open bookmark search in popup frame.
Searches ~/org/bookmarks.org and opens selected URL in browser.
Frame auto-closes after selection or cancellation."
  (interactive)
  (let ((original-frame (selected-frame))
        (bookmark-frame nil))
    (unwind-protect
        (condition-case err
            (progn
              ;; Step 1: Create and focus popup frame
              (setq bookmark-frame (my/create-centered-bookmark-frame))
              (select-frame-set-input-focus bookmark-frame)
              
              ;; Step 2: Run consult search
              (let ((url (my/consult-bookmarks)))
                
                ;; Step 3: Open URL if selected (nil if C-g cancelled)
                (when url
                  (my/open-url-in-browser url)
                  (message "Opened: %s" url))))
          
          ;; Handle errors gracefully
          (error
           (message "Bookmark search error: %s" (error-message-string err))))
      
      ;; Cleanup: ALWAYS delete frame and restore focus
      (when (and bookmark-frame (frame-live-p bookmark-frame))
        (delete-frame bookmark-frame))
      (when (frame-live-p original-frame)
        (select-frame-set-input-focus original-frame)))))

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
;; KEYBINDINGS
;; ============================================================================

;; Keybinding: SPC b u (bookmark → URL), SPC b c (bookmark → capture)
(zzc/leader-keys
  "bu" '(my/search-bookmarks-in-frame :which-key "url bookmarks")
  "bc" '(my/capture-url-bookmark :which-key "capture url"))

;; Future enhancements (documented, not implemented):
;; - Preview: Show URL in minibuffer before opening
;; - Categories: Narrow by org heading (Development, Documentation, etc.)
;; - Edit: C-c C-e to open bookmarks.org at selected entry
;; - Recent: Sort by access frequency/recency
;; - Multiple files: Search across multiple bookmark files

(provide 'bookmarks)
;;; bookmarks.el ends here
