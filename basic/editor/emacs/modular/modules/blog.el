;;; blog.el --- Blog article tracking system -*- lexical-binding: t; -*-

;;; Commentary:
;; Blog article management with denote integration and ox-html export.
;; Tracks articles through stages: idea → working → release.
;; Bilingual support: English (en) and Chinese (zh) posts.
;;
;; Publishing exports the source note to a standalone HTML page in the
;; myblog repo (~/projects/blog/myblog), following the exported-post
;; contract documented in that repo's README and docs/adr/0001:
;;   - blog:* meta tags in <head> (read by scripts/build-manifest.mjs)
;;   - css/org-post.css + js/theme.js linked relatively
;;   - htmlize CSS classes for code (org-html-htmlize-output-type 'css)
;;   - KaTeX auto-render included only when the post contains LaTeX
;;   - local images copied to blogs/{lang}/assets/{slug}/
;;   - #+blog_password ⇒ body encrypted via scripts/encrypt-post.mjs
;;     (docs/adr/0002); the password never leaves the source note
;;
;; DEPENDENCIES: core (my/org-base-dir), denote-config, keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

(declare-function denote "denote")
(declare-function denote-retrieve-title-value "denote")
(declare-function org-table-align "org-table")
(declare-function org-ctrl-c-ctrl-c "org")
(declare-function org-export-as "ox")
(declare-function zzc/leader-keys "keybindings")

(require 'cl-lib)

(defvar my/org-base-dir)
(defvar denote-directory)
(defvar org-html-htmlize-output-type)
(defvar org-html-htmlize-font-prefix)

;; ============================================================================
;; DIRECTORY & CONSTANTS
;; ============================================================================

(defvar my/blog-directory (expand-file-name "blog" my/org-base-dir)
  "Directory for blog article notes.")

(defvar my/blog-site-dir (expand-file-name "~/projects/blog/myblog")
  "Static site repo that published posts are exported into.")

(defconst my/blog-stages '("idea" "working" "release")
  "Valid blog article stages in lifecycle order.")

(defconst my/blog-languages
  '(("en" . ((author  . "Zheng Zhicheng")
             (back    . "&larr; back")
             (updated . "updated")))
    ("zh" . ((author  . "郑之成")
             (back    . "&larr; 返回")
             (updated . "更新于"))))
  "Blog language configurations.")

(defun my/blog-ensure-directory ()
  "Ensure blog directory exists."
  (unless (file-exists-p my/blog-directory)
    (make-directory my/blog-directory t)))

;; ============================================================================
;; HTMLIZE
;; ============================================================================

;; ox-html needs htmlize to fontify src blocks into the org-* CSS classes
;; that the site's org-post.css styles; without it code exports as plain text.
(use-package htmlize
  :defer t)

;; ============================================================================
;; FRONT-MATTER HELPERS
;; ============================================================================

(defun my/blog-get-front-matter (file keyword)
  "Get #+KEYWORD value from FILE front-matter.
Returns nil when the keyword is absent or has an empty value. The match
is confined to the keyword's own line (\\s-* would walk across newlines)."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^#\\+" (regexp-quote keyword) ":[ \t]*\\(.*\\)$") nil t)
      (let ((val (string-trim (match-string 1))))
        (unless (string-empty-p val) val)))))

(defun my/blog-get-stage (file)
  "Get #+blog_stage value from FILE."
  (or (my/blog-get-front-matter file "blog_stage") "idea"))

(defun my/blog-get-lang (file)
  "Get #+blog_lang value from FILE."
  (or (my/blog-get-front-matter file "blog_lang") "en"))

(defun my/blog-lang-config (lang key)
  "Get config KEY for LANG from `my/blog-languages'."
  (alist-get key (cdr (assoc lang my/blog-languages))))

(defun my/blog--truthy-p (val)
  "Non-nil when the keyword value VAL means true."
  (and val (member (downcase (string-trim val)) '("t" "true" "yes" "1"))))

;; ============================================================================
;; STAGE UTILITIES
;; ============================================================================

(defun my/blog-set-stage (stage)
  "Set #+blog_stage in current org buffer to STAGE."
  (interactive
   (list (completing-read "Stage: " my/blog-stages nil t)))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+blog_stage:.*$" nil t)
        (replace-match (concat "#+blog_stage: " stage))
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags:.*$" nil t)
          (end-of-line)
        (re-search-forward "^#\\+title:.*$" nil t)
        (end-of-line))
      (insert (concat "\n#+blog_stage: " stage))))
  (save-buffer)
  (message "Blog stage set to: %s" stage))

(defun my/blog-advance-stage ()
  "Advance current blog article to next stage.
When advancing to release, auto-exports to the site via `my/blog-publish'."
  (interactive)
  (let* ((current (my/blog-get-stage (buffer-file-name)))
         (idx (cl-position current my/blog-stages :test #'string=))
         (next (when (and idx (< idx (1- (length my/blog-stages))))
                 (nth (1+ idx) my/blog-stages))))
    (if next
        (progn
          (my/blog-set-stage next)
          (when (string= next "release")
            (my/blog-publish))
          (message "Blog stage: %s → %s" current next))
      (message "Already at final stage: %s" current))))

;; ============================================================================
;; HTML EXPORT — helpers
;; ============================================================================

(defun my/blog--html-escape (s)
  "Escape S for use in HTML text and double-quoted attributes."
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;")))
    (setq s (string-replace (car pair) (cdr pair) s)))
  s)

(defun my/blog--meta (name content)
  "A blog:NAME meta tag line, or nil when CONTENT is empty."
  (when (and content (not (string-empty-p content)))
    (format "<meta name=\"blog:%s\" content=\"%s\">"
            name (my/blog--html-escape content))))

(defun my/blog--iso-date (raw)
  "Extract YYYY-MM-DD from RAW (an org #+date value), or nil."
  (when (and raw (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" raw))
    (match-string 0 raw)))

(defun my/blog--update-lastmod ()
  "Set #+blog_lastmod in the current buffer to now (inserting if missing).
Returns the timestamp as \"YYYY-MM-DD HH:MM\" for the blog:lastmod meta."
  (save-excursion
    (goto-char (point-min))
    (let ((ts (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (if (re-search-forward "^#\\+blog_lastmod:.*$" nil t)
          (replace-match (concat "#+blog_lastmod: " ts) t t)
        (when (re-search-forward "^#\\+blog_stage:.*$" nil t)
          (end-of-line)
          (insert "\n#+blog_lastmod: " ts)))))
  (format-time-string "%Y-%m-%d %H:%M"))

(defun my/blog--export-body ()
  "Export the current org buffer to an HTML body-only string."
  (require 'ox-html)
  (let ((org-html-htmlize-output-type 'css)
        (org-html-htmlize-font-prefix "org-"))
    (org-export-as 'html nil nil t
                   '(:with-toc nil
                     :section-numbers nil
                     :html-doctype "html5"
                     :html-html5-fancy t))))

(defun my/blog--strip-tags (html)
  "Visible text of HTML."
  (replace-regexp-in-string "<[^>]*>" " " html))

(defun my/blog--read-time (html lang)
  "Read-time string for HTML body text in LANG.
Mirrors the site's autoMin: en ~200 words/min, zh ~400 chars/min."
  (let ((text (my/blog--strip-tags html)))
    (if (string= lang "zh")
        (format "%d 分钟"
                (max 1 (ceiling (cl-count-if (lambda (c) (<= #x4e00 c #x9fff)) text)
                                400)))
      (format "%dm"
              (max 1 (ceiling (length (split-string text "[ \t\n]+" t)) 200))))))

(defun my/blog--copy-assets (body org-dir lang slug)
  "Copy local <img> sources in BODY into the site's per-post asset dir.
Relative paths resolve against ORG-DIR. Returns BODY with srcs rewritten
to assets/SLUG/…; remote (URL) and data: images pass through untouched."
  (let ((asset-dir (expand-file-name (format "blogs/%s/assets/%s" lang slug)
                                     my/blog-site-dir)))
    (replace-regexp-in-string
     "<img \\([^>]*\\)src=\"\\([^\"]+\\)\""
     (lambda (m)
       (let ((pre (match-string 1 m))
             (src (match-string 2 m)))
         (if (string-match-p "\\`\\(?:[a-z]+:\\)?//\\|\\`data:" src)
             m
           (let ((file (expand-file-name src org-dir)))
             (unless (file-exists-p file)
               (user-error "Image not found: %s" file))
             (make-directory asset-dir t)
             (copy-file file
                        (expand-file-name (file-name-nondirectory file) asset-dir)
                        t)
             (format "<img %ssrc=\"assets/%s/%s\""
                     pre slug (file-name-nondirectory file))))))
     body t t)))

(defun my/blog--encrypt (plaintext password)
  "Encrypt PLAINTEXT with PASSWORD via the site's encrypt-post script.
Returns the base64 payload."
  (let ((script (expand-file-name "scripts/encrypt-post.mjs" my/blog-site-dir))
        (node (executable-find "node")))
    (unless node (user-error "Cannot encrypt: node not found on exec-path"))
    (unless (file-exists-p script) (user-error "Missing %s" script))
    (with-temp-buffer
      (insert plaintext)
      (let* ((process-environment
              (cons (concat "POST_PASSWORD=" password) process-environment))
             (exit (call-process-region (point-min) (point-max)
                                        node t t nil script)))
        (unless (eq exit 0)
          (error "encrypt-post.mjs failed (%s): %s" exit (buffer-string)))
        (string-trim (buffer-string))))))

(defconst my/blog--katex-head
  (concat
   "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\">\n"
   "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\"></script>\n"
   "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js\""
   " onload=\"renderMathInElement(document.getElementById('content'),{delimiters:["
   "{left:'$$',right:'$$',display:true},"
   "{left:'\\\\[',right:'\\\\]',display:true},"
   "{left:'\\\\(',right:'\\\\)',display:false}"
   "],throwOnError:false})\"></script>")
  "KaTeX auto-render snippet, included only for posts containing LaTeX.")

(defun my/blog--page (info)
  "Assemble a standalone post page. INFO is a plist (see `my/blog-publish')."
  (let* ((lang     (plist-get info :lang))
         (locked   (plist-get info :locked))
         (title    (my/blog--html-escape (plist-get info :title)))
         (author   (plist-get info :author))
         (lastmod  (plist-get info :lastmod))
         (head
          (delq nil
                (list
                 "<meta charset=\"utf-8\">"
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 (format "<title>%s</title>" title)
                 (my/blog--meta "date" (plist-get info :date))
                 (my/blog--meta "lastmod" lastmod)
                 (my/blog--meta "tags" (plist-get info :tags))
                 (my/blog--meta "deck" (plist-get info :deck))
                 (when (plist-get info :featured) (my/blog--meta "featured" "true"))
                 (when locked (my/blog--meta "locked" "true"))
                 (when locked (my/blog--meta "min" (plist-get info :min)))
                 (format "<meta name=\"author\" content=\"%s\">"
                         (my/blog--html-escape author))
                 "<link rel=\"stylesheet\" href=\"../../css/org-post.css\">"
                 "<script src=\"../../js/theme.js\"></script>"
                 (when locked "<script src=\"../../js/decrypt.js\" defer></script>")
                 (when (plist-get info :math) my/blog--katex-head))))
         (content
          (if locked
              (format (concat "<h1 class=\"title\">%s</h1>\n"
                              "<script type=\"text/x-encrypted-post\" id=\"locked-payload\">%s</script>")
                      title (plist-get info :payload))
            (format "<h1 class=\"title\">%s</h1>\n%s" title (plist-get info :body)))))
    (concat
     "<!DOCTYPE html>\n"
     (format "<html lang=\"%s\">\n" (if (string= lang "zh") "zh-CN" "en"))
     "<head>\n" (mapconcat #'identity head "\n") "\n</head>\n"
     "<body>\n"
     (format "<div id=\"preamble\" class=\"status\"><a href=\"../../index.html#writing\">%s</a></div>\n"
             (my/blog-lang-config lang 'back))
     "<div id=\"content\" class=\"content\">\n" content "\n</div>\n"
     (let* ((date (plist-get info :date))
            ;; Show "updated …" only when the post was touched after its
            ;; publish date.
            (upd (when (and lastmod (not (string-prefix-p date lastmod)))
                   (format " · %s %s"
                           (my/blog-lang-config lang 'updated)
                           (substring lastmod 0 10)))))
       (format "<div id=\"postamble\" class=\"status\"><p>%s%s · %s</p></div>\n"
               date (or upd "") (my/blog--html-escape author)))
     "</body>\n</html>\n")))

;; ============================================================================
;; PUBLISH
;; ============================================================================

(defun my/blog-publish ()
  "Export the current blog article to the site as a standalone HTML post.
Validates front-matter, copies local images into the repo, and encrypts
the body when #+blog_password is set."
  (interactive)
  (unless (buffer-file-name) (user-error "Buffer is not visiting a file"))
  (let* ((lastmod (my/blog--update-lastmod))
         (_ (save-buffer))
         (file  (buffer-file-name))
         (title (my/blog-get-front-matter file "title"))
         (date  (my/blog--iso-date (my/blog-get-front-matter file "date")))
         (lang  (my/blog-get-lang file))
         (stage (my/blog-get-stage file))
         (slug  (my/blog-get-front-matter file "export_file_name"))
         (password (my/blog-get-front-matter file "blog_password")))
    ;; The slug is the permalink and RSS guid — frozen after first publish,
    ;; so refuse anything that isn't a deliberate value.
    (unless (and title (not (string-empty-p title)))
      (user-error "Missing #+title"))
    (unless date
      (user-error "Missing or non-ISO #+date (need YYYY-MM-DD)"))
    (unless (assoc lang my/blog-languages)
      (user-error "Bad #+blog_lang: %s (want en or zh)" lang))
    ;; The slug is the filename and URL: no whitespace or path separators
    ;; (CJK is fine — browsers percent-encode it), and not "index".
    (unless (and slug (string-match-p "\\`[^[:space:]/\\]+\\'" slug)
                 (not (string= slug "index")))
      (user-error "Bad #+export_file_name: %S (the slug; no spaces or slashes, not \"index\")" slug))
    (unless (or (string= stage "release")
                (y-or-n-p (format "Stage is '%s', not release. Publish anyway? " stage)))
      (user-error "Publish cancelled"))
    (let* ((body (my/blog--export-body))
           (body (my/blog--copy-assets body (file-name-directory file) lang slug))
           (math (string-match-p "\\\\(\\|\\\\\\[" body))
           (info (list :lang lang
                       :title title
                       :date date
                       :lastmod lastmod
                       :author (or (my/blog-get-front-matter file "author")
                                   (my/blog-lang-config lang 'author))
                       :tags (my/blog-get-front-matter file "blog_tags")
                       :deck (my/blog-get-front-matter file "description")
                       :featured (my/blog--truthy-p
                                  (my/blog-get-front-matter file "blog_featured"))
                       :math math))
           (out (expand-file-name (format "blogs/%s/%s.html" lang slug)
                                  my/blog-site-dir)))
      (if (and password (not (string-empty-p password)))
          (setq info (plist-put
                      (plist-put
                       (plist-put info :locked t)
                       ;; The manifest can't compute a read time from
                       ;; ciphertext, so it ships as a meta tag.
                       :min (my/blog--read-time body lang))
                      :payload (my/blog--encrypt body password)))
        (setq info (plist-put info :body body)))
      (make-directory (file-name-directory out) t)
      (write-region (my/blog--page info) nil out)
      (my/blog--rebuild-manifest)
      (message "Published: %s" out)
      out)))

(defun my/blog--rebuild-manifest ()
  "Regenerate the site's manifest + feeds locally (CI does it on push too)."
  (let ((node (executable-find "node"))
        (default-directory my/blog-site-dir))
    (if node
        (with-temp-buffer
          (if (eq 0 (call-process node nil t nil "scripts/build-manifest.mjs"))
              (message "%s" (string-trim (buffer-string)))
            (message "build-manifest.mjs failed: %s" (string-trim (buffer-string)))))
      (message "node not found — run scripts/build-manifest.mjs in %s" my/blog-site-dir))))

(defun my/blog-publish-all ()
  "Export all released blog articles to the site."
  (interactive)
  (let ((released (seq-filter
                   (lambda (f) (string= (my/blog-get-stage f) "release"))
                   (my/blog-list-files)))
        (count 0))
    (dolist (file released)
      (with-current-buffer (find-file-noselect file)
        (when (my/blog-publish)
          (cl-incf count))))
    (message "Published %d/%d released articles" count (length released))))

;; ============================================================================
;; CAPTURE & NAVIGATION
;; ============================================================================

(defun my/blog-create ()
  "Create a new blog article note in blog/ subdirectory.
Prompts for language (en/zh) and title. The generated #+export_file_name
defaults to the title and is the post's slug — edit it before first
publish, never after (it's the permalink). Slugs must contain no
whitespace, so multi-word titles need editing before publish."
  (interactive)
  (my/blog-ensure-directory)
  (let* ((lang (completing-read "Language: " '("en" "zh") nil t))
         (title (read-string "Blog article title: "))
         (author (my/blog-lang-config lang 'author))
         (keywords '("blog"))
         (body (concat "#+author: " author "\n"
                       "#+blog_lang: " lang "\n"
                       "#+blog_stage: idea\n"
                       "#+blog_lastmod: \n"
                       "#+export_file_name: " title "\n"
                       "#+blog_tags: \n"
                       "#+description: \n"
                       "#+blog_featured: \n"
                       "#+blog_password: \n"
                       "\n* Outline\n\n* Draft\n\n* Notes\n\n")))
    (denote title keywords 'org my/blog-directory nil body)))

(defun my/blog-list-files ()
  "Return list of all blog article files."
  (when (file-exists-p my/blog-directory)
    (directory-files my/blog-directory t "\\.org$")))

(defun my/blog-find (&optional stage-filter)
  "Find and open a blog article, optionally filtering by STAGE-FILTER."
  (interactive)
  (let* ((all-files (my/blog-list-files))
         (files (if stage-filter
                    (seq-filter (lambda (f)
                                 (string= (my/blog-get-stage f) stage-filter))
                               all-files)
                  all-files))
         (candidates (mapcar (lambda (f)
                               (cons (format "[%s|%s] %s"
                                             (my/blog-get-lang f)
                                             (my/blog-get-stage f)
                                             (denote-retrieve-title-value f 'org))
                                     f))
                             files)))
    (if (null candidates)
        (message "No blog articles found%s. Create one with SPC b l n"
                 (if stage-filter (format " with stage '%s'" stage-filter) ""))
      (let* ((selected (completing-read "Blog article: " candidates nil t))
             (file (cdr (assoc selected candidates))))
        (find-file file)))))

;; ============================================================================
;; DYNAMIC BLOCK
;; ============================================================================

(defun my/blog-get-title-from-file (file)
  "Extract blog title from FILE using #+title."
  (with-temp-buffer
    (insert-file-contents file nil 0 1024)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)" nil t)
        (string-trim (match-string 1))
      (file-name-base file))))

(defun org-dblock-write:blog-tracker (params)
  "Generate blog article status table.
PARAMS can include:
  :stage - Filter to specific stage (default: show all)
  :lang  - Filter to specific language (default: show all)
  :dir   - Blog directory (default: `my/blog-directory')"
  (let* ((stage-filter (plist-get params :stage))
         (lang-filter (plist-get params :lang))
         (dir (or (plist-get params :dir) my/blog-directory))
         (files (when (file-exists-p dir)
                  (directory-files dir t "\\.org$")))
         (articles
          (seq-filter
           (lambda (row)
             (and (or (null stage-filter)
                      (string= (nth 1 row) stage-filter))
                  (or (null lang-filter)
                      (string= (nth 2 row) lang-filter))))
           (mapcar (lambda (f)
                     (list (my/blog-get-title-from-file f)
                           (my/blog-get-stage f)
                           (my/blog-get-lang f)
                           f))
                   (or files '())))))
    (if (null articles)
        (insert "| No blog articles found |\n")
      ;; Header
      (insert "| Title | Lang | Stage | File |\n")
      (insert "|-------+------+-------+------|\n")
      ;; Rows grouped by stage order
      (dolist (stage my/blog-stages)
        (dolist (article articles)
          (when (string= (nth 1 article) stage)
            (let* ((title (nth 0 article))
                   (lang (nth 2 article))
                   (file (nth 3 article))
                   (identifier (when (string-match
                                      "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                                      (file-name-nondirectory file))
                                 (match-string 1 (file-name-nondirectory file))))
                   (link (format "[[denote:%s][%s]]"
                                 (or identifier file) title)))
              (insert (format "| %s | %s | %s | %s |\n" link lang stage file))))))
      ;; Summary row
      (insert "|-------+------+-------+------|\n")
      (dolist (stage my/blog-stages)
        (let ((count (length (seq-filter
                              (lambda (a) (string= (nth 1 a) stage))
                              articles))))
          (when (> count 0)
            (insert (format "| *%s: %d* | | | |\n" stage count)))))
      (forward-line -1)
      (org-table-align))))

(defun my/blog-insert-tracker-block ()
  "Insert a blog tracker dynamic block at point."
  (interactive)
  (insert "#+BEGIN: blog-tracker\n")
  (insert "#+END:\n")
  (forward-line -1)
  (org-ctrl-c-ctrl-c))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(with-eval-after-load 'general
  (eval '(zzc/leader-keys
           "b l"   '(:ignore t :which-key "blog")
           "b l n" '(my/blog-create        :which-key "new article")
           "b l f" '(my/blog-find          :which-key "find article")
           "b l i" '((lambda () (interactive) (my/blog-find "idea"))    :which-key "ideas")
           "b l w" '((lambda () (interactive) (my/blog-find "working")) :which-key "working")
           "b l r" '((lambda () (interactive) (my/blog-find "release")) :which-key "released")
           "b l a" '(my/blog-advance-stage :which-key "advance stage")
           "b l s" '(my/blog-set-stage     :which-key "set stage")
           "b l t" '(my/blog-insert-tracker-block :which-key "insert tracker")
           "b l p" '(my/blog-publish       :which-key "publish current")
           "b l P" '(my/blog-publish-all   :which-key "publish all released"))))

(provide 'blog)
;;; blog.el ends here
