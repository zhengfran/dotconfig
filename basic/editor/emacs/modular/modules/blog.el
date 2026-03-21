;;; blog.el --- Blog article tracking system -*- lexical-binding: t; -*-

;;; Commentary:
;; Blog article management with denote integration and ox-hugo export.
;; Tracks articles through stages: idea → working → release.
;; Bilingual support: English (en) and Chinese (zh) posts.
;; Auto-exports to Hugo via ox-hugo when stage advances to release.
;; Org dynamic block for status overview.
;;
;; DEPENDENCIES: core (my/org-base-dir), denote-config, keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

(declare-function denote "denote")
(declare-function denote-retrieve-title-value "denote")
(declare-function org-table-align "org-table")
(declare-function org-ctrl-c-ctrl-c "org")
(declare-function org-hugo-export-to-md "ox-hugo")
(declare-function zzc/leader-keys "keybindings")

(require 'cl-lib)

(defvar my/org-base-dir)
(defvar denote-directory)

;; ============================================================================
;; DIRECTORY & CONSTANTS
;; ============================================================================

(defvar my/blog-directory (expand-file-name "blog" my/org-base-dir)
  "Directory for blog article notes.")

(defvar my/blog-hugo-base-dir (expand-file-name "~/projects/blog/zhengfran.github.io")
  "Hugo site root directory.")

(defconst my/blog-stages '("idea" "working" "release")
  "Valid blog article stages in lifecycle order.")

(defconst my/blog-languages
  '(("en" . ((author . "Zheng Zhicheng")
             (section . "en/posts")))
    ("zh" . ((author . "郑之成")
             (section . "zh/posts"))))
  "Blog language configurations.
Each entry maps a language code to its Hugo section and author name.")

(defun my/blog-ensure-directory ()
  "Ensure blog directory exists."
  (unless (file-exists-p my/blog-directory)
    (make-directory my/blog-directory t)))

;; ============================================================================
;; OX-HUGO
;; ============================================================================

(use-package ox-hugo
  :after ox)

;; ============================================================================
;; FRONT-MATTER HELPERS
;; ============================================================================

(defun my/blog-get-front-matter (file keyword)
  "Get #+KEYWORD value from FILE front-matter."
  (with-temp-buffer
    (insert-file-contents file nil 0 2048)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^#\\+" (regexp-quote keyword) ":\\s-*\\(.+\\)") nil t)
      (string-trim (match-string 1)))))

(defun my/blog-get-stage (file)
  "Get #+blog_stage value from FILE."
  (or (my/blog-get-front-matter file "blog_stage") "idea"))

(defun my/blog-get-lang (file)
  "Get #+blog_lang value from FILE."
  (or (my/blog-get-front-matter file "blog_lang") "en"))

(defun my/blog-lang-config (lang key)
  "Get config KEY for LANG from `my/blog-languages'."
  (alist-get key (cdr (assoc lang my/blog-languages))))

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
When advancing to release, auto-exports to Hugo via ox-hugo."
  (interactive)
  (let* ((current (my/blog-get-stage (buffer-file-name)))
         (idx (cl-position current my/blog-stages :test #'string=))
         (next (when (and idx (< idx (1- (length my/blog-stages))))
                 (nth (1+ idx) my/blog-stages))))
    (if next
        (progn
          (my/blog-set-stage next)
          (when (string= next "release")
            (my/blog-publish-to-hugo))
          (message "Blog stage: %s → %s" current next))
      (message "Already at final stage: %s" current))))

;; ============================================================================
;; HUGO EXPORT
;; ============================================================================

(defun my/blog-ensure-hugo-front-matter ()
  "Ensure current buffer has ox-hugo front-matter keywords.
Adds #+hugo_base_dir, #+hugo_section, and #+author if missing.
Derives section from #+blog_lang."
  (let* ((lang (my/blog-get-lang (buffer-file-name)))
         (section (my/blog-lang-config lang 'section))
         (author (my/blog-lang-config lang 'author)))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+hugo_base_dir:" nil t)
        (goto-char (point-min))
        (re-search-forward "^#\\+blog_lang:.*$" nil t)
        (end-of-line)
        (insert (concat "\n#+hugo_base_dir: " my/blog-hugo-base-dir
                        "\n#+hugo_section: " section
                        "\n#+hugo_bundle: "
                        "\n#+export_file_name: index"
                        "\n#+author: " author
                        "\n#+hugo_draft: false"))))))

(defun my/blog-update-lastmod ()
  "Update #+hugo_lastmod in current buffer to current timestamp."
  (save-excursion
    (goto-char (point-min))
    (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (if (re-search-forward "^#\\+hugo_lastmod:.*$" nil t)
          (replace-match (concat "#+hugo_lastmod: " timestamp))
        (when (re-search-forward "^#\\+hugo_draft:.*$" nil t)
          (beginning-of-line)
          (insert (concat "#+hugo_lastmod: " timestamp "\n")))))))

(defun my/blog-publish-to-hugo ()
  "Export current blog article to Hugo via ox-hugo.
Updates #+hugo_lastmod to current time before exporting."
  (interactive)
  (require 'ox-hugo)
  (my/blog-ensure-hugo-front-matter)
  (my/blog-update-lastmod)
  (save-buffer)
  (let ((output-file (org-hugo-export-to-md)))
    (if output-file
        (message "Published to Hugo: %s" output-file)
      (message "Hugo export failed"))))

(defun my/blog-publish-all ()
  "Export all released blog articles to Hugo."
  (interactive)
  (require 'ox-hugo)
  (let ((released (seq-filter
                   (lambda (f) (string= (my/blog-get-stage f) "release"))
                   (my/blog-list-files)))
        (count 0))
    (dolist (file released)
      (with-current-buffer (find-file-noselect file)
        (my/blog-ensure-hugo-front-matter)
        (save-buffer)
        (when (org-hugo-export-to-md)
          (cl-incf count))))
    (message "Published %d/%d released articles to Hugo" count (length released))))

;; ============================================================================
;; CAPTURE & NAVIGATION
;; ============================================================================

(defun my/blog-create ()
  "Create a new blog article note in blog/ subdirectory.
Prompts for language (en/zh) and title.
Includes ox-hugo front-matter for Hugo export."
  (interactive)
  (my/blog-ensure-directory)
  (let* ((lang (completing-read "Language: " '("en" "zh") nil t))
         (title (read-string "Blog article title: "))
         (section (my/blog-lang-config lang 'section))
         (author (my/blog-lang-config lang 'author))
         (keywords '("blog"))
         (body (concat "#+blog_lang: " lang "\n"
                       "#+blog_stage: idea\n"
                       "#+hugo_base_dir: " my/blog-hugo-base-dir "\n"
                       "#+hugo_section: " section "\n"
                       "#+hugo_bundle: " title "\n"
                       "#+export_file_name: index\n"
                       "#+author: " author "\n"
                       "#+hugo_lastmod: \n"
                       "#+hugo_draft: false\n"
                       "#+description: \n"
                       "#+hugo_tags: \n"
                       "#+hugo_categories: \n"
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
           "b l p" '(my/blog-publish-to-hugo :which-key "publish current")
           "b l P" '(my/blog-publish-all     :which-key "publish all released"))))

(provide 'blog)
;;; blog.el ends here
