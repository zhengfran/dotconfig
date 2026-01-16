;;; ai.el --- AI/LLM Configuration with GPTel -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for GPTel AI assistant integration
;; Supports multiple backends: Gemini, ChatGPT, Claude, OpenRouter
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

(use-package gptel
  :straight t
  :init
  ;; Helper function to read API keys from environment variables
  (defun my/gptel-api-key-from-env (var-name)
    "Read API key from environment variable VAR-NAME."
    (let ((key (getenv var-name)))
      (if (and key (not (string-empty-p key)))
          key
        (progn
          (message "Warning: %s not set in environment" var-name)
          nil))))
  :config
  ;; Enable streaming for faster responses
  (setq gptel-stream t)

  ;; Backend: Gemini
  (gptel-make-gemini "Gemini"
    :key (lambda () (my/gptel-api-key-from-env "GEMINI_API_KEY"))
    :stream t)

  ;; Backend: ChatGPT (OpenAI)
  (gptel-make-openai "ChatGPT"
    :key (lambda () (my/gptel-api-key-from-env "OPENAI_API_KEY"))
    :models '(gpt-4o gpt-4o-mini gpt-4-turbo)
    :stream t)

  ;; Backend: Claude (Anthropic)
  (gptel-make-anthropic "Claude"
    :key (lambda () (my/gptel-api-key-from-env "ANTHROPIC_API_KEY"))
    :stream t)

  ;; Backend: OpenRouter (Default) - supports many models
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (my/gptel-api-key-from-env "OPENROUTER_API_KEY"))
          :models '(google/gemini-2.5-flash
		    google/gemini-2.5-pro
		    google/gemini-3-pro-preview
		    anthropic/claude-sonnet-4.5
		    anthropic/claude-opus-4.5
		    anthropic/claude-haiku-4.5
		    openai/gpt-4o-mini
		    openai/gpt-5.2)))

  ;; Org-mode integration
  (setq gptel-org-branching-context t)

  ;; Custom directives for various tasks
  (add-to-list 'gptel-directives
               '(code-review . "You are an expert code reviewer. Analyze code for quality, potential bugs, security issues, and adherence to best practices. Provide constructive, actionable feedback."))

  (add-to-list 'gptel-directives
               '(explain-code . "You are a programming teacher. Explain code clearly and concisely, focusing on what it does, how it works, and why it's structured that way. Use simple language."))

  (add-to-list 'gptel-directives
               '(debug-help . "You are a debugging expert. Help identify the root cause of bugs, suggest fixes, and explain why the issue occurred. Be systematic and thorough."))

  (add-to-list 'gptel-directives
               '(refactor . "You are a refactoring specialist. Suggest improvements to code structure, readability, and maintainability while preserving functionality. Explain the benefits of each suggestion."))

  (add-to-list 'gptel-directives
               '(write-tests . "You are a test engineer. Generate comprehensive unit tests that cover edge cases, normal cases, and error handling. Use appropriate testing frameworks."))

  (add-to-list 'gptel-directives
               '(commit-msg . "You are a git expert. Generate clear, concise commit messages following conventional commits format. Focus on the 'why' not just the 'what'."))

  (add-to-list 'gptel-directives
               '(translate-zh . "You are a translator. Translate the text to Chinese. Maintain the original tone and meaning. Be natural and idiomatic."))

  (add-to-list 'gptel-directives
               '(summarize . "You are a summarization expert. Create concise summaries that capture the key points and essential information. Be clear and structured."))
  
  (add-to-list 'gptel-directives
               '(journal . "You are a supportive and understanding life coach. I want you to review my daily journal ang give your thoughts and suggestions(maximum 2 suggestions), be concise, respond in simplifies chinese and org format under the AI Summary Heading"))

  ;; Display gptel chat buffers in right split
  (add-to-list 'display-buffer-alist
               '("\\*gptel.*\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.5)))

  ;; Helper: Explain selected code
  (defun my/gptel-explain-code ()
    "Explain the selected code or code at point."
    (interactive)
    (if (use-region-p)
        (let ((code (buffer-substring-no-properties (region-beginning) (region-end)))
              (mode major-mode))
          (gptel-request code
                         :system (format "Explain this %s code clearly and concisely. Focus on what it does, why, and any important details." mode)))
      (message "No region selected")))

  ;; Helper: Add org heading as context
  (defun my/gptel-add-org-heading ()
    "Add current org heading and its content as context for gptel."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point))
             (headline (org-element-property :raw-value element))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (content (buffer-substring-no-properties begin end)))
        (with-current-buffer (gptel-buffer-name)
          (goto-char (point-max))
          (insert (format "\n\n--- Context: %s ---\n%s\n---\n" headline content)))
        (message "Added org heading '%s' as context" headline))))

  ;; Helper: Add buffer as context
  (defun my/gptel-add-context-buffer ()
    "Add entire current buffer as context for gptel."
    (interactive)
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (buf-name (buffer-name)))
      (with-current-buffer (gptel-buffer-name)
        (goto-char (point-max))
        (insert (format "\n\n--- Context: Buffer %s ---\n%s\n---\n" buf-name content)))
      (message "Added buffer '%s' as context" buf-name)))

  ;; Helper: Add region as context
  (defun my/gptel-add-context-region ()
    "Add selected region as context for gptel."
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer (gptel-buffer-name)
            (goto-char (point-max))
            (insert (format "\n\n--- Context: Region ---\n%s\n---\n" content)))
          (message "Added region as context"))
      (message "No region selected")))

  ;; Helper: Add file as context
  (defun my/gptel-add-context-file ()
    "Add external file as context for gptel."
    (interactive)
    (let* ((file (read-file-name "File to add as context: "))
           (content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
      (with-current-buffer (gptel-buffer-name)
        (goto-char (point-max))
        (insert (format "\n\n--- Context: File %s ---\n%s\n---\n" file content)))
      (message "Added file '%s' as context" file)))

  ;; Helper: Clear all context
  (defun my/gptel-clear-context ()
    "Clear all manually added context in gptel buffer."
    (interactive)
    (with-current-buffer (gptel-buffer-name)
      (goto-char (point-min))
      (while (re-search-forward "^--- Context:.*?^---$" nil t)
        (delete-region (match-beginning 0) (match-end 0))))
    (message "Cleared all context")))

;; Key bindings
(zzc/leader-keys
  "a"   '(:ignore t :which-key "ai/llm")
  "aa"  '(gptel :which-key "gptel chat")
  "am"  '(gptel-menu :which-key "gptel menu")
  "as"  '(gptel-send :which-key "send")
  "ar"  '(gptel-rewrite-menu :which-key "rewrite")
  "ae"  '(my/gptel-explain-code :which-key "explain code")
  "ac"  '(:ignore t :which-key "context")
  "ach" '(my/gptel-add-org-heading :which-key "add org heading")
  "acb" '(my/gptel-add-context-buffer :which-key "add buffer")
  "acr" '(my/gptel-add-context-region :which-key "add region")
  "acf" '(my/gptel-add-context-file :which-key "add file")
  "acc" '(my/gptel-clear-context :which-key "clear context"))

(provide 'ai)
;;; ai.el ends here
