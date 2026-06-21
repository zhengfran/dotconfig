;;; ai.el --- AI/LLM Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI/LLM integration via agent-shell (Claude Code, etc.)
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; TRANSIENT - required by agent-shell's menus
;; ============================================================================
;; Emacs ships a built-in `transient' that is older than what agent-shell
;; needs (it calls `transient--set-layout', absent from the built-in version).
;; Pull the current release via straight and load it *before* agent-shell so
;; straight's build dir is prepended to `load-path' and agent-shell's
;; `(require 'transient)' resolves to the newer copy.
(use-package transient
  :straight t
  :demand t)

;; ============================================================================
;; AGENT SHELL - Interactive LLM agent buffer (Claude Code, Gemini CLI, etc.)
;; ============================================================================
;; Prerequisites:
;;   npm install -g @zed-industries/claude-code-acp  (for Claude Code agent)
;;   On Windows: ensure npm global bin (~\AppData\Roaming\npm) is in exec-path
;;               (handled automatically by core.el)

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell")
  :config
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Corporate proxy: disable Node.js SSL verification for claude-code-acp
  ;; TODO: replace with NODE_EXTRA_CA_CERTS pointing to your corporate CA cert
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         :inherit-env t
         "NODE_TLS_REJECT_UNAUTHORIZED" "0"))
  ;; Always display agent-shell buffers in a dedicated right-half window.
  ;; Combined with the Agent tab group in buffer-tabs.el, agent buffers get
  ;; their own window AND their own tabs, never mixed with normal or terminal
  ;; buffers.
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (with-current-buffer (get-buffer buffer-or-name)
                     (derived-mode-p 'agent-shell-mode
                                     'agent-shell-viewport-view-mode
                                     'agent-shell-viewport-edit-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.5))))

;; Manager: tabulated list view of all open agent sessions
(use-package agent-shell-manager
  :straight (:host github :repo "jethrokuan/agent-shell-manager")
  :after agent-shell
  :custom (agent-shell-manager-side 'bottom))

;; Code review: send git diff to a second agent for review
(use-package agent-review
  :straight (:host github :repo "nineluj/agent-review")
  :after agent-shell)

;; Mode-line indicator showing pending/busy agent buffer counts
(use-package agent-shell-attention
  :straight (:host github :repo "ultronozm/agent-shell-attention.el")
  :after agent-shell
  :config (agent-shell-attention-mode 1))

;; Key bindings
(zzc/leader-keys
  "a"  '(:ignore t :which-key "agent shell")
  "aa" '(agent-shell :which-key "open agent")
  "ac" '(agent-shell-anthropic-start-claude-code :which-key "claude code")
  "am" '(agent-shell-manager-toggle :which-key "manager")
  "ar" '(agent-review :which-key "code review"))

;; Top-level toggle/summon for the agent panel, paralleling vterm's "C-c t".
;; `agent-shell' is DWIM: creates an agent if none exists, shows it if hidden,
;; toggles it if already visible. The display rule above keeps it right-half.
(global-set-key (kbd "C-c s") #'agent-shell)

(provide 'ai)
;;; ai.el ends here
