;;; ui.el --- UI configuration (themes, modeline, icons) -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual appearance: themes, modeline, icons, display buffer settings
;;
;; DEPENDENCIES: keybindings (zzc/leader-keys)
;; USED BY: None

;;; Code:

;; ============================================================================
;; DISPLAY BUFFER CONFIGURATION
;; ============================================================================

(setq
 display-buffer-alist
 '(;; Help windows
   ("^\\*[Hh]elp"                            ;正则匹配 buffer name
    (display-buffer-reuse-window
   ;入口函数，一个个调用直到有返回值，参数是：1.buffer 2.剩下的这些 alist
     display-buffer-in-side-window)
    (side . right)                        ;参数 alist 从这里开始。这个 side 会被 display-buffer-in-side-window 使用
    (window-width . 0.5)                     ;emacs 会自动把这个设置到 window-parameter 里
    (window-height . 0.33)                   ;同上
    (slot . 1)                               ;这个会被 display-buffer-in-side-window 使用，控制 window 位置
    (reusable-frames . visible)              ;这个参数看第三个链接的 display-buffer
    (haha . whatever)                        ;当然随你放什么
    (window-parameters                       ;emacs 26 及以上会自动把下面的设置到 window-parameter 里
     (select . t)                            ;自定义的 param
     (quit . t)                              ;同上
     (popup . t)                             ;同上
     (mode-line-format . none)               ;emacs version > 25， none 会隐藏 mode line，nil 会显示...
     (no-other-window . t)                   ;随你设置其他的 window-parameter，看文档
     ))))

;; ============================================================================
;; EMOJIFY
;; ============================================================================

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; ============================================================================
;; THEME FUNCTIONS
;; ============================================================================

(defun my/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
					(->> (custom-available-themes)
					     (-map #'symbol-name)
					     (--select (string-prefix-p "doom-" it)))))))
  (my/switch-theme theme)
  (set-face-foreground 'org-indent (face-background 'default)))

(defun my/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
					(->> (custom-available-themes)
					     (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

;; ============================================================================
;; DOOM THEMES
;; ============================================================================

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t) ; 加载主题
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (doom-themes-org-config)
  ;; Fix Corfu face inheritance cycle error with Gnus faces
  ;; Reset problematic Gnus faces since we don't use Gnus
  (custom-set-faces
   '(gnus-group-news-low ((t :inherit default)))
   '(gnus-group-news-low-empty ((t :inherit default)))
   '(gnus-group-mail-1 ((t :inherit default)))
   '(gnus-group-mail-low ((t :inherit default)))))

;; ============================================================================
;; THEME KEYBINDINGS
;; ============================================================================

(zzc/leader-keys
  "t"  '(:ignore t :which-key "toggle")
  "tt" '(my/load-doom-theme :which-key "themes"))

;; ============================================================================
;; ICONS
;; ============================================================================

(use-package all-the-icons
  :if (display-graphic-p)) ;M-x all-the-icon-install-fonts.

;; ============================================================================
;; MODELINE
;; ============================================================================

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-unicode-fallback t)
  ;; Show tab-bar workspace name in modeline
  (doom-modeline-workspace-name t)
  :config
  ;; Ensure workspace segment is visible
  (setq doom-modeline-workspace-name t))

;; ============================================================================
;; RAINBOW DELIMITERS
;; ============================================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'ui)
;;; ui.el ends here
