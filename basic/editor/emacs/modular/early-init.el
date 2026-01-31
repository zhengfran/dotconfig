;; -*- lexical-binding: t; -*-
;; early-init.el - Loaded before init.el and GUI initialization
(setq debug-on-error t)
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;;; Font Configuration
;; Configured here for better startup performance (before GUI renders)

;; Platform-specific font sizes
(defvar my/font-size
  (pcase system-type
    ('windows-nt 160)  ; 16pt for Windows
    ('darwin 160)      ; 16pt for macOS
    ('gnu/linux 160)   ; 16pt for Linux
    (_ 140))           ; Fallback
  "Default font size in 1/10 pt units.")

;; LaTeX preview scale (fixed value)
(defvar my/latex-preview-scale 1.3
  "Scale factor for LaTeX preview rendering.")

;; Preferred font names
(defvar my/preferred-fonts
  '((default . "Iosevka Nerd Font Propo")
    (variable-pitch . "Iosevka Nerd Font Propo")
    (fixed-pitch . "Iosevka Nerd Font Mono")
    (math . "Latin Modern Math")
    (chinese . "LXGW WenKai"))
  "Preferred font families for different use cases.")

;; Font family helper: Check if font exists, fallback to system default
(defun my/get-font-or-default (preferred-font)
  "Return PREFERRED-FONT if available, otherwise system default."
  (if (and (display-graphic-p)
           (find-font (font-spec :name preferred-font)))
      preferred-font
    (face-attribute 'default :family)))

;; Function to apply fonts to a specific frame
(defun my/setup-frame-fonts (&optional frame)
  "Apply font configuration to FRAME (or current frame if nil)."
  (when (display-graphic-p frame)
    (with-selected-frame (or frame (selected-frame))
      ;; Resolve fonts with fallback
      (let ((default-font (my/get-font-or-default (alist-get 'default my/preferred-fonts)))
            (variable-font (my/get-font-or-default (alist-get 'variable-pitch my/preferred-fonts)))
            (fixed-font (my/get-font-or-default (alist-get 'fixed-pitch my/preferred-fonts)))
            (math-font (my/get-font-or-default (alist-get 'math my/preferred-fonts)))
            (chinese-font (my/get-font-or-default (alist-get 'chinese my/preferred-fonts))))
        
        ;; Set face fonts
        (set-face-attribute 'default frame :font default-font :height my/font-size)
        (set-face-attribute 'variable-pitch frame :font variable-font :height my/font-size)
        (set-face-attribute 'fixed-pitch frame :font fixed-font :height my/font-size)
        
        ;; Set charset-specific fonts
        (set-fontset-font "fontset-default" 'mathematical math-font nil 'prepend)
        (set-fontset-font "fontset-default" 'han chinese-font nil 'prepend)
        (set-fontset-font "fontset-default" 'unicode chinese-font nil 'prepend)
        
        ;; Performance optimizations
        (setq inhibit-compacting-font-caches t)
        (setq auto-window-vscroll nil)))))

(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)
;; Apply fonts after Emacs initialization (when font system is ready)
(add-hook 'after-init-hook
          (lambda ()
            (when (display-graphic-p)
              (my/setup-frame-fonts))))

;; Apply fonts to new frames in daemon/client mode
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (my/setup-frame-fonts (selected-frame))))
