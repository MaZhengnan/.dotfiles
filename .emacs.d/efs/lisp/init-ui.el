;;; init-ui.el --- UI, Theme and Fonts -*- lexical-binding: t -*-

;; ----- Theme -----
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; ----- Fonts -----
;; Set default font to JetBrainsMono Nerd Font
(defun efs/set-font-faces ()
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)
  
  ;; Set Chinese/CJK font
  ;; Adjust "Noto Sans CJK SC" to your preferred installed CJK font if needed
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Noto Sans CJK SC" :size 16))))

;; Apply font settings on startup and when creating a new frame
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (efs/set-font-faces))))
  (efs/set-font-faces))

;; ----- Modeline -----
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-icon t)) ; Requires nerd-icons

;; ----- Icons (Nerd Icons) -----
(use-package nerd-icons
  ;; You may need to run M-x nerd-icons-install-fonts if icons are missing
  )

;; Enhance Dired with Nerd Icons
;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; ----- Rainbow Delimiters -----
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ----- Which-key -----
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'bottom))

(provide 'init-ui)
