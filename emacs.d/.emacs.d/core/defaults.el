;;; my-config.el --- 我的配置
;;; Commentary:
;;; Code:

;; 添加这一行，启用词法绑定
;;; -*- lexical-binding: t; -*-

(use-package emacs
      :custom
      (menu-bar-mode nil)         ;; Disable the menu bar
      (scroll-bar-mode nil)       ;; Disable the scroll bar
      (tool-bar-mode nil)         ;; Disable the tool bar
      (inhibit-startup-screen t)  ;; Disable welcome screen

      (delete-selection-mode t)   ;; Select text and delete it by typing.
      (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
      (electric-pair-mode t)      ;; Turns on automatic parens pairing

      (blink-cursor-mode nil)     ;; Don't blink cursor
      (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

      ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
      (recentf-mode t) ;; Enable recent file mode

      ;;(global-visual-line-mode t)           ;; Enable truncated lines
      (display-line-numbers-type 'relative) ;; Relative line numbers
      (global-display-line-numbers-mode t)  ;; Display line numbers

      (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
      (scroll-conservatively 10) ;; Smooth scrolling
      ;;(scroll-margin 8)

      (tab-width 4)

      (make-backup-files nil) ;; Stop creating ~ backup files
      (auto-save-default nil) ;; Stop creating # auto save files
      :hook
      (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
      :config
      ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
      (setq custom-file (locate-user-emacs-file "custom-vars.el"))
      (load custom-file 'noerror 'nomessage)
      :bind (
             ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
             ;; Zooming In/Out
             ("C-+" . text-scale-increase)
             ("C--" . text-scale-decrease)
             ("<C-wheel-up>" . text-scale-increase)
             ("<C-wheel-down>" . text-scale-decrease)
             )
      )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(provide 'defaults)