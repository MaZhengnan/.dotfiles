;; init-eshell.el --- Initialize eshell and vterm configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Eshell and vterm configurations.
;;
;;; Code:

;; Disabling company mode in eshell, because it's annoying.
(defun fancy-shell ()
  "A pretty shell with git status"
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (ref (magit-get-shortname "HEAD"))
         (stat (magit-file-status))
         (x-stat eshell-last-command-status)
         (git-chunk
          (if ref
              (format "%s%s%s "
                      (propertize (if stat "[" "(") 'font-lock-face (list :foreground (if stat "#e81050" "#9bee8b")))
                      (propertize ref 'font-lock-face '(:foreground "#c897ff"))
                      (propertize (if stat "]" ")") 'font-lock-face (list :foreground (if stat "#e81050" "#9bee8b"))))
            "")))
    (propertize
     (format "\n%s %s %s$ "
             (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "#e81050")) x-stat)
               (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "#e81050" "#9bee8b"))))
             (propertize cwd 'font-lock-face '(:foreground "#45babf"))
             git-chunk)
     'read-only t
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only))))

(setq company-global-modes '(not eshell-mode))

;; Adding a keybinding for 'pcomplete-list' on F9 key.
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "<f9>") #'pcomplete-list)))

;; A function for easily creating multiple buffers of 'eshell'.
;; NOTE: `C-u M-x eshell` would also create new 'eshell' buffers.
(defun eshell-new (name)
  "Create new eshell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

  ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  ;; eshell-aliases-file -- sets an aliases file for the eshell.

  ;; (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
  ;;       eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
  ;;       eshell-history-size 5000
  ;;       eshell-buffer-maximum-lines 5000
  ;;       eshell-hist-ignoredups t
  ;;       eshell-scroll-to-bottom-on-input t
  ;;       eshell-destroy-buffer-when-process-dies t
  ;;       eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(setq   eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))


(use-package vterm
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000))


(use-package vterm-toggle
  :after vterm
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(provide 'init-eshell)

;;; init-eshell.el ends here
