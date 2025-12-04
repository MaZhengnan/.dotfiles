;;; my-config.el --- 我的配置
;;; Commentary:
;;; Code:

;; 添加这一行，启用词法绑定
;;; -*- lexical-binding: t; -*-

;; Customer Dashboard
(defun my/simple-dashboard ()
    "A minimal dashboard with evil navigation and RET to open items."
    (interactive)
    (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
        (let ((inhibit-read-only t))
        (erase-buffer)

        ;; 标题
        (insert "=== Welcome to Emacs ===\n\n")

        ;; Recent Files
        (insert "Recent Files:\n")
        (dolist (f (seq-take recentf-list 5))
            (insert (propertize (format "  %s\n" f)
                                'dashboard-type 'file
                                'dashboard-path f)))

        ;; Projects
        (insert "\nProjects:\n")
        (dolist (p (seq-take (project-known-project-roots) 5))
            (insert (propertize (format "  %s\n" p)
                                'dashboard-type 'project
                                'dashboard-path p)))

        (read-only-mode 1)))

    ;; 切换到 dashboard
    (switch-to-buffer buf)

    ;; 开启 evil-normal-state 让你 j/k 移动
    (when (fboundp 'evil-normal-state)
        (evil-normal-state))

    ;; 绑定回车
    (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") #'my/dashboard-activate-line)
        (use-local-map map))
    buf))

(defun my/dashboard-activate-line ()
    "Open file or project under cursor."
    (interactive)
    (let* ((pos (point))
            (type (get-text-property pos 'dashboard-type))
            (path (get-text-property pos 'dashboard-path)))
    (cond
        ((and (equal type 'file) path)
        (find-file path))
        ((and (equal type 'project) path)
        (project-switch-project path))
        (t
        (message "This line is not selectable.")))))

;; 设置 Emacs 启动打开 dashboard
(setq initial-buffer-choice #'my/simple-dashboard)


;;Doom Modeline A fancy, fast and customizable mode-line.
(use-package doom-modeline
    :custom
    (doom-modeline-height 25) ;; Set modeline height
    :hook (after-init . doom-modeline-mode))


;; Nerd Icons
;; This is an icon set that can be used with dired, ibuffer and other Emacs packages.
;; Don't forget nerd-icons-install-fonts to install the resource fonts.

;; We use nerd-icons because it supports both GUI and TUI unlike all-the-icons.
;; Also Doom modeline requires nerd icons.
(use-package nerd-icons
    :if (display-graphic-p))

(use-package nerd-icons-dired
    :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;Set catppuccin theme
(use-package catppuccin-theme
    :config
    (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
    (catppuccin-reload)) ;; We need to add t to trust this package

;; Set fonts
(set-face-attribute 'default nil
                    :font "JetBrains Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 120
                    :weight 'medium)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)



;;With Emacs version 29, true transparency has been added.
(add-to-list 'default-frame-alist '(alpha-background . 90)) ;; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'appearance)