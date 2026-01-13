;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-ui.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires

(set-face-attribute 'default nil
                    :font "JetBrains Mono NF"
                    :weight 'normal
                    :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono NF"
                    :weight 'normal
                    :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "JetBrains Mono NF"
                    :height 120
                    :weight 'normal)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Make vertical window separators look nicer in terminal Emacs
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(use-package emacs-solo-rainbow-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-keyword-face
                    font-lock-type-face
                    font-lock-function-name-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-builtin-face
                    font-lock-string-face
                    )))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))

;; themes
(efs-require-package 'doom-themes)
(require 'doom-themes)
(load-theme 'doom-one)

;; --- 1. 安装必要的包 ---
;; (efs-require-package 'minions)
;; (efs-require-package 'doom-modeline)
;; (efs-require-package 'nerd-icons) ;; 确保核心图标库已安装

;; ;; --- 2. Minions 配置 (用于管理 Mode-line 上的从属模式) ---
;; (require 'minions)
;; (require 'doom-modeline)
;; (add-hook 'doom-modeline-mode-hook #'minions-mode)

;; ;; --- 3. Doom Modeline 变量设置 ---
;; (setq doom-modeline-height 15
;;       doom-modeline-bar-width 6
;;       doom-modeline-lsp t
;;       doom-modeline-github nil
;;       doom-modeline-mu4e nil
;;       doom-modeline-irc nil
;;       doom-modeline-minor-modes t
;;       doom-modeline-persp-name nil
;;       doom-modeline-buffer-file-name-style 'truncate-except-project
;;       doom-modeline-major-mode-icon nil
;;       ;; 强制指定使用 nerd-icons 而不是 all-the-icons
;;       doom-modeline-icon (display-graphic-p)
;;       doom-modeline-support-imenu t)

;; ;; 设置 Mode-line 字体高度
;; (custom-set-faces
;;  '(mode-line ((t (:height 0.85))))
;;  '(mode-line-inactive ((t (:height 0.85)))))

;; ;; --- 4. 定义并启动 Modeline ---
;; (defun dw/setup-doom-modeline ()
;;   ;; 定义您自定义的默认布局
;;   (doom-modeline-def-modeline 'default
;;     '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;     '(objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

;;   ;; 应用此定义
;;   (doom-modeline-set-modeline 'default t)

;;   ;; 启动模式
;;   (doom-modeline-mode 1))

;; ;; 在初始化完成后启动
;; (add-hook 'after-init-hook #'dw/setup-doom-modeline)


(provide 'efs-ui)
