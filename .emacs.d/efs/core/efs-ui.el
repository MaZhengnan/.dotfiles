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

(defun efs-ayu-dark-style ()
  (interactive)
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-common-palette-overrides
          `((bg-main "#0F111B")
            (bg-active bg-main)
            (fg-main "#C3CCDF")
            (fg-active fg-main)
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-mode-line-active "#B3B1AD")
            (bg-mode-line-active "#171B27")
            (fg-mode-line-inactive "#65737E")
            (bg-mode-line-inactive "#1C1F29")
            (bg-tab-bar      "#1C1F29")
            (bg-tab-current  bg-main)
            (bg-tab-other    "#171B27")
            (fg-prompt "#F6C177")
            (bg-prompt unspecified)
            (bg-hover-secondary "#65737E")
            (bg-completion "#2f447f")
            (fg-completion "#ffffff")
            (bg-region "#2B2E36")
            (fg-region "#ffffff")

            ;; Heading colors
            (fg-heading-0 "#81A1C1")
            (fg-heading-1 "#81A1C1")
            (fg-heading-2 "#F6C177")
            (fg-heading-3 "#FFB974")
            (fg-heading-4 "#C792EA")

            (fg-prose-verbatim "#A3BE8C")
            (bg-prose-block-contents "#171B27")
            (fg-prose-block-delimiter "#65737E")
            (bg-prose-block-delimiter "#171B27")

            (accent-1 "#7FDBCA")

            (keyword   "#F6C177")
            (builtin   "#81A1C1")
            (comment   "#65737E")
            (string    "#A3BE8C")
            (fnname    "#7FDBCA")
            (type      "#C792EA")
            (variable  "#FFB974")
            (docstring "#8996A2")
            (constant  "#F07178"))))

(defun efs-palenight-style ()
  (interactive)
  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-common-palette-overrides
          `((bg-main "#292D3E")
            (bg-active bg-main)
            (fg-main "#EEFFFF")
            (fg-active fg-main)
            (fringe unspecified)
            (border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
            (fg-mode-line-active "#A6Accd")
            (bg-mode-line-active "#232635")
            (fg-mode-line-inactive "#676E95")
            (bg-mode-line-inactive "#282c3d")
            (bg-tab-bar      "#242837")
            (bg-tab-current  bg-main)
            (bg-tab-other    bg-active)
            (fg-prompt "#c792ea")
            (bg-prompt unspecified)
            (bg-hover-secondary "#676E95")
            (bg-completion "#2f447f")
            (fg-completion white)
            (bg-region "#3C435E")
            (fg-region white)

            (fg-heading-0 "#82aaff")
            (fg-heading-1 "#82aaff")
            (fg-heading-2 "#c792ea")
            (fg-heading-3 "#bb80b3")
            (fg-heading-4 "#a1bfff")

            (fg-prose-verbatim "#c3e88d")
            (bg-prose-block-contents "#232635")
            (fg-prose-block-delimiter "#676E95")
            (bg-prose-block-delimiter bg-prose-block-contents)

            (accent-1 "#79a8ff")

            (keyword "#89DDFF")
            (builtin "#82aaff")
            (comment "#676E95")
            (string "#c3e88d")
            (fnname "#82aaff")
            (type "#c792ea")
            (variable "#ffcb6b")
            (docstring "#8d92af")
            (constant "#f78c6c"))))

(use-package modus-themes
  :ensure nil
  :demand t
  :init
  (load-theme 'modus-vivendi-tinted t)
  (efs-ayu-dark-style))

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
