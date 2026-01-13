;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-common.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires

;;; evil-visualstar enables searching visual selection with *
;;; evil-numbers enables vim style numeric incrementing and decrementing


;;(require 'evil-visualstar)

;;(setq evil-mode-line-format 'before)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-insert-state-cursor '("gray" bar))
(setq evil-motion-state-cursor '("gray" box))

;; ============================================
;; ~/.emacs.d/personal/evil-config.el
;; 完整的 Evil 配置（适配 Prelude）
;; ============================================

;; 1. 让 Prelude 安装所有 Evil 相关包
(efs-require-packages '(evil
                            evil-collection
                            evil-commentary
                            evil-escape
                            evil-org
                            evil-surround
                            evil-visualstar
                            evil-anzu))

;; ==================== 核心 Evil 配置 ====================
(setq evil-want-keybinding nil)      ; 禁用其他模式的自动绑定（由 evil-collection 处理）
(require 'evil)
(evil-mode 1)

;; 基本设置
;;(setq evil-want-keybinding nil)      ; 禁用其他模式的自动绑定（由 evil-collection 处理）
(setq evil-want-C-u-scroll t)        ; C-u 向上滚动（Vim 风格）
(setq evil-want-C-i-jump nil)        ; 禁用 C-i 跳转（避免与 Tab 冲突）
(setq evil-undo-system 'undo-redo)   ; 使用 Emacs 的 undo-redo 系统
(setq evil-search-module 'evil-search) ; 使用 Evil 的搜索模块
(setq evil-ex-complete-emacs-commands nil) ; 不补全 Emacs 命令

;; 初始状态设置
(dolist (mode '(help-mode
                info-mode
                compilation-mode
                debugger-mode
                flycheck-error-list-mode
                proced-mode))
  (evil-set-initial-state mode 'emacs))  ; 这些模式用 Emacs 状态

;; 解除特定键绑定（防止与 Prelude 冲突）
(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd "TAB") nil)

;; ==================== evil-collection ====================
(require 'evil-collection)
;; 配置要启用 Evil 绑定的模式（可以根据需要调整）
(setq evil-collection-mode-list
      '(dired
        ibuffer
        magit
        magit-status
        magit-log
        magit-blob
        magit-diff
        consult
        vertico
        corfu
        company
        flycheck
        flymake
        help
        info
        vc-annotate
        calendar
        custom
        debug
        diff
        elfeed
        embark
        ert
        eshell
        eval
        geiser
        guix
        lispy
        kotlin
        nov
        password-store
        pdf
        proced
        profiler
        quickrun
        racer
        racket
        rfcview
        ruby-test
        simple
        slime
        speedbar
        tab-bar
        tablist
        term
        transmission
        typescript
        vterm
        wdired
        wgrep
        xref
        youtube-dl))
(evil-collection-init)

;; ==================== evil-surround (包围操作) ====================
(require 'evil-surround)
(global-evil-surround-mode 1)

;; 可选：自定义包围对
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)

;; ==================== evil-commentary (快速注释) ====================
(require 'evil-commentary)
(evil-commentary-mode)

;; 常用键绑定（默认已绑定）：
;; gc{motion} - 注释/反注释区域
;; gcc - 注释/反注释当前行
;; gcG - 注释到文件末尾

;; ==================== evil-escape (快速退出插入模式) ====================
(require 'evil-escape)
(setq-default evil-escape-key-sequence "jk")  ; 按 j 然后快速按 k
(setq evil-escape-delay 0.2)                  ; 延迟时间（秒）
(evil-escape-mode 1)

;; ==================== evil-visualstar (可视模式搜索) ====================
(require 'evil-visualstar)
(global-evil-visualstar-mode)

;; 使用方法：
;; 1. 进入可视模式 (v/V/C-v)
;; 2. 选择文本
;; 3. 按 * 向前搜索选中文本，按 # 向后搜索

;; ==================== evil-org (Org mode 集成) ====================
;; 注意：evil-org 有时与新版 Org mode 不兼容
;; (when (package-installed-p 'evil-org)
;;   (require 'evil-org)
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (evil-org-set-key-theme '(textobjects insert navigation additional))
  
;;   ;; 解决常见问题：Tab 键冲突
;;   (define-key evil-org-mode-map (kbd "TAB") nil)
;;   (define-key evil-org-mode-map (kbd "<tab>") nil))

;; ==================== evil-anzu (替换计数) ====================
;; 注意：需要先安装 anzu 包（Prelude 默认已安装）
(when (package-installed-p 'anzu)
  (require 'evil-anzu)
  (global-anzu-mode +1)
  (setq anzu-mode-line-update-function 'evil-anzu-update-mode-line))

;; ==================== 自定义键绑定 ====================

;; 在 normal 模式下使用 C-c 作为前缀（类似 Emacs）
(define-key evil-normal-state-map (kbd "C-c") nil)

;; 更方便的窗口导航（结合 avy）
(when (package-installed-p 'ace-window)
  (define-key evil-normal-state-map (kbd "C-w w") 'ace-window)
  (define-key evil-normal-state-map (kbd "C-w C-w") 'ace-window))

;; 更好的跳转（结合 avy）
(when (package-installed-p 'avy)
  (define-key evil-normal-state-map (kbd "gs") 'avy-goto-char)
  (define-key evil-normal-state-map (kbd "gl") 'avy-goto-line)
  (define-key evil-normal-state-map (kbd "gw") 'avy-goto-word-1))

;; 快速保存（替代 C-x C-s）
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)

;; 快速退出（替代 C-x C-c）
(define-key evil-normal-state-map (kbd "C-q") 'save-buffers-kill-terminal)

;; ==================== 模式特定配置 ====================

;; 在 term/eshell 中使用插入模式
(evil-set-initial-state 'term-mode 'insert)
(evil-set-initial-state 'eshell-mode 'insert)
(evil-set-initial-state 'eat-mode 'insert)  ; 如果你用 eat 终端

;; Magit 配置
(with-eval-after-load 'magit
  ;; 在 magit 中使用 Emacs 状态或调整键绑定
  (define-key magit-status-mode-map (kbd "j") 'magit-next-line)
  (define-key magit-status-mode-map (kbd "k") 'magit-previous-line))

;; ==================== 解决常见冲突 ====================
(setq evil-collection-unimpaired-want-updown-p nil)
(setq evil-collection-unimpaired-want-nextprevious-p nil)

;; ==================== 性能优化 ====================

;; 延迟加载某些组件（如果启动慢）
(run-with-idle-timer 1 nil (lambda ()
                             (require 'evil-collection)
                             (evil-collection-init)))


(provide 'efs-evil)
