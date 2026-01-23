;;; init-base.el --- 基础设置 -*- lexical-binding: t -*-

;; ----- 界面净化 (极简主义) -----
(setq inhibit-startup-message t)   ; 关闭启动画面
(menu-bar-mode -1)                 ; 关闭菜单栏
(tool-bar-mode -1)                 ; 关闭工具栏
(scroll-bar-mode -1)               ; 关闭滚动条
(tooltip-mode -1)                  ; 关闭鼠标提示框

;; ----- 基础体验 -----
(setq visible-bell t)              ; 使用视觉响铃（屏幕闪烁）代替声音
(setq ring-bell-function 'ignore)  ; 彻底关闭讨厌的蜂鸣声
(column-number-mode)               ; 在底部状态栏显示列号
(global-display-line-numbers-mode t) ; 全局显示行号

;; 设置行号类型为相对行号 (对 Vim 用户非常友好，方便 j/k 跳转)
(setq display-line-numbers-type 'relative) 

;; 在某些不需要行号的模式中禁用它
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ----- 文件与备份管理 -----
;; Emacs 默认会生成很多 ~ 结尾的备份文件和 # 开头的自动保存文件，很乱
(setq make-backup-files nil)       ; 不生成备份文件 (~结尾)
(setq auto-save-default nil)       ; 不生成自动保存文件 (#开头)
(setq create-lockfiles nil)        ; 不生成锁文件 (.#开头)

;; ----- 编码设置 (UTF-8) -----
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; ----- 缩进设置 (2空格) -----
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil) ; 使用空格代替 Tab

;; ----- 其他实用设置 -----
;; 文件被外部修改时自动刷新
(global-auto-revert-mode t)

;; ESC 键可以退出各种 prompts (虽然有了 Evil，这个依然有用)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; 记住上次打开文件时的光标位置
(save-place-mode 1)

;; ----- 自动补全括号 -----
(electric-pair-mode 1) ; 自动补全成对的括号

;; ----- 内置功能增强 -----

;; savehist: 持久化 Minibuffer 历史记录
(use-package savehist
  :init
  (savehist-mode))

;; recentf: 记录最近打开的文件
(use-package recentf
  :init
  (recentf-mode)
  :config
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25))

(provide 'init-base)
