;; init-project.el --- Initialize project configurations.	-*- lexical-binding: t -*-
;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Project configurations like projectile.
;;
;;; Code:
;; 🚀 Projectile 配置
(use-package projectile
  :demand t
  :diminish projectile-mode
  :bind-keymap
  ;; ("C-c p" . projectile-command-map)  ;; 绑定快捷键前缀
  :custom
  (projectile-enable-caching t)  ;; 启用缓存，加速文件搜索
  (projectile-completion-system 'auto)  ;; 自动选择补全系统
  (projectile-sort-order 'recentf)  ;; 最近访问的文件优先
  (projectile-globally-ignored-directories '(".git" "node_modules" "target" "vendor"))
  :init
  (projectile-mode 1)  ;; 启动 projectile
  :config
  (setq projectile-project-search-path '("~/projects/" "~/work/"))  ;; 定义项目搜索路径
  (setq projectile-indexing-method 'alien)  ;; 使用最快的外部索引（rg/fd）
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden --follow --glob '!.git/'"))
  (when (executable-find "fd")
    (setq projectile-generic-command "fd --type f --hidden --follow --exclude .git"))
  (setq projectile-switch-project-action #'projectile-dired))  ;; 切换项目后打开 dired 目录

(use-package rg
  :ensure t
  :init)

;; 🚀 结合 Consult 增强 Projectile（可选）
(use-package consult-projectile
  :after projectile)



(provide 'init-project)

;;; init-project.el ends here
