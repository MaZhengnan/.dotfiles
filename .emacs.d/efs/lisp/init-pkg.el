;;; init-pkg.el --- 包管理器配置 -*- lexical-binding: t -*-

(require 'package)

;; 1. 设置软件源 (MELPA 是最常用的源)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; 2. 初始化包系统
(package-initialize)

;; 3. 自动安装 use-package
;; use-package 是配置 Emacs 插件的神器，如果没安装，自动安装它
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; 4. 默认启用 :ensure t
;; 这样我们在配置插件时，不需要每次都写 :ensure t，它会自动安装缺失的插件
(setq use-package-always-ensure t)

(provide 'init-pkg)
