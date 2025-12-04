;;; my-config.el --- 我的配置
;;; Commentary:
;;; Code:

;; 添加这一行，启用词法绑定
;;; -*- lexical-binding: t; -*-

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; 移除这行 - 不需要单独 require use-package-ensure
;; (require 'use-package-ensure)

;; 安装 use-package（如果尚未安装）
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'packages)