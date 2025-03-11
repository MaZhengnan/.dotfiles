;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; General programming configurations.
;;
;;; Code:

;; ============================
;; 🚀 高性能 LSP 配置
;; ============================
(use-package eglot
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (unless (bound-and-true-p eglot--managed-mode)
                   (eglot-ensure))))
  :config
  ;; 提升 Eglot 性能
  (use-package eglot-booster
    :ensure t
    :config
    (eglot-booster-mode))

  ;; 自动格式化代码
  (add-hook 'before-save-hook 'eglot-format-buffer)
  :init
  (setq-default prettify-symbols-alist mzneon-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; ============================
;; 🚀 Treesit 高亮配置
;; ============================
(use-package treesit-auto
  :demand t
  :custom
  ;; 📜 如果未安装解析器，提示安装
  (treesit-auto-install 'prompt)

  :init
  ;; 📜 提升 Treesitter 的语法高亮等级
  (setq treesit-font-lock-level 4)

  :config
  ;; 📜 只添加需要的语言到 auto-mode-alist
  (setq treesit-auto-languages
        '(c cpp python go dockerfile html css cmake javascript typescript))


  ;; 📜 自动切换 major-mode 为 treesit 版本
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (python-mode     . python-ts-mode)
          (go-mode         . go-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (html-mode       . html-ts-mode)
          (css-mode        . css-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (js-mode         . js-ts-mode)
          (typescript-mode . typescript-ts-mode)))
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; ✅ 开启全局 Treesitter 自动模式
  (global-treesit-auto-mode))
;; ============================
;; 🚀 Consult-Eglot 配置
;; ============================
(use-package consult-eglot
  :ensure t
  :after eglot
  :bind
  (("M-." . eglot-find-definition)
   ("M-?" . eglot-find-references)
   ("M-r" . eglot-rename)
   ("C-c f" . eglot-format-buffer)))

;; ============================
;; 🚀 Eldoc 提示
;; ============================
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-hook 'eglot-managed-mode-hook #'eldoc-mode))

;; ============================
;; 🚀 Xref 配置
;; ============================
(use-package xref
  :ensure t
  :config
  (setq xref-history-storage 'xref-window-local-history))

;; ============================
;; 🚀 EditorConfig 配置
;; ============================
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; ============================
;; 🚀 LSP Server 配置
;; ============================
(setq eglot-server-programs
      '((c-mode . ("clangd"))
        (c++-mode . ("clangd"))
        (python-mode . ("pyright"))
        (go-mode . ("gopls"))
        (css-mode . ("vscode-css-language-server" "--stdio"))
        (html-mode . ("vscode-html-language-server" "--stdio"))
        (dockerfile-mode . ("docker-langserver" "--stdio"))
        (cmake-mode . ("cmake-language-server"))
        (typescript-mode . ("typescript-language-server" "--stdio"))
        (javascript-mode . ("typescript-language-server" "--stdio"))))
;; ============================
;; 🚀 自动保存 + 格式化
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

;; ============================
;; 🚀 Dockerfile 支持
;; ============================
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; ============================
;; 🚀 CMake 支持
;; ============================
(use-package cmake-mode
  :ensure t)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
