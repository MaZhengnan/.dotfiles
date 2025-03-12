;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; General programming configurations.
;;
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist mzneon-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))


;; 🚀 Eglot 配置
(use-package eglot
   :hook ((prog-mode . (lambda ()
                         (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                           (eglot-ensure))))
          ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
   :init
   (setq read-process-output-max (* 1024 1024)) ; 1MB
   (setq eglot-autoshutdown t
         eglot-events-buffer-size 0
         eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
   :after consult eglot
   :bind (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols)))

;; 🚀 Treesit 高亮配置
(use-package treesit-auto
  :demand t
  :custom
  ;; 如果未安装解析器，提示安装
  (treesit-auto-install 'prompt)

  :init
  ;; 提升 Treesitter 的语法高亮等级
  (setq treesit-font-lock-level 4)

  :config
  ;; 只添加需要的语言到 auto-mode-alist
  (setq treesit-auto-languages
        '(c cpp python go dockerfile html css cmake javascript typescript))

  ;; 自动切换 major-mode 为 treesit 版本
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
  ;; 开启全局 Treesitter 自动模式
  (global-treesit-auto-mode))



;; 🚀 Eldoc 提示
(use-package eldoc
  :ensure nil
  :diminish
  :config
    (use-package eldoc-box
      :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
      :custom
      (eldoc-box-lighter nil)
      (eldoc-box-only-multi-line t)
      (eldoc-box-clear-with-C-g t)
      :custom-face
      (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
      (eldoc-box-body ((t (:inherit tooltip))))
      :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Prettify `eldoc-box' frame
      (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
            (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

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
