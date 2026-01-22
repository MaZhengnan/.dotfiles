;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-programming.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires
;; --- 1. 基础包安装 ---
(efs-require-package 'yasnippet)
(efs-require-package 'yasnippet-snippets)
(efs-require-package 'yasnippet-capf)
(efs-require-package 'sideline-flymake)
(efs-require-package 'nerd-icons-corfu)
;; 语言特定包
(efs-require-package 'rust-mode)
(efs-require-package 'cmake-ts-mode)
(efs-require-package 'verilog-mode)
(efs-require-package 'slime)       ; Common Lisp
(efs-require-package 'geiser-guile) ; Scheme (以 Guile 为例)

;; 代码格式化
(efs-require-package 'apheleia)

;; --- 2. YASnippet 配置 ---
(require 'yasnippet)
(yas-global-mode 1)
(require 'yasnippet-snippets)

;; --- 3. Eglot (LSP) 核心配置 ---
(with-eval-after-load 'eglot
  ;; 配置各语言服务器映射
  (setq eglot-server-programs
        (append '(((c-ts-mode c++-ts-mode cuda-mode) . ("clangd" "--header-insertion=never"))
                  (python-ts-mode . ("pylsp"))
                  (go-ts-mode . ("gopls"))
                  (rust-ts-mode . ("rust-analyzer"))
                  (cmake-ts-mode . ("cmake-language-server"))
                  (verilog-mode . ("verible-verilog-ls" "--format-wait")))
                eglot-server-programs))

  ;; Eglot 优化
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-report-progress nil)

  ;; 自动启动 Eglot 的 Hook
  (defun efs/eglot-ensure-hooks ()
    (dolist (hook '(c-ts-mode-hook
                    c++-ts-mode-hook
                    python-ts-mode-hook
                    go-ts-mode-hook
                    rust-ts-mode-hook
                    cmake-ts-mode-hook
                    cuda-mode-hook))
      (add-hook hook #'eglot-ensure)))
  (efs/eglot-ensure-hooks)


  )

;; --- 4. 补全后端集成 (CAPF) ---
;; 将 YASnippet 整合进补全列表
(require 'yasnippet-capf)
(add-to-list 'completion-at-point-functions #'yasnippet-capf)

;; 强制让 Eglot 使用 nerd-icons-corfu 格式化图标
(require 'nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; --- 5. 特定语言细节配置 ---

;; C/C++/Qt (Qt 开发通常使用 clangd，它能很好识别 Qt 宏)
(setq-default c-basic-offset 4)

;; Rust (使用内置 ts-mode 但依赖 rust-mode 提供的额外工具)
(require 'rust-mode)
(setq rust-mode-treesitter-derive t)

;; Common Lisp (Slime)
(setq inferior-lisp-program "sbcl") ; 确保系统安装了 sbcl
(with-eval-after-load 'slime
  (slime-setup '(inferior-slime-indent slime-fancy slime-company)))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Verilog
(add-hook 'verilog-mode-hook #'eglot-ensure)

;; --- 6. 界面辅助 (Sideline & Flymake) ---
(require 'sideline-flymake)
(setq sideline-flymake-display-mode 'line)
(setq sideline-backends-right '(sideline-flymake))
(add-hook 'flymake-mode-hook #'sideline-mode)



(require 'apheleia)
;; 全局启用：保存文件时自动格式化
(apheleia-global-mode 1)
(with-eval-after-load 'apheleia
  ;; 1. 自定义格式化工具 (如果默认的不符合需求)
  ;; 例如为 C++ 显式指定 clang-format 的风格
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--style=file" "--fallback-style=Google"))

  ;; 2. 映射模式到工具 (Mode -> Formatter)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'cuda-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)
  (setf (alist-get 'cmake-ts-mode apheleia-mode-alist) 'cmake-format)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)

  ;; Verilog 示例 (需要系统安装了 verible)
  (setf (alist-get 'verilog-mode apheleia-mode-alist) 'verible-verilog-format))
;; 在 emacs-lisp-mode 中禁用自动格式化
(add-hook 'emacs-lisp-mode-hook (lambda () (apheleia-mode -1)))
(global-set-key (kbd "C-c f") #'apheleia-format-buffer)


(provide 'efs-programming)
