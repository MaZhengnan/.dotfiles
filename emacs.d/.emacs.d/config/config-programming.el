;; -*- coding: utf-8; lexical-binding: t; ;; -*- coding: utf-8; lexical-binding: t; -*-
;;; config-programming.el 
;;; Commentary:
;;; Code:

;; ==================== 1. Treesit 配置 ====================
(use-package treesit :ensure nil)

(use-package treesit-auto
  :ensure t  ; 需要安装，不是内置的
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  ;; 设置主要模式映射
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
  
  ;; 添加到 auto-mode-alist
  (treesit-auto-add-to-auto-mode-alist 'all)
  
  ;; 启用全局模式
  (global-treesit-auto-mode t)

  ;; 测试函数：检查 treesit 是否工作
  (defun my-check-treesit ()
    "检查当前 buffer 是否使用 treesit"
    (interactive)
    (message "当前模式: %s" major-mode)
    (message "是否 treesit 模式: %s" (treesit-parser-list))
    (message "语法节点: %s" (when (treesit-parser-list)
                              (treesit-node-type (treesit-buffer-root-node)))))

  ;; 绑定快捷键检查
  (global-set-key (kbd "C-c t") 'my-check-treesit))

;; ==================== 2. Eglot 配置 ====================
(use-package eglot
  :ensure nil  ; eglot 是内置的
  :hook
  ;; 注意：这里要用 ts-mode，不是原来的 mode
  ((c-ts-mode c++-ts-mode python-ts-mode go-ts-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)       ; 不显示事件缓冲区
  (eglot-autoshutdown t)             ; 自动关闭不用的服务器
  (eglot-report-progress nil)        ; 不显示 LSP 进度信息
  :config
  ;; 配置语言服务器 - 这里才是正确的位置
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp")))  ; 或者 "pyright"
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls")))
  
  ;; 诊断函数：检查 eglot 状态
  (defun my-check-eglot ()
    "检查 eglot 状态"
    (interactive)
    (message "当前模式: %s" major-mode)
    (message "Eglot 管理状态: %s" (bound-and-true-p eglot--managed-mode))
    (message "当前服务器: %s" (eglot-current-server))
    (message "服务器程序列表: %s" eglot-server-programs))

  ;; 如果 eglot 没有自动启动，手动启动
  (defun my-ensure-eglot ()
    "确保 eglot 已启动"
    (interactive)
    (unless (eglot-current-server)
      (call-interactively #'eglot)))
  
  ;; 绑定检查快捷键
  (global-set-key (kbd "C-c e") 'my-check-eglot)
  (global-set-key (kbd "C-c C-l") 'my-ensure-eglot))

;; ==================== 3. 安装语言服务器 ====================
;; 运行这些命令安装必要的语言服务器

(defun my-install-language-servers ()
  "安装常用的语言服务器"
  (interactive)
  (message "请确保已安装以下语言服务器:")
  (message "1. C/C++: clangd (从 LLVM 官网下载)")
  (message "2. Python: pip install python-lsp-server")
  (message "3. Go: go install golang.org/x/tools/gopls@latest")
  (message "4. JavaScript: npm install -g typescript typescript-language-server"))

;; ==================== 4. 辅助函数 ====================
(use-package yasnippet-snippets
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

;; ==================== 5. 自动修复 eglot 启动问题 ====================
(defun my-auto-fix-eglot ()
  "自动修复 eglot 启动问题"
  (when (and (memq major-mode '(c-ts-mode c++-ts-mode python-ts-mode go-ts-mode))
             (not (bound-and-true-p eglot--managed-mode)))
    ;; 延迟启动，避免冲突
    (run-with-timer 0.5 nil
                    (lambda ()
                      (condition-case err
                          (eglot-ensure)
                        (error (message "Eglot 启动失败: %s" err)))))))

;; 添加多个钩子确保触发
(add-hook 'find-file-hook 'my-auto-fix-eglot)
(add-hook 'after-change-major-mode-hook 'my-auto-fix-eglot)

(provide 'config-programming)
