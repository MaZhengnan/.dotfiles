;;; ../.dotfiles/doom-emacs/.doom.d/+debugger.el -*- lexical-binding: t; -*-
;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; ---------------------
;; DAP (调试器) 配置
;; ---------------------
(after! dap-mode
  ;; 调试界面
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  ;; 自动加载工程目录下的 launch.json
  (require 'dap-launch)
  (setq dap-launch-json-template-path (list ".")) ;; 当前工程根目录

  ;; C++ 支持
  (require 'dap-lldb)    ;; 需要 lldb-vscode 或 codelldb
  ;; (require 'dap-gdb-lldb) ;; 如果想用 gdb

  ;; Python 支持
  (require 'dap-python)  ;; 需要 debugpy
  (setq dap-python-debugger 'debugpy))


;; (after! dape
;;   ;; 自动加载当前工程目录的 launch.json
;;   (setq dape-configs
;;         (append dape-configs
;;                 (dape-config-from-json (expand-file-name "launch.json" (projectile-project-root))))))
