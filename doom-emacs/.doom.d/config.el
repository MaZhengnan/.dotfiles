;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; code:

(load! "+ui")
(load! "+platform")
(load! "+lang-python")
(load! "+cmake-cpp")
;; (load! "+clipboard")

;; 快速 ESC 配置（jk/kj/jj）
(setq evil-escape-key-sequence "jk"
      evil-escape-delay 0.3   ; 按键间最大间隔秒数
      evil-escape-excluded-major-modes '(vterm-mode eshell-mode term-mode))
(evil-escape-mode 1)

;; ======================
;; Tree-sitter Highlighter
;; ======================
(dolist (hook '(python-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))

(when (and (eq system-type 'gnu/linux)

           (string-match-p "microsoft" (shell-command-to-string "uname -r")))

  (load! "~/.doom.d/+clipboard.el"))

;;; 终端 Evil 模式光标配置
;; (defun my/terminal-cursor-setup ()
;;   "设置终端中的 Evil 模式光标样式."
;;   (when (not (display-graphic-p))

;;     ;; 检查终端是否支持光标样式
;;     (if (and (fboundp 'tty-supports-changing-cursor-p)
;;              (tty-supports-changing-cursor-p))
;;         (progn
;;           ;; 详细的 Evil 状态光标配置
;;           (setq evil-normal-state-cursor '(("green" :background "green") box)
;;                 evil-insert-state-cursor '(("red" :background "red") bar)
;;                 evil-visual-state-cursor '(("orange" :background "orange") hollow)
;;                 evil-motion-state-cursor '(("blue" :background "blue") box)
;;                 evil-emacs-state-cursor '(("magenta" :background "magenta") box)
;;                 evil-replace-state-cursor '(("cyan" :background "cyan") hbar)
;;                 evil-operator-state-cursor '(("yellow" :background "yellow") box))

;;           ;; 设置默认光标
;;           (setq evil-default-cursor evil-normal-state-cursor)

;;           ;; 添加模式切换钩子
;;           (dolist (hook '(evil-insert-state-entry-hook
;;                           evil-normal-state-entry-hook
;;                           evil-visual-state-entry-hook
;;                           evil-emacs-state-entry-hook
;;                           evil-replace-state-entry-hook
;;                           evil-operator-state-entry-hook))
;;             (add-hook hook #'my/update-terminal-cursor))

;;           (message "Terminal cursor styles configured for Evil modes"))

;;       ;; 终端不支持光标样式变化
;;       (message "Terminal does not support cursor style changes"))))

;; ;; 更新光标样式的函数
;; (defun my/update-terminal-cursor ()
;;   "根据当前 Evil 状态更新终端光标."
;;   (let ((cursor (cond
;;                  (evil-insert-state evil-insert-state-cursor)
;;                  (evil-normal-state evil-normal-state-cursor)
;;                  (evil-visual-state evil-visual-state-cursor)
;;                  (evil-emacs-state evil-emacs-state-cursor)
;;                  (evil-replace-state evil-replace-state-cursor)
;;                  (evil-operator-state evil-operator-state-cursor)
;;                  (t evil-default-cursor))))
;;     (when cursor
;;       (setq cursor-type (cadr cursor)))))

;; ;; 在启动时运行
;; (add-hook 'after-init-hook #'my/terminal-cursor-setup)

(use-package! corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))
;;;
