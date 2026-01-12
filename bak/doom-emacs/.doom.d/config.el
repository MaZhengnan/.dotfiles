;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; code:
(load! "+ui")
(load! "+platform")
(load! "+lang-python")
(load! "+cmake-cpp")
(load! "+debugger")

;; 快速 ESC 配置（jk/kj/jj）
(setq evil-escape-key-sequence "jk"
      evil-escape-delay 0.3   ; 按键间最大间隔秒数
      ;; evil-escape-excluded-major-modes '(vterm-mode eshell-mode term-mode)
      )
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

(use-package! corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))
;;;
