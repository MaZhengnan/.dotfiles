;;; +ui.el -*- lexical-binding: t; -*-
;;;
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq doom-font (font-spec :family "Iosevka" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 20)
      doom-symbol-font (font-spec :family "Iosevka Fixed" :size 20)
      doom-big-font (font-spec :family "Iosevka Term" :size 40))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Noto Sans Mono CJK SC"))))

(add-hook 'after-setting-font-hook #'my-cjk-font)
(prefer-coding-system 'utf-8)


;; 在终端下使用不同的光标形状 (依赖终端支持 DECSCUSR, Alacritty/Kitty/Wezterm OK)
(use-package! evil-terminal-cursor-changer
  :after evil
  :config
  ;; 仅在终端下启用（GUI 下仍用默认设置）
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))
;;(setq blink-cursor-interval 0.5)
(setq blink-cursor-blinks -1) ; 设置为 0 表示一直闪烁，-1 表示不闪烁

(setq evil-motion-state-cursor 'box      ; 方块
      evil-visual-state-cursor 'hbar     ; 横线
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar      ; 竖线
      evil-replace-state-cursor 'hbar)
