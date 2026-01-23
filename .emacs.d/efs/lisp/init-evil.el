;;; init-evil.el --- Vim 模拟配置 -*- lexical-binding: t -*-

;; ----- Evil 核心 -----
(use-package evil
  :init
  ;; 这些变量必须在加载 evil 之前设置
  (setq evil-want-integration t)  ; 允许与其他包集成
  (setq evil-want-keybinding nil) ; 禁用默认按键绑定 (我们将使用 evil-collection)
  (setq evil-want-C-u-scroll t)   ; 允许使用 C-u 向上滚动
  (setq evil-undo-system 'undo-redo) ; 使用 Emacs 28+ 原生的 undo-redo
  
  :config
  (evil-mode 1)
  
  ;; 在 Insert 模式下，使用 Ctrl-g 快速切回 Normal 模式
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  
  ;; 视觉行移动：对于自动换行的长行，j/k 按视觉行移动而不是逻辑行
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; 设置初始状态
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; ----- Evil Collection -----
;; 为绝大多数 Emacs 内置模式和第三方插件提供 Vim 键位绑定
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ----- Evil Commentary -----
;; 快速注释代码：gcc 注释当前行，gc + motion 注释区域
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; ----- Evil Surround -----
;; 修改包围符号：cs"' (把双引号改成单引号), ysiw] (给单词加上中括号)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
