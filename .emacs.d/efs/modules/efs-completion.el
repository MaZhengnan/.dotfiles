;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-completion.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires

(efs-require-packages '(vertico
vertico-posframe
corfu
cape
orderless
wgrep
consult
consult-dir
marginalia
embark
embark-consult
nerd-icons
nerd-icons-corfu))

(require 'savehist)
(require 'vertico)
(require 'vertico-posframe)
(require 'corfu)
(require 'cape)
(require 'orderless)
(require 'consult)
(require 'consult-dir)
(require 'marginalia)
(require 'embark)
(require 'nerd-icons)
(require 'nerd-icons-corfu)
;; --- 1. 基础历史记录 ---
(setq history-length 25)
(savehist-mode 1)

;; --- 2. Vertico 配置 ---
(vertico-mode 1)
(setq vertico-cycle t)
;; 自定义当前选中项的背景色
(custom-set-faces '(vertico-current ((t (:background "#3a3f5a")))))

(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)
(define-key vertico-map (kbd "C-f") #'vertico-exit-input)
(define-key vertico-map (kbd "RET") #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
;; 绑定您自定义的删除函数
(define-key minibuffer-local-map (kbd "M-h") #'dw/minibuffer-backward-kill)

;; 先等 vertico 加载，然后在里面等 orderless
(with-eval-after-load 'vertico
  (with-eval-after-load 'orderless nil)

  ;; vertico 的独立配置
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler
        #'posframe-poshandler-frame-center
        vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (width . 0.6)
          (min-width . 60)
          (max-width . 100)
          (height . 0.4)
          (min-height . 10)
          (max-height . 20))))

;; --- 3. Corfu 配置 ---
(setq corfu-cycle t
      corfu-auto t
      corfu-preview-current nil
      corfu-quit-at-boundary t
      corfu-quit-no-match t)

(define-key corfu-map (kbd "TAB") #'corfu-next)
(define-key corfu-map (kbd "<backtab>") #'corfu-previous)
(define-key corfu-map (kbd "S-TAB") #'corfu-previous)
(define-key corfu-map (kbd "RET") #'corfu-insert)
(define-key corfu-map (kbd "C-f") #'corfu-scroll-up)
(define-key corfu-map (kbd "C-b") #'corfu-scroll-down)
(define-key corfu-map (kbd "C-g") #'corfu-quit)
(define-key corfu-map (kbd "ESC") #'corfu-quit)

(with-eval-after-load 'evil
  ;; Bind C-. to the standard Emacs completion command, which Corfu is designed to use.
  (define-key evil-normal-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-visual-state-map (kbd "C-.") 'completion-at-point)
  (define-key evil-motion-state-map (kbd "C-.") 'completion-at-point))

;; Set the global binding as well for non-Evil buffers
(keymap-global-set "C-." 'completion-at-point)
(global-corfu-mode 1)
  ;; Add a list of cape completion functions to the standard Emacs CAPF
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-keyword)

;; --- 3. Cape 配置 ---

;; --- 4. Nerd Icons 配置 (替换 Kind-Icon) ---
;; 注意：首次使用需执行 M-x nerd-icons-install-fonts
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; --- 5. Orderless 配置 ---
(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))

(setq completion-styles '(orderless)
      completion-category-defaults nil
      orderless-matching-styles '(orderless-literal orderless-regexp)
      completion-category-overrides '((file (styles partial-completion))))

;; --- 6. Consult 配置 ---
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-M-l") #'consult-imenu)
(define-key minibuffer-local-map (kbd "C-r") #'consult-history)

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(setq consult-project-root-function #'dw/get-project-root
      completion-in-region-function #'consult-completion-in-region)

;; --- 7. Consult-dir 配置 ---
(global-set-key (kbd "C-x C-d") #'consult-dir)
(define-key vertico-map (kbd "C-x C-d") #'consult-dir)
(define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file)
(setq consult-dir-project-list-function nil)

;; --- 8. Marginalia & Embark ---
(setq marginalia-annotators '(marginalia-annotators-heavy
                                 marginalia-annotators-light
                                 nil))
(marginalia-mode 1)

(global-set-key (kbd "C-M-.") #'embark-act)
(define-key minibuffer-local-map (kbd "C-d") #'embark-act)
(setq prefix-help-command #'embark-prefix-help-command)

;; --- 9. Wgrep ---
(with-eval-after-load 'grep
  (require 'wgrep)
  (add-hook 'grep-mode-hook #'wgrep-setup))

(provide 'efs-completion)
