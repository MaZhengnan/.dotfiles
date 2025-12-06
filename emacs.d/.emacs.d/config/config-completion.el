;; -*- coding: utf-8; lexical-binding: t; -*-
;;; completion.el 

;; 基础补全框架
(use-package vertico
  :init
  (vertico-mode)
  :bind
  (:map minibuffer-local-map
        ("C-j" . next-line)
        ("C-k" . previous-line)
        :map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

;; 保存历史
(savehist-mode)

;; 顺序很重要：orderless 应该在 vertico 之后
(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; posframe 支持
(use-package vertico-posframe
  :ensure t
  :after (vertico orderless)
  :init
  (vertico-posframe-mode)
  :config
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

;; 注释增强
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; 图标支持
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

;; 补全框架
(use-package corfu
  :ensure t
  :after orderless
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  (corfu-separator ?\s)
  (completion-ignore-case t)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode))

;; Corfu 图标支持
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 补全后端
(use-package cape
  :ensure t
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package consult
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :bind
    ("C-s" . consult-line)
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :init
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    ;; (consult-customize
    ;; consult-theme :preview-key '(:debounce 0.2 any)
    ;; consult-ripgrep consult-git-grep consult-grep
    ;; consult-bookmark consult-recent-file consult-xref
    ;; consult--source-bookmark consult--source-file-register
    ;; consult--source-recent-file consult--source-project-recent-file
    ;; :preview-key "M-."
    ;; :preview-key '(:debounce 0.4 any))

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
        ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
        ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
        ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
        ;;;; 4. projectile.el (projectile-project-root)
    ;;(autoload 'projectile-project-root "projectile")
    ;;(setq consult-project-function (lambda (_) (projectile-project-root)))
        ;;;; 5. No project support
    ;; (setq consult-project-function nil)
    )

(provide 'config-completion)
