;;; init-tools.el --- Tools and Completion -*- lexical-binding: t -*-

;; ----- Minibuffer Completion (Vertico + Orderless + Marginalia + Consult) -----

;; 1. Vertico: Vertical completion UI for minibuffer
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Vertico Posframe: Center the UI
(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  :config
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8))))

;; 2. Orderless: Fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 3. Marginalia: Annotations
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; 4. Consult: Enhanced commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

;; ----- In-Buffer Completion (Corfu) -----
;; Essential for LSP (Eglot) completion
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-cycle t)                ; Cycle through candidates
  (corfu-preselect 'prompt)      ; Always preselect the prompt
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; Use Nerd Icons in Corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ----- Snippets (YASnippet) -----
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ----- Git (Magit) -----
(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ----- Projectile -----
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default)) ; Use Vertico
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; ----- Restart Emacs -----
(use-package restart-emacs)

(provide 'init-tools)
