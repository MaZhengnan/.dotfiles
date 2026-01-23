;;; init-prog.el --- Programming Languages & LSP -*- lexical-binding: t -*-

;; ----- Treesitter (Better Syntax Highlighting) -----
;; Emacs 29+ has built-in support.
;; treesit-auto automatically installs grammars and configures modes.

(use-package treesit
  :when (fboundp 'treesit-available-p)
  :ensure nil ;; It's built-in!
  :mode (("\\.tsx\\'" . tsx-ts-mode)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ----- Eglot (LSP Client) -----
;; Lightweight, built-in (Emacs 29+), fast.
(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (cuda-mode . eglot-ensure))
  :config
  ;; Optimization for performance
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect 0)
  
  ;; Eglot Booster (if using eglot-booster external tool)
  ;; (use-package eglot-booster :ensure t :config (eglot-booster-mode))
  )

;; ----- Language Specifics -----

;; Python
(use-package python
  :mode ("\\.py\\'" . python-ts-mode))

;; Jupyter Notebooks (Code Cells)
;; Treating notebooks as code with special separators, works great with Eglot
(use-package code-cells
  :init
  (add-hook 'python-ts-mode-hook 'code-cells-mode-maybe)
  :config
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    (define-key map (kbd "C-c C-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-p") 'code-cells-backward-cell)))

;; C/C++
;; Eglot will automatically use 'clangd' if installed.

;; CMake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; CUDA
(use-package cuda-mode
  :mode "\\.cu\\'")

;; Rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;; Embedded
;; Usually relies on C/C++ mode. PlatformIO can be added if requested.
;; For now, standard C++ support covers most embedded cases.

;; ----- Code Formatting (Apheleia) -----
(use-package apheleia
  :init (apheleia-global-mode +1))

(provide 'init-prog)
