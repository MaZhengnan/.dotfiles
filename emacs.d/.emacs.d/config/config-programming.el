;; -*- coding: utf-8; lexical-binding: t; -*-
;;; config-programming.el 
;;; Commentary:
;;; Code:
(use-package treesit :ensure nil)

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :init
  (setq treesit-font-lock-level 4)
  :config
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
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :hook ((c-mode c++-mode ;; Autostart lsp servers for a given mode
                 lua-mode) ;; Lua-mode needs to be installed
         . eglot-ensure)
  :custom
  ;; Good default
  (eglot-events-buffer-size 0) ;; No event buffers (LSP server logs)
  (eglot-autoshutdown t);; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable LSP server logs (Don't show lsp messages at the bottom, java)
  ;; Manual lsp servers
  ;;:config
  ;;(add-to-list 'eglot-server-programs
  ;;             `(lua-mode . ("PATH_TO_THE_LSP_FOLDER/bin/lua-language-server" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
  )

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(provide 'config-programming)
