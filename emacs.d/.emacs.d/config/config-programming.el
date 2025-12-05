;;; -*- lexical-binding: t; -*-
;;; config-programming.el 
;;; Commentary:
;;; Code:
;; ==================== 配置 Scoop MinGW 给 Tree-sitter ====================

(defun my/setup-scoop-mingw-for-treesit ()
  "配置 Scoop 安装的 MinGW 供 Tree-sitter 使用"
  (interactive)
  
  ;; 1. 查找 Scoop 路径（常见位置）
  (let ((scoop-home (or (getenv "SCOOP") "~/scoop"))
        (mingw-paths '("apps/mingw/current"
                       "apps/gcc/current"
                       "apps/mingw-w64/current"
                       "shims")))
    
    ;; 2. 添加到 PATH
    (dolist (subpath mingw-paths)
      (let ((mingw-path (expand-file-name subpath scoop-home)))
        (when (file-directory-p mingw-path)
          ;; 添加到 PATH 环境变量
          (setenv "PATH" (concat mingw-path ";" (getenv "PATH")))
          ;; 添加到 exec-path（Emacs 内部使用）
          (add-to-list 'exec-path mingw-path))))
    
    ;; 3. 设置编译器标志
    (setenv "CFLAGS" "-std=c11 -O2")
    (setenv "CXXFLAGS" "-std=c++11 -O2")
    (setenv "CC" "gcc")
    (setenv "CXX" "g++")
    
    ;; 4. 测试编译器
    (message "检查 GCC 版本: %s" 
             (shell-command-to-string "gcc --version | head -1"))
    (message "PATH: %s" (getenv "PATH"))))

(my/setup-scoop-mingw-for-treesit)

;; (setenv "CFLAGS" "-std=c99")
;; (setenv "CXXFLAGS" "-std=gnu++11")
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun start/install-treesit-grammars ()
  "Install missing treesitter grammars"
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

;; Call this function to install missing grammars
(start/install-treesit-grammars)

;; Optionally, add any additional mode remappings not covered by defaults
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (mhtml-mode . html-ts-mode)
        (javascript-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        ))

;; Or if there is no built in mode
(use-package cmake-ts-mode :ensure nil :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package go-ts-mode :ensure nil :mode "\\.go\\'")
(use-package go-mod-ts-mode :ensure nil :mode "\\.mod\\'")
(use-package rust-ts-mode :ensure nil :mode "\\.rs\\'")
(use-package tsx-ts-mode :ensure nil :mode "\\.tsx\\'")



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
