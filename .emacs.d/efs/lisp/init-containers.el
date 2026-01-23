;;; init-containers.el --- Podman/Docker Integration -*- lexical-binding: t -*-

(use-package docker
  :bind ("C-c d" . docker)
  :init
  (setq docker-command "podman")
  ;; Optional: Use podman-remote if you are on macOS connecting to Linux
  ;; (setq docker-command "podman-remote")
  )

;; Tramp support for Podman
(use-package tramp
  :ensure nil
  :config
  ;; Add 'podman' method to Tramp
  (add-to-list 'tramp-methods
               '("podman"
                 (tramp-login-program "podman")
                 (tramp-login-args (("exec") ("-it") ("-u" "%u") ("%h") ("/bin/bash")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-args ("-i") ("-c"))))
  
  ;; Recognize podman containers in completion
  (tramp-set-completion-function "podman"
                                 '((tramp-container--completion-function "podman"))))

(provide 'init-containers)
