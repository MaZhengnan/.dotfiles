;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;; Optimize GC during startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Define lisp directory based on user-emacs-directory
;; This ensures we use the correct logical path regardless of symlinks
(defvar my-lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "Directory for modular configuration.")

;; Add lisp directory to load-path
(add-to-list 'load-path my-lisp-dir)

;; ----- Load Modules -----

(require 'init-pkg)   ; 1. Package Management
(require 'init-base)  ; 2. Basic Settings
(require 'init-ui)    ; 3. UI, Theme, Fonts (Nerd Icons)
(require 'init-evil)  ; 4. Vim Emulation
(require 'init-files) ; 5. File Explorer (Dirvish)
(require 'init-keys)  ; 6. Keybindings (English)
(require 'init-tools) ; 7. Tools (Vertico Center, Magit)
(require 'init-prog)  ; 8. Programming (LSP, Treesitter, Formatting)
(require 'init-org)   ; 9. Org Mode
(require 'init-containers) ; 10. Docker/Podman


;; ----- Startup Hooks -----

;; Restore GC threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; Display startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
