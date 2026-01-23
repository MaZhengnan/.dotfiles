;;; init-keys.el --- Keybindings -*- lexical-binding: t -*-

(use-package general
  :config
  ;; ----- Leader Key Definition -----
  (general-create-definer leader-keys
    :states '(normal visual motion insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; ----- Keybindings (English Descriptions) -----
  (leader-keys
    ;; Top level
    "SPC" '(execute-extended-command :which-key "M-x")
    "."   '(find-file :which-key "Find file")
    "TAB" '(comment-line :which-key "Comment/Uncomment line")
    
    ;; b - Buffer
    "b"  '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "br" '(revert-buffer :which-key "Revert buffer")
    "bs" '(save-buffer :which-key "Save buffer")
    
    ;; c - Code
    "c"  '(:ignore t :which-key "Code")
    "cf" '(apheleia-format-buffer :which-key "Format buffer")

    ;; d - Docker/Podman
    "d"  '(:ignore t :which-key "Docker/Podman")
    "dd" '(docker :which-key "Docker Menu")
    "dc" '(docker-containers :which-key "List Containers")
    "di" '(docker-images :which-key "List Images")

    ;; e - Explorer
    "e" '(dirvish-side :which-key "Toggle Explorer")
    
    ;; f - File
    "f"  '(:ignore t :which-key "File")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(recentf-open-files :which-key "Recent files")
    "fc" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Open config")
    
    ;; w - Window
    "w"  '(:ignore t :which-key "Window")
    "wm" '(delete-other-windows :which-key "Maximize")
    "w/" '(split-window-right :which-key "Split right")
    "w-" '(split-window-below :which-key "Split below")
    "wd" '(delete-window :which-key "Delete window")
    "ww" '(other-window :which-key "Next window")
    "wh" '(evil-window-left :which-key "Window left")
    "wl" '(evil-window-right :which-key "Window right")
    "wk" '(evil-window-up :which-key "Window up")
    "wj" '(evil-window-down :which-key "Window down")
    "w=" '(balance-windows :which-key "Balance windows")
    
    ;; q - Quit
    "q"  '(:ignore t :which-key "Quit")
    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qr" '(restart-emacs :which-key "Restart Emacs")
    
    ;; g - Git
    "g"  '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "Magit status")
    
    ;; o - Open / Org
    "o"  '(:ignore t :which-key "Open/Org")
    "oa" '(org-agenda :which-key "Org Agenda")
    
    ;; p - Project
    "p"  '(:ignore t :which-key "Project")
    "pf" '(projectile-find-file :which-key "Find file in project")
    "ps" '(projectile-switch-project :which-key "Switch project")
    "pr" '(projectile-replace :which-key "Replace in project")
    
    ;; h - Help
    "h"  '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "Describe function")
    "hv" '(describe-variable :which-key "Describe variable")
    "hk" '(describe-key :which-key "Describe key")
    
    ;; t - Toggle
    "t"  '(:ignore t :which-key "Toggle")
    "tl" '(display-line-numbers-mode :which-key "Toggle line numbers")))

(provide 'init-keys)
