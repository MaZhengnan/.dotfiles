;;; init-files.el --- File Explorer (Dirvish) -*- lexical-binding: t -*-

(use-package dirvish
  :init
  (dirvish-override-dired-mode) ; Replace Dired
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("c" "~/.dotfiles/.emacs.d/efs/"   "Config")
     ("p" "~/Projects/"                 "Projects")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  :config
  ;; Enable mouse drag-and-drop
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  
  ;; Sidebar layout (Treemacs-like)
  (setq dirvish-side-width 30)
  (setq dirvish-side-display-alist `((side . left) (slot . -1)))
  (dirvish-side-follow-mode) ; Auto-follow current file
  
  :bind
  ;; Bindings for Dirvish
  (("C-c f" . dirvish-side)
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dired-up-directory)
   ("l"   . dired-find-file)
   ("s"   . dirvish-quicksort)
   ("v"   . dirvish-vc-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :config
  ;; Fix for TAB key in Evil mode and ensure navigation works
  (with-eval-after-load 'evil
    (evil-define-key 'normal dirvish-mode-map
      (kbd "TAB") 'dirvish-subtree-toggle
      (kbd "<backtab>") 'dirvish-subtree-toggle
      (kbd "h") 'dired-up-directory
      (kbd "l") 'dired-find-file
      (kbd "q") 'dirvish-quit)))

(provide 'init-files)
