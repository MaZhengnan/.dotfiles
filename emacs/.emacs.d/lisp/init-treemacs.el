;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; A tree layout file explorer
(use-package treemacs
  :init
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           t)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :init
  :demand t
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :init
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(use-package treemacs-tab-bar
  :init
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-projectile
  :after (treemacs project)
  :ensure t)

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
