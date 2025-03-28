;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-
;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Dashboard configurations.
;;
;;; Code:
(eval-when-compile
  (require 'init-custom))

(use-package dashboard
  ;;:ensure t
  :diminish dashboard-mode
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face normal)))))
  (dashboard-items-face ((t (:weight light))))
  (dashboard-no-items-face ((t (:weight light))))
  :init
  (setq dashboard-banner-logo-title "MSpace EMACS - Enjoy Programming & Writing"
        dashboard-buffer-name "*MSpace Emacs*"
        dashboard-startup-banner (or mzneon-logo 'official)
        dashboard-projects-backend 'projectile
        dashboard-path-style 'truncate-middle
        dashboard-path-max-length 60
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts nil
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 10)
                          (projects . 5))
        dashboard-item-shortcuts '((recents   . "r")
                                   (projects  . "p"))
        dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-briefcase"))
          dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-banner-title
                                      dashboard-insert-navigator
                                      dashboard-insert-init-info
                                      dashboard-insert-items
                                      dashboard-insert-footer))

  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
