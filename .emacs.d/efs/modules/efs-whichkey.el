;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-whichkey.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires

(require 'which-key)
(require 'general)

;; which-key configuration
(setq which-key-side-window-location 'bottom)
(setq which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
(setq which-key-sort-uppercase-first nil)
(setq which-key-enable-extended-define-key t)
(setq which-key-idle-delay 0.8)
(setq which-key-max-description-length 27)
(setq which-key-add-column-padding 1)
(setq which-key-max-display-columns nil)
(setq which-key-min-display-lines 6)  ;; Increase the minimum lines to display because the default is only 1
(setq which-key-allow-imprecise-window-fit nil) ;; Fixes which-key window slipping out in Emacs Daemon
(setq which-key-separator " → " )
(setq which-key-unicode-correction 3)
(setq which-key-prefix-prefix "+" )
(setq which-key-special-keys nil)
;;(setq which-key-show-prefix 'left)
(setq which-key-show-remaining-keys nil)
(which-key-mode 1)

;; general configuration
(general-create-definer efs-leader-key
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(provide 'efs-whichkey)
