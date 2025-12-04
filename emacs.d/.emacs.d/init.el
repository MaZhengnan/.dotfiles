;;; init.el
;;; Commentary:
;;; Code:


;;; -*- lexical-binding: t; -*-


;; To create both core and config configuration directory.
(defvar my-emacs-core-dir (expand-file-name "core" user-emacs-directory))
(defvar my-emacs-config-dir (expand-file-name "config" user-emacs-directory))

;; Ensure both 'core' and 'config' directory exists.
(dolist (dir (list my-emacs-core-dir my-emacs-config-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))  ;; 't' is ensure creating the father directory

(add-to-list 'load-path my-emacs-core-dir)
(add-to-list 'load-path my-emacs-config-dir)


;; load core configuration
(require 'packages)      ;; packages manager
(require 'defaults)
(require 'appearance)     ;; appearance configure

;; load config configuration
;;(require 'keybindings)   ;; keybindings configuration
(require 'evil-keybindings)
(require 'completion)
