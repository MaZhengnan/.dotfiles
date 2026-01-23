;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires

;;; ----- Basic Configuration -----

;; Always load newest byte code
(setq load-prefer-newer t)
;; Define Prelude's directory structure
(defvar efs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs efs distribution.")
(defvar efs-core-dir (expand-file-name "core" efs-dir)
  "The home of efs's core functionality.")
(defvar efs-modules-dir (expand-file-name  "modules" efs-dir)
  "This directory houses all of the built-in efs modules.")

(defvar efs-vendor-dir (expand-file-name "vendor" efs-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar efs-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar efs-modules-file (expand-file-name "efs-common.el" efs-modules-dir)
  "This file contains a list of modules that will be loaded by efs.")
(defvar efs-override-package-user-dir t
  "By default prelude installs downloaded packages in <efs-dir>/elpa.
   Set to nil to override this behaviour")

(unless (file-exists-p efs-savefile-dir)
  (make-directory efs-savefile-dir))

(defun efs-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (efs-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path efs-core-dir)
(add-to-list 'load-path efs-modules-dir)
(add-to-list 'load-path efs-vendor-dir)
(efs-add-subfolders-to-load-path efs-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(require 'efs-packages)
(require 'efs-core)
(require 'efs-ui)
;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'efs-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'efs-linux))

;; WSL specific setting
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (require 'efs-wsl))

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'efs-windows))

(load efs-modules-file)
