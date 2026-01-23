;; -*- coding: utf-8; lexical-binding: t; -*-
;;; efs-core.el --- Emacs from scratch configuration entry point.
;; Copyright (c) 2011-2025 Bozhidar Batsov
;;; Commentary:
;; This file simply sets up the default load path and requires


;; Core settings
(setq ;; Yes, this is Emacs
      inhibit-startup-message t

      ;; Instruct auto-save-mode to save to the current file, not a backup file
      auto-save-default nil

      ;; No backup files, please
      make-backup-files nil

      ;; Make it easy to cycle through previous items in the mark ring
      set-mark-command-repeat-pop t

      ;; Don't warn on large files
      large-file-warning-threshold nil

      ;; Follow symlinks to VC-controlled files without warning
      vc-follow-symlinks t

      ;; Don't warn on advice
      ad-redefinition-action 'accept

      ;; Revert Dired and other buffers
      global-auto-revert-non-file-buffers t

      display-line-numbers-type 'relative ;; Relative line numbers
      ;; Silence compiler warnings as they can be pretty disruptive
      native-comp-async-report-warnings-errors nil)

;; Core modes
(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(display-time-mode 1)          ;; Display time in mode line / tab bar
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files
(set-default-coding-systems 'utf-8)
;; Tabs to spaces
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Match completion substrings that may be out of order
(defun efs/override-fido-completion-styles ()
  (setq-local completion-styles '(basic substring partial-completion emacs22)))

(setopt tab-always-indent 'complete
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t

        ;; This *may* need to be set to 'always just so that you don't
        ;; miss other possible good completions that match the input
        ;; string.
        completion-auto-help t

        ;; Include more information with completion listings
        completions-detailed t

        ;; Move focus to the completions window after hitting tab
        ;; twice.
        completion-auto-select 'second-tab

        ;; If there are 3 or less completion candidates, don't pop up
        ;; a window, just cycle through them.
        completion-cycle-threshold 3

        ;; Cycle through completion options vertically, not
        ;; horizontally.
        completions-format 'vertical

        ;; Sort recently used completions first.
        completions-sort 'historical

        ;; Only show up to 10 lines in the completions window.
        completions-max-height 10

        ;; Don't show the unneeded help string at the top of the
        ;; completions buffer.
        completion-show-help nil

        ;; Add more `completion-styles' to improve candidate selection.
        completion-styles '(basic partial-completion substring initials))

(provide 'efs-core)
