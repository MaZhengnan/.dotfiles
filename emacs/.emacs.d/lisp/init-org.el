;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:


(defun mzneon-org-font-setup ()
  "Font setup for Org mode."
  (variable-pitch-mode 1)
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))
  ;; Ensure fixed-pitch for code, tables, etc.
  (dolist (face '(org-block org-table org-formula org-code org-verbatim
                   org-special-keyword org-meta-line org-checkbox
                   line-number line-number-current-line))
    (set-face-attribute face nil :inherit 'fixed-pitch)))
  ;; Set font size for Org mode headings
  (set-face-attribute 'org-level-1 nil :height 1.4)  ;; Level 1 heading
  (set-face-attribute 'org-level-2 nil :height 1.3)  ;; Level 2 heading
  (set-face-attribute 'org-level-3 nil :height 1.2)  ;; Level 3 heading
  (set-face-attribute 'org-level-4 nil :height 1.2)  ;; Level 4 heading
  (set-face-attribute 'org-level-5 nil :height 1.15)  ;; Level 5 heading
  (set-face-attribute 'org-level-6 nil :height 1.15)  ;; Level 6 heading
  (set-face-attribute 'org-level-7 nil :height 1.05)  ;; Level 7 heading
  (set-face-attribute 'org-level-8 nil :height 1.05)) ;; Level 8 heading


(use-package org
  :ensure nil
  :defer
  :custom
  (org-ellipsis " ⤵")
  (org-agenda-include-diary t)
  ;; Where the org files live
  (org-directory "~/work/notes/")
  ;; Where archives should go
  ;; (org-archive-location (concat (expand-file-name "~/.emacs.d/org/private/org-roam/gtd/archives.org") "::"))
  ;; Make sure we see syntax highlighting
  (org-src-fontify-natively t)
  ;; I dont use it for subs/super scripts
  (org-use-sub-superscripts nil)
  ;; Should everything be hidden?
  (org-startup-folded t)
  (org-M-RET-may-split-line '((default . nil)))
  ;; Don't hide stars
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  ;; Show as utf-8 chars
  (org-pretty-entities t)
  ;; put timestamp when finished a todo
  (org-log-done 'time)
  ;; timestamp when we reschedule
  (org-log-reschedule t)
  ;; Don't indent the stars
  (org-startup-indented nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  ;; Save notes into log drawer
  (org-log-into-drawer t)
  ;;
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  ;;
  (org-fontify-quote-and-verse-blocks t)
  ;; See down arrow instead of "..." when we have subtrees
  ;; (org-ellipsis "⤵")
  ;; catch invisible edit
  ( org-catch-invisible-edits 'show-and-error)
  ;; Only useful for property searching only but can slow down search
  (org-use-property-inheritance t)
  ;; Count all children TODO's not just direct ones
  (org-hierarchical-todo-statistics nil)
  ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Don't allow TODO's to close without their dependencies done
  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; Where should notes go to? Dont even use them tho
  (org-default-notes-file (concat org-directory "notes.org"))
  ;; The right side of | indicates the DONE states
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c!)" "DELEGATED(p!)")))
  ;; Needed to allow helm to compute all refile options in buffer
  (org-outline-path-complete-in-steps nil)
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  ;; Repeat to previous todo state
  ;; If there was no todo state, then dont set a state
  (org-todo-repeat-to-state t)

  (org-insert-heading-respect-content t)
  :hook ((org-mode . org-indent-mode)
         (org-mode . mzneon-org-font-setup)
         (org-mode . org-display-inline-images))
  :custom-face
  (org-scheduled-previously ((t (:foreground "orange"))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t)
     (rust . t)
     (python . t)
     (js . t)
     (html . t)
     (css . t)
     (cpp . t)
     (C . t)
     (emacs-lisp . t)
     (shell . t)))
  ;; Save history throughout sessions
  (org-clock-persistence-insinuate))


(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  ;; (add-to-list 'org-structure-template-alist '("c" . "src c"))
  ;; (add-to-list 'org-structure-template-alist '("cc" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

;; Table of contents
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width (cond (sys/win32p 110)
                                       (sys/linuxp 110)
                                       (sys/macp)))
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :after org
  :hook (org-mode . org-mode-visual-fill))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autolinks t))

;; Org Superstar for prettier bullets
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")
        org-superstar-item-bullet "■"
        org-superstar-remove-leading-stars t))


(use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-mode-turns-on-hiding-stars nil))

;; Org Roam for note-taking
(use-package org-roam
  :diminish
  :functions mzneon-browse-url
  :init
  (setq org-roam-directory (file-truename mzneon-org-directory)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (unless (file-exists-p mzneon-org-roam-directory)
    (make-directory mzneon-org-roam-directory))
  (add-to-list 'org-agenda-files (format "%s/%s" mzneon-org-roam-directory "roam"))
  (org-roam-db-autosync-enable))

(provide 'init-org)

;;; init-org.el ends here
