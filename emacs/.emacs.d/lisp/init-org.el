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

(eval-when-compile
  (require 'init-custom))

(require 'org)

;; Basic Org mode configuration
(defun mzneon-org-basic ()
  "Basic configuration for Org mode."
  (org-indent-mode)
  (setq org-fold-core-style 'overlays
        org-ellipsis " ⤵"
        org-startup-indented t
        org-hide-emphasis-markers t ;; Hide emphasis markers like '*', '/'.
        org-src-fontify-natively t  ;; Use native syntax highlighting in code blocks.
        org-src-tab-acts-natively t ;; Use native Tab key behavior inside code blocks.
        )
   (org-babel-do-load-languages
     'org-babel-load-languages
     '((perl . t)
       (ruby . t)
       (shell . t)
       (rust . t)
       ;; (typescript . t)
       (js . t)
       (latex .t)
       (python . t)
       (emacs-lisp . t)
       (go . t)
       (C . t))))

;; Font setup for Org mode
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
  (set-face-attribute 'org-level-1 nil :height 1.5)  ;; Level 1 heading
  (set-face-attribute 'org-level-2 nil :height 1.4)  ;; Level 2 heading
  (set-face-attribute 'org-level-3 nil :height 1.3)  ;; Level 3 heading
  (set-face-attribute 'org-level-4 nil :height 1.2)  ;; Level 4 heading
  (set-face-attribute 'org-level-5 nil :height 1.15)  ;; Level 5 heading
  (set-face-attribute 'org-level-6 nil :height 1.15)  ;; Level 6 heading
  (set-face-attribute 'org-level-7 nil :height 1.05)  ;; Level 7 heading
  (set-face-attribute 'org-level-8 nil :height 1.05)) ;; Level 8 heading



;; Browse URL with xwidget-webkit or default browser
(defun mzneon-browse-url (url &optional pop-buffer new-session)
  "Open URL using xwidget-webkit or the default browser.
POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (cond
   ((and (featurep 'xwidget-internal) (display-graphic-p))
    (xwidget-webkit-browse-url url new-session)
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (if pop-buffer
            (pop-to-buffer buf)
          (switch-to-buffer buf)))))
   (t (browse-url url))))

;; Org mode setup
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (mzneon-org-basic)
                      (mzneon-org-font-setup)
                      (when mzneon-prettify-org-symbols-alist
                        (if prettify-symbols-alist
                            (push mzneon-prettify-org-symbols-alist prettify-symbols-alist)
                          (setq prettify-symbols-alist mzneon-prettify-org-symbols-alist))
                        (prettify-symbols-mode 1))))
  :config
  (add-to-list 'org-export-backends 'md)
  (require 'org-tempo))  ;; Load org-tempo for code block templates

;; Org-rich-yank, toc, preview html
(use-package org-rich-yank
  :after org
  :bind (:map org-mode-map
         ("C-M-y" . org-rich-yank)))

;; Table of contents
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

;; HTML preview
(use-package org-preview-html
  :after org
  :diminish
  :bind (:map org-mode-map
              ("C-c C-h" . org-preview-html-mode))
  :init (when (and (featurep 'xwidget-internal) (display-graphic-p))
          (setq org-preview-html-viewer 'xwidget)))

;; Visual fill column for Org mode
(defun org-mode-visual-fill ()
  (setq visual-fill-column-width (if sys/win32p 110 90))
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

;; Org Roam for note-taking
(use-package org-roam
  :diminish
  :functions mzneon-browse-url
  :defines org-roam-graph-viewer
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
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
