;;; init-org.el --- Org Mode Configuration -*- lexical-binding: t -*-

(use-package org
  :hook (org-mode . org-indent-mode) ; Indent text according to outline structure
  :config
  (setq org-ellipsis " ▾")           ; Fold symbol
  (setq org-hide-emphasis-markers t) ; Hide *bold* /param/ markers
  (setq org-src-fontify-natively t)  ; Syntax highlighting in code blocks
  (setq org-src-tab-acts-natively t) ; Tab acts as in language buffer
  (setq org-edit-src-content-indentation 0)
  
  ;; Org Babel Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  
  ;; Auto-save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

;; ----- Org Modern (Beautification) -----
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "wl" "▾" "▹"))
  (setq org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (setq org-modern-tag t)
  (setq org-modern-priority t)
  (setq org-modern-todo t)
  (setq org-modern-table t))

;; ----- Org Roam (Optional - Knowledge Management) -----
;; Uncomment if you need Zettelkasten features
;; (use-package org-roam
;;   :custom (org-roam-directory (file-truename "~/Org/roam"))
;;   :config (org-roam-db-autosync-mode))

(provide 'init-org)
