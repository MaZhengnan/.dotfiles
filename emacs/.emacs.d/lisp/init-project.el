;; init-project.el --- Initialize project configurations.	-*- lexical-binding: t -*-
;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Project configurations like projectile.
;;
;;; Code:
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-dired "Dired")
                                  (project-vc-dir "VC")
                                  (magit-status "Magit")  ;; ✅ Git 版本控制
                                  (project-eshell "Eshell")))
  (setq project-vc-extra-root-markers '(".git" "Cargo.toml" "package.json" "CMakeLists.txt"))

  (setq project-list-file (expand-file-name "projects" user-emacs-directory))
)

(provide 'init-project)

;;; init-project.el ends here
