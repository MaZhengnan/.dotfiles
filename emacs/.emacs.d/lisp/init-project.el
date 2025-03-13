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
;; 启用 project 包
(use-package project
  :ensure nil
  :config
  ;; Set the identification rules for the project root directory
  (setq project-vc-ignores '("node_modules" "target" "dist" "build" ".cask" ".clangd" ".direnv" ".elixir_ls" ".idea" ".mypy_cache" ".pytest_cache" ".venv" "__pycache__"))
  ;; Set up project file cache
  (setq project-list-file (expand-file-name "projects" user-emacs-directory))
  ;; Add custom logic to project-find-functions
)

(provide 'init-project)

;;; init-project.el ends here
