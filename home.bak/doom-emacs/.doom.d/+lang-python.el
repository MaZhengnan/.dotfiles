;; ~/.doom.d/config.el

;; ======================
;; Python Project Detection
;; ======================

(defun my/is-python-project-p ()
  "检查当前项目是否是 Python 项目。"
  (and (projectile-project-p)
       (or (file-exists-p (concat (projectile-project-root) "requirements.txt"))
           (file-exists-p (concat (projectile-project-root) "setup.py"))
           (file-exists-p (concat (projectile-project-root) "pyproject.toml"))
           (file-exists-p (concat (projectile-project-root) "Pipfile"))
           (file-exists-p (concat (projectile-project-root) "tox.ini")))))

;; ======================
;; Python Run Project
;; ======================

(defun my/projectile-run-project (&optional arg)
  "智能运行：如果是 Python 项目，提供运行选项。"
  (interactive "P")
  (if (my/is-python-project-p)
      (my/python-run-project)
    (projectile-default-run-command arg)))

(defun my/python-run-project ()
  "运行 Python 项目。"
  (let ((choice (completing-read "Run Python: "
                                 '("main.py" "app.py" "manage.py" "select file" "custom command")
                                 nil t)))
    (cond ((string= choice "main.py")
           (my/python-run-file "main.py"))
          ((string= choice "app.py")
           (my/python-run-file "app.py"))
          ((string= choice "manage.py")
           (my/python-run-file "manage.py"))
          ((string= choice "select file")
           (my/python-select-and-run-file))
          ((string= choice "custom command")
           (my/python-run-custom-command)))))

(defun my/python-run-file (filename)
  "运行指定的 Python 文件。"
  (let* ((project-root (projectile-project-root))
         (file-path (concat project-root filename)))
    (if (file-exists-p file-path)
        (async-shell-command (format "cd %s && python %s" project-root filename))
      (message "File %s not found!" filename))))

(defun my/python-select-and-run-file ()
  "选择并运行 Python 文件。"
  (let* ((project-root (projectile-project-root))
         (python-files (projectile-project-files project-root))
         (filtered-files (seq-filter (lambda (f) (string-suffix-p ".py" f)) python-files))
         (selected-file (completing-read "Select Python file to run: " filtered-files nil t)))
    (async-shell-command (format "cd %s && python %s" project-root selected-file))))

(defun my/python-run-custom-command ()
  "运行自定义 Python 命令。"
  (let* ((project-root (projectile-project-root))
         (command (read-string "Python command: ")))
    (async-shell-command (format "cd %s && %s" project-root command))))

;; ======================
;; Python Test Project
;; ======================

(defun my/projectile-test-project (&optional arg)
  "智能测试：如果是 Python 项目，提供测试选项。"
  (interactive "P")
  (if (my/is-python-project-p)
      (my/python-run-tests)
    (projectile-default-test-command arg)))

(defun my/python-run-tests ()
  "运行 Python 项目的测试。"
  (let ((choice (completing-read "Run tests with: "
                                 '("pytest" "unittest" "nose" "select test file" "custom test command")
                                 nil t)))
    (cond ((string= choice "pytest")
           (my/python-run-pytest))
          ((string= choice "unittest")
           (my/python-run-unittest))
          ((string= choice "nose")
           (my/python-run-nose))
          ((string= choice "select test file")
           (my/python-run-specific-test))
          ((string= choice "custom test command")
           (my/python-run-custom-test-command)))))

(defun my/python-run-pytest ()
  "使用 pytest 运行测试。"
  (let ((project-root (projectile-project-root)))
    (compile (format "cd %s && pytest -v" project-root))))

(defun my/python-run-unittest ()
  "使用 unittest 运行测试。"
  (let ((project-root (projectile-project-root)))
    (compile (format "cd %s && python -m unittest discover -v" project-root))))

(defun my/python-run-nose ()
  "使用 nose 运行测试。"
  (let ((project-root (projectile-project-root)))
    (compile (format "cd %s && nosetests -v" project-root))))

(defun my/python-run-specific-test ()
  "选择并运行特定的测试文件。"
  (let* ((project-root (projectile-project-root))
         (test-files (projectile-project-files project-root))
         (filtered-files (seq-filter (lambda (f)
                                       (or (string-suffix-p "_test.py" f)
                                           (string-suffix-p "test_.py" f)
                                           (string-contains-p "tests/" f)))
                                     test-files))
         (selected-file (completing-read "Select test file: " filtered-files nil t)))
    (compile (format "cd %s && python -m pytest %s -v" project-root selected-file))))

(defun my/python-run-custom-test-command ()
  "运行自定义测试命令。"
  (let* ((project-root (projectile-project-root))
         (command (read-string "Test command: " "pytest ")))
    (compile (format "cd %s && %s" project-root command))))

;; ======================
;; Override Projectile Commands
;; ======================

(advice-add 'projectile-run-project :override #'my/projectile-run-project)
(advice-add 'projectile-test-project :override #'my/projectile-test-project)

;; ======================
;; Python-specific Configuration
;; ======================

(after! lsp-mode
  (require 'lsp-pyright)
  (add-to-list 'lsp-disabled-clients 'pyls)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-enable-semantic-tokens-enables t)

  ;; 自动为 python 启用 pyright
  (add-hook 'python-mode-hook #'(lambda ()
                                  (require 'lsp-pyright)
                                  (lsp))))

(add-hook 'python-mode-hook
          (lambda ()
            (when (bound-and-true-p conda-env-current-name)
              (setq-local python-shell-interpreter (executable-find "python")))))

;; ======================
;; Conda Integration
;; ======================

(setq conda-anaconda-home my-conda-path)
(setq conda-env-home-directory conda-anaconda-home)
(setq conda-env-subdirectory "envs")
(conda-env-initialize-interactive-shells)
(conda-env-autoactivate-mode t)

;; ======================
;; Org + Jupyter
;; ======================

(use-package! jupyter
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)
     (python . t))))

;; ======================
;; Message
;; ======================

(message "Python project support loaded. Use SPC p r to run, SPC p t to test.")
