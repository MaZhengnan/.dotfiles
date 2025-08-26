;; ~/.doom.d/config.el

;; 重写 projectile-compile-project 命令
(defun my/projectile-compile-project (&optional arg)
  "智能编译：如果是 CMake 项目，自动处理 build 目录和编译。"
  (interactive "P")
  (if (my/is-cmake-project-p)
      (my/cmake-smart-compile)
    (projectile-default-compile-command arg)))

(defun my/is-cmake-project-p ()
  "检查当前项目是否是 CMake 项目。"
  (and (projectile-project-p)
       (file-exists-p (concat (projectile-project-root) "CMakeLists.txt"))))

(defun my/cmake-smart-compile ()
  "智能 CMake 编译：自动创建 build 目录并编译。"
  (let* ((project-root (projectile-project-root))
         (build-dir (concat project-root "build/"))
         (compile-command))

    ;; 自动创建 build 目录（如果不存在）
    (unless (file-exists-p build-dir)
      (make-directory build-dir t)
      (message "Created build directory: %s" build-dir))

    ;; 检查是否需要先运行 cmake 配置
    (unless (file-exists-p (concat build-dir "CMakeCache.txt"))
      (message "Running cmake configuration...")
      (let ((default-directory build-dir))
        (shell-command "cmake ..")))

    ;; 设置编译命令并编译
    (setq compile-command (format "cmake --build %s --parallel 8" build-dir))
    (compile compile-command)))

;; 重写 projectile-run-project 命令
(defun my/projectile-run-project (&optional arg)
  "智能运行：如果是 CMake 项目，从 build 目录选择可执行文件运行。"
  (interactive "P")
  (if (my/is-cmake-project-p)
      (my/cmake-run-executable)
    (projectile-default-run-command arg)))

(defun my/cmake-run-executable ()
  "从 build 目录中选择并运行可执行文件。"
  (let* ((project-root (projectile-project-root))
         (build-dir (concat project-root "build/"))
         (executables (my/find-executables-in-dir build-dir)))

    (if executables
        (let ((target (completing-read "Run executable: " executables nil t)))
          (async-shell-command (concat "cd " build-dir " && ./" target)))
      (message "No executables found in build directory! Compile first with SPC p c"))))

(defun my/find-executables-in-dir (dir)
  "在指定目录中查找可执行文件。"
  (when (file-exists-p dir)
    (seq-filter (lambda (f)
                  (and (file-executable-p (concat dir f))
                       (not (file-directory-p (concat dir f)))
                       (not (string-prefix-p "." f))))
                (directory-files dir))))

;; 覆盖默认的命令
(advice-add 'projectile-compile-project :override #'my/projectile-compile-project)
(advice-add 'projectile-run-project :override #'my/projectile-run-project)

;; 可选：添加快捷键提示
(message "CMake project support loaded. Use SPC p c to compile, SPC p r to run.")



;; ~/.doom.d/config.el

;; 重写 projectile-test-project 命令
(defun my/projectile-test-project (&optional arg)
  "智能测试：如果是 CMake 项目，运行 ctest，否则使用默认测试命令。"
  (interactive "P")
  (if (my/is-cmake-project-p)
      (my/cmake-run-tests)
    (projectile-default-test-command arg)))

(defun my/cmake-run-tests ()
  "运行 CMake 项目的测试。"
  (let* ((project-root (projectile-project-root))
         (build-dir (concat project-root "build/")))

    ;; 检查 build 目录是否存在
    (unless (file-exists-p build-dir)
      (error "Build directory not found! Compile first with SPC p c"))

    ;; 运行 ctest
    (let ((default-directory build-dir))
      (compile "ctest --output-on-failure"))))

;; 覆盖默认的测试命令
(advice-add 'projectile-test-project :override #'my/projectile-test-project)
