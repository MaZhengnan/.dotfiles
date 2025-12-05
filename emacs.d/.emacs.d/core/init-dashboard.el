;; -*- coding: utf-8; lexical-binding: t; -*-
;; Customer Dashboard

;; (defun my/simple-dashboard ()
;;     "A minimal dashboard with evil navigation and RET to open items."
;;     (interactive)
;;     (let ((buf (get-buffer-create "*dashboard*")))
;;     (with-current-buffer buf
;;         (let ((inhibit-read-only t))
;;         (erase-buffer)

;;         ;; 标题
;;         (insert "=== Welcome to Emacs ===\n\n")

;;         ;; Recent Files
;;         (insert "Recent Files:\n")
;;         (dolist (f (seq-take recentf-list 5))
;;             (insert (propertize (format "  %s\n" f)
;;                                 'dashboard-type 'file
;;                                 'dashboard-path f)))

;;         ;; Projects
;;         (insert "\nProjects:\n")
;;         (dolist (p (seq-take (project-known-project-roots) 5))
;;             (insert (propertize (format "  %s\n" p)
;;                                 'dashboard-type 'project
;;                                 'dashboard-path p)))

;;         (read-only-mode 1)))

;;     ;; 切换到 dashboard
;;     (switch-to-buffer buf)

;;     ;; 开启 evil-normal-state 让你 j/k 移动
;;     (when (fboundp 'evil-normal-state)
;;         (evil-normal-state))

;;     ;; 绑定回车
;;     (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "RET") #'my/dashboard-activate-line)
;;         (use-local-map map))
;;     buf))

;; (defun my/dashboard-activate-line ()
;;     "Open file or project under cursor."
;;     (interactive)
;;     (let* ((pos (point))
;;             (type (get-text-property pos 'dashboard-type))
;;             (path (get-text-property pos 'dashboard-path)))
;;     (cond
;;         ((and (equal type 'file) path)
;;         (find-file path))
;;         ((and (equal type 'project) path)
;;         (project-switch-project path))
;;         (t
;;         (message "This line is not selectable.")))))

;; ;; 设置 Emacs 启动打开 dashboard
;; (setq initial-buffer-choice #'my/simple-dashboard)
;; -*- coding: utf-8; lexical-binding: t; -*-
;; Customer Dashboard with Nerd Icons

(defun my/create-centered-banner ()
  "创建居中的 banner"
  (let* ((width (window-width))
         (banner-text "EMACS DASHBOARD")
         (banner-length (length banner-text))
         (margin-left (/ (- width banner-length) 2))
         (separator (make-string width ?═))
         (banner-line (concat (make-string margin-left ?\s) banner-text)))
    
    (concat separator "\n"
            banner-line "\n"
            separator "\n\n")))

(defun my/create-centered-line (text)
  "创建居中的文本行"
  (let* ((width (window-width))
         (text-length (length text))
         (margin-left (/ (- width text-length) 2)))
    (concat (make-string margin-left ?\s) text)))

(defun my/create-separator (char)
  "创建分割线"
  (let ((width (window-width)))
    (concat "\n" (make-string width char) "\n")))

(defun my/create-section-header (title)
  "创建部分标题"
  (let* ((width (window-width))
         (title-text (concat " " title " "))
         (title-length (length title-text))
         (dash-count (/ (- width title-length) 2))
         (left-dashes (make-string dash-count ?─))
         (right-dashes (make-string (- width dash-count title-length) ?─)))
    
    (concat left-dashes title-text right-dashes "\n")))

(defun my/get-file-icon (filename)
  "根据文件类型获取 nerd icon"
  (cond
   ((string-match "\\.org$" filename)
    (nerd-icons-octicon "nf-oct-book" :face 'nerd-icons-lorange))
   ((string-match "\\.el$" filename)
    (nerd-icons-octicon "nf-oct-tools" :face 'nerd-icons-purple))
   ((string-match "\\.py$" filename)
    (nerd-icons-octicon "nf-oct-terminal" :face 'nerd-icons-dyellow))
   ((string-match "\\.js$" filename)
    (nerd-icons-octicon "nf-oct-file_code" :face 'nerd-icons-yellow))
   ((string-match "\\.ts$" filename)
    (nerd-icons-octicon "nf-oct-typescript" :face 'nerd-icons-blue-alt))
   ((string-match "\\.json$" filename)
    (nerd-icons-octicon "nf-oct-settings" :face 'nerd-icons-yellow))
   ((string-match "\\.html$" filename)
    (nerd-icons-octicon "nf-oct-browser" :face 'nerd-icons-orange))
   ((string-match "\\.css$" filename)
    (nerd-icons-octicon "nf-oct-paintcan" :face 'nerd-icons-blue))
   ((string-match "\\.md$" filename)
    (nerd-icons-octicon "nf-oct-markdown" :face 'nerd-icons-lblue))
   ((string-match "\\.c$" filename)
    (nerd-icons-octicon "nf-oct-c" :face 'nerd-icons-blue))
   ((string-match "\\.cpp$" filename)
    (nerd-icons-octicon "nf-oct-cpp" :face 'nerd-icons-blue))
   ((string-match "\\.rs$" filename)
    (nerd-icons-octicon "nf-oct-rust" :face 'nerd-icons-orange))
   ((string-match "\\.go$" filename)
    (nerd-icons-octicon "nf-oct-go" :face 'nerd-icons-blue-alt))
   ((string-match "\\.java$" filename)
    (nerd-icons-octicon "nf-oct-java" :face 'nerd-icons-red))
   ((string-match "\\.rb$" filename)
    (nerd-icons-octicon "nf-oct-ruby" :face 'nerd-icons-red))
   (t
    (nerd-icons-octicon "nf-oct-file" :face 'nerd-icons-dsilver))))

(defun my/get-project-icon (project-path)
  "根据项目类型获取 nerd icon"
  (cond
   ((string-match "\\.git" project-path)
    (nerd-icons-octicon "nf-oct-mark_github" :face 'nerd-icons-silver))
   ((string-match "node_modules" project-path)
    (nerd-icons-octicon "nf-oct-terminal" :face 'nerd-icons-green))
   ((string-match "python" project-path)
    (nerd-icons-octicon "nf-oct-terminal" :face 'nerd-icons-dyellow))
   ((string-match "rust" project-path)
    (nerd-icons-octicon "nf-oct-gear" :face 'nerd-icons-orange))
   (t
    (nerd-icons-octicon "nf-oct-repo" :face 'nerd-icons-blue))))

(defun my/simple-dashboard ()
  "A dashboard with nerd icons, centered content and separators."
  (interactive)
  (let ((buf (get-buffer-create "*dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; 1. 顶部 banner（居中）
        (insert (my/create-centered-banner))
        
        ;; 2. 欢迎信息（居中）
        (insert (my/create-centered-line 
                 (concat (nerd-icons-octicon "nf-oct-rocket" :face 'nerd-icons-green)
                         " Welcome to Emacs "
                         (nerd-icons-octicon "nf-oct-rocket" :face 'nerd-icons-green))))
        (insert "\n\n")
        
        ;; 3. 系统信息（居中）
        (insert (my/create-centered-line 
                 (concat (nerd-icons-octicon "nf-oct-calendar" :face 'nerd-icons-blue)
                         " "
                         (format-time-string "%Y-%m-%d")
                         "   "
                         (nerd-icons-octicon "nf-oct-clock" :face 'nerd-icons-purple)
                         " "
                         (format-time-string "%H:%M:%S"))))
        (insert "\n")
        
        ;; 4. 分割线
        (insert (my/create-separator ?─))
        
        ;; 5. 最近文件部分
        (insert (my/create-section-header "RECENT FILES"))
        
        (dolist (f (seq-take recentf-list 5))
          (let ((display-name (abbreviate-file-name f))
                (file-icon (my/get-file-icon f)))
            (insert (propertize (format "%s %s\n" file-icon display-name)
                                'dashboard-type 'file
                                'dashboard-path f
                                'face 'font-lock-string-face))))
        
        ;; 6. 分割线
        (insert (my/create-separator ?─))
        
        ;; 7. 项目部分
        (insert (my/create-section-header "PROJECTS"))
        
        (dolist (p (seq-take (project-known-project-roots) 5))
          (let ((display-name (file-name-nondirectory (directory-file-name p)))
                (project-icon (my/get-project-icon p)))
            (insert (propertize (format "%s %s\n" project-icon display-name)
                                'dashboard-type 'project
                                'dashboard-path p
                                'face 'font-lock-function-name-face))))
        
        ;; 8. 分割线
        (insert (my/create-separator ?─))
        
        ;; 9. 统计信息（居中）
        (let ((file-count (length recentf-list))
              (project-count (length (project-known-project-roots))))
          (insert (my/create-centered-line 
                   (concat (nerd-icons-octicon "nf-oct-graph" :face 'nerd-icons-cyan)
                           " Stats: "
                           (propertize (number-to-string file-count) 'face 'bold)
                           " files | "
                           (propertize (number-to-string project-count) 'face 'bold)
                           " projects"))))
        (insert "\n")
        
        ;; 10. 分割线
        (insert (my/create-separator ?─))
        
        ;; 11. 帮助信息（居中）
        (insert (my/create-centered-line 
                 (concat ;;(nerd-icons-octicon "nf-oct-keyboard" :face 'nerd-icons-dsilver)
                         ;;" RET:open  "
                         (nerd-icons-octicon "nf-oct-arrow_up" :face 'nerd-icons-dsilver)
                         "/"
                         (nerd-icons-octicon "nf-oct-arrow_down" :face 'nerd-icons-dsilver)
                         ":navigate  "
                         (nerd-icons-octicon "nf-oct-x" :face 'nerd-icons-red)
                         ":quit")))
        (insert "\n")
        
        (read-only-mode 1)))
    
    ;; 切换到 dashboard
    (switch-to-buffer buf)
    
    ;; 开启 evil-normal-state
    (when (fboundp 'evil-normal-state)
      (evil-normal-state))
    
    ;; 绑定按键
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'my/dashboard-activate-line)
      (define-key map (kbd "q") 
        (lambda () 
          (interactive) 
          (kill-buffer (current-buffer))
          (switch-to-buffer "*scratch*")))
      (define-key map (kbd "r") 
        (lambda () 
          (interactive) 
          (my/simple-dashboard)))  ; 刷新
      (use-local-map map))
    buf))

(defun my/dashboard-activate-line ()
  "Open file or project under cursor."
  (interactive)
  (let* ((pos (point))
         (type (get-text-property pos 'dashboard-type))
         (path (get-text-property pos 'dashboard-path)))
    (cond
     ((and (equal type 'file) path)
      (find-file path)
      (kill-buffer "*dashboard*"))
     ((and (equal type 'project) path)
      (project-switch-project path)
      (kill-buffer "*dashboard*"))
     (t
      (message "This line is not selectable.")))))

;; 设置 Emacs 启动打开 dashboard
(setq initial-buffer-choice #'my/simple-dashboard)

(provide 'init-dashboard)
