;;; ~/.doom.d/+clipboard.el --- WSL2 clipboard integration with win32yank



(require 'cl-lib)
;; 检查 win32yank 是否可用
(defun win32yank-available-p ()
  (executable-find "win32yank.exe"))

(when (win32yank-available-p)
  ;; 设置复制到剪切板
  (defun wsl2-copy (text &optional push)
    (interactive)
    (let ((process-connection-type nil))
      (let ((proc (start-process "win32yank" nil "win32yank.exe" "-i" "--crlf")))
        (process-send-string proc text)
        (process-send-eof proc))))
  ;; 设置从剪切板粘贴
  (defun wsl2-paste ()
    (interactive)
    (shell-command-to-string "win32yank.exe -o --lf"))

  ;; 设置剪切板函数
  (setq interprogram-cut-function 'wsl2-copy)
  (setq interprogram-paste-function 'wsl2-paste)

  ;; 可选：添加自定义快捷键映射
  (defun +clipboard/init ()
    "Initialize WSL2 clipboard keybindings."
    (map! :leader
          :desc "Copy to Windows clipboard" "y" #'kill-ring-save
          :desc "Paste from Windows clipboard" "p" #'yank))

  ;; 自动初始化
  ;; (+clipboard/init)
  (message "WSL2 clipboard integration loaded successfully"))

(provide '+clipboard)
