;;; ../.dotfiles/doom-emacs/.doom.d/+win32yank-clipboard.el -*- lexical-binding: t; -*-

;;; +win32yank-clipboard.el --- Win32yank integration for WSL2

(defun win32yank-available-p ()
  "Check if win32yank is available."
  (executable-find "win32yank"))

(defun win32yank-copy-to-clipboard (text &optional push)
  "Copy text to Windows clipboard using win32yank."
  (when (win32yank-available-p)
    (let ((process-connection-type nil))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "win32yank" nil 0 nil
                             "-i" "--crlf")))))

(defun win32yank-get-from-clipboard ()
  "Get text from Windows clipboard using win32yank."
  (when (win32yank-available-p)
    (with-temp-buffer
      (call-process "win32yank" nil t nil "-o" "--lf")
      (buffer-string))))

;;(provide '+win32yank-clipboard)
