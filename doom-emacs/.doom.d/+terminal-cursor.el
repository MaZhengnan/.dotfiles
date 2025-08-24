;;; +terminal-cursor.el -*- lexical-binding: t; -*-

(defun my/terminal-cursor-setup ()
  "Setup terminal cursor styles for Evil modes."
  (when (and (not (display-graphic-p))
             (fboundp 'tty-supports-changing-cursor-p)
             (tty-supports-changing-cursor-p))

    (setq evil-normal-state-cursor '(("green" :background "green") box)
          evil-insert-state-cursor '(("red" :background "red") bar)
          evil-visual-state-cursor '(("orange" :background "orange") hollow)
          evil-motion-state-cursor '(("blue" :background "blue") box)
          evil-default-cursor evil-normal-state-cursor)

    (dolist (hook '(evil-insert-state-entry-hook
                    evil-normal-state-entry-hook
                    evil-visual-state-entry-hook))
      (add-hook hook #'my/update-terminal-cursor))

    (message "Terminal cursor styles configured")))

(defun my/update-terminal-cursor ()
  "Update terminal cursor based on Evil state."
  (let ((cursor (cond
                 (evil-insert-state evil-insert-state-cursor)
                 (evil-normal-state evil-normal-state-cursor)
                 (evil-visual-state evil-visual-state-cursor)
                 (t evil-default-cursor))))
    (when cursor
      (setq cursor-type (cadr cursor)))))

(add-hook 'after-init-hook #'my/terminal-cursor-setup)

;;(provide '+terminal-cursor)
