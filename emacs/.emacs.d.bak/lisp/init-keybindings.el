;; init-keybindings.el --- Initialize keybindings configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; General keybindings configurations.
;;
;;; Code:

;; `general'
(use-package general
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer mzn/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  ;; usually used keybindings
  (mzn/leader-keys
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "Find file")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  ;; buffers[b]
  (mzn/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(consult-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers"))
  ;; bookmarks[B]
  (mzn/leader-keys
    "B" '(:ignore t :wk "Bookmarks")
    "B d" '(bookmark-delete :wk "Delete bookmark")
    "B f" '(consult-bookmark :wk "Find bookmark")
    "B l" '(list-bookmarks :wk "List bookmarks")
    "B m" '(bookmark-set :wk "Set bookmark")
    "B w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  ;; code/check[c] TODO
  (mzn/leader-keys
    "c" '(:ignore t :wk "Code")
    "c c" '(flymake-show-buffer-diagnostics :wk "Fly check show")
    "c f" '(apheleia-format-buffer :wk "Format")
    "c l" '(list-bookmarks :wk "List bookmarks")
    "c m" '(bookmark-set :wk "Set bookmark")
    "c w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))
  ;; file[f]
  (mzn/leader-keys
    "f" '(:ignore t :wk "Files")
    "f e" '((lambda () (interactive)
              (dired "~/.dotfiles/emacs/.emacs.d"))
            :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f i" '((lambda () (interactive)
              (find-file "~/.dotfiles/emacs/.emacs.d/init.el"))
            :wk "Open emacs init.el")
    "f r" '(consult-recent-file :wk "Find recent files"))

  ;; git[g]
  (mzn/leader-keys
    "g" '(:ignore t :wk "Git")
    "g /" '(magit-dispatch :wk "Magit dispatch")
    "g ." '(magit-file-dispatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create")
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find")
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

  ;; help[h]
  (mzn/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.dotfiles/emacs/.emacs.d/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h r R" '(restart-emacs :wk "Restart emacs")
    "h r q" '(kill-emacs :wk "Quit emacs")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  ;;org-mode[n]
   (mzn/leader-keys
    "n" '(:ignore t :wk "Org roam notes")
    "n p" '(org-preview-html-mode :wk "Preview html")
    "n c" '(org-roam-capture :wk "Roam capture")
    "n f" '(org-roam-node-find :wk "Roam node fine")
    "n g" '(org-roam-graph :wk "Roam graph")
    "n i" '(org-roam-node-insert :wk "Roam node insert")
    "n j" '(org-roam-dailies-capture-today :wk "Roam dailies capture today")
    "n j" '(org-roam-buffer-toggle :wk "Roam buffer toggle")
    "n y" '(org-rich-yank :wk "Org rich yank")
)


  ;; project[p]
  (mzn/leader-keys
    "p" '(:ignore t :wk "Project")
    "p a" '(projectile-add-known-project :wk "Project add")
    "p b" '(consult-projectile-switch-to-buffer :wk "Project switch buffer")
    "p d" '(projectile-dired :wk "Project find directory")
    "p f" '(consult-projectile-find-file :wk "Project find file")
    "p p" '(projectile-switch-project :wk "Project switch")
    "p s" '(projectile-ripgrep :wk "Project search code"))

   ;; Search[s]
  (mzn/leader-keys
    "s" '(:ignore t :wk "Search")
    "s b" '(consult-project-buffer :wk "Project switch buffer")
    "s c" '(project-compile :wk "Project compile")
    "s d" '(project-find-dir :wk "Project find directory")
    "s f" '(project-find-file :wk "Project find file")
    "s g" '(project-find-regexp :wk "Project find regexp")
    "s p" '(project-switch-project :wk "Project switch"))

   ;; toggle[t]
  (mzn/leader-keys
    "t" '(:ignore t :wk "Toggle/Treemacs")
    "t b" '(treemacs-bookmark :wk "Treemacs bookmark")
    "t d" '(treemacs-select-directory :wk "Treemacs directory")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t F" '(treemacs-find-tag :wk "Treemacs find tag")
    "t f" '(treemacs-find-file :wk "Treemacs find file")
    "t p" '(treemacs-projectile :wk "Treemacs projectile")
    "t r" '(treemacs-remove-project-from-workspace :wk "Treemacs remove projectile")
    "t s" '(treemacs-select-directory :wk "Treemacs directory")
    "t t" '(treemacs :wk "Treemacs Toggle")
    "t v" '(vterm-toggle :wk "Toggle vterm"))


  ;; window/word[w]
  (mzn/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))
)

(provide 'init-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here
