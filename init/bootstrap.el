;;; Code:
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Global bindings
(bind-key "C-c C-b" 'eval-buffer)
(bind-key "C-x e" 'other-frame)
(bind-key "C-z" 'undo)

(global-unset-key (kbd "C-x C-b"))

;; Change M-x to M-b
(define-key key-translation-map [?\M-x] [?\M-b])
(define-key key-translation-map [?\M-b] [?\M-x])

;; Change C-x to C-e
(keyboard-translate ?\C-x ?\C-b)
(keyboard-translate ?\C-b ?\C-x)

;; Switch command and control for Mac-OSX
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

(bind-key "C-c f" 'find-file)
(bind-key "C-c e s" 'eshell)

(use-package avy
  :bind(("C-," . avy-goto-char-2)
	("C-;" . avy-goto-word-1)
	("C-'" . avy-goto-line)))

(use-package ace-window
  :bind(("C-." . ace-window)))

(use-package ivy
  :init
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))

(use-package smex)

(use-package undo-tree
  :bind(("M-z" . undo-tree-visualize)))
