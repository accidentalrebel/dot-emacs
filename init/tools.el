(use-package evil
  :config
  (evil-mode 1)
  )

(use-package rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-global-mode t)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :bind ( :map yas-minor-mode-map
	      ("<tab>" . nil)
	      ("TAB" . nil)
	      ("<backtab>" . yas-expand)
	      ("C-c TAB" . yas-expand)))

(use-package auto-complete-config
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

(global-auto-revert-mode 1)

(use-package bind-key)

(use-package smartparens-config
  :diminish (smartparens-mode . "")
  :config
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'haxe-mode-hook #'smartparens-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-smartparens
  :diminish (evil-smartparens-mode . ""))

(use-package auto-complete
  :diminish (auto-complete-mode . ""))

(use-package undo-tree
  :diminish (undo-tree-mode . ""))

(use-package golden-ratio
  :diminish (golden-ratio-mode . "")
  :config
  (golden-ratio-mode 1)

  (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

  (defun pl/helm-alive-p ()
    (and (boundp 'helm-alive-p)
	 (symbol-value 'helm-alive-p))))

(use-package tomatinho
  :load-path "~/.emacs.d/tomatinho/")

(use-package flycheck-package
  :config
  (global-flycheck-mode)
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(use-package buffer-move)

(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  abort-recursive-edit
	  forward-char
	  backward-char
	  previous-line
	  next-line
	  org-agenda-next-line
	  org-agenda-previous-line
	  org-self-insert-command
	  org-delete-backward-char
	  undo-tree-undo
	  undo-tree-redo
	  mwheel-scroll
	  mouse-set-point
	  mouse-drag-region
	  sp-backward-delete-char
	  delete-backward-char
	  helm-previous-line
	  helm-next-line
	  evil-forward-word-begin
	  evil-forward-word-end
	  evil-backward-word-begin
	  evil-backward-word-end
	  evil-forward-char
	  evil-backward-char
	  evil-tree-undo
	  evil-delete-char
	  evil-replace
	  evil-insert
	  evil-delete
	  evil-visual-line
	  evil-normal-state
	  ewil-paste-after
	  evil-previous-line
	  evil-next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package ido
  :config
  (ido-mode t)
  (setq org-completion-use-ido t)
  (setq ido-enable-flex-matching t)
  :bind (("C-c f" . ido-find-file)))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package evil
  :config
  (evil-mode 1)
  :bind ( :map evil-normal-state-map
	      ("C-k" . evil-scroll-up)
	      ("C-j" . evil-scroll-down)))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'toaster))

(use-package avy
  :bind ( :map arebel-custom-key-map
	       ("C-'" . avy-goto-char-2)
	       ("C-;" . avy-goto-word-1)
	       ("C-," . avy-goto-line)))

(use-package abbrev
  :diminish (abbrev-mode . ""))

(use-package ace-window
  :bind (("C-@" . ace-window)))

(use-package open-junk-file
  :bind (("C-c j" . open-junk-file)))

(use-package restart-emacs)

(use-package golden-ratio
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package async
  :config
  (async-bytecomp-package-mode 1)
  )
;;; tools.el ends here
