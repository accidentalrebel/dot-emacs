(use-package ag)

(use-package hydra
  :config
  (defvar hydra-stack nil)
  
  (defun hydra-push (expr)
    (push `(lambda () ,expr) hydra-stack))

  (defun hydra-pop ()
    (interactive)
    (let ((x (pop hydra-stack)))
      (when x
	(funcall x))))
  
  (defhydra hydra-movement (:color amaranth)
    "movement and editing."
    ("c" previous-line)
    ("t" next-line)
    ("h" backward-char)
    ("n" forward-char)
    ("r" forward-word)
    ("g" backward-word)
    ("s" move-end-of-line)
    ("d" move-beginning-of-line)
    ("'" comment-dwim)
    ("q" kill-ring-save)
    ("j" kill-region)
    ("/" beginning-of-buffer)
    ("-" end-of-buffer)
    ("f" scroll-down-command)
    ("b" scroll-up-command)

    ("u" (delete-char 1))
    ("o" (delete-char -1))
    ("p" (kill-word 1))
    ("," (kill-word -1))
    ("e" kill-whole-line)
    
    ("k" yank)
    ("." undo)
    ("i" undo-tree-visualize)

    ("w" (progn
	   (hydra-sexp/body)
	   (hydra-push '(hydra-movement/body)))
     "To sexp" :color blue)
    ("q" hydra-pop "exit" :color blue)
    )
  
  (defhydra hydra-sexp (:color red)
    "sexps."
    ("h" sp-backward-sexp)
    ("n" sp-forward-sexp)
    ("c" sp-up-sexp)
    ("t" sp-down-sexp)
    
    ("e" sp-kill-sexp)
    ("." sp-raise-sexp)
    ("u" sp-forward-slurp-sexp)
    ("a" sp-backward-slurp-sexp)
    ("p" sp-forward-barf-sexp)
    ("," sp-backward-barf-sexp)

    ("q" hydra-pop "exit" :color blue)
    )
  
  (global-set-key (kbd "C-t") 'hydra-movement/body)
  
  (defhydra hydra-window (global-map "C-o")
    "window, buffers, and files."
    ("o" other-window)
    ("e" other-frame)
    ("u" delete-other-windows)
    ("k" delete-window)
    ("p" split-window-right)
    ("." split-window-below)
    ("," make-frame-command)
    (";" delete-frame)

    ("b" helm-mini)))

(use-package elmacro
  :config
  (elmacro-mode))

(use-package twittering-mode
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)
  (setq twittering-use-icon-storage t)
  )

(use-package rainbow-delimiters
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
	      ("C-c TAB" . yas|||-expand)))

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
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'haxe-mode-hook #'smartparens-mode)
  :bind (("C-}" . sp-forward-slurp-sexp)
	 ("C-{" . sp-backward-slurp-sexp)
	 ("C-(" . sp-raise-sexp)
	 ("C-#" . sp-forward-barf-sexp)
	 ("C-!" . sp-backward-barf-sexp)))

(use-package auto-complete
  :diminish (auto-complete-mode . ""))

(use-package undo-tree
  :diminish (undo-tree-mode . ""))

(use-package golden-ratio
  :diminish (golden-ratio-mode . "")
  :config
  (golden-ratio-mode)

  (add-to-list 'golden-ratio-extra-commands 'ace-window)
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
	  helm-next-line))
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

(use-package which-key
  :diminish (which-key-mode . "")
  :config
  (which-key-mode))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'toaster))

(use-package avy
  :bind ( :map arebel-custom-key-map
	       ("C-<" . avy-goto-char-2)
	       ("C-," . avy-goto-word-1)
	       ("C-;" . avy-goto-line)))

(use-package abbrev
  :diminish (abbrev-mode . ""))

(use-package ace-window
  :bind (("C-@" . ace-window)))

(use-package open-junk-file
  :bind (("C-c j" . open-junk-file)))

(use-package restart-emacs)

(use-package async
  :config
  (async-bytecomp-package-mode 1)
  )

(use-package eshell
  :config
  (defun clipboard/set (astring)
    "Copy a string to clipboard"
    (with-temp-buffer
      (insert astring)
      (clipboard-kill-region (point-min) (point-max))))

  (defun eshell/copy-pwd ()
    (interactive)
    (clipboard/set (eshell/pwd))
    (message (concat "Copied path: " (eshell/pwd)) ))

  (defun eshell/copy-fpath (fname)
    (interative)
    (let ((fpath (concat (eshell/pwd) "/" fname)))
      (clipboard/set fpath)
      (message "Copied path: " fpath))) 
  )

;;; tools.el ends here
