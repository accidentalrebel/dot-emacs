(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package counsel
  :config
  (counsel-mode)
  :bind (("C-x r" . counsel-recentf)
	 ("C-x c p" . counsel-list-processes)))

(use-package counsel-projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :config
  (counsel-projectile-mode))

(use-package ripgrep)

(use-package projectile-ripgrep
  :bind (("C-c g" . projectile-ripgrep)))

(use-package swiper
  :preface
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("M-s s" . swiper)
	 ("M-s M-s" . swiper-at-point)))

(use-package rainbow-delimiters
   :hook ((emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook ((csharp-mode . yas-minor-mode))
  :bind ( :map yas-minor-mode-map
	       ("<backtab>" . yas-expand)))

(use-package smartparens
  :hook ((emacs-lisp-mode . smartparens-mode))
  :bind (("M-s f" . sp-forward-slurp-sexp)
	 ("M-s b" . sp-backward-slurp-sexp)
	 ("M-s e" . sp-forward-barf-sexp)
	 ("M-s a" . sp-backward-barf-sexp)
	 ("M-s p" . sp-raise-sexp)))

(use-package golden-ratio
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (golden-ratio-mode))

(use-package flycheck
  :hook ((flycheck-mode . flycheck-rust-setup)
	 (emacs-lisp-mode . flycheck-mode)
	 (rust-mode . flycheck-mode)))

(use-package eshell
  :init
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
  
  :bind(("C-c e p" . eshell/copy-pwd)))

(use-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  :config
  (add-to-list 'company-backends 'company-omnisharp))
;;  :hook ((after-init . global-company-mode)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package omnisharp)
  ;; :config
  ;; (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda()
				(omnisharp-mode)
				(company-mode)
				(flycheck-mode)

				;; (setq indent-tabs-mode nil)
				;; (setq c-syntactic-indentation t)
				(c-set-style "ellemtel")
				;; (setq c-basic-offset 4)
				;; (setq tab-width 4)
				(setq truncate-lines t)

				(local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
				(local-set-key (kbd "C-c C-c") 'recompile))))

(use-package rust-mode
  :init
  (setq rust-format-on-save t))

(use-package racer
  :init
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode)
	 (racer-mode . company-modea)))

(use-package eww
  :init
  (setq shr-color-visible-luminance-min 70)
  (setq shr-use-fonts nil)
  :bind ( :map eww-mode-map
	       ("f" . eww-lnum-follow)
	       ("F" . eww-lnum-universal)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package restart-emacs)
(use-package hackernews)
