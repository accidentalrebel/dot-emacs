;; (require 'epa-file)
;; (epa-file-enable)

;; (setq epa-file-select-keys nil)
;; (setq epa-pinentry-mode 'loopback)

;; (use-package org-journal
;;   :init
;;   (setq org-journal-enable-encryption t))

(use-package org-crypt
  :init
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key "accidentalrebel@protonmail.com")
  :config
  (org-crypt-use-before-save-magic))

(use-package magitgpg
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
  (setq company-tooltip-idle-delay 0.5))

(use-package omnisharp)

(use-package csharp-mode
  :after (omnisharp company)
  :config
  (add-hook 'csharp-mode-hook (lambda()
				(omnisharp-mode)
				(company-mode)
				(add-to-list 'company-backends 'company-omnisharp)

				(flycheck-mode)
				(abbrev-mode)

				(setq company-frontends '(company-echo-metadata-frontend
							  company-preview-frontend))

				(setq indent-tabs-mode nil)
				(setq c-syntactic-indentation t)
				(c-set-style "ellemtel")
				(setq c-basic-offset 4)
				(setq truncate-lines t)
				(setq tab-width 4)
			
				(local-set-key (kbd "C-c r s") '(lambda()
								  (interactive)
								  (omnisharp-reload-solution)
								  (revert-buffer t t)))
				(local-set-key (kbd "C-c r f") 'omnisharp-code-format-entire-file)
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
	 (racer-mode . company-mode)))

(use-package eww-lnum)

(use-package eww
  :after eww-lnum
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

(use-package multiple-cursors)

(use-package pomidor
  :init
  (setq pomidor-play-sound-file
	(lambda (file))))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify nil)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :client-id user--slack-client-id
   :client-secret user--slack-client-secret
   :token user--slack-client-token
   :subscribed-channels '(chefwars_dev)
   :full-and-display-names t
   )

  (use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
  )

(use-package pomodoro
  :init
  (setq use-dialog-box nil)
  (setq pomodoro-play-sounds nil)
  (setq pomodoro-sound-player nil)
  :config
  (pomodoro-add-to-mode-line))

;;
