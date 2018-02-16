(when (file-exists-p "~/development/projects/elisp/cobalt/cobalt.el")
  (load "~/development/projects/elisp/cobalt/cobalt.el"))

(use-package cobalt
  :init
  (setq cobalt-site-paths '("~/blogs/testblog" "~/blogs/cobalt-org.github.io/" "~/blogs/accidentalrebel.github.com/")))

(use-package ace-window)
(use-package smex)
(use-package speed-type)
(use-package cargo)
(use-package yaml-mode)
(use-package magit)
(use-package diminish)
(use-package avy)
(use-package swiper)
(use-package markdown-mode)

(use-package rust-mode
  :init
  (setq rust-format-on-save t)
  )

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package pomidor)

(use-package omnisharp
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda()
				(setq truncate-lines t))))

(use-package ivy
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :preface
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (
	 ("M-x" . counsel-M-x)
	 ("C-x b" . ivy-switch-buffer)
	 ("C-c h u" . swiper)
	 ("C-c h o" . swiper-at-point)
	 ("C-c h a" . counsel-apropos)
	 ("C-c f" . counsel-git)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x r" . counsel-recentf)
	 ("C-x C-r" . ivy-resume)
	 ("C-c f" . counsel-find-file)
	 ("C-x c p" . counsel-list-processes)
	 )
  )

(use-package counsel)

(use-package projectile-ripgrep)

(use-package counsel-projectile
  :diminish (projectile-mode . "")
  :init
  ;; NOTE: Temp workaround for slowdown
  (setq projectile-mode-line
	'(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (counsel-projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :bind (("C-c p h" . counsel-projectile-rg)
	 ("C-c p p" . counsel-projectile-switch-project)
	 ("C-x f" . counsel-projectile-find-file)))

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

  (defun hydra-flush ()
    (interactive)
    (defvar hydra-stack nil))
  
  (defhydra hydra-main (:color red)
    "MAIN"

    ;; LEFT HAND SIDE
    ("a" set-mark-command)
    ("'" comment-dwim)
    ("u" (delete-char 1))
    ("o" (delete-char -1))
    ("p" (kill-word 1))
    ("," (kill-word -1))
    ("e" kill-whole-line)
    ("." kill-line)
    
    ("q" kill-ring-save)
    ("j" kill-region)
    ("k" yank)

    ("i" undo)
    ("y" undo-tree-visualize :color blue)
    ("x" counsel-yank-pop :color blue)

    ;; RIGHT HAND SIDE
    ("!" projectile-ripgrep)
    ("-" end-of-buffer)
    ("/" beginning-of-buffer)
    ("m" scroll-down-command)
    ("v" scroll-up-command)

    ("f" avy-goto-line)
    ("=" avy-goto-char-2)
    ("b" avy-goto-word-1)

    ("c" previous-line)
    ("t" next-line)
    ("h" backward-char)
    ("n" forward-char)
    ("r" forward-word)
    ("g" backward-word)
    ("s" move-end-of-line)
    ("d" move-beginning-of-line);

    (")" (progn
	   (hydra-sexp/body)
	   (hydra-push '(hydra-main/body)))
     "hydra-sexp" :color blue)
 
    ("\\" google-this-search)
    
    ("<SPC>" hydra-flush "exit" :exit t)
    ("C-<SPC>" hydra-flush "exit" :exit t)
    )
  
  (defhydra hydra-sexp (:color red)
    "SEXPS"
    ("o" sp-backward-sexp "backward-sexp")
    ("u" sp-forward-sexp "forward-sexp")
    ("." sp-up-sexp "up-sexp")
    ("e" sp-down-sexp "down-sexp")

    ("j" sp-kill-sexp "kill-sexp")
    ("a" sp-raise-sexp "raise-sexp")
    ("p" sp-forward-slurp-sexp "f-slurp")
    ("," sp-backward-slurp-sexp "b-slurp")
    ("k" sp-forward-barf-sexp "f-barf")
    ("q" sp-backward-barf-sexp "b-barf")

    ("i" undo "undo")
    ("y" undo-tree-visualize "undo-tree" :color blue)
    ("x" helm-show-kill-ring "helm-kill-ring" :color blue)

    (";" sp-backward-parallel-sexp "b-parallel")
    ("'" sp-forward-parallel-sexp "f-parallel")

    ;; RIGHT HAND SIDE
    ("-" end-of-buffer "eob")
    ("/" beginning-of-buffer "bob")
    ("m" scroll-down-command "scroll-up")
    ("v" scroll-up-command "scroll-down")

    ("f" avy-goto-line "avy-line")
    ("=" avy-goto-char-2 "avy-char")
    ("b" avy-goto-word-1 "avy-word")

    ("c" previous-line)
    ("t" next-line)
    ("h" backward-char)
    ("n" forward-char)
    ("r" forward-word)
    ("g" backward-word)
    ("s" move-end-of-line)
    ("d" move-beginning-of-line);

    ("<SPC>" hydra-pop "back" :color blue)
    ("C-<SPC>" hydra-flush :exit t)
    )
  
  (defhydra hydra-window (:color red)
    "WINDOWS"
    ("m" delete-window "k-window")
    ("w" delete-frame "k-frame")

    ("c" make-frame-command "new-frame")
    ("d" delete-other-windows "k-other-wwindows")
    ("h" split-window-right "split-horizontally")
    ("g" split-window-below "split-vertically")

    ("," text-scale-decrease "text-scale-dec")
    ("p" text-scale-increase "text-scale-inc")

    ("<SPC>" hydra-flush "exit" :exit t)
    )
  
  (global-set-key (kbd "C-<SPC>") 'hydra-main/body)
  (global-set-key (kbd "C-w") 'hydra-window/body)
  )

;; (use-package twittering-mode
;;   :config
;;   (setq twittering-use-master-password t)
;;   (setq twittering-icon-mode t)
;;   (setq twittering-use-icon-storage t)
;;   )

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
	      ("C-c TAB" . yas-expand)))

(global-auto-revert-mode 1)

(use-package bind-key)

(use-package smartparens
  :diminish (smartparens-mode . "")
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  :bind (("C-}" . sp-forward-slurp-sexp)
	 ("C-{" . sp-backward-slurp-sexp)
	 ("C-(" . sp-raise-sexp)
	 ("C-#" . sp-forward-barf-sexp)
	 ("C-!" . sp-backward-barf-sexp)))

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

(use-package flycheck
  ;;:diminish (flycheck-mode . "")
  :init
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'csharp-mode-hook 'flycheck-mode))

(use-package flycheck-package
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(use-package alert
  :commands (alert)
  :init
  (setq alert-log-messages t)
  (setq alert-default-style 'message)
  (setq alert-user-configuration '((((:severity high)) toaster nil))))

(use-package abbrev
  :diminish (abbrev-mode . ""))

(use-package ace-window
  :bind (("C-;" . ace-window)))

(use-package open-junk-file
  :bind (("C-c j" . open-junk-file)))

(use-package restart-emacs)

(use-package async
  :config
  (async-bytecomp-package-mode 1))

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

  :bind (("C-c e s" . eshell)
	 ("C-c e p" . eshell/copy-pwd))
  )

(use-package company
  :diminish (company-mode . "")
  :init
  (setq company-dabbrev-downcase nil)  
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefx-length 3)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  )

(use-package eshell-up)
(use-package hackernews)

(use-package eww
  :init
  (setq shr-color-visible-luminance-min 70)
  (setq shr-use-fonts nil)
  :bind ( :map eww-mode-map
	       ("f" . eww-lnum-follow)
	       ("F" . eww-lnum-universal)))

(use-package google-this
  :diminish (google-this-mode . "")
  :init
  (google-this-mode 1))

(use-package racer
  :config
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  )

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

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

;;; tools.el ends here
