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
    (swiper (thing-at-point 'word))
    )
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

(use-package counsel-projectile
  :diminish (projectile-mode . "")
  :init
 ;; NOTE: Temp workaround for slowdown
  (setq projectile-mode-line
	'(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :bind (("C-c p s" . counsel-projectile-ag)
	 ("C-c p p" . counsel-projectile-switch-project)
	 ("C-x f" . counsel-projectile-find-file)
	 )
  )

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

  (defun hydra-flush ()
    (interactive)
    (defvar hydra-stack nil))
  
  (defhydra hydra-main (:color red)
    "MAIN"

    ;; LEFT HAND SIDE
    ("a" set-mark-command "mark")
    ("'" comment-dwim "comment")
    ("u" (delete-char 1) "kill-char-forward")
    ("o" (delete-char -1) "kill-char-backward")
    ("p" (kill-word 1) "kill-word-forward")
    ("," (kill-word -1) "kill-word-backward")
    ("e" kill-whole-line "kill-whole-line")
    ("." kill-line "kill-line")
    
    ("q" kill-ring-save "copy")
    ("j" kill-region "cut")
    ("k" yank "yank")

    ("i" undo "undo")
    ("y" undo-tree-visualize "undo-tree" :color blue)
    ("x" counsel-yank-pop "kill-ring" :color blue)

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
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  :config
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package flycheck-package
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

;; (use-package keyfreq
;;   :config
;;   (setq keyfreq-excluded-commands
;; 	'(self-insert-command
;; 	  abort-recursive-edit
;; 	  forward-char
;; 	  backward-char
;; 	  previous-line
;; 	  next-line
;; 	  org-agenda-next-line
;; 	  org-agenda-previous-line
;; 	  org-self-insert-command
;; 	  org-delete-backward-char
;; 	  undo-tree-undo
;; 	  undo-tree-redo
;; 	  mwheel-scroll
;; 	  mouse-set-point
;; 	  mouse-drag-region
;; 	  sp-backward-delete-char
;; 	  delete-backward-char
;; 	  helm-previous-line
;; 	  helm-next-line))
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package alert
  :commands (alert)
  :init
  (setq alert-log-messages t)
  (setq alert-default-style 'message)
  (setq alert-user-configuration '((((:severity high)) toaster nil)))
  )

(use-package avy
  :bind ( :map arebel-custom-key-map
	       ("C-<" . avy-goto-char-2)
	       ("C-," . avy-goto-word-1)
	       ("C-;" . avy-goto-line)))

(use-package abbrev
  :diminish (abbrev-mode . ""))

(use-package ace-window
  :bind (("C-&" . ace-window)))

(use-package open-junk-fil
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
  )

(use-package eshell-up)

;; (use-package coin-ticker
;;   :config
;;   (setq coin-ticker-syms '("BTC" "BCH" "LTC"))
;;   (coin-ticker-mode 1)
;;   )

(use-package request)
(use-package request-deferred)
(use-package hackernews)

(use-package eww
  :init
  (setq shr-color-visible-luminance-min 70)
  :bind ( :map eww-mode-map
	       ("f" . eww-lnum-follow)
	       ("F" . eww-lnum-universal)))

(use-package google-this
  :diminish (google-this-mode . "")
  :init
  (google-this-mode 1))

(use-package simpleclip
  :diminish (simpleclip-mode . "")
  :init
  (simpleclip-mode))

(use-package pomidor)

;;; tools.el ends here
