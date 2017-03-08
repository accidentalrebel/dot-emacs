(use-package helm
  :diminish (helm-mode . "")
  :config
  (helm-mode 1)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)

  :bind (("M-x" . helm-M-x)
	 ("C-h SPC" . helm-all-mark-rings)
	 ("C-x b" . helm-mini)
	 ("C-c C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("C-c h o" . helm-occur)
	 ("C-c h a" . helm-apropos)
	 :map (helm-map
	       ("<tab>" . helm-execute-persistent-action)
	       ("C-i" . helm-select-action)
	       ("C-z" . helm-select-action))))

(use-package helm-config)
(use-package helm-projectile
  :diminish (projectile-mode . "")
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm
	projectile-indexing-method 'alien
	projectile-enable-caching t
	projectile-switch-project-action 'helm-projectile-find-file)
  :bind (("C-x f" . helm-projectile-find-file)
	 ("C-x C-f" . helm-projectile-find-file-in-known-projects)))

(global-unset-key (kbd "C-x c"))
