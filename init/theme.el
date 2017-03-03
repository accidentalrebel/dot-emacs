(use-package doom-themes
  :diminish (doom-buffer-mode . "")
  :init
  (setq doom-enable-bold t
	doom-enable-italic t
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t)
  :config
  (load-theme 'doom-one t)
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode))

(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful
	sml/name-width 35
	sml/mode-width 15 
	sml/no-confirm-load-theme t)
  :config
  (sml/setup))
