;; (load-theme 'tango-dark)

(use-package doom-themes
  :config
  (load-theme 'doom-one  t))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package powerline
  :config
  (powerline-default-theme))
