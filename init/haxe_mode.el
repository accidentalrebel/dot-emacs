(use-package haxe-mode
  :config
  (defconst my-haxe-style
    '("java" (c-offsets-alist . ((case-label . +)
				 (arglist-intro . +)
				 (arglist-cont-nonempty . 0)
				 (arglist-close . 0)
				 (cpp-macro . 0))))
  "My haXe Programming Style.")

  (add-hook 'haxe-mode-hook 'haxe-imports-scan-file)
  (add-hook 'haxe-mode-hook (lambda () (setq truncate-lines t)))
  (add-hook 'haxe-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'haxe-mode-hook (function (lambda () (c-add-style "haxe" my-haxe-style t))))
  (add-hook 'haxe-mode-hook (function
			     (lambda ()
			       (setq tab-width 4)
			       (setq indent-tabs-mode t)
			       (setq fill-column 80)
			       (local-set-key [(return)] 'newline-and-indent))))
  )

(use-package haxe-imports
  :config
  (bind-key "C-c h i" 'haxe-imports-add-import-dwim))
