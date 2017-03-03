(use-package magit
  :config
  (if (equal system-type 'gnu/linux)
      (progn
	(setq exec-path (append exec-path '("/usr/bin/")))
	(setq magit-git-executable (expand-file-name "/usr/bin/git")) 
	)
    (progn
      (setenv "SSH_ASKPASS" "git-gui--askpass")
      (setq magit-git-executable "C:/development/tools/git/bin/git.exe")
      (add-to-list 'exec-path "C:/development/tools/git/bin")
      )
    )
  :bind
  (("C-x g" . magit-status)))

(use-package monky
  :bind
  (("C-h g" . monky-status)))
