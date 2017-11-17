(use-package auth-source)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq shr-external-browser '(lambda(url &rest ARGS)
			      (start-process "cmd" nil "cmd.exe" "/C" "start" url)
			      ))

(setq browse-url-browser-function
      '(("http://msi:8080" . (lambda (url &optional ARGS)
			       (funcall shr-external-browser url ARGS)))
	("." . eww-browse-url )))

(use-package org-page
  :init
  (setq op/repository-directory (concat (if (equal system-type 'gnu/linux)
					    (progn
					      (if (string= (system-name) "Karbuntu")
						  (concat user--linux-karbuntu-arebel-home-folder "Dropbox/")
						(concat user--linux-msi-arebel-home-folder "Dropbox/")))
					  (concat user--win-arebel-home-folder "Dropbox/")) "orgmode/blog"))
  (setq op/site-domain "https://accidentalrebel.github.io/")
  (setq op/site-main-title "Accidental Rebel")
  (setq op/site-sub-title "My personal blog.")
  (setq op/personal-github-link "https://github.com/accidentalrebel/")
  (setq op/personal-disqus-shortname "accidentalrebel")
  (setq op/personal-google-analytics-id user--blog-google-analytics-id)
  ;;  (setq op/personal-avatar user--blog-avatar-link)
  (setq op/theme-root-directory (concat (if (equal system-type 'gnu/linux)
					    (progn
					      (if (string= (system-name) "Karbuntu")
						  (concat user--linux-karbuntu-arebel-home-folder "Dropbox/")
						(concat user--linux-msi-arebel-home-folder "Dropbox/")))
					  (concat user--win-arebel-home-folder "Dropbox/")) "orgmode/blog/themes"))
  (setq op/theme 'arebel)
  )

(use-package dokuwiki
  :config
  (setq dokuwiki-xml-rpc-url user--gamedevph-xmlrpc-url
	dokuwiki-login-user-name user--gamedevph-username))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-completing-read-function 'ido-completing-read)
  (setq slack-request-timeout 30)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :client-id user--slack-client-id
   :client-secret user--slack-client-secret 
   :token user--slack-client-token
   :subscribed-channels '(chefwars_dev))
  :bind (("C-c s s" . slack-start)
	 ("C-c s c" . slack-channel-select)))

(use-package eshell)

(if (equal system-type 'gnu/linux)
    (progn
      (if (string= (system-name) "MSI")
	  (progn
	    (setenv "development" user--linux-msi-dev-folder)
	    (setenv "chefwars" user--linux-msi-chefwars-folder)
	    (setenv "desktop" (concat user--linux-msi-arebel-home-folder "Desktop"))
	    (setenv "dropbox" (concat user--linux-msi-arebel-home-folder "Dropbox"))
	    (setenv "downloads" (concat user--linux-msi-arebel-home-folder "Downloads")))
	(setenv "dropbox" (concat user--linux-karbuntu-arebel-home-folder "Dropbox"))
	(setenv "downloads" (concat user--linux-karbuntu-arebel-home-folder "Downloads")))
	)

  (setenv "development" user--win-dev-folder)
  (setenv "chefwars" user--win-chefwars-folder)
  (setenv "desktop" (concat user--win-arebel-home-folder "Desktop"))
  (setenv "dropbox" (concat user--win-arebel-home-folder "Dropbox"))
  (setenv "downloads" (concat user--win-arebel-home-folder "Downloads")))

(setq user-mail-address user--email	
	  user-full-name user--full-name)

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nntp "news.gmane.org") (nntp "news.gnus.org")))

(defvar arebel-custom-key-map nil "My custom keymap.")

(setenv "PATH" (concat
		"c:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Auxiliary/Build" ";"
		(concat user--win-dev-folder "projects/accidentalrebel/handmade_hero/misc") ";"
		(getenv "PATH")))

;; (require 'ansi-color)
;; (defun arebel-ansi-colorize-buffer ()
;;   (let ((buffer-read-only nil))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook 'arebel-ansi-colorize-buffer)

(setq shell-command-switch "-ic")
