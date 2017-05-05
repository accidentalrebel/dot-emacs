(use-package auth-source)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package org2blog
  :config
  (let (credentials)
    (add-to-list 'auth-sources "~/.netrc")
    (setq credentials (auth-source-user-and-password "accidentalrebel")
	  org2blog/wp-blog-alist '(("accidentalrebel"
				    :url user--blog-xmlrpc-url
				    :default-categories ("Blog Post")
				    :username "admin"))))
  )

(use-package org-page
  :init
  (setq op/repository-directory (concat user--linux-dropbox-folder "orgmode/blog"))
  (setq op/site-domain "https://accidentalrebel.github.io/")
  (setq op/site-main-title "Accidental Rebel")
  (setq op/site-sub-title "My personal blog.")
  (setq op/personal-github-link "https://github.com/accidentalrebel/")
  (setq op/personal-disqus-shortname "accidentalrebel")
  ;;  (setq op/personal-avatar user--blog-avatar-link)
  (setq op/theme-root-directory (concat user--linux-dropbox-folder "orgmode/blog/themes"))
  (setq op/theme 'arebel)
  )

(use-package dokuwiki
  :config
  (setq dokuwiki-xml-rpc-url user--gamedevph-xmlrpc-url
	dokuwiki-login-user-name user--gamedevph-username)
  )

(if (equal system-type 'gnu/linux)
    (progn
      (setenv "development" user--linux-dev-folder)
      (setenv "dropbox" user--linux-dropbox-folder)
      )
  (progn
    (setenv "development" user--win-dev-folder)
    (setenv "dropbox" user--win-dropbox-folder)
    ))

(use-package slack
  :commands (slack-start)
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

(use-package eshell
  :config
  (setenv "desktop" (concat user--win-arebel-home-folder "Desktop"))
  (setenv "chefwars" user--win-chefwars-folder)
  (setenv "downloads" (concat user--win-arebel-home-folder "Downloads")))

(setq user-mail-address user--email	
	  user-full-name user--full-name)

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nntp "news.gmane.org") (nntp "news.gnus.org")))

(defvar arebel-custom-key-map nil "My custom keymap.")
