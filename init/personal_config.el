(use-package auth-source)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(when (not (eq system-type 'darwin))
  (setq shr-external-browser
	'(lambda(url &rest ARGS)
	   (start-process "cmd" nil "cmd.exe" "/C" "start" url)
	   )))

(setq browse-url-browser-function
      '(("http://msi:8080" . (lambda (url &optional ARGS)
			       (funcall shr-external-browser url ARGS)))
	("http://127.0.0.1:3000" . browse-url-default-browser)
	("." . eww-browse-url )))

(setq user-mail-address user--email
      user-full-name user--full-name)

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nntp "news.gmane.org") (nntp "news.gnus.org")))

(defvar arebel-custom-key-map nil "My custom keymap.")

