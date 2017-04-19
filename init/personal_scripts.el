(defun arebel-putty-connect-linode-zuko ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-zuko-connect-url))

(defun arebel-dired-connect-linode-zuko ()
  (interactive)
  (dired (concat "/" user--linode-zuko-connect-url ":/")))

(defun arebel-putty-connect-linode-box ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-box-connect-url))

(defun arebel-org-journal-entry-to-org-page-post ()
  "Copy the org-journal entry at point and then convert it to a org-page new post buffer."
  (interactive)
  (if (eq 'org-journal-mode major-mode)
      (let ((headline-text (nth 4 (org-heading-components)))
	    (entry-text (org-get-entry)))
	(funcall-interactively 'op/new-post "blog" (concat (buffer-name) "-" headline-text))
	(goto-char (point-max))
	(insert entry-text))
    (message "This function can only be called inside org-journal-mode.")) )

(defun arebel-blog-commit ()
  (interactive)
  (let ((blog-path (concat user--linux-dropbox-folder "orgmode/blog")))
    (magit-git-command (concat "git -C " blog-path " add -A"))
    (magit-git-command (concat "git -C " blog-path " commit -m \"" (format-time-string "%Y-%m-%d_%H-%M-%S") "\""))))

(defun arebel-blog-publish ()
  (interactive)
  (let ((blog-path (concat user--linux-dropbox-folder "orgmode/blog")))
    (magit-git-command (concat "git -C " blog-path " push"))
    (op/do-publication t nil nil t t)))
