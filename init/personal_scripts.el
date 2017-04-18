(defun arebel-putty-connect-linode-zuko ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-zuko-connect-url))

(defun arebel-dired-connect-linode-zuko ()
  (interactive)
  (dired (concat "/" user--linode-zuko-connect-url ":/")))

(defun arebel-putty-connect-linode-box ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-box-connect-url))

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
