(defun arebel-putty-connect-linode-zuko ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-zuko-connect-url))

(defun arebel-dired-connect-linode-zuko ()
  (interactive)
  (dired (concat "/" user--linode-zuko-connect-url ":/")))

(defun arebel-putty-connect-linode-box ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-box-connect-url))
