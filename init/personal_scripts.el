;;; personal_scripts.el --- 

(defun arebel-putty-connect-linode-zuko ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-zuko-connect-url))

(defun arebel-dired-connect-linode-zuko ()
  (interactive)
  (dired (concat "/" user--linode-zuko-connect-url ":/")))

(defun arebel-putty-connect-linode-box ()
  (interactive)
  (start-process "putty" nil "putty.exe" "-load" "LinodeARebel" user--linode-box-connect-url))

(defun arebel-quick-notes-to-journal-entry ()
  (interactive)
  "Converts notes made through Ogzly to Org-Journal entries."
  (let ((path (concat user--linux-arebel-home-folder "Dropbox/orgmode/notes/a_quick_notes.org")))
    (org-map-entries '(lambda ()
			(let ((scheduled-time (org-get-scheduled-time (point)))
			      (entry-content (progn
					       (org-schedule '(4))
					       (org-get-entry)))
			      (tags-string (concat (nth 5 (org-heading-components)) "quick_note:"))
			      )
			  (save-excursion
 			    (with-temp-buffer
			      (let ((org-journal-time-format ""))
				(org-journal-new-entry nil scheduled-time))
			      (insert (format-time-string org-journal-time-format scheduled-time))
			      (org-set-tags-to tags-string)
			      (org-set-tags t)
			      (forward-line 1)
			      (insert entry-content)
			      )))
			)
		     nil (list path))
    (write-region "" nil path)))

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

(defun arebel-post-dump-entry-to-org-page-post ()
  "A quick and hacky function that I made that to convert blog posts from my old website to an org-page new post buffer."
  (interactive)
  (if (eq 'org-journal-mode major-mode)
      (let* ((headline-text (nth 4 (org-heading-components)))
	     (post-title (mapconcat 'identity (cdr (split-string headline-text " - ")) " - "))
	     (date-string (car (split-string headline-text " - ")))
	     (entry-text (org-get-entry)))
	(op/new-post "blog" (replace-regexp-in-string " " "-" (replace-regexp-in-string "-" "" date-string)))
	(goto-char (point-max))
	(insert entry-text)
	(goto-char (point-min))
	(kill-whole-line)
	(insert (concat "#+TITLE: " post-title "\n"))
	(search-forward "#+DATE:")
	(kill-whole-line)
	(insert (concat "#+DATE: " (car (split-string date-string " ")) " Nan\n"))
	(kill-whole-line)
	(insert (concat "#+URI: /blog/%y/%m/%d/" (encode-string-to-url post-title) "\n"))
	(search-forward "#+TAGS:")
	(kill-whole-line)
	(insert (concat "#+TAGS: " (read-string "Input tags: " nil nil "misc") "\n"))
	(toggle-truncate-lines)
	)
    (message "This function can only be called inside org-journal-mode.")) )

(defun arebel-blog-commit ()
  (interactive)
  (let ((blog-path (concat user--linux-arebel-home-folder "Dropbox/orgmode/blog")))
    (magit-git-command (concat "git -C " blog-path " add -A"))
    (magit-git-command (concat "git -C " blog-path " commit -m \"" (format-time-string "%Y-%m-%d_%H-%M-%S") "\""))))

(defun arebel-blog-publish ()
  (interactive)
  (let ((blog-path (concat user--linux-arebel-home-folder "Dropbox/orgmode/blog")))
    (magit-git-command (concat "git -C " blog-path " push"))
    (op/do-publication t nil nil t t)))

(provide 'personal_scripts)

;;; personal_scripts.el ends here
