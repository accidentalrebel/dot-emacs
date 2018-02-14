;;; personal_scripts.el ---
;;; Code:

(defun arebel-show-splash-screen ()
  "Show the splash screen.

Closes all buffers and opens Hackernews, Elfeed, Calendar, Org-Agenda, and Speed Type buffers."
  (interactive)
  (switch-to-buffer "*splash*")
  (delete-other-windows)

  (calendar)
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 1)
  (org-agenda-list)
  (other-window 2)
  (switch-to-buffer "*hackernews top stories*")
  (hackernews 10)
  (split-window-horizontally)
  (other-window 1)
  ;(elfeed)
  (goto-char (point-min))
  (other-window 3)
  (speed-type-text))

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
  (let ((path (concat user--mac-arebel-home-folder "Dropbox/orgmode/notes/a_quick_notes.org"))
	(current-buffer-name "a_quick_notes.org"))
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

    (pop-to-buffer current-buffer-name)
    (erase-buffer)
    (save-buffer)
    (kill-buffer current-buffer-name)
    ))

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
	     (date-string (car (split-string (car (split-string headline-text " - ")) " ")))
	     (time-string (replace-regexp-in-string ":" "-" (car (cdr (split-string (car (split-string headline-text " - ")) " ")))))
	     (entry-text (org-get-entry)))
	(message "headline-text: %s" headline-text)
	(message "post-title: %s" post-title)
	(message "date-string: %s" date-string)
	(message "time-string: %s" time-string)
	
	(op/new-post "blog" (concat date-string "_" time-string))
	(goto-char (point-max))
	(insert entry-text)
	(goto-char (point-min))
	(kill-whole-line)
	(insert (concat "#+TITLE: " post-title "\n"))
	(search-forward "#+DATE:")
	(kill-whole-line)
	(insert (concat "#+DATE: " date-string " Nan\n"))
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
  (let* ((blog-path (concat user--mac-arebel-home-folder "Dropbox/orgmode/blog"))
	 (default-directory blog-path))
    (magit-git-command (concat "git checkout source"))
    (magit-git-command (concat "git -C " blog-path " add -A"))
    (magit-git-command (concat "git -C " blog-path " commit -m \"" (format-time-string "%Y-%m-%d_%H-%M-%S") "\""))))

(defun arebel-blog-amend ()
  (interactive)
  (let* ((blog-path (concat user--mac-arebel-home-folder "Dropbox/orgmode/blog"))
	 (default-directory blog-path))
    (magit-git-command (concat "git checkout source"))
    (magit-git-command (concat "git -C " blog-path " add -A"))
    (magit-git-command (concat "git -C " blog-path " commit --amend  -m \"" (format-time-string "%Y-%m-%d_%H-%M-%S") "\""))))

(defun arebel-blog-publish ()
  (interactive)
  (let* ((blog-path (concat user--mac-arebel-home-folder "Dropbox/orgmode/blog"))
	(default-directory blog-path))
    (op/do-publication t nil nil t t)
    (magit-git-command (concat "git -C " blog-path " push"))))

(provide 'personal_scripts)

;;; personal_scripts.el ends here
