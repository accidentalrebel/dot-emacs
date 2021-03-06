(use-package org
  :mode ("\\.org\\'" . org-mode)
  :interpreter ("org" . org-mode)
  :init
  (add-hook 'org-mode-hook (lambda()
			     (local-set-key "\C-ct" 'org-table-recalculate-buffer-tables)))
  (add-hook 'org-agenda-mode-hook (lambda()
  				    (bind-key "P" 'org-pomodoro org-agenda-mode-map)))
  :config
  (setq org-directory
	(cond
	 ((eq system-type 'gnu/linuxx)
	  (if (string= (system-name) "Karbuntu")
	      (expand-file-name (concat user--linux-karbuntu-arebel-home-folder "Dropbox/orgmode/"))
	    (expand-file-name (concat user--linux-msi-arebel-home-folder "Dropbox/orgmode/"))))
	 ((eq system-type 'darwin)
	  (expand-file-name (concat user--mac-arebel-home-folder "Dropbox/orgmode/")))
	 (t
	  (expand-file-name (concat user--win-arebel-home-folder "Dropbox/orgmode/")))))

  (setq org-default-notes-file
	(cond
	 ((eq system-type 'gnu/linux)
	  (if (string= (system-name) "Karbuntu")
	      (expand-file-name (concat user--linux-karbuntu-arebel-home-folder "Dropbox/orgmode/uncategorized.org"))
	    (expand-file-name (concat user--linux-msi-arebel-home-folder "Dropbox/orgmode/uncategorized.org"))))
	 ((eq system-type 'darwin)
	  (expand-file-name (concat user--mac-arebel-home-folder "Dropbox/orgmode/uncategorized.org")))
	 (t
	  (expand-file-name (concat user--win-arebel-home-folder "Dropbox/orgmode/uncategorized.org")))))

  (setq
   org-agenda-files (list (concat org-directory "todos/"))
   org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
   org-agenda-window-setup 'current-window
   org-agenda-start-on-weekday 1
   org-log-done 'time)

  (setq org-agenda-custom-commands
	'(("c" . "My Custom Agendas")
	  ("ca" "Daily Agenda" ((org-agenda-list 1)) nil nil)
	  ("cw" "Weekly Agenda + Unscheduled" ((org-agenda-list)
					       (todo "" ((org-agenda-overriding-header "\nUnscheduled TODO")
							 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
	   nil nil)
	  ("cu" "Unscheduled TODO" ((todo "" ((org-agenda-overriding-header "\nUnscheduled TODO")
					      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
	   nil
	   nil)))

  (setq org-tag-alist
	'(("achievement" . ?a)
	  ("business" . ?b)
	  ("dev" . ?d)
	  ("emacs" . ?e)
	  ("family" . ?f)
	  ("games" . ?g)
	  ("dianne" . ?i)
	  ("books" . ?k)
	  ("mindcake" . ?m)
	  ("nsfw" . ?n)
	  ("goals" . ?o)
	  ("productivity" . ?p)
	  ("rant" . ?r)
	  ("technical" . ?t)
	  ("tv" . ?v)))

  (setq org-crypt-disable-auto-save 'encrypt)

  ;;(bind-key "P" 'org-pomodoro org-agenda-mode-map)

  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("C-'" . nil))
  )

(use-package org-journal
  :init
  (setq org-journal-dir
	(cond
	 ((eq system-type 'gnu/linux)
	  (if (string= system-name "Karbuntu")
	      (expand-file-name (concat user--linux-karbuntu-arebel-home-folder "Dropbox/orgmode/journal"))
	    (expand-file-name (concat user--linux-msi-arebel-home-folder "Dropbox/orgmode/journal"))))
	 ((eq system-type 'darwin)
	  (expand-file-name (concat user--mac-arebel-home-folder "Dropbox/orgmode/journal")))
	 (t
	  (expand-file-name  (concat user--win-arebel-home-folder "Dropbox/orgmode/journal")))))
  
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-enable-encryption nil)
  (setq epa-pinentry-mode 'loopback)
  (add-hook 'org-journal-mode-hook (lambda()
				     ;;(flyspell-mode 1)
				     (auto-save-mode)
				     ))
  )

;; 
