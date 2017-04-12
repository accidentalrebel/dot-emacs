(use-package org
  :mode ("\\.org\\'" . org-mode)
  :interpreter ("org" . org-mode)
  :init
  (add-hook 'org-mode-hook (lambda()
			     (local-set-key "\C-ct" 'org-table-recalculate-buffer-tables)))
  (add-hook 'org-agenda-mode-hook (lambda()
  				    (bind-key "P" 'org-pomodoro org-agenda-mode-map)))
  :config
  (setq org-directory (if (eq system-type 'gnu/linux)
			  (expand-file-name "~/Dropbox/orgmode")
			(expand-file-name (concat user--win-dropbox-folder "orgmode/")))
  	org-default-notes-file (if (eq system-type 'gnu/linux)
				   (expand-file-name "~/Dropbox/orgmode/uncategorized.org")
				 (expand-file-name (concat user--win-dropbox-folder "orgmode/uncategorized.org")))
  	org-journal-dir (if (eq system-type 'gnu/linux)
			    (expand-file-name "~/Dropbox/orgmode/journal")
			  (expand-file-name  (concat user--win-dropbox-folder "orgmode/journal")))
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

  ;;(bind-key "P" 'org-pomodoro org-agenda-mode-map)

  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	       ("C-'" . nil))
  )
