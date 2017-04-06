(defun arebel-haxe-build-hxml (
  (interactive)
  (shell-command (concat (replace-regexp-in-string "\n$" "" (shell-command-to-string "hg root")) "\\build.bat"))
  (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/k" "build.bat")))

(defun arebel-haxe-run-hxml ()
  (interactive)
  (let ((bat-path (concat (replace-regexp-in-string "\n$" "" (shell-command-to-string "hg root")) "\\run.bat")))
    (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe" "/k" bat-path)))

;; 2DK
(defun 2dk-run-project-debug ()
  "Run the current 2dk project."
  (interactive)
  (2dk-update-project-build-timestamp)
  (if (not (equal (buffer-name) "*compilation*"))
      (let ((other-buffer (switch-to-buffer-other-frame "*compilation*")))
	(if (not (other-buffer))
	    (switch-to-buffer-other-window "*compilation*"))))

  (projectile-run-project "2dk run flash --debug"))

(defun 2dk-update-project-build-timestamp ()
  "Updates the timestamp of the current build to the current time."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (folder-location (concat project-root "/build/web/"))
	 (file-location (concat project-root "build/web/build_info.txt"))
	 (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S")))
    (if (file-directory-p folder-location)
	(progn
	  (write-region timestamp nil file-location 0)
	  (message "Added the build timestamp: %s" timestamp))
      (message "Directory build/web does not exist! Make sure it does."))))

(defun 2dk-build-offline-package ()
  "Create the offline package."
  (interactive)
  (if (eq 'haxe-mode major-mode)
      (let ((project-root (projectile-project-root)))
	(message "Creating the offline package...")
	(async-start
	 `(lambda ()
	    ,(async-inject-variables "\\`project-root\\'")
	    (delete-directory (concat project-root "build/offline") t)
	    (copy-directory (concat project-root "tools/alpha_packager") (concat project-root "build/offline") t t t)
	    (copy-directory (concat project-root "build/web") (concat project-root "build/offline/package") t t t)
	    (shell-command (concat "7z a " project-root "build/offline/chefwars_offline_" (format-time-string "%Y-%m-%d_%H-%M-%S")
				   ".zip " project-root "build/offline/* -x!*.zip -r"))
	    "Offline package created and zipped!"
	    )
	 (lambda (result)
	   (message "%s" result)
	   (alert result :title "2dk-build-offline-package")
	   )))
    (message "Should be in a haxe-mode buffer to do this!")))

(defun 2dk-upload-zipped-offline-build-to-gdrive ()
  "Copies the zipped offline build to the local Google Drive folder."
  (interactive)
  (if (eq 'haxe-mode major-mode)
      (let ((target-directory user--win-gdrive-folder)
	    (zipped-files (f-files (concat (projectile-project-root) "build/offline")
				   (lambda (file)
				     (s-matches? ".\\.zip$" file))
				   nil)))
	(if zipped-files
	    (progn
	      (dolist (zipped-file zipped-files)
		(copy-file zipped-file target-directory 1))
	      (message "Successfully copied the zipped offline package to Google Drive"))
	  (message "There were no zipped files found in the build/offline folder!")))
    (message "Should be in a haxe-mode buffer to do this!")))

(require 'f)
(require 's)
(defun 2dk-remove-ogg-from-build ()
  "Removes all OGG files from the build."
  (interactive)
  (if (eq 'haxe-mode major-mode)
      (progn
	(message "Getting all .OGG files.")
	(let ((ogg-list (f-entries (concat (projectile-project-root) "build/web")
				   (lambda (file)
				     (s-matches? ".\\.ogg$" file))
				   t)))
	  (if ogg-list
	      (progn
		(message "Deleting: %s" ogg-list)
		(async-start
		 `(lambda ()
		    ,(async-inject-variables "\\`ogg-list\\'")
		    (dolist (file ogg-list)
		      (delete-file file t)
		      (message "Deleted %s." file))
		    "Successfully removed all .OGG files")
		 (lambda (result)
		   (message "%s" result)
		   (alert result :title "2dk-remove-ogg-from-build")
		   )))
	    (message "THere are no .OGG files found"))))
    (message "Should be in a haxe-mode buffer to do this!")))

(defun arebel-async-test ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (message "root is %s" project-root)
    (async-start
     `(lambda ()
	,(async-inject-variables "\\`project-root\\'")
	(message "project-root is %s" project-root)
	project-root)
     (lambda (result)
       (message "result %s" result)
       )
     )
    )
  )

(defun arebel-open-command-line ()
  (interactive)
  (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe"))

(defun haxe-tools-add-package-line ()
  "Uses haxe-tools-add-package-line-for-current-buffer and supplies the project root plus the source folder."
  (interactive)
  (haxe-tools-add-package-line-for-current-buffer (concat (projectile-project-root) "src/"))
  )

(global-set-key (kbd "<f5>") '2dk-run-project-debug)
(global-set-key (kbd "<f6>") '2dk-build-offline-package)
(global-set-key (kbd "<f7>") 'arebel-haxe-run-hxml)
