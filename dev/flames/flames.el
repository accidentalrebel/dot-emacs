;;; flames.el --- Flames

;;; Commentary:

;;; Code:

(defvar flames--wait-duration 0.2
  "Wait duration for animation.")

(defvar flames--alist '(("F" . "Friendship")
			("L" . "Love")
			("A" . "Anger")
			("M" . "Marriage")
			("E" . "Enemies")
			("S" . "Soulmates"))
  "The A-List containing the associations per letter.")

(defun flames-start ()
  "Start."
  (interactive)
  (flames--add-equals-signs)
  (flames--highlight-duplicates)
  (flames--count-and-print)
  (forward-line 1)
  (flames--count-and-print)
  (open-line 1)
  (flames--compute-total)
  (flames--play-flames-counting-animation)
  (flames--show-flames-result)
  )

(defun flames--compute-total ()
  "Computes the total."
  (let ((firstCount (progn
		      (sit-for flames--wait-duration)
		      (forward-line -1)
		      (end-of-line)
		      (backward-char 1)
		      (thing-at-point 'char t)))
	(secondCount (progn
		       (sit-for flames--wait-duration)
		       (forward-line 1)
		       (end-of-line)
		      (backward-char 1)
		       (thing-at-point 'char t))))
    (sit-for flames--wait-duration)
    (forward-line 1)
    (open-line 1)
    (insert firstCount)
    (sit-for flames--wait-duration)
    (insert " + " )
    (sit-for flames--wait-duration)
    (insert secondCount)
    (sit-for flames--wait-duration)
    (insert " = " )
    (sit-for flames--wait-duration)
    (insert (number-to-string (+ (string-to-number firstCount) (string-to-number secondCount))))
    )
  )

(defun flames--show-flames-result ()
  "Show the flames result."
  (let ((result (cdr (assoc (thing-at-point 'char) flames--alist))))
    (forward-line 1)
    (insert (propertize result 'font-lock-face '(:foreground "orange")))
    )
  )

(defun flames--count-and-print ()
  "Counts and prints the characters for flames."
  (end-of-line)
  (insert (number-to-string (- (flames--count-chars-in-name) (flames--count-duplicates-at-current-line))))
  )

(defun flames--play-flames-counting-animation ()
  "Play animation that selets the flames character."
  (forward-line 1)
  (open-line 1)
  (sit-for flames--wait-duration)
  (insert "F.L.A.M.E.S. ")
  (forward-line -1)
  (end-of-line)
  (backward-char 1)
  (let ((totalCount (string-to-number (thing-at-point 'char)))
	(index 0))
    (forward-line 1)
    (move-to-column 0)
    (while (< index (- totalCount 1))
      (sit-for flames--wait-duration)
      (if (> index 4)
	  (progn
	    (move-to-column 0)
	    (setq index (- index 6))
	    (setq totalCount (- totalCount 6)))
	(forward-char 2)
	)
      (setq index (+ index 1))
      )
    )
  )

(defun flames--count-chars-in-name()
  "Count the characters in the name at line."
  (move-to-column 0)
  (let ((charCount 0))
    (while (not (string= (thing-at-point 'char) "="))
      (when (and (not (string= (thing-at-point 'char) " ")))
	(setq charCount (+ charCount 1))
	)
      (forward-char 1)
      )
    (end-of-line)
    charCount
    )
  )

(defun flames--count-duplicates-at-current-line ()
  "Counts the duplicates in the given line/name."
  (move-to-column 0)
  (let ((charCount 0))
    (while (not (string= (thing-at-point 'char) "="))
      (when (and (not (string= (thing-at-point 'char) " "))
		  (flames--is-character-at-point-a-duplicate))
	(setq charCount (+ charCount 1))
	(message "Duplicate found %s" (number-to-string charCount))
	)
      (sit-for flames--wait-duration)
      (forward-char 1)
      )
    (end-of-line)
    charCount
    )
  )

(defun flames--highlight-duplicates ()
  "Highlight the current duplicates between two lines."
  (let ((currentIndex 0))
    (while (not (string= (thing-at-point 'char) "="))0
      (sit-for flames--wait-duration)
      (let ((currentChar (thing-at-point 'char))
	    (hasDuplicate nil)
	    )
	(forward-line)
	(sit-for flames--wait-duration)
	(move-to-column 0)
	(while (not (string= (thing-at-point 'char) "="))
	  (let ((compareChar (thing-at-point 'char)))
	    (message "Comparing %s and %s" currentChar compareChar)
	    (when (string= currentChar compareChar)
	      (message "Equal at %s" currentChar)
	      (delete-char 1)
	      (insert (propertize compareChar 'font-lock-face '(:foreground "red")))
	      (setq hasDuplicate t)
	      (backward-char 1)
	      )
	    )
	  (sit-for flames--wait-duration)
	  (forward-char 1)
	  )
	(sit-for flames--wait-duration)
	(forward-line -1)
	(move-to-column 0)
	(sit-for flames--wait-duration)
	(forward-char currentIndex)
	(when hasDuplicate
	  (delete-char 1)
	  (insert (propertize currentChar 'font-lock-face '(:foreground "red")))
	  (backward-char 1)
	  )
	(forward-char 1)
	(setq currentIndex (+ currentIndex 1))
	)
      (sit-for flames--wait-duration)
      (message (thing-at-point 'char))
      )
    )
  )

(defun flames--add-equals-signs ()
  "Add an equal sign to each name per line."
  (end-of-line)
  (insert (propertize "=" 'font-lock-face '(:foreground "orange")))
  (forward-line)
  (end-of-line)
  (insert (propertize "=" 'font-lock-face '(:foreground "orange")))
  (forward-line -1)
  (move-to-column 0)
  )

(defun flames--is-character-at-point-a-duplicate ()
  "Check if the current at point is a duplicate.
This is a hack that gets the text property at point and just checks
if the property is color red."
  (string= "red" (car (cdr (assoc ':foreground (text-properties-at (point))))))
  )

;;; flames.el ends here
