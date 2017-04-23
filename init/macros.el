(fset 'arebel-macro-remove-next-html-tag
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 60 return 2 67108896 19 62 return backspace] 0 "%d")) arg)))

(fset 'arebel-macro-open-href-in-browser
   [?\C-s ?h ?t ?t ?p ?: ?/ ?/ return ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-  ?\C-s ?\" return ?\C-b ?\M-w ?\M-x ?o ?r ?g ?- ?i ?n ?s ?e ?r ?t backspace backspace backspace backspace backspace backspace ?o ?p ?e ?n tab tab ?l ?i ?n ?k ?- ?f ?r ?o ?m ?- ?s ?t ?r ?i ?n ?g return ?\C-y return])
