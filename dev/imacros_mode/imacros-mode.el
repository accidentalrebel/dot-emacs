(setq imacros-highlights
      '(("'.+" . font-lock-comment-face)
        ("VERSION\\|SET\\|TAG\\|TAB\\|ADD\\|SAVEAS\\|EVAL\\|PROMPT\\|URL" . font-lock-function-name-face)
        ("POS\\|TYPE\\|ATTR\\|FOLDER\\|FILE\\|BUILD\\|GOTO)" . font-lock-keyword-face)
        ("{{.+}}" . font-lock-variable-name-face)))

(define-derived-mode imacros-mode fundamental-mode "imacros"
  "major mode for editing imacros code."
  (setq font-lock-defaults '(imacros-highlights)))

