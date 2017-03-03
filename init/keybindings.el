;;; Code:
;; Global bindings
(bind-key "C-c b" 'eval-buffer)

;; Lisp dev related bindings
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e r" 'eval-region)

(global-unset-key (kbd "C-h e"))

(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)
(bind-key "C-h e v" 'find-variable)

;; Frames related bindings
(bind-key "C-x e" 'other-frame)
(bind-key "C--" 'indent-region)

;; Change M-x to M-b
(define-key key-translation-map [?\M-x] [?\M-b])
(define-key key-translation-map [?\M-b] [?\M-x])

;; Change C-x to C-
(keyboard-translate ?\C-x ?\C-b)
(keyboard-translate ?\C-b ?\C-x)

(global-unset-key (kbd "C-x c"))

(bind-key "C-x c c" 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c"))

(defvar arebel-custom-key-map nil "Custom personal key map")
