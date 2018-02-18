;;; Code:
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Global bindings
(bind-key "C-c C-b" 'eval-buffer)
(bind-key "C-x e" 'other-frame)

(global-unset-key (kbd "C-x C-b"))

;; Change M-x to M-b
(define-key key-translation-map [?\M-x] [?\M-b])
(define-key key-translation-map [?\M-b] [?\M-x])

;; Change C-x to C-e
(keyboard-translate ?\C-x ?\C-b)
(keyboard-translate ?\C-b ?\C-x)

;; Switch command and control for Mac-OSX
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super))

(defvar arebel-custom-key-map nil "Custom personal key map")

(bind-key "C-c f" 'find-file)
(bind-key "C-c e s" 'eshell)

