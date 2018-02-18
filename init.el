;;; init.el -- My init file
;;; Commentary:
;;; Code:

(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq load-prefer-newer t)
(prefer-coding-system 'utf-8)

(if (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
  (require 'cask))
(cask-initialize)
(require 'pallet)
(pallet-mode 1)

(require 'use-package)

(when (file-exists-p "~/.emacs.d/user.el")
  (load "~/.emacs.d/user.el"))

(load "~/.emacs.d/init/bootstrap")
(load "~/.emacs.d/init/tools")
(load "~/.emacs.d/init/org_mode")
(load "~/.emacs.d/init/haxe_mode")
(load "~/.emacs.d/init/dev_scripts")
(load "~/.emacs.d/init/personal_scripts")
(load "~/.emacs.d/init/personal_config")

(when (file-exists-p "~/.emacs.d/dev/haxe-tools/haxe-tools.el")
  (require 'haxe-tools "~/.emacs.d/dev/haxe-tools/haxe-tools"))

(when (file-exists-p "~/.emacs.d/dev/emacs_asana/emacs_asana.el")
  (require 'emacs-asana "~/.emacs.d/dev/emacs_asana/emacs_asana.el"))

(when (file-exists-p "~/.emacs.d/dev/tic-tac-toe/tic-tac-toe.el")
  (add-to-list 'load-path "~/.emacs.d/dev/tic-tac-toe")
  (require 'tic-tac-toe))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(asana-selected-workspace nil t)
 '(csv-field-index-mode t)
 '(csv-separators (quote ("," ";")))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "cdf96318f1671344564ba74ef75cc2a3f4692b2bee77de9ce9ff5f165de60b1f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(doom-one-brighter-comments t)
 '(doom-one-brighter-modeline nil)
 '(doom-one-comment-bg t)
 '(fci-rule-color "#555556")
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#525254"))
 '(magit-submodule-arguments nil)
 '(mode-line-in-non-selected-windows t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up priority-down category-keep todo-state-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (projectile-ripgrep omnisharp yasnippet-snippets cargo ghub pocket-reader smex ripgrep cask racer csharp-mode powerline json-reformat json-snatcher flycheck-rust rust-mode doom-themes smart-mode-line-powerline-theme smart-mode-line pomidor simpleclip eww-lnum google-this request speed-type haxe-mode swiper hackernews alert async deferred git-commit markdown-mode company request-deferred drag-stuff expand-region flycheck-cask free-keys goto-chg idle-highlight-mode multiple-cursors eshell-up ivy-hydra counsel-projectile counsel ivy coin-ticker ido-vertical-mode flycheck-package keyfreq golden-ratio gotham-theme dired-du org s undo-tree websocket yaml-mode hydra list-unicode-display diminish magit-popup nyan-mode imgur twittering-mode elmacro csv-mode all-the-icons flycheck package-build restart-emacs smartparens web-mode o-blog org-page sudoku open-junk-file json-mode which-key ag with-editor yasnippet pallet php-mode f 2048-game ace-window rainbow-delimiters exec-path-from-shell use-package slack org-pomodoro synosaurus dokuwiki markdown-mode+ org2blog avy dokuwiki-mode haxe-imports evil-smartparens org-journal monky magit evil better-defaults ##)))
 '(send-mail-function nil)
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#202730" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 144 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(aw-leading-char-face ((t (:background "cyan" :foreground "black" :height 2.0))))
 '(hackernews-link ((t (:inherit link :foreground "light green" :underline nil))))
 '(mode-line ((t (:background "dark slate gray" :foreground "cyan"))))
 '(pomidor-break-face ((t (:inherit (quote font-lock-constant-face)))))
 '(pomidor-time-face ((t (:height 2.0))))
 '(pomidor-timer-face ((t (:height 2.0))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "medium orchid"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "medium orchid"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "orange red"))))
 '(tomatinho-current-ok-face ((t (:inherit tomatinho-ok-face :height 2.5))))
 '(tomatinho-current-pause-face ((t (:inherit tomatinho-pause-face :height 2.5))))
 '(tomatinho-time-face ((t (:height 2.5 :width semi-condensed :family "DejaVu Sans")))))

(setq inhibit-startup-screen t)

(set-face-attribute 'default nil :height 155)

(when (fboundp 'arebel-show-splash-screen)
  (arebel-show-splash-screen))
