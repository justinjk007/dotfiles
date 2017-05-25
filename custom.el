;;; package --- custom-file
;;; Author:Justin Kaipada
;;; Branch:linux
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(airline-cursor-colors nil)
 '(airline-display-directory nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(cursor-type (quote (bar . 3)))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("2b8dff32b9018d88e24044eb60d8f3829bd6bbeab754e70799b78593af1c3aba"
     "6e771f5545f720302e62fedb0adf8b254f58c1916f54dbb2df11614fc9e24c67"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "23cf1bbd82721df1785aa1a10f742e555d6ea41921b65fab0345947bdd56c3f8" default)))
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/elpa/airline-themes-20170425.1646"
     "~/.emacs.d/elpa/solarized-theme-1.2.2" custom-theme-directory t)))
 '(electric-pair-mode t)
 '(flycheck-indication-mode (quote right-fringe))
 '(flyspell-abbrev-p t)
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Dropbox/org-files/todo")))
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-agenda-todo-ignore-schedules nil)
 '(org-hide-leading-stars t)
 '(org-startup-indented t)
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (org
     flyspell-popup
     flyspell-correct
     evil
     flycheck
     rainbow-mode
     yasnippet
     solarized-theme
     emmet-mode)))
 '(powerline-height nil)
 '(ring-bell-function (quote ignore) t)
 '(scroll-restore-mode t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(solarized-distinct-doc-face nil)
 '(solarized-use-variable-pitch nil)
 '(standard-indent 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                         :strike-through nil :overline nil :underline nil :slant normal
                         :weight normal :height 110 :width normal :foundry "outline" :family
                         "Hack"))))
 '(column-marker-1 ((t (:background "dim grey"))))
 '(airline-emacs-inner ((t (:background "#002b36" :foreground "orange red"))))
 '(airline-normal-inner ((t (:background "#002b36" :foreground "orange red"))))
 '(comint-highlight-prompt ((t (:foreground "orange red"))))
 '(cursor ((t (:background "#FF7D9E"))))
 '(fic-face ((t (:foreground "#ff6a6a" :slant normal :weight bold))))
 '(error ((t (:foreground "indian red" :weight bold))))
 '(highlight-numbers-number ((t (:inherit nil :foreground "coral1"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "lightskyblue" :foreground:black :weight bold))))
 '(lazy-highlight ((t (:background "gray17" :foreground "orange red" :weight bold))))
 '(minibuffer-prompt ((t (:inherit nil :box nil :background nil :foreground "red"))))
 '(org-default ((t (:family "Hack"))))
 '(org-level-1 ((t (:foreground "#7c91ea" :family "Hack"))))
 '(org-level-2 ((t (:foreground "plum" :family "Hack"))))
 '(org-level-3 ((t (:foreground "pink" :family "Hack"))))
 '(org-link ((t (:foreground "SkyBlue1" :underline t))))
 '(org-verbatim ((t (:foreground "tomato"))))
 '(font-lock-reference-face ((t (:foreground "firebrick1"))))
 '(linum ((t (:foreground "grey68" :distant-foreground "black" :box nil :strike-through nil :underline nil :weight thin :height 110 :width normal :family "Raleway Medium"))))
 '(org-warning ((t (:foreground "red" :underline t))))
 '(org-clock-overlay ((t (:background "pale green"))))
 '(powerline-inactive1 ((t (:background "dim gray" :foreground "white"))))
 '(powerline-inactive2 ((t (:background "#4a4a4a" :foreground "pink"))))
 '(web-mode-folded-face ((t (:foreground "#F6358A" :underline nil)))))
(set-face-attribute 'region nil :foreground "#2aa198" :background "#fdf6e3")
(provide 'custom.el)
;;; custom.el ends here
