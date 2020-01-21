;;; package --- custom-file
;;; Author:Justin Kaipada
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(cursor-type (quote (bar . 3)))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    (
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     default)))
 '(electric-pair-mode t)
 '(flycheck-indication-mode nil)
 '(flyspell-abbrev-p t)
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(inhibit-startup-screen t)
 '(ledger-reports
   (quote
    (("bal cleared" "ledger [[ledger-mode-flags]] -f %(ledger-file) bal --cleared")
     (#("bal" 0 1
	(idx 0))
      "%(binary) -f %(ledger-file) bal")
     (#("reg" 0 1
	(idx 1))
      "%(binary) -f %(ledger-file) reg")
     (#("payee" 0 1
	(idx 2))
      "%(binary) -f %(ledger-file) reg @%(payee)")
     (#("account" 0 1
	(idx 3))
      "%(binary) -f %(ledger-file) reg %(account)"))))
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-agenda-todo-ignore-schedules nil)
 '(org-hide-leading-stars t)
 '(org-startup-indented t)
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
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
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build" "Build")))
 '(projectile-use-git-grep t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
 	   (quote before-save-hook)
 	   (quote my-evaluate-time-table-automatically)
 	   nil t))))
 '(scroll-restore-mode t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(standard-indent 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                         :strike-through nil :overline nil :underline nil :slant normal
                         :weight normal :height 110 :width medium :foundry "outline" :family
                         "IBM Plex Mono"))))
 ;; https://github.com/source-foundry/Hack/releases
 '(comint-highlight-prompt ((t (:foreground "orange red"))))
 '(cursor ((t (:background "#d33682"))))
 '(fic-face ((t (:foreground "#ff6a6a" :slant normal :weight bold))))
 '(error ((t (:foreground "indian red" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:background "light steel blue" :weight bold))))
 '(lazy-highlight ((t (:background "gray17" :foreground "orange red" :weight bold))))
 '(font-lock-reference-face ((t (:foreground "firebrick1"))))
 '(minibuffer-prompt ((t (:inherit nil :box nil :background nil :foreground "red"))))
 '(org-default ((t (:family "IBM Plex Mono"))))
 '(org-level-1 ((t (:family "IBM Plex Mono"))))
 '(org-level-2 ((t (:family "IBM Plex Mono"))))
 '(org-level-3 ((t (:foreground "#6c71c4" :family "IBM Plex Mono"))))
 '(org-link ((t (:foreground "SkyBlue1" :underline t))))
 '(org-verbatim ((t (:foreground "tomato"))))
 '(org-warning ((t (:foreground "red" :underline t))))
 '(org-clock-overlay ((t (:background "pale green"))))
 '(evil-goggles-delete-face ((t (:inherit (quote diff-removed) :weight bold ))))
 '(evil-goggles-paste-face ((t (:inherit (quote diff-added) :weight bold ))))
 '(evil-goggles-yank-face ((t (:inherit (quote diff-changed) :weight bold))))
 '(powerline-inactive1 ((t (:background "dim gray" :foreground "white"))))
 '(powerline-inactive2 ((t (:background "#4a4a4a" :foreground "pink"))))
 '(web-mode-folded-face ((t (:foreground "#F6358A" :underline nil)))))

(custom-theme-set-faces
 'solarized-dark
 `(org-block-begin-line ((t (:foreground "dim gray" :height 0.8))))
 `(org-block-end-line ((t (:foreground "dim gray" :height 0.8))))
 )

(provide 'custom.el)
;;; custom.el ends here
