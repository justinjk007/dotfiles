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
 '(cursor-type '(bar . 3))
 '(custom-safe-themes
   '("13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(electric-pair-mode t)
 '(flycheck-indication-mode nil)
 '(flyspell-abbrev-p t)
 '(frame-background-mode 'dark)
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(inhibit-compacting-font-caches t t)
 '(inhibit-startup-screen t)
 '(ledger-reports
   '(("bal cleared" "%(binary) [[ledger-mode-flags]] -f %(ledger-file) bal --cleared --real")
     (#("bal" 0 1
	(idx 0))
      "%(binary) -f %(ledger-file) bal --real")
     ("budget" "%(binary) --empty -S -T -f %(ledger-file) bal ^assets:budget")
     (#("reg" 0 1
	(idx 1))
      "%(binary) -f %(ledger-file) reg --real")
     (#("payee" 0 1
	(idx 2))
      "%(binary) -f %(ledger-file) reg @%(payee) --real")
     (#("account" 0 1
	(idx 3))
      "%(binary) -f %(ledger-file) reg %(account) --real")
     (#("account (even virtual)" 0 1
	(idx 3))
      "%(binary) -f %(ledger-file) reg %(account)")
     ("TFSA room bal" "%(binary) -S -T -f %(ledger-file) bal ^TFSAroom")
     ("TFSA room reg" "%(binary) -S -T -f %(ledger-file) reg ^TFSAroom")))
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-agenda-todo-ignore-schedules nil)
 '(org-hide-leading-stars t)
 '(org-startup-indented t)
 '(org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 '(package-enable-at-startup t)
 '(package-selected-packages
   '(ox-mediawiki orgmode-mediawiki indent-bars treesit-auto evil-markdown cmake-project cmake-mode clang-format modern-cpp-font-lock auto-virtualenvwrapper perltidy org-contrib orgalist ansible flyspell-popup flyspell-correct evil flycheck rainbow-mode yasnippet emmet-mode))
 '(package-vc-selected-packages
   '((orgmode-mediawiki :vc-backend Git :url "https://www.github.com/tomalexander/orgmode-mediawiki")
     (indent-bars :vc-backend Git :url "https://www.github.com/jdtsmith/indent-bars")))
 '(powerline-height nil)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build" "Build"))
 '(projectile-use-git-grep t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((flyspell-mode)
     (eval add-hook 'before-save-hook 'my-evaluate-time-table-automatically nil t)))
 '(scroll-restore-mode t)
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t)
 '(standard-indent 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width medium :foundry "outline" :family "IBM Plex Mono"))))
 '(ansible-section-face ((t (:foreground "#d33682"))))
 '(ansible-task-label-face ((t (:foreground "#2AA198" :weight bold))))
 '(anzu-replace-highlight ((t (:background "#d33682" :foreground "gray93" :strike-through t))))
 '(anzu-replace-to ((t (:foreground "#d33682" :weight bold))))
 '(comint-highlight-prompt ((t (:foreground "orange red"))))
 '(cursor ((t (:background "#d33682"))))
 '(error ((t (:foreground "indian red" :weight bold))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit 'diff-removed :weight bold))))
 '(evil-goggles-paste-face ((t (:inherit 'diff-added :weight bold))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit 'diff-changed :weight bold))))
 '(fic-face ((t (:foreground "#ff6a6a" :slant normal :weight bold))))
 '(font-lock-reference-face ((t (:foreground "firebrick1"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "light steel blue" :weight bold))))
 '(lazy-highlight ((t (:background "gray17" :foreground "orange red" :weight bold))))
 '(minibuffer-prompt ((t (:inherit nil :box nil :background nil :foreground "red"))))
 '(org-clock-overlay ((t (:background "pale green"))))
 '(org-default ((t (:family "IBM Plex Mono"))))
 '(org-level-1 ((t (:family "IBM Plex Mono"))))
 '(org-level-2 ((t (:family "IBM Plex Mono"))))
 '(org-level-3 ((t (:foreground "#6c71c4" :family "IBM Plex Mono"))))
 '(org-link ((t (:foreground "#b58900" :underline t :weight normal))))
 '(org-verbatim ((t (:foreground "tomato"))))
 '(org-warning ((t (:foreground "red" :underline t))))
 '(powerline-inactive1 ((t (:background "dim gray" :foreground "white"))))
 '(powerline-inactive2 ((t (:background "#4a4a4a" :foreground "pink"))))
 '(web-mode-folded-face ((t (:foreground "#d33682" :underline nil)))))


(provide 'custom.el)
;;; custom.el ends here
