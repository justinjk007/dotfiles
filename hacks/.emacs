;;; package --- init-file for Christin's emacs
;;; Author:Justin Kaipada
;;; Commentary:

;;; Code:
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-s" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(setq gc-cons-threshold (* 50 1024 1024))
(add-hook 'focus-out-hook #'garbage-collect) ;; Garbage-collect on focus-out
(fset 'yes-or-no-p 'y-or-n-p); Change yes/no to y/n
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-fill-column 81)
(blink-cursor-mode 0)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq auto-window-vscroll nil) ;; Increase scrolling performance
(setq-default line-spacing 4)
(setq debug-on-error nil)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; New line numbering system starts from emacs 26

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(setq-default frame-title-format '("%f [%m%*mode]"))

(use-package company
  :pin melpa
  :config
  (global-company-mode t)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2
	company-show-numbers t
        company-idle-delay 0)
  (add-to-list 'company-transformers #'company-sort-by-occurrence)
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind ("C-c r" . ivy-resume)
  )

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  (define-key evil-normal-state-map "/" 'swiper)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package counsel
  ;; For the future during new emacs installation if it fails saying
  ;; use-package cannot install counsel mode, just install counsel the
  ;; old fashion way using 'M-x package-install <ret> counsel' .
  :init (counsel-mode)
  )

(use-package projectile
  :config
  (projectile-global-mode)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (global-set-key (kbd "C-c j") 'projectile-grep)
  (eval-after-load "projectile"
    '(setq projectile-mode-line
           '(:eval (list " ["
                         (propertize (projectile-project-name)
                                     'face '(:weight bold :foreground "#6c71c4"))
			 "]"))))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package ledger-mode
  :defer t
  :mode ("\\.ledger\\'" . ledger-mode)
  :bind ("C-c l c" . ledger-mode-clean-buffer)
  :config
  (add-hook 'ledger-mode-hook 'display-line-numbers-mode)
  (setq compile-command (concat "hledger -f " (file-name-nondirectory buffer-file-name) " bal --cleared" ))
  )

(global-set-key (kbd "S-SPC") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-c k w") '(lambda ()
				   (interactive)
				   (delete-trailing-whitespace)
				   (message "Deleted all whitespaces...")))

(provide '.emacs)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel ledger-mode projectile swiper ivy which-key company use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
