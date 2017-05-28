;;; package --- init-file
;;; Author:Justin Kaipada
;;; Branch:linux
;;; Commentary:
"Thou shalt not cross 80 columns in thy file"

;;; Code:
;;(let ((file-name-handler-alist nil))
;;File handler should be enabled at the bottom as well
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa-s" . "http://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ))
(package-initialize)
(setq gc-cons-threshold 20000000)
(setq user-full-name "Justin Kaipada")
(setq user-mail-address "justinjoseph0007@gmail.com")
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p); Change yes/no to y/n
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-fill-column 81)
(blink-cursor-mode 0)
(setq disabled-command-function nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(defvar org-hide-emphasis-markers t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(setq default-directory "~/Dropbox/org-files/" )
(setq-default frame-title-format '("%f [%m%*mode]"))
;; (add-to-list 'default-frame-alist '(width  . 110))
;; (add-to-list 'default-frame-alist '(height . 37))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;-------------------------------------Server------------------
(require 'server)
(or (server-running-p)
    (server-start))
;;-------------------------------------Server------------------

(use-package evil
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "do[ne-archive]" 'my-org-archive-done-tasks)
  (define-key evil-normal-state-map "n" 'scroll-up)
  (define-key evil-normal-state-map "N" 'scroll-down)
  (define-key evil-normal-state-map (kbd "z") 'org-open-at-point)
  (define-key evil-visual-state-map (kbd "L") 'end-of-line)
  (define-key evil-visual-state-map (kbd "H") 'beginning-of-line)
  (global-set-key  (kbd "M-C-j")  'move-line-down)
  (global-set-key (kbd "M-C-k")  'move-line-up)
  (global-set-key (kbd "C-x C-m") 'move-file)
  (global-set-key (kbd "C-h")  'windmove-left)
  (global-set-key (kbd "C-l") 'windmove-right)
  (global-set-key (kbd "C-j")  'windmove-down)
  (global-set-key (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map "/" 'swiper)
  )

(use-package company
  :diminish company-mode
  :config
  (global-company-mode)
  (setq-local company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2
	company-show-numbers t
        company-idle-delay 0)
  (add-to-list 'company-transformers #'company-sort-by-occurrence)
  )

;; (use-package company-ycmd
;;   :after ycmd
;;   :config
;;   (push '(company-ycmd :with company-yasnippet company-dabbrev-code) company-backends)
;;   )

;; (setq url-show-status nil)              ; make ycmd more quiet

;; (use-package ycmd
;;   :defer t
;;   :init
;;   (setq ycmd-server-command '("python2" "~/src/ycmd/ycmd/__main__.py")
;;         ycmd-global-config "~/.emacs.d/.ycm_extra_conf.py")
;;   (defun my-lower-request-message-level ()
;;     "Lower `request-message-level' in this buffer only."
;;     (set (make-variable-buffer-local 'request-message-level) -1))
;;   (add-hook 'ycmd-mode-hook #'my-lower-request-message-level)
;;   (add-hook 'c++-mode-hook #'ycmd-mode)
;;   :config
;;   (define-key ycmd-mode-map (kbd "M-.") #'ycmd-goto)
;;   (define-key ycmd-mode-map (kbd "M-,") #'pop-tag-mark)
;;   )

(use-package company-c-headers
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  )

(use-package clang-format
  :defer t
  :init
  (global-set-key (kbd "C-c r") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)
  :config
  (setq clang-format-style-option "llvm")
  )

(use-package magit
  :bind ("C-x g" . magit-status)
  :defer t
  :config
  (add-hook 'magit-status-mode-hook 'magit-keys)
  (add-hook 'magit-log-mode-hook 'magit-keys)
  (add-hook 'magit-diff-mode-hook 'magit-keys)
  (add-hook 'magit-staged-section-mode-hook 'magit-keys)
  )

(load-file "~/.emacs.d/custom-functions.el") ;; Loads my custom-functions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)

;;Put backup files neatly away -- saved me many times
(let ((backup-dir "~/Dropbox/Code/Emacs/backups")
      (auto-saves-dir "~/Dropbox/Code/Emacs/autosavedir/")
      )
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; make electric-pair-mode work on more brackets
(defvar electric-pair-pairs '(
                              (?\{ . ?\})
                              ) )

(use-package org
  :defer t
  :mode
  ("\\.org$" . org-mode)
  :pin manual
  :config
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-agenda-mode-hook 'magit-keys)
  (use-package org-bullets
    :init
    (setq org-ellipsis "↷");Change the elipsies org mode to this arrow #Neat
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package ox-twbs)
  )

(use-package powerline)
(use-package airline-themes
  :after powerline
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'airline-solarized-alternate-gui)))
    (load-theme 'airline-solarized-alternate-gui))
  (setq airline-utf-glyph-separator-left      #xe0b0
	airline-utf-glyph-separator-right     #xe0b2
	airline-utf-glyph-subseparator-left   #xe0b1
	airline-utf-glyph-subseparator-right  #xe0b3
	airline-utf-glyph-branch              #xE0A0
	airline-utf-glyph-readonly            #xe0a2
	airline-utf-glyph-linenumber          #xe0a1 )
  )

(use-package engine-mode
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "M-a"))
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "a")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  )

(use-package web-mode
  :defer t
  :mode (("\\.djhtml\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         )
  :config
  (setq-default web-mode-markup-indent-offset tab-width)
  (setq-default web-mode-php-indent-offset tab-width)
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook  'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook  'highlight-numbers-mode)
  )

;;---------------------Python---------------------------
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)
;;---------------------Python---------------------------

(use-package emmet-mode
  :defer t
  :diminish emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :diminish auto-revert-mode
  :diminish undo-tree-mode
  :load-path "~/.emacs.d/snippets/"
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  )

(use-package flycheck
  :config
  (global-flycheck-mode t)
  )

(use-package column-marker
  :defer t
  :init
  (add-hook 'prog-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  (add-hook 'web-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  )

(use-package speed-type
  :defer t
  )

(use-package htmlize
  :defer t
  :ensure t
  )

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (aggressive-indent-global-mode t)
  )

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay  0.5) ;0.5 seconds delay time
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "rr" 'revert-buffer-no-confirm)
  (key-chord-define evil-normal-state-map "ff" 'ispell-word);Corrects singleWord
  (key-chord-define evil-normal-state-map "GG" 'org-agenda);Org-agenda
  (key-chord-mode 1)
  )

(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'css-mode-hook 'highlight-numbers-mode)
  )

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  )

(use-package ispell
  :config
  (use-package flyspell-correct
    :defer t
    :init
    (add-hook 'sgml-mode-hook 'flyspell-prog-mode)
    (add-hook 'js-mode-hook 'flyspell-prog-mode)
    ))

(use-package ivy
  :diminish ivy-mode
  :diminish auto-revert-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  )

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  )

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (global-set-key (kbd "C-c j") 'projectile-grep)
  )

(use-package counsel-projectile
  :config
  (counsel-projectile-on)
  )

(use-package all-the-icons
  :config
  (use-package all-the-icons-dired
    :defer t
    :diminish all-the-icons-dired-mode
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    ))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-color "#00ff7f")
  )

(use-package fic-mode
  ;; Highlight TODO statements in certain modes
  :defer t
  :init
  (add-hook 'prog-mode-hook 'fic-mode)
  )

(use-package hungry-delete
  ;; deletes all the whitespace when you hit backspace or delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode)
  )

(use-package expand-region
  ;; expand the marked region in semantic increments
  :diminish abbrev-mode
  :config
  (define-key evil-normal-state-map (kbd "C-v") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "C-v") 'er/expand-region)
  )

(use-package ledger-mode
  :defer t
  :diminish auto-fill-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :bind ("C-c l c" . ledger-mode-clean-buffer)
  )

(use-package cmake-mode
  :defer t
  :mode (("\\.cmake\\'" . cmake-mode)
	 ("CMakeLists\\.txt\\'" . cmake-mode))
  )

(global-set-key (kbd "M-z") 'shell-command)
(global-set-key (kbd "C-x 2") 'my-window-split-v)
(global-set-key (kbd "C-x 3") 'my-window-split-h)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "S-SPC") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))

;;  ) ;; !IMPORTANT for closing the file name handler, see begining of file
(provide 'init.el)
;;; init.el ends here
