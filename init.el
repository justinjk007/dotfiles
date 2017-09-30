;;; package --- init-file
;;; Author:Justin Kaipada
;;; Commentary:
"Thou shalt not cross 80 columns in thy file"

;;; Code:
;;(let ((file-name-handler-alist nil))
;;File handler should be enabled at the bottom as well
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-s" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ))

(package-initialize)
(setq gc-cons-threshold (* 50 1024 1024))
(setq user-full-name "Justin Kaipada"
      user-mail-address "justinjoseph0007@gmail.com"
      )
(setq initial-scratch-message nil)
(setq message-log-max 10000)
(fset 'yes-or-no-p 'y-or-n-p); Change yes/no to y/n
(setq save-abbrevs 'silently)
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
(setq-default line-spacing 4)
(defvar org-hide-emphasis-markers t)
(fringe-mode '(8 . 6)) ; Make left fringe 8 pixel and right 6.
;; For emacs 26
;; (line-number              (:foreground red :background black))
;; (line-number-current-line (:foreground orange :background black))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (setq browse-url-browser-function 'browse-url-default-windows-browser)
    ))
 )

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
  (setq evil-operator-state-tag "OPERATOR"
	evil-normal-state-tag "NORMAL"
	evil-insert-state-tag "INSERT"
	evil-visual-state-tag "VISUAL"
	evil-replace-state-tag "REPLACE"
	evil-emacs-state-tag "EMACS"
	evil-motion-state-tag "MOTION" )
  )

(use-package solarized-theme
  :pin melpa
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

(use-package magit
  ;; Install Diffutils for windows from here
  ;; Step1 - http://gnuwin32.sourceforge.net/packages/diffutils.htm
  ;; Step2 - And add the bin folder to path
  ;; Try this instead of step 2 one day ->
  ;; (when (string-equal system-type "windows-nt")
  ;;   (progn
  ;;     (setq diff-path "your-diff-path")
  ;;     (setenv "PATH"
  ;; 	      (concat diff-path ";"))
  ;;     (setq exec-path
  ;; 	    '(diff-path))))
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
      ring-bell-function 'ignore)

;;Put backup files neatly away -- saved me many times
(let ((backup-dir "~/.emacs.d/Emacs/backups")
      (auto-saves-dir "~/.emacs.d/Emacs/autosavedir/")
      )
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir
        )
  )
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
  :config
  (setq org-clock-mode-line-total 'current)
  (setq org-agenda-files `(,(expand-file-name "org-files/todo.org" (getenv "DROPBOX_DIR"))))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'my-abbrev-mode-defs)
  (add-hook 'org-agenda-mode-hook 'magit-keys)
  )
(use-package org-bullets
  :after org
  :init
  (setq org-ellipsis "↷");Change the elipsies org mode to this arrow #Neat
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "●"))
  )
(use-package ox-twbs
  :after org
  )
(use-package ox-reveal
  :after org
  :config
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.5.0/")
  )

(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/ -- Read all about it
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package spaceline
  :ensure powerline
  ;; https://gist.github.com/epegzz/1634235/fe5100a91157c5d0f0c8b7b6dedd126c6396ae19
  )
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme)
  (set-face-attribute 'spaceline-evil-emacs nil :foreground "#fdf6e3" :background "#6c71c4")
  (set-face-attribute 'spaceline-evil-normal nil :foreground "#fdf6e3" :background "#268bd2")
  (set-face-attribute 'spaceline-evil-insert nil :foreground "#fdf6e3" :background "#859902")
  (set-face-attribute 'spaceline-evil-visual nil :foreground "#fdf6e3" :background "#2AA198")
  (set-face-attribute 'spaceline-evil-motion nil :foreground "#fdf6e3" :background "#268bd2")
  (set-face-attribute 'spaceline-evil-replace nil :foreground "#fdf6e3" :background "#cd5c5c")
  (spaceline-toggle-evil-state-on)
  (spaceline-toggle-org-clock-on)
  (spaceline-toggle-projectile-root-on)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq ns-use-srgb-colorspace nil)
  (setq-default
   powerline-height 19
   powerline-default-separator 'bar
   spaceline-flycheck-bullet "❖ %s")
  (setq x-underline-at-descent-line t)
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  ;; fancy git icon
  (defadvice vc-mode-line (after strip-backend () activate)
    (propertize (all-the-icons-octicon "git-branch")
		'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
		'display '(raise -0.1))
    (when (stringp vc-mode)
      (let ((gitlogo (replace-regexp-in-string
		      "^ Git"
		      (propertize (all-the-icons-octicon
				   "git-branch"
				   :face 'all-the-icons-orange)
				  'display '(raise 0)
				  ) vc-mode)))
	(setq vc-mode gitlogo))))
  (spaceline-compile)
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

(use-package emmet-mode
  :defer t
  :diminish emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :diminish undo-tree-mode
  :load-path "~/.emacs.d/snippets/"
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  )

(use-package flycheck
  :diminish flycheck-mode
  :diminish major-mode-icons-mode
  :config
  (global-flycheck-mode t)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    (vector #b00000000
            #b10000000
            #b11000000
            #b11100000
            #b11110000
            #b11111000
            #b11111100
            #b11111110
            #b11111111
            #b11111110
            #b11111100
            #b11111000
            #b11110000
            #b11100000
            #b11000000
            #b10000000
            #b00000000))
  )

(use-package speed-type
  :defer t
  )

(use-package htmlize
  :defer t
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
  ;; MatLab doesnot like me and vice versa
  (add-to-list 'aggressive-indent-excluded-modes 'octave-mode)
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
  :config
  (set-face-attribute 'highlight-numbers-number nil :inherit nil :foreground "coral1")
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
  (cond
   ((string-equal system-type "windows-nt")
    (progn
      :init
      (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
      (setq ispell-program-name "aspell")
      (setq ispell-personal-dictionary "C:/Program Filesx(x86)/Aspell/dict")
      ))
   )
  :config
  (use-package flyspell-correct
    :defer t
    :init
    (add-hook 'sgml-mode-hook 'flyspell-prog-mode)
    (add-hook 'js-mode-hook 'flyspell-prog-mode)
    ))

(use-package ivy
  :diminish ivy-mode
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
  (define-key evil-normal-state-map "/" 'swiper)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package counsel
  :ensure swiper
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
  (global-set-key (kbd "C-x j") 'counsel-grep)
  )

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (global-set-key (kbd "C-c j") 'projectile-grep)
  (eval-after-load "projectile"
    '(setq projectile-mode-line
           '(:eval (list " ["
                         (propertize (projectile-project-name)
                                     'face '(:weight bold :inherit t :foreground "#cd5c5c"))
			 "]"))))
  )

(use-package ibuffer-projectile
  :defer t
  :init
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))
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
    :config
    (require 'ls-lisp)
    (setq ls-lisp-dirs-first t
	  ls-lisp-use-insert-directory-program nil
	  )
    ))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-color "#00ff00")
  )

(use-package evil-goggles
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
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
  :config
  (define-key evil-normal-state-map (kbd "C-v") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "C-v") 'er/expand-region)
  )

(use-package ledger-mode
  :defer t
  :diminish auto-fill-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :bind ("C-c l c" . ledger-mode-clean-buffer)
  :config
  (setq compile-command (concat "ledger -f " (file-name-nondirectory buffer-file-name) " bal --cleared" ))
  )

(use-package nlinum
  :defer t
  :diminish auto-revert-mode
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'ledger-mode-hook 'nlinum-mode)
  )

(use-package anzu
  :diminish anzu-mode
  :init
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-c a r") 'anzu-query-replace-at-cursor-thing)
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-replace-to nil :foreground "#d33682")
  )

(use-package go-mode
  ;; go get -u github.com/dougm/goflymake
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  )
(use-package company-go)

(use-package evil-tutor
  :defer t
  :init
  (evil-ex-define-cmd "evil-tutor[-start]" 'evil-tutor-start)
  )

(use-package hydra
  :config
  (global-set-key
   (kbd "C-c C-v")
   (defhydra toggle ()
     "toggle"
     ("f" auto-fill-mode "fill" :color blue)
     ("t" toggle-truncate-lines "truncate" :color blue)
     ("w" whitespace-mode "whitespace" :color blue)
     ("q" nil "cancel")))

  (defhydra hydra-next-error
    (global-map "C-x")
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))
  )

(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  )

(use-package octave
  ;; Used for matlab and octave files
  :mode ("\\.m\\'" . octave-mode)
  )

(use-package wotd
  ;; Gives you word of the day
  :defer t
  :bind ("C-c C-w" . wotd-select)
  )

(use-package dashboard
  :diminish page-break-lines-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Lets start hacking !")
  (setq dashboard-items '((projects . 5)
			  (recents  . 3)
			  (agenda . 3)))
  )

(use-package tex
  :ensure auctex
  )

(use-package origami
  :defer t
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  :bind
  ("C-c C-f" . origami-toggle-node )
  )

;; C++ Config -----------------------------------------------------------------

(use-package modern-cpp-font-lock
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  :config
  (set-fill-column 100) ;; Equal with clang format
  (setq-default c-basic-offset 4) ;; Eqaul with clang format
  (font-lock-add-keywords 'c++-mode
			  '(("\\<\\(FIXME\\):" 1 font-lock-constant-face))
			  )
  )

(use-package company-c-headers
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  )

;; Install LLVM - http://releases.llvm.org/download.html
;; Install MingW - http://mingw-w64.org/doku.php - 32 bit version
(use-package clang-format
  :defer t
  :init
  (global-set-key (kbd "C-c r") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)
  :config
  (setq clang-format-style-option "file")
  ;; Stop aggressive-indent mode to use clang-format
  (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'c-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cmake-project-mode)
  )

(use-package cmake-mode
  :defer t
  :after cmake-project
  :mode (("\\.cmake\\'" . cmake-mode)
	 ("CMakeLists\\.txt\\'" . cmake-mode))
  )

(use-package cmake-project
  :defer t
  :init
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
  :config
  (setq cmake-project-default-build-dir-name "build\/")

  ;; TODO Checkout cmake-ide https://github.com/atilaneves/cmake-ide
  )

(use-package irony
  :diminish irony-mode
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package company-irony
  :ensure irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (setq w32-pipe-read-delay 0)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024))
  :config
  (setq irony-additional-clang-options '("-std=c++14"))
  (setq company-idle-delay              nil
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  )

(use-package irony-eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(use-package flycheck-irony
  :defer t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

(use-package ggtags
  ;; Get gnu global
  ;; sudo apt install global
  ;; http://adoxa.altervista.org/global/
  :diminish ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )

(use-package counsel-gtags
  :diminish counsel-gtags-mode
  :after ggtags
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(counsel-gtags-mode))))
  :bind (("M-t" . counsel-gtags-find-definition)
	 ("M-r" . counsel-gtags-find-reference)
	 ("M-s" . counsel-gtags-find-symbol)
	 ("M-," . counsel-gtags-go-backward))
  )

;; C++ Config -----------------------------------------------------------------

;; Misc Bindings
(global-set-key (kbd "M-z") 'shell-command)
(global-set-key (kbd "C-x 2") 'my-window-split-v)
(global-set-key (kbd "C-x 3") 'my-window-split-h)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "S-SPC") 'recompile)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k o b") 'kill-other-buffers)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;  ) ;; !IMPORTANT for closing the file name handler, see begining of file
(provide 'init.el)
;;; init.el ends here
