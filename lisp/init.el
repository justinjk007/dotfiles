﻿;;; package --- init-file
;;; Author:Justin Kaipada
;;; Commentary:
"Thou shalt not cross 80 columns in thy file"

;;; Code:
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("melpa-s" . "http://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(setq gc-cons-threshold (* 100 1024 1024));; 100mb
;; Increase the amount of data which Emacs reads from the process. Again the emacs default is too
;; low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq-default read-process-output-max (* 1024 1024)) ;; 1mb
(add-function :after after-focus-change-function #'garbage-collect) ;; Garbage-collect on focus-out
(setq user-full-name "Justin Kaipada"
      user-mail-address "justinjoseph0007@gmail.com")
(setq initial-scratch-message nil)
(setq message-log-max 10000)
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
(setq auto-window-vscroll nil) ;; Increase scrolling performance
(setq-default line-spacing 4)
(setq debug-on-error nil)
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; New line numbering system starts from emacs 26
(add-hook 'tex-mode-hook 'display-line-numbers-mode) ; For latex
(add-to-list 'load-path "~/.emacs.d/lisp/") ;; Tell emacs where is your personal elisp lib dir

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

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
  (define-key evil-normal-state-map "J" 'evil-scroll-line-up)
  (define-key evil-normal-state-map "K" 'evil-scroll-line-down)
  (define-key evil-normal-state-map (kbd "z") 'org-open-at-point)
  (define-key evil-visual-state-map (kbd "L") 'end-of-line)
  (define-key evil-visual-state-map (kbd "H") 'beginning-of-line)
  (global-set-key  (kbd "M-C-j")  'move-line-down)
  (global-set-key (kbd "M-C-k")  'move-line-up)
  (global-set-key (kbd "C-x C-m") 'move-file)
  (setq evil-operator-state-tag " OPERATOR "
	evil-normal-state-tag " NORMAL "
	evil-insert-state-tag " INSERT "
	evil-visual-state-tag " VISUAL "
	evil-replace-state-tag " REPLACE "
	evil-emacs-state-tag " EMACS "
	evil-motion-state-tag " MOTION " )
  (define-key minibuffer-local-map (kbd "C-y") 'yank)
  )

(use-package solarized-theme
  :pin melpa
  :config
  (load-theme 'solarized-dark t)
  (setq solarized-distinct-fringe-background t)
  (setq x-underline-at-descent-line t
	solarized-distinct-doc-face nil
	solarized-use-variable-pitch nil)
  (custom-theme-set-faces
   'solarized-dark
   `(org-block-begin-line ((t (:foreground "dim gray" :height 0.8))))
   `(org-block-end-line ((t (:foreground "dim gray" :height 0.8))))
   )
  )

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

(use-package eldoc)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  ;; Add a default commit message to clipboard when opening magit status
  (defadvice magit-status
      (before magit-status activate)
    (kill-new (concat "Update " (file-name-nondirectory (if (null buffer-file-name) "" buffer-file-name)))))
  (ad-activate 'magit-status)
  ;; Add username section in magit status buffer
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-user-header
			  )
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  )

(load-file "~/.emacs.d/custom-functions.el") ;; Loads my custom-functions
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load machine specific customizations if any!
(when (file-exists-p "~/.emacs.d/machine-specific.el")
  (progn (message "Machine specific customizations exits")
	 (load-file "~/.emacs.d/machine-specific.el")))

;;-------------------------------------Eshell------------------
(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize "┌─[" 'face `(:foreground "#b58900"))
	 (propertize (concat (eshell/pwd)) 'face `(:foreground "#93a1a1"))
	 (propertize "]\n" 'face `(:foreground "#b58900"))
	 (propertize "└─>" 'face `(:foreground "#b58900"))
	 (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "#859902"))
	 )))
;;-------------------------------------Eshell------------------

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
  :ensure org-plus-contrib
  :pin org
  :defer t
  :mode ("\\.org$" . org-mode)
  ;; :bind ("C-'" . org-cycle-agenda-files)
  :init
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'my-abbrev-mode-defs)
  (add-hook 'org-mode-hook 'olivetti-mode)
  ;; (setq org-agenda-files `(,(expand-file-name "org-files/todo.org" (getenv "DROPBOX_DIR"))))
  :config
  (defvar org-hide-emphasis-markers t) ;; Stop seeing all the markup symbols in org file, beautiful
  (setq org-file-apps
      '((auto-mode . emacs)
	(directory . emacs)
        ("cron\\." . emacs)
        ("\\.cron" . emacs)
        ("\\.bds" . emacs)
        ("\\.bld" . emacs)
        ("\\.bvt" . emacs)
        ("\\.cron" . emacs)
        ("\\.mkv" . "vlc \"%s\"")
	))
  (setq org-clock-mode-line-total 'current
	org-duration-format (quote h:mm)
	org-src-preserve-indentation 't
	)
  (font-lock-add-keywords 'org-mode
			  '(
			    ("\\<\\(✗\\)" . font-lock-warning-face)
			    ("\\<\\(✓\\)" . font-lock-keyword-face))
			  )
  (setq org-todo-keywords '((sequence "TODO" "DONE" "CANCELED")))
  (setq org-todo-keyword-faces '(
				 ("CANCELED" . font-lock-warning-face)
				 ("DONE" . font-lock-keyword-face)
				 ))
  (define-key org-mode-map (kbd "C-c m") 'org-table-mark-field)
  (define-key org-mode-map (kbd "C-c d") 'insert-date)
  (require 'ox-latex)
  (setq org-latex-listings 't)
  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     ;; (ipython . t)
     (shell . t)
     ;; Include other languages here...
     ))
  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Fix an incompatibility between the ob-async and ob-ipython packages
  ;; (setq ob-async-no-async-languages-alist '("ipython"))
  ;; Load org to media wiki lisp file
  ;; https://github.com/tomalexander/orgmode-mediawiki
  (load-file "~/.emacs.d/ox-mediawiki.el")
  )

(use-package org-roam
  ;; Need manual install on Windows, this will automatically compile a
  ;; custom SQLite DB for us on Mac and Linux
  ;; Check variable 'org-roam--sqlite-available-p' to see if db is installed
  :if (eq (string-equal system-type "windows-nt") nil) ; Don't run on windows
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Repos/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  )

(use-package org-bullets
  :after org
  :init
  (setq org-ellipsis "↷");Change the elipsies org mode to this arrow #Neat
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("●" "●" "●" "●" "●" "●" "●"))
  )

(use-package ox-twbs
  ;; Export org-mode to twitter bootstrap
  :after org
  )

(use-package ox-reveal
  :after org
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  )

(use-package ox-hugo
  :after ox
  )

(use-package csv-mode
  :mode (".tsv" ".csv" ".tabular" ".vcf")
  :config
  (setq-default csv-align-mode t)
  (setq-default csv-header-line t)
  (setq-default toggle-truncate-lines 1)
  )

(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/ -- Read all about it
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (require 'evil-markdown) ;; load .emacs.d/lisp/evil-markdown.el
  (set-face-attribute 'markdown-header-face-1 nil :foreground "#cb4b16" :height 1.3 :family "IBM Plex Mono" )
  (set-face-attribute 'markdown-header-face-2 nil :foreground "#859902" :height 1.2 :family "IBM Plex Mono" )
  (set-face-attribute 'markdown-header-face-3 nil :foreground "#6c71c4" :height 1.15 :family "IBM Plex Mono" )
  (set-face-attribute 'markdown-header-face-4 nil :foreground "#b58900" :height 1.10 :family "IBM Plex Mono" )
  (set-face-attribute 'markdown-inline-code-face nil :foreground "coral1")
  (set-face-attribute 'markdown-language-keyword-face nil :inherit font-lock-string-face)
  (setq markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook 'olivetti-mode)
  (add-hook 'markdown-mode-hook 'my-abbrev-mode-defs)
  (font-lock-add-keywords 'markdown-mode
			  '(("\\<\\(✗\\)" . font-lock-warning-face)
			    ("\\<\\(TODO\\)" . font-lock-string-face)
			    ("\\<\\(DONE\\)" . font-lock-keyword-face)
			    ("\\<\\(✓\\)" . font-lock-keyword-face))
			  )
  )

(use-package academic-phrases
  :after org-mode
  )

(use-package olivetti
  :config
  (setq-default olivetti-body-width 120)
  (add-hook 'dired-mode-hook 'olivetti-mode)
  )

(use-package shackle
  :config
  (shackle-mode t)
  (setq shackle-rules
	'(
	  (compilation-mode         :select t :other t)
	  ("*eshell*"               :select t :other t)
	  (flycheck-error-list-mode :select nil :align below :size 0.25)
	  ("*Org Select*"           :select t   :align below :size 0.33)
	  ;; ("*Org Agenda*"           :select t   :align right :size 0.33)
	  (" *Org Todo*"            :select t   :align below :size 0.2)
	  ("*Man.*"                 :select t   :align below :size 0.5  :regexp t)
	  ))
  )

(use-package multiple-cursors)
(use-package all-the-icons)

(use-package minions
  :config (minions-mode 1)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-major-mode-icon nil)
  ;; This line slows emacs down a lot in big org files on OS X alone
  ;; (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-minor-modes t) ;; Adds the gear icon with minions
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-modal-icon nil)
  (setq column-number-mode t)
  (display-time-mode 1)
  ;; Here height is the scale of the font, 0.8 means 80%
  (set-face-attribute 'doom-modeline-evil-emacs-state nil :background "#6c71c4" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-insert-state nil :background "#859902" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-motion-state nil :background "#268bd2" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-normal-state nil :background "#268bd2" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-operator-state nil :background "#268bd2" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-replace-state nil :background "#cd5c5c" :foreground "#fdf6e3" :height 1)
  (set-face-attribute 'doom-modeline-evil-visual-state nil :background "#2AA198" :foreground "#fdf6e3" :height 1)
  )

(use-package engine-mode
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "M-a"))
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "a")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine cpp-reference
    "http://en.cppreference.com/mwiki/index.php?search=%s"
    :keybinding "c")
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
	 ("\\.json\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
	 )
  :config
  (setq-default web-mode-php-indent-offset tab-width)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook  'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook  'highlight-numbers-mode)
  )

;; https://patrickskiba.com/emacs/2019/09/07/emacs-for-react-dev.html
;; (use-package tide
;;   :defer t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save))
;;   :init
;;   (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   ;; (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)
;;   ;; formats the buffer before saving
;;   (add-hook 'before-save-hook 'tide-format-before-save)
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   (add-hook 'js-mode-hook #'setup-tide-mode)
;;   ;; :config
;;   ;; (flycheck-javascript-standard-executable "/usr/bin/standardx")
;;   )

(use-package prettier
  ; Better to install on project basis like this, and then add a config file
  ; npm install --save-dev --save-exact prettier
  ; https://github.com/prettier/prettier
  ; https://github.com/jscheid/prettier.el
  :defer t
  :after web-mode
  :bind (:map web-mode-map ("C-c u" . prettier-prettify))
  :bind (:map js-mode-map ("C-c u" . prettier-prettify))
  :bind ("C-c {" . global-prettier-mode)
  )

(use-package yafolding
  :init
  (add-hook 'prog-mode-hook (lambda () (yafolding-mode)))
  )

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  )

(use-package yasnippet
  :load-path "~/.emacs.d/snippets/"
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  :preface
  ;; enable yasnippet everywhere
  ;; https://onze.io/emacs/c++/2017/03/16/emacs-cpp.html
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
	 (not company-mode/enable-yas)
	 (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize)
  )

(use-package ivy-yasnippet
  :after yasnippet
  :after ivy
  )

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (setq flycheck-python-pycompile-executable "python3")
  )

(use-package speed-type
  :defer t
  )

(use-package htmlize
  :defer t
  )

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :defer t
  )

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay              0.5) ;0.5 seconds delay time
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "rr" 'revert-buffer-no-confirm)
  (key-chord-define evil-normal-state-map "ff" 'ispell-word);Corrects singleWord
  ;; (key-chord-define evil-normal-state-map "GG" 'org-agenda);Org-agenda
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
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  )

(use-package ispell)
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (add-to-list 'exec-path "c:/Program Files (x86)/Hunspell/bin/")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    ))
 )
(cond
 ((string-equal system-type "darwin")
  (progn
    (setq ispell-program-name "/usr/local/bin/aspell")
    ))
 )

(use-package flyspell-correct
  :ensure ispell
  :defer t
  :init
  (add-hook 'tex-mode-hook 'flyspell-mode) ;; For latex
  (add-hook 'sgml-mode-hook 'flyspell-prog-mode)
  (add-hook 'js-mode-hook 'flyspell-prog-mode)
  (add-hook 'js2-mode-hook 'flyspell-prog-mode)
  )

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

(use-package avy
  :config (avy-setup-default)
  :bind ("M-g M-g" . avy-goto-line)
  )

(use-package projectile
  :config
  (projectile-global-mode)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (global-set-key (kbd "C-c j") 'projectile-grep)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
  (counsel-projectile-mode)
  (global-set-key (kbd "C-x j") 'counsel-projectile-git-grep)
  )

;; For changing dired list order
(require 'ls-lisp)
(setq ls-lisp-dirs-first t
      ls-lisp-use-insert-directory-program nil
      dired-listing-switches "-laGh1v" ;; Sort ".git" type directories first
      )

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#d33682")
  )

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )

(use-package hungry-delete
  ;; deletes all the whitespace when you hit backspace or delete
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
  :mode ("\\.ledger\\'" . ledger-mode)
  :bind (:map ledger-mode-map ("C-c u" . ledger-mode-clean-buffer))
  :config
  (add-hook 'ledger-mode-hook 'display-line-numbers-mode)
  (add-hook 'ledger-mode-hook 'olivetti-mode)
  (setq compile-command (concat "ledger -f " (file-name-nondirectory buffer-file-name) " bal --cleared" ))
  (if (eq system-type 'darwin)
      (setq ledger-binary-path "/usr/local/bin/ledger")
    )
  )

(use-package flycheck-ledger
  :after ledger-mode
  :if (string-equal system-type "gnu/linux")
  )

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package anzu
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
  :init
  (setq compile-command "go build -v")
  (setq compilation-read-command nil)
  )
(use-package company-go)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  ;; :hook (nxml-mode . lsp-deferred)
  :config
  ;; This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc
  ;; while you type. Slow it down so emacs don't get stuck.
  (setq lsp-idle-delay 0.500)
  (setq lsp-prefer-capf t) ;; company-capf, use this instead of company lsp, better performance
  :hook
  ((go-mode) . lsp)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind ("M-g f" . lsp-ui-sideline-apply-code-actions)
  :init
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'top ;; top right
	lsp-signature-auto-activate t
	lsp-ui-sideline-delay 5 ;; 5 seconds
	lsp-signature-doc-lines 1 )
  )

(use-package lsp-dart
  :init
  (if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
      (add-hook 'dart-mode-hook #'lsp)
    )
  :config
  ;;;;;;;;;;;;;;;;;
  ;; Dart config ;;
  ;;;;;;;;;;;;;;;;;
  (setq lsp-dart-sdk-dir "~/Dev/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-widget-guides 'nil)
  (setq lsp-auto-guess-root t)
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  (defun project-try-dart (dir)
    (let ((project (or (locate-dominating-file dir "pubspec.yaml")
		       (locate-dominating-file dir "BUILD"))))
      (if project
	  (cons 'dart project)
	(cons 'transient dir))))
  (cl-defmethod project-roots ((project (head dart)))
    (list (cdr project)))
  (add-hook 'project-find-functions #'project-try-dart)
  )

(use-package dart-mode
  :init
  (add-hook 'dart-mode-hook (lambda () (eldoc-mode -1))) ; This feature is given by lsp ui anyways
  )

(use-package dart-server
  ;; Added this to path D:\Dev\flutter\bin\cache\dart-sdk\bin\
  :bind (:map dart-mode-map ("C-c u" . dart-server-format))
  :init
  (add-hook 'dart-mode-hook 'dart-server)
  )

(use-package flutter
  :after dart-mode
  :bind
  (:map dart-mode-map
	("S-SPC" . #'flutter-run-or-hot-reload)
	("C-M-z" . #'flutter-hot-restart)
	)
  :custom
  ;; file mode specification error means environment var is not set
  (flutter-sdk-path "~/Dev/flutter/")
  )

(use-package evil-tutor
  :defer t
  :init
  (evil-ex-define-cmd "evil-tutor[-start]" 'evil-tutor-start)
  )

(use-package hydra
  :config
  (load-file "~/.emacs.d/myhydras.el") ;; Loads my hydras
  )

(use-package smartparens
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

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :init
  (add-hook 'yaml-mode-hook 'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook (lambda () (yafolding-mode)))
  )

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (expand-file-name "~/.emacs.d/meditate.png"))
  (setq dashboard-banner-logo-title "Lets start hacking !")
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((projects . 5)
			  (recents  . 3)))
  )

(use-package tex
  :ensure auctex
  )

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  )

(use-package js2-refactor
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil) ;; Unbind
  )

(use-package xref-js2
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook (lambda ()
			     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

(use-package crontab-mode
  :defer t
  :mode ("cron\\." . crontab-mode)
  :mode ("\\.cron" . crontab-mode)
  )

(use-package groovy-mode
  :defer t
  :mode ("\\.groovy" . groovy-mode)
  :mode ("\\Jenkinsfile\\'" . groovy-mode)
  :config
  (add-hook 'groovy-mode-hook 'rainbow-mode)
  )

;; Tramp setup ---------------
(require 'tramp)
(setq tramp-default-method "ssh")
;; Settings for tramp that are machine specific are loaded from machine-specific.el see line 40
;; Tramp setup ---------------

;; Perl modifications
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)
;; sudo cpan install Perl::Tidy
;; then use emacs command package-install-file to install from local perltidy.el
(require 'perltidy)
(define-key cperl-mode-map (kbd "C-c u") 'perltidy-dwim)

(use-package yapfify
  :config
  (eval-after-load "python"
    '(define-key python-mode-map (kbd "C-c u") 'yapfify-buffer))
  )

(use-package auto-virtualenvwrapper
  :init
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  )

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
  :init
  (require 'cc-mode)
  (add-hook 'c-mode-common-hook '(lambda ()(ignore-errors (clang-format))))
  :config
  (setq clang-format-style-option "file")
  (define-key c-mode-base-map (kbd "C-c r") 'clang-format-region)
  (define-key c-mode-base-map (kbd "C-c u") 'clang-format-buffer)
  )

(use-package cmake-mode
  :defer t
  :after cmake-project
  :mode (("\\.cmake\\'" . cmake-mode)
	 ("CMakeLists\\.txt\\'" . cmake-mode))
  )

(use-package cmake-project
  :defer t
  :bind (:map cmake-mode-map ("C-c C-c" . cmake-project-configure-project))
  :init
  (defun maybe-cmake-project-hook ()
    (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
  (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
  :config
  (setq cmake-project-default-build-dir-name "build\/")
  (setq cmake-project-architecture "Win64")
  )

(use-package ggtags
  ;; Get gnu global
  ;; sudo apt install global
  ;; http://adoxa.altervista.org/global
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  (add-to-list 'company-backends 'company-gtags)
  )

(use-package counsel-gtags
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

(use-package ansible
  :defer t
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  :config
  (setq ansible-vault-password-file "~/ansible_vault_password.txt")
  ;; (global-set-key (kbd "C-c b") 'ansible-decrypt-buffer)
  ;; (global-set-key (kbd "C-c g") 'ansible-encrypt-buffer)
  ;; (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt)
  )

;; Misc Bindings
(define-key c-mode-base-map (kbd "<M-return>") 'ff-find-other-file)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "M-Z") 'eshell)
(global-set-key (kbd "M-z") 'shell-command)
(global-set-key (kbd "C-x 2") 'my-window-split-v)
(global-set-key (kbd "C-x 3") 'my-window-split-h)
(global-set-key (kbd "C-x d") 'dired-jump)
(define-key dired-mode-map "a" (lambda () (interactive) (find-alternate-file "..")))
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "S-SPC") 'recompile)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key evil-normal-state-map (kbd ",") 'kmacro-call-macro)
(global-set-key (kbd "C-c k b") 'kill-other-buffers)
(global-set-key (kbd "C-c k t") '(lambda ()
				   (interactive )
				   (tramp-cleanup-all-buffers)
				   (tramp-cleanup-all-connections)
				   (message "Cleaned all tramp connections...")))
(global-set-key (kbd "C-c k w") '(lambda ()
				   (interactive)
				   (yafolding-show-all)
				   (delete-trailing-whitespace)
				   (message "Deleted all whitespaces...")))

(provide 'init.el)
;;; init.el ends here
