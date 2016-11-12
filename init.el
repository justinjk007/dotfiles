;;; package --- init-file
;;; Author:Justin Kaipada
;;; Date:01110100 01101111 01101101 01101101 01101111 01110010 01101111 01110111
;;; Commentary:

;;; "Thou shalt not cross 80 columns in thy file"

;;; Code:
(let ((file-name-handler-alist nil))
  (package-initialize)
  (setq gc-cons-threshold 20000000)
  (setq initial-scratch-message nil)
  
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8)

  (add-hook 'after-init-hook 'global-company-mode);Reccomended to be on the Top
  (setq default-directory "~/Desktop/" )
  (setq-default frame-title-format '("%f")) ;;Set file name as the frame title
  (add-to-list 'default-frame-alist '(width  . 83))
  (scroll-bar-mode -1)
  ;;(add-to-list 'default-frame-alist '(height . 40))
  ;;(setq initial-frame-alist '((left . 550) (top . 135)))
  (setq browse-url-browser-function 'browse-url-default-windows-browser)

  ;;-------------------------------------EVIL------------------
  (add-to-list 'load-path "~/.emacs.d/elpa/evil")
  (require 'evil)
  (evil-mode 1)
  ;;-------------------------------------EVIL------------------

  ;;-------------------------------------All-Funtions---------------

  ;;This funtions are mapped to Key-Bindings at the end of the file
  (defun revert-buffer-no-confirm ()
    "Revert(Reload/Refresh) buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
  (defun move-line-up ()
    "Move up the current line."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode))
  (defun move-line-down ()
    "Move down the current line."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode))
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 4)
    (global-set-key (kbd "C-c C-c") 'web-mode-fold-or-unfold)
    )
  ;;Put backup files neatly away-- SAVED Me many times
  (let ((backup-dir "~/Desktop/code/emacs/backups")
	(auto-saves-dir "~/Desktop/code/emacs/autosavedir"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
	(make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
	  auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	  auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	  tramp-backup-directory-alist `((".*" . ,backup-dir))
	  tramp-auto-save-directory auto-saves-dir))
  (setq backup-by-copying t    ; Don't delink hardlinks
	delete-old-versions t  ; Clean up the backups
	version-control t      ; Use version numbers on backups,
	kept-new-versions 3    ; keep some new versions
	kept-old-versions 2)   ; and some old ones, too
  (defun org-cd ()
    (cd "~/.emacs.d/org-files") ; changes the working directory
    )
  (defun my-org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (defun evilAndJdee ()
    (evil-ex-define-cmd "jc[ompile]" 'jdee-compile)
    (evil-ex-define-cmd "jr[un]" 'jdee-run)
    )
  ;;-------------------------------------All-Funtions---------------

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
   '(custom-enabled-themes (quote (dracula)))
   '(custom-safe-themes
     (quote
      ("f6bec3705ed4656c1081fae7a9fa2b63f20885db235370fd232b8bf83eea0835"
       "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
       "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
       "1e90834a232ff3b63c41b00e484754293a5c38d73080ddc6f77db72feb0b2f98"
       "b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc"
       default)))
   '(flyspell-abbrev-p t)
   '(flycheck-indication-mode (quote right-fringe))
   '(frame-background-mode nil)
   '(global-flycheck-mode t)
   '(company-idle-delay 0)
   '(global-hl-line-mode t)
   '(global-linum-mode t)
   '(lazy-highlight-cleanup nil)
   '(inhibit-startup-screen t)
   '(jdee-compiler (quote ("javac")))
   '(jdee-jdk-registry
     (quote
      (("1.8.0101" . "C:/Program Files (x86)/Java/jdk1.8.0_101"))))
   '(menu-bar-mode nil)
   '(neo-smart-open t)
   '(neo-theme (quote classic))
   '(org-agenda-files (quote ("~/.emacs.d/org-files/todo")))
   '(org-agenda-todo-ignore-deadlines nil)
   '(org-agenda-todo-ignore-schedules nil)
   '(org-hide-leading-stars t)
   '(org-startup-indented t)
   '(package-archives
     (quote
      (("melpa" . "https://stable.melpa.org/packages/")
       ("marmalade" . "https://marmalade-repo.org/packages/")
       ("gnu" . "http://elpa.gnu.org/packages/"))))
   '(package-enable-at-startup t)
   '(package-selected-packages
     (quote
      (org flyspell-popup
	   flyspell-correct
	   workgroups
	   evil
	   flycheck
	   minimap
	   rainbow-mode
	   yasnippet
	   neotree
	   dracula-theme
	   emmet-mode       )))

   '(powerline-height nil)
   '(ring-bell-function (quote ignore) t)
   '(show-paren-mode t)
   '(electric-pair-mode t)
   '(sml/mode-width (quote full))
   '(sml/theme (quote dark))
   '(standard-indent 2)
   '(tool-bar-mode nil)
   '(visible-bell t))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
			   :strike-through nil :overline nil :underline nil
			   :slant normal :weight normal :height 98
			   :width normal :foundry "outline"
			   :family "Hack"))))
   '(cursor ((t (:background "#e8e8ae"))))
   '(error ((t (:foreground "indian red" :weight bold))))
   '(highlight-numbers-number ((t (:inherit nil :foreground "coral1"))))
   '(column-marker-1 ((t (:background "light green"))))
   '(lazy-highlight ((t (:background "gray17" :foreground "red"))))
   '(org-level-1 ((t (:foreground "#7c91ea" :weight light :height 1.1))))
   '(org-level-2 ((t (:foreground "pink" :weight normal))))
   '(org-level-3 ((t (:foreground "MistyRose1" :weight bold))))
   '(org-level-4 ((t (:foreground "plum" :weight normal))))
   '(org-link ((t (:foreground "SkyBlue1" :underline t))))
   '(org-warning ((t (:foreground "red" :underline t))))
   '(font-lock-variable-name-face ((t (:foreground "#89976C"))))
   '(web-mode-folded-face ((t (:foreground "SystemMenuHilight"))))
   '(web-mode-html-tag-face ((t (:foreground "#50fa7b"))))
   '(web-mode-html-attr-name-face ((t (:foreground "#ff79c6"))))
   '(web-mode-html-attr-value-face ((t (:foreground "#e8e8ae"))))
   '(powerline-inactive1 ((t (:background "dim gray"
					  :foreground "white"))))
   '(powerline-inactive2 ((t (:background "#4a4a4a"
					  :foreground "pink")))))
  ;;Selection Colors OR Visaul Block colors
  (set-face-attribute 'region nil :foreground "#f9d6f8" :background "#4f5368")


  ;; make electric-pair-mode work on more brackets
  (setq electric-pair-pairs '(
			      (?\{ . ?\})
			      (?\< . ?\>)
			      ) )

  ;;------------------------ORG-mode-----------------------------------------
  (add-to-list 'load-path "~/.emacs.d/elpa/org-mode")
  (require 'org)
  ;;Make org-mode work with files ending in .org
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (require 'org-bullets)
  (setq org-ellipsis "↷");Change the ... org mode to this arrow #Neat
  ;;------------------------ORG-mode-----------------------------------------

  (add-to-list 'load-path "~/.emacs.d/elpa/neotree/emacs-neotree")
  (require 'neotree)

  ;;------------------------------------POWER-LINE-----------------------
  (setq airline-utf-glyph-separator-left      #x2161
	airline-utf-glyph-separator-right     #x21c1
	airline-utf-glyph-subseparator-left   #x21c5
	airline-utf-glyph-subseparator-right  #x21c5
	airline-utf-glyph-branch              #xE0A0 ;#x3bb
	airline-utf-glyph-readonly            #x2194
	airline-utf-glyph-linenumber          #x2191)
  ;;source --> https://github.com/milkypostman/powerline
  (add-to-list 'load-path "~/.emacs.d/elpa/powerline")
  (require 'powerline)
  ;;source --> https://github.com/AnthonyDiGirolamo/airline-themes
  (ignore-errors(add-to-list 'load-path "~/.emacs.d/elpa/airline-themes")
		(require 'airline-themes)
		(load-theme 'airline-kolor t))
  ;;------------------------------------POWER-LINE-----------------------

  ;;----------------------------------EMMET MODE--------------------------
  (add-to-list 'load-path "~/.emacs.d/elpa/emmet-mode")
  (require 'emmet-mode)
  ;;----------------------------------EMMET MODE--------------------------

  ;;----------------------------------Yas-snippets-------------
  (add-to-list 'load-path "~/.emacs.d/elpa/snippets")
  (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1)
  ;;----------------------------------Yas-snippets-------------

  (require 'flycheck)

  ;;-------------------------WEB-mode--------------
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (setq-default web-mode-markup-indent-offset tab-width)
  (setq-default web-mode-php-indent-offset tab-width)
  ;;------------------------------------------------

  (add-to-list 'load-path "~/.emacs.d/elpa/column-marker")
  (require 'column-marker)

  (add-to-list 'load-path "~/.emacs.d/elpa/htmlize")
  (require 'htmlize)

  (add-to-list 'load-path "~/.emacs.d/elpa/aggressive-indent")
  (require 'aggressive-indent)
  (aggressive-indent-global-mode t)

  ;;---------------JAVA Migration-------------- Kinda regret this thou.
  (add-to-list 'load-path "~/.emacs.d/elpa/jdee")
  (require 'jdee)
  ;;---------------JAVA Migration-------------- Kinda regret this thou.

  ;;----------------------------------ASPEL-DICTIONARY-------------
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "C:/Program Filesx(x86)/Aspell/dict")
  (require 'ispell)
  ;;----------------------------------ASPEL-DICTIONARY-------------

  ;;-------------------------------HOOKS--------------
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'sgml-mode-hook 'flyspell-prog-mode)
  (add-hook 'js-mode-hook 'flyspell-prog-mode)
  (add-hook 'jdee-mode-hook 'flyspell-prog-mode)
  (add-hook 'jdee-mode-hook 'evilAndJdee)
  (add-hook 'jdee-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'highlight-numbers-mode) ;Highlight-numbers-mode
  (add-hook 'prog-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode) ;;enabling rainbow in lisp
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  (add-hook 'org-mode-hook 'org-cd)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'web-mode-hook 'web-mode)
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;;-------------------------------HOOKS--------------

  ;;-------------------------------KeY-Maps--------------
  (evil-ex-define-cmd "b[utterfly]" 'butterfly)
  (evil-ex-define-cmd "sh[ell]" 'shell)
  (evil-ex-define-cmd "k[ill-this-buffer]" 'kill-this-buffer)
  (evil-ex-define-cmd "do[ne-archive]" 'my-org-archive-done-tasks)
  (global-set-key [(meta up)]  'move-line-up)
  (global-set-key [(meta down)]  'move-line-down)
  (define-key evil-normal-state-map "n" 'scroll-up)
  (define-key evil-normal-state-map "N" 'scroll-down)
  (global-set-key [f5] 'neotree-toggle)
  (global-set-key [f6] 'rainbow-mode)
  (global-set-key (kbd "<f7>") 'flyspell-mode) ;Activates the spell-checker
  (global-set-key (kbd "C-x g") 'magit-status)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "z") 'org-open-at-point)

  ;;This section uses the key-chord minor mode
  (setq key-chord-two-keys-delay  0.5) ;0.5 seconds delay time
  ;;Exit insert mode by pressing j and then j quickly
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "rr" 'revert-buffer-no-confirm)
  (key-chord-define evil-normal-state-map "ff" 'ispell-word);Corrects singleWord
  (key-chord-define evil-normal-state-map "GG" 'org-agenda);Org-agenda
  (key-chord-define evil-normal-state-map "zz" 'org-mode); Toggling org mode
  (key-chord-mode 1)
  ;;-------------------------------KeY-Maps--------------

  ) ;; !IMPORTANT for closing the file name handler, see begining of file

(provide 'init.el)
;;; init.el ends here
