;;; package --- init-file
;;; Author:Justin Kaipada
;;; Branch:windows
;;; Date:01110100 01101111 01101101 01101111 01110010 01110010 01101111 01110111
;;; Commentary:
"Thou shalt not cross 80 columns in thy file"

;;; Code:
;;(let ((file-name-handler-alist nil))
;;File handler should be enabled at the bottom as well
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa-s" . "https://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ))
(package-initialize)
(setq gc-cons-threshold 20000000)
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
  (require 'use-package))

(setq browse-url-browser-function 'browse-url-default-windows-browser)
(setq default-directory "D:/Dropbox/Code" )
(setq-default frame-title-format '("%f [%m%*mode]"))
(add-to-list 'default-frame-alist '(width  . 110))
(add-to-list 'default-frame-alist '(height . 37))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;-------------------------------------Server------------------
(require 'server)
(or (server-running-p)
    (server-start))
;;-------------------------------------Server------------------

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "do[ne-archive]" 'my-org-archive-done-tasks)
  (define-key evil-normal-state-map "n" 'scroll-up)
  (define-key evil-normal-state-map "N" 'scroll-down)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "z") 'org-open-at-point)
  (define-key evil-normal-state-map (kbd "Z") 'org-insert-link)
  (define-key evil-visual-state-map (kbd "L") 'end-of-line)
  (define-key evil-visual-state-map (kbd "H") 'beginning-of-line)
  (global-set-key  (kbd "M-C-j")  'move-line-down)
  (global-set-key (kbd "M-C-k")  'move-line-up)
  (global-set-key (kbd "C-x C-m") 'move-file)
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-status-mode-hook 'magit-keys)
  (add-hook 'magit-log-mode-hook 'magit-keys)
  (add-hook 'magit-diff-mode-hook 'magit-keys)
  (add-hook 'magit-staged-section-mode-hook 'magit-keys)
  :bind ("C-x g" . magit-status)
  )

(load-file "~/.emacs.d/custom-functions.el") ;; Loads my custom-functions

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)

;;Put backup files neatly away -- saved me many times
(let ((backup-dir "D:/Dropbox/Code/Emacs/backups")
      (auto-saves-dir "D:/Dropbox/Code/Emacs/autosavedir/")
      )
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t))
    )
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
 '(company-idle-delay 0)
 '(cursor-type (quote (bar . 3)))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "4d5ce1a2a9c608a48f0489971bf2ebededdef436107fb4278278043e37062f79"
     "6e771f5545f720302e62fedb0adf8b254f58c1916f54dbb2df11614fc9e24c67"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "23cf1bbd82721df1785aa1a10f742e555d6ea41921b65fab0345947bdd56c3f8" default)))
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/elpa/airline-themes/"
     "~/.emacs.d/elpa/solarized-theme-1.2.2" custom-theme-directory t)))
 '(electric-pair-mode t)
 '(flycheck-indication-mode (quote right-fringe))
 '(flyspell-abbrev-p t)
 '(frame-background-mode (quote dark))
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(magit-ellipsis 8631)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-files/pc" "~/.emacs.d/org-files/todo")))
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
 '(python-shell-exec-path (quote ("C:/Python27")))
 '(python-shell-prompt-detect-failure-warning t)
 '(python-shell-virtualenv-root nil)
 '(ring-bell-function (quote ignore))
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Hack"))))
 '(column-marker-1 ((t (:background "dim grey"))))
 '(airline-emacs-inner ((t (:foreground "orange red"))))
 '(airline-normal-inner ((t (:foreground "orange red"))))
 '(comint-highlight-prompt ((t (:foreground "orange red"))))
 '(cursor ((t (:background "#F73A54"))))
 '(error ((t (:foreground "indian red" :weight bold))))
 '(highlight-numbers-number ((t (:inherit nil :foreground "coral1"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "light steel blue" :weight bold))))
 '(lazy-highlight ((t (:background "gray17" :foreground "orange red" :weight bold))))
 '(font-lock-reference-face ((t (:foreground "firebrick1"))))
 '(linum ((t (:background "#fdf6e3" :foreground "black" :distant-foreground "black" :foreground "#93a1a1" :box nil :strike-through nil :underline nil :weight thin :height 100 :width normal :family "Raleway Medium"))))
 '(minibuffer-prompt ((t (:inherit nil :box nil :background nil :foreground "red"))))
 '(org-default ((t (:family "Hack"))))
 '(org-level-1 ((t (:foreground "#7c91ea" :weight light :height 1.1))))
 '(org-level-2 ((t (:foreground "plum" :weight normal))))
 '(org-level-3 ((t (:foreground "pink" :weight bold))))
 '(org-level-4 ((t (:foreground "MistyRose1" :weight normal))))
 '(org-link ((t (:foreground "SkyBlue1" :underline t))))
 '(org-verbatim ((t (:foreground "tomato"))))
 '(org-warning ((t (:foreground "red" :underline t))))
 '(powerline-inactive1 ((t (:background "dim gray" :foreground "white"))))
 '(powerline-inactive2 ((t (:background "#4a4a4a" :foreground "pink"))))
 '(web-mode-folded-face ((t (:foreground "#F6358A" :underline nil)))))
(set-face-attribute 'region nil :foreground "#2aa198" :background "#586e75")

;; make electric-pair-mode work on more brackets
(defvar electric-pair-pairs '(
                              (?\{ . ?\})
                              ) )

(use-package org
  :ensure t
  :pin manual
  :config
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-agenda-mode-hook 'magit-keys)
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    ;; (setq org-ellipsis "↷");Change the elipsies org mode to this arrow #Neat
    )
  (use-package ox-twbs
    :ensure t)
  :mode
  ("\\.org$" . org-mode)
  )

(use-package powerline
  :ensure t
  :config
  (use-package airline-themes
    :ensure t
    :config
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (select-frame frame)
                    (load-theme 'airline-solarized-alternate-gui t)))
      (load-theme 'airline-solarized-alternate-gui t))
    (setq airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xE0A0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1 ))
  )

(use-package engine-mode
  :ensure t
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
  (engine/set-keymap-prefix (kbd "M-a"))
  )

(use-package web-mode
  :ensure t
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
  :ensure t
  :defer t
  :diminish emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  )

(use-package yasnippet
  :ensure t
  :load-path "~/.emacs.d/elpa/snippets"
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  )

(use-package column-marker
  :ensure t
  :config
  (add-hook 'prog-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  (add-hook 'web-mode-hook '(lambda () (interactive) (column-marker-1 80)))
  )

(use-package speed-type
  :ensure t
  :defer t
  )

(use-package htmlize
  :ensure t
  :defer t
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (aggressive-indent-global-mode t)
  )

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay  0.5) ;0.5 seconds delay time
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "rr" 'revert-buffer-no-confirm)
  (key-chord-define evil-normal-state-map "ff" 'ispell-word);Corrects singleWord
  (key-chord-define evil-normal-state-map "GG" 'org-agenda);Org-agenda
  (key-chord-mode 1)
  )

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'css-mode-hook 'highlight-numbers-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook 'rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  )

(use-package ispell
  :init
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "C:/Program Filesx(x86)/Aspell/dict")
  :ensure t
  :config
  (use-package flyspell-correct
    :ensure t
    :config
    (add-hook 'sgml-mode-hook 'flyspell-prog-mode)
    (add-hook 'js-mode-hook 'flyspell-prog-mode)
    ))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  )

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

(use-package all-the-icons
  :ensure t
  :config
  (use-package all-the-icons-dired
    :ensure t
    :diminish all-the-icons-dired-mode
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    ))


(global-set-key (kbd "M-z") 'shell-command)
(global-set-key (kbd "C-x 2") 'my-window-split-v)
(global-set-key (kbd "C-x 3") 'my-window-split-h)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "S-SPC") 'recompile)

;;  ) ;; !IMPORTANT for closing the file name handler, see begining of file
(provide 'init.el)
;;; init.el ends here
