;;; package --- init-file
;;; Author:Justin Kaipada
;;; Branch:windows
;;; Date:01110100 01101111 01101101 01101101 01101111 01110010 01101111 01110111
;;; Commentary:
"Thou shalt not cross 80 columns in thy file"

;;; Code:
;;(let ((file-name-handler-alist nil))
;;File handler should be enabled at the bottom as well
(package-initialize)
(setq gc-cons-threshold 20000000)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p); Change yes/no to y/n
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-fill-column 80)
(blink-cursor-mode 0)
(put 'upcase-region 'disabled nil)
(defvar org-hide-emphasis-markers t)

(add-hook 'after-init-hook 'global-company-mode);Reccomended to be on the Top
(setq browse-url-browser-function 'browse-url-default-windows-browser)
(setq default-directory "D:/Dropbox/Code" )
(setq-default frame-title-format '("%f")) ;;Set file name as the frame title
(add-to-list 'default-frame-alist '(width  . 110))
(add-to-list 'default-frame-alist '(height . 37))
(scroll-bar-mode -1)

;;-------------------------------------Server------------------
(require 'server)
(or (server-running-p)
    (server-start))
;;-------------------------------------Server------------------

;;-------------------------------------EVIL------------------
(add-to-list 'load-path "~/.emacs.d/elpa/evil")
(require 'evil)
(evil-mode 1)
;;-------------------------------------EVIL------------------

;;-------------------------------------All-Funtions---------------

;;This funtions are mapped o Key-Bindings at the end of the file
(defun revert-buffer-no-confirm ()
  "Revert(Reload/Refresh) buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(defun move-line-up ()
  "Move up the currnt line."
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
  "Change when using web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 4)
  (global-set-key (kbd "C-c C-c") 'web-mode-fold-or-unfold))
(defun my-org-archive-done-tasks ()
  "Move all done tasks in the current buffer to archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(defun magit-keys()
  "Change emacs evil mode n and p to j and k repectively"
  (define-key evil-emacs-state-map (kbd "j") 'next-line)
  (define-key evil-emacs-state-map (kbd "k") 'previous-line))
(defun insert-date (prefix)
  "Insert the current date with as PREFIX."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d-%m-%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))
(defun my-window-split-h (prefix)
  (interactive "p")
  "Splits window right with older window open"
  (split-window-right)
  (other-window 1 nil)
  (if (- prefix 1) (switch-to-next-buffer)))
(defun my-window-split-v (prefix)
  (interactive "p")
  "Splits window below with older window open"
  (split-window-below)
  (other-window 1 nil)
  (if (- prefix 1) (switch-to-next-buffer)))
(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

(setq visible-bell nil
      ring-bell-function 'my-terminal-visible-bell)

;;Put backup files neatly away -- saved me many times
(let ((backup-dir "~/Dropbox/Code/Emacs/backups")
      (auto-saves-dir "~/Dropbox/Code/Emacs/autosavedir")
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
 '(company-idle-delay 0)
 '(cursor-type (quote (bar . 3)))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4d5ce1a2a9c608a48f0489971bf2ebededdef436107fb4278278043e37062f79" "6e771f5545f720302e62fedb0adf8b254f58c1916f54dbb2df11614fc9e24c67" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "23cf1bbd82721df1785aa1a10f742e555d6ea41921b65fab0345947bdd56c3f8" default)))
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
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-files/pc" "~/.emacs.d/org-files/todo")))
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
 '(standard-indent 2)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                         :strike-through nil :overline nil :underline nil
                         :slant normal :weight normal :height 100
                         :width normal :foundry "outline" :family "Hack"))))
 '(column-marker-1 ((t (:background "dim grey"))))
 '(comint-highlight-prompt ((t (:foreground "orange red"))))
 '(cursor ((t (:background "#F73A54"))))
 '(error ((t (:foreground "indian red" :weight bold))))
 '(highlight-numbers-number ((t (:inherit nil :foreground "coral1"))))
 '(lazy-highlight ((t (:background "gray17" :foreground "orange red" :weight bold))))
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
(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            ) )

;;------------------------ORG-mode-----------------------------------------
(add-to-list 'load-path "~/.emacs.d/elpa/org-mode")
(require 'org)
;;Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org-bullets)
;; (setq org-ellipsis "↷");Change the elipsies org mode to this arrow #Neat

;;------------------------ORG-mode-----------------------------------------

;;------------------------------------POWER-LINE-----------------------
;;source --> https://github.com/milkypostman/powerline
(add-to-list 'load-path "~/.emacs.d/elpa/powerline")
(require 'powerline)

;;source --> https://github.com/AnthonyDiGirolamo/airline-themes
(ignore-errors
  (add-to-list 'load-path "~/.emacs.d/elpa/airline-themes")
  (require 'airline-themes)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'airline-solarized-alternate-gui t)))
    (load-theme 'airline-solarized-alternate-gui t))
  )

(setq airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xE0A0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1 )
;;------------------------------------POWER-LINE-----------------------

;;-----------------------------------Engine-mode-----------------------
(require 'engine-mode)
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
  :keybinding "g")
(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")
(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")
(engine-mode t)
;;-----------------------------------Engine-mode-----------------------

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

(add-to-list 'load-path "~/.emacs.d/elpa/column-marker")
(require 'column-marker)

(add-to-list 'load-path "~/.emacs.d/elpa/speed-type")
(require 'speed-type)

(add-to-list 'load-path "~/.emacs.d/elpa/htmlize")
(require 'htmlize)

(add-to-list 'load-path "~/.emacs.d/elpa/aggressive-indent")
(require 'aggressive-indent)
(aggressive-indent-global-mode t)

;;----------------------------------ASPEL-DICTIONARY-------------
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Filesx(x86)/Aspell/dict")
(require 'ispell)
;;----------------------------------ASPEL-DICTIONARY-------------

;;-------------------------------HOOKS--------------
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-agenda-mode-hook 'magit-keys)
(add-hook 'sgml-mode-hook 'flyspell-prog-mode)
(add-hook 'js-mode-hook 'flyspell-prog-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode) ;Highlight-numbers-mode
(add-hook 'prog-mode-hook '(lambda () (interactive) (column-marker-1 80)))
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode) ;;enabling rainbow in lisp
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'web-mode-hook '(lambda () (interactive) (column-marker-1 80)))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'rainbow-delimiters-mode)
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'highlight-numbers-mode)
(add-hook 'css-mode-hook 'highlight-numbers-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-delimiters-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'magit-status-mode-hook 'magit-keys)
(add-hook 'magit-log-mode-hook 'magit-keys)
(add-hook 'magit-diff-mode-hook 'magit-keys)
(add-hook 'magit-staged-section-mode-hook 'magit-keys)
;;-------------------------------HOOKS--------------

;;-------------------------------KeY-Maps--------------
(engine/set-keymap-prefix (kbd "M-a"))
(evil-ex-define-cmd "do[ne-archive]" 'my-org-archive-done-tasks)
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(define-key evil-normal-state-map "n" 'scroll-up)
(define-key evil-normal-state-map "N" 'scroll-down)
(global-set-key (kbd "<f7>") 'flyspell-mode) ;Activates the spell-checker
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-z") 'shell-command)
(global-set-key (kbd "C-x 2") 'my-window-split-v)
(global-set-key (kbd "C-x 3") 'my-window-split-h)
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "S-SPC") 'recompile)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "z") 'org-open-at-point)
(define-key evil-normal-state-map (kbd "Z") 'org-insert-link)
(define-key evil-visual-state-map (kbd "L") 'end-of-line)
(define-key evil-visual-state-map (kbd "H") 'beginning-of-line)
(define-key evil-visual-state-map (kbd "q") 'kill-this-buffer)
(global-set-key (kbd "C-x C-m") 'move-file)

;;This section uses the key-chord minor mode
(setq key-chord-two-keys-delay  0.5) ;0.5 seconds delay time
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-normal-state-map "rr" 'revert-buffer-no-confirm)
(key-chord-define evil-normal-state-map "ff" 'ispell-word);Corrects singleWord
(key-chord-define evil-normal-state-map "GG" 'org-agenda);Org-agenda
(key-chord-mode 1)
;; -------------------------------KeY-Maps--------------

;;  ) ;; !IMPORTANT for closing the file name handler, see begining of file
(provide 'init.el)
;;; init.el ends here
