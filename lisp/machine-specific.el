;;; package --- Extra settings to aid emacs that are specific to each machine
;;; Author:Justin Kaipada
;;; Commentary:
;; These are extra settings to make tramp easier to use with work servers and proxy servers
;; Learn more about these customization here:
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Multi_002dhops.html#Multi_002dhops

;;; Code:

(require 'tramp)
;; Default user for the main proxy server, the gsa server
(add-to-list 'tramp-default-user-alist
	     '("ssh" "canlab\\.ibm\\.com\\'" "vabld"))
;; This overrides user just for bpidevlnx, this should be always last among the list
(add-to-list 'tramp-default-user-alist
	     '("ssh" "bpidevlnx11\\.fyre\\.ibm\\.com\\'" "kaipada"))

;; For hopping, in order... host, user, proxy
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpi25\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`extract\\.canlab\\.ibm\\.com\\'" "\\`shadowd\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpi20\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpi20\\.canlab\\.ibm\\.com\\'" "\\`root\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpi07\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpidevaix\\.canlab\\.ibm\\.com\\'" "\\`xlrtcbld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpidevaix\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`bpidevaix\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpileivt11.fyre.ibm.com:"))

;; IVT machines, use bpidevlnx as proxy to hop when connecting to all the ivt machines
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7101\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7102\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7103\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7104\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7105\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7106\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7107\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7108\\.canlab\\.ibm\\.com\\'" "\\`vainst\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))

;; Build machines, use bpidevlnx as proxy to hop when connecting to all the ivt machines
(add-to-list 'tramp-default-proxies-alist
	     '("\\`cspr7025\\.canlab\\.ibm\\.com\\'" "\\`vabld\\'"  "/ssh:bpidevlnx11.fyre.ibm.com:"))

;; When connecting to bpi servers use this new version of git instead of the installed old one which is incompatible with Magit
(push "/opt/rh/rh-git218/root/bin" tramp-remote-path)

;; Use specific shells when connecting to any machines
(add-to-list
  'tramp-connection-properties
  `(,(regexp-quote "machine1.canlab.ibm.com") "remote-shell" "/bin/bash")
  `(,(regexp-quote "machine2.google.internal.com") "remote-shell" "/bin/bash")
  )

;; Testing tramp, load custom-tramp files if they exist
;; (setq tramp-verbose 10)

(defvar ledger-binary-path "/usr/local/bin/ledger")
(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;; Changes for work macbook
(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :height 145)
      (setq-default org-hide-emphasis-markers nil) ;; See all markdown in org file, makes copying directory names, links, commands more correct
      (setq split-height-threshold nil) ;; Make windows split vertically first
      (setq split-width-threshold 0)
      )
  )

;; Load these to exec path so flutter and go lsp environment works

;; Even after setting these the best way to get emacs inhert SHELL
;; seems to be start emacs from terminal, instead from the Dock or
;; application tray

(setq exec-path
	  '(
	    "/bin"
	    "/sbin"
	    "/usr/bin"
	    "/usr/sbin"
	    "/usr/local/bin"
	    "/usr/local/gsa/bin"
	    "/Library/Apple/usr/bin"
	    "/Users/kaipada/perl5/bin"
	    "/Users/kaipada/Dev/flutter/bin/"
	    "/Users/kaipada/Dev/flutter/.pub-cache/bin/"
	    "/Users/kaipada/Dev/flutter/bin/cache/dart-sdk/bin/"
	    "/Users/kaipada/Library/Python/3.8/bin/"
	    "/usr/local/go/bin/"
	    "/Users/kaipada/go/bin"
	    )
	  )

;;; machine-specific.el ends here
