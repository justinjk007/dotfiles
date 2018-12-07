;;; package --- Extra settings to aid emacs that are specific to each machine
;;; Author:Justin Kaipada
;;; Commentary:
;; These are extra settings to make tramp easier to use with work servers and proxy servers
;; Learn more about these customization here:
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Multi_002dhops.html#Multi_002dhops

;;; Code:

;; Default user for the main proxy server, the gsa server
(add-to-list 'tramp-default-user-alist
	     '("ssh" "canlab\\.ibm\\.com\\'" "default-user-name"))

;; For hopping, in order... host, user, proxy
(add-to-list 'tramp-default-proxies-alist
	     '("\\`machine\\.canlab\\.ibm\\.com\\'" "\\`username\\'"  "/ssh:machine.to.use..as.proxy.com:"))

;; When connecting to bpi servers use this new version of git instead of the installed old one which is incompatible with Magit
(push "/specific/path/to/git/bin/" tramp-remote-path)

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

(setenv "PATH" (concat "/usr/local/bin/:" (getenv "PATH")))
;;; machine-specific.el ends here
