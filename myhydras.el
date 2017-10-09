(global-set-key
 (kbd "C-c C-v")
 (defhydra toggle ()
   "toggle"
   ("f" auto-fill-mode "fill" :color blue)
   ("t" toggle-truncate-lines "truncate" :color blue)
   ("w" whitespace-mode "whitespace" :color blue)
   ("q" nil "cancel"))
 )

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
  ("q" nil            nil :color blue)
  )

;;;; Table Field Marking
(defun org-table-mark-field ()
  "Mark the current table field."
  (interactive)
  (require 'org-table)
  ;; Do not try to jump to the beginning of field if the point is already there
  (when (not (looking-back "|[[:blank:]]?"))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

(defhydra hydra-org-table-mark-field
  (:body-pre (org-table-mark-field)
	     :color red
	     :hint nil)
  "
   ^^      ^▲^     ^^
   ^^      _k_     ^^
 ◀ _h_  selection  _l_ ▶          Org table mark field
   ^^      _j_     ^^
   ^^      ^▼^     ^^
"
  ("x" exchange-point-and-mark "exchange point/mark")
  ("l" (lambda (arg)
	 (interactive "p")
	 (when (eq 1 arg)
	   (setq arg 2))
	 (org-table-end-of-field arg)))
  ("h" (lambda (arg)
	 (interactive "p")
	 (when (eq 1 arg)
	   (setq arg 2))
	 (org-table-beginning-of-field arg)))
  ("j" next-line)
  ("k" previous-line)
  ("q" nil "cancel" :color blue))

(bind-keys
 :filter (org-at-table-p)
 ("C-c m" . hydra-org-table-mark-field/body)
 )

;; ------------------- Hydras for gnus --------------------------------.
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do ?:"
       ("s" gnus-summary-show-thread "Show thread")
       ("h" gnus-summary-hide-thread "Hide thread")
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))
;; ------------------- Hydras for gnus --------------------------------.
