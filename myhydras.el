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
   ^^      ^🠙^     ^^
   ^^      _p_     ^^
🠘 _b_  selection  _f_ 🠚          | Org table mark ▯field▮ |
   ^^      _n_     ^^
   ^^      ^🠛^     ^^
"
  ("x" exchange-point-and-mark "exchange point/mark")
  ("f" (lambda (arg)
	 (interactive "p")
	 (when (eq 1 arg)
	   (setq arg 2))
	 (org-table-end-of-field arg)))
  ("b" (lambda (arg)
	 (interactive "p")
	 (when (eq 1 arg)
	   (setq arg 2))
	 (org-table-beginning-of-field arg)))
  ("n" next-line)
  ("p" previous-line)
  ("q" nil "cancel" :color blue))

(bind-keys
 :filter (org-at-table-p)
 ("C-c m" . hydra-org-table-mark-field/body))
