;;; package --- myhydras
;;; Author:Justin Kaipada
;;; Commentary:
;;; This is the file that contains all my hydras
;;; Code:

(global-set-key
 (kbd "C-c C-v")
 (defhydra hydra-toggle-stuff ()
   "toggle"
   ("f" auto-fill-mode "fill" :color blue)
   ("t" toggle-truncate-lines "truncate" :color blue)
   ("w" whitespace-mode "whitespace" :color blue)
   ("q" nil "cancel"))
 )

(defhydra hydra-org (:color green)
  "
^
^Org^               ^Links^             ^Outline^
^───^───────────────^─────^─────────────^───────^───────────
_q_ quit            _i_ insert          _<_ previous
^^                  _n_ next            _>_ next
^^                  _p_ previous        _a_ all
^^                  _s_ store           _v_ overview
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview))

(defhydra hydra-org-template (:color blue :hint nil)
  "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("n" (hot-expand "<not"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (hot-expand "<s" "emacs-lisp"))
  ("p" (hot-expand "<s" "perl"))
  ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
  ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str &optional mod header)
  "Expand org template.
  STR is a structure template string recognised by org like <s. MOD
  is a string with additional parameters to add the begin line of
  the structure element. HEADER string includes more parameters
  that are prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (when header (insert "#+HEADER: " header) (forward-line))
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(with-eval-after-load "org"
  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
	  (hydra-org-template/body)
	(self-insert-command 1)))))
(eval-after-load "org"
  '(cl-pushnew
    '("not" "#+BEGIN_NOTES\n?\n#+END_NOTES")
    org-structure-template-alist))

;;; myhydras.el ends here
