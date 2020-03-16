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

(global-set-key
 (kbd "C-c s")
 (defhydra hydra-smartparens (:color blue :hint nil :exit t)
   "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap          [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
   ;; Moving
   ("a" sp-beginning-of-sexp)
   ("e" sp-end-of-sexp)
   ("f" sp-forward-sexp)
   ("b" sp-backward-sexp)
   ("n" sp-down-sexp)
   ("N" sp-backward-down-sexp)
   ("p" sp-up-sexp)
   ("P" sp-backward-up-sexp)

   ;; Slurping & barfing
   ("h" sp-backward-slurp-sexp)
   ("H" sp-backward-barf-sexp)
   ("l" sp-forward-slurp-sexp)
   ("L" sp-forward-barf-sexp)

   ;; Wrapping
   ("R" sp-rewrap-sexp)
   ("u" sp-unwrap-sexp)
   ("U" sp-backward-unwrap-sexp)
   ("(" sp-wrap-round)
   ("{" sp-wrap-curly)
   ("[" sp-wrap-square)

   ;; Sexp juggling
   ("S" sp-split-sexp)
   ("s" sp-splice-sexp)
   ("r" sp-raise-sexp)
   ("j" sp-join-sexp)
   ("t" sp-transpose-sexp)
   ("A" sp-absorb-sexp)
   ("E" sp-emit-sexp)
   ("o" sp-convolute-sexp)

   ;; Destructive editing
   ("c" sp-change-inner)
   ("C" sp-change-enclosing)
   ("k" sp-kill-sexp)
   ("K" sp-backward-kill-sexp)
   ("w" sp-copy-sexp)

   ("q" nil)
   ("g" nil)))

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

(require 'org-tempo) ; Required from org 9 onwards for old template expansion
;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
(setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
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
    (org-tempo-complete-tag)
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
    '("not" . "note")
    org-structure-template-alist))

;;; myhydras.el ends here
