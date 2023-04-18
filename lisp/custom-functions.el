;;; package --- custom-functions -*- lexical-binding: t; -*-
;;; Author:Justin Kaipada
;;; Commentary:
;;; Code:

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
  (defvar web-mode-markup-indent-offset 2)
  (defvar web-mode-code-indent-offset 2)
  (defvar web-mode-indent-style 4)
  (global-set-key (kbd "C-c C-c") 'web-mode-fold-or-unfold))

;; https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
(defun my-aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key (kbd "C-c t") 'my-aj-toggle-fold)

(defun my-org-archive-done-tasks ()
  "Move all done tasks in the current buffer to archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun insert-date ()
  "Insert the current date."
  (interactive)
  (let ((format "%B %e, %Y" )
	(system-time-locale "en_US"))
    (insert (format-time-string format))
    )
  )

(defun format-date-in-region (start end)
  "Format date from START to END 05/11/2020 -> November  5, 2020."
  (interactive "r")
  (let* ((time (s-split "/" (buffer-substring-no-properties start end)))
          (day (string-to-number (nth 0 time)))
          (month (string-to-number (nth 1 time)))
          (year (string-to-number (nth 2 time)))
	  (system-time-locale "en_US")
	  )
    (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert (format-time-string "%B %e, %Y" (encode-time 0 0 0 day month year))) )
    )
  )

(defun my-window-split-h (prefix)
  "Splits window right with older window open, with PREFIX arg."
  (interactive "p")
  (split-window-right)
  (other-window 1 nil)
  (if (- prefix 1) (switch-to-next-buffer)))

(defun my-window-split-v (prefix)
  "Splits window below with the older window open, with PREFIX arg."
  (interactive "p")
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

(defun align-values (start end)
  "Aligns region based on lengths of the START value of each line to th END.
Example output:
foo        bar
foofoo     bar
foofoofoo  bar"
  (interactive "r")
  (align-regexp start end
                "\\S-+\\(\\s-+\\)"
                1 1 nil))

(defun kill-other-buffers ()
  "Kill all other buffers except scratch."
  (interactive)
  (let ((preserved-buffers '("*scratch*" "*dashboard*" "*Messages*" "tt.org")))
    (mapc (lambda (buffer)
            (unless (member (buffer-name buffer) preserved-buffers)
              (kill-buffer buffer)))
          (buffer-list))))

(defun my-abbrev-mode-defs ()
  "This function defins some basic abbrevations."
  (abbrev-mode 1)
  (define-abbrev-table 'global-abbrev-table '(
					      ("$alpha" "α")
					      ("$beta" "β")
					      ("$gamma" "γ")
					      ("$tick" "✓")
					      ("$cross" "✗")
					      ("$delta" "Δ")))
  )

(defun my-expenses-today ()
  "Get todays expenses using ledger-cli using this INTERACTIVE function."
  (interactive)
  (let* ((tomorrow (time-add (current-time) (* 24 3600))))
    (defvar todays-date-flag-start (format-time-string "-b %m/%d"))
    (defvar tomorrows-date-flag (format-time-string "-e %m/%d" tomorrow))
    (defvar ledger-todays-expenses-command (concat "ledger -f "
						   (file-name-nondirectory buffer-file-name)
						   " bal ^Expenses " todays-date-flag-start
						   " " tomorrows-date-flag))
    (kill-new ledger-todays-expenses-command)
    (message ledger-todays-expenses-command)
    ) ;; let finishes
  )

(defun my-expenses-last-week ()
  "Get last-weeks expenses using ledger-cli using this INTERACTIVE function."
  (interactive)
  (let* ((last-week (time-subtract (current-time) (* 7 24 3600))))
    (defvar last-weeks-date-flag (format-time-string "-b %m/%d" last-week))
    (defvar todays-date-flag (format-time-string "-e %m/%d"))
    (defvar ledger-this-weeks-expenses-command (concat "ledger -f "
						       (file-name-nondirectory buffer-file-name)
						       " bal ^Expenses " last-weeks-date-flag
						       " " todays-date-flag))
    (message ledger-this-weeks-expenses-command)
    (kill-new ledger-this-weeks-expenses-command)
    ) ;; let finishes
  )

(defun point-in-comment ()
  "This function is a helper funtion for the fucntion down below."
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun my-capitalize-all-mysql-keywords ()
  "This is an INTERACTIVE funtion that can be called to capitalize all your my sql keywords."
  (interactive)
  (require 'sql)
  (save-excursion
    (dolist (keywords sql-mode-mysql-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (unless (point-in-comment)
          (goto-char (match-beginning 0))
          (upcase-word 1))))))

(defun my-evaluate-time-table-automatically()
  "Evaluate the org-clock summary automatically if in tt.org file.  This is called in tt.org as file-local-variable."
  (save-excursion
    (goto-char (point-min))
    (forward-line 9) ;; Line 10 is where the table is, starts from line 1 and move 9 times = line 10
    (org-clock-report)))

;; Table Field Marking
(defun org-table-mark-field ()
  "Mark the current table field."
  (interactive)
  (require 'org-table)
  ;; Do not try to jump to the beginning of field if the point is already there
  (when (not (looking-back "|[[:blank:]]?"))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

(defun delete-carrage-returns ()
  "Remove all the ^M chars from files."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

(defun my-dired-copy-path-at-point ()
  "Copy the full link to the file at point."
    (interactive)
    (dired-copy-filename-as-kill 0))
; Small 'w' copies file name, so big 'W' copies the whole link
(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "W") 'my-dired-copy-path-at-point)
  )

(defun perl-add-debug ()
  "Add a debug statement to the perl file."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1) ; prev line
  (insert "$DB::single = 1;")
  (beginning-of-line)
  )

(defun git-clone ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|git\\|ssh\\|org\\)" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Repos/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists.  delete? " (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter))
  )

;; Macro will convert env var file item from legacy build to perl env var defenition
(fset 'convert-make-env-to-perl-env
   (lambda (&optional arg) "Keyboard
   macro." (interactive "p") (kmacro-exec-ring-item (quote ([73
   escape 73 36 69 78 86 123 39 escape 108 108 108 134217848 122
   97 112 return 61 98 108 112 73 tab escape 65 59 escape 58
   return] 0 "%d")) arg)))


(fset 'wealthsimple-email-to-ledger
   (kmacro-lambda-form [?A escape ?k ?v ?B ?y ?k ?k ?k ?k ?k ?O
   ?I ?b ?e backspace backspace ?b ?e backspace backspace ?n ?v
   ?e ?s ?t ?m ?e ?n ?t ?: ?W ?e ?a ?l ?t ?h ?s ?i ?m ?p ?l ?e ?
   backspace ?: escape ?j ?b ?b ?l ?a backspace backspace escape
   ?I backspace escape ?o escape ?j ?j ?A escape ?v ?B ?y ?k ?k
   ?O ?I ?n ?v ?e ?s ?t ?m ?e ?n ?t ?  backspace ?: ?W ?a
   backspace ?e ?a ?l ?t ?h ?s ?i ?m ?p ?l ?e ?  backspace ?:
   escape ?p ?j ?j ?j ?j ?j ?j ?v ?B ?y ?k ?k ?k ?k ?k ?k ?A
   ?\C-u ?  ?$ escape ?p ?B ?x ?j ?j ?v ?B ?B escape ?I escape ?v
   ?e ?e ?d ?x ?j ?v ?e ?e ?d ?x ?i backspace ?  escape ?j ?I
   escape ?v ?e ?e ?l ?d ?i backspace ?  escape ?j ?I escape ?v
   ?e ?e ?e ?l ?d ?i backspace ?  ?@ ?  escape ?j ?V ?j ?d ?k ?k
   ?d ?d ?d ?d ?k ?k ?P ?i backspace escape ?j ?\C-c ?u] 0 "%d"))


(provide 'custom-functions)
;;; custom-functions.el ends here
