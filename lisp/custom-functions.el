;;; package --- custom-functions
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

(defun my-org-archive-done-tasks ()
  "Move all done tasks in the current buffer to archive file."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun insert-date (prefix)
  "Insert the current date with as PREFIX."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d/%m/%Y")
                 ((equal prefix '(4)) "%Y/%m/%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

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
  (mapc 'kill-buffer (delq "*scratch*" (buffer-list))))

(defun my-abbrev-mode-defs ()
  "This function defins some basic abbrevations."
  (abbrev-mode 1)
  (define-abbrev-table 'global-abbrev-table '(
					      ("$alpha" "α")
					      ("$beta" "β")
					      ("$gamma" "γ")
					      ("$delta" "Δ")))
  )

(defun my-expenses-today ()
  "Get todays expenses using ledger-cli using this INTERACTIVE function."
  (interactive)
  (let* ((tomorrow (time-add (current-time) (* 24 3600))))
    (defvar todays-date-flag (format-time-string "-b %m/%d"))
    (defvar tomorrows-date-flag (format-time-string "-e %m/%d" tomorrow))
    (defvar ledger-todays-expenses-command (concat "ledger -f "
						   (file-name-nondirectory buffer-file-name)
						   " bal ^Expenses " todays-date-flag
						   " " tomorrows-date-flag))
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
  "This function evaluates the org-clock summary automatically if
  in tt.org file. This is called in tt.org as file-local-variable"
  (save-excursion
    (goto-char (point-min))
    (forward-line 9) ;; Line 10 is where the table is, starts from line 1 and move 9 times = line 10
    (org-clock-report)))

(provide 'custom-functions)
;;; custom-functions.el ends here
