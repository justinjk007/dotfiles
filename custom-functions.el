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

(provide 'custom-functions)
;;; custom-functions.el ends here
