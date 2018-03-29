;;; functions.el --- some functions                  -*- lexical-binding: t; -*-

;; Author: CryptoManiac <cryptomaniac.512@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file contains some my functions

;;; Code:

(defun cm-comment-or-uncomment-region-or-line ()
  "Comments or uncomments region of current line if there's no active region.
http://stackoverflow.com/a/9697222/4911008"
  (interactive)
  (let (bgn end)
    (if (region-active-p)
	(setq bgn (region-beginning) end (region-end))
      (setq bgn (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region bgn end)
    (forward-line)))

(defun cm-py-isort-buffer-or-region ()
  "Call py-isort for region or for buffer."
  (interactive)
  (if (region-active-p)
      (py-isort-region)
    (py-isort-buffer)))

(global-set-key (kbd "C--") 'cm-comment-or-uncomment-region-or-line)

(defun sort-words (reverse bgn end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" bgn end))

(global-set-key (kbd "M-p M-w") 'sort-words)

(provide 'functions)
;;; functions.el ends here
