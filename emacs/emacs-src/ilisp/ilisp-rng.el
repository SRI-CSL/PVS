;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-rng.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;;
;;; ILISP match ring.
;;;
(defun match-ring (ring regexp start)
  "Return the index in RING of REGEXP starting at START."
  (let ((n 0)
	(len (ring-length ring)))
    (while (and (< n len) 
		(not (string-match regexp (ring-ref ring n))))
      (setq n (1+ n)))
    (if (= n len)
	nil
	n)))

;;;
(defun lisp-match-ring (regexp string &optional no-insert)
  "Match REGEXP in the input-ring of the current buffer and set the
ring variables to look like comint-previous-similar-input if found.
If not found insert STRING, unless NO-INSERT."
  (let ((n (if regexp (match-ring (ilisp-get-input-ring) regexp 0))))
    (if n
	(let ((point (progn (comint-kill-input) (point))))
	  (insert (ring-ref (ilisp-get-input-ring) n))
	  (save-excursion
	    (goto-char (+ point (length string)))
	    (skip-chars-forward "^ \t\n\)")
	    (setq point (point)))
	  (push-mark point)
	  (set-ilisp-input-ring-index n)
	  (setq this-command 'comint-previous-similar-input
		comint-last-similar-string string)
	  t)
	(if (and string (not no-insert))
	    (progn (comint-kill-input) (insert string) t)
	    nil))))
