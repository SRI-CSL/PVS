;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-prn.el --

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
;;;
;;; ILISP paren handling
;;;
;;;


;;;%Unbalanced parentheses
(defun lisp-skip (end)
  "Skip past whitespace, comments, backslashed characters and strings
in the current buffer as long as you are before END.  This does move
the point."
  (if (< (point) end)
      (let ((comment (and comment-start (string-to-char comment-start)))
	    (done nil)
	    char)
	(while (and (< (point) end)
		    (not done))
	  (skip-chars-forward "\n\t " end)
	  (setq char (char-after (point)))
	  (cond ((eq char ?\")
		 (forward-sexp))
		((eq char comment)
		 (forward-char)
		 (skip-chars-forward "^\n" end))
		((eq char ?\\)
		 (forward-char 2))
		(t (setq done t)))))))

;;;
(defun lisp-count-pairs (begin end left-delimiter right-delimiter)
  "Return the number of top-level pairs of LEFT-DELIMITER and
RIGHT-DELIMITER between BEGIN and END.  If they don't match, the point
will be placed on the offending entry."
  (let ((old-point (point))
	(sexp 0)
	left)
    (goto-char begin)
    (lisp-skip end)
    (while (< (point) end)
      (let ((char (char-after (point))))
	(cond ((or (eq char left-delimiter)
		   ;; For things other than lists
		   (eq (char-after (1- (point))) ?\n))
	       (setq sexp (1+ sexp))
	       (if (condition-case ()
		       (progn (forward-sexp) nil)
		     (error t))
		   (error "Extra %s" (char-to-string left-delimiter))))
	      ((eq char right-delimiter)
	       (error "Extra %s" (char-to-string right-delimiter)))
	      ((< (point) end) (forward-char))))
      (lisp-skip end))
    (goto-char old-point)
    sexp))

;;;
(defun find-unbalanced-region-lisp (start end)
  "Go to the point in region where LEFT-DELIMITER and RIGHT-DELIMITER
become unbalanced.  Point will be on the offending delimiter."
  (interactive "r")
  (lisp-count-pairs start end
		    (string-to-char left-delimiter)
		    (string-to-char right-delimiter))
  (if (not ilisp-complete) (progn (beep) (message "Delimiters balance"))))

;;;
(defun find-unbalanced-lisp (arg)
  "Go to the point in buffer where LEFT-DELIMITER and RIGHT-DELIMITER
become unbalanced.  Point will be on the offending delimiter.  If
called with a prefix, use the current region."
  (interactive "P")
  (if arg
      (call-interactively 'find-unbalanced-region-lisp)
      (find-unbalanced-region-lisp (point-min) (point-max))))
