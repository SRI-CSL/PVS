;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-prn.el --
;;; ILISP paren handling.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;%Unbalanced parentheses
(defun lisp-skip (end)
  "Skip past whitespace, comments, backslashed characters and strings.
The operation is done in the current buffer as long as we are before END.
This does move the point."
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
  "Return the number of top-level pairs of LEFT-DELIMITER and RIGHT-DELIMITER.
Counting is done only between BEGIN and END.  If they don't match, the point
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
  "Go to the point where LEFT-DELIMITER and RIGHT-DELIMITER become unbalanced.
Point will be on the offending delimiter within the region."
  (interactive "r")
  (lisp-count-pairs start end
		    (string-to-char left-delimiter)
		    (string-to-char right-delimiter))
  (if (not ilisp-complete) (progn (beep) (message "Delimiters balance"))))

;;;
(defun find-unbalanced-lisp (arg)
  "Go to the point where LEFT-DELIMITER and RIGHT-DELIMITER become unbalanced.
Point will be on the offending delimiter in the buffer.
If called with a prefix, use the current region."
  (interactive "P")
  (if arg
      (call-interactively 'find-unbalanced-region-lisp)
      (find-unbalanced-region-lisp (point-min) (point-max))))

;;; end of file -- ilisp-prn.el --
