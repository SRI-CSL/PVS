;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-low.el --

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
;;; ILISP low level interface functions Lisp <-> Emacs
;;;
;;;



;;;%Lisp mode extensions
;;;%%Sexps
(defun lisp-previous-sexp (&optional prefix)
  "Return the previous sexp.  If PREFIX is T, then prefix like ' or #'
are allowed."
  (save-excursion
    (condition-case ()
	(progn
	  (if (and (memq major-mode ilisp-modes)
		   (= (point)
		      (process-mark (get-buffer-process (current-buffer)))))
	      nil
	      (if (not
		   (or (eobp) (memq (char-after (point)) '(? ?\) ?\n ?\t))))
		  (forward-sexp))
	      (skip-chars-backward " \t\n")
	      (let ((point (point)))
		(backward-sexp)
		(skip-chars-backward "^ \t\n(\",")
		(if (not prefix) (skip-chars-forward "#'"))
		(buffer-substring (point) point))))
      (error nil))))

;;;
(defun lisp-def-name (&optional namep)
  "Return the name of a definition assuming that you are at the start
of the sexp.  If the form starts with DEF, the form start and the next
symbol will be returned.  Optional NAMEP will return only the name without the defining symbol."
  (let ((case-fold-search t))
    (if (looking-at
	 ;; (( \( (def*) (( \( (setf)) | \(?)) | \(?) (symbol)
	 ;; 12    3    3 45    6    65      42      1 7      7
	 ;;0011\(22 def*        22         32 43\(54 setf54         43   \(?32 11      00 60           60
	 "\\(\\((\\(def[^ \t\n]*\\)[ \t\n]+\\(\\((\\(setf\\)[ \t\n]+\\)\\|(?\\)\\)\\|(?\\)\\([^ \t\n)]*\\)")
	(let ((symbol (buffer-substring (match-beginning 7) (match-end 7))))
	  (if (match-end 6)
	      (concat (if (not namep) 
			  (concat 
			   (buffer-substring (match-beginning 3) (match-end 3))
			   " "))
		      "("
		      (buffer-substring (match-beginning 6) (match-end 6))
		      " " symbol ")")
	      (if (match-end 3)
		  (concat (if (not namep)
			      (concat 
			       (buffer-substring (match-beginning 3) 
						 (match-end 3))
			       " "))
			  symbol)
		  symbol))))))


;;;
(defun lisp-minus-prefix ()
  "Set current-prefix-arg to its absolute value if numeric and return
T if it is a negative."
  (if current-prefix-arg
      (if (symbolp current-prefix-arg)
	  (progn (setq current-prefix-arg nil) t)
	  (if (< (setq current-prefix-arg
		       (prefix-numeric-value current-prefix-arg))
		 0)
	      (progn 
		(setq current-prefix-arg (- current-prefix-arg)) t)))))



;;;%%Defuns
(defun lisp-defun-region-and-name ()
  "Return the region of the current defun and the name starting it."
  (save-excursion
    (let ((end (lisp-defun-end))
	  (begin (lisp-defun-begin)))
      (list begin end (lisp-def-name)))))
  
;;;
(defun lisp-region-name (start end)
  "Return a name for the region from START to END."
  (save-excursion
    (goto-char start)
    (if (re-search-forward "^[ \t]*[^;\n]" end t)
	(forward-char -1))
    (setq start (point))
    (goto-char end)
    (re-search-backward "^[ \t]*[^;\n]" start 'move)
    (end-of-line)
    (skip-chars-backward " \t")
    (setq end (min (point) end))
    (goto-char start)
    (let ((from
	   (if (= (char-after (point)) ?\()
	       (lisp-def-name)
	       (buffer-substring (point) 
				 (progn (forward-sexp) (point))))))
      (goto-char end)
      (if (= (char-after (1- (point))) ?\))
	  (progn
	    (backward-sexp)
	    (if (= (point) start)
		from
		(concat "from " from " to " (lisp-def-name))))
	  (concat "from " from " to " 
		  (buffer-substring (save-excursion
				      (backward-sexp)
				      (point)) 
				    (1- (point))))))))
