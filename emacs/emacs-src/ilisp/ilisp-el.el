;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-el.el --

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
;;; ILISP extensions to emacs lisp
;;;



;;;%Utils
;;; This should be in emacs, but it isn't.
(defun lisp-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Option comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
	  (setq list (cdr list))))
    done))



;;;%%Misc
(defun lisp-memk (item list key)
  "Test to see if ITEM is in LIST using KEY on each item in LIST
before comparing it to ITEM."
  (lisp-mem item list (function (lambda (x y)
			(equal x (funcall key y))))))

;;; This should be in emacs, but it isn't.
(defun lisp-del (item list &optional test)
  "Delete ITEM from LIST using TEST comparison and return the result.
Default test is equal."
  (let ((test (or test (function equal)))
	(element list)
	(prev nil)
	(done nil))
    (while (and element (not done))
      (if (funcall test item (car element))
	  (progn
	    (setq done t)
	    (if prev
		(rplacd prev (cdr element))
		(setq list (cdr list))))
	  (setq prev element
		element (cdr element))))
    list))

;;;
(defun lisp-last (list)
  "Return the last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))
