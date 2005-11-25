;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-sym.el --
;;; ILISP Lisp symbol utils.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;;; Notes:
;;;
;;; 19990804 Marco Antoniotti
;;; This should become a DEFSTRUCT
;;;
;;; (defstruct (lisp-symbol (:constructor lisp-symbol))
;;;   package
;;;   (delimiter "::")
;;;   name)

;;;%%Symbol
(defun lisp-symbol (package delimiter name)
  "Create a LISP symbol."
  (list package (when package (or delimiter "::")) name))

(defun lisp-symbol-name (symbol)
  "Return the name of SYMBOL."
  (car (cdr (cdr symbol))))

(defun lisp-symbol-package (symbol)
  "Return the package of SYMBOL."
  (car symbol))

(defun lisp-symbol-delimiter (symbol)
  "Return the qualifier of SYMBOL."
  (car (cdr symbol)))

;;;
(defun lisp-symbol= (symbol1 symbol2)
  "Return T is SYMBOL1 is equal to SYMBOL2."
  (and (string= (lisp-symbol-name symbol1) (lisp-symbol-name symbol2))
       (string= (lisp-symbol-package symbol1) (lisp-symbol-package symbol2))
       (string= (lisp-symbol-delimiter symbol1)
		(lisp-symbol-delimiter symbol2))))

;;; end of file -- ilisp-sym.el --
