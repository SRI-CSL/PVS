;;; -*- Mode: Lisp; Package: TYPE-CHECK -*-
;;;
;;; Defines functions which define and allow the use of 
;;; PROCLAIM-FTYPE, DECLARE-FTYPE to declare types of Lisp function
;;; and have them checked selectively.
;;;
;;; The checking functions are
;;; (type-check [packages] [symbols])
;;;  where package defaults to all packages and could be a list or
;;;  symbol, and symbols could be a list of symbols or :accessible,
;;;  :external, :present.  The default is :accessible.
;;;  Arguments are not evaluated.
;;;
;;; (type-uncheck [packages])
;;;  undoes the type checking for the declared functions types in
;;;  list of packages.
;;;  Arguments are not evaluated.
;;;
;;; This facility needs to be updated to cooperate with tdefun. (conal)
;;;  
;;; Author: Frank Pfenning.  Last Modified Thu Nov  3 13:27:55 1988
;;;
;;; Sccs Id @(#)type-check.lisp	1.3 9/23/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(defpackage "TYPE-CHECK")
(in-package :TYPE-CHECK) (use-package '(:ergolisp))

;;; Programming support
(eexport '(declare-ftype proclaim-ftype))

;;; Runtime environment support.  (Should they be eexported?)
(eexport '(undeclare-ftype
	   type-check type-check-functions type-uncheck
	   *type-check-exceptions*))


;;;; Programming support

(defmacro proclaim-ftype (name argtypes &rest resulttypes)
  "Expands to nothing, but saves something on the property list of
NAME.  This can be used on the top level, or as the first thing in
a DEFUN."
  `(eval-when (compile load eval)
     (setf (get ',name 'type-info) (cons ',argtypes ',resulttypes))))

(defmacro declare-ftype (name argtypes &rest resulttypes)
  "Expands to nothing, but saves something on the property list of
NAME.  This can be used on the top level, or as the first thing in
a DEFUN.  The problem is that if loading compiled code, this won't
be executed.  Therefore, for exported functions, PROCLAIM-FTYPE is
preferable."
  (setf (get name 'type-info) (cons argtypes resulttypes))
  `nil)


;;;; Runtime environment support.


#|
(proclaim-ftype type-check
		((or (member t) symbol string list) (or keyword symbol list))
		list)
(proclaim-ftype type-check-functions "t" list)
(proclaim-ftype type-uncheck ((or (member t) symbol string list)) list)
(proclaim-ftype undeclare-ftype "t" list)
|#

(defun check-type-list (sym when values-list types-list)
  (do ((vs values-list (cdr vs))
       (ts types-list (cdr ts)))
      ((or (null vs) (null ts))
       (cond ((not (null vs))
	      (error "More values than types when ~Aing function ~S.
Values: ~S
Type Specifiers: ~S"
		     when sym values-list types-list))
	     ((not (null ts))
	      (error "More types than values when ~Aing function ~S.
Type Specifiers: ~S
Values: ~S"
		     when sym types-list values-list))
	     (t nil)))
    (when (not (typep (car vs) (car ts)))
      (cerror "Ignore type inconsistency."
	      "Incorrect type when ~Aing function ~S.
~S is not of type ~S." when sym (car vs) (car ts)))))

(defvar *type-check-undefined-functions* '()
  "List of symbols with type information but no function definition.")

(defmacro undeclare-ftype (&rest symbols)
  "Undeclares the type of the given functions.  If no arguments are given,
the type information of all undefined functions is removed."
  (let ((undeclare-syms
	 (if (null symbols)
	     (remove-if #'fboundp *type-check-undefined-functions*)
	     symbols)))
    `(let ((syms ',undeclare-syms))
       (dolist (sym syms)
	 (remprop sym 'type-info))
       ,(if (null symbols) `(setq *type-check-undefined-functions* nil) `nil)
       syms)))



(defvar *type-check-exceptions*
  (mapcar #'find-package '("LISP" "LUCID" "SYSTEM" "KEYWORD"))
  "List of packages which should never be type-checked except when
explicitly given as an argument to TYPE-CHECK.")

(defun make-trace-spec (sym package)
  (let ((type-info (get sym 'type-info)))
    (if (not type-info)
	nil
	(if (not (fboundp sym))
	    (progn
	      (warn "Symbol ~S has a type declaration, ~
but is not defined as a function." sym)
	      (pushnew sym *type-check-undefined-functions*)
	      nil)
	    (progn
	      (when (member (symbol-package sym) *type-check-exceptions*)
		(warn "Symbol ~S is accessible in ~S, but present in ~S,
an excepted package."
		      sym package (symbol-package sym)))
	      ;; The :entrycond and :exitcond are put there only for
	      ;; the side-effect: if it returns, it always returns NIL.
	      ;; I put the explicit GET in there so that a change of
	      ;; the type declaration has immediate effect on a type-checked
	      ;; function.
	      `(,sym :entrycond (check-type-list
				 ',sym "enter" *trace-arglist*
				 (car (get ',sym 'type-info)))
		     :exitcond (check-type-list
				',sym "exit" *trace-values*
				(cdr (get ',sym 'type-info)))))))))

(defvar *type-checked-packages* '()
  "Property list with key PACKAGE and values FUNCTIONS, which is the
list of functions currently being type checked in PACKAGE.")

(defun type-check-p (sym package present-only)
  (or (not present-only)
      (eq (symbol-package sym) package)))

(defun type-check-expand (package symbols)
  "Will type-trace all functions with type information in PACKAGES,
which may be a single package or a list.  Package names are also
legal."
  (when (not (typep package 'package))
    (error "~S is not a package." package))
  (let ((trace-specs '())
	trace-spec)
    (cond ((eq symbols :external)
	   (do-external-symbols (sym package)
				(when (type-check-p sym package nil)
				  (setq trace-spec (make-trace-spec sym package))
				  (when trace-spec (push trace-spec trace-specs)))))
	  ((or (eq symbols :accessible)
	       (eq symbols :present))
	   (do-symbols (sym package)
		       (when (type-check-p sym package (eq symbols :present))
			 (setq trace-spec (make-trace-spec sym package))
			 (when trace-spec (push trace-spec trace-specs)))))
	  (t (dolist (sym symbols)
	       (setq trace-spec (make-trace-spec sym package))
	       (when trace-spec (push trace-spec trace-specs)))))
    `(progn (mapcar #'(lambda (checked-fun)
			(pushnew checked-fun
				 (getf *type-checked-packages* ',package)))
		    ',(mapcar #'car trace-specs))
	    (trace ,@trace-specs))))

(defun coerce-package (package-name)
  (cond ((find-package package-name))
	(t (error "~S does not name a known package." package-name))))

(defmacro type-check (&optional (packages t)
				(symbols :accessible))
  "Type checks all functions in packages PACKAGES (all if T or omitted).
The second argument is either a symbol or a list of symbols, or one of the
keywords :accessible, :present, :external.
This does not work correctly is there is more than one package, and
the symbols are explicitly specified."
  (cond ((eq packages t)
	 (setq packages
	       (remove-if #'(lambda (p) (member p *type-check-exceptions*))
			  (list-all-packages))))
	((not (listp packages))
	 (setq packages (list (coerce-package packages))))
	((and (not (= (length packages) 1))
	      (not (member symbols '(:accessible :present :external))))
	 (error "Illegal call to TYPE-CHECK.  Either specify one package
as the first argument, or one of the keywords :accessible, :present, 
or :external as the second argument."))
	(t (setq packages (mapcar #'coerce-package symbols))))
  `(delete-duplicates
    (append ,@(mapcar #'(lambda (p) (type-check-expand p symbols))
		      packages))))

(defmacro type-check-functions (&rest symbols)
  "This typechecks the functions given.  It is dangerous, however, since
the symbol may not be present in the package you may think.
Instead, TYPE-CHECK is recommended."
  `(append ,@(mapcar #'(lambda (sym) (type-check-expand (symbol-package sym)
						       (list sym)))
		    symbols)))

(defun type-uncheck-expand (package)
  (let ((entries (getf *type-checked-packages* package :not-checked)))
    (cond ((not (typep package 'package))
	   `(warn "~S is not a package." ',package))
	  ((eq entries :not-checked)
	   `(warn "Package ~S currently not type checked." ',package))
	  ((null entries)
	   `(warn "No functions in package ~S are currently type checked."
		  ',package))
	  (t `(progn (untrace ,@(cdr (assoc package *type-checked-packages*)))
		     (remf *type-checked-packages* ',package))))))

(defmacro type-uncheck (&optional (packages t))
  "Removes all the type checking from the specified packages, or all if
omitted."
  (cond ((eq packages t)
	 (setq packages (do ((x *type-checked-packages* (cddr x))
			     (packages nil (cons (car x) packages)))
			    ((null x)
			     (nreverse packages)))))
	((symbolp packages)
	 (setq packages (list (coerce-package packages))))
	(t (setq packages (mapcar #'coerce-package packages))))
  `(progn ,@(mapcar #'type-uncheck-expand packages)
	  nil))
