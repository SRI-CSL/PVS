;;; -*- Mode: Lisp;  Package: sort; Log: sort-changes.log  -*-
;;; Sccs Id @(#)sorts.lisp	1.26 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Sort and type ADT originally defined for the Syntax Box. 

;;; Scott Dietzen, Thu May 21 12:49:21 1987

#-gcl
(defpackage :sort #+sbcl (:use :common-lisp :ergolisp :oper))
(in-package :sort) #-sbcl (use-package :ergolisp)

#-sbcl (use-package '(:oper))




;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed, so a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy is necessary so that unnecessary recompilation does
;;; not insue every time this code is revised.  Particularly since so many
;;; clients depend on it. 



(export '(ttype ttypep 
	  opsig opsigp 
	  check-ttype-type
	  check-opsig-type
	  ;; mk-ttype ttype-kind ttype-arg
	  mk-opsig opsig-inputs opsig-output opsig-arity opsig-lang-name

	  mk-id-ttype  is-id-ttype  ds-id-ttype
	  mk-num-ttype is-num-ttype ds-num-ttype
	  mk-str-ttype is-str-ttype ds-str-ttype
	  mk-lit-ttype is-lit-ttype ds-lit-ttype
	  ;; Only the above leaf ttypes can appear directly in abstract syntax
	  ;; (.i.e. not inserted by lexical terminals.)

	  mk-op-ttype is-op-ttype ds-op-ttype
	  mk-sort-ttype is-sort-ttype ds-sort-ttype
	  mk-list-ttype is-list-ttype ds-list-ttype
	  mk-null-ttype is-null-ttype 
	  mk-elist-ttype is-elist-ttype 
	  mk-union-ttype is-union-ttype ds-union-ttype

	  ttype-equal opsig-equal ttype-overlap?
          ttype-to-sexp sexp-to-ttype 
	  opsig-to-sexp sexp-to-opsig

	  make-sort-table sort-table-insert sort-table-delete
	  add-ttype-to-sort 
	  sort-table-lookup sort-table-contents
	  
	  make-opsig-table opsig-table-insert opsig-table-delete
	  opsig-table-lookup opsig-table-contents

	  ))


;;; The two types defined here each have a macro character and sexp printed
;;; representation:   #t is used for ttypes (term types) and #@ is used for
;;; opsig's (operator signatures).


;;; The representation. 

(defstruct (ttype-struct (:predicate ttypep)
			 (:print-function print-ttype))
  kind
  arg)

#+lcl4.1
(defmethod make-load-form ((obj ttype-struct))
  (make-load-form-saving-slots obj))

#+(or allegro cmu sbcl)
(defmethod make-load-form ((obj ttype-struct) &optional environment)
  (make-load-form-saving-slots obj))
  
(deftype ttype ()
  "The type of term types."
  'ttype-struct)

(defun check-ttype-type (x)
  "Make sure X is a ttype"
  (check-type x ttype "a ttype"))




(defstruct (opsig-struct (:predicate opsigp)
			 (:print-function print-opsig))
  lang-name				; name of containing language
  inputs				; ttypes of arguments
  output)				; ttype of result


#+lcl4.1
(defmethod make-load-form ((obj opsig-struct))
  (make-load-form-saving-slots obj))

#+(or allegro cmu sbcl)
(defmethod make-load-form ((obj opsig-struct) &optional environment)
  (make-load-form-saving-slots obj))

(deftype opsig ()
  "The type of operator signatures."
  'opsig-struct)

(defun check-opsig-type (x)
  "Make sure X is a ttype"
  (check-type x opsig "a ttype"))





;;; Primitive ADT operators:


(defun mk-ttype (kind arg)
  "Build a ttype from a kind and an arg."
  (declare (type keyword kind))
  (make-ttype-struct :kind kind :arg arg))

(defun ttype-kind (ttype)
  "Get a ttype's kind."
  (declare (type ttype ttype))
  (ttype-struct-kind ttype))

(defun ttype-arg (ttype)
  "Get a ttype's argument."
  (declare (type ttype ttype))
  (ttype-struct-arg ttype))



(defun mk-opsig (lang-name inputs output)
  "Build an opsig from the input ttypes and output ttype."
  (declare (type string lang-name)
	   (type list inputs)  ; (type (list ttype) inputs)
	   (type ttype output))
  (make-opsig-struct :lang-name lang-name
		     :inputs inputs
		     :output output))

(defun opsig-lang-name (opsig)
  "Get an opsig's language name."
  (declare (type opsig opsig))
  (opsig-struct-lang-name opsig))

(defun opsig-inputs (opsig)
  "Get an opsig's argument (input) ttypes."
  (declare (type opsig opsig))
  (opsig-struct-inputs opsig))

(defun opsig-arity (opsig)
  "Get the arity of an operator from the signature."
  (length (opsig-inputs opsig)))

(defun opsig-output (opsig)
  "Get an opsig's result (output) ttype."
  (declare (type opsig opsig))
  (opsig-struct-output opsig))




;;; ADT routines 

;;; Leaf ttypes for term trees: 
;;;   The following must defined for all values directly inserted into abstract
;;;   syntax (not through lexical terminals).

(defun mk-id-ttype (symbol)
  "Make an identifier ttype for a given SYMBOL."
  (declare (type symbol symbol))
  (mk-ttype :id symbol))
(defun is-id-ttype (x)
  "Is the argument an identifier ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :id)))
(defun ds-id-ttype (x)
  "Return the symbol associated with an identifier ttype."
  (ttype-arg x))


(defun mk-num-ttype (number)
  "Make a number ttype for a given NUMBER."
  (declare (type number number))
  (mk-ttype :num number))
(defun is-num-ttype (x)
  "Is the argument a number ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :num)))
(defun ds-num-ttype (x)
  "Return the number associated with a number ttype."
  (ttype-arg x))


(defun mk-str-ttype (string)
  "Make a string ttype for a given STRING."
  (declare (type string string))
  (mk-ttype :str string))
(defun is-str-ttype (x)
  "Is the argument a string ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :str)))
(defun ds-str-ttype (x)
  "Return the string associated with a string ttype."
  (ttype-arg x))


(defun mk-lit-ttype (symbol)
  "Make a literal ttype for a given SYMBOL."
  (declare (type symbol symbol))
  (mk-ttype :lit symbol))
(defun is-lit-ttype (x)
  "Is the argument a literal ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :lit)))
(defun ds-lit-ttype (x)
  "Return the symbol associated with a literal ttype."
  (ttype-arg x))






;;; Null ttypes 
(defun mk-null-ttype ()
  "Make a null ttype."
  (mk-ttype :null nil))
(defun is-null-ttype (x)
  "Is the argument a null ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :null)))




;;; ADT routines 

;;; Operators ttypes (terms that have the given operator).

(defun mk-op-ttype (oper)
  "Makes a simple operator ttype."
  (declare (type oper oper))
  (mk-ttype :op oper))
(defun is-op-ttype (x)
  "Is the argument an opertor ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :op)))
(defun ds-op-ttype (ttype)
  "Return the operator associated with the operator ttype."
  (ttype-arg ttype))



;;; Sort ttypes (named ttypes).

(defun mk-sort-ttype (name)
  "Makes a sort ttype."
  (declare (type symbol name))
  (mk-ttype :sort name))
(defun is-sort-ttype (x)
  "Is the argument a sort ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :sort)))
(defun ds-sort-ttype (ttype)
  "Return the symbol associated with the sort ttype."
  (ttype-arg ttype))





;;; ADT routines (con).


;;; List ttypes 

(defun mk-list-ttype (ttype)
  "Make a list ttype from argument."
  (declare (type ttype ttype))
  (mk-ttype :list ttype))
(defun is-list-ttype (x)
  "Is the argument a list ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :list)))
(defun ds-list-ttype (ttype)
  "Return the ttype associated with the list ttype."
  (ttype-arg ttype))


;;; Empty list ttypes (arbitrary types)

(defun mk-elist-ttype ()
  "Make an empty list ttype."
  (mk-ttype :elist nil))
(defun is-elist-ttype (x)
  "Is the argument an empty list ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :elist)))


;;; Union ttypes

(defun mk-union-ttype (ttypes)
  "Given a list of ttypes, construct a new union ttype containing 
   the given argument ttypes."
  (declare (type list ttypes))		; (declare (type (list ttype) ttypes))
  (let ((sons (join-lists (flatten-unions ttypes))))
    (if (eql 1 (length sons))
	(car sons)
	(mk-ttype :union sons))))
(defun is-union-ttype (x)
  "Is the argument a union ttype?"
  (and (ttypep x)
       (eq (ttype-kind x) :union)))
(defun ds-union-ttype (ttype)
  "Returns a list of operators in the union."
  (ttype-arg ttype))


(defun flatten-unions (ttypes)
  (do ((runner ttypes (cdr runner))
       (result nil (if (is-union-ttype (car runner))
                       (union result (ds-union-ttype (car runner))
			      :test #'ttype-equal)
                       (union (list (car runner)) result
			      :test #'ttype-equal))))
      ((null runner)
       (reverse result))))

(defun join-lists (ttypes)
  (let ((first-empty-list nil)
	(first-list nil))
    (do ((runner ttypes (cdr runner)))
	((null runner)
	 (if (and first-empty-list
		  first-list)
	     (remove first-empty-list ttypes)
	     ttypes))
      (if (is-elist-ttype (car runner))
	  (if first-empty-list
	      (setq first-empty-list nil)
	      (setq first-empty-list (car runner))))
      (if (is-list-ttype (car runner))
	  (if first-list
	      (setq first-list nil)
	      (setq first-list (car runner)))))))
  


  


(defun ttype-equal (ttype1 ttype2)
  "Determine if two ttypes are equivalent."
  (and (eq (ttype-kind ttype1)
	   (ttype-kind ttype2))
       (case (ttype-kind ttype1)
	 ((:id :num :str :lit)
	  (equal (ttype-arg ttype1) (ttype-arg ttype2)))
	 ((:null :elist)
	  t)
	 (:op
	  (oper:oper-equal (ttype-arg ttype1) (ttype-arg ttype2)))
	 (:sort
	  (equal (ttype-arg ttype1) (ttype-arg ttype2)))
	 (:list
	  (ttype-equal (ttype-arg ttype1) (ttype-arg ttype2)))
	 (:union
	  (and (eval (cons 'and
			   (mapcar #'(lambda (ttype)
				       (if (member ttype (ttype-arg ttype2)
						   :test #'ttype-equal)
					   t))
				   (ttype-arg ttype1))))
	       (eval (cons 'and
			   (mapcar #'(lambda (ttype)
				       (if (member ttype (ttype-arg ttype1)
						   :test #'ttype-equal)
					   t))
				   (ttype-arg ttype2)))))))))


(defun opsig-equal (opsig1 opsig2)
  "Determine if twp opsigs are equivalent"
  (and (mapcar #'ttype-equal
	       (opsig-inputs opsig1)
	       (opsig-inputs opsig2))
       (ttype-equal (opsig-output opsig1)
		    (opsig-output opsig2))
       (equal (opsig-lang-name opsig1)
	      (opsig-lang-name opsig1))))




(defun ttype-to-sexp (ttype)
  "Convert an ttype into a sexp."
  (ecase (ttype-kind ttype)
    (:sort
     (list :sort (ds-sort-ttype ttype)))
    (:union
     (cons :union (mapcar #'ttype-to-sexp
			  (ds-union-ttype ttype))))
    (:op
     (list :op (ds-op-ttype ttype)))
    (:null
     (list :null))
    (:list
     (list :list (ttype-to-sexp (ds-list-ttype ttype))))
    ((:id :num :str :lit)
     (list (ttype-kind ttype)
	   (ttype-arg ttype)))))

     


(defun sexp-to-ttype (x)
  "Convert an sexp into a ttype."
  (ecase (car x)
    (:op
     (mk-op-ttype (cadr x)))
    (:sort
     (mk-sort-ttype (cadr x)))
    (:null
     (mk-null-ttype))
    (:list
     (mk-list-ttype (sexp-to-ttype (cadr x))))
    (:union
     (mk-union-ttype (mapcar #'sexp-to-ttype
			     (cdr x))))
    (:elist
     (mk-elist-ttype))
    (:id (mk-id-ttype (cadr x)))
    (:num (mk-num-ttype (cadr x)))
    (:str (mk-str-ttype (cadr x)))
    (:lit (mk-lit-ttype (cadr x)))))



(defun opsig-to-sexp (opsig)
  "Convert an opsig to an sexp."
  (list (opsig-lang-name opsig) ':=
	(opsig-inputs opsig) ':->
	(opsig-output opsig)))

(defun sexp-to-opsig (x)
  "Convert an sexp to an opsig."
  (assert (and (eq (nth 1 x) ':=)
	       (eq (nth 3 x) ':->))
	  ()
	  "Illegal operator signature, does not contain := or :->")
  (mk-opsig (nth 0 x)
	    (nth 2 x)
	    (nth 4 x)))   ;; skip the '-> symbol. 



(defun print-ttype (ttype stream depth)
  "Uses ttype-to-sexp to show a nice printed representation of TTYPE."  
  (declare (ignore depth))
  (format stream "#t~S" (ttype-to-sexp ttype)))

(defun read-sexp-to-ttype (stream subchar arg)
  (declare (ignore subchar arg))
  (sexp-to-ttype (read stream)))

(defun ergolisp::\#t ()
  (set-dispatch-macro-character #\# #\t #'read-sexp-to-ttype))

(eval-when (:load-toplevel :execute)
  (ergolisp::\#t))




(defun print-opsig (opsig stream depth)
  "Uses opsig-to-sexp to show a nice printed representation of OPSIG."  
  (declare (ignore depth))
  (format stream "#@~S" (opsig-to-sexp opsig)))

(defun read-sexp-to-opsig (stream subchar arg)
  (declare (ignore subchar arg))
  (sexp-to-opsig (read stream)))



(defun ergolisp::\#\@ ()
  (set-dispatch-macro-character #\# #\@ #'read-sexp-to-opsig))

(eval-when (:load-toplevel :execute)
  (ergolisp::\#\@))





;;; Tables for sorts.

(defvar *global-sort-table* nil
  "The global sort-table")

(defun make-sort-table (&optional (contents ()))
  "Build a sort-table.  Contents is a list of oper,opsig pairs to be inserted
   in the table."
  (let ((result (make-hash-table :test #'eq)))
    (mapc #'(lambda (entry)
	      (let ((key (car entry))
		    (value (cdr entry)))
		(declare (type ttype value))
		(assert (is-sort-ttype key) ()
			"Key value must be a sort: ~S" key)
		(sort-table-insert key value result)))
	  contents)
    result))



(defun sort-table-insert (sort-ttype ttype &optional
				     (sort-table *global-sort-table*)
				     (no-warnings nil))
  "Sort-ttype is defined to represent ttype. 
   If sort-table is not specified, inserts are performed on the
   global table only.  Otherwise inserts are done on both the
   local and global table.
   No warnings simply disables warning messages."
  (let ((sort-name (ds-sort-ttype sort-ttype)))
    (if (and (not no-warnings)
	     (gethash sort-name sort-table)
	     (not (ttype-equal (gethash sort-name sort-table)
			       ttype)))
	(warn "The sort ~S has already been given a different sort ~
              definition.~%" sort-name))
    (setf (gethash sort-name sort-table)
	  ttype)
    (if (not (eq sort-table *global-sort-table*))
	(sort-table-insert sort-ttype ttype
			   *global-sort-table* :no-warnings))))

(defun sort-table-delete (sort-ttype &optional
				     (sort-table *global-sort-table*)
				     (no-warnings nil))
  "The Sort-ttype definition is removed. 
   If sort-table is not specified, deletes are performed on the
   global table only.  Otherwise deletes are done on both the
   local and global table.			
   No warnings simply disables warning messages."
  (let ((sort-name (ds-sort-ttype sort-ttype)))
    (if (and (not no-warnings)
	     (not (gethash sort-name sort-table)))
	(warn "The sort ~S is not defined.~%" sort-name))
    (setf (gethash sort-name sort-table)
	  nil)
    (if (not (eq sort-table *global-sort-table*))
	(sort-table-delete sort-ttype 
			   *global-sort-table* :no-warnings))))



(defun sort-table-lookup (sort-ttype &optional
				     (sort-table *global-sort-table*))
  "Look up sort-ttype is the global or local table depending on
   whether or not the optional argument was included. "
  (gethash (ds-sort-ttype sort-ttype) sort-table))



(defun sort-table-contents (&optional (sort-table *global-sort-table*))
  "Build a list of the oper,opsig pairs in the table (use the
   global table if none is provided.)."
  (let ((result nil))
    (maphash #'(lambda (key value)
		 (setf result
		       (cons (cons (mk-sort-ttype key) value)
			     result)))
	     sort-table)
    result))



(defun add-ttype-to-sort (ttype sort-ttype &optional
				(sort-table *global-sort-table*))
  "Add the given to ttype to those included in sort-ttype in both
   the given table and local table, or only the global table if no
   local table is provided.  If sort-ttype is not already in the
   sort-table it is inserted."
  (let* ((current-ttype (sort-table-lookup sort-ttype sort-table))
	 (new-ttype (cond ((and current-ttype
				(is-union-ttype current-ttype))
			   (mk-union-ttype
			    (cons ttype (ds-union-ttype current-ttype))))
			  (current-ttype
			   (mk-union-ttype
			    (list ttype current-ttype)))
			  (t
			   ttype))))
    (sort-table-insert sort-ttype new-ttype sort-table :no-warnings)))




;;; Tables for operator signatures.

(defvar *global-opsig-table* nil
  "The global opsig-table (operator signatures).")


(defun make-opsig-table (&optional (contents ()))
  "Initialize an opsig-table.
   Contents is a list of oper,opsig pairs to be inserted in the table. "
  (let ((result (make-hash-table :test #'equal)))
    (mapc #'(lambda (entry)
	      (let ((oper (car entry))
		    (opsig (cdr entry)))
		(declare (type oper oper)
			 (type opsig opsig))
		(opsig-table-insert oper opsig result t)))
	  contents)
    result))


(defun oper-to-symbol (oper)
  (declare (type oper oper))
  (cond ((oper:is-sim-op oper)
	 (oper:ds-sim-op oper))
	((oper:is-leaf-op oper)
	 (oper:leaf-op-kind oper))))


(defun opsig-table-insert (oper opsig &optional
				(opsig-table *global-opsig-table*)
				(no-warnings nil))
  "Oper is given an operator signature.
   If opsig-table is not specified, inserts are performed on the
   global table only.  Otherwise inserts are done on both the
   local and global table.
   No warnings simply disables warning messages."
  (declare (type oper oper)
	   (type opsig opsig))
  (let ((op-name (oper-to-symbol oper)))
    (if (and (not no-warnings)
	     (gethash op-name opsig-table)
	     (not (opsig-equal opsig
			       (opsig-table-lookup oper opsig-table))))
	(warn "The operator ~S has already been given a different signature.~%"
	      op-name))
    (setf (gethash op-name opsig-table)
	  (cons oper opsig))
    (if (not (eq opsig-table *global-opsig-table*))
	(opsig-table-insert oper opsig *global-opsig-table* :no-warnings))
    opsig))


(defun opsig-table-delete (oper &optional
				(opsig-table *global-opsig-table*)
				(no-warnings nil))
  "The opsig associated with oper is removed. 
   If opsig-table is not specified, deletes are performed on the
   global table only.  Otherwise deletes are done on both the
   local and global table.			
   No warnings simply disables warning messages."

  (declare (type oper oper))
  (let ((op-name (oper-to-symbol oper)))
    (if (and (not no-warnings)
	     (not (gethash op-name opsig-table)))
	(warn "The operator ~S has no signature.~%" op-name))
    (setf (gethash op-name opsig-table)
	  nil)
    (if (not (eq opsig-table *global-opsig-table*))
	(opsig-table-delete oper *global-opsig-table* :no-warnings))))


(defun opsig-table-lookup (oper &optional (opsig-table *global-opsig-table*))
  "Look up oper is the global or local table depending on
   whether or not the optional argument was included."
  (let ((op-name (oper-to-symbol oper)))
    (cdr (gethash op-name opsig-table))))


(defun opsig-table-contents (&optional (opsig-table *global-opsig-table*))
  "Build a list of the oper,opsig pairs in the table (use the
   global table if none is provided.)."
  (let ((result nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (setf result
		       (cons value
			     result)))
	     opsig-table)
    result))





(eval-when (:load-toplevel)
  (setq *global-sort-table* (make-sort-table))
  (setq *global-opsig-table* (make-opsig-table)))

#+kcl
(progn
  (setq *global-sort-table* (make-sort-table))
  (setq *global-opsig-table* (make-opsig-table)))
  



;;; ttype overlap? 


(defun ttype-overlap? (ttype1 ttype2
			      &optional (sort-table *global-sort-table*))
  "Determine if two ttype have any terms in common.  Sorts are translated."
  (declare (type ttype ttype1 ttype2))
  (cond ((is-sort-ttype ttype1)
	 (let ((sort-def (sort-table-lookup ttype1 sort-table)))
	   (assert sort-def ()
		   "Sort for ~S is undefined." ttype1)
	   (if sort-def
	       (ttype-overlap? sort-def ttype2 sort-table))))
	((is-sort-ttype ttype2)
	 (let ((sort-def (sort-table-lookup ttype2 sort-table)))
	   (assert sort-def ()
		   "Sort for ~S is undefined." ttype2)
	   (if sort-def
	       (ttype-overlap? ttype1 sort-def sort-table))))
	((is-union-ttype ttype1)
	 (do ((ttypes1 (ds-union-ttype ttype1) (cdr ttypes1)))
	     ((or (null ttypes1)
		  (ttype-overlap? (car ttypes1) ttype2 sort-table))
	      (if ttypes1
		  't))))
	((is-union-ttype ttype2)
	 (do ((ttypes2 (ds-union-ttype ttype2) (cdr ttypes2)))
	     ((or (null ttypes2)
		  (ttype-overlap? (car ttypes2) ttype1 sort-table))
	      (if ttypes2
		  't))))
	((is-list-ttype ttype1)
	 (if (and (is-list-ttype ttype2)
		  (ttype-overlap? (ds-list-ttype ttype1)
				  (ds-list-ttype ttype2)
				  sort-table))
	     't))
	((is-op-ttype ttype1)
	 (if (equal ttype1 ttype2)
	     't))
	((or (is-id-ttype ttype1)
	     (is-num-ttype ttype1)
	     (is-str-ttype ttype1)
	     (is-lit-ttype ttype1))
	 (if (ttype-equal ttype1 ttype2)
	     't))))
