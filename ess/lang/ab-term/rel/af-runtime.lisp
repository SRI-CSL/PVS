;;; -*- Mode: Lisp; Package: af-runtime-lib -*-
;;;
;;; Sccs Id @(#)af-runtime.lisp	1.5 9/28/89");
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; AF-RUNTIME.LISP - Ergo Analysis Facility (Runtime Support)
;;;
;;; ABSTRACT
;;;
;;;     This module provides utilities used by the attribute evaluator
;;;     generated by AB:AB
;;; 
;;; AUTHOR
;;;
;;;	R Nord
;;;
;;; HISTORY
;;;
;;;     07-22-87	rln	Initial development release.
;;;

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (unless (find-package :af-runtime-lib)
;;     (make-package :af-runtime-lib
;; 		  :nicknames '(:abrt :afrt)
;; 		  :use '(:common-lisp))))
(in-package :af-runtime-lib) 

;; (export '(opcase argcase rt-delta-error rt-term-argn rt-term-args
;; 	  rt-symbol rt-ite rt-opt rt-function rt-ast
;; 	  #+termocc rt-termocc #+termocc rt-mk-termocc #+termocc rt-termocc-argn
;; 	  rt-trap-constraint-errors rt-constraint-check
;; 	  rt-first-child rt-last-child rt-nchild
;; 	  rt-defconfam rt-defsynfam rt-defattrfam
;; 	  rt-defconfam-deltafun rt-defsynfam-compfun rt-defattrfam-attrfun
;; 	  rt-delta-confam
;; 	  rt-get-synfam rt-get-synfam-argn
;; 	  rt-opt-present rt-get-synfam-list
;; 	  rt-get-synfam-opt rt-get-synfam-argn-opt
;; 	  rt-get-iterate rt-get-iterate-opt
;; 	  rt-get-attrfam
;; 	  rt-get-attrfam-at
;; 	  rt-mvb rt-vls
;; 	  ))


(defconstant undefined :undefined)

(defun idsortp (sort)
  (if (equal "ID" (symbol-name sort)) t nil))

(defun numsortp (sort)
  (if (equal "NUMBER" (symbol-name sort)) t nil))

(defun strsortp (sort)
  (if (equal "STRING" (symbol-name sort)) t nil))

(defmacro argcase (&body body)
  `(case ,@body))

(defmacro rt-delta-error (sort)
  `(error "Unexpected child in ~A" ,sort))


;;; TERM package interface

(defmacro rt-term-argn (term n)
  `(term:term-argn ,term ,n))

(defmacro rt-term-args (term)
  `(term:term-args ,term))

(defmacro rt-nchild (term)
  "returns the number of children of the term"
  `(length (term:term-args ,term)))

(defmacro rt-first-child (term)
  "returns the position of the first child of term"
  (declare (ignore term))
  `0)

(defmacro rt-last-child (term)
  "returns the position of the last child of term"
  `(1- (length (term:term-args ,term))))

;;; greasy hack: reach inside lists because of the problem with
;;; the attribute ADT.  This indirectly redefines term-argn also.
;;; Used only for conversion from old term package.
#|
(defun term:term-args (term)
  "Get a term's arguments (children)"
  (term:check-term term)
  (let ((args (term::term-struct-args term)))
    (if (and (= (length args) 1)
	     (listp (term:term-op (first args)))
	     (eq (first (term:term-op (first args))) 'list))
	(term::term-struct-args (first args))
	args)))
|#


;;; Expression code

(defmacro rt-symbol (symbol)
  `(quote ,symbol))

(defmacro rt-ite (pred then else)
  `(if ,pred ,then ,else))

(defmacro rt-opt (tag present null)
  `(if ,tag ,present ,null))

(defmacro rt-function (name &rest arglist)
  `(,name ,@arglist))

(defmacro rt-ast (tterm)
  tterm)

#+termocc
(defun rt-mk-termocc (tterm &optional (occ (occ:make-empty-occ)))
  (cons tterm occ))

#+termocc
(defmacro termocc-root (termocc)
  `(car ,termocc))

#+termocc
(defmacro termocc-occ (termocc)
  `(cdr ,termocc))

#+termocc
(defun rt-termocc-argn (termocc n)
  (cons (termocc-root termocc)
	(occ:occ-push n (termocc-occ termocc))))

#+termocc
(defmacro rt-termocc (sym)
  sym)

;;; Handle analyze constraint errors by printing error message 
;;; and aborting the body form.
;;; Value is true if an error occurred, else false.

(defmacro rt-trap-constraint-errors (&body body)
  (let ((status (gentemp)))
    `(let ((,status (catch 'constraint-error-tag
		      (multiple-value-list (progn ,@body)))))
       (if (stringp ,status)
	   (progn
	     (format t "~A~%" ,status)
	     :error)
	   (values-list ,status)))))

(defmacro rt-constraint-check (pred mess)
  `(unless ,pred
     (throw 'constraint-error-tag ,mess)))
;;(error (concatenate 'simple-string "Constraint error:~%   " ,mess))))


;;; NEATTR interface

(defmacro rt-defconfam (confam-name lang-name)
  `(newattr:defconfam ,confam-name ,lang-name))

(defmacro rt-defsynfam (synfam-name lang-name confam-name)
  `(newattr:defsynfam ,synfam-name ,lang-name ,confam-name))

(defmacro rt-defattrfam (attrfam-name lang-name synfam-name)
  `(newattr:defattrfam ,attrfam-name ,lang-name ,synfam-name))

(defmacro rt-defconfam-deltafun (confam-name sort term-vars con-vars &rest body)
  `(newattr:defconfam-deltafun ,confam-name ,sort ,term-vars ,con-vars ,@body))

(defmacro rt-defsynfam-compfun (synfam-name sort term-vars con-vars &rest body)
  `(newattr:defsynfam-compfun ,synfam-name ,sort ,term-vars ,con-vars ,@body))

(defmacro rt-defattrfam-attrfun (attrfam-name sort term-vars con-vars syn-vars
					&rest body)
  `(newattr:defattrfam-attrfun ,attrfam-name ,sort ,term-vars ,con-vars ,syn-vars
			       ,@body))

(defmacro rt-delta-confam (confam sort n term &rest context)
  `(newattr:delta-confam ,confam ,sort ,n ,term ,@context))

(defmacro rt-get-synfam-argn (synfam sort child-sort n term &rest context)
  (cond ((idsortp child-sort)
	 `(newattr:get-attrfam newattr:idsym #+ergo-term :id #-ergo-term sb-runtime::id (rt-term-argn ,term ,n)))
	((numsortp child-sort)
	 `(newattr:get-attrfam newattr:numval #+ergo-term :number #-ergo-term sb-runtime::number (rt-term-argn ,term ,n)))
	((strsortp child-sort)
	 `(newattr:get-attrfam newattr:strval #+ergo-term :string #-ergo-term sb-runtime::string (rt-term-argn ,term ,n)))
	(t
	 `(newattr:get-synfam-argn
	   ,synfam ,sort ,child-sort ,n ,term ,@context))))

(defmacro rt-get-synfam (synfam sort term &rest context)
  (cond ((idsortp sort)
	 `(newattr:get-attrfam newattr:idsym #+ergo-term :id #-ergo-term sb-runtime::id ,term))
	((numsortp sort)
	 `(newattr:get-attrfam newattr:numval #+ergo-term :number #-ergo-term  sb-runtime::number ,term))
	((strsortp sort)
	 `(newattr:get-attrfam newattr:strval #+ergo-term :string #-ergo-term sb-runtime::string ,term))
	(t
	 `(newattr:get-synfam ,synfam ,sort ,term ,@context))))

#+ergo-term
(defmacro rt-opt-present (term)
  `(or (not (symbolp (term:term-op ,term)))
       (not (equal (symbol-name (term:term-op ,term)) "NULL"))))

#-ergo-term
(defmacro rt-opt-present (term)
  `(if (oper:is-leaf-op (term:term-op ,term)) t
       (let ((opsig (sort:opsig-table-lookup (term:term-op ,term))))
	 (if opsig
	     (not (sort:is-null-ttype (first (sort:opsig-inputs opsig))))
	     (error "Unknown operator ~S" (term:term-op ,term))))))


(defmacro rt-get-synfam-list (synfam sort child-sort term &rest context)
  (let ((result (gensym)) (nchild (gensym)) (nthchild (gensym)))
    `(let ((,result nil)
	   (,nchild (rt-nchild ,term)))
       (unless (eq ,nchild 0)
	 (values-list
	  (apply #'mapcar #'list
		 (dotimes (,nthchild ,nchild (reverse ,result))
		   (push
		    (multiple-value-list
		     (rt-get-synfam-argn
		      ,synfam ,sort ,child-sort ,nthchild ,term ,@context))
		    ,result))))))))


(defmacro rt-get-synfam-opt (synfam child-sort term &rest context)
  `(if (rt-opt-present term)
       (values-list (cons t
		  (multiple-value-list
		   (rt-get-synfam ,synfam ,child-sort ,term ,@context))))
       (values nil)))

(defmacro rt-get-synfam-argn-opt (synfam sort child-sort pos term &rest context)
  `(if (rt-opt-present (rt-term-argn ,term ,pos))
       (values-list (cons t
		  (multiple-value-list
		   (rt-get-synfam-argn
		    ,synfam ,sort ,child-sort ,pos ,term ,@context))))
       (values nil)))
#|
(defmacro rt-get-iterate (attr initial next test)
  (let ((prev-value (gensym))
	(cur-value attr))
    `(do ((,prev-value undefined ,cur-value)
	  (,cur-value ,initial ,next))
	 ((,test ,cur-value ,prev-value) ,cur-value))))
|#
(defmacro rt-get-iterate (attr initial next test)
  (let ((prev-value (gensym))
	(cur-value attr))
    `(let ((,cur-value ,initial))
       (do ((,prev-value ,cur-value ,cur-value)
	    (,cur-value ,next ,next))
	 ((,test ,cur-value ,prev-value) ,cur-value)))))

(defmacro rt-get-iterate-opt (term attr initial next test)
  `(if (rt-opt-present ,term)
       (values-list
	(cons
	 t
	 (multiple-value-list
	  (rt-get-iterate ,attr ,initial ,next ,test))))
       (values nil)))

(defmacro rt-get-attrfam (attrfam sort term &rest context)
  `(newattr:get-attrfam ,attrfam ,sort ,term ,@context))

(defmacro rt-get-attrfam-at (attrfam-name term occ &rest context-values)
  (let* ((local-label (gensym))
	 (syn-name (newattr::attrfam-synfam attrfam-name))
	 (con-name (newattr::synfam-confam syn-name)))
    `(labels ((,local-label (path term &rest context-values)
       (let ((sort (language:op-sort (term:term-op term))))
	 (if (null path)
	     (multiple-value-call
		 (newattr::attrfam-attrfun ',attrfam-name sort)
	       term
	       (values-list context-values)
	       (multiple-value-call
		   (newattr::synfam-compfun ',syn-name sort)
		 term
		 (values-list context-values)))
	     (multiple-value-call
		 #',local-label (cdr path)
		 (term:term-argn term (car path))
		 (apply (newattr::confam-deltafun ',con-name sort)
			(car path) term context-values))))))
       (,local-label (occ:occ-path ,occ) ,term ,@context-values))))

(defmacro rt-mvb (vars values-form &rest body)
  `(newattr:mvb ,vars ,values-form ,@body))

(defmacro rt-vls (&rest args)
  `(newattr:vls ,@args))


;;; Conal's term-case to work with the new term oper package.

(defvar *term-var*)

(defun term-case-clause (clause)
  (multiple-value-bind (op arg-names)
      (get-op-and-arg-names (first clause))
    `(,op ,@(let ((rest (rest clause)))
	      (if (null arg-names)
		  rest
		  `((let ,(arg-binders arg-names 0)
		      ,@rest)))))))


(defmacro term-case (term &rest clauses)
  (let ((*term-var* (gentemp)))
    `(let ((,*term-var* ,term))
       (ecase (oper:ds-sim-op (term:term-op ,*term-var*))
	 ,@(mapcar #'term-case-clause clauses)))))


(defun get-op-and-arg-names (key)
  ;; Extract the operator and list of arg names.  Return two values
  (etypecase key
    (symbol (values key () ))
    (cons (values (first key) (rest key)))))

(defun arg-binders (arg-names i)
  ;; Build code for let-bindings.
  (if (null arg-names)
      nil
      (cons `(,(first arg-names) (term:term-argn ,*term-var* ,i))
	    (arg-binders (rest arg-names) (1+ i)))))


#+ergo-term
(defmacro opcase (&body body)
  `(term:term-case ,@body))

#-ergo-term
(defmacro opcase (&body body)
  `(term-case ,@body))

