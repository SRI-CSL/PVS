;;; -*- Mode: Lisp; Package: ANALYSIS-FACILITY -*-
;;;
;;; Sccs Id @(#)code-emitters.lisp	1.4 9/28/89");
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; CODE-EMITTERS.LISP - Ergo Analysis Facility
;;;
;;; ABSTRACT
;;; 
;;; Generate code for attribute evaluator using AF-RUNTIME-LIB macros.
;;;
;;; AUTHOR
;;;
;;;	R Nord
;;;
;;; HISTORY
;;;
;;;     07-22-87	rln	Initial development release.

(in-package "ANALYSIS-FACILITY")
(use-package '("AF-RUNTIME-LIB"))
(use-package '(:ergolisp))

(defun get-confam-name (pass)
  "Given the PASS, construct the context family name."
  (symbol-catenate pass 'confam))

(defun get-synfam-name (pass)
  "Given the PASS, construct the syntext family name."
  (symbol-catenate pass 'synfam))

(defun get-attrfam-name (pass)
  "Given the PASS, construct the attribute family name."
  (symbol-catenate pass 'attrfam))

(defun mldec-fun (grammar pass root-sort final-sorts confam synfam attrfam extgrams)
  "Code for top level interface."
  (declare (ignore extgrams))
  (let ((analyzer (if pass
		      (symbol-catenate grammar pass 'analyze)
		      (symbol-catenate grammar 'analyze)))
	(analyzer-at (if pass
			 (symbol-catenate grammar pass 'analyze-at)
			 (symbol-catenate grammar 'analyze-at))))
    `((export '(,confam ,synfam ,attrfam ,analyzer ,analyzer-at))
      (rt-defconfam ,confam ,grammar)
      (rt-defsynfam ,synfam ,grammar ,confam)
      (rt-defattrfam ,attrfam ,grammar ,synfam)
      (defun ,analyzer (%term% &key (nt ',root-sort) context)
	(rt-trap-constraint-errors
	  (ecase nt
	    ,@(mapcar
	       #'(lambda (fsortpair)
		   (let ((fsort (car fsortpair))
			 (inh (cdr fsortpair)))
		     (if inh
			 `(,fsort
			   (multiple-value-bind ,inh (values-list context)
			     (rt-get-attrfam ,attrfam ,fsort %term% #+termocc (rt-mk-termocc %term%) ,@inh)))
			 `(,fsort (rt-get-attrfam ,attrfam ,fsort %term% #+termocc (rt-mk-termocc %term%))))))
	       final-sorts))))
      (defun ,analyzer-at (%term% %occ% &key (nt ',root-sort) context)
	(rt-trap-constraint-errors
	  (ecase nt
	    ,@(mapcar
	       #'(lambda (fsortpair)
		   (let ((fsort (car fsortpair))
			 (inh (cdr fsortpair)))
		     (if inh
			 `(,fsort
			   (multiple-value-bind ,inh (values-list context)
			     (rt-get-attrfam-at ,attrfam %term% %occ% #+termocc (rt-mk-termocc %term%) ,@inh)))
			 `(,fsort (rt-get-attrfam-at ,attrfam %term% %occ% #+termocc (rt-mk-termocc %term%))))))
	       final-sorts)))))))

(defmacro mk-cu (code used)
  `(cons ,code ,used))
(defmacro  get-code (item)
  `(mapcar #'(lambda (x) (car x)) ,item))
(defmacro get-used (item)
  `(mapcan #'(lambda (x) (cdr x)) ,item))

;;; ergo-ignore-if-unused is declared on the entire context because the ABOX
;;; passes on the context, but has no way of knowing if it actually gets used.
;;; This will suppress the warning messages that these variables are declared but not used.

;;; Bug - the used variables in altcode are abtained by checking if attrs are passed down (used) but not
;;; if they are used in expressions giving a warning
;;; Warning: variable SORT is used yet it was declared ignored
;;; For now, the check is commented out.

(defun rulecon-fun (confam name context altcode)
  "Code for context family delta function keying on operator."
  (let* ((code (get-code altcode))
	 (used (get-used altcode))
	 (ignore (set-difference context used)))
    `(rt-defconfam-deltafun ,confam ,name (%n% %term%) 
			    #+termocc ,(cons '%termocc% context)
			    #-termocc ,context
			    ;;(declare (ignore ,@ignore))
			    (ergo-ignore-if-unused ,@context)
			    (opcase %term% ,@code))))

(defun rulesyn-fun (synfam name context altcode)
  "Code for syntext family computation function keying on operator."
  (let* ((code (get-code altcode))
	 (used (get-used altcode))
	 (ignore (set-difference context used)))
    `(rt-defsynfam-compfun ,synfam ,name (%term%)  
			   #+termocc ,(cons '%termocc% context)
			   #-termocc ,context
			   ;;(declare (ignore ,@ignore))
			   (ergo-ignore-if-unused ,@context)
			   (opcase %term% ,@code))))

(defun rulecontlist-fun (confam synfam sort attrs subsort)
  "Code for context family delta function for a term that is a list."
  (let ((*dp-used* nil)
	(context (get-inh-names attrs)))
    (let ((code (altcontlist-fun synfam sort attrs
		  (changeinit nil context) attrs subsort))
	  (ignore (set-difference context *dp-used*)))
      `(rt-defconfam-deltafun ,confam ,sort (%n% %term%)  
			      #+termocc ,(cons '%termocc% context)
			      #-termocc ,context
			      ;;(declare (ignore ,@ignore))
			      (ergo-ignore-if-unused ,@context)
			      ,code))))


(defun rulesyntlist-fun (confam synfam sort attrs subsort)
  "Code for syntext family comp function for a term that is a list."
  (let ((*dp-used* nil)
	(context (get-inh-names attrs)))
    (let ((code (altsynreg-fun sort attrs
		  (changeinit (enterlist nil (get-syn-names attrs)
					 (make-visit (mk-family confam synfam)
						     subsort attrs 0 nil attrs) nil)
			      context)))
	  (ignore (set-difference context *dp-used*)))
      `(rt-defsynfam-compfun ,synfam ,sort (%term%) 
			     #+termocc ,(cons '%termocc% context)
			     #-termocc ,context
			     ;;(declare (ignore ,@ignore))
			     (ergo-ignore-if-unused ,@context)
			     ,code))))


(defun ruleatt-fun (attrfam name context syntext altcode)
  "Code for the attribute family attribute function."
  (if (remove nil altcode)
      (let* ((code (get-code altcode))
	     (used (get-used altcode))
	     (ignore (set-difference (union context syntext) used)))
	`(rt-defattrfam-attrfun ,attrfam ,name (%term%)
				#+termocc ,(cons '%termocc% context)
				#-termocc ,context
				,syntext
				;;(declare (ignore ,@ignore))
				(ergo-ignore-if-unused ,@context)
				(opcase %term%
					,@code)))))

(defun altcon-fun (synfam sort attr name attrdefs child-con tlist errcon)
  "Code for one alterntive of a context in the form (operator (...code...))."
  (in-error-context errcon
    (let ((*dp-used* nil))
      (let ((code
	     `( ,name 
		,(if tlist
		     (altcontlist-fun
		      synfam sort attr attrdefs (car child-con) tlist)
		     (altconreg-fun sort attr name attrdefs child-con)))))
	(mk-cu code *dp-used*)))))
	    
#+termocc
(defmacro add-termocc (names)
  `(cons '%termocc% ,names))

(defun altconreg-fun (sort attr name attrdefs child-con)
  "Code for the children of an operator for a context."
  (let ((context #+termocc (add-termocc (attr-names (split-attrs attr)))
		 #-termocc (attr-names (split-attrs attr))))
    `(argcase %n%
	      ,@(do* ((con nil (attr-names (split-attrs (car clist))))
		      (val nil `(rt-vls #+termocc (rt-termocc-argn %termocc% %n%) ,@con))
		      (body nil
			    (cons
			     `(,pos
			       ,(encode
				 (schedule attrdefs con)
				 val sort context))
			     body))
		      (pos 0 (1+ pos))
		      (clist child-con (cdr clist)))
		    ((null clist) (reverse body)))
	      (otherwise ,(delta-error name)))))

(defun altcontlist-fun (synfam sort attr attrdefs child-con child-sort)
  "Code for the children of an operator (that contains a list) for a context."
  (multiple-value-bind (con-attrs syn-attrs) (split-attrs child-con)
    (let* ((context #+termocc (add-termocc (attr-names (split-attrs attr)))
		    #-termocc (attr-names (split-attrs attr)))
	   (con-names (attr-names con-attrs))
	   (syn-names (attr-names syn-attrs))
	   (con-lr (attr-names (attr-bbf con-attrs)))
	   (con-rl (attr-names (attr-bbl con-attrs)))
	   (syn-lr (attr-names (attr-bbl syn-attrs)))
	   (syn-rl (attr-names (attr-bbf syn-attrs)))
	   (lr-margs (mapmates syn-names syn-lr child-con))
	   (rl-margs (mapmates syn-names syn-rl child-con))
	   (val `(rt-vls #+termocc (rt-termocc-argn %termocc% %n%) ,@con-names))
	   (val0 `(rt-vls ,@(cons-undef con-names)))
	   (lval
	    (if (or con-rl con-lr)
	      `(cond ((> %n% ,(last-child '%term%))
		      (error "Not that many children"))
		     ((= ,(nchild '%term%) 0)
		      ,val0)
		     ((= ,(nchild '%term%) 1)
		      ,val)
		     ((= %n% ,(first-child '%term%))
		      ,(if con-rl
			   `(rt-mvb ,rl-margs
				(rt-get-synfam-argn
				 ,synfam ,sort ,child-sort (1+ %n%) %term%
				 ,@context)
			      ,val)
			   val))
		     ((= %n% ,(last-child '%term%))
		      ,(if con-lr
			   `(rt-mvb ,lr-margs
				(rt-get-synfam-argn
				 ,synfam ,sort ,child-sort (1- %n%) %term%
				 ,@context)
			      ,val)
			   val))
		     (t
		      ,(let ((bbf-body
			      (if con-lr
				  `(rt-mvb ,lr-margs
				       (rt-get-synfam-argn
					,synfam ,sort ,child-sort (1- %n%)
					%term% ,@context)
				     ,val)
				  val)))
			 (if con-rl
			     `(rt-mvb ,rl-margs
				  (rt-get-synfam-argn
				   ,synfam ,sort ,child-sort (1+ %n%)
				   %term% ,@context)
				,bbf-body)
			     bbf-body))))
	      `(cond ((> %n% ,(last-child '%term%))
		      (error "Not that many children"))
		     (t ,val)))))
    `,(encode (schedule attrdefs con-names) lval sort context))))
      

(defun altsyn-fun (sort attr name attrdefs constraints errcon)
  "Code for computing the syntext in the form (operator (...code...))."
  (in-error-context errcon
    (let ((*dp-used* nil))
      (let ((code
	     `( ,name
		,(altsynreg-fun sort attr attrdefs constraints))))
	(mk-cu code *dp-used*)))))

(defun altsynreg-fun (sort attr attrdefs &optional (constraints nil))
  "Code for the children of an operator for a syntext."
  (multiple-value-bind (con syn) (split-attrs attr)
    (let* ((context #+termocc (add-termocc (attr-names con))
		    #-termocc (attr-names con))
	   (syntext (attr-names syn))
	   (val `(rt-vls ,@syntext)))
      (encode (schedule attrdefs syntext constraints) val sort context))))


(defun altatt-fun (sort attr name attrdefs decls errcon)
  "Code for computing the attributes."
  (in-error-context errcon
    (multiple-value-bind (con syn) (split-attrs attr)
      (let* ((context #+termocc (add-termocc (attr-names con))
		      #-termocc (attr-names con))
	     (syntext (attr-names syn))
	     (ad (changeinit attrdefs syntext))
	     (val `(rt-vls ,@decls)))
	(if decls
	    (let ((*dp-used* nil))
	      (let ((code
		     `( ,name ,(encode (schedule ad decls) val sort context))))
		(mk-cu code *dp-used*))))))))


(defun symbol-fun (symbol)
  "Lisp symbol."
  `(rt-symbol ,symbol))

(defun ite-fun (pred then else)
  "If then else."
  `(rt-ite ,pred ,then ,else))

(defun opt-fun (tag null op)
  "Optional reference."
  `(rt-opt ,tag ,op ,null))

(defun funct-fun (name arglist)
  "Function call."
  `(rt-function ,name ,@arglist))

(defun ast-fun ()
  "Reference to abstract syntax tree."
  `(rt-ast %term%))

#+termocc
(defun termocc-fun ()
  "Reference to term occurrence."
  `(rt-termocc %termocc%))

(defun mult-fun (e1 e2)
  `(* ,e1 ,e2))

(defun div-fun (e1 e2)
  `(/ ,e1 ,e2))

(defun plus-fun (e1 e2)
  `(+ ,e1 ,e2))

(defun minus-fun (e1 e2)
  `(- ,e1 ,e2))

(defun neg-fun (e)
  `(- ,e))

(defun delta-error (name)
  `(rt-delta-error ',name))

(defun constraint-check (pred mess args)
  `(rt-constraint-check ,pred ,mess ,@args))

(defun term-selector (form pos)
  `(rt-term-argn ,form ,pos))

#+termocc
(defun termocc-selector (form pos)
  `(rt-termocc-argn ,form ,pos))

(defun first-child (term)
  `(rt-first-child ,term))

(defun last-child (term)
  `(rt-last-child ,term))

(defun nchild (term)
  `(rt-nchild ,term))

(defun opt-present (opt)
  `(rt-opt-present ,opt))
