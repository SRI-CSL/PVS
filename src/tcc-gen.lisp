;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcc-gen.lisp -- Generates the TCCs
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 00:32:38 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:16:57 1998
;; Update Count    : 45
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

(export '(generate-subtype-tcc generate-recursive-tcc
			       generate-existence-tcc
			       generate-assuming-tccs generate-actuals-tcc))

(defvar *tccdecls* nil)

(defvar *old-tcc-name* nil)

(defmacro total-tccs () '(car (tcc-info (current-theory))))
(defmacro tccs-proved () '(cadr (tcc-info (current-theory))))
(defmacro tccs-matched () '(caddr (tcc-info (current-theory))))
(defmacro tccs-simplified () '(cadddr (tcc-info (current-theory))))

(defstruct tccinfo
  formula
  reason
  kind
  expr
  type)

(defun generate-subtype-tcc (expr expected incs)
  (if (every #'(lambda (i) (member i *tcc-conditions* :test #'tc-eq))
	     incs)
      (add-tcc-comment 'subtype expr expected 'in-context)
      (let* ((*old-tcc-name* nil)
	     (ndecl (make-subtype-tcc-decl expr incs)))
	(if ndecl
	    (if (termination-tcc? ndecl)
		(insert-tcc-decl 'termination-subtype
				 expr
				 (recursive-signature (current-declaration))
				 ndecl)
		(insert-tcc-decl 'subtype expr expected ndecl))
	    (add-tcc-comment 'subtype expr expected)))))

(defvar *simplify-tccs* nil)

(defun make-subtype-tcc-decl (expr incs)
  (assert (every #'type incs))
  (let* ((*generate-tccs* 'none)
	 (conc (make!-conjunction* incs))
	 (*no-expected* nil)
	 (*bound-variables* *keep-unbound*)
	 (true-conc? (tcc-evaluates-to-true conc))
	 (tform (unless true-conc? (add-tcc-conditions conc)))
	 (uform (cond ((or true-conc? (tcc-evaluates-to-true tform))
		       *true*)
		      (*simplify-tccs*
		       (pseudo-normalize tform))
		      (t tform)))
	 (id (make-tcc-name expr)))
    (when (and (forall-expr? uform)
	       (duplicates? (bindings uform) :key #'id))
      (break "repeated bindings in make-subtype-tcc-decl"))
    (when (some #'(lambda (fv) (not (memq (declaration fv) *keep-unbound*)))
		(freevars uform))
      (break "make-subtype-tcc-decl freevars"))
    (assert (tc-eq (find-supertype (type uform)) *boolean*))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error expr "Subtype TCC for ~a simplifies to FALSE~@[:~2%  ~a~]"
		    expr (unless (tc-eq uform *false*) uform)))
      (typecheck* (cond ((and *recursive-subtype-term*
			      (occurs-in-eq *recursive-subtype-term* incs))
			 (mk-termination-tcc id uform))
			((equal (cdr (assq expr *compatible-pred-reason*))
				"judgement")
			 (mk-judgement-tcc id uform))
			((rec-application-judgement? (current-declaration))
			 ;; The below doesn't work - expr is not known a priori
			 ;;(equal (cdr (assq expr *compatible-pred-reason*))
			 ;;"recursive-judgement")
			 (mk-recursive-judgement-tcc id uform))
			(t (mk-subtype-tcc id uform)))
		  nil nil nil))))

;; (defun existing-tcc-subsumes (tcc)
;;   (some-tcc-subsumes tcc (visible-tccs)))

;; (defun some-tcc-subsumes (tcc tccs)
;;   (and tccs
;;        (or (tcc-subsumes tcc (car tccs))
;; 	   (some-tcc-subsumes tcc (cdr tccs)))))

;; (defun tcc-subsumes (tcc1 tcc2)
;;   (or (tcc-equal tcc1 tcc2)
;;       (tcc-subsumes* (tcc-clauses tcc1) (tcc-clauses tcc2) nil)))

;; (defun tcc-clauses (tcc)
;;   (if (eq 

;; (defun tcc-subsumes* (tcc1-clauses tcc2-clauses bindings
;; 				   last-tcc1-lit last-tcc2-clause)
;;   (or (null tcc2-clauses)		; Success
;;       (and tcc1-clauses
;; 	   (multiple-value-bind (mbindings tcc1-lit tcc2-clause)
;; 	       (find-subsume-match
;; 		(car tcc1-clauses) tcc2-clauses
;; 		bindings last-tcc1-lit last-ttc2-clause)
;; 	     (and (not (eq mbindings 'fail))
;; 		  (or (tcc-subsumes* (cdr tcc1-clauses)
;; 				     (remove-subsumed-clauses
;; 				      tcc1-clause tcc2-clauses
;; 				      mbindings)
;; 				     mbindings tcc2-clause)
;; 		      (tcc-subsumes* tcc1-clauses tcc2-clauses
;; 				     bindings tcc2-clauses)))))))

;; (defun find-subsume-match (clause clauses bindings
;; 				  &optional (last (car clauses)))
;;   (if (null clause)
;;       bindings ; success
;;       (multiple-value-bind (nbindings lit next)
;; 	  (next-matching-tcc-lit (car clause) last)
;; 	(if (eq nbindings 'fail)
;; 	    'fail
;; 	    (let ((rem-clauses
;; 		   (remove-if-subsumed (car clause) clauses nbindings)))
;; 	      (or (find-subsume-match (cdr clause) rem-clauses nbindings nil)
;; 		  (find-subsume-match clause clauses bindings next)))))))

;; (defun remove-if-subsumed (clause clauses bindings &optional kept)
;;   (if (null clauses)
;;       (nreverse kept)
;;       (remove-if-subsumed
;;        clause
;;        (cdr clauses)
;;        (if (clause-subsumes clause (car clauses) bindings)
;; 	   kept
;; 	   (cons clause kept)))))

;; (defun clause-subsumes (clause1 clause2 bindings)
;;   (subsetp clause1 clause2
;; 	   :test #'(lambda (x y) (tc-eq-with-bindings x y bindings))))

;; (defun next-matching-tcc-lit (lit clause bindings
;; 				  &optional (cdr-clause clause))
;;   (if (null cdr-clause)
;;       'fail
;;       (let ((nbindings (simple-match* lit (car cdr-clause) bindings nil)))
;; 	(if (eq nbindings 'fail)
;; 	    (next-matching-tcc-lit lit clause bindings (cdr cdr-clause))
;; 	    (values (car cdr-clause) nbindings (cdr cdr-clause))))))


;; (defun cnf (expr)
;;   (mapcar #'(lambda (x) (or+ (list x)))
;;     (and+ expr)))
       

(defvar *substitute-let-bindings* nil)

(defun add-tcc-conditions (expr)
  (multiple-value-bind (conditions vdecl1 ch1?)
      (subst-var-for-recs (remove-duplicates *tcc-conditions* :test #'equal))
    (multiple-value-bind (srec-expr vdecl ch?)
	(subst-var-for-recs expr vdecl1)
      (let* ((osubsts (get-tcc-binding-substitutions
		       (reverse (cons srec-expr conditions))))
	     (substs (if vdecl
			 (acons vdecl
				(lcopy vdecl
				  'type (substit (type vdecl) osubsts)
				  'declared-type (substit (declared-type vdecl)
						   osubsts))
				osubsts)
			 osubsts)))
	(universal-closure (add-tcc-conditions*
			    (raise-actuals srec-expr)
			    (if (and vdecl
				     (or ch1?
					 (and ch?
					      (member vdecl
						      (freevars srec-expr)
						      :test #'same-declaration))))
				(insert-var-into-conditions vdecl conditions)
				conditions)
			    substs nil))))))

(defun insert-var-into-conditions (vdecl conditions)
  (let* ((fvars (freevars vdecl))
	 (elt (find-if #'(lambda (bd)
			   (and (binding? bd)
				(member bd fvars :key #'declaration)))
		conditions)))
    (if elt
	(let ((post (memq elt conditions)))
	  (append (ldiff conditions post) (list vdecl) post))
	(append conditions (list vdecl)))))
    

(defun add-tcc-conditions* (expr conditions substs antes)
  (cond ((null conditions)
	 (let ((ex (substit (if antes
				(make!-implication
				 (make!-conjunction* antes) expr)
				expr)
		     substs)))
	   (assert (type ex))
	   ex))
	((consp (car conditions))
	 ;; bindings from a lambda-expr application (e.g., let-expr)
	 (add-tcc-conditions*
	  expr
	  (cdr conditions)
	  (if *substitute-let-bindings*
	      (cons (car conditions) substs)
	      substs)
	  (if *substitute-let-bindings*
	      antes
	      (let ((eqn (make!-equation
			  (mk-name-expr (caar conditions))
			  (cdar conditions))))
		(cons eqn antes)))))
	((typep (car conditions) 'bind-decl)
	 ;; Binding from a binding-expr
	 (add-tcc-bindings expr conditions substs antes))
	(t     ;; We collect antes so we can form (A & B) => C rather than
	 ;; A => (B => C)
	 (add-tcc-conditions* expr (cdr conditions)
			      substs
			      (cons (car conditions) antes)))))


;;; This creates a substitution from the bindings in *tcc-conditions*
;;; The substitution associates the given binding with one that has
;;;  1. unique ids
;;;  2. lifted actuals
;;;  3. types are made explicit
;;; Anytime a binding is modified for one of these reasons, it must be
;;; substituted for in the other substitutions.  Because of this the
;;; conditions are the reverse of *tcc-conditions*.

(defun get-tcc-binding-substitutions (conditions &optional substs prior)
  (cond ((null conditions)
	 (nreverse substs))
	((typep (car conditions) 'bind-decl)
	 (let ((nbd (car conditions)))
	   (when (untyped-bind-decl? nbd)
	     (let ((dtype (or (declared-type nbd)
			      (and (type nbd) (print-type (type nbd)))
			      (type nbd))))
	       (setq nbd (change-class (copy nbd 'declared-type dtype)
				       'bind-decl))))
	   (let* ((dtype (raise-actuals (declared-type nbd)))
		  (ptype (when (print-type (type nbd))
			   (if (tc-eq (print-type (type nbd))
				      (declared-type nbd))
			       dtype
			       (raise-actuals (print-type (type nbd)))))))
	     (unless (and (eq dtype (declared-type nbd))
			  (or (null ptype)
			      (eq ptype dtype)
			      (eq ptype (print-type (type nbd)))))
	       (if (eq nbd (car conditions))
		   (setq nbd (copy (car conditions)
			       'declared-type dtype
			       'type (copy (type nbd) 'print-type ptype)))
		   (setf (declared-type nbd) dtype
			 (type nbd) (copy (type nbd) 'print-type ptype)))))
	   (when (binding-id-is-bound (id nbd) prior)
	     (let ((nid (make-new-variable (id nbd) conditions 1)))
	       (if (eq nbd (car conditions))
		   (setq nbd (copy (car conditions) 'id nid))
		   (setf (id nbd) nid))))
	   (let ((*pseudo-normalizing* t))
	     (setq nbd (substit-binding nbd substs)))
	   (get-tcc-binding-substitutions
	    (cdr conditions)
	    (if (eq nbd (car conditions))
		substs
		(acons (car conditions) nbd substs))
	    (cons (car conditions) prior))))
	(t (get-tcc-binding-substitutions (cdr conditions) substs
					  (cons (car conditions) prior)))))

(defun add-tcc-bindings (expr conditions substs antes &optional bindings)
  (if (typep (car conditions) 'bind-decl)
      ;; Recurse till there are no more bindings, so we build
      ;;  FORALL x, y ... rather than FORALL x: FORALL y: ...
      (let* ((bd (car conditions))
	     (nbd (or (cdr (assq bd substs)) bd)))
;; 	(assert (or (member bd (freevars expr) :key #'declaration)
;; 		    (not (occurs-in bd expr))))
;; 	(assert (eq (and (member bd (freevars bindings) :key #'declaration) t)
;; 		    (and (occurs-in bd bindings) t)))
;; 	(assert (eq (and (member bd (freevars antes) :key #'declaration) t)
;; 		    (and (occurs-in bd antes) t)))
;; 	(assert (eq (and (assq bd substs) t)
;; 		    (and (occurs-in bd substs) t)))
	(add-tcc-bindings expr (cdr conditions) substs antes
			  (if (or (member bd (freevars expr)
					  :key #'declaration)
				  (member bd (freevars bindings)
					  :key #'declaration)
				  (member bd (freevars antes)
					  :key #'declaration)
				  (assq bd substs)
				  (possibly-empty-type? (type bd)))
			      (cons nbd bindings)
			      bindings)))
      ;; Now we can build the universal closure
      (let* ((nbody (substit (if antes
				 (make!-implication
				  (make!-conjunction* (reverse antes))
				  expr)
				 expr)
		      substs))
	     (nbindings (get-tcc-closure-bindings bindings nbody))
	     (nexpr (if nbindings
			(make!-forall-expr nbindings nbody)
			nbody)))
	(add-tcc-conditions* nexpr conditions substs nil))))


(defun binding-id-is-bound (id expr)
  (let ((found nil))
    (mapobject #'(lambda (ex)
		   (or found
		       ;; Skip things bound in type exprs; unlikely to
		       ;; cause confusion
		       (typep ex 'type-expr)
		       (and (typep ex 'binding)
			    (eq id (id ex))
			    (setq found ex))))
	       expr)
    found))


;;; Puts the dependent bindings after the non-dependent ones

(defun get-tcc-closure-bindings (bindings body)
  (when bindings
    (let ((fvars (union (freevars body)
			(reduce #'(lambda (x y) (union x y :test #'tc-eq))
				(mapcar #'freevars bindings))
			:test #'tc-eq)))
      (remove-if (complement
		  #'(lambda (bd)
		      (or (member bd fvars :key #'declaration)
			  (possibly-empty-type? (type bd)))))
	bindings))))


(defun insert-tcc-decl (kind expr type ndecl)
  (if (or *in-checker* *in-evaluator* *collecting-tccs*)
      (add-tcc-info kind expr type ndecl)
      (insert-tcc-decl1 kind expr type ndecl)))

(defun add-tcc-info (kind expr type ndecl)
  (pushnew (make-tccinfo
	    :formula (definition ndecl)
	    :reason (cdr (assq expr *compatible-pred-reason*))
	    :kind kind
	    :expr expr
	    :type type)
	   *tccforms*
	   :test #'tc-eq :key #'tccinfo-formula))

(defun insert-tcc-decl1 (kind expr type ndecl)
  (let ((*generate-tccs* 'none))
    (setf (tcc-disjuncts ndecl) (get-tcc-disjuncts ndecl))
    (let ((match (car (member ndecl *tccdecls* :test #'tcc-subsumed-by)))
	  (decl (declaration *current-context*)))
      (when (eq (spelling ndecl) 'OBLIGATION)
	(incf (total-tccs)))
      (cond
       ((and match
	     (not (and (declaration? decl)
		       (id decl)
		       (assq expr *compatible-pred-reason*))))
	#+pvsdebug
	(when (and (not (tc-eq (definition ndecl) (definition match)))
		   (not (tc-eq (simplify-expression
				(make!-implication (definition match)
						   (definition ndecl))
				:strategy '(lazy-grind))
			       *true*)))
	  ;; lazy-grind doesn't always work, but neither does anything else.
	  ;; To make certain that subsumption is correct, run
	  ;; simplify-expression with :interactive? t
	  (break "Something may be wrong with subsumes"))
	(add-tcc-comment kind expr type (list 'subsumed match) match t))
       (t (when match
	    (pvs-warning "The judgement TCC generated for and named ~a ~
                          is subsumed by ~a,~%  ~
                          but generated anyway since it was given a name"
	      (id decl) (id match)))
	  (insert-tcc-decl* kind expr type decl ndecl))))))

(defvar *simplify-disjunct-quantifiers* nil)
(defvar *simplify-disjunct-bindings* nil)

(defun get-tcc-disjuncts (tcc-decl)
  (let* ((*simplify-disjunct-bindings* nil)
	 (*simplify-disjunct-quantifiers* t)
	 (disjuncts (simplify-disjunct (definition tcc-decl) nil)))
    (cons *simplify-disjunct-bindings* disjuncts)))

(defmethod simplify-disjunct ((formula forall-expr) depth)
  (cond ((and *simplify-disjunct-quantifiers*
	      (or (not (integerp depth))
		  (not (zerop depth))))
	 (setq *simplify-disjunct-bindings*
	       (append *simplify-disjunct-bindings* (bindings formula)))
	 (simplify-disjunct (expression formula) (when depth (1- depth))))
	(t (call-next-method))))

(defmethod simplify-disjunct* ((formula forall-expr) depth)
  (cond (*simplify-disjunct-quantifiers*
	 (setq *simplify-disjunct-bindings*
	       (append *simplify-disjunct-bindings* (bindings formula)))
	 (simplify-disjunct (expression formula) (when depth (1- depth))))
	(t (list formula))))

(defun insert-tcc-decl* (kind expr type decl ndecl)
  (let ((submsg (case kind
		  (actuals nil)
		  (assuming (format nil "generated from assumption ~a.~a"
			      (id (module type)) (id type)))
		  (cases
		   (format nil
		       "for missing ELSE of CASES expression over datatype ~a"
		     (id type)))
		  (coverage nil)
		  (disjointness nil)
		  (existence nil)
		  ((subtype termination-subtype)
		   (format nil "expected type ~a"
		     (unpindent type 19 :string t :comment? t)))
		  (termination nil)
		  (well-founded (format nil "for ~a" (id decl))))))
    (when (and *typecheck-using*
	       (typep (declaration *current-context*) 'importing))
      (setf (importing-instance ndecl)
	    (list *typecheck-using* *set-type-formal*)))
    (push (definition ndecl) *tccs*)
    (push ndecl *tccdecls*)
    (when *typechecking-module*
      (let* ((str (unpindent (or *set-type-actuals-name* expr type)
			     4 :string t :comment? t))
	     (place (or (place *set-type-actuals-name*)
			(place* expr) (place type)))
	     (plstr (when place
		      (format nil "(at line ~d, column ~d) "
			(starting-row place) (starting-col place)))))
	(add-comment ndecl
	  "~@(~@[~a ~]~a~) TCC generated ~@[~a~]for~:[ ~;~%    %~]~a~
           ~@[~%    % ~a~]"
	  (or (cdr (assq expr *compatible-pred-reason*))
	      (when (rec-application-judgement? (current-declaration))
		"Recursive judgement"))
	  kind
	  plstr
	  (> (+ (length (cdr (assq expr *compatible-pred-reason*)))
		(length (string kind))
		(length plstr)
		(length str)
		25)
	     *default-char-width*)
	  str
	  submsg)
	(when *set-type-actuals-name*
	  (let ((pex (unpindent expr 4 :string t :comment? t))
		(astr (unpindent (raise-actuals *set-type-actuals-name* 1)
				 4 :string t :comment? t)))
	    (add-comment ndecl
	      "at parameter ~a in its theory instance~%    %~a"
	      pex astr)))
	(when (and (eq kind 'existence)
		   (formal-decl-occurrence type))
	  (add-comment ndecl
	    "May need to add an assuming clause to prove this.")))
      (when (eq (spelling ndecl) 'OBLIGATION)
	(setf (kind ndecl) 'tcc))
      (pushnew ndecl (refers-to decl) :test #'eq)
      (when *old-tcc-name*
	(push (cons (id ndecl) *old-tcc-name*) *old-tcc-names*))
;      (pvs-output "~2%~a~%"
;		  (let ((*no-comments* t)
;			(*unparse-expanded* t))
;		    (string-trim '(#\Space #\Tab #\Newline)
;				 (unparse ndecl :string t))))
      (add-decl ndecl))))

(defun tcc-submsg-string (kind expr type decl)
  (case kind
    (subtype (format nil "coercing ~a to ~a"
	       (unparse expr :string t) type))
    (termination (format nil "for ~a" (id decl)))
    (existence (format nil "for type ~a" type))
    (assuming (format nil "for assumption ~a on instance ~a"
		(id type) (unparse expr :string t)))
    (cases (format nil "for missing ELSE of datatype ~a"
	     (id type)))
    (well-founded (format nil "for ~a" (id decl)))
    (monotonicity (format nil "for ~a" (id decl)))))

(defun formal-decl-occurrence (expr)
  (let ((foundit nil))
    (mapobject #'(lambda (ex)
		   (or foundit
		       (when (and (name? ex)
				  (formal-decl? (declaration ex)))
			 (setq foundit t))))
	       expr)
    foundit))
		       
(defun tcc-subsumed-by (tcc2 tcc1)
  (and (or (not (assuming-tcc? tcc2)) (assuming-tcc? tcc1))
       (<= (length (tcc-disjuncts tcc1)) (length (tcc-disjuncts tcc2)))
       (if (same-binding-op tcc2 tcc1)
	   (multiple-value-bind (bindings leftovers)
	       (subsumes-bindings (car (tcc-disjuncts tcc1))
				  (car (tcc-disjuncts tcc2)))
	     (if (and leftovers
		      (or (and (typep (definition tcc2) 'forall-expr)
			       (some #'(lambda (b)
					 (not (assq b bindings)))
				     (car (tcc-disjuncts tcc1))))
			  (and (typep (definition tcc2) 'exists-expr)
			       (some #'(lambda (b)
					 (not (assq b bindings)))
				     (car (tcc-disjuncts tcc2))))))
		 (tc-eq tcc2 tcc1)
		 (subsetp (cdr (tcc-disjuncts tcc1)) (cdr (tcc-disjuncts tcc2))
			  :test #'(lambda (x y)
				    (tc-eq-with-bindings x y bindings)))))
	   (member (definition tcc1) (cdr (tcc-disjuncts tcc2))
		    :test #'(lambda (x y) (tc-eq x y))))))

(defun same-binding-op (tcc1 tcc2)
  (let ((f1 (definition tcc1))
	(f2 (definition tcc2)))
    (or (not (quant-expr? f1))
        (not (quant-expr? f2))
	(typecase f1
	  (forall-expr (typep f2 'forall-expr))
	  (t (typep f2 'exists-expr))))))

;(defmethod subsumes ((tcc1 binding-expr) (tcc2 binding-expr))
;  (let ((bindings (subsumes-bindings (bindings tcc1) (bindings tcc2))))
;    (subsetp (or+ (list (expression tcc1))) (or+ (list (expression tcc2)))
;	     :test #'(lambda (x y) (tc-eq* x y bindings)))))

(defun subsumes-bindings (bind1 bind2 &optional binds leftovers)
  (if (null bind1)
      (values (nreverse binds) (nconc leftovers bind2))
      (let* ((b1 (car bind1))
	     (b2 (or (find-if #'(lambda (b)
				  (and (same-id b1 b)
				       (tc-eq-with-bindings (type b1) (type b)
							    binds)))
			      bind2)
		     (find-if #'(lambda (b)
				  (tc-eq-with-bindings (type b1) (type b)
						       binds))
			      bind2))))
	(subsumes-bindings (cdr bind1) (remove b2 bind2)
			   (if b2 (cons (cons b1 b2) binds) binds)
			   (if b2 leftovers (cons b1 leftovers))))))

(defun generate-recursive-tcc (name arguments)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-recursive-tcc-decl name arguments)))
    (if ndecl
	(insert-tcc-decl 'termination
			 (make!-applications name arguments) nil ndecl)
	(add-tcc-comment 'termination
			 (make!-applications name arguments)
			 nil))))

(defun make-recursive-tcc-decl (name arguments)
    (when (null arguments)
      (type-error name
	"Recursive definition occurrence ~a must have arguments" name))
    (let* ((*generate-tccs* 'none)
	   (meas (measure (declaration *current-context*)))
	   (ordering (or (copy (ordering (declaration *current-context*)))
			 '<))
	   (appl1 (mk-recursive-application
		   meas
		   (outer-arguments (declaration *current-context*))))
	   (appl2 (mk-recursive-application
		   meas
		   arguments))
	   (relterm (beta-reduce
		     (typecheck* (mk-application ordering appl2 appl1)
				 *boolean* nil nil)))
	   (true-conc? (tcc-evaluates-to-true relterm))
	   (form (unless true-conc? (add-tcc-conditions relterm)))
	   (uform (cond ((or true-conc? (tcc-evaluates-to-true form))
			 *true*)
			((and *simplify-tccs*
			      (not (or *in-checker* *in-evaluator*)))
			 (pseudo-normalize form))
			(t (beta-reduce form))))
	   (id (make-tcc-name)))
      (unless (tc-eq uform *true*)
	(when (and *false-tcc-error-flag*
		   (tc-eq uform *false*))
	  (type-error name
	    "Termination TCC for this expression simplifies to false:~2%  ~a"
	    form))
	(typecheck* (mk-termination-tcc id uform) nil nil nil))))

(defun mk-recursive-application (op args)
  (if (null args)
      op
      (mk-recursive-application (mk-application* op (car args)) (cdr args))))

(defun make!-recursive-application (op args)
  (if (null args)
      op
      (make!-recursive-application (make!-application* op (car args))
				   (cdr args))))

(defun outer-arguments (decl)
  (outer-arguments* (append (formals decl)
			    (bindings* (definition decl)))
		    (measure-depth decl)
		    (type (measure decl))))

(defun outer-arguments* (bindings depth mtype &optional args)
  (let ((nargs (mapcar #'make-variable-expr
		 (or (typecheck* (car bindings) nil nil nil)
		     (make-new-bind-decls
		       (domain-types (find-supertype mtype)))))))
    (if (zerop depth)
	(nreverse (cons nargs args))
	(outer-arguments* (cdr bindings) (1- depth)
			  (range (find-supertype mtype))
			  (cons nargs args)))))

(defun generate-well-founded-tcc (decl mtype)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-well-founded-tcc-decl decl mtype)))
    (if ndecl
	(insert-tcc-decl 'well-founded (ordering decl) nil ndecl))))

(defun make-well-founded-tcc-decl (decl mtype)
  (unless (well-founded-type? (type (ordering decl)))
    (let* ((ordering (ordering decl))
	   (var1 (make-new-variable '|x| ordering))
	   (var2 (make-new-variable '|y| ordering))
	   (wfform (mk-lambda-expr (list (mk-bind-decl var1 mtype)
					 (mk-bind-decl var2 mtype))
		     (mk-application ordering
		       (mk-name-expr var1) (mk-name-expr var2))))
	   (form (typecheck* (mk-application '|well_founded?| wfform)
			     *boolean* nil nil))
	   (xform (cond ((tcc-evaluates-to-true form)
			 *true*)
			((and *simplify-tccs*
			      (not (or *in-checker* *in-evaluator*)))
			 (pseudo-normalize form))
			(t (beta-reduce form))))
	   (uform (expose-binding-types (universal-closure xform)))
	   (id (make-tcc-name)))
      (unless (tc-eq uform *true*)
	(when (and *false-tcc-error-flag*
		   (tc-eq uform *false*))
	  (type-error ordering
	    "TCC for this expression simplifies to false:~2%  ~a"
	    form))
	(typecheck* (mk-well-founded-tcc id uform) nil nil nil)))))

(defmethod well-founded-type? ((otype subtype))
  (or (and (name-expr? (predicate otype))
	   (eq (id (module-instance (predicate otype))) '|orders|)
	   (eq (id (declaration (predicate otype))) '|well_founded?|))
      (well-founded-type? (supertype otype))))

(defmethod well-founded-type? ((otype type-expr))
  nil)

(defun check-nonempty-type (te expr)
  (let ((type (if (dep-binding? te) (type te) te)))
    (unless (or (nonempty? type)
		(typep (declaration *current-context*) 'adt-accessor-decl)
		(member type (nonempty-types (current-theory)) :test #'tc-eq))
      (when (possibly-empty-type? type)
	(generate-existence-tcc type expr)
	(unless (or (or *in-checker* *in-evaluator*)
		    *tcc-conditions*)
	  (set-nonempty-type type))))))

(defmethod possibly-empty-type? :around ((te type-expr))
  (unless (nonempty? te)
    (call-next-method)))

(defmethod possibly-empty-type? ((te type-name))
  (let ((tdecl (declaration (resolution te))))
    (cond ((nonempty? te) nil)
	  ((typep tdecl 'formal-type-decl) t)
	  (t t))))

(defmethod possibly-empty-type? ((te subtype))
  (if (recognizer-name-expr? (predicate te))
      (let ((accs (accessors (constructor (predicate te)))))
	(some #'possibly-empty-type? (mapcar #'type accs)))
      t))

(defmethod possibly-empty-type? ((te datatype-subtype))
  (possibly-empty-type? (declared-type te)))

(defun uninterpreted-predicate? (expr)
  (and (name-expr? expr)
       (or (typep (declaration expr) 'field-decl)
	   (and (typep (declaration expr) '(or const-decl def-decl))
		(null (definition (declaration expr)))))
       ;;(recognizer? expr)
       ))

(defmethod possibly-empty-type? ((te tupletype))
  (possibly-empty-type? (types te)))

(defmethod possibly-empty-type? ((te cotupletype))
  (every #'possibly-empty-type? (types te)))

(defmethod possibly-empty-type? ((te funtype))
  (possibly-empty-type? (range te)))

(defmethod possibly-empty-type? ((te recordtype))
  (possibly-empty-type? (mapcar #'type (fields te))))

(defmethod possibly-empty-type? ((te dep-binding))
  (possibly-empty-type? (type te)))

(defmethod possibly-empty-type? ((list list))
  (when list
    (or (possibly-empty-type? (car list))
	(possibly-empty-type? (cdr list)))))

;;;;;;;;;;
;(defmethod check-nonempty-type ((te type-name) expr)
;  (let ((tdecl (declaration (resolution te))))
;    (when (and (formal-type-decl? tdecl)
;	       (not (nonempty? tdecl)))
;      (make-nonempty-assumption te dtypes)
;;      (format t "~%Added nonempty assertion for ~a to ~a"
;;	te (id (current-theory)))
;      (setf (nonempty? tdecl) t)))
;  nil)

(defun make-nonempty-assumption (type)
  (let* ((var (make-new-variable '|x| type))
	 (form (mk-exists-expr (list (mk-bind-decl var type)) *true*))
	 (tform (typecheck* form *boolean* nil nil))
	 (id (make-tcc-name))
	 (decl (typecheck* (mk-existence-tcc id tform) nil nil nil)))
    (setf (spelling decl) 'ASSUMPTION)
    (unless (member decl (assuming (theory *current-context*))
		    :test #'(lambda (d1 d2)
			      (and (formula-decl? d2)
				   (eq (spelling d2) 'ASSUMPTION)
				   (tc-eq (definition d1) (definition d2)))))
      (setf (assuming (theory *current-context*))
	    (append (assuming (theory *current-context*)) (list decl))))
    (add-decl decl nil)
    decl))
    

;(defmethod check-nonempty-type ((te subtype) expr &optional dtypes)
;  (unless (nonempty? te)
;    (if (uninterpreted-predicate? (predicate te))
;	(generate-existence-tcc te expr dtypes 'AXIOM)
;	(generate-existence-tcc te expr dtypes))
;    (setf (nonempty? te) t)))

;(defmethod check-nonempty-type ((te tupletype) expr &optional dtypes)
;  (check-nonempty-type (types te) expr dtypes))

;(defmethod check-nonempty-type ((te funtype) expr &optional dtypes)
;  ;;(or (empty? (domain te))
;      (check-nonempty-type
;       (range te) expr
;       (append dtypes (domain te))))
;  ;;)

;(defmethod check-nonempty-type ((te recordtype) expr &optional dtypes)
;  (check-nonempty-type (mapcar #'type (fields te)) expr dtypes))

;(defmethod check-nonempty-type ((te dep-binding) expr &optional dtypes)
;  (check-nonempty-type (type te) expr dtypes))

;(defmethod check-nonempty-type ((list list) expr &optional dtypes)
;  (when list
;    (check-nonempty-type (car list) expr dtypes)
;    (check-nonempty-type (cdr list) expr dtypes)))

(defmethod set-nonempty-type :around ((te type-expr))
  (unless (nonempty? te)
    (push te (nonempty-types (current-theory)))
    (setf (nonempty? te) t)
    (call-next-method)))

(defmethod set-nonempty-type ((te type-name))
  nil)

(defmethod set-nonempty-type ((te type-application))
  nil)

(defmethod set-nonempty-type ((te subtype))
  (set-nonempty-type (supertype te)))

(defmethod set-nonempty-type ((te tupletype))
  (set-nonempty-type (types te)))

(defmethod set-nonempty-type ((te cotupletype))
  ;; Can't propagate, as [S + T] nonempty implies S or T nonempty
  )

(defmethod set-nonempty-type ((te funtype))
  )

(defmethod set-nonempty-type ((te recordtype))
  (set-nonempty-type (mapcar #'type (fields te))))

(defmethod set-nonempty-type ((te dep-binding))
  (set-nonempty-type (type te)))

(defmethod set-nonempty-type ((list list))
  (when list
    (set-nonempty-type (car list))
    (set-nonempty-type (cdr list))))

(defun generate-existence-tcc (type expr &optional (fclass 'OBLIGATION))
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-existence-tcc-decl type fclass)))
    (insert-tcc-decl 'existence expr type ndecl)
    ndecl))

(defun make-existence-tcc-decl (type fclass)
  (let* ((*generate-tccs* 'none)
	 (stype (raise-actuals type nil))
	 (var (make-new-variable '|x| type))
	 (form (make!-exists-expr (list (mk-bind-decl var stype stype))
				  *true*))
	 (uform (add-tcc-conditions form))
	 (id (make-tcc-name)))
    (typecheck* (if (eq fclass 'OBLIGATION)
		    (mk-existence-tcc id uform)
		    (mk-formula-decl id uform fclass))
		nil nil nil)))

(defun make-domain-tcc-bindings (types type &optional bindings)
  (if (null types)
      (nreverse bindings)
      (let ((nbind (unless (and (typep (car types) 'dep-binding)
				(or (occurs-in (car types) type)
				    (occurs-in (car types) (cdr types))))
		     (let ((id (if (typep (car types) 'dep-binding)
				   (id (car types))
				   (make-new-variable '|x| (cons type types)))))
		       (typecheck* (mk-bind-decl id (car types) (car types))
				   nil nil nil)))))
	(make-domain-tcc-bindings (cdr types) type
				  (if nbind
				      (cons nbind bindings)
				      bindings)))))


;;; Assuming and mapped axiom TCC generation.  

(defun generate-assuming-tccs (modinst expr &optional theory)
  (let ((mod (or theory (get-theory modinst))))
    (unless (or (eq mod (current-theory))
		(null (assuming mod))
		(and (formals-sans-usings mod) (null (actuals modinst))))
      (let ((cdecl (or (and (or *in-checker* *in-evaluator*)
			    *top-proofstate*
			    (declaration *top-proofstate*))
		       (declaration *current-context*))))
	(unless (and (member modinst (assuming-instances (current-theory))
			     :test #'(lambda (x y)
				       (not (eq (simple-match y x) 'fail))))
		     (not (existence-tcc? cdecl)))
	  (let ((assumptions (remove-if-not #'assumption? (assuming mod))))
	    ;; Don't want to save this module instance unless it does not
	    ;; depend on any conditions, including implicit ones in the
	    ;; prover
	    (unless (or (or *in-checker* *in-evaluator*)
			(mappings modinst)
			(some #'(lambda (tcc-cond)
				  (not (typep tcc-cond '(or bind-decl list))))
			      *tcc-conditions*))
	      (push modinst (assuming-instances (current-theory))))
	    (dolist (ass assumptions)
	      (if (or (eq (kind ass) 'existence)
		      (nonempty-formula-type ass))
		  (let ((atype (subst-mod-params (existence-tcc-type ass)
						 modinst
						 mod)))
		    (if (typep cdecl 'existence-tcc)
			(let ((dtype (existence-tcc-type cdecl)))
			  (if (tc-eq atype dtype)
			      (generate-existence-tcc atype expr)
			      (check-nonempty-type atype expr)))
			(check-nonempty-type atype expr)))
		  (let* ((*old-tcc-name* nil)
			 (ndecl (make-assuming-tcc-decl ass modinst)))
		    (if ndecl
			(insert-tcc-decl 'assuming modinst ass ndecl)
			(add-tcc-comment 'assuming modinst ass)))))))))))

(defmethod existence-tcc-type ((decl existence-tcc))
  (existence-tcc-type (definition decl)))

(defmethod existence-tcc-type ((decl formula-decl))
  (existence-tcc-type (definition decl)))

(defmethod existence-tcc-type ((ex application))
  (existence-tcc-type (args2 ex)))

(defmethod existence-tcc-type ((ex quant-expr))
  (type (car (bindings ex))))

(defun make-assuming-tcc-decl (ass modinst)
  (let* ((*generate-tccs* 'none)
	 (expr (subst-mod-params (definition ass) modinst (module ass)))
	 (true-conc? (tcc-evaluates-to-true expr))
	 (tform (unless true-conc? (add-tcc-conditions expr)))
	 (uform (cond ((or true-conc? (tcc-evaluates-to-true tform))
		       *true*)
		      (*simplify-tccs*
		       (pseudo-normalize tform))
		      (t (beta-reduce tform))))
	 (id (make-tcc-name)))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error ass
	  "Assuming TCC for this expression simplifies to false:~2%  ~a"
	  tform))
      (typecheck* (mk-assuming-tcc id uform modinst ass) nil nil nil))))

(defun generate-mapped-axiom-tccs (modinst)
  (let ((mod (get-theory modinst)))
    (unless (and (not *collecting-tccs*)
		 (or (eq mod (current-theory))
		     (member modinst (assuming-instances (current-theory))
			     :test #'tc-eq)))
      ;; Don't want to save this module instance unless it does not
      ;; depend on any conditions, including implicit ones in the
      ;; prover
      (unless (or *in-checker* *in-evaluator* *collecting-tccs*
		  (some #'(lambda (tcc-cond)
				  (not (typep tcc-cond '(or bind-decl list))))
			      *tcc-conditions*))
	(push modinst (assuming-instances (current-theory))))
      (let* ((*old-tcc-name* nil))
	(dolist (axiom (collect-mapping-axioms modinst mod))
	  (multiple-value-bind (ndecl mappings-alist)
	      (make-mapped-axiom-tcc-decl axiom modinst mod)
	    (let ((netype (when ndecl (nonempty-formula-type ndecl))))
	      (if (and ndecl
		       (or (null netype)
			   (possibly-empty-type? netype)))
		  (let ((unint (find-uninterpreted
				(definition ndecl)
				modinst mod mappings-alist)))
		    (if unint
			(unless *collecting-tccs*
			  (pvs-warning
			      "Axiom ~a not translated because '~a' not interpreted"
			    (id axiom) (id unint)))
			(insert-tcc-decl 'mapped-axiom modinst axiom ndecl)))
		  (if ndecl
		      (add-tcc-comment
		       'mapped-axiom nil modinst
		       (cons 'map-to-nonempty 
			     (format nil
				 "%~a~%  % was not generated because ~a is non-empty"
			       (unpindent ndecl 2 :string t :comment? t)
			       (unpindent netype 2 :string t :comment? t))))
		      (add-tcc-comment
		       'mapped-axiom nil modinst))))))))))

(defmethod collect-mapping-axioms (thinst theory)
  (append (collect-mapping-axioms* (mappings thinst))
	  (remove-if-not #'axiom? (all-decls theory))))

(defmethod collect-mapping-axioms* ((list list))
  (mapcan #'collect-mapping-axioms* list))

(defmethod collect-mapping-axioms* ((map mapping))
  (collect-mapping-axioms* (lhs map)))

(defmethod collect-mapping-axioms* ((n name))
  (collect-mapping-axioms* (declaration n)))

(defmethod collect-mapping-axioms* ((theory module))
  (remove-if-not #'axiom? (all-decls theory)))

(defmethod collect-mapping-axioms* ((decl declaration))
  nil)

(defun find-uninterpreted (expr thinst theory mappings-alist)
  (let (;;(refs (collect-references expr))
	(theories (interpreted-theories thinst theory))
	(foundit nil))
    (mapobject #'(lambda (ex)
		   (or foundit
		       (when (and (name? ex)
				  (declaration? (declaration ex))
				  (interpretable? (declaration ex))
				  (not (assq (declaration ex) mappings-alist))
				  (or (memq (module (declaration ex)) theories)
				      ;;(mappings (module-instance ex))
				      ))
			 (setq foundit ex))))
	       expr)
    foundit))

(defun interpreted-theories (thinst theory)
  (cons theory (interpreted-theories* thinst)))

(defmethod interpreted-theories* ((name name))
  (interpreted-theories* (mappings name)))

(defmethod interpreted-theories* ((list list))
  (interpreted-theories-list list nil))

(defun interpreted-theories-list (list theories)
  (if (null list)
      theories
      (let ((ths (interpreted-theories* (car list))))
	(interpreted-theories-list
	 (cdr list)
	 (nunion ths theories)))))

(defmethod interpreted-theories* ((map mapping))
  (when (module? (declaration (lhs map)))
    (cons (declaration (lhs map))
	  (interpreted-theories* (rhs map)))))

(defmethod interpreted-theories* ((rhs mapping-rhs))
  (interpreted-theories* (expr rhs)))

(defmethod nonempty-formula-type ((decl formula-decl))
  (nonempty-formula-type (definition decl)))

(defmethod nonempty-formula-type ((ex exists-expr))
  (when (and (tc-eq (expression ex) *true*)
	     (singleton? (bindings ex)))
    (type (car (bindings ex)))))

(defmethod nonempty-formula-type ((ex expr))
  nil)

(defun make-mapped-axiom-tcc-decl (axiom modinst mod)
  (let* ((*generate-tccs* 'none)
	 (*generating-mapped-axiom-tcc* t))
    (unless (closed-definition axiom)
      (let* ((*in-checker* nil)
	     (*current-context* (context axiom)))
	(setf (closed-definition axiom)
	      (universal-closure (definition axiom)))))
    (multiple-value-bind (expr mappings-alist)
	(subst-mod-params (closed-definition axiom) modinst mod)
      (let* ((tform (add-tcc-conditions expr))
	     (xform (if *simplify-tccs*
			(pseudo-normalize tform)
			(beta-reduce tform)))
	     (uform (raise-actuals (expose-binding-types
				    (universal-closure xform))
				   t))
	     (id (make-tcc-name nil (id axiom))))
	(unless (tc-eq uform *true*)
	  (when (and *false-tcc-error-flag*
		     (tc-eq uform *false*))
	    (type-error axiom
	      "Mapped axiom TCC for this expression simplifies to false:~2%  ~a"
	      tform))
	  (values (typecheck* (mk-mapped-axiom-tcc id uform modinst axiom)
			      nil nil nil)
		  mappings-alist))))))

(defun generate-selections-tccs (expr constructors adt)
  (when (and constructors
	     (not (eq *generate-tccs* 'none)))
    (let ((unselected
	   (remove-if #'(lambda (c)
			  (member (id c) (selections expr)
				  :test #'(lambda (x y)
					    (eq x (id (constructor y))))))
	     constructors)))
      (when unselected
	(generate-selections-tcc unselected expr adt)))))

(defun generate-selections-tcc (constructors expr adt)
  (let* ((*generate-tccs* 'none)
	 (id (make-tcc-name))
	 (form (typecheck* (mk-application 'NOT
			     (mk-disjunction
			      (mapcar #'(lambda (c)
					  (mk-application (recognizer c)
					    (expression expr)))
				      constructors)))
			   *boolean* nil nil))
	 (tform (add-tcc-conditions form))
	 (uform (beta-reduce tform))
	 (*bound-variables* nil)
	 (*old-tcc-name* nil)
	 (ndecl (typecheck* (mk-cases-tcc id uform) nil nil nil)))
    ;;(assert (tc-eq (type uform) *boolean*))
    (insert-tcc-decl 'cases (expression expr) adt ndecl)))

(defun generate-coselections-tccs (expr)
  (unless (eq *generate-tccs* 'none)
    (let* ((all-ins (dotimes (i (length (types (type (expression expr)))))
		      (makesym "in_~d" (1+ i))))
	   (unselected
	    (remove-if-not
		#'(lambda (in)
		    (member in (selections expr)
			    :key #'(lambda (s) (id (constructor s)))))
	      all-ins)))
      (when unselected
	(break "generate-coselections-tccs")))))

(defun make-tcc-name (&optional expr extra-id)
  (assert *current-context*)
  (if (or *in-checker* *in-evaluator*)
      (gensym)
      (make-tcc-name* (current-declaration) expr extra-id)))

(defmethod make-tcc-name* ((decl declaration) expr extra-id)
  (declare (ignore expr))
  (let ((decl-id (op-to-id
		  (or (if (declaration? (generated-by (current-declaration)))
			  (generated-by (current-declaration))
			  (current-declaration))))))
    (make-tcc-name** decl-id
		     (remove-if-not #'declaration?
		       (all-decls (current-theory)))
		     extra-id
		     1)))

(defmethod make-tcc-name* ((decl subtype-judgement) expr extra-id)
  (declare (ignore extra-id))
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl number-judgement) expr extra-id)
  (declare (ignore extra-id))
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl name-judgement) expr extra-id)
  (declare (ignore extra-id))
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl application-judgement) expr extra-id)
  (declare (ignore extra-id))
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl rec-application-judgement) expr extra-id)
  (call-next-method))

(defmethod make-tcc-name* ((imp importing) expr extra-id)
  (declare (ignore expr))
  (let ((old-id (make-tcc-using-id))
	(new-id (make-tcc-importing-id)))
    (setq *old-tcc-name* (make-tcc-name** old-id nil nil 1))
    (make-tcc-name** new-id
		     (remove-if-not #'declaration?
		       (all-decls (current-theory)))
		     extra-id
		     1)))

(defun make-tcc-name** (id decls extra-id num)
  (let ((tcc-name (makesym "~a_~@[~a_~]TCC~d" id extra-id num)))
    (if (or (member tcc-name decls :key #'id)
	    (assq tcc-name *old-tcc-names*))
	(make-tcc-name** id decls extra-id (1+ num))
	tcc-name)))

(defmethod generating-judgement-tcc? ((decl number-judgement) expr)
  (declare (ignore expr))
  (id decl))

(defmethod generating-judgement-tcc? ((decl subtype-judgement) expr)
  (declare (ignore expr))
  (id decl))

(defmethod generating-judgement-tcc? ((decl name-judgement) expr)
  (and (id decl)
       (eq expr (name decl))))

(defmethod generating-judgement-tcc? ((decl name-judgement) (expr application))
  (and (id decl)
       (eq (operator* expr) (name decl))))

(defmethod generating-judgement-tcc? (decl expr)
  (declare (ignore decl expr))
  nil)


(defun make-tcc-using-id ()
  (let* ((theory (current-theory))
	 (imp (current-declaration))
	 (tdecls (all-decls theory))
	 (decls (memq imp tdecls))
	 (dimp (find-if
		   #'(lambda (d)
		       (and (typep d '(or importing
					  theory-abbreviation-decl))
			    (not (chain? d))))
		 decls))
	 (remimps (remove-if-not
		      #'(lambda (d)
			  (and (typep d '(or importing
					     theory-abbreviation-decl))
			       (not (chain? d))))
		    tdecls)))
    (makesym "IMPORTING~d" (1+ (position dimp remimps)))))

(defun make-tcc-importing-id ()
  (let ((imp (current-declaration)))
    (makesym "IMP_~a" (id (theory-name imp)))))

(defun subst-var-for-recs (expr &optional vdecl)
  (let ((recdecl (declaration *current-context*)))
    (if (and (def-decl? recdecl)
	     (recursive-signature recdecl))
	(let* ((vid (if vdecl (id vdecl) (make-new-variable '|v| expr)))
	       (vbd (or vdecl
			(make-bind-decl vid (recursive-signature recdecl))))
	       (vname (make-variable-expr vbd))
	       (sexpr (gensubst expr
			#'(lambda (x) (declare (ignore x)) (copy vname))
			#'(lambda (x) (and (name-expr? x)
					   (eq (declaration x) recdecl))))))
	  (values (if (and (not (eq sexpr expr))
			   (forall-expr? sexpr))
		      (copy sexpr
			'bindings (append (bindings sexpr)
					  (list (declaration vname))))
		      sexpr)
		  vbd
		  (not (eq sexpr expr))))
	expr)))

(defun make-new-variable (base expr &optional num)
  (let ((vid (if num (makesym "~a~d" base num) base)))
    (if (or (member vid *bound-variables* :key #'id)
	    (id-occurs-in vid expr))
	(make-new-variable base expr (if num (1+ num) 1))
	vid)))


;;; check-actuals-equality takes two names (with actuals) and generates
;;; the TCCs that give the equality of the two names.

(defun check-actuals-equality (name1 name2)
  (mapcar #'generate-actuals-tcc (actuals name1) (actuals name2)))

(defun generate-actuals-tcc (act mact)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-actuals-tcc-decl act mact)))
    (if ndecl
	(insert-tcc-decl 'actuals act nil ndecl)
	(add-tcc-comment 'actuals act nil))))

(defun make-actuals-tcc-decl (act mact)
  (let* ((*generate-tccs* 'none)
	 (conc (typecheck* (make-actuals-equality act mact)
			   *boolean* nil nil))
	 (true-conc? (tcc-evaluates-to-true conc))
	 (form (unless true-conc? (add-tcc-conditions conc)))
	 (uform (cond ((or true-conc? (tcc-evaluates-to-true form))
		       *true*)
		      (*simplify-tccs*
		       (pseudo-normalize (expose-binding-types
					  (universal-closure form))))
		      (t (beta-reduce (expose-binding-types
				       (universal-closure form))))))
	 (id (make-tcc-name)))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error act
	  "Actuals TCC for this expression simplifies to false:~2%  ~a"
	  form))
      (typecheck* (mk-same-name-tcc id uform) nil nil nil))))
  
(defun make-actuals-equality (act mact)
  (if (type-value act)
      (cond ((tc-eq (type-value act) (type-value mact))
	     *true*)
	    ((not (compatible? (type-value act) (type-value mact)))
	     (type-error act "Types are not compatible"))
	    (t (equality-predicates (type-value act) (type-value mact))))
      (mk-application '= (expr act) (expr mact))))

(defun equality-predicates (t1 t2 &optional precond)
  (equality-predicates* t1 t2 nil nil precond nil))

(defmethod equality-predicates* :around (t1 t2 p1 p2 precond bindings)
  (if (tc-eq-with-bindings t1 t2 bindings)
      (make-equality-between-predicates t1 p1 p2 precond)
      (call-next-method)))

(defun make-equality-between-predicates (t1 p1 p2 precond)
  (when (or p1 p2)
    (let* ((vid (make-new-variable '|x| (append p1 p2)))
	   (type (or t1
		     (reduce #'compatible-type (or p1 p2)
			     :key #'(lambda (p)
				      (domain (find-supertype (type p)))))))
	   (bd (make-bind-decl vid type))
	   (var (make-variable-expr bd))
	   (iff (make!-iff
		 (make!-conjunction*
		  (mapcar #'(lambda (p)
			      (make!-reduced-application p var))
		    p1))
		 (make!-conjunction*
		  (mapcar #'(lambda (p)
			      (make!-reduced-application p var))
		    p2))))
	   (ex (if precond
		   (make!-implication (make!-application precond var) iff)
		   iff)))
      (make!-forall-expr (list bd) ex))))

(defmethod equality-predicates* ((t1 dep-binding) t2 p1 p2 precond bindings)
  (equality-predicates* (type t1) t2 p1 p2 precond bindings))

(defmethod equality-predicates* (t1 (t2 dep-binding) p1 p2 precond bindings)
  (equality-predicates* t1 (type t2) p1 p2 precond bindings))

(defmethod equality-predicates* ((t1 type-name) (t2 type-name) p1 p2 precond
				 bindings)
  (assert (eq (declaration t1) (declaration t2)))
  (let ((npred (make-equality-between-predicates t1 p1 p2 precond)))
    (equality-predicates-list (actuals (module-instance t1))
			      (actuals (module-instance t2))
			      bindings npred)))

(defmethod equality-predicates* ((t1 subtype) (t2 type-expr) p1 p2 precond
				 bindings)
  (equality-predicates*
   (supertype t1) t2 (cons (predicate t1) p1) p2 precond bindings))

(defmethod equality-predicates* ((t1 type-expr) (t2 subtype) p1 p2 precond
				 bindings)
  (equality-predicates*
   t1 (supertype t2) p1 (cons (predicate t2) p2) precond bindings))

(defmethod equality-predicates* ((t1 subtype) (t2 subtype) p1 p2 precond
				 bindings)
  (cond ((subtype-of? t1 t2)
	 (equality-predicates* (supertype t1) t2
			       (cons (predicate t1) p1) p2 precond
			       bindings))
	((subtype-of? t2 t1)
	 (equality-predicates* t1 (supertype t2)
			       p1 (cons (predicate t2) p2) precond
			       bindings))
	(t
	 (equality-predicates* (supertype t1) (supertype t2)
			       (cons (predicate t1) p1)
			       (cons (predicate t2) p2)
			       precond
			       bindings))))

(defmethod equality-predicates* ((t1 funtype) (t2 funtype) p1 p2 precond
				 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2 precond)))
    (equality-predicates-list (list (domain t1) (range t1))
			      (list (domain t2) (range t2))
			      bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 tupletype) (t2 tupletype) p1 p2 precond
				 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2 precond)))
    (equality-predicates-list (types t1) (types t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 cotupletype) (t2 cotupletype) p1 p2
				 precond bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2 precond)))
    (equality-predicates-list (types t1) (types t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 recordtype) (t2 recordtype) p1 p2
				 precond bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2 precond)))
    (equality-predicates-list (fields t1) (fields t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((f1 field-decl) (f2 field-decl) p1 p2
				 precond binding)
  (assert (and (null p1) (null p2)))
  (equality-predicates* (type f1) (type f2) nil nil precond binding))

(defun equality-predicates-list (l1 l2 bindings preds)
  (if (null l1)
      (when preds
	(make!-conjunction* (nreverse preds)))
      (let* ((npred (equality-predicates* (car l1) (car l2) nil nil nil
					  bindings))
	     (newpreds (if npred (cons npred preds) preds))
	     (carl1 (car l1))
	     (carl2 (car l2))
	     (dep-bindings? (and (dep-binding? carl1)
				(dep-binding? carl2))))
	(cond (dep-bindings?
	       (let* ((bd (make-bind-decl (id carl1) carl1))
		      (bdvar (make-variable-expr bd))
		      (new-cdrl1 (subst-var-into-deptypes
				  bdvar carl1 (cdr l1)))
		      (new-cdrl2 (subst-var-into-deptypes
				  bdvar carl2 (cdr l2)))
		      (*bound-variables* (cons bd *bound-variables*))
		      (epred (equality-predicates-list
			      new-cdrl1 new-cdrl2 bindings newpreds)))
		 (when epred
		   (make!-forall-expr (list bd) epred))))
	      ((dep-binding? carl1)
	       (let* ((bd (make-unique-binding carl1 l2))
		      (bdvar (make-variable-expr bd))
		      (new-cdrl1 (subst-var-into-deptypes
				  bdvar carl1 (cdr l1)))
		      (*bound-variables* (cons bd *bound-variables*))
		      (epred (equality-predicates-list
			      new-cdrl1 (cdr l2) bindings newpreds)))
		 (when epred
		   (make!-forall-expr (list bd) epred))))
	      ((dep-binding? carl2)
	       (let* ((bd (make-unique-binding carl2 l1))
		      (bdvar (make-variable-expr bd))
		      (new-cdrl2 (subst-var-into-deptypes
				  bdvar carl2 (cdr l2)))
		      (*bound-variables* (cons bd *bound-variables*))
		      (epred (equality-predicates-list
			      (cdr l1) new-cdrl2 bindings newpreds)))
		 (when epred
		   (make!-forall-expr (list bd) epred))))
	      (t
	       (equality-predicates-list (cdr l1) (cdr l2) bindings newpreds))))))

(defmethod equality-predicates* ((a1 actual) (a2 actual) p1 p2 precond
				 bindings)
  (if (type-value a1)
      (equality-predicates* (type-value a1) (type-value a2) p1 p2 precond
			    bindings)
      (let ((npred (make-equality-between-predicates nil p1 p2 precond)))
	(if npred
	    (make!-conjunction npred (make-equation (expr a1) (expr a2)))
	    (make-equation (expr a1) (expr a2))))))


;; make-new-var makes a unique binding from the given binding
(defun make-unique-binding (binding expr)
  (if (unique-binding? binding expr)
      (make-bind-decl (id binding) (type binding))
      (let ((vid (make-new-variable (id binding) expr 1)))
	(make-bind-decl vid (type binding)))))

(defun unique-binding? (binding expr)
  (let ((unique? t))
    (mapobject #'(lambda (ex)
		   (or (not unique?)
		       (when (and (name? ex)
				  (declaration ex)
				  (not (eq (declaration ex) binding))
				  (eq (id ex) (id binding)))
			 (setq unique? nil)
			 t)))
	       expr)
    unique?))

(defun generate-cond-disjoint-tcc (expr conditions values)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-cond-disjoint-tcc expr conditions values)))
    (when ndecl
      (insert-tcc-decl 'disjointness expr nil ndecl)
      (add-tcc-comment 'disjointness expr nil))))

(defun make-cond-disjoint-tcc (expr conditions values)
  (let* ((*generate-tccs* 'none)
	 (conc (make-disjoint-cond-property conditions values)))
    (when conc
      (let* ((*no-expected* nil)
	     (*bound-variables* *keep-unbound*)
	     (true-conc? (tcc-evaluates-to-true conc))
	     (tform (unless true-conc? (add-tcc-conditions conc)))
	     (uform (cond ((or true-conc? (tcc-evaluates-to-true tform))
			   *true*)
			  (*simplify-tccs*
			   (pseudo-normalize tform))
			  (t tform)))
	     (id (make-tcc-name)))
	(assert (tc-eq (find-supertype (type uform)) *boolean*))
	(unless (tc-eq conc *true*)
	  (when (and *false-tcc-error-flag*
		     (tc-eq uform *false*))
	    (type-error expr
	      "Disjointness TCC for this expression simplifies to false:~2%  ~a"
	      tform))
	  (typecheck* (mk-cond-disjoint-tcc id uform) nil nil nil))))))

(defun make-disjoint-cond-property (conditions values)
  (let ((pairs (make-disjoint-cond-pairs conditions
					 (pseudo-normalize values))))
    (make-disjoint-cond-property* (nreverse pairs) nil)))

(defun make-disjoint-cond-property* (pairs prop)
  (cond ((null pairs)
	 prop)
	((null prop)
	 (make-disjoint-cond-property* (cdr pairs) (car pairs)))
	(t (make-disjoint-cond-property*
	    (cdr pairs)
	    (make!-conjunction (car pairs) prop)))))

(defun make-disjoint-cond-pairs (conditions values &optional result)
  (if (null (cdr conditions))
      result
      (make-disjoint-cond-pairs
       (cdr conditions)
       (cdr values)
       (append result
	       (mapcan #'(lambda (c v)
			   (unless (tc-eq v (car values))
			     (list
			      (make-negated-conjunction (car conditions) c))))
		       (cdr conditions)
		       (cdr values))))))

(defun generate-cond-coverage-tcc (expr conditions)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-cond-coverage-tcc expr conditions)))
    (when ndecl
      (insert-tcc-decl 'coverage expr nil ndecl)
      (add-tcc-comment 'coverage expr nil))))

(defun make-cond-coverage-tcc (expr conditions)
  (let* ((*generate-tccs* 'none)
	 (conc (make!-disjunction* conditions))
	 (true-conc? (tcc-evaluates-to-true conc))
	 (tform (unless true-conc? (add-tcc-conditions conc)))
	 (uform (cond ((or true-conc? (tcc-evaluates-to-true tform))
		       *true*)
		      (*simplify-tccs*
		       (pseudo-normalize tform))
		      (t (beta-reduce tform))))
	 (*no-expected* nil)
	 (*bound-variables* *keep-unbound*)
	 (id (make-tcc-name)))
    (assert (tc-eq (find-supertype (type uform)) *boolean*))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error expr
	  "Coverage TCC for this expression simplifies to false:~2%  ~a"
	  tform))
      (typecheck* (mk-cond-coverage-tcc id uform) nil nil nil))))

(defvar *evaluate-tccs* t)

(defun tcc-evaluates-to-true (&rest exprs)
  (when *evaluate-tccs*
    (tcc-evaluates-to-true* exprs)))

(defun tcc-evaluates-to-true* (exprs)
  (and exprs
       (or (and (ground-arithmetic-term? (car exprs))
		(get-arithmetic-value (car exprs)))
	   (tcc-evaluates-to-true* (cdr exprs)))))

(defun add-tcc-comment (kind expr type &optional reason subsumed-by in-insert?)
  (unless (or *in-checker* *in-evaluator* *collecting-tccs*)
    (let* ((decl (current-declaration))
	   (theory (current-theory))
	   (place (or (place *set-type-actuals-name*)
		      (place expr) (place type)))
	   (preason (cdr (assq expr *compatible-pred-reason*)))
	   (aname (or *set-type-actuals-name* expr))
	   (tcc-comment (list kind aname type reason place preason))
	   (decl-tcc-comments (assq decl (tcc-comments theory))))
      (cond ((member tcc-comment (cdr decl-tcc-comments) :test #'equal)
	     (when in-insert?
	       (decf (total-tccs))))
	    (t
	     (if decl-tcc-comments
		 (nconc decl-tcc-comments (list tcc-comment))
		 (push (list decl tcc-comment) (tcc-comments theory)))
	     (cond (subsumed-by
		    (incf (tccs-matched))
		    (push subsumed-by (refers-to (current-declaration))))
		   (t (if (numberp (tccs-simplified))
			  (incf (tccs-simplified))
			  (setf (tccs-simplified) 1)))))))))

(defun print-tcc-comment (decl kind expr type reason place preason)
  (let* ((submsg (case kind
		   (actuals nil)
		   (assuming (format nil "generated from assumption ~a.~a"
			       (id (module type)) (id type)))
		   (cases
		    (format nil
			"for missing ELSE of CASES expression over datatype ~a"
		      (id type)))
		   (coverage nil)
		   (disjointness nil)
		   (existence nil)
		   (mapped-axiom
		    (unpindent type 4 :string t :comment? t))
		   ((subtype termination-subtype)
		    (format nil "expected type ~a"
		      (unpindent type 19 :string t :comment? t)))
		   (termination nil)
		   (well-founded (format nil "for ~a" (id decl)))))
	 (plstr (when place
		  (format nil "(at line ~d, column ~d)"
		    (starting-row place) (starting-col place)))))
    (format nil
	"% The ~@[~a ~]~a TCC ~@[~a~] in decl ~a for~
                        ~:[ ~;~%    % ~]~@[~a~]~@[~%    % ~a~]~%  ~a"
      preason kind plstr
      (if (importing? decl) "IMPORTING" (id decl))
      (> (+ (length preason)
	    (length (string kind))
	    (length plstr)
	    (if expr
		(length (unpindent expr 4 :string t :comment? t))
		0)
	    25)
	 *default-char-width*)
      (when expr
	(unpindent expr 4 :string t :comment? t))
      submsg
      (tcc-reason-string reason))))

(defun tcc-reason-string (reason)
  (cond ((eq reason 'in-context)
	 "% TCC is in the logical context")
	((and (consp reason) (eq (car reason) 'subsumed))
	 (format nil "% is subsumed by ~a" (id (cadr reason))))
	((and (consp reason) (eq (car reason) 'map-to-nonempty))
	 (cdr reason))
	((null reason)
	 "% was not generated because it simplifies to TRUE.")
	(t (break "problem"))))
