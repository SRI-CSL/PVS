;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace.lisp -- Body of the replace primitive rule.
;; Author          : N. Shankar and Sam Owre
;; Created On      : Sat Oct 31 02:59:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 03:00:29 1998
;; Update Count    : 1
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


(defun replace-rule-fun (sformnum &optional sformnums dir hide? 
				  actuals? dont-delete?) 
  #'(lambda (ps)(replace-step sformnum sformnums dir hide?
			      actuals?  dont-delete? ps)))

(defun replace-step (source-sformnum target-sformnums direction hide?
			      actuals? dont-delete? ps)  
  (let* ((goalsequent (current-goal ps))
	 (sformnum (find-sform (s-forms goalsequent) source-sformnum))
	 (selected-s-forms (select-seq (s-forms goalsequent)
				       (list sformnum)))
	 (sformnums (find-all-sformnums (s-forms (current-goal ps)) target-sformnums #'always-true))
	 ;;(remaining-s-forms (delete-seq (s-forms goalsequent)
	 ;;(list sformnum)))
	 (*replace-in-actuals?* actuals?)
	 (*modsubst* t))
    (cond ((null selected-s-forms)
	   (if (memq sformnum '(- + *))
	       (error-format-if "~%Can only replace single formulae.  Look at (help replace*)~%")
	       (error-format-if "~%No sequent formula corresponding to ~a,~%" source-sformnum))
	   (values 'X nil nil))
	  ((not sformnums)
	   (error-format-if "~%No sequent formula corresponding to target ~a~%" target-sformnums)
	   (values 'X nil nil))
	  (t (let* ((sform (car selected-s-forms))
		    (fmla (formula sform))
		    ;;(freevars-fmla (freevars fmla))
		    (lhs
		     (if  (negation? fmla)
			  (if (or (equation? (args1 fmla))
				  (iff-or-boolean-equation? (args1 fmla)))
			      (if (eq direction 'rl)
				  (args2 (args1 fmla))
				  (args1 (args1 fmla)))
			      (args1 fmla))
			  fmla))
		    (rhs
		     (if  (negation? fmla)
			  (if (or (equation? (args1 fmla))
				  (iff-or-boolean-equation? (args1 fmla)))
			      (if (eq direction 'rl)
				  (args1 (args1 fmla))
				  (args2 (args1 fmla)))
			      *true*)
			  *false*)))
	       ;;	     (format-if "~%Replacing using formula ~a," source-sformnum)
	       (let* ((*replace-cache* (make-hash-table :test #'eq))
		      (new-s-forms
		       (replace-loop lhs rhs sformnum
				     (if (null sformnums)
					 '* sformnums)
				     (s-forms goalsequent)
				     1 -1 dont-delete?)))
		 (if (every #'tc-eq new-s-forms (s-forms goalsequent))
		     (values 'X nil nil)
		     (let* ((new-s-forms
			     (if hide?
				 (remove sform new-s-forms)
				 new-s-forms))
			    (hidden-s-forms
			     (hidden-s-forms (current-goal ps)))
			    (hidden-s-forms 
			     (if hide?
				 (pushnew sform
					  hidden-s-forms
					  :test
					  #'(lambda (x y)
					      (tc-eq (formula x)(formula y))))
				 hidden-s-forms)))
		       (values '?
			       (list
				(lcopy goalsequent
				  's-forms new-s-forms
				  'hidden-s-forms hidden-s-forms
				  )))))))))))

;(defvar *no-match-in-replace* nil)
;(defun match-eq (lhs rhs)
;  (if (and *no-match-in-replace*
;	   (memq rhs (arguments *no-match-in-replace*)))
;      (tc-eq lhs rhs)
;  (not (eq (match lhs rhs nil nil) 'fail))))


(defun replace-loop (lhs rhs sformnum sformnums sforms pos neg dont-delete?)
  (when sforms
    (let ((*replace-cache* (make-hash-table :test #'eq)))
      (if (negation? (formula (car sforms)))
	  (if (or (eq sformnum neg)
		  (not (in-sformnums? (car sforms) pos neg sformnums)))
	      (cons (car sforms)
		    (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				  pos (1- neg) dont-delete?))
	      (let* ((result (replace-expr lhs rhs (car sforms)))
		     (new-fmla (formula result))
		     (keep-result 
		      (if (and dont-delete?
			       (tc-eq new-fmla *false*))
			  (car sforms)
			  result)))
		(cons keep-result
		      (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				    pos (1- neg) dont-delete?))))
	  (if (or (eq sformnum pos)
		  (not (in-sformnums? (car sforms) pos neg sformnums)))
	      (cons (car sforms)
		    (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				  (1+ pos) neg dont-delete?))
	      (let* ((result (replace-expr lhs rhs (car sforms)))
		     (new-fmla (formula result))
		     (keep-result 
		      (if (and dont-delete?
			       (tc-eq new-fmla *false*))
			  (car sforms)
			  result)))
		(cons keep-result
		      (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				    (1+ pos) neg dont-delete?))))))))

	       
(defun replace-expr (lhs rhs sequent)
  (replace-expr* lhs rhs sequent nil))

(defmethod replace-expr* (lhs rhs (sequent sequent) lastopinfix?)
  (declare (ignore lastopinfix?))
  (lcopy sequent
    's-forms (replace-expr* lhs rhs (s-forms sequent) nil)))

(defmethod replace-expr* (lhs rhs (list list) lastopinfix?)
  (declare (ignore lastopinfix?))
  (cond ((null list) nil)
	(t (let ((car-expr (replace-expr* lhs rhs (car list) nil))
		 (cdr-expr (replace-expr* lhs rhs (cdr list) nil)))
	     (if (and (eql car-expr (car list))
		      (equal cdr-expr (cdr list)))
		 list
		 (cons car-expr cdr-expr))))))

(defmethod replace-expr* (lhs rhs (s-formula s-formula) lastopinfix?)
  (declare (ignore lastopinfix?))
  (lcopy s-formula
    'formula (replace-expr* lhs rhs (formula s-formula) nil)))

(defmethod replace-expr* :around (lhs rhs (expr expr) lastopinfix?)
  (or (gethash expr *replace-cache*)
      (let ((rexpr (call-next-method)))
	(setf (gethash expr *replace-cache*) rexpr))))

(defmethod replace-expr* (lhs rhs (expr equation) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((newarg (replace-expr* lhs rhs (argument expr)
				    (infix-equation? expr)))
	     (newexprs (if (tuple-expr? newarg)
			   (exprs newarg)
			   (make-projections newarg))))
	(if (tc-eq (car newexprs)(cadr newexprs))
	    *true*
	    (if (iff-or-boolean-equation? expr)
		(if (tc-eq (car newexprs) *true*)
		    (cadr newexprs)
		    (if (tc-eq (cadr newexprs) *true*)
			(car newexprs)
			(lcopy expr 'argument newarg)))
		(lcopy expr 'argument newarg))))))

(defmethod replace-expr* (lhs rhs (expr conjunction) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((newarg (replace-expr* lhs rhs (argument expr)
				    (infix-conjunction? expr)))
	     (newexprs (if (tuple-expr? newarg)
			   (exprs newarg)
			   (make-projections newarg))))
	(if (tc-eq (car newexprs) *true*)
	    (cadr newexprs)
	    (if (tc-eq (cadr newexprs) *true*)
		(car newexprs)
		(if (or (tc-eq (car newexprs) *false*)
			(tc-eq (cadr newexprs) *false*))
		    *false*
		    (lcopy expr 'argument newarg)))))))

(defmethod replace-expr* (lhs rhs (expr disjunction) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((newarg (replace-expr* lhs rhs (argument expr)
				    (infix-disjunction? expr)))
	     (newexprs (if (tuple-expr? newarg)
			   (exprs newarg)
			   (make-projections newarg))))
	(if (tc-eq (car newexprs) *false*)
	    (cadr newexprs)
	    (if (tc-eq (cadr newexprs) *false*)
		(car newexprs)
		(if (or (tc-eq (car newexprs) *true*)
			(tc-eq (cadr newexprs) *true*))
		    *true*
		    (lcopy expr 'argument newarg)))))))

(defmethod replace-expr* (lhs rhs (expr implication) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((newarg (replace-expr* lhs rhs (argument expr)
				    (infix-implication? expr)))
	     (newexprs (if (tuple-expr? newarg)
			   (exprs newarg)
			   (make-projections newarg))))
	(if (tc-eq (car newexprs) *true*)
	    (cadr newexprs)
	    (if (or (tc-eq (car newexprs) *false*)
		    (tc-eq (cadr newexprs) *true*))
		*true*
		(if (tc-eq (cadr newexprs) *false*)
		    (negate! (car newexprs))
		    (lcopy expr 'argument newarg)))))))

(defmethod replace-expr* (lhs rhs (expr negation) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((newarg (replace-expr* lhs rhs (argument expr)
				    nil)))
	(if (tc-eq newarg *true*)
	    *false*
	    (if (tc-eq newarg *false*)
		*true*
		(lcopy expr 'argument newarg))))))

(defmethod replace-expr* (lhs rhs (expr application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let ((op (replace-expr* lhs rhs (operator expr) nil))
	    (arg (replace-expr* lhs rhs (argument expr)
				(typep expr 'infix-application))))
	(if (and (eq op (operator expr))
		 (eq arg (argument expr)))
	    expr
	    (let* ((stype (find-supertype (type op)))
		   (rtype (if (typep (domain stype) 'dep-binding)
			      (substit (range stype)
				(acons (domain stype) arg nil))
			      (range stype)))
		   (ex (lcopy expr
			 'operator op
			 'argument arg
			 'type rtype)))
	      (unless (eq op (operator expr))
		(change-application-class-if-necessary expr ex))
	      ex)))))

(defmethod replace-expr* (lhs rhs (expr field-application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'argument (replace-expr* lhs rhs (argument expr) nil))))

(defmethod replace-expr* (lhs rhs (expr projection-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      expr))

(defmethod replace-expr* (lhs rhs (expr injection-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      expr))

(defmethod replace-expr* (lhs rhs (expr injection?-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      expr))

(defmethod replace-expr* (lhs rhs (expr extraction-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      expr))

(defmethod replace-expr* (lhs rhs (expr projection-application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let ((arg (replace-expr* lhs rhs (argument expr) nil)))
	(if (tc-eq (argument expr) arg)
	    expr
	    (let* ((stype (find-supertype (type arg)))
		   (ntype (make!-projection-type*
			   (types stype) (index expr) 1 arg)))
	      (lcopy expr
		'argument arg
		'type ntype))))))

(defmethod replace-expr* (lhs rhs (expr injection-application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'argument (replace-expr* lhs rhs (argument expr) nil))))

(defmethod replace-expr* (lhs rhs (expr injection?-application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'argument (replace-expr* lhs rhs (argument expr) nil))))

(defmethod replace-expr* (lhs rhs (expr extraction-application) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'argument (replace-expr* lhs rhs (argument expr) nil))))

(defmethod replace-expr* (lhs rhs (expr record-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'assignments (replace-expr* lhs rhs (assignments expr) lastopinfix?))))

(defmethod replace-expr* (lhs rhs (expr tuple-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let ((nexprs (replace-expr* lhs rhs (exprs expr) lastopinfix?)))
	(if (equal (exprs expr) nexprs)
	    expr
	    (lcopy expr
	      'exprs nexprs
	      'type (mk-tupletype (mapcar #'type nexprs)))))))

(defmethod replace-expr* (lhs rhs (expr update-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'expression (replace-expr* lhs rhs (expression expr) lastopinfix?)
	'assignments (replace-expr* lhs rhs (assignments expr) lastopinfix?))))

(defmethod replace-expr* (lhs rhs (expr assignment) lastopinfix?)
  (lcopy expr
    'expression (replace-expr* lhs rhs (expression expr) lastopinfix?)
    'arguments (replace-expr* lhs rhs (arguments expr) lastopinfix?)))


(defmethod replace-expr* (lhs rhs (expr binding-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (let* ((nexpr (let ((*bound-variables* (append (bindings expr)
						     *bound-variables*)))
		      (replace-expr* lhs rhs (expression expr) nil)))
	     (new-bindings (make-new-bindings
			    (bindings expr)
			    (acons (mk-bind-decl (gensym) *boolean*)
				   (make!-tuple-expr* (list lhs rhs)) nil)
			    nexpr))
	     (nalist (unless (equal new-bindings (bindings expr))
		       (substit-pairlis (bindings expr) new-bindings nil)))
	     ;;(nexpr (when nalist (substit (expression expr) nalist)))
	     (*bound-variables* (append new-bindings *bound-variables*)))
	(if nalist
	    (lcopy expr
	      'bindings new-bindings
	      'expression (substit nexpr nalist))
	    (lcopy expr
	      'expression nexpr)))))

(defmethod replace-expr* (lhs rhs (expr cases-expr) lastopinfix?)
  (if (replace-eq expr lhs)
      (parenthesize rhs lastopinfix?)
      (lcopy expr
	'expression (replace-expr* lhs rhs (expression expr) nil)
	'selections (replace-expr* lhs rhs (selections expr) nil)
	'else-part  (replace-expr* lhs rhs (else-part expr) nil))))

(defmethod replace-expr* (lhs rhs (expr selection) lastopinfix?)
  (declare (ignore lastopinfix?))
  (let ((*bound-variables* (append (args expr) *bound-variables*)))
    (lcopy expr
      'expression (replace-expr* lhs rhs (expression expr) nil))))
	  

(defmethod replace-expr* (lhs rhs (expr branch) lastopinfix?)
  (if (replace-eq expr lhs)
      (parenthesize rhs lastopinfix?)
      (let ((new-condition
	     (replace-expr* lhs rhs (condition expr) nil)))
	(if (exequal new-condition *true*)
	    (replace-expr* lhs rhs (then-part expr) nil)
	    (if (exequal new-condition *false*)
		(replace-expr* lhs rhs (else-part expr) nil)
		(let ((new-then (replace-expr* lhs rhs (then-part expr) nil))
		      (new-else (replace-expr* lhs rhs (else-part expr)
				    nil)));;NSH(3.3.94): needed for lcopy.
		  (if (and (eql new-condition (condition expr))
			   (eql new-then (then-part expr))
			   (eql new-else (else-part expr)))
		      expr
		      (copy expr
			'argument
			(make-arg-tuple-expr
			 (list
			  new-condition new-then new-else))
			 ))))))))

(defmethod replace-expr* (lhs rhs (expr number-expr) lastopinfix?)
  (if (tc-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      expr))

(defmethod replace-expr* (lhs rhs (expr name-expr) lastopinfix?)
  (if (replace-eq lhs expr)
      (parenthesize rhs lastopinfix?)
      (if *replace-in-actuals?*
	  (lcopy expr
	    'actuals (replace-expr* lhs rhs (actuals expr) nil)
	    'resolutions
	    (replace-expr* lhs rhs (resolutions expr) nil))
	  expr)))

;;NSH(2.26.95): replace no longer goes inside actuals in response
;;to Paul Miner's complaint.  

(defmethod replace-expr* (lhs rhs (expr resolution) lastopinfix?)
  (declare (ignore lastopinfix?))
  (lcopy expr
    'module-instance (replace-expr* lhs rhs (module-instance expr) nil)))

(defmethod replace-expr* (lhs rhs (expr modname) lastopinfix?)
  (declare (ignore lastopinfix?))
  (lcopy expr 'actuals (replace-expr* lhs rhs (actuals expr) nil)))

(defmethod replace-expr* (lhs rhs (expr actual) lastopinfix?)
  (declare (ignore lastopinfix?))
  (if (type-value expr)
      (let ((ntype (replace-expr* lhs rhs (type-value expr) nil)))
	(if (eq ntype (type-value expr))
	    (lcopy expr
	      'expr (replace-expr* lhs rhs (expr expr) nil))
	    (lcopy expr
	      'expr ntype
	      'type-value ntype)))
      (let ((nexpr (replace-expr* lhs rhs (expr expr) nil)))
	(if (eq nexpr (expr expr))
	    expr
	    (lcopy expr 'expr (pseudo-normalize nexpr))))))

;(defmethod replace-expr* (lhs rhs (expr coercion)
;			      lastopinfix?)
;  (lcopy expr 'expression
;	 (replace-expr* lhs rhs (expression expr) nil)))
			     
(defmethod replace-expr* (lhs rhs (expr expr) lastopinfix?)
  (declare (ignore lhs rhs lastopinfix?))
  expr)

;;; Type exprs may be triggered in actuals if *replace-in-actuals?* is set

(defmethod replace-expr* (lhs rhs (expr type-name) lastopinfix?)
  (if *replace-in-actuals?* ;; Probably not needed - can't get here unless set
      (lcopy expr
	'actuals (replace-expr* lhs rhs (actuals expr) nil)
	'resolutions (replace-expr* lhs rhs (resolutions expr) nil))
      expr))

(defmethod replace-expr* (lhs rhs (expr subtype) lastopinfix?)
  (let ((ntype (replace-expr* lhs rhs (supertype expr) nil))
	(npred (replace-expr* lhs rhs (predicate expr) nil)))
    (if (and (eq ntype (supertype expr))
	     (eq npred (predicate expr)))
	(lcopy expr
	  'print-type (replace-expr* lhs rhs (print-type expr) nil))
	(copy expr
	  'supertype ntype
	  'predicate (pseudo-normalize npred)
	  'print-type nil))))

(defmethod replace-expr* (lhs rhs (expr funtype) lastopinfix?)
  (let* ((ndom (replace-expr* lhs rhs (domain expr) nil))
	 (*bound-variables* (if (dep-binding? (domain expr))
				(cons (domain expr) *bound-variables*)
				*bound-variables*))
	 (srng (if (or (not (binding? ndom))
		       (eq ndom (domain expr)))
		   (range expr)
		   (substit (range expr) (acons (domain expr) ndom nil))))
	 (nrng (replace-expr* lhs rhs srng nil))
	 (ptype (when (and (eq ndom (domain expr))
			   (eq nrng (range expr)))
		  (replace-expr* lhs rhs (print-type expr) nil))))
    (lcopy expr 'domain ndom 'range nrng 'print-type ptype)))

(defmethod replace-expr* (lhs rhs (expr tupletype) lastopinfix?)
  (let ((ntypes (replace-expr-types lhs rhs (types expr))))
    (lcopy expr
      'types ntypes
      'print-type (when (eq ntypes (types expr))
		    (replace-expr-types lhs rhs (print-type expr))))))

(defmethod replace-expr* (lhs rhs (expr cotupletype) lastopinfix?)
  (let ((ntypes (replace-expr-types lhs rhs (types expr))))
    (lcopy expr
      'types ntypes
      'print-type (when (eq ntypes (types expr))
		    (replace-expr-types lhs rhs (print-type expr))))))

(defun replace-expr-types (lhs rhs types)
  (let ((ntypes (replace-expr-types* lhs rhs types)))
    (if (equal ntypes types)
	types
	ntypes)))

(defun replace-expr-types* (lhs rhs types &optional result)
  (if (null types)
      (nreverse result)
      (let* ((ncar (replace-expr* lhs rhs (car types) nil))
	     (*bound-variables* (if (binding? (car types))
				    (cons ncar *bound-variables*)
				    *bound-variables*)))
	(replace-expr-types*
	 lhs rhs
	 (if (or (not (binding? (car types)))
		 (eq ncar (car types)))
	     (cdr types)
	     (substit (cdr types) (acons (car types) ncar nil)))
	 (cons ncar result)))))

(defmethod replace-expr* (lhs rhs (expr recordtype) lastopinfix?)
  (let ((nfields (replace-expr-types lhs rhs (fields expr))))
    (lcopy expr
      'fields nfields
      'print-type (when (eq nfields (fields expr))
		    (replace-expr-types lhs rhs (print-type expr))))))

(defmethod replace-expr* (lhs rhs (expr dep-binding) lastopinfix?)
  (let ((ntype (replace-expr* lhs rhs (type expr) nil)))
    (lcopy expr 'type ntype)))

(defmethod replace-expr* (lhs rhs (expr field-decl) lastopinfix?)
  (let ((ntype (replace-expr* lhs rhs (type expr) nil)))
    (lcopy expr
      'type ntype
      'declared-type (if (eq ntype (type expr))
			 (replace-expr* lhs rhs (declared-type expr) nil)
			 ntype))))
