;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Oct 31 02:59:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 03:00:29 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)



(defun replace-rule-fun (sformnum &optional sformnums dir hide?
				  actuals?) 
  #'(lambda (ps)(replace-step sformnum sformnums dir hide?
			      actuals? ps)))

(defun replace-step (source-sformnum target-sformnums direction hide?
			      actuals? ps)  
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
	       (let ((new-s-forms
		      (replace-loop lhs rhs sformnum
				    (if (null sformnums)
					'* sformnums)
				    (s-forms goalsequent)
				    1 -1))) 
		 (if (every #'eql  new-s-forms (s-forms goalsequent))
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


(defun replace-loop (lhs rhs sformnum sformnums sforms pos neg)
  (let ((*replace-cache*
	 (make-hash-table :test #'eq)))
    (if (null sforms) nil
	(if (negation? (formula (car sforms)))
	    (if (or (eq sformnum neg)
		    (not (in-sformnums? (car sforms) pos neg sformnums)))
		(cons (car sforms)
		      (replace-loop lhs rhs sformnum sformnums  (cdr sforms)
				    pos (1- neg)))
		(cons (replace-expr lhs rhs (car sforms))
		      (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				    pos (1- neg))))
	    (if (or (eq sformnum pos)
		    (not (in-sformnums? (car sforms) pos neg sformnums)))
		(cons (car sforms)
		      (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				    (1+ pos) neg))
		(cons (replace-expr lhs rhs (car sforms))
		      (replace-loop lhs rhs sformnum sformnums (cdr sforms)
				    (1+ pos) neg)))))))

	       
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
      (lcopy expr
	'argument (replace-expr* lhs rhs (argument expr) nil))))

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
      (let ((*bound-variables* (append (bindings expr) *bound-variables*)))
	(lcopy expr
	  'expression (replace-expr* lhs rhs (expression expr) nil)))))

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
  (if (type-value expr) expr ;;NSH(7.15.94): no replace on types.
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
