;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert.lisp -- 
;; Author          : Natarajan Shankar
;; Created On      : Fri Oct  8 12:10:11 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr  5 03:30:03 1998
;; Update Count    : 102
;; Status          : Beta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'PVS)

;;assert-sformnums calls sequent-reduce with a list of sformnums and
;;invokes assert-sform on each selected sform.  assert-sform invokes
;;the decision procedure on each sform.  rewrite-flag is used to direct
;;simplification to only the lhs or rhs of a rewrite so that a match
;;is not destroyed through simplification.  flush? = T flushes the
;;ground-prover database for a fresh start.  linear? = T makes nonlinear
;;multiplication uninterpreted.  flag is either note, simplify,
;;assert, or *rewrite.

(defun pvs-initprover ()
  (setq *alists* (copy *init-alists*))
  (setq *dp-state* (dp::push-new-cong-state *init-dp-state*)))

(defun invoke-simplification (sformnums record? rewrite?
				   rewrite-flag flush? linear?
				   cases-rewrite? type-constraints?
				   ignore-prover-output?)
				   ;;assert-connectives?
  #'(lambda (ps)
      (let ((*cases-rewrite* cases-rewrite?)
	    ;;(*assert-connectives?* assert-connectives?)
	    (*ignore-prover-output?* ignore-prover-output?))
	(if record?
	    (if rewrite?
		(assert-sformnums
		 sformnums rewrite-flag flush? linear? nil
		 T type-constraints? ps)
		(assert-sformnums
		 sformnums rewrite-flag flush? linear? 'RECORD
		 T type-constraints? ps))
	    (if rewrite?
		(assert-sformnums
		 sformnums rewrite-flag flush? linear? 'REWRITE
		 T type-constraints? ps)
		(assert-sformnums
		 sformnums rewrite-flag flush? linear? 'SIMPLIFY
		 T type-constraints? ps))))))
      
 
(defun assert-sformnums (sformnums 
			 rewrite-flag flush? linear?
			 flag hash-rewrites?
			 type-constraints? ps)
  (let* ((*printerpmult* (if linear? 'normal *printerpmult*))
	 (*printerpdivide* (if linear? 'no *printerpdivide*))
	 (*assert-flag* flag)
	 (*top-assert-flag* flag)
	 (*process-output* nil)
	 (goalsequent (current-goal ps))
	 (*dependent-decls* nil)
	 (*hash-rewrites?* hash-rewrites?)
	 (*rewrite-hash* (if *hash-rewrites?*
			     (copy (rewrite-hash ps))
			     (rewrite-hash ps)))
	 (*subtype-hash* (if flush?
			     (make-hash-table :hash-function 'pvs-sxhash
					      :test 'tc-eq)
			     (copy (subtype-hash ps))))
	 (*assert-typepreds-off* (not type-constraints?))
	 (*alists* (alists ps))
	 (*dp-state* (dp-state ps)))
    (unwind-protect
	(protecting-cong-state
	 ((*dp-state* (if flush?
			  *init-dp-state*
			  *dp-state*))
	  (*alists* (if flush?
			*init-alists*
			*alists*)))
	 (assert-sequent goalsequent sformnums rewrite-flag))
      (clrhash *subst-type-hash*))))

(defun find-remaining-sformnums (sforms sformnums sub-sformnums
					&optional (pos 1)(neg -1)(acc nil))
  (if (null sforms)
      (nreverse acc)
      (let* ((sign (not (negation? (formula (car sforms)))))
	     (newpos (if sign (1+ pos) pos))
	     (newneg (if sign neg (1- neg)))
	     (newacc (if (and (in-sformnums? (car sforms) pos neg sformnums)
			      (not (in-sformnums? (car sforms) pos neg
						  sub-sformnums)))
			 (cons (if sign pos neg) acc)
			 acc)))
	(find-remaining-sformnums (cdr sforms) sformnums sub-sformnums
				  newpos newneg newacc))))

(defun assert-sequent (sequent sformnums &optional rewrite-flag)
  (let* ((simplifiable-sformnums
	  (find-all-sformnums (s-forms sequent) sformnums
			      #'(lambda (fmla)
				  (if (negation? fmla)
				      (connective-occurs? (args1 fmla))
				      (connective-occurs? fmla)))))
	 (other-sformnums
	  (find-remaining-sformnums (s-forms sequent) sformnums
			      simplifiable-sformnums)))
  (multiple-value-bind
	(signal subgoal)
      (sequent-reduce sequent
		      #'(lambda (sform) (assert-sform sform rewrite-flag))
		      other-sformnums)
    (cond ((eq signal '!) (values '! nil
				  (list 'dependent-decls *dependent-decls*)))
	  (t (multiple-value-bind
		   (newsignal newsubgoal)
		 (if (eq *assert-flag* 'record)
		     (values 'X
			     (if (eq signal 'X) sequent subgoal))
		     (sequent-reduce-around
		      (if (eq signal 'X) sequent
			  subgoal)
		      #'(lambda (sform)
			  (assert-sform sform
					rewrite-flag
					T))
		      simplifiable-sformnums))
	       (cond ((eq newsignal '!)
		      (values '! nil (list 'dependent-decls *dependent-decls*)))
		     ((and (eq signal 'X)(eq newsignal 'X))
		      (values 'X nil nil))
		     (t 
		      (values
		       '?
		       (list (cons (if (eq newsignal 'X)
				       subgoal
				       newsubgoal)
				   (list 'rewrite-hash *rewrite-hash*
					 'subtype-hash *subtype-hash*
					 'dependent-decls *dependent-decls*
					 'dp-state *dp-state*
					 'alists *alists*))))))))))))

;;this is needed to take care of the output from process.
(defun sequent-reduce-around (sequent simplifier sformnums)
  (multiple-value-bind (signal newsequent)
      (sequent-reduce sequent simplifier sformnums)
    (cond ((eq signal '!)
	   (values signal newsequent))
	  ((or (memq *assert-flag* '(simplify rewrite))
	       *ignore-prover-output?*)
	   (values signal newsequent))
	  (t (assert-process-output signal newsequent)))))

(defun assert-process-output (signal sequent)
  (nprotecting-cong-state
   ((*dp-state* *dp-state*)
    (*alists* *alists*))
   (let ((result (catch 'context (process-assert *process-output*))))
     (if (false-p result)
	 (values '! sequent)
	 (values signal sequent)))))

(defun process-assert (forms)
  (if (null forms) nil
      (let* ((fmla (car forms))
	     (op (when (consp fmla) (car fmla))))
	(cond ((eq op 'OR)
	       (when (loop for x in (cdr fmla)
			   always (nprotecting-cong-state
				   ((*dp-state* *dp-state*)
				    (*alists* *alists*))
				   (let* ((result
					   (catch 'context 
					     (process-assert
					      (cons x (cdr forms))))))
				     (false-p result))))
		   (retfalse)))
	      ((memq op '(if if* implies not and iff))
	       (process-assert (cdr forms)))
	      (t (let ((result (call-process fmla *dp-state* *alists*)))
		   (if (false-p result)
		       (throw 'context *false*)
		       (process-assert (cdr forms)))))))))

(defmethod unit-recognizer? (rec) ;recognizer for 0-ary constructor.
  (declare (ignore rec))
  nil)

(defmethod unit-recognizer? ((rec recognizer-name-expr))
  (if (eq (unit? rec) 'unbound)
      (setf (unit? rec)
	    (let* ((constructor (constructor rec))
		   (accessors (accessors constructor)))
	      (when (null accessors) constructor)))
      (unit? rec)))

(defun unit-derecognize (expr)
  (if (negation? expr)
      (negate (unit-derecognize (args1 expr)))
      (if (application? expr)
	  (let ((unit (unit-recognizer? (operator expr))))
	    (if (or (not unit)(tc-eq (args1 expr) unit))
		expr
		;;multiple-value-bind (sig fmla);;assert-if too slow
		  ;;  (assert-if unit);;to get its subtype constraint.
		(progn (record-type-constraints unit)
		       (make!-equation (args1 expr) unit))))
	  expr)))

(defun assert-sform (sform &optional rewrite-flag simplifiable?)
  (let ((*assert-typepreds* nil)
	(*auto-rewrite-depth* 0))
    (multiple-value-bind (signal sform)
	(assert-sform* sform rewrite-flag simplifiable?)
      (cond ((eq signal '!)(values signal sform))
	    ((or (eq signal '?) *assert-typepreds*)
	     ;;(break "assert-typepreds")
	     (if (some #'(lambda (fmla)
			   (let* ((sign (not (negation? fmla)))
				  (body (if sign fmla (args1 fmla)))
				  ;;(*update-occurs?* T)
				  )
			     ;;NSH(5.13.97): rearranged lets
			     ;;so translation avoided
			     ;;when there is a connective.
			     ;;want to check for connectives even if
			     ;;assert-connectives? is T
			     (and (not (connective-occurs? body))
				  (let* (
					 ;(translated-body
					 ; (top-translate-to-prove
					 ;  body))
					 ;(translated-fmla
					 ; (if sign translated-body
					 ;     (list 'NOT translated-body)))
					 (res (call-process fmla
							    *dp-state*
							    *alists*)))
				    (when (consp res)
				      (loop for x in res
					    do (push x *process-output*)))
				    (false-p res)))))
		       *assert-typepreds*)
		 (values '! sform)
		 (values '? sform)))
	    (t (values signal sform))))))

(defun assert-typepreds (typepreds)
  (when (consp typepreds)
      (let* ((fmla (car typepreds))
	     (sign (not (negation? fmla)))
	     (body (if sign
		       fmla
		       (args1 fmla)))
	     ;;(*update-occurs?* T)
	     )
			     ;;NSH(5.13.97): rearranged lets
			     ;;so translation avoided
			     ;;when there is a connective.
			     ;;want to check for connectives even if
			     ;;assert-connectives? is T
	(or (and (not (connective-occurs? body))
		 (let* (
			;(translated-body
			; (top-translate-to-prove
			;  body))
			;(translated-fmla
			; (if sign translated-body
			;     (list 'NOT
			;	   translated-body)))
			(res (call-process fmla
					   *dp-state* *alists*)))
		   (when (consp res)
		     (loop for x in res
			   do (push x *process-output*)))
		   (false-p res)))
	    (assert-typepreds (cdr typepreds))))))

(defun assert-sform* (sform &optional rewrite-flag simplifiable?)
  (let* ((fmla (formula sform))
	 (sign (not (negation? fmla)))
	 (body (if sign fmla (args1 fmla)))
	 (*bound-variables* nil)
	 (*top-rewrite-hash* *rewrite-hash*))
    (protecting-cong-state
     ((*top-dp-state* *dp-state*)
      (*top-alists* *alists*))
     ;(break "0")
     (cond (rewrite-flag
	    (multiple-value-bind (sig newbodypart)
		(if (or (iff? body)(equation? body))
		    (if (eq rewrite-flag 'RL)
			(assert-if (args1 body))
			(assert-if (args2 body)))
		    (values 'X body))
	      (if (eq sig 'X)
		  (if (or (and sign (tc-eq fmla *false*))
			  (and (not sign)(tc-eq body *true*)))
		      (values '? nil)
		      (values 'X sform))
		  (let ((newbody
			 (copy body
			   'argument
			   (make!-arg-tuple-expr*
			    (if (eq rewrite-flag 'RL)
				(list newbodypart (args2 body))
				(list (args1 body) newbodypart))))))
		    (values '? (copy sform
				 'formula
				 (if sign newbody
				     (copy fmla
				       'argument
				       newbody))))))))
	   (simplifiable?		;(connective-occurs? body)
	    (multiple-value-bind (sig newfmla)
	           ;;NSH(7.27.96): I've been going back and forth
	           ;;on  assert-if-inside vs. assert-if here.
	           ;;assert-if fails because for an enum type
	           ;;red?(expr) triggers check-all-recognizers
	           ;;which causes self-simplification.  I don't
	           ;;recall when assert-if-inside misbehaves.
		(if (if sign (application? fmla)
			(application? (args1 fmla)))
		    (assert-if-inside fmla) 
		    (assert-if fmla))
	      (cond ((eq sig 'X)
		     (if (or (and sign (tc-eq fmla *false*))
			     (and (not sign)(tc-eq body *true*)))
			 (values '? nil)
			 (values 'X sform)))
		    ((and (not (eq *assert-flag* 'simplify))
			  (not (connective-occurs? newfmla)))
		     (process-sform sform newfmla sig))
		    (t (values '? (copy sform 'formula newfmla))))))
	   (t ;(break "1")
	      (multiple-value-bind (sig newfmla)
		  (assert-if-inside fmla)
		;(break "2")
		(if (memq *assert-flag* '(simplify rewrite))
		    (values sig (if (eq sig '?) (copy sform
						  'formula newfmla)
				    sform))
		    (process-sform sform
				   (if (eq sig '?) newfmla fmla)
				   sig))))))))

(defun process-sform (sform newfmla sig)
  ;(when (connective-occurs? newfmla)(break))
  (let* ((*bindings* nil)
	 ;(transformula (top-translate-to-prove (negate newfmla)))
	 (result (call-process (negate newfmla) *dp-state* *alists*)))
    ;(break "cp")
    (when (consp result)
      (loop for x in result do (push x *process-output*)))
    (if (false-p result)
	(values '! sform)
	(if (eq sig '?)
	    (let ((new-sform (copy sform
			       'formula newfmla)))
	      (values '? new-sform))
	     ;;;***Need a flag to check if *top-dp-state* was changed,
	     ;;;namely, is the new stuff essentially empty.  
	    (if (dp-changed *top-dp-state* *dp-state* *top-alists* *alists*)
		(values '? sform) 
		(values 'X sform))))))

(defun dp-changed (old-dpstate new-dpstate old-alists new-alists)
  (let ((new-changed (and *new-ground?*
			  (dp::dp-changed old-dpstate new-dpstate)))
	(old-changed (and *old-ground?*
			  (alists-changed old-alists new-alists))))
    (assert (or (not *break-on-ground-diff*)
		(not (and *new-ground?* *old-ground?*))
		(eq new-changed old-changed))
	    (*break-on-ground-diff*))
    (if *new-ground?*
	new-changed
	old-changed)))

(defun top-translate-to-dc (expr)
  (let ((*newdc* t))
    (top-translate-to-prove expr)))

(defun top-translate-to-old-prove (expr)
  (let ((*newdc* nil))
    (top-translate-to-prove expr)))

(defun translate-from-prove (expr)
  (cond
   ((true-p expr) *true*)
   ((false-p expr) *false*)
   (t expr)))

(defun translate-from-prove-list (list)
  (if (listp list)
      (mapcar #'translate-from-prove list)
      (translate-from-prove list)))

(defun init-cong-state ()
  (when *new-ground?* (dp::null-single-cong-state)))

(defun new-cs (old-cs)
  (when *new-ground?*
    (dp::push-new-cong-state old-cs)))

(defun restore-old-cs (new-cs)
  (when *new-ground?*
    (dp::npop-cong-state new-cs)))

(defun init-dp (&optional strong)
  (when *new-ground?*
    (dp::init-dp-0 strong)))

(defun compatible-dp-results (new-result old-result)
  (or (tc-eq new-result old-result)
      (and (true-p new-result) (true-p old-result))
      (and (true-p new-result) (null old-result))
      (and (false-p new-result) (false-p old-result))
      (and 
	   (or (listp old-result)
	       (and (not (or (true-p old-result)
			     (false-p old-result)))
		    (typep old-result 'syntax)))
	   (null new-result))))

(defvar *init-dp-state*
  (when nil
    (init-cong-state)))

(defun init-dc (&optional strong)
  (init-dp strong)
  (dp::return-all-cong-states dp::*made-cong-states*)
  (setq *init-dp-state* (init-cong-state))
  (reset-translate-from-dc)
  (reset-translate-to-dc))

(defun dp::restore ()
  (in-package pvs)
  (restore))

(defun alists-changed (old-alists new-alists)
  (not (and (eq (dpinfo-usealist old-alists)
		(dpinfo-usealist new-alists))
	    (eq (dpinfo-findalist old-alists)
		(dpinfo-findalist new-alists))
	    (eq (dpinfo-sigalist old-alists)
		(dpinfo-sigalist new-alists)))))

(defmethod quant-occurs? ((expr projection-application))
  (with-slots (argument) expr
    (quant-occurs? argument)))

(defmethod quant-occurs? ((expr field-application))
  (with-slots (argument) expr
    (quant-occurs? argument)))

(defmethod quant-occurs? ((expr application))
  (with-slots (operator argument) expr
    (or (quant-occurs? operator)
	(quant-occurs? argument))))

(defmethod quant-occurs? ((expr branch))
  (or (quant-occurs? (condition expr))
      (quant-occurs? (then-part expr))
      (quant-occurs? (else-part expr))))

(defmethod quant-occurs? ((expr list))
  (cond ((null expr) nil)
	(t (or (quant-occurs? (car expr))
	       (quant-occurs? (cdr expr))))))

;(defmethod quant-occurs? ((expr quant-expr))
;  t)

(defmethod quant-occurs? ((expr binding-expr))
  t)
;;   (quant-occurs? (expression expr))
;;(NSH:10/25): I'm not sure whether this should be just true
;;or as above.
;;(NSH:2/92): I've changed it to true since the ground prover
;;forgets the type constraints on binding-exprs and erroneously
;;declares two
;;nonequal binding exprs to be the same.

(defmethod quant-occurs? ((expr record-expr))
  (with-slots (assignments) expr
  (quant-occurs? assignments)))

(defmethod quant-occurs? ((assignment assignment))
  (with-slots (expression arguments) assignment
    (or (quant-occurs? arguments)
	(quant-occurs? expression))))

(defmethod quant-occurs? ((expr tuple-expr))
  (with-slots (exprs) expr
    (quant-occurs? exprs)))

(defmethod quant-occurs? ((expr update-expr))
  (with-slots (expression assignments) expr
    (or (quant-occurs? expression)
	(quant-occurs? assignments))))

(defmethod quant-occurs? ((expr expr))
  nil)

;;connective-occurs? tests if there are any propositional
;;connectives or IF-exprs in the expression that the decision
;;procedure could be used to simplify.

(defmethod connective-occurs? ((expr name-expr))
  NIL)

(defmethod connective-occurs? ((expr branch))
    T)

(defmethod connective-occurs? ((expr cases-expr))
  T)

(defmethod connective-occurs? ((expr projection-application))
  (connective-occurs? (argument expr)))

(defmethod connective-occurs? ((expr tuple-expr))
  (connective-occurs? (exprs expr)))

(defmethod connective-occurs? ((expr record-expr))
  (connective-occurs? (assignments expr)))

(defmethod connective-occurs? ((expr assignment))
  (or (connective-occurs? (arguments expr))
      (connective-occurs? (expression expr))))

(defmethod connective-occurs? ((expr field-application))
  (connective-occurs? (argument expr)))

(defmethod connective-occurs? ((expr application))
    (or ;;NSH(4.8.96) (negation? expr)
	(implication? expr)(conjunction? expr)
	(disjunction? expr)(iff? expr)
;	(and (equation? expr)(typep (args1 expr) 'tuple-expr)
;	     (typep (args2 expr) 'tuple-expr))
;	(and (equation? expr)(typep (args1 expr) 'record-expr)
;	     (typep (args2 expr) 'record-expr))
;;	(and (equation? expr)(adt-subtype? (type (args1 expr)))
;;	     (adt-subtype? (type (args2 expr))))
;	(and (projection? (operator expr))
;	     (typep (args1 expr) 'tuple-expr))
	(connective-occurs? (operator expr))
	(connective-occurs? (argument expr))))

(defmethod connective-occurs? ((expr list))
  (some #'connective-occurs? expr))
;  (cond ((null  expr) nil)
;	(t (or (connective-occurs? (car expr))
;	       (connective-occurs? (cdr expr)))))

(defmethod connective-occurs? ((expr binding-expr))
  nil
  ;(connective-occurs? (expression expr))
  )

(defmethod connective-occurs? ;;NSH(5.13.97) needed for updates
    ;;or the translations get HUGE.
    ((expr update-expr))
  T)

(defmethod connective-occurs? ((expr expr))
  NIL)

(defun cond-assert-if (expr &optional conditions)
  (if (number-expr? expr)  ;;NSH(4.7.96)
      (values 'X expr)
      (nprotecting-cong-state   ;;;changed from LET on alists
       ((*dp-state* *dp-state*)
	(*alists* *alists*))
      (let ((*rewrite-hash* (if *hash-rewrites?*
				(copy *rewrite-hash*)
				*rewrite-hash*))
	    (conditions (if (not (listp conditions))
			    (list conditions)
			    conditions))
	    )
	(loop for condition
	      in conditions  ;;NSH(5.18.97):restored check to catch
	                    ;;nested updates.
	      when (and (not (false-p condition)) ;;; DAC: condition
			;;;should never be false
			(not (check-for-connectives? condition)))
	      do (call-process condition *dp-state* *alists*))
	;;    (format T "~%  Simplifying ~a under conditions ~{~a, ~}"
	;;	       expr conditions);;NSH(10.10.94)omitting for now.
	(assert-if expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;assert-if-inside rewrites only inside the expression but leaves
;;the topmost level alone.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod assert-if-inside ((expr name-expr))
  (do-auto-rewrite expr 'X))
;;  (values 'X expr) ;;NSH(2.28.94)

;NSH(9.13.94): not needed. handled by default case. 
;(defmethod assert-if-inside ((expr projection-application))
;  (multiple-value-bind (sig newarg)
;      (assert-if (argument expr))
;    (if (typep newarg 'tuple-expr)
;	(nth (1- (index expr)) (exprs newarg))
;	(do-auto-rewrite (lcopy expr 'argument newarg) sig))))
  
(defmethod assert-if-inside ((expr application))
  (cond ((negation? expr)
	 (multiple-value-bind (sig newarg)
	     (assert-if-inside-sign (args1 expr) nil) ;;NSH(3.21.94) sign.
	   (if (eq sig '?)
	       (values '?
		       (lcopy expr
			      'argument newarg))
	       (values 'X expr))))
	(t (assert-if-inside-sign expr T))))   ;;NSH(3.21.94) sign.

;;NSH(11.22.94)
(defmethod assert-if-inside ((expr field-application))
  (with-slots (id argument) expr
	      (multiple-value-bind (sigarg newarg)
		  (assert-if argument)
		(reduce-field-application sigarg newarg id))))

(defmethod assert-if-inside ((expr projection-application))
  (with-slots (index argument) expr
	      (multiple-value-bind (sigarg newarg)
		  (assert-if argument)
      		      ;;NSH(2.28.95) correction: field->proj
		(reduce-proj-application sigarg newarg index))))

(defun assert-if-inside-sign (expr sign)
  (cond ((and (application? expr)
	      (not (branch? expr)))
	 (multiple-value-bind (sigop newop)
	     (assert-if (operator expr))
	   (multiple-value-bind (sigargs newargs)
	       (assert-if-arg expr)
	     (let* ((sig (if (eq sigop '?) '? sigargs))
		    (expr;;shadowing expr
		     (lcopy expr 'operator
			    (if (eq sigop '?) newop (operator expr))
			    'argument
			    (if (eq sigargs '?) newargs
				(argument expr)))))
	       (cond ((and (eq sigop '?)
			   (typep newop 'lambda-expr))
		      (multiple-value-bind (sig val)
			  (assert-if (substit (expression newop)
				       (pvs-pairlis (bindings newop)
						    (argument-list newargs))))
			(declare (ignore sig))
			(values '? val)))
		     ((and (is-predicate? newop)
			   (adt? (find-supertype
				  (type newargs)))
			   (typep newop 'name-expr)
			   (recognizer? newop))
		      (let ((result (check-other-recognizers
				     newop newargs T)))
			(if (and (false-p result) (null sign))
			    (values '? *false*)
			    (if (and (true-p result) sign)
				(values '? *true*)
				(do-auto-rewrite expr
						 sig)))))
		     ((and sign
			   (is-predicate? newop)
			   (member expr (type-constraints newargs t)
				   :test #'tc-eq));;(break)
		      (values '? *true*))
		     (t
		      (multiple-value-bind (newsig newval)
			  (assert-if-application expr newop newargs
						 (if (eq sigop '?) '?
						     sigargs))
			(if (eq newsig '?)
			    (if (if sign
				    (tc-eq newval *false*)
				    (tc-eq newval *true*))
				(values sig expr)
				(values '? newval))
			    (values sig expr)))))))))
	(t (assert-if-inside expr))))

(defmethod assert-if-inside ((expr branch))
  (assert-if expr))

(defun check-no-other-recognizers (recog arg)
  (loop for rec in
	(recognizers (find-supertype
		      (type arg)))
	always
	(or (tc-eq rec recog)
	    (false-p (assert-test (make!-application rec arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(7/23/93): The functions for recognizers below are meant to;;
;;speed up the simplification of case expressions which right   ;;
;;now make quadratic number of calls to process.                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-all-recognizers (arg)
  (let ((recs (recognizers (find-supertype (type arg))))
	(constructor
	      (if (and (name-expr? arg)(constructor? arg))
		  arg
		  (if (and (application? arg)
			   (constructor? (operator arg)))
		      (operator arg)
		      nil))))
    (if constructor
	(loop for rec in recs
	      collect
	      (if (tc-eq-ops (recognizer constructor) rec)
		  (cons rec *TRUE*)
		  (cons rec *FALSE*)))	     
	(loop for rec in recs
	      collect
	      (cons rec (assert-test (make!-application rec arg)))))))


(defun check-rest-recognizers (rec all-result &optional indirect)
  (cond ((null all-result) 'RESTFALSE)
        ((same-id rec (caar all-result))
         (if (and (or (true-p (cdar all-result))
		      (false-p (cdar all-result)))
		  (null indirect))
             (cdar all-result)
             (check-rest-recognizers rec (cdr all-result) indirect)))
        (t (if (true-p (cdar all-result))
                *FALSE*
               (let ((rest (check-rest-recognizers rec (cdr all-result)
						   indirect))) 
                  (cond ((or (true-p rest)
			     (false-p rest))
			 rest)
                        ((eq rest 'RESTFALSE)
                         (if (false-p (cdar all-result))
                             'RESTFALSE
                             nil))
                        (t nil)))))))

(defun check-some-recognizer (rec all-result &optional indirect) ;;;all-result comes from
					      ;;;check-all-recognizers
    (let ((rest (check-rest-recognizers rec all-result indirect)))
       (if (eq rest 'RESTFALSE) *TRUE*
           rest)))


(defun check-other-recognizers (recog arg &optional indirect)
  (check-some-recognizer recog (check-all-recognizers arg) indirect))


(defmethod assert-if ((expr update-expr))
  (with-slots (expression assignments) expr
    (let* ((newexpr (nth-value 1 (assert-if expression)))
	   (newassign (nth-value 1 (assert-if assignments)))
	   (new-update-expr (simplify-nested-updates newexpr newassign expr)))
      (if (eq new-update-expr expr)
	  (do-auto-rewrite expr 'X)
	  (do-auto-rewrite new-update-expr '?)))))

(defmethod simplify-nested-updates ((expr record-expr) outer-assignments
				    update-expr)
  (with-slots (assignments) expr
    (let* ((new-expr-assigns
	    (loop for assign in assignments
		  collect (make-updated-assign assign outer-assignments)))
	   (new-outer-assigns
	    (collect-new-outer-assigns assignments outer-assignments))
	   (outer-to-inner-assigns
	    (loop for assign in new-outer-assigns
		  when (singleton? (arguments assign))
		  collect assign))
	   (outer-outer-assigns
	    (loop for assign in new-outer-assigns
		  when (not (singleton? (arguments assign)))
		  collect assign))
	   (new-expr (if (and (equal new-expr-assigns assignments)
			      (null outer-to-inner-assigns))
			 expr
			 (lcopy expr 'assignments
				(nconc new-expr-assigns
				       outer-to-inner-assigns)))))
      (if (null outer-outer-assigns)
	  new-expr
	  (if (equal outer-outer-assigns outer-assignments)
	      (lcopy update-expr 'expression new-expr)
	      (lcopy update-expr
		'expression new-expr
		'assignments outer-outer-assigns))))))

(defmethod simplify-nested-updates ((expr update-expr) outer-assignments
				    update-expr)
  (declare (ignore update-expr))
  (with-slots (expression assignments) expr
    (let* ((new-expr-assigns
	    (loop for assign in assignments
		  collect (make-updated-assign assign outer-assignments)))
	   (new-outer-assigns
	    (collect-new-outer-assigns assignments outer-assignments))
	   (new-merged-assignments
	    (nconc new-expr-assigns new-outer-assigns)))
      (lcopy expr
	'assignments new-merged-assignments))))

(defun make-updated-assign (assignment outer-assignment)
  (with-slots (arguments expression) assignment
    (lcopy assignment
      'expression (make-updated-assign-expr arguments expression
					    outer-assignment nil))))

;;checks if assign-args are reassigned in outer-assignments and
;;returns the updated or overridden assign-expr.
(defun make-updated-assign-expr (assign-args assign-expr outer-assignments
					     accum-assignments)
  (if (consp outer-assignments)
      (let* ((outer-args1 (arguments (car outer-assignments)))
	     (outer-assgn1 (expression (car outer-assignments)))
	     (match (match-update-args-prefix? assign-args outer-args1)))
	(if match
	    (make-updated-assign-expr assign-args assign-expr
				      (cdr outer-assignments)
				      (cons (cons match outer-assgn1)
					    accum-assignments))
	    (make-updated-assign-expr assign-args assign-expr
				      (cdr outer-assignments)
				      accum-assignments)))
      (if accum-assignments
	  (let ((final-override
		 (some #'(lambda (x)
			   (and (eq (car x) T) x)) accum-assignments)))
	    (if final-override
		(cdr final-override)
		(let* ((naccum-assignments (nreverse accum-assignments))
		       (tc-naccum-assignments
			(loop for (x . y) in naccum-assignments
			      collect (make-assignment x y))))
		  (simplify-nested-updates
		   assign-expr
		   tc-naccum-assignments
		   (make!-update-expr assign-expr tc-naccum-assignments)))))
	  assign-expr)))

(defun collect-new-outer-assigns (assignments outer-assignments)
  (when (consp outer-assignments)
      (if (member (arguments (car outer-assignments))
		  assignments
		  :test #'(lambda (x y)
			    (match-update-args-prefix? y x))
		  :key #'arguments)
	  (collect-new-outer-assigns assignments
				     (cdr outer-assignments))
	  (cons (car outer-assignments)
		(collect-new-outer-assigns assignments
					   (cdr outer-assignments))))))

;;checks if assignment lhs args1 is a prefix of lhs args2 and returns
;;the remainder of args2.
(defun match-update-args-prefix? (args1 args2)
  (if (and (consp args1)
	   (consp args2)
	   (tc-eq (car args1)(car args2)))
    (match-update-args-prefix? (cdr args1)(cdr args2))
    (when (null args1) (if (null args2) T args2))))

(defmethod simplify-nested-updates
    (expr outer-assignments update-expr)
  (lcopy update-expr
    'expression expr
    'assignments outer-assignments))
  
(defmethod assert-if-inside ((expr expr))
  (assert-if expr))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-type-constraints (expr) ;(break "in-ctc")
  (when (and (not *assert-typepreds-off*)
	     (not (null *subtype-hash*))
	     (not (gethash expr *subtype-hash*)))
    (collect-type-constraints* expr)))

(defun collect-type-constraints* (ex)
  (type-constraints ex t))

;;NSH(7.11.94): old code triggered a loop since collect-type-constraints
;;calls substit which calls pseudo-normalize which calls
;;collect-type-constraints.  Probably should turn off collect-type-preds
;;for pseudo-normalize.
(defun record-type-constraints (expr)
  (unless (or *assert-typepreds-off*
	      (connective-occurs? expr))
    (let ((constraints (collect-type-constraints expr)))
      (when (and *subtype-hash* constraints)
	(setq *assert-typepreds* (nconc constraints *assert-typepreds*))
	(setf (gethash expr *subtype-hash*) T)))))


(defmethod assert-if :around ((expr expr))
	   (record-type-constraints expr)
	   (call-next-method))

(defun rewrite-declaration (expr)
  (if (constant? expr) (declaration expr) expr))


(defmethod assert-if ((expr name-expr))
  (cond ((tc-eq (type expr) *boolean*)
	 (if (or (tc-eq expr *true*) (tc-eq expr *false*))
	     (values 'X expr)
	     (let ((result
		    (assert-test expr)))
	       (cond ((true-p result)
		      (values-assert-if '? *true* expr))
		     ((false-p result)
		      (values-assert-if '? *false* expr))
		     (t (do-auto-rewrite expr 'X))))))
	((and (not (memq *assert-flag* '(record simplify)))
	      (gethash (declaration expr) *auto-rewrites-ops*))
	 (do-auto-rewrite expr 'X))
	(t (values 'X expr))))

(defun assert-then-else (test expr condition-flg)
  ;;condition-flg indicates whether test is assertable.
 (multiple-value-bind
       (sigthen newthen)
     ;;(progn (format T "~% Assuming test ~a true." test))
	    (cond-assert-if (then-part expr)
			    (if  condition-flg
				 (and+ test)
				 nil))
   (multiple-value-bind
	 (sigelse newelse)
       ;(progn (format T "~%Assuming test ~a false." test))
	      (cond-assert-if (else-part expr)
			      (if  (and condition-flg
					(not (cond-expr? expr))) ;NSH(4.7.96)
				   (and+ (negate test))
				   nil))
     (cond ((tc-eq newthen newelse) (values '? newthen))
	   ((and (tc-eq test (condition expr)) ;;sigtest is irrelevant
		 (eq sigthen 'X)(eq sigelse 'X))
	    (values 'X expr))
	   (t (values '? (copy expr
			       'argument (make!-arg-tuple-expr
					  test newthen newelse))))))))


(defmethod assert-if ((expr assignment))
  (multiple-value-bind
	(asig newarguments)
      (assert-if (arguments expr))
    (multiple-value-bind
	  (sig newexpression)
	(assert-if (expression expr))
      (if (or (eq asig '?)(eq sig '?))
	  (values '?
		  (lcopy expr 'arguments
			 (if (eq asig '?) newarguments
			     (arguments expr))
			 'expression
			 (if (eq sig '?)
			     newexpression
			     (expression expr))))
	  (values 'X expr)))))

(defun assert-if-simplify (expr)
  (let ((*top-assert-flag* NIL))
  (multiple-value-bind (sig newexpr)
      (assert-if expr)
    (if (eq sig 'X) expr newexpr))))

(defun assert-if-simp (expr)
  (let ((*top-assert-flag* NIL))
      (assert-if expr)))

(defmethod assert-if ((expr branch))  ;;;change to rec. branch?
  (if (eq *assert-flag* 'rewrite)
      (call-next-method expr)
      (let ((newtest (nth-value 1 (assert-if-simp (condition expr)))))
	(cond ((tc-eq newtest *true*)
	       (let ((newthen (nth-value 1 (assert-if (then-part expr)))))
		 (values-assert-if '? newthen expr)))
	      ((tc-eq newtest *false*)
	       (let ((newelse (nth-value 1 (assert-if (else-part expr)))))
		 (values-assert-if '? newelse expr)))
	      ;;DAC(6.23.94) The typereds of expressions in the
	      ;;then and else partmay not be valid in the top level
	      ;;context and thus should not be asserted.
	      ;;NSH(7.25.94): The above change should be rolled back
	      ;;once the typechecker becomes consistent about type
	      ;;assignment. 
	      (t (let ((*assert-typepreds-off* t))
		   (assert-then-else newtest expr T)))))))


(defun find-selection (constructor-id args cases-expr)
  (let ((selection
	 (find constructor-id (selections cases-expr)
	       :test #'(lambda (x y) (eq x (id (constructor y)))))))
    (cond ((null selection)
	   (else-part cases-expr))
	  (t (substit (expression selection)
		      (pvs-pairlis (args selection)
			       args))))))

(defun get-adt (type)
  (cond ((adt? type) type)
	((typep type 'subtype) (get-adt (supertype type)))))

(defun values-assert-if (sig result input)
  (declare (ignore input))
  (values sig result))

(defun lazy-assert-cases (sig expression case-expr)
    (let* ((all-result (check-all-recognizers expression))
         (selections (selections case-expr))
         (select (loop for sel in selections
                   thereis
                    (let ((check
                           (check-some-recognizer
                             (recognizer (constructor sel))
                             all-result)))
                      (when (true-p check) sel)))))
   (cond ((null select)
	  (if (else-part case-expr)
	      (if (loop for sel in selections
			always (false-p (check-some-recognizer
					 (recognizer (constructor sel))
					 all-result)))
		  (values-assert-if '? (else-part case-expr)
				    case-expr)
		  (values-assert-if sig
				    (lcopy case-expr
				      'expression expression)
				    case-expr))
	      (values-assert-if sig
		      (lcopy case-expr
			'expression expression)
		      case-expr)))
         ((and (name-expr? expression)(constructor? expression))
          (values-assert-if '? (expression select) case-expr))
         ((and (application? expression)(constructor? (operator expression)))
          (values-assert-if '?
			    (substit (expression select)
			      (pvs-pairlis (args select)
					   (arguments expression)))
			    case-expr))
         (t (values-assert-if '?
			      (subst-accessors-in-selection
			       expression select)
			      case-expr)))))

(defmethod assert-if ((expr cases-expr))
  (with-slots (expression selections else-part)
      expr
    (multiple-value-bind (sigexpr newexpr)
	(cond-assert-if expression)
      (let ((expression (if (eq sigexpr '?) newexpr expression)))
;	(cond ((eq *assert-flag* 'rewrite)
;	       (if (eq sigexpr '?)
;		   (values-assert-if
;		    '?
;		    (lcopy expr
;		      'expression newexpr
;		      )
;		    expr)
;		   (values 'X expr)))
;	      (t)
	(multiple-value-bind (sig selected-expr)
	    (if (eq *assert-flag* 'rewrite)
		(values 'X nil)
		(assert-cases expression expr)) ; ((selections expr))
					;((else-part expr) T)
	  (if (eq sig 'X)
	      (if *cases-rewrite*
		  (multiple-value-bind (sigsel newselections)
		      (assert-if (selections expr))
		    (multiple-value-bind (sigelse newelse)
			(assert-if (else-part expr))
		      (if (memq '? (list sigexpr sigsel sigelse))
			  (values '?
				  (lcopy expr
				    'expression newexpr
				    'selections newselections
				    'else-part newelse))
			  (values 'X expr))))
		  (if (eq sigexpr 'X)
		      (values 'X expr)
		      (values-assert-if
		       '? (lcopy expr
			    'expression newexpr)
		       expr)))
	      (values-assert-if '?
				selected-expr expr)))))))

(defmethod assert-if ((expr selection))
  (multiple-value-bind (sig val)
      (assert-if (expression expr))
    (if (eq sig '?)(values-assert-if
		    '? (lcopy expr 'expression val) expr)
	(values 'X expr))))


(defun assert-subgoal (expr)
  (multiple-value-bind (sig newexpr)
      (assert-if expr)
    (if (eq sig 'X)(values '? expr)
	(values '? newexpr))))

(defun assert-cases (expression case-expr)
  (let* ((all-result (check-all-recognizers expression))
         (selections (selections case-expr))
         (select (loop for sel in selections
                   thereis
                    (let ((check
                           (check-some-recognizer
                             (recognizer (constructor sel))
                             all-result)))
                      (when (true-p check) sel)))));;(break "cases")
   (cond ((null select)
	  (if (else-part case-expr)
	      (if (loop for sel in selections
			always (false-p
				(check-some-recognizer
				 (recognizer (constructor sel))
				 all-result)))
		  (assert-subgoal (else-part case-expr))
		  (values 'X case-expr))
	      (values 'X case-expr)))
         ((and (name-expr? expression)(constructor? expression))
          (assert-subgoal (expression select)))
         ((and (application? expression)(constructor? (operator expression)))
          (assert-subgoal (substit (expression select)
                                (pvs-pairlis (args select)
                                             (arguments expression)))))
         (t (assert-subgoal (subst-accessors-in-selection expression
							  select)))))) 

;;pvs-pairlis is careful to treat tuples as lists in pairing
;;formals and actuals. 
(defun pvs-pairlis (list1 list2)
  (if (and (singleton? list1)
	   (typep (find-supertype (type (car list1))) 'tupletype)
	   (not (singleton? list2)))
      (list (cons (car list1)
		  (make!-tuple-expr list2 (type (car list1)))))
      (if (and (not (singleton? list1))
	       (singleton? list2)
	       (typep (find-supertype (type (car list2)))
		      'tupletype))
	  (if (typep (car list2) 'tuple-expr)
	      (pairlis list1 (exprs (car list2)))
	      (pairlis list1 (make!-projections (car list2))))
	  (pairlis list1 list2))))
      

;;; SO 9/2/94 - changed record-redex? to handle field-applications

(defmethod record-redex? (expr)
  (declare (ignore expr))
  nil)

(defmethod record-redex? ((expr field-application))
  (typep (argument expr) 'record-expr))


(defun function-update-redex? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'update-expr)))
       ;;I'm  making use of the fact that an operator
       ;;must be a function and not a record.

(defun simplify-function-update-redex (expr &optional in-beta-reduce?)
  (let* ((op (operator expr))
	 (args (arguments expr))
	 (fun (expression op))
	 (updates (reverse (assignments op))))
    (reduce-update expr fun       ;;NSH(8.23.94)
		   (check-update-args*  updates args
				       in-beta-reduce?)
		   args
		   in-beta-reduce? NIL)))

(defun check-update-args* (updates args in-beta-reduce?)
  ;;returns a list of matching updates, or 'NOIDEA if there
  ;;is an assignment that does resolve to TRUE or FALSE.
  (if (null updates)
      NIL
      (let* ((update-args (arguments (car updates)))
	     ;;(update-expr (expression (car updates)))
	     (first (check-update-args (car update-args)
				      args
				      in-beta-reduce?)))
	(if (eq first 'NOIDEA)
	    'NOIDEA
	    (if (false-p first)
		(check-update-args* (cdr updates)
				    args
				    in-beta-reduce?)
		(if (singleton?  update-args)
		    (list (car updates))
		    (let ((rest (check-update-args* (cdr updates)
						    args
						    in-beta-reduce?)))
		      (if (eq rest 'NOIDEA)
			  'NOIDEA
			  (cons (car updates)
				rest)))))))))

;;;Given that updates comes from check-update-args*.
(defun reduce-update (redex expr updates args in-beta-reduce?
			    new-updates)
  ;;reduce-update constructs the reduced form of the given
  ;;update redex.
  (let ((*generate-tccs* 'NONE)));;NSH(9.15.94): prevents TCCS.
    (if (eq updates 'NOIDEA)
	redex
	(if (null updates)
	    (let* ((newexpr (make!-application* expr args))
		   (newexpr
		    (if in-beta-reduce?
			(beta-reduce newexpr)
			(multiple-value-bind (sig value)
			    (assert-if-application
			     newexpr expr
			     (make!-arg-tuple-expr* args) '?)
			  (if (eq sig '?) value newexpr)))))
	      (if new-updates
		  (make!-update-expr newexpr new-updates)
		  newexpr))
	    (let ((update-expr (expression (car updates)))
		  (update-args (arguments (car updates))))
	      (if (singleton? update-args)
		  (if new-updates
		      (make!-update-expr
		       update-expr
		       new-updates)
		      update-expr)
		  (reduce-update redex expr (cdr updates)
				 args in-beta-reduce?
				 (cons (lcopy (car updates)
					 'arguments
					 (cdr update-args))
				       new-updates)))))))
		       

(defun scalar? (expr)
  (and (enum-adt? (find-supertype (type expr))) expr))

(defun scalar-constant? (x)
  (and (constructor? x)(scalar? x)))			 

(defun check-update-args (update-args args &optional in-beta-reduce?)
  (if (null update-args) 'TRUE
      (let* ((uarg1 (car update-args))
	     (arg1 (car args))
	     (equality (make!-equation uarg1 arg1))
	     (result
	      (if in-beta-reduce?
		  (if (tc-eq uarg1 arg1)
		      'TRUE
		      (if (or (and (number-expr? uarg1)
				   (number-expr? arg1))
			      (and (scalar-constant? uarg1)
				   (scalar-constant? arg1)))
			  'FALSE
			  'NOIDEA))
		  (if (connective-occurs? equality)
		      'NOIDEA
		      (let ((newresult (nth-value 1
					 (assert-equality
					  equality (list uarg1 arg1) 'X))))
			(assert-test newresult))))))
	(cond ((true-p result)
	       (check-update-args (cdr update-args) (cdr args)))
	      ((false-p result) 'FALSE)
	      (t 'NOIDEA)))))

(defmethod assert-if ((expr tuple-expr))
  (multiple-value-bind (sig newexprs)
      (assert-if (exprs expr))
    (if (eq sig '?)
	(do-auto-rewrite (lcopy expr 'exprs newexprs) '?)
	(do-auto-rewrite expr 'X))))
	    
	
(defmethod assert-if ((expr record-expr))
  (multiple-value-bind  (sig newassign)
	(assert-if (assignments expr))
    (if (eq sig '?)
	(do-auto-rewrite (lcopy expr 'assignments newassign) '?)
	(do-auto-rewrite expr 'X))))
       
(defmethod record-update-redex? (expr)
  (declare (ignore expr))
  nil)

(defmethod record-update-redex? ((expr field-application))
  (with-slots (argument) expr
    (typep argument 'update-expr)))


(defun make-record-update-reduced-application (op arg)
  (let* ((new-application (make!-field-application op arg)))
    (if (record-update-redex? new-application)
	(nth-value 1 (record-update-reduce new-application op arg))
	new-application)))


(defun record-update-reduce (expr op arg);;NSH(7.15.94):removed sig
  ;;expr applies record-access to record-update: a(exp WITH [..])
  ;;new-application ensures that any newly created redexes are reduced.
  (declare (ignore expr))
  (let ((updates
	 (loop for assn in (assignments arg)
	       when (eq op (id (caar (arguments assn))))
	       collect assn))
	(expr-of-arg (expression arg)))
    (if updates
	(if (every #'(lambda (x) (cdr (arguments x)))
		   updates) ;;;a curried update::
	                             ;;;a(exp WITH [((a)(i)):= e])
	    (let ((newexpr;;NSH(9.15.94): otherwise TCCs
		   ;;are generated when domain is subtype.
		   ;;(let ((*generate-tccs* 'NONE)))
		   (make!-update-expr
		    (make-record-update-reduced-application
		     op expr-of-arg)
		    (mapcar #'(lambda (x)
				(lcopy x 'arguments
				       (cdr (arguments x))))
		      updates))))
	      (do-auto-rewrite
	       newexpr
	       '?));;return a(exp) WITH [(i) := e] simplified.
	    (make-update-expr-from-updates
	     updates))
	(do-auto-rewrite  (make-record-update-reduced-application
			   op expr-of-arg)
			  '?))))

;;assumes that at least one update has an empty cdr arguments
;;invoked from record-update-reduce.  Makes sure that all
;;previous updates from last single update are irrelevant.
;;NSH(10.10.07) moved values '? in here and added do-auto-rewrite
(defun make-update-expr-from-updates (updates)
  (if (and (consp updates)
	   (null (cdr (arguments (car updates))))
	   (every #'(lambda (x) (cdr (arguments x)))
		  (cdr updates)))
      (if (null (cdr updates))
	  (values '? (expression (car updates)))
	  (do-auto-rewrite
	   (make!-update-expr
	    (expression (car updates))
	    (mapcar #'(lambda (x) (lcopy x 'arguments
					 (cdr (arguments x))))
	      (cdr updates)))
	   '?))
      (make-update-expr-from-updates (cdr updates))))

(defun is-predicate? (expr)
  (predtype? (type expr)))


(defmethod arity ((expr expr))
  (arity (type expr)))

;;; SO 7/19/94 - modified for new form of domain.
(defmethod arity ((te funtype))
  (with-slots (domain) te
    (arity* domain)))

(defmethod arity* ((te dep-binding))
  (with-slots (type) te
    (arity* type)))

(defmethod arity* ((te tupletype))
  (with-slots (types) te
    (length types)))

(defmethod arity* ((te type-expr))
  1)

(defmethod arith ((expr T))
  0)

(defun is-plus? (op)
  (and (typep op 'name-expr)
       (interpreted? op)
       (eq (id op) '+)))

(defun is-minus? (op)
  (and (typep op 'name-expr)
       (interpreted? op)
       (eq (id op) '-)))

(defun is-sub-minus? (op)
  (and (is-minus? op)
       (equal (arity op) 2)))

(defun is-mult? (op)
  (and (typep op 'name-expr)
       (interpreted? op)
       (eq (id op) '*)))

(defun is-div? (op)
  (and (typep op 'name-expr)
       (interpreted? op)
       (eq (id op) '/)))

(defmethod is-division? ((expr application))
  (with-slots (operator)
      expr
    (and (is-div? operator)
	 (= (length (arguments expr)) 2))))

(defmethod is-division? ((expr T))
  nil)

(defmethod is-addition? ((expr application))
  (with-slots (operator)
      expr
    (and (is-plus? operator)
	 (= (length (arguments expr)) 2))))

(defmethod is-addition? ((expr T))
  nil)

(defmethod is-subtraction? ((expr application))
  (with-slots ((op operator)(arg argument))
      expr
    (and (is-minus? op)
	 (tuple-expr? arg)
	 (= (length (exprs arg)) 2))))

(defmethod is-subtraction? ((expr T))
  nil)

(defmethod is-multiplication? ((expr application))
  (with-slots (operator)
      expr
    (and (is-mult? operator)
	 (= (length (arguments expr)) 2))))

(defmethod is-multiplication? ((expr T))
  nil)

(defun norm-addition (expr)
  (if (or (is-addition? expr)(is-subtraction? expr))
      (simplify-addition expr)
      expr))

;;assumes that expr is an addition.  newargs is for use from
;;assert-if-application
(defun simplify-addition (expr &optional newargs)
  (let* ((hashvalue (gethash expr *assert-if-arith-hash*))
	 (msum (or hashvalue
		   (get-merged-sum expr newargs))));;(break "simp-add")
    (unless hashvalue
      (setf (gethash expr *assert-if-arith-hash*)
	    msum)
      (setf (gethash msum *assert-if-arith-hash*)
	    msum))
    msum))

(defun get-merged-sum (expr newargs)
  (let* ((nargs (if newargs
		    (argument-list newargs)
		    (arguments expr)))
	 (lhs (car nargs))
	 (rhs (cadr nargs))
	 (lsums (addends lhs))
	 (rsums (if (is-addition? expr)
		    (addends rhs)
		    (mapcar #'make-minus (addends rhs))))
	 (msum (mergesums lsums rsums *real*)))
    (if (tc-eq msum expr) expr msum)))


(defun assert-if-addition  (expr newargs sig);;expr must be
					     ;;addition/subtraction.
  (let* ((hashvalue (gethash expr *assert-if-arith-hash*))
	 (msum (if hashvalue hashvalue
		   (let* ((nargs (argument-list newargs))
			  (lhs (car nargs))
			  (rhs (cadr nargs))
			  (lsums (addends lhs))
			  (rsums (if (is-addition? expr)
				     (addends rhs)
				     (mapcar #'make-minus (addends rhs))))
			  ;;SO 2/7/93 - make the type integer (at least) if not addition
			  (ctype (compatible-type
				  (or (car (types expr)) (type expr))
				  (compatible-type
				   (or (car (types lhs)) (type lhs))
				   (or (car (types rhs)) (type rhs)))))
			  (type (if (or (null *integer*)
					(is-addition? expr))
				    ctype
				    (compatible-type ctype *integer*))))
		     (if (subtype-of? type *real*)
			 (mergesums lsums rsums type)
			 (mergesums lsums rsums *real*))))))
;    (when hashvalue (format t "~%arith"))
    (unless hashvalue
      (setf (gethash expr *assert-if-arith-hash*)
	    msum)
      (setf (gethash msum *assert-if-arith-hash*)
	    msum))
    (if (tc-eq msum expr)
	(do-auto-rewrite expr sig)
	(do-auto-rewrite msum '?))))

(defun make-minus (expr)
  (let* ((coef (coefficient expr))
	 (body (noncoefficient expr))
	 (newcoeffexpr (if (minusp coef)
			   (make!-number-expr (- coef))
			   (make!-minus (make!-number-expr coef)))))
    (if (null body)
	newcoeffexpr
	(make-prod (list newcoeffexpr body)
		   (compatible-type (type newcoeffexpr) (type body))))))

(defun addends (expr)
  (multiple-value-bind (pos neg)
      (addends* expr)
    (nconc pos (mapcar #'make-minus neg))))

(defun addends* (expr)
  (if (typep expr 'application)
      (if (is-addition? expr)
	  (multiple-value-bind (pos1 neg1)
	      (addends* (args1 expr))
	    (multiple-value-bind (pos2 neg2)
		(addends* (args2 expr))
	      (values (nconc pos1 pos2)(nconc neg1 neg2))))
	  ;; SO - 5/11/93 changed is-minus? to is-subtraction?
	  ;;NSH - 5/25/93 changed is-subtraction? to is-sub-minus?
	  (if (is-subtraction? expr)
	      (multiple-value-bind (pos1 neg1)
		  (addends* (args1 expr))
		(multiple-value-bind (pos2 neg2)
		    (addends* (args2 expr))
		  (values (nconc pos1 neg2)(nconc neg1 pos2))))
	      (list expr)))
      (list expr)))

(defun multiplicands (expr)
  (with-slots ((op operator)(arg argument))
      expr
  (if (and (typep expr 'application)
	   (is-mult? op))
      (multiplicands*
       (if (tuple-expr? arg)
	   (exprs arg)
	   (list arg)))
      (list expr))))

(defun multiplicands* (expr-list)
  (if (consp expr-list)
      (nconc (multiplicands (car expr-list))
	     (multiplicands* (cdr expr-list)))
      nil))

(defun make-sum (list type)
  (if (connective-occurs? list)
      (make-sum* list type)
      (let ((*newdc* nil))
	(make-sum* (sort list #'(lambda (x y)
				  (arithord (top-translate-to-prove x)
					    (top-translate-to-prove y))))
		   type))))

(defun make-sum* (list type)
  (cond ((null list) (make!-number-expr 0))
	((null (cdr list)) (car list))
	(t 
	 (let* ((A1 (car list))
		(A2 (cadr list))
		(E1 (cond ((<  (coefficient A2) 0)
			   (make!-difference A1 (make-minus A2)))
			  ((<  (coefficient A1) 0)
			   (make!-difference A2 (make-minus A1)))
			  ((eql (coefficient A2) 0) A1)
			  ((eql (coefficient A1) 0) A2)
			  (t (make!-plus A1 A2)))))
	   (make-sum* (cons E1 (cddr list)) type)))))

(defun make-prod (list type)
  (let ((*newdc* nil))
    (make-prod* (sort list #'(lambda (x y)
			       (arithord (top-translate-to-prove x)
					 (top-translate-to-prove y))))
		type)))

(defun arith-ord-translate (x y)
  (arithord (top-translate-to-prove x)
	    (top-translate-to-prove y)))

(defun make-prod* (list type)
  (cond ((null list) (make!-number-expr 1))
	((null (cdr list)) (car list))
	((or (eql (coefficient (car list)) 0)
	     (eql (coefficient (cadr list)) 0))
	 (make!-number-expr 0))
	(t (let* ((A1 (car list))
		  (A2 (cadr list))
		  (prod (* (coefficient A1) (coefficient A2)))
		  (coeff (if (minusp prod)
			     (make!-minus (make!-number-expr (- prod)))
			     (make!-number-expr prod)))
		  (A1B (noncoefficient A1))
		  (A2B (noncoefficient A2))
		  (body (if (null A1B)
			    A2B
			    (if (null A2B)
				A1B
				(make!-times A1B A2B))))
		      (E (if (null body) coeff
			     (if (and (typep coeff 'number-expr)
				      (eql (number coeff) 1))
				 body
				 (make!-times coeff body)))))
	     (make-prod* (cons E (cddr list)) type)))))

(defun negative-number? (expr)
  (and (typep  expr 'unary-application)
       (is-minus? (operator  expr))
       (typep (args1  expr) 'number-expr)))

(defun coefficient (expr)
  (if (is-multiplication? expr)
      (if (typep (args1 expr) 'number-expr)
	  (number (args1 expr))
	  (if (negative-number? (args1 expr))
	      (- 0 (number (args1 (args1 expr))))
	      1))
      (if (typep expr 'number-expr)
	  (number expr)
	  (if (negative-number? expr)
	      (- 0 (number (args1 expr)))
	      1))))

(defun noncoefficient (expr)
  (if (and (is-multiplication? expr)
	   (or (typep (args1 expr) 'number-expr)
	       (negative-number? (args1 expr))))
      (args2 expr)
      (if (or (typep expr 'number-expr)
	      (negative-number? expr))
	  nil
	  expr)))

(defun mergesums* (list1 list2 type)
  (cond ((null list1) list2)
	((null list2) list1)
	(t (let* ((L1 (car list1))
		  (L1C (coefficient L1))
		  (L1B (noncoefficient L1))
		  (L1Blist2 (loop for exp in list2
				  sum
				  (if (tc-eq L1B (noncoefficient exp))
				      (coefficient exp)
				      0)))
		  (newlist2 (loop for exp in list2
				  when (not (tc-eq L1B (noncoefficient exp)))
				  collect exp))
		  (newcoeff (+ L1C L1Blist2))
		  (ncoeff (if (minusp newcoeff)
			      (make!-minus (make!-number-expr (- newcoeff)))
			      (make!-number-expr newcoeff)))
		  (newterm (if (null L1B)
			       ncoeff
			       (if (eql newcoeff 1)
				   L1B
				   (make-prod (list ncoeff L1B)
					      type))))
		  (restlist (if (or (cdr list1) newlist2)
				(mergesums* (cdr list1) newlist2 type)
				nil)));;NSH(11.14.95)
	     ;;was (list (make-number-expr 0))
	     (if (equal newcoeff 0)
		 restlist 
		 (cons newterm restlist))))))

(defun mergesums (list1 list2 type)
  (make-sum (mergesums* list1 list2 type) type))


(defun merge-products (L R type)
  (cond ((or (is-addition? L)
	     (is-subtraction? L))
	 (let ((list (loop for x in (addends L)
			  collect
			  (merge-products x R type))))
	   (make-sum list type)))
	((or (is-addition? R)
	     (is-subtraction? R))
	 (let ((list (loop for x in (addends R)
			  collect
			  (merge-products L x type))))
	   (make-sum list type)))
	(t (let* ((Lcoeff (coefficient L))
		  (LB (noncoefficient L))
		  (Rcoeff (coefficient R))
		  (RB (noncoefficient R))
		  (newcoeff (* Lcoeff Rcoeff))
		  (prodlist (if LB (if RB (append (multiplicands LB)
						  (multiplicands RB))
				       (multiplicands LB))
				(if RB (multiplicands RB) nil))))
	     (if (eql newcoeff 0)
		 (make!-number-expr 0)
		 (if (eql newcoeff 1)
		     (make-prod prodlist
				type)
		     (make-prod (list (if (minusp newcoeff)
					  (make!-minus (make!-number-expr
							(- newcoeff)))
					  (make!-number-expr newcoeff))
				      (make-prod prodlist type))
				type)))))))
		    

;;; SO 8-18-94 - Fixed for new form of application
(defun assert-if-multiplication (expr newargs sig)
  (let* ((hashvalue (gethash expr *assert-if-arith-hash*))
	 (prod (if hashvalue hashvalue
		   (let* ((nargs (argument-list newargs))
			  (lhs (car nargs))
			  (rhs (cadr nargs))
			  (type (compatible-type (type lhs)(type rhs))))
		     (if (or (is-multiplication? lhs)
			     (is-addition? lhs)
			     (is-subtraction? lhs)
			     (loop for x in (multiplicands rhs)
				   thereis (or (is-addition? x)
					       (is-subtraction? x)
					       (typep x 'number-expr))))
			 (merge-products lhs rhs type)
			 expr)))))
;    (when hashvalue (format t "~%arith"))
    (unless hashvalue
      (setf (gethash expr *assert-if-arith-hash*) prod)
      (setf (gethash prod *assert-if-arith-hash*) prod))
    (if (tc-eq prod expr)
	(do-auto-rewrite expr sig)
	(do-auto-rewrite prod '?))))




;(defmethod compare-expr ((L number-expr) (R number-expr))
;  (if (< (number L)(number R)) '<
;      (if (eql (number L)(number R)) '=
;	  '>)))
;
;(defmethod compare-expr ((L name) (R name)))
;
;
;(defmethod compare-expr ((L list)(R list))
;  (if (null L)
;      (if (null R) '=  '<)
;      (if (null R) '>
;	  (let ((x (compare-expr (car L)(car R))))
;	    (if (eq x '=)
;		(compare-expr (cdr L)(cdr R))
;		x)))))
;
;(defmethod compare-expr ((L tuple-expr) (R tuple-expr))
;  (compare-expr (exprs L)(exprs R)))
;
;(defmethod compare-expr ((L record-expr)(R record-expr))
;  (compare-expr (assignments L)(assignments R)))
;
;(defmethod compare-expr ((L update-expr)(R update-expr))
;  (let ((x (compare-expr (expression L)(expression R))))
;    (if (eq x '=)
;	(compare-expr (assignments L)(assignments R))
;	x)))
;
;
;(defmethod compare-expr ((L binding-expr)(R binding-expr))
;  (let ((x (compare-binding-op L R)))
;    (if (eq x '=)
;	(let ((y (compare-expr (bindings L)(bindings R))))
;	  (if (eq y '=)
;	      (compare-expr (expression L)(expression R))
;	      y))
;	x)))
			    
	
(defun cancel-terms (lhs-terms rhs-terms)
  (cancel-terms* lhs-terms rhs-terms NIL 'X))

(defun cancel-terms* (lhs* rhs* lhs-accum sig)
  (if (null lhs*)
      (values sig (nreverse lhs-accum) rhs*)
      (let ((rhs-match
	     (find (car lhs*) rhs*
		   :test #'tc-eq)) ;#'(lambda (x y)
				   ;(true-p (assert-test (make-equality x y))))
	    )
	(if rhs-match
	    (cancel-terms* (cdr lhs*)
			   (remove rhs-match rhs*
				   :count 1);;NSH(2.21.97) 
			   lhs-accum '?)
	    (cancel-terms* (cdr lhs*)
			   rhs*
			   (cons (car lhs*) lhs-accum)
			   sig)))))
      
(defun assert-numeric-equality (expr sig)
  ;;NSH(12.1.94): assumes that equality has been simplified and
  ;;process has been unsuccessfully
  ;;applied to the given equality. 
  (let* ((lhs (args1 expr))
	 (rhs (args2 expr))
	 (lhs-addends (addends (args1 expr)))
	 (rhs-addends (addends (args2 expr))))
    (if (and (singleton? lhs-addends)
	     (singleton? rhs-addends))
	(do-auto-rewrite expr sig)
	(multiple-value-bind (newsig new-lhs-addends new-rhs-addends)
	    (cancel-terms lhs-addends rhs-addends)
	  (if (eq newsig '?)
	      (if (and (null new-lhs-addends)
		       (null new-rhs-addends))
		  (values '? *true*)
		  (do-auto-rewrite (make!-equation
				    (make-sum new-lhs-addends
					      (compatible-type (type lhs)
							       *integer*))
				    (make-sum new-rhs-addends
					      (compatible-type (type rhs)
							       *integer*)))
				   '?))
	      (do-auto-rewrite expr sig))))))

(defun assert-disequality (expr newargs sig)
  (let* ((nargs (argument-list newargs))
	(equality (make!-equation (car nargs)(cadr nargs))))
    (multiple-value-bind (sig newequality)
	(assert-equality equality newargs sig)
      (if (eq sig '?)
	  (let ((disequality (negate newequality)))
	    (assert-if-application disequality
				   (operator disequality)
				   newequality '?))
	  (do-auto-rewrite expr sig)))))

;;NSH(10.21.94)
(defun assert-equality (expr newargs sig)
  (let ((nargs (argument-list newargs)))
    (cond ((tc-eq (car nargs)(cadr nargs))
	   (values '? *true*))
;;NSH(10.21.94): separated out since this shouldn't be
;invoked from assert-if-inside.
	  ((tc-eq (find-supertype (type (car nargs)))
		  *number*)
	   (assert-numeric-equality expr sig))
	  ((and (typep (car nargs) 'tuple-expr)
		(typep (cadr nargs) 'tuple-expr))
	   (assert-tuple-equality expr))
	  ((and (typep (car nargs) 'record-expr)
		(typep (cadr nargs) 'record-expr))
	   (assert-record-equality expr))
	  (t (let ((p1 (adt-subtype? (type (args1 expr))))
		   (p2 (adt-subtype? (type (args2 expr)))))
	       (if (and p1 p2)
		   (if (not (eq (id p1) (id p2)));;NSH(12.1.95)
		       (values '? *false*)
		       (do-auto-rewrite expr sig))
		   (let* ((pred (or p1 p2))
			  (term (if p1 (args2 expr) (args1 expr)))
			  (result (when pred
				    (check-rest-recognizers
				     pred
				     (check-all-recognizers term)))))
		     (cond ((null pred)(do-auto-rewrite expr sig))
			   ((false-p result)
			    (values '? *false*))
			   ((and (eq result 'RESTFALSE)
				 (null (accessors (constructor pred))))
			    (values '? *true*))
			   (t (do-auto-rewrite expr sig))))))))))

(defun assert-if-application (expr newop newargs sig)
  (cond ((eq *assert-flag* 'rewrite) (do-auto-rewrite expr sig))
	((and (not (negation? expr))
	      (branch? newargs))
	 (let ((thenval (nth-value 1
			  (assert-if-application
			   (make!-application newop (then-part newargs))
			   newop (then-part newargs) '?)))
	       (elseval (nth-value 1
			  (assert-if-application
			   (make!-application newop (else-part newargs))
			   newop (else-part newargs) '?))))
	   (values-assert-if
	    '?
	    (make!-if-expr (condition newargs) thenval elseval)
	    expr)))
	((branch? newop)
	 (let ((thenval (nth-value 1
			  (assert-if-application
			   (make!-application (then-part newop) newargs)
			   (then-part newop) newargs '?)))
	       (elseval (nth-value 1
			  (assert-if-application
			   (make!-application (else-part newop) newargs)
			   (else-part newop) newargs '?))))
	   (values-assert-if
	    '?
	    (make!-if-expr (condition newop) thenval elseval)
	    expr)))
	((or (is-addition? expr) (is-subtraction? expr))
	 (assert-if-addition  expr newargs sig))
	((is-multiplication? expr)
	 (assert-if-multiplication expr newargs sig))
	((and (typep newop 'name-expr)
	      (accessor? newop)
	      (typep newargs 'application)
	      (member (operator newargs) (constructor newop)
		      :test #'tc-eq-ops))
	 (values-assert-if
	  '?
	  (let ((accessors (accessors (operator newargs))))
	    (if (cdr accessors)
		(nth (position newop accessors :test #'tc-eq-ops)
		     (arguments newargs))
		(argument newargs)))
	  expr))
	((lambda? newop)
	 (let ((val (nth-value 1
		      (assert-if (substit (expression newop)
				   (pairlis-args (bindings newop)
						 (argument-list newargs)))))))
	   (values-assert-if '? val expr)))
	((negation? expr)
	 (if (tc-eq newargs *true*)
	     (values '? *false*)
	     (if (tc-eq newargs *false*)
		 (values '? *true*)
		 (if (negation? newargs)
		     (values '? (args1 newargs))
		     (do-auto-rewrite expr sig)))))
	((implication? expr)
	 (let ((nargs (argument-list newargs)))
	   (cond ((or (tc-eq (car nargs) *false*)
		      (tc-eq (cadr nargs) *true*))
		  (values '? *true*))
		 ((tc-eq (car nargs) *true*)
		  (values '? (cadr nargs)))
		 ((tc-eq (cadr nargs) *false*)
		  (values '? (make!-negation (car nargs))))
		 (t (do-auto-rewrite expr sig)))))
	((conjunction? expr)
	 (let ((nargs (argument-list newargs)))
	   (cond ((tc-eq (car nargs) *true*)
		  (values '? (cadr nargs)))
		 ((or (tc-eq (car nargs) *false*)
		      (tc-eq (cadr nargs) *false*))
		  (values '? *false*))
		 ((tc-eq (cadr nargs) *true*)
		  (values '? (car nargs)))
		 (t (do-auto-rewrite expr sig)))))
	((disjunction? expr)
	 (let ((nargs (argument-list newargs)))
	   (cond ((or (tc-eq (car nargs) *true*)
		      (tc-eq (cadr nargs) *true*))
		  (values '? *true*))
		 ((tc-eq (car nargs) *false*)
		  (values '? (cadr nargs)))
		 ((tc-eq (cadr nargs) *false*)
		  (values '? (car nargs)))
		 (t (do-auto-rewrite expr sig)))))
	((iff-or-boolean-equation? expr)
	 (let* ((nargs (argument-list newargs))
		(left (car nargs))
		(right (cadr nargs)))
	   (cond ((tc-eq left *true*)
		  (values '? right))
		 ((tc-eq right *true*)
		  (values '? left))
		 ((tc-eq left *false*) ;;NSH(1.20.96) added
		  (values '? (negate right)))
		 ((tc-eq right *false*)
		  (values '? (negate left)))
		 ((tc-eq (car nargs)(cadr nargs))
		  (values '? *true*))
		 (t (do-auto-rewrite expr sig)))))
	((equation? expr)
	 (assert-equality expr newargs sig)) ;;NSH(8.9.95) newop gone
	((disequation? expr)
	 (assert-disequality expr newargs sig))
	((function-update-redex? expr)
	 (let ((result
		(simplify-function-update-redex expr)))
	   (if (tc-eq result expr)
	       (do-auto-rewrite expr sig)
	       (values '? result))))
	(t (do-auto-rewrite expr sig))))

(defmethod assert-if ((expr projection-application))
  (with-slots (index argument) expr
    (multiple-value-bind (sig newarg)
	(assert-if argument)
      (multiple-value-bind (newsig newexpr)
	  (reduce-proj-application sig newarg index expr)
	;;NSH(11.22.94)
	(if (and (not (connective-occurs? newexpr))
		 ;;*boolean-context*
		 (tc-eq (find-supertype (type newexpr))
			*boolean*))
		(let ((result (assert-test newexpr)));;NSH(11.18.94)
		  (if (false-p result)
		      (values-assert-if '? *false* newexpr)
		      (if (true-p result)
			  (values-assert-if '? *true* newexpr)
			  (do-auto-rewrite newexpr
					   sig))))
		(values newsig newexpr))))))

(defun reduce-proj-application (sig newarg index &optional expr)
  ;;expr is only given at the top-level call.
  (cond ((typep newarg 'tuple-expr)
	 (values
	  '? (nth (1- index) (exprs newarg))))
	((typep newarg 'update-expr)
	 (tuple-update-reduce index newarg))
	((branch? newarg)
	 (let* ((thenval (nth-value 1
			   (reduce-proj-application
			    '? (then-part newarg) index)))
		(elseval (nth-value 1
			   (reduce-proj-application
			    '? (else-part newarg) index)))
		(new-expr (make!-if-expr (condition newarg) thenval elseval)))
	   (if expr
	       (values-assert-if '? new-expr expr)
	       (values '? new-expr))))
	(t (let ((new-expr (if expr 
			       (lcopy expr 'argument newarg)
			       (make!-projection-application index newarg))))
	     (do-auto-rewrite new-expr sig)))))

(defun tuple-update-reduce (index arg)
  (let ((updates (loop for assn in (assignments arg)
		 when (eql index (number (caar (arguments assn))))
		 collect assn))
	(expr-of-arg (expression arg)));(break)
    (if updates
	(if (every #'(lambda (x) (cdr (arguments x)))
		     updates) ;;;a curried update::
	                             ;;;PROJ_n(exp WITH [(n)(i):= e])
	    (let ((newexpr (make!-update-expr
			    (make-tuple-update-reduced-projection
			     index expr-of-arg)
			    (mapcar #'(lambda (x)
				    (lcopy x 'arguments
					   (cdr (arguments x))))
			      updates)
;			    (list (lcopy update 'arguments
;					 (cdr (arguments update)))
			    ;;(type (make!-projection-application index arg))
			    )))
	      (do-auto-rewrite
	       newexpr
	       '?));;return a(exp) WITH [(i) := e] simplified.
	    (make-update-expr-from-updates updates))
	(do-auto-rewrite (make-tuple-update-reduced-projection
			  index expr-of-arg)
			 '?))))

(defun make-tuple-update-reduced-projection (index expr)
  (let ((new-application (make!-projection-application index expr)))
    (if (typep expr 'update-expr)
	(nth-value 1 (tuple-update-reduce index expr))
	new-application)))
  

;;NSH(9.14.94): updated assert-if(projection/field-application) to
;;distribute through if-then-else. 
(defmethod assert-if ((expr field-application))
  (with-slots (id argument) expr
    (multiple-value-bind (sig newarg)
	(assert-if argument)
      (multiple-value-bind
	  (newsig newexpr)
	  (reduce-field-application sig newarg id expr)
	(cond
	 ((and (not (connective-occurs? newexpr))
	       ;;*boolean-context*
	       (tc-eq (find-supertype (type newexpr)) *boolean*))
	  (let ((result (assert-test newexpr)));;NSH(11.18.94)
	    (if (false-p result)
		(values-assert-if '? *false* newexpr)
	      (if (true-p result)
		  (values-assert-if '? *true* newexpr)
		(do-auto-rewrite newexpr
				 newsig)))))
	 (t (values newsig newexpr)))))))

(defun reduce-field-application (sig newarg id &optional expr)
  (let ((newexpr (if expr
		     (lcopy expr 'argument newarg)
		     (make!-field-application id newarg))))
    (cond ((record-redex? newexpr)
	   (values-assert-if
	    '?
	    (expression
	     (find id (assignments newarg)
		   :test #'(lambda (x y)
			     (eq x (id (caar (arguments y)))))))
	    newexpr))
	  ((record-update-redex? newexpr)
	   (record-update-reduce newexpr id newarg))
	  ((branch? newarg)
	   (let* ((thenval (nth-value 1 (reduce-field-application
					 '? (then-part newarg) id)))
		  (elseval (nth-value 1 (reduce-field-application
					 '? (else-part newarg) id)))
		  (newexpr (make!-if-expr (condition newarg) thenval elseval)))
	     (if expr
		 (values-assert-if '? newexpr expr)
		 (values '? newexpr))))
	  (t (do-auto-rewrite newexpr sig)))))

(defun assert-if-arg (expr)
  ;;called from assert-if(application)
  ;;expr is the original expr
  (let ((arg (argument expr)))
    (cond ((or (implication? expr)
	       (conjunction? expr)
	       (disjunction? expr))
	   (let* ((arg1 (args1 expr))
		  (arg2 (args2 expr)))
	     (multiple-value-bind
		 (sig1 new-arg1)
		 (assert-if arg1)
	       (multiple-value-bind
		   (sig2 new-arg2)
		   (let ((*assert-typepreds-off* T))
		     (assert-if arg2))
		 (if (memq '? (list sig1 sig2))
		     (do-auto-rewrite
		      (lcopy arg
			'exprs (list new-arg1 new-arg2))
		      '?)
		     (do-auto-rewrite arg 'X))))))
	  (t (assert-if (argument expr))))))

(defmethod assert-if ((expr application)) ; (break "assert-if-ap")
  (with-slots (operator argument) expr
    (multiple-value-bind
	(sigop newop)
	(if (and (lambda? operator)
		 (not (eq *assert-flag* 'rewrite)))
	    (values 'X operator)
	    (assert-if operator))
      (multiple-value-bind (sigargs newargs)
	  (assert-if (argument expr))
	(let* ((sig (if (eq sigop '?) '? sigargs))
	       (expr;;shadowing expr
		(lcopy expr
		  'operator (if (eq sigop '?) newop
				(operator expr))
		  'argument (if (eq sigargs '?) newargs
				(argument expr))))
	       (result			;(nil)
		(when (and (not (connective-occurs? expr))
			   (tc-eq (type expr) *boolean*)
			   (not (eq *top-assert-flag* 'rewrite)))
		  (assert-test expr))
		))			;(break "assert-if-ap2")
	  (cond ((true-p result) (values '? *true*))
		((false-p result) (values '? *false*))
		((and (is-predicate? newop)
		      (adt? (find-supertype (type newargs)))
		      (typep newop 'name-expr)
		      (recognizer? newop))
		 (let ((result (check-other-recognizers
				newop newargs)))
		   (if (false-p result)
		       (values-assert-if '? *false* expr)
		       (if (true-p result)
			   (values-assert-if '? *true* expr)
			   (do-auto-rewrite expr
					    sig)))))
		((and (is-predicate? newop)
		      (member expr (type-constraints newargs t)
			      :test #'tc-eq));;(break)
		 (values-assert-if '? *true* expr))
		;;NSH(9.10.93) The case above is kept here so that assert-if-inside doesn't
		;;remove something brought in by typepred.
		((and (equation? expr);;moved here from
		      ;;assert-if-application so that
		      ;;assert-if-inside does not
		      ;;self-simplify antecedent
		      ;;equalities.
		      (tc-eq (find-supertype (type (car (exprs newargs))))
			     *number*)
		      (not (connective-occurs? expr)))
		 (assert-numeric-equality expr sig))

		(t  (assert-if-application expr newop newargs sig))))))))

(defun do-auto-rewrite (expr sig)
  (let* ((op* (operator* expr))
	 ;;;these are all the rewritable op*s.
	 (hashname (auto-rewrite-hashname op*))
	 (decl (if (name-expr? hashname)
		   (declaration hashname)
		   hashname)))  ;;(break "in-do-auto-rewrite")
    (if (and (not (memq *assert-flag*
			'(record simplify)));;do only if flag is rewrite/assert
	     decl
	     (gethash decl *auto-rewrites-ops*))
	(if *hash-rewrites?*
	    (do-auto-rewrite-memo expr op* decl sig)
	    (do-auto-rewrite-non-memo expr op* decl sig))
	(values sig expr))))

(defun do-auto-rewrite-memo (expr op* decl sig)
  (let ((hash-res (gethash expr *rewrite-hash*)))
    (if hash-res
	(do-auto-rewrite-memo* expr op* decl sig hash-res)
	(let ((top-hash-res (when *top-rewrite-hash*
			      (gethash expr *top-rewrite-hash*))))
	  (if top-hash-res
	      (do-auto-rewrite-memo* expr op* decl sig top-hash-res)
	      (do-auto-rewrite-non-memo-then-hash expr op* decl sig))))))


(defun do-auto-rewrite-memo* (expr op* decl sig hash-res)
  (let* (
	 (hashed-result  (nth 0 hash-res))
	 (hashed-dp-state (nth 1 hash-res))
	 (hashed-alists  (nth 2 hash-res))
	 (hashed-rewrites  (nth 3 hash-res))
	 (hashed-rewrites!  (nth 4 hash-res))
	 (hashed-macros (nth 5 hash-res))) ;;(break "memo")
	(progn
	  (incf *rewrite-hits*)
	  (if (and (not (dp-changed hashed-dp-state *dp-state*;;if context
				    hashed-alists *alists*));;unchanged since
		   (eq *auto-rewrites-names* hashed-rewrites);;hashing
		   (eq *auto-rewrites!-names* hashed-rewrites!)
		   (eq *macro-names* hashed-macros))
	      (if (eq hashed-result 'X) ; Previous rewrites did not alter expr.
		  (values sig expr)
		  (values '? hashed-result))
	      (multiple-value-bind (newsig newexpr)
		  (progn (remhash expr *rewrite-hash*)
			 (if (eq hashed-result 'X);then rewrite, else assert rhs.
			     (do-auto-rewrite-non-memo expr op* decl 'X)
			     (lazy-assert-if hashed-result)))
		(cond ((and (eq hashed-result 'X)
			    (eq newsig 'X))
		       (set-rewrite-hash expr 'X)
		       (values sig expr))
		      ((eq newsig 'X)
		       (set-rewrite-hash expr hashed-result)
		       (values '? hashed-result))
		      (t (set-rewrite-hash expr newexpr)
			 (values '? newexpr))))))))

(defun do-auto-rewrite-non-memo-then-hash (expr op* decl sig)
  (if (or (null *top-rewrite-hash*)
	  (eq *top-rewrite-hash* *rewrite-hash*)
	  (freevars expr));;NSH(4.2.95):means there are free
      ;;occurrences of bound variables in expr and these should
      ;;not be hashed at the top level.  
      (do-auto-rewrite-non-memo-then-hash* expr op* decl sig)
      (multiple-value-bind
	  (topsig topexpr)
	  (nprotecting-cong-state  ;;;changed from LET on alists
	   ((*dp-state* *top-dp-state*)
	    (*alists* *top-alists*))
	   (let ((*rewrite-hash* *top-rewrite-hash*))
	     (do-auto-rewrite-non-memo-then-hash* expr op* decl sig)))
	(if (eq topsig 'X)
	    (do-auto-rewrite-non-memo-then-hash* expr op* decl sig)
	    (multiple-value-bind
		(newsig newexpr)
		(lazy-assert-if topexpr)
	      (cond ((eq newsig topexpr)
		     (set-rewrite-hash expr topexpr)
		     (values '? topexpr))
		    (t (set-rewrite-hash expr newexpr)
		       (values '? newexpr))))))))

(defun set-rewrite-hash (expr result)
  (let ((hashed-dp-state
	 (when *new-ground?*
	   (dp::make-cong-state :stack nil :reverse nil
				:used-assertions
				(dp::cong-state-used-assertions *dp-state*))))
	(hashed-alists
	 (make-dpinfo (dpinfo-sigalist *alists*)
		      (dpinfo-findalist *alists*)
		      (dpinfo-usealist *alists*))))
    (setf (gethash expr *rewrite-hash*)
	  (list result hashed-dp-state hashed-alists;;(cons findalist usealist)
		*auto-rewrites-names*
		*auto-rewrites!-names*
		*macro-names*))))

(defun do-auto-rewrite-non-memo-then-hash* (expr op* decl sig)
      (multiple-value-bind (newsig newexpr)
	  (do-auto-rewrite-non-memo expr op* decl 'X)
	(incf *rewrite-misses*)
	(cond ((eq newsig 'X)
	       (set-rewrite-hash expr 'X)
	       (values sig expr))
	    (t (set-rewrite-hash expr newexpr)
	       (values newsig newexpr)))))



(defun do-auto-rewrite-non-memo (expr op* decl sig)
      (auto-rewrite* expr
		     sig
		     (gethash  decl    ;;;get the rewrite rules for decl.
			       *auto-rewrites*)
		     op*))


(defun flag-assert-if (expr &optional flag)
  (let ((*assert-flag* flag))
    (assert-if expr)))

(defun is-res-rewrite (res)
  (find res *all-rewrites-names* :test #'tc-eq))

(defun is-res-macro (res)
  (memq res *macro-names*))

(defun is-res-auto-rewrite-non! (res)
  (memq res *auto-rewrites-names*))

(defun is-res-auto-rewrite! (res)
  (memq res *auto-rewrites!-names*))

(defun is-res-auto-rewrite (res)
    (or (is-res-auto-rewrite-non! res)
	(is-res-auto-rewrite! res)
	(is-res-macro res)))

(defun generic? (res)
  (null (actuals (module-instance res))))

	      
(defun op*-depth (expr)
  (if (application? expr)(1+ (op*-depth (operator expr))) 0))

(defmethod id ((res resolution))
  (id (declaration res)))

(defun auto-rewrite* (expr oldsig hashentry &optional op*)
  ;;hashentry is a list of rewrite rules for op*
  (if (null hashentry)
      (values oldsig expr)
      (let* ((hashentry1 (car hashentry));;get first rewrite rule
	     (res (res hashentry1));; get resolution for rewrite rule.
	     (res-decl (unless (consp res) (declaration res)))
	     ;;to handle antecedent rewrites.
	     (mod-inst (when res-decl (module-instance res)))
	     (modsubst
	      (if (and res-decl
		       (typep res-decl 'formula-decl))
		  (let* ((current-mod? (eq (get-theory mod-inst)
					   (current-theory)))
			 (actuals (unless current-mod? (actuals mod-inst)))
			 (formals (unless current-mod?
				    (formals-sans-usings
				     (get-theory mod-inst)))))
		    (if (or (null formals) actuals)
			T
			(mapcar #'(lambda (x) (list x))
			  formals)))
		  T))
	     (lhs-hashentry (lhs hashentry1)))
	(cond ((and (eq expr op*)
		    (is-res-macro res)
		    (if (generic? res)
			(tc-eq (declaration op*)(declaration res))
			(tc-eq (resolution op*) res)))
	       (let* ((defns (create-formulas (resolution op*)))
		      (rhs (args2 (car (last defns)))))
		 (format-rewrite-msg (id (declaration res)) expr rhs)
		 (push-references-list (module-instance res) *dependent-decls*)
		 (pushnew (declaration res) *dependent-decls*)
		 (values '? rhs)))
	      ((not (eql (op*-depth expr)
			 (op*-depth lhs-hashentry)))
	       (auto-rewrite* expr oldsig (cdr hashentry) op*))
	      (t (let* ((def-or-lemma? (if (is-res-auto-rewrite res)
					   (if (typep res-decl 'def-decl)
					       'def-decl
					       (if (typep res-decl 'const-decl)
						   'const-decl
						   'formula-decl))
					   nil))
			(defn
			  (when (and (constant? op*)
				     (memq def-or-lemma? '(const-decl def-decl))
				     (if (generic? res)
					 (tc-eq (declaration op*) res-decl)
					 (tc-eq (resolution op*) res)))
			    (let (	;(defs (def-axiom (declaration op*)))
				  (defns (create-formulas (resolution op*))))
					;if (is-res-auto-rewrite! res)
					;   (last defns)
			      (car defns))))
			(defbody (when defn (if (forall-expr? defn)
						(expression defn)
						defn)))
			(lhs (if defbody (args1 defbody)
				 (if (eq def-or-lemma? 'formula-decl)
				     (lhs hashentry1)
				     nil)))
			(rhs (if defbody (args2 defbody)
				 (if (eq def-or-lemma? 'formula-decl)
				     (rhs hashentry1)
				     nil)))
			(if-flag (if defbody
				     (or (def-decl? res-decl)
					 (is-res-auto-rewrite-non! res))
				     (is-res-auto-rewrite-non! res))))
		   (multiple-value-bind
		       (subst modsubst)
		       (if defn;;NSH(5.8.98) Optimizes defns
			   (values
			    (loop for vars in (arguments* lhs)
				  as args in (arguments* expr)
				  nconc (pairlis-args
					 (mapcar #'declaration vars)
					 args))
			    T)
			   (if lhs
			       (let ((*modsubst* modsubst);;no tccs in match
				     (*generate-tccs* 'NONE));;NSH(11.24.98)
				 (values (match lhs expr nil nil)
					 *modsubst*))
			       'fail))
		     (let* ((modsubst (unless  (or (eq subst 'fail)
						   (eq modsubst T))
					(if (every #'cdr modsubst)
					    (mk-modname
						(id mod-inst)
					      (mapcar
						  #'(lambda (x)
						      (mk-actual (cdr x)))
						modsubst))
					    'fail)))
			    (subst (if (or (eq subst 'fail)
					   (null modsubst)
					   (eq modsubst 'FAIL))
				       subst
				       (loop for (x . y) in subst
					     collect
					     (cons (subst-mod-params x modsubst)
						   y))))
			    (hyp (hyp hashentry1))
			    (hyp (unless (or (null hyp)
					     (eq subst 'fail)
					     (eq modsubst 'fail))
				   (if (null modsubst)
				       hyp
				       (subst-mod-params hyp modsubst))))
			    (rhs (unless (or (eq subst 'fail)
					     (eq modsubst 'fail))
				   (if (null modsubst)
				       rhs
				       (subst-mod-params rhs modsubst)))))
		       (cond ((or (eq subst 'fail)(eq modsubst 'fail))
			      (if lhs;;then match must've failed.
				  (track-rewrite-format
				   res expr
				   "LHS ~a does not match."
				   lhs))
			      (auto-rewrite* expr oldsig
					     (cdr hashentry) op*))
			     (t 
			      (if defbody
				  (multiple-value-bind (sigrhs newrhs)
				      (cond ((is-res-auto-rewrite! res)
					     (inc-rewrite-depth res)
					     (flag-assert-if (substit rhs subst)))
					    (t (top-lazy-assert-if-with-subst
						rhs subst
						if-flag
						res)))
				    (decf *auto-rewrite-depth*);;NSH(5.26.95)
				    (cond ((and if-flag (eq sigrhs 'X))
			 ;;;then ignore current rewrite.
					   (track-rewrite-format
					    res expr
					    "RHS did not simplify.")
					   (auto-rewrite* expr oldsig
							  (cdr hashentry) op*))
					  (t (format-rewrite-msg
					      (if (consp res)
						  (car res)
						  (id res-decl))
					      expr newrhs)
					     (when (not (consp res))
					       (push-references-list
						(module-instance res)
						*dependent-decls*)
					       (pushnew res-decl
							*dependent-decls*))
					     ;;Records too many dependencies
					     ;;but this is conservative.
					     (values '? newrhs))))
				  (multiple-value-bind
				      (subst tccforms)
				      (let* ((*tccforms* NIL)
					     (*keep-unbound* *bound-variables*)
					     (subst (sort-alist subst))
					     (subst (tc-alist subst NIL 'TOP))
					     )
					(values subst *tccforms*))
				    (when (modname? modsubst);;NSH(2.8.97)
				      ;;ensure generation of assumption TCCS
				      ;;for generic rewrites.
				      (typecheck modsubst :tccs 'ALL))
				    (let ((newhyp
					   (nth-value 1
					     (when (hyp hashentry1)
					       (inc-rewrite-depth res)
					       ;;hit it with full assert.
					       (flag-assert-if  
						(substit hyp subst))))))
				      (when hyp
					(decf *auto-rewrite-depth*))
				      (cond
				       ((or (null hyp)
					    (tc-eq newhyp *true*))
					(let ((newtccs
					       (nth-value 1
						 (progn 
						   (inc-rewrite-depth res)
						   (flag-assert-if
						    (mapcar #'tccinfo-formula
						      tccforms))))))
					  (decf *auto-rewrite-depth*)
					  (cond ((every #'true-p newtccs)
						 (multiple-value-bind
						     (sigrhs newrhs)
						     (top-lazy-assert-if-with-subst
						      rhs
						      subst
						      if-flag
						      res)
						   (decf *auto-rewrite-depth*)
						   (cond ((and if-flag (eq sigrhs 'X))
							  (track-rewrite-format
							   res expr
							   "RHS did not simplify."
							   )
							  (auto-rewrite* expr oldsig
									 (cdr hashentry)
									 op*))
							 (t (format-rewrite-msg
							     (if (consp res)
								 (car res)
								 (id res-decl))
							     expr newrhs)
							    (when (not (consp res))
							      (push-references-list
							       (module-instance res)
							       *dependent-decls*)
							      (pushnew res-decl
								       *dependent-decls*
								       ))

							    (values '? newrhs)))))
						(t
						 (track-rewrite-format
						  res expr
						  "TCC(s)~{~a, ~} remain."
						  (loop for x in newtccs
							when (not (tc-eq x *true*))
							collect x))
						 (auto-rewrite* expr oldsig
								(cdr hashentry) op*)))))
				       (t (track-rewrite-format
					   res expr
					   "Hypothesis ~a did not simplify."
					   newhyp)
					  (auto-rewrite* expr oldsig
							 (cdr hashentry) op*))))))))))))))))

;;NSH(5.18.95)
(defun gensort (list order &optional accum);;generic sorter, CLISP sort sucks!!
  (cond ((null list) accum)
	(t (let* ((greatest (select-greatest list order))
		  (rest (remove greatest list)))
	     (gensort rest order (cons greatest accum))))))

(defun select-greatest (list order)
  (cond ((null list) nil)  ;;this should never happen if input is nonempty
	((member (car list)(cdr list)
		 :test order)
	 (select-greatest (cdr list) order))
	(t (car list))))
	     
(defun sort-alist (alist)
  (gensort alist
	#'(lambda (x y)
	    (member (declaration (car x))
		    (freevars (car y))
		    :test #'(lambda (u v)
			      (eq u (declaration v)))))))

(defun top-lazy-assert-if-with-subst (expr subst if-flag res)
  (let ((*assert-flag* nil))
    (inc-rewrite-depth res)
    (lazy-assert-if-with-subst expr subst if-flag)))

(defun case-or-branch? (expr)
  (or (branch? expr)(cases? expr)))

(defmethod lazy-assert-if-with-subst ((expr branch) subst &optional if-flag)
  (let ((newtest (assert-if-simplify (substit (condition expr) subst))))
    ;;check if assert-if-simplify is needed.  Why another assert-test
    ;;below.
    (if (check-for-connectives? newtest)
	(if if-flag
	    (values 'X expr);;expr is irrelevant
	    (values '? (substit expr subst)))
	(let ((result newtest));;instead of (assert-test newtest)
	  (cond ((tc-eq result *true*)
		 (let ((newthen
			(nth-value 1 (lazy-assert-if-with-subst
				      (then-part expr) subst))))
		   (values-assert-if '? newthen expr)))
		((tc-eq result *false*)
		 (let ((newelse
			(nth-value 1 (lazy-assert-if-with-subst
				      (else-part expr) subst))))
		   (values-assert-if  '? newelse expr)))
		(if-flag (values 'X expr))
		(t (values '? (substit expr subst))))))))

(defmethod lazy-assert-if ((expr branch))
  (let ((newtest (assert-if-simplify (condition expr))))
    ;;check if assert-if-simplify is needed.  Why another assert-test
    ;;below.  
    (if (check-for-connectives? newtest)
	(values 'X expr)
	(let ((result newtest));;instead of (assert-test newtest)
	  (cond ((tc-eq result *true*)
		 (let ((newthen (nth-value 1
				  (lazy-assert-if (then-part expr)))))
		   (values-assert-if '? newthen expr)))
		((tc-eq result *false*)
		 (let ((newelse (nth-value 1
				  (lazy-assert-if (else-part expr)))))
		   (values-assert-if  '? newelse expr)))
		(t (values 'X expr)))))))

(defun sig-assert-if (expr sig)
  (multiple-value-bind (newsig newexpr)
      (assert-if expr)
    (if (eq newsig 'X) (values sig expr)(values newsig newexpr))))

(defun sig-lazy-assert-if (expr sig)
  (multiple-value-bind (newsig newexpr)
      (lazy-assert-if expr)
    (if (eq newsig 'X) (values sig expr)(values newsig newexpr))))

(defun sig-lazy-assert-if-with-subst (expr sig subst)
  (multiple-value-bind (newsig newexpr)
      (lazy-assert-if-with-subst expr subst)
    (if (eq newsig 'X) (values sig newexpr)(values newsig newexpr))))

(defmethod lazy-assert-if-with-subst ((expr cases-expr) subst &optional if-flag)
  (with-slots (expression selections else-part)
      expr
    (let ((expression (substit expression subst)))
      (multiple-value-bind (sigexpr newexpr)
	  (assert-if expression);;(10.8.96):was cond-assert-if
	(let* ((expression (if (eq sigexpr '?) newexpr expression))
	       (all-result (check-all-recognizers expression))
	       (select (loop for sel in selections
			     thereis
			     (let ((check
				    (check-some-recognizer
				     (recognizer (constructor sel))
				     all-result)))
			       (when (true-p check) sel)))))
	  (cond ((null select)
		 (if (and else-part
			  (loop for sel in selections
				always (false-p
					(check-some-recognizer
					 (recognizer (constructor sel))
					 all-result))))
		     (sig-lazy-assert-if-with-subst
		      else-part '?  subst)
		     (if  if-flag;;NSH(2.27.97)
			  ;;was lcopying even on if-flag.
			  (values 'X expr)
			  (if (eq sigexpr '?)
			      (values '?
				      (lcopy expr 'expression expression
					     'selections
					     (substit selections subst)
					     'else-part
					     (substit else-part subst))) 
			      (values '? (substit expr subst))))))
		((and (name-expr? expression)(constructor? expression))
		 (sig-lazy-assert-if-with-subst
		  (expression select)  '? subst))
		((and (application? expression)
		      (constructor? (operator expression)))
		 (sig-lazy-assert-if-with-subst
		  (expression select)
		  '?
		  (nconc (pvs-pairlis (args select)
				      (arguments expression))
			 subst)))
		(t (sig-lazy-assert-if-with-subst
		    (expression select)
		    '?
		    (get-subst-accessors-in-selection-with-subst
		     expression
		     select subst)))))))))

(defun get-subst-accessors-in-selection-with-subst (expr sel subst)
  (let* ((thinst (module-instance (find-supertype (type expr))))
	 (accs (subst-mod-params (accessors (constructor sel)) thinst))
	 (vars (args sel)))
    (nconc (pairlis vars
		    (mapcar #'(lambda (acc) (make!-application acc expr))
		      accs))
	   subst)))

(defmethod lazy-assert-if ((expr cases-expr))
  (with-slots (expression selections else-part)
      expr
    (multiple-value-bind (sigexpr newexpr)
	(cond-assert-if expression)
      (let* ((expression (if (eq sigexpr '?) newexpr expression))
	     (all-result (check-all-recognizers expression))
	     (select (loop for sel in selections
			   thereis
			   (let ((check
				  (check-some-recognizer
				   (recognizer (constructor sel))
				   all-result)))
			     (when (true-p check) sel)))))
	(cond ((null select)
	       (if else-part
		   (if (loop for sel in selections
			     always (false-p (check-some-recognizer
					      (recognizer (constructor sel))
					      all-result)))
		       (sig-lazy-assert-if else-part '?)
		       (values 'X expr))
		   (values 'X expr)))
	      ((and (name-expr? expression)(constructor? expression))
	       (sig-lazy-assert-if (expression select) '?))
	      ((and (application? expression)(constructor? (operator expression)))
	       (sig-lazy-assert-if (substit (expression select)
				 (pvs-pairlis (args select)
					      (arguments expression))) '?))
	      (t (sig-lazy-assert-if (subst-accessors-in-selection expression
							       select) '?)))))))

(defmethod lazy-assert-if-with-subst ((expr application) subst &optional if-flag)
  (with-slots ((op operator) (arg argument))
      expr
  (if (lambda? op)  ;;Don't bother simplifying within a lambda op.
      (lazy-assert-if-with-subst
       (expression (operator expr))
       (append (pairlis-args (bindings op)
			     (substit (argument* arg) subst))
	       subst)
       if-flag)
      (call-next-method))))

(defmethod lazy-assert-if-with-subst ((expr expr) subst &optional if-flag)
  (declare (ignore if-flag))
  (let ((result (nth-value 1 (assert-if (substit expr subst)))))
    (values-assert-if '? result result)))

(defmethod lazy-assert-if ((expr expr))
  (assert-if expr))
		       
    

(defun adt-subtype? (type);;returns recognizer.
  (and (typep type 'subtype)
       (adt? (find-supertype type))
       (find-if #'recognizer? (type-predicates type))))

(defun assert-record-equality (expr)
  (let* ((labels (loop for x in (assignments (args1 expr))
		       collect  (caar (arguments x))))
	 (equalities
	  (loop for x in labels
		collect (make!-equation
			 (expression
			  (find x (assignments (args1 expr))
				:test #'(lambda (y z)
					  (same-id y (caar (arguments z))))))
			 (expression
			  (find x (assignments (args2 expr))
				:test #'(lambda (y z)
					  (same-id y (caar (arguments z)))))))))
	 (conjunction (make!-conjunction* equalities)))
    (multiple-value-bind (sig newexpr)
	(assert-if conjunction)
      (if (eq sig '?) (values '? newexpr)(values '? conjunction)))))
						   
				       

(defun assert-tuple-equality (expr)
  ;;NSH(10/91)This code will have to be revisited if we bring in
  ;;structural subtyping.
  (let ((conjunction
	 (make!-conjunction* (assert-tuple-equality* (exprs (args1 expr))
						     (exprs (args2 expr))))))
    (multiple-value-bind  (sig newexpr)
	(assert-if conjunction)
      (if (eq sig '?) (values '? newexpr) (values '? conjunction)))))

(defun assert-tuple-equality* (lhs* rhs*)
  (cond ((null lhs*) *true*)
	((null (cdr lhs*)) (list (make!-equation (car lhs*)(car rhs*))))
	(t  (cons (make!-equation (car lhs*)(car rhs*))
		  (assert-tuple-equality* (cdr lhs*)(cdr rhs*))))))

(defmethod assert-if ((expr list))
  (cond ((null expr) (values 'X nil))
	(t (multiple-value-bind (sig1 newcar)
	       (assert-if (car expr))
	     (multiple-value-bind (sig2 newcdr)
		 (assert-if (cdr expr))
		 (if (and (eq sig1 'X)(eq sig2 'X))
		     (values 'X expr)
		     (values '? (cons newcar newcdr))))))))

(defmethod assert-if ((expr binding-expr));;(break "assert-if-binding")
  (with-slots (bindings expression) expr
    (let* ((*subtype-hash* (when *subtype-hash* (copy *subtype-hash*)))
	   (*assert-typepreds-off* T) 
	   (*bound-variables* (append bindings
				      *bound-variables*))
	   (*local-typealist* *local-typealist*);;shadowing
	   (typepreds (loop for x in bindings
			    nconc
			    (let ((y (make!-name-expr
				      (id x) nil nil
				      (make-resolution x nil (type x))
				      'VARIABLE))
				  (*keep-unbound* *bound-variables*))
			      ;; NSH(12.30.93)not sure if *keep-unbound*
			      ;; needs to be set.
			      (collect-type-constraints* y))))
	   );;(break "binding")
      (multiple-value-bind (sig newexpr)
	  (cond-assert-if expression typepreds)
	(if (or (and (forall-expr? expr)
		     (tc-eq newexpr *true*))
		(and (exists-expr? expr)
		     (tc-eq newexpr *false*)))
	    (values '? newexpr)
	    (do-auto-rewrite (lcopy expr 'expression newexpr) sig))))))

(defmethod assert-if ((expr expr))
	   (values 'X expr))

;;tests the value of a formula in the current dec. procedure alist.
(defun assert-test (fmla)
  (unless (check-for-connectives? fmla)
    (nprotecting-cong-state  ;;changed from LET on alists
     ((*dp-state* *dp-state*)
      (*alists* *alists*))
     (when *pseudo-normalizing*
       (mapc #'assert-typepreds *assert-typepreds*))
     (call-process fmla *dp-state* *alists*))))

(defun assert-test0 (fmla)
  (unless (check-for-connectives? fmla)
    (nprotecting-cong-state
     ((*dp-state* *init-dp-state*)
      (*alists* *init-alists*))
     (let ((result (call-process fmla *dp-state* *alists*)))
       result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto-rewriting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto-rewrite (names ps &optional force?)
  (declare (ignore ps))
  (loop for name in names
	do (let ((force? (or force?
			     (and (consp name)
				  (if (consp (car name)) '!! T))))
		 (name (if (consp name)
			   (if (consp (car name)) (car name) name)
			   (list name))))
	     (loop for nm in name
		   do (let ((res-alist
			     (collect-auto-rewrite-res nm *current-context*)))
			(if (null res-alist)
			    (format-if "~%No resolution for ~a" nm)
			    (loop for (res . fmla) in res-alist
				  do (if (integerp res)
					 (auto-rewrite-antecedent
					  res fmla force?)
					 (auto-rewrite-res
					  res force? *current-context*)))))))))

(defun prefix? (x y) ;both strings
  (let ((lx (length x))
	(ly (length y)))
    (and (<= lx ly)
	 (equal x (subseq y 0 lx)))))

(defun auto-rewrite-antecedent (num fmla always?)
  (let* ((string (format nil "~a_~a" (label *top-proofstate*)
				 (abs num)))
	 (occurrences
	  (loop for res in *auto-rewrites-names*
		when (and (consp res)
			  (prefix? string (string (car res))))
		collect (symbol-index (string (car res)))))
	 (occurrences!
	  (loop for res in *auto-rewrites!-names*
		when (and (consp res)
			  (prefix? string (string (car res))))
		collect (symbol-index (string (car res)))))
	 (all-occurrences (append occurrences occurrences!))
	 (new-index (if all-occurrences (apply #'max all-occurrences) 1))
	 (name (intern (format nil "~a$~a" string new-index))))
    (install-rewrite-res (list name fmla) name fmla always?)))


(defun collect-auto-rewrite-res (name context)
  (if (integerp name)
      (if (minusp name)
	  (let* ((fmlas (mapcar #'formula
				(select-seq (s-forms (current-goal *ps*))
					    (list name))))
		 (fmla (when fmlas (args1 (car fmlas)))))
	    (when fmla (list (cons name fmla))))
	  (progn (format-if "~%Consequent formula numbered ~a ~
cannot be used for rewriting." name)
		 nil))
      (let* ((name (pc-parse name 'name))
	     (formula-resolutions
	      ;(let ((*generate-tccs* 'ALL))
		(resolve name 'formula nil context));)
	     (constant-resolutions
	      ;(let ((*generate-tccs* 'ALL))
		(loop for res in (resolve name 'expr nil context)
		      when (with-slots (declaration) res
			     (and (slot-exists-p declaration
						 'definition)
				  (definition
				    declaration)))
		      collect res));)
	     ;;(*generate-tccs* NIL)
	     (formulas (loop for res in formula-resolutions
			     collect
			     (select-from-fmlas
			      (create-formulas res context))))
	     (fmla-alist (loop for res in formula-resolutions
			       as fmla in formulas
			       when  (check-auto-rewrite res fmla)
			       collect (cons res fmla)))
	     (defn-alist (loop for res in constant-resolutions
			       collect
			       (cons res
				     (select-from-fmlas
				      (create-formulas res context)))))
	     (res-alist (nconc fmla-alist defn-alist)))
	res-alist)))
		       


(defun check-auto-rewrite (res fmla)
  (let* ((mod-inst (module-instance res))
	 (theory (get-theory mod-inst))
	 (current-mod? (eq theory (current-theory)))
	 (actuals (unless current-mod?
		    (actuals mod-inst)))
	 (formals (unless current-mod?
		    (formals-sans-usings theory))))
    (multiple-value-bind
	(lhs rhs hyp)
	(split-rewrite fmla)
      (let* ((lhs-freevars (freevars lhs))
	     (rhs-freevars (freevars rhs))
	     (hyp-freevars (freevars hyp))
	     (op* (operator* lhs))
	     (hashname (auto-rewrite-hashname op*)))
	(cond
	 ((null hashname)
	  (format-if "~%Can't rewrite using ~a:  LHS key ~a is bad."
		     (id (declaration res)) op*)
	  nil)
	 ((not (subsetp rhs-freevars lhs-freevars
			:test #'tc-eq))
	  (format-if "~%RHS free variables in ~a must be contained in the LHS f
ree variables in: ~a" rhs lhs)
	  nil)
	 ((not (subsetp hyp-freevars lhs-freevars
			:test #'tc-eq))
	  (format-if "~%Hypothesis free variables in ~a must be contained in th
e LHS free variables in ~a" hyp lhs)
	  nil)
	 ((and formals (null actuals)
	       (not (subsetp (free-params lhs) formals)))
	  (format-if "~%Theory~a is generic; No actuals given;
Free parameters in the LHS of rewrite must contain all theory formals."
		     mod-inst)
	  nil)
	 (t))))))
		 
  
(defun select-from-fmlas (fmlas)
  (cond ((null fmlas) nil)
	(t (car fmlas))))
;above defn selects most curried form instead of
;f(x) form of a function definition for auto-rewriting.
;	((null (cdr fmlas))
;	 (car fmlas))
;
;	((null (cddr fmlas))
;	 (car fmlas))
;	(t (select-from-fmlas (cdr fmlas)))

(defmethod auto-rewrite-hashname ((expr name-expr))
  expr)

(defmethod auto-rewrite-hashname ((expr record-expr))
  'recordcons)

(defmethod auto-rewrite-hashname ((expr tuple-expr))
  'tuplecons)

(defmethod auto-rewrite-hashname ((expr update-expr))
  'update)

(defmethod auto-rewrite-hashname ((expr projection-application))
  (id expr))

(defmethod auto-rewrite-hashname ((expr field-application))
  (id expr))  ;;NSH(9.15.95): in progress.

(defmethod auto-rewrite-hashname ((expr lambda-expr))
  'lambda)

(defmethod auto-rewrite-hashname ((expr forall-expr))
  'forall)

(defmethod auto-rewrite-hashname ((expr exists-expr))
  'exists)

(defmethod auto-rewrite-hashname ((expr cases-expr))
  'cases)

(defmethod auto-rewrite-hashname ((expr expr))
  NIL)


(defun auto-rewrite-res (res always? context)
  (cond ((member res *auto-rewrites-off* :test #'tc-eq)
;	 (setf *auto-rewrites-off*
;	       (delete res *auto-rewrites-off* :test #'tc-eq))
	 nil)
	(t (let* ((fmlas (create-formulas res context))
		  (thefmla (select-from-fmlas fmlas))
		  (name (id (declaration res))))
	     (when thefmla
	       (install-rewrite-res res name thefmla always?))))))

(defun install-rewrite-res (res name fmla always?)	       
  (multiple-value-bind
      (lhs rhs hyp)
      (split-rewrite fmla)
    (let* ((res (or (is-res-rewrite res) res))
	   (lhs-freevars (freevars lhs))
	   (rhs-freevars (freevars rhs))
	   (hyp-freevars (freevars hyp))
	   (op* (operator* lhs))
	   (hashname (auto-rewrite-hashname op*))
	   (decl (if (typep hashname 'name-expr)
		     (declaration hashname)
		     hashname))) ;;(break "install-rewrite-res")
      (cond
       ((null hashname)
	(format-if "~%Can't rewrite using ~a:  LHS key ~a is bad" name op*))
       ((not (subsetp rhs-freevars lhs-freevars
		      :test #'tc-eq))
	(format-if "~%Can't rewrite using ~a: non-LHS freevars in RHS." name ))
       ((not (subsetp hyp-freevars lhs-freevars
		      :test #'tc-eq))
	(format-if "~%Can't rewrite using ~a: non-LHS freevars in hypotheses." name))
       ((and (resolution? res)
	     (inductive-decl? (declaration res)))
	(format-if "~%Can't rewrite using ~a: inductive definition cannot be used." name))
       (t
	(unless (consp res);;(6.16.95)avoids antecedent rewrites
	  (typecheck (module-instance res) :tccs 'ALL))
	;;NSH(6.14.95): above typecheck needed to generate
	;;assuming TCCS.  
	(pushnew
	 (make-instance 'rewrite
	   'lhs lhs
	   'rhs rhs
	   'hyp hyp
	   'res res)
	 (gethash decl
		  *auto-rewrites*)
	 :test #'(lambda (x y)(tc-eq (res x)(res y))))
	(pushnew res *all-rewrites-names*) ;;don't need tc-eq
	;;				(*auto-rewrites*)
	(cond ((and (eq always? '!!)   ;;NSH(5.8.98) inserted macro case
		    (not (and (resolution? res) ;;NSH(12.1.95)
			      (def-decl? (declaration res)))))
	       (pushnew res *macro-names*)
			;;:test #'tc-eq
	       (setq *auto-rewrites-names*
		     (remove res *auto-rewrites-names*))
	       (setq *auto-rewrites!-names*
		     (remove res *auto-rewrites!-names*))
			     ;;:test #'tc-eq
	       (format-if "~%Installing macro(!!) ~a" name))
	      ((and always?   ;;NSH(10.7.95) decl -> (declaration res)
		    (not (and (resolution? res) ;;NSH(12.1.95)
			      (def-decl? (declaration res)))))
	       (pushnew res *auto-rewrites!-names*)
			;;:test #'tc-eq
	       (setq *auto-rewrites-names*
		     (remove res *auto-rewrites-names*))
	       (setq *macro-names*
		     (remove res *macro-names*))
			     ;;:test #'tc-eq
	       (format-if "~%Installing rewrite rule(!) ~a" name))
	      (t (pushnew res *auto-rewrites-names*)
			  ;;:test #'tc-eq
		 (setq *auto-rewrites!-names*
		       (remove res *auto-rewrites!-names*))
		 (setq *macro-names*
		     (remove res *macro-names*))
			       ;;:test #'tc-eq
		 (format-if "~%Installing rewrite rule ~a" name)))
	(setf (gethash (rewrite-declaration hashname) *auto-rewrites-ops*) T)
	(format-if "~%Installing rewrite rule ~a" name))))))




(defun auto-rewrite-step (names &optional force?)
  #'(lambda (ps)
      (let* ((*auto-rewrites-names* *auto-rewrites-names*)
	     (*auto-rewrites!-names* *auto-rewrites!-names*)
	     (*macro-names*  *macro-names*)
	     (*auto-rewrites* (copy *auto-rewrites*)))
	(auto-rewrite names ps force?)
	(if (and (eq *auto-rewrites-names*
		     (auto-rewrites-names (current-auto-rewrites ps)))
		 (eq *auto-rewrites!-names*
		     (auto-rewrites!-names (current-auto-rewrites ps)))
		 (eq *macro-names*
		     (macro-names (current-auto-rewrites ps))))
	    (values 'X nil nil)
	    (values '? (list (cons (current-goal ps)
				   (list 'current-auto-rewrites
					 (mk-auto-rewrites-info
					  *auto-rewrites-names*
					  *auto-rewrites!-names*
					  *macro-names*
					  *all-rewrites-names*
					  *auto-rewrites*
					  (current-auto-rewrites ps))))))))))

      
      
      

;      (cond ((consp names)
;	     (loop for name in names do
;		   (auto-rewrite name (context *current-theory*))))
;	    (t (auto-rewrite names (context *current-theory*))))
;      (values '? (list (current-goal ps)))

(defmethod arithop-decl? ((x name-expr))
  (and (memq (id x)
	     '(+ - * / < <= > >=))
       (memq (id (module x))
	     '(|numbers| |reals| |rationals|
	       |integers| |naturalnumbers|))))

(defmethod arithop-decl? ((x const-decl))
  (and (memq (id x)
	     '(+ - * / < <= > >=))
       (memq (id (module x))
	     '(|numbers| |reals| |rationals|
	       |integers| |naturalnumbers|))))

(defmethod arithop-decl? ((x T))
  NIL)

;;NSH(7.25.94): Sam's improved collect-explicit-names.  The earlier one
;;was inefficient in exploring paths repeatedly.
;;NSH(12.14.94): collect-explicit-names not used anymore.
;(defun collect-explicit-names (decl ps explicit)
;  (let ((decls (collect-referenced-decls decl ps explicit)))
;    (mapcar #'(lambda (d)
;		(mk-name-expr (id d) nil (id (module d))))
;	    decls)))

;;NSH(3.28.95): exclude-theories must be a list of theories.
(defun collect-referenced-decls (decl ps explicit exclude-theories)
  (let ((decls nil)
	(all-decls nil))
    (labels ((collect (decl)
	       (unless (memq decl all-decls)
		 (push decl all-decls)
		 (when (and (not (memq decl decls))
			    (not (arithop-decl? decl))
			    (not (memq (module decl)
				       exclude-theories)))
		   (when (and (or (and (const-decl? decl)
				       (not (def-decl? decl)));;NSH(4.3.95)
				  (and (not explicit)
				       (def-decl? decl)))
			      (definition decl))
		     (push decl decls))
		   (dolist (d (refers-to decl))
		     (collect d))))))
      (collect decl)
      (loop for x in (dependent-decls ps) do (collect x))
      decls)))

;;NSH(12.21.93) moved auto-rewrite-defs, auto-rewrite-explicit to strategies.lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto-rewriting for theory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(12.7.94): Changing install rewrites so that theories are given
;;in one list with keywords attached to each theory name.


;(defun install-rewrites (names theories theory-definitions
;			       exclude-names always? no-tccs?)
;  #'(lambda (ps)
;      (let* ((*auto-rewrites-names* *auto-rewrites-names*)
;	     (*auto-rewrites!-names* *auto-rewrites!-names*)
;	     (names (if (listp names) names (list names)))
;	     (exclude-names
;	      (mapcar #'(lambda (x) (pc-parse x 'name))
;		      (if (listp exclude-names)
;			  exclude-names
;			  (list exclude-names))))
;	     (exclude-resolutions
;	      (loop for name in exclude-names
;		    nconc (nconc (resolve name 'formula nil)
;				 (resolve name 'expr nil))))
;	     (theories (if (listp theories) theories
;			   (list theories)))
;	     (theory-definitions (if (listp theory-definitions)
;				     theory-definitions
;				     (list theory-definitions))))
;	(loop for theory in theories do
;	      (auto-rewrite-theory theory nil exclude-resolutions
;				   always? (not no-tccs?) ps))
;	(loop for theory in theory-definitions do
;	      (auto-rewrite-theory theory T exclude-resolutions
;				   always? (not no-tccs?) ps))
;	(auto-rewrite names ps always?) ;;NSH(11.22.94) changed order.
;	(let ((new-rewrites-info
;	       (mk-auto-rewrites-info
;				      *auto-rewrites-names*
;				      *auto-rewrites!-names*
;				      (current-auto-rewrites ps))))
;	  (cond ((eq new-rewrites-info
;		     (current-auto-rewrites ps))
;		 (format-if "~%No new rewrites installed.")
;		 (values 'X nil nil))
;		(t 
;		 (values '? (list (cons (current-goal ps)
;					(list 'current-auto-rewrites
;					      new-rewrites-info))))))))))


(defun auto-rewrite-theory (name &optional defs-only? exclude-resolutions
				 always? tccs? ps)
      (auto-rewrite-theory* name defs-only? exclude-resolutions
			    always? tccs? ps))

(defun check-theory-names (names modules &optional defs-only?)
  (cond ((null modules) T)
	((null (car modules))
	 (format-if "~%Could not find theory ~a" (car names))
	 nil)
	((and (eq (id (car modules))(id *current-theory*))
	      (actuals (car names))) ;;NSH(9.21.94)
	 (format-if "~%Current theory ~a should not have actuals."
		    (id (car modules)))
	 nil)
	((and (null defs-only?)
	      (not (or (eq (id (car modules))(id *current-theory*))
		       (null (formals-sans-usings (car modules)))
		       (actuals (car names)))))
	 (format-if
	  "~%~a is not a fully instantiated theory." (car names))
	 nil)
	(t t)))
	       
	       

(defun auto-rewrite-theory* (name defs-only? exclude-resolutions
				  always? tccs? ps)
  (let* (;(*auto-rewrites-names* *auto-rewrites-names* )
	 ;(*auto-rewrites!-names* *auto-rewrites!-names* )
	 (names (list name)) ;;historical, names->name in args.
	 ;;(*generate-tccs* 'ALL)
	 (names (loop for name in names
		      collect (typecheck
			       (pc-parse name 'modname)
			       :tccs 'ALL
			       :context *current-context*)))
	 ;;(*generate-tccs* NIL)
	 (modules (loop for name in names
			collect (get-theory name)))
	(check (check-theory-names names modules defs-only?)))
    (cond ((null names) (values 'X nil))
	  ((not check) (values 'X nil))
	  (t (loop for name in names
		   as module in modules
		   do (auto-rewrite-theory-name name module
						defs-only?
						exclude-resolutions
						always? tccs? ps))
;	     (values '? (list
;			 (cons (current-goal ps)
;			       (list 'current-auto-rewrites
;				     (mk-auto-rewrites-info
;				      *auto-rewrites-names*
;				      *auto-rewrites!-names*
;				      (current-auto-rewrites ps))))))
	     ))))

(defun auto-rewrite-theory-name (name module defs-only?
				      exclude-resolutions
				      always? tccs? ps)
  (declare (ignore ps))
  (let* ((assuming-decls (when module (assuming module)))
	 (theory-decls (when module (theory module)))
	 (all-decls (append assuming-decls theory-decls))
	 (fdecls (loop for decl in all-decls
		       when
		       (and (if defs-only?
				(typep decl '(or const-decl def-decl))
				(and (typep decl
					    '(or formula-decl const-decl
						 def-decl))
				     (or tccs? (not (tcc? decl)))))
			    (not (member decl
					 exclude-resolutions
					 :test
					 #'(lambda (x y)
					     (let* ((modinst
						     (module-instance y))
						    (actuals (actuals modinst)))
					       (and (eq x (declaration y))
						    (eq (id modinst)
							(id module))
						    (or (null actuals)
							(tc-eq actuals
							       (actuals module)))))))))
		       collect decl))
	 (fdecls
	  (if (eq (id module)(id *current-theory*))
	      (ldiff fdecls
		     (memq (declaration *top-proofstate*)
			   fdecls))
	      fdecls)))
    (format-if "~%Installing rewrites from theory ~a" name)
    (loop for decl in fdecls
	  do (auto-rewrite-res
	      (make-resolution decl name)
	      always? 
	      *current-context*)))
  (format-if "~%Installed rewrites from theory ~a" name))


	       
(defun get-antec-name (name all-names &optional (max 0))
  (if (consp all-names)
      (let ((aname (when (consp (car all-names))
		     (string (caar all-names)))))
	(if (and aname
		 (prefix? name aname)
		 (> (symbol-index aname) max))
	    (get-antec-name name (cdr all-names)
			    (symbol-index aname))
	    (get-antec-name name (cdr all-names) max))) 
      (unless (zerop max)
	(intern (format nil "~a$~a" name max)))))    


(defun stop-rewrite-step (names)
  #'(lambda (ps)
      (if (null names)
	  (cond ((or *auto-rewrites-names* *auto-rewrites!-names*
		     *macro-names*)
		 (format-if "~%Disabling all current auto-rewrites.")
		 (values '?
			 (list (cons (current-goal ps)
				     (list 'current-auto-rewrites
					   (mk-auto-rewrites-info
					    nil  nil nil
					    *all-rewrites-names*
					    *auto-rewrites*
					    (current-auto-rewrites ps))
					   'rewrite-hash (clrhash
							  *rewrite-hash*))))))
		(t (format-if "~%No current auto-rewrites.")
		   (values 'X nil nil)))
	  (let* ((names (if (consp names) names (list names)))
		 (context *current-context*)
		 (old-auto-rewrites-names *auto-rewrites-names*)
		 (old-auto-rewrites!-names *auto-rewrites!-names*)
		 (old-macro-names *macro-names*)
		 (antecedent-names
		  (let* ((all-names (append *auto-rewrites-names*
					    *auto-rewrites!-names*
					    *macro-names*))
			 (numbered-antecedents
			  (loop for name in names
				when (integerp name)
				collect
				(get-antec-name
				 (format nil "~a_~a"
				   (label *top-proofstate*)
				   (abs name))
				 all-names)))
			 (named-antecedents
			  (loop for name in names
				when (member name
					     all-names
					     :test
					     #'(lambda (x y)
						 (when (consp y)
						   (same-id x (car y)))))
				collect name)))
		    (append numbered-antecedents named-antecedents)))
		 (names (remove-if #'integerp names))
		 (names (set-difference names antecedent-names
					     :test #'eq))
		 (parsenames (loop for name in names
				   collect (pc-parse name 'name)))
		 (fmla-resolutions
		  (loop for name in parsenames
			append (resolve name
				       'formula
				       nil context)))
		 (const-resolutions
		  (loop for name in parsenames
			append (resolve name 'expr nil context)))
		 (constant-resolutions
		  (loop for res in  const-resolutions
			when (and (typep (declaration res) 'const-decl)
				  (definition (declaration res)))
			collect res))
		 (resolutions (append fmla-resolutions constant-resolutions)))
	    (loop for name in antecedent-names
		  do (progn
		       (setq *auto-rewrites-names*
			     (remove name *auto-rewrites-names*
				     :test #'(lambda (x y)	
				       (when (consp y)
						 (same-id x (car y))))))
		       (setq *auto-rewrites!-names*
			     (remove name *auto-rewrites!-names*
				     :test #'(lambda (x y)
					       (when (consp y)
						 (same-id x (car y))))))
		       (setq *macro-names*
			     (remove name *macro-names*
				     :test #'(lambda (x y)
					       (when (consp y)
						 (same-id x (car y))))))
		       nil))
	    (loop for res in resolutions do
		  (if (or (member  res *auto-rewrites-names* :test #'tc-eq)
			  (member  res *auto-rewrites!-names*
				   :test #'tc-eq)
			  (member res *macro-names* :test #'tc-eq))
		      (stop-rewrite-res res)
		      (format-if "~%~a is not an auto-rewrite"
				 (id (declaration res)))))
	    (if (and (eq *auto-rewrites-names* old-auto-rewrites-names)
		     (eq *auto-rewrites!-names* old-auto-rewrites!-names)
		     (eq *macro-names* old-macro-names))
		(values 'X nil nil)
		(values '? (list (cons (current-goal ps)
				       (list 'current-auto-rewrites
					     (mk-auto-rewrites-info
					      *auto-rewrites-names*
					      *auto-rewrites!-names*
					      *macro-names*
					      *all-rewrites-names*
					      *auto-rewrites*
					      (current-auto-rewrites ps))
					     'rewrite-hash
					     (clrhash *rewrite-hash*))))))))))

(defun stop-rewrite-res (res)
  (cond ((member res *auto-rewrites-names* :test #'tc-eq)
	 (setq *auto-rewrites-names*
	       (remove res *auto-rewrites-names* :test #'tc-eq))
	 (format-if "~%Turned off ~a" (id (declaration res)))
;;	 (pushnew res *auto-rewrites-off* :test #'tc-eq)
	 nil)
	((member res *auto-rewrites!-names* :test #'tc-eq)
	 (setq *auto-rewrites!-names*
	       (remove res *auto-rewrites!-names* :test #'tc-eq))
	 (format-if "~%Turned off ~a" (id (declaration res)))
;;	 (pushnew res *auto-rewrites-off* :test #'tc-eq)
	 nil)
	((member res *macro-names* :test #'tc-eq)
	 (setq *macro-names*
	       (remove res *macro-names* :test #'tc-eq))
	 nil)
	(t nil)))



;;NSH(1.4.95): The stop-rewrite-theory code is no longer used.
	    

(defun stop-rewrite-theory (names)
  #'(lambda (ps)
      (let ((*auto-rewrites-names* *auto-rewrites-names*)
	    (*auto-rewrites!-names* *auto-rewrites!-names*))
	(stop-rewrite-theory-names names ps))))

(defun stop-rewrite-theory-names (names ps)
  (cond ((null names)(values 'X nil))
	(t (let* ((name (car names))
		  (name (typecheck (pc-parse name 'modname)
			      :context *current-context*))
		  (module (get-theory name))
		  (carval (stop-rewrite-theory-name name module ps))
		  (cdrval (stop-rewrite-theory-names (cdr names) ps)))
	     (if (or (eq carval '?)(eq cdrval '?))
		 (values '? (list (cons (current-goal ps)
					(list 'current-auto-rewrites
					      (mk-auto-rewrites-info
					       *auto-rewrites-names*
					       *auto-rewrites!-names*
					       *macro-names*
					       *all-rewrites-names*
					       *auto-rewrites*
					       (current-auto-rewrites ps))
					      'rewrite-hash
					      (clrhash *rewrite-hash*)))))
		 (values 'X nil))))))
	 
(defun stop-rewrite-theory-name (name module ps)
  (cond (module
	 (cond ((or (eq (id module)(id *current-theory*))
		    (null (formals-sans-usings module))(actuals name))
		(let ((assuming-resolves
		       (loop for decl in (assuming module)
			     when (typep decl '(or formula-decl
						const-decl def-decl))
			     collect (make-resolution decl name)))
		      (theory-resolves
		       (loop for decl in (theory module)
			     when (typep decl '(or formula-decl
						const-decl def-decl))
			     collect (make-resolution decl name))))
		  (loop for res in assuming-resolves
			do (stop-rewrite-res res))
		  (loop for res in theory-resolves
			do (stop-rewrite-res res))
		  (values '? (list (current-goal ps)))))
	       (t (format-if "~%~a is not a fully instantiated theory." name)
		  (values 'X nil))))
	((format-if "~%Could not find theory ~a" name)
	 (values 'X nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(1/20/93): The issue here is to define simpler versions of assert,
;;specifically
;;1. record/note: just note the assertable statements.
;;2. simplify: do arithmetic and boolean simplification but no note.
;;3. auto-expand: which expands definitions using decision procs, I can
;;probably get expand to do this automatically.
;;The main new code that is needed is an arithmetic simplifier.
;;The transformations are to put arithmetic terms into sum of products
;;normal form so that each product looks like 2*x*x*y and the sum looks
;;like A + B + 4.

(defun collect-auto-rewrites ()
  (when (macro-names *ps*)
    (format t "The following definitions are always macro-expanded.~2%")
    (loop for res in (macro-names *ps*)
	  do (if (consp res)
		 (format t "~a: ~a~2%" (car res)
			 (unpindent (cadr res)
				    (+ (length (string (car res))) 2)
				    :string T))
		 (let* ((name (make-instance 'name
			       'id (id (declaration res))
			       'actuals (actuals (module-instance res))))
		       (name-string (unparse name :string t)))
		   (format t "~a: ~:[ ~;~%    ~]~a~2%"
		     name-string
		     (> (length name-string) 15)
		     (unpindent (car (create-formulas res))
				(if (> (length name-string) 15)
				    4
				    (+ (length name-string) 2))
				:string T)
		     )))))
  (format t "~2%The remaining rewrites of function definitions only occur 
when an expression matches the most curried form of the LHS of the
definition. ~2%")
  (when (auto-rewrites!-names *ps*)
    (format t "The following rewrite rules apply unconditionally
in reverse chronological order: ~3%")
    (loop for res in (auto-rewrites!-names *ps*)
	  do (if (consp res)
		 (format t "~a: ~a~2%" (car res)
			 (unpindent (cadr res)
				    (+ (length (string (car res))) 2)
				    :string T))
		 (let* ((name (make-instance 'name
			       'id (id (declaration res))
			       'actuals (actuals (module-instance res))))
		       (name-string (unparse name :string t)))
		   (format t "~a: ~:[ ~;~%    ~]~a~2%"
		     name-string
		     (> (length name-string) 15)
		     (unpindent (car (create-formulas res))
				(if (> (length name-string) 15)
				    4
				    (+ (length name-string) 2))
				:string T)
		     ))))
    (format t "~3%"))
  (when (auto-rewrites-names *ps*)
    (format t "The following rewrite rules apply only if any top-level
IF-THEN-ELSE or CASE in the RHS simplifies.  The rules in reverse
chronological order are:~3%") 
    (let ((names (auto-rewrites-names *ps*)))
      (loop for res in names do
	    (if (consp res)
		(format t "~a: ~a~2%" (car res)
			(unpindent (cadr res)
				   (+ (length (string (car res))) 2)
				   :string T))
		(let* ((name (make-instance 'name
			       'id (id (declaration res))
			       'actuals (actuals (module-instance res))))
		       (name-string (unparse name :string t)))
		  (format t "~a: ~:[ ~;~%    ~]~a~2%"
		     name-string
		     (> (length name-string) 15)
		     (unpindent (car (create-formulas res))
				(if (> (length name-string) 15)
				    4
				    (+ (length name-string) 2))
				:string T)
		     )))))))

(defun show-auto-rewrites ()
  (if (and *in-checker* *ps*)
      (pvs-buffer "*Auto-Rewrites*"
	(with-output-to-string (*standard-output*)
	   (collect-auto-rewrites))
	t t)
      (pvs-message "No current proof")))

