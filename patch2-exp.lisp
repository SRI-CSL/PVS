(in-package :pvs)

(defvar *patch-exp-revision* "$Revision$")
(setq *patch-exp-revision* "$Revision$")

(defparameter *binfile-version* 100)

(eval-when (eval compile load)
  (unless (find-package :dp)
    (make-package :dp)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  PVS 2.2 extensions go in here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;NSH(5.14.97): assert-typepreds with updates are no longer asserted
;;;since their translation is exponential and the ground prover
;;;is very slow on these.

(in-package 'pvs)

(defvar *assert-connectives?* nil)
(defvar *ignore-prover-output?* nil)
(defvar *update-is-connective?* nil)

(defmethod connective-occurs? ;;NSH(5.13.97) needed for updates
    ;;or the translations get HUGE.
    ((expr update-expr))
  *update-is-connective?*)

(defmacro check-for-connectives? (fmla)
  `(unless *assert-connectives?*
     (connective-occurs? ,fmla)))

(defmacro track-rewrite-format (res expr format-string &rest args)
  `(let ((id (if (consp ,res)(car ,res)(id ,res))))
     ;;NSH(9.19.97) removed (format nil "~a" ...) from above
     (when (member id *track-rewrites* :test #'same-id)
       (let ((expr-string (format nil "~a" ,expr)))
	 (format t "~%;;~a failed to rewrite " id)
	 (if (> (+ (length id)(length expr-string) 21) *default-char-width*)
	     (format t "~%;;~a" (unpindent ,expr 2 :string T))
	     (format t "~a" expr-string))
	 (format t "~%;;;;")
	 (format t ,format-string ,@args)))))

(addrule 'simplify
	 ()
	 ((fnums *) record? rewrite? 
	   rewrite-flag flush? linear? cases-rewrite? (type-constraints? T)
	   ignore-prover-output?
	   (assert-connectives? T))
	 (invoke-simplification fnums record? rewrite?
				rewrite-flag flush? linear?
				cases-rewrite? type-constraints?
				ignore-prover-output?
				assert-connectives?)
	 "Uses the decision procedures to to simplify the formulas in
FNUM and record them for further simplification.  The proof steps
ASSERT, RECORD, SIMPLIFY, DO-REWRITE are instances of this primitive
rule and should always be preferred.  The arguments have the following
effect:
 RECORD?: If T, the formula is simplified and recorded as true or false
          by the decision procedures to be used for further
          simplification.  The formula is only recorded if it does not
          contain top-level Boolean connectives or conditionals.
 REWRITE?: If T, the installed rewrite rules are applied but no
          simplification is done except on the right-hand side.
          Full-blown simplify is used to simplify the right-hand side
          instance of the rewrite, the TCCs, and the conditions of
          conditional rewrite rules.
 If  REWRITE? is NIL the formula is simplified so that
    1. arithmetic expressions are put into a sum-of-products form.
    2. lambda, tuple, record, datatype, and update redexes are reduced.
    3. decision procedures are used to determine whether boolean
       expressions are TRUE/FALSE in the context in which they appear.
    3. Boolean/conditional expressions are simplified to eliminate
       TRUE/FALSE.
 REWRITE-FLAG: If RL(LR) then only lhs(rhs) is simplified.
 FLUSH?: If T, then the current asserted facts are deleted for
         efficiency.
 LINEAR?: If T, then multiplication and division are uninterpreted.
 CASES-REWRITE: If T, then the selections and else parts of a
         CASES expression are simplified, otherwise, they are only
         simplified when simplification selects a case.
 TYPE-CONSTRAINTS?: IF T (the default) asserts subtype constraints of each
         sub-expression to the ground prover data-structures.
 IGNORE-PROVER-OUTPUT?: The ground prover returns a disjunction of literals
         equivalent to the asserted formula.  At the end of a SIMPLIFY with
         RECORD? set to T, PVS tries to show that the conjunction of these
         disjunctions is unsatisfiable using the ground prover.  This step
         can sometimes be expensive and fruitless, and setting
         IGNORE-PROVER-OUTPUT? to T, cases this step to be skipped.
 ASSERT-CONNECTIVES?: asserting formulas with connectives, i.e.,
         IF-THEN-ELSEs, AND, OR, IMPLIES, and update expressions can be
         expensive for the ground prover to process.  When the
         ASSERT-CONNECTIVES? flag is T, such formulas are not
         asserted to the ground prover." 
	 "~%Simplifying with decision procedures,")

(defstep assert  (&optional (fnums *) rewrite-flag
			    flush? linear? cases-rewrite? (type-constraints? T)
			    ignore-prover-output?
			    (assert-connectives? T)) 
	 (simplify
	  fnums T T rewrite-flag flush? linear? cases-rewrite? type-constraints? ignore-prover-output? assert-connectives?)
 "Simplifies/rewrites/records formulas in FNUMS using decision
procedures.  Variant of SIMPLIFY with RECORD? and REWRITE? flags set
to T. If REWRITE-FLAG is RL(LR) then only lhs(rhs) of equality
is simplified. If FLUSH? is T then the current asserted facts are
deleted for efficiency.  If LINEAR? is T, then multiplication and
division are uninterpreted.  If CASES-REWRITE? is T, then
the selections and else parts of a CASES expression are simplified,
otherwise, they are only simplified when simplification selects
a case.  See also SIMPLIFY, RECORD, DO-REWRITE.
Examples:
 (assert):  Simplifies, rewrites, and records all formulas.
 (assert -1 :rewrite-flag RL): Apply assert to formula -1 leaving
    RHS untouched if the formula is an equality.
 (assert :flush? T :linear? T): Apply assert with fully uninterpreted
  nonlinear arithmetic after flushing existing decision procedure
  database."
"Simplifying, rewriting, and recording with decision procedures")

(defstep record (&optional (fnums *) rewrite-flag
			    flush? linear? (type-constraints? T)
			    ignore-prover-output?
			    (assert-connectives? T))
	 (simplify
	  fnums T NIL rewrite-flag flush? linear? type-constraints?
	  ignore-prover-output?
	  assert-connectives?)
	 "Uses decision procedures to simplify and record the formulas
in FNUMS for further simplification.   Variant of SIMPLIFY with RECORD?
flag set to T and REWRITE? flags set to NIL. If REWRITE-FLAG is
RL(LR) then only lhs(rhs) is simplified.  If FLUSH? is T then the
current asserted facts are deleted for efficiency.  If LINEAR? is T,
then multiplication and division are uninterpreted.  Example:
 (record - :flush? T): flushes database and records antecedent formulas."
	 "Simplifying and recording with decision procedures")

(defun invoke-simplification (sformnums record? rewrite?
				   rewrite-flag flush? linear?
				   cases-rewrite? type-constraints?
				   ignore-prover-output?
				   assert-connectives?)
  #'(lambda (ps)
      (let ((*cases-rewrite* cases-rewrite?)
	    (*assert-connectives?* assert-connectives?)
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
		 T  type-constraints? ps))))))

(defun sequent-reduce-around (sequent simplifier sformnums)
  (multiple-value-bind (signal newsequent)
      (sequent-reduce sequent simplifier sformnums)
    (cond ((eq signal '!)
	   (values signal newsequent))
	  ((or (memq *assert-flag* '(simplify rewrite))
	       *ignore-prover-output?*)
	   (values signal newsequent))
	  (t (assert-process-output signal newsequent)))))

(defun assert-sform* (sform &optional rewrite-flag simplifiable?)
  (let* ((fmla (formula sform))
	 (sign (not (not-expr? fmla)))
	 (body (if sign fmla (args1 fmla)))
	 (*bound-variables* nil)
	 (*top-rewrite-hash* *rewrite-hash*)
	 (*top-findalist* findalist)
	 (*top-usealist* usealist)
	 (*top-sigalist* sigalist))
    (cond (rewrite-flag
	   (multiple-value-bind (sig newbodypart)
	       (if (or (iff-expr? body)(equality? body))
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
			  (make-arg-tuple-expr
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
	       (assert-if fmla) ;;NSH(5.1.96) was assert-if-inside
	     (cond ((eq sig 'X)
		    (if (or (and sign (tc-eq fmla *false*))
			    (and (not sign)(tc-eq body *true*)))
			(values '? nil)
			(values 'X sform)))
		   ((and (not (eq *assert-flag* 'simplify))
			 (not (connective-occurs? newfmla)))
		           ;;not check-for-connectives.
		    (process-sform sform newfmla sig))
		   (t (values '? (copy sform 'formula newfmla))))))
	  (t (multiple-value-bind (sig newfmla)
		 (assert-if-inside fmla)
	       (if (memq *assert-flag* '(simplify rewrite))
		   (values sig (if (eq sig '?) (copy sform
						 'formula newfmla)
				   sform))
		   (process-sform sform
				  (if (eq sig '?) newfmla fmla)
				  sig)))))))

(defun record-type-constraints (expr)
  (unless (or *assert-typepreds-off*
	      (connective-occurs? expr));;not check-for-connectives?
    (let ((constraints (collect-type-constraints expr)))
      (when (and *subtype-hash* constraints)
	(setq *assert-typepreds* (nconc constraints
					*assert-typepreds*))
	(setf (pvs-gethash expr *subtype-hash*) T)))))

(defun check-update-args (update-args args &optional in-beta-reduce?)
  (if (null update-args) 'TRUE
      (let* ((uarg1 (car update-args))
	     (arg1 (car args))
	     (equality (make-equality uarg1 arg1))
	     (result (if in-beta-reduce?
			 (if (tc-eq uarg1 arg1)
			     'TRUE
			     (if (or (and (number-expr? uarg1)
					  (number-expr? arg1))
				     (and (scalar-constant? uarg1)
					  (scalar-constant? arg1)))
				 'FALSE  ;;NSH(11.23.94)
			     'NOIDEA))
			 (if (connective-occurs? equality)
			     'NOIDEA
			     (multiple-value-bind
				 (sig newresult)
				 (assert-equality equality (list uarg1 arg1) 'X)
			     (assert-test newresult))
			     ))))
	(cond ((true-p result)
	       (check-update-args (cdr update-args)(cdr args)))
	      ((false-p result) 'FALSE)
	      (t 'NOIDEA)))))

(defun make-sum (list type)
  (if (check-for-connectives? list)
      (make-sum* list type)
      (make-sum* (sort list #'(lambda (x y)
				(arithord (top-translate-to-prove x)
					  (top-translate-to-prove y))))
		 type)))

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

(defmethod lazy-assert-if-with-subst ((expr if-expr) subst &optional if-flag)
  (cond ((not (branch? expr))
	 (assert-if (substit expr subst)))
	(t (let ((newtest
		  (assert-if-simplify (substit (condition expr) subst))))
	   ;;check if assert-if-simplify is needed.  Why another assert-test
	   ;;below.  
	   (cond ((check-for-connectives? newtest)
		  (if if-flag
		      (values 'X expr);;expr is irrelevant
		      (values '? (substit expr subst))) )
		 (t (let ((result newtest))  ;;instead of (assert-test newtest)
		      (cond ((tc-eq result *true*)
			    (multiple-value-bind
				   (sigthen newthen)
				 (lazy-assert-if-with-subst (then-part expr)
							    subst)
				  (values-assert-if '? newthen expr)))
			    ((tc-eq result *false*)
			     (multiple-value-bind
				   (sigelse newelse)
				 (lazy-assert-if-with-subst (else-part expr)
							    subst)
			       (values-assert-if  '? newelse expr)))
			    (if-flag (values 'X expr))
			    (t (values '? (substit expr subst)))))))))))

(defmethod lazy-assert-if ((expr if-expr))
  (cond ((not (branch? expr))
	 (call-next-method expr))
	(t
	 (let ((newtest (assert-if-simplify (condition expr))))
	   ;;check if assert-if-simplify is needed.  Why another assert-test
	   ;;below.  
	   (cond ((check-for-connectives? newtest)
		  (values 'X expr))
		 (t (let ((result newtest))  ;;instead of (assert-test newtest)
		      (cond ((tc-eq result *true*)
			    (multiple-value-bind
				   (sigthen newthen)
				 (lazy-assert-if (then-part expr))
				  (values-assert-if '? newthen expr)))
			    ((tc-eq result *false*)
			     (multiple-value-bind
				   (sigelse newelse)
				 (lazy-assert-if (else-part expr))
				   (values-assert-if  '? newelse expr)))
			    (t (values 'X expr))))))))))

(defun record-type-constraints (expr)
  (unless (or *assert-typepreds-off*
	      (check-for-connectives? expr))
    (let ((constraints (collect-type-constraints expr)))
      (when (and *subtype-hash* constraints)
	(setq *assert-typepreds* (nconc constraints
					*assert-typepreds*))
	(setf (pvs-gethash expr *subtype-hash*) T)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  The NRL/PVS 2.2 extensions for Myla Archer go here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun label-step (label fnums push?)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (fnums (if (consp fnums) fnums (list fnums))))
	(cond ((or (stringp label)(symbolp label))
	       (multiple-value-bind
		   (signal subgoal)
		   (sequent-reduce goalsequent
				   #'(lambda (sform)
				       (values '?
					       (if (and push? label)
						   (lcopy sform
						     'label
						     (cons (intern label)
							   (label sform)))
					       (lcopy sform
						 'label
						 (when label 
						 (list (intern label)))))))
				   fnums)
		 (values signal (list subgoal);;(substitution ps)
			 )))
	      (t (format-if "~%Label ~a is not a string." label)
		 (values 'X nil nil))))))

(addrule 'label (label fnums) (push?)  (label-step label fnums push?)
	 "Labels a collection of formulas given by FNUMS by the
string  LABEL.  If PUSH? is T, then the new label is added to any existing
ones.  Otherwise, the new labels replaces all existing ones."
	 "Labelling formulas ~a by ~a")



(defun display-sform (sform sfnum stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (cond (*report-mode*
	   (unless (and (memq sform par-sforms)
			(every #'(lambda (ps)
				   (memq sform
					 (s-forms (current-goal ps))))
			       *print-descendants*))
	     (format stream "~%~V@T" *prover-indent*)
	     (format stream "{~a}   ~a" sfnum
		     (unparse-sform sform))))
	  (t 
	   (format stream "~%~V@T" *prover-indent*)
	   (let ((old (memq sform par-sforms)))
	     (format stream "~a~a" (if old "[" "{") sfnum)
	     (when (label sform) (format stream "~{,~a~}"
				(label sform))) 
	     (format stream "~a" (if old "]" "}"))
	     (if (label sform)
		 (format stream "~%   ~a" (unparse-sform sform))
		 (format stream "   ~a" (unparse-sform sform))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decomposing flatten

(addrule 'flatten-disjunct () (fnums depth) (flatten fnums depth)
 "Disjunctively simplifies chosen formulas.  It simplifies 
top-level antecedent conjunctions, equivalences, and negations, and
succedent disjunctions, implications, and negations from the sequent.
The DEPTH argument can either be a non-negative integer indicating
the nesting of top-level disjunctions to be elimnated, or NIL
which eliminates all top-level disjuncts in the indicated FNUMS."
 "~%Applying disjunctive simplification to flatten sequent,")

(setf (gethash 'flatten *rulebase*) nil)

(defstep flatten (&rest fnums) (flatten-disjunct fnums nil)
 "Disjunctively simplifies chosen formulas.  It simplifies 
top-level antecedent conjunctions, equivalences, and negations, and
succedent disjunctions, implications, and negations from the sequent."
 "Applying disjunctive simplification to flatten sequent")

(defun flatten (sformnums  depth)
  #'(lambda (ps)
      (let ((sformnums (if (null sformnums) '*       ;;NSH(10.18.94)
			   (if (consp sformnums)
			       (loop for sn in sformnums append ;;was nconc
				     (if (consp sn) sn (list sn)))
			       (list sformnums)))))
	(multiple-value-bind (signal subgoal)
	    (sequent-reduce
	     (current-goal ps) #'(lambda (sf)
				   (simplify-disjunct-sform sf depth))
	     sformnums)
	  (values signal (list subgoal))))))

(defun simplify-disjunct-sform (sform depth)
  (let ((new-sforms
	 (loop for x in (simplify-disjunct (formula sform) depth)
	       collect (lcopy sform 'formula x))))
    (if (and (= (length new-sforms) 1)
	     (s-form-equal? sform (car new-sforms)))
	(values 'X sform)
      (values '? new-sforms))))

(defun simplify-disjunct (formula &optional depth)
  (if (and (integerp depth)
	   (zerop depth))
      (list formula)
      (let ((depth (if (integerp depth)
		       (1- depth)
		       depth)))
	(if (and (typep formula 'application)
		 (typep (operator formula) 'name-expr))
	    (cond ((disjunction? formula)
		   (loop for arg in (arguments formula)
			 nconc (simplify-disjunct arg depth)))
		  ((implication? formula)
		   (nconc (simplify-disjunct (negate (args1 formula)) depth)
			  (simplify-disjunct (args2 formula) depth)))
		  ((inequality? formula)
		   (list (negate (make-equality (args1 formula)(args2 formula)))))
		  ((negation? formula)
		   (let ((arg (args1 formula)))
		     (if (and (typep arg 'application)
			      (typep (operator arg) 'name-expr))
			 (cond ((conjunction? arg)
				(loop for argum in (arguments arg)
				      nconc (simplify-disjunct (negate argum)
							       depth)))
			       ((inequality? arg)
				(list (make-equality (args1 arg)(args2 arg))))
			       ((negation? arg)
				(simplify-disjunct (args1 arg) depth))
			       ((ifff? arg)
				(nconc
				 (simplify-disjunct
				  (negate
				   (make-implication
				    (args1 arg)(args2 arg)))
				  depth)
				 (simplify-disjunct
				  (negate
				   (make-implication
				    (args2 arg)(args1 arg)))
				  depth)))
			       (t (list formula)))
			 (list formula))))
		  (t (list formula)))
	    (list formula)))))

(addrule 'split () ((fnum *) depth) ;; labels
	 (split-rule-fun fnum depth)
 "Conjunctively splits formula FNUM.  If FNUM is -, + or *, then
the first conjunctive sequent formula is chosen from the antecedent,
succedent, or the entire sequent.  Splitting eliminates any
top-level conjunction, i.e., positive AND, IFF, or IF-THEN-ELSE, and
negative OR, IMPLIES, or IF-THEN-ELSE."
"~%Splitting conjunctions,")

(defun split-rule-fun (sformnum &optional depth labels)
  #'(lambda (ps) (split-step sformnum ps depth labels)))

(defun and+ (form &optional depth)
  (if (and (integerp depth)
	   (zerop depth))
      (list form)
      (if (and (typep form 'application)
	       (typep (operator form) 'name-expr))
	  (let ((args (arguments form))
		(depth (if (integerp depth)
			   (1- depth)
			   depth)))
	    (cond  
	     ((conjunction? form)
	      (loop for conjunct in args
		    append (and+ conjunct depth)))
	     ((ifff? form)
	      (list (make-implication (car args)(cadr args))
		    (make-implication (cadr args)(car args))))
	     ((negation? form)
	      (if (and (typep (car args) 'application)
		       (typep (operator (car args)) 'name-expr))
		  (let ((args2 (arguments (car args))))
		    (cond  
		     ((implication? (car args))
		      (append (and+ (negate
				     (cadr args2)) depth)
			      (and+ (car args2) depth)))
		     ((disjunction? (car args))
		      (append (and+ (negate
				     (car args2)) depth)
			      (and+ (negate
				     (cadr args2)) depth)))
		     ((negation? (car args))
		      (and+ (car args2) depth))
		     ((cases? (car args))
		      (and+ (negate (translate-cases-to-if (car args))) depth))
		     ((branch? (car args))
		      (list (negate (make-conjunction
				     (list (condition (car args))
					   (then-part (car args)))))
			    (negate
			     (make-conjunction
			      (list (negate (condition (car args)))
				    (else-part (car args)))))))
		     (t (list form))))
		  (list form)))
	     ((cases? form)
	      (and+ (translate-cases-to-if form) depth))
	     ((branch? form)
	      (list (make-implication (condition form)(then-part form))
		    (make-implication (negate (condition form))(else-part form))))
	     (t (list form))))
	  (list form))))


(defun split-step (sformnum ps depth labels)
  (let* ((goalsequent (current-goal ps))
	 (sformnum (find-sform (s-forms goalsequent) sformnum
			       #'(lambda (sf)(and+form? (formula sf)))))
	 (selected-sform (select-seq (s-forms goalsequent) (list sformnum))))
    ;;    (format t "~%conjunct = ~a" (formula selected-sform))
    (if (or (null selected-sform)
	    (not (and+form? (formula (car selected-sform))))
	    (eql depth 0))
	(values 'X nil nil)
	(let* ((sel-sform (car selected-sform))
	       (new-sforms (delete-seq (s-forms goalsequent) (list sformnum)))
	       (conjuncts (and+ (formula sel-sform) depth))
	       (conjunct-sforms
		(mapcar #'(lambda (x)
			    (copy sel-sform
				  'formula x))
			conjuncts))
	       (labelled-conjunct-sforms
		(label-sforms conjunct-sforms labels))
	       (new-sequents
		(mapcar #'(lambda (x)
			    (copy goalsequent
				  's-forms
				  (cons x new-sforms)))
			labelled-conjunct-sforms)))
	  (values '? new-sequents;;(substitution ps)
		  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(3.16.97): checkpointing of proofs.

;;NSH(3.16.97): adding just-install-proof rule to install a subproof
;;on a subgoal without actually running it.  The proof is then marked
;;unfinished.

(addrule 'just-install-proof (proof) ()
	 #'(lambda (ps)
	     (just-install-proof-step proof ps))
	 "Installs an edited PROOF without actually checking it,
declares the subgoal as finished, but then marks the proof as
unfinished."
	 "Installing without checking, the proof ~a")

(defun just-install-proof-step (proof ps)
  (progn (setq *context-modified* T)
	 (values '! nil (list 'justification
			      (make-instance 'justification
				'label (label-suffix (label ps))
				'rule `(rerun ,proof))))))

(defstrat checkpoint ()
  (query*)
  "A synonym for (query*): inserting (checkpoint) an edited proof and
 rerunning it causes the non-checkpointed subproofs to be installed
 (using JUST-INSTALL-PROOF) so that the proof is only run up to the
checkpoint.  "
 " ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strat-eval (strat)
  (cond ((typep strat 'strategy) strat)
	((null strat) (get-rule '(skip) *ps*))
	((not (consp strat))
	 (format-if "~%Ill-formed rule/strategy: ~s " strat)
	 (get-rule '(skip) *ps*))
	((quote? strat)(strat-eval (cadr strat)))
	((if-form? strat);;(break "if")
	 (if (expr-eval (cadr strat))
	     (strat-eval (caddr strat))
	     (strat-eval (cadddr strat))))
	((try-form? strat)
	 (make-instance 'strategy
	   'topstep (strat-eval (cond-expr strat))
	   'subgoal-strategy (then-expr strat)
	   'failure-strategy (else-expr strat)))
	((let-form? strat)
	 (let ((let-value (let-eval (let-bindings strat))))
	   (strat-eval (subst-stratexpr
			     (let-body strat)
			     let-value
			     (reverse let-value)
			     ))))
	((eq (car strat) 'note)
	 ;;if this evaluates to a rule, then the input is noted with
	 ;;comment string which becomes part of the printout.
	 (let ((result
		(strat-eval (cadr strat))))
	   (when (typep result 'rule-instance)
	     (setf (rule-input result) strat
		   (rule-format result)
		   (if (cddr strat)
		       (cons (format nil "~%(~a)~a" (caddr strat)
				     (car (rule-format result)))
			     (cdr (rule-format result)))
		       (rule-format result))))
	   result))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (pair-formals-args (formals def)
					       (cdr strat)))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect
			    (if (consp x) ;;NHS(4.23.97)
				;;was ignoring args, otherwise.
				(cdr (assoc (car x) subalist))
				(cdr (assoc x subalist)))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr;;2/91:so that rules are APPLYed.
		 `(apply ,def-expr))
		(result (strat-eval new-def-expr)))
	   (setf (rule-input result)
		 strat
		 (rule-format result)
		 (when (format-string (rule-definition (car strat)))
		   (cons (format-string (rule-definition (car strat))) args)
		   ))
	   result))
	((step-definition (car strat))
	 (let* ((def (step-definition (car strat)))
		(alist (pair-formals-args (formals def)
					       (cdr strat)))
		(def-expr  (subst-stratexpr
			    (defn def)
			    alist
			    (reverse alist)
			    )))
	   (strat-eval def-expr)))
	((primitive-rule (car strat))
	 (get-rule strat
		   *ps*))
	(t (format-if "~%Ill-formed rule/strategy: ~s " strat)
	   (get-rule '(skip) *ps*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(4.3.97): extending operations to use labels.

(defun cleanup-fnums (fnums)
  (cond ((consp fnums)(loop for fnum in fnums
			    collect (if (stringp fnum)
					(intern fnum)
					fnum)))
	((stringp fnums) (list (intern fnums)))
	((memq fnums '(* + -)) fnums)
	(t (list fnums))))

(defmacro in-sformnums? (sform pos neg sformnums)
  `(let ((sign (not (not-expr? (formula ,sform))))
	 (sformnums (cleanup-fnums ,sformnums)))
    (cond ((eql ,sformnums '*) T)
	((eql ,sformnums '+) sign)
	((eql ,sformnums '-) (not sign))
	((eql ,sformnums ,pos) sign)
	((eql ,sformnums ,neg) (not sign))
	((and (label ,sform) (memq ,sformnums (label ,sform))) T)
	((consp ,sformnums)
	 (cond ((memq '* ,sformnums) T)
	       ((memq '+ ,sformnums) sign)
	       ((memq '- ,sformnums) (not sign))
	       ((and (label ,sform)(intersection (label ,sform) ,sformnums)))
	       ((consp (car ,sformnums))
		(if sign (member ,pos (car ,sformnums))
		      (member ,neg (car ,sformnums))))
	       (t (if sign (member ,pos ,sformnums)
		      (member ,neg ,sformnums)))))
	(t nil))))

(defun select-seq1 (seq nums pos neg)
       (if (consp seq)
	   (if (not-expr? (formula (car seq)))
	       (if (or (memq neg nums)
		       (and (label (car seq))
			    (intersection (label (car seq))
					  nums)))
		   (cons (car seq)
			 (select-seq1 (cdr seq) nums pos (1- neg)))
		   (select-seq1 (cdr seq) nums pos (1- neg)))
	       (if (or (memq pos nums)
		       (and (label (car seq))
			    (intersection (label (car seq))
					  nums)))
		   (cons (car seq)
			 (select-seq1 (cdr seq) nums (1+ pos) neg))
		   (select-seq1 (cdr seq) nums (1+ pos) neg)))
	   nil))

(defun delete-seq1 (seq nums pos neg)
       (if (consp seq)
	   (if (not-expr? (formula (car seq)))
	       (if (or (memq neg nums)
		       (and (label (car seq))
			    (intersection (label (car seq))
					  nums)))
		   (delete-seq1 (cdr seq) nums pos (1- neg))
		   (cons (car seq)
			 (delete-seq1 (cdr seq) nums pos (1- neg))))
	       (if (or (memq pos nums)
		       (and (label (car seq))
			    (intersection (label (car seq))
					  nums)))
		   (delete-seq1 (cdr seq) nums (1+ pos) neg)
		   (cons (car seq)
			 (delete-seq1 (cdr seq) nums (1+ pos) neg))))
	   nil))

(defun select-seq (seq nums)
  (let ((nums (cleanup-fnums nums)));;NSH(4.3.97)
    (cond ((eq nums '*) seq)
	  ((eq nums '+) (loop for sform in seq
			      when (not (not-expr? (formula sform)))
			      collect sform))
	  ((eq nums '-) (loop for sform in seq
			      when (not-expr? (formula sform))
			      collect sform))
	  ((consp nums)(select-seq1 seq nums +1 -1))
	  (t NIL))));;NSH(2.24.97): added consp check or memq breaks
(defun delete-seq (seq nums)
  (let ((nums (cleanup-fnums nums)));;NSH(4.3.97)
    (cond ((eq nums '*) NIL)
	  ((eq nums '-) (loop for sform in seq
			      when (not (not-expr? (formula sform)))
			      collect sform))
	  ((eq nums '+) (loop for sform in seq
			      when (not-expr? (formula sform))
			      collect sform))
	  ((consp nums) (delete-seq1 seq nums +1 -1))
	  (t NIL))))

(defun gather-seq (seq yesnums nonums
		       &optional (pred #'(lambda (x) T))
		       (pos 1) (neg -1))
  (let ((yesnums (cleanup-fnums yesnums))
	(nonums (cleanup-fnums nonums)))
    (gather-seq* seq yesnums nonums pred pos neg)))

(defun gather-seq* (seq yesnums nonums
		       pred pos neg)
   (cond ((null seq) nil)
	 ((not-expr? (formula (car seq)))
	  (if (and (in-sformnums? (car seq) pos neg yesnums)
		   (not (in-sformnums? (car seq) pos neg nonums))
		   (funcall pred (car seq)))
	      (cons (car seq)
		    (gather-seq* (cdr seq) yesnums nonums pred
				pos (1- neg)))
	      (gather-seq* (cdr seq) yesnums nonums pred pos (1- neg))))
	 (t (if (and (in-sformnums? (car seq) pos neg yesnums)
		     (not (in-sformnums? (car seq) pos neg nonums))
		     (funcall pred (car seq)))
	      (cons (car seq)
		    (gather-seq* (cdr seq) yesnums nonums
				pred (1+ pos) neg))
	      (gather-seq* (cdr seq) yesnums nonums pred
			  (1+ pos)  neg)))))

(defun gather-fnums (sforms yesnums nonums
		       &optional (pred #'(lambda (x) T))
		       (pos 1) (neg -1))
  (let ((yesnums (cleanup-fnums yesnums))
	(nonums (cleanup-fnums nonums)))
    (gather-fnums* sforms yesnums nonums pred pos neg)))

(defun gather-fnums* (seq yesnums nonums
		       pred pos neg)
   (cond ((null seq) nil)
	 ((not-expr? (formula (car seq)))
	  (if (and (in-sformnums? (car seq) pos neg yesnums)
		   (not (in-sformnums? (car seq) pos neg nonums))
		   (funcall pred (car seq)))
	      (cons neg
		    (gather-fnums* (cdr seq) yesnums nonums pred
				pos (1- neg)))
	      (gather-fnums* (cdr seq) yesnums nonums pred pos (1- neg)))) 
	 (t (if (and (in-sformnums? (car seq) pos neg yesnums)
		     (not (in-sformnums? (car seq) pos neg nonums))
		     (funcall pred (car seq)))
	      (cons pos
		    (gather-fnums* (cdr seq) yesnums nonums
				pred (1+ pos) neg))
	      (gather-fnums* (cdr seq) yesnums nonums pred
			  (1+ pos)  neg)))))

(defun find-all-sformnums (sforms sformnums pred
				  &optional (pos 1)(neg -1)(acc nil))
  (cond ((null sforms) (nreverse acc))
	(t (let* ((sign (not (not-expr? (formula (car sforms)))))
		  (newpos (if sign (1+ pos) pos))
		  (newneg (if sign neg (1- neg)))
		  (newacc (if (and (in-sformnums?
				    (car sforms) pos neg sformnums)
				   (funcall pred (formula (car sforms))))
			      (cons (if sign pos neg) acc)
			      acc)))  ;;(break "find-all")
		 (find-all-sformnums (cdr sforms) sformnums pred
				     newpos newneg newacc)))))
				     
(defun find-sform (sforms sformnum &optional (pred #'(lambda (x) T)))
  (find-sform* sforms sformnum pred
		      1 -1))

(defun find-sform* (sforms sformnum pred pos neg)
  (cond ((null sforms) nil)
	((not-expr? (formula (car sforms)))
	 (if (and (or (memq sformnum '(* -))
		      (equal sformnum neg)
		      (and (label (car sforms))
			   (or (symbolp sformnum)
				 (stringp sformnum))
			   (memq (intern sformnum)
			       (label (car sforms)))))
		  (funcall pred  (car sforms)))
	     neg
	     (find-sform* (cdr sforms) sformnum pred pos (1- neg))))
	(t (if (and (or (memq sformnum '(* +))
			(equal sformnum pos)
			(and (label (car sforms))
			     (or (symbolp sformnum)
				 (stringp sformnum))
			     (memq (intern sformnum)
				   (label (car sforms)))))
		  (funcall pred (car sforms)))
	       pos
	       (find-sform* (cdr sforms) sformnum pred (1+ pos) neg)))))	     
	
(defun find-remaining-sformnums (sforms sformnums sub-sformnums
				  &optional (pos 1)(neg -1)(acc nil))
  (cond ((null sforms) (nreverse acc))
	(t (let* ((sign (not (not-expr? (formula (car sforms)))))
		  (newpos (if sign (1+ pos) pos))
		  (newneg (if sign neg (1- neg)))
		  (newacc (if (and (in-sformnums?
				    (car sforms) pos neg sformnums)
				   (not (in-sformnums? (car sforms)
						       pos
						       neg
						       sub-sformnums)))
			      (cons (if sign pos neg) acc)
			      acc)))
		 (find-remaining-sformnums (cdr sforms) sformnums
					   sub-sformnums
				     newpos newneg newacc)))))

(defun expand-sforms (name sforms sformnums occurrence 
			   pos neg accum)
  (if (null sforms)
      (nreverse accum)
      (let* ((*count-occurrences* 0)
	     (fmla (formula (car sforms)))
	     (new-fmla
	      (if (in-sformnums? (car sforms) pos neg sformnums)
		  (expand-defn name fmla occurrence)
		  fmla))
	     )
	(if (eq fmla new-fmla)
	    (expand-sforms name (cdr sforms)
			   sformnums occurrence
			   (if (not (not-expr? fmla))
			       (1+ pos)
			       pos)
			   (if (not-expr? fmla)
			       (1- neg)
			       neg)
			   (cons (car sforms) accum))
	    (multiple-value-bind (sig result)
		(if (eq *assert-flag* 'none)
		    (values 'X new-fmla) ;;NSH(1/16/97): Myla Archer
		        ;;wanted no-simplification option.
		    (assert-if-inside new-fmla))
	      (let ((new-sform (lcopy (car sforms)
				 'formula (if (eq sig 'X)
					      new-fmla
					      result))))
		(if occurrence
		    (append (nreverse accum)
			    (cons new-sform (cdr sforms)))
		    ;;since only one formula is
		      ;;processed when occurrence is
			;;given.  
		  (expand-sforms name (cdr sforms)
				 sformnums occurrence
				 (if (not (not-expr? fmla))
				     (1+ pos)
				     pos)
				 (if (not-expr? fmla)
				     (1- neg)
				     neg)
				 (cons new-sform accum)))))))))

(defun sform-reduce (sformlist simplifier sformnums pos neg)
  (if (null sformlist) (values 'X NIL)
    (let* ((x (car sformlist))
	   (sign (not (not-expr? (formula x)))))
      (multiple-value-bind
	    (signal result)
	  (if (in-sformnums? x pos neg sformnums)
	      (funcall simplifier x)
	      (values 'X x));;no change
	(let ((result (if (listp result) result (list result))))
	  (cond
	    ((eq signal '!) '!)
	    (t (multiple-value-bind
		     (rsignal rresult)
		   (sform-reduce (cdr sformlist) simplifier sformnums
				 (if sign (1+ pos) pos)
				 (if sign neg (1- neg)))
		 (values (if (eq signal 'X);;compute result signal
			     rsignal
			     (if (eq rsignal '!)
				 '!
				 signal))
		    ;;compute result sforms
			 (nconc (delete-if
				 #'(lambda (x)
				     (with-slots (formula)
					 x
				       (or
					(tc-eq formula
					       *false*)
					(and (not-expr?
					      formula)
					     (tc-eq (args1
						     formula)
						    *true*)))))
				     result)
			       rresult))))))))))

(defun replace-loop (lhs rhs sformnum sformnums sforms pos neg)
  (let ((*replace-cache*
	 (make-hash-table :test #'eq)))
    (if (null sforms) nil
	(if (not-expr? (formula (car sforms)))
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

(defun nth-or-last (n list)
  (if (< n (length list))
      (nth n list)
      (if (consp list)
	  (car (last list))
	  list)))

(defhelper label-fnums (labels fnums &optional push?)
  (if (consp labels)
      (if (consp (cdr labels))
	  (if (consp fnums)
	      (let ((label (car labels))
		    (cdr-labels (cdr labels))
		    (fnum (car fnums))
		    (cdr-fnums (cdr fnums)))
		(then (label label fnum :push? push?)
		      (label-fnums cdr-labels cdr-fnums :push? push?)))
	      (skip))
	  (let ((label (car labels)))
	    (label label fnums)))
      (label labels fnums :push? push?))
  "Labels a list of formulas from FNUMS with corresponding labels
taken from LABELS so that the last label is applied to any
remaining fnums.  If PUSH? is T, the new label is added to existing
labels, otherwise, the old labels are deleted."
  "Labelling some formulas")

(defstep with-labels (rule labels &optional push?)
  (then rule
	(let ((fnums *new-fmla-nums*)
	      (labels (if (consp labels) labels (list labels)))
	      (current-labels
	       (nth-or-last (subgoalnum *ps*) labels)))
	  (label-fnums current-labels fnums :push? push?)))
  "If RULE generates subgoal sequents S1...Sn where each Si has
new formulas, i.e., those numbered with {}, numbered fi1..fim, then
if LABELS is a list of list of labels ((l11 ... l1k)...(ln1...lnm)),
then each formula fij is labelled with label lij.  As usual, any of
the lists can be replaced a single label, and if there are not enough
labels in a list, then the last label is applied to the remaining
fnums."
  "Applying ~a and labelling new subgoal formulas with ~a")


(defun format-printout (ps &optional quiet-flag)
  (let ((pp (printout ps)))
    (when (and pp
	       (or quiet-flag
		   (not *suppress-printing*)))
      (let ((pp (if (consp pp)
		    (apply #'format nil
			   (car pp)
			   (mapcar #'(lambda (x)
				       (if (stringp x)
					   (protect-format-string x)
					   x))
				   (cdr pp)))
		    pp)))   ;;NSH(12/5/97: was (format-if pp) broke
	                   ;;on strings with ~
	(if quiet-flag pp (format-if "~a" pp))))))

(defun call-show-proof ()
  (if (and *in-checker* *ps*)
      (pvs-buffer "*Proof*"
	(with-output-to-string (*standard-output*)
	  (write (editable-justification
		  (collect-justification *top-proofstate*))
		 :stream *standard-output* :pretty t :escape t
		 :level nil :length nil
		 :pprint-dispatch *proof-script-pprint-dispatch*))
	t t)
      (pvs-message "No proof is currently running")))

(eval-when (eval compile load)
(defmacro defcl* (name classes &rest args)
  (let ((cl (macroexpand `(defcl ,name ,classes ,@args))))
    (eval (second cl))
    (eval (sixth cl))   ;; updates *slot-info*
    (append cl
	    (generate-defcl-methods (list name))
	    (generate-update-fetched-methods (list name)))))

(defvar *classes-done* nil)
(defvar *methods-collected* nil)

(defun generate-defcl-methods (names)
  (let ((*classes-done* nil)
	(*methods-collected* nil))
    (generate-defcl-methods* names)
    *methods-collected*))

(defun generate-defcl-methods* (names)
  (when names
    (let* ((name (car names))
	   (class (find-class name)))
      (unless (memq name *classes-done*)
	(push name *classes-done*)
	(setq *methods-collected*
	      (nconc *methods-collected*
		     (list (generate-copy-method name)
			   (generate-store-object*-method name)
			   ;;(generate-update-fetched-method name)
			   )))
	(generate-defcl-methods* (mapcar #'class-name
				   (class-direct-subclasses class)))))
    (generate-defcl-methods* (cdr names))))

(defun generate-update-fetched-methods (names)
  (let ((*classes-done* nil)
	(*methods-collected* nil))
    (generate-update-fetched-methods* names)
    (nreverse *methods-collected*)))

(defun generate-update-fetched-methods* (names)
  (when names
    (let* ((name (car names))
	   (class (find-class name)))
      (unless (memq name *classes-done*)
	(push name *classes-done*)
	(push (generate-update-fetched-method name) *methods-collected*)
	(generate-update-fetched-methods* (mapcar #'class-name
					    (class-direct-subclasses class)))))
    (generate-update-fetched-methods* (cdr names))))

(defun generate-copy-method (name)
  (let* ((slots (get-all-slots-of (list name)))
	 (unignored-slots (mapcar #'car (unignored-slots% slots))))
    `(defmethod copy ((obj ,name) &rest initargs)
       (with-slots ,unignored-slots obj
	 (make-instance ',name
	   ,@(mapcan #'(lambda (sl)
			 `(',(car sl)
			   (let ((getfv (getf initargs ',(car sl)
					      '%nogetf)))
			     (if (eq getfv '%nogetf)
				 ,(if (ignored-slot% sl)
				      (getf (cdr sl) :initform)
				      (car sl))
				 getfv))))
	       slots))))))

(defun generate-store-object*-method (name)
  (let* ((slots (get-all-slots-of (list name)))
	 (saved-slots (saved-slots% slots)))
    `(defmethod store-object* ((obj ,name))
       (reserve-space ,(1+ (length saved-slots))
	 (with-slots ,(mapcar #'car saved-slots) obj
	   (push-word (store-obj ',name))
	   ,@(mapcar #'(lambda (a)
			 `(push-word (store-obj ,(car a))))
	       saved-slots))))))

(defun generate-update-fetched-method (name)
  (let* ((slots (get-all-slots-of (list name)))
	 (saved-slots (saved-slots% slots))
	 (unsaved-slots (unsaved-slots% slots)))
    `(defmethod update-fetched ((obj ,name))
       (with-slots (,@(mapcar #'car saved-slots)
		      ,@(mapcar #'car unsaved-slots)) obj
	 ,@(let ((arg-setters nil))
	     (dotimes (i (length saved-slots))
	       (let ((a (nth i saved-slots)))
		 (push `(setf ,(car a)
			      (fetch-obj (stored-word ,(1+ i))))
		       arg-setters)))
	     (dolist (a unsaved-slots)
	       (push `(setf ,(car a)
			    ,(getf (cdr a) :fetch-as))
		     arg-setters))
	     (nreverse arg-setters))))))
)

(defun make-update (field value ps)
  ;;(break)
  (case field
    (status-flag (setf (status-flag ps) value))
    (strategy (setf (strategy ps) value))
    (context (setf (context ps) value))
;    (out-context (setf (out-context ps) value))
;    (out-substitution (setf (out-substitution ps) value))
    (alists (setf (alists ps) value))
    (current-auto-rewrites (setf (current-auto-rewrites ps) value))
    (rewrite-hash (setf (rewrite-hash ps) value))
    (subtype-hash (setf (subtype-hash ps) value))
    (dependent-decls (setf (dependent-decls ps) ;;NSH(4.21.95):special
			   (union value (dependent-decls ps))))
    (justification (setf (justification ps) 
			 value)) ;;NSH(3.16.97) added for checkpointing
    (current-xrule (setf (current-xrule ps) value))
    (comment (setf (comment ps) value))))

(defun semi-colonize (comment-string)
  (let ((newline-position
	 (position #\newline comment-string)))
    (if newline-position
	(let ((preline (subseq comment-string 0 newline-position))
	      (postline (subseq comment-string (1+ newline-position))))
	  (format nil ";;;~a~%~a"
	    preline
	    (semi-colonize postline)))
	(format nil ";;;~a" comment-string))))


(defun comment-step (string)
  #'(lambda (ps)
      (cond ((stringp string)
	     (values '? (list (list (current-goal ps)
				    'comment
				    (semi-colonize string)))))
	    (t (format-if "~%Input ~a is not a string.")
	       (values 'X nil nil)))))

(addrule 'comment (string) () (comment-step string)
	 "Adds a comment to the sequent."
	 "Adding comment: ~a")

(defmethod print-object ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*))))
  (if *debugging-print-object*
      (call-next-method)
      (if (comment ps)
	  (format stream "~%~a : ~%~a~%~a"
	    (label ps)
	    (comment ps)
	    (current-goal ps))
	  (format stream "~%~a :  ~%~a"  (label ps) 
		  (current-goal ps))))))

(defun make-subgoal-proofstates (proofstate strategy subgoals
					    &optional tcc-subgoals)
;  (when (and (tcc-proofstate? proofstate)
;	     (not (tcc-sequent? (current-goal proofstate))))
;    (break "bad ps"))
  (let* ((allsubgoals (append subgoals tcc-subgoals))
	 (numsubgoals (length allsubgoals))
	 (colwidth (length (format nil "~a" numsubgoals))))
;;    (cond ((consp allsubgoals)))
    (loop for goal in allsubgoals
	  as goalnum from 1
	  collect
	  (let* ((sequent
		  (if (consp goal)(car goal) goal))
		 (tcc-to-sequent? (and (memq goal subgoals)
				       (tcc-sequent? sequent)
				       (tcc-sequent?;;NSH(11.3.94)
					(current-goal proofstate))
				       (eq (tcc (current-goal proofstate))
					   (tcc sequent))))
		 (goalstate
		  (make-instance
		      (if (typep strategy 'strategy)
			  'strat-proofstate
			  (if (and (tcc-sequent? sequent)
				   (not tcc-to-sequent?))
			      'tcc-proofstate
			      'proofstate))
		    'current-goal
		    (let* (
			   (sequent
			    (if tcc-to-sequent? ;;NSH(10.3.95)
				;;added copy since sequents are shared
				;;so destructive change-class is bad.
				(change-class (copy sequent) 'sequent)
				sequent)))
		      (clean-goal sequent))
		    'context (copy (context proofstate))
		    'strategy  strategy
		    'label
		    (if (= (length allsubgoals) 1)
			(label proofstate)
			(format nil "~a.~a" (label proofstate)
				goalnum))
		    'subgoalnum (1- goalnum)
		    'dependent-decls (dependent-decls proofstate)
		    'alists (copy (alists proofstate))
		    'current-auto-rewrites
		    (current-auto-rewrites proofstate)
		    'rewrite-hash (rewrite-hash proofstate)
		    'subtype-hash (subtype-hash proofstate)
		    'parent-proofstate proofstate
		    'comment (comment proofstate))))
	    (if (consp goal)
		(make-updates (cdr goal)
			      goalstate)
		goalstate)))))

(defun apply-step (step &optional comment save? time?)
  #'(lambda (ps)
      (apply-step-body ps step comment save? time?)))

(defun apply-step-body (ps step comment save? time?)
  (let* ((*generate-tccs* 'NONE)
	 (strat (let ((*in-apply* ps));;NSH(8.22.94)
		  ;;otherwise (apply (query*)) misbehaves.
		  (strat-eval* step ps))))
    (cond ((or (typep strat 'strategy)
	       (typep strat 'rule-instance))
	   (let* ((new-strat
		   (if (typep strat 'strategy)
		       strat
		       (make-instance 'strategy
			 'topstep strat)))
		  (newps0 (copy ps
			    'strategy
			    new-strat
			    'parent-proofstate nil))
		  (newps (if (typep newps0 'top-proofstate)
			     (change-class newps0 'proofstate)
			     newps0))
		  (*noninteractivemode* t)
		  (*suppress-printing* t)
		  (*dependent-decls* nil)
		  (init-time (get-internal-run-time))
		  (result		;catch 'abort-in-apply ;;NSH(8.22.94)
		   (let ((*in-apply* ps)
			 )
		     (prove* newps)))
		  (end-time (/ (- (get-internal-run-time) init-time)
			       internal-time-units-per-second)))
                         ;;;;(break "in-apply")
	     ;;		 if (null result);;when aborted ;;NSH(8.22.94)
	     ;;		     if *in-apply*
	     ;;			 (throw 'abort-in-apply nil)
	     ;;			 (values 'X nil nil)
	     (let* ((subgoals (collect-subgoals newps))
		    (justif (collect-justification newps))
		    (xrule `(apply
				(rerun
				 ,(editable-justification
				   justif nil T)))))
	       ;; (format t "~%step= ~a~%decls = ~a" step *dependent-decls*)
	       (when time? (format t "~%;;;Used ~,2F seconds in ~s." end-time step))
	       (if (eq (status-flag newps) 'XX)
		   (values 'X nil nil);;was 'XX
		   (if (or (eq (status-flag newps) 'X)
			   (and (singleton? subgoals)
				(null (pending-subgoals newps))
				;;NSH(1.27.98)added null test to prevent skipping
				;;of nested apply(save?).
				(or (and (not (consp (car subgoals)))
					 (exequal (car subgoals)
						  (current-goal ps)))
				    (and (consp (car subgoals))
					 (exequal (caar subgoals)
						  (current-goal ps))
					 (eq
					  (nth 6 (car subgoals))
					  (current-auto-rewrites newps))))))
		       (if save?
			   (values '? (list (current-goal newps)) nil)
			   (values 'X nil nil))
		       (if (eq (status-flag newps) '!);;(null subgoals)
			   (values '! nil
				   (list 'dependent-decls
					 *dependent-decls*
					 'current-xrule
					 xrule))
			   (values '? subgoals
				   (list;;NSH(4.20.95)
				    ;;'dependent-decls
				    ;; *dependent-decls*
				    'current-xrule
				    xrule))))))))
	  (t (values 'X nil nil)))))

(addrule 'apply (strategy) (comment save? time?) (apply-step strategy comment save? time?) 
	 "Applies STRATEGY as if it were a rule, and prints COMMENT string.
If SAVE? is T, then the APPLY step is saved even if the strategy
does nothing, e.g., (SKIP), which is useful for setting values of
globals, e.g., (APPLY (LISP (setq *xx* ...)) \"recording value of *xx*\" T).
This is the basic way of converting a glass-box strategy into an
atomic step so that internal execution of the strategy is hidden
and only the resulting subgoals are returned.  E.g.,
 (apply (then (skosimp*)(flatten)(inst?))
      \"Skolemizing, flattening, and instantiating\")."
	 "~%Applying ~%   ~s,~@[~%~a~]")

(defcl justification ()
  label   ;;the label is needed since proofs might get out of order.
  rule
  (subgoals :initform nil)
  (xrule :initform nil) ;;this is the expanded rule in primitive terms
  (comment :initform nil)
  )

(defun new-comment (proofstate)
  (let ((par-ps (parent-proofstate proofstate)))
    (and (or (not par-ps)
	     (not (eq (comment par-ps)(comment proofstate))))
	(comment proofstate))))

(defun success-step (proofstate)
  (wish-done-proof proofstate)
  (setf (status-flag proofstate) '!
	(done-subgoals proofstate)
	(sort (done-subgoals proofstate)
	      #'mystring<= :key #'label)
	(current-subgoal proofstate) nil  ;;NSH(9.19.95) was retaining old value.
	(justification proofstate)
	(cond ((current-rule proofstate)

	       (make-instance 'justification
		 'label (label-suffix (label proofstate))
		 'rule  (sexp-unparse (current-rule proofstate))
		 'xrule (current-xrule proofstate)
		 'comment (new-comment proofstate)
		 'subgoals
		 (mapcar #'(lambda (x) (justification x))
			 (done-subgoals proofstate))))
	      (t (justification (car (done-subgoals proofstate)))))
	)
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (y)
			  (pushnew y
				   (dependent-decls
				    proofstate)))
		      (dependent-decls x)))
	  (done-subgoals proofstate))
  ;;   (format t "~%decls = ~a" (dependent-decls proofstate))
  proofstate)

(defun collect-justification (proofstate)
  (cond ((null (current-rule proofstate))
	 (if (done-subgoals proofstate)
	     (collect-justification (car (done-subgoals proofstate)))
	     (if (pending-subgoals proofstate)
		 (collect-justification (car (pending-subgoals
					      proofstate)))
		 (if (remaining-subgoals proofstate)
		     (collect-justification (car (remaining-subgoals
						  proofstate)))
		     (if (current-subgoal proofstate)
			 (collect-justification (current-subgoal
						 proofstate))
			 (make-instance 'justification
			   'label (label-suffix (label proofstate))
			   'comment (new-comment proofstate)
			   'rule '(postpone)))))))
	((eq (status-flag proofstate) '!)
	 (or (justification proofstate)
	     (make-instance 'justification
	       'label (label-suffix (label proofstate))
	       'rule (sexp-unparse (current-rule proofstate))
	       'xrule (current-xrule proofstate)
	       'comment (new-comment proofstate)
	       'subgoals
	       (sort 
		(mapcar #'collect-justification
		  (done-subgoals proofstate))
		#'<
		:key #'(lambda (x)(safe-parse-integer (label x)))))))
	((memq (status-flag proofstate) '(? *))
	 (make-instance 'justification
	   'label (label-suffix (label proofstate))
	   'rule (sexp-unparse (current-rule proofstate))
	   'xrule (current-xrule proofstate)
	   'comment (new-comment proofstate)
	   'subgoals
	   (let* ((current (when (current-subgoal proofstate)
			     (collect-justification (current-subgoal proofstate))))
		  (done (mapcar #'collect-justification
			  (done-subgoals proofstate)))
		  (pending (mapcar #'collect-justification
			     (pending-subgoals proofstate)))
		  (remaining (mapcar #'collect-justification
			       (remaining-subgoals proofstate)))
		  (all-but-current (append done pending remaining))
		  (all (if (and current (not (eq (status-flag proofstate) '*))
				(not (member (label current)
					     all-but-current
					     :test #'equal
					     :key #'(lambda (x) (label x)))))
			   (cons current all-but-current)
			   all-but-current)))
	     (sort all #'< :key #'(lambda (x) (safe-parse-integer (label x)))))))
	(t (make-instance 'justification
	     'label (label-suffix (label proofstate))
	     'rule '(postpone)
	     'comment (new-comment proofstate)))))


(defmethod extract-justification-sexp ((justification justification))
  (list (label justification)
	(sexp-unparse (rule  justification))
	(extract-justification-sexp (subgoals justification))
	(comment justification)))

(defmethod comment ((list list)) (cadddr list))

(defun pp-justification* (justification label)
  (cond ((null justification)
	 nil)
	(t (when (comment justification)
	     (format T "~%~a" (comment justification)))
	   (cond ((equal (label justification) label)
		  (format T "~%")
		  (format T "~V@T" (+ 3 (length (string label)))))
		 (t (format T "~%~a : " (label justification))))
	   (write (format-rule (rule justification)) :pretty t)
	   (loop for entry in (reverse (subgoals justification))
		 do (pp-justification* entry (car justification))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(4.23.97) : adding hide-all-but step.

(defstep hide-all-but (&optional (fnums *) keep-fnums)
  (let ((fnums (gather-fnums (s-forms (current-goal *ps*))
			     fnums keep-fnums)))
    (hide :fnums fnums))
  "Hides all sequent formulas from FNUMS except those listed in
KEEP-FNUMS.  Useful when all but a few formulas need to be hidden."
  "Hiding ~a but keeping ~a")

(defun strat-eval (strat)
  (cond ((typep strat 'strategy) strat)
	((null strat) (get-rule '(skip) *ps*))
	((not (consp strat))
	 (format-if "~%Ill-formed rule/strategy: ~s " strat)
	 (get-rule '(skip) *ps*))
	((quote? strat)(strat-eval (cadr strat)))
	((if-form? strat);;(break "if")
	 (if (expr-eval (cadr strat))
	     (strat-eval (caddr strat))
	     (strat-eval (cadddr strat))))
	((try-form? strat)
	 (make-instance 'strategy
	   'topstep (strat-eval (cond-expr strat))
	   'subgoal-strategy (then-expr strat)
	   'failure-strategy (else-expr strat)))
	((let-form? strat)
	 (let ((let-value (let-eval (let-bindings strat))))
	   (strat-eval (subst-stratexpr
			     (let-body strat)
			     let-value
			     (reverse let-value)
			     ))))
	((eq (car strat) 'note)
	 ;;if this evaluates to a rule, then the input is noted with
	 ;;comment string which becomes part of the printout.
	 (let ((result
		(strat-eval (cadr strat))))
	   (when (typep result 'rule-instance)
	     (setf (rule-input result) strat
		   (rule-format result)
		   (if (cddr strat)
		       (cons (format nil "~%(~a)~a" (caddr strat)
				     (car (rule-format result)))
			     (cdr (rule-format result)))
		       (rule-format result))))
	   result))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (pair-formals-args (formals def)
					       (cdr strat)))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect
			    (if (consp x) ;;NHS(4.23.97)
				;;was ignoring args, otherwise.
				(cdr (assoc (car x) subalist))
				(cdr (assoc x subalist)))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr;;2/91:so that rules are APPLYed.
		 `(apply ,def-expr))
		(result (strat-eval new-def-expr)))
	   (setf (rule-input result)
		 strat
		 (rule-format result)
		 (when (format-string (rule-definition (car strat)))
		   (cons (format-string (rule-definition (car strat))) args)
		   )) 
	   result))
	((step-definition (car strat))
	 (let* ((def (step-definition (car strat)))
		(alist (pair-formals-args (formals def)
					       (cdr strat)))
		(def-expr  (subst-stratexpr
			    (defn def)
			    alist
			    (reverse alist)
			    )))
	   (strat-eval def-expr)))
	((primitive-rule (car strat))
	 (get-rule strat
		   *ps*))
	(t (format-if "~%Ill-formed rule/strategy: ~s " strat)
	   (get-rule '(skip) *ps*))))

;;; Takes a proof with checkpoints inserted and adds (JUST-INSTALL-PROOF
;;; ...) wrappers around each branch that does not contain a (CHECKPOINT).

;;; Thus given
;;; (""
;;;  (INDUCT "i")
;;;  (("1" (INST 1 1 1) (ASSERT))
;;;   ("2"
;;;    (SKOSIMP*)
;;;    (CASE "n5!1=0")
;;;    (("1" (CHECKPOINT)
;;;          (INST 1 "n3!1-3" "2") (("1" (ASSERT)) ("2" (ASSERT))))
;;;     ("2" (INST 2 "n3!1+2" "n5!1-1") (("1" (ASSERT)) ("2" (ASSERT))))))))

(defvar *checkpointed-branches* nil)

(defun complete-checkpointed-proof (proof)
  (let ((*checkpointed-branches* nil))
    (complete-checkpointed-proof* proof)))

(defun complete-checkpointed-proof* (form)
  (cond ((and (consp form)
	      (stringp (car form)))
	 (cond ((member '(checkpoint) (cdr form) :test #'equal)
		(push form *checkpointed-branches*)
		(ldiff form (cdr (member '(checkpoint) (cdr form) :test #'equal))))
	       ((and (consp (car (last form)))
		     (every #'consp (car (last form))))
		(let ((nlast (mapcar #'complete-checkpointed-proof*
			       (car (last form)))))
		  (cond ((some #'(lambda (ff)
				   (memq ff *checkpointed-branches*))
			       (car (last form)))
			 (push form *checkpointed-branches*)
			 (append
			  (butlast form)
			  (list
			   (mapcar #'(lambda (ff nf)
				       (if (memq ff *checkpointed-branches*)
					   nf
					   (list (car ff)
						 (list 'just-install-proof
						       nf))))
			     (car (last form))
			     nlast))))
			(t form))))
	       (t form)))
	(t form)))

(defun install-proof (tmpfilename name line origin buffer prelude-offset)
  ;; If the origin is supplied, simply install the proof.  Otherwise the
  ;; proof is being installed from the Proof buffer, and the declaration
  ;; is gotten from *edit-proof-info*, in this case ask before installing.
  (when (or origin
	    (if *edit-proof-info*
		(prog1 (pvs-y-or-n-p "Install proof on formula ~a? "
				     (id (car *edit-proof-info*)))
		  (pvs-message ""))
		(pvs-message "No proof is being edited.")))
    (let ((sexpr (ignore-errors (with-open-file (in tmpfilename) (read in)))))
      (unless (listp sexpr)
	(justification-error sexpr sexpr "Proof must be a list"))
      (multiple-value-bind (msg subexpr)
	  (check-edited-justification sexpr)
	(if subexpr
	    (justification-error subexpr sexpr msg)
	    (let ((just (revert-justification
			 (complete-checkpointed-proof sexpr))))
	      (multiple-value-bind (fdecl place)
		  (if origin
		      (formula-decl-to-prove name line origin)
		      (car *edit-proof-info*))
		(when (and origin fdecl)
		  (setq *edit-proof-info*
			(list fdecl place buffer prelude-offset)))
		(cond ((null fdecl)
		       (pvs-message "Proof is not associated with a formula"))
		      ((equal (extract-justification-sexp (justification fdecl))
			      just)
		       (pvs-message "Proof was not changed")
		       t)
		      (t (setf (justification2 fdecl) (justification fdecl))
			 (setf (justification fdecl) just)
			 (setf (proof-status fdecl) 'unfinished)
			 (unless (from-prelude? (module fdecl))
			   (save-all-proofs (module fdecl)))
			 (pvs-message "Proof installed on ~a" (id fdecl))
			 t)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH (4.29.97) modified editable-justification to keep comment.

(defun strip-rerun (justif)
  (let ((label (label justif))
	(rule (rule justif)))
    (if (and (consp rule)
	     (eq (car rule) 'rerun)
	     (equal (label (cadr rule)) label))
	(cadr rule)
	justif)))


(defun editable-justification (justif &optional label xflag full-label)
  ;;NSH(1.3.98) if full-label is given, then the full label is
  ;;printed rather than just the branch numbers.
  (unless (null justif)
    (let ((jlabel (label justif))
	  (rule (rule justif)))
      (if (and (consp rule)
	       (eq (car rule) 'rerun)
	       (equal (label (cadr rule)) jlabel))
	  (cadr rule)
	  (let* ((top-step (if (and xflag (xrule justif))
			       (format-rule (xrule justif))
			       (format-rule rule)))
		 (full-label (if (and full-label
				      (not (equal jlabel label))
				      (> (length jlabel) 0))
				     (format nil "~a.~a" full-label jlabel)
				 full-label))
		 (ejustif (cons top-step
				(editable-justification* (subgoals justif)
							 jlabel
							 xflag
							 full-label)))
		 (ejustif (if (comment justif)
			      (cons (comment justif) ejustif)
			      ejustif)))
	    (if (equal jlabel label)
		ejustif
		(cons (or full-label jlabel) ejustif)))))))

(defun editable-justification* (justifs &optional label xflag full-label)
  (unless (null justifs)
    (if (singleton? justifs)
	(editable-justification (car justifs) label xflag full-label)
	(list (loop for justif in justifs
		    collect (editable-justification
			       justif nil xflag full-label))))))

;;;NSH(5.1.97): Modified revert-justification to handle comment strings.

(defun revert-justification (ejustif &optional label)
  (unless (null ejustif)
    (if  (null label)
	 (if (consp (cadr ejustif)) 
  	     (list (car ejustif)
	       (unformat-rule (cadr ejustif))
	       (revert-justification (cddr ejustif)(car ejustif)))
	     (revert-justification (cddr ejustif)(car ejustif)))
	 (if (and (consp (car ejustif))(consp (caar ejustif)))
	     (mapcar #'revert-justification (car ejustif))
	     (if (consp (car ejustif))
  	         (list (list label
			 (unformat-rule (car ejustif))
			 (revert-justification (cdr ejustif) label)))
		 (revert-justification (cdr ejustif) label))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;NSH(5.4.97): skolem-inst strategy for simultaneous skolemization
;;;and instantiation.

(defun seq-form-bindings (formula)
  (if (or (exists-expr? formula)
	  (forall-expr? formula))
      (bindings formula)
      (if (and (not-expr? formula)
	       (or (exists-expr? (args1 formula))
		   (forall-expr? (args1 formula))))
	  (bindings (args1 formula))
	  nil)))

(defstep skolem_inst (&optional (sk_fnum *) (inst_fnum *))
  (let ((sk_fnum (find-!quant sk_fnum *ps*))
	(sforms (select-seq (s-forms (current-goal *ps*))(list sk_fnum)))
	(bindings (when sforms (seq-form-bindings (formula (car sforms)))))
	(newterms (fill-up-terms sk_fnum nil *ps*))
	(inst_fnums (gather-fnums
		     (s-forms (current-goal *ps*))
		     inst_fnum nil
		     #'(lambda (sform)
			 (and (exists-sform? sform)
			      (eql (length (seq-form-bindings (formula sform)))
				   (length newterms))
			      (subsetp (seq-form-bindings (formula sform))
				       bindings
				       :test #'same-id)))))
	(inst_fnum (when inst_fnums (car inst_fnums))))
    (if inst_fnums
	(then (skolem sk_fnum newterms)
	      (inst inst_fnum :terms newterms))
	(skip-msg "Couldn't find matching top-level quantifiers.")))
    ""
    "Simultaneously skolemizing and instantiating with skolem constants")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;NSH(5.21.97): added conditional simplification of conjunction,
;;;implication, and disjunction, and reduction of cons(x1, y1) = cons(x2, y2)
;;;to x1 = x2 and y1 = y2.

(defmacro sig-value-value (expr)
	 `(multiple-value-bind (sig value)
	      ,expr
	    value))
    
;;NSH(10.21.94)
(defun assert-equality (expr newargs sig)
  (let ((nargs (argument* newargs)))
    (cond ((tc-eq (car nargs)(cadr nargs))
	   (values '? *true*))
;;NSH(10.21.94): separated out since this shouldn't be
;invoked from assert-if-inside.
	  ((and (application? (car nargs))
		(application? (cadr nargs))
		(constructor? (operator (car nargs)))
		(tc-eq (operator (car nargs))
		       (operator (cadr nargs)))
		(eql (length (arguments (car nargs)))
		     (length (arguments (cadr nargs)))))
	   (values '?
		   (make-conjunction
		    (loop for x in (arguments (car nargs))
			  as y in (arguments (cadr nargs))
			  collect
			  (let ((neweq (make-equation x y)))
			    (sig-value-value
			     (assert-equality neweq
					      (argument neweq) '?)
			    ))))))
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
		   (if (not (eq (id p1) (id p2)))
		       (values '? *false*)
		       (do-auto-rewrite expr sig))
		   (let* ((pred (or p1 p2))
			  (term (if p1 (args2 expr) (args1 expr)))
			  (result (when pred
				    (check-rest-recognizers
				     pred
				     (check-all-recognizers term)))))
		     (cond ((null pred)(do-auto-rewrite expr sig))
			   ((eq result 'FALSE)
			    (values '? *false*))
			   ((and (eq result 'RESTFALSE)
				 (null (accessors (constructor pred))))
			    (values '? *true*))
			   (t (do-auto-rewrite expr sig))))))))))

(defun assert-if-arg (expr)
  ;;called from assert-if(application)
  ;;expr is the original expr
  (let ((arg (argument expr)))
    (cond ((or (implication? expr)
	       (conjunction? expr))
	   (let* ((arg1 (args1 expr))
		  (arg2 (args2 expr)))
	     (multiple-value-bind
		 (sig1 new-arg1)
		 (assert-if arg1)
	       (multiple-value-bind
		   (sig2 new-arg2)
		   (cond-assert-if arg2 (list new-arg1))
		 (do-auto-rewrite
		  (lcopy arg
		    'exprs (list new-arg1 new-arg2))
		  (if (or (eq sig1 '?)(eq sig2 '?))
		      '? 'X))))))
	  ((disjunction? expr)
	   (let* ((arg1 (args1 expr))
		  (arg2 (args2 expr)))
	     (multiple-value-bind
		 (sig1 new-arg1)
		 (assert-if arg1)
	       (multiple-value-bind
		   (sig2 new-arg2)
		   (cond-assert-if arg2 (list (negate new-arg1)))
		 (do-auto-rewrite
		  (lcopy arg
		    'exprs (list new-arg1 new-arg2))
		  (if (or (eq sig1 '?)(eq sig2 '?))
		      '? 'X))))))
	  (t (assert-if (argument expr))))))

(defmethod assert-if ((expr application)) ; (break "assert-if-ap")
  (with-slots (operator argument)
      expr
  (multiple-value-bind
	(sigop newop)
      (if (and (lambda? operator)
	       (not (eq *assert-flag* 'rewrite)))
	  (values 'X operator)
	  (assert-if operator))
    (multiple-value-bind (sigargs newargs)
	(assert-if-arg expr)
	     (let* ((sig (if (eq sigop '?) '? sigargs))
		    (expr;;shadowing expr
		     (lcopy expr
		       'operator (if (eq sigop '?) newop
				     (operator expr))
		       'argument (if (eq sigargs '?) newargs
				     (argument expr))))
		    (result ;(nil)
		     (when (and (not (connective-occurs? expr))
				;;NSH(7.14.97) was check-for-connectives
				(tc-eq (type expr) *boolean*)
				(not (eq *top-assert-flag* 'rewrite)))
		       (assert-test expr))
		    )) ;(break "assert-if-ap2")
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
			   (typep (or (car (types newargs))
				      (type newargs))
				  'subtype)
			   (member newop
				   (type-constraints
				    (or (car (types newargs))
					(type newargs))
				    T)
				   :test #'tc-eq));;(break)
		      (values-assert-if '? *true* expr))
		     ;;NSH(9.10.93) The case above is kept here so that assert-if-inside doesn't
		     ;;remove something brought in by typepred.
		     ((and (equality? expr);;moved here from
			   ;;assert-if-application so that
			   ;;assert-if-inside does not
			   ;;self-simplify antecedent
			   ;;equalities.
			   (tc-eq (find-supertype (type (car (exprs newargs))))
				  *number*)
			   (not (check-for-connectives? expr)))
		      (assert-numeric-equality expr sig))

		     (t  (assert-if-application expr newop newargs sig))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Multiple-proofs support

;;; A proof-info object contains the information pertaining to a given
;;; proof.  A formula-decl has a list of these, and if there is a default
;;; proof, it is one of these.  The id is an optional identifier
;;; associated with the proof.  The description is an optional string
;;; describing the proof.  The script is the proof script or justification
;;; that is run to get the proof.  The status is one of PROVED, UNTRIED,
;;; UNFINISHED, or UNCHECKED.  The refers-to is a list of declarations
;;; that are referred to during the proof.  Real-time and run-time are
;;; times in internal time units reflecting the clock time and CPU time
;;; for the given proof.  Interactive? is T or NIL indicating whether the
;;; last attempt of this proof script was interactive or not.  The 

(defcl proof-info ()
  id
  description
  create-date
  run-date
  script
  status
  refers-to
  real-time
  run-time
  interactive?)

(defmethod print-object ((prinfo proof-info) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#PROOF-INFO~@[ ~a:~] ~a>"
	(id prinfo) (if (run-date prinfo)
			(date-string (run-date prinfo))))))

(defun date-string (time &optional long?)
  (multiple-value-bind (sec min hour date month year day-of-week dst time-zone)
      (decode-universal-time time)
    (if long?
	(format nil "~a ~a ~d ~2,'0d:~2,'0d:~2,'0d ~d"
	  (nth day-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	  (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
			    "Aug" "Sep" "Oct" "Nov" "Dec"))
	  date hour min sec year)
	(format nil "~2,'0d:~2,'0d:~2,'0d ~2,'0d/~2,'0d/~2,'0d"
	  hour min sec month date (mod year 100)))))

(defcl decl-reference ()
  id
  class
  type
  theory-id
  library)


(defvar *subgoals* nil)

;;; sexp converts a given object to a list; 

(defmethod sexp ((prinfo proof-info))
  (with-slots (id description create-date run-date script status
		  refers-to real-time run-time interactive?)
      prinfo
    (list id description create-date run-date script status
	  (sexp refers-to) real-time run-time interactive?)))

(defmethod sexp ((list list))
  (mapcar #'sexp list))

(defmethod sexp ((dref decl-reference))
  (with-slots (id class type theory-id library) dref
    (list id class type theory-id library)))

(defmethod sexp ((decl declaration))
  (list (id decl)
	(type-of decl)
	(when (and (typed-declaration? decl)
		   (not (typep decl 'formal-type-decl)))
	  (or (declared-type-string decl)
	      (setf (declared-type-string decl)
		    (unparse (declared-type decl) :string t))))
	(when (module decl) (id (module decl)))
	(when (typep (module decl) 'library-theory)
	  (library (module decl)))))

(defun mk-proof-info (id description create-date run-date script status
			 refers-to real-time run-time interactive?)
  (make-instance 'proof-info
    'id id
    'description description
    'create-date create-date
    'run-date run-date
    'script (if (= (length script) 3)
		(append script (list nil))
		script)
    'status status
    'refers-to (typecase (car refers-to)
		 (declaration refers-to)
		 (declaration-entry
		  (mapcar #'get-declaration-entry-decl refers-to))
		 (t (mapcar #'get-referenced-declaration
		      (remove-if #'null refers-to))))
    'real-time real-time
    'run-time run-time
    'interactive? interactive?))

;;; Formula-decl slots have the following meaning:
;;;   spelling:   One of FORMULA, AXIOM, LEMMA, etc.
;;;   definition: The body of the formula declaration
;;;   closed-definition: The closure of the body of the definition
;;;   kind:       The kind of formula-decl (e.g., TCC, EXISTENCE)
;;;   tcc-disjuncts: The disjuncts of the definition used for TCCs
;;;   justification: The default justification
;;;   justifications: The justifications for this delaration - the default
;;;                   justification is one of these
;;;   proof-status: The status of the default justification - one of
;;;                   proved, unproved, unfinished, or unchecked
;;;   proof-refers-to: The delarations that the default justification refers to
;;;   proof-time: The times associated with the proof; a list of the form
;;;               (runtime, realtime, interactive?)

(defcl* formula-decl (declaration)
  (spelling :parse t)
  (definition :parse t)
  ;; The universal closure of the definition, used in create-formulas
  closed-definition
  kind
  tcc-disjuncts
  (default-proof :fetch-as nil)
  (proofs :fetch-as nil))

(defmethod update-instance-for-redefined-class :before
  ((fdecl formula-decl) added deleted plist &rest initargs)
  (when (and plist
	     (getf plist 'justification))
    (let ((prinfo (make-proof-info (getf plist 'justification)
				   (next-proof-id fdecl))))
      (setf (refers-to prinfo) (getf plist 'proof-refers-to))
      (when (getf plist 'proof-time)
	(setf (run-time prinfo) (car (getf plist 'proof-time)))
	(setf (real-time prinfo) (cadr (getf plist 'proof-time)))
	(setf (interactive? prinfo) (caddr (getf plist 'proof-time))))
      (setf (status prinfo) (getf plist 'proof-status))
      (setf (proofs fdecl) (list prinfo))
      (setf (default-proof fdecl) prinfo))))

(defmethod justification ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (script (default-proof decl))))

(defmethod (setf justification) (just (decl formula-decl))
  (ensure-default-proof decl)
  (setf (script (default-proof decl))
	(extract-justification-sexp just)))

(defmethod proof-status ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (status (default-proof decl))))

(defmethod (setf proof-status) (stat (decl formula-decl))
  (ensure-default-proof decl)
  (setf (status (default-proof decl)) stat))

(defmethod proof-refers-to ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (refers-to (default-proof decl))))

(defmethod (setf proof-refers-to) (refs (decl formula-decl))
  (ensure-default-proof decl)
  (setf (refers-to (default-proof decl)) refs))

(defun ensure-default-proof (fdecl &optional script id description)
  (unless (default-proof fdecl)
    (if (proofs fdecl)
	(setf (default-proof fdecl) (car (proofs fdecl)))
	(make-default-proof fdecl script id description))))

(defun make-default-proof (fdecl script &optional id description)
  (let* ((pid (or id (next-proof-id fdecl)))
	 (prinfo (make-proof-info script pid description)))
    (push prinfo (proofs fdecl))
    (setf (default-proof fdecl) prinfo)))

(defun next-proof-id (fdecl &optional (num 1))
  (let ((id (makesym "~a-~d" (id fdecl) num)))
    (if (and (slot-boundp fdecl 'proofs)
	     (member id (proofs fdecl)
		     :test #'(lambda (x y) (eq x (id y)))))
	(next-proof-id fdecl (1+ num))
	id)))

(defun make-proof-info (script &optional id description)
  (assert (symbolp id))
  (assert (or (null description) (stringp description)))
  (assert (typep script '(or list justification)))
  ;;(assert (not (null script)))
  (make-instance 'proof-info
    'id id
    'description description
    'script (if (= (length script) 3)
		(append script (list nil))
		script)
    'create-date (get-universal-time)
    'status 'unchecked))

;;; From eproofcheck.lisp

(defun make-dpinfo (sigalist findalist usealist)
  (make-instance 'dpinfo
    'dpinfo-sigalist sigalist
    'dpinfo-findalist findalist
    'dpinfo-usealist usealist))

(defun save-proof-info (decl init-real-time init-run-time)
  (let ((prinfo (default-proof decl))
	(script (extract-justification-sexp
		 (collect-justification *top-proofstate*))))
    (cond ((null (script prinfo))
	   (setf (script prinfo) script))
	  ((and (not *proving-tcc*);; interactive
		script
		(not (equal script '("" (POSTPONE) NIL NIL)))
		(not (equal (script prinfo) script))
		(let ((ids (mapcar #'id
			     (remove-if-not #'(lambda (prinfo)
						(equal (script prinfo) script))
			       (proofs decl)))))
		  (cond (ids
			 (format t "~%This proof is already associated with ~
                                    this formula as ~{~a~^, ~}" ids)
			 nil)
			((pvs-yes-or-no-p
			  "Would you like the proof to be saved? "))))
	   (cond ((pvs-yes-or-no-p
		   "Would you like to overwrite the current proof (~a)? "
		   (id prinfo))
		  (setf (script prinfo) script))
		 (t (let ((id (read-proof-id (next-proof-id decl)))
			  (description (read-proof-description)))
		      (setq prinfo
			    (make-default-proof decl script id
						description))))))))
    (setf (real-time prinfo) (- (get-internal-real-time) init-real-time))
    (setf (run-time prinfo) (- (get-internal-run-time) init-run-time))
    (setf (run-date prinfo) (get-universal-time))
    (setf (proof-status decl)
	  (if (and (eq (status-flag *top-proofstate*) '!)
		   (not *context-modified*))
	      'proved
	      'unfinished))
    (when *context-modified*
      (setf (proof-status decl) 'unfinished)
      (when (and (not *proving-tcc*)
		 (pvs-yes-or-no-p
		  "~%Context was modified in mid-proof.  ~
                     Would you like to rerun the proof?~%"))
	(prove-decl decl :strategy '(then (rerun) (query*)))))))

(defun read-proof-id (default)
  (format t "Please enter a proof identifier (default ~a): " default)
  (let ((id (read-line)))
    (cond ((equal id "") default)
	  ((valid-proof-id id) (intern id))
	  (t (format t "~a is not a legal proof identifier:~%" id)
	     (read-proof-id default)))))

(defun valid-proof-id (str)
  (and (alpha-char-p (char str 0))
       (every #'(lambda (ch)
		  (or (alpha-char-p ch)
		      (digit-char-p ch)
		      (member ch '(#\_ #\? #\-) :test #'char=)))
	      (subseq str 1))))

(defun read-proof-description ()
  (format t "Please enter a description: ")
  (read-line))

(defun before-prove* ()
  (when *start-proof-display*
    (let ((*ps* *top-proofstate*))
      (call-x-show-proof)))
  (pvs-emacs-eval "(setq pvs-in-checker t)"))

(defun after-prove* ()
  (pvs-emacs-eval "(setq pvs-in-checker nil)")
  (display-proofstate nil)
  (when *subgoals*
    (setq *subgoals*
	  (mapcar #'current-goal
	    (collect-all-remaining-subgoals *top-proofstate*))))
  (unless *recursive-prove-decl-call*
    (clear-proof-hashes)))

(defun clear-proof-hashes ()
  (clrhash *auto-rewrites-ops*)
  (clrhash *auto-rewrites*)
  (clrhash *auto-rewrites-ops*)
  (clrhash *subtype-of-hash*)
  (clrhash *prtype-hash*)
  (clrhash *local-prtype-hash*)
  (clrhash *beta-cache*)
  (clrhash *match-cache*)
  (clrhash *assert-if-arith-hash*)
  (clrhash *pvs-sxhash-cache*)
  (clrhash *term-print-strings*)
  (clrhash *translate-from-prove-hash*)
  (clrhash *translate-from-dc-hash*)
  (pvs-clrhash *translate-to-prove-hash*)
  (pvs-clrhash *translate-id-hash*)
  (pvs-clrhash *dc-named-exprs*)
  (pvs-clrhash *translate-to-dc-hash*)
  (pvs-clrhash *dc-translate-id-hash*)
  (pvs-clrhash *create-formulas-cache*))

;;; From pvs.lisp

(defun prove-file-at (name line rerun?
			   &optional origin buffer prelude-offset
			   background? display?)
  (let ((*to-emacs* background?))
    (if *in-checker*
	(pvs-message "Must exit the current proof first")
	(multiple-value-bind (fdecl place)
	    (formula-decl-to-prove name line origin)
	  (if (and rerun?
		   fdecl
		   (null (justification fdecl)))
	      (pvs-message "Formula ~a has no proof to rerun." (id fdecl))
	      (if fdecl
		  (let ((*current-theory* (module fdecl))
			(*current-system* (if (member origin '("tccs" "ppe"))
					      'pvs
					      (intern (string-upcase
						       origin))))
			(*start-proof-display* display?)
			(ojust (extract-justification-sexp
				(justification fdecl)))
			(*justifications-changed?* nil))
		    (read-strategies-files)
		    (let ((proof (cond (background?
					(pvs-prove-decl fdecl t))
				       (t (auto-save-proof-setup fdecl)
					  (prove (id fdecl)
						 :strategy
						 (when rerun? '(rerun)))))))
		      (when (typep proof 'proofstate)
			(setq *last-proof* proof)))
		    ;; Save the proof if it is different.
		    (unless (or (equal origin "prelude")
				(from-prelude? fdecl))
		      (when (or *justifications-changed?*
				(not (equal ojust
					    (extract-justification-sexp
					     (justification fdecl)))))
			(save-all-proofs *current-theory*))
		      ;; If the proof status has changed, update the context.
		      (update-context-proof-status fdecl))
		    (remove-auto-save-proof-file)
		    (let ((*to-emacs* t))
		      (pvs-locate buffer fdecl
				  (if prelude-offset
				      (vector (- (line-begin place) prelude-offset)
					      (col-begin place)
					      (- (line-end place) prelude-offset)
					      (col-end place))
				      place))))))))))

(defun prove-tcc (decl)
  (unless (and (default-proof decl)
	       (proved? decl))
    (unless (default-proof decl)
      (make-default-proof decl (tcc-strategy decl))
      (setq *justifications-changed?* t))
    (let* ((start-time (get-universal-time))
	   (*proving-tcc* 'TCC)
	   (proof (rerun-prove decl))
	   (proof-time (- (get-universal-time) start-time)))
      (pvs-message
	  "~:[Unable to prove~;Proved~] ~:[~;TCC ~]~a in ~d seconds"
	(eq (status-flag proof) '!) (tcc? decl) (id decl) proof-time)
      ;; Must return non-NIL if proved, NIL otherwise.
      (if (eq (status-flag proof) '!)
	  (setf (proof-status decl) 'proved)
	  (setf (proof-status decl) 'unfinished))
      (eq (status-flag proof) '!))))

(defun prove-proof-at (line step? display?)
  (let* ((fdecl (car *edit-proof-info*))
	 (*current-theory* (module fdecl)))
    (read-strategies-files)
    (auto-save-proof-setup fdecl)
    (let ((*start-proof-display* display?))
      (setq *last-proof*
	    (if step?
		(prove (id fdecl))
		(prove (id fdecl) :strategy '(rerun)))))
    ;; Save the proof.
    (unless (from-prelude? fdecl)
      (save-all-proofs *current-theory*)
      ;; If the proof status has changed, update the context.
      (update-context-proof-status fdecl))
    (when (typep fdecl 'tcc-decl)
      (update-tcc-info (module fdecl) (collect-tccs (module fdecl))))
    (remove-auto-save-proof-file)
    (when (default-proof fdecl)
      (setf (interactive? (default-proof fdecl)) t))
    (let* ((*to-emacs* t)
	   (place (second *edit-proof-info*))
	   (buffer (third *edit-proof-info*))
	   (prelude-offset (fourth *edit-proof-info*)))
      (pvs-locate buffer fdecl
		  (if prelude-offset
		      (vector (- (line-begin place) prelude-offset)
			      (col-begin place)
			      (- (line-end place) prelude-offset)
			      (col-end place))
		      place)))))

(defun install-proof (tmpfilename name line origin buffer prelude-offset)
  ;; If the origin is supplied, simply install the proof.  Otherwise the
  ;; proof is being installed from the Proof buffer, and the declaration
  ;; is gotten from *edit-proof-info*, in this case ask before installing.
  (when (or origin
	    (if *edit-proof-info*
		(prog1 (pvs-y-or-n-p "Install proof on formula ~a? "
				     (id (car *edit-proof-info*)))
		  (pvs-message ""))
		(pvs-message "No proof is being edited.")))
    (let ((sexpr (ignore-errors (with-open-file (in tmpfilename) (read in)))))
      (unless (listp sexpr)
	(justification-error sexpr sexpr "Proof must be a list"))
      (multiple-value-bind (msg subexpr)
	  (check-edited-justification sexpr)
	(if subexpr
	    (justification-error subexpr sexpr msg)
	    (let ((just (revert-justification
			 (complete-checkpointed-proof sexpr))))
	      (multiple-value-bind (fdecl place)
		  (if origin
		      (formula-decl-to-prove name line origin)
		      (car *edit-proof-info*))
		(when (and origin fdecl)
		  (setq *edit-proof-info*
			(list fdecl place buffer prelude-offset)))
		(cond ((null fdecl)
		       (pvs-message "Proof is not associated with a formula"))
		      ((equal (extract-justification-sexp (justification fdecl))
			      just)
		       (pvs-message "Proof was not changed")
		       t)
		      ((some #'(lambda (prinfo)
				 (equal (script prinfo) just))
			     (proofs fdecl))
		       (let ((prinfo (find #'(lambda (prinfo)
					       (equal (script prinfo) just))
					   (proofs fdecl))))
			 (setf (default-proof fdecl) prinfo)
			 (unless (from-prelude? (module fdecl))
			     (save-all-proofs (module fdecl)))
			 (pvs-message "Proof installed on ~a as ~a"
			   (id fdecl) (id prinfo))
			 t))
		      (t (let ((prinfo (make-proof-info
					just
					(next-proof-id fdecl)
					(description
					 (default-proof
					   (car *edit-proof-info*))))))
			   (push prinfo (proofs fdecl))
			   (setf (default-proof fdecl) prinfo)
			   (unless (from-prelude? (module fdecl))
			     (save-all-proofs (module fdecl)))
			   (pvs-message "Proof installed on ~a as ~a"
			     (id fdecl) (id prinfo)))
			 t)))))))))

(defun remove-proof-at (name line origin)
  (multiple-value-bind (fdecl place)
      (formula-decl-to-prove name line origin)
    (cond ((and fdecl (default-proof fdecl))
	   (let ((prf (default-proof fdecl)))
	     (setf (proofs fdecl) (delete prf (proofs fdecl)))
	     (setf (default-proof fdecl) (car (proofs fdecl)))
	     (when (tcc? fdecl)
	       (setf (tccs-tried? (module fdecl)) nil))
	     (update-context-proof-status fdecl)
	     (save-all-proofs *current-theory*)
	     (pvs-message "Proof ~a removed from ~a"
	       (id prf) (id fdecl))))
	  (fdecl
	   (pvs-message "Formula ~a has no proof to remove" (id fdecl)))
	  (t (pvs-message "Unable to find formula declaration")))))

(defun reset-proof-statuses (theory)
  (invalidate-proofs theory))

;;; need to remove revert-proof-at here and in pvs-prover .el


;;; From untypecheck.lisp

(defmethod untypecheck-theory ((decl formula-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (definition decl))
  (setf (kind decl) nil)
  (setf (tcc-disjuncts decl) nil)
  (setf (closed-definition decl) nil)
  (setf (default-proof decl) nil)
  (setf (proofs decl) nil))

(defun reset-tccs-proof-status (decls)
  (dolist (d decls)
    (when (and (formula-decl? d)
	       (generated-by d))
      (dolist (prinfo (proofs d))
	(setf (status d) 'untried))
      (update-context-proof-status d))))


;;; From context.lisp

(defstruct (declaration-entry (:conc-name de-))
  id
  class
  type
  theory-id
  (library nil))

(defvar *testing-restore* nil)

(defun write-object-files (&optional force?)
  (update-stored-mod-depend)
  (if *testing-restore*
      (maphash #'(lambda (file theories)
		    (declare (ignore theories))
		    (write-object-file file force?))
		*pvs-files*)
      (multiple-value-bind (value condition)
	  (ignore-file-errors
	   (maphash #'(lambda (file theories)
			(declare (ignore theories))
			(write-object-file file force?))
		    *pvs-files*))
	(declare (ignore value))
	(when condition
	  (pvs-warning "~a" condition)))))

(defun restore-formula-info (decl finfolist valid?)
  (let* ((finfo (find-if #'(lambda (fi)
			     (if (consp fi)
				 (eq (id decl) (car fi))
				 (same-id decl fi)))
			 finfolist))
	 (stat (when finfo
		 (if (consp finfo)
		     (cadr finfo)
		     (fe-status finfo)))))
    (setf (proof-status decl)
	  (if (and stat
		   (justification decl)
		   (not valid?))
	      'unchecked
	      (if (eq stat t) 'proved stat)))
    (when finfo
      (unless (consp finfo)
	(dolist (dref (fe-proof-refers-to finfo))
	  (pushnew (get-declaration-entry-decl dref)
		   (proof-refers-to decl)))))))

(defun get-declaration-entry-decl (de)
  (get-referenced-declaration*
   (de-id de)
   (de-class de)
   (de-type de)
   (de-theory-id de)
   (de-library de)))

(defun get-referenced-declaration (declref)
  (apply #'get-referenced-declaration* declref))

(defun get-referenced-declaration* (id class type theory-id library)
  (let ((theory (get-theory theory-id library)))
    (when theory
      (let ((decls (remove-if-not
		       #'(lambda (d)
			   (eq (type-of d) class))
		     (gethash id (declarations theory)))))
	(cond ((singleton? decls)
	       (car decls))
	      ((and (cdr decls) type)
	       (let ((ndecls (remove-if-not
				 #'(lambda (d)
				     (string= (unparse (declared-type d)
						:string t)
					      type))
			       decls)))
		 (when (singleton? ndecls)
		   (car ndecls)))))))))

(defun restore-proofs (filestring theory)
  (if *testing-restore*
      (with-open-file (input filestring :direction :input)
	  (restore-theory-proofs input filestring theory))
      (multiple-value-bind (ignore error)
	  (ignore-errors
	    (with-open-file (input filestring :direction :input)
	      (restore-theory-proofs input filestring theory)))
	(when error
	  (pvs-message "Error reading proof file ~a"
	    (namestring filestring))
	  (pvs-log (format nil "  ~a" error))))))

(defun restore-theory-proofs (input filestring theory)
  (let ((theory-proofs (read input NIL NIL)))
    (when theory-proofs
      (let* ((theoryid (car theory-proofs))
	     (proofs (cdr theory-proofs)))
	(unless (every #'consp proofs)
	  (pvs-message "Proofs file ~a is corrupted, will try to keep going."
	    filestring)
	  (setq proofs (remove-if-not #'consp proofs)))
	(if (eq theoryid (id theory))
	    (let ((restored (mapcar #'(lambda (decl)
					(restore-theory-proofs* decl proofs))
				    (append (assuming theory) (theory theory)))))
	      (copy-proofs-to-orphan-file
	       theoryid (set-difference proofs restored :test #'equal)))
	    (restore-theory-proofs input filestring theory))))))

(defun restore-from-context (filename theory)
  (let ((prf-path (make-prf-pathname filename)))
    (when (probe-file prf-path)
      (restore-proofs prf-path theory))
    (unless (valid-proofs-file filename)
      (invalidate-proofs theory))))

(defun invalidate-proofs (theory)
  (when theory
    (mapc #'(lambda (d)
	      (when (typep d 'formula-decl)
		(mapc #'(lambda (prinfo)
			  (when (eq (status prinfo) 'proved)
			    (setf (status prinfo) 'unchecked)))
		      (proofs d))))
	  (append (assuming theory) (theory theory)))))

(defmethod valid-proofs-file ((entry context-entry))
  (and (valid-context-entry entry)
       (valid-proofs-file (ce-file entry))))

(defmethod valid-proofs-file (filename)
  (multiple-value-bind (valid? entry)
      (valid-context-entry filename)
    (and valid?
	 (let ((prf-file (make-prf-pathname filename)))
	   (and (probe-file prf-file)
		(eql (file-write-date prf-file)
		     (ce-proofs-date entry)))))))

(defun collect-theory-proofs* (decls proofs)
  (if (null decls)
      (nreverse proofs)
      (let* ((decl (car decls))
	     (prfs (collect-decl-proofs decl)))
	(collect-theory-proofs*
	 (cdr decls)
	 (if prfs
	     (cons prfs proofs)
	     proofs)))))

(defmethod collect-decl-proofs ((decl formula-decl))
  (let ((prfs (proofs decl)))
    (when prfs
      (cons (id decl)
	    (cons (position (default-proof decl) prfs)
		  (mapcar #'sexp prfs))))))

(defmethod collect-decl-proofs (obj)
  nil)

(defun restore-theory-proofs* (decl proofs)
  (when (formula-decl? decl)
    (let ((prf-entry (assoc (id decl) proofs :test #'eql)))
      (cond ((integerp (cadr prf-entry))
	     (setf (proofs decl)
		   (mapcar #'(lambda (p) (apply #'mk-proof-info p))
		     (cddr prf-entry)))
	     (setf (default-proof decl)
		   (nth (cadr prf-entry) (proofs decl))))
	    (prf-entry
	     (unless (some #'(lambda (prinfo)
			       (equal (script prinfo) (cdr prf-entry)))
			   (proofs decl))
	       (let ((prinfo (make-proof-info (cdr prf-entry)
					      (next-proof-id decl)))
		     (fe (get-context-formula-entry decl)))
		 (when fe
		   (dolist (dref (fe-proof-refers-to fe))
		     (pushnew (get-declaration-entry-decl dref)
			      (refers-to prinfo)))
		   (setf (status prinfo) (fe-status fe)))
		 (push prinfo (proofs decl))
		 (setf (default-proof decl) prinfo)))))
      prf-entry)))


;;; In status-cmds.lisp

(defun proof-summary (theory-id &optional times? (indent 0))
  (format t "~2%~vTProof summary for theory ~a" indent (ref-to-id theory-id))
  (let ((tot 0) (proved 0) (unfin 0) (untried 0))
    (let* ((theory (get-theory theory-id))
	   (valid? (or (and theory
			    (from-prelude? theory))
		       (valid-proofs-file (context-entry-of theory-id)))))
      (if (and theory
	       (typechecked? theory))
	  (mapc #'(lambda (decl)
		    (format t "~%    ~55,1,0,'.a~10a"
		      (id decl)
		      (proof-status-string decl))
		    (when times?
		      (if (real-time (default-proof decl))
			  (format t
			      "~%      Run time: ~6,2F s, Real time: ~6,2F s - ~a"
			    (/ (run-time (default-proof decl))
			       internal-time-units-per-second)
			    (/ (real-time (default-proof decl))
			       internal-time-units-per-second)
			    (if (interactive? (default-proof decl))
				"interactive" "background"))
			  (format t "~%      Proof times unavailable")))
		    (incf tot)
		    (cond ((proved? decl)
			   (incf proved))
			  ((justification decl) (incf unfin))
			  (t (incf untried))))
		(provable-formulas theory))
	  (let ((te (get-context-theory-entry theory-id)))
	    (mapc #'(lambda (fe)
		      (let ((status (fe-status fe)))
			(format t "~%    ~55,1,0,'.a~(~10a~)"
			  (fe-id fe)
			  (fe-proof-status-string fe valid?))
			(incf tot)
			(case status
			  ((proved-complete proved-incomplete)
			   (if valid?
			       (incf proved)
			       (incf unfin)))
			  ((unchecked unfinished)
			   (incf unfin))
			  (t (incf untried)))))
		  (te-formula-info te)))))
    (format t "~%    Theory totals: ~d formulas, ~d attempted, ~d succeeded."
	tot (+ proved unfin) proved)
    (values tot proved unfin untried)))

;;; Support for browsing proofs

;;; The *show-proofs-info* variable contains the header and proofs.  The
;;; car is the string representing the header, and the cdr is the list of
;;; proofs.

(defvar *show-proofs-info* nil)

(defun display-proofs-formula-at (name origin line)
  (multiple-value-bind (fdecl place)
      (formula-decl-to-prove name line origin)
    (cond ((null fdecl)
	   (pvs-message "Not at a formula declaration"))
	  ((null (proofs fdecl))
	   (pvs-message "Formula ~a does not have any proofs" (id fdecl)))
	  (t (setq *show-proofs-info*
		   (cons 'formula
			 (cons fdecl
			       (mapcar #'(lambda (p) (cons fdecl p))
				 (proofs fdecl)))))
	     (display-proofs-buffer)))))

(defun display-proofs-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond (theory
	   (setq *show-proofs-info*
		 (cons 'theory
		       (cons theory
			     (mapcan #'(lambda (d)
					 (when (typep d 'formula-decl)
					   (mapcar #'(lambda (p) (cons d p))
					     (proofs d))))
			       (all-decls theory)))))
	   (display-proofs-buffer))
	  (t (pvs-message "~a has not been typechecked" theoryname)))))

(defun display-proofs-pvs-file (filename)
  (let ((theories (get-theories filename)))
    (cond (theories
	   (setq *show-proofs-info*
		 (cons 'pvs-file
		       (cons filename
			     (mapcan
				 #'(lambda (theory)
				     (mapcan #'(lambda (d)
						 (when (typep d 'formula-decl)
						   (mapcar #'(lambda (p)
							       (cons d p))
						     (proofs d))))
				       (all-decls theory)))
			       theories))))
	   (display-proofs-buffer))
	  (t (pvs-message "PVS file ~a is not in the current context"
	       filename)))))

(defun display-proofs-buffer (&optional line)
  (let ((idsize (max 8
		     (apply #'max
		       (mapcar #'(lambda (fs)
				   (length (string (id (cdr fs)))))
			 (cddr *show-proofs-info*)))))
	(declsize (max 11
		       (apply #'max
			 (mapcar #'(lambda (fs)
				     (length (string (id (car fs)))))
			   (cddr *show-proofs-info*)))))
	(thsize (max 6
		     (apply #'max
			 (mapcar #'(lambda (fs)
				     (length (string (id (module (car fs))))))
			   (cddr *show-proofs-info*))))))
    (pvs-buffer "Display Proofs"
      (format nil "~a~%~{~a~%~}"
	(display-proofs-header (car *show-proofs-info*)
			       (cadr *show-proofs-info*)
			       idsize declsize thsize)
	(proofs-formula-strings (car *show-proofs-info*)
				(cddr *show-proofs-info*)
				idsize declsize thsize))
      t t))
  (when line
    (pvs-locate "Display Proofs" nil (list line 0))))

(defparameter *proofs-format-string*
  "~a~va ~:[~*~;~:*~va ~]~:[~*~;~:*~va ~]~10a ~17a ~a")

(defun display-proofs-header (type obj idsize declsize thsize)
  (concatenate 'string
    (format nil "~?" *proofs-format-string*
	    (list "  "
		  idsize
		  "Proof Id"
		  (unless (eq type 'formula) declsize)
		  (unless (eq type 'formula) "Declaration")
		  (when (eq type 'pvs-file) thsize)
		  (when (eq type 'pvs-file) "Theory")
		  "Status"
		  "Date"
		  "Description"))
    (format nil "~%~?" *proofs-format-string*
	    (list "  "
		  idsize
		  "--------"
		  (unless (eq type 'formula) declsize)
		  (unless (eq type 'formula) "----")
		  (when (eq type 'pvs-file) thsize)
		  (when (eq type 'pvs-file) "------")
		  "------"
		  "----"
		  "----------------"))))

(defun proofs-formula-strings (type proofs idsize declsize thsize)
  (mapcar #'(lambda (prf)
	      (proof-formula-string type (car prf) (cdr prf)
				    idsize declsize thsize))
    proofs))

(defun proof-formula-string (type fdecl prf idsize declsize thsize)
  (format nil "~?" *proofs-format-string*
	  (list (if (eq prf (default-proof fdecl)) "+ " "  ")
		idsize
		(id prf)
		(unless (eq type 'formula) declsize)
		(unless (eq type 'formula) (id fdecl))
		(when (eq type 'pvs-file) thsize)
		(when (eq type 'pvs-file) (id (module fdecl)))
		(string-downcase (string (status prf)))
		(if (run-date prf)
		    (date-string (run-date prf))
		    "")
		(or (description prf) ""))))

(defun proofs-get-proof-at (line)
  (let ((pair (proofs-get-pair-at line)))
    (values (car pair) (cdr pair))))

(defun proofs-get-pair-at (line)
  (nth (+ (case (car *show-proofs-info*)
	    (formula -3)
	    (theory -3)
	    (pvs-file -3))
	  line)
       (cddr *show-proofs-info*)))

(defun set-proofs-default (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (setf (default-proof fdecl) prf)
    (display-proofs-buffer line)))

(defun proofs-delete-proof (line)
  (let* ((pair (proofs-get-pair-at line))
	 (fdecl (car pair))
	 (prf (cdr pair)))
    (setf (cdr *show-proofs-info*)
	  (delete pair (cdr *show-proofs-info*)))
    (setf (proofs fdecl) (delete prf (proofs fdecl)))
    (when (eq prf (default-proof fdecl))
      (setf (default-proof fdecl) (car (proofs fdecl))))
    (display-proofs-buffer line)))

(defun proofs-rename (line id)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (setf (id prf) id)
    (display-proofs-buffer line)))

(defun proofs-show-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (pvs-buffer (format nil "Proof:~a" (id prf))
      (with-output-to-string (out)
	(format out "Id: ~a~%Description: ~a~%Status: ~a~%~
                     Formula Declaration: ~a~%Creation Date: ~a~%~
                     Date Last Run: ~a~%Run Time: ~a~%Proof:~%"
	  (id prf)
	  (or (description prf) "None")
	  (string-downcase (status prf))
	  (id fdecl)
	  (if (create-date prf)
	      (date-string (create-date prf))
	      "Unknown")
	  (if (run-date prf)
	      (date-string (run-date prf))
	      "Unknown")
	  (or (run-time prf)
	      "Unknown"))
	(write (editable-justification (script prf))
	       :stream out :pretty t :escape t
	       :level nil :length nil
	       :pprint-dispatch *proof-script-pprint-dispatch*))
      t)))

(defun proofs-change-description (line description)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (setf (description prf) description)
    (display-proofs-buffer line)))

(defun proofs-rerun-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (setf (default-proof fdecl) prf)
    (let ((*current-theory* (module fdecl)))
      (read-strategies-files)
      (auto-save-proof-setup fdecl)
      (setq *last-proof* (prove (id fdecl) :strategy '(rerun)))
      (unless (from-prelude? fdecl)
	(save-all-proofs *current-theory*)
	;; If the proof status has changed, update the context.
	(update-context-proof-status fdecl))
      (remove-auto-save-proof-file))
    (let ((*to-emacs* t))
      (display-proofs-buffer line))))

(defun proofs-edit-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (setq *edit-proof-info* (list fdecl (place fdecl) "Display Proofs" 0))
    (pvs-buffer "Proof"
      (with-output-to-string (out)
	(write (editable-justification (script prf))
	       :stream out :pretty t :escape t
	       :level nil :length nil
	       :pprint-dispatch *proof-script-pprint-dispatch*))
      'popto)))

(defun edit-proof-at (filename line origin buffer prelude-offset full-label)
  (multiple-value-bind (fdecl place)
      (formula-decl-to-prove filename line origin)
    (when fdecl
      (setq *edit-proof-info* (list fdecl place buffer prelude-offset)))
    (cond ((and fdecl (justification fdecl))
	   ;;(setq *current-theory* (module fdecl))
	   (pvs-buffer "Proof"
	     (with-output-to-string (out)
	       (format out ";;; Proof ~a for formula ~a.~a~%"
		 (id (default-proof fdecl)) (id (module fdecl)) (id fdecl))
	       (write (editable-justification (justification fdecl)
					      nil nil (when full-label ""))
		      :stream out :pretty t :escape t
		      :level nil :length nil
		      :pprint-dispatch *proof-script-pprint-dispatch*))
	     'popto))
	  (fdecl
	   (pvs-buffer "Proof" " " 'popto)
	   (pvs-message "Formula ~a has no proof to edit"
	     (id fdecl)))
	  (t (pvs-message "Unable to find formula declaration")))))

(defun show-all-proofs-nostatus (outstr theoryid proofs)
  (dolist (prf proofs)
    (format outstr "~3%~a.~a~2%"
      theoryid (car prf))
    (write (get-editable-justification prf)
	   :stream outstr :pretty t :escape t :level nil :length nil
	   :pprint-dispatch *proof-script-pprint-dispatch*)))

(defun get-editable-justification (prf)
  (if (integerp (cadr prf))
      (editable-justification
       (fifth (nth (cadr prf) (cddr prf))))
      (editable-justification (cdr prf))))

(defun show-all-proofs-theory* (outstr proofs decls theory)
  (dolist (prf proofs)
    (let ((decl (find-if #'(lambda (d)
			     (and (typep d 'formula-decl)
				  (eq (id d) (car prf))))
		  decls)))
      (when decl
	(format outstr "~3%~a.~a: ~a~2%"
	  (id theory) (id decl) (proof-status-string decl))
	(write (get-editable-justification prf)
	       :stream outstr :pretty t :escape t :level nil
	       :length nil :pprint-dispatch *proof-script-pprint-dispatch*)))))

(defun show-all-proofs-theory-ctx (outstr proofs finfo thid valid?)
  (dolist (prf proofs)
    (let* ((fe (car (member (car prf) finfo
			    :test #'(lambda (x y) (eq x (fe-id y))))))
	   (status (or (and fe (fe-proof-status-string fe valid?))
		       "unchecked")))
      (format outstr "~3%~a.~a: ~a~2%" thid (car prf) status)
      (write (get-editable-justification prf)
	     :stream outstr :pretty t :escape t :level nil :length nil
	     :pprint-dispatch *proof-script-pprint-dispatch*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstep chain-antecedent (fnum)
  (let ((fnums (gather-fnums (s-forms (current-goal *ps*))
			      fnum '+)))
    (chain-antecedent*$ fnums))
  "Forward chains on antecedent formula chosen by FNUM (which can be
- or *)."
  "Forward chaining on antecedent formula ~a")

(defhelper chain-antecedent* (fnums)
  (if (consp fnums) ;;fnums has to be a list from chain-antecedent
      (let ((fnum (car fnums))
	    (fmla1 (formula (car (select-seq (s-forms (current-goal *ps*))
				    fnum))))
	    (nfmla1 (negate fmla1))
	    (rest-fnums (cdr fnums))
	    (info (when (forall-expr? nfmla1)
		    (check-forward-formula nfmla1))))
	(if info
	    (let ((conc (car info))
		  (antec-fmlas (cdr info))
		  (formlist (remove fmla1
				    (append *-*
					    (mapcar #'negate *+*))))
		  (sub (forward-match* conc antec-fmlas
				      formlist formlist nil)))
	      (if (eq sub 'fail)
		  (chain-antecedent* rest-fnums)
		  (let ((fsub (loop for var in
				    (bindings nfmla1)
				    collect
				    (cdr (assoc var sub
						:test
						#'same-declaration)))))
		    (then* (inst  fnum :terms fsub)
			   (let ((x (car *new-fmla-nums*)))
			     (split x))
			   (flatten)))))
		  (chain-antecedent* rest-fnums)))
      (skip-msg "Could not find a matching forward-chaining antecedent
formula."))
  "Forward chains on antecedent formulas given by FNUMS until a match is
found. "
  "Forward chaining on antecedent formulas ~a")

(defun check-forward-lemma (res)
  (let* ((fmlas (create-formulas res))
         (fmla (when fmlas (car fmlas)))
	 )
    (check-forward-formula fmla)))

(defun check-forward-formula (fmla)
  (let* ((body (forall-body* fmla))			      
	 (antec (when (implies-expr? body)
		  (args1 body)))
	 (antec-fmlas (when antec (conjuncts antec)))
	 (conc (if (implies-expr? body) (args2 body) body)))
    (if (subsetp (freevars conc)(freevars antec-fmlas)
		 :test #'tc-eq)
	(cons conc antec-fmlas)
	nil)))

(defun make!-update-expr (expression assignments &optional expected)
  (let ((type (or expected (type expression))))
    (make-instance 'update-expr
      'expression expression
      'assignments assignments
      'type type
      'types (list type))))

(defun make-assignment-subst-expr* (args expr type proj)
  (make!-update-expr proj (list (mk-assignment nil args expr)) (type proj)))

(defun translate-update-to-if-ass (assignments expr args &optional chain?)
  (if (null assignments)
      (translate-update-to-if (make-applications expr args))
      (let* ((ass (car assignments))
	     (ass-args (arguments ass))
	     (ass-expr (expression ass)))
	(multiple-value-bind (cond remass remargs)
	    (make-update-condition ass-args args)
	  (let ((then (if remass
			  (make!-update-expr (make-applications expr args)
					     (list (mk-assignment nil
						     remass ass-expr)))
			  (make-applications ass-expr remargs)))
		(else (translate-update-to-if-ass
		       (cdr assignments) expr args t)))
	    (if chain?
		(make-chained-if-expr cond then else)
		(make-if-expr cond then else)))))))

(defun reduce-update (redex expr updates args in-beta-reduce?
			    new-updates)
  ;;reduce-update constructs the reduced form of the given
  ;;update redex.
  (let ((*generate-tccs* 'NONE)));;NSH(9.15.94): prevents TCCS.
    (if (eq updates 'NOIDEA)
	redex
	(if (null updates)
	    (let* ((newexpr (make-application* expr args))
		   (newexpr
		    (if in-beta-reduce?
			(beta-reduce newexpr)
			(multiple-value-bind (sig value)
			    (assert-if-application
			     newexpr expr
			     (make-arg-tuple-expr args) '?)
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

(defun record-update-reduce (expr op arg) ;;NSH(7.15.94):removed sig
;;expr applies record-access to record-update: a(exp WITH [..])
;;new-application ensures that any newly created redexes are reduced.
    (let ((update
	   (find op
		 (assignments arg)
		 :test
		 #'(lambda (x y)
		     (eq x (id (caar (arguments y)))))))
	  (expr-of-arg (expression arg)))
      (if update
	  (if (cdr (arguments update)) ;;;a curried update::
	                             ;;;a(exp WITH [((a)(i)):= e])
	      (let ((newexpr  ;;NSH(9.15.94): otherwise TCCs
		             ;;are generated when domain is subtype.
		     ;;(let ((*generate-tccs* 'NONE)))
		       (make!-update-expr
			(make-record-update-reduced-application
			 op expr-of-arg)
			(list (lcopy update 'arguments
				     (cdr (arguments update))))
			(type expr))))
	      (do-auto-rewrite
	       newexpr
	       '?));;return a(exp) WITH [(i) := e] simplified.
	      (values '? (expression update)))
	  (do-auto-rewrite  (make-record-update-reduced-application
			     op expr-of-arg)
			    '?))))

(defun tuple-update-reduce (index arg)
  (let ((update (find index (assignments arg)
		      :test #'(lambda (x y)
				(= x (number (caar (arguments y)))))))
	(expr-of-arg (expression arg)))
    (if update
	(if (cdr (arguments update)) ;;;a curried update::
	                             ;;;PROJ_n(exp WITH [(n)(i):= e])
	    (let ((newexpr (make!-update-expr
			    (make-tuple-update-reduced-projection
			     index expr-of-arg)
			    (list (lcopy update 'arguments
					 (cdr (arguments update))))
			    (type (make-projection-application index arg)))))
	      (do-auto-rewrite
	       newexpr
	       '?));;return a(exp) WITH [(i) := e] simplified.
	    (values '? (expression update)))
	(do-auto-rewrite (make-tuple-update-reduced-projection
			  index expr-of-arg)
			 '?))))

(defmethod beta-reduce* ((expr field-application))
  (with-slots (id argument) expr
    (let ((arg (beta-reduce* argument)))
      (cond ((record-redex? expr)
	     (beta-reduce*
	      (expression
	       (find id (assignments arg)
		     :test #'(lambda (x y)
			       (eq x (id (caar (arguments y)))))))))
	    ((record-update-redex? expr)
	     (let ((update-field (find id (assignments arg)
				       :test
				       #'(lambda (x y)
					   (eq x (id (caar (arguments y))))))))
	       (if update-field
		   (if (cdr (arguments update-field))
		       (beta-reduce*
			(make!-update-expr
			 (make-field-application id
						 (expression arg))
			 (list (lcopy update-field 'arguments
				      (cdr (arguments update-field))))
			 (type expr)))
	       (beta-reduce* (expression update-field)))
		   (beta-reduce*
		    (make-field-application id (expression arg))))))
	    (t (lcopy expr 'argument arg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Changes for the new decision procedures.

;;; New (From dc-pvs)

(eval-when (load compile eval)
  (unless (find-package 'dp) (make-package 'dp)))

(unless (member "src/prover/dc-prototypes" *pvs-directories*)
  (setq *pvs-directories*
	(append *pvs-directories*
		(list "src/prover/dc-prototypes"))))

(unless (member "src/prover/dc-prototypes/polylib" *pvs-directories*)
  (setq *pvs-directories*
	(append *pvs-directories*
		(list "src/prover/dc-prototypes/polylib"))))


(defun dp::dp-changed (old-cong-state new-cong-state)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::invoke-process (eqn cong-state)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::null-single-cong-state ()
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::push-new-cong-state (cong-state)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::npop-cong-state (cong-state)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::init-dp-0 ()
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::return-all-cong-states (made-cong-states)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defun dp::copy-cong-state (cong-state)
  (break "Should not reach here.~%It should be safe to continue, but please report this to pvs-bugs@csl.sri.com."))

(defvar *dp-state* nil)

(defvar *new-ground?* nil)
(defvar *old-ground?* t)
(defvar *newdc* nil)

(defvar *init-alists* (make-instance 'dpinfo
			'dpinfo-sigalist nil
			'dpinfo-findalist nil
			'dpinfo-usealist *init-usealist*))

(declaim (special *dp-changed* *alists* *dp-state*
		  *top-alists* *top-dp-state*))

(defvar *print-expanded-dpinfo* t)

(defmethod print-object ((alists dpinfo) stream)
  (if (or (not *print-expanded-dpinfo*) *debugging-print-object*)
      (call-next-method)
      (format stream "<#dpinfo:~a>" (dpinfo-findalist alists))))

(defun new-ground ()
  (setq *newdc* t
	*new-ground?* t
	*old-ground?* nil))

(defun old-ground ()
  (setq *newdc* nil
	*new-ground?* nil
	*old-ground?* t))

(defun both-ground ()
  (setq *newdc* nil
	*new-ground?* t
	*old-ground?* t))

(defmacro nprotecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (unwind-protect
	   (setq ,resultsym
		 (multiple-value-list (progn ,@body)))
	 (restore-old-cs ,new-cong-state))
       (values-list ,resultsym))))

(defmacro protecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defun alists-changed (old-alists new-alists)
  (not (and (eq (dpinfo-usealist old-alists)
		(dpinfo-usealist new-alists))
	    (eq (dpinfo-findalist old-alists)
		(dpinfo-findalist new-alists))
	    (eq (dpinfo-sigalist old-alists)
		(dpinfo-sigalist new-alists)))))

(defvar *break-on-ground-diff* t)

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

(defun translate-from-dc (expr)
  (cond
   ((eq expr dp::*true*) *true*)
   ((eq expr dp::*false*) *false*)
   (t expr)))

(defun translate-from-prove (expr)
  (cond
   ((eq expr 'true) 'true)
   ((eq expr 'false) 'false)
   (t expr)))

(defun invoke-process (lit)
  (catch 'context
    (let ((canlit (canon lit 'dont-add-use)))
      (process canlit))))

(defvar *dp-print-incompatible-warning* t)

(defmacro call-process (expr dp-state alists)
  (let ((g-expr (gentemp))
	(g-dp-state (gentemp))
	(g-alists (gentemp)))
    `(let* ((g-expr ,expr)
	    (g-dp-state ,dp-state)
	    (g-alists ,alists)
	    (typealist (append *local-typealist* typealist))
	    (sigalist (dpinfo-sigalist g-alists))
	    (findalist (dpinfo-findalist g-alists))
	    (usealist (dpinfo-usealist g-alists))
	    (new-expr (when *new-ground?* (top-translate-to-dc g-expr)))
	    ;; put in (typep expr 'syntax) check
	    ;; in case call-process is called from process-assert
	    ;; which already has translated exprs
	    (old-expr (if (typep g-expr 'syntax)
			  (top-translate-to-old-prove g-expr)
			  g-expr))
	    (new-result nil)
	    (old-result nil))
       (when *new-ground?*
	 (setq new-result (translate-from-dc
			   (dp::invoke-process new-expr g-dp-state))))
       (when *old-ground?*
	 (setq old-result (translate-from-prove-list
			   (invoke-process old-expr)))
	 (setf (dpinfo-sigalist g-alists) sigalist
	       (dpinfo-findalist g-alists) findalist
	       (dpinfo-usealist g-alists) usealist))
       (let ((not-incompatible
	      (or (not (and *new-ground?* *old-ground?*))
		  (and (compatible-dp-results new-result old-result)))))
	 (when (and *dp-print-incompatible-warning*
		    (not not-incompatible))
	   (format t "~%***IncompatibleWarning*** expr: ~A,~%new-result: ~A, ~%old-result:~A"
	     new-expr new-result old-result))
	 (assert (or (not *break-on-ground-diff*)
		     not-incompatible)
		 (*break-on-ground-diff*)))
       (setq *break-on-ground-diff* t)
       (if *new-ground?*
	   new-result
	   old-result))))

(defun init-cong-state ()
  (when *new-ground?* (dp::null-single-cong-state)))

(defun new-cs (old-cs)
  (when *new-ground?*
    (dp::push-new-cong-state old-cs)))

(defun restore-old-cs (new-cs)
  (when *new-ground?*
    (dp::npop-cong-state new-cs)))

(defun init-dp ()
  (when *new-ground?*
    (dp::init-dp-0)))

(defun compatible-dp-results (new-result old-result)
  (or (tc-eq new-result old-result)
      (and (eq new-result dp::*true*) (eq old-result TRUE))
      (and (eq new-result dp::*false*) (eq old-result FALSE))
      (and (or (listp old-result)
	       (typep old-result 'syntax))
	   (null new-result))))

(defvar *init-dp-state*
  (when nil
    (init-cong-state)))

(defun init-dc ()
  (init-dp)
  (dp::return-all-cong-states dp::*made-cong-states*)
  (setq *init-dp-state* (init-cong-state))
  (reset-translate-to-dc))

(defun sigma (term)
  (cond
   ((symbolp term) term)
   ((qnumberp term) term)
   ;; (SJ 5/14/86) added following clause.
   ((and *canon-beta-reduce-on* (isapplylambda term))
    (canon-beta-reduce term))
   ;; JMR 5/30/90.  Why was following clause missing from Stan's code? FIXME
   ((and (boolp term)
	 (is-apply-n-x (funsym term))
	 (memq (arg1 term)
	       (append *rational-pred*
		       *real-pred*
		       *integer-pred*)))
    TRUE)
   ((is-apply-n-x (funsym term)) (sigapply term))
   ((is-tupsel-n (funsym term)) (sigtupsel term))
   (t (case (funsym term)
	(PLUS       (sigplus term))
	(TIMES      (sigtimes term))
	(DIVIDE     (sigdivide term))
	(MINUS      (sigminus term))
	(DIFFERENCE (sigdifference term))
;	(TUPSEL     (sigtupsel term))
;	(ARRAYSEL   (sigapply term))
	(UPDATE     (sigupdate term))
;	(ARRAYREST  (sigarrayrest term))
	(FLOOR      (sigfloor term))
	(t          term)
      ))))


;;; Found in patch2
(defmacro translate-to-ground (expr)
  `(if *newdc*
      (translate-to-dc ,expr)
      (translate-to-prove ,expr)))

(defmacro translate-with-new-hash (&rest body)
  `(if *newdc*
       (let ((*translate-to-dc-hash*
	      (make-pvs-hash-table  ;;NSH(2.5.95)
	       :hashfn #'pvs-sxhash ;;hash to pvs-hash
	       :test #'tc-eq)))
	 ,@body)
      (let ((*translate-to-prove-hash*
	      (make-pvs-hash-table  ;;NSH(2.5.95)
	       :hashfn #'pvs-sxhash ;;hash to pvs-hash
	       :test #'tc-eq)))
	 ,@body)))

(defun top-translate-to-prove (expr)
  (let ((*bindings* nil)
	(*generate-tccs* 'NONE))
    (cond ((typep *translate-to-prove-hash* 'ht)
	   (setq *integer-pred* (translate-to-ground (predicate *integer*)))
	   (setq *rational-pred* (translate-to-ground (predicate *rational*)))
	   (setq *real-pred* (translate-to-ground (predicate *real*)))
	   (when *newdc*
	     (setf (dp::node-initial-type *integer-pred*) 'dp::integer-pred)
	     (setf (dp::node-initial-type *rational-pred*) 'dp::rational-pred)
	     (setf (dp::node-initial-type *real-pred*) 'dp::real-pred))
	   (translate-to-ground (unit-derecognize expr)))
	  (t (translate-with-new-hash
	       (unless *integer-pred*
		 (setq *integer-pred*
		       (when *integer*
			 (translate-to-ground (predicate *integer*)))))
	       (translate-to-ground (unit-derecognize expr)))))))

(defmacro with-zero-context (lisp-expr)
  `(nprotecting-cong-state
    ((*dp-state* *init-dp-state*)
     (*alists* *init-alists*))
    ,lisp-expr))

;;; Found in patch2-exp
(defcl* proofstate ()
  (label :initform " ")
  current-goal         ;;is a sequent
  (current-rule :initform nil)
  (alists :initform nil)       ;dpinfo
  (dp-state :initform *init-dp-state*)
  (done-subgoals :initform nil)
  (pending-subgoals :initform nil)
  current-subgoal      
  (remaining-subgoals :initform nil)
  (status-flag :initform nil)
  (subgoalnum :initform 0)
  (justification :initform nil)
  (current-input :initform nil)
  (printout :initform nil)
  (comment :initform nil)
  strategy
  (context :initform nil)
  (parent-proofstate :initform nil)
  (dependent-decls :initform nil)
  (current-auto-rewrites :initform nil)
  (tcc-hash :initform (make-pvs-hash-table       ;;NSH(7.26.94)
			   :hashfn #'pvs-sxhash
			   :test #'tc-eq))
  (subtype-hash :initform (make-pvs-hash-table
			   :hashfn #'pvs-sxhash
			   :test #'tc-eq))
  (rewrite-hash :initform (make-pvs-hash-table
			   :hashfn #'pvs-sxhash
			   :test #'tc-eq))
  (current-xrule :initform nil))

(defun apply-step (step &optional comment save? time?)
  #'(lambda (ps)
      (apply-step-body ps step comment save? time?)))

(defun apply-step-body (ps step comment save? time?)
  (let* ((*generate-tccs* 'NONE)
	 (strat (let ((*in-apply* ps));;NSH(8.22.94)
		  ;;otherwise (apply (query*)) misbehaves.
		  (strat-eval* step ps))))
    (cond ((or (typep strat 'strategy)
	       (typep strat 'rule-instance))
	   (let* ((new-strat
		   (if (typep strat 'strategy)
		       strat
		       (make-instance 'strategy
			 'topstep strat)))
		  (newps0 (copy ps
			    'strategy
			    new-strat
			    'parent-proofstate nil))
		  (newps (if (typep newps0 'top-proofstate)
			     (change-class newps0 'proofstate)
			     newps0))
		  (*noninteractivemode* t)
		  (*suppress-printing* t)
		  (*dependent-decls* nil)
		  (init-time (get-internal-run-time))
		  (result		;catch 'abort-in-apply ;;NSH(8.22.94)
		   (let ((*in-apply* ps)
			 )
		     (prove* newps)))
		  (end-time (/ (- (get-internal-run-time) init-time)
			       internal-time-units-per-second)))
                         ;;;;(break "in-apply")
	     ;;		 if (null result);;when aborted ;;NSH(8.22.94)
	     ;;		     if *in-apply*
	     ;;			 (throw 'abort-in-apply nil)
	     ;;			 (values 'X nil nil)
	     (let* ((subgoals (collect-subgoals newps))
		    (justif (collect-justification newps))
		    (xrule `(apply
				(rerun
				 ,(editable-justification
				   justif nil T)))))
	       ;; (format t "~%step= ~a~%decls = ~a" step *dependent-decls*)
	       (when time? (format t "~%;;;Used ~,2F seconds in ~s." end-time step))
	       (if (eq (status-flag newps) 'XX)
		   (values 'X nil nil);;was 'XX
		   (if (or (eq (status-flag newps) 'X)
			   (and (singleton? subgoals)
				(or (and (not (consp (car subgoals)))
					 (exequal (car subgoals)
						  (current-goal ps)))
				    (and (consp (car subgoals))
					 (exequal (caar subgoals)
						  (current-goal ps))
					 (eq
					  (nth 8 (car subgoals))
					;(nth 6 (car subgoals))
					  (current-auto-rewrites newps))))))
		       (if save?
			   (values '? (list (current-goal newps)) nil)
			   (values 'X nil nil))
		       (if (eq (status-flag newps) '!);;(null subgoals)
			   (values '! nil
				   (list 'dependent-decls
					 *dependent-decls*
					 'current-xrule
					 xrule))
			   (values '? subgoals
				   (list;;NSH(4.20.95)
				    ;;'dependent-decls
				    ;; *dependent-decls*
				    'current-xrule
				    xrule))))))))
	  (t (values 'X nil nil)))))

(addrule 'apply (strategy) (comment save? time?) (apply-step strategy comment save? time?) 
	 "Applies STRATEGY as if it were a rule, and prints COMMENT string.
If SAVE? is T, then the APPLY step is saved even if the strategy
does nothing, e.g., (SKIP), which is useful for setting values of
globals, e.g., (APPLY (LISP (setq *xx* ...)) \"recording value of *xx*\" T).
This is the basic way of converting a glass-box strategy into an
atomic step so that internal execution of the strategy is hidden
and only the resulting subgoals are returned.  E.g.,
 (apply (then (skosimp*)(flatten)(inst?))
      \"Skolemizing, flattening, and instantiating\")."
	 "~%Applying ~%   ~s,~@[~%~a~]")

(defmethod collect-subgoals ((ps proofstate) &optional accum)
  (mapcar #'(lambda (x) (pushnew x *dependent-decls*))
			(dependent-decls ps))
  (cond ((and (eq (status-flag ps) '*)
	      (null (remaining-subgoals ps)))
	 (cond ((null (pending-subgoals ps))
		(cons (list (current-goal ps)
			    'alists (alists ps)
			    'dp-state (dp-state ps)
			    'context (context ps)
			    'current-auto-rewrites (current-auto-rewrites ps)
			    'subtype-hash (subtype-hash ps)
			    'rewrite-hash (rewrite-hash ps)
			    'dependent-decls *dependent-decls*
			    'comment (comment ps))
		      accum))
	       (t (collect-subgoals (nreverse (pending-subgoals ps))
				    accum))))
	(t (collect-subgoals (remaining-subgoals ps) accum))))


;;;Found in patch2-exp

(fmakunbound 'prove-decl)

(defvar *recursive-prove-decl-call* nil)

(eval-when (eval compile load)
  (setq *auto-rewrites-ops* (init-symbol-table))
  (setq *auto-rewrites* (init-symbol-table))
  (setq *auto-rewrites-ops* (init-symbol-table))
  (setq *subtype-of-hash* (init-symbol-table))
  (setq *prtype-hash* (init-symbol-table))
  (setq *local-prtype-hash* (init-symbol-table))
  (setq *beta-cache* (init-symbol-table))
  (setq *match-cache* (init-symbol-table))
  (setq *assert-if-arith-hash* (init-symbol-table))
  (setq *pvs-sxhash-cache* (init-symbol-table))
  (setq *term-print-strings* (init-symbol-table))
  (setq *translate-to-prove-hash*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  (setq *translate-id-hash*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  (setq *dc-named-exprs*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  (setq *translate-to-dc-hash*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  (setq *dc-translate-id-hash*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  (setq *create-formulas-cache*
	(make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  )

(defmacro init-if-rec (gvar)
  (let ((val (gentemp)))
    `(if *recursive-prove-decl-call*
	 (let ((,val ,gvar))
	   (if (hash-table-p ,val)
	       (setq ,gvar (init-symbol-table))
	       (setq ,gvar
		     (make-pvs-hash-table
		      :hashfn #'pvs-sxhash :test #'tc-eq))))
	 ,gvar)))

(defmethod prove-decl ((decl formula-decl) &key strategy)
  (ensure-default-proof decl)
  (let ((init-real-time (get-internal-real-time))
	(init-run-time (get-internal-run-time))
	(*skovar-counter* nil)		;initialize counters
	(*skofun-counter* nil)
	(*bind-counter* nil)
	(*recursive-prove-decl-call* *in-checker*)
	(*in-checker* t)
	(*displaying-proof* nil)
	(*current-displayed* nil)
	(*flush-displayed* nil)
	(*auto-rewrites-names* nil)
	(*auto-rewrites-ops* (init-if-rec *auto-rewrites-ops*))
	(*auto-rewrites* (init-if-rec *auto-rewrites*))
	(*auto-rewrites-off* nil)
	(auto-rewrites-info
	 (make-instance 'auto-rewrites-info
	   'rewrites (init-symbol-table)
	   'auto-rewrites-names nil
	   'auto-rewrites!-names nil
	   'macro-names nil))	
	(*subtype-names* nil)
	(*named-exprs* nil)
	(*rec-type-dummies* nil)
	(*assert-typepreds* nil)
	(*pvs-bdd-hash* nil)
	(*bdd-pvs-hash* nil)
	(*bdd-counter* *bdd-counter*)
	(*subtype-of-hash* (init-if-rec *subtype-of-hash*))
	(*track-rewrites* nil)
	(*context-modified* nil)
	(*generate-tccs* 'NONE);;
	(*rewrite-msg-off* *rewrite-msg-off*);NSH(3.5.97)
	(*ruletrace* NIL);;NSH(5.6.95):for tracing strategies.
	(*ruletracedepth* 0)
	(*translate-to-prove-hash* (init-if-rec *translate-to-prove-hash*))
	(*translate-id-hash* (init-if-rec *translate-id-hash*))
	(*dc-named-exprs* (init-if-rec *dc-named-exprs*))
	(*translate-id-counter* nil)
	(*translate-to-dc-hash* (init-if-rec *translate-to-dc-hash*))
	(*dc-translate-id-hash* (init-if-rec *dc-translate-id-hash*))
	(*dc-translate-id-counter* nil)
	(*prtype-hash* (init-if-rec *prtype-hash*))
	(*local-prtype-hash* (init-if-rec *local-prtype-hash*))
	(*beta-cache* (init-if-rec *beta-cache*))
	(*match-cache* (init-if-rec *match-cache*))
	(*assert-if-arith-hash* (init-if-rec *assert-if-arith-hash*))
	(*create-formulas-cache* (init-if-rec *create-formulas-cache*))
	(*pvs-sxhash-cache* (init-if-rec *pvs-sxhash-cache*))
	(*term-print-strings* (init-if-rec *term-print-strings*))
	(*all-subst-mod-params-caches* (copy-subst-mod-params-cache))
	(*pseudo-normalize-hash* (pvs-copyhash *pseudo-normalize-hash*))
	(*pseudo-normalize-translate-id-hash*
	 (pvs-copyhash *pseudo-normalize-translate-id-hash*))
	(typealist primtypealist);;NSH(2.16.94): now global to a proof.
	(*local-typealist* *local-typealist*)
	(applysymlist nil)
	(sigalist sigalist)
	(usealist usealist)
	(findalist findalist)
	(*alists* (make-instance 'dpinfo
		    'dpinfo-sigalist sigalist
		    'dpinfo-findalist findalist
		    'dpinfo-usealist usealist))
	(*dp-state* (when *new-ground?*
		      (dp::push-new-cong-state *init-dp-state*)))
	(*current-context* (context decl)))
    (initprover)			;initialize prover
    (newcounter  *skovar-counter*)
    (newcounter  *skofun-counter*)
    (newcounter  *bind-counter*)
    (newcounter *translate-id-counter*)
    (newcounter *dc-translate-id-counter*)
    (unless (closed-definition decl)
      (setf (closed-definition decl)
	    (universal-closure (definition decl))))
    (let* ((top-formula (closed-definition decl))
	   (s-form (make-instance 's-formula 'formula top-formula))
	   (sequent (make-instance 'sequent 's-forms (list s-form)))
	   (*top-proofstate*
	    (make-instance 'top-proofstate
	      'current-goal sequent
	      'label (string (id decl))
	      'strategy (if strategy strategy (query*-step))
	      'context *current-context*
	      'alists (make-dpinfo sigalist findalist usealist)
	      'dp-state *dp-state*
	      'justification (justification decl)
	      'declaration decl
	      'current-auto-rewrites auto-rewrites-info
	      )))
      (before-prove*)
      (unwind-protect
	  (catch 'quit			;to quit proofs
	    (if *please-interrupt*
		(prove* *top-proofstate*)
		(with-interrupts-deferred
		 (prove* *top-proofstate*))))
	(after-prove*)
	(unless *recursive-prove-decl-call*
	  (save-proof-info decl init-real-time init-run-time)))
      *top-proofstate*)))

(defun copy-subst-mod-params-cache ()
  (let ((table *all-subst-mod-params-caches*))
    (make-ht 
     :test (ht-test table)
     :size (ht-size table)
     :rehash-size (ht-rehash-size table)
     :rehash-threshold (ht-rehash-threshold table)
     :hashfn (ht-hashfn table)
     :num-elements 0
     :array (let ((new (make-array (ht-size table)))
		  (old (ht-array table)))
	      (dotimes (x (ht-size table))
		(setf (svref new x)
		      (mapcar #'(lambda (pair)
				  (cons (car pair) (pvs-copyhash (cdr pair))))
			      (svref old x))))
	      new))))

(defmethod prove-decl ((decl declaration) &key strategy)
  (format-if "~%Couldn't find formula ~a in module ~a."
	     (id decl) (id *current-theory*)))

;;;Found in patch2-test

;;;I couldn't find substantive difference here,
;;;but it was in my interface file.
;;; Later bug found here in that *alists* were not being updated.
(defun rule-apply (step ps)
  (let* ((*ps* ps)
	 (* '*)
	 (*goal* (current-goal ps))
	 (*label* (label  ps))
					;	 (*subgoalnum* (subgoalnum ps))
	 (*+* (mapcar #'formula (pos-s-forms (s-forms (current-goal ps)))))
	 (*-* (mapcar #'formula (neg-s-forms (s-forms (current-goal ps)))))
	 (*par-label* (when (parent-proofstate ps)
			(label (parent-proofstate ps))))
	 (*par-goal* (when (parent-proofstate ps)
		       (current-goal (parent-proofstate ps))))
	 (*current-context* *current-context*)
	 (*module-context* (context *current-theory*))
	 (*current-theory* *current-theory*)
	 (*auto-rewrites* (rewrites ps))
	 (*auto-rewrites-names* (auto-rewrites-names ps))
	 (*auto-rewrites!-names* (auto-rewrites!-names ps))
	 (*macro-names* (macro-names ps))	 
	 (*rewrite-hash* (rewrite-hash ps))
	 (*alists* (alists ps))
	 (*dp-state* (dp-state ps)))
    ;;(break)
    (cond ((typep step 'rule-instance);;if step is a rule, then
	   ;;reinvoke rule-apply with corresponding strategy. 
	   (rule-apply (make-instance 'strategy
			 'topstep step)
		       ps));;else step is a strategy
	  ((typep (topstep step) 'rule-instance)
	  (let* ((*tccforms* NIL)
		 ;;(*generate-tccs* NIL) ;;NSH(10.20.94)
		 (topstep (topstep step))
		 (name (if (consp (rule-input topstep))
			   (car (rule-input topstep))
			   topstep)))
	    (when (memq name *ruletrace*)
	      (format t "~%~vTEnter: ~a" *ruletracedepth*
		      (rule-input topstep))
	      (incf *ruletracedepth*))
	    (multiple-value-bind (signal subgoals
					 updates)
		(funcall (rule topstep) ps) ;;(break "rule-ap")
	      (cond ((eq signal '!);;success
		     (when (memq name *ruletrace*)
		       (decf *ruletracedepth*)
		       (format t "~%~vT Exit: ~a -- Proved subgoal"
			   *ruletracedepth* name ))
		     (setf (status-flag ps) '!      
			   (current-rule ps)(rule-input topstep)
			   (printout ps)
			   (rule-format topstep)
			   (justification ps)
			   (make-instance 'justification
			     'label (label-suffix (label ps))
			     'rule (rule-input topstep)
			     'comment (new-comment ps)))
		     (make-updates updates ps)
		     ps)
		    ((eq signal '?);;subgoals generated
		     (let* ((*tccforms* (remove-duplicates *tccforms*
					  :test #'tc-eq
					  :key #'tccinfo-formula))
			    (tcc-hash-counter 0)
			    (*tccforms*
			     (loop for tcc in *tccforms*
				   when (or
					 (null (pvs-gethash
					       (tccinfo-formula tcc)
					       (tcc-hash ps)))
					 (and (incf tcc-hash-counter)
					      nil))
				   collect tcc))
			    (new-tcc-hash
			     (if *tccforms*
				 (pvs-copyhash (tcc-hash ps))
				 (tcc-hash ps)))
			    (tccforms (assert-tccforms *tccforms*
						       ps))
			    (tcc-subgoals
			     (mapcar
			      #'(lambda (x)
				  (let ((y 
					 (change-class
					  (copy (current-goal ps)
					    's-forms
					    (cons
					     (make-instance
						 's-formula
					       'formula
					       (tccinfo-formula x))
					     (s-forms
					      (current-goal ps))))
					  'tcc-sequent)))
				    (setf (tcc y)
					  (tccinfo-formula x)
					  (reason y)
					  (tccinfo-reason x)
					  (expr y)
					  (tccinfo-expr x)
					  (kind y)
					  (tccinfo-kind x)
					  (type y)
					  (tccinfo-type x))
				    y))
			      tccforms))
			    (subgoal-proofstates
			     (make-subgoal-proofstates
			      ps
			      (subgoal-strategy step)
			      subgoals
			      tcc-subgoals
			      ;updates;must be attached to subgoals.
			      )))
		       ;;cleaning up (NSH(7.27.94)
		       ;;1. convert main subgoals of tccs into
		       ;;non-tccs.
		       ;;2. hash the new tccs into new-tcc-hash
		       ;;3. set tcc-hash of main subgoals as
		       ;;new-tcc-hash
		       (when (> tcc-hash-counter 0)
			 (format-if "~%Ignoring ~a repeated TCCs."
				  tcc-hash-counter))
		       (loop for tcc in *tccforms*
			     do
			     (setf (pvs-gethash (tccinfo-formula tcc)
						new-tcc-hash)
				   T))
		       (assert
			(every #'(lambda (sps)
				   (every #'(lambda (sfmla)
					      (null (freevars (formula sfmla))))
					    (s-forms (current-goal sps))))
				 subgoal-proofstates))
		       (loop for sps in subgoal-proofstates
			     when (not (tcc-proofstate? sps))
			     do (setf (tcc-hash sps)
				      new-tcc-hash))
		       (when (memq name *ruletrace*)
			 (decf *ruletracedepth*)
			 (format t "~%~vT Exit: ~a -- ~a subgoal(s) generated."
			   *ruletracedepth* name (length subgoal-proofstates)))
		       (push-references *tccforms* ps)
		       (setf (status-flag ps) '?
			     (current-rule ps) (rule-input topstep)
			     (printout ps)
			     (rule-format topstep)
			     (remaining-subgoals ps)
			     subgoal-proofstates)
		       (unless (typep ps 'top-proofstate)
			 (setf (strategy ps) nil))
		       (make-updates updates ps)
		       ps))
		    ((eq signal 'X)
		     (when (memq name *ruletrace*)
		       (decf *ruletracedepth*)
		       (format t "~%~vT Exit: ~a -- No change."
			   *ruletracedepth* name))
		     (format-if "~%No change on: ~s" (rule-input topstep))
		     (setf (status-flag ps) nil;;start afresh
			   (strategy ps)
			   (failure-strategy step))
		     (make-updates updates ps)
		     ps)
		    ((eq signal 'XX);;marks the current goal a failure
		     (setf (status-flag ps) 'XX)
		     ps)
		    ((eq signal '*)
		     (setf (status-flag ps) '*)
		     ps)
		    (t  (undo-proof signal ps)))
	      )))
    ((typep (topstep step) 'strategy)
     (setf (status-flag ps) '?
	   ;;		 (current-rule ps) (strategy-input rule)
	   (remaining-subgoals ps)
	   (make-subgoal-proofstates ps
				     (topstep step)
				     (list (current-goal ps))))
				     ;nil
     ps)
    (t (format t "~%Bad rule: ~a~%" step);;treated as skip
					;(break)
       (setf (status-flag ps) nil;;start afresh
	     (strategy ps)
	     (failure-strategy (strategy ps)))
       ps))))

;;; Found in patch2-exp
(defun make-subgoal-proofstates (proofstate strategy subgoals
					    &optional tcc-subgoals)
;  (when (and (tcc-proofstate? proofstate)
;	     (not (tcc-sequent? (current-goal proofstate))))
;    (break "bad ps"))
  (let* ((allsubgoals (append subgoals tcc-subgoals))
	 (numsubgoals (length allsubgoals))
	 (colwidth (length (format nil "~a" numsubgoals))))
;;    (cond ((consp allsubgoals)))
    (loop for goal in allsubgoals
	  as goalnum from 1
	  collect
	  (let* ((sequent
		  (if (consp goal)(car goal) goal))
		 (tcc-to-sequent? (and (memq goal subgoals)
				       (tcc-sequent? sequent)
				       (tcc-sequent?;;NSH(11.3.94)
					(current-goal proofstate))
				       (eq (tcc (current-goal proofstate))
					   (tcc sequent))))
		 (goalstate
		  (make-instance
		      (if (typep strategy 'strategy)
			  'strat-proofstate
			  (if (and (tcc-sequent? sequent)
				   (not tcc-to-sequent?))
			      'tcc-proofstate
			      'proofstate))
		    'current-goal
		    (let* (
			   (sequent
			    (if tcc-to-sequent? ;;NSH(10.3.95)
				;;added copy since sequents are shared
				;;so destructive change-class is bad.
				(change-class (copy sequent) 'sequent)
				sequent)))
		      (clean-goal sequent))
		    'context (copy (context proofstate))
		    'strategy  strategy
		    'label
		    (if (= (length allsubgoals) 1)
			(label proofstate)
			(format nil "~a.~a" (label proofstate)
				goalnum))
		    'subgoalnum (1- goalnum)
		    'dependent-decls (dependent-decls proofstate)
		    'alists (copy (alists proofstate))
		    'dp-state (when *new-ground?*
				(dp::copy-cong-state (dp-state proofstate)))
		    'current-auto-rewrites
		    (current-auto-rewrites proofstate)
		    'rewrite-hash (rewrite-hash proofstate)
		    'subtype-hash (subtype-hash proofstate)
		    'parent-proofstate proofstate
		    'comment (comment proofstate))))
	    (if (consp goal)
		(make-updates (cdr goal)
			      goalstate)
		goalstate)))))

(defun make-update (field value ps)
  ;;(break)
  (case field
    (status-flag (setf (status-flag ps) value))
    (strategy (setf (strategy ps) value))
    (context (setf (context ps) value))
;    (out-context (setf (out-context ps) value))
;    (out-substitution (setf (out-substitution ps) value))
    (alists (setf (alists ps) value))
    (dp-state (setf (dp-state ps) value))
    (current-auto-rewrites (setf (current-auto-rewrites ps) value))
    (rewrite-hash (setf (rewrite-hash ps) value))
    (subtype-hash (setf (subtype-hash ps) value))
    (dependent-decls (setf (dependent-decls ps) ;;NSH(4.21.95):special
			   (union value (dependent-decls ps))))
    (justification (setf (justification ps) 
			 value)) ;;NSH(3.16.97) added for checkpointing
    (current-xrule (setf (current-xrule ps) value))
    (comment (setf (comment ps) value))))


;;; Found in eproofcheck.lisp
(defun assert-tccforms (tccforms ps)
  (let* ((dp-state (dp-state ps))
	 (alists (alists ps)))
    (nprotecting-cong-state
     ((*dp-state* dp-state)
      (*alists* alists))
     ;(break "atc")
     (let ((*rewrite-hash* (pvs-copyhash (rewrite-hash ps)))
	   (*subtype-hash* (pvs-copyhash (subtype-hash ps))))
       (assert-tccforms* tccforms ps)))))

(defun assert-test-list (fmla-list ps)
  (let* ((alists (alists ps))
	 (dp-state (dp-state ps)))
    (nprotecting-cong-state
     ((*dp-state* dp-state)
      (*alists* alists))
     (let ((*rewrite-hash* (pvs-copyhash (rewrite-hash ps)))
	   (*subtype-hash* (pvs-copyhash (subtype-hash ps))))
       (loop for fmla in fmla-list
	     nconc
	     (multiple-value-bind (sig value)
		 (assert-if fmla)
	       (cond ((tc-eq value *true*) nil)
		     ((eq sig 'X) (list fmla))
		     (t (list value)))))))))

;;;Found in patch2-exp
(defun cond-assert-if (expr &optional conditions)
  (if (number-expr? expr)  ;;NSH(4.7.96)
      (values 'X expr)
      (nprotecting-cong-state   ;;;changed from LET on alists
       ((*dp-state* *dp-state*)
	(*alists* *alists*))
      (let ((*rewrite-hash* (if *hash-rewrites?*
				(pvs-copyhash *rewrite-hash*)
				*rewrite-hash*))
	    ;;(typealist typealist) ;;NSH(2.16.94)
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


;;;Found in assert

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

;;;Found in patch2-exp
(defun assert-test  (fmla)
  (unless (check-for-connectives? fmla)
    (nprotecting-cong-state  ;;changed from LET on alists
     ((*dp-state* *dp-state*)
      (*alists* *alists*))
     (let (;;(typealist typealist) ;;NSH(2.16.94)
	   (result (call-process fmla *dp-state* *alists*)))
       result))))

(defun assert-test0 (fmla)
  (unless (check-for-connectives? fmla)
    (nprotecting-cong-state
     ((*dp-state* *init-dp-state*)
      (*alists* *init-alists*))
     (let (;;(typealist typealist) ;;NSH(2.16.94)
	   (result (call-process fmla *dp-state* *alists*)))
       result))))


;;;From assert
(defun pvs-initprover ()
  (setq *alists* (copy *init-alists*))
  (setq *dp-state* (dp::push-new-cong-state *init-dp-state*)))


;;;From patch2
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
			     (pvs-copyhash (rewrite-hash ps))
			     (rewrite-hash ps)))
	 (*subtype-hash* (if flush?
			     (make-pvs-hash-table :hashfn #'pvs-sxhash
						  :test #'tc-eq)
			     (pvs-copyhash (subtype-hash ps))))
	 (*assert-typepreds-off* (not type-constraints?))
	 (*alists* (alists ps))
	 (*dp-state* (dp-state ps)));;was let*
    (protecting-cong-state
     ((*dp-state* (if flush?
		      *init-dp-state*
		      *dp-state*))
      (*alists* (if flush?
		    *init-alists*
		    *alists*)))
     (assert-sequent goalsequent sformnums rewrite-flag))))


;;;From assert
(defun assert-sequent (sequent sformnums &optional rewrite-flag assert-connectives?) 
  (let* (
	 (simplifiable-sformnums
	  (find-all-sformnums (s-forms sequent) sformnums
			      #'(lambda (fmla)
				  (if (not-expr? fmla)
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
	       ;;(break)
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
					 'alists *alists*
					 ))))))))))))
(defun process-assert (forms) nil)

(defun process-assert (forms)
  (if (null forms) nil
      (let* ((fmla (car forms)))
	(cond ((disjunction? fmla)
	       (if (loop for x in (arguments fmla)
			 always (nprotecting-cong-state
				 ((*dp-state* *dp-state*)
				  (*alists* *alists*))
				 (let* (;;(sigalist sigalist)
					;;(findalist findalist)
					;;(usealist usealist)
					;;(typealist typealist);NSH(2.16.94)
					(result
					 (catch 'context 
					   (process-assert (cons x (cdr forms))))))
				   (tc-eq result *false*))))
		   (throw 'context *false*)))
	      ((or (negation? fmla) (implication? fmla) (ifff? fmla))
	       (process-assert (cdr forms)))
	      (t (let ((result (call-process fmla *dp-state* *alists*)))
		   (if (false-p result)
		       (throw 'context *false*)
		       (process-assert (cdr forms)))))))))

(defun assert-process-output (signal sequent)
  (nprotecting-cong-state ((*dp-state* *dp-state*)
			  (*alists* *alists*))
  (let* ;;(sigalist sigalist)
	 ;;(findalist findalist)
	 ;;(usealist usealist)
	 ;;(typealist typealist);;NSH(2.16.94)
	 ((result (catch 'context (process-assert *process-output*))))
    (cond ((tc-eq result *false*)(values '! sequent))
	  (t (values signal sequent))))))

;;;Found in patch2-exp
(defun assert-sform (sform &optional rewrite-flag simplifiable?)
  (let ((*assert-typepreds* nil)
	(*auto-rewrite-depth* 0))
    (multiple-value-bind (signal sform)
	(assert-sform* sform rewrite-flag simplifiable?)
      (cond ((eq signal '!)(values signal sform))
	    ((or (eq signal '?) *assert-typepreds*)
	     ;;(break "assert-typepreds")
	     (if (some #'(lambda (fmla)
			   (let* ((sign (not (not-expr? fmla)))
				  (body (if sign
					    fmla
					    (args1 fmla)))
				  (*update-occurs?* T))
			     ;;NSH(5.13.97): rearranged lets
			     ;;so translation avoided
			     ;;when there is a connective.
			     ;;want to check for connectives even if
			     ;;assert-connectives? is T
			     (and (not (connective-occurs? body))
				  (let ((res (call-process fmla
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

(defun assert-sform* (sform &optional rewrite-flag simplifiable?)
  (let* ((fmla (formula sform))
	 (sign (not (not-expr? fmla)))
	 (body (if sign fmla (args1 fmla)))
	 (*bound-variables* nil)
	 (*top-rewrite-hash* *rewrite-hash*))
    (protecting-cong-state
     ((*top-dp-state* *dp-state*)
      (*top-alists* *alists*))
     ;(break "0")
     (cond (rewrite-flag
	    (multiple-value-bind (sig newbodypart)
		(if (or (iff-expr? body)(equality? body))
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
			   (make-arg-tuple-expr
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
		(assert-if fmla);;NSH(5.1.96) was assert-if-inside
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
	 (result (call-process (negate newfmla) *dp-state* *alists*)))
    ;(break "cp")
    (if (consp result)
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


;;;Found in patch2-test
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
		  (progn (pvs-remhash expr *rewrite-hash*)
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
    (setf (pvs-gethash expr *rewrite-hash*)
	  (list result hashed-dp-state hashed-alists;;(cons findalist usealist)
		*auto-rewrites-names*
		*auto-rewrites!-names*
		*macro-names*))))

;;;Found in patch2-exp
(defun make-sum (list type)
  (if (connective-occurs? list)
      (make-sum* list type)
      (let ((*newdc* nil))
	(make-sum* (sort list #'(lambda (x y)
				  (arithord (top-translate-to-prove x)
					    (top-translate-to-prove y))))
		   type))))


;;;Found in assert
(defun make-prod (list type)
  (let ((*newdc* nil))
    (make-prod* (sort list #'(lambda (x y)
			       (arithord (top-translate-to-prove x)
					 (top-translate-to-prove y))))
		type)))


;;;Found in equantifiers
(defun skolem-step (sformnum ps &optional terms copy?)
  (let* ((*assert-typepreds* nil)
	 (*subtype-hash* (pvs-copyhash (subtype-hash ps)))
	 (*dp-state* (dp-state ps))
	 (*alists* (alists ps))
;;	 (alists (alists ps))
;;	 (findalist (dpinfo-findalist alists))
;;	 (usealist (dpinfo-usealist alists))
;;	 (sigalist (dpinfo-sigalist alists))
	 (new-context (copy *current-context*
			   'local-proof-decls
			   (copy (local-proof-decls *current-context*))))
	(terms (if (consp terms) terms (list terms)))
	(sformnum (find-sform (s-forms (current-goal ps)) sformnum
			      #'(lambda (sform)
				   (or (and (forall-expr? (formula sform))
					    (eql (length (bindings
							  (formula sform)))
						 (length terms)))
				       (and (not-expr? (formula sform))
					    (exists-expr?
					     (args1 (formula sform)))
					    (eql (length (bindings
							  (args1
							   (formula sform))))
						 (length terms))))))))
    (protecting-cong-state
     ((*dp-state* *dp-state*)
      (*alists* *alists*))
     (cond ((null sformnum)
	    (format-if "~%No suitable (+ve FORALL/-ve EXISTS) quantified expression found.")
	    (values 'X nil nil))
	   (t (multiple-value-bind (signal subgoal)
		  (sequent-reduce (current-goal ps)
				  #'(lambda (sform)
				      (skolem-step-sform ps sform
							 new-context
							 terms))
				  (list sformnum))
		(if (eq signal 'X)(values 'X nil nil)
		    (if (some #'(lambda (fmla)
				  (let* ((sign (not (not-expr? fmla)))
					 (body (if sign
						   fmla
						   (args1 fmla))))
				    (and (not (connective-occurs? body))
					 (let ((res (call-process
						     fmla
						     *dp-state* *alists*)))
					   (when (consp res)
					     (loop for x in res
						   do (push x *process-output*)))
					   (false-p res)))))
			      *assert-typepreds*)
			(values '! sformnum) ; SO - changed from sform
			(values signal (list
					(cons subgoal
					      (list 'context new-context
						    'subtype-hash *subtype-hash*
						    'dp-state *dp-state*
						    'alists *alists*))))))))))))


;;;Found in patch2-test
(defun find-quant-terms (sforms subst where if-match
				polarity?
				ps)
  (cond ((null sforms)
	 (format-if "~%Couldn't find a suitable quantified formula.")
	 NIL)
	(t
	 (let* ((*dp-state* (dp-state ps))
		(*alists* (alists ps)))
	   (nprotecting-cong-state
	    ((*dp-state* *dp-state*)
	     (*alists* *alists*))
	    (find-quant-terms* sforms subst where if-match polarity? ps))))))


;;;Found in patch2
(defun pseudo-normalize (expr)
  (if (or *pseudo-normalizing*		; Don't allow recursion
	  (not (fully-instantiated? expr)))
      expr
      (let* ((fvars (freevars expr))
	     (nexpr (unless fvars
		      (pvs-gethash expr *pseudo-normalize-hash*))))
	(when (eq *assert-flag* 'simplify) 'break)
	(if nexpr
	    (if (tc-eq nexpr expr)
		expr
		nexpr)
	    (let* ((*pseudo-normalizing* t)
		   (*generate-tccs* 'none)
		   ;;(typealist primtypealist);;NSH(2.16.94)
		   (*assert-flag* 'simplify)
		   (*process-output* nil)
		   (*assert-if-arith-hash*
		    (if *assert-if-arith-hash*;;NSH(11.30.95) 
			*assert-if-arith-hash*;;not real shadowing
			(make-hash-table :test #'eq))))
	      (nprotecting-cong-state
	       ((*dp-state* *init-dp-state*)
		(*alists* *init-alists*))
	       (let ((result (if *translate-id-counter*
				 (assert-if-simplify expr)
				 (let* ((*translate-id-hash*
					 (pvs-clrhash
					  *pseudo-normalize-translate-id-hash*))
					(*translate-id-counter* nil)
					(typealist typealist))
				   (newcounter *translate-id-counter*)
				   (assert-if-simplify expr)))))
		 (when (and nil (not (tc-eq result expr)))
		   (break "pseudo-norm changed expr"))
		 (unless fvars
		   (setf (pvs-gethash expr *pseudo-normalize-hash*)
			 result))
		 result
		 )))))))

;;Found in assert.lisp

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

(defun check-all-recognizers (arg)
  (let ((recs (recognizers (find-supertype (type arg)))))
    (loop for rec in recs
       collect
       (let ((constructor
	      (if (and (name-expr? arg)(constructor? arg))
		  arg
		  (if (and (application? arg)
			   (constructor? (operator arg)))
		      (operator arg)
		      nil))))
	 (if constructor
	     (if (tc-eq (recognizer constructor) rec)
		 (cons rec *TRUE*)
		 (cons rec *FALSE*))
	     (cons rec (assert-test (make-application rec arg))))))))

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

;;;Found in patch2.lisp

(defmethod lazy-assert-if-with-subst ((expr cases-expr) subst &optional if-flag)
  (with-slots (expression selections else-part)
      expr
    (let ((expression (substit expression subst)))
      (multiple-value-bind (sigexpr newexpr)
	(assert-if expression) ;;(10.8.96):was cond-assert-if
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
		     (if  if-flag ;;NSH(2.27.97)
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



;;(lf "types")
;;(lf "node-structures")
;;(lf "integer")
;;(lf "arithmetic")
;;(lf "shostak")
;;(lf "shostak-interp")
;;(lf "arith-solve")
;;(lf "foreign")
;;(lf "polyhedron")
;;(lf "translate-to-dc")

;;End of changes for the new decision procedures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Changes for translating from the prover so that shankar can call arithsimp.

(defun mk-equation (lhs rhs)
  (mk-application '= lhs rhs))

(defun mk-update-expr (expr assignments)
  (make-instance 'update-expr
    'expression expr
    'assignments assignments))

(defun mk-update-expr-1 (expr index value)
  (let ((assignment (mk-assignment 'uni `((,index)) value)))
    (mk-update-expr expr (list assignment))))

(defvar *reverse-prover-name* nil)
(defvar *pvs-typealist* nil)
(defvar *translate-from-prove-hash* (make-hash-table :test #'equal))

(defmethod translate-to-prove :around (obj)
  (let ((hashed-value (pvs-gethash obj *translate-to-prove-hash*)))
    (or hashed-value
	(let ((result (call-next-method)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (pvs-gethash obj *translate-to-prove-hash*) result))
	  (setf (gethash result *translate-from-prove-hash*) obj)
	  result))))

(defun add-to-reverse-prover-name (id expr)
  (unless (assoc id *reverse-prover-name*)
    (push (cons id (id expr)) *reverse-prover-name*)))

(defun add-to-pvs-typealist (id expr &optional (type (or (type expr)
							 (car (ptypes expr)))))
  (unless (assoc id *pvs-typealist*)
    (push (cons id type) *pvs-typealist*)))

(defun unique-prover-name (expr)
  ;(when (equal (id expr) '|stall_issue|) (break))
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	 (let* ((id-hash (pvs-gethash (normalize-name-expr-actuals expr)
				      *translate-id-hash*))
		(newconst (or id-hash
			      (when (true-p expr) 'TRUE)
			      (when (false-p expr) 'FALSE)
			      (list (intern (concatenate 'string
					      (string (id expr))
					      "_"
					      (princ-to-string
					       (funcall
						*translate-id-counter*))))))))
	   (when (consp newconst)
	     (add-to-reverse-prover-name (car newconst) expr)
	     (add-to-pvs-typealist (car newconst) expr))
	   (unless id-hash
	     (setf (pvs-gethash expr *translate-id-hash*)
		   newconst)
	     ;;(format t "~%adding ~a to typealist" (car newconst))
	     (when (consp newconst)
	       (add-to-typealist (car newconst) expr)
	       (add-to-reverse-prover-name (car newconst) expr)
	       (add-to-pvs-typealist (car newconst) expr)))
	   newconst))
	(t (add-to-local-typealist (id expr) expr)
	   (add-to-reverse-prover-name (id expr) expr)
	   (add-to-pvs-typealist (id expr) expr)
	   (id expr))))

(defun get-pvs-name (func)
  (or (cdr (assoc func *reverse-prover-name*))
      func))

(defun get-pvs-type (func)
  (cdr (assoc func *pvs-typealist*)))

(defun translate-from-prove-args (list)
  (mapcar #'translate-from-prove-expr list))

(defun translate-from-prove-var (var)
  (cond ((eq var 'true) *true*)
	((eq var 'false) *false*)
	((numberp var) (mk-number-expr var))
	(t (mk-name-expr (get-pvs-name var) nil nil nil 'variable))))

(defun translate-from-prove-if (if-expr)
  (mk-if-expr
   (translate-from-prove-expr (arg1 if-expr))
   (translate-from-prove-expr (arg2 if-expr))
   (translate-from-prove-expr (arg3 if-expr))))

(defun translate-from-prove-update (update)
  (let* ((array (translate-from-prove-expr (arg1 update)))
	 (array-type (translate-from-prove-type (arg1 update)))
	 (index (translate-from-prove-index (arg2 update)
					    array-type))
	 (value (translate-from-prove-expr (arg3 update))))
    (mk-update-expr-1 array index value)))

(defun translate-from-prove-type (expr)
  (cond
   ((and (consp expr) (eq (funsym expr) 'IF))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr) (eq (funsym expr) 'UPDATE))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr)
	 (memq (funsym expr)
	       '(AND NOT OR IMPLIES EQUAL LESSP LESSEQP GREATERP GREATEREQP)))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr) (null (argsof expr)))
    (translate-from-prove-type (car expr)))
   ((get-pvs-type expr))
   (t (translate-from-prove-type* expr))))

(defun translate-from-prove-type* (expr)
  (type (translate-from-prove expr)))

(defun translate-from-prove-index (index type)
  (cond
   ((numberp index)
    (let* ((fields (fields (find-supertype type)))
	   (sfields (sort-fields fields))) ;(break)
      (mk-name-expr (id (nth index sfields)))))
   (t (translate-from-prove-expr index))))

(defun translate-from-prove-func (func)
  (translate-from-prove-expr func))

(defun translate-from-prove-apply (apply)
  (let* ((func (arg1 apply))
	 (func-type (translate-from-prove-type func)))
    (cond
     ((typep func-type 'recordtype)
      (translate-from-prove-field-apply func (arg2 apply) func-type))
     ((arg2 apply) (mk-application* (translate-from-prove-func func)
		     (translate-from-prove-args (cdr (argsof apply)))))
     (t (translate-from-prove-expr func)))))

(defun translate-from-prove-field-apply (func
					 index &optional
					 (type (translate-from-prove-type
						func)))
  (let ((new-func (translate-from-prove-index index type)))
    (mk-application new-func
      (translate-from-prove-expr func))))

(defun translate-from-prove-infix (term)
  (mk-rec-application-left (infix-fun (funsym term)) 0
    (translate-from-prove-args (argsof term))))

(defun translate-from-prove-cons (expr)
  (let ((func (funsym expr)))
    (cond
     ((eq func 'update) (translate-from-prove-update expr))
     ((is-apply-n-x func) (translate-from-prove-apply expr))
     ((eq func 'if) (translate-from-prove-if expr))
     ((argsof expr) (translate-from-prove-infix expr))
     (t (translate-from-prove-constant expr)))))

(defun translate-from-prove-constant (expr)
  (translate-from-prove-expr (get-pvs-name (funsym expr))))

(defun translate-from-prove-expr (expr)
  (let ((hashed-value (gethash expr *translate-from-prove-hash*)))
    (or hashed-value
	(let ((result (translate-from-prove-expr* expr)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (gethash expr *translate-from-prove-hash*) result))
	  result))))

(defun translate-from-prove-expr* (expr)
  (cond
   ((consp expr) (translate-from-prove-cons expr))
   (t (translate-from-prove-var expr))))

(defun translate-from-prove (expr)
  (let* ((struct (translate-from-prove-expr expr))
	 (eqlty (mk-equation struct struct))
	 (typed-eqlty (typecheck eqlty :expected *boolean*)))
    (args1 typed-eqlty)))

(defun translate-from-prove-list (list)
  (if (listp list)
      (mapcar #'translate-from-prove list)
      (translate-from-prove list)))

(defvar *infix-trans-table*
  '((equal . =)
    (lessp . <)
    (lesseqp . <=)
    (greaterp . >)
    (greatereqp . >=)
    (plus . +)
    (difference . -)
    (times . *)
    (divide . /)
    (quo . /)))

(defun infix-fun (func)
  (or (cdr (assoc func *infix-trans-table*))
      func))

(defun ground-arithsimp (term)
  (cond
   ((symbolp term) term)
   ((integerp term) term)
   ((interp term)
    (let ((newterm
	   (sigma (cons (funsym term)
			(loop for arg in (argsof term)
			      collect (ground-arithsimp arg) )))))
      newterm ))
   (T term) ))

(defun arithsimp (expr)
  (let ((ground-expr (top-translate-to-prove expr)))
    (translate-from-prove (ground-arithsimp ground-expr))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Changes of the form (eq res 'true) to (true-p res):

(defun true-p (expr)
  (or (eq expr 'true)
      (tc-eq expr *true*)))

(defun false-p (expr)
  (or (eq expr 'false)
      (tc-eq expr *false*)))


;;;From checker-macros:
(defmacro make-assert-expr (expr)
  `(let* ((*tccforms* nil)
	  (*keep-unbound* *bound-variables*)
	  (*generate-tccs* 'ALL!)
	  (expr ,expr)
	  (test (loop for tccinfo in *tccforms*
		      always
		      (true-p (assert-test (tccinfo-formula tccinfo))))))
    (when test expr )))

;;;From match:
(defmethod match* ((lhs application)(instance expr) bind-alist subst)
    (cond ;((eq subst 'fail) 'fail)
        ((higher-order-pattern? lhs bind-alist)
	 (match-ho-pattern lhs instance bind-alist subst))
	((and (tc-eq (find-supertype (type lhs)) *number*)
	      (tc-eq (find-supertype (type instance)) *number*))
	 (if (is-addition? lhs)
	     (if (number-expr? (args1 lhs))
		 (let ((diff
		       (make-assert-expr
			(make-difference instance (args1 lhs)
					 (type (args2 lhs))))))
		   (if diff
		       (match* (args2 lhs) diff
			      bind-alist subst)
		       'fail))
		 (if (number-expr? (args2 lhs))
		     (let ((diff (make-assert-expr
				  (make-difference instance (args2 lhs)
						   (type (args1 lhs))))))
		       (if diff (match* (args1 lhs) diff
				       bind-alist subst)
			   'fail))
		     'fail))
	     (if (is-subtraction? lhs)
		 (if (number-expr? (args2 lhs))
		     (let ((sum (make-assert-expr
				 (make-sum (list instance (args2 lhs))
					   (type (args1 lhs))))))
		       (if sum (match* (args1 lhs) sum
				      bind-alist subst)
			   'fail))
		     'fail)
		 'fail)))
	(t 'fail)))

;;;From patch2-test:

(defmethod match* :around
  ((expr expr)(instance expr) bind-alist subst)
  (if (eq subst 'fail) 'fail
      (if (and (null (freevars expr))(tc-eq expr instance))
	  subst
      (let ((ans
;	     (if (no-freevars? expr)
;		     (let ((newmodsubst
;			    (if (eq *modsubst* T)
;				(or (tc-eq expr instance)
;				    (when (and (typep expr 'name)
;					       (typep instance 'name))
;				      (arith-match expr instance)))
;				(tc-unify instance
;					  expr
;					  *modsubst*))))
;		       (cond (newmodsubst
;			      (setq *modsubst* newmodsubst)
;			      subst)
;			     (t 'fail))))
		     (call-next-method)))
	(if (and (eq ans 'fail);;(null bind-alist)
		 (not (bind-decl? expr));;NSH(12.1.94)
		 (type expr) (type instance)
		 ;;NSH(9.19.97)fixes Wilding's rewriting inefficiency
		 ;;by avoiding assert-test0 on numbers
		 (not (and (number-expr? expr)
			   (number-expr? instance))) 
		 (tc-eq (find-supertype (type expr)) *number*)
		 (tc-eq (find-supertype (type instance)) *number*))
	    (let* (
		   ;;NSH(9.19.94) not needed anymore.
;		   (*bound-variables*
;		    (nconc (loop for (x . y) in bind-alist
;				 collect y)
;			   *bound-variables*))  ;;;nconc (list x y)))
		   (*keep-unbound* *bound-variables*)
		   (subst-bind-alist
		    (loop for x in bind-alist
			  collect
			  (if (consp (cdr x))
			      (cons (car x)
				    (make-tuple-expr
				     (mapcar #'(lambda (z)
						 (let ((z1
							(change-class
							 (copy z) 'name-expr)))
						   (setf (kind z1) 'VARIABLE)
						   z1)) ;;NSH(8.28.96)
					     (cdr x))))
			      x)))
		   (substituted-expr
		    (substit expr (append subst-bind-alist subst)))
		   (lhs (if (eq expr substituted-expr)
			    expr
			    (beta-reduce substituted-expr)))
		   (equality (make-equality lhs instance))
;;NSH(11.18.96): switching back to using assert-test0 over
;;tc-eq-norm-addition. 
		   (result
		    (if (subsetp (freevars lhs) *bound-variables* :test
				 #'tc-eq)
			(assert-test0;;pseudo-normalize would do
			 ;;too much work.  
			 equality)
			'fail))
; 		   (result
;		    (and (subsetp (freevars lhs) *bound-variables* :test
;				 #'tc-eq)
;			 (tc-eq (norm-addition lhs)(norm-addition instance))))
		   ) ;;(when (not (eq (eq result 'TRUE) result2))(break "match :around"))
	      (if  (true-p result)  subst ;;NSH(4.10.97) was 'fail
		  (multiple-value-bind
		      (sig lhs-terms rhs-terms)
		      (light-cancel-terms (addends lhs)
					  (addends instance))
		    (if (and (null lhs-terms)(null rhs-terms))
			subst
			(if (eq sig '?)
			    (match* (make-sum lhs-terms
					     (compatible-type (type lhs)
							      *integer*))
				   (make-sum rhs-terms
					     (compatible-type (type instance)
							      *integer*))
				   bind-alist
				   subst)
		    'fail)))
		  ))
	    ans)))))

;;;From patch2:
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

(defun cancel-terms* (lhs* rhs* lhs-accum sig)
  (cond ((null lhs*)(values sig (nreverse lhs-accum)
			    rhs*))
	(t (let ((rhs-match
		  (find (car lhs*) rhs*
			:test #'(lambda (x y)
				  (true-p (assert-test (make-equality x y))))
			)))
	     (if rhs-match
		 (cancel-terms* (cdr lhs*)
				  (remove rhs-match rhs*
					  :count 1);;NSH(2.21.97) 
				  lhs-accum '?)
		 (cancel-terms* (cdr lhs*)
				  rhs*
				  (cons (car lhs*) lhs-accum)
				  sig))))))


;;;From utils:

(defun translate-cases-to-if* (expr selections else-part &optional chained?)
  (cond ((and (null (cdr selections))
	      (null else-part))
	 (subst-accessors-in-selection expr (car selections)))
	((null selections)
	 else-part)
	(t (let* ((sel (car selections))
		  (thinst (module-instance (find-supertype (type expr))))
		  (cons-or-rec
		   (if (args sel)
		       (subst-mod-params (recognizer (constructor sel))
					 thinst)
		       (subst-mod-params (constructor sel)
					 thinst)))
		  (cond (if (args sel)
			    (make-application cons-or-rec expr)
			    (make-equation expr cons-or-rec)))
		  (then ;(subst-mod-params
			 (subst-accessors-in-selection expr sel)
			 ;thinst)
		    )
		  (else (translate-cases-to-if* expr (cdr selections)
						else-part t)))
	     (if chained?
		 (make-chained-if-expr cond then else)
		 (make-if-expr cond then else))))))


;;; Makes for <= ...

(defun mk-greatereq (a1 a2)
  (mk-application (mk-greatereqop) a1 a2))

(defun make-greatereq (x y)
  (typecheck (mk-greatereq x y) :expected *boolean* :tccs 'top))

(let ((greatereqop nil))
  (defun mk-greatereqop ()
    (if greatereqop
	(copy greatereqop)
	(let ((>=op (mk-name-expr '>= nil '|reals|)))
	  (typecheck >=op)
	  (setf (mod-id >=op) nil)
	  (setq greatereqop >=op)
	  (copy >=op))))
  (defun reset-greatereqop ()
    (setq greatereqop nil)))

(defun mk-greater (a1 a2)
  (mk-application (mk-greaterop) a1 a2))

(defun make-greater (x y)
  (typecheck (mk-greater x y) :expected *boolean* :tccs 'top))

(let ((greaterop nil))
  (defun mk-greaterop ()
    (if greaterop
	(copy greaterop)
	(let ((>op (mk-name-expr '> nil '|reals|)))
	  (typecheck >op)
	  (setf (mod-id >op) nil)
	  (setq greaterop >op)
	  (copy >op))))
  (defun reset-greaterop ()
    (setq greaterop nil)))

(defun mk-lesseq (a1 a2)
  (mk-application (mk-lesseqop) a1 a2))

(defun make-lesseq (x y)
  (typecheck (mk-lesseq x y) :expected *boolean* :tccs 'top))

(let ((lesseqop nil))
  (defun mk-lesseqop ()
    (if lesseqop
	(copy lesseqop)
	(let ((<=op (mk-name-expr '<= nil '|reals|)))
	  (typecheck <=op)
	  (setf (mod-id <=op) nil)
	  (setq lesseqop <=op)
	  (copy <=op))))
  (defun reset-lesseqop ()
    (setq lesseqop nil)))

(defun mk-less (a1 a2)
  (mk-application (mk-lessop) a1 a2))

(defun make-less (x y)
  (typecheck (mk-lessop x y) :expected *boolean* :tccs 'top))

(let ((lessop nil))
  (defun mk-lessop ()
    (if lessop
	(copy lessop)
	(let ((<op (mk-name-expr '< nil '|reals|)))
	  (typecheck <p)
	  (setf (mod-id <op) nil)
	  (setq lessop <op)
	  (copy <op))))
  (defun reset-lessop ()
    (setq lessop nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Loading the new decision procedures.
(load "/project/pvs/pvs2.1/src/prover/dc-prototypes/freeze/dp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NSH(7.15.97): Changes to prop to use an fnum argument

(defun find-sform* (sforms sformnum pred pos neg)
  (cond ((null sforms) nil)
	((not-expr? (formula (car sforms)))
	 (if (and (in-sformnums? (car sforms) pos neg sformnum)
		  ;;was (or (memq sformnum '(* -))
		            ;(equal sformnum neg)
		  (funcall pred  (car sforms)))
	     neg
	     (find-sform* (cdr sforms) sformnum pred pos (1- neg))))
	(t (if (and (in-sformnums? (car sforms) pos neg sformnum)
		    ;NSH(7.15.98) was (or (memq sformnum '(* +))
			;    (equal sformnum pos)
		  (funcall pred (car sforms)))
	     pos
	     (find-sform* (cdr sforms) sformnum pred (1+ pos) neg)))))


(defun check-yes-no-sforms (sf yes-sforms no-sforms)
  (and (or (null yes-sforms)
	   (memq sf yes-sforms))
       (or (null no-sforms)
	   (not (memq sf no-sforms)))))

(defun fnums-sforms (sforms yes-sforms no-sforms)
  (gather-fnums sforms '* nil
		#'(lambda (sf) (check-yes-no-sforms sf yes-sforms no-sforms))))
				 

(defstep prop (&optional (fnums *))
  (let ((sforms (gather-seq (s-forms (current-goal *ps*))
			    '* fnums)))
    (prop-helper$ sforms))
  "A black-box rule for propositional simplification.  When this
is applied, there should be no top-level conjunctions, disjunctions,
or conditionals, left in the indicated formulas."
 "Applying propositional simplification")

(defhelper prop-helper (sforms)
  (let ((fnums (fnums-sforms (s-forms (current-goal *ps*))
			     nil sforms)))
    (try (flatten :fnums fnums)
	 (prop-helper$ sforms)
	 (try (split fnums)(prop-helper$ sforms) (skip))))
  "The main recursion in the PROP strategy which is a
black-box rule for propositional simplification."
  "Applying propositional simplification")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;End of prop with fnum changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
