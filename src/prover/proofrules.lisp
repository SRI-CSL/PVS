;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proofrules.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Oct 31 02:45:32 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:59:09 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In proofrules-1-91.lisp, the rules are listed out without any
;; particular organization.  These rules include the propositional and
;; quantifier rules, and a rule to invoke the decision procedure.  A
;; better organized approach would be partition the rules into those
;; that split the goal sequent into subgoal sequents, those that make
;; local replacements by transforming a sequent formula into one or more
;; disjuncts, and the others, mainly those that return modifications or
;; signals.   

(in-package 'pvs)


(defun sform-search (sformlist pred pos neg)
  (loop for sform in sformlist
	thereis
	(if (funcall pred sform pos neg)
	    (if (not-expr? (formula sform)) neg pos)
	    nil)))
	

;;simplifier:[sform -> sform/list of sforms]
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
;	    ((member *true* result :key #'formula
;		     :test #'tc-eq)
;	     '!)
;	    ((member (make-negation *false*) result :key #'formula
;		     :test #'tc-eq)
;	     '!)
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

(defun sequent-reduce (sequent simplifier sformnums)
  (multiple-value-bind
   (signal result)
   (sform-reduce (s-forms sequent) simplifier sformnums 1 -1)
   (cond ((eq signal '!) (values '! nil))
	 ((eq signal 'X) (values 'X sequent))
	 ((eq signal '?)(values '?  (lcopy sequent
					  's-forms
					  result))))))
			   


(defmethod simplify ((goalsequent sequent)  simplifier
		     &key repeat sformnums)
  (let* ((selected-s-forms (select-seq (s-forms goalsequent) sformnums))
	 (remaining-s-forms (delete-seq (s-forms goalsequent) sformnums))
	 (simplified-s-forms
	  (loop for sform in selected-s-forms
		nconc (simplify sform simplifier :repeat repeat))))
    (if (loop for sform1 in simplified-s-forms
	      as sform2 in selected-s-forms
	      always (s-form-equal? sform1 sform2))
	(values 'X nil)
      (values '? (list (lcopy goalsequent
			     's-forms
			     (nconc simplified-s-forms
				    remaining-s-forms)))))))

(defun s-form-equal? (sform1 sform2)
 (exequal (formula sform1)(formula sform2)))
       ;;(equal (variables sform1)(variables sform2))

;;;How to deal with signals????  
;;;In the above, the result of the simplification is compared with the
;;;output to determine if the simplification was successful.  The
;;;methods below do not need to deal with  signals at all.

(defmethod simplify ((sform s-formula) simplifier &key repeat)
  (mapcar #'(lambda (x)(lcopy sform
			     'formula x))
	  (simplify (formula sform) simplifier :repeat repeat)))

(defmethod simplify ((formula expr) simplifier &key repeat)
  (let ((first-simplification (funcall simplifier formula)))
    (if (or (not repeat)  ;;no repeats or simplification unsuccessful
	    (and (= (length first-simplification) 1)
		 (exequal (car first-simplification) formula)))
	first-simplification
      (loop for form in first-simplification
	    nconc (simplify form simplifier :repeat repeat)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;simplify-disjunct takes a formula and returns a list of formulas, the
;;disjunction of which is equivalent to the original formula.  
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
 
(defun simplify-disjunct-sform (sform depth)
  (let ((new-sforms
	 (loop for x in (simplify-disjunct (formula sform) depth)
	       collect (lcopy sform 'formula x))))
    (if (and (= (length new-sforms) 1)
	     (s-form-equal? sform (car new-sforms)))
	(values 'X sform)
      (values '? new-sforms))))

;(defun dsimp-rule (sformnums) (make-instance 'rule
;		      'rule-part (dsimp sformnums)
;		      'rule-input `(dsimp ,sformnums)))





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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Lifting IF occurrences to the top of a sequent formula
;;;;;;;;;;

;(defun lift-if-rule (sformnums)
;  (make-instance 'rule
;		 'rule-part (liftif-step sformnums)
;		 'rule-input `(lift-if ,sformnums)))



(defun liftif-step (sformnums updates?)
  #'(lambda (ps)
      (let ((*lift-if-updates* updates?)
	    (sformnums (if (null sformnums) '*
			   (if (consp sformnums) ;;NSH(5.8.95)
			       (loop for sn in sformnums;;nconc->append
				     append (if (consp sn) sn (list sn)))
			       (list sformnums)))))
      (multiple-value-bind (signal subgoal)
	  (sequent-reduce
	    (current-goal ps)
	    #'(lambda (sform)
		 (lift-if ps sform))
	    sformnums)
;;	(when (eq signal '?)
;;	  (format-if "~%Lifting the IF-conditions in ~a" sformnums))
	(values signal (list subgoal) )))))

(defun lift-if-expr (expr) ;;NSH(4.5.95) utility for if-lifting
                          ;; on arbitrary expressions
  (let* ((conds (collect-conds expr))
	 (*top-simplify-ifs-hash*
	  (make-hash-table :test #'eq))
	 (if-expr (make-top-if-expr expr conds)))
    (typecheck if-expr :expected (type expr))))
;;NSH:typecheck needed since mk-if-expr used below.



(defun lift-if (ps sform)
  (let* ((formula (formula sform))
	 (conds (top-collect-conds formula))
	 (body (if (not-expr? formula) (args1 formula) formula))
	 (*top-simplify-ifs-hash*
	  (make-hash-table :test #'eq));;faster than pvs-hash.(NSH:10.19.94)
;	  (make-pvs-hash-table :hashfn #'pvs-sxhash
;			       :test #'tc-eq)
	 (if-expr (make-top-if-expr body conds )))
    (if (null conds)
	(if (typep body 'cases-expr)
	    (values '? (lcopy sform 'formula
			      (let ((newbody
				     (translate-cases-to-if body)))
				(if (not-expr? formula)
				    (negate newbody)
				    newbody))))
	    (values 'X sform))
	(values '?
		(lcopy sform
		  'formula
		  (typecheck (if (not-expr? formula)
				 (negate if-expr)
				 if-expr)
		    :expected *boolean* :context
		    *current-context*))))))

(defun truecond? (cond trueconds falseconds)   ;;NSH(9.27.95)
  (or (equal cond *true*)
      (member cond trueconds :test #'tc-eq)
      (and (equality? cond)
	   (tc-eq (args1 cond)(args2 cond)))
      (and (not-expr? cond)
	   (member (args1 cond) falseconds :test #'tc-eq))))

(defun falsecond? (cond trueconds falseconds)  ;;NSH(9.27.95)
  (or (equal cond *false*)
      (member cond falseconds :test #'tc-eq)
      (and (not-expr? cond)
	   (or (member (args1 cond) trueconds :test #'tc-eq)
	       (and (equality? (args1 cond))
		    (tc-eq (args1 (args1 cond))
			   (args2 (args1 cond))))))))

(defun truefalsecond-reduce (cond trueconds falseconds)
  (if (truecond? cond trueconds falseconds)
      *true*
      (if (falsecond? cond trueconds falseconds)
	  *false*
	  cond)))

  
(defun make-top-if-expr (expr conds &optional trueconds falseconds)
  (if (null conds)
;      (let ((*local-simplify-ifs-hash*  ;;NSH(9.28.95) removing local hash
;              ;;since it is unlikely to be as useful as a top-level hash.  
;	     (make-hash-table :test #'eq));;faster than pvs-hash
;;	     (make-pvs-hash-table :hashfn #'pvs-sxhash
;;				  :test #'tc-eq)
;	    )
      (simplify-ifs expr trueconds falseconds)
      (let ((cond (car conds)))
	(if (truecond? cond trueconds falseconds)
	  (make-top-if-expr expr  (cadr conds) trueconds falseconds)
	  (if (falsecond? cond trueconds falseconds)
	      (make-top-if-expr expr (caddr conds) trueconds falseconds)
	      (let* ((newthen
		      (make-top-if-expr expr (cadr conds)
					(cons (car conds) trueconds)
					falseconds))
		     (newthen (truefalsecond-reduce newthen trueconds falseconds))
		    (newelse
		     (make-top-if-expr expr (caddr conds) trueconds
					    (cons (car conds) falseconds)))
		    (newelse (truefalsecond-reduce newelse trueconds falseconds)))
		(if (tc-eq newthen newelse)
		    newthen
		    (if (and (eq newthen *true*)
			     (eq newelse *false*))
			(car conds)
			(mk-if-expr (car conds)
				    newthen
				    newelse
				    )))))))))


(defmethod simplify-ifs :around ((expr expr) trueconds falseconds)
	   (let ((top-hash-value (gethash
				  expr
				  *top-simplify-ifs-hash*)))
	     ;;(when top-hash-value (break "top"))
	     (if (and top-hash-value
		      (loop for lst in top-hash-value
			    thereis (and (subsetp trueconds lst); :test #'tc-eq)
					 (subsetp falseconds lst)))); :test #'tc-eq))))
			expr
		 (let ((result (call-next-method)))
		   (cond ((eq result expr)
			  (push (append trueconds falseconds)
				(gethash expr
					 *top-simplify-ifs-hash*))
			  result)
			 (t result))))))
		 

;;; SO 8/17/94 - use new form of application
(defmethod simplify-ifs ((expr if-expr) trueconds falseconds)
  (let ((simple-condition (simplify-ifs (condition expr) trueconds falseconds)))
    ;;(break "simplify if")
    (if (truecond? simple-condition trueconds falseconds)
	(simplify-ifs (then-part expr) trueconds falseconds)
	(if (falsecond? simple-condition trueconds falseconds)
	    (simplify-ifs (else-part expr) trueconds falseconds)
	    (let* ((new-then  ;;NSH(9.27.95) fixed leaky hash
		   (simplify-ifs (then-part expr)
				 (cons simple-condition trueconds)
				 falseconds))
		   (new-then (truefalsecond-reduce new-then trueconds falseconds))
		   (new-else (simplify-ifs (else-part expr)
					   trueconds
					   (cons simple-condition falseconds)))
		   (new-else (truefalsecond-reduce new-else trueconds falseconds)))
	      (if (tc-eq new-then new-else);;NSH(9.27.95) equality test
		  new-then
		  (if (and (eq new-then *true*)
			   (eq new-else *false*))
		      simple-condition
		      (if (and (eq simple-condition (condition expr))
			       (eq new-then (then-part expr))
			       (eq new-else (else-part expr)))
			  expr;;NSH(8.2.95) was destroying eq-ness without
			  ;; this case.
			  (lcopy expr
			    `argument
			    (make-arg-tuple-expr
			     (list simple-condition
				   new-then
				   new-else)))))))))))

(defmethod simplify-ifs ((expr binding-expr) trueconds falseconds)
  (lcopy expr
	'expression (simplify-ifs (expression expr) trueconds falseconds)))

;;; SO 9/5/94 - Added projection-application and field-application methods
(defmethod simplify-ifs ((expr projection-application) trueconds falseconds)
  (lcopy expr
    'argument (simplify-ifs (argument expr) trueconds falseconds)))

(defmethod simplify-ifs ((expr field-application) trueconds falseconds)
  (lcopy expr
    'argument (simplify-ifs (argument expr) trueconds falseconds)))

(defmethod simplify-ifs ((expr application) trueconds falseconds)
  (if (and *lift-if-updates* (update-expr? (operator* expr)))
      (let ((translation (translate-update-to-if expr)))
	(if (and (if-expr? translation)
		 (or (truecond? (condition translation)
				trueconds falseconds)
		     (falsecond? (condition translation)
				trueconds falseconds)))
	    (simplify-ifs translation trueconds falseconds)
	    (lcopy expr
	      'operator (simplify-ifs (operator expr)
				      trueconds falseconds)
	      'argument (simplify-ifs (argument expr) trueconds
				 falseconds))))
      (lcopy expr
	'operator (simplify-ifs (operator expr) trueconds falseconds)
	'argument (simplify-ifs (argument expr) trueconds
				falseconds))))

(defmethod simplify-ifs ((expr record-expr) trueconds falseconds)
  (lcopy expr
	'assignments (simplify-ifs (assignments expr)
				   trueconds falseconds)))

(defmethod simplify-ifs ((expr tuple-expr) trueconds falseconds)
  (lcopy expr
	'exprs (simplify-ifs (exprs expr) trueconds falseconds)))

(defmethod simplify-ifs ((expr update-expr) trueconds falseconds)
  (lcopy expr
	'expression (simplify-ifs (expression expr)
				  trueconds falseconds)
	'assignments (simplify-ifs (assignments expr)
				   trueconds falseconds)))

(defmethod simplify-ifs ((expr assignment) trueconds falseconds)
  (lcopy expr
	'expression (simplify-ifs (expression expr)
				  trueconds falseconds)))

(defmethod simplify-ifs ((expr cases-expr) trueconds falseconds)
  (let* ((expression (simplify-ifs (expression expr) trueconds falseconds))
	 (recs (recognizers (find-supertype (type expression))))
	 (selections
	  (loop for sel in (selections expr)
		when (not (member (make-application
				      (recognizer (constructor sel))
				    expression)
				  falseconds :test #'tc-eq))
		collect sel))
	 (true-selection
	  (loop for sel in selections
		when (member (make-application (recognizer (constructor sel))
			       expression)
			     trueconds :test #'tc-eq)
		return (subst-accessors-in-selection expression
						     sel))))
    (if (and (null selections)(else-part expr));;NSH(4.24.97)
	;;if all selections are false, there better be an else-part.
	(simplify-ifs (else-part expr) trueconds falseconds)
	(if (and (singleton? selections) (null (else-part expr)))
	    ;;NSH(4.24.97) PYGloess noticed lift-if unsound
	    ;;if we just check singleton?.  
	    (simplify-ifs  (subst-accessors-in-selection
			    expression (car selections))
			   trueconds falseconds)
	(if true-selection
	    (simplify-ifs true-selection trueconds falseconds)
	    (lcopy expr
	      'expression expression
	      'selections (simplify-ifs (selections expr) trueconds falseconds)
	      'else-part (simplify-ifs (else-part expr) trueconds falseconds)))))))

(defmethod simplify-ifs ((expr selection) trueconds falseconds)
  (lcopy expr
	'expression (simplify-ifs (expression expr) trueconds falseconds)))
    

(defmethod simplify-ifs ((list list) trueconds falseconds)
  (let ((result
	 (loop for elem in list
	       collect (simplify-ifs elem trueconds falseconds))))
    (if (every #'eq list result)
	list
	result)))

(defmethod simplify-ifs ((expr name-expr) trueconds falseconds)
  (declare (ignore trueconds falseconds))
  expr)

(defmethod simplify-ifs ((expr number-expr) trueconds falseconds)
  (declare (ignore trueconds falseconds))
  expr
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun branch?  (expr)
  (and (typep expr 'if-expr)
  (let ((op (operator expr)))
    (and (typep op 'name-expr)
	 (eq (id op) 'if)
	 (eq (id (module-instance (resolution op))) '|if_def|)))))

(defmethod top-collect-conds ((expr if-expr))
  (let ((if-conds (collect-conds (condition expr))))
    (if (null if-conds)
	(let ((then-conds (top-collect-conds (then-part expr))))
	  (if (null then-conds)
	      (top-collect-conds (else-part expr))
	      then-conds))
	if-conds)))

(defmethod top-collect-conds ((expr cases-expr))
  (top-collect-conds (translate-cases-to-if expr)))

(defmethod top-collect-conds ((expr application))
  (let ((nexpr (if *lift-if-updates*
		   (translate-update-to-if expr)
		   expr)))
    (if (eq nexpr expr)
	(call-next-method)
	(collect-conds nexpr))))

(defmethod top-collect-conds ((expr expr))
  (if (not-expr? expr)
      (top-collect-conds (args1 expr))
      (collect-conds expr)))

(defmethod collect-conds ((expr if-expr)  &optional  boundvars)
  (cond ((branch? expr)
	 (let ((condn (condition expr)))
	   (if (null (intersection (freevars condn) boundvars
				   :test #'same-declaration))
	       ;;no bound variables in the conditional
	       (let ((conds
		      (collect-conds (condition expr) boundvars)))
		 (if (null conds)
		     (list condn (collect-conds (then-part expr) boundvars)
			   (collect-conds (else-part expr) boundvars))
		     conds))
	       nil)))
	(t 
	(collect-conds (arguments expr)
		       boundvars))))

;	(collect-conds
;	     (else-part expr)
;	     (collect-conds (then-part expr)
;				  (pushnew (condition expr)
;					   conds
;					   :test
;					   #'exequal)))
;      (collect-conds
;	     (else-part expr)
;	     (collect-conds (then-part expr)
;				  (collect-conds cond
;							conds boundvars)
;				  boundvars)
;	     boundvars))))

(defmethod collect-conds ((expr binding-expr) &optional  boundvars)
  (collect-conds (expression expr) 
			(append (bindings expr) boundvars)))

;;; SO 9/5/94 - Added methods for projection-application and field-application
(defmethod collect-conds ((expr projection-application) &optional boundvars)
  (collect-conds (argument expr) boundvars))

(defmethod collect-conds ((expr field-application) &optional boundvars)
  (collect-conds (argument expr) boundvars))

(defmethod collect-conds ((expr application)  &optional  boundvars)
  (if (and *lift-if-updates*
	   (update-expr? (operator* expr))
	   (null (intersection (freevars (update-application* expr))
			       boundvars
			       :test #'same-declaration)))
      (let ((nexpr (translate-update-to-if expr)))
	(if (eq nexpr expr)
	    (let ((opconds (collect-conds (operator expr) boundvars)))
	      (if (null opconds)
		  (collect-conds (arguments expr)
				 boundvars)
		  opconds))
	    (collect-conds nexpr)))
      (let ((opconds (collect-conds (operator expr) boundvars)))
	(if (null opconds)
	    (collect-conds (arguments expr)
			   boundvars)
	    opconds))))

(defun update-application* (expr)
  (if (typep (operator expr) 'update-expr)
      expr
      (update-application* (operator expr))))

(defmethod collect-conds ((expr record-expr) &optional boundvars)
  (collect-conds (assignments expr) boundvars))

(defmethod collect-conds ((expr tuple-expr) &optional boundvars)
  (collect-conds (exprs expr) boundvars))

(defmethod collect-conds ((expr update-expr) &optional boundvars)
  (let ((expr-conds (collect-conds (expression expr) boundvars)))
    (if (null expr-conds)
	(collect-conds (assignments expr) boundvars)
	expr-conds)))

(defmethod collect-conds ((expr assignment) &optional boundvars)
  (collect-conds (expression expr) boundvars))

(defmethod collect-conds ((expr cases-expr) &optional boundvars)
  ;;CRW(8/5/94) changed tc-eq to same-declaration
  (if (null (intersection (freevars (expression expr)) boundvars
			  :test #'same-declaration))
      (collect-conds (translate-cases-to-if expr) boundvars) ;;added boundvars
      nil))

;(let ((expr-conds (collect-conds (expression expr) boundvars)))
;    (if (null expr-conds)
;	(let ((sel-conds (collect-conds (selections expr) boundvars)))
;	  (if (null sel-conds)
;	      (collect-conds (else-part expr) boundvars)
;	      sel-conds))
;	expr-conds))

(defmethod collect-conds ((expr selection) &optional boundvars)
  (collect-conds (expression expr)
		 (append (args expr) boundvars)))

(defmethod collect-conds ((list list)  &optional  boundvars)
  (if (null list)
      nil
      (let ((carconds (collect-conds (car list) boundvars)))
	(if (null carconds)
	    (collect-conds (cdr list) boundvars)
	    carconds))))
			  
			  

(defmethod collect-conds ((expr name-expr)  &optional  boundvars)
  (declare (ignore boundvars))
  nil)

(defmethod collect-conds ((expr number-expr)  &optional  boundvars)
  (declare (ignore boundvars))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(2-20) So far we have written proof rules for the propositional
;;connectives, quantifiers, the IF-connective, the decision procedures
;;for equality and inequality.  The main thing left is to deal with the
;;use of lemmas.  Right now, I have three proof rules in mind: one, 
;;to introduce the universal closure of the lemma as a premise; two, to
;;apply a simple equality as a rewrite rule; and three, to use a
;;conditional equality as a conditional rewrite rule.  

;(defun lemma-rule (name substs)
;  (make-instance 'rule
;		 'rule-part (lemma-rule-fun name substs)
;		 'rule-input `(lemma ,name ,substs)))


  
(defun lemma-rule-fun (name &optional substs)
  #'(lambda (ps)(lemma-step name substs ps)))

;;converts a flat substitution into an alist, while
;;typechecking the bindings.  Sam asks to generate a warning
;;when an irrelevant variable is given.(NSH:6/6/91)
(defun make-bindalist (vars simple-alist ps)
  (if (null vars)
      NIL
    (let* ((var (car vars))
	   (pair (assoc (id var) simple-alist
			:test #'format-equal)))
      (if (null pair)
	  (make-bindalist (cdr vars) simple-alist ps)
	(cons (cons var (cdr pair))
	      (make-bindalist (cdr vars) simple-alist ps))))))

(defun subst-ids (substs)
  (cond ((null substs) nil)
	((null (cdr substs)) nil)
	(t (cons (car substs)(cddr substs)))))

(defmethod substitutable-vars ((expr forall-expr))
  (append (bindings expr)
	  (substitutable-vars (expression expr))))

(defmethod substitutable-vars ((expr expr))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tc-unify-over is used by check-with-subst* to check if there is a
;matching substitution for the module formals corresponding to any
;combination of types for the substitutions.  The dummy substitution is
;given in match, and tc-unify is used to find a successful substitution.
;Note that set-type is used to set the type of the rhs-expr to the
;chosen type.

(defun tc-unify-over (lhs-types  rhs-exprs rhs-typelists
				 match)
  (cond ((null lhs-types) (if match
			      (if (every #'cdr match)
				  match
				  nil)
			      T))
	((null (car rhs-typelists)) nil)
	(t (let* (
		  (rhs1 (caar rhs-typelists))
		  (first-match
		   (if match
		       (tc-unify rhs1 (car lhs-types) match)
		       (if (compatible? (car lhs-types) rhs1) T NIL))))
	     (if (null first-match)
		 (tc-unify-over lhs-types
				rhs-exprs
				(cons (cdar rhs-typelists)
				      (cdr rhs-typelists))
				match)
		 (let ((rest-first-match
			(tc-unify-over (cdr lhs-types)
				       (cdr rhs-exprs)
				       (cdr rhs-typelists)
				       (if (eq first-match T)
					   nil
					   first-match))))
		   (cond ((null rest-first-match)
			  (tc-unify-over lhs-types
					 rhs-exprs
					 (cons (cdar rhs-typelists)
					       (cdr rhs-typelists))
					 match))
			 (t
			  (set-type (car rhs-exprs) rhs1)
			  rest-first-match))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check-with-subst(resolutions, subalist, context) tries
;check-with-subst* on each resolution in resolutions.  If the resolution
;is not in the current-theory, then we pass on the actuals and formals
;of the resolution.  The list of formulas corresponding to the
;resolution is also passed down - a single function name can generate
;many formulas due to the differing degrees of lambda-removal.  The
;subalist is used to determine which form of the definition is
;appropriate.   The (nreverse (create-formulas ..)) is significant since
;this is where we make sure that the least lambda-reduced form of the
;definition is used, i.e., if we want more lambdas removed, these
;variables have to be listed in the substitution.

(defun check-with-subst (resolutions subalist context)
  (if (null resolutions) nil
      (let* ((resolution (car resolutions))
	     (mod-inst (module-instance resolution))
	     (current-mod? (eq (get-theory mod-inst) *current-theory*))
	     (res-params (external-free-params resolution))
	     (forms (reverse (create-formulas resolution context)))
	     (form (check-with-subst* forms subalist
				      mod-inst res-params))
	     (rest (check-with-subst (cdr resolutions) subalist context)))
	;; (break)
	(if form
	    (cons (cons form resolution) rest)
	    rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check-with-subst* checks if every substituted variable in subalist is a
;substitutable variable in the first form.  If not, it tries the
;remaining forms.  If so, it checks if there are uninstantiated formals.
;If not, it typechecks the substitutions (with expected type from
;substitutable-vars) and returns T for formal-inst.  Otherwise, it
;typechecks the substitutions with no expected type,  and tries to
;determine if the formals of the resolution can be determined from any
;combination of the resulting types for the substitutions using
;tc-unify-over.  If there is no suitable formal-inst, it once again
;recurses on the remaining forms, otherwise, it applies subst-mod-params
;to (car form) which is the result.
;		(cond ((or (null formals) actuals)
;		       (let ((result
;			      (loop for (x . y) in subalist
;			     always
;			     (multiple-value-bind
;				   (supertype preds)
;				 (compatible? (type (find x (substitutable-vars
;							  (car forms))
;						       :test #'same-id))
;			     (typecheck y
;					   :expected
;					   (type (find x (substitutable-vars
;							  (car forms))
;						       :test #'same-id))
;					   :context *current-context*))
;		       T)))))))
;		(loop for (x . y) in subalist do
;		      (typecheck y :context *current-context*))


(defun check-with-subst* (forms subalist mod-inst res-params)
  (cond ((null forms) nil)
	((let ((subvars (substitutable-vars (car forms))))
	   (loop for (id . nil) in subalist
		 always (member id subvars
				:test
				#'(lambda (x y) (format-equal x (id y))))))
	 (let ((formal-inst
		(tc-unify-over
		 (loop for (x . y) in subalist
		       collect
		       (type (find x
				   (substitutable-vars
				    (car forms))
				   :test #'same-id )))
		 (mapcar #'cdr  subalist)
		 (mapcar #'(lambda (x)
			     (if (type (cdr x))
				 (list (type (cdr x)))
				 (types (cdr x))))
		   subalist)
		 (when res-params
		   (mapcar #'list res-params)))))
	   (cond ((null formal-inst)
		  (check-with-subst* (cdr forms) subalist mod-inst
				     res-params))
		 (t (let ((newform
			   (if (eq formal-inst T)
			       (car forms)
			       (subst-mod-params-alist (car forms)
						       formal-inst))))
		      (if (fully-instantiated? newform)
			  newform
			  (check-with-subst* (cdr forms) subalist mod-inst
					     res-params)))))))
	(t (check-with-subst* (cdr forms) subalist mod-inst res-params))))

(defun format-equal (x y)
  (let* ((x (format nil "~a" x))
	 (posx (position-if
		#'(lambda (x)(member x (list  #\! #\$)))
		x))
	 (y (format nil "~a" y))
	 (posy (position-if
		#'(lambda (x)(member x (list  #\! #\$)))
		y))
	 (x (subseq x 0 posx))
	 (y (subseq y 0 posy)))
    (equal x y)))


(defun delete-first-occurrence (x y &key test)
  (cond ((null x) y)
	(t (delete-first-occurrence (cdr x)
				    (remove (car x) y :test test
					    :count 1)
				    :test test))))

(defmethod forall-body* ((expr forall-expr))
  (forall-body* (expression expr)))

(defmethod forall-body* ((expr expr))
  expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lemma-step(name substs ps) collects the possible resolutions for name,
;either as a constant or a formula-name, and uses chec-with-subst to
;resolve from the context of the substitution substs, the intended
;resolution of name.  The formula returned by check-with-subst is used
;to extract the substitutable variables (subvars) which are the outer
;series of universally quantified variables, remove the variables
;that will be substituted leaving remaining-vars, make an alist with
;subvars (bindalist), construct intermediate-form which is form with the
;subvars made free, then construct subform by substituting for the
;subvars, and using subform to form the sform and the newsequent as a
;subgoal containing the relevant instance of the lemma as a premise.
;;;;;;;;;;;;;

(defun lemma-step (name substs ps)
  (let* ((name-expr (pc-parse name 'name))
	 (resolutions (append ;;was nconc(nsh:10.18.94)
		       (resolve name-expr 'formula nil)
		       (resolve name-expr 'expr nil)))
	 (resolutions
	  (cond ((freevars resolutions)
		 (format-if "~%Free variables in lemma name: ~a" name)
		 nil)
		(t resolutions)))
	 (pre-alist (make-alist substs))
	 (badnames (loop for (x . y) in pre-alist
			 when (not (typep (pc-parse x 'expr) 'name-expr))
			 collect x))
	 (subalist (loop for (x . y) in pre-alist
			 collect (cons x (internal-pc-typecheck
					  (pc-parse y  'expr)
					  :context
					  *current-context*
					  :uniquely? NIL))))
	                 ;;tccs ALL is checked in tc-alist below.
	 ;;(dependent-decls nil)
	 )
    (cond (;; SO - 4/14/93: Added this case to protect against bad
	   ;; substitution arguments (like numbers)
	   (not (listp substs))
	   (format-if
	    "~%The form of a substitution is: (<var1> <term1>...<varn> <termn>).")
	   (values 'X nil))
	  ((not (null badnames))
	   (format-if
	    "~%The form of a substitution is: (<var1> <term1>...<varn> <termn>).
The following are not possible variables: ~{~a,~}" badnames)
	   (values 'X nil))
	  ((oddp (length substs))
	   (format-if
	    "~%The form of a substitution is: (<varn> <termn>...<varn> <termn>).")
	   (values 'X nil))
;	  (subfreevars
;	   (format-if "~%Irrelevant free variables ~a in substitution."
;		      subfreevars)
;	   (values 'X nil))
	  (t (let* ((all-possibilities
		     (check-with-subst resolutions
				       subalist
				       *current-context*))
		    (possibilities
		     (or (remove-if-not #'(lambda (poss)
					    (typep (declaration (cdr poss))
						   'formula-decl))
			   all-possibilities)
			 all-possibilities))
		    (form (when (singleton? possibilities)
			    (caar possibilities)))
		    (res  (when (singleton? possibilities)
			    (cdar possibilities)))
		    (newalist
		     (when form
		       (loop for x in
			     (substitutable-vars form)
			     when (assoc x subalist :test #'same-id)
			     collect
			     (cons x
				   (cdr (assoc x subalist
					       :test #'same-id)))))))
;	       (loop for (x . y) in newalist
;		     do (setf (type y) nil))
	       (when form ;;NSH(10.20.94)(let ((*generate-tccs* 'ALL)))
			    (typecheck (module-instance res)
			      :tccs 'ALL)
			    (tc-alist newalist);;does tccs all.
			    )
	       (let ((subfreevars (loop for (x . y) in newalist
					append (freevars y)))) ;;was nconc
		 (cond ((null resolutions)
		      (format-if "~%Couldn't find a definition or lemma named ~a" name)
		      (values 'X nil))
		     ((null form)
		      (format-if "~%Found ~a resolutions for ~a relative to the substitution.
Please check substitution, provide actual parameters for lemma name,
or supply more substitutions."
				 (length possibilities) name)
		      (values 'X nil))
		     (subfreevars
		      (format-if "~%Irrelevant free variables ~a in substitution."
				 subfreevars)
		      (values 'X nil))
		     (t
			(let* 
			    ((subvars (substitutable-vars form))
			     (remaining-vars
			      (delete-first-occurrence
			       (mapcar #'car subalist)
			       subvars :test #'same-id))
			     (bindalist newalist)
;;			      (make-bindalist subvars subalist ps)
			     (intermediate-body (forall-body* form))
			     (intermediate-form (if (forall? form)
						    (if  (null remaining-vars)
							 (expression form)
							 (make-forall-expr
							     remaining-vars
							   (forall-body* form)))
						    form))
			     (subform
			      (substit intermediate-form bindalist))
			     (sform    (make-instance 's-formula
					 'formula (negate subform)))
			     (newsequent (lcopy (current-goal ps)
						's-forms (cons sform
							       (s-forms
								(current-goal
								 ps)))))
			     (dependent-decls NIL))
			  (push-references-list
			   (module-instance res) dependent-decls)
			  (push-references-list newalist
						dependent-decls)
			  (pushnew (declaration res)
				   dependent-decls)
			  (values '? (list (list newsequent
						 'dependent-decls
						 dependent-decls))))))))))))
   
		     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(NSH:4-12-91) rule for extracting the subtyping predicates
;;corresponding to a subtype type for a given expression.  Part of the
;;code is adapted from Sam's definition of type-constraints in prtop.
;;(NSH:6/6/91) Sam: generate warning if there are no predicates.

;(defun typepred-rule (expr)
;  (make-instance 'rule
;		 'rule-part (typepred-fun expr)
;		 'rule-input `(lemma ,expr)))

(defun typepred-fun (exprs all?)
  #'(lambda (ps)
      (if (listp exprs)
	  (typepred-step exprs all? ps)
	  (typepred-step (list exprs) all? ps))))

(defun typepred-step (exprs all? ps);;(6.18.96) added all?
  (let ((preds (loop for expr in exprs
		     append (collect-typepreds expr ps all?))))
    (if (null preds)
	(values 'X nil nil)
	(let* ((new-sforms
		(mapcar #'(lambda (fmla)
			    (make-instance 's-formula
			      'formula
			      (negate fmla)))
		  preds))
	       (references NIL))
	  (push-references-list
	   (mapcar #'formula new-sforms)
	   references)
	  (values '?
		  (list (list
			 (lcopy (current-goal ps)
			   's-forms
			   (append new-sforms
				   (s-forms
				    (current-goal ps))))
			 'dependent-decls
			 references)))))))
		

(defun collect-typepreds (expr ps &optional all?)
  (if (consp expr)
      (loop for x in expr append (collect-typepreds x ps all?))
      (let* ((tc-expr
	      (let ((*generate-tccs* 'all))
		(pc-typecheck (pc-parse expr 'expr))))
	     (freevars-expr (freevars tc-expr))
	     (freevars-seq (loop for sform in
				 (s-forms (current-goal ps))
				 append (freevars (formula sform)))))
	(cond ((loop for var in freevars-expr
		     thereis (not (member var freevars-seq :test #'tc-eq)))
	       (format-if "~%Irrelevant free variables in ~a" tc-expr)
	       nil)
	      (t (let* ((constraints (type-constraints tc-expr all?))
			(reduced-constraints
			 (loop for fmla in constraints
			       nconc (and+ fmla))))
		   reduced-constraints))))))



;;;One of the points of having simplify is so that steps that do not
;;;involve any branching can be combined into a single proof step.
;;;Simplify as described above does not treat steps such as adding
;;;substitutions, applying substitutions, introducing a lemma, etc.
;;;We need another function, simplify-step, to take us through the
;;;simplifications.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun iff-rule-fun (sformnums)
  #'(lambda (ps)
      (let ((sformnums (if (null sformnums) '*
			   (loop for sn in sformnums
				 append (if (consp sn) sn (list sn))))))
      (multiple-value-bind (signal subgoal)
	  (sequent-reduce
	   (current-goal ps) #'(lambda (sform) (iff-sform sform ps))
	   sformnums)
	(when (eq signal '?)
	  (format-if "~%Converting top level boolean equality into IFF form,"))
	(values signal (list subgoal))))))

(defun iff-sform (sform ps)
  (let ((fmla (formula sform)))
    (cond ((and (not-expr? fmla)
	       (equality? (args1 fmla))
	       (tc-eq (type (args1 (args1 fmla))) *boolean*)
	       (tc-eq (type (args2 (args1 fmla))) *boolean*))
	  (values '? (lcopy sform
			   'formula
			   (typecheck
			    (negate (mk-iff (args1 (args1 fmla))
					      (args2 (args1 fmla))))
			    :expected *boolean*
			    :context *current-context*))))
	  ((and (equality? fmla)
		(tc-eq (type  (args1 fmla)) *boolean*)
	       (tc-eq (type (args2  fmla)) *boolean*))
	   (values '? (lcopy sform 'formula
			    (typecheck
			     (mk-iff (args1 fmla)(args2 fmla))
			     :expected *boolean*
			     :context *current-context*))))
	  (t (values 'X sform)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun case-rule-fun (fmlas)
  #'(lambda (ps)
      (let* ((fmlas (if (listp fmlas) fmlas (list fmlas)))
	     ;;(*generate-tccs* 'ALL) ;;(NSH:10.20.94)
	     (tc-fmlas (loop for fml in fmlas
			    collect
			    (internal-pc-typecheck (pc-parse fml 'expr)
				       :expected *boolean*
				       :tccs 'ALL 
				       :context *current-context*)))
	     (freevars (freevars tc-fmlas)))
	(cond ((null tc-fmlas)
	       (format-if "~%No formulas given.")
	       (values 'X nil nil))
	      ((not (null freevars))
	       (format-if
		 "~%Irrelevant free variables ~{~a, ~} occur in formulas."
		 freevars)
	       (values 'X nil nil))
	      (t ;;(format-if "~%Introducing case splits,")
	       ;;(push-references tc-fmlas ps)
	       (values '? (make-cases (current-goal ps) tc-fmlas
				      (dependent-decls ps))))))))

(defun make-cases (goal fmlas &optional references accum)
  (cond ((null fmlas) (cons (list goal 'dependent-decls references)
			    accum))
	(t (let* ((neg-fmla (negate (car fmlas)))
		  (pos-sform (make-instance 's-formula 'formula (car fmlas)))
		  (neg-sform (make-instance 's-formula 'formula neg-fmla))
		  (references references)
		  
		  (neg-goal (copy goal 's-forms
				  (cons neg-sform (s-forms goal))))
		  
		  )
	     (push-references-list (car fmlas)
				   references)
	     (let ((pos-goal (list
			     (copy goal 's-forms
				   (cons pos-sform (s-forms goal)))
			     'dependent-decls
			     references)))
	       (make-cases neg-goal (cdr fmlas) references
			   (cons pos-goal accum)))))))


;(defun make-cases (goal fmlas)
;  (cond ((null fmlas) (list goal))
;	(t (let* ((result (make-cases goal (cdr fmlas)))
;		  (neg-fmla (negate (car fmlas)))
;		  (pos-sform (make-instance 's-formula 'formula (car fmlas)))
;		  (neg-sform (make-instance 's-formula 'formula neg-fmla))
;		  (neg-cases (loop for x in result
;				   collect
;				   (copy x 's-forms
;					 (cons neg-sform (s-forms x)))))
;		  (pos-cases (loop for x in result
;				   collect
;				   (copy x 's-forms (cons pos-sform
;							  (s-forms x))))))
;	     (append neg-cases pos-cases)))))


						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;extensionality axioms introduced as premises for types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-variable (id type context)
  (let ((var (pc-parse id 'expr)))
    (setf (type var) type
	  (kind var) 'variable
	  (resolutions var) 
	  (list
	   (make-resolution (mk-bind-decl id type type)
	     (mod-name context))))
    var))

;;; SO 8/17/94 - Fixed for new form of application
(defun function-extensionality (type given context)
  (let* ((fname (generate-variable  (gentemp "f_")
				   given
				   context))
	 (gname (generate-variable (gentemp "g_")
				   given
				   context))
	 (argtypes (domain-types type))
	 (args (function-extensionality-args argtypes context))
	 (hyp (close-freevars (make-equality
			       (make-application* fname args)
			       (make-application* gname args))
			      context
			      args))
	 (conc (make-equality fname gname))
	 (fmla (close-freevars (make-implication hyp conc)
			       context
			       (list fname gname))))
    fmla))

(defun function-extensionality-args (argtypes context &optional vars)
  (if (null argtypes)
      (nreverse vars)
      (let* ((dep (car argtypes))
	     (typ (if (typep dep 'dep-binding) (type dep) dep))
	     (var (generate-variable (gentemp "x_") typ context)))
	(function-extensionality-args
	 (if (typep dep 'dep-binding)
	     (substit (cdr argtypes) (acons dep var nil))
	     (cdr argtypes))
	 context
	 (cons var vars)))))

(defun list-of-projections (types tuptype context &optional (num 1))
  (when types
    (let* ((decl (mk-proj-decl (makesym "PROJ_~d" num)
		  (mk-funtype (list tuptype)
			      (if (dep-binding? (car types))
				  (type (car types))
				  (car types)))))
	  (proj (change-class (mk-name-expr  decl nil nil nil 'constant)
			  'projection-expr)))
      (add-decl decl context)
      (setf (index proj) num)
      (cons proj
	    (list-of-projections (cdr types) tuptype context (1+ num))))))

(defun make-and-equal* (lhs-list rhs-list)
  (make-conjunction (loop for lhs in lhs-list
			  as rhs in rhs-list
			  collect (make-equality lhs rhs))))

(defun tuple-extensionality (type given context)
  (let* ((lhs (generate-variable (gentemp "u_")
				  given context))
	 (rhs (generate-variable  (gentemp "v_")
				  given context))
	 (equality-list
	  (loop for index from 1 to
		(length (types type))
		collect
		(make-equality (make-projection-application index lhs)
			       (make-projection-application index rhs))))

	 (hyp (make-conjunction equality-list))
	 (conc (make-equality lhs rhs)))
    (close-freevars (make-implication hyp conc) context (list lhs rhs))))

(defun record-extensionality (type given  context)
  (let* ((lhs (generate-variable  (gentemp "r_")
				  given context))
	 (rhs (generate-variable  (gentemp "s_")
				  given context))
	 ;;SO 9/3/94 - changed for field-applications
	 (lhs-list
	  (loop for decl in (fields type)
		collect (make-field-application (id decl) lhs)))
	 (rhs-list
	  (loop for decl in (fields type)
		collect (make-field-application (id decl) rhs)))
;	 (fun-list (loop for decl in (fields type)
;			 collect (make-name-expr decl T)))
;	 (lhs-list
;	  (loop for fun in fun-list
;		collect
;		(make-application*
;		    fun
;		  (list lhs))))
;	 (rhs-list
;	  (loop for fun in fun-list
;		collect
;		(make-application*
;		    fun
;		  (list rhs))))
	 (hyp (make-and-equal* lhs-list rhs-list))
	 (conc (make-equality lhs rhs)))
    (close-freevars (make-implication hyp conc) context (list lhs rhs))))



;(defun record-extensionality (type context)
;  (let* ((lhs (generate-variable (new-symbol '|r| *bind-counter*)
;				  type context))
;	 (fun-list (loop for decl in (fields type)
;			 collect (make-name-expr decl T)))
;	 (rhs-list
;	  (loop for fun in fun-list
;		collect
;		(mk-assignment 'uni (list (list fun))
;			       (make-application*
;				   fun
;				 (list lhs)))))
;	 (rhs (make-record-expr rhs-list type)))
;    (close-freevars (make-equality lhs rhs) context (list lhs))))

(defun extensionality (texpr)
  #'(lambda (ps)
      (let* ((expr (pc-parse texpr 'type-expr))
	     ;;(*generate-tccs* 'ALL) ;;NSH(10.20.94)
	     (tc-expr (typecheck expr :tccs 'ALL
				 :context *current-context*))
	     ;;(*generate-tccs* NIL)
	     (freevars (freevars tc-expr)))
	(cond (freevars
	       (format-if "~%The following irrelevant free variables
occur in the given type expression: ~a." freevars)
	       (values 'X nil))
	      ((not (fully-instantiated? tc-expr))
	       ;;NSH(5.5.97): Hoffman reported this test missing.
	       (format-if "~%The given type expression contains ~
free theory parameters
which should be fully instantiated. Please supply actual parameters.")
	       (values 'X nil))
	      (t (extensionality-step tc-expr tc-expr ps))))))

(defmethod extensionality-step ((texpr funtype) given ps)
  (let* ((references NIL))
    (push-references-list texpr references)
    (values '?
	    (list
	     (list
	      (copy (current-goal ps)
		's-forms
		(cons (make-instance 's-formula
			'formula
			(negate
			 (function-extensionality
			  texpr given
			  *current-context*)))
		      (s-forms (current-goal ps))))
	      'dependent-decls
	      references)))))

(defmethod extensionality-step ((texpr tupletype) given ps)
  (let* ((references NIL))
    (push-references-list texpr references)
    (values '?
	    (list
	     (list (copy (current-goal ps)
		     's-forms
		     (cons (make-instance 's-formula
			     'formula
			     (negate
			      (tuple-extensionality
			       texpr given 
			       *current-context*)))
			   (s-forms (current-goal ps))))
		   'dependent-decls
		   references)))))

(defmethod extensionality-step ((texpr recordtype) given ps)
  (let* ((references NIL))
    (push-references-list texpr references)
    (values '?
	  (list
	   (list (copy (current-goal ps)
		   's-forms
		   (cons (make-instance 's-formula
			   'formula
			   (negate
			    (record-extensionality
			     texpr given 
			     *current-context*)))
			 (s-forms (current-goal ps))))
		 'dependent-decls references)))))

(defmethod extensionality-step ((texpr type-name) given ps)
  (let ((tdecl (declaration texpr)))
    (cond ((and (or (typep tdecl 'type-eq-decl)
		    (typep tdecl 'type-from-decl))
		(not (typep (type-expr tdecl) 'enumtype)))
	   (extensionality-step (type-value (declaration texpr))
				given ps))
	  (t
	   (format-if "~%Could not find a suitable extensionality axiom for ~a." given)
	   (values 'X nil)))))

(defmethod extensionality-step ((texpr subtype) given ps)
  (if (and (adt? (supertype texpr))
	   (recognizer? (predicate texpr)))
      (let* ((constructor (constructor (predicate texpr)))
	     (fmla-decl (when constructor
			  (get-formula (get-module
					(module-instance constructor))
				       (intern
					(format nil
					    "~a_~a_extensionality"
					  (id (supertype texpr))
					  (id constructor)
					  )))))
	     ;;the 2nd fmla-decl binding is needed because the prelude as of
	     ;;Nov. '93 is not consistent in its ADT conventions with that
	     ;;of generated ADTs.
	     (fmla-decl (if fmla-decl fmla-decl 
			(when constructor
			  (get-formula (get-module
					(module-instance constructor))
				       (intern
					(format nil
					    "~a_extensionality"
					  (id constructor)))))))    
	     (new-fmla (when fmla-decl
			 (subst-mod-params
			  (definition fmla-decl)
			  (module-instance
			   (resolution constructor))))))
	(cond (new-fmla
	       (let* ((references NIL))
		 (push-references-list texpr references)
		 (pushnew fmla-decl references)
		 (values '?
			 (list
			  (list (copy (current-goal ps)
				  's-forms
				  (cons (make-instance 's-formula
					  'formula
					  (negate new-fmla))
					(s-forms (current-goal ps))))
				'dependent-decls
				references)))))
	      (t (format-if "~%Could not find ADT extensionality axiom for ~a." given)
		 (values 'X nil))))
      (extensionality-step (supertype texpr) (supertype texpr) ps)))

(defmethod extensionality-step ((texpr type-expr) given ps)
  (format-if "~%Could not find extensionality axiom for ~a." given)
    (values 'X nil nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(3-15-92):name rule is useful for naming a subexpression, particularly
;;if it is useful for generalizing the statement for a second induction.
;;



(defun add-name (name expr)
  #'(lambda (ps)
      (add-name-step name expr ps)))

(defun get-prelude-decls (ref)
  (let ((decls nil))
    (maphash #'(lambda (mid mod)
		 (declare (ignore mid))
		 (when (module? mod)
		   (setq decls (append (gethash (ref-to-id ref)
						(declarations mod))
				       decls))))
	     *prelude*)
    decls))

;; Changed to create a skolem-const-decl instead of a const-decl -- crw
;; (the name skolem-const-decl should be changed, but I didn't want
;; to change that much code)
(defun add-name-step (name expr ps)
  (let* ((name (if (stringp name) (intern name) name))
	 ;;(*generate-tccs* 'ALL)
	 (pc-name (pc-parse name 'expr))
	 (expr (internal-pc-typecheck (pc-parse expr 'expr)
		  :tccs 'ALL
		  :context *current-context*))
	  (context (copy-prover-context)))
     (cond ((not (valid-pvs-id* name))
	    (format-if "~%Error: ~a is not a valid symbol." name)
	    (values 'X nil nil))
	   ((resolve pc-name 'expr nil)
	    (format-if "~%Error: ~a is already declared." name)
	    (values 'X nil nil))
	   (t (put-decl (make-instance
			   'skolem-const-decl
			 'id name
			 'type (type expr)
			 'module (module context))
		       (local-proof-decls context))
	      (let* ((name (typecheck (pc-parse name 'expr)
			     :tccs 'ALL
			     :context context))
		     (formula (make-equality expr name))
		     (references NIL)
		     (fvars (freevars formula)))
	       (cond (fvars
		      (format-if "~%Free variables ~a in expr = name" fvars)
		      (values 'X nil nil))
	       ;;(push-references expr ps)
		     (t (push-references-list formula references)
			(values '?
				(list
				 (cons (copy (current-goal ps)
					 's-forms
					 (cons (make-instance 's-formula
						 'formula
						 (negate formula))
					       (s-forms (current-goal ps))))
				       (list 'context context
					     'dependent-decls
					     references)))))))))))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
