;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random-test.lisp -- Random testing in PVS
;; Author          : Sam Owre
;; Created On      : 
;; Last Modified By: Sam Owre
;; Last Modified On: 
;; Update Count    : 50
;; Status          : Beta test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

;;; Based on "Random testing in Isabelle/HOL"

;; Generate random elements of a given PVS type, and use them to evaluate a
;; given formula.

;; Future work
;; - user-defined random tests at the PVS level
;; - different distributions
;; - 'test' can take formula names as well as formulas
;; - inductive definitions
;; - remove duplicate tests
;; - allow different sizes to be associated with different types
;;     (e.g., 100000 for int, 10 for list[int])
;; - generate random types for type parameters/uninterpreted types
;; - higher-order (e.g. A \subset B U C)

(in-package :pvs)

(defun random-range (l h)
  (+ (random (1+ (- h l))) l))

(defun one-of (list)
  (nth (random (length list)) list))

(defun frequency (alist)
  ;; alist keys are integers >= 0
  (let* ((tot (reduce #'+ alist :key #'car))
	 (rnum (random tot)))
    (frequency* alist rnum)))

(defun frequency* (alist rnum &optional (cnum 0))
  (if (null alist)
      (break "somethings wrong")
      (let ((nnum (+ (caar alist) cnum)))
	(if (< rnum nnum)
	    (cdar alist)
	    (frequency* (cdr alist) rnum nnum)))))

(defvar *random-generator-sizes*)

(defvar *lazy-random-functions*)

(defvar *in-random-generator* nil)

;; Returns a generator that takes two size arguments: size (i) and dtsize (d)
(defun random-generator (te)
  (assert (not *in-random-generator*) () "random-generator is not re-entrant")
  (let ((*in-random-generator* t))
    #'(lambda (i d)
	(let ((*random-generator-sizes* nil))
	  (random-generator* te i d)))))

(defmethod random-generator* :around (x i d)
  (declare (ignore x i d))
  (let ((v (call-next-method)))
    (assert (type v))
    v))

(defmethod random-generator* ((te type-name) i d)
  (declare (ignore d))
  (cond ((tc-eq te *boolean*)
	 (one-of (list *true* *false*)))
	((tc-eq te *number*)
	 (let ((num (/ (random-range (- i) i) (random-range 1 i))))
	   (make!-number-expr num)))
	(t (invoke-restart 'terminate-random-loop
	     "~a uninterpreted - use :instance to provide a type" te))))

(defmethod random-generator* ((te tupletype) i d)
  (make!-tuple-expr* (random-generator-types (types te) i d)))

(defmethod random-generator* ((te dep-binding) i d)
  (random-generator* (type te) i d))

(defun random-generator-types (types i d &optional values)
  (if (null types)
      (nreverse values)
      (let* ((ty (car types))
	     (rval (random-generator* ty i d)))
	(random-generator-types
	 (if (dep-binding? ty)
	     (substit (cdr types) (acons ty rval nil))
	     (cdr types))
	 i d
	 (cons rval values)))))

(defmethod random-generator* ((te cotupletype) i d)
  (one-of (let ((index 0))
	    (mapcar #'(lambda (ty)
			(make!-injection-application
			 (incf index)
			 (random-generator* ty i d)
			 te))
	      (types te)))))

(defmethod random-generator* ((te funtype) i d)
  (let* ((id (make-new-variable 'x (list te)))
	 (bd (make-bind-decl id (domain te)))
	 (var (make-variable-expr bd))
	 (ran (range te))
	 (pairs (random-funtype-values (domain te) ran i d)))
    (if pairs
	(make!-lambda-expr (list bd)
	  (if (compatible? (domain te) *integer*)
	      (make-random-subrange-if-expr pairs var)
	      (make-random-datatype-cases-expr pairs var)))
	(if (tc-eq (range te) *boolean*)
	    (make-random-set-expr (domain te) i d)
	    (make-lazy-random-function te i d)))))

(defmethod random-generator* ((te recordtype) i d)
  (make-record-expr
   (random-generator-fields (fields te) i d)
   te))

(defun random-generator-fields (fields i d &optional assigns)
  (if (null fields)
      (nreverse assigns)
      (let* ((fld (car fields))
	     (rval (random-generator* (type fld) i d))
	     (assn (make-assignment (mk-name-expr (id fld)) rval)))
	(random-generator-fields
	 (substit (cdr fields) (acons fld rval nil))
	 i d
	 (cons assn assigns)))))

(defun lazy-random-function? (ex)
  (and (boundp '*lazy-random-functions*)
       (assq ex *lazy-random-functions*)))

(defun make-lazy-random-function (te i d)
  (let* ((fun-id (gentemp "lazy-fun"))
	 (fun-name (makeskoconst (mk-name-expr fun-id) te *current-context*)))
    (push (list fun-name i d) *lazy-random-functions*)
    fun-name))

(defun generate-lazy-random-lisp-function (expr)
  (let* ((ex.i.d (assq expr *lazy-random-functions*))
	 (ht (when ex.i.d (make-hash-table :test 'equal))))
    (if (dependent? (type expr))
	(progn
	  (setf (symbol-function (id (car ex.i.d)))
		#'(lambda (e)
		    (or (gethash e ht)
			(let* ((dtype (domain (type expr)))
			       (pe (catch 'cant-translate
				     (cl2pvs e (type dtype)))))
			  (cond (pe
				 (typecheck pe :expected (type dtype))
				 (let* ((ran (substit (range (type expr))
					       (acons dtype pe nil)))
					(pv (apply (random-generator ran)
					      (cdr ex.i.d)))
					(val (eval (pvs2cl pv))))
				   (setf (gethash e ht) val)))
				(t (error "Can't untranslate")))))))
	  (setf (symbol-value (id (car ex.i.d))) ht)
	  (id expr))
	(let ((gen (random-generator (range (type expr)))))
	  (setf (symbol-function (id (car ex.i.d)))
		#'(lambda (e)
		    (or (gethash e ht)
			(let ((val (eval (pvs2cl (funcall gen (cdr ex.i.d))))))
			  (setf (gethash e ht) val)))))
	  (setf (symbol-value (id (car ex.i.d))) ht)
	  (id expr)))))

(defun make-random-set-expr (dom i d)
  (let ((values nil))
    (dotimes (cnt (random (1+ i)))
      (pushnew (random-generator* dom i d) values :test #'tc-eq))
    (when (compatible? dom *real*)
      (setq values
	    (sort values #'< :key #'get-number-value)))
    (make!-set-list-expr values dom)))

(defmethod get-number-value ((ex number-expr))
  (number ex))

(defmethod get-number-value ((ex application))
  (cond ((tc-eq (operator ex) (minus-operator))
	 (- (get-number-value (argument ex))))
	((tc-eq (operator ex) (divides-operator))
	 (/ (get-number-value (args1 ex)) (get-number-value (args2 ex))))
	(t (break "Something's wrong"))))
      
      

(defun make-random-subrange-if-expr (pairs var)
  (assert pairs)
  (make-random-subrange-if-expr* (reverse pairs) var))

(defun make-random-subrange-if-expr* (pairs var &optional if-expr)
  (let ((eqn (make!-equation var (caar pairs))))
    (if (null (cdr pairs))
	(if if-expr
	    (make!-if-expr eqn (cdar pairs) if-expr)
	    (cdar pairs))
	(make-random-subrange-if-expr*
	 (cdr pairs) var
	 (if if-expr
	     (make!-chained-if-expr eqn (cdar pairs) if-expr)
	     (cdar pairs))))))

(defun make-random-datatype-cases-expr (pairs var)
  (assert pairs)
  (make-random-datatype-cases-expr* (reverse pairs) var))

(defun make-random-datatype-cases-expr* (pairs var &optional selections)
  (if (null pairs)
      (make!-cases-expr var selections nil)
      (make-random-datatype-cases-expr*
       (cdr pairs) var
       (cons (mk-selection (caar pairs) nil (cdar pairs)) selections))))

(defmethod random-funtype-values ((dom subtype) ran i d)
  (cond ((below? dom)
	 (let ((ub (below? dom)))
	   (when (number-expr? ub)
	     (random-subrange-funtype-values 0 (1- (number ub)) ran i d))))
	((upto? dom)
	 (let ((ub (upto? dom)))
	   (when (number-expr? ub)
	     (random-subrange-funtype-values 0 (number ub) ran i d))))
	((subrange? dom)
	 (let ((bds (subrange? dom)))
	   (when (and (ground-number-expr? (car bds))
		      (ground-number-expr? (cdr bds)))
	     (random-subrange-funtype-values
	      (ground-number (car bds)) (ground-number (cdr bds)) ran i d))))
	(t (let ((pairs (random-funtype-values (supertype dom) ran i d)))
	     (remove-if #'(lambda (elt)
			    (let ((pred (make!-application (predicate dom)
					  (car elt))))
			      (multiple-value-bind (stval error)
				  (catch 'undefined (pvs2cl pred))
				(when (eq stval 'cant-translate)
				  (error "~s could not be translated:~%~a"
					 pred error))
				(multiple-value-bind (pval error)
				    (catch 'undefined (eval stval))
				  (when error
				    (error "~%~a" error))
				  (not pval)))))
	       pairs)))))

;; (defmethod random-funtype-values ((dom dep-binding) ran i d)
;;   (break "dep-binding"))

(defmethod random-funtype-values ((dom adt-type-name) ran i d)
  (if (enum-adt? dom)
      (let ((constrs (constructors dom)))
	(random-enum-funtype-values constrs ran i d))
      (call-next-method)))

(defmethod random-funtype-values (dom ran i d)
  (declare (ignore dom ran i d))
  nil)

(defun random-enum-funtype-values (constrs ran i d &optional pairs)
  (if (null constrs)
      (nreverse pairs)
      (random-enum-funtype-values
       (cdr constrs) ran i d
       (acons (car constrs) (random-generator* ran i d) pairs))))

(defmethod ground-number-expr? ((ex number-expr))
  t)

(defmethod ground-number-expr? ((ex application))
  (and (is-minus? (operator ex))
       (ground-number-expr? (argument ex))))

(defmethod ground-number-expr? (expr)
  (declare (ignore expr))
  nil)

(defmethod ground-number ((ex number-expr))
  (number ex))

(defmethod ground-number ((ex application))
  (- (ground-number (argument ex))))

(defun random-subrange-funtype-values (lbnd ubnd ran i d &optional pairs)
  (if (> lbnd ubnd)
      (nreverse pairs)
      (random-subrange-funtype-values
       (1+ lbnd) ubnd ran i d
       (acons (make!-number-expr lbnd) (random-generator* ran i d) pairs))))
				      

(defvar *random-subtype-gen-bound*)

(defmethod random-generator* ((te subtype) i d)
  (cond ((available-random-generator te i d))
	((tc-eq te *naturalnumber*)
	 (make!-number-expr (random (1+ i))))
	((tc-eq te *integer*)
	 (make!-number-expr (random-range (- i) i)))
	((and (subtype-of? te *number*)
	      (subtype-of? *rational* te))
	 (let ((num (/ (random-range (- i) i) (random-range 1 i))))
	   (make!-number-expr num)))
	(t (dotimes (j *random-subtype-gen-bound*
		       (error "Could not generate random element for subtype ~%~a of size ~d after ~d attempts"
			 te i *random-subtype-gen-bound*))
	     (let* ((stran (random-generator* (supertype te) i d))
		    (pred (make!-application (predicate te) stran)))
	       (multiple-value-bind (stval error)
		   (catch 'undefined (pvs2cl pred))
		 (when (eq stval 'cant-translate)
		   (error "~s could not be translated:~%~a" pred error))
		 (multiple-value-bind (pval error)
		     (catch 'undefined (eval stval))
		   (when error
		     (error "~%~a" error))
		   (when pval
		     (return stran)))))))))

(defun available-random-generator (subtype i d)
  ;; Hook for specific random generators to be provided for subtypes.
  nil)
		     

(defmethod random-generator* ((te datatype-subtype) i d)
  (random-generator* (declared-type te) i d))

(defmethod random-generator* ((te adt-type-name) i d)
  (let* ((constrs (constructors te))
	 (recs (recursive-constructors constrs))
	 (nonrecs (remove-if #'(lambda (c) (memq c recs)) constrs))
	 (entry (assoc te *random-generator-sizes* :test #'tc-eq))
	 (j (or (cdr entry) d)))
    (assert nonrecs () "No non-recursive constructors?")
    (if recs
	(funcall
	 (frequency
	  (list
	   (cons j
		 (if (cdr recs)
		     (lambda ()
		       (funcall
			(one-of (mapcar
				    #'(lambda (c)
					(lambda ()
					  (let ((*random-generator-sizes*
						 (acons te (1- j)
							*random-generator-sizes*)))
					    (make!-application*
					     c
					     (mapcar #'(lambda (a)
							 (random-acc-arg-value
							  a i d))
					       (accessors c))))))
				  recs))))
		     (lambda ()
		       (let ((*random-generator-sizes*
			      (acons te (1- j) *random-generator-sizes*)))
			 (make!-application* (car recs)
					     (mapcar #'(lambda (a)
							 (random-acc-arg-value
							  a i d))
					       (accessors (car recs))))))))
	   (cons 1
		 (if (cdr nonrecs)
		     (lambda ()
		       (funcall
			(one-of
			 (mapcar #'(lambda (c)
				     (lambda ()
				       (if (accessors c)
					   (make!-application*
					    c
					    (mapcar #'(lambda (a)
							(random-acc-arg-value
							 a i d))
					      (accessors c)))
					   c)))
			   nonrecs))))
		     (lambda ()
		       (if (accessors (car nonrecs))
			   (make!-application*
			    (car nonrecs)
			    (mapcar #'(lambda (a) (random-acc-arg-value a i d))
			      (accessors (car nonrecs))))
			   (car nonrecs))))))))
	(if (cdr nonrecs)
	    (funcall
	     (one-of
	      (mapcar #'(lambda (c)
			  (lambda ()
			    (if (accessors c)
				(make!-application*
				 c
				 (mapcar #'(lambda (a)
					     (random-acc-arg-value a i d))
				   (accessors c)))
				c)))
		nonrecs)))
	    (if (accessors (car nonrecs))
		(make!-application*
		 (car nonrecs)
		 (mapcar #'(lambda (a)
			     (random-acc-arg-value a i d))
		   (accessors (car nonrecs))))
		(car nonrecs))))))

(defun random-acc-arg-value (a i d)
  (let ((av (random-generator* (range (type a)) i d)))
    (assert (type av))
    av))

(defun recursive-constructors (constrs &optional recs)
  (if (null constrs)
      (nreverse recs)
      (recursive-constructors
       (cdr constrs)
       (if (recursive-constructor? (car constrs))
	   (cons (car constrs) recs)
	   recs))))

(defun recursive-constructor? (constr)
  (when (funtype? (type constr))
    (let ((dtype (supertype (range (type constr)))))
      (occurs-in dtype (domain (type constr))))))


;; (defun gen-list* (constructors i j)
;;   (if (zerop i)
;;       (car constructors)
;;       (frequency
;;        (list
;; 	(let* ((c (cadr constructors))
;; 	       (cte (domain (type c))))
;; 	  (cons i (make!-application c
;; 		    (funcall (random-generator (car (types cte))) j)
;; 		    (gen-list* constructors (1- i) j))))
;; 	(cons 1 (car constructors))))))

;; (defun gen-list (list-type i) (gen-list* (constructors list-type) i i))
      
    

;;; Allow testing for sequents

(addrule 'random-test () ((fnum *) (count 10) (size 100) (dtsize 10)
			  all? verbose? instance (subtype-gen-bound 1000))
  (random-test-step fnum count size dtsize all? verbose? instance
		    subtype-gen-bound)
  "Runs a random test on the given FNUMs, by creating random values for the
skolem constants and running the ground evaluator on those values.  This is
useful for checking if the given sequent is worth proving - if it comes back
with a counter example, then it may not be worth trying to prove.  Of
course, it may just be that a lemma is needed, or relevant formulas were
hidden, and that it isn't really a counter example.  COUNT tests are run,
and SIZE controls how the random data is generated by providing a bound, if
necessary (e.g., an integer will be generated between -SIZE and SIZE).
Normally the test stops when a counter example is found; if ALL? is t then
it stops only after COUNT tests.  VERBOSE?, if t, causes each test to be
printed before evaluation.  INSTANCE allows formals and uninterpreted types
and constants to be given as a theory instance with actuals and mappings.
The current theory may also be instantiated this way.")

(defun random-test-step (fnum count size dtsize all? verbose? instance
			      subtype-gen-bound)
  #'(lambda (ps) (random-test-step* fnum count size dtsize all? verbose?
				    instance subtype-gen-bound ps)))

(defun random-test-step* (fnum count size dtsize all? verbose? instance
			       subtype-gen-bound ps)
  (let* ((goalsequent (current-goal ps))
	 (sforms (select-seq (s-forms goalsequent) fnum)))
    (when sforms
      (let* ((form (create-sequent-formula sforms))
	     (skoconsts (collect-subterms form #'skolem-constant?))
	     (bindalist (make-constant-bind-decl-alist skoconsts nil form))
	     (varalist (mapcar #'(lambda (sko->decl)
				   (cons (car sko->decl)
					 (make-variable-expr (cdr sko->decl))))
			 bindalist))
	     (sform (gensubst form
		      #'(lambda (x) (cdr (assoc x varalist :test #'tc-eq)))
		      #'(lambda (x) (assoc x varalist :test #'tc-eq))))
	     (uform (universal-closure sform)))
	(run-random-test uform count size dtsize all? verbose?
			 instance subtype-gen-bound varalist)))))

(defun create-sequent-formula (sforms)
  (make!-implication (make!-conjunction*
		      (mapcar #'(lambda (sf)
				  (negate! (formula sf)))
			(neg-s-forms sforms)))
		     (make!-disjunction*
		      (mapcar #'formula (pos-s-forms sforms)))))


(defun run-random-test (ex count size dtsize
			   &optional all? verbose? instance
			   (subtype-gen-bound 1000) skomap)
  (let* ((*random-subtype-gen-bound* subtype-gen-bound)
	 (inst (when instance
		 (typecheck (pc-parse instance 'modname))))
	 (expr (if inst
		   (subst-mod-params ex inst)
		   ex))
	 (body (quant-body* expr nil))
	 (vars (quant-bndvars* expr nil))
	 (terminated? nil))
    (restart-case
     (dotimes (i count)
       (let* ((*lazy-random-functions* nil)
	      (subst (create-random-test-subst vars size dtsize))
	      (sbody (substit body subst)))
	 (when subst
	   (multiple-value-bind (cl-input error)
	       (catch 'undefined (pvs2cl sbody))
	     (when (eq cl-input 'cant-translate)
	       (format t "~s could not be translated:~%~a" expr error)
	       (throw 'abort t))
	     (when verbose?
	       (format t "~%Testing ~{~a:=~a~^, ~}:"
		 (mapcan #'(lambda (s)
			     (let ((sk (rassoc (car s) skomap
					       :key #'declaration))
				   (lf (when (name-expr? (cdr s))
					 (assoc (cdr s)
						*lazy-random-functions*))))
			       (list (if sk
					 (id (car sk))
					 (id (car s)))
				     (if lf
					 (lazy-function-values lf)
					 (cdr s)))))
		   subst)))
	     (multiple-value-bind (cl-eval error)
		 (ignore-lisp-errors (catch 'undefined (eval cl-input)))
	       (if (not error)
		   (let ((clval (catch 'cant-translate
				  (cl2pvs cl-eval (type expr)))))
		     (cond (clval
			    (cond
			     ((tc-eq clval *false*)
			      (format t
				  "~%The formula is falsified with the substitutions: ~{~% ~a ==> ~{~a~^, ~}~}"
				(mapcan
				    #'(lambda (s)
					(let ((sk (rassoc (car s) skomap
							  :key #'declaration))
					      (lf (when (name-expr? (cdr s))
						    (assoc (cdr s)
							   *lazy-random-functions*))))
					  (list (if sk
						    (id (car sk))
						    (id (car s)))
						(if lf
						    (lazy-function-values lf)
						    (list (cdr s))))))
				  subst))
			      (unless all?
				(setq terminated? t)
				(return)))
			     (verbose? (format t " Formula is valid"))))
			   (t
			    (format t "Result not ground.  Cannot convert back to PVS.")
			    (format t "~%~a" cl-eval))))
		   (format t "~%~a" error)))))))
     (terminate-random-loop (fmt &rest args)
			    (setq terminated? t)
			    (format t "~%~?~%Terminating random test"
			      fmt args)))
    (unless terminated?
      (format t "~%No counter-examples found in ~d attempts" count))))

(defun lazy-function-values (ex.i)
  (let* ((fun-id (id (car ex.i)))
	 (ht (symbol-value fun-id))
	 (pairs nil))
    (maphash #'(lambda (x y)
		 (push (format nil "~a -> ~a" x y) pairs))
	     ht)
    pairs))
    
(defun create-random-test-subst (vars size dtsize &optional (ovars vars) subst)
  (if (null vars)
      (nreverse subst)
      (let ((bd (car vars)))
	(multiple-value-bind (v err)
	    (ignore-lisp-errors
	      (funcall (random-generator (type bd)) size dtsize))
	  (if err
	      (invoke-restart 'terminate-random-loop
			      "~%Value not generated for ~a:~%~a" bd err)
	      (let ((nsubst (acons bd v subst)))
		(create-random-test-subst
		 (substit (cdr vars) nsubst)
		 size dtsize (cdr ovars)
		 (acons (car ovars) v subst))))))))
