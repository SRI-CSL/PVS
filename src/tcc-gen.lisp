;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcc-gen.lisp -- Generates the TCCs
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 00:32:38 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:16:57 1998
;; Update Count    : 45
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

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
      (add-tcc-comment 'subtype expr expected incs nil)
      (let* ((*old-tcc-name* nil)
	     (ndecl (make-subtype-tcc-decl expr incs)))
	(if ndecl
	    (if (termination-tcc? ndecl)
		(insert-tcc-decl 'termination-subtype
				 expr
				 (recursive-signature (current-declaration))
				 ndecl)
		(insert-tcc-decl 'subtype expr expected ndecl))
	    (add-tcc-comment 'subtype expr expected incs nil)))))

(defvar *simplify-tccs* nil)

(defun make-subtype-tcc-decl (expr incs)
  (assert (every #'type incs))
  (let* ((*generate-tccs* 'none)
	 (conc (make!-conjunction* incs))
	 (*no-expected* nil)
	 (*bound-variables* *keep-unbound*)
	 (tform (add-tcc-conditions conc))
	 (uform (cond ((tcc-evaluates-to-true conc tform)
		       *true*)
		      (*simplify-tccs*
		       (pseudo-normalize tform))
		      (t tform)))
	 (id (make-tcc-name expr)))
    (assert (tc-eq (find-supertype (type uform)) *boolean*))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error expr "Subtype TCC for ~a simplifies to FALSE~@[:~2%  ~a~]"
		    expr (unless (tc-eq uform *false*) uform)))
      (typecheck* (if (and *recursive-subtype-term*
			   (occurs-in-eq *recursive-subtype-term* incs))
		      (mk-termination-tcc id uform)
		      (mk-subtype-tcc id uform))
		  nil nil nil))))

(defvar *substitute-let-bindings* nil)

(defun add-tcc-conditions (expr)
  (multiple-value-bind (conditions vdecl1)
      (subst-var-for-recs (remove-duplicates *tcc-conditions* :test #'equal))
    (multiple-value-bind (srec-expr vdecl)
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
	(universal-closure (add-tcc-conditions* (raise-actuals srec-expr)
						conditions substs nil))))))

(defun add-tcc-conditions* (expr conditions substs antes)
  (if (null conditions)
      (let ((ex (substit (if antes
			     (make!-implication
			      (make!-conjunction* antes) expr)
			     expr)
		  substs)))
	(assert (type ex))
	ex)
      (cond ((consp (car conditions))
	     ;; bindings from a lambda-expr application (e.g., let-expr)
	     (add-tcc-conditions*
	      expr
	      (cdr conditions)
	      (if *substitute-let-bindings*
		  (cons (car conditions) substs)
		  substs)
	      (if *substitute-let-bindings*
		  antes
		  (cons (make!-equation
			 (mk-name-expr (caar conditions))
			 (cdar conditions))
			antes))))
	    ((typep (car conditions) 'bind-decl)
	     ;; Binding from a binding-expr
	     (add-tcc-bindings expr conditions substs antes))
	    (t ;; We collect antes so we can form (A & B) => C rather than
	       ;; A => (B => C)
	     (add-tcc-conditions* expr (cdr conditions)
				    substs
				    (cons (car conditions) antes))))))


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
	(add-tcc-bindings expr (cdr conditions) substs antes
			  (if (or (occurs-in bd expr)
				  (occurs-in bd antes)
				  (occurs-in bd substs)
				  (possibly-empty-type? (type bd)))
			      (cons nbd bindings)
			      bindings)))
      ;; Now we can build the universal closure
      (let* ((nbody (substit (if antes
				 (make!-implication
				  (make!-conjunction* antes)
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
  (let ((nbindings (remove-if-not #'(lambda (ff)
				      (some #'(lambda (fff)
						(member fff bindings
							:test #'same-declaration))
					    (freevars ff)))
		     (freevars body))))
    (sort-freevars
     (append (remove-if #'(lambda (b)
			    (or (some #'(lambda (nb)
					  (same-declaration b nb))
				      nbindings)
				(not (possibly-empty-type? (type b)))))
	       bindings)
	     (mapcar #'declaration nbindings)))))


(defun insert-tcc-decl (kind expr type ndecl)
  (if (or *in-checker* *in-evaluator* *collecting-tccs*)
      (add-tcc-info kind expr type ndecl)
      (insert-tcc-decl1 kind expr type ndecl)))

(defun add-tcc-info (kind expr type ndecl)
  (push (make-tccinfo
	 :formula (definition ndecl)
	 :reason (cdr (assq expr *compatible-pred-reason*))
	 :kind kind
	 :expr expr
	 :type type)
	*tccforms*))

(defun insert-tcc-decl1 (kind expr type ndecl)
  (let ((*generate-tccs* 'none))
    (setf (tcc-disjuncts ndecl) (get-tcc-disjuncts ndecl))
    (let ((match (car (member ndecl *tccdecls* :test #'subsumes)))
	  (decl (declaration *current-context*)))
      (when (eq (spelling ndecl) 'OBLIGATION)
	(incf (total-tccs)))
      (cond
       ((and match
	     (not (and (declaration? decl)
		       (id decl)
		       (assq expr *compatible-pred-reason*))))
	(add-tcc-comment kind expr type ndecl match))
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
	  (cdr (assq expr *compatible-pred-reason*))
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
      (push ndecl (refers-to decl))
      (when *old-tcc-name*
	(push (cons (id ndecl) *old-tcc-name*) *old-tcc-names*))
      (pvs-output "~2%~a~%"
		  (let ((*no-comments* t)
			(*unparse-expanded* t))
		    (string-trim '(#\Space #\Tab #\Newline)
				 (unparse ndecl :string t))))
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
		       

(defun subsumes (tcc2 tcc1)
  (and (<= (length (tcc-disjuncts tcc1)) (length (tcc-disjuncts tcc2)))
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
	(incf (tccs-simplified)))))

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
	   (form (add-tcc-conditions relterm))
	   (uform (cond ((tcc-evaluates-to-true relterm form)
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
		 (or (car bindings)
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
	   (id (make-tcc-name)))
      (unless (tc-eq xform *true*)
	(when (and *false-tcc-error-flag*
		   (tc-eq xform *false*))
	  (type-error ordering
	    "TCC for this expression simplifies to false:~2%  ~a"
	    form))
	(typecheck* (mk-well-founded-tcc id xform) nil nil nil)))))

(defmethod well-founded-type? ((otype subtype))
  (or (and (name-expr? (predicate otype))
	   (eq (id (module-instance (predicate otype))) '|orders|)
	   (eq (id (declaration (predicate otype))) '|well_founded?|))
      (well-founded-type? (supertype otype))))

(defmethod well-founded-type? ((otype type-expr))
  nil)

(defun check-nonempty-type (type expr)
  (unless (or (nonempty? type)
	      (typep (declaration *current-context*) 'adt-accessor-decl)
	      (member type (nonempty-types (current-theory)) :test #'tc-eq))
    (when (possibly-empty-type? type)
      (generate-existence-tcc type expr)
      (unless (or (or *in-checker* *in-evaluator*)
		  *tcc-conditions*)
	(set-nonempty-type type)))))

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


;;; Assuming TCC generation

(defun generate-assuming-tccs (modinst expr)
  (let ((mod (get-theory modinst)))
    (unless (or (eq mod (current-theory))
		(null (assuming mod))
		(and (formals-sans-usings mod) (null (actuals modinst))))
      (let ((cdecl (or (and (or *in-checker* *in-evaluator*)
			    *top-proofstate*
			    (declaration *top-proofstate*))
		       (declaration *current-context*))))
	(unless (and (member modinst (assuming-instances (current-theory))
			     :test #'tc-eq)
		     (not (existence-tcc? cdecl)))
	  (let ((assumptions (remove-if-not #'assumption? (assuming mod))))
	    (check-assumption-subterm-visibility assumptions modinst)
	    ;; Don't want to save this module instance unless it does not
	    ;; depend on any conditions, including implicit ones in the
	    ;; prover
	    (unless (or (or *in-checker* *in-evaluator*)
			*tcc-conditions*)
	      (push modinst (assuming-instances (current-theory))))
	    (dolist (ass assumptions)
	      (if (eq (kind ass) 'existence)
		  (let ((atype (subst-mod-params (existence-tcc-type ass)
						 modinst)))
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
			(incf (tccs-simplified))))))))))))

(defun check-assumption-subterm-visibility (assumptions modinst)
  (dolist (ass assumptions)
    (let* ((subassdef (subst-mod-params (closed-definition ass) modinst))
	   (badobj (find-nonvisible-assuming-reference subassdef)))
      (when badobj
	(type-error badobj
	"Error: assumption refers to ~%  ~a,~%~
         which is not visible in the current theory"
	(full-name badobj 1))))))

(defun find-nonvisible-assuming-reference (subassdef)
  (let ((name nil))
    (mapobject #'(lambda (ex)
		   (or name
		       (when (nonvisible-assuming-reference? ex)
			 (setq name ex))))
	       subassdef)
    name))

(defmethod nonvisible-assuming-reference? ((ex name))
  (and (resolution ex)
       (let ((decl (declaration ex)))
	 (unless (eq (module decl) (current-theory))
	   (and (not (formal-decl? decl))
		(not (and (const-decl? decl)
			  (formal-subtype-decl? (generated-by decl))))
		(not (binding? decl))
		(not (skolem-const-decl? decl))
		(or (not (memq decl (gethash (id decl)
					     (current-declarations-hash))))
		    (let ((importings (gethash (module decl)
					       (current-using-hash))))
		      (not (or (some #'(lambda (imp) (null (actuals imp)))
				     importings)
			       (member (module-instance ex) importings
				       :test #'tc-eq))))))))))

(defmethod nonvisible-assuming-reference? (ex)
  (declare (ignore ex))
  nil)

(defmethod existence-tcc-type ((decl existence-tcc))
  (existence-tcc-type (definition decl)))

(defmethod existence-tcc-type ((ex application))
  (existence-tcc-type (args2 ex)))

(defmethod existence-tcc-type ((ex quant-expr))
  (type (car (bindings ex))))

(defun make-assuming-tcc-decl (ass modinst)
  (let* ((*generate-tccs* 'none)
	 (expr (subst-mod-params (definition ass) modinst))
	 (tform (add-tcc-conditions expr))
	 (uform (cond ((tcc-evaluates-to-true expr tform)
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
    (unless (or (eq mod (current-theory))
		(member modinst (assuming-instances (current-theory))
			:test #'tc-eq))
      ;; Don't want to save this module instance unless it does not
      ;; depend on any conditions, including implicit ones in the
      ;; prover
      (unless (or (or *in-checker* *in-evaluator*)
		  *tcc-conditions*)
	(push modinst (assuming-instances (current-theory))))
      (dolist (ax (remove-if-not #'axiom? (theory mod)))
	(let* ((*old-tcc-name* nil)
	       (ndecl (make-mapped-axiom-tcc-decl ax modinst)))
	  (if ndecl
	      (let ((refs (collect-references (definition ndecl))))
		(if (some #'(lambda (d)
			      (same-id (module d) modinst))
			  refs)
		    (pvs-warning "Axiom ~a not translated" (id ax))
		    (insert-tcc-decl 'mapped-axiom modinst ax ndecl)))
	      (incf (tccs-simplified))))))))

(defun make-mapped-axiom-tcc-decl (ax modinst)
  (let* ((*generate-tccs* 'none)
	 (*generating-mapped-axiom-tcc* t)
	 (expr (subst-mod-params (definition ax) modinst))
	 (tform (add-tcc-conditions expr))
	 (xform (if *simplify-tccs*
		    (pseudo-normalize tform)
		    (beta-reduce tform)))
	 (uform (expose-binding-types (universal-closure xform)))
	 (id (make-tcc-name nil (id ax))))
    (unless (tc-eq uform *true*)
      (when (and *false-tcc-error-flag*
		 (tc-eq uform *false*))
	(type-error ax
	  "Mapped axiom TCC for this expression simplifies to false:~2%  ~a"
	  tform))
      (typecheck* (mk-mapped-axiom-tcc id uform modinst ax) nil nil nil))))

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
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl number-judgement) expr extra-id)
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl name-judgement) expr extra-id)
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

(defmethod make-tcc-name* ((decl application-judgement) expr extra-id)
  (or (when (equal (cdr (assq expr *compatible-pred-reason*)) "judgement")
	(id decl))
      (call-next-method)))

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
	 (dimp (find-if #'(lambda (d)
			    (and (importing? d) (not (chain? d))))
		 decls))
	 (remimps (remove-if-not #'(lambda (d)
				     (and (importing? d)
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
		  vbd))
	expr)))

(defun make-new-variable (base expr &optional num)
  (let ((vid (if num (makesym "~a~d" base num) base)))
    (if (id-occurs-in vid expr)
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
	(unless (or *in-checker* *in-evaluator*)
	  (incf (tccs-simplified))))))

(defun make-actuals-tcc-decl (act mact)
  (let* ((*generate-tccs* 'none)
	 (conc (typecheck* (make-actuals-equality act mact)
			   *boolean* nil nil))
	 (form (add-tcc-conditions conc))
	 (uform (cond ((tcc-evaluates-to-true conc form)
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

(defun equality-predicates (t1 t2)
  (equality-predicates* t1 t2 nil nil nil))

(defmethod equality-predicates* :around (t1 t2 p1 p2 bindings)
  (if (tc-eq-with-bindings t1 t2 bindings)
      (make-equality-between-predicates t1 p1 p2)
      (call-next-method)))

(defun make-equality-between-predicates (t1 p1 p2)
  (when (or p1 p2)
    (let* ((vid (make-new-variable '|x| (append p1 p2)))
	   (bd (make-bind-decl vid t1))
	   (var (make-variable-expr bd)))
      (make!-forall-expr (list bd)
	(make!-iff
	  (make!-conjunction* (mapcar #'(lambda (p)
					  (make!-reduced-application p var))
				p1))
	  (make!-conjunction* (mapcar #'(lambda (p)
					  (make!-reduced-application p var))
				p2)))))))

(defmethod equality-predicates* ((t1 dep-binding) t2 p1 p2 bindings)
  (equality-predicates* (type t1) t2 p1 p2 bindings))

(defmethod equality-predicates* (t1 (t2 dep-binding) p1 p2 bindings)
  (equality-predicates* t1 (type t2) p1 p2 bindings))

(defmethod equality-predicates* ((t1 type-name) (t2 type-name) p1 p2 bindings)
  (assert (eq (declaration t1) (declaration t2)))
  (let ((npred (make-equality-between-predicates t1 p1 p2)))
    (equality-predicates-list (actuals (module-instance t1))
			      (actuals (module-instance t2))
			      npred bindings)))

(defmethod equality-predicates* ((t1 subtype) (t2 type-expr) p1 p2 bindings)
  (equality-predicates*
   (supertype t1) t2 (cons (predicate t1) p1) p2 bindings))

(defmethod equality-predicates* ((t1 type-expr) (t2 subtype) p1 p2 bindings)
  (equality-predicates*
   t1 (supertype t2) p1 (cons (predicate t2) p2) bindings))

(defmethod equality-predicates* ((t1 subtype) (t2 subtype) p1 p2 bindings)
  (cond ((subtype-of? t1 t2)
	 (equality-predicates* (supertype t1) t2
			       (cons (predicate t1) p1) p2
			       bindings))
	((subtype-of? t2 t1)
	 (equality-predicates* t1 (supertype t2)
			       p1 (cons (predicate t2) p2)
			       bindings))
	(t
	 (equality-predicates* (supertype t1) (supertype t2)
			       (cons (predicate t1) p1)
			       (cons (predicate t2) p2)
			       bindings))))

(defmethod equality-predicates* ((t1 funtype) (t2 funtype) p1 p2 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2)))
    (equality-predicates-list (list (domain t1) (range t1))
			      (list (domain t2) (range t2))
			      bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 tupletype) (t2 tupletype) p1 p2 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2)))
    (equality-predicates-list (types t1) (types t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 cotupletype) (t2 cotupletype) p1 p2 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2)))
    (equality-predicates-list (types t1) (types t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((t1 recordtype) (t2 recordtype) p1 p2
				 bindings)
  (let ((npred (make-equality-between-predicates t1 p1 p2)))
    (equality-predicates-list (fields t1) (fields t2) bindings
			      (when npred (list npred)))))

(defmethod equality-predicates* ((f1 field-decl) (f2 field-decl) p1 p2 binding)
  (assert (and (null p1) (null p2)))
  (equality-predicates* (type f1) (type f2) nil nil binding))

(defun equality-predicates-list (l1 l2 bindings preds)
  (if (null l1)
      (when preds
	(make!-conjunction* (nreverse preds)))
      (let ((npred (equality-predicates* (car l1) (car l2) nil nil bindings))
	    (nbindings (new-tc-eq-list-bindings (car l1) (car l2) bindings)))
	(equality-predicates-list
	 (substit (cdr l1) nbindings) (cdr l2) nbindings 
	 (if npred
	     (cons npred preds)
	     preds)))))

(defmethod equality-predicates* ((a1 actual) (a2 actual) p1 p2 bindings)
  (if (type-value a1)
      (equality-predicates* (type-value a1) (type-value a2) p1 p2 bindings)
      (let ((npred (equality-predicates* p1 p2 nil nil bindings)))
	(if npred
	    (make!-conjunction npred (make-equation (expr a1) (expr a2)))
	    (make-equation (expr a1) (expr a2))))))


(defun generate-cond-disjoint-tcc (expr conditions values)
  (let* ((*old-tcc-name* nil)
	 (ndecl (make-cond-disjoint-tcc expr conditions values)))
    (when ndecl
      (insert-tcc-decl 'disjointness expr nil ndecl)
      (unless (or *in-checker* *in-evaluator*)
	(incf (tccs-simplified))))))

(defun make-cond-disjoint-tcc (expr conditions values)
  (let* ((*generate-tccs* 'none)
	 (conc (make-disjoint-cond-property conditions values)))
    (when conc
      (let* ((*no-expected* nil)
	     (*bound-variables* *keep-unbound*)
	     (tform (add-tcc-conditions conc))
	     (uform (cond ((tcc-evaluates-to-true conc tform)
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
      (unless (or *in-checker* *in-evaluator*)
	(incf (tccs-simplified))))))

(defun make-cond-coverage-tcc (expr conditions)
  (let* ((*generate-tccs* 'none)
	 (conc (make!-disjunction* conditions))
	 (tform (add-tcc-conditions conc))
	 (uform (cond ((tcc-evaluates-to-true conc tform)
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

(defun add-tcc-comment (kind expr type ndecl subsumed-by)
  (declare (ignore ndecl))
  (unless (or *in-checker* *in-evaluator* *collecting-tccs*)
    (cond (subsumed-by
	   (incf (tccs-matched))
	   (push subsumed-by (refers-to (current-declaration))))
	  (t (if (numberp (tccs-simplified))
		 (incf (tccs-simplified))
		 (setf (tccs-simplified) 1))))
    (let* ((decl (current-declaration))
	   (submsg (case kind
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
		     (well-founded (format nil "for ~a" (id decl)))))
	   (place (or (place *set-type-actuals-name*)
		     (place expr) (place type)))
	   (plstr (when place
		    (format nil "(at line ~d, column ~d) "
		      (starting-row place) (starting-col place))))
	   (tccstr (format nil
		       "% The ~@[~a ~]~a TCC ~@[~a~]for~
                        ~:[ ~;~%    % ~]~a~@[~%    % ~a~]~%  ~a"
		     (cdr (assq expr *compatible-pred-reason*))
		     kind plstr
		     (> (+ (length (cdr (assq expr
					      *compatible-pred-reason*)))
			   (length (string kind))
			   (length plstr)
			   (length (unpindent (or *set-type-actuals-name*
						  expr type)
					      4 :string t :comment? t))
			   25)
			*default-char-width*)
		     (unpindent (or *set-type-actuals-name* expr type)
				4 :string t :comment? t)
		     submsg
		     (if subsumed-by
			 (format nil "% is subsumed by ~a"
			   (id subsumed-by))
			 "% was not generated because it simplified to TRUE."))))
      (pvs-info tccstr)
      (let* ((decl (current-declaration))
	     (theory (current-theory))
	     (entry (assq decl (tcc-comments theory))))
	(if entry
	    (nconc entry (list tccstr))
	    (push (list decl tccstr) (tcc-comments theory)))))))
