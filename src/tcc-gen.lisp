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

(defmacro total-tccs () '(car (tcc-info *current-theory*)))
(defmacro tccs-proved () '(cadr (tcc-info *current-theory*)))
(defmacro tccs-matched () '(caddr (tcc-info *current-theory*)))
(defmacro tccs-simplified () '(cadddr (tcc-info *current-theory*)))

(defstruct tccinfo
  formula
  reason
  kind
  expr
  type) 

(defun generate-subtype-tcc (expr expected incs)
  (if (every #'(lambda (i) (member i *tcc-conditions* :test #'tc-eq))
	     incs)
      (unless *in-checker*
	(if (numberp (tccs-simplified))
	    (incf (tccs-simplified))
	    (setf (tccs-simplified) 1)))
      (let ((ndecl (make-subtype-tcc-decl expr incs)))
	(if ndecl
	    (insert-tcc-decl 'subtype expr expected ndecl)
	    (unless *in-checker*
	      (incf (tccs-simplified)))))))

(defvar *simplify-tccs* nil)

(defun make-subtype-tcc-decl (expr incs)
  (assert (every #'type incs))
  (let* ((gen-tccs *generate-tccs*)
	 (*generate-tccs* 'none)
	 (conc (make!-conjunction* incs))
	 (tform (raise-actuals (add-tcc-conditions conc) nil))
	 (xform (if *simplify-tccs*
		    (pseudo-normalize (subst-var-for-recs
				       tform
				       (declaration *current-context*)))
		    (subst-var-for-recs
				  tform
				  (declaration *current-context*))))
	 (*no-expected* nil)
	 (uform (universal-closure xform))
	 (*bound-variables* *keep-unbound*)
	 (id (make-tcc-name)))
    (assert (tc-eq (type uform) *boolean*))
    (unless (tc-eq uform *true*)
      (when (and (not (eq gen-tccs 'all!))
		 (tc-eq uform *false*))
	(type-error expr "Subtype TCC for ~a simplifies to FALSE~@[:~2%  ~a~]"
		    expr (unless (tc-eq tform *false*) tform)))
      (typecheck* (mk-subtype-tcc id uform) nil nil nil))))

(defvar *substitute-let-bindings* nil)

(defun add-tcc-conditions (expr &optional
				(conditions (remove-duplicates *tcc-conditions*
							      :test #'equal))
				substs
				antes)
  (if (null conditions)
      (let ((ex (substit (if antes
			     (make!-implication
			      (make!-conjunction* antes) expr)
			     expr)
		  (self-substit substs))))
	(assert (type ex))
	ex)
      (cond ((consp (car conditions))
	     (add-tcc-conditions expr (cdr conditions)
				 (if *substitute-let-bindings*
				     (cons (car conditions) substs)
				     substs)
				 (if *substitute-let-bindings*
				     antes
				     (cons (make-equation
					     (mk-name-expr (caar conditions))
					     (cdar conditions))
					   antes))))
	    ((typep (car conditions) 'bind-decl)
	     (make-tcc-closure expr conditions substs antes))
	    (t (add-tcc-conditions expr (cdr conditions)
				   substs
				   (cons (car conditions) antes))))))

(defun make-tcc-closure (expr conditions substs antes &optional bindings)
  (if (typep (car conditions) 'bind-decl)
      (if (or (occurs-in (car conditions) expr)
	      (occurs-in (car conditions) bindings)
	      (occurs-in (car conditions) antes)
	      (occurs-in (car conditions) substs)
	      (possibly-empty-type? (type (car conditions))))
	  (make-tcc-closure expr (cdr conditions) substs antes
			    (cons (car conditions) bindings))
	  (make-tcc-closure expr (cdr conditions) substs antes
			    bindings))
      (let* ((nbody (substit (if antes
				 (make!-implication
				  (make!-conjunction* antes)
				  expr)
				 expr)
		      (self-substit substs)))
	     (nbindings (get-tcc-closure-bindings bindings nbody))
	     (nexpr (if nbindings
			(make-unique-typechecked-forall-expr nbindings nbody)
			nbody)))
	(add-tcc-conditions nexpr conditions nil nil))))

(defun make-unique-typechecked-forall-expr (bindings expr &optional
						     (nbindings bindings)
						     result)
  (if (null bindings)
      (make!-forall-expr (nreverse result) expr)
      (if (binding-id-is-bound (id (car bindings)) expr)
	  (let* ((nid (make-new-variable (id (car bindings)) expr 1))
		 (nbd (make-bind-decl nid (type (car nbindings))))
		 (nvar (make-variable-expr nbd))
		 (alist (acons (car bindings) nvar nil)))
	    (make-unique-typechecked-forall-expr
	     (cdr bindings)
	     (substit expr alist)
	     (substit (cdr nbindings) alist)
	     (cons nbd result)))
	  (make-unique-typechecked-forall-expr
	   (cdr bindings)
	   (substit expr (acons (car bindings) (car nbindings) nil))
	   (cdr nbindings)
	   (cons (car nbindings) result)))))

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

(defun get-tcc-closure-bindings (bindings body)
  (let ((nbindings (remove-if-not #'(lambda (ff)
				      (some #'(lambda (fff)
						(member fff bindings
							:test #'same-declaration))
					    (freevars ff)))
		     (freevars body))))
    (sort-freevars (append (remove-if #'(lambda (b)
					  (some #'(lambda (nb)
						    (same-declaration b nb))
						nbindings))
			     bindings)
			   (mapcar #'declaration nbindings)))))

(defun self-substit (substs &optional nsubsts (exprs (mapcar #'cdr substs)))
  (if (null substs)
      (nreverse nsubsts)
      (self-substit (cdr substs)
		    (cons (cons (caar substs) (car exprs)) nsubsts)
		    (substit (cdr exprs) (list (car substs))))))

;(defun add-tcc-conditions (expr &optional
;				(conditions *tcc-conditions*) antes)
;  (if (null conditions)
;      (if antes
;	  (mk-implication (mk-conjunction antes) expr)
;	  expr)
;      (if (consp (car conditions))
;	  (let ((lexpr ;;(mk-let-expr (car conditions)
;			 (if antes
;			     (substit (mk-implication (mk-conjunction antes) expr)
;			       (car conditions))
;			     (substit expr (car conditions))))
;		;;)
;		)
;	    (add-tcc-conditions lexpr (cdr conditions) nil))
;	  (add-tcc-conditions expr (cdr conditions)
;			      (cons (copy (car conditions)) antes)))))

(defun insert-tcc-decl (kind expr type ndecl)
  (if *in-checker*
      (add-tcc-info kind expr type ndecl)
      (insert-tcc-decl1 kind expr type ndecl)))

(defun add-tcc-info (kind expr type ndecl)
  (push (make-tccinfo
	 :formula (definition ndecl)
	 :reason (car *compatible-pred-reason*)
	 :kind kind
	 :expr expr
	 :type type)
	*tccforms*))

(defun insert-tcc-decl1 (kind expr type ndecl)
  (let ((*generate-tccs* 'none))
    (setf (tcc-disjuncts ndecl)
	  (if (binding-expr? (definition ndecl))
	      (cons (bindings (definition ndecl))
		    (simplify-disjunct (expression (definition ndecl))))
	      (cons nil (simplify-disjunct (definition ndecl)))))
    (let ((match (car (member ndecl *tccdecls* :test #'subsumes)))
	  (decl (declaration *current-context*)))
      (when (eq (spelling ndecl) 'OBLIGATION)
	(incf (total-tccs)))
      (cond
       (match
	(assert (declaration? match))
	(let ((msg (format nil
		       "~@(~a~) TCC for ~a subsumed by earlier TCC - not generated"
		     kind (or expr type))))
	  (when (eq *tcc-messages* 'yes)
	    (pvs-message msg))
	  (incf (tccs-matched))
	  ;;(add-comment decl msg)
	  (push match (refers-to decl))))
       (t (insert-tcc-decl* kind expr type decl ndecl))))))

(defun insert-tcc-decl* (kind expr type decl ndecl)
  (let ((submsg (case kind
		  (subtype (format nil "coercing ~a to ~a"
			     (unparse expr :string t) type))
		  (termination (format nil "for ~a" (id decl)))
		  (existence (format nil "for type ~a" type))
		  (assuming (format nil "for assumption ~a on instance ~a"
			      (id type) (unparse expr :string t)))
		  (cases (format nil "for missing ELSE of datatype ~a"
			   (id type)))
		  (well-founded (format nil "for ~a" (id decl))))))
    (when (and *typecheck-using*
	       (typep (declaration *current-context*) 'using))
      (setf (importing-instance ndecl)
	    (list *typecheck-using* *set-type-formal*)))
    (push (definition ndecl) *tccs*)
    (push ndecl *tccdecls*)
    (when *typechecking-module*
      (let* ((str (unpindent (or *set-type-actuals-name* expr type)
			     4 :string t :comment? t))
	     (place (or (place *set-type-actuals-name*)
			(place expr) (place type)))
	     (plstr (when place
		      (format nil "(at line ~d, column ~d) "
			(starting-row place) (starting-col place)))))
	(add-comment ndecl
	  "~@(~@[~a ~]~a~) TCC generated ~@[~a~]for~:[~;~%    %~]~a"
	  (car *compatible-pred-reason*)
	  kind
	  plstr
	  (> (+ (length (car *compatible-pred-reason*))
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
  (let* ((ndecl (make-recursive-tcc-decl name arguments)))
    (if ndecl
	(insert-tcc-decl 'termination name nil ndecl)
	(incf (tccs-simplified)))))

(defun make-recursive-tcc-decl (name arguments)
    (when (null arguments)
      (type-error name
	"Recursive definition occurrence ~a must have arguments" name))
    (let* ((gen-tccs *generate-tccs*)
	   (*generate-tccs* 'none)
	   (meas (measure (declaration *current-context*)))
	   (ordering (or (copy (ordering (declaration *current-context*)))
			 '<))
	   (appl1 (mk-recursive-application
		   meas
		   (outer-arguments (declaration *current-context*))))
	   (appl2 (mk-recursive-application
		   meas
		   arguments))
	   (form (add-tcc-conditions
		  (beta-reduce
		   (typecheck* (mk-application ordering appl2 appl1)
			       *boolean* nil nil))))
	   (xform (if (and *simplify-tccs*
			   (not *in-checker*))
		      (pseudo-normalize
		       (subst-var-for-recs form (declaration *current-context*)))
		      (beta-reduce
		       (subst-var-for-recs form (declaration *current-context*)))))
	   (uform (universal-closure xform))
	   (id (make-tcc-name)))
      (push name *recursive-tcc-names*)
      (unless (tc-eq uform *true*)
	(when (and (not (eq gen-tccs 'all!))
		   (tc-eq uform *false*))
	  (type-error name
	    "Termination TCC for this expression simplifies to false:~2%  ~a"
	    form))
	(typecheck* (mk-termination-tcc id uform) nil nil nil))))

(defun mk-recursive-application (op args)
  (if (null args)
      op
      (mk-recursive-application (mk-application* op (car args)) (cdr args))))

(defun outer-arguments (decl)
  (outer-arguments* (append (formals decl)
			    (bindings* (definition decl)))
		    (measure-depth decl)
		    (type (measure decl))))

(defmethod bindings* ((expr lambda-expr))
  (cons (bindings expr) (bindings* (expression expr))))

(defmethod bindings* (expr)
  nil)

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
  (let* ((ndecl (make-well-founded-tcc-decl decl mtype)))
    (if ndecl
	(insert-tcc-decl 'well-founded (ordering decl) nil ndecl))))

(defun make-well-founded-tcc-decl (decl mtype)
  (let* ((meas (measure decl))
	 (ordering (ordering decl))
	 (var1 (make-new-variable '|x| ordering))
	 (var2 (make-new-variable '|y| ordering))
	 (wfform (mk-lambda-expr (list (mk-bind-decl var1 mtype)
				       (mk-bind-decl var2 mtype))
		   (mk-application ordering
		     (mk-name-expr var1) (mk-name-expr var2))))
	 (form (typecheck* (mk-application '|well_founded?| wfform)
			   *boolean* nil nil))
	 (xform (if (and *simplify-tccs*
			 (not *in-checker*))
		    (pseudo-normalize form)
		    (beta-reduce form)))
	 (id (make-tcc-name)))
    (unless (tc-eq xform *true*)
      (when (and (not (eq *generate-tccs* 'all!))
		 (tc-eq xform *false*))
	(type-error ordering
	  "TCC for this expression simplifies to false:~2%  ~a"
	  form))
      (typecheck* (mk-well-founded-tcc id xform) nil nil nil))))

(defun check-nonempty-type (type expr)
  (unless (or (nonempty? type)
	      (typep (declaration *current-context*) 'adt-accessor-decl))
    (when (possibly-empty-type? type)
      (generate-existence-tcc type expr)
      (unless *in-checker*
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
  t)

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
;;	te (id *current-theory*))
;      (setf (nonempty? tdecl) t)))
;  nil)

(defun make-nonempty-assumption (type)
  (let* ((var (make-new-variable '|x| type))
	 (form (mk-exists-expr (list (mk-bind-decl var type)) *true*))
	 (tform (typecheck* form *boolean* nil nil))
	 (id (make-tcc-name))
	 (decl (typecheck* (mk-existence-tcc id tform) nil nil nil)))
    (setf (spelling decl) 'ASSUMPTION)
    (unless (member decl (assuming (module *current-context*))
		    :test #'(lambda (d1 d2)
			      (and (formula-decl? d2)
				   (eq (spelling d2) 'ASSUMPTION)
				   (tc-eq (definition d1) (definition d2)))))
      (setf (assuming (module *current-context*))
	    (append (assuming (module *current-context*)) (list decl))))
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
    (push te (nonempty-types *current-theory*))
    (setf (nonempty? te) t)
    (call-next-method)))

(defmethod set-nonempty-type ((te type-name))
  nil)

(defmethod set-nonempty-type ((te subtype))
  (set-nonempty-type (supertype te)))

(defmethod set-nonempty-type ((te tupletype))
  (set-nonempty-type (types te)))

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
  (let ((ndecl (make-existence-tcc-decl type fclass)))
    (insert-tcc-decl 'existence expr type ndecl)
    ndecl))

(defun make-existence-tcc-decl (type fclass)
  (let* ((*generate-tccs* 'none)
	 (stype (pc-parse (unparse (raise-actuals type nil) :string t)
		  'type-expr))
	 (var (make-new-variable '|x| type))
	 (form (mk-exists-expr (list (mk-bind-decl var stype)) *true*))
	 (tform (add-tcc-conditions (typecheck* form *boolean* nil nil)))
	 (uform (universal-closure tform))
	 (id (make-tcc-name)))
    (typecheck* (if (eq fclass 'obligation)
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
  (let ((mod (get-theory modinst))
	(cdecl (declaration *current-context*)))
    (unless (or (eq mod *current-theory*)
		(and (formals-sans-usings mod) (null (actuals modinst)))
		(and (member modinst (assuming-instances *current-theory*)
			     :test #'tc-eq)
		     (not (typep cdecl 'existence-tcc))))
      ;; Don't want to save this module instance unless it does not depend on
      ;; any conditions, including implicit ones in the prover
      (unless (or *in-checker*
		  *tcc-conditions*)
	(push modinst (assuming-instances *current-theory*)))
      (dolist (ass (remove-if-not #'(lambda (a)
				      (and (formula-decl? a)
					   (eq (spelling a) 'ASSUMPTION)))
		     (assuming mod)))
	(if (eq (kind ass) 'existence)
	    (let ((atype (subst-mod-params (existence-tcc-type ass) modinst)))
	      (if (typep cdecl 'existence-tcc)
		  (let ((dtype (existence-tcc-type cdecl)))
		    (if (tc-eq atype dtype)
			(generate-existence-tcc atype expr)
			(check-nonempty-type atype expr)))
		  (check-nonempty-type atype expr)))
	    (let ((ndecl (make-assuming-tcc-decl ass modinst)))
	      (if ndecl
		  (insert-tcc-decl 'assuming modinst ass ndecl)
		  (incf (tccs-simplified)))))))))

(defmethod existence-tcc-type ((decl existence-tcc))
  (existence-tcc-type (definition decl)))

(defmethod existence-tcc-type ((ex application))
  (existence-tcc-type (args2 ex)))

(defmethod existence-tcc-type ((ex quant-expr))
  (type (car (bindings ex))))

(defun make-assuming-tcc-decl (ass modinst)
  (let* ((gen-tccs *generate-tccs*)
	 (*generate-tccs* 'none)
	 (expr (subst-mod-params (definition ass) modinst))
	 (tform (add-tcc-conditions expr))
	 (xform (if *simplify-tccs*
		    (pseudo-normalize (subst-var-for-recs
				       tform
				       (declaration *current-context*)))
		    (beta-reduce (subst-var-for-recs
				  tform
				  (declaration *current-context*)))))
	 (uform (universal-closure xform))
	 (id (make-tcc-name)))
    (unless (tc-eq uform *true*)
      (when (and (not (eq gen-tccs 'all!))
		 (tc-eq uform *false*))
	(type-error ass
	  "Assuming TCC for this expression simplifies to false:~2%  ~a"
	  tform))
      (typecheck* (mk-assuming-tcc id uform modinst ass) nil nil nil))))

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
	 (xform (beta-reduce (subst-var-for-recs
			      tform
			      (declaration *current-context*))))
	 (uform (universal-closure xform))
	 (*bound-variables* nil)
	 (ndecl (typecheck* (mk-cases-tcc id uform) nil nil nil)))
    ;;(assert (tc-eq (type uform) *boolean*))
    (when *in-checker*
      (push uform *tccforms*))
    (insert-tcc-decl 'cases (expression expr) adt ndecl)))


(defun make-tcc-name (&optional (num 1))
  (let* ((decl-id (if (using? (declaration *current-context*))
		      (make-tcc-using-id)
		      (op-to-id
		       (or (if (declaration?
				(generated-by
				 (declaration *current-context*)))
			       (generated-by (declaration *current-context*))
			       (declaration *current-context*))))))
	 (tcc-name (makesym "~a_TCC~d" decl-id num)))
    (assert decl-id)
    (if (gethash tcc-name (local-decls *current-context*))
	(make-tcc-name (1+ num))
	tcc-name)))

(defmethod generating-judgement-tcc? ((decl number-judgement) expr)
  (id decl))

(defmethod generating-judgement-tcc? ((decl subtype-judgement) expr)
  (id decl))

(defmethod generating-judgement-tcc? ((decl name-judgement) expr)
  (and (id decl)
       (eq expr (name decl))))

(defmethod generating-judgement-tcc? ((decl name-judgement) (expr application))
  (and (id decl)
       (eq (operator* expr) (name decl))))

(defmethod generating-judgement-tcc? (decl expr)
  nil)


(defun make-tcc-using-id ()
  (let ((mod (module *current-context*)))
    (makesym "IMPORTING~d"
	     (1+ (position (declaration *current-context*)
			   (remove-if-not #'using?
					  (append (formals mod)
						  (assuming mod)
						  (theory mod))))))))

(defun subst-var-for-recs (expr recdecl)
  (if (and (def-decl? recdecl)
	   (type recdecl))
      (let* ((vid (make-new-variable '|v| expr))
	     (vname (make-new-variable-name-expr vid (type recdecl))))
	(gensubst expr
	  #'(lambda (x) (declare (ignore x)) (copy vname))
	  #'(lambda (x) (and (name-expr? x)
			     (eq (declaration x) recdecl)))))
      expr))

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
  (let ((ndecl (make-actuals-tcc-decl act mact)))
    (if ndecl
	(insert-tcc-decl 'actuals act nil ndecl)
	(unless *in-checker*
	  (incf (tccs-simplified))))))

(defun make-actuals-tcc-decl (act mact)
  (let* ((gen-tccs *generate-tccs*)
	 (*generate-tccs* 'none)
	 (conc (typecheck* (make-actuals-equality act mact)
			   *boolean* nil nil))
	 (form (add-tcc-conditions conc))
	 (uform (if *simplify-tccs*
		    (pseudo-normalize (universal-closure form))
		    (beta-reduce (universal-closure form))))
	 (id (make-tcc-name)))
    (unless (tc-eq uform *true*)
      (when (and (not (eq gen-tccs 'all!))
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
	   (bd (mk-bind-decl vid (or (print-type t1) t1) t1))
	   (var (mk-name-expr vid)))
      (mk-forall-expr (list bd)
	(mk-application 'IFF
	  (mk-conjunction (mapcar #'(lambda (p)
				      (mk-application p (copy var)))
				  p1))
	  (mk-conjunction (mapcar #'(lambda (p)
				      (mk-application p (copy var)))
				  p2)))))))

(defmethod equality-predicates* ((t1 dep-binding) t2 p1 p2 bindings)
  (equality-predicates* (type t1) t2 p1 p2 bindings))

(defmethod equality-predicates* (t1 (t2 dep-binding) p1 p2 bindings)
  (equality-predicates* t1 (type t2) p1 p2 bindings))

(defmethod equality-predicates* ((t1 type-name) (t2 type-name) p1 p2 bindings)
  (declare (ignore p1 p2 bindings))
  (break "Something's wrong with equality-predicates*"))

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
	(mk-conjunction preds))
      (let ((npred (equality-predicates* (car l1) (car l2) nil nil bindings)))
	(equality-predicates-list
	 (cdr l1) (cdr l2)
	 (new-tc-eq-list-bindings (car l1) (car l2) bindings)
	 (if npred
	     (cons npred preds)
	     preds)))))


(defun generate-cond-disjoint-tcc (expr conditions)
  (let ((ndecl (make-cond-disjoint-tcc expr conditions)))
    (when ndecl
      (insert-tcc-decl 'disjointness expr nil ndecl)
      (unless *in-checker*
	(incf (tccs-simplified))))))

(defun make-cond-disjoint-tcc (expr conditions values)
  (let* ((gen-tccs *generate-tccs*)
	 (*generate-tccs* 'none)
	 (conc (make-disjoint-cond-property conditions values)))
    (when conc
      (let* ((tform (raise-actuals (add-tcc-conditions conc)))
	     (xform (if *simplify-tccs*
			(pseudo-normalize (subst-var-for-recs
					   tform
					   (declaration *current-context*)))
			(subst-var-for-recs
			 tform
			 (declaration *current-context*))))
	     (*no-expected* nil)
	     (uform (universal-closure xform))
	     (*bound-variables* *keep-unbound*)
	     (id (make-tcc-name)))
	(assert (tc-eq (type uform) *boolean*))
	(unless (tc-eq conc *true*)
	  (when (and (not (eq gen-tccs 'all!))
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
  (let ((ndecl (make-cond-coverage-tcc expr conditions)))
    (when ndecl
      (insert-tcc-decl 'coverage expr nil ndecl)
      (unless *in-checker*
	(incf (tccs-simplified))))))

(defun make-cond-coverage-tcc (expr conditions)
  (let* ((gen-tccs *generate-tccs*)
	 (*generate-tccs* 'none)
	 (conc (make!-disjunction* conditions))
	 (tform (raise-actuals (add-tcc-conditions conc)))
	 (xform (if *simplify-tccs*
		    (pseudo-normalize (subst-var-for-recs
				       tform
				       (declaration *current-context*)))
		    (beta-reduce (subst-var-for-recs
				  tform
				  (declaration *current-context*)))))
	 (*no-expected* nil)
	 (uform (universal-closure xform))
	 (*bound-variables* *keep-unbound*)
	 (id (make-tcc-name)))
    (assert (tc-eq (type uform) *boolean*))
    (unless (tc-eq uform *true*)
      (when (and (not (eq gen-tccs 'all!))
		 (tc-eq uform *false*))
	(type-error expr
	  "Coverage TCC for this expression simplifies to false:~2%  ~a"
	  tform))
      (typecheck* (mk-cond-coverage-tcc id uform) nil nil nil))))
