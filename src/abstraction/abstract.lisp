;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abstract.lisp --
;; Author: N. Shankar 
;; Created On      : Feb 19, 2001
;; Last Modified By: N. Shankar
;; Last Modified On: Feb 2000
;; Status          : Unknown, Use with caution!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)


(defvar *abs-cache+* nil)
(defvar *abs-cache-* nil)
(defvar *need-feasible* nil)
(defvar *skolem-states* nil)
(defvar *strategy* nil)
(defvar *abs-verbose* nil)

(defstep abstract-and-mc
  (cstate astate amap &optional theories rewrites exclude
	  (strategy '(assert)) feasible verbose?)
  (try (abstract$ cstate astate amap theories rewrites exclude
		  strategy feasible verbose?)
       (musimp)
       (skip))
  "Invokes abstraction followed by symbolic mu-calculus model checking."
  "~%Abstracting from concrete state ~a
to abstract state ~a
using the abstraction map: ~a")


(defstep abstract (cstate astate amap &optional theories rewrites exclude
			  (strategy '(assert)) feasible verbose?)
  (let ((cuth *current-theory*)
	(cuthstr (string (id cuth))))
    (then  (install-rewrites$ :defs defs :theories theories
			      :rewrites rewrites :exclude exclude)
	   (auto-rewrite-theory cuthstr :always? t)
	   (auto-rewrite-theory "ctlops" :defs t :always? !!)
	   (auto-rewrite-theory "fairctlops" :defs t :always? !!)
	   (auto-rewrite-theory "Fairctlops" :defs t :always? !!)
	   (auto-rewrite "/=")
	   (auto-rewrite "K_conversion")
	   (stop-rewrite "mucalculus.mu" "mucalculus.nu" "Reachable.Reachable")
	   (rewrite-msg-off)
	   (assert)
	   (abs-simp cstate astate amap strategy feasible verbose?)
	   (simplify)))
  "Constructs an abstraction of the given sequent by mapping the
concrete state CSTATE to the abstract state ASTATE based on the abstraction
map AMAP.  CSTATE and ASTATE must be record types.  The field types
of ASTATE must be either booleans or scalars (for the time being).
THEORIES, REWRITES, and EXCLUDE are as in install-rewrites.
STRATEGY (defaults to ASSERT) is the proof strategy used to discharge abstraction proof obligations.  FEASIBLE is a predicate over ASTATE that identifies
the concretely feasible abstract states.  VERBOSE? when T prints out the
abstraction proof obligations. "
  "~%Abstracting from concrete state ~a
to abstract state ~a
using the abstraction map: ~a")


(addrule 'abs-simp (cstate astate amap) ((strategy '(assert)) feasible verbose?) 
	 (abstract-fun cstate astate amap feasible strategy verbose?)
	 "Builds an abstraction of the current sequent where AMAP is an
abstraction map between concrete state type CSTATE and abstract state type
ASTATE, FEASIBLE is a predicate characterizing the set of abstract states
that correspond to some concrete state, and STRATEGY is the proof strategy
to be used for discharging the abstraction proof obligations."
	 "~%Abstracting state type ~a to ~a")

;;abstract-fun implements the abs-simp rule by invoking
;;check-and-build-abstract. 
(defun abstract-fun (cstate astate amap feasible strategy verbose?)
  #'(lambda (ps)
      (let ((*abs-verbose* verbose?))
      (check-and-build-abstract ps cstate astate amap feasible strategy))))

;;abstract-type? checks the allowed abstract variable types, i.e.,
;;boolean and enum.  This could be expanded later on to allow all
;;mu-interpretable types but this would complicate the abstraction process
;;since enumeration over these types is hard. 
(defmethod abstract-type? ((type enumtype))
  t)
(defmethod abstract-type? ((type subtype))
  (or (sub-range? type)
      (abstract-type? (supertype type))))
(defmethod abstract-type? ((type t))
    (tc-eq type *boolean*))
(defmethod abstract-type? ((type adt-type-name))
  (abstract-type? (adt type)))

;;checks the field types of abstract state 
(defun check-abstract-type (astate)
  (let ((fields (fields astate)))
    (check-abstract-type* fields)))

(defun check-abstract-type* (fields)
  (if (consp fields)
      (if (abstract-type? (type (car fields)))
	  (check-abstract-type* (cdr fields))
	  (format t "~%Type ~a is not a scalar or a boolean."
	    (type (car fields))))
      t))

;;ensures that the fields shared between cfields and afields have
;;the same type.  
(defun check-common-fields (cfields afields)
  (if (consp cfields)
      (let* ((cf (car cfields))
	     (cfinaf (member cf afields :test #'same-id)))
	(if cfinaf
	    (if (tc-eq (type cf) (type (car cfinaf)))
		(check-common-fields (cdr cfields) afields)
		(format t "~%Common field: ~a has incompatible abstract and concrete types"
		  (id cf)))
	    (check-common-fields (cdr cfields) afields)))
      t))

;;checks that the abstraction map amap is type-correct w.r.t. the
;;cstate and astate.  
(defun check-abstraction-map (cstate astate amap)
  (if (null amap)
      (format t "~%Abstraction map is empty. ")
      (check-abstraction-map* cstate (fields astate) amap nil)))

(defun check-abstraction-map* (cstate afields amap outalist)
  (if (consp amap)
      (if (and (consp (car amap))(consp (cdar amap)))
	  (let* ((id (id (pc-parse (caar amap)  'name)))
		 (af (find id afields :test #'same-id)))
	    (if (null af)
		(format t "~%~a is not a field in the abstract state."  
		  id)
		(let* ((mtype (make-funtype cstate (type af)))
		       (amapfun (pc-parse (cadar amap) 'expr))
		       (atype (typecheck amapfun :expected mtype)))
		  (declare (ignore atype))
		  (if (lambda-expr?  amapfun)
		      (check-abstraction-map* cstate afields
					      (cdr amap)
					      (cons (cons id amapfun) outalist))
		      (format t "~%Abstraction map target ~a should be a lambda abstraction."
			amapfun)))))
	  (format t "~%Abstraction map entry ~a is not well-formed"
	    (car amap)))
      (check-for-missing-abstraction-map cstate afields amap outalist)))

(defun check-for-missing-abstraction-map (cstate afields amap outalist)
  (if (consp afields)
      (if (or (assoc (id (car afields)) outalist)
	      (find (id (car afields)) (fields cstate) :key #'id))
	  (check-for-missing-abstraction-map cstate (cdr afields)
					     amap outalist)
	  (format t "~%Abstract field ~a is not mapped in the abstraction map."
	    (id (car afields))))
      (amap-with-fields (nreverse outalist))))

;;checks cstate, astate, amap, feasible, and invokes build-abstract
;;to build abstract subgoal, and build-feasible to build generate the TCC
;;that the feasibility predicate is correct. 
(defun check-and-build-abstract (ps cstate astate amap feasible strategy)
  (let* ((cstate (typecheck (pc-parse cstate 'type-expr)))
	 (astate (typecheck (pc-parse astate 'type-expr)))
	 (feasible (when feasible (pc-parse feasible 'expr)))
	 (amap (check-abstraction-map cstate astate amap))
	 (*strategy* strategy)
	 (*need-feasible* nil)
	 (cfields (fields cstate))
	 (afields (fields astate)))
    (cond ((null (recordtype? cstate))
	   (format t "~%Concrete state type ~a is not a record type." cstate)
	   (values 'X nil nil))
	  ((null (recordtype? astate))
	   (format t "~%Abstract state type ~a is not a record type." astate)
	   (values 'X nil nil))
	  ((null (check-abstract-type astate))
	   (values 'X nil nil))
	  ((null (check-common-fields cfields afields))
	   (format t "~%Common concrete and abstract fields must have identical types.")
	   (values 'X nil nil))
	  ((null amap)
	   (values 'X nil nil))
	  (t 
	   (let* ((ftype (make-predtype astate))
		  (feasible (when feasible (typecheck feasible
					     :expected ftype)))
		  (cgoal (current-goal ps))
		  (new-context (copy-prover-context))
		  (abstract-subgoal
		   (build-abstract
		    cgoal
		    cstate astate amap
		    feasible new-context))
		  (feasible-subgoal-list
		   (when (or *need-feasible* feasible)
		     (list (build-feasible cgoal;;not defined yet
					   cstate astate amap
					   feasible)))))
	     (values '? (cons (list abstract-subgoal 'context new-context)
			      feasible-subgoal-list) nil))))))

;;builds the feasibility predicate validation subgoal to show that
;;there is a concrete state corresponding to each feasible abstract state.
(defun build-feasible (cgoal cstate astate amap feasible)
  (let* ((abnd (make-bind-decl (make-new-variable '|astate| astate)
		 astate))
	 (cbnd (make-bind-decl (make-new-variable '|cstate| cstate)
		 cstate))
	 (avar (make-variable-expr abnd))
	 (cvar (make-variable-expr cbnd))
	 (field-equalities
	  (loop for fld in (fields (find-supertype astate))
		collect
		(let* ((id (id fld))
		       (entry (assoc id amap))
		       (lhs (make-field-application (id fld) avar))
		       (rhs (if entry
				(make-application
				    (amap-entry-map (cdr (assoc (id fld) amap)))
				  cvar)
				(make-field-application (id fld) cvar))))
		  (make-equation lhs rhs))))
	 (feasible-app (when feasible (make-application feasible avar)))
	 (exists-conc (make-exists-expr (list cbnd)
			(make-conjunction field-equalities)))
	 (fmla (make-forall-expr (list abnd)
		 (if feasible
		     (make-implication
		      feasible-app
		      exists-conc)
		     exists-conc))))
    (if (freevars fmla) (break))
    (lcopy cgoal
      's-forms
      (cons (make-instance 's-formula 'formula fmla)(s-forms cgoal)))))

;;builds abstracted goal. 
(defun build-abstract (cgoal cstate astate amap feasible context)
  (let ((*current-context* context))
    (lcopy cgoal
      's-forms 
      (abstract-sforms (s-forms cgoal) cstate astate amap feasible))))

;;builds abstraction for each sform using abstract-formula
(defun abstract-sforms (sforms cstate astate amap feasible)
  (let ((*abs-cache+*
	 (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
	(*abs-cache-*
	 (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
	(*skolem-states* nil))
    (loop for sf in sforms
	  collect
	  (make-instance 's-formula
	    'formula
	    (abstract-formula (formula sf) nil
			      cstate astate
			      amap feasible)))))

;;These are redefined here 
(defun mu-nu-operator? (expr)
  (and (typep expr 'name-expr)
       (memq (id expr) '(|mu| |nu|))
       (eq (id (module (declaration expr))) '|mucalculus|)))


(defun any-mu-nu-expr? (expr)
  ;; /= from mu-nu-expr?. Without restriction to mu-translatable types
  (and (application? expr)
       (let ((op (operator expr)))
	 (mu-nu-operator? op))))

(defun any-mu-nu-expr-application? (expr) 
  ;; /= from mu-nu-expr-application?. Without restriction to
  ;; mu-translatable types
  (and (application? expr)
       (let ((op (operator expr)))
	 (any-mu-nu-expr? op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;abstracts a formula fmla with respect to polarity sign given
;;concrete state cstate, abstract state astate, abstraction map amap,
;;and feasibility predicate feasible. 
(defun abstract-formula (fmla sign cstate astate amap feasible)
  (let* ((skosymbs (collect-state-skosymbs fmla cstate))
	 (mu-map (loop for sk in skosymbs
		       collect (let ((skentry
				      (assoc sk *skolem-states*
					     :test #'same-declaration)))
				 (or skentry
				     (let ((abs-sk-pair
					    (cons sk (abstract-skolem
						      sk
						      (feasible-astate 
						       astate feasible)))))
				       (push abs-sk-pair
					     *skolem-states*)
				       abs-sk-pair)))))
	 (abs-fmla (abstract-formula* fmla sign cstate astate amap feasible
				      mu-map nil nil)))
    (assert (null (freevars abs-fmla)))
    abs-fmla))

;;A mu-atom? is a formula that is not a negation, conjunction,
;;disjunction, implication, iff, branch, state-quantification,
;;mu-application. 
(defmethod mu-atom? ((fmla negation) cstate)
  (and (mu-atom? (argument fmla) cstate)
       fmla))

(defmethod mu-atom? ((fmla conjunction) cstate)
  (declare (ignore cstate))
  nil)

(defmethod mu-atom? ((fmla disjunction) cstate)
  (declare (ignore cstate))
  nil)

(defmethod mu-atom? ((fmla implication) cstate) 
  (declare (ignore cstate))
  nil)

(defmethod mu-atom? ((fmla iff) cstate)
  (declare (ignore cstate))
  nil)

(defmethod mu-atom? ((fmla branch) cstate)
  (declare (ignore cstate))
  nil)

(defmethod mu-quant-expr? ((fmla quant-expr) cstate)
  (with-slots (bindings) fmla
    (loop for bnd in bindings
	  always (compatible? (type bnd) cstate))))

(defmethod mu-atom? ((fmla quant-expr) cstate)
  (not (mu-quant-expr? fmla cstate)))

(defmethod mu-atom? ((fmla application) cstate)
  (not (mu-application? fmla cstate)))

(defmethod mu-application? ((fmla application) cstate)
  (with-slots (operator argument) fmla
    (and (any-mu-nu-expr? operator)
	 (loop for arg in (arguments fmla)
	       always (and (compatible? (type arg) cstate)
			   (or (variable? arg)
			       (skolem-constant? arg)))))))

;;For future expansion: calls next method right now.
(defmethod abstract-formula* :around (fmla sign cstate astate amap
				      feasible  mu-map subst
				      atoms)
  (declare (ignore cstate astate amap feasible mu-map subst))	   
  (let* ((abs-cache (if sign *abs-cache+* *abs-cache-*))
	 (hashlist (gethash fmla abs-cache))
	 (hashentry (assoc atoms hashlist :test #'tc-eq)))
    (if hashentry
	(cdr hashentry)
	(let ((result (call-next-method)))
	  (setf (gethash fmla abs-cache)
		(cons (cons atoms result) hashlist))
	  result))))


;;abstract-formula* collects conjunctive atoms as it works its way inside
;;a formula.  This way, the abstraction has the same structure as the
;;concrete, there is no need to convert the formula into a normal form,
;;and yet the context is suitably strengthened in the proof obligations.
;;mu-map holds the correspondence between the concrete state
;;variables/skolem constants and the abstract ones. 
(defmethod abstract-formula* ((fmla list) sign cstate astate amap feasible
			      mu-map subst atoms)
  (if (consp fmla)
      (cons (abstract-formula* (car fmla) sign cstate astate amap feasible
			       mu-map subst atoms)
	    (abstract-formula* (cdr fmla) sign cstate astate amap feasible
			       mu-map subst atoms))
      nil))
	    

(defmethod abstract-formula* ((fmla negation) sign cstate astate amap feasible
			   mu-map subst atoms)
  (if sign
      (let ((conjuncts (conjuncts fmla)))
	(abstract-flattened conjuncts sign cstate astate amap feasible
			     mu-map subst atoms))
      (negate (abstract-formula* (argument fmla) (not sign) cstate astate amap
				 feasible  mu-map subst atoms))))


;;If sign is T, then the conjuncts are flattened using abstract-flattened.
(defmethod abstract-formula* ((fmla conjunction) sign cstate astate amap
			      feasible  mu-map subst atoms)
  (if sign
      (let ((conjuncts (conjuncts fmla)))
	(abstract-flattened conjuncts sign cstate astate amap feasible
			    mu-map subst atoms))      
      (make-conjunction
       (abstract-formula* (arguments fmla) sign cstate astate amap
			  feasible  mu-map subst atoms))))

(defmethod flatten-equality ((fmla equation) cstate)
  (let* ((arg1 (args1 fmla))
	 (arg2 (args2 fmla))
	 (argtype (find-supertype (type arg1))))
    (if (and (recordtype? argtype)
	     (compatible? argtype cstate))
	(loop for fld in (fields argtype)
	      collect
	      (make-equality (beta-reduce (make!-field-application
					   (id fld) arg1))
			     (beta-reduce (make!-field-application
					   (id fld) arg2))))
	(call-next-method))))

(defmethod flatten-equality ((fmla t) cstate)
  (declare (ignore cstate))
  (list fmla))

;;abstract-flattened separates out the mu-atoms and the non-mu-atoms,
;;accumulates the atoms and invokes abstract-nonatomic on the non-atoms.  
(defun abstract-flattened (conjuncts sign cstate astate amap
				     feasible  mu-map subst atoms)
  (let* ((mu-atoms (loop for c in conjuncts
			 when (mu-atom? c cstate)
			 nconc (flatten-equality c cstate)))
	 (mu-rest (loop for c in conjuncts
			when (not (mu-atom? c cstate))
			collect c))
	 (atoms (nconc mu-atoms atoms)))
    (if mu-rest
	(make-conjunction
	 (abstract-nonatomic mu-rest sign cstate astate amap
			     feasible  mu-map subst
			     atoms))
	(abstract-conjunction atoms cstate astate amap mu-map subst))))

;;abstract-nonatomic invokes abstract-formula* back again.  
(defmethod abstract-nonatomic ((fmla list) sign cstate astate amap
			       feasible  mu-map subst atoms)
  (if (consp fmla)
      (cons (abstract-nonatomic (car fmla) sign cstate astate amap feasible
			        mu-map subst atoms)
	    (abstract-nonatomic (cdr fmla) sign cstate astate amap feasible
			        mu-map subst atoms))
      nil))

(defmethod abstract-nonatomic ((fmla negation) sign cstate astate amap
			       feasible  mu-map subst atoms)
  (negate (abstract-formula* (negate fmla) (not sign) cstate astate amap
			     feasible  mu-map subst atoms)))

(defmethod abstract-nonatomic ((fmla t) sign cstate astate amap
			       feasible  mu-map subst atoms)
  (abstract-formula*  fmla sign cstate astate amap
		      feasible  mu-map subst atoms))


(defmethod abstract-formula* ((fmla disjunction) sign cstate astate amap
			      feasible  mu-map subst atoms)
  (make-disjunction
   (abstract-formula* (arguments fmla) sign cstate astate amap
		      feasible  mu-map subst atoms)))

(defmethod abstract-formula* ((fmla implication) sign cstate astate amap feasible
			      mu-map subst atoms)
  (make-implication
   (abstract-formula* (args1 fmla) (not sign) cstate astate amap
		      feasible  mu-map subst atoms)
   (abstract-formula* (args2 fmla)  sign cstate astate amap
		      feasible  mu-map subst atoms)))

(defmethod abstract-formula* ((fmla iff)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (make-conjunction
   (list 
    (make-implication
     (abstract-formula* (args1 fmla) (not sign) cstate astate amap
			feasible mu-map subst atoms)
     (abstract-formula* (args2 fmla)  sign cstate astate amap
			feasible mu-map subst atoms))
    (make-implication
     (abstract-formula* (args2 fmla) (not sign) cstate astate amap
			feasible mu-map subst atoms)
     (abstract-formula* (args1 fmla)  sign cstate astate amap
			feasible mu-map subst atoms)))))

(defun compatible-cstate-bindings? (bindings cstate)
  (loop for bnd in bindings
	    always (compatible? (type bnd) cstate)))

(defmethod abstract-formula* ((fmla quant-expr)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (with-slots (bindings expression) fmla
    (cond
     ((compatible-cstate-bindings? bindings cstate)
      (when (or (and (null sign) (exists-expr? fmla))
		(and sign (forall-expr? fmla)))
	(setq *need-feasible* t))
      (let* ((abs-bindings
	      (abstract-bindings bindings astate cstate feasible))
	     (vars-for-abs-bindings
	      (loop for bd in abs-bindings
		    collect (make-variable-expr bd)))
	     (sko-bindings (loop for bind in bindings
				 collect (abstract-skolem bind (type bind)))))
	(copy fmla
	  'bindings abs-bindings
	  'expression
	  (abstract-formula*  expression
				    
			      sign cstate astate amap
			      feasible 
			      (append (pairlis bindings
					       vars-for-abs-bindings)
				      mu-map)
			      (append (pairlis bindings sko-bindings) subst) atoms))))
     (t (abstract-atom  fmla sign cstate astate amap mu-map subst atoms)))))

(defmethod mu-predicate? ((fmla application) cstate)
  (with-slots (operator argument) fmla
    (and (mu-nu-operator? operator)
	 (lambda-expr? argument)
	 (compatible? (domain (find-supertype (type fmla))) cstate))))

(defmethod mu-predicate? ((fmla t) cstate)
  (declare (ignore cstate))
  nil)

(defmethod abstract-formula* ((fmla equation)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (declare (ignore feasible sign))
  (let* ((arg1 (args1 fmla))
	 (arg2 (args2 fmla))
	 (argtype (find-supertype (type arg1))))
    (if (recordtype? argtype)
	(let ((comp-equalities
	       (loop for fld in (fields argtype)
		     collect
		     (make-equality (beta-reduce (make!-field-application
						  (id fld) arg1))
				    (beta-reduce (make!-field-application
						  (id fld) arg2))))))
	  (abstract-conjunction (nconc comp-equalities atoms)
				cstate astate amap mu-map subst))
	(call-next-method))))
  
(defmethod abstract-formula* ((fmla application)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (with-slots (operator argument) fmla
    (if (mu-predicate? fmla cstate)
	(let ((mu-nu (typecheck
			 (mk-name-expr (id operator)
			   (list (mk-actual astate))
			   (id (module (declaration operator)))))))
	  (make-application mu-nu
	    (abstract-formula* argument sign cstate astate amap feasible
			       mu-map subst atoms)))
	(if (mu-application? fmla cstate)
	    (make-application*
		(abstract-formula* operator sign cstate astate amap
				   feasible  mu-map subst atoms)
	      (abstract-args (arguments fmla) mu-map))
	    (abstract-atom fmla sign cstate astate amap mu-map subst atoms)))))

(defmethod abstract-formula* ((fmla lambda-expr)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  ;;must be a mu-lambda or this call would not occur.
  (with-slots (bindings expression) fmla
    (let* ((new-bindings (abstract-bindings bindings astate cstate nil))
					;no feasible needed
	   (sko-bindings (loop for bind in bindings
			       collect (abstract-skolem bind (type bind))))
	   (vars-for-new-bindings
	    (loop for bd in new-bindings
		  collect (make-variable-expr bd)))
	   (mu-map (append (pairlis bindings vars-for-new-bindings)
			   mu-map))
	   (subst (append (pairlis bindings sko-bindings) subst)))
      (make-lambda-expr new-bindings
	(abstract-formula* expression
			   sign cstate astate amap
			   feasible  mu-map subst atoms)))))

(defmethod abstract-formula* ((fmla branch)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (make-disjunction
   (list (abstract-formula* (make-conjunction (list (condition fmla)
						    (then-part fmla)))
			    sign cstate astate amap feasible
			    mu-map subst atoms)
	 (abstract-formula* (make-conjunction (list (negate (condition fmla))
						    (else-part fmla)))
			    sign cstate astate amap feasible
			    mu-map subst atoms))))

(defmethod abstract-formula* ((fmla t)
			      sign cstate astate amap feasible
			      mu-map subst atoms)
  (declare (ignore feasible))
  (abstract-atom fmla sign cstate astate amap mu-map subst atoms))

(defun abstract-args (args mu-map)
  (loop for arg in args
	collect
	(cond ((variable? arg);;it must be in mu-map
	       (cdr (assoc arg mu-map :test #'same-declaration))) 
	      (t			;it must be a cached skolem state constant
	       (let ((cached-symbol (assoc arg *skolem-states*
					   :test #'same-declaration)))
		 (cdr cached-symbol))))))

(defun abstract-skolem (arg astate)
  (let ((abs-arg-id (gen-symbol (id arg) #\! *skofun-counter*)))
    (if (or (declared? abs-arg-id *current-context*)
	    (loop for (nil . y) in *skolem-states*
		  thereis (same-id y abs-arg-id)))
	(abstract-skolem arg astate)
	(makeskoconst  (pc-parse abs-arg-id 'expr) astate *current-context*))))

(defun conc-predtype? (type cstate)
  (and (predtype? type)
       (compatible?  (domain type) cstate)))

(defun feasible-astate (astate feasible)
  (if feasible
      (mk-setsubtype astate feasible)
      astate))

(defun abstract-bindings (bindings astate cstate feasible)
  (loop for bnd in bindings
	collect (let ((new-bnd (copy bnd
				 'type (if (conc-predtype? (type bnd) cstate)
					   (make-predtype astate)
					   (feasible-astate astate feasible))
				 'declared-type nil)))
		  (setf (resolutions new-bnd)
			(list (copy (resolution new-bnd)
				'declaration new-bnd
				'type nil)))
		  new-bnd)))


;;abstract-atom is where the hard work of abstracting a mu-atom fmla
;;starts.
;;if sign is T, then the over-approximator abstract-over-atom is invoked.
;;Otherwise, the negation is over-approximated and negated. 
(defun abstract-atom (fmla sign cstate astate amap
			   mu-map subst atoms)
  (if sign
      (abstract-over-atom fmla cstate astate amap
			  mu-map subst atoms)
      (negate (abstract-over-atom (negate fmla) cstate astate amap
				  mu-map subst atoms))))

;;abstract-over-atom invokes abstract-conjunction with the context atoms
;;in atoms.  
(defun abstract-over-atom (fmla cstate astate amap
				mu-map subst atoms)
;  (format t "~%Over-approximating ~a ~%given ~{~a~%~}" fmla atoms)  
  (abstract-conjunction (cons fmla atoms)
			cstate astate amap mu-map subst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;free-fields gets the free fields in a formula
(defun lambda-free-fields (lambda-expr)
  (let* ((state (type (car (bindings lambda-expr))))
	 (ff (free-fields (expression lambda-expr) state)))
    (loop for fld in ff collect (id fld))))

(defun free-fields (expr state)
  (free-fields* expr state nil))

(defmethod free-fields* ((expr name-expr) state accum)
  (if (compatible? (find-supertype (type expr)) state)
      (union
       (loop for fl in (fields (find-supertype state))
	     collect (make-field-application fl expr))
       accum :test #'tc-eq)
      accum))

(defmethod free-fields* ((expr application) state accum)
  (with-slots (operator argument) expr
    (free-fields* operator state
		  (free-fields* argument state accum))))

(defmethod free-fields* ((expr tuple-expr) state accum)
  (free-fields* (exprs expr) state accum))

(defmethod free-fields* ((expr record-expr) state accum)
  (free-fields* (mapcar #'expression (assignments expr))
		state accum))

(defmethod free-fields* ((expr binding-expr) state accum)
  (with-slots (bindings expression) expr
    (let ((ff (free-fields* expression
			    state
			    (free-fields* (mapcar #'type bindings)
					  state accum))))
      (loop for fld in ff		;remove bound var fields
	    when (not (intersection (freevars fld) bindings
				    :test #'same-declaration))
	    collect fld))))

(defmethod free-fields* ((expr update-expr) state accum)
  (with-slots (expression assignments) expr
    (free-fields* assignments
		  state
		  (free-fields* expression state accum))))

(defmethod free-fields* ((expr assignment) state accum)
  (with-slots (arguments expression) expr
    (free-fields* expression state (free-fields* arguments state accum))))

(defmethod free-fields* ((expr field-name-expr) state accum)
  (declare (ignore state))
  accum)

;;reminder: check that exprs. and amaps are well-formed so that
;;this is the only way state is used.  
(defmethod free-fields* ((expr field-application) state accum)
  (with-slots (id argument) expr
    (if (and (compatible? (type argument) state)
	     (or (variable? argument)
		 (skolem-constant? argument)))
	(pushnew expr accum :test #'tc-eq)
	accum)))

(defmethod free-fields* ((expr projection-application) state accum)
  (free-fields* (argument expr) state accum))

(defmethod free-fields* ((expr injection-application) state accum)
  (free-fields* (argument expr) state accum))

(defmethod free-fields* ((expr list) state accum)
  (if (consp expr)
      (free-fields* (cdr expr) state (free-fields* (car expr) state accum))
      accum))

(defmethod free-fields* ((expr t) state accum)
  (declare (ignore state))
  accum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;partition-amap returns a list of list of fields .  It might be
;;;worth implementing sets of fields as bit-vectors.
(defstruct amap-entry map fieldnames)

(defun amap-with-fields (amap)
  (loop for (x . y) in amap
	collect (cons x (make-amap-entry :map y
					 :fieldnames
					 (lambda-free-fields y)))))

(defun partition-amap (amap)
  (if (consp amap)
      (merge-partition (amap-entry-fieldnames (cdar amap))
		       (partition-amap (cdr amap)))
      nil))

(defun merge-partition (fields partition)
  (let* ((overlaps (loop for set in partition
			 when (intersection fields set)
			 collect set))
	 (rest (set-difference partition overlaps))
	 (set-overlaps (loop for set in overlaps
			     append set)))
    (cons (union fields set-overlaps)
	  rest)))

(defun state-skosymb? (symb state)
  (and (skolem-constant? symb)
       (compatible? (type symb) state)))


(defun collect-state-skosymbs (term state)
  (collect-terms #'(lambda (tm) (state-skosymb? tm state)) term))


(defun state-constant? (symb state)
  (and (constant? symb)
       (compatible? (type symb) state)))

(defun collect-state-constants (term state)
  (collect-terms #'(lambda (tm) (state-constant? tm state)) term))

;;specializes an amap-partition to a specific state variable. 
(defun apply-partition (partition symb)
  (loop for fnamelist in partition
	collect (loop for fname in fnamelist
		      collect (make-field-application fname symb))))

(defun partition-conjunction (conjunction amap cstate)
  (let* ((freevars-conj (freevars conjunction))
	 (skosymbs-conj (collect-state-constants conjunction cstate))
	 (statevars-conj (loop for fvar in freevars-conj
			       when (compatible? (type fvar) cstate)
			       collect fvar))
	 (state-symbs (nconc statevars-conj skosymbs-conj))
	 (amap-partition (partition-amap amap))
	 (init-var-partition 
	  (loop for symb in state-symbs
		nconc (apply-partition amap-partition symb))))
    (multiple-value-bind
	(predconjs partition)
	(partition-conjunction* conjunction cstate init-var-partition)
      (format *abs-verbose*
	  "~%Partitioning conjunct ~a ~%yields recursive calls ~a and partition ~a"
	conjunction predconjs partition)
      (values predconjs partition))))

;;Extracts applications of mu-bound variables. 
(defun extract-pred-conjuncts (conjuncts cstate)
  (extract-pred-conjuncts* conjuncts cstate nil nil))

(defmethod mu-pred-application? ((expr application) cstate)
  (with-slots (operator) expr
    (and (variable? operator)
	 (compatible? (domain (find-supertype (type operator)))
		      cstate))))

(defmethod mu-pred-application? ((expr t) cstate)
  (declare (ignore cstate))
  nil)

(defun extract-pred-conjuncts* (conjuncts cstate predconjs rest)
  (if (consp conjuncts)
      (if  (or (mu-pred-application? (car conjuncts) cstate)
	       (and (negation? (car conjuncts))
		    (mu-pred-application? (argument (car conjuncts))
					  cstate)))
	   (extract-pred-conjuncts* (cdr conjuncts) cstate
				    (cons (car conjuncts) predconjs)
				    rest)
	   (extract-pred-conjuncts* (cdr conjuncts) cstate
				    predconjs
				    (cons (car conjuncts) 
					  rest)))
      (values predconjs rest)))

(defun partition-conjunction* (conjunction cstate partition)
  (multiple-value-bind (predconjs rest)
      (extract-pred-conjuncts conjunction cstate)
    (values predconjs 
	    (partition-conjunction-rec rest cstate partition nil))))

(defun partition-conjunction-rec (conjuncts cstate map-partition
					    formula-partition)
  (if (consp conjuncts)
      (let* ((fmla (car conjuncts))
	     (fmla-fields (free-fields fmla cstate))
	     (overlapping-fields
	      (loop for fields in map-partition
		    when (intersection fields fmla-fields :test #'tc-eq)
		    append fields))
	     (both-fields (union fmla-fields overlapping-fields :test #'tc-eq)))
	(partition-conjunction-rec (cdr conjuncts) cstate
				   map-partition
				   (merge-formula-partition
				    fmla both-fields formula-partition)))
      formula-partition))

(defstruct form-field forms fields)

(defun merge-formula-partition (fmla fields formula-partition)
  (let* ((all-matches (loop for fmfld in formula-partition
			    when (intersection (form-field-fields fmfld)
					       fields :test #'tc-eq)
			    collect fmfld))
	 (non-matches (set-difference formula-partition
				      all-matches))
	 (merged-formulas (loop for fmfld in all-matches
				append (form-field-forms fmfld)))
	 (merged-fields (loop for fmfld in all-matches
			      append (form-field-fields fmfld)))
	 (new-fmla-partition
	  (cons (make-form-field :forms (cons fmla merged-formulas)
				 :fields (union fields merged-fields))
		non-matches)))
    new-fmla-partition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	      
;;abstract-conjunction abstracts a conjunction for a given abstraction map
;;amap and a correspondence mu-map.  First the conjunction is
;;partitioned.  The mu-predicates are abstracted by substitution, and
;;the each partition is abstracted by abstract-partition.  Note that
;;distinct partitions have no relevant abstract fields in common.  
(defun abstract-conjunction (conjunction cstate astate
				amap  mu-map subst)
  (multiple-value-bind
      (predconjs partition)
      (partition-conjunction conjunction amap cstate)
    (let ((abs-predconjs
	   (loop for conj in predconjs
		collect  (termsubst conj
				    (conc-abs-subst mu-map
						    #'assoc-same-decl?)
				    (conc-abs-test mu-map #'assoc-same-decl?))))
	  (abs-partition (loop for fmfld in partition
			      append
			      (abstract-partition (form-field-forms fmfld)
						  (form-field-fields fmfld)
						  cstate astate
						  amap mu-map subst))))
      (make-conjunction (nconc abs-predconjs
			       (mapcar #'make-disjunction abs-partition))))))

;;take the atoms in fields and enumerating all combinations of increasing
;;length and collecting all successful ones.
;;0. collect all the conjuncts that directly imply a literal, and remove
;;these literals and anything they subsume. 
;;1. take the conjunction of the formulas (incl. those from 0)
;;2. Get all the literals corresponding to booleans and scalar fields
;;3. Keep two scores: successful disjunctions S and pending disjunctions P
;;4. Each time a conjunction C succeeds, filter out any subsumed
;;disjunctions from P and add C to S.  Each time a conjunction C fails, we
;;extend it with all possible literals and add those that are not subsumed
;;by S to P.  
;;
;;   so that it is not subsumed by a successful formula

(defun set-equal (x y &key test key)
  (null (set-exclusive-or x y :test test :key key)))

(defun set-tc-eq (x y)
  (set-equal x y :test #'tc-eq))

(defun build-abstract-fields (fields amap mu-map accum)
  (if (consp fields)
      (let* ((fld (car fields))
	     (id (id fld))
	     (abs-state (cdr (assoc (argument fld) mu-map
				    :test #'same-declaration)))
	     (relevant-afields
	      (loop for entry in amap
		    when (member id
				 (amap-entry-fieldnames (cdr entry)))
		    collect (car entry)))
	     (relevant-afields (or relevant-afields
				   (and (member id
						(fields (find-supertype (type abs-state)))
						:key #'id)
					(list id))))
	     (new-accum
	      (if relevant-afields
		  (add-afields relevant-afields abs-state accum)
		  accum))) 
	(build-abstract-fields (cdr fields) amap mu-map new-accum))
      accum))

(defun add-afields (afields abs-state accum)
  (if (consp afields)
      (add-afields (cdr afields) abs-state
		   (pushnew (make-field-application (car afields) abs-state)
			    accum :test #'tc-eq))
      accum))

(defun maximal-sets (sets &key test)
  (maximal-sets* sets nil test))

(defun not-subset-in (x sets test)
  (if (null sets)
      t
      (and (not (subsetp x (car sets) :test test))
	   (not-subset-in x (cdr sets) test))))

	  

(defun maximal-sets* (sets accum test)
  (if (null sets)
      accum
      (if (and
	   (not-subset-in (car sets) accum test)
	   (not-subset-in (car sets) (cdr sets) test))
	  (maximal-sets* (cdr sets) (cons (car sets) accum) test)
	  (maximal-sets* (cdr sets) accum test))))

;;abstract-partition takes a partition consisting of a list of formulas
;;and its free fields.  It finds the maximal sets amoung the sets of free
;;fields, and abstracts each maximal set separately but using all the
;;formulas as the context.  The idea is that even if there is a large
;;overlap, even one extra field can double the amount of work so it is
;;better to partition the work into the maximal sets.  subst precomputes
;;the substitutions of bindings and their skolemized forms, since only the
;;latter will be used in the proof obligations.  
(defun abstract-partition (formulas fields cstate astate amap
				    mu-map subst)
  (let* ((conj-cfields (loop for fmla in formulas
			     collect (cons fmla (free-fields fmla cstate))))
	 (cfield-sets (loop for (nil . y) in conj-cfields
			    collect y))
	 (cfield-sets (maximal-sets cfield-sets :test #'tc-eq))
	 (absvars (build-abstract-fields  fields
					  amap mu-map nil)))
    (and absvars
	 (abstract-sorted-conjuncts conj-cfields cfield-sets absvars
				    formulas
				    astate amap mu-map subst))))

(defun extract-mu-predicate-vars (conjunct-fields pred-accum rest-accum)
  (if (consp conjunct-fields)
      (let ((cf (car conjunct-fields)))
	(if (and (application? (car cf))
		 (variable? (operator (car cf))))
	    (extract-mu-predicate-vars (cdr conjunct-fields)
				       (cons cf pred-accum)
				       rest-accum)
	    (extract-mu-predicate-vars (cdr conjunct-fields)
				       pred-accum
				       (cons cf rest-accum))))
      (values pred-accum rest-accum)))

;;abstract-sorted-conjuncts invokes abstract-sorted-conjuncts* to make
;;sure that the accumulator contains all the successful disjunctions so
;;that it can use them to eliminate subsumed proof obligations across all
;;the maximal sets in field-sets.
(defun abstract-sorted-conjuncts (conjunct-fields field-sets absvars
						  formulas astate
						  amap mu-map subst)
  (abstract-sorted-conjuncts* conjunct-fields field-sets absvars formulas
			      astate
			      amap mu-map subst nil))

(defun assoc-same-decl? (x y)
  (and (name-expr? x)(same-declaration x y)))

(defun conc-abs-test (abs-conc-map testfn &optional rassoc?)
  (if rassoc?
      #'(lambda (x) (rassoc x abs-conc-map :test testfn))
      #'(lambda (x) (assoc x abs-conc-map :test testfn))))
  
(defun conc-abs-subst (abs-conc-map testfn &optional rassoc?)
    (if rassoc?
      #'(lambda (x)(let ((res (rassoc x abs-conc-map :test testfn)))
		     (if res (car res) x)))
      #'(lambda (x)(let ((res (assoc x abs-conc-map :test testfn)))
		     (if res (cdr res) x)))))

(defun abstract-sorted-conjuncts* (conjunct-fields field-sets absvars
						   formulas astate
						   amap mu-map subst accum)
  (if (consp field-sets)
      (let* ((set1 (car field-sets));;take first partition set
	     (abs-conc-map
	      (make-abs-conc-map set1 absvars astate amap mu-map))) 
	(if (every #'(lambda (x)(null (assoc (id (car x)) amap)))
		   abs-conc-map);;all unabstracted vars
	    (let ((abs-conj1 
		   (if (loop for x in set1 
			     always (assoc (id x) abs-conc-map :key #'id))
		       (loop for (cn . flds) in  conjunct-fields
			     when (subsetp flds set1 :test #'tc-eq)
			     collect
			     (list (termsubst cn
					      (conc-abs-subst abs-conc-map
							      #'tc-eq T)
					      (conc-abs-test abs-conc-map
							     #'tc-eq T))))
		       nil)));;has an unabstracted variable
	      (abstract-sorted-conjuncts* conjunct-fields
					  (cdr field-sets) absvars
					  formulas astate
					  amap mu-map subst
					  (nconc abs-conj1
						 accum)))
	    (let ((new-accum
		   (enumerate-and-filter
		    abs-conc-map (substit formulas subst) subst accum)))
	      (abstract-sorted-conjuncts*  conjunct-fields
					   (cdr field-sets) absvars
					   formulas
					   astate
					   amap mu-map subst new-accum))))
      accum))

(defun make-abs-conc-map (field-set absvars astate amap mu-map)
  (loop for ff in field-set
	nconc
	(let* ((id (id ff))
	       (abs-ids (loop for af in amap
			      when (member id
					   (amap-entry-fieldnames
					    (cdr af)))
			      collect (car af)))
	       (abs-ids (if (and (member id (fields astate) :key #'id)
				 (null (assoc id amap)))
			    (cons id abs-ids)
			    abs-ids))
	       (abs-state
		(cdr (assoc (argument ff)
			    mu-map :test #'same-declaration)))
	       (cf-absvars
		(loop for av in absvars
		      when (and (member (id av) abs-ids)
				(tc-eq (argument av)
				       abs-state))
		      collect av))
	       (cf-absvars
		(remove-duplicates cf-absvars :test #'tc-eq))
	       (abs-conc-map
		(make-field-abs-conc-map cf-absvars amap ff)))
	  abs-conc-map)))

(defun make-field-abs-conc-map (cf-absvars amap ff)
  (loop for av in cf-absvars
	collect (let ((av-amap-entry
		       (assoc (id av) amap)))
		  (if av-amap-entry
		      (cons av
			    (make-application
				(amap-entry-map (cdr av-amap-entry))
			      (argument ff)))
		      (cons av
			    (make-field-application
			     (id av)(argument ff)))))))

(defun sformify (x)
  (make-instance 's-formula 'formula x))

(defun make-conc-entry (x y subst)
  (let ((atype (find-supertype (type x)))
	(cterm (substit y subst)))
    (if (tc-eq atype *boolean*)
	(cons x (list (cons (sformify cterm) x)
		      (cons (sformify (negate cterm)) (negate x))))
	(let* ((recs (recognizers atype)))
	  (cons x 
		(loop for rec in recs
		      collect (cons (sformify
				     (negate
				      (make-application
					  rec
					cterm)))
				    (negate (make-application
						rec x)))))))))

;;with abstract-to-concrete map abs-conc-map, abstract conjuncts conj. 
(defun enumerate-and-filter (abs-conc-map conj subst accum)
  (let* ((nconj-sforms (loop for cnj in conj collect
			     (make-instance 's-formula 'formula (negate cnj))))
	 (abs-conc-alist
	  (loop for (x . y) in abs-conc-map
		collect (make-conc-entry x y subst)))
			    
	 (result (enumerate-and-filter* abs-conc-alist nconj-sforms nil 
				        accum 0)))
    (format t "~%Abstracted ~a to ~%  ~a" conj result)
    result))

(defun enumerate-and-filter* (abs-conc-alist nconj-sforms fail accum n)
  (if (or (and (> n 0)(null fail))(>= n (length abs-conc-alist)))
      accum
      (enumerate-and-filter+ abs-conc-alist abs-conc-alist
			     nconj-sforms fail nil accum n)))

(defun abs-key (entry)
  (if (application? entry)
      (abs-key (args1 entry))
      entry))

(defun list-earlier (x y list key)
  (< (position x list :key key)
     (position y list :key key)))

(defun ordsubsetp (x y)
  (if (consp x)
      (if (consp y)
	  (if (eq (car x)(car y))
	      (ordsubsetp (cdr x) (cdr y))
	      (ordsubsetp x (cdr y)))
	  nil)
      t))

(defun check-against-accum (ab fail-abs accum)
  (if (null accum)
      t
      (if (eq ab (abs-key (caar accum)))
	  (and  (not (ordsubsetp (cdar accum) fail-abs))
		(check-against-accum ab fail-abs (cdr accum)))
	  (check-against-accum ab fail-abs (cdr accum)))))

		
(defun abs-not-subsumed (ab fail-pair abs-conc-alist accum)
  (and (let ((abpair1 (caar fail-pair)))
	 (list-earlier ab (abs-key abpair1)
		  abs-conc-alist #'car))
       (check-against-accum ab (car fail-pair) accum)))

      
;;Starting with abs-conc-alist, enumerate and test all the disjunctions.
(defun enumerate-and-filter+ (tmp-alist abs-conc-alist nconj-sforms fail
					next-fail accum n) 
  (if (null tmp-alist)
      (enumerate-and-filter* abs-conc-alist nconj-sforms next-fail accum (1+ n))
      (let ((ab (caar tmp-alist)))
	(if (eql n 0);;then fail is empty
	    (if (check-against-accum ab nil accum);;if (ab) not already in accum
		(enumerate-and-filter0 ab nil tmp-alist abs-conc-alist
				       nconj-sforms fail next-fail accum n)
		(enumerate-and-filter+ (cdr tmp-alist) abs-conc-alist nconj-sforms fail
				       next-fail accum n))
	    (let ((abfails (loop for pair in fail
				 when (abs-not-subsumed ab pair abs-conc-alist accum)
				 collect  pair)))
	      (enumerate-and-filter0 ab abfails tmp-alist abs-conc-alist
				     nconj-sforms fail next-fail accum n))))))

(defun enumerate-and-filter0 (ab abfails tmp-alist abs-conc-alist
				 nconj-sforms fail next-fail accum n)
  (if (null abfails)
      (if (eql n 0)
	  (let ((ab-entry (cdr (assoc ab abs-conc-alist))))
	    (multiple-value-bind (wins losses)
		(test-abstract-point ab-entry nil nil nconj-sforms)
	      (enumerate-and-filter+ (cdr tmp-alist) abs-conc-alist
				     nconj-sforms
				     fail (nconc losses next-fail)
				     (nconc wins accum) n)))
	  (enumerate-and-filter+ (cdr tmp-alist) abs-conc-alist nconj-sforms
				 fail next-fail accum n))
      (let ((ab-entry (cdr (assoc ab abs-conc-alist)))
	    (fail-abs (car (car abfails)))
	    (fail-conc (cdr (car abfails))))
	(multiple-value-bind (wins losses)
	    (test-abstract-point ab-entry
				 fail-abs fail-conc
				 nconj-sforms)
	  (enumerate-and-filter0 ab (cdr abfails) tmp-alist
				 abs-conc-alist nconj-sforms
				 fail (nconc losses next-fail)
				 (nconc wins accum) n)))))

;;ab-entry is a list of concrete-abstract literal pairs, fail-abs is a previously
;;failed abstract disjunction, fail-conc is its concrete counterpart,
;;new-fail accumulates the failed abstract disjunctions.
(defun or-test-abstract-point (ab-entry fail-conc fail-abs nconj-sforms
					new-fail)
  (if (null ab-entry)
      (values nil new-fail)
      (let* ((conc (caar ab-entry))
	     (ab (cdar ab-entry))
	     (ab-goal (cons ab fail-abs))
	     (seq-sf (cons conc fail-conc))
	     (goal (make-instance 'sequent
		     's-forms (append nconj-sforms seq-sf)))
	     (res (test-sequent goal)))
	(if res
	    (values (list ab-goal) nil)
	    (or-test-abstract-point (cdr ab-entry) fail-conc fail-abs
				    nconj-sforms (cons (cons ab-goal
							     seq-sf)
						       new-fail))))))

;;Tests all entries in ab-entry disjoined with fail-abs, and accumulate the
;;successful disjunctions in new-accum, and the failed abstract/concrete
;;disjunction pairs in new-fail.  For data abstraction, if there is only a
;;single failure, since the tests are already negated, we can negate the literal,
;;concatenate it to the remaining disjuncts and report that as a success.  
(defun and-test-abstract-point (ab-entry fail-conc fail-abs nconj-sforms
					 new-accum
					 new-fail)
  (if (null ab-entry)
      (if (singleton? new-fail)
	  (values  (list (list (negate (caaar new-fail))))
		  nil)
	  (values  new-accum new-fail))
      (let* ((conc (caar ab-entry))
	     (ab (cdar ab-entry))
	     (ab-goal (cons ab fail-abs))
	     (seq-sf (cons conc fail-conc))
	     (goal (make-instance 'sequent
		     's-forms (append nconj-sforms seq-sf)))
	     (res (test-sequent goal)))
	(if res
	    (and-test-abstract-point (cdr ab-entry) fail-conc fail-abs
				     nconj-sforms (cons ab-goal new-accum)
				     new-fail)
	    (and-test-abstract-point (cdr ab-entry) fail-conc fail-abs
				     nconj-sforms new-accum
				     (cons (cons ab-goal seq-sf)
					   new-fail))))))

(defun test-sequent (sequent)
  (let ((dummy (when *abs-verbose* (format t "~%Proving ~a" sequent)))
	(result (simplify-sequent sequent *ps* *strategy* t)))
    (declare (ignore dummy))
    (cond ((null result);;(proved? proof-obligation )
	   (when *abs-verbose* (format t "~%Sequent proved!"))
	   t)
	  (t (when *abs-verbose* (format t "~%Sequent unproved.")) nil))))

;;use or-testing for boolean abstractions, and and-testing for data abstractions.
(defun test-abstract-point (ab-entry fail-abs fail-conc  nconj-sforms)
  (if (eql (length ab-entry) 2)
      (or-test-abstract-point ab-entry fail-conc fail-abs nconj-sforms nil)
      (and-test-abstract-point ab-entry fail-conc fail-abs nconj-sforms nil nil))) 
	 

(defun simplify-sequent (sequent ps step time?)
  (let* ((*generate-tccs* 'none)
	 (strat (let ((*in-apply* ps))	
	  (strat-eval* step ps))))
    (cond ((or (typep strat 'strategy)
	       (typep strat 'rule-instance))
	   (let* ((new-strat (if (typep strat 'strategy)
				 strat
				 (make-instance 'strategy
				   'topstep strat)))
		  (newps0 (copy ps
			    'current-goal sequent
			    'strategy new-strat
			    'parent-proofstate nil))
		  (newps (if (typep newps0 'top-proofstate)
			     (change-class newps0 'proofstate)
			     newps0))		  
		  (*noninteractivemode* t)
		  (*suppress-printing* t)
		  (*dependent-decls* nil)
		  (init-time (get-internal-run-time))
		  (result (let ((*in-apply* ps))
			    (prove* newps)))
		  (end-time (/ (- (get-internal-run-time) init-time)
			       internal-time-units-per-second)))
	     (declare (ignore result))
	     (let* ((subgoals (collect-subgoals newps)))
	       (when (and time? *abs-verbose*)
		 (format t "~%;;;Used ~,2F seconds in ~s." end-time step))
	       subgoals)))
	  (t (list sequent)))))
      
