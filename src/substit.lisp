;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; substit.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Oct 27 00:15:26 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 16:54:32 1998
;; Update Count    : 6
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002-2004 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(export '(substit make-new-bindings substit-binding))

;;; NOTE: Many attempts have been made to use hash tables to speed up this
;;; function, and for particular examples it may look like a win, but it
;;; often makes things slower.  The real slowness in this function is
;;; in the calls to pseudo-normalize, which are needed in subtypes,
;;; actuals, and type-applications.

;;; Here we are using hash tables, but we now make sure that substit* never
;;; directly calls substit.  Keep in mind that pseudo-normalize can make
;;; calls to substit.

(defvar *needs-pseudo-normalizing* nil)

(defvar *alist-freevars*)
(defvar *alist-boundvars*)
(defvar *substit-hash*)

(defun substit (obj alist)
  ;; At some point, should verify that car of every element of the
  ;; alist is a declaration.
  #+pvsdebug
  (assert (every #'(lambda (a)
		     (and (typep (car a)
				 '(or simple-decl declaration))
			  (eq (declaration (car a)) (car a))))
		 alist))
  (let* ((*alist-freevars* 'unbound)
	 (*alist-boundvars* 'unbound)
	 (*substit-hash* (make-hash-table :test #'eq))
	 (fvars (freevars obj))
	 (nalist (remove-if
		     (complement
		      #'(lambda (a)
			  (and (not (eq (car a) (cdr a)))
			       (member (declaration (car a)) fvars
				       :key #'declaration :test #'eq))))
		   alist)))
    (if (null nalist)
	obj
	(substit* obj nalist))))

(defmethod substit* :around ((expr expr) alist)
  (declare (ignore alist))
  (if (null (freevars expr))
      expr
      (or (gethash expr *substit-hash*)
	  (setf (gethash expr *substit-hash*) (call-next-method)))))

(defmethod substit* :around ((expr type-expr) alist)
  (if (freevars expr)
      (if *subst-type-hash*
	  (let ((result (lookup-subst-hash expr alist *subst-type-hash*)))
	    (if result result
		(let ((result (call-next-method)))
		  (install-subst-hash expr alist result *subst-type-hash*)
		  result)))
	  (or (gethash expr *substit-hash*)
	      (setf (gethash expr *substit-hash*) (call-next-method))))
      expr))

(defun lookup-subst-hash (expr alist hash)
  (gethash (cons expr
		 (pick-freevars-entries (freevars expr) alist))
	   hash))

(defun install-subst-hash (expr alist result hash)
  (let* ((fv (freevars expr))
	 (fv-subs (pick-freevars-entries fv alist)))
    (setf (gethash (cons expr fv-subs) hash)
	  result)
    t))

(defun pick-freevars-entries (freevars alist &optional entries)
  (if (null freevars)
      (nreverse entries)
      (let ((entry (assq (declaration (car freevars)) alist)))
	(pick-freevars-entries (cdr freevars) alist
			       (if entry
				   (cons (cdr entry) entries)
				   entries)))))


;(defmethod substit* :around (expr alist)
;  (if (null (freevars expr)) ;; (substit-possible? expr alist)
;      expr
;      (call-next-method)))

(defun substit-possible? (expr alist)
  (substit-possible*? (freevars expr) alist))

(defun substit-possible*? (freevars alist)
  (when freevars
    (or (assq (declaration (car freevars)) alist)
	(substit-possible*? (cdr freevars) alist))))

(defmethod substit* ((expr name-expr) alist)
  (with-slots (resolutions type actuals) expr
    (let* ((decl (declaration (car resolutions)))
	   (binding (assq decl alist)))
      (cond ((null binding)
	     (let ((res (substit* resolutions alist))
		   (ntype (substit* type alist)))
	       (if (eq res resolutions)
		   (unless (eq ntype type)
		     (setq res (list (copy (car res) 'type ntype))))
		   (setf (type (car res)) ntype))
	       (lcopy expr
		 'type ntype
		 'actuals (substit* actuals alist)
		 'resolutions res)))
	    ((typep (cdr binding) 'binding)
	     (if (eq (car binding) (cdr binding))
		 expr
		 (let ((nex (if (typep (cdr binding) 'field-decl)
				(change-class (copy (cdr binding))
					      'field-name-expr)
				(change-class (copy (cdr binding))
					      'name-expr))))
		   (setf (parens nex) 0)
		   (setf (resolutions nex)
			 (list (mk-resolution (cdr binding)
				 (current-theory-name)
				 (type (cdr binding)))))
		   nex)))
	    (t (unless (name-expr? (cdr binding))
		 ;; It is now possible that pseudo-normalize will actually
		 ;; change things.
		 (setq *needs-pseudo-normalizing* t))
	       (cdr binding))))))

(defmethod substit* ((expr adt-name-expr) alist)
  (let ((nex (call-next-method)))
    (if (eq nex expr)
	nex
	(copy nex 'adt-type (substit* (adt-type expr) alist)))))

(defmethod substit* ((expr projection-expr) alist)
  (with-slots (actuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr injection-expr) alist)
  (with-slots (actuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr injection?-expr) alist)
  (with-slots (actuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr extraction-expr) alist)
  (with-slots (actuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr projection-application) alist)
  (with-slots (argument actuals type index) expr
    (let ((narg (substit* argument alist)))
      (cond ((and (not *substit-dont-simplify*)
		  (tuple-expr? narg))
	     (nth (1- index) (exprs narg)))
	    ((eq argument narg)
	     expr)
	    (t (let ((ntype (substit* (type expr) alist)))
		 (lcopy expr
		   'argument narg
		   'actuals (substit* actuals alist)
		   'type ntype)))))))

(defmethod substit* ((expr injection-application) alist)
  (with-slots (argument actuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'type ntype))))

(defmethod substit* ((expr injection?-application) alist)
  (with-slots (argument actuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'type ntype))))

(defmethod substit* ((expr extraction-application) alist)
  (with-slots (argument actuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'type ntype))))

(defmethod substit* ((expr field-application) alist)
  (with-slots (id argument type) expr
    (let ((narg (substit* argument alist)))
      (cond ((and (not *substit-dont-simplify*)
		  (record-expr? narg))
	     (let ((ass (find id (assignments narg)
			      :key #'(lambda (a) (id (caar (arguments a)))))))
	       (assert ass)
	       (expression ass)))
	    ((eq argument narg)
	     expr)
	    (t (let ((ntype (substit* type alist)))
		 (copy expr 'argument narg 'type ntype)))))))

(defmethod substit* ((expr resolution) alist)
  (let ((new-modinst (substit* (module-instance expr) alist)))
    (if (eq new-modinst (module-instance expr))
	expr
	(mk-resolution (declaration expr) new-modinst
		       (substit* (type expr) alist)))))

(defmethod substit* ((expr modname) alist)
  (lcopy expr 'actuals (substit* (actuals expr) alist)))


(defmethod substit* ((act actual) alist)
  (with-slots (expr type-value) act
    (let ((ntype (when type-value
		   (substit* type-value alist))))
      (lcopy act
	'expr (cond ((and ntype (eq ntype type-value))
		     expr)
		    (type-value ntype)
		    (t (let* ((*needs-pseudo-normalizing* nil)
			      (nexpr (substit* expr alist)))
			 (if t ;*needs-pseudo-normalizing*
			     (pseudo-normalize nexpr)
			     nexpr))))
	'type-value ntype))))

;(defmethod substit* ( (expr if-expr) alist) ;;NSH(7-30)get rid
;  (copy expr
;	'arguments
;	(substit* (arguments expr) alist)))
;    'condition (substit*  (condition expr) alist)
;    'then-part (substit* (then-part expr) alist)
;    'else-part (substit*  (else-part expr) alist)))


(defmethod substit* ((expr application) alist)
  (with-slots (operator argument) expr
    (let ((op (substit* operator alist))
	  (arg (substit* argument alist)))
      (cond ((and (not *substit-dont-simplify*)
		  (lambda-expr? op)
		  (not (let-expr? expr)))
	     (make!-reduced-application* op arg))
	    ((and (eq op operator)
		  (eq arg argument))
	     expr)
	    ((typep op '(or projection-expr injection-expr injection?-expr
			    extraction-expr))
	     (typecase op
	       (projection-expr
		(make!-projection-application (index op) arg (actuals op)))
	       (injection-expr
		(make!-injection-application
		 (index op) arg (range (type op)) (actuals op)))
	       (injection?-expr
		(make!-injection?-application (index op) arg (actuals op)))
	       (extraction-expr
		(make!-extraction-application (index op) arg (actuals op)))))
	    (t (let* ((stype (find-supertype (type op)))
		      (nex (copy expr
			     'operator op
			     'argument arg
			     'type (if (typep (domain stype) 'dep-binding)
				       (let ((*alist-freevars* 'unbound)
					     (*alist-boundvars* 'unbound)
					     (*substit-hash*
					      (make-hash-table :test #'eq)))
					 (substit* (range stype)
						   (acons (domain stype)
							  arg nil)))
				       (range stype)))))
		 ;; Note: the copy :around (application) method takes care of
		 ;; changing the class if it is needed.
		 nex))))))


(defmethod make!-reduced-application* ((op lambda-expr) (arg tuple-expr))
  (if (singleton? (bindings op))
      (call-next-method)
      (let ((*alist-freevars* 'unbound)
	    (*alist-boundvars* 'unbound)
	    (*substit-hash* (make-hash-table :test #'eq)))
	(substit* (expression op)
		  (pairlis (bindings op) (exprs arg))))))

(defmethod make!-reduced-application* ((op lambda-expr) arg)
  (let ((*alist-freevars* 'unbound)
	(*alist-boundvars* 'unbound)
	(*substit-hash* (make-hash-table :test #'eq)))
    (if (singleton? (bindings op))
	(substit* (expression op)
		  (acons (car (bindings op)) arg nil))
	(substit* (expression op)
		  (pairlis (bindings op) (make!-projections arg))))))

(defmethod substit* ((expr equation) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let* ((result (call-next-method)))
	(if (equation? result)
	    (let* ((arg1 (args1 result))
		   (arg2 (args2 result)))
	      (if (tc-eq arg1 arg2)
		  *true*
		  (if (iff-or-boolean-equation? result)
		      (if (tc-eq arg1 *true*)
			  arg2
			  (if (tc-eq arg2 *true*)
			      arg1
			      result))
		      result)))
	    result))))


(defmethod substit* ((expr conjunction) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (conjunction? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *true*)
		  arg2
		  (if (tc-eq arg2 *true*)
		      arg1
		      (if (or (tc-eq arg1 *false*)
			      (tc-eq arg2 *false*))
			  *false*
			  result))))
	    result))))

(defmethod substit* ((expr disjunction) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (disjunction? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *false*)
		  arg2
		  (if (tc-eq arg2 *false*)
		      arg1
		      (if (or (tc-eq arg1 *true*)
			      (tc-eq arg2 *true*))
			  *true*
			  result))))
	    result))))

(defmethod substit* ((expr implication) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (implication? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *true*)
		  arg2
		  (if (or (tc-eq arg1 *false*)
			  (tc-eq arg2 *true*))
		      *true*
		      (if (tc-eq arg2 *false*)
			  (negate! arg1)
			  result))))
	    result))))

(defmethod substit* ((expr negation) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (negation? result)
	    (let ((arg (argument result)))
	      (if (tc-eq arg *true*)
		  *false*
		  (if (tc-eq arg *false*)
		      *true*
		      result)))
	    result))))

(defmethod substit* ((expr branch) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (branch? result)
	    (let ((cond (condition result))
		  (then (then-part result))
		  (else (else-part result)))
	      (if (or (tc-eq cond *true*)
		      (tc-eq then else))
		  then
		  (if (tc-eq cond *false*)
		      else
		      result)))
	    result))))

(defmethod change-to-infix-appl? ((expr infix-application))
  nil)

(defmethod change-to-infix-appl? ((expr application))
  (with-slots (operator argument) expr
    (and (infix-op? operator)
	 (= (the fixnum (arg-length argument)) 2))))

(defmethod infix-op? (expr)
  (declare (ignore expr))
  nil)

(defmethod infix-op? ((expr name-expr))
  (with-slots (id) expr
    (memq id *infix-operators*)))

(defmethod arg-length (expr)
  (declare (ignore expr))
  1)

(defmethod arg-length ((expr tuple-expr))
  (with-slots (exprs) expr
    (length exprs)))

(defmethod substit* :around ((expr table-expr) alist)
  (let ((nexpr (call-next-method)))
    (if (eq expr nexpr)
	expr
	(if (table-expr? nexpr)
	    (lcopy nexpr
	      'row-expr (substit* (row-expr nexpr) alist)
	      'col-expr (substit* (col-expr nexpr) alist)
	      'row-headings (substit* (row-headings nexpr) alist)
	      'col-headings (substit* (col-headings nexpr) alist)
	      'table-entries (substit* (table-entries nexpr) alist))
	    nexpr))))

(defmethod substit* ((expr record-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr tuple-expr) alist)
  (let ((nexprs (substit*-simple-list (exprs expr) alist)))
    (if (eq nexprs (exprs expr))
	expr
	(let ((ntype (if (every #'(lambda (nex ex) (eq (type nex) (type ex)))
				nexprs (exprs expr))
			 (type expr)
			 (mk-tupletype (mapcar #'type nexprs)))))
	  (copy expr
	    'exprs nexprs
	    'type ntype)))))

(defun substit*-simple-list (list alist)
  (let ((slist (substit*-simple-list* list alist nil)))
    (if (equal list slist) list slist)))

(defun substit*-simple-list* (list alist result)
  (if (null list)
      (nreverse result)
      (substit*-simple-list* (cdr list) alist
			     (cons (substit* (car list) alist) result))))

(defmethod substit* ((expr update-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'expression (substit* (expression expr) alist)
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr assignment) alist)
  (lcopy expr
    'arguments (substit* (arguments expr) alist)
    'expression (substit* (expression expr) alist)))

(defmethod substit* ((expr list) alist)
  (let ((nlist (substit*-list expr alist nil)))
    (if (equal nlist expr) expr nlist)))

(defun substit*-list (expr alist result)
  (cond ((null expr)
	 (nreverse result))
	((binding? (car expr))
	 (let ((newcar (substit-binding* (car expr) alist)))
	   (cond ((eq newcar (car expr))
		  (substit*-list (cdr expr) alist (cons newcar result)))
		 (t (let ((*alist-freevars* 'unbound)
			  (*alist-boundvars* 'unbound)
			  (*substit-hash* (make-hash-table :test #'eq)))
		      (substit*-list (cdr expr)
				     (acons (car expr) newcar alist)
				     (cons newcar result)))))))
	(t (substit*-list (cdr expr) alist
			  (cons (substit* (car expr) alist)
				result)))))

(defun substit-binding (expr alist)
  (let ((*alist-freevars* 'unbound)
	(*alist-boundvars* 'unbound)
	(*substit-hash* (make-hash-table :test #'eq)))
    (substit-binding* expr alist)))

(defun substit-binding* (expr alist)
  (let* ((newtype (substit* (type expr) alist))
	 (newdtype (if (tc-eq (print-type (type expr))
			      (declared-type expr))
		       (if (eq (print-type (type expr)) (print-type newtype))
			   (declared-type expr)
			   (print-type newtype))
		       (substit* (declared-type expr) alist))))
    (lcopy expr
      'type newtype
      'declared-type newdtype)))

(defmethod substit* ((expr binding-expr) alist)
  (if (not (substit-possible? expr alist))
      expr
      (let* ((new-bindings (make-new-bindings-internal
			    (bindings expr) alist (expression expr)))
	     (nalist (substit-pairlis (bindings expr) new-bindings alist))
	     (nexpr (let ((*alist-freevars* 'unbound)
			  (*alist-boundvars* 'unbound)
			  (*substit-hash* (make-hash-table :test #'eq)))
		      (substit* (expression expr) nalist)))
	     (ntype (if (quant-expr? expr)
			(type expr)
			(substit* (type expr) alist))))
	(copy expr
	  'bindings new-bindings
	  'type ntype
	  'expression nexpr
	  'parens 0))))

(defun substit-pairlis (bindings new-bindings alist)
  (if (null bindings)
      alist
      (substit-pairlis (cdr bindings) (cdr new-bindings)
		       (if (eq (car bindings) (car new-bindings))
			   alist
			   (acons (car bindings) (car new-bindings) alist)))))

(defun make-new-bindings (old-bindings alist expr)
  (let* ((*alist-freevars* 'unbound)
	 (*alist-boundvars* 'unbound)
	 (*substit-hash* (make-hash-table :test #'eq)))
    (make-new-bindings* old-bindings
			(alist-freevars alist)
			(alist-boundvars alist)
			alist
			expr)))

(defun make-new-bindings-internal (old-bindings alist expr)
  (make-new-bindings* old-bindings
		      (alist-freevars alist)
		      (alist-boundvars alist)
		      alist
		      expr))

(defun alist-freevars (alist)
  (if (eq *alist-freevars* 'unbound)
      (setq *alist-freevars*
	    (delete-duplicates (mapappend #'alist-freevars* alist)))
      *alist-freevars*))

(defun alist-freevars* (alist-pair)
  (freevars (cdr alist-pair)))

(defun add-alist-freevars (expr alist-freevars)
  (add-alist-freevars* (freevars expr) alist-freevars))

(defun add-alist-freevars* (freevars alist-freevars)
  (if (null freevars)
      alist-freevars
      (add-alist-freevars*
       (cdr freevars)
       (if (memq (car freevars) alist-freevars)
	   alist-freevars
	   (cons (declaration (car freevars)) alist-freevars)))))

(defun alist-boundvars (alist)
  (if (eq *alist-boundvars* 'unbound)
      (let ((bvars nil))
	(dolist (acons alist)
	  (mapobject #'(lambda (ex)
			 (or (subtype? ex)
			     (when (binding-expr? ex)
			       (dolist (bd (bindings ex)) (pushnew bd bvars))
			       nil)))
		     (cdr acons)))
	(setq *alist-boundvars* bvars))
      *alist-boundvars*))

;;freevars must be the free variables in alist.

(defun make-new-bindings* (old-bindings freevars boundvars alist expr
					&optional nbindings)
  (if (null old-bindings)
      (nreverse nbindings)
      (let* ((bind (car old-bindings))
	     (btype (type bind))
	     (check (or (member (id bind) freevars :key #'id)
			(member (id bind) boundvars :key #'id)
			(bindings-subst-clash bind alist)
;; 			(some #'(lambda (fv)
;; 				  (let ((bval (cdr (assq (declaration fv)
;; 							 alist))))
;; 				    (and bval
;; 					 (member bind (collect-references bval)
;; 						 :test #'(lambda (x y)
;; 							   (and (eq (id x)
;; 								    (id y))
;; 								(not (eq x y))))))))
;; 			      (freevars expr))
			))
	     (stype (substit* btype alist))
	     (dec-type (declared-type bind))
	     (new-binding
	      (if (not check)
		  (lcopy bind
		    'type stype
		    'declared-type (substit* dec-type alist))
		  (copy bind
		    'id (new-boundvar-id (id bind) expr)
		    'type stype
		    'declared-type (substit* dec-type alist)))))
	(unless (or (eq bind new-binding)
		    (declared-type new-binding))
	  (setf (declared-type new-binding) (or (print-type stype) stype)))
	(make-new-bindings*
	 (cdr old-bindings)
	 (add-alist-freevars new-binding freevars)
	 boundvars
	 (acons bind new-binding alist)
	 (cons new-binding expr)
	 (cons new-binding nbindings)))))

(defun bindings-subst-clash (bind alist)
  (let ((found-one nil)) 
    (mapobject #'(lambda (x) 
		   (or found-one
		       (type-expr? x)
		       (when (name-expr? x)
			 (when (and (eq (id x) (id bind))
				    (not (eq (declaration x) bind)))
			   (setq found-one t))
			 t)))
	       (mapcar #'cdr alist))
    found-one))


(defmethod substit* ((expr cases-expr) alist)
  (let* ((ntype (substit* (type expr) alist))
	 (nexpr (substit* (expression expr) alist)))
    (if (or (eq nexpr (expression expr))
	    (compatible? (type nexpr) (type (expression expr))))
	(lcopy expr
	  'expression nexpr
	  'selections (mapcar #'(lambda (s) (substit* s alist))
			(selections expr))
	  'else-part (substit* (else-part expr) alist)
	  'type ntype)
	(substit* (translate-cases-to-if expr) alist))))

(defmethod substit* ((expr selection) alist)
  (let ((new-bindings (make-new-bindings-internal
		       (args expr) alist (expression expr))))
    (lcopy expr
      'constructor (substit* (constructor expr) alist)
      'args new-bindings
      'expression (let ((*alist-freevars* 'unbound)
			(*alist-boundvars* 'unbound)
			(*substit-hash* (make-hash-table :test #'eq)))
		    (substit* (expression expr)
			      (nconc (pairlis (args expr)
					      new-bindings)
				     alist))))))

;(defmethod substit* ((expr coercion) alist)
;  (lcopy expr
;    'expression (substit* (expression expr) alist)
;    'declared-type (substit* (declared-type expr) alist)
;    'type (substit* (type expr) alist)))
  


;;2-21-91: remaining exprs are treated as unsubstitutable.
(defmethod substit* ((expr expr) alist)
  (declare (ignore alist))
  expr)

;;NSH:8-21-91: substit* for type-expressions.
(defmethod substit* ((texpr type-name) alist)
  (let ((nacts (substit* (actuals texpr) alist))
	(ptype (substit* (print-type texpr) alist))
	(mi (substit* (module-instance (resolution texpr)) alist)))
    (if (and (eq nacts (actuals texpr))
	     (eq ptype (print-type texpr))
	     (eq mi (module-instance (resolution texpr))))
	texpr
	(let ((nte (copy texpr
		     'actuals nacts
		     'print-type ptype)))
	  (setf (gethash texpr *substit-hash*) nte)
	  (setf (resolutions nte)
		(list
		 (if (eq texpr (type (resolution texpr)))
		     (mk-resolution (declaration (resolution texpr))
		       mi
		       nte)
		     (mk-resolution (declaration (resolution texpr))
		       mi
		       (substit* (type (resolution texpr)) alist)))))
	  nte))))

(defmethod substit* ((texpr subtype) alist)
  (with-slots (supertype predicate print-type) texpr
    (let* ((*needs-pseudo-normalizing* nil) ;; set to t in substit* (name-expr)
	   (npred (substit* predicate alist)))
      (if (eq npred predicate)
	  texpr
	  (let* ((spred (if t ;*needs-pseudo-normalizing*
			    (pseudo-normalize npred)
			    npred))
		 (stype (domain (find-supertype (type spred)))))
	    (copy texpr
	      'supertype stype
	      'predicate spred
	      'print-type (substit* print-type alist)))))))

(defmethod substit* ((texpr setsubtype) alist)
  (declare (ignore alist))
  (let ((nexpr (call-next-method)))
    (unless (or (not (typep nexpr 'setsubtype))
		(eq (predicate texpr) (predicate nexpr)))
      ;;(assert (lambda-expr? (predicate nexpr)))
      (setf (formula nexpr) (expression (predicate nexpr))))
    nexpr))

(defmethod substit* ((texpr expr-as-type) alist)
  (lcopy texpr
    'expr (beta-reduce (substit* (expr texpr) alist))))

(defmethod substit* ((texpr datatype-subtype) alist)
  (lcopy (call-next-method)
    'declared-type (substit* (declared-type texpr) alist)))

(defmethod substit* ((texpr funtype) alist)
  (let* ((typelist (list (domain texpr) (range texpr)))
	 (ntypelist (substit* typelist alist)))
    (lcopy texpr
      'domain (car ntypelist)
      'range (cadr ntypelist)
      'print-type (substit* (print-type texpr) alist))))

(defmethod substit* ((texpr tupletype) alist)
  (lcopy texpr
    'types (substit* (types texpr) alist)
    'print-type (substit* (print-type texpr) alist)))

(defmethod substit* ((texpr cotupletype) alist)
  (lcopy texpr
    'types (substit* (types texpr) alist)
    'print-type (substit* (print-type texpr) alist)))

(defmethod substit* ((te recordtype) alist)
  (let ((fields (substit* (fields te) alist)))
    (lcopy te
      'fields (sort-fields fields (dependent-fields? fields))
      'print-type (substit* (print-type te) alist))))

(defmethod substit* ((te type-application) alist)
  (lcopy te
    'type (substit* (type te) alist)
    'parameters (let* ((nparms (substit* (parameters te) alist)))
		  (pseudo-normalize nparms))
    'print-type (substit* (print-type te) alist)))

(defmethod substit* ((fd field-decl) alist)
  (let ((ntype (substit* (type fd) alist))
	(dtype (substit* (declared-type fd) alist)))
    (lcopy fd 'type ntype 'declared-type dtype)))

(defmethod substit* ((db dep-binding) alist)
  (let ((ntype (substit* (type db) alist)))
	(lcopy db 'type ntype)))

(defmethod substit* ((sym symbol) alist)
  (declare (ignore alist))
  sym)

;; (defun pseudo-normalize* (expr)
;;   (gensubst expr #'pseudo-normalize! #'pseudo-normalize?))

;; (defmethod pseudo-normalize? (ex)
;;   (declare (ignore ex))
;;   nil)

;; (defmethod pseudo-normalize? ((ty subtype))
;;   t)

;; (defmethod pseudo-normalize? ((act actual))
;;   (not (type-value act)))

;; (defmethod pseudo-normalize! ((ty subtype))
;;   (lcopy ty 'predicate (pseudo-normalize (predicate ty))))

;; (defmethod pseudo-normalize! ((act actual))
;;   (lcopy act 'expr (pseudo-normalize (expr act))))
