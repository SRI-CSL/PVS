;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  9 14:44:26 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr  5 01:15:56 1998
;; Update Count    : 38
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

(defvar *resolve-error-info* nil)

(defvar *field-records* nil)

(defvar *get-all-resolutions* nil)

;;; Name Resolution

;;; Typechecking a name means to resolve it in its context.  The kind
;;; allows the same name to be used for different purposes, thus a
;;; module named list can have a type named list and a function named
;;; list, since these are of different kinds.  When the kind is 'expr,
;;; the arguments are used to determine the possible types.  The decls
;;; and using provide the search space, decls is the set of declarations
;;; typechecked so far in the current module, and using is the
;;; catenation of the usings preceding the current declaration.  Args
;;; are the actual arguments this name is being applied to.  Note that
;;; 'foo' and 'foo()' are not the same; the former has unspecified
;;; arguments and can be of any type, while the latter has no arguments
;;; and must be of the type 'function[ -> range]'.  Note that typecheck
;;; will complain if there is no resolution, but resolve simply returns
;;; nil; thus resolve can be used to test if a name resolves without
;;; causing an error.

(defmethod typecheck* ((name name) expected kind argument)
  (declare (ignore expected))
  (let* ((*resolve-error-info* nil)
	 (*field-records* nil)
	 (k (or kind 'expr))
	 (args (if (and argument (eq k 'expr))
		   (argument-list argument)
		   argument))
	 (res (resolve* name k args)))
    (cond ((null res)
	   (or (and (eq k 'expr)
		    (or (argument-conversion name args)
			(function-conversion name args)))
	       (resolution-error name k args)))
	  ((and (cdr res)
		(not (eq k 'expr)))
	   (let* ((nres (if argument
			    (delete-if-not #'(lambda (r)
					       (formals (declaration r)))
			      res)
			    res))
		  (fres (or (delete-if-not #'fully-instantiated? nres) nres)))
	     (setf (resolutions name) fres)
	     (when (cdr fres)
	       (type-ambiguity name))))
	  (t (setf (resolutions name) res))))
  name)

(defmethod typecheck* ((name modname) expected kind argument)
  (declare (ignore expected kind argument))
  (let ((theory (get-typechecked-theory name)))
      (when (actuals name)
	(unless (= (length (formals-sans-usings theory))
		   (length (actuals name)))
	  (type-error name "Wrong number of actuals in ~a" name))
	;; Skip typechecking if already done.  Note that this has no effect on
	;; TCCs generated, since set-type-actuals is called in any case.  It
	;; just keeps actuals from being typechecked to something else.
	;; This fix is needed because the prover calls typecheck on the
	;; module-instances it finds to ensure that the proper TCCs are
	;; generated (see lemma-step).
	(unless (every #'typed? (actuals name))
	  (typecheck-actuals name))
	(unless (member name (gethash theory (using-hash *current-context*))
			:test #'tc-eq)
	  (set-type-actuals name)
	  (check-compatible-params (formals-sans-usings theory)
				   (actuals name) nil)))))

(defmethod module (binding)
  (declare (ignore binding))
  *current-theory*)

(defun resolve* (name kind args)
  (typecheck-actuals name kind)
  (filter-preferences name (get-resolutions name kind args) kind args))

;;(type-error name "May not provide actuals for entities defined locally")
;;(type-error name "Free variables not allowed here")

(defun get-resolutions (name kind args)
  (let* ((adecls (gethash (id name) (current-declarations-hash)))
	 (ldecls (if (library name)
		     (let ((lpath (get-library-pathname (library name))))
		       (if lpath
			   (remove-if-not
			       #'(lambda (d)
				   (and (library-theory? (module d))
					(equal lpath (library (module d)))))
			     adecls)
			   (type-error name "~a is an unknown library"
				       (library name))))
		     adecls))
	 (decls (if (mod-id name)
		    (remove-if-not
			#'(lambda (d) (eq (id (module d)) (mod-id name)))
		      ldecls)
		    ldecls))
	 (theory-aliases (get-theory-aliases name)))
    (nconc (get-binding-resolutions name kind args)
	   (get-record-arg-resolutions name kind args)
	   (get-decls-resolutions decls (actuals name) kind args)
	   (get-theory-alias-decls-resolutions theory-aliases adecls
					       kind args)))))

(defun get-theory-alias-decls-resolutions (theory-aliases adecls kind args
							  &optional reses)
  (if (null theory-aliases)
      reses
      (let* ((thalias (car theory-aliases))
	     (decls (remove-if-not #'(lambda (d)
				       (eq (id (module d)) (id thalias)))
		      adecls))
	     (res (get-decls-resolutions decls (actuals thalias) kind args)))
	(get-theory-alias-decls-resolutions
	 (cdr theory-aliases) adecls kind args
	 (nconc reses res)))))

(defun get-theory-aliases (name)
  (when (mod-id name)
    (let ((mod-decls (remove-if-not #'mod-decl?
		       (gethash (mod-id name) (current-declarations-hash)))))
      (assert (every #'(lambda (md) (fully-instantiated? (modname md)))
		     mod-decls))
      (mapcar #'modname mod-decls))))

(defmethod get-binding-resolutions ((name name) kind args)
  (with-slots (mod-id library actuals id) name
    (when (and (eq kind 'expr)
	       (null library)
	       (null mod-id)
	       (null actuals))
      (let ((bdecls (remove-if-not #'(lambda (bd) (eq (id bd) id))
		      *bound-variables*)))
	(when bdecls
	  (if args
	      (let ((thinst (current-theory-name)))
		(mapcan #'(lambda (bd)
			    (when (compatible-arguments? bd thinst args
							 (current-theory))
			      (list (mk-resolution bd thinst (type bd)))))
		  bdecls))
	      (mapcar #'(lambda (bd)
			  (mk-resolution bd (current-theory-name) (type bd)))
		bdecls)))))))

(defmethod get-record-arg-resolutions ((name name) kind args)
  (with-slots (mod-id library actuals id) name
    (when (and (eq kind 'expr)
	       (null library)
	       (null mod-id)
	       (null actuals)
	       (singleton? args))
      (let ((rtypes (get-arg-record-types (car args))))
	(when rtypes
	  (mapcan #'(lambda (rtype)
		      (let ((fdecl (find-if #'(lambda (fd)
						(eq (id fd) (id name)))
				     (fields rtype))))
			(when fdecl
			  (let ((ftype
				 (if (and (dependent? rtype)
					  (some #'(lambda (x)
						    (and (not (eq x fdecl))
							 (occurs-in x fdecl)))
						(fields rtype)))
				     (let* ((db (mk-dep-binding
						 (make-new-variable '|r| rtype)
						 rtype))
					    (ne (make-dep-field-name-expr
						 db rtype))
					    (ftype (field-application-type
						    (id fdecl) rtype ne)))
				       (mk-funtype db ftype))
				     (mk-funtype (list rtype) (type fdecl)))))
			  (list (make-resolution fdecl (current-theory-name)
						 ftype))))))
	    rtypes))))))

(defun get-arg-record-types (arg)
  (get-arg-record-types* (ptypes arg)))

(defun get-arg-record-types* (types &optional rtypes)
  (if (null types)
      rtypes
      (let ((stype (find-supertype (car types))))
	(get-arg-record-types* (cdr types)
			       (if (typep stype 'recordtype)
				   (cons stype rtypes)
				   rtypes)))))

(defun get-decls-resolutions (decls acts kind args &optional reses)
  (if (null decls)
      reses
      (let ((dreses (get-decl-resolutions (car decls) acts kind args)))
	(get-decls-resolutions (cdr decls) acts kind args
			       (nconc dreses reses)))))

(defun get-decl-resolutions (decl acts kind args)
  (let ((dth (module decl)))
    (when (and (kind-match (kind-of decl) kind)
	       (or (eq dth (current-theory))
		   (and (visible? decl)
			(can-use-for-assuming-tcc? decl)))
	       (not (disallowed-free-variable? decl))
	       (or (null args)
		   (case kind
		     (type (and (formals decl)
				(or (length= (formals decl) args)
				    (singleton? (formals decl))
				    (singleton? args))))
		     (expr (let ((ftype (find-supertype (type decl))))
			     (and (typep ftype 'funtype)
				  (or (length= (domain-types ftype) args)
				      (singleton? (domain-types ftype))
				      (singleton? args)))))
		     (t nil))))
      (if (null acts)
	  (if (or (null (formals-sans-usings dth))
		  (eq dth (current-theory)))
	      (if (null args)
		  (list (mk-resolution decl (mk-modname (id (module decl)))
				       (case kind
					 (expr (type decl))
					 (type (type-value decl)))))
		  (let ((modinsts (decl-args-compatible? decl args)))
		    (mapcar #'(lambda (thinst)
				(make-resolution decl thinst))
		      modinsts)))
	      (let ((modinsts (decl-args-compatible? decl args)))
		(mapcar #'(lambda (thinst)
			    (make-resolution decl thinst))
		  modinsts)))
	  (when (length= acts (formals-sans-usings dth))
	    (let* ((thinsts (gethash dth (current-using-hash)))
		   (genthinst (find-if-not #'actuals thinsts)))
	      (if genthinst
		  (when (and (decl-args-compatible? decl args)
			     (compatible-parameters?
			      acts (formals-sans-usings dth)))
		    (let* ((nacts (copy-actuals acts))
			   (dthi (mk-modname-no-tccs (id dth) nacts))
			   (*generate-tccs* 'none))
		      (set-type-actuals dthi)
		      #+pvsdebug (assert (fully-typed? dthi))
		      (list (make-resolution decl dthi))))
		  (let* ((modinsts (decl-args-compatible? decl args))
			 (thinst (or (find-if
					 #'(lambda (thinst)
					     (tc-eq acts (actuals thinst)))
				       modinsts)
				     (find-if
					 #'(lambda (dth)
					     (every #'matching-actual
						    acts (actuals dth)
						    (formals-sans-usings
						     (get-theory dth))))
				       modinsts))))
		    (unless thinst (break "No matching thinst"))
		    (when thinst
		      (list (make-resolution decl thinst)))))))))))

(defun copy-actuals (acts)
  acts)

(defun decl-args-compatible? (decl args)
  (if (eq (module decl) (current-theory))
      (compatible-arguments? decl (current-theory-name) args (current-theory))
      (let ((thinsts (gethash (module decl) (current-using-hash))))
	(mapcan #'(lambda (thinst)
		    (compatible-arguments? decl thinst args (current-theory)))
	  thinsts))))

;;; This will set the types of the actual paramters.
;;; Name-exprs are typechecked twice; once as an expression and once as
;;; a type.  This is where any restriction of actuals is enforced, as
;;; the grammar allows arbitrary expressions and type expressions.  Note
;;; that the actual parameters must uniquely resolve.

(defun typecheck-actuals (name &optional (kind 'expr))
  (mapc #'(lambda (a) (typecheck* a nil kind nil))
	(actuals name)))

(defmethod typecheck* ((act actual) expected kind arguments)
  (declare (ignore expected kind arguments))
  #+pvsdebug (when (type (expr act)) (break "Type set already???"))
  (unless (and ;;*in-checker*
	       (typed? act))
    (typecase (expr act)
      (name-expr (let* ((name (expr act))
			(tres (with-no-type-errors (resolve* name 'type nil)))
			(eres (with-no-type-errors (resolve* name 'expr nil))))
		   (unless (or tres eres)
		     (type-error (expr act) "Actual does not resolve"))
		   (if (cdr tres)
		       (cond (eres
			      (setf (resolutions name) eres))
			     (t (setf (resolutions name) tres)
				(type-ambiguity name)))
		       (setf (resolutions name) (nconc tres eres)))
		   (when eres
		     (setf (types (expr act)) (mapcar #'type eres))
		     (setf (kind (expr act))
			   (if (or (memq (declaration (car eres))
					 *bound-variables*)
				   (typep (declaration (car eres))
					  '(and (or binding var-decl)
						(not field-decl))))
			       'VARIABLE 'CONSTANT))
		     (when (and (plusp (parens (expr act)))
				(some #'(lambda (ty)
					  (let ((sty (find-supertype ty)))
					    (and (funtype? sty)
						 (tc-eq (range sty) *boolean*))))
				      (ptypes (expr act))))
		       (setf (type-value act)
			     (typecheck* (make-instance 'expr-as-type
					   'expr (copy-untyped (expr act)))
					 nil nil nil))))
		   (when tres
		     (if (type-value act)
			 (unless (compatible? (type-value act) (type (car tres)))
			   (push (car tres) (resolutions (expr act)))
			   (type-ambiguity (expr act)))
			 (progn
			   (when (and (mod-id (expr act))
				      (typep (type (car tres)) 'type-name)
				      (same-id (expr act) (type (car tres))))
			     (setf (mod-id (type (car tres)))
				   (mod-id (expr act))))
			   (setf (type-value act) (type (car tres)))
			   (push 'type (types (expr act))))))))
      (set-expr (typecheck* (expr act) nil nil nil)
		(let ((texpr (typecheck* (setsubtype-from-set-expr (expr act))
					 nil nil nil)))
		  (when texpr
		    (setf (type-value act) texpr)
		    (push 'type (types (expr act))))))
      (application (with-no-type-errors
		    (typecheck* (expr act) nil nil nil))
		   (cond ((and (zerop (parens (expr act)))
			       (typep (operator (expr act)) 'name))
			  (with-no-type-errors
			   (let* ((tn (mk-type-name (operator (expr act))))
				  (args (arguments (expr act)))
				  (tval (typecheck*
					 (make-instance 'type-application
					   'type tn
					   'parameters args)
					 nil nil nil)))
			     (setf (type-value act) tval))))
			 ((and (plusp (parens (expr act)))
			       (some #'(lambda (ty)
					 (let ((sty (find-supertype ty)))
					   (and (funtype? sty)
						(tc-eq (range sty) *boolean*))))
				     (ptypes (expr act))))
			  (setf (type-value act)
				(typecheck* (make-instance 'expr-as-type
					      'expr (expr act))
					    nil nil nil))))
		   (unless (or (ptypes (expr act))
			       (type-value act))
		     (type-error (expr act) "Actual does not resolve")))
      (expr (typecheck* (expr act) nil nil nil)
	    (when (and (plusp (parens (expr act)))
		       (some #'(lambda (ty)
				 (let ((sty (find-supertype ty)))
				   (and (funtype? sty)
					(tc-eq (range sty) *boolean*))))
			     (ptypes (expr act))))
	      (setf (type-value act)
		    (typecheck* (make-instance 'expr-as-type
				  'expr (expr act))
				nil nil nil))))
      (t (unless (type-value act)
	   (setf (type-value act)
		 (typecheck* (expr act) nil nil nil))))))
  act)

(defun setsubtype-from-set-expr (expr)
  (let ((suptype (declared-type (car (bindings expr)))))
    (assert (typep (car (bindings expr)) 'bind-decl))
    (make-instance 'setsubtype
      'supertype suptype
      'formals (car (bindings expr))
      'formula (expression expr))))

(defun eq-id (x y)
  (eq x (id y)))


;;; At this point, we know that the module has the right id, the right
;;; number of (matching) parameters, and at least one possibly matching
;;; declaration, and that the name has actuals (see above).  This method
;;; tests whether the given module instance matches the name by
;;; comparing the actuals of the name with the actuals of the module
;;; instance.  They automatically match if the is nil.

(defvar *matching-actual-to-formal* nil)

(defun matching-actuals (actuals mod modinsts result)
  (if (null modinsts)
      (when result
	(let ((eqs (remove-if-not #'(lambda (mi) (tc-eq (actuals mi) actuals))
		     result)))
	  ;;(assert (equal eqs result))
	  (cons mod
		(mapcar #'(lambda (mi)
			    (let ((nmi (copy mi
					 'actuals (mapcar #'copy
							  (actuals mi)))))
			      ;;(set-type-actuals nmi)
			      nmi))
			(or eqs result)))))
      (let* ((*matching-actual-to-formal* nil)
	     (mactuals (actuals (car modinsts)))
	     (mi (cond ((null mactuals)
			(when (compatible-parameters?
			       actuals
			       (formals-sans-usings mod))
			  (let ((mn (mk-modname-no-tccs (id mod) actuals))
				(*generate-tccs* 'none))
			    (set-type-actuals mn)
			    #+pvsdebug (assert (fully-typed? mn))
			    mn)))
		       ((every #'matching-actual
				 actuals mactuals
				 (formals-sans-usings mod))
			(if *matching-actual-to-formal*
			    (copy (car modinsts) 'actuals actuals)
			    (car modinsts))))))
	(matching-actuals actuals mod (cdr modinsts)
			  (if mi (cons mi result) result)))))

(defun compatible-parameters? (actuals formals)
  (when (length= actuals formals)
    (compatible-parameters?* actuals formals)))

(defun compatible-parameters?* (actuals formals &optional alist)
  (or (null actuals)
      (multiple-value-bind (nfml nalist)
	  (subst-actuals-in-next-formal (car actuals) (car formals) alist)
	(let ((type (compatible-parameter? (car actuals) nfml)))
	  (and type
	       (if (typep nfml 'formal-type-decl)
		   (compatible-parameters?* (cdr actuals) (cdr formals) nalist)
		   (some #'(lambda (ty)
			     (let ((nact (copy-untyped (car actuals))))
			       (typecheck* nact nil nil nil)
			       (set-type (expr nact) ty)
			       (compatible-parameters?*
				(cdr actuals) (cdr formals)
				(acons (car formals) nact (cdr nalist)))))
			 type)))))))

(defun compatible-parameter? (actual formal)
  (if (formal-type-decl? formal)
      (type-value actual)
      (unless (type-expr? (expr actual))
	(remove-if-not #'(lambda (ptype)
			   (and (type-expr? ptype)
				(compatible? ptype (type formal))))
	      (ptypes (expr actual))))))

(defun matching-actual (actual mactual formal)
  (if (formal-type-decl? formal)
      (let ((atv (type-value actual)))
	(and atv
	     (or (null mactual)
		 (tc-eq atv (type-value mactual))
		 (and (name? (type-value mactual))
		      (formal-type-decl? (declaration (type-value mactual)))
		      (setq *matching-actual-to-formal* t)))))
      (matching-actual-expr (expr actual) mactual)))

(defmethod matching-actual-expr ((aex expr) (maex implicit-conversion))
  (or (call-next-method)
      (matching-actual-expr aex (argument maex))))

(defmethod matching-actual-expr ((aex expr) (maex actual))
  (matching-actual-expr aex (expr maex)))

(defmethod matching-actual-expr ((aex expr) (maex null))
  t)

(defmethod matching-actual-expr ((aex expr) maex)
  (if (type aex)
      (tc-eq aex maex)
      (matching-actual-expr* aex maex nil)))

(defmethod matching-actual-expr (aex maex)
  (declare (ignore aex maex))
  nil)

(defmethod matching-actual-expr* (aex maex bindings)
  (declare (ignore aex maex bindings))
  nil)

(defmethod matching-actual-expr* (aex (maex implicit-conversion) bindings)
  (or (call-next-method)
      (matching-actual-expr* aex (argument maex) bindings)))

(defmethod matching-actual-expr* ((l1 cons) (l2 cons) bindings)
  (matching-actual-expr-lists l1 l2 bindings))

(defun matching-actual-expr-lists (l1 l2 bindings)
  (declare (list l1 l2))
  (cond ((null l1) (null l2))
	((null l2) nil)
	(t (and (matching-actual-expr* (car l1) (car l2) bindings)
		(matching-actual-expr-lists
		 (cdr l1) (cdr l2)
		 (new-tc-eq-list-bindings (car l1) (car l2)
					  bindings))))))

(defmethod matching-actual-expr* ((e1 name-expr) (e2 field-name-expr) bindings)
  (with-slots ((id1 id) (ty1 types)) e1
    (with-slots ((id2 id) (ty2 type)) e2
      (and (eq id1 id2)
	   (some #'(lambda (ty) (tc-eq-with-bindings ty ty2 bindings))
		 ty1)))))

(defmethod matching-actual-expr* ((e1 projection-application)
				  (e2 projection-application)
				  bindings)
  (or (eq e1 e2)
      (with-slots ((id1 id) (arg1 argument)) e1
	(with-slots ((id2 id) (arg2 argument)) e2
	  (and (eq id1 id2)
	       (matching-actual-expr* arg1 arg2 bindings))))))

(defmethod matching-actual-expr* ((e1 projection-application) (e2 name-expr)
				  bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (matching-actual-expr* (cdr bind) e2 nil))))

(defmethod matching-actual-expr* ((e1 name-expr) (e2 projection-application)
				  bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (matching-actual-expr* (cdr bind) e2 nil))))

(defmethod matching-actual-expr* ((e1 application) (e2 field-application)
				  bindings)
  (with-slots ((op1 operator) (arg1 argument)) e1
    (with-slots ((id2 id) (arg2 argument)) e2
      (and (typep op1 'name-expr)
	   (eq (id op1) id2)
	   (matching-actual-expr* arg1 arg2 bindings)))))

(defmethod matching-actual-expr* ((e1 number-expr) (e2 number-expr) bindings)
  (declare (ignore bindings))
  (with-slots ((n1 number)) e1
    (with-slots ((n2 number)) e2
      (= (the integer n1) (the integer n2)))))

(defmethod matching-actual-expr* ((e1 record-expr) (e2 record-expr) bindings)
  (with-slots ((ass1 assignments)) e1
    (with-slots ((ass2 assignments)) e2
      (matching-actual-expr-fields ass1 ass2 bindings))))

(defun matching-actual-expr-fields (flds1 flds2 bindings)
  (cond ((null flds1) (null flds2))
	((null flds2) nil)
	(t (let ((fld2 (find (car flds1) flds2
			     :test #'(lambda (x y)
				       (same-id (caar (arguments x))
						(caar (arguments y)))))))
	     (and fld2
		  (matching-actual-expr*
		   (expression (car flds1)) (expression fld2) bindings)
		  (matching-actual-expr-fields
		   (cdr flds1) (remove fld2 flds2) bindings))))))

(defmethod matching-actual-expr* ((e1 tuple-expr) (e2 tuple-expr) bindings)
  (with-slots ((ex1 exprs)) e1
    (with-slots ((ex2 exprs)) e2
      (matching-actual-expr* ex1 ex2 bindings))))

(defmethod matching-actual-expr* ((e1 cases-expr) (e2 cases-expr) bindings)
  (with-slots ((expr1 expression) (sel1 selections) (else1 else-part)) e1
    (with-slots ((expr2 expression) (sel2 selections) (else2 else-part)) e2
      (and (matching-actual-expr* expr1 expr2 bindings)
	   (matching-actual-expr-selections sel1 sel2 bindings)
	   (matching-actual-expr* else1 else2 bindings)))))

(defun matching-actual-expr-selections (sel1 sel2 bindings)
  (cond ((null sel1) (null sel2))
	((null sel2) nil)
	(t (let ((s2 (find (constructor (car sel1)) sel2
			   :test #'tc-eq :key #'constructor)))
	     (and s2
		  (matching-actual-expr* (car sel1) s2 bindings)
		  (matching-actual-expr-selections
		   (cdr sel1) (remove s2 sel2) bindings))))))

(defmethod matching-actual-expr* ((s1 selection) (s2 selection) bindings)
  (with-slots ((args1 args) (ex1 expression)) s1
    (with-slots ((args2 args) (ex2 expression)) s2
      (multiple-value-bind (eq? abindings)
	  (bindings-eq args1 args2 bindings)
	(and eq?
	     (matching-actual-expr* ex1 ex2 abindings))))))

(defmethod matching-actual-expr* ((e1 application) (e2 application) bindings)
  (with-slots ((op1 operator) (arg1 argument)) e1
    (with-slots ((op2 operator) (arg2 argument)) e2
      (and (matching-actual-expr-ops op1 op2 bindings)
	   (matching-actual-expr* arg1 arg2 bindings)))))

(defmethod matching-actual-expr-ops ((op1 name-expr) (op2 field-name-expr)
				     bindings)
  (with-slots ((id1 id) (ty1 types)) op1
    (with-slots ((id2 id) (ty2 type)) op2
      (and (eq id1 id2)
	   (some #'(lambda (ty) (tc-eq-with-bindings ty ty2 bindings))
		 ty1)))))

(defmethod matching-actual-expr-ops (op1 op2 bindings)
  (matching-actual-expr* op1 op2 bindings))

(defmethod matching-actual-expr* ((e1 binding-expr) (e2 binding-expr)
				  ibindings)
  (with-slots ((b1 bindings) (ex1 expression)) e1
    (with-slots ((b2 bindings) (ex2 expression)) e2
      (and (same-binding-op? e1 e2)
	   (multiple-value-bind (eq? abindings)
	       (bindings-eq b1 b2 ibindings)
	     (and eq?
		  (matching-actual-expr* ex1 ex2 abindings)))))))

(defmethod matching-actual-expr* ((A coercion) (B coercion) bindings)
  (or (eq A B)
      (matching-actual-expr* (args1 A) (args1 B) bindings)))

(defmethod matching-actual-expr* ((A coercion) (B expr) bindings)
  (matching-actual-expr* (args1 A) B bindings))

(defmethod matching-actual-expr* ((A expr) (B coercion) bindings)
  (matching-actual-expr* A (args1 B) bindings))

(defmethod matching-actual-expr* ((A name-expr) (B coercion) bindings)
  (matching-actual-expr* A (args1 B) bindings))

(defmethod matching-actual-expr* ((e1 update-expr) (e2 update-expr) bindings)
  (with-slots ((ex1 expression) (ass1 assignments)) e1
    (with-slots ((ex2 expression) (ass2 assignments)) e2
      (and (matching-actual-expr* ex1 ex2 bindings)
	   (matching-actual-expr* ass1 ass2 bindings)))))

(defmethod matching-actual-expr* ((a1 assignment) (a2 assignment) bindings)
  (with-slots ((args1 arguments) (ex1 expression)) a1
    (with-slots ((args2 arguments) (ex2 expression)) a2
      (and (matching-actual-expr* args1 args2 bindings)
	   (matching-actual-expr* ex1 ex2 bindings)))))

(defmethod matching-actual-expr* ((n1 binding) (n2 binding) bindings)
  (declare (ignore bindings))
  (eq n1 n2))

(defmethod matching-actual-expr* ((n1 binding) (n2 name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod matching-actual-expr* ((n1 name) (n2 binding) bindings)
  (declare (ignore bindings))
  nil)

(defmethod matching-actual-expr* ((n1 name) (n2 modname) bindings)
  (declare (ignore bindings))
  nil)

(defmethod matching-actual-expr* ((n1 modname) (n2 name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod matching-actual-expr* ((n1 name) (n2 name) bindings)
  (with-slots ((id1 id) (res1 resolutions)) n1
    (with-slots ((id2 id) (res2 resolutions)) n2
      (some #'(lambda (r)
		(if (actuals (module-instance r))
		    (if (actuals (module-instance (car res2)))
			(tc-eq-with-bindings r (car res2) bindings)
			(same-declaration r (car res2)))
		    (if (actuals (module-instance (car res2)))
			(same-declaration r (car res2))
			(tc-eq-with-bindings r (car res2) bindings))))
	    res1))))

(defun match-record-arg-decls (name kind args)
  (when (and (eq kind 'expr)
	     (singleton? args)
	     (null (mod-id name))
	     (null (actuals name))
	     (null (library name)))
    (let ((fdecls (mapcan #'(lambda (pty)
			      (let ((sty (find-supertype pty)))
				(when (typep sty 'recordtype)
				  (let ((fdecl (find name (fields sty)
						     :test #'same-id)))
				    (when fdecl
				      (push (cons fdecl sty) *field-records*)
				      (list fdecl))))))
			  (ptypes (car args))))
	  (modinsts (list (theory-name *current-context*))))
      ;;; FIXME - create resolutions here
      (matching-decls* name fdecls modinsts args nil))))

(defun can-use-for-assuming-tcc? (d)
  (or (not *in-checker*)
      (not *top-proofstate*)
      (not (typep (declaration *top-proofstate*) 'assuming-tcc))
      (let ((ass (generating-assumption (declaration *top-proofstate*))))
	(or (not (eq (module d) (module ass)))
	    (not (memq d (memq ass (all-decls (module ass)))))))))

(defun kind-match (kind1 kind2)
  (or (eq kind1 kind2)
      (and (eq kind2 'rewrite)
	   (memq kind1 '(expr formula)))))

(defun matching-decls* (name decls modinsts args result)
  (if (null decls)
      result
      (let ((res (match-decl name (car decls) modinsts args nil)))
	(matching-decls* name (cdr decls) modinsts args
			 (nconc res result)))))

(defun match-decl (name decl modinsts args result)
  (if (null modinsts)
      result
      (let ((res (matching-decl decl (car modinsts) args
				(theory *current-context*))))
	(match-decl name decl (cdr modinsts) args (nconc res result)))))

;;; Returns a resolution or nil, depending on whether the provided decl matches.
;;;   modinst	- is the module instance, represented by a name with the
;;;		  module id and the actual parameters.
;;;   args	- the arguments to the name, if it is an application
;;;		  note that 'foo()' and 'foo' are treated differently.

(defun matching-decl (decl modinst args mod)
  (let ((modinsts (compatible-arguments? decl modinst args mod)))
    (mapcar #'(lambda (mi)
		(let ((type (when (and (field-decl? decl)
				       (not (member decl *bound-variables*)))
			      (assert args)
			      (subst-mod-params
			       (make-field-type decl)
			       mi))))
		  (make-resolution decl mi type))) 
	    modinsts)))

(defun make-field-type (field-decl)
  (assert *field-records*)
  (let ((rectype (cdr (assq field-decl *field-records*))))
    (if (and (dependent? rectype)
	     (some #'(lambda (x)
		       (and (not (eq x field-decl))
			    (occurs-in x field-decl)))
		   (fields rectype)))
	(let* ((db (mk-dep-binding (make-new-variable '|r| rectype) rectype))
	       (ne (make-dep-field-name-expr db rectype))
	       (ftype (field-application-type (id field-decl) rectype ne)))
	  (mk-funtype db ftype))
	(mk-funtype (list rectype) (type field-decl)))))

(defun make-dep-field-name-expr (db rectype)
  (let ((dres (make-resolution db (theory-name *current-context*) rectype)))
    (mk-name-expr (id db) nil nil dres 'variable)))

(defmethod compatible-arguments? (decl modinst args mod)
  (declare (type list args))
  (if (null args)
      (list modinst)
      (let* ((stype (find-supertype (type decl)))
	     (dtypes (when (typep stype 'funtype)
		       (domain-types stype)))
	     (args (if (and (singleton? dtypes)
			    (not (singleton? args)))
		       (let ((atup (mk-arg-tuple-expr* args)))
			 (setf (types atup)
			       (all-possible-tupletypes args))
			 (list atup))
		       args)))
	(and dtypes
	     (or (length= dtypes args)
		 (and (singleton? args)
		      (some #'(lambda (ty)
				(let ((sty (find-supertype ty)))
				  (and (typep sty 'tupletype)
				       (length= (types sty) dtypes))))
			    (ptypes (car args))))
		 (progn (push (list :arg-length decl)
			      *resolve-error-info*)
			nil))
	     (if (uninstantiated? modinst mod decl)
		 (compatible-uninstantiated? decl modinst dtypes args)
		 (let* ((*generate-tccs* 'none)
			(domtypes (mapcar #'(lambda (dt)
					      (let ((*smp-include-actuals* t)
						    (*smp-dont-cache* t))
						(if (actuals modinst)
						    (subst-mod-params
						     dt modinst))
						dt))
				    dtypes)))
		   (assert (fully-instantiated? modinst))
		   ;;(set-type-actuals modinst)
		   (when (compatible-args? decl args domtypes)
		     (list modinst))))))))

(defmethod compatible-arguments? ((decl field-decl) modinst args mod)
  (declare (ignore mod))
  (cond ((null args) (list modinst))
	((memq decl *bound-variables*)
	 (let* ((stype (find-supertype (type decl)))
		(dtypes (when (funtype? stype)
			  (domain-types stype))))
	   (and (funtype? stype)
		(or (length= dtypes args)
		    (and (singleton? args)
			 (some #'(lambda (ty)
				   (let ((sty (find-supertype ty)))
				     (and (tupletype? sty)
					  (length= (types sty) dtypes))))
			       (ptypes (car args)))))
		(when (compatible-args?
		       decl
		       args
		       (mapcar #'(lambda (dt)
				   (let ((*smp-include-actuals* t)
					 (*smp-dont-cache* t))
				     (if (actuals modinst)
					 (subst-mod-params dt modinst)
					 dt)))
			 dtypes))
		  (list modinst)))))
	(t (if (uninstantiated-theory? modinst)
	       (let* ((stype (find-supertype (type decl)))
		      (dtypes (when (funtype? stype)
				(domain-types stype))))
		 (compatible-uninstantiated? decl (copy modinst) dtypes args))
	       (list modinst)))))

(defmethod compatible-arguments? ((decl type-decl) modinst args mod)
  (if (null args)
      (list modinst)
      (let ((fargs (car (formals decl))))
	(and fargs
	     (or (length= fargs args)
		 (and (singleton? args)
		      (some #'(lambda (ty)
				(let ((sty (find-supertype ty)))
				  (and (typep sty 'tupletype)
				       (length= (types sty) fargs))))
			    (ptypes (car args))))
		 (progn (push (list :arg-length decl)
			      *resolve-error-info*)
			nil))
	     (if (uninstantiated? modinst mod decl)
		 (compatible-uninstantiated?
		  decl
		  modinst
		  (mapcar #'(lambda (fd) (find-supertype (type fd)))
		    (car (formals decl)))
		  args)
		 (when (compatible-args?
			decl args
			(mapcar #'(lambda (fa)
				    (subst-mod-params (type fa) modinst))
				fargs))
		   (list modinst)))))))

(defun uninstantiated-theory? (modinst)
  (assert *current-theory*)
  (unless (same-id modinst *current-theory*)
    (let ((theory (get-theory modinst)))
      (and (formals theory)
	   (or (null (actuals modinst))
	       (some #'(lambda (a)
			 (let ((d (declaration a)))
			   (and (formal-decl? d)
				(not (member d (formals *current-theory*))))))
		     (actuals modinst)))))))

(defun uninstantiated? (modinst mod decl)
  (and (not (binding? decl))
       (not (eq mod (module decl)))
       (formals (module decl))
       (or (null (actuals modinst))
	   (some #'(lambda (a)
		     (let ((d (declaration a)))
		       (and d
			    (typep d 'formal-decl)
			    (not (member d (formals mod))))))
		 (actuals modinst)))))

(defmethod compatible-uninstantiated? (decl modinst dtypes args)
  (let ((bindings (find-compatible-bindings
		   args
		   dtypes
		   (mapcar #'list (formals-sans-usings (module decl))))))
    (or (create-compatible-modinsts modinst bindings nil)
	(progn (push (list :no-instantiation decl)
		     *resolve-error-info*)
	       nil))))

(defmethod compatible-uninstantiated? ((decl type-decl) modinst dtypes args)
  (let ((bindings (find-compatible-bindings
		   args
		   dtypes
		   (mapcar #'list
		     (formals-sans-usings (get-theory modinst))))))
    (create-compatible-modinsts modinst bindings nil)))

(defun create-compatible-modinsts (modinst bindings result)
  (if (null bindings)
      result
      (create-compatible-modinsts
       modinst (cdr bindings)
       (if (every #'cdr (car bindings))
	   (cons (copy modinst
		   'actuals (mapcar #'(lambda (a)
					(mk-res-actual (cdr a) modinst))
			      (car bindings)))
		 result)
	   (cons (copy modinst) result)))))

(defun mk-res-actual (expr modinst)
  (if (member (id modinst) '(|equalities| |notequal|))
      (mk-actual (find-supertype expr))
      (mk-actual expr)))


;;; Compatible-args? checks that there is some assignment of the
;;; possible types of the arguments that is compatible with the
;;; corresponding type provided by the declaration.  In making this
;;; check, the actual parameters will be substituted for the formal
;;; parameters in the declaration.

(defun compatible-args? (decl args types)
  (if (length= args types)
      (compatible-args*? decl args types 1)
      (some #'(lambda (ptype)
		(let ((sty (find-supertype ptype)))
		  (and (tupletype? sty)
		       (length= (types sty) types)
		       (every #'compatible? (types sty) types))))
	    (ptypes (car args)))))

(defun compatible-args*? (decl args types argnum)
  (or (null args)
      (if (some #'(lambda (ptype)
		    (compatible-args**? ptype (car types)))
		(ptypes (car args)))
	  (compatible-args*? decl (cdr args) (cdr types) (1+ argnum))
	  (progn (push (list :arg-mismatch decl argnum args types)
		       *resolve-error-info*)
		 nil))))

(defun compatible-args**? (atype etype)
  (if (and (recognizer? atype) (recognizer? etype)
	   (fully-instantiated? atype) (fully-instantiated? etype))
      (tc-eq atype
	     (if (typep etype 'dep-binding)
		 (type etype)
		 etype))
      (compatible? atype etype)))

(defun disallowed-free-variable? (decl)
  (and (not *allow-free-variables*)
       (typep decl 'var-decl)
       (not (typep (declaration *current-context*) 'formula-decl))))


;;; Filter-preferences returns a subset of the list of decls, filtering
;;; out declarations which are not as preferred as others.  Given
;;; declarations D1 and D2 of types T1 and T2, D1 is prefered to D2 if:
;;;   
;;;   T1 is a subtype of T2, or

(defun filter-preferences (name reses kind args)
  (declare (ignore name))
  (if (cdr reses)
      (let* ((dreses (remove-duplicates reses :test #'tc-eq))
	     (res (or (remove-if
			  #'(lambda (r)
			      (typep (module-instance r) 'datatype-modname))
			dreses)
		      dreses)))
	(if (eq kind 'expr)
	    (if (cdr res)
		(filter-local-expr-resolutions
		 (filter-bindings res args))
		res)
	    (remove-outsiders (remove-generics res))))
      reses))

(defun filter-bindings (reses args)
  (or (remove-if-not #'(lambda (r) (memq (declaration r) *bound-variables*))
	reses)
      (and args
	   (filter-res-exact-matches reses args))
      reses))

(defun filter-res-exact-matches (reses args)
  (let ((exact-matches
	 (remove-if-not #'(lambda (r)
			    (let* ((stype (find-supertype (type r)))
				   (dtypes (if (cdr args)
					       (domain-types stype)
					       (list (if (dep-binding?
							  (domain stype))
							 (type (domain stype))
							 (domain stype))))))
			      (every #'(lambda (a dt)
					 (some #'(lambda (aty)
						   (tc-eq aty dt))
					       (types a)))
				     args dtypes)))
	   reses)))
    (append exact-matches
	    (remove-if #'(lambda (r)
			   (or (memq r exact-matches)
			       (let* ((stype (find-supertype (type r))))
				 (some #'(lambda (r2)
					   (compatible? (range stype)
							(range (find-supertype (type r2)))))
				       exact-matches))))
	      reses))))

(defun filter-local-expr-resolutions (reses)
  (if *get-all-resolutions*
      reses
      (filter-local-expr-resolutions* reses nil)))

(defun filter-local-expr-resolutions* (reses freses)
  (if (null reses)
      freses
      (let ((res (car reses)))
	(filter-local-expr-resolutions*
	 (cdr reses)
	 (if (or (member res (cdr reses) :test #'same-type-but-less-local)
		 (member res freses :test #'same-type-but-less-local))
	     freses
	     (cons res freses))))))

(defun same-type-but-less-local (res1 res2)
  (and (tc-eq (type res1) (type res2))
       (< (locality res2) (locality res1))))

(defmethod locality ((res resolution))
  (locality (declaration res)))

(defmethod locality ((decl binding))
  0)

(defmethod locality ((decl declaration))
  (with-slots (module) decl
    (cond ((eq module (current-theory))
	   1)
	  ((from-prelude? module)
	   4)
	  ((typep module '(or library-theory library-datatype))
	   3)
	  (t 2))))

(defun partition-on-same-ranges (reses partition)
  (if (null reses)
      (mapcar #'cdr partition)
      (let* ((range (range (find-supertype (type (car reses)))))
	     (part (assoc range partition :test #'tc-eq)))
	(cond (part
	       (nconc part (list (car reses)))
	       (partition-on-same-ranges (cdr reses) partition))
	      (t (partition-on-same-ranges
		  (cdr reses)
		  (acons range (list (car reses)) partition)))))))

(defun remove-outsiders (resolutions)
  (filter-local-resolutions resolutions))

(defun remove-indirect-formals (resolutions)
  (or (remove-if-not
       #'(lambda (r)
	   (every #'(lambda (a)
			   (let ((name (or (type-value a)
					   (expr a))))
			     (or (not (name? name))
				 (eq (module (declaration name))
				     (theory *current-context*)))))
		       (actuals (module-instance r))))
       resolutions)
      resolutions))

(defun remove-generics (resolutions)
  (delete-if #'(lambda (r)
		 (and (null (actuals (module-instance r)))
		      (some@ #'(lambda (rr)
				(and (same-id (module-instance r)
					      (module-instance rr))
				     (actuals (module-instance rr))
				     (eq (declaration r)
					 (declaration rr))))
			    resolutions)))
	     resolutions))

(defmethod resolve ((name symbol) kind args &optional
		    (context *current-context*))
  (let* ((n (mk-name-expr name))
	 (res (resolve n kind args context)))
    (when (singleton? res)
      (setf (resolutions n) res)
      n)))

(defmethod resolve ((name name) kind args
		    &optional (context *current-context*))
  (let ((res (resolution name)))
    (if (resolution name)
	(when (eq (kind-of (declaration res)) kind)
	  (list res))
	(let* ((*current-context* context)
	       (nname (if (actuals name)
			  (copy-untyped name)
			  name))
	       (*get-all-resolutions* t))
	  (resolve* nname kind args)))))


;;; resolve-theory-name is called by the prover.  It returns a list of
;;; instances of the specified theory that are visible in the current
;;; context.
;;;   IF generic input
;;;   THEN IF the generic was imported,
;;;        THEN return it as a singleton list
;;;        ELSE return list of instances
;;;   ELSE IF the instance was imported
;;;        THEN return it as a singleton
;;;        ELSE IF the generic was imported,
;;;             THEN check the actuals, and return the singleton instance
;;;             ELSE return nil

(defun resolve-theory-name (modname)
  (if (eq (id modname) (id (theory *current-context*)))
      (if (actuals modname)
	  (type-error modname "May not instantiate the current theory")
	  modname)
      (if (get-theory modname)
	  (progn
	    (typecheck* modname nil nil nil)
	    (let* ((importings (gethash (get-theory modname)
					(using-hash *current-context*))))
	      (unless importings
		(type-error modname
		  "Theory ~a is not imported in the current context"
		  (id modname)))
	      (when (and (actuals modname)
			 (not (member modname importings :test #'tc-eq))
			 (not (find-if #'(lambda (mi) (null (actuals mi)))
				importings)))
		(type-error modname
		  "Theory instance ~a is not imported in the current context"
		  modname)))
	    modname)
	  (resolve-theory-abbreviation modname))))

(defun resolve-theory-abbreviation (theory-name)
  (let* ((abbrs (remove-if-not #'mod-decl?
		  (gethash (id theory-name) (current-declarations-hash)))))
    (cond ((null abbrs)
	   (type-error theory-name
	     "Theory ~a is not an abbreviation, not is it imported ~
              in the current context"
	     (id theory-name)))
	  ((singleton? abbrs)
	   (if (fully-instantiated? (modname (car abbrs)))
	       (modname (car abbrs))
	       (break "resolve-theory-abbreviation not fully-instantiated")))
	  (t (break "resolve-theory-abbreviation too many abbreviations")))))
    

(defmethod imported-theory-abbreviation (decl)
  (declare (ignore decl))
  nil)

(defmethod imported-theory-abbreviation ((decl mod-decl))
  (visible? decl))


(defun argument-conversion (name arguments)
  (when arguments
    (let ((reses (remove-if-not #'(lambda (r)
				    (typep (find-supertype (type r)) 'funtype))
		   (resolve name 'expr nil))))
      (when (argument-conversions (mapcar #'type reses) arguments)
	(let ((nreses (resolve name 'expr arguments)))
	  (when nreses
	    (setf (resolutions name) nreses)))))))

(defun argument-conversions (optypes arguments &optional found-one)
  (if optypes
      (let* ((rtype (find-supertype (car optypes)))
	     (dtypes (when (typep rtype 'funtype)
		       (domain-types rtype)))
	     (conversions (when (length= arguments dtypes)
			    (argument-conversions* arguments dtypes))))
	(when conversions
	  (mapc #'(lambda (arg convs)
		    (when convs
		      (setf found-one t)
		      (setf (types arg)
			    (remove-duplicates
				(append (types arg)
					(mapcar #'(lambda (c)
						    (get-conversion-range-type
						     c arg))
					  convs))
			      :test #'tc-eq))))
		arguments conversions))
	(argument-conversions (cdr optypes) arguments found-one))
      found-one))

(defun argument-conversions* (arguments dtypes &optional result)
  (if (null arguments)
      (unless (every #'null result)
	(nreverse result))
      (let* ((atypes (types (car arguments)))
	     (convs (unless (some #'(lambda (aty)
				      (or (tc-eq aty (car dtypes))
					  (and (compatible? aty (car dtypes))
					       (fully-instantiated?
						(car dtypes)))))
				  atypes)
		      (argument-conversions** atypes (car dtypes)))))
	(argument-conversions* (cdr arguments) (cdr dtypes)
			       (cons convs result)))))

(defun argument-conversions** (atypes etype &optional result)
  (if (null atypes)
      (nreverse result)
      (let ((conv (find-conversions-for (car atypes) etype)))
	(argument-conversions** (cdr atypes) etype
				(append conv result)))))

;;; This is called by typecheck* (name) when the resolution is nil.  If there
;;; are arguments, it first checks whether there is a conversion available that
;;; would turn the given name into a function of the right type.  If not, then
;;; it looks for a k-conversion so that it can push the conversion down on the
;;; arguments.

(defun function-conversion (name arguments)
  (when arguments
    (let ((creses (append (simple-function-conversion name arguments)
			  (let* ((resolutions (resolve name 'expr nil)))
			    (resolutions-with-argument-conversions
			     resolutions arguments)))))
      (when creses
	(setf (resolutions name) creses)))))

(defun resolutions-with-argument-conversions (resolutions arguments
							  &optional result)
  (if (null resolutions)
      result
      (let* ((rtype (find-supertype (type (car resolutions))))
	     (compats (compatible-arg-conversions? rtype arguments)))
	(resolutions-with-argument-conversions
	 (cdr resolutions)
	 arguments
	 (if (and compats
		  (some #'(lambda (x) (not (eq x 't))) compats))
	     (cons (make-function-conversion-resolution
		    (car resolutions) arguments compats)
		   result)
	     result)))))

(defun make-function-conversion-resolution (res args compats)
  (declare (ignore args))
  (let ((theories (delete *current-theory*
			  (delete-duplicates (mapcar #'module
						     (free-params res)))))
	(dtypes (domain-types (type res)))
	(nres res))
    (dolist (th theories)
      (mapc #'(lambda (dt compat)
		(when (typep compat 'name-expr)
		  (let ((bindings (tc-match
				   (domain (type compat)) dt
				   (mapcar #'(lambda (x)
					       (cons x nil))
				     (formals-sans-usings th)))))
		    (when (and bindings (every #'cdr bindings))
		      (setq nres
			    (subst-mod-params
			     nres
			     (mk-modname (id th)
			       (mapcar #'(lambda (a)
					   (mk-res-actual (cdr a) th))
				 bindings))))))))
	    dtypes compats))
    (change-class nres 'lambda-conversion-resolution)
    (setf (conversion nres) compats)
    ;;    (let ((ftype (mk-funtype (type res) (range (type (car compats))))))
    ;;      (break)
    ;;      (setf (type nres) ftype))
    nres))

(defun compatible-arg-conversions? (rtype arguments)
  (and (typep rtype 'funtype)
       (let ((dtypes (domain-types rtype)))
	 (and (length= dtypes arguments)
	      (compatible-arguments-k-conversions dtypes arguments)))))

(defun compatible-arguments-k-conversions (dtypes arguments &optional result)
  (if (null dtypes)
      (nreverse result)
      (let ((conv (compatible-argument-k-conversions
		   (car dtypes) (car arguments))))
	(when conv
	  (assert (or (eq conv t)
		      (typep conv 'name-expr)))
	  (compatible-arguments-k-conversions
	   (cdr dtypes) (cdr arguments) (cons conv result))))))

(defun compatible-argument-k-conversions (dtype argument)
  (or (some #'(lambda (pty)
		(compatible? dtype pty))
	    (ptypes argument))
      (compatible-argument-k-conversion dtype (ptypes argument))))

(defun compatible-argument-k-conversion (dtype ptypes)
  (when ptypes
    (let ((fty (find-supertype (car ptypes))))
      (or (and (typep fty 'funtype)
	       (compatible? (range fty) dtype)
	       (car (get-k-conversions fty)))
	  (compatible-argument-k-conversion dtype (cdr ptypes))))))

(defun k-conversion-resolutions (res kcs arguments &optional result)
  (if (null res)
      result
      (let ((nres (k-conversion-resolutions1 (car res) kcs arguments)))
	(k-conversion-resolutions (cdr res) kcs arguments
				(if nres (cons nres result) result)))))

(defun k-conversion-resolutions1 (res kcs arguments)
  (let ((rtype (find-supertype (type res))))
    (when (and (typep rtype 'funtype)
	       (length= (domain-types rtype) arguments))
      (let ((kc (find-if #'(lambda (kc)
			     (compatible-k-args kc (domain-types rtype) arguments))
		  kcs)))
	(when kc
	  (let ((sres (instantiate-resolution-from-k
		       res (domain (find-supertype (type kc))))))
	    (change-class sres 'conversion-resolution)
	  (setf (conversion sres) kc)
	  sres))))))

(defun instantiate-resolution-from-k (res domain)
  (if (fully-instantiated? (type res))
      res
      (let ((bindings (tc-match domain (domain (find-supertype (type res)))
				(mapcar #'(lambda (x) (cons x nil))
					(formals-sans-usings
					 (get-theory
					  (module-instance res)))))))
	(if (every #'cdr bindings)
	    (subst-mod-params res
			      (car (create-compatible-modinsts
				    (module-instance res)
				    (list bindings)
				    nil)))
	    res))))

(defun compatible-k-args (kc dtypes args)
  (let ((lt (car (domain-types (type kc))))
	(rt (range (type kc))))
    (compatible-k-args* lt rt dtypes args)))

(defun compatible-k-args* (lt rt dtypes args)
  (or (null dtypes)
      (and (some #'(lambda (pty)
		     (or (compatible? pty (car dtypes))
			 (and (compatible? pty rt)
			      (compatible? lt (car dtypes)))))
		 (ptypes (car args)))
	   (compatible-k-args* lt rt (cdr dtypes) (cdr args)))))

(defun matching-k-conversions (args &optional result)
  (if (null args)
      (remove-duplicates result :test #'tc-eq)
      (matching-k-conversions
       (cdr args)
       (append (matching-k-conversions1 (ptypes (car args)))
	       result))))

(defun matching-k-conversions1 (atypes &optional result)
  (if (null atypes)
      result
      (matching-k-conversions1
       (cdr atypes)
       (append (get-k-conversions (car atypes)) result))))

(defun simple-function-conversion (name arguments)
  (let ((reses (resolve name 'expr nil))
	(creses nil))
    (mapc #'(lambda (r)
	      (let ((conv (car (get-resolution-conversions r arguments))))
		(when conv
		  (assert (typep conv 'name-expr))
		  (push (make-conversion-resolution r conv name)
			creses))))
	  reses)
    creses))

(defun get-resolution-conversions (res arguments)
  (get-resolution-conversions*
   (remove-if #'k-combinator? (conversions *current-context*))
   res arguments nil))

(defun get-resolution-conversions* (conversions res arguments result)
  (if (null conversions)
      result
      (let ((conv (compatible-resolution-conversion (car conversions)
						    res arguments)))
	(get-resolution-conversions* (cdr conversions) res arguments
				     (if conv
					 (cons conv result)
					 result)))))

(defun compatible-resolution-conversion (conversion res arguments)
  (let ((nconv (compatible-operator-conversion conversion (type res) arguments)))
    (when nconv (name nconv))))

(defun compatible-resolution-conversion-args (bindings ctype arguments)
  (let* ((cran (find-supertype (range ctype)))
	 (dtypes (when (typep cran 'funtype)
		   (domain-types cran)))
	 (bindings-list (find-compatible-bindings arguments dtypes bindings)))
    (car bindings-list)))

(defun compatible-resolution-conversion? (ctype res arguments)
  (and (compatible? (domain ctype) (type res))
       (or (null arguments)
	   (let ((cran (find-supertype (range ctype))))
	     (and (typep cran 'funtype)
		  (let* ((dtypes (domain-types cran))
			 (args (if (and (singleton? dtypes)
					(not (singleton? arguments)))
				   (list (typecheck* (mk-arg-tuple-expr*
						      arguments)
						     nil nil nil))
				   arguments)))
		    (and (length= dtypes args)
			 (every #'(lambda (a dty)
				    (some #'(lambda (pty)
					      (compatible? pty dty))
					  (ptypes a)))
				args dtypes))))))))

(defun make-conversion-resolution (res conv name)
  (let* ((ctype (find-supertype (type conv)))
	 (dep? (typep (domain ctype) 'dep-binding))
	 (nname (when dep?
		  (copy name
		    'type (type res)
		    'resolutions (list res)
		    'kind (if (or (memq (declaration res) *bound-variables*)
				  (typep (declaration res)
					 '(or var-decl bind-decl dep-binding)))
			      'variable
			      'constant))))
	 (rtype (if dep?
		    (substit (range ctype)
		      (acons (domain ctype) nname nil))
		    (range ctype)))
	 (cconv (copy conv)))
    (change-name-expr-class-if-needed (declaration conv) cconv)
    (make-instance 'conversion-resolution
      'module-instance (module-instance res)
      'declaration (declaration res)
      'type (copy rtype 'from-conversion cconv)
      'conversion cconv)))

(defun get-conversion-range-type (conv expr)
  (let* ((ctype (find-supertype (type conv)))
	 (dep? (typep (domain ctype) 'dep-binding))
	 (nexpr (when dep?
		  (typecheck* (copy-untyped expr) (domain ctype) nil nil)))
	 (rtype (if dep?
		    (substit (range ctype)
		      (acons (domain ctype) nexpr nil))
		    (range ctype)))
	 (cconv (copy conv)))
    (change-name-expr-class-if-needed (declaration conv) cconv)
    (copy rtype 'from-conversion cconv)))


;;; Resolution error handling

(defun resolution-error (name kind arguments)
  (let ((reses (when (actuals name)
		 (resolve (copy name 'actuals nil) kind arguments))))
    (if (and *resolve-error-info*
	     (or (assq :arg-mismatch *resolve-error-info*)
		 (not (assq :no-instantiation *resolve-error-info*))))
	(resolution-args-error *resolve-error-info* name arguments)
	(type-error name
	  "Expecting a~a~%No resolution for ~a~@[ with arguments of types:~
       ~:{~%  ~a : ~{~a~^, ~}~}~]~:[~;~%Check the actual parameters~]"
	  (case kind
	    (expr "n expression")
	    (type " type")
	    (formula " formula")
	    (t kind))
	  name
	  (mapcar #'(lambda (a)
		      (list a (full-name (ptypes a) 1)))
		  arguments)
	  reses))))

(defun resolution-args-error (infolist name arguments)
  (when infolist
    (let ((info (car (best-guess-resolution-error infolist arguments))))
      (case (car info)
	(:arg-length
	 (type-error name
	   "Wrong number of arguments:~%  ~d provided, ~d expected"
	   (length arguments)
	   (length (if (typep (cadr info) 'expr)
		       (domain-types (type (cadr info)))
		       (car (formals (cadr info)))))))
	(:arg-mismatch
	 ;; Info = (:arg-mismatch decl argnum args types)
	 (type-error (car (fourth info))
	   "~:r argument to ~a has the wrong type~
          ~%     Found: ~{~a~%~^~12T~}  Expected: ~a"
	   (third info)
	   name
	   (mapcar #'(lambda (fn) (unpindent fn 12 :string t))
		   (full-name (ptypes (car (fourth info))) 1))
	   (full-name (car (fifth info)) 1)))))))

(defun best-guess-resolution-error (infolist arguments)
  (if (cdr infolist)
      (let ((mismatches (remove-if-not
			    #'(lambda (x) (eq (car x) :arg-mismatch))
			  infolist)))
	(if mismatches
	    (best-guess-mismatch-error mismatches arguments)
	    (best-guess-length-error infolist arguments)))
      infolist))

(defun best-guess-mismatch-error (infolist arguments &optional best bnum)
  (if (null infolist)
      best
      (let ((num (number-of-compatible-args (fifth (car infolist))
					    (fourth (car infolist))
					    (1- (third (car infolist))))))
	(best-guess-mismatch-error
	 (cdr infolist)
	 arguments
	 (cond ((or (null best)
		    (> num bnum))
		(list (car infolist)))
	       ((= num bnum)
		(cons (car infolist) best))
	       (t best))
	 (if (or (null bnum)
		 (> num bnum))
	     num
	     bnum)))))

(defun number-of-compatible-args (types args &optional (num 0))
  (if (null args)
      num
      (number-of-compatible-args
       (cdr types)
       (cdr args)
       (if (some #'(lambda (ptype)
		    (compatible-args**? ptype (car types)))
		(ptypes (car args)))
	   (1+ num)
	   num))))

(defun best-guess-length-error (infolist arguments)
  (declare (ignore arguments))
  infolist)
