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
		   (argument* argument)
		   argument))
	 (res (resolve* name k args)))
    (cond ((null res)
	   (or (and (eq k 'expr)
		    (or (argument-conversion name args)
			(function-conversion name args)))
	       (resolution-error name k args)))
	  ((and (cdr res)
		(not (eq k 'expr)))
	   (let ((nres (remove-if-not #'(lambda (r)
					  (if argument
					      (formals (declaration r))
					      (not (formals (declaration r)))))
			 res)))
	     (setf (resolutions name) nres)
	     (when (cdr nres)
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
	(unless (every #'typechecked? (actuals name))
	  (typecheck-actuals name))
	(unless (member name (assq theory (using *current-context*))
			:test #'tc-eq)
	  (set-type-actuals name)
	  (check-compatible-params (formals-sans-usings theory)
				   (actuals name) nil)))))

(defun set-argument-formals (res arguments)
  (mapc #'(lambda (r)
	    (set-argument-formals* (domain-types (find-supertype (type r)))
				   arguments))
	res))

(defun domain-types (type)
  (domain-types* (domain type)))

(defmethod domain-types* ((type dep-binding))
  (domain-types* (type type)))

(defmethod domain-types* ((type tupletype))
  (types type))

(defmethod domain-types* ((type type-expr))
  (list type))

(defun set-argument-formals* (domain arguments)
  (mapc #'set-arg-formals domain arguments))

(defun set-arg-formals (dom arg)
  (mapc #'(lambda (ty) (set-arg-formals* dom ty arg))
			(ptypes arg)))

(defun set-arg-formals* (dom type arg)
  (let ((frees (free-formals type)))
    (if frees
	(let ((bindings (mapcar #'(lambda (ff) (list (declaration ff)))
				frees)))
	  (tc-match dom type bindings)
	  (if (and bindings (every #'cdr bindings))
	      (subst-for-formals arg bindings)
	      type))
	type)))
	

(defmethod typechecked? ((act actual))
  (when (or (type-value act)
	    (and (typep (expr act) 'expr)
		 (type (expr act))))
    t))

(defmethod module (binding)
  (declare (ignore binding))
  *current-theory*)

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
	   (length (domain-types (type (cadr info))))))
	(:arg-mismatch
	 (type-error name
	   "~:r argument has the wrong type~
          ~%     Found: ~{~a~%~^~12T~}  Expected: ~a"
	   (caddr info)
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

(defun compatible-args*? (decl args types argnum)
  (or (null args)
      (if (some #'(lambda (ptype)
		    (compatible-args**? ptype (car types)))
		(ptypes (car args)))
	  (compatible-args*? decl (cdr args) (cdr types) (1+ argnum))
	  (progn (push (list :arg-mismatch decl argnum args types)
		       *resolve-error-info*)
		 nil))))

(defun best-guess-length-error (infolist arguments)
  (declare (ignore arguments))
  infolist)

(defun resolve* (name kind args)
  (typecheck-actuals name kind)
  (filter-preferences
   name
   (let* ((lmodinsts (get-modinsts-list name (using *current-context*)))
	  (res (delete-duplicates
		(nconc (match-record-arg-decls name kind args)
		       (match-local-decls name kind args)
		       (matching-decls name lmodinsts kind args nil))
		:test #'tc-eq))
	  (fres (delete-if #'disallowed-free-variable? res)))
     (if fres
	 (if (actuals name)
	     (let ((ares (delete-if #'(lambda (r)
					(eq (id (module-instance r))
					    (id *current-theory*)))
				    fres)))
	       (or ares
		   (type-error name
		     "May not provide actuals for entities defined locally")))
	     fres)
	 (when res
	   (type-error name "Free variables not allowed here"))))
   kind args))


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
	       (typechecked? act))
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
					   'expr (expr act))
					 nil nil nil))))
		   (when tres
		     (if (type-value act)
			 (unless (compatible? (type-value act) (type (car tres)))
			   (push (car tres) (resolutions (expr act)))
			   (type-ambiguity (expr act)))
			 (progn
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


;;; Returns a set of module instances of the same form as the using of a
;;; context, i.e. ((mod modname_1 ... modname_n) ... ), where each
;;; mod contains a declaration with the same id, and each modname_i is
;;; compatible with the name.

(defun get-modinsts-list (name using-list)
  (when (library name)
    (let ((lib (get-library-pathname (library name) *current-theory*)))
      (if lib
	  (setq using-list
		(remove-if-not #'(lambda (ulist)
				   (and (typep (car ulist) 'library-theory)
					(equal (pvs-truename (library
							      (car ulist)))
					       (pvs-truename lib))))
		  using-list))
	  (type-error name "Library ~a not found" (library name)))))
  (cond ((mod-id name)
	 (or (module-synonym-instance name)
	     (let ((modinsts (assoc (mod-id name)
				    (cons (list (module *current-context*)
						(mod-name *current-context*))
					  using-list)
				    :test #'(lambda (x y) (eq x (id y))))))
	       (if modinsts
		   (let ((mis (matching-modinsts
			       (id name) (actuals name) modinsts)))
		     (when mis (list mis)))
		   (type-error name "Theory ~a is not in the IMPORTINGs"
			       (mod-id name))))))
	((actuals name)
	 (get-modinsts-list* (id name) (actuals name) using-list nil))
	(t (or ;(gethash (id name) *id-to-modinsts*)
	       (let ((modinsts (get-modinsts-list* (id name) nil
						   using-list nil)))
		 ;(setf (gethash (id name) *id-to-modinsts*) modinsts)
		 modinsts)))))

(defun module-synonym-instance (name)
  (when (mod-id name)
    (let ((mdecl (find-if #'(lambda (d) (typep d 'mod-decl))
		   (gethash (mod-id name)
			    (local-decls *current-context*)))))
      (if mdecl
	  (list (list (get-theory (modname mdecl)) (modname mdecl)))
	  (used-mod-syn (mod-id name) (using *current-context*))))))

(defun used-mod-syn (mid usings)
  (when usings
    (let ((mdecl (find-if #'(lambda (d)
			      (and (typep d 'mod-decl)
				   (visible? d)))
		   (gethash mid (declarations (caar usings))))))
      (if mdecl
	  (list (cons (get-typechecked-theory (id (modname mdecl)))
		      (mapcar #'(lambda (u)
				  (subst-mod-params (modname mdecl) u))
			      (cdar usings))))
	  (used-mod-syn mid (cdr usings))))))

(defun get-modinsts-list* (id actuals lmodinsts result)
  (if (null lmodinsts)
      result
      (let ((modinsts (matching-modinsts id actuals (car lmodinsts))))
	(get-modinsts-list* id actuals (cdr lmodinsts)
			    (if modinsts (cons modinsts result) result)))))

;;; modinsts is of the form (#mod name1 name2 ...).  Finds those
;;; instances which match the module name, have a declaration with the
;;; right id, and match the actuals.

(defun matching-modinsts (id acts modinsts)
  (let ((mod (car modinsts)))
    ;; If it is in the current context, it will be handled by
    ;; match-local-decls
    (unless (eq mod (module *current-context*))
      (and (gethash id (declarations mod))
	   (if acts
	       (let ((minsts (or (and acts
				      (remove-if #'actuals (cdr modinsts)))
				 (cdr modinsts))))
		 (and (length= acts (formals-sans-usings mod))
		      (or (matching-actuals acts mod minsts nil)
			  (compatible-actuals acts mod minsts nil))))
	       modinsts)))))

(defun compatible-actuals (acts mod minsts result)
  (if (null minsts)
      (when result
	(cons mod
	      (mapcar #'(lambda (mi)
			  (copy mi 'actuals (mapcar #'copy (actuals mi))))
		result)))
      (compatible-actuals acts mod (cdr minsts)
			  (if (and (actuals (car minsts))
				   (compatible? acts (actuals (car minsts))))
			      (cons (car minsts) result)
			      result))))


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
  (when (and (length= actuals formals)
	     (every #'(lambda (a f)
			(if (typep f 'formal-type-decl)
			    (type-value a)
			    (ptypes (expr a))))
		    actuals formals))
    (compatible-parameters?* actuals formals)))

(defun compatible-parameters?* (actuals formals)
  (or (null actuals)
      (let ((type (compatible-parameter? (car actuals) (car formals))))
	(and type
	     (if (typep (car formals) 'formal-type-decl)
		 (compatible-parameters?*
		  (cdr actuals)
		  (subst-actual-in-remaining-formals actuals formals))
		 (some #'(lambda (ty)
			   (let ((nact (copy-untyped (car actuals))))
			     (typecheck* nact nil nil nil)
			     (set-type (expr nact) ty)
			     (compatible-parameters?*
			      (cdr actuals)
			      (subst-actual-in-remaining-formals
			       (cons nact (cdr actuals)) formals))))
		       type))))))

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
  nil)

(defmethod matching-actual-expr* (aex maex bindings)
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
	     (singleton? args))
    (let ((fdecls (mapcan #'(lambda (pty)
			      (let ((sty (find-supertype pty)))
				(when (typep sty 'recordtype)
				  (let ((fdecl (find name (fields sty)
						     :test #'same-id)))
				    (when fdecl
				      (push (cons fdecl sty) *field-records*)
				      (list fdecl))))))
			  (ptypes (car args))))
	  (modinsts (list (mod-name *current-context*))))
      (matching-decls* name fdecls modinsts args nil))))


;;; Returns the set of resolutions which match the name, in that it is the
;;; right kind and has compatible arguments.  Each pair is composed of a
;;; declaration and a name representing the module instance; thus the id
;;; is the module name, and the actuals specifies the instance.
;;; In the future, we may carry bindings as well, so that the actual
;;; parameters can be inferred.  For now, if the module has not been
;;; instantiated either in the USING clause or in the name itself, and
;;; the context doesn't eliminate the resulting pair, then it is an error.
;;;   name	- is the name to be resolved
;;;   kind	- is the kind of name expected (module, type, expr or field)
;;;   modnames	- is the names of the modules to search in
;;;   args	- the arguments provided if it is an application

(defun match-local-decls (name kind args)
  (when (and (or (null (mod-id name))
		 (eq (mod-id name) (id (mod-name *current-context*))))
	     (null (library name))
	     (or (null (actuals name))
		 (let ((formals (formals-sans-usings *current-theory*)))
		   (and (length= (actuals name) formals)
			(every #'matching-actual
			       (actuals name)
			       (make-list (length formals))
			       formals)))))
    #+pvsdebug (assert (every #'binding? *bound-variables*))
    (let* ((bvars (get-distinct-bound-variables *bound-variables* name))
	   (ldecls (append (gethash (id name)
				    (local-decls *current-context*))
			   (gethash (id name)
				    (local-proof-decls *current-context*))))
	   (decls (if bvars
		      (append bvars (remove-if #'var-decl? ldecls))
		      ldecls))
	   (pdecls (possible-decls (id name) decls kind))
	   (modinsts (list (mod-name *current-context*))))
      (matching-decls* name pdecls modinsts args nil))))

(defun get-distinct-bound-variables (bvars name &optional result)
  (if (null bvars)
      (nreverse result)
      (get-distinct-bound-variables
       (cdr bvars)
       name
       (if (and (eq (id (car bvars)) (id name))
		(not (eq (car bvars) name))
		(not (member (type (car bvars)) result
			     :test #'compatible? :key #'type)))
	   (cons (car bvars) result)
	   result))))

(defun matching-decls (name lmodinsts kind args result)
  (if (null lmodinsts)
      result
      (let* ((decls (gethash (id name) (declarations (caar lmodinsts))))
	     (pdecls (possible-decls (id name) decls kind))
	     (modinsts (cdar lmodinsts))
	     (res (matching-decls* name pdecls modinsts args nil)))
	(matching-decls name (cdr lmodinsts) kind args
			(if res (nconc result res) result)))))

(defun possible-decls (id decls kind &optional result)
  (if (null decls)
      result
      (possible-decls id (cdr decls) kind
		      (let ((d (car decls)))
			(if (and (kind-match (kind-of d) kind)
				 (or (memq d *bound-variables*)
				     (typep d 'binding)
				     (eq (module d) (module *current-context*))
				     (and (visible? d)
					  (can-use-for-assuming-tcc? d))))
			    (cons d result)
			    result)))))

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
				(module *current-context*))))
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
  (let ((dres (make-resolution db (mod-name *current-context*) rectype)))
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
		 (compatible-uninstantiated? decl modinst args)
		 (when (compatible-args?
			decl
			args
			(mapcar #'(lambda (dt)
				    (let ((*smp-include-actuals* t)
					  (*smp-dont-cache* t))
				      (if (actuals modinst)
					  (let ((*generate-tccs* 'none))
					    (subst-mod-params
					     dt (set-type-actuals modinst)))
					  dt)))
				dtypes))
		   (list modinst)))))))

(defmethod compatible-arguments? ((decl field-decl) modinst args mod)
  (declare (ignore mod))
  (cond ((null args) (list modinst))
	((memq decl *bound-variables*)
	 (let ((stype (find-supertype (type decl))))
	   (and (funtype? stype)
		(or (length= (domain-types stype) args)
		    (and (singleton? args)
			 (some@ #'(lambda (ty)
				    (let ((sty (find-supertype ty)))
				      (and (tupletype? sty)
					   (length= (types sty)
						    (domain-types stype)))))
				(ptypes (car args)))))
		(when (compatible-args?
		       decl
		       args
		       (mapcar #'(lambda (dt)
				   (if (actuals modinst)
				       (subst-mod-params dt modinst)
				       dt))
			       (domain-types stype)))
		  (list modinst)))))
	(t
	 (if (uninstantiated-theory? modinst)
	     (compatible-uninstantiated? decl (copy modinst) args)
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
		 (compatible-uninstantiated? decl modinst args)
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
	   (some@ #'(lambda (a)
		     (let ((d (declaration a)))
		       (and d
			    (typep d 'formal-decl)
			    (not (member d (formals mod))))))
		 (actuals modinst)))))

(defmethod declaration ((act actual))
  (if (type-value act)
      (when (typep (type-value act) 'type-name)
	(declaration (resolution (type-value act))))
      (when (typep (expr act) 'name-expr)
	(declaration (expr act)))))

(defmethod compatible-uninstantiated? (decl modinst args)
  (let ((bindings (find-compatible-bindings
		   args
		   (if (singleton? args)
		       (list (find-supertype (domain (type decl))))
		       (mapcar #'find-supertype (domain-types (type decl))))
		   (mapcar #'list
			   (formals-sans-usings (module decl))))))
    (or (create-compatible-modinsts modinst bindings nil)
	(progn (push (list :no-instantiation decl)
		     *resolve-error-info*)
	       nil))))

(defmethod compatible-uninstantiated? ((decl type-decl) modinst args)
  (let ((bindings (find-compatible-bindings
		   args
		   (mapcar #'(lambda (fd) (find-supertype (type fd)))
			   (car (formals decl)))
		   (mapcar #'list
			   (formals-sans-usings (get-theory modinst))))))
    (create-compatible-modinsts modinst bindings nil)))

(defun create-compatible-modinsts (modinst bindings result)
  (if (null bindings)
      result
      (create-compatible-modinsts
       modinst (cdr bindings)
       (cond ((every #'cdr (car bindings))
	      (cons (copy modinst
		      'actuals (mapcar #'(lambda (a)
					   (mk-res-actual (cdr a) modinst))
				       (car bindings)))
		    result))
;	     ((some #'cdr (car bindings))
;	      (cons (copy modinst
;		      'actuals (mapcar #'(lambda (a)
;					   (when (cdr a)
;					     (mk-res-actual (cdr a) modinst)))
;				       (car bindings)))
;		    result))
	     (t (cons (copy modinst) result))))))

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
	  (progn (push (list :arg-mismatch decl argnum (car types))
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

(defun disallowed-free-variable? (res)
  (and (not *allow-free-variables*)
       ;;(not *generating-tcc*)
       (typep (declaration res) 'var-decl)
       (not (typep (declaration *current-context*) 'formula-decl))
       (not (member (declaration res) *bound-variables*
		    :test #'(lambda (x y) (eq (id x) (id y)))))))


;;; Filter-preferences returns a subset of the list of decls, filtering
;;; out declarations which are not as preferred as others.  Given
;;; declarations D1 and D2 of types T1 and T2, D1 is prefered to D2 if:
;;;   
;;;   T1 is a subtype of T2, or

(defun filter-preferences (name reses kind args)
  (let ((res (or (remove-if #'(lambda (r)
				(typep (module-instance r) 'datatype-modname))
		   reses)
		 reses)))
    (if (eq kind 'expr)
	(if (cdr res)
	    (let* ((bres (filter-bindings res))
		   (nres (if (and args (cdr bres))
			     (find-best-resolutions name res args)
			     bres)))
	      nres)
	    res)
	(remove-outsiders (remove-generics res)))))

(defun filter-bindings (reses)
  (or (delete-if-not #'(lambda (r) (typep (declaration r) 'bind-decl))
	reses)
      reses))

(defun find-best-resolutions (op reses args)
  (let ((mreses (or (remove-if-not
			#'(lambda (r)
			    (let ((dtypes (if (cdr args)
					      (domain-types (type r))
					      (list (domain (type r))))))
			      (every #'(lambda (arg dtype)
					 (some #'(lambda (ty)
						   (tc-eq ty dtype))
					       (types arg)))
				     args dtypes)))
		      reses)
		    reses)))
    (multiple-value-bind (ires gres)
	(split-on #'fully-instantiated? mreses)
      (let* ((res-part (partition-on-same-ranges ires nil))
	     (breses (find-best-resolutions* op res-part args nil))
	     (cres-part (partition-on-compatible-ranges breses nil)))
	(nconc (mapcan #'filter-local-resolutions cres-part) gres)))))

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

(defun partition-on-compatible-ranges (reses partition)
  (if (null reses)
      (mapcar #'cdr partition)
      (let* ((range (range (find-supertype (type (car reses)))))
	     (part (assoc range partition :test #'compatible?)))
	(cond (part
	       (nconc part (list (car reses)))
	       (partition-on-compatible-ranges (cdr reses) partition))
	      (t (partition-on-compatible-ranges
		  (cdr reses)
		  (acons range (list (car reses)) partition)))))))

(defun find-best-resolutions* (op res-part args reses)
  (if (null res-part)
      reses
      (let ((res (find-best-operator-resolution op (car res-part) args)))
	(find-best-resolutions* op (cdr res-part) args (cons res reses)))))

(defun find-best-operator-resolution (op reses args)
  (if (cdr reses)
      (let ((lreses (filter-local-resolutions reses)))
	(if (cdr lreses)
	    (let ((optype (find-best-operator-type
			   op args
			   (mapcar #'(lambda (r) (find-supertype (type r)))
				   lreses)
			   lreses)))
	      (if (typep optype 'resolution)
		  optype
		  (let* ((breses (remove-if-not
				     #'(lambda (r)
					 (tc-eq (find-supertype (type r))
						optype))
				   reses))
			 (ngreses (or (remove-generics breses) breses)))
		    (if (cdr ngreses)
			(let ((freses (or (remove-redeclared-field-decls
					   ngreses)
					  ngreses)))
			  (cond ((cdr freses)
				 (setf (resolutions op) freses)
				 (type-ambiguity op))
				(t (car freses))))
			(car ngreses)))))
	    (car lreses)))
      (car reses)))

(defun remove-redeclared-field-decls (reses &optional result)
  (cond ((null reses)
	 (nreverse result))
	((and (typep (declaration (car reses)) 'const-decl)
	      (definition (declaration (car reses)))
	      (or (member (car reses) (cdr reses)
			  :test #'redeclared-field-decl)
		  (member (car reses) result
			  :test #'redeclared-field-decl)))
	 (remove-redeclared-field-decls (cdr reses) result))
	(t (remove-redeclared-field-decls (cdr reses)
					  (cons (car reses) result)))))

(defun redeclared-field-decl (res1 res2)
  (and (tc-eq (type res1) (type res2))
       (typep (declaration res2) 'field-decl)
       (let ((def (args2 (car (last (def-axiom (declaration res1)))))))
	 (and (typep def 'lambda-expr)
	      (typep (expression def) 'field-application)
	      (eq (id (expression def)) (id (declaration res2)))
	      (typep (argument (expression def)) 'name-expr)
	      (eq (declaration (argument (expression def)))
		  (car (bindings def)))))))

(defun partition-on-compatible-ranges (reses partition)
  (if (null reses)
      (mapcar #'cdr partition)
      (let* ((range (range (find-supertype (type (car reses)))))
	     (part (assoc range partition :test #'compatible?)))
	(cond (part
	       (nconc part (list (car reses)))
	       (partition-on-compatible-ranges (cdr reses) partition))
	      (t (partition-on-compatible-ranges
		  (cdr reses)
		  (acons range (list (car reses)) partition)))))))

(defun compatible-wrt-freeparams (ty1 ty2)
  (and (compatible? ty1 ty2)
       (let ((fp1 (free-params ty1))
	     (fp2 (free-params ty2)))
	 (or (subsetp fp1 fp2)
	     (subsetp fp2 fp1)))))
		 
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
				     (module *current-context*)))))
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

(defun filter-preferences* (res resolutions args result)
  (if (null res)
      result
      (if (or (member res resolutions
		      :test #'(lambda (x y)
				(less-preferred? x y)))
	      (member res result
		      :test #'(lambda (x y)
				(less-preferred? x y))))
	  (filter-preferences* (car resolutions) (cdr resolutions)
			       args result)
	  (filter-preferences* (car resolutions) (cdr resolutions)
			       args (cons res result)))))

(defun less-preferred? (r1 r2)
  (let ((d1 (declaration r1))
	(d2 (declaration r2)))
    (or (and (not (typep d1 'bind-decl))
	     (typep d2 'bind-decl))
	(and (memq d2 *bound-variables*)
	     (not (memq d1 *bound-variables*)))
	(and (tc-eq (type r1) (type r2))
	     (eq (module d2) *current-theory*)
	     (not (eq (module d1) *current-theory*))))))

(defmethod resolve ((name symbol) kind args &optional (context *current-context*))
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
	       (reses (resolve* nname kind args)))
	  reses))))


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
  (if (eq (id modname) (id (module *current-context*)))
      (if (actuals modname)
	  (type-error modname "May not instantiate the current theory")
	  modname)
      (if (get-theory modname)
	  (progn
	    (typecheck* modname nil nil nil)
	    (let* ((importings (cdr (or (assq (get-theory modname)
					      (using *current-context*))
					(assoc modname
					       (using *current-context*)
					       :test #'same-id)))))
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

(defun resolve-theory-abbreviation (modname)
  (let ((thabbr (find-if #'(lambda (d) (typep d 'mod-decl))
		  (gethash (id modname) (local-decls *current-context*)))))
    (if thabbr
	(if (actuals modname)
	    (type-error modname
	      "May not provide actuals for entities defined locally")
	    (modname thabbr))
	(resolve-imported-theory-abbreviation modname
					      (using *current-context*)))))

(defun resolve-imported-theory-abbreviation (modname importings)
  (when importings
    (let ((mdecl (find-if #'imported-theory-abbreviation
		   (gethash (id modname) (declarations (caar importings))))))
      (if mdecl
	  (cond ((actuals modname)
		 (resolve-theory-name (copy (modname mdecl)
					'actuals (actuals modname))))
		((singleton? (cdar importings))
		 (resolve-theory-name (subst-mod-params (modname mdecl)
							(cadar importings))))
		(t (resolve-theory-name (modname mdecl))))
	  (resolve-imported-theory-abbreviation modname (cdr importings))))))

(defmethod imported-theory-abbreviation (decl)
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
  (get-resolution-conversions* (conversions *current-context*)
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

