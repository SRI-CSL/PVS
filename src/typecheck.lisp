;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typecheck.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 19:01:35 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 17:41:07 1998
;; Update Count    : 35
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

(export '(typecheck typecheck*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top level typechecking functions

(defmethod typecheck ((m module) &key expected context tccs)
  (declare (ignore expected context tccs))
  (let ((*generate-tccs* 'ALL))
    (typecheck* m nil nil nil)))


;;; Typecheck, returning the original object.  The gen-tccs keyword indicates
;;; that all tccs must be generated, even if the expression is fully
;;; typechecked.  This is for the prover, which may be using the formula in
;;; different contexts.

(defmethod typecheck (obj &key expected (context *current-context*)
			  (tccs nil given))
  (assert context)
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*current-theory* (current-theory))
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (assert *generate-tccs*)
    (typecheck* obj expected nil nil))
  obj)


;;; Typecheck, returning the canonical form

(defmethod typecheck ((te type-expr) &key expected (context *current-context*)
		      (tccs nil given))
  (assert context)
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*current-theory* (current-theory))
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (typecheck* te expected 'type nil)))

(defmethod typecheck ((ex expr) &key expected (context *current-context*)
		      (tccs nil given))
  (assert context)
  ;;(assert (or (not given) expected (type ex)))
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*current-theory* (current-theory))
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (assert *generate-tccs*)
    (cond ((type ex)
	   (cond ((eq *generate-tccs* 'all)
		  (call-next-method))
		 (expected
		  (set-type ex (or expected (type ex))))))
	  (t (call-next-method)
	     (unless (or expected (type ex))
	       (let ((type (get-unique-type ex)))
		 (when type
		   (set-type ex type))))))
    ex))

(defvar *empty-expression-types* (make-hash-table :test 'eq))

(defmethod typecheck :around (obj &key expected context tccs)
  (declare (ignore expected context tccs))
  (protect-types-hash obj (call-next-method)))

(defmethod types ((ex expr))
  (gethash ex *expression-types*))

(defmethod (setf types) (types (ex expr))
;   (when (some #'(lambda (ty)
; 		  (not (subsetp (freevars ty) *bound-variables*
; 				:test #'same-declaration)))
; 	      types)
;     (break "setf types has freevars"))
  (setf (gethash ex *expression-types*) types))

(defmethod get-unique-type ((ex name-expr))
  (if (singleton? (resolutions ex))
      (let ((type (type (car (resolutions ex)))))
	(when (fully-instantiated? type)
	  type))
      (call-next-method)))

(defmethod get-unique-type ((ex expr))
  (let ((types (remove-duplicates (types ex) :test #'tc-eq)))
    (when (and (singleton? types)
	       (fully-instantiated? (car types)))
      (car types))))

(defun typecheck-uniquely (expr &key (tccs 'all given))
  (let ((*generate-tccs* (if given tccs *generate-tccs*)))
    (typecheck* expr nil nil nil)
    (cond ((and (null (type expr))
		(not (every #'(lambda (ty)
				(compatible? ty (car (types expr))))
			    (cdr (types expr)))))
	   (unless *suppress-printing*
	     (if (types expr)
		 (type-ambiguity expr)
		 (type-error expr
		   "~%Given expression does not typecheck uniquely.~%")))
	   (type-ambiguity expr))
	  ((not (fully-instantiated? (car (types expr))))
	   (unless *suppress-printing*
	     (type-error expr
	       "Could not determine the full theory instance")))
	  (t (set-type expr (car (types expr))))))
  expr)


;;; Typecheck* methods for theories - returns the theory

(defmethod typecheck* ((m module) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (and (memq 'typechecked (status m))
	       (typechecked? m))
    (let ((*subtype-of-hash* (make-hash-table :test #'eq)))
      (tcdebug "~%Typecheck ~a" (id m))
;       (setf (declarations m)
; 	    (make-hash-table :test #'eq :size (length (theory m))))
      (setf (formals-sans-usings m)
	    (remove-if #'(lambda (ff) (typep ff 'importing)) (formals m)))
      (let* ((*current-theory* m)
	     (*typechecking-module* t)
	     (*tccs* nil)
	     (*tccdecls* nil)
	     (*tccforms* nil)
	     (*current-context* (if (eq (current-theory) m)
				    *current-context*
				    (make-new-context m))))
	(tcdebug "~%  Processing formals")
	(typecheck-decls (formals m))
	(set-dependent-formals (formals-sans-usings m))
	(tcdebug "~%  Processing assuming")
	(when (and (assuming m)
		   (null (formals-sans-usings m)))
	  (type-error m
	    "Theory ~a has no formal parameters, hence no need for ASSUMING section"
	    (id m)))
	(typecheck-decls (assuming m))
	(tcdebug "~%  Processing theory")
	(typecheck-decls (theory m))
;	(maphash #'(lambda (id decls)
;		     (let ((ndecls (remove-if #'formal-decl? decls)))
;		       (when ndecls
;			 (setf (gethash id (declarations m)) ndecls))))
;		 (local-decls *current-context*))
	(tcdebug "~%  Processing exporting")
	(generate-xref m)
	(assert (eq (current-theory) m))
	(check-exporting m)
       (setf (all-usings m)
	     (let ((imps nil))
	       (maphash #'(lambda (th thinsts)
			    (unless (from-prelude? th)
			      (push (cons th thinsts) imps)))
			(using-hash *current-context*))
	       imps))
	(setf (saved-context m) *current-context*)
	;;(reset-fully-instantiated-cache)
	)
      (push 'typechecked (status m))
      m)))

(defun set-dependent-formals (formals)
  (mapc #'(lambda (f1)
	    (typecase f1
	      (formal-subtype-decl
	       (when (some #'(lambda (f2)
			       (occurs-in f2 (type-value f1)))
			   formals)
		 (setf (dependent? f1) t)))
	       
	      (formal-const-decl
	       (when (some #'(lambda (f2)
			       (occurs-in f2 (type f1)))
			   formals)
		 (setf (dependent? f1) t)))))
	formals))

;(defun adt-generating-theory (theory)
;  (unless *generating-adt*
;    (let ((cmt (comment theory)))
;      (when (and cmt (string= cmt "% Generated from file " :end1 22))
;	(let ((*typechecking-module* nil))
;	  (typecheck-file (subseq cmt 22 (- (length cmt) 4))))))))

(defmethod typecheck* ((list list) expected kind args)
  (typecheck*-list list expected kind args))

(defun typecheck*-list (list expected kind args &optional result)
  (if (null list)
      (nreverse result)
      (let ((obj (typecheck* (car list) expected kind args))
	    (*bound-variables* (cond ((binding? (car list))
				      (cons (car list) *bound-variables*))
				     ((and (listp (car list))
					   (every #'binding? (car list)))
				      (append (car list) *bound-variables*))
				     (t *bound-variables*))))
	(typecheck*-list (cdr list) expected kind args
			 (cons obj result)))))


(defmethod typecheck* ((use importing) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck-using (theory-name use))
  (setf (saved-context use) (copy-context *current-context*))
  use)

(defun typecheck-using (theory-inst)
  (if (and (null (library theory-inst))
	   (eq (id theory-inst) (id (current-theory))))
      (type-error theory-inst "A theory may not import itself")
      (let ((mod (get-typechecked-theory theory-inst)))
	(typecheck-using* mod theory-inst))))

(defvar *ignore-exportings* nil)

(defvar *typecheck-using* nil)

(defmethod typecheck-using* (obj inst)
  (declare (ignore obj))
  (type-error inst "Theory ~a not found" (id inst)))

(defmethod typecheck-using* ((mod module) inst)
  (let ((nmodinst inst)
	(*typecheck-using* inst))
    (cond ((actuals inst)
	   (unless (length= (formals-sans-usings mod) (actuals inst))
	     (type-error inst "Wrong number of actuals in ~a" inst))
	   (typecheck-actuals inst)
	   (typecheck-mappings (mappings inst) inst)
	   (setq nmodinst (set-type-actuals inst))
	   (check-compatible-params (formals-sans-usings mod)
				    (actuals inst) nil))
	  (t (typecheck-mappings (mappings inst) inst)
	     (setq nmodinst (set-type-actuals inst))))
    (add-to-using nmodinst)
    (unless (eq nmodinst inst)
      (pushnew inst (gethash (get-theory inst) (current-using-hash))))
    (when (some #'(lambda (m) (mod-decl? (declaration (lhs m))))
		(mappings nmodinst))
      (add-theory-mappings-importings mod nmodinst))
    (when (some #'formal-theory-decl? (formals mod))
      (add-theory-parameters-importings mod nmodinst))
    (unless *ignore-exportings*
      (add-exporting-with-theories mod nmodinst))))

(defun add-theory-parameters-importings (theory inst)
  (when (and (formals-sans-usings theory)
	     (actuals inst))
    (mapc #'(lambda (fm act)
	      (when (formal-theory-decl? fm)
		(add-to-using (mk-modname (id (expr act))
				(actuals (expr act))
				(library (expr act))
				(mappings (expr act)))
			      (get-theory (expr act)))))
	  (formals-sans-usings theory)
	  (actuals inst))))

(defun add-theory-mappings-importings (theory inst)
  (mapc #'(lambda (map)
	    (when (mod-decl? (declaration (lhs map)))
	      (add-to-using (mk-modname (id (expr (rhs map)))
			      (actuals (expr (rhs map)))
			      (library (expr (rhs map)))
			      (mappings (expr (rhs map))))
			    (get-theory (expr (rhs map))))))
	(mappings inst)))

(defmethod typecheck-using* ((adt recursive-type) inst)
  (let* ((th1 (adt-theory adt))
	 (th2 (adt-map-theory adt))
	 (th3 (adt-reduce-theory adt))
	 (use1 (copy inst 'id (id th1)))
	 (use2 (when th2 (copy inst 'id (id th2) 'actuals nil)))
	 (use3 (copy inst 'id (id th3) 'actuals nil))
	 (*typecheck-using* inst))
    (typecheck-using use1)
    (let ((*ignore-exportings* t)
	  (supinst (adt-modinst use1)))
      (mapc #'typecheck-using
	    `(,@(unless (eq supinst inst) (list supinst))
		,@(when use2 (list use2))
		,use3)))))

;;; Handles EXPORTING WITH clauses.  For example,
;;;
;;; m: THEORY [t:TYPE, c:t]		m1: THEORY [s:TYPE, a:s]
;;;   USING m1[t,c]			   USING m2[s]
;;;					   EXPORTING ALL WITH m2[s]
;;;
;;; The using list generated for m should include (#m2 m2[t])
;;; The function will be called with (#m1 m1[t]), (#m2 m2[t])
;;; The theory is associated with the inst (i.e., they have the same id)

(defun add-exporting-with-theories (theory inst)
  (when (exporting theory)
    (dolist (ename (closure (exporting theory)))
      (let* ((itheory (get-theory ename))
	     (lname (if (typep itheory 'library-theory)
			(copy ename
			  'library
			  (makesym "~a"
				   (cdr (assoc (library itheory)
					       *library-alist*
					       :test #'equal))))
			ename))
	     (iname (if (actuals inst)
			(subst-mod-params lname inst)
			lname)))
	(assert itheory)
	(unless (and (formals-sans-usings itheory) (null (actuals iname)))
	  ;; Add this to the assuming-instances list if fully instantiated
	  (pushnew iname (assuming-instances (current-theory))
		   :test #'tc-eq))
	(add-to-using iname itheory)))))


;;; Returns all of the theorynames directly used by the specified
;;; theory, either through USINGs or MOD-DECLs.  Note that when a datatype
;;; is referenced, it is replaced by (instances of) its generated
;;; theories.

(defmethod get-immediate-usings ((theory module))
  (with-slots (immediate-usings formals assuming (theory-part theory)) theory
    (if (eq immediate-usings 'unbound)
	(setf immediate-usings
	      (mapcan #'(lambda (thname)
			  (let ((th (get-theory thname)))
			    (or (and (typep th 'recursive-type)
				     (datatype-instances thname))
				(list thname))))
		(mapcar #'theory-name
		  (remove-if-not #'mod-or-using?
		    (all-decls theory)))))
	immediate-usings)))

(defmethod get-immediate-context-usings ((theory module))
  (mapcan #'(lambda (thname)
	      (unless (library thname)
		(let ((th (get-theory thname)))
		  (or (and (typep th 'recursive-type)
			   (datatype-instances thname))
		      (list thname)))))
    (mapcar #'theory-name
      (remove-if-not #'mod-or-using?
	(all-decls theory)))))

(defmethod theory-name ((mdecl mod-decl))
  (modname mdecl))

(defun datatype-instances (imported-adt)
  (let* ((adt (get-theory imported-adt))
	 (th1 (adt-theory adt))
	 (th2 (adt-map-theory adt))
	 (th3 (adt-reduce-theory adt)))
    (when th1
      (nconc (list (mk-modname (id th1)
		     (when (actuals imported-adt)
		       (ldiff (actuals imported-adt)
			      (nthcdr (length (formals-sans-usings adt))
				      (actuals imported-adt))))))
	     (when th2
	       (list (mk-modname (id th2))))
	     (when th3
	       (list (mk-modname (id th3))))))))

(defmethod get-immediate-usings ((adt recursive-type))
  (append (mapcar #'theory-name
	    (remove-if-not #'mod-or-using?
	      (append (formals adt)
		      (assuming adt))))
	  (when (importings adt)
	    (mapcar #'theory-name (importings adt)))))

(defmethod get-immediate-context-usings ((adt recursive-type))
  (append (mapcar #'theory-name
	    (remove-if-not #'mod-or-using?
	      (append (formals adt)
		      (assuming adt))))
	  (when (importings adt)
	    (mapcan #'(lambda (imp)
			(unless (library (theory-name imp))
			  (list (theory-name imp))))
	      (importings adt)))))

(defun mod-or-using? (obj)
  (typep obj '(or mod-decl theory-abbreviation-decl importing)))

(defmethod modules ((decl mod-decl))
  (list (modname decl)))

(defmethod modules ((decl theory-abbreviation-decl))
  (list (theory-name decl)))


;;; Perform the substitution.  In the above, would be called with
;;; (m1[t,c] #m1 m2[s]) and return m2[t].

(defun subst-actuals (inst theory target-inst)
  (let* ((etheory (subst-mod-params target-inst inst))
	 (actuals (subst-actuals* inst
				  (formals-sans-usings theory)
				  (actuals etheory)
				  nil)))
    (if (equal actuals (actuals etheory))
	etheory
	(mk-modname (id etheory) actuals))))

(defun subst-actuals* (inst formals actuals result)
  (if (null actuals)
      (nreverse result)
      (let* ((pos (if (name-expr? (expr (car actuals)))
		      (position (expr (car actuals)) formals :test #'same-id)))
	     (nactual (or (and pos
			       (nth pos (actuals inst)))
			  (car actuals))))
	(subst-actuals* inst formals (cdr actuals) (cons nactual result)))))


;;; The using list of a context has the form
;;;   ((theory theoryname_1 ... theoryname_n) ... )
;;; where theory is a theory and the theoryname_i's are the theory instances
;;; This is the form that is most convenient in resolving names.

(defun add-to-using (theoryname &optional itheory)
  (assert *current-context*)
  (let ((theory (or itheory (get-typechecked-theory theoryname))))
    (unless theory
      (type-error theoryname "Theory ~a not found" (id theoryname)))
    (let ((entry (gethash theory (current-using-hash)))
	  (tname (remove-coercions
		  (remove-indirect-formals-of-name theoryname))))
      #+pvsdebug
      (assert (or (null entry)
		  (every #'(lambda (d)
			     (if (and (declaration? d) (visible? d))
				 (memq d (gethash (id d)
						  (current-declarations-hash)))
				 t))
			 (theory theory))))
      (if entry
	  (unless (member tname entry :test #'tc-eq)
	    (if (actuals tname)
		(let ((imps (get-immediate-usings (current-theory))))
		  (setf (gethash theory (current-using-hash))
			(nconc (delete-if
				   #'(lambda (imp)
				       (and (actuals imp)
					    (not (fully-instantiated? imp))
					    (member (id imp) imps :key #'id)
					    (not (member imp imps
							 :test #'tc-eq))))
				 entry)
			       (list tname))))
		(nconc entry (list tname)))
	    (update-current-context theory theoryname))
	  (progn (update-current-context theory theoryname)
		 (dolist (decl (append (assuming theory) (theory theory)))
		   (when (and (declaration? decl)
			      (visible? decl)
			      (not (find decl (mappings theoryname)
					 :key #'(lambda (m)
						  (declaration (lhs m))))))
		     (check-for-importing-conflicts decl)
		     (put-decl decl (current-declarations-hash))))
		 (setf (gethash theory (current-using-hash))
		       (list tname)))))))

(defmethod check-for-importing-conflicts ((decl lib-decl))
  (let ((lib (get-library-pathname (library decl))))
    (dolist (d (gethash (id decl) (current-declarations-hash)))
      (when (and (lib-decl? d)
		 (not (string= lib (get-library-pathname (library d)))))
	(if (= (locality d) (locality decl))
	    (pvs-warning
		"Library id ~a declared in imported theory ~a and ~a ~
               with a different path.~%References to this library id will ~
               lead to ambiguity errors."
	      (id decl) (id (module d)) (id (module decl)))
	    (pvs-warning 
		"Library id ~a declared in imported theories ~a and ~a ~
               with a different path.~%References to this library id will ~
               use the path~%  ~a"
	      (id decl) (id (module d)) (id (module decl))
	      (if (< (locality d) (locality decl))
		  (library d)
		  (library decl))))))))

(defmethod check-for-importing-conflicts (decl)
  (declare (ignore decl))
  nil)

(defun update-current-context (theory theoryname)
  (assert (saved-context theory))
  (update-known-subtypes theory theoryname)
  (update-judgements-of-current-context theory theoryname)
  (update-conversions-of-current-context theory theoryname))

(defun update-conversions-of-current-context (theory theoryname)
  (dolist (conversion (conversions (saved-context theory)))
    (when (eq (module conversion) theory)
      (pushnew (subst-params-decl conversion theoryname)
	       (conversions *current-context*)
	       :test #'eq)))
  (dolist (conversion (disabled-conversions (saved-context theory)))
    (when (eq (module conversion) theory)
      (pushnew (subst-params-decl conversion theoryname)
	       (disabled-conversions *current-context*)
	       :test #'eq))))

(defmethod subst-params-decl ((c conversion-decl) modinst)
  (lcopy c
    'name (subst-mod-params (name c) modinst)))

(defmethod subst-params-decl ((c typed-conversion-decl) modinst)
  (lcopy (call-next-method)
    'declared-type (subst-mod-params (declared-type c) modinst)))

;;; Remove formals that are not a part of the current module.  This
;;; handles the following circumstance:
;;;
;;; t1[t:TYPE]:THEORY     t2[tt:TYPE]:THEORY         t3: THEORY
;;;   ...                   EXPORTING ALL WITH t1      USING t2
;;;   ...                   USING t1[tt]
;;;
;;; So in typechecking t3, add-to-using is called with t1[tt] and
;;; returns t1.

(defun remove-indirect-formals-of-name (theoryname)
  (if (and (actuals theoryname)
	   (every #'(lambda (a)
		      (let ((aval (or (type-value a) (expr a))))
			(and (name? aval)
			       (formal-decl? (declaration aval))
			       (not (eq (module (declaration aval))
					(current-theory))))))
		  (actuals theoryname)))
      (copy theoryname 'actuals nil)
      theoryname))

(defun check-compatible-params (formals actuals assoc)
  (or (null formals)
      (and (check-compatible-param (car formals) (car actuals) assoc)
	   (check-compatible-params
	    (cdr formals) (cdr actuals)
	    (acons (car formals)
		   (if (formal-type-decl? (car formals))
		       (type-value (car actuals))
		       (expr (car actuals)))
		   (if (formal-subtype-decl? (car formals))
		       (acons (find-if #'(lambda (c) (typep c 'const-decl))
				(generated (car formals)))
			      (subtype-pred (type-value (car actuals))
					    (subst-types
					     (supertype (type-value
							 (car formals)))
					     assoc))
			      assoc)
		       assoc))))))

(defun check-compatible-param (formal actual assoc)
  (typecase formal
    (formal-type-decl
     (unless (type-value actual)
       (type-error actual "Expression provided where a type is expected"))
     (when (formal-subtype-decl? formal)
       (let ((type (subst-types (supertype (type-value formal)) assoc)))
	 (unless (compatible? (type-value actual) type)
	   (type-error actual "~a Should be a subtype of ~a"
		       (type-value actual) type)))))
    (formal-theory-decl
     (unless (typep (declaration (expr actual))
		    '(or module mod-decl theory-abbreviation-decl
		         formal-theory-decl))
       (type-error actual "Theory name expected here")))
    (t (let ((type (subst-types (type formal) assoc)))
	 (typecheck* (expr actual) type nil nil))))
  t)

(defun subst-types (type assoc)
  (if assoc
      (gensubst type
	#'(lambda (te) (cdr (assoc (declaration te) assoc)))
	#'(lambda (te) (and (name? te)
			    (assoc (declaration te) assoc))))
      type))

(defmethod typecheck-mappings (mappings (inst modname))
  (when mappings
    (let ((lhs-theory (get-theory inst)))
      (unless lhs-theory
	(type-error inst "Theory ~a not found" inst))
      (let ((lhs-context (context lhs-theory)))
	(dolist (mapping mappings)
	  (let* ((*current-theory* lhs-theory)
		 (*current-context* lhs-context)
		 (*generate-tccs* 'none)
		 (tres (with-no-type-errors
			(resolve* (lhs mapping) 'type nil)))
		 (eres (with-no-type-errors
			(resolve* (lhs mapping) 'expr nil)))
		 (thres (with-no-type-errors
			 (resolve* (lhs mapping) 'module nil))))
	    (unless (or eres tres thres)
	      (type-error (lhs mapping) "Map lhs does not resolve"))
	    (if (cdr tres)
		(cond (eres
		       (setf (resolutions (lhs mapping)) eres))
		      (t (setf (resolutions (lhs mapping)) tres)
			 (type-ambiguity (lhs mapping))))
		(setf (resolutions (lhs mapping)) (nconc tres eres thres)))
	    (assert (resolutions (lhs mapping))))
	  (typecheck-mapping-rhs (rhs mapping))
	  ;;(assert (ptypes (expr (rhs mapping))))
	  )))))

(defmethod typecheck-mappings (mappings (name name))
  ;; Used with a name that has no mod-id - don't try to typecheck lhs in
  ;; this case
  (when mappings
    (if (mod-id name)
	(typecheck-mappings mappings (name-to-modname name))
	(dolist (mapping mappings)
	  (typecheck-mapping-rhs (rhs mapping))))))

(defmethod typecheck-mappings (mappings (n number-expr))
  nil)

(defun typecheck-mapping-rhs (rhs)
  (typecheck-mapping-rhs* (expr rhs) rhs))

(defmethod typecheck-mapping-rhs* ((ex name-expr) rhs)
  (let ((tres (with-no-type-errors (resolve* ex 'type nil)))
	(eres (with-no-type-errors (resolve* ex 'expr nil)))
	(thres (with-no-type-errors (unless (mod-id ex)
				      (with-no-type-errors
					  (resolve* (name-to-modname ex)
						    'module nil))))))
    (if (cdr tres)
	(cond (eres
	       (setf (resolutions ex) eres))
	      (t (setf (resolutions ex) tres)
		 (type-ambiguity ex)))
	(setf (resolutions ex) (nconc tres eres thres)))
    (unless (resolutions ex)
      (type-error ex "No resolution for ~a as a type, expr, or theory" ex))
    (when eres
      (setf (types ex) (mapcar #'type eres))
      (when (and (plusp (parens ex))
		 (some #'(lambda (ty)
			   (let ((sty (find-supertype ty)))
			     (and (funtype? sty)
				  (tc-eq (range sty) *boolean*))))
		       (ptypes ex)))
	(setf (type-value rhs)
	      (typecheck* (make-instance 'expr-as-type 'expr (copy-untyped ex))
			  nil nil nil))))
    (when tres
      (if (type-value rhs)
	  (unless (compatible? (type-value rhs) (type (car tres)))
	    (push (car tres) (resolutions ex))
	    (type-ambiguity ex))
	  (progn
	    (setf (type-value rhs) (type (car tres)))
	    (push 'type (types ex)))))))

(defmethod typecheck-mapping-rhs* (ex rhs)
  (typecheck* ex nil nil nil))

(defun interpretable-declarations (theory)
  (remove-if-not #'interpretable? (all-decls theory)))

(defmethod interpretable? ((th module))
  (some #'interpretable? (all-decls th)))

(defmethod interpretable? ((d type-decl))
  t)

(defmethod interpretable? ((d type-def-decl))
  nil)

(defmethod interpretable? ((d mod-decl))
  (interpretable? (get-theory (modname d))))

(defmethod interpretable? ((d const-decl))
  (null (definition d)))

(defmethod interpretable? ((imp importing))
  nil)

(defmethod interpretable? ((res resolution))
  (interpretable? (declaration res)))

(defmethod interpretable? ((decl declaration))
  nil)

(defmethod interpretable? ((name name))
  (and (resolutions name)
       (interpretable? (car (resolutions name)))))

(defun check-mapping-lhs (lhs)
  (unless (interpretable? (resolution lhs))
    (type-error lhs "Must be uninterpreted to be used in a mapping.")))
	      

;;; check-exporting checks the names and theory instances being exported.

(defun check-exporting (theory)
  (check-exported-theories (modules (exporting theory)))
  (let* ((alldecls (collect-all-exportable-decls theory))
	 (edecls (cond ((eq (kind (exporting theory)) 'DEFAULT)
			alldecls)
		       ((but-names (exporting theory))
			(set-difference
			 alldecls
			 (check-exported-names (but-names (exporting theory))
					       (all-decls theory)
					       nil)))
		       ((eq (names (exporting theory)) 'ALL)
			alldecls)
		       ((names (exporting theory))
			(check-exported-names (names (exporting theory))
					      (all-decls theory)
					      nil))
		       (t (error "Something's wrong with EXPORTINGs")))))
    (mapc #'set-visibility edecls)
    (check-exported-completeness (exporting theory) edecls)))

(defun collect-all-exportable-decls (theory)
  (remove-if #'(lambda (d)
		 (typep d '(or importing var-decl field-decl recursive-type)))
	     (append (assuming theory)
		     (theory theory))))

(defun check-exported-names (expnames decls expdecls)
  (if (null expnames)
      expdecls
      (let ((expname (car expnames)))
	(when (type-expr? (kind expname))
	  (setf (type expname) (typecheck* (kind expname) 'type nil nil))
	  (set-type (kind expname) nil))
	(let* ((edecls (remove-if-not #'(lambda (d)
					  (and (declaration? d)
					       (eq (id d) (id expname))))
			 decls))
	       (kdecls (remove-if-not #'(lambda (d)
					  (correct-expkind d expname))
				      edecls))
	       (vdecls (remove-if #'(lambda (d)
				      (typep d '(or var-decl field-decl)))
				  kdecls)))
	  (unless edecls
	    (if (member expname (formals (current-theory)) :test #'same-id)
		(type-error expname "May not export formal parameters")
		(type-error expname "Name ~a is not declared in this theory"
			    expname)))
	  (unless kdecls
	    (type-error expname "Name ~a is not declared as ~a in this theory"
			expname (kind expname)))
	  (unless vdecls
	    (type-error expname "~a may not be exported" expname))
	  (check-exported-names
	   (cdr expnames) decls (append expdecls vdecls))))))

(defun correct-expkind (decl expname)
  (case (kind expname)
    ((nil) t)
    (type (type-decl? decl))
    (formula (formula-decl? decl))
    (t (tc-eq (type decl) (type expname)))))

(defun check-exported-completeness (exporting expdecls)
  (check-exported-internal-completeness (names exporting) expdecls)
  (case (kind exporting)
    ((all default)
     (setf (closure exporting)
	   (collect-all-exporting-with-theories
	    (get-immediate-usings (current-theory)))))
    (closure
     (setf (closure exporting)
	   (let ((insts nil))
	     (mapobject #'(lambda (ex)
			    (when (external-name ex)
			      (pushnew (module-instance ex) insts
				       :test #'tc-eq)))
			expdecls)
	     insts)))
    (t (check-exported-external-completeness
	(names exporting)
	(exporting-with-closure (modules exporting))
	expdecls)
       (setf (closure exporting)
	     (collect-all-exporting-with-theories
	      (modules exporting))))))

(defun collect-all-exporting-with-theories (theories)
  (remove-duplicates
      (mapcan #'(lambda (thinst)
		  (collect-all-exporting-with-theories* thinst
						    (get-theory thinst)))
	      theories)
    :test #'tc-eq))

(defmethod collect-all-exporting-with-theories* (thinst (theory module))
  (nconc (list thinst)
	 (if (actuals thinst)
	     (mapcar #'(lambda (thinst2)
			 (subst-mod-params thinst2 thinst))
		     (closure (exporting theory)))
	     (copy-list (closure (exporting theory))))))

(defmethod collect-all-exporting-with-theories* (thinst (adt recursive-type))
  (let ((th1 (adt-theory adt))
	(th2 (adt-map-theory adt))
	(th3 (adt-reduce-theory adt)))
    (nconc (collect-all-exporting-with-theories*
	    (mk-modname (id th1)
	      (actuals thinst))
	    th1)
	   (when th2
	     (collect-all-exporting-with-theories*
	      (mk-modname (id th2)) th2))
	   (collect-all-exporting-with-theories*
	    (mk-modname (id th3)) th3))))

(defmethod exporting ((adt recursive-type))
  (exporting (adt-theory adt)))

(defvar *theory-instances* nil)

(defun exporting-with-closure (instances)
  (let ((*theory-instances* nil))
    (mapc #'collect-exporting-with-theories instances)
    *theory-instances*))

(defun collect-exporting-with-theories (inst)
  (push inst *theory-instances*)
  (let ((theory (get-typechecked-theory inst)))
    (when (exporting theory)
      (dolist (etheory (modules (exporting theory)))
	(let ((itheory (subst-mod-params etheory inst)))
	  (collect-exporting-with-theories itheory))))))

(defun check-exported-internal-completeness (expnames expdecls)
  (mapc #'(lambda (edecl)
	    (let ((rdecls (remove-if
			   #'(lambda (d)
			       (or (not (eq (module d) (current-theory)))
				   (typep d '(or formal-decl importing var-decl
					      field-decl recursive-type module))
				   (and (const-decl? d)
					(formal-subtype-decl?
					 (generated-by d)))
				   (member d expdecls :test #'eq)))
			   (refers-to edecl))))
	      (when rdecls
		(let ((expname (if (consp expnames)
				   (car (member edecl expnames
						:test #'same-id))
				   edecl)))
		  (type-error expname
		    "~a may not be exported unless the following are also:~
                     ~%  ~{~a~^, ~}"
		    (if (consp expnames)
			expname
			(format nil "~a:~a" (id expname) (ptype-of expname)))
		    (mapcar #'(lambda (d) (format nil "~a:~a"
					    (id d) (ptype-of d)))
			    rdecls))))))
	expdecls))

(defun check-exported-external-completeness (expnames exptheories expdecls)
  (unless (null expdecls)
    (let ((decl (car expdecls)))
      (mapobject #'(lambda (ex)
		     (when (and (external-name ex)
				(not (member (module-instance ex) exptheories
					     :test #'tc-eq)))
		       (let ((expname (if (consp expnames)
					  (car (member decl expnames
						       :test #'same-id))
					  decl)))
			 (type-error expname
			   "~a refers to ~a~@[[~{~a~^, ~}]~].~a, which must be exported"
			   (id decl) (id (module-instance ex))
			   (actuals (module-instance ex)) (id ex)))))
		 decl))
    (check-exported-external-completeness expnames exptheories (cdr expdecls))))

(defun external-name (ex)
  (and (name? ex)
       (not (freevars ex))
       (module-instance ex)
       (not (eq (module (declaration ex))
		(current-theory)))
       (not (from-prelude? (declaration ex)))))

(defun set-visibility (decl)
  (unless (or (typep decl '(or var-decl field-decl))
	      (and (type-def-decl? decl)
		   (enumtype? (type-expr decl))))
    (setf (visible? decl) t)))

(defun check-exported-theories (theories)
  (unless (symbolp theories);; Handles NIL, ALL, and CLOSURE
    (when (actuals (car theories))
      (typecheck-actuals (car theories))
      (set-type-actuals (car theories)))
    (unless (member (car theories)
		    (gethash (get-theory (car theories))
			     (using-hash *current-context*))
		    :test #'check-exported-theories-test)
      (type-error (car theories)
	"~a occurs in an EXPORTING WITH but is not in a IMPORTING clause"
	(car theories)))
    (check-exported-theories (cdr theories))))

(defun check-exported-theories-test (u v)
  (and (same-id u v)
       (or (null (actuals v))
	   (tc-eq (actuals u) (actuals v)))))
