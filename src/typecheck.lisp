;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typecheck.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 19:01:35 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Mon May 24 17:48:12 2004
;; Update Count    : 36
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002-2004 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(export '(typecheck typecheck* typecheck-uniquely set-dependent-formals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top level typechecking functions

(defmethod typecheck ((m module) &key expected context tccs)
  (declare (ignore expected context tccs))
  (let ((*generate-tccs* 'all))
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
  (if types
      (setf (gethash ex *expression-types*) types)
      (remhash ex *expression-types*)))

(defmethod get-unique-type ((ex name-expr))
  (let ((freses (if (cdr (resolutions ex))
		    (filter-local-resolutions (resolutions ex))
		    (resolutions ex))))
    (setf (resolutions ex) freses)
    (if (singleton? freses)
	(let ((type (type (car freses))))
	  (when (fully-instantiated? type)
	    type))
	(call-next-method))))

(defmethod get-unique-type ((ex expr))
  (let ((types (remove-duplicates (ptypes ex) :test #'tc-eq)))
    (if (and (singleton? types)
	     (fully-instantiated? (car types)))
	(car types)
	(if (every #'(lambda (ty)
		       (compatible? ty (car types)))
		   (cdr types))
	    (let ((ctype (reduce #'compatible-type types)))
	      (when (fully-instantiated? ctype)
		ctype))
	    (type-ambiguity ex)))))

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
    (let ((*subtype-of-hash* (make-hash-table :test #'equal))
	  (*beta-cache* (make-hash-table :test #'eq))
	  (*assert-if-arith-hash* (make-hash-table :test #'eq))
	  (*bound-variables* *bound-variables*))
      (reset-pseudo-normalize-caches)
      (tcdebug "~%Typecheck ~a" (id m))
      (setf (formals-sans-usings m)
	    (remove-if #'(lambda (ff) (typep ff 'importing)) (formals m)))
      (setf (all-imported-theories m) 'unbound)
      (let* ((*current-theory* m)
	     (*typechecking-module* t)
	     (*tccs* nil)
	     (*tccdecls* nil)
	     (*tccforms* nil)
	     (*current-context* (if (eq (current-theory) m)
				    *current-context*
				    (make-new-context m))))
	(tcdebug "~%  Processing formals")
	(typecheck-decls (remove-if #'generated-by (formals m)))
	(set-dependent-formals (formals-sans-usings m))
	(tcdebug "~%  Processing assuming")
	(when (and (assuming m)
		   (null (formals-sans-usings m)))
	  (type-error m
	    "Theory ~a has no formal parameters, hence no need for ASSUMING section"
	    (id m)))
	(typecheck-decls (remove-if #'generated-by (assuming m)))
	(tcdebug "~%  Processing theory")
	(typecheck-decls (remove-if #'generated-by (theory m)))
	(tcdebug "~%  Processing exporting")
	(generate-xref m)
	(assert (eq (current-theory) m))
	(setf (all-usings m)
	      (let ((imps nil))
		(maphash #'(lambda (th thinsts)
			     (unless (and (from-prelude? th)
					  (singleton? thinsts))
			       (push (cons th thinsts) imps)))
			 (lhash-table (current-using-hash)))
		imps))
	(check-exporting m)
	(setf (dependent-known-subtypes m)
	      (remove-if (complement
			  #'(lambda (elt)
			     (some #'(lambda (fp)
				       (memq (declaration fp)
					     (formals-sans-usings m)))
				   (free-params elt))))
		(current-known-subtypes)))
	(setf (saved-context m) *current-context*))
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
  (let ((lib-id (library theory-inst)))
    (when lib-id
      (let ((lib-ref (get-library-reference lib-id)))
	(when (file-equal (libref-to-pathname lib-ref) *pvs-context-path*)
	  (type-error theory-inst
	    "Library \"~a\" refers to the current PVS context - it must be external"
	    lib-id)))))
  (if (and (null (library theory-inst))
	   (eq (id theory-inst) (id (current-theory))))
      (type-error theory-inst "A theory may not import itself")
      (let* (;; Need to keep track of where we are for untypechecking
	     ;; Everything after this will be untypechecked if
	     ;; something changed underneath
	     (*tc-theories* (acons (current-theory) (current-declaration)
				   *tc-theories*))
	     (plib-context *prelude-library-context*)
	     (mod (get-typechecked-theory theory-inst)))
	;; If get-typechecked-theory ended up loading a new prelude library,
	;; we need to update the current context.
	(assert (saved-context mod))
	(when (context-difference? plib-context *prelude-library-context*)
	  (setf (lhash-next (using-hash *current-context*))
		(using-hash *prelude-library-context*))
	  (setf (lhash-next (declarations-hash *current-context*))
		(declarations-hash *prelude-library-context*)))
	(when (and *tc-add-decl*
		   ;; Check for circularities
		   (memq (current-theory) (all-importings mod)))
	  (type-error theory-inst
	    "Circularity found in importings of theory ~a" theory-inst))
	;;(assert (get-theory theory-inst))
	(typecheck-using* mod theory-inst))))

(defun context-difference? (old-ctx new-ctx)
  (if (null old-ctx)
      (not (null new-ctx))
      (not (eq old-ctx new-ctx))))

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
	   (setq nmodinst (set-type-actuals inst mod))
	   (unless (fully-instantiated? inst)
	     (type-error inst "Importing actuals must be fully instantiated"))
	   (check-compatible-params (formals-sans-usings mod)
				    (actuals inst) nil))
	  ((mappings inst)
	   (typecheck-mappings (mappings inst) inst)
	   (setq nmodinst (set-type-actuals inst mod))))
    (add-to-using nmodinst mod)
    (unless (eq nmodinst inst)
      (let ((theory (get-theory inst)))
	(assert (or (not (library-datatype-or-theory? theory))
		    (library inst)))
	(put-importing inst theory)))
    (when (some #'(lambda (m) (mod-decl? (declaration (lhs m))))
		(mappings nmodinst))
      (add-theory-mappings-importings mod nmodinst))
    (when (some #'formal-theory-decl? (formals mod))
      (add-theory-parameters-importings mod nmodinst))
    (when (mappings nmodinst)
      (generate-mapped-axiom-tccs nmodinst))
    (unless *ignore-exportings*
      (add-exporting-with-theories mod nmodinst t))))

(defun add-theory-parameters-importings (theory inst)
  (when (and (formals-sans-usings theory)
	     (actuals inst))
    (mapc #'(lambda (fm act)
	      (when (and (formal-theory-decl? fm)
			 (not (theory-abbreviation-decl?
			       (declaration (expr act)))))
		(add-to-using (mk-modname (id (expr act))
				(actuals (expr act))
				(library (expr act))
				(mappings (expr act)))
			      (get-theory (expr act)))))
	  (formals-sans-usings theory)
	  (actuals inst))))

(defun add-theory-mappings-importings (theory inst)
  (declare (ignore theory))
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
	 (use2 (when th2 (copy inst 'id (id th2) 'actuals nil 'mappings nil)))
	 (use3 (copy inst 'id (id th3) 'actuals nil 'mappings nil))
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

(defun add-exporting-with-theories (theory inst &optional skip-add-to-using?)
  (when (exporting theory)
    (dolist (entry (the list (closure (exporting theory))))
      (let* ((itheory (cdr entry))
	     (ename (if (and (library-datatype-or-theory? (cdr entry))
			     (null (library (car entry))))
			(copy (car entry)
			  'library (libref-to-libid (lib-ref (cdr entry))))
			(car entry)))
	     (iname (if (or (actuals inst)
			    (mappings inst))
			(subst-mod-params ename inst theory)
			(remove-indirect-formals-of-name ename))))
	(assert itheory)
	(assert (or (not (library-datatype-or-theory? itheory))
		    (library iname)
		    (from-prelude-library? itheory)
		    (file-equal (lib-ref itheory) *pvs-context-path*)))
	#+pvsdebug (assert (or (null (actuals iname))
			       (fully-instantiated? iname)))
	(unless (and (formals-sans-usings itheory) (null (actuals iname)))
	  ;; Add this to the assuming-instances list if fully instantiated
	  (pushnew iname (assuming-instances (current-theory))
		   :test #'tc-eq))
	(unless skip-add-to-using?
	  (add-to-using iname itheory))))))


;;; Returns all of the theorynames directly used by the specified
;;; theory, either through USINGs or MOD-DECLs.  Note that when a datatype
;;; is referenced, it is replaced by (instances of) its generated
;;; theories.

(defmethod get-immediate-usings ((theory module))
  (with-slots (immediate-usings formals assuming (theory-part theory)) theory
    (if (eq immediate-usings 'unbound)
	(let* ((usings (mapcar #'theory-name
			 (remove-if-not #'mod-or-using?
			   (all-decls theory))))
	       (all-there? t)
	       (imm-usings (mapcan #'(lambda (thname)
				       (let ((th (get-theory thname)))
					 (unless th (setq all-there? nil))
					 (append
					  (when (target thname)
					    (list (target thname)))
					  (or (and (typep th 'datatype)
						   (datatype-instances thname))
					      (list thname)))))
			     usings)))
	  (if all-there?
	      (setf immediate-usings imm-usings)
	      imm-usings))
	immediate-usings)))

(defmethod get-immediate-using-names ((theory module))
  (with-slots (immediate-usings formals assuming (theory-part theory)) theory
    (let* ((usings (mapcar #'theory-name
		     (remove-if-not #'mod-or-using?
		       (all-decls theory)))))
      usings)))

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

(defmethod modname ((d formal-theory-decl))
  (theory-name d))

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
  (typep obj
	 '(or mod-decl theory-abbreviation-decl formal-theory-decl importing)))

(defmethod modules ((decl mod-decl))
  (list (modname decl)))

(defmethod modules ((decl theory-abbreviation-decl))
  (list (theory-name decl)))


;;; Perform the substitution.  In the above, would be called with
;;; (m1[t,c] #m1 m2[s]) and return m2[t].

(defun subst-actuals (inst theory target-inst)
  (let* ((etheory (subst-mod-params target-inst inst theory))
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
  #+pvsdebug (assert (valid-importing-entry? theoryname))
  (let ((theory (or itheory (get-typechecked-theory theoryname))))
    (unless theory
      (type-error theoryname "Theory ~a not found" (id theoryname)))
    ;; Need to update using-hash, declarations-hash, library-alist,
    ;; known-subtypes, judgements, conversions, auto-rewrites, and
    ;; named-theories of current context from saved-context of theory.
    ;; Also need to update assuming-instances of current theory.
    (let ((entry (get-importings theory)))
      #+pvsdebug (assert (valid-importing-entries? entry))
      (unless (and entry (member theoryname entry :test #'tc-eq))
	(update-current-context theory theoryname)))))

(defun update-current-context (theory theoryname)
  (assert (saved-context theory))
  (update-library-alist theory)
  (update-usings-hash theory theoryname)
  (update-declarations-hash theory theoryname)
  (update-known-subtypes theory theoryname)
  (update-judgements-of-current-context theory theoryname)
  (update-conversions-of-current-context theory theoryname)
  (update-auto-rewrites-of-current-context theory theoryname)
  (update-named-theories-of-current-context theory theoryname))

(defun update-usings-hash (theory theoryname)
  (let ((thimps (get-importings theory)))
    (setf (get-importings theory)
	  (append thimps (list theoryname))))
  (maphash #'(lambda (th ithinsts)
	       (unless (eq th theory)
		 (let* ((thinsts (exportable-theory-instances ithinsts theory))
			(curimps (get-importings th))
			(expinsts (get-exported-theory-instances
				   thinsts (closure (exporting theory))))
			(sthinsts (subst-theory-importings
				   th expinsts theoryname theory))
			(newinsts (remove-if #'(lambda (sth)
						 (member sth curimps
							 :test #'tc-eq))
				    sthinsts)))
		   (assert (or (not (fully-instantiated? theoryname))
			       (valid-importing-entries? newinsts)))
		   (when newinsts
		     (setf (get-importings th) (append curimps newinsts))))))
	   (lhash-table (using-hash (saved-context theory)))))

(defun exportable-theory-instances (thinsts theory &optional expinsts)
  (if (null thinsts)
      (nreverse expinsts)
      (exportable-theory-instances
       (cdr thinsts)
       theory
       (if (every #'(lambda (x) (or (formal-decl? x) (exportable? x theory)))
		  (collect-references (actuals (car thinsts))))
	   (cons (car thinsts) expinsts)
	   expinsts))))

(defun get-exported-theory-instances (thinsts closure &optional insts)
  (if (null thinsts)
      (nreverse insts)
      (let ((inst (car (assoc (car thinsts) closure :test #'expinst-eq))))
	(get-exported-theory-instances
	 (cdr thinsts) closure
	 (if inst
	     (cons (if (actuals inst)
		       inst
		       (car thinsts))
		   insts)
	     insts)))))

(defun expinst-eq (inst1 inst2)
  (and (eq (id inst1) (id inst2))
       (eq (library inst1) (library inst2))
       (or (null (actuals inst1))
	   (null (actuals inst2))
	   (tc-eq (actuals inst1) (actuals inst2)))
       (tc-eq (mappings inst1) (mappings inst2))))
	   

(defun subst-theory-importings (th thinsts theoryname theory)
  (let* ((mthinsts (if (mappings theoryname)
		       (mapcan #'(lambda (thinst)
				   (when (and (eq (id theoryname) (id thinst))
					      (eq (library theoryname)
						  (library thinst))
					      (fully-instantiated? thinst))
				     (list
				      (copy thinst
					'mappings (mappings theoryname)))))
			 thinsts)
		       thinsts))
;; 	 (lib-id (when (library-datatype-or-theory? th)
;; 		   (car (rassoc (lib-ref th) (current-library-alist)
;; 				:test #'equal))))
	 (lthinsts (if (library-datatype-or-theory? th)
		       (mapcar #'(lambda (thinst)
				   (if (library thinst)
				       thinst
				       (copy thinst
					 'library (library theoryname))))
			 mthinsts)
		       mthinsts)))
    (if (fully-instantiated? theoryname)
	(mapcar #'(lambda (thinst)
		    (subst-theory-importing thinst theoryname theory))
	  lthinsts)
	(mapcar #'remove-indirect-formals-of-name lthinsts))))

(defun subst-theory-importing (thinst theoryname theory)
  (if (or (actuals thinst)
	  (mappings thinst))
      (subst-mod-params thinst theoryname theory)
      thinst))


(defun valid-importing-entries? (theory-names)
  (every #'valid-importing-entry? theory-names))

(defun valid-importing-entry? (theory-name)
  (or (and (null (actuals theory-name))
	   (null (mappings theory-name)))
      (fully-instantiated? theory-name)))


(defun update-declarations-hash (theory theoryname)
  (maphash #'(lambda (id decls)
	       (declare (ignore id))
	       (dolist (decl decls)
		 (when (and (declaration? decl)
			    (visible? decl)
			    (exportable? decl theory))
		   (when (and (eq (id (current-theory)) 'min_seq)
			      (eq (id decl) 'amax))
		     (break "update-declarations-hash"))
		   (let ((map (find decl (mappings theoryname)
				    :key #'(lambda (m)
					     (declaration (lhs m))))))
		     (unless (mapping-subst? map)
		       (check-for-importing-conflicts decl)
		       (put-decl decl))))))
	   (lhash-table (declarations-hash (saved-context theory)))))

;;; Checks whether the given declaration is exportable from the given theory.
;;; to the current theory.
;;; Used mostly in merging judgements from one context to another.

;;; There are three theories involved here: the (module decl), the given
;;; theory, and the (current-theory).  The tricky bit is judgements, which
;;; when instantiated end up belonging to a different theory (see
;;; subst-params-decl).  
(defun exportable? (decl theory)
  (assert *current-context*)
  (or (eq (module decl) (current-theory))
      (from-prelude? decl)
      (from-prelude-library? decl)
      (unless (or (from-prelude? theory)
		  (from-prelude-library? theory))
	(let ((exp (exporting theory)))
	  (if (eq (module decl) theory)
	      (or (eq (kind exp) 'default)
		  (if (eq (names exp) 'all)
		      (not (member decl (but-names exp) :test #'expname-test))
		      (member decl (names exp) :test #'expname-test)))
	      (or ;;(eq (kind exp) 'default)
	       (and (rassoc (module decl) (closure exp) :test #'eq)
		    (exportable? decl (module decl)))))))))

(defun remove-uninstantiated-repeated-importings (entry imps)
  (remove-if
      #'(lambda (imp)
	  (uninstantiated-repeated-importing imp imps))
    entry))

(defun uninstantiated-repeated-importing (imp imps)
  (and (actuals imp)
       (not (fully-instantiated? imp))
       (member (id imp) imps :key #'id)
       (not (member imp imps :test #'tc-eq))))

(defmethod check-for-importing-conflicts ((decl lib-decl))
  (let ((lib-ref (lib-ref decl)))
    (dolist (d (get-declarations (id decl)))
      (when (and (lib-decl? d)
		 (not (string= lib-ref (lib-ref d))))
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

(defun update-library-alist (theory)
  (dolist (elt (reverse (library-alist (saved-context theory))))
    (pushnew elt (library-alist *current-context*) :test #'equal)))

(defun update-conversions-of-current-context (theory theoryname)
  (unless (and (not *loading-prelude*)
	       (from-prelude? theory))
    (let ((new-convs (get-new-imported-conversions
		      (mapcar #'(lambda (conv)
				  (if (eq (module conv) theory)
				      (subst-params-decl conv
							 theoryname theory)
				      conv))
			(conversions (saved-context theory)))
		      (current-conversions))))
      (dolist (conversion new-convs)
	(push conversion (conversions *current-context*))))
    (dolist (conversion (disabled-conversions (saved-context theory)))
      (if (eq (module conversion) theory)
	  (pushnew (subst-params-decl conversion theoryname theory)
		   (disabled-conversions *current-context*)
		   :test #'eq)
	  (pushnew conversion (disabled-conversions *current-context*)
		   :test #'eq)))))

;; Note that this returns the new conversions in reverse order
(defun get-new-imported-conversions (imported-convs current-convs
						    &optional new-convs)
  (if (null imported-convs)
      new-convs
      (get-new-imported-conversions
       (cdr imported-convs)
       current-convs
       (if (member (car imported-convs) current-convs
		   :test #'subsumed-conversion)
	   new-convs
	   (cons (car imported-convs) new-convs)))))

(defun subsumed-conversion (imp-conv cur-conv)
  (and (eq (id imp-conv) (id cur-conv))
       (or (tc-eq (expr imp-conv) (expr cur-conv))
	   (let ((bindings (tc-match (expr cur-conv) (expr imp-conv)
				     (mapcar #'list (free-params imp-conv)))))
	     (and bindings (every #'cdr bindings))))))
		     

(defun update-auto-rewrites-of-current-context (theory theoryname)
  (declare (ignore theoryname))
  (dolist (r (auto-rewrites (saved-context theory)))
    (pushnew r (auto-rewrites *current-context*)))
  (dolist (r (disabled-auto-rewrites (saved-context theory)))
    (pushnew r (disabled-auto-rewrites *current-context*))))

(defun update-named-theories-of-current-context (theory theoryname)
  (declare (ignore theoryname))
  (when (saved-context theory)
    (dolist (nt (named-theories (saved-context theory)))
      (pushnew nt (named-theories *current-context*)))))

(defun list-diff (l1 l2 &optional elts)
  (if (or (null l1) (equal l1 l2))
      (nreverse elts)
      (list-diff (cdr l1) l2 (cons (car l1) elts))))

(defmethod subst-params-decl ((c conversion-decl) modinst theory)
  (if (or (mappings modinst)
	  (and (memq theory (free-params-theories c))
	       (actuals modinst)
;; 	       (not (every #'(lambda (a)
;; 			       (if (type-value a)
;; 				   (and (type-name? (type-value a))
;; 					(memq (declaration (type-value a))
;; 					      (free-params (current-theory))))
;; 				   (and (name-expr? (expr a))
;; 					(memq (declaration (expr a))
;; 					      (free-params (current-theory))))))
;; 			   (actuals modinst)))
	       ))
      (let ((nc (lcopy c
		  'expr (subst-mod-params (expr c) modinst theory))))
	(unless (eq c nc)
	  (setf (module nc)
		(if (fully-instantiated? modinst)
		    (current-theory)
		    (module c)))
	  (add-decl nc))
	nc)
      c))
		    
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
  (if (or (null (actuals theoryname))
	  (fully-instantiated? (actuals theoryname)))
      theoryname
      (copy theoryname 'actuals nil 'mappings nil)))

(defun check-compatible-params (formals actuals assoc)
  (or (null formals)
      (and (check-compatible-param (car formals) (car actuals) assoc)
	   (check-compatible-params
	    (cdr formals) (cdr actuals)
	    (if (formal-theory-decl? (car formals))
		(let* ((mdecl (declaration (resolution (expr (car actuals)))))
		       (fmappings (theory-mapping (generated-theory (car formals))))
		       (amappings (typecase mdecl
				    (theory-abbreviation-decl (mapping mdecl))
				    (mod-decl
				     (theory-mapping (generated-theory mdecl)))
				    (t (make-subst-mod-params-map-bindings
					(expr (car actuals))
					(mappings (expr (car actuals)))
					nil))))
		       (nalist (compose-formal-to-actual-mapping
				fmappings amappings)))
		  (assert (= (length fmappings) (length amappings)))
		  (nconc nalist assoc))
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
			   assoc)))))))

(defun check-compatible-param (formal actual assoc)
  (typecase formal
    (formal-type-decl
     (unless (type-value actual)
       (type-error actual "Expression provided where a type is expected"))
     (typecase formal
       (formal-subtype-decl
	(let ((type (subst-types (supertype (type-value formal)) assoc)))
	  (unless (compatible? (type-value actual) type)
	    (type-error actual "~a Should be a subtype of ~a"
			(type-value actual) type))))
       (formal-struct-subtype-decl
	(let ((struct-type (type (resolution (type-expr formal))))
	      (act-type (type-value actual)))
	  (unless (sub-struct-type? act-type struct-type)
	    (type-error actual
	      "Not a structural subtype"))))))
    (formal-theory-decl
     (unless (typep (declaration (expr actual))
		    '(or module mod-decl theory-abbreviation-decl
		         formal-theory-decl))
       (type-error actual "Theory name expected here")))
    (t (let ((type (subst-types (type formal) assoc)))
	 (typecheck* (expr actual) type nil nil))))
  t)

(defmethod sub-struct-type? ((t1 recordtype) (t2 recordtype))
  (let ((subfields (remove-if (complement
			       #'(lambda (fld)
				   (member (id fld) (fields t2) :key #'id)))
		     (fields t1))))
    (tc-eq subfields (fields t2))))

(defmethod sub-struct-type? ((t1 tupletype) (t2 tupletype))
  (every #'tc-eq (types t1) (types t2)))

(defmethod sub-struct-type? ((t1 type-expr) (t2 type-expr))
  nil)

(defun subst-types (type assoc)
  (if assoc
      (gensubst type
	#'(lambda (te) (cdr (assoc (declaration te) assoc)))
	#'(lambda (te) (and (name? te)
			    (assoc (declaration te) assoc))))
      type))

(defmethod typecheck-mappings (mappings (thinst modname))
  (unless (or (not mappings)
	      (already-typed? mappings))
    (let* ((aliases (get-theory-aliases thinst))
	   (inst (or (car aliases) thinst))
	   (lhs-theory (get-theory inst)))
      (unless lhs-theory
	(type-error inst "Theory ~a not found" inst))
      (let ((lhs-context (context lhs-theory))
	    (lhs-theory-decls (interpretable-declarations lhs-theory)))
	(dolist (mapping mappings)
	  (unless (already-typed? mapping)
	    (let* ((*current-theory* lhs-theory)
		   (*current-context* lhs-context)
		   (*generate-tccs* 'none)
		   (type (when (and (kind mapping)
				    (eq (kind mapping) 'expr))
			   (prog1 (typecheck* (declared-type mapping)
					      nil nil nil)
			     (let ((*generate-tccs* 'none))
			       (set-type (declared-type mapping) nil)))))
		   (tres (unless (and (kind mapping)
				      (not (eq (kind mapping) 'type)))
			   (delete-if-not
			       #'(lambda (r)
				   (memq (declaration r) lhs-theory-decls))
			     (with-no-type-errors
			      (resolve* (lhs mapping) 'type nil)))))
		   (eres (unless (and (kind mapping)
				      (not (eq (kind mapping) 'expr)))
			   (delete-if-not
			       #'(lambda (r)
				   (and (memq (declaration r) lhs-theory-decls)
					(or (null type)
					    (compatible? type (type r)))))
			     (with-no-type-errors
			      (resolve* (lhs mapping) 'expr nil)))))
		   (nres (unless (and (kind mapping)
				      (not (eq (kind mapping) 'expr)))
			   (when (and (integerp (id (lhs mapping)))
				      (or (null (mod-id (lhs mapping)))
					  (eq (mod-id (lhs mapping))
					      'numbers)))
			     (list (mk-resolution
				       (number-declaration (id (lhs mapping)))
				     (mk-modname 'numbers) *number*)))))
		   (thres (unless (and (kind mapping)
				       (not (eq (kind mapping) 'theory)))
			    (with-no-type-errors
			     (resolve* (lhs mapping) 'module nil)))))
	      (unless (or eres nres tres thres)
		(type-error (lhs mapping)
		  "Map lhs ~a does not resolve to an uninterpreted type or constant"
		  (lhs mapping)))
	      (if (cdr tres)
		  (cond ((or eres nres)
			 (setf (resolutions (lhs mapping)) (nconc eres nres)))
			(t (setf (resolutions (lhs mapping)) tres)
			   (type-ambiguity (lhs mapping))))
		  (setf (resolutions (lhs mapping)) (nconc tres eres nres thres)))
	      (assert (resolutions (lhs mapping)))
	      (when (mapping-rename? mapping)
		(if (cdr (resolutions (lhs mapping)))
		    (type-ambiguity (lhs mapping))
		    (check-duplication (copy (declaration
					      (car (resolutions (lhs mapping))))
					 'id (id (expr (rhs mapping)))
					 'module (current-theory))))))
	    (unless (mapping-rename? mapping)
	      (typecheck-mapping-rhs mapping))
	    ;;(assert (ptypes (expr (rhs mapping))))
	    ))))))

(defmethod already-typed? ((list list))
  (every #'already-typed? list))

(defmethod already-typed? ((map mapping))
  (and (already-typed? (lhs map))
       (already-typed? (rhs map))))

(defmethod already-typed? ((name name))
  (resolution name))

(defmethod already-typed? ((rhs mapping-rhs))
  (or (type-value rhs)
      (type (expr rhs))))

(defmethod typecheck-mappings (mappings (name name))
  ;; Used with a name that is not necessarily a modname
  (when mappings
    (if (mod-id name)
	(typecheck-mappings mappings (name-to-modname name))
	(dolist (mapping mappings)
	  (typecheck-mapping-rhs mapping)))))

(defmethod typecheck-mappings (mappings (n number-expr))
  (declare (ignore mappings))
  nil)

(defmethod typecheck-mapping-rhs ((mapping mapping))
  (when (declared-type mapping)
    (setf (type mapping) (typecheck* (declared-type mapping) nil nil nil)))
  (typecheck-mapping-rhs* (expr (rhs mapping))
			  (kind mapping)
			  (type mapping)
			  (rhs mapping)))

(defmethod typecheck-mapping-rhs ((mapping mapping-rename))
  (when (declared-type mapping)
    (setf (type mapping) (typecheck* (declared-type mapping) nil nil nil)))
  ;; Just check that the rhs won't clash with any existing names of the
  ;; source theory.
  )

(defmethod typecheck-mapping-rhs* ((ex name-expr) kind type rhs)
  (assert (or (null (type ex)) (null type) (compatible? (type ex) type)))
  (unless (type ex)
    (let* ((tres (unless (and kind
			      (not (eq kind 'type)))
		   (with-no-type-errors (resolve* ex 'type nil))))
	   (eres (unless (and kind
			      (not (eq kind 'expr)))
		   (if (or tres (null (mod-id ex)))
		       (with-no-type-errors (resolve* ex 'expr nil))
		       (resolve* ex 'expr nil))))
	   (thres (unless (or (mod-id ex)
			      (and kind
				   (not (eq kind 'theory))))
		    (if (or tres eres)
			(with-no-type-errors (resolve* (name-to-modname ex)
						       'module nil))
			(resolve* (name-to-modname ex) 'module nil)))))
;;       (when type
;; 	(setf eres (delete-if-not #'(lambda (r) (compatible? (type r) type))
;; 		     eres)))
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
	      (setf (type-value rhs) (type (car tres)))))))))

(defmethod typecheck-mapping-rhs* (ex kind type rhs)
  (declare (ignore kind type))
  (let ((typed-ex (typecheck* ex nil nil nil)))
    (when (type-expr? typed-ex)
      (setf (type-value rhs) typed-ex))
    typed-ex))

(defun interpretable-declarations (theory)
  (remove-if-not #'interpretable? (all-decls theory)))

(defmethod interpretable? ((th module))
  (some #'interpretable? (all-decls th)))

(defmethod interpretable? ((ty recursive-type))
  t)

(defmethod interpretable? ((d type-decl))
  t)

(defmethod interpretable? ((d type-def-decl))
  nil)

(defmethod interpretable? ((d mod-decl))
  (interpretable? (get-theory (modname d))))

(defmethod interpretable? ((d formal-type-decl))
  nil)

(defmethod interpretable? ((d const-decl))
  (null (definition d)))

(defmethod interpretable? ((imp importing))
  nil)

(defmethod interpretable? ((res resolution))
  (interpretable? (declaration res)))

(defmethod interpretable? ((decl declaration))
  nil)

(defmethod interpretable? ((bd binding))
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
	 (edecls (cond ((eq (kind (exporting theory)) 'default)
			alldecls)
		       ((but-names (exporting theory))
			(set-difference
			 alldecls
			 (check-exported-names (but-names (exporting theory))
					       (all-decls theory)
					       nil)))
		       ((eq (names (exporting theory)) 'all)
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

(defun expname-test (decl expname)
  (and (eq (id decl) (id expname))
       (correct-expkind decl expname)))

(defun correct-expkind (decl expname)
  (case (kind expname)
    ((nil) t)
    (type (type-decl? decl))
    (formula (formula-decl? decl))
    (t (tc-eq (type decl) (type expname)))))

(defun check-exported-completeness (exporting expdecls)
  (check-exported-internal-completeness (names exporting) expdecls)
  (case (kind exporting)
    ;; kind refers to what the modnames means:
    ;;  default == no exporting clause
    ;;  all     == EXPORTING names WITH all
    ((all default)
     (setf (closure exporting)
	   (mapcan #'(lambda (imp)
		       (mapcar #'(lambda (inst)
				   (cons inst (car imp)))
			 (cdr imp)))
	     (all-usings (current-theory)))))
    ;;  closure == EXPORTING names WITH closure
    (closure
     (setf (closure exporting)
	   (let ((insts nil))
	     (mapobject #'(lambda (ex)
			    (when (external-name ex)
			      (pushnew (cons (module-instance ex)
					     (get-theory (module-instance ex)))
				       insts
				       :test #'tc-eq)))
			expdecls)
	     insts)))
    ;;  nil     == EXPORTING names    (e.g., no WITH)
    ;;  or the names were listed explicitely
    (t (check-exported-external-completeness
	(names exporting)
	(exporting-with-closure (modules exporting))
	expdecls)
       (setf (closure exporting)
	     (collect-all-exporting-with-theories
	      (modules exporting)
	      nil)))))

(defun collect-all-exporting-with-theories (theories sorted)
  (let ((collected (remove-duplicates
		       (mapcan #'(lambda (thinst)
				   (collect-all-exporting-with-theories*
				    thinst
				    (get-theory thinst)))
			 theories)
		     :test #'tc-eq)))
    (assert (or (null sorted)
		(every #'(lambda (c) (memq (cdr c) sorted)) collected)))
    (if sorted
	(mapcan #'(lambda (th)
		    (let ((entries
			   (remove-if #'(lambda (e) (not (eq (cdr e) th)))
			     collected)))
		      ;;(assert (or (recursive-type? th)
			;;	  (generated-by th) entries))
		      entries))
	  sorted)
	collected)))

(defmethod collect-all-exporting-with-theories* (thinst (theory module))
  (assert (or (not (library-datatype-or-theory? theory))
	      (library thinst)
	      (file-equal (lib-ref theory) *pvs-context-path*)))
  (let ((closure (if (library thinst)
		     (mapcar #'(lambda (entry)
				 (if (and (library-datatype-or-theory?
					   (cdr entry))
					  (null (library (car entry)))
					  (string= (lib-ref (cdr entry))
						   (lib-ref theory)))
				     (cons (copy (car entry)
					     'library (library thinst))
					   (cdr entry))
				     entry))
		       (closure (exporting theory)))
		     (closure (exporting theory)))))
    (nconc (if (or (actuals thinst) (mappings thinst))
	       (mapcar #'(lambda (entry)
			   (let* ((thinst2 (car entry))
				  (nthinst2 (subst-mod-params
					     thinst2 thinst theory)))
			     (assert (or (null (actuals nthinst2))
					 (fully-instantiated? nthinst2)))
			     (if (eq thinst2 nthinst2)
				 entry
				 (cons nthinst2 (cdr entry)))))
		 closure)
	       (mapcar #'(lambda (entry)
			   (let* ((thinst2 (car entry))
				  (nthinst2 (remove-indirect-formals-of-name
					     thinst2)))
			     (assert (or (null (actuals nthinst2))
					 (fully-instantiated? nthinst2)))
			     (if (eq thinst2 nthinst2 )
				 entry
				 (cons nthinst2 (cdr entry)))))
		 closure))
	   (list (cons thinst theory)))))

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
	(let ((itheory (subst-mod-params etheory inst theory)))
	  (collect-exporting-with-theories itheory))))))

(defun check-exported-internal-completeness (expnames expdecls)
  (mapc #'(lambda (edecl)
	    (let ((rdecls (remove-if
			      #'(lambda (d)
				  (or (not (eq (module d) (current-theory)))
				      (typep d '(or formal-decl importing
						    var-decl field-decl
						    recursive-type module
						    mapping-subst))
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
      (unless (and (judgement? decl)
		   (generated-by decl))
	(mapobject #'(lambda (ex)
		       (when (and (external-name ex)
				  (not (member (module-instance ex) exptheories
					       :test #'tc-eq)))
			 (let ((expname (if (consp expnames)
					    (car (member decl expnames
							 :test #'same-id))
					    decl)))
			   (type-error expname
			     "~a refers to~_ ~w~@[[~{~w~^, ~}]~].~w,~_ which must be exported"
			     (id decl) (id (module-instance ex))
			     (actuals (module-instance ex)) (id ex)))))
		   decl)))
    (check-exported-external-completeness expnames exptheories (cdr expdecls))))

(defun external-name (ex)
  (and (name? ex)
       (not (variable? ex))
       (not (freevars ex))
       (module-instance ex)
       (not (eq (module (declaration ex))
		(current-theory)))
       (not (from-prelude? (declaration ex)))))

(defun set-visibility (decl)
  (unless (or (typep decl '(or var-decl field-decl inline-recursive-type))
	      (and (type-def-decl? decl)
		   (enumtype? (type-expr decl))))
    (setf (visible? decl) t)))

(defun check-exported-theories (theories)
  (unless (symbolp theories);; Handles NIL, ALL, and CLOSURE
    (let ((theory (get-theory (car theories))))
      (unless theory
	(type-error (car theories) "Theory ~a not found" (car theories)))
      (when (actuals (car theories))
	(typecheck-actuals (car theories))
	(set-type-actuals (car theories) theory))
      (unless (member (car theories)
		      (get-importings theory)
		      :test #'check-exported-theories-test)
	(type-error (car theories)
	  "~a occurs in an EXPORTING WITH but is not in a IMPORTING clause"
	  (car theories))))
    (check-exported-theories (cdr theories))))

(defun check-exported-theories-test (u v)
  (and (same-id u v)
       (or (null (actuals v))
	   (tc-eq (actuals u) (actuals v)))))
