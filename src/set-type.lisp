;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-type.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Oct 20 00:42:24 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 29 18:14:49 1999
;; Update Count    : 159
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :pvs)

(export '(set-type kind-of-name-expr set-type-actuals
		   subst-actual-in-remaining-formals set-lambda-dep-types
		   free-formals
		   subst-for-formals))

(defvar *dont-worry-about-full-instantiations* nil)

(defvar *ignore-for-tccs* nil)

(defmethod set-type ((ex expr) expected)
  (assert *current-context*)
  (assert *generate-tccs*)
  (cond ((or (not (type ex))
	     (eq *generate-tccs* 'all))
	 (set-type* ex expected))
	((eq *generate-tccs* 'top)
	 (check-for-subtype-tcc ex expected))))

(defmethod set-type ((te type-expr) expected)
  (assert *current-context*)
  (set-type* te expected))

(defmethod set-type ((te dep-binding) expected)
  (set-type* te expected))

(defmethod typed? ((expr expr))
  (and (not (memq *generate-tccs* '(all top)))
       (type expr)))

(defmethod typed? ((expr name-expr))
  (and (call-next-method)
       (singleton? (resolutions expr))))

(defmethod typed? ((act actual))
  (or (type-value act)
      (and (typep (expr act) 'expr)
	   (type (expr act)))))


;;; This is the basic function for selecting the best type from the set of
;;; possible types, and setting the type slot accordingly.  After setting
;;; the type, subtype tccs are checked for.

(defun set-expr-type (expr expected)
  (unless (type expr)
    #+pvsdebug (assert (not (duplicates? (ptypes expr))))
    (let* ((ptypes (types expr))
	   (possibles
	    (delete-if-not #'(lambda (ty)
			       (and (not (eq ty 'type))
				    (compatible? ty expected)))
	      ptypes)))
      (cond ((singleton? possibles)
	     (setf (type expr)
		   (if (fully-instantiated? (car possibles))
		       (car possibles)
		       (instantiate-from (car possibles) expected expr)))
	     #+pvsdebug (assert (fully-instantiated? (type expr))))
	    ((null possibles)
	     (type-incompatible expr ptypes expected))
	    (t (type-ambiguity expr))))))

(defvar *has-tccs* nil
  "Used to keep track of whether an expression generated any TCCs that
required a context.")

(defmethod set-type* :around ((ex expr) expected)
  #+pvsdebug (assert (fully-typed? expected))
  #+pvsdebug (assert (fully-instantiated? expected))
  #+pvsdebug (assert (or (type ex) (types ex)))
  #+pvsdebug (assert (not (and (type ex) (types ex))))
  #+pvsdebug (assert (every #'type-expr? (types ex)))
  #+pvsdebug (assert (or (null (type ex)) (fully-instantiated? ex)))
  (unless (type ex)
    (cond ((some #'(lambda (ty)
		     (and (not (from-conversion ty))
			  (compatible? ty expected)))
		 (types ex))
	   (call-next-method))
	  ((and (not *no-conversions-allowed*)
		(some #'from-conversion (types ex)))
	   (change-to-conversion ex expected))
	  ((and (not *no-conversions-allowed*)
		(look-for-conversion ex expected)))
	  (t (call-next-method)
	     (unless (type ex)
	       (type-incompatible ex (types ex) expected))))
    (setf (types ex) nil))
  #+pvsdebug (assert (fully-typed? ex))
  #+pvsdebug (assert (fully-instantiated? ex))
  (unless (typep ex '(or branch lambda-expr))
    (check-for-subtype-tcc ex expected)))

(defun look-for-conversion (expr expected)
  (let ((ctypes (argument-conversions** (types expr) expected)))
    (when ctypes
      (change-to-conversion expr expected
			    (get-conversion-range-type (car ctypes) expr)))))


(defun change-to-conversion (expr expected &optional convtype)
  (let ((ctype (or convtype
		   (find-if #'(lambda (ty)
				(and (from-conversion ty)
				     (compatible? ty expected)))
		     (types expr)))))
    (assert ctype)
    (cond
     ((and (typep expr 'application)
	   (typep (operator expr) 'name-expr)
	   (some #'(lambda (r)
		     (and (typep r 'lambda-conversion-resolution)
			  (let ((c (if (consp (conversion r))
				       (car (conversion r))
				       (conversion r))))
			    (compatible? (range (type c)) ctype))))
		 (resolutions (operator expr))))
      (change-application-to-conversion expr)
      (set-type expr expected)
      expr)
     (t
      #+pvsdebug (assert (typep (from-conversion ctype) 'expr))
      (add-conversion-info (from-conversion ctype) expr)
      (let* ((nexpr (copy expr))
	     (dom (domain (type (from-conversion ctype)))))
	(change-class expr 'implicit-conversion)
	(setf (argument expr) nexpr)
	(setf (types nexpr) (list (if (typep dom 'dep-binding) (type dom) dom)))
	(setf (operator expr) (copy (from-conversion ctype)))
	(setf (types expr) (list ctype))
	(when (typep (argument expr) 'name-expr)
	  (setf (resolutions (argument expr)) nil
		(types (argument expr)) nil))
	;; If expr already has a type, this doesn't do anything
	(typecheck* expr expected nil nil))))))

(defun add-conversion-info (conversion expr)
  (let ((str "pretty-print-expanded (M-x ppe) shows the conversions."))
    (unless *in-checker*
      (pushnew str (info *current-theory*) :test #'string=)))
  (let ((conv-name (if (typep conversion 'name-expr)
		       conversion
		       (name conversion))))
    (pvs-info "In declaration ~a:~
             ~%  added conversion ~a~
             ~%  to ~a, converting~
             ~%     ~a~%  to ~a"
      (if (typep (declaration *current-context*) 'declaration)
	  (id (declaration *current-context*))
	  (unparse (declaration *current-context*) :string t))
      (raise-actuals conversion 1)
      expr
      (domain (find-supertype (type conv-name)))
      (range (find-supertype (type conv-name))))))

(defmethod set-type* ((ex name-expr) expected)
  (assert (or (null (type ex)) (resolution ex)))
  (unless (type ex)
    (let* ((creses (remove-if-not #'(lambda (r)
				      (compatible? (type r) expected))
		     (resolutions ex)))
	   (sreses (remove-if-not
		       #'(lambda (r)
			   (strict-compatible? (type r) expected))
		     creses)))
      (set-type-name-expr ex expected
			  (or sreses creses (resolutions ex))))))

(defun set-type-name-expr (ex expected creses)
  (assert creses)
  (setf (resolutions ex) creses)
  (let ((res (find-best-name-resolution ex creses expected)))
    (setf (resolutions ex) (list res))
    (when (actuals (module-instance ex))
      (setf (module-instance (resolution ex))
	    (set-type-actuals ex)))
    (change-name-expr-class-if-needed (declaration res) ex)
    (let ((alias (cdr (assq (declaration res) (boolean-aliases)))))
      (when alias
	(setf (declaration res) alias)))
    (when (and (typep (declaration ex) 'bind-decl)
	       (not (memq (declaration ex) *bound-variables*)))
      (type-error ex "Bound variable outside of context"))
    ;; Why not just check for actuals of the module-instance
    (unless (fully-instantiated? res)
      (let ((nres (instantiate-resolution ex res expected)))
	(if (or *dont-worry-about-full-instantiations*
		(fully-instantiated? nres))
	    (setf res nres
		  (resolutions ex) (list nres))
	    (type-error ex
	      "Could not determine the full theory instance for ~a" ex))))
    (unless (compatible? (type res) expected)
      (type-incompatible ex (list (type res)) expected))
    (setf (type ex) (type res))))

(defmethod change-name-expr-class-if-needed ((decl field-decl) expr)
  (change-class expr 'field-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-constructor-decl) expr)
  (change-class expr 'constructor-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-constructor-decl)
					     (expr null-expr)))

(defmethod change-name-expr-class-if-needed ((decl adt-recognizer-decl) expr)
  (change-class expr 'recognizer-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-accessor-decl) expr)
  (change-class expr 'accessor-name-expr))

(defmethod change-name-expr-class-if-needed (decl expr)
  (declare (ignore decl expr))
  nil)


(let ((boolean-alias-table nil))
  (defun boolean-aliases ()
    (or boolean-alias-table
	(let ((bth (get-theory "booleans")))
	  (when bth
	    (let ((bdecls (theory bth)))
	      (setq boolean-alias-table
		    (list (cons (find '& bdecls :key #'id)
				(find 'AND bdecls :key #'id))
			  (cons (find '=> bdecls :key #'id)
				(find 'IMPLIES bdecls :key #'id))
			  (cons (find '<=> bdecls :key #'id)
				(find 'IFF bdecls :key #'id)))))))))
  (defun reset-boolean-aliases ()
    (setq boolean-alias-table nil)))

;;; The order of preference is those that are bound-variables, then the
;;; local resolutions (current theory, context, or library, in that
;;; order).  If a unique resolution is determined that way, then find the
;;; best resolution from the resolution and its associated judgements and
;;; return it.  Otherwise an ambiguity error is invoked.
	
(defun bound-variable-resolution (resolutions)
  (let ((breses (remove-if-not #'(lambda (r)
				   (memq (declaration r) *bound-variables*))
		  resolutions)))
    (when breses
      (labels ((first-one (reses res rest-of-bvars)
		  (if (null reses)
		      res
		      (let ((nrest-of-bvars (memq (declaration (car reses))
						  rest-of-bvars)))
			(first-one (cdr reses)
				   (if nrest-of-bvars res (car reses))
				   (if (and (cdr reses)
					    (null nrest-of-bvars))
				       (memq (declaration (car reses))
					     *bound-variables*)
				       rest-of-bvars))))))
	(first-one (cdr breses)
		   (car breses)
		   (memq (declaration (car breses))
			 *bound-variables*))))))

(defun find-best-name-resolution (ex resolutions expected)
  (if (cdr resolutions)
      (or (bound-variable-resolution resolutions)
	  (let* ((mreses (find-tc-matching-resolutions resolutions expected))
		 (lreses (filter-local-resolutions (or mreses resolutions))))
	    (if (cdr lreses)
		(let ((dreses (or (remove-if-not #'fully-instantiated? lreses)
				  lreses)))
		  (if (cdr dreses)
		      (let ((mreses (or (remove-if #'from-datatype-modname? dreses)
					dreses)))
			(if (cdr mreses)
			    (let ((freses (or (remove-if-not
						  #'instantiated-importing?
						mreses)
					      mreses)))
			      (cond ((cdr freses)
				     (setf (resolutions ex) freses)
				     (type-ambiguity ex))
				    (t (car freses))))
			    (car mreses)))
		      (car dreses)))
		(car lreses))))
      (car resolutions)))

(defun instantiated-importing? (res)
  (find-if #'(lambda (x) (tc-eq x (module-instance res)))
    (gethash (get-theory (module-instance res))
	     (using-hash *current-context*))))

(defun from-datatype-modname? (res)
  (typep (find-if #'(lambda (x) (tc-eq x (module-instance res)))
	   (gethash (get-theory (module-instance res))
		    (using-hash *current-context*)))
	 'datatype-modname))

(defun find-tc-matching-resolutions (resolutions expected &optional mreses)
  (cond ((null resolutions)
	 (nreverse mreses))
	((fully-instantiated? (car resolutions))
	 (find-tc-matching-resolutions
	  (cdr resolutions) expected
	  (if (and (not (member (car resolutions) mreses :test #'tc-eq))
		   (compatible? (type (car resolutions)) expected))
	      (cons (car resolutions) mreses)
	      mreses)))
	(t (let* ((bindings (tc-match expected (type (car resolutions))
				      (mapcar #'(lambda (x) (cons x nil))
					      (formals-sans-usings
					       (get-theory
						(module-instance
						 (car resolutions)))))))
		  (mres (when (every #'cdr bindings)
			  (subst-mod-params (car resolutions)
					    (car (create-compatible-modinsts
						  (module-instance (car resolutions))
						  (list bindings)
						  nil))))))
	     (find-tc-matching-resolutions
	      (cdr resolutions) expected
	      (if (and mres
		       (not (member mres mreses :test #'tc-eq)))
		  (cons mres mreses)
		  mreses))))))

(defun kind-of-name-expr (expr)
  #+pvsdebug (assert (every #'declaration *bound-variables*))
  (or (kind expr)
      (if (or (memq (declaration expr) *bound-variables*)
	      (typep (declaration expr) '(or var-decl bind-decl dep-binding)))
	  'variable
	  'constant)))


;;; should be split into two methods

(defmethod set-type-actuals ((modinst modname))
  (let ((fmls (formals-sans-usings (get-theory modinst))))
    (set-type-actuals* (actuals modinst) fmls))
  (let ((nmodinst (simplify-modinst modinst)))
    (unless (eq *generate-tccs* 'none)
      (generate-assuming-tccs nmodinst modinst)
      ;; Compare the given actuals with those determined by the typechecker 
      ;;(generate-actuals-tccs (actuals expr) (actuals nmodinst))
      )
    nmodinst))

(defmethod set-type-actuals ((expr name))
  #+pvsdebug (assert (null *set-type-actuals-name*))
  (let* ((modinst (module-instance expr))
	 (*typecheck-using* nil)
	 (*set-type-actuals-name* expr))
    (unless modinst
      (type-ambiguity expr))
    (when (and (not *dont-worry-about-full-instantiations*)
	       (some #'null (actuals modinst)))
      (type-error expr
	"Could not determine the full theory instance for ~a~
         ~%  Theory instance: ~a" expr (full-name (module-instance expr))))
    ;; if modname was previously generated without allowing tccs
    ;; now we have to force tccs. (unless we don't want any tccs.)
    ;; see resolve.lisp for where modname-no-tccs are generated
    (let ((*generate-tccs* (cond ((eq *generate-tccs* 'none) 'none)
				 ((typep modinst 'modname-no-tccs) 'all)
				 (t *generate-tccs*)))
	  (fmls (formals-sans-usings (get-theory modinst))))
      (set-type-actuals* (actuals modinst) fmls))
    (when (typep modinst 'modname-no-tccs)
      (change-class modinst 'modname))
    #+pvsdebug (fully-typed? (actuals modinst))
    ;; this only should occur in one of the two methods
    (when (actuals expr)
      (mapc #'(lambda (ea ma)
		(unless (type-value ma)
		  (setf (type-value ea) nil)
		  (set-type* (expr ea) (type (expr ma)))))
	    (actuals expr) (actuals modinst)))
    (let ((nmodinst (simplify-modinst modinst)))
      (unless (eq *generate-tccs* 'none)
	(generate-assuming-tccs nmodinst expr)
	;; Compare the given actuals with those determined by the typechecker
	(generate-actuals-tccs (actuals expr) (actuals nmodinst)))
      nmodinst)))


;;; Sets the types of the actuals.

(defun set-type-actuals* (actuals formals &optional alist)
  (when actuals
    (let ((act (car actuals))
	  (form (car formals)))
      (multiple-value-bind (nform nalist)
	  (subst-actuals-in-next-formal act form alist)
	(set-type-actual act nform)
	(set-type-actuals* (cdr actuals) (cdr formals) nalist)))))

(defmethod set-type-actual (act (formal formal-type-decl))
  (cond ((type-value act)
	 (when (and (typep (expr act) 'name)
		    (zerop (parens (expr act)))
		    (actuals (expr act)))
	   (setf (resolutions (expr act))
		 (remove-if-not #'(lambda (res)
				    (typep (declaration res) 'type-decl))
		   (resolutions (expr act))))
	   (setf (module-instance (resolution (expr act)))
		 (set-type-actuals (expr act))))
	 #+pvsdebug (assert (fully-typed? act)))
	(t (type-error act
	     "Expression provided where a type is expected"))))

(defmethod set-type-actual (act (formal formal-subtype-decl))
  (call-next-method)
  (let* ((tact (type-value act))
	 (texp (type-value formal))
	 (vid (make-new-variable '|x| act))
	 (vb (typecheck* (mk-bind-decl vid tact) nil nil nil))
	 (svar (mk-name-expr vid nil nil
			     (make-resolution vb (current-theory-name) tact))))
    (check-for-subtype-tcc svar (supertype texp))))

(defmethod set-type-actual (act (formal formal-const-decl))
  #+pvsdebug (assert (fully-typed? (type formal)))
  (unless (and (typep (expr act) 'expr)
	       (or (type (expr act))
		   (setf (types (expr act))
			 (delete-if-not #'type-expr? (types (expr act))))))
    (type-error act
      "Type provided where an expression is expected"))
  (set-type* (expr act) (type formal))
  (setf (type-value act) nil)
  #+pvsdebug (assert (fully-instantiated? (type formal)))
  #+pvsdebug (assert (fully-instantiated? (type (expr act))))
  #+pvsdebug (assert (fully-instantiated? (resolution (expr act))))
  #+pvsdebug (assert (fully-typed? (expr act))))

(defvar *simplify-actuals* t)

(defun simplify-modinst (modinst)
  (if (and *simplify-actuals*
	   (actuals modinst))
      (let ((nactuals (simplify-actuals (actuals modinst))))
	(if (equal nactuals (actuals modinst))
	    modinst
	    (copy modinst 'actuals nactuals)))
      modinst))

(defun simplify-actuals (actuals &optional result)
  (if (null actuals)
      (nreverse result)
      (simplify-actuals
       (cdr actuals)
       (cons (if (type-value (car actuals))
		 (car actuals)
		 (let ((nexpr (pseudo-normalize (expr (car actuals)))))
		   (if (tc-eq nexpr (expr (car actuals)))
		       (car actuals)
		       (lcopy (car actuals)
			 'expr nexpr))))
	     result))))
  


;;; subst-actuals-in-next-formal takes an actual, a corresponding formal,
;;; and an alist mapping formals to actuals and returns two values:
;;;   1. A substituted form of the formal
;;;   2. The new alist      

(defun subst-actuals-in-next-formal (act formal alist)
  (let* ((pred (get-actual-subtype-predicate act formal alist))
	 (nalist (if pred
		     (acons (car (generated formal)) pred alist)
		     alist))
	 (nform (if (and alist (dependent? formal))
		    (subst-acts-in-form formal nalist)
		    formal)))
    (when (and (typep formal 'formal-subtype-decl)
	       (not (eq nform formal)))
      (setf (print-type (type-value nform)) nil))
    (values nform (acons nform act nalist))))

(defmethod get-actual-subtype-predicate (act (formal formal-subtype-decl) alist)
  (let ((tact (type-value act)))
    (if (type-value act)
	(let ((tform (subst-acts-in-form (supertype (type-value formal))
					 alist)))
	  (if (compatible? tact tform)
	      (let* ((*generate-tccs* 'none))
		(or (subtype-pred tact tform)
		    (gen-true-lambda tform)))
	      (type-incompatible act (list tact) tform)))
	(type-error act
	  "Expression provided where a type is expected"))))

(defmethod get-actual-subtype-predicate (act formal alist)
  (declare (ignore act formal alist))
  nil)

(defun subst-acts-in-form (formal alist)
  (gensubst formal
    #'(lambda (ex) (subst-acts-in-form! ex alist))
    #'(lambda (ex) (subst-acts-in-form? ex alist))))

(defmethod subst-acts-in-form? ((ex name) alist)
  (assoc (declaration ex) alist :test #'subst-formal-eq))

(defmethod subst-acts-in-form? (ex alist)
  (declare (ignore ex alist))
  nil)

(defmethod subst-acts-in-form! ((ex name) alist)
  (let ((act (cdr (assoc (declaration ex) alist :test #'subst-formal-eq))))
    (if (typep act 'actual)
	(or (type-value act)
	    (expr act))
	act)))

(defmethod subst-acts-in-form! ((ex actual) alist)
  (let ((act (cdr (assoc (declaration (expr ex)) alist
			 :test #'subst-formal-eq))))
    act))

(defun subst-formal-eq (formal1 formal2)
  (and (eq (class-of formal1) (class-of formal2))
       (same-id formal1 formal2)
       (eq (module formal1) (module formal2))))

(defun gen-true-lambda (type)
  (let* ((id '|x|)
	 (bd (typecheck* (mk-bind-decl id type type) nil nil nil))
	 ;;(nvar (mk-name-expr id nil nil (make-resolution bd nil type)))
	 )
    (make-lambda-expr (list bd) *true*)))


(defun generate-actuals-tccs (acts macts)
  (when acts
    (unless (tc-eq (car acts) (car macts))
      (generate-actuals-tcc (car acts) (car macts)))
    (generate-actuals-tccs (cdr acts) (cdr macts))))	  


;;; Setting the type for number-exprs is complicated by the fact the there may
;;; now be judgements given on numbers.  If there is a single ptype, then
;;; there were no judgements.  If nat is a subtype of the expected type, then
;;; the judgements were unnecessary.  Otherwise we look for the "best"
;;; matching judgement type, and use that.

(defmethod set-type* ((ex number-expr) expected)
  (unless (compatible? expected *number*)
    (type-incompatible ex (ptypes ex) expected))
  (setf (type ex) *number*))

(defun check-for-subtype-tcc (ex expected)
  #+pvsdebug (assert (fully-instantiated? expected))
  #+pvsdebug (assert (fully-instantiated? ex))
  #+pvsdebug (assert (fully-typed? ex))
  (unless (or (eq *generate-tccs* 'none)
	      (subtype-of? (type ex) expected))
    (let ((*generate-tccs* 'none)
	  (type (type ex)))
      (or (and (not (strict-compatible? type expected))
	       (find-funtype-conversion type expected ex))
	  (let* ((jtypes (judgement-types+ ex))
		 (incs (compatible-predicates jtypes expected ex)))
	    (when incs
	      (generate-subtype-tcc ex expected incs)))))))

(defmethod compatible-predicates (types expected ex &optional incs)
  (if (null types)
      incs
      (compatible-predicates
       (cdr types) expected ex
       (let ((npreds (compatible-preds (car types) expected ex)))
	 (if incs
	     (or (nintersection incs npreds :test #'tc-eq)
		 npreds)
	     npreds)))))


(defun find-funtype-conversion (type expected expr)
  (unless (or *no-conversions-allowed*
	      (typep expr '(or implicit-conversion binding)))
    (let* ((ftype (mk-funtype (list type) expected))
	   (conversions (get-conversions ftype)))
      (when conversions
	(let ((nexpr (copy expr)))
	  (add-conversion-info (car conversions) expr)
	  ;; Warn if more than one possible.
	  (change-class expr 'implicit-conversion)
	  ;;(setf (abstract-syntax expr) nil)
	  (setf (argument expr) nexpr)
	  (setf (operator expr) (copy (car conversions)))
	  (setf (type expr) nil (types expr) nil)
	  (set-type-actuals (operator expr))
	  (typecheck* expr expected nil nil)
	  expr)))))

;(defun find-funtype-conversion (type expected expr)
;  (unless (typep expr 'implicit-conversion)
;    (let* ((ftype (mk-funtype (list type) expected))
;	   (conversions (get-conversions ftype)))
;      (if conversions
;	  (let ((nexpr (copy expr)))
;	    (pvs-warning "Added conversion ~a to ~a, converting ~%     ~a~% to  ~a"
;			 (car conversions) expr type expected)
;	    ;; Warn if more than one possible.
;	    (change-class expr 'implicit-conversion)
;	    ;;(setf (abstract-syntax expr) nil)
;	    (setf (argument expr) nexpr)
;	
;	    (setf (operator expr) (copy (car conversions)))
;	    (typecheck* expr expected nil nil)
;	    expr)
;	  (let* ((stype (compatible-type (domain type) (domain expected)))
;		 (sftype (mk-funtype (list stype) (range type)))
;		 (ftype1 (mk-funtype (list type) sftype))
;		 (ftype2 (mk-funtype (list sftype) expected))
;		 (conversions1 (get-conversions ftype1))
;		 (conversions2 (get-conversions ftype2)))
;	    (when (and conversions1 conversions2)
;	      (let ((nexpr (copy expr)))
;		(change-class expr 'implicit-conversion)
;		)))))))


(defmethod set-type* ((expr tuple-expr) expected)
  (let ((stype (find-supertype expected))
	(exprs (exprs expr)))
    (unless (typep stype 'tupletype)
      (type-incompatible expr (ptypes expr) expected))
    (let ((types (types stype)))
      (unless (= (length exprs) (length types))
	(type-error expr "Wrong arity: expect ~d subexpressions"
		    (length types)))
      (set-tup-types exprs types)
      (setf (type expr) (mk-tupletype (mapcar #'type exprs))))))

(defun set-tup-types (exprs etypes)
  (when exprs
    (let* ((dep? (typep (car etypes) 'dep-binding))
	   (type (if dep?
		     (type (car etypes))
		     (car etypes))))
      (set-type* (car exprs) type)
      (set-tup-types (cdr exprs)
		     (if dep?
			 (substit (cdr etypes)
			   (acons (car etypes) (car exprs) nil))
			 (cdr etypes))))))


(defmethod set-type* ((expr cases-expr) expected)
  (set-type* (expression expr) (car (ptypes (expression expr))))
  (let* ((atype (find-supertype (type (expression expr))))
	 (adt (adt atype)))
    (mapc #'(lambda (s)
	      (let* ((equality (make-selection-equality s expr))
		     (*bound-variables* (append (args s) *bound-variables*))
		     (*tcc-conditions* (cons equality *tcc-conditions*)))
		(setf (resolutions (constructor s))
		      (set-sel-constr (constructor s) atype))
		(if (resolution (constructor s))
		    (let ((ctype (car (ptypes (constructor s)))))
		      (setf (type (constructor s)) ctype))
		    (type-ambiguity (constructor s)))
		(change-class (constructor s) 'constructor-name-expr)
		(setf (constructor s) (subst-mod-params (constructor s)
							(module-instance atype)))
		(set-type* (expression s) expected)))
	  (selections expr))
    (if (else-part expr)
	(let ((*tcc-conditions* (append (make-else-cases-conditions expr)
					 *tcc-conditions*)))
	  (set-type* (else-part expr) expected))
	(generate-selections-tccs expr (constructors adt) adt)))
  (setf (type expr) expected)
  ;;(setf (types expr) nil)
  )

(defun make-selection-equality (sel expr)
  (make-equation
   (expression expr)
   (typecheck* (if (args sel)
		   (mk-application* (copy (constructor sel))
		     (mapcar #'mk-name-expr (args sel)))
		   (copy (constructor sel)))
	       (find-supertype (type (expression expr))) nil nil)))

(defun make-else-cases-conditions (expr)
  (mapcar #'(lambda (s)
	      (let* ((type (type (constructor s)))
		     (rtype (if (and (funtype? type)
				     (adt (supertype (range type))))
				(range type)
				type))
		     (id (if (typep (predicate rtype) 'coercion)
			     (id (args1 (predicate rtype)))
			     (id (predicate rtype)))))
		(typecheck* (mk-application 'NOT
			      (mk-application id
				(expression expr)))
			    *boolean* nil nil)))
	  (selections expr)))


(defun set-sel-constr (constructor type)
  (let* ((res (resolutions constructor))
	 (res1 (remove-if-not
		   #'(lambda (r)
		       (and (typep (declaration r) 'adt-constructor-decl)
			    (compatible? type
					 (if (and (funtype? (type r))
						  (adt (supertype
							(range (type r)))))
					     (range (type r))
					     (type r)))))
		 res))
	 (res2 (remove-if-not
		   #'(lambda (r)
		       (tc-eq type (find-supertype (type r))))
		 (or res1 res)))
	 (res3 (remove-if-not #'fully-instantiated? (or res2 res1 res))))
    (or res3 res2 res1 res)))



(defmethod set-type* ((ex projection-application) expected)
  (let ((ptypes (remove-if-not
		    #'(lambda (pty)
			(let ((types (types (find-supertype pty))))
			  (compatible? (nth (1- (index ex)) types)
				       expected)))
		  (ptypes (argument ex)))))
    (cond ((null ptypes)
	   (type-incompatible
	    ex
	    (projection-application-types (ptypes (argument ex)) ex)
	    expected))
	  ((cdr ptypes)
	   (cond ((typep (argument ex) 'name-expr)
		  (let ((reses (filter-local-resolutions
				(resolutions (argument ex)))))
		    (cond ((cdr reses)
			   (setf (resolutions (argument ex)) reses)
			   (setf (types (argument ex))
				 (remove-duplicates (mapcar #'type reses)
				   :test #'tc-eq))
			   (type-ambiguity (argument ex)))
			  (reses
			   (setf (resolutions (argument ex)) reses)
			   (setf (types (argument ex))
				 (list (type (car reses))))
			   (set-type* (argument ex) (type (car reses)))
			   (setf (types ex)
				 (list (projection-application-type
					ex (type (car reses))))))
			  (t (setf (types (argument ex)) ptypes)
			     (type-ambiguity (argument ex))))))
		 (t (setf (types (argument ex)) ptypes)
		    (type-ambiguity (argument ex)))))
	  (t (set-type* (argument ex) (car ptypes))
	     (setf (type ex)
		   (projection-application-type
		    ex (type (argument ex))))))))

(defmethod set-type* ((expr field-application) expected)
  (unless (typed? expr)
    (let ((ptypes (remove-if-not
		      #'(lambda (pty)
			  (let ((fields (fields (find-supertype pty))))
			    (compatible? (type (find expr fields
						     :test #'same-id))
					 expected)))
		    (ptypes (argument expr)))))
      (cond ((null ptypes)
	     (type-incompatible
	      expr
	      (field-application-types (ptypes (argument expr)) expr)
	      expected))
	    ((cdr ptypes)
	     (setf (types (argument expr)) ptypes)
	     (type-ambiguity (argument expr)))
	    (t (set-type* (argument expr) (car ptypes))
	       (set-expr-type expr expected))))))


;;; Applications

(defmethod set-type* ((ex application) expected)
  (with-slots (operator argument type) ex
    (let* ((types (types ex))
	   (ptypes1 (remove-if-not #'(lambda (ty) (tc-eq ty expected)) types))
	   (ptypes (or ptypes1
		       (remove-if-not #'(lambda (ty) (compatible? ty expected))
			 types)))
	   (ftypes (or (remove-if-not #'fully-instantiated? ptypes) ptypes)))
      (if (null ftypes)
	  (type-incompatible ex types expected)
	  (let ((optype (determine-operator-type
			 operator argument expected)))
	    (cond ((and (typep argument 'tuple-expr)
			(boolean-op? operator
				     '(and & implies => or when)))
		   (setf type (car ftypes))
		   (let* ((reses (delete-if-not
				     #'(lambda (r)
					 (eq (id (module-instance r))
					     '|booleans|))
				   (resolutions operator)))
			  (alias (cdr (assq (declaration (car reses))
					    (boolean-aliases)))))
		     (setf (resolutions operator) reses)
		     (when alias
		       (setf (declaration (car reses)) alias)))
		   (set-conditional-arg-types
		    (id operator) (args1 ex) (args2 ex))
		   (setf (type operator) (type (resolution operator)))
		   (setf (types operator) nil)
		   (setf (type argument)
			 (mk-tupletype (list *boolean* *boolean*)))
		   (setf (types argument) nil)
		   (setf (type ex) *boolean*)
		   (change-to-propositional-class ex)
		   ;;(check-for-subtype-tcc ex expected)
		   )
		  (t #+pvsdebug (assert (fully-instantiated? optype))
		     (set-type* argument (domain optype))
		     (setf type (application-range-type argument optype))
		     (let ((*tcc-conditions*
			    (nconc (appl-tcc-conditions operator argument)
				   *tcc-conditions*)))
		       (set-type* operator optype))
		     (cond ((and (typep operator 'field-name-expr)
				 (not (memq (declaration operator)
					    *bound-variables*))
				 (typep (find-supertype (domain optype))
					'recordtype))
			    (change-class ex 'field-application)
			    (setf (id ex) (id operator)))
			   (t (change-application-class-if-needed
			       ex))))))))
    #+pvsdebug (assert (fully-instantiated? ex))
    (when (and (not *in-checker*)
	       (not *generating-adt*)
	       (not (eq *generate-tccs* 'none))
	       (typep (declaration *current-context*) 'def-decl))
      (multiple-value-bind (name arguments)
	  (rec-call-of-depth ex nil
			     (measure-depth (declaration *current-context*)))
	(when name
	  (generate-recursive-tcc name arguments))))))

(defun change-application-class-if-needed (ex)
  (let ((operator (operator ex)))
    (cond ((and (typep operator 'name-expr)
		(boolean-op? operator '(not and & or implies => iff <=> when)))
	   (change-to-propositional-class ex))
	  ((and (typep operator 'name-expr)
		(eq (id operator) '=)
		(eq (id (module-instance operator)) '|equalities|))
	   (change-to-propositional-class ex))
	  ((and (typep operator 'name-expr)
		(eq (id operator) '/=)
		(eq (id (module-instance operator)) '|notequal|))
	   (change-to-propositional-class ex))
	  ((and (typep operator 'name-expr)
		(eq (id operator) 'IF)
		(eq (id (module-instance operator)) '|if_def|)
		(not (typep ex 'branch)))
	   (change-class ex 'branch)))))

(defmethod change-to-propositional-class ((ex propositional-application))
  nil)

(defmethod change-to-propositional-class ((ex equation))
  nil)

(defmethod change-to-propositional-class ((ex disequation))
  nil)

(defmethod change-to-propositional-class ((ex branch))
  nil)

(defmethod change-to-propositional-class ((ex unary-application))
  (case (id (operator ex))
    ((NOT) (change-class ex 'unary-negation))))

(defmethod change-to-propositional-class ((ex infix-application))
  (case (id (operator ex))
    ((AND &) (change-class ex 'infix-conjunction))
    ((OR) (change-class ex 'infix-disjunction))
    ((IMPLIES =>) (change-class ex 'infix-implication))
    ((IFF <=>) (change-class ex 'infix-iff))
    (WHEN (change-class ex 'infix-when-expr)
	  (setf (operator ex) (mk-implies-operator))
	  (setf (exprs (argument ex)) (reverse (exprs (argument ex)))))
    (= (if (compatible? (type (args1 ex)) *boolean*)
	   (change-class ex 'infix-boolean-equation)
	   (change-class ex 'infix-equation)))
    (/= (change-class ex 'infix-disequation))))

(defmethod change-to-propositional-class ((ex application))
  (case (id (operator ex))
    ((NOT) (change-class ex 'negation))
    ((AND &) (change-class ex 'conjunction))
    ((OR) (change-class ex 'disjunction))
    ((IMPLIES =>) (change-class ex 'implication))
    ((IFF <=>) (change-class ex 'iff))
    (WHEN (change-class ex 'when-expr)
	  (setf (operator ex) (mk-implies-operator))
	  (setf (exprs (argument ex)) (reverse (exprs (argument ex)))))
    (= (if (compatible? (type (args1 ex)) *boolean*)
	   (change-class ex 'boolean-equation)
	   (change-class ex 'equation)))
    (/= (change-class ex 'disequation))))

(defmethod change-to-propositional-class ((ex else-condition))
  ex)
    

(defmethod appl-tcc-conditions ((op lambda-expr) argument)
  (with-slots (bindings) op
    (if (cdr bindings)
	(if (typep argument 'tuple-expr)
	    (pairlis bindings (exprs argument))
	    (pairlis bindings (make-projections argument)))
	(acons (car bindings) argument nil))))

(defmethod appl-tcc-conditions (op argument)
  (declare (ignore op argument))
  nil)

(defun set-conditional-arg-types (op arg1 arg2)
  (case op
    ((and & implies =>)
     (set-type* arg1 *boolean*)
     (let ((*tcc-conditions* (cons arg1 *tcc-conditions*)))
       (set-type* arg2 *boolean*)))
    ((or)
     (set-type* arg1 *boolean*)
     (let ((*tcc-conditions* (cons (make!-negation arg1) *tcc-conditions*)))
       (set-type* arg2 *boolean*)))
    ((when)
     (set-type* arg2 *boolean*)
     (let ((*tcc-conditions* (cons arg2 *tcc-conditions*)))
       (set-type* arg1 *boolean*)))))

(defun determine-operator-type (operator argument expected)
  (if (type operator)
      (if (fully-instantiated? (type operator))
	  (type operator)
	  (instantiate-operator-type
	   (type operator) operator (argument-list argument) expected))
      (let ((coptypes (delete-if-not #'(lambda (ty)
					 (compatible? (range ty) expected))
			(types operator))))
	(unless coptypes
	  (type-incompatible operator (types operator) expected))
	(let* ((optypes1 (if (cdr coptypes)
			     (or (delete-if-not
				     #'(lambda (oty)
					 (let ((dty (domain
						     (find-supertype oty))))
					   (some #'(lambda (aty)
						     (compatible? dty aty))
						 (types argument))))
				   coptypes)
				 coptypes)
			     coptypes))
	       (optypes2 (if (cdr optypes1)
			     (local-operator-types operator optypes1 argument)
			     optypes1))
	       (optypes3 (if (cdr optypes2)
			     (or (delete-if-not #'fully-instantiated? optypes1)
				 optypes2)
			     optypes2))
	       (optypes (if (cdr optypes3)
			    (instantiable-operator-types
			     operator optypes3 (argument* argument) expected)
			    optypes3)))
	  (assert optypes)
	  #+pvsdebug (assert (null (duplicates? optypes :test #'tc-eq)))
	  (cond ((cdr optypes)
		 (setf (types operator) optypes)
		 (when (typep operator 'name-expr)
		   (setf (resolutions operator)
			 (remove-if-not #'(lambda (r)
					    (member (type r) optypes
						    :test #'tc-eq))
			   (resolutions operator))))
		 (type-ambiguity operator))
		((fully-instantiated? (car optypes))
		 (car optypes))
		(t (instantiate-operator-type (car optypes) operator
					      (argument-list argument)
					      expected)))))))

(defmethod local-operator-types ((op name-expr) optypes argument)
  (let* ((reses (remove-if-not #'(lambda (r)
				   (member (type r) optypes :test #'tc-eq))
		  (resolutions op)))
	 (lreses (local-resolutions reses))
	 (loptypes (mapcar #'type lreses)))
    (if (cdr loptypes)
	(optypes-for-local-arguments argument loptypes)
	loptypes)))

(defun local-resolutions (reses)
  (or (let ((breses (remove-if-not #'(lambda (r)
				       (memq (declaration r)
					     *bound-variables*))
		      reses)))
	(when breses
	  (list (nearest-bound-variable breses))))
      (remove-if-not #'(lambda (r)
			 (eq (module (declaration r))
			     *current-theory*))
	reses)
      (remove-if #'(lambda (r)
		     (let ((th (module (declaration r))))
		       (or (typep th '(or library-theory
					  library-datatype))
			   (from-prelude? th))))
	reses)
      (remove-if #'(lambda (r)
		     (let ((th (module (declaration r))))
		       (from-prelude? th)))
	reses)
      reses))

(defmethod local-operator-types ((op application) optypes argument)
  (declare (ignore argument))
  (let* ((opoptypes (remove-if-not #'(lambda (oty1)
				       (let ((soty1 (find-supertype oty1)))
					 (some #'(lambda (oty2)
						   (compatible? oty2
								(range soty1)))
					       optypes)))
		      (types (operator op))))
	 (doptypes (local-operator-types
		    (operator op) opoptypes (argument op))))
    (if (length= optypes doptypes)
	(call-next-method)
	(let ((rtypes (application-range-types-op
		       doptypes (types (argument op))
		       (operator op) (argument op) nil)))
	  (remove-if-not #'(lambda (oty2)
			     (some #'(lambda (oty1)
				       (let ((soty1 (find-supertype oty1)))
					 (tc-eq oty2 soty1)))
				   rtypes))
	    optypes)))))

(defmethod local-operator-types ((op lambda-expr) optypes argument)
  (declare (ignore argument))
  (let* ((*bound-variables* (append (bindings op) *bound-variables*))
	 (pos (optypes-for-local-arguments* (expression op)
					    (mapcar #'range optypes))))
    (if pos
	(list (nth pos optypes))
	optypes)))

(defmethod local-operator-types (op optypes argument)
  (declare (ignore op))
  (optypes-for-local-arguments argument optypes))

(defun optypes-for-local-arguments (arg optypes)
  (let ((pos (optypes-for-local-arguments*
	      arg
	      (mapcar #'(lambda (oty)
			  (let ((dom (domain (find-supertype oty))))
			    (if (typep dom 'dep-binding)
				(type dom)
				dom)))
		optypes))))
    (if pos
	(list (nth pos optypes))
	optypes)))

(defmethod optypes-for-local-arguments* ((ex name-expr) domtypes)
  (with-slots (resolutions) ex
    (when (cdr resolutions)
      (let* ((reses (local-resolutions resolutions)))
	(when (singleton? reses)
	  (let* ((rtype (type (car reses)))
		 (dtypes (member rtype domtypes :test #'tc-eq)))
	    (unless (member rtype (cdr dtypes) :test #'tc-eq)
	      (position (car dtypes) domtypes :test #'tc-eq))))))))

(defmethod optypes-for-local-arguments* ((ex tuple-expr) domtypes)
  (let ((dtypes-list (mapcar #'types domtypes)))
    (optypes-for-local-arguments-list (exprs ex) dtypes-list)))

(defun dtypes-list (types-list)
  (when (car types-list)
    (cons (mapcar #'car types-list)
	  (dtypes-list (mapcar #'cdr types-list)))))

(defun optypes-for-local-arguments-list (exprs dtypes-list &optional pos)
  (if (null exprs)
      pos
      (if (length= (types (car exprs)) dtypes-list)
	  (let ((apos (optypes-for-local-arguments*
		       (car exprs) (mapcar #'car dtypes-list))))
	    (when (or (null apos)
		      (null pos)
		      (= pos apos))
	      (optypes-for-local-arguments-list
	       (cdr exprs) (mapcar #'cdr dtypes-list) (or apos pos))))
	  (optypes-for-local-arguments-list
	   (cdr exprs) (mapcar #'cdr dtypes-list) pos))))

(defmethod optypes-for-local-arguments* ((ex application) domtypes)
  (optypes-for-local-arguments*
   (operator ex)
   (mapcar #'(lambda (dtype)
	       (find-if #'(lambda (opty)
			    (tc-eq (range (find-supertype opty)) dtype))
		 (types (operator ex))))
     domtypes)))

(defmethod optypes-for-local-arguments* ((ex update-expr) domtypes)
  (optypes-for-local-arguments* (expression ex) domtypes))

(defmethod optypes-for-local-arguments* (ex domtypes)
  (declare (ignore ex domtypes))
  nil)

(defun instantiable-operator-types (op optypes args expected &optional result)
  (if (null optypes)
      (nreverse result)
      (instantiable-operator-types
       op (cdr optypes) args expected
       (if (instantiable-operator-type (car optypes) op args expected)
	   (cons (car optypes) result)
	   result))))

(defun instantiable-operator-type (optype op args expected)
  (or (fully-instantiated? optype)
      (let* ((frees (free-params optype))
	     (bindings (instantiate-operator-bindings frees))
	     (domain (domain-types optype))
	     (range (range optype)))
	(assert bindings)
	(let ((nbindings (tc-match-domain
			  op optype args domain
			  (tc-match expected range bindings))))
	  (not (null nbindings))))))


(defmethod set-type-application (expr (operator lambda-expr) argument expected)
  (with-slots (bindings expression) operator
    (let ((args (if (cdr bindings)
		    (get-arguments-list argument)
		    (list argument))))
      (set-type-application-lambda expr operator args argument expected))))

(defmethod get-arguments-list ((arg tuple-expr))
  (with-slots (exprs) arg
    exprs))

(defmethod get-arguments-list ((arg expr))
  #+pvsdebug (assert (singleton? (types arg)))
  (let ((*generate-tccs* 'none))
    (typecheck* arg (car (types arg)) nil nil)
    (if (typep (find-supertype (type arg)) 'tupletype)
	(make!-projections arg)
	(list arg))))

(defun set-type-application-lambda (expr operator args argument expected)
  (let ((bindings (bindings operator)))
    (unless (type expr)
      (mapc #'(lambda (b) (when (declared-type b)
			    (set-type (declared-type b) nil)))
	    bindings))
    (if (typep argument 'tuple-expr)
	(set-lambda-appl-types args
			       (mapcar #'type bindings)
			       bindings)
	(set-type* argument (if (cdr bindings)
				(make-tupletype-from-bindings bindings)
				(type (car bindings)))))
    (let* ((*tcc-conditions* (nconc (pairlis bindings args) *tcc-conditions*))
	   (otype (determine-operator-type operator argument expected)))
      (set-type* operator otype)
      (unless (type argument)
	(let ((etype (domain (find-supertype (type operator)))))
	  (set-expr-type argument etype)))
      (set-type-application* expr operator args argument expected otype))))

(defmethod set-type-application (expr operator argument expected)
  (let ((arguments (argument-list argument)))
    (set-type-application* expr operator arguments argument expected nil)))

(defun set-type-application* (expr operator args argument expected optype)
  (let* ((ptypes (ptypes expr))
	 (nexpected (if (or (type expr)
			    (every #'(lambda (ty)
				       (not (from-conversion ty)))
				   ptypes)
			    (some #'(lambda (ty)
				      (and (not (from-conversion ty))
					   (compatible? ty expected)))
				  ptypes))
			expected
			(find-if #'(lambda (ty)
				     (and (from-conversion ty)
					  (compatible? ty expected)))
			  ptypes)))
	 (otype (or optype
		    (determine-operator-type operator argument nexpected))))
    (set-type* operator otype)
    (cond ((and (typep operator 'field-name-expr)
		(not (memq (declaration operator) *bound-variables*))
		(typep (find-supertype (domain otype)) 'recordtype))
	   (change-class expr 'field-application)
	   (setf (id expr) (id operator))
	   (set-type* argument (domain otype))
	   (setf (types expr) (list (application-range-type argument otype)))
	   #+pvsdebug (assert (types expr))
	   (set-expr-type expr expected))
	  (t (set-type-application-arguments expr args argument
					     expected otype optype)))))

(defun set-type-application-arguments (expr args argument
					    expected otype lambda?)
  #+pvsdebug (assert (every #'(lambda (a) (or (typep a 'binding) (types a)))
			    args))
  (cond ((or (conjunction? expr)
	     (implication? expr))
	 (set-type* (car args) *boolean*)
	 (let ((*tcc-conditions* (cons (car args) *tcc-conditions*)))
	   (set-type* (cadr args) *boolean*)
	   (setf (type argument) (mk-tupletype (mapcar #'type args)))
	   (set-expr-type expr expected)))
	((disjunction? expr)
	 (set-type* (car args) *boolean*)
	 (let ((*tcc-conditions* (cons (make!-negation (car args))
				       *tcc-conditions*)))
	   (set-type* (cadr args) *boolean*)
	   (setf (type argument) (mk-tupletype (mapcar #'type args)))
	   (set-expr-type expr expected)))
	((boolean-when-expr? expr)
	 (set-type* (cadr args) *boolean*)
	 (let ((*tcc-conditions* (cons (cadr args) *tcc-conditions*)))
	   (set-type* (car args) *boolean*)
	   (setf (type argument) (mk-tupletype (mapcar #'type args)))
	   (set-expr-type expr expected)))
	(lambda?
	 (setf (types expr)
	       (cons (application-range-type argument otype)
		     (delete-if-not #'from-conversion (ptypes expr))))
	 (set-expr-type expr expected))
	(t (set-type* argument (if (typep (domain otype) 'dep-binding)
				   (type (domain otype))
				   (domain otype)))
	   (setf (types expr)
		 (cons (application-range-type argument otype)
		       (delete-if-not #'from-conversion (ptypes expr))))
	   (set-expr-type expr expected)))
  (when (and (not *in-checker*)
	     (not *generating-adt*)
	     (not (eq *generate-tccs* 'none))
	     (typep (declaration *current-context*) 'def-decl))
    (multiple-value-bind (name arguments)
	(rec-call-of-depth expr nil
			   (measure-depth (declaration *current-context*)))
      (when name
	(generate-recursive-tcc name arguments)))))

(defmethod subst-dep-range ((optype funtype) expr)
  (with-slots (domain range) optype
    (subst-dep-range* domain range expr)))

(defmethod subst-dep-range ((optype subtype) expr)
  (with-slots (supertype) optype
    (subst-dep-range supertype expr)))

(defmethod subst-dep-range* ((domain dep-binding) range expr)
  (let* ((nres (make-resolution domain (theory-name *current-context*)))
	 (nname (mk-name-expr (id domain) nil nil nres)))
    #+pvsdebug (assert (fully-typed? expr))
    (substit range (list (cons nname expr)))))

(defmethod subst-dep-range* ((domain type-expr) range expr)
  (declare (ignore expr))
  range)


(defun set-lambda-appl-types (exprs expected bindings)
  (when exprs
    (let ((type (car expected)))
      (set-type* (car exprs) type)
      (set-lambda-appl-types (cdr exprs)
			     (substit (cdr expected)
			       (acons (car bindings) (car exprs) nil))
			     (cdr bindings)))))

(defmethod from-conversion ((db dep-binding))
  (from-conversion (type db)))

(defun rec-call-of-depth (expr args depth)
  (when (typep expr 'application)
    (if (or (null depth) (zerop depth))
	(when (and (typep (operator expr) 'name-expr)
		   (eq (declaration (operator expr))
		       (declaration *current-context*)))
	  (values (operator expr) (cons (arguments expr) args)))
	(rec-call-of-depth (operator expr)
			   (cons (arguments expr) args)
			   (1- depth)))))

(defun instantiate-resolution (ex res type)
  (let ((theories (delete *current-theory*
			  (delete-duplicates (mapcar #'module
						     (free-params res)))))
	(nres nil))
    (dolist (th theories)
      (let ((bindings (tc-match type (type res)
				(mapcar #'(lambda (x) (cons x nil))
					(formals-sans-usings th)))))
	(when (every #'cdr bindings)
	  (push (subst-mod-params
		 res
		 (mk-modname (id th)
		   (mapcar #'(lambda (a) (mk-res-actual (cdr a) th))
			   bindings)))
		nres))))
    (cond ((cdr nres)
	   (type-ambiguity ex))
	  (nres (car nres))
	  (*dont-worry-about-full-instantiations* res)
	  (t (type-error ex
	       "Could not determine the full theory instance for ~a~
                ~%  Theory instance: ~a"
	       ex (full-name (module-instance ex)))))))

(defun instantiate-operator-type (optype op args expected)
  (let* ((frees (free-params optype))
	 (bindings (instantiate-operator-bindings frees))
	 (domain (domain-types optype))
	 (range (range optype)))
    (assert bindings)
    (let ((nbindings (tc-match-domain op optype args domain
				       (tc-match expected range
						 bindings))))
      (if (every #'cdr nbindings)
	  (instantiate-operator-from-bindings optype nbindings)
	  (if *dont-worry-about-full-instantiations*
	      optype
	      (type-error op
		"Could not determine the full theory instance for ~a~
                 ~:[~;~%  May be a problem with splitting dependent types~]~
                 ~@[~%  Theory instance: ~a~]"
		op
		(some #'(lambda (a)
			  (some #'(lambda (aty)
				    (and (typep (find-supertype aty)
						'(or funtype tupletype))
					 (dependent? (find-supertype aty))))
				(ptypes a)))
		      args)
		(when (typep op 'name-expr)
		  (full-name (module-instance op)))))))))

(defun instantiate-operator-bindings (formals)
  (let ((theories nil))
    (dolist (frm formals)
      (pushnew (module frm) theories :test #'eq))
    (let ((bindings nil))
      (dolist (th theories)
	(unless (eq th *current-theory*)
	  (setq bindings
		(nconc bindings (mapcar #'list (formals-sans-usings th))))))
      bindings)))

(defun instantiate-operator-from-bindings (optype bindings)
  (instantiate-operator-from-bindings*
   optype (bindings-to-modinsts bindings nil)))

(defun instantiate-operator-from-bindings* (optype modinsts)
  (if (null modinsts)
      optype
      (instantiate-operator-from-bindings*
       (let ((noptype (subst-mod-params optype (car modinsts))))
	 (assert (fully-instantiated? noptype))
	 noptype)
       (cdr modinsts))))

(defun bindings-to-modinsts (bindings modinsts)
  (if (null bindings)
      modinsts
      (let ((theory (module (caar bindings))))
	(bindings-to-modinsts
	 (nthcdr (length (formals-sans-usings theory)) bindings)
	 (cons (make-instance 'modname
		 'id (id theory)
		 'actuals (mapcar #'(lambda (a)
				      (mk-res-actual (cdr a) theory))
				  bindings))
	       modinsts)))))

(defun tc-match-domain (op optype args domain-types bindings)
  (cond ((every #'cdr bindings)
	 bindings)
	((null args)
	 bindings)
	(t (tc-match-domain op optype (cdr args) (cdr domain-types)
			    (tc-match-domain* (car args) (car domain-types)
					      bindings)))))

(defun tc-match-domain* (arg dom-type bindings)
  (tc-match-domain** (ptypes arg) dom-type bindings))

(defun tc-match-domain** (arg-types dom-type bindings)
  (if (null arg-types)
      bindings
      (tc-match-domain** (cdr arg-types) dom-type
			 (tc-match (car arg-types) dom-type bindings))))


;;; Returns the resolutions "closest" to the current theory.

(defun filter-local-resolutions (reses)
  (let ((resolutions
	 (remove-if #'(lambda (vr)
			(and (typep (declaration vr) 'var-decl)
			     (some #'(lambda (sr)
				       (and (typep (declaration sr)
						   'skolem-const-decl)
					    (tc-eq (type vr) (type sr))))
				   reses)))
	   reses)))
    (or (resolutions-of-current-theory* resolutions nil)
	(resolutions-of-current-context* resolutions nil)
	(resolutions-of-visible-library* resolutions nil)
	resolutions)))

(defun resolutions-of-current-theory* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-current-theory*
       (cdr resolutions)
       (if (tc-eq (module-instance (car resolutions))
		  (current-theory-name))
	   (cons (car resolutions) result)
	   result))))

(defun resolutions-of-current-context* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-current-context*
       (cdr resolutions)
       (let ((th (get-theory (module-instance (car resolutions)))))
	 (if (and th
		  (or (from-prelude? th)
		      (typep th 'library-theory)))
	     result
	     (cons (car resolutions) result))))))

(defun resolutions-of-visible-library* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-visible-library*
       (cdr resolutions)
       (let ((th (get-theory (module-instance (car resolutions)))))
	 (if (and th (from-prelude? th))
	     result
	     (cons (car resolutions) result))))))

(defun nearest-bound-variable (reses &optional res)
  (cond ((null reses)
	 res)
	((null res)
	 (nearest-bound-variable (cdr reses) (car reses)))
	((memq (declaration (car reses))
	       (memq (declaration res) *bound-variables*))
	 (nearest-bound-variable (cdr reses) res))
	(t (nearest-bound-variable (cdr reses) (car reses)))))


;;; Set-type (if-expr)

(defvar *ignore-else-part-tccs* nil)

(defmethod set-type* ((ex if-expr) expected)
  #+pvsdebug (assert (fully-typed? (operator ex)))
  (let* ((op (operator ex))
	 (optype (determine-operator-type op (argument ex) expected)))
    (set-type* op optype)
    (let ((reses (remove-if-not
		     #'(lambda (r)
			 (let ((stype (find-supertype (type r))))
			   (and stype
				(typep stype 'funtype)
				(compatible? (range stype) expected))))
		   (resolutions op))))
      (unless (singleton? reses)
	(if reses
	    (type-ambiguity ex)
	    (type-incompatible ex (ptypes ex) expected)))
      (if (eq (id (module-instance (car reses))) '|if_def|)
	  (let ((econd (condition ex))
		(ethen (then-part ex))
		(eelse (else-part ex)))
	    (set-type* econd *boolean*)
	    (let ((*tcc-conditions* (cons econd *tcc-conditions*)))
	      (set-type* ethen expected))
	    (let* ((*generate-tccs* (if *ignore-else-part-tccs*
					'none
					*generate-tccs*))
		   (*tcc-conditions*
		    (if (typep ex '(or first-cond-expr single-cond-expr
				       cond-expr last-cond-expr))
			*tcc-conditions*
			(cons (make!-negation econd) *tcc-conditions*))))
	      (set-type* eelse expected))
	    (let ((iftype (compatible-type (type ethen) (type eelse))))
	      (assert iftype)
	      ;;(set-type* op (mk-funtype (list *boolean* iftype iftype)
	      ;;			iftype))
	      (setf (type op) (mk-funtype (list *boolean* iftype iftype)
					  iftype))
	      (setf (resolutions op) reses)
	      (unless (eq (id *current-theory*) '|if_def|)
		(setf (actuals (module-instance op))
		      (list (mk-actual iftype))))
	      #+pvsdebug (assert (fully-instantiated? op))
	      (setf (type (argument ex))
		    (mk-tupletype (list *boolean* iftype iftype)))
	      (setf (type ex) iftype))
	    (if (typep ex 'chained-if-expr)
		(change-class ex 'chained-branch)
		(change-class ex 'mixfix-branch)))
	  (call-next-method)))))

(defvar *generating-cond-tcc* nil)

(defmethod set-type* ((expr first-cond-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (generate-cond-tccs expr))

(defmethod set-type* ((expr single-cond-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (generate-cond-tccs expr))

(defun generate-cond-tccs (expr)
  (unless *generating-cond-tcc*
    (let ((*generating-cond-tcc* t))
      (multiple-value-bind (conditions values else?)
	  (collect-cond-expr-conditions expr nil nil)
	(let ((dj-conds (if else? (butlast conditions) conditions)))
	  (when (cdr dj-conds)
	    (unless (trivial-disjointness dj-conds)
	      (generate-cond-disjoint-tcc expr dj-conds values))))
	(unless (trivial-cond-coverage conditions)
	  (generate-cond-coverage-tcc expr conditions))))))

(defun trivial-disjointness (dj-conds)
  (let ((right-hand-sides (collect-disjoint-values-from-equations dj-conds)))
    (when right-hand-sides
      #+pvsdebug (assert (length= right-hand-sides dj-conds))
      (let ((dup (duplicates? right-hand-sides :test #'=)))
	(if dup
	    (let ((edup (args2 (find dup dj-conds
				     :test #'(lambda (x y)
					       (= x (get-arithmetic-value
						     (args2 y))))))))
	      (type-error edup "Duplicated entry: ~a" edup))
	    t)))))

(defun collect-disjoint-values-from-equations (conditions &optional var rhss)
  (cond ((null conditions)
	 (nreverse rhss))
	((null var)
	 (when (and (equation? (car conditions))
		    (typep (args1 (car conditions)) 'name-expr)
		    (ground-arithmetic-term? (args2 (car conditions))))
	     (collect-disjoint-values-from-equations
	      (cdr conditions)
	      (args1 (car conditions))
	      (list (get-arithmetic-value (args2 (car conditions)))))))
	((and (equation? (car conditions))
	      (tc-eq (args1 (car conditions)) var)
	      (ground-arithmetic-term? (args2 (car conditions))))
	 (collect-disjoint-values-from-equations
	      (cdr conditions)
	      var
	      (cons (get-arithmetic-value (args2 (car conditions))) rhss)))))

(defmethod ground-arithmetic-term? (expr)
  (declare (ignore expr))
  nil)

(defmethod ground-arithmetic-term? ((expr number-expr))
  t)

(defmethod ground-arithmetic-term? ((expr tuple-expr))
  (every #'ground-arithmetic-term? (exprs expr)))

(defmethod ground-arithmetic-term? ((expr application))
  (and (arithmetic-op? (operator expr))
       (ground-arithmetic-term? (argument expr))))

(defmethod arithmetic-op? ((ex name-expr))
  (and (memq (id ex) '(+ - * /))
       (eq (id (module-instance (car (resolutions ex)))) '|reals|)))

(defmethod arithmetic-op? (ex)
  (declare (ignore ex))
  nil)

(defmethod get-arithmetic-value ((expr number-expr))
  (number expr))

(defmethod get-arithmetic-value ((expr application))
  (let ((a1 (get-arithmetic-value (args1 expr)))
	(a2 (when (args2 expr)
	      (get-arithmetic-value (args2 expr)))))
    (case (id (operator expr))
      (+ (+ a1 a2))
      (- (if a2 (- a1 a2) (- a1)))
      (* (* a1 a2))
      (/ (/ a1 a2)))))

(defun trivial-cond-coverage (conditions)
  (let ((last-cond (car (last conditions))))
    (and (typep last-cond 'else-condition)
	 (= (1+ (length (disjuncts (args1 last-cond))))
	    (length conditions)))))

(defmethod set-type* ((expr last-cond-expr) expected)
  (declare (ignore expected))
  (let ((*ignore-else-part-tccs* t))
    (call-next-method)))


(defmethod collect-cond-expr-conditions ((expr first-cond-expr) conditions
					 values)
  (if conditions
      (values conditions values nil)
      (collect-cond-expr-conditions (else-part expr)
				    (cons (condition expr) conditions)
				    (cons (then-part expr) values))))

(defmethod collect-cond-expr-conditions ((expr single-cond-expr) conditions
					 values)
  (values (cons (condition expr) conditions)
	  (cons (then-part expr) values)
	  nil))

(defmethod collect-cond-expr-conditions ((expr cond-expr) conditions
					 values)
  (collect-cond-expr-conditions (else-part expr)
				(cons (condition expr) conditions)
				(cons (then-part expr) values)))

(defmethod collect-cond-expr-conditions ((expr last-cond-expr) conditions
					 values)
  (values (nreverse (cons (condition expr) conditions))
	  (nreverse (cons (then-part expr) values))
	  (typep (condition expr) 'else-condition)))

(defmethod collect-cond-expr-conditions ((expr expr) conditions values)
  (values (nreverse conditions) (nreverse values) t))


;;; For lambda-exprs, we set the type of the body first to the range of
;;; the expected type.  We then check the types of the bind-decls against
;;; the expected types, they must be equal (or generate TCCs to that
;;; effect).  Throughout this process, we must allow for dependent types.
;;; Note that there is no need to check for function conversions here,
;;; they only apply if the lambda-expr is an operator of an application,
;;; in which case the expected here reflects the chosen conversion.

(defmethod set-type* ((ex lambda-expr) expected)
  (let ((sexpected (find-supertype expected)))
    (unless (typep sexpected 'funtype)
      (type-error ex "~a expected here" expected))
    (let* ((edomain (if (dep-binding? (domain sexpected))
			(type (domain sexpected))
			(domain sexpected)))
	   (erange (if (dep-binding? (domain sexpected))
		       (substit (range sexpected)
			 (acons (domain sexpected)
				(let ((vars (mapcar #'make-variable-expr
					      (bindings ex))))
				  (if (cdr vars)
				      (make!-tuple-expr vars)
				      (car vars)))
				nil))
		       (range sexpected)))
	   (adomain (get-lambda-expr-domain ex)))
      (unless (compatible? adomain edomain)
	(type-incompatible ex (list adomain) expected))
      (let ((*bound-variables* (append (bindings ex)
				       *bound-variables*)))
	(set-type* (expression ex) erange)
	(let ((atype (make-formals-funtype (list (bindings ex))
					   (type (expression ex)))))
	  (setf (type ex) atype)
	  (unless (tc-eq adomain edomain)
	    (or (find-funtype-conversion atype sexpected ex)
		(let ((epreds (equality-predicates adomain edomain)))
		  (when epreds
		    (generate-subtype-tcc ex expected (list epreds)))))))
	(let ((toppreds (compatible-preds sexpected expected ex)))
	  (when toppreds
	    (generate-subtype-tcc ex expected toppreds)))))))


(defun get-lambda-expr-domain (ex)
  (get-lambda-expr-domain* (bindings ex)))

(defun get-lambda-expr-domain* (bindings &optional types)
  (if (null bindings)
      (if (cdr types)
	  (mk-tupletype (nreverse types))
	  (car types))
      (let ((bind (car bindings)))
	(if (member bind (freevars (cdr bindings)) :key #'declaration)
	    (let ((db (mk-dep-binding (id bind) (type bind)
				      (declared-type bind))))
	      (get-lambda-expr-domain*
	       (substit (cdr bindings) (acons bind db nil))
	       (cons db types)))
	    (get-lambda-expr-domain* (cdr bindings)
				     (cons (type bind) types))))))

;;; set-lambda-dep-types is called to handle lambda-expressions.  The
;;; input is a list of bindings and expected-types, where the
;;; expected-types may include dependencies.  There is no output, just the
;;; side-effect of checking for TCCs.  The expected-types gets substituted
;;; for in the recursive call if there is a dependent type.

(defun set-lambda-dep-types (bindings expected-types)
  (when bindings
    (let* ((dep? (typep (car expected-types) 'dep-binding))
	   (etype (if dep?
		      (type (car expected-types))
		      (car expected-types)))
	   (*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
      (set-type* (car bindings) etype)
      (set-lambda-dep-types
       (cdr bindings)
       (if dep?
	   (substit (cdr expected-types)
	     (acons (car expected-types)
		    (make-variable-expr (car bindings))
		    nil))
	   (cdr expected-types))))))


(defun add-bindings-to-tcc-conditions (bindings conditions)
  (if (null bindings)
      conditions
      (add-bindings-to-tcc-conditions
       (cdr bindings)
       (if (member (car bindings) conditions
		   :test #'(lambda (x y)
			     (and (consp y) (eq x (car y)))))
	   conditions
	   (cons (car bindings) conditions)))))


(defun lambda-expr-expected-domain (expr expected)
  (let* ((dexp (domain expected))
	 (dtypes (if (singleton? (bindings expr))
		     (list dexp)
		     (if (and (null (cdr (domain-types expected)))
			      (typep (find-supertype
				      (car (domain-types expected)))
				     'tupletype)
			      (or (not (null (cdr (bindings expr))))
				  (not (typep (find-supertype
					       (type (car (bindings expr))))
					      'tupletype))))
			 (types (domain-types (find-supertype (car dexp))))
			 (domain-types expected)))))
    (unless (or (singleton? (bindings expr))
		(length= dtypes (bindings expr)))
      (type-error expr "Wrong number of bound variables - ~d expected"
		  (length dtypes)))
    dtypes))

(defmethod set-type* ((ex bind-decl) expected)
  (declare (ignore expected))
  (when (declared-type ex)
    (set-type* (declared-type ex) nil)))

(defmethod set-type* ((ex quant-expr) expected)
  ;;(set-formals-types (bindings ex))
  (let ((*bound-variables* (append (bindings ex) *bound-variables*)))
    (set-quant-dep-types (append (bindings ex) (list (expression ex)))
			 (nconc (mapcar #'type (bindings ex))
				(list expected))))
  (setf (type ex) *boolean*))


;;; set-quant-dep-types is called to handle quant-exprs.  The input is a
;;; list of bindings and expected-types, where the expected-types may
;;; include dependencies.  There is no output, just the side-effect of
;;; checking for TCCs.  The expected-types gets substituted for in the
;;; recursive call if there is a dependent type.  The last element of the
;;; bindings list is actually an expression, and its type is set relative
;;; to the last expected-type, that has had all of its prior
;;; substitutions.  Otherwise, this is the same as set-lambda-dep-types.

(defun set-quant-dep-types (bindings expected-types)
  (if (cdr bindings)
      (let* ((dep? (typep (car expected-types) 'dep-binding))
	     (etype (if dep?
			(type (car expected-types))
			(car expected-types))))
	(set-type* (car bindings) etype)
	(let ((*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
	  (set-quant-dep-types
	   (cdr bindings)
	   (if dep?
	       (substit (cdr expected-types)
		 (acons (car expected-types)
			(make-variable-expr (car bindings))
			nil))
	       (cdr expected-types)))))
      (set-type* (car bindings) (car expected-types))))



(defmethod set-type* ((expr record-expr) expected)
  (let ((sexpected (find-supertype expected))
	(ass (assignments expr)))
    (unless (typep sexpected 'recordtype)
      (type-error expr "~a expected here" expected))
    (let ((fields (fields sexpected)))
      (unless (length= ass fields)
	(type-error expr "Wrong number of assignments - ~d expected"
		    (length fields)))
      (let ((badass (find-if-not #'(lambda (a)
				     (member (id (caar (arguments a))) fields
					     :test #'(lambda (x y)
						       (eq x (id y)))))
		      ass)))
	(when badass
	  (type-error badass "Field not found")))
      (set-rec-assignment-types ass nil fields sexpected nil))
    (setf (type expr)
	  (mk-recordtype
	   (mapcar #'(lambda (a)
		       (let ((fld (caar (arguments a)))
			     (ty (type (expression a))))
			 (mk-field-decl (id fld) ty ty)))
		   ass)
	   nil))))

(defmethod set-type* ((expr update-expr) (expected recordtype))
  (with-slots (expression assignments) expr
    (with-slots (fields) expected
      (let ((etypes (collect-compatible-recordtypes (types expression)
						    expected)))
	(check-unique-type etypes expr expected)
	(set-type* expression (contract-expected expr expected))
	(let* ((stype (find-supertype (car etypes)))
	       (atype (if (fully-instantiated? stype)
			  stype
			  (instantiate-from stype expected expr))))
	  (set-rec-assignment-types (complete-assignments expr expected)
				    expression fields expected nil)
	  (setf (type expr) atype))))))

(defmethod contract-expected (expr (expected recordtype))
  (let* ((new-ids (mapcar #'(lambda (a) (id (caar (arguments a))))
		    (remove-if-not #'maplet? (assignments expr))))
	 (types (collect-compatible-recordtypes
		 (types (expression expr)) expected new-ids)))
    (check-unique-type types (expression expr) expected)
    (let ((fields (fields (find-supertype (car types)))))
      ;; Remove those fields added by maplets, and replace fields changed
      ;; by maplets.
      (lcopy expected
	'fields (mapcar #'(lambda (fld)
			    (if (memq (id fld) new-ids)
				(car (member (id fld) fields :key #'id))
				fld))
		  (remove-if-not #'(lambda (fld)
				     (member fld fields :test #'same-id))
		    (fields expected)))))))

(defun check-unique-type (types expr expected)
  (cond ((cdr types)
	 (setf (types expr) types)
	 (type-ambiguity expr))
	((null types)
	 (type-incompatible expr (types expr) expected))))

(defun collect-compatible-recordtypes (types expected &optional ignore-ids)
  (delete-if-not
      #'(lambda (ty)
	  (let ((sty (find-supertype ty)))
	    (and (typep sty 'recordtype)
		 (or ignore-ids
		     (length= (fields sty) (fields expected)))
		 (every #'(lambda (fld)
			    (or (memq (id fld) ignore-ids)
				(member fld (fields expected)
					:test #'(lambda (x y)
						  (and (eq (id x) (id y))
						       (compatible?
							(type x) (type y)))))))
			(fields sty)))))
    types))

(defmethod set-type* ((expr update-expr) (expected tupletype))
  (with-slots (expression assignments) expr
    (with-slots (types) expected
      (let ((utypes (collect-compatible-tupletypes (types expr) expected)))
	(check-unique-type utypes expr expected)
	(set-type* expression (contract-expected expr expected))
	(let* ((stype (find-supertype (type expression)))
	       (atype (if (fully-instantiated? stype)
			  stype
			  (instantiate-from stype expected expr))))
	  (set-tup-assignment-types (complete-assignments expr expected)
				    expression types expected)
	  (setf (type expr) atype))))))

(defmethod contract-expected (expr (expected tupletype))
  (let* ((new-indices (mapcar #'(lambda (a) (number (caar (arguments a))))
			(remove-if-not #'maplet? (assignments expr))))
	 (types (collect-compatible-tupletypes
		 (types (expression expr)) expected new-indices)))
    (check-unique-type types (expression expr) expected)
    (let ((types (types (find-supertype (car types)))))
      ;; Remove those types added by maplets, and replace types changed
      ;; by maplets.
      (lcopy expected
	'types (let ((index 0))
		 (mapcar #'(lambda (ty ety)
			     (if (member (incf index) new-indices :test #'=)
				 ty
				 ety))
		   types (types expected)))))))

(defun collect-compatible-tupletypes (types expected &optional ignore-indices)
  (remove-if-not
      #'(lambda (ty)
	  (let ((sty (find-supertype ty)))
	    (and (typep sty 'tupletype)
		 (or ignore-indices
		     (= (length (types sty)) (length (types expected))))
		 (let ((index 0))
		   (every #'(lambda (ety ty)
			      (or (member (incf index) ignore-indices
					  :test #'=)
				  (compatible? ety ty)))
			  (types expected) (types sty))))))
    types))


;;; set-rec-assignment-types is the record equivalent of
;;; set-tup-assignment-types for tuples, except that it works for both
;;; record-exprs and updates.  For update-exprs, the expr arg is the
;;; expression being updated.  For record-exprs, it is NIL.  This causes
;;; no problem, since record-exprs may not have multiple arguments
;;; (e.g., (# (a)(1) := 3 #) ).

(defun set-rec-assignment-types (assns ex fields rectype nfields)
  (when assns
    (let ((ass (find-if #'(lambda (a) (eq (id (caar (arguments a)))
					  (id (car fields))))
		 assns)))
      (unless (or ass
		  (not (dependent? rectype)))
	(type-error (car fields) "Field not found"))
      (when ass
	(change-class (caar (arguments ass)) 'field-name-expr)
	(set-assignment-types ass rectype ex))	
      (let* ((dep? (and (dependent? rectype)
			(some #'(lambda (fld)
				  (member (car fields) (freevars fld)
					  :key #'declaration))
			      fields)))
	     (aexpr (when dep?
		      (make-assignment-subst-expr
		       ass (type (car fields)) ex)))
	     (subst-fields (if dep?
			       (subst-rec-dep-type
				aexpr (car fields) (cdr fields))
			       (cdr fields)))
	     (rem-assns (remove ass assns))
	     (done-with-field?
	      (not (member (car fields) rem-assns
			   :test #'same-id
			   :key #'(lambda (a) (caar (arguments a))))))
	     (next-fields (if done-with-field?
			      (cons (car fields) nfields)
			      nfields)))
	(set-rec-assignment-types
	 rem-assns
	 ex
	 (if done-with-field? subst-fields (cons (car fields) subst-fields))
	 (if dep?
	     (make-instance 'recordtype
	       'fields (sort-fields (append (reverse next-fields)
					    subst-fields)
				    t)
	       'dependent? t)
	     rectype)
	 next-fields)))))

;;; Set the assignment types of a tupletype update-expr.  The only
;;; difficulty here is with dependent types.  The inputs are the
;;; assignments assns, the expected types, the expected tuptype, the newly
;;; newly created types ntypes, and the number we're working on.  We
;;; recurse on the types, find the associated assignment (if any), and set
;;; its type.  In recursing, we substitute the current expression in the
;;; remaining types, and create a new tuptype from it.

(defun set-tup-assignment-types (assns expr types tuptype)
  (when assns
    (let* ((ass (car assns))
	   (type (nth (1- (number (caar (arguments ass)))) types)))
      (set-assignment-types ass tuptype expr)
      (let* ((dep? (typep type 'dep-binding))
	     (aexpr (when dep?
		      (make-assignment-subst-expr ass (type type) expr)))
	     (subst-types (if dep?
			     (subst-tup-dep-type aexpr type types)
			     types)))
	(set-tup-assignment-types
	 (cdr assns)
	 expr
	 subst-types
	 (if dep?
	     (make-instance 'tupletype
	       'types subst-types)
	     tuptype))))))


;;; make-assignment-subst-expr takes an assignment of the form, e.g.,
;;;  (1)(x,y)(3) := true, and a type of the form [[t1, t2] -> [int -> bool]]
;;; and the expr associated with the update (e.g., the T in T WITH ...)
;;; and creates the expression
;;;  (LAMBDA (x1:[t1, t2]):
;;;     IF x1 = (x,y)
;;;        THEN (LAMBDA (x2:int):
;;;                 IF x2 = 3 THEN TRUE ELSE PROJ_1(T)(x1)(x2) ENDIF)
;;;        ELSE PROJ_1(T)(x1) ENDIF)

(defun make-assignment-subst-expr (ass type expr)
  (let ((ass-args (arguments ass))
	(ass-expr (expression ass)))
    (if (cdr ass-args)
	(let ((app (make-assignment-appl-expr (caar ass-args) expr)))
	  (make-assignment-subst-expr* (cdr ass-args) ass-expr type app))
	ass-expr)))

(defmethod make-assignment-appl-expr ((arg number-expr) expr)
  (make!-projection-application (number arg) expr))

(defmethod make-assignment-appl-expr ((arg name-expr) expr)
  (make!-field-application arg expr))

(defmethod make-assignment-else-expr ((arg number-expr) expr)
  (make!-projection-application (number arg) expr))

(defmethod make-assignment-else-expr ((arg name-expr) expr)
  (make!-field-application arg expr))

(defun make-assignment-subst-expr* (args expr type proj)
  (multiple-value-bind (bindings vars)
      (make-assignment-bindings args type nil nil)
    (make-assignment-lambda-expr bindings vars args expr proj type)))

(defun make-assignment-bindings (args type bindings vars)
  (if (null args)
      (values (nreverse bindings) (nreverse vars))
      (let* ((stype (find-supertype type))
	     (dom (domain stype))
	     (dep? (typep dom 'dep-binding))
	     (dtype (if dep? (type dom) dom))
	     (ran (range stype))
	     (id (make-new-variable '|x| (cons type bindings)))
	     (bd (typecheck* (mk-bind-decl id dtype) nil nil nil))
	     (var (mk-name-expr id nil nil (make-resolution bd nil dtype))))
	(make-assignment-bindings (cdr args)
				  (if dep?
				      (substit ran (acons dom var nil))
				      ran)
				  (cons bd bindings)
				  (cons var vars)))))

(defun make-assignment-equality (var arg)
  (let ((narg (if (listp arg)
		  (if (cdr arg)
		      (make!-tuple-expr arg)
		      (car arg))
		  arg)))
    (make!-equation var narg)))


(defun make-assignment-lambda-expr (bindings vars args expr proj type)
  (typecheck* (make-assignment-lambda-expr* bindings vars args expr proj)
	      type nil nil))

(defun make-assignment-lambda-expr* (bindings vars args expr proj)
  (if (null bindings)
      expr
      (let ((nproj (mk-application proj (car vars))))
	(mk-lambda-expr (list (car bindings))
	  (add-parens
	   (mk-if-expr (make-assignment-equality (car vars) (car args))
		       (make-assignment-lambda-expr*
			(cdr bindings) (cdr vars) (cdr args) expr
			nproj)
		       nproj))))))

(defun add-parens (expr)
  (setf (parens expr) 1)
  expr)

(defun make-assignment-projection (proj vars)
  (if (null vars)
      proj
      (make-assignment-projection (mk-application proj (car vars))
				  (cdr vars))))

(defun subst-tup-dep-type (expr type types)
  (substit types (acons type expr nil)))


;;; Update-exprs on funtypes

(defmethod set-type* ((expr update-expr) (expected funtype))
  (with-slots (expression assignments) expr
    (let* ((cetypes (remove-if-not #'(lambda (ty) (compatible? ty expected))
		      (ptypes expression)))
	   (etypes (or (remove-if-not #'(lambda (ty)
					  (strict-compatible? ty expected))
			 cetypes)
		       cetypes))
	   (fetypes (mapcar #'(lambda (ety)
				(if (fully-instantiated? ety)
				    ety
				    (instantiate-from ety expected expression)))
		      etypes))
	   (cutypes (remove-if-not #'(lambda (ty) (compatible? ty expected))
		      (ptypes expr)))
	   (utypes (or (remove-if-not #'(lambda (ty)
					  (strict-compatible? ty expected))
			 cutypes)
		       cutypes)))
      (cond ((null fetypes)
	     (type-incompatible expression (ptypes expression) expected))
	    ((null utypes)
	     (type-incompatible expr (ptypes expr) expected))
	    ((cdr fetypes)
	     (setf (types expression) fetypes)
	     (type-ambiguity expression))
	    ((cdr utypes)
	     (setf (types expr) utypes)
	     (type-ambiguity expr))
	    (t (set-type* expression (car fetypes))
	       (set-assignment-types-for-funtype
		assignments expected expression (car utypes))
	       (setf (type expr)
		     (if (fully-instantiated? (car utypes))
			 (car utypes)
			 (instantiate-from (car utypes) (type expression)
					   expr))))))))

(defun set-assignment-types-for-funtype (assignments expected expression utype)
  (set-assignment-types (car assignments) expected expression)
  (when (cdr assignments)
    (set-assignment-types-for-funtype
     (cdr assignments)
     expected
     (if (typep expression 'update-expr)
	 (copy expression
	   'assignments (append (assignments expression)
				(list (car assignments))))
	 (make-instance 'update-expr
	   'expression expression
	   'assignments (list (car assignments))
	   'type utype))
     utype)))

(defmethod set-type* ((expr update-expr) (expected subtype))
  (let ((stype (find-supertype expected)))
    (set-type* expr stype)))

(defmethod set-type* ((expr update-expr) (expected dep-binding))
  (set-type* expr (type expected)))

(defmethod set-type* ((expr update-expr) expected)
  (type-error expr "~a is not a record, function, or array type"
	      expected))

(defmethod complete-assignments (expr (rtype recordtype))
  (if (or (dependent? rtype)
	  (dependent? (find-supertype (type (expression expr)))))
      (let ((*field-records* (mapcar #'(lambda (fld) (cons fld rtype))
				     (fields rtype))))
	(complete-rec-assignments (assignments expr) (fields rtype) expr nil))
      (assignments expr)))

(defun complete-rec-assignments (assignments fields expr cassigns)
  (if (null fields)
      (nreverse cassigns)
      (let ((assn (find (car fields) assignments
			:test #'(lambda (x y)
				  (same-id x (caar (arguments y)))))))
	(complete-rec-assignments
	 assignments
	 (cdr fields)
	 expr
	 (cons (or assn
		   (make-rec-assignment (car fields) (expression expr)))
	       cassigns)))))

(defun make-rec-assignment (field expr)
  (let* ((ftype (make-field-type field))
	 (apptype (field-application-type field (type expr) expr))
	 (res (make-resolution field
		(theory-name *current-context*) ftype))
	 (name (mk-name-expr (id field) nil nil res))
	 (appl (make-instance 'field-application
		 'id (id field)
		 'argument expr
		 'type apptype)))
    (mk-assignment 'uni (list (list name)) appl)))

(defmethod complete-assignments (expr (rtype tupletype))
  (let ((tuptype (find-supertype (type (expression expr)))))
    (if (or (dependent? rtype)
	    (dependent? tuptype))
	(complete-tup-assignments (assignments expr) (types tuptype)
				  (expression expr) 1 nil)
	(assignments expr))))

(defun complete-tup-assignments (assignments types expr num cassigns)
  (if (null types)
      (nreverse cassigns)
      (let ((ass (or (find-if #'(lambda (a)
				  (= (number (caar (arguments a))) num))
		       assignments)
		     (make-tup-assignment expr num))))
	(complete-tup-assignments assignments (cdr types) expr (1+ num)
				  (cons ass cassigns)))))

(defun make-tup-assignment (expr num)
  (let ((type (nth (1- num) (types (find-supertype (type expr))))))
    (make-instance 'uni-assignment
      'arguments (list (list (make-number-expr num)))
      'expression (make-instance 'projection-application
		    'id (makesym "PROJ_~d" num)
		    'index num
		    'argument expr
		    'type type))))

(defmethod dependent? ((type tupletype))
  (some #'(lambda (ty) (typep ty 'dep-binding)) (types type)))

(defmethod dependent? ((type funtype))
  (typep (domain type) 'dep-binding))

(defun subst-rec-dep-type (expr fld fields &optional bindings result)
  (if (null fields)
      (nreverse result)
      (let ((nfld (substit (car fields) (acons fld expr bindings))))
	(subst-rec-dep-type expr fld (cdr fields)
			 (if (eq nfld (car fields))
			     bindings
			     (cons (cons (car fields) nfld) bindings))
			 (cons nfld result)))))


;;; Called for update expressions and record-exprs.  Walks down the
;;; arguments and the expected types, until there are no more arguments.
;;; The expr type is then set relative to the remaining expected type.

(defun set-assignment-types (assignment expected expr)
  (let ((args (arguments assignment)))
    (set-assignment-types*
     args
     (expression assignment)
     expected
     (maplet? assignment)
     (when (some-dependent-arg-domain-type args expected) expr))))

(defmethod some-dependent-arg-domain-type ((args null) expected)
  (declare (ignore expected))
  nil)

(defmethod some-dependent-arg-domain-type (args (expected funtype))
  (or (typep (domain expected) 'dep-binding)
      (some-dependent-arg-domain-type
       (cdr args)
       (range expected))))

(defmethod some-dependent-arg-domain-type (args (expected recordtype))
  (or (dependent? expected)
      (some-dependent-arg-domain-type
       (cdr args)
       (let ((field (find-if #'(lambda (fld) (eq (id (caar args)) (id fld)))
		      (fields expected))))
	 (type field)))))

(defmethod some-dependent-arg-domain-type (args (expected tupletype))
  (or (some #'(lambda (ty) (typep ty 'dep-binding)) (types expected))
      (some-dependent-arg-domain-type
       (cdr args)
       (nth (1- (number (caar args))) (types expected)))))

(defmethod some-dependent-arg-domain-type (args (expected subtype))
  (some-dependent-arg-domain-type args (supertype expected)))

(defmethod some-dependent-arg-domain-type (args (expected dep-binding))
  (some-dependent-arg-domain-type args (type expected)))

(defmethod set-assignment-types* ((args null) expr expected maplet? oexpr)
  (declare (ignore maplet? oexpr))
  (set-type* expr expected))

(defmethod set-assignment-types* (args expr (expected funtype) maplet? oexpr)
  (with-slots (domain range) expected
    (let* ((dtypes (domain-types expected))
	   (exprs (cond ((length= dtypes (car args))
			 (car args))
			((cdr dtypes)
			 (get-arguments-list (caar args)))
			(t (exprs (caar args))))))
      (unless (length= dtypes exprs)
	(type-incompatible (caar args) (ptypes (caar args)) (domain expected)))
      (set-tup-types exprs dtypes)
      (unless (eq exprs (car args))
	(setf (type (caar args)) (mk-tupletype (mapcar #'type exprs))))
      (let* ((nrange (if (typep domain 'dep-binding)
			 (let ((arg (if (and (eq exprs (car args))
					     (cdr (car args)))
					(make-tuple-expr (car args))
					(caar args))))
			   (substit range
			     (acons domain arg nil)))
			 range))
	     (srange (find-supertype nrange)))
	(if (and oexpr
		 (typep srange '(or recordtype tupletype))
		 (dependent? srange))
	    (let* ((nexpr (apply #'make!-application oexpr (car args)))
		   (nupdate (make!-update-expr
			     nexpr
			     (list (make-instance 'assignment
				     'arguments (cdr args)
				     'expression expr))))
		   (nass (complete-assignments nupdate
					       (find-supertype nrange))))
	      (if (typep srange 'recordtype)
		  (set-rec-assignment-types nass nexpr (fields srange)
					    (type nexpr) nil)
		  (set-tup-assignment-types nass nexpr (types srange)
					    (type nexpr))))
	    (set-assignment-types*
	     (cdr args)
	     expr
	     nrange
	     maplet?
	     (when oexpr
	       (apply #'make!-application oexpr (car args)))))))))

(defmethod set-assignment-types* (args expr (expected recordtype)
				       maplet? oexpr)
  (let ((field (find-if #'(lambda (fld) (eq (id (caar args)) (id fld)))
		 (fields expected))))
    (unless field
      (type-error (car args) "Field not found"))
    (change-class (caar args) 'field-assignment-arg)
    (let* ((dep? (and oexpr
		      (freevars field)
		      (some #'(lambda (fd)
				(member fd (freevars field)
					:key #'declaration))
			    (fields expected))))
	   (ftype (if dep?
		      (field-application-type field expected oexpr)
		      (type field))))
      (set-assignment-types*
       (cdr args) expr ftype maplet?
       (when dep?
	 (make!-field-application field oexpr))))))

(defmethod set-assignment-types* (args expr (expected tupletype) maplet? oexpr)
  (setf (type (caar args)) *naturalnumber*)
  (set-assignment-types*
   (cdr args)
   expr
   (nth (1- (number (caar args))) (types expected))
   maplet?
   (when oexpr
     (make!-projection-application (number (caar args)) oexpr))))

(defmethod set-assignment-types* (args expr (expected dep-binding)
				       maplet? oexpr)
  (set-assignment-types* args expr (type expected) maplet? oexpr))

(defmethod set-assignment-types* (args expr expected maplet? oexpr)
  (declare (ignore maplet? oexpr))
  (unless (null args)
    (type-error expr "~a is not a record, function, or array type"
		expected))
  (set-type* expr expected))


;;; set-type for types

(defmethod set-type* :around ((te type-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (when (print-type te)
    (set-type* (print-type te) nil)))

(defmethod set-type* ((te type-name) expected)
  (declare (ignore expected))
  #+pvsdebug (assert (resolution te))
  (when (and (actuals te)
	     (not (actuals (module-instance te))))
    (if (same-id (module-instance te) *current-theory*)
	(type-error te
	  "May not provide actuals for entities defined locally")
	(type-error te
	  "May not specify actuals for this type name")))
;  (unless (or (same-id (module-instance te) *current-theory*)
;	      (from-prelude? (get-theory (module-instance te))))
;    (pushnew (module-instance te)
;	     (instances-used *current-theory*)
;	     :test #'tc-eq))
  (when (actuals (module-instance te))
    (setf (module-instance (resolution te))
	  (set-type-actuals te)))
  #+pvsdebug (fully-typed? te)
  (when (and (typep (type (resolution te)) 'type-name)
	     (adt (type (resolution te))))
    (setf (adt te) (adt (type (resolution te)))))
  (unless (or *dont-worry-about-full-instantiations*
	      (fully-instantiated? te))
    (type-error te
      "Could not determine the full theory instance for ~a~
       ~%  Theory instance: ~a"
      te (full-name (module-instance te)))))

(defmethod set-type* ((te type-application) expected)
  (declare (ignore expected))
  (set-type* (type te) nil)
  (let ((typeslist (make-formals-type-app
		      (subst-mod-params (formals (declaration (type te)))
					(module-instance
					 (resolution (type te)))))))
    (mapc #'set-type (parameters te) (car typeslist)))) 

(defmethod set-type* ((te subtype) expected)
  (when (print-type te)
    (set-type* (print-type te) expected))
  (when (supertype te)
    (set-type* (supertype te) expected))
  (when (predicate te)
    (unless (typed? (predicate te))
      (set-type* (predicate te)
		 (or (type (predicate te))
		     (mk-funtype (list (typecheck* (supertype te) nil 'type nil))
				 *boolean*))))))

(defmethod set-type* ((te expr-as-type) expected)
  (declare (ignore expected))
  (assert (type (expr te)))
  (call-next-method))

(defmethod set-type* ((te funtype) expected)
  (with-slots (domain range) te
    (set-type* domain expected)
    (set-type* range expected)))

(defmethod set-type* ((te dep-binding) expected)
  (set-type* (type te) expected))

(defmethod set-type* ((te tupletype) expected)
  (mapc #'(lambda (ty) (set-type* ty expected)) (types te)))

(defmethod set-type* ((te recordtype) expected)
  (declare (ignore expected))
  (set-type-fields (fields te)))

(defun set-type-fields (field-decls)
  (when field-decls
    (set-type* (declared-type (car field-decls)) nil)
    (let ((*bound-variables* (cons (car field-decls) *bound-variables*)))
      (set-type-fields (cdr field-decls)))))


;;; instantiate-from

(defvar *instantiate-from-expr* nil)
(defvar *instantiate-from-modinsts* nil)

(defun instantiate-from (type expected expr)
  #+pvsdebug (assert (fully-instantiated? expected))
  (let* ((bindings (tc-match expected type
			     (instantiate-operator-bindings
			      (free-params type)))))
    (if (every #'cdr bindings)
	(instantiate-operator-from-bindings type bindings)
	(if *dont-worry-about-full-instantiations*
	    type
	    (type-error expr
	      "Could not determine the full theory instance for ~a~
               ~:[~;~%  May be a problem with splitting dependent types~]~
               ~@[~%  Theory instance: ~a~]"
	      expr
	      (and (typep expr 'application)
		   (some #'(lambda (a)
			     (some #'(lambda (aty)
				       (and (typep (find-supertype aty)
						   '(or funtype tupletype))
					    (dependent? (find-supertype aty))))
				   (ptypes a)))
			 (arguments expr)))
	      (when (typep expr 'name-expr)
		(full-name (module-instance expr))))))))

(defmethod instantiate-from* :around (te1 te2)
  (declare (ignore te2))
  (if (fully-instantiated? te1)
      te1
      (let ((nte (call-next-method)))
	#+pvsdebug (assert (fully-instantiated? nte))
	nte)))

(defmethod instantiate-from* ((l1 list) (l2 list))
  (mapcar #'instantiate-from* l1 l2))

(defmethod instantiate-from* (te1 (te2 dep-binding))
  (instantiate-from* te1 (type te2)))

(defmethod instantiate-from* ((te1 type-name) (te2 type-name))
  (let ((res (instantiate-from* (resolution te1) (resolution te2))))
    (cond ((eq res (resolution te1))
	   te1)
	  (t (pushnew (module-instance res) *instantiate-from-modinsts*
		      :test #'tc-eq)
	     (let ((acts (mapcar #'(lambda (a) (or (type-value a) (expr a)))
				 (actuals (module-instance te1)))))
	       (when (and (every #'(lambda (a) (and (name? a)
						    (typep (declaration a)
							   'formal-decl)))
				 acts)
			  (every #'(lambda (a)
				     (eq (module (declaration a))
					 (module (declaration (car acts)))))
				 (cdr acts)))
		 (break "Do something here")))
	     (lcopy te1
	       'actuals (when (actuals te1) (actuals (module-instance res)))
	       'resolutions (list res))))))

(defmethod instantiate-from* ((res1 resolution) (res2 resolution))
  res2)

(defmethod instantiate-from* ((te1 type-expr) (te2 type-expr))
  (let ((modinsts (collect-module-instances te2)))
    (instantiate-from-modinsts te1 modinsts)))

(defmethod instantiate-from* ((te1 subtype) (te2 subtype))
  (lcopy te1
    'supertype (instantiate-from* (supertype te1) (supertype te2))
    'predicate (instantiate-from* (predicate te1) (type (predicate te2)))))

(defmethod instantiate-from* ((te1 funtype) (te2 funtype))
  (if (compatible? te1 te2)
      (let ((dom (mapcar #'instantiate-from* (domain te1) (domain te2))))
	(lcopy te1
	  'domain (if (equal dom (domain te1)) (domain te1) dom)
	  'range (instantiate-from* (range te1) (range te2))))
      (call-next-method)))

(defmethod instantiate-from* ((te1 tupletype) (te2 tupletype))
  (lcopy te1
    'types (instantiate-from* (types te1) (types te2))))

(defmethod instantiate-from* ((te1 recordtype) (te2 recordtype))
  (let ((nfields (instantiate-from-fields (fields te1) (fields te2))))
    (if (eq nfields (fields te1))
	te1
	(let ((nte (copy te1 'fields nfields)))
	  ;;(update-fields nte (dependent? te1))
	  nte))))

(defun instantiate-from-fields (fields1 fields2 &optional result)
  (if (null fields1)
      (let ((fields (nreverse result)))
	(if (equal fields1 fields)
	    fields1
	    fields))
      (let* ((fld1 (car fields1))
	     (fld2 (find-if #'(lambda (fld) (same-id fld fld1)) fields2))
	     (ifld (instantiate-from* fld1 fld2)))
	(instantiate-from-fields (cdr fields1)
				 (remove fld2 fields2)
				 (cons ifld result)))))
	 

(defmethod instantiate-from* ((fd1 field-decl) (fd2 field-decl))
  (lcopy fd1 'type (instantiate-from* (type fd1) (type fd2))))


;;; Expressions

(defmethod instantiate-from* ((ex expr) type)
  (let ((modinsts (collect-module-instances type)))
    (instantiate-from-modinsts ex modinsts)))

(defun instantiate-from-modinsts (ex modinsts)
  (if (fully-instantiated? ex)
      ex
      (gensubst ex
	#'(lambda (x) (instantiate-from-modinsts! x modinsts))
	#'instantiate-from-modinsts?)))

(defmethod instantiate-from-modinsts? :around (ex)
  (unless (fully-instantiated? ex)
    (call-next-method)))

(defmethod instantiate-from-modinsts? ((ex name))
  (and (not (actuals (module-instance ex)))
       (formals (module (declaration ex)))))

(defmethod instantiate-from-modinsts? ((ex modname))
  nil)

(defmethod instantiate-from-modinsts? (ex)
  (declare (ignore ex))
  nil)

(defmethod instantiate-from-modinsts! :around ((ex name-expr) modinsts)
  (declare (ignore modinsts))
  (let ((ne (call-next-method)))
    (if (eq ne ex)
	ex
	(progn (setf (type ne) (type (resolution ne)))
	       ne))))

(defmethod instantiate-from-modinsts! ((ex name) modinsts)
  (let* ((mi (or (module-instance ex) (module (declaration ex))))
	 (matches (remove-if-not #'(lambda (m) (same-id m mi))
		    modinsts)))
    (cond ((singleton? matches)
	   (let ((nres (make-resolution (declaration ex) (car matches))))
	     (lcopy ex
	       'actuals (when (actuals ex) (actuals (car matches)))
	       'resolutions (list nres))))
	  ((cdr matches)
	   (type-error *instantiate-from-expr*
	     "Ambiguous theory instance, could be one of:~{~%  ~a~}"
	     matches))
	  (*dont-worry-about-full-instantiations* ex)
	  (t (type-error *instantiate-from-expr*
	       "Could not determine the full theory instance for ~a"
	       *instantiate-from-expr*)))))

(defun collect-module-instances (type-expr)
  (let ((modinsts nil))
    (mapobject #'(lambda (te)
		   (when (typep te 'name)
		     (pushnew (module-instance te) modinsts :test #'tc-eq)))
	       type-expr)
    modinsts))

(defmethod resolution-matching ((te type-name) modinst)
  (when (same-id (module-instance (resolution te)) modinst)
    (resolution te)))

(defmethod resolution-matching ((te dep-binding) modinst)
  (resolution-matching (type te) modinst))

(defmethod resolution-matching ((te subtype) modinst)
  (resolution-matching (supertype te) modinst))

(defmethod resolution-matching ((te funtype) modinst)
  (or (resolution-matching (domain te) modinst)
      (resolution-matching (range te) modinst)))

(defmethod resolution-matching ((te tupletype) modinst)
  (resolution-matching (types te) modinst))

(defmethod resolution-matching ((te recordtype) modinst)
  (resolution-matching (fields te) modinst))

(defmethod resolution-matching ((fd field-decl) modinst)
  (resolution-matching (type fd) modinst))

(defmethod resolution-matching ((list list) modinst)
  (when list
    (or (resolution-matching (car list) modinst)
	(resolution-matching (cdr list) modinst))))

;(defun modinsts-with-free-formals (obj)
;  (let ((frees nil))
;    (mapobject #'(lambda (ex)
;		   (when (and (typep ex '(or name-expr type-name))
;			      (resolution ex)
;			      (some #'(lambda (act)
;					(let ((tex (or (type-value act)
;						       (expr act))))
;					  (and (typep tex 'name)
;					       (typep (declaration
;						       (type-value act))
;						      'formal-decl))))
;				    (actuals (module-instance ex))))
;		     (pushnew ex frees :test #'tc-eq)))
;	       obj)
;    frees))

(defun free-formals (obj)
  (let ((frees nil))
    (mapobject #'(lambda (ex)
		   (when (and (typep ex '(or name-expr type-name))
			      (resolution ex))
		     (mapc #'(lambda (act)
			       (when (free-actual act)
				 (pushnew (act-value act) frees
					  :test #'tc-eq)))
			   (actuals (module-instance ex)))))
	       obj)
    frees))

(defun act-value (actual)
  (or (type-value actual)
      (expr actual)))

(defun free-actual (act)
  (let ((texp (or (type-value act)
		  (expr act))))
    (and (typep texp 'name)
	 (typep (declaration texp) 'formal-decl)
	 (not (eq (module (declaration texp)) *current-theory*)))))

;(defun find-modinsts-matching (obj modids)
;  (let ((modinsts nil))
;    (mapobject #'(lambda (ex)
;		   (when (and (typep ex '(or name-expr type-name))
;			      (memq (id (module-instance ex)) modids))
;		     (pushnew (module-instance ex) modinsts
;			      :test #'tc-eq)))
;	       obj)
;    modinsts))

(defun subst-for-formals (obj alist)
  (gensubst obj
    #'(lambda (ex) (subst-for-formals! ex alist))
    #'(lambda (ex) (subst-for-formals? ex alist))))

(defmethod subst-for-formals? ((ex name) alist)
  (assq (declaration ex) alist))

(defmethod subst-for-formals? ((ex modname) alist)
  (let ((mod (get-theory ex)))
    (and (null (actuals ex))
	 (every #'(lambda (fm) (assq fm alist))
		(formals-sans-usings mod)))))
       
(defmethod subst-for-formals? (ex alist)
  (declare (ignore ex alist))
  nil)

(defmethod subst-for-formals! ((ex name) alist)
  (copy (cdr (assq (declaration ex) alist))))

(defmethod subst-for-formals! ((ex modname) alist)
  (let ((mod (get-theory ex)))
    (copy ex
      'actuals (mapcar #'(lambda (fm)
			   (let ((exp (cdr (assq fm alist))))
			     (mk-actual exp)))
		       (formals-sans-usings mod)))))
