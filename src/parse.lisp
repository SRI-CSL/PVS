;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 21 19:36:29 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Nov  4 18:13:31 1998
;; Update Count    : 54
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :pvs)

;;; Makes extensive use of the following functions from ERGO:
;;;	sim-term-op - returns the symbol which is the operator of a term
;;;	is-sop	    - checks whether a given symbol is the operator of a term
;;;	term-args   - returns the list of arguments of a term
;;;	ds-id       - returns the symbol of an id term


;;; Parsing

(defun parse (&rest keys)
  (let* ((*current-file* (or (cadr (member :file keys))
			     *current-file*))
	 (start-time (when *current-file* (get-universal-time))))
    (init-parser)
    (multiple-value-bind (term error? place msg args)
	(apply #'pvs-parse :return-errors t keys)
      (when error?
	(parse-error place (parse-error-msg msg args)))
      (let* ((nt (cadr (member :nt keys)))
	     (fn (if nt
		     (makesym "XT-~a" (sim-term-op term))
		     #'xt-adt-or-modules))
	     (*parsing-or-unparsing* t)
	     (parsed-object (funcall fn term)))
	;;(assert-places-set)
	(values parsed-object (if *current-file*
				  (- (get-universal-time) start-time)
				  0))))))

(defun init-parser ()
  (setq sbrt:*last-syntax* nil)
  (setq sbrt:*last-newline-comment* nil)
  (setq sbrt:*num-keywords-skipped* -1)
  (setq *table-bracket-counter* 0)
  (setq *elsif-places* nil)
  (setq *operator-places* nil))

(defun parse-error-msg (message args)
  (cond ((initial-error? message)
	 (format nil "Found ~A when expecting~%  ~A" (car args) (cadr args)))
	((lam-error? message)
	 (format nil "~A expected here" (car args)))
	((medial-error? message)
	 (format nil "Found ~A when expecting~%  ~A" (car args) (cadr args)))
	(t (format nil "~?" message args))))

(defun initial-error? (message)
  (and (stringp message)
       (>= (length message) 16)
       (string= message "Initial error." :start1 2 :end1 16)))

(defun lam-error? (message)
  (and (stringp message)
       (>= (length message) 27)
       (string= message "Look ahead set match error." :start1 0 :end1 27)))

(defun medial-error? (message)
  (and (stringp message)
       (>= (length message) 13)
       (string= message "Medial error." :start1 0 :end1 13)))

(defun parse-expr (string)
  (init-parser)
  (pvs-parse :string string :nt 'expr))

(defun psa (string &optional (nt 'adt-or-modules))
  (init-parser)
  (pvs-parse :string string :nt nt))

(defun ps (string &optional (nt 'adt-or-modules))
  (init-parser)
  (let* ((term (pvs-parse :string string :nt nt))
	 (fn (makesym "XT-~a" (sim-term-op term))))
    (funcall fn term)))

(defvar *escaped-operators-used* nil)

(defun xt-adt-or-modules (adt-or-modules)
  (mapcar #'(lambda (arg) (funcall #'xt-adt-or-module arg))
	  (term-args adt-or-modules)))

(defun xt-adt-or-module (adt-or-module)
  (let* ((*escaped-operators-used* nil)
	 (id (ds-vid (term-arg0 adt-or-module)))
	 (formals (term-arg1 adt-or-module))
	 (adt-or-mod (term-arg2 adt-or-module))
	 (aorm (case (sim-term-op adt-or-mod)
		 (module (funcall #'xt-module adt-or-mod id))
		 (datatype (funcall #'xt-datatype adt-or-mod id))
		 (datatypes (funcall #'xt-datatypes adt-or-mod id)))))
    (setf (id aorm) id
	  (formals aorm) (xt-theory-formals formals)
	  (place aorm) (term-place adt-or-module))
    ;;(assert (every #'place (formals aorm)))
    (when *escaped-operators-used*
      (let ((*current-theory* aorm))
	(pvs-info "This theory uses the operator~p ~:*~{~a~^ ~}~
                 ~%Remember that the backslash character must be doubled ~
                 in strings~%E.g., (expand \"/\\\\\")"
	  *escaped-operators-used*)))
    aorm))

(defun xt-datatypes (datatype &optional id inline)
  (let* ((subtypes-term (term-arg0 datatype))
	 (subtypes (mapcar #'(lambda (st)
			       (make-instance 'type-name
				 'id (ds-vid st)
				 'place (term-place st)))
			   (term-args subtypes-term)))
	 (assuming-term (term-arg1 datatype))
	 (assuming (unless (is-sop 'datatype-null-2 assuming-term)
		     (xt-assumings assuming-term)))
	 (using-term (term-arg2 datatype))
	 (using (unless (is-sop 'datatype-null-1 using-term)
		  (xt-using-elt using-term)))
	 (adtcases-term (term-arg3 datatype))
	 (adtcases (xt-adtcases adtcases-term))
	 (endid (ds-vid (term-arg4 datatype))))
    (check-subtypes-constructors-consistency id subtypes adtcases)
    (unless (or (null id)
		(eq id endid))
      (parse-error (term-arg4 datatype)
	"End id ~a does not match datatype id ~a" endid id))
    (if (or (null id) inline)
	(make-instance 'inline-datatype-with-subtypes
	  'subtypes subtypes
	  'assuming assuming
	  'using using
	  'constructors adtcases
	  'place (term-place datatype))
	(make-instance 'datatype-with-subtypes
	  'subtypes subtypes
	  'assuming assuming
	  'using using
	  'constructors adtcases
	  'place (term-place datatype)))))

(defun check-subtypes-constructors-consistency (id subtypes adtcases)
  (let ((badsubtype (find id subtypes :key #'id)))
    (when badsubtype
      (parse-error badsubtype "May not use datatype name as a subtype")))
  (let ((dup (duplicates? subtypes :test #'same-id)))
    (when dup
      (parse-error dup "Duplicate subtypes are not allowed")))
  (let ((badconstr (find-if #'(lambda (x)
				(or (not (typep x 'constructor-with-subtype))
				    (not (member (subtype x) subtypes
						 :test #'same-id))))
		     adtcases)))
    (when badconstr
      (if (typep badconstr 'constructor-with-subtype)
	  (parse-error (subtype badconstr)
	    "The subtype for this constructor was not declared above")
	  (parse-error badconstr
	    "Must specify a subtype for this constructor"))))
  (let ((badsubtype (find-if-not #'(lambda (s)
				     (some #'(lambda (a)
					       (same-id s (subtype a)))
					   adtcases))
		      subtypes)))
    (when badsubtype
      (parse-error badsubtype
	"This subtype has no associated constructors"))))

(defun xt-theory-formals (theory-formals)
  (mapcan #'xt-theory-formal (term-args theory-formals)))

(defun xt-theory-formal (theory-formal)
  (let* ((using (term-arg0 theory-formal))
	 (idops (term-arg1 theory-formal))
	 (decl-body (term-arg2 theory-formal)))
    (multiple-value-bind (decl dtype)
	(xt-declaration-body decl-body)
      (append (unless (is-sop 'theory-formal-null-1 using)
		(list (xt-using-elt using)))
	      (xt-chained-decls (term-args idops) dtype nil decl
				theory-formal)))))

(defun xt-datatype (datatype &optional id inline)
  (let ((assuming (term-arg0 datatype))
	(using (term-arg1 datatype))
	(adtcases (term-arg2 datatype))
	(endid (ds-vid (term-arg3 datatype))))
    (unless (or (null id)
		(eq id endid))
      (parse-error (term-arg3 datatype)
	"End id ~a does not match datatype id ~a" endid id))
    (make-instance (if (or (null id) inline) 'inline-datatype 'datatype)
      'assuming (unless (is-sop 'datatype-null-2 assuming)
		  (xt-assumings assuming))
      'using (unless (is-sop 'datatype-null-1 using)
	       (xt-using-elt using))
      'constructors (xt-adtcases adtcases)
      'place (term-place datatype))))

(defun xt-adtcases (adtcases)
  (mapcar #'xt-adtcase (term-args adtcases)))

(defun xt-adtcase (adtcase)
  (let* ((constructor (term-arg0 adtcase))
	 (recognizer (term-arg1 adtcase))
	 (constr (xt-constructor constructor))
	 (subtype (when (is-sop 'adtcase-subtype adtcase)
		    (make-instance 'type-name
		      'id (ds-id (term-arg2 adtcase))
		      'place (term-place (term-arg2 adtcase))))))
    (when subtype
      (change-class constr 'constructor-with-subtype)
      (setf (subtype constr) subtype))
    (setf (recognizer constr) (xt-idop recognizer))
    (setf (place constr) (term-place constructor))
    constr))

(defun xt-constructor (constructor)
  (let ((idop (term-arg0 constructor))
	(adtdecls (term-arg1 constructor)))
    (make-instance 'simple-constructor
      'id (xt-idop idop)
      'arguments (unless (is-sop 'constructor-null-1 adtdecls)
		   (xt-adtdecls adtdecls))
      'place (term-place constructor))))

(defun xt-adtdecls (decls)
  (mapcan #'xt-adtdecl (term-args decls)))

(defun xt-adtdecl (adtdecl)
  (let* ((idops (term-arg0 adtdecl))
	 (type (term-arg1 adtdecl))
	 (dtype (xt-not-enum-type-expr type)))
    (xt-chained-decls (term-args idops)
		      type
		      nil
		      (make-instance 'adtdecl
			'declared-type dtype
			'place (term-place type))
		      adtdecl)))

(defun xt-module (theory &optional id)
  (let* ((exp-t (term-arg0 theory))
	 (exp (if (is-sop 'noexp exp-t)
		  (make-instance 'exporting
		    'kind 'DEFAULT
		    'place (term-place exp-t))
		  (xt-exporting exp-t)))
	 (assum-t (term-arg1 theory))
	 (assum (unless (is-sop 'noass assum-t)
		  (xt-assumings assum-t)))
	 (tpart-t (term-arg2 theory))
	 (tpart (unless (is-sop 'notheory tpart-t)
		  (xt-theory-part tpart-t)))
	 (endid (ds-vid (term-arg3 theory))))
    (unless (or (null id) (eq id endid))
      (parse-error (term-arg3 theory)
	"End id ~a does not match theory id ~a" endid id))
;    (assert (every #'place (modules exp)))
;    (assert (every #'place assum))
;    (assert (every #'place tpart))
    (make-instance 'module
      'exporting exp
      'assuming assum
      'theory tpart
      'place (term-place theory))))

(defun xt-exporting (exporting)
  (let ((names (term-arg0 exporting))
	(modnames (term-arg1 exporting)))
    (make-instance 'exporting
      'names (case (sim-term-op names)
	       (exporting-all 'ALL)
	       (expnames (mapcar #'xt-expname (term-args names)))
	       (t (break "Something's wrong with the parser")))
      'but-names (when (and (is-sop 'exporting-all names)
			    (not (is-sop 'noexpbuts (term-arg0 names))))
		   (mapcar #'xt-expname
			   (term-args (term-arg0 names))))
      'kind (case (sim-term-op modnames)
	      (expmodall 'ALL)
	      (expmodclosure 'CLOSURE)
	      (t nil))
      'modules (case (sim-term-op modnames)
		 (noexportingmods nil)
		 (expmodall nil)
		 (expmodclosure nil)
		 (t (mapcar #'xt-modname (term-args modnames))))
      'place (term-place exporting))))

(defun xt-expname (expname)
  (let ((id (xt-idop (term-arg0 expname)))
	(kind (term-arg1 expname)))
    (make-instance 'expname
      'id id
      'kind (case (sim-term-op kind)
	      (noexpkind nil)
	      (exptype 'TYPE)
	      (expformula 'FORMULA)
	      (t (xt-not-enum-type-expr kind)))
      'place (term-place expname))))

(defun xt-assuming-part (assumings)
  (mapcan #'(lambda (ass)
	      (if (is-sop 'using-elt ass)
		  (list (xt-using-elt ass))
		  (xt-assuming ass)))
	  (term-args assumings)))

(defun xt-assumings (assumings)
  (mapcan #'(lambda (ass)
	      (if (is-sop 'using-elt ass)
		  (list (xt-using-elt ass))
		  (xt-assuming ass)))
	  (term-args assumings)))

(defun xt-assuming (assuming)
  (let* ((idops (term-arg0 assuming))
	 (formals (term-arg1 assuming))
	 (decl (term-arg2 assuming))
	 (semi (term-arg3 assuming)))
    (multiple-value-bind (pdecl tdecl)
	(xt-declaration-body decl idops)
      (let ((decls (xt-chained-decls (term-args idops)
				     tdecl
				     (unless (is-sop 'noformals
						     formals)
				       (xt-pdf formals))
				     pdecl
				     assuming)))
	(setf (semi pdecl) (when (is-sop 'semic semi) t))
	decls))))

(defun xt-theory-part (theory)
  (mapcan #'(lambda (decl)
	      (case (sim-term-op decl)
		(using-elt (list (xt-using-elt decl)))
		(judgement-elt (xt-judgement-elt decl))
		(conversion-elt (xt-conversion-elt decl))
		(t (xt-theory decl))))
	  (term-args theory)))

(defun xt-theory (tdecl)
  (let ((idops (term-arg0 tdecl))
	(formals (term-arg1 tdecl))
	(decl (term-arg2 tdecl))
	(semi (term-arg3 tdecl)))
    (cond ((is-sop 'datatype decl)
	   (xt-theory-datatype idops formals decl semi))
	  ((is-sop 'datatypes decl)
	   (xt-theory-datatypes idops formals decl semi))
	  (t (multiple-value-bind (pdecl type-decl)
		 (xt-declaration-body decl idops)
	       (let ((decls (xt-chained-decls (term-args idops)
					      type-decl
					      (unless (is-sop 'noformals
							      formals)
						(xt-pdf formals))
					      pdecl
					      tdecl)))
		 (setf (semi pdecl) (when (is-sop 'semic semi) t))
		 decls))))))

(defun xt-theory-datatypes (idops formals decl semi)
  (cond ((is-sop 'pdf formals)
	 (parse-error (term-arg0 formals)
	   "Inline datatypes may not have parameters"))
	((cdr (term-args idops))
	 (parse-error (term-arg0 (term-arg1 idops))
	   "Only one id allowed for datatypes"))
	((not (is-id (term-arg0 (term-arg0 idops))))
	 (parse-error (term-arg0 idops)
	   "Datatype identifier must be an id"))
	(t (let* ((id (ds-vid (term-arg0 (term-arg0 idops))))
		  (adt (funcall #'xt-datatypes decl id t)))
	     (setf (id adt) id
		   (formals adt) (xt-theory-formals formals)
		   (semi adt) (when (is-sop 'semic semi) t))
	     (list adt)))))

(defun xt-theory-datatype (idops formals decl semi)
  (cond ((is-sop 'pdf formals)
	 (parse-error (term-arg0 formals)
	   "Inline datatypes may not have parameters"))
	((cdr (term-args idops))
	 (parse-error (term-arg0 (term-arg1 idops))
	   "Only one id allowed for datatypes"))
	((not (is-id (term-arg0 (term-arg0 idops))))
	 (parse-error (term-arg0 idops)
	   "Datatype identifier must be an id"))
	(t (let* ((id (ds-vid (term-arg0 (term-arg0 idops))))
		  (adt (funcall #'xt-datatype decl id t)))
	     (setf (id adt) id
		   (formals adt) (xt-theory-formals formals)
		   (semi adt) (when (is-sop 'semic semi) t))
	     (list adt)))))

(defun xt-using-elt (using)
  (make-instance 'using
    'modules (mapcar #'xt-modname (term-args (term-arg0 using)))
    'place (term-place using)
    'semi (when (is-sop 'semic (term-arg1 using)) t)))

(defun xt-using (using)
  (make-instance 'using
    'modules (mapcar #'xt-modname (term-args using))
    'place (term-place using)))

(defun xt-judgement-elt (decl)
  (let ((jdecls (xt-judgement (term-arg0 decl))))
    (dolist (jdecl jdecls)
      (setf (semi jdecl) (when (is-sop 'semic (term-arg1 decl)) t)))
    jdecls))

(defun xt-judgement-decl (decl idops)
  (multiple-value-bind (jdecls dtype)
      (xt-judgement decl)
    (cond ((cdr (term-args idops))
	   (parse-error idops
	     "May not declare multiple ids for a judgement"))
	  ((cdr jdecls)
	   (parse-error decl
	     "May not declare multiple judgements when an id is given"))
	  (t (values (car jdecls) dtype)))))

(defun xt-judgement (decl)
  (let* ((jdecls (term-args (term-arg0 decl)))
	 (class (ds-id (term-arg1 decl)))
	 (type (term-arg2 decl))
	 (dtype (xt-not-enum-type-expr type))
	 (place (term-place decl)))
    (values (if (eq class 'subtype-of)
		(xt-subtype-judgement jdecls dtype place)
		(xt-const-judgement jdecls dtype place))
	    type)))

(defun xt-subtype-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-subtype-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))

(defun xt-subtype-judgement* (jdecl dtype place)
  (cond ((is-sop 'jnamedecl jdecl)
	 (let ((name (xt-name (term-arg0 jdecl))))
	   (make-instance 'subtype-judgement
	     'declared-subtype (change-class name 'type-name)
	     'declared-type dtype
	     'chain? t
	     'place place)))
	((is-sop 'jappldecl jdecl)
	 (let ((name (xt-name (term-arg0 jdecl)))
	       (formals (xt-pdf (term-arg1 jdecl))))
	   (when (cdr formals)
	     (parse-error formals "Type applications may not be Curried"))
	   (make-instance 'subtype-judgement
	     'declared-subtype (make-instance 'type-application
				 'type (change-class name 'type-name)
				 'parameters (car formals)
				 'place (place name))
	     'declared-type dtype
	     'chain? t
	     'place place)))
	((is-number jdecl)
	 (parse-error jdecl "type expression expected here"))
	(t (make-instance 'subtype-judgement
	     'declared-subtype (xt-not-enum-type-expr jdecl)
	     'declared-type dtype
	     'chain? t
	     'place place))))

(defun xt-const-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-const-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))

(defun xt-const-judgement* (jdecl dtype place)
  (case (sim-term-op jdecl)
    (jnumberdecl (make-instance 'number-judgement
		   'number (make-instance 'number-expr
			     'number (ds-number (term-arg0 jdecl)))
		   'declared-type dtype
		   'chain? t
		   'place place))
    (jnamedecl (make-instance 'name-judgement
		 'name (change-class (xt-name (term-arg0 jdecl)) 'name-expr)
		 'declared-type dtype
		 'chain? t
		 'place place))
    (jappldecl (make-instance 'application-judgement
		 'name (change-class (xt-name (term-arg0 jdecl)) 'name-expr)
		 'formals (xt-pdf (term-arg1 jdecl))
		 'declared-type dtype
		 'chain? t
		 'place place))
    (t (parse-error jdecl "Types may not have HAS_TYPE judgements."))))

(defun xt-conversion-elt (decl)
  (let ((cdecls (xt-conversion (term-arg0 decl))))
    (dolist (cdecl cdecls)
      (setf (semi cdecl) (when (is-sop 'semic (term-arg1 decl)) t)))
    cdecls))

(defun xt-conversion (decl)
  (let* ((enames (term-args decl))
	 (jdecls (mapcar #'(lambda (ename)
			     (let ((name (xt-name (term-arg0 ename)))
				   (ntype (term-arg1 ename)))
			       (if (is-sop 'notype ntype)
				   (make-instance 'conversion-decl
				     'id (id name)
				     'name (change-class name 'name-expr)
				     'chain? t
				     'place (term-place decl))
				   (make-instance 'typed-conversion-decl
				     'id (id name)
				     'name (change-class name 'name-expr)
				     'declared-type dtype
				     'chain? t
				     'place (term-place decl)))))
			 enames)))
    (setf (chain? (car (last jdecls))) nil)
    jdecls))


(defun xt-chained-decls (idops dtype formals decl absyn &optional result)
  (if (null idops)
      (nreverse result)
      (let ((ndecl (if (cdr idops)
		       (copy decl
			     'id (if (is-id (car idops))
				     (ds-vid (car idops))
				     (xt-idop (car idops)))
			     'chain? t)
		       (progn (setf (id decl)
				    (if (is-id (car idops))
					(ds-vid (car idops))
					(xt-idop (car idops))))
			      decl))))
	(when (and (slot-exists-p ndecl 'declared-type)
		   (declared-type ndecl))
	  (assert dtype)
	  (setf (declared-type ndecl)
		(xt-not-enum-type-expr dtype)))
	(let* ((idpos (position-if #'(lambda (tm)
				       (eq (ds-sim-op (term-op tm)) 'idops))
				   (term-args absyn))))
	  (when idpos
	    (setf (place ndecl)
		  (let ((splace (term-place (car idops)))
			(eplace (term-place absyn)))
		    (when (and splace eplace)
		      (vector (starting-row splace) (starting-col splace)
			      (ending-row eplace) (ending-col eplace)))))))
	(when formals (setf (formals ndecl) formals))
	(xt-chained-decls (cdr idops) dtype formals decl absyn
			  (cons ndecl result)))))

(defun xt-pdf (pdf)
  (mapcar #'xt-adformals (term-args pdf)))

(defun xt-adformals (adformals)
  (xt-adformals* (term-args adformals)))

(defun xt-adformals* (adformals &optional untyped formals)
  (if (null adformals)
      (nconc formals untyped)
      (let ((df (car adformals)))
	(if (is-sop 'typed-ids df)
	    (xt-adformals* (cdr adformals)
			   nil
			   (nconc formals untyped (xt-typed-ids df)))
	    (let ((formal (xt-typed-id df)))
	      (cond ((declared-type formal)
		     (mapc #'(lambda (un)
			       (setf (declared-type un)
				     (declared-type formal))
			       (setf (chain? un) t))
			   untyped)
		     (xt-adformals* (cdr adformals)
				    nil
				    (nconc formals untyped (list formal))))
		    (t (xt-adformals* (cdr adformals)
				      (nconc untyped (list formal))
				      formals))))))))

(defun xt-typed-ids (typed-ids)
  (let* ((idops (term-arg0 typed-ids))
	 (type-expr (term-arg1 typed-ids))
	 (expr (term-arg2 typed-ids))
	 (no-pred (is-sop 'no-pred expr))
	 (type (unless (is-sop 'no-type-expr type-expr)
		 (xt-not-enum-type-expr type-expr))))
    (unless (or no-pred (not (cdr (term-args idops))))
      (parse-error (term-arg0 (term-arg1 (term-arg0 typed-ids)))
	"May not have multiple ids with '|'"))
    (xt-chained-decls
     (term-args idops)
     type-expr
     nil
     (if no-pred
	 (xt-typed-id-bind-decl nil type (term-place typed-ids))
	 (let* ((id (ds-vid (term-arg0 (car (term-args idops)))))
		(place (term-place (term-arg0 (car (term-args idops)))))
		(formals (xt-typed-id-bind-decl id type place))
		(dtype (make-instance (if type 'setsubtype 'nsetsubtype)
			 'supertype type
			 'formals formals
			 'formula (xt-expr expr)
			 'place (term-place type-expr))))
	   (make-instance 'pred-bind-decl
	     'declared-type dtype
	     'place (term-place typed-ids))))
     typed-ids)))

(defun xt-typed-id (typed-id)
  (let* ((idop (term-arg0 typed-id))
	 (type-expr (term-arg1 typed-id))
	 (expr (term-arg2 typed-id))
	 (no-pred (is-sop 'no-pred expr))
	 (type (unless (is-sop 'no-type-expr type-expr)
		 (xt-not-enum-type-expr type-expr))))
    (if no-pred
	(xt-typed-id-bind-decl (xt-idop idop) type (term-place typed-id))
	(let* ((id (xt-idop idop))
	       (place (term-place idop))
	       (formals (xt-typed-id-bind-decl id type place))
	       (dtype (make-instance (if type 'setsubtype 'nsetsubtype)
			'supertype type
			'formals formals
			'formula (xt-expr expr)
			'place (term-place type-expr))))
	  (make-instance 'pred-bind-decl
	    'id (xt-idop idop)
	    'declared-type dtype
	    'place (term-place typed-id))))))

(defun xt-typed-id-bind-decl (id type place)
  (if type
      (make-instance 'bind-decl
	'id id
	'declared-type type
	'place place)
      (make-instance 'untyped-bind-decl
	'id id
	'place place)))

;;; Declarations

(defun xt-declaration-body (body &optional idops)
  (case (sim-term-op body)
    (ftype-decl (xt-ftype-decl body))
    (fnetype-decl (xt-fnetype-decl body))
    (fconst-decl (xt-fconst-decl body))
    (lib-decl (xt-lib-decl body))
    (mod-decl (xt-mod-decl body))
    (uninterp-type-decl (xt-uninterp-type-decl body))
    (type-decl (xt-type-decl body))
    ;;(uninterp-netype-decl (xt-uninterp-netype-decl body))
    (netype-decl (xt-netype-decl body))
    (judgement (xt-judgement-decl body idops))
    (var-decl (xt-var-decl body))
    (uninterp-const-decl (xt-const-decl body))
    (const-decl (xt-const-decl body))
    (def-decl (xt-def-decl body))
    (ind-decl (xt-ind-decl body))
    (assumption (xt-assumption body))
    (formula-decl (xt-formula-decl body))
    (t (error "Decl not recognized - ~a" body))))

(defun xt-ftype-decl (ftype-decl)
  (let ((type-expr (term-arg0 ftype-decl)))
    (if (is-sop 'theory-formal-decl-null-1 type-expr)
	(make-instance 'formal-type-decl
	  'place (term-place ftype-decl))
	(make-instance 'formal-subtype-decl
	  'type-expr (xt-not-enum-type-expr type-expr)
	  'place (term-place ftype-decl)))))

(defun xt-fnetype-decl (ftype-decl)
  (let ((keyword (sim-term-op (term-arg0 ftype-decl)))
	(type-expr (term-arg1 ftype-decl)))
    (if (is-sop 'theory-formal-decl-null-2 type-expr)
	(make-instance 'formal-nonempty-type-decl
	  'keyword keyword
	  'place (term-place ftype-decl))
	(make-instance 'formal-nonempty-subtype-decl
	  'type-expr (xt-not-enum-type-expr type-expr)
	  'keyword keyword
	  'place (term-place ftype-decl)))))

(defun xt-fconst-decl (fconst-decl)
  (let ((dtype (term-arg0 fconst-decl)))
    (values (make-instance 'formal-const-decl
	      'declared-type (xt-not-enum-type-expr dtype)
	      'place (term-place fconst-decl))
	    dtype)))

(defun xt-lib-decl (lib-decl)
  (let* ((libstr (coerce (ds-string (term-arg1 lib-decl)) 'string))
	 (dirstr (if (char= (char libstr (1- (length libstr))) #\/)
		       libstr
		       (concatenate 'string libstr "/"))))
    (make-instance (if (is-sop 'noeq (term-arg0 lib-decl))
		       'lib-decl
		       'lib-eq-decl)
      'lib-string libstr
      'library dirstr
      'place (term-place lib-decl))))

(defun xt-mod-decl (mod-decl)
  (make-instance 'mod-decl
    'modname (xt-modname (term-arg0 mod-decl))
    'place (term-place mod-decl)))

(defun xt-uninterp-type-decl (utd)
  (make-instance 'type-decl
    'place (term-place utd)))

;(defun xt-uninterp-netype-decl (utd)
;  (make-instance 'nonempty-type-decl
;    'keyword (sim-term-op (term-arg0 utd))
;    'place (term-place utd)))

(defun xt-type-decl (type-decl)
  (let ((tdef (xt-typedef (term-arg0 type-decl))))
    (setf (place tdef) (term-place type-decl))
    tdef))

(defun xt-typedef (typedef)
  (let ((alt (term-arg0 typedef))
	(type-expr (term-arg1 typedef)))
    (if (is-sop 'equal alt)
	(make-instance 'type-eq-decl
	  'type-expr (xt-type-expr type-expr))
	(make-instance 'type-from-decl
	  'type-expr (xt-type-expr type-expr)))))

(defun xt-netype-decl (utd)
  (if (is-sop 'no-type-def (term-arg1 utd))
      (make-instance 'nonempty-type-decl
	'keyword (sim-term-op (term-arg0 utd))
	'place (term-place utd))
      (let ((tdef (xt-netypedef (term-arg1 utd))))
	(setf (keyword tdef) (sim-term-op (term-arg0 utd)))
	(setf (place tdef) (term-place utd))
	tdef)))

(defun xt-netypedef (typedef)
  (let ((alt (term-arg0 typedef))
	(type-expr (term-arg1 typedef)))
    (if (is-sop 'equal alt)
	(make-instance 'nonempty-type-eq-decl
	  'type-expr (xt-type-expr type-expr))
	(make-instance 'nonempty-type-from-decl
	  'type-expr (xt-type-expr type-expr)))))

(defun xt-var-decl (var-decl)
  (let ((dtype (term-arg0 var-decl)))
    (values (make-instance 'var-decl
	      'declared-type (xt-not-enum-type-expr dtype)
	      'place (term-place var-decl))
	    dtype)))

(defun xt-const-decl (const-decl)
  (let* ((dtype (term-arg0 const-decl))
	 (type-expr (xt-not-enum-type-expr dtype))
	 (value (cadr (term-args const-decl))))
    (values (make-instance 'const-decl
	      'declared-type type-expr
	      'definition (when value (xt-expr value))
	      'place (term-place const-decl))
	    dtype)))

(defun xt-def-decl (def-decl)
  (let ((type-expr (term-arg0 def-decl))
	(def (term-arg1 def-decl))
	(meas (term-arg2 def-decl))
	(ordering (term-arg3 def-decl)))
    (values (make-instance 'def-decl
	      'declared-type (xt-not-enum-type-expr type-expr)
	      'definition (xt-expr def)
	      'declared-measure (xt-expr meas)
	      'ordering (unless (is-sop 'noexpr ordering)
			  (xt-expr ordering))
	      'place (term-place def-decl))
	    type-expr)))

(defun xt-ind-decl (ind-decl)
  (let ((type-expr (term-arg0 ind-decl))
	(def (term-arg1 ind-decl)))
    (values (make-instance 'inductive-decl
	      'declared-type (xt-not-enum-type-expr type-expr)
	      'definition (xt-expr def)
	      'place (term-place ind-decl))
	    type-expr)))

(defun xt-assumption (assumption)
  (make-instance 'formula-decl
    'spelling 'assumption
    'definition (xt-expr (term-arg0 assumption))
    'place (term-place assumption)))

(defun xt-formula-decl (formula-decl)
  (let ((fname (term-arg0 formula-decl))
	(expr (term-arg1 formula-decl)))
    (when (and *no-obligations-allowed*
	       (is-sop 'obligation fname))
      (parse-error fname
	"The OBLIGATION keyword is not allowed here"))
    (make-instance 'formula-decl
      'spelling (sim-term-op fname)
      'definition (xt-expr expr)
      'place (term-place formula-decl))))


;;; Type Expressions

(defun xt-not-enum-type-expr (type-expr)
  (let ((te (xt-type-expr type-expr)))
    (if (enumtype? te)
	(parse-error te
	  "Enumeration types are only allowed at the top level")
	te)))

(defun xt-type-expr (type-expr)
  (case (sim-term-op type-expr)
    ((type-name name-expr)		; Allow name-exprs from actuals
     (xt-type-name type-expr))
    (type-appl (xt-type-appl type-expr))
    ;;(quotienttype (xt-quotienttype type-expr))
    (subtype (xt-subtype type-expr))
    (expr-as-type (xt-expr-as-type type-expr))
    ;;(enum-or-subtype (xt-enum-or-subtype type-expr))
    (enumtype (xt-enumtype type-expr))
    (funtype (xt-funtype type-expr))
    (recordtype (xt-recordtype type-expr))
    (t (error "type-expr not recognized - ~a" type-expr))))

(defun xt-type-name (type-name)
  (let ((name (xt-name (term-arg0 type-name))))
    (change-class name 'type-name)
    (setf (place name) (term-place type-name))
    name))

(defun xt-type-appl (type-expr)
  (let ((type (term-arg0 type-expr))
	(args (term-arg1 type-expr)))
    (make-instance 'type-application
      'type (change-class (xt-name type) 'type-name)
      'parameters (mapcar #'xt-expr (term-args args))
      'place (term-place type-expr))))

(defun xt-enumtype (type-expr)
  (let ((args (xt-enumtype-args (term-arg0 type-expr)))
	(containing (term-arg1 type-expr)))
    (unless (is-sop 'nocontaining containing)
      (parse-error containing "CONTAINING not expected here"))
    (xt-enum args type-expr)))

(defun xt-enum (args enum)
  (let ((constrs
	 (mapcar #'(lambda (e)
		     (let ((constr (make-instance 'simple-constructor
				     'id (xt-idop e)
				     'recognizer
				     (makesym "~a?" (op-to-id (xt-idop e))))))
		       ;;(unparse constr :string t)
		       (setf (place constr) (term-place e))
		       constr))
		 args)))
    (make-instance 'enumtype
      'constructors constrs
      'place (term-place enum))))

(defun xt-enumtype-args (args &optional result)
  (if (is-sop 'set-formals args)
      (if (is-sop 'comma (term-arg1 args))
	  (xt-enumtype-args (term-arg2 args)
			    (cons (xt-enumtype-arg (term-arg0 args))
				  result))
	  (parse-error (term-arg1 args) "Comma expected here"))
      (nreverse (cons (xt-enumtype-arg args) result))))

(defun xt-enumtype-arg (arg)
  (unless (is-sop 'set-id arg)
    (parse-error arg "( not expected here"))
  (unless (is-sop 'notype (term-arg1 arg))
    (parse-error (term-arg1 arg) ": not expected here"))
  (term-arg0 arg))

(defun xt-subtype (type-expr)
  (let* ((args (term-arg0 type-expr))
	 (expr (term-arg1 type-expr))
	 (containing (term-arg2 type-expr))
	 (set-expr (mk-ergo-term* 'set-expr args expr))
	 (pred (make-xt-bind-expr 'set-expr set-expr set-expr)))
    (setf (place pred) (term-place type-expr))
    (make-instance 'subtype
      'predicate pred
      'contains (unless (is-sop 'nocontaining containing)
		  (xt-expr containing))
      'place (term-place type-expr))))
	  
(defun xt-expr-as-type (expr-as-type)
  (make-instance 'expr-as-type
    'expr (xt-expr (term-arg0 expr-as-type))
    'contains (unless (is-sop 'nocontains (term-arg1 expr-as-type))
		(xt-expr (term-arg1 expr-as-type)))
    'place (term-place expr-as-type)))

(defun xt-idops (idops)
  (mapcar #'xt-idop (term-args idops)))

(defun xt-funtype (funtype)
  (let ((kind (term-arg0 funtype))
	(dep-type-exprs (term-arg1 funtype))
	(range (term-arg2 funtype)))
    (if (is-sop 'comp-type-expr-null-2 range)
	(if (is-sop 'comp-type-expr-null-1 kind)
	    (let ((dep-types (xt-dep-type-exprs dep-type-exprs)))
	      (if (cdr dep-types)
		  (make-instance 'tupletype
		    'types dep-types
		    'place (term-place funtype))
		  (progn (incf (parens (car dep-types)))
			 (car dep-types))))
	    (parse-error funtype "Function type must have a range"))
	(let* ((range (xt-not-enum-type-expr range))
	       (dom (xt-dep-type-exprs dep-type-exprs))
	       (tvar (make-new-variable '|t| (cons range dom)))
	       (domain (xt-funtype-domain dom tvar)))
	  (make-instance (case (sim-term-op kind)
			   (comp-type-expr-null-1 'funtype)
			   (function 'functiontype)
			   (array 'arraytype))
	  'domain domain
	  'range (xt-subst-new-domain-dep domain range)
	  'place (term-place funtype))))))

(defmethod xt-subst-new-domain-dep (domain range)
  (declare (ignore domain))
  range)

(defmethod xt-subst-new-domain-dep ((domain dep-binding) range)
  (if (typep (declared-type domain) 'dep-domain-tupletype)
      (let ((tvar (id domain))
	    (bindings (var-bindings (declared-type domain))))
	(gensubst range
	  #'(lambda (ex)
	      (let ((index (cdr (assq (id ex) bindings))))
		(make-instance 'projection-application
		  'id (makesym "PROJ_~d" index)
		  'index index
		  'argument (make-instance 'name-expr 'id tvar))))
	  #'(lambda (ex)
	      (and (typep ex 'name-expr)
		   (assq (id ex) bindings)))))
      range))

(defun xt-funtype-domain (type-exprs tvar)
  (if (cdr type-exprs)
      (if (some #'(lambda (te) (typep te 'dep-binding)) type-exprs)
	  (xt-funtype-dep-domain type-exprs tvar)
	  (make-instance 'domain-tupletype
	    'types type-exprs))
      (car type-exprs)))

(defun xt-funtype-dep-domain (type-exprs tvar &optional (index 0)
					 types var-bindings)
  (if (null type-exprs)
      (make-instance 'dep-binding
	'id tvar
	'declared-type (make-instance 'dep-domain-tupletype
			 'types (nreverse types)
			 'var-bindings (nreverse var-bindings)))
      (if (typep (car type-exprs) 'dep-binding)
	  (let* ((id (id (car type-exprs)))
		 (var-binding (cons id (1+ index)))
		 (occurs? (id-occurs-in id (cdr type-exprs))))
	    (xt-funtype-dep-domain
	     (cdr type-exprs) tvar (1+ index)
	     (cons (if occurs?
		       (car type-exprs)
		       (declared-type (car type-exprs)))
		   types)
	     (cons var-binding var-bindings)))
	  (xt-funtype-dep-domain
	   (cdr type-exprs) tvar (1+ index)
	   (cons (car type-exprs) types)
	   var-bindings))))
	   
				   
      
(defun xt-dep-type-exprs (dep-type-exprs)
  (mapcar #'xt-dep-type-expr (term-args dep-type-exprs)))

(defun xt-dep-type-expr (dep-type-expr)
  (if (is-sop 'dep-binding dep-type-expr)
      (xt-dep-binding dep-type-expr)
      (xt-not-enum-type-expr dep-type-expr)))

(defun xt-dep-binding (dep-binding)
  (let ((idop (term-arg0 dep-binding))
	(type (term-arg1 dep-binding)))
    (make-instance 'dep-binding
      'id (xt-idop idop)
      'declared-type (xt-not-enum-type-expr type)
      'place (term-place dep-binding))))

(defun xt-recordtype (recordtype)
  (make-instance 'recordtype
    'fields (xt-fields (term-arg0 recordtype))
    'place (term-place recordtype)))

(defun xt-fields (fields)
  (mapcan #'xt-field-decls (term-args fields)))

(defun xt-field-decls (field-decls)
  (let ((ids (term-arg0 field-decls))
	(type-expr (term-arg1 field-decls)))
    (xt-chained-decls (term-args ids)
		      type-expr
		      nil
		      (make-instance 'field-decl
			'declared-type (xt-not-enum-type-expr type-expr)
			'place (term-place field-decls))
		      field-decls)))

;;; Expressions

(defun xt-expr (expr)
  (case (sim-term-op expr)
    (number-expr (xt-number-expr expr))
    (string-expr (xt-string-expr expr))
    (name-expr (xt-name-expr expr))
    (list-expr (xt-list-expr expr))
    ;;(true (xt-true expr))
    ;;(false (xt-false expr))
    (rec-expr (xt-rec-expr expr))
    (tuple-expr (xt-tuple-expr expr))
    (term-expr (xt-term-expr expr))
    (unary-term-expr (xt-unary-term-expr expr))
    (fieldappl (xt-fieldappl expr))
    (projappl (xt-projappl expr))
    ;;(intype (xt-intype expr))
    (coercion (xt-coercion expr))
    (if-expr (xt-if-expr expr))
    (application (xt-application expr))
    (bind-expr (xt-bind-expr expr))
    (name-bind-expr (xt-name-bind-expr expr))
    (set-expr (xt-set-expr expr))
    (let-expr (xt-let-expr expr))
    (where-expr (xt-where-expr expr))
    (update-expr (xt-update-expr expr))
    ;;(override-expr (xt-override-expr expr))
    (cases-expr (xt-cases-expr expr))
    (cond-expr (xt-cond-expr expr))
    (table-expr (xt-table-expr expr))
    (skovar (xt-skovar expr))
    (brack-expr (xt-brack-expr expr))
    (t (error "Unrecognized expr - ~a" expr))))

(defun xt-number-expr (expr)
  (make-instance 'number-expr
    'number (ds-number (term-arg0 expr))
    'place (term-place expr)))

(defun xt-string-expr (expr)
  (let ((string (ds-string (term-arg0 expr)))
	(ne (mk-name-expr '|char?|)))
    (setf (parens ne) 1)
    (make-instance 'string-expr
      'string-value string
      'operator (mk-name-expr '|list2finseq| (list (mk-actual ne)))
      'argument (xt-string-to-charlist string (term-place expr)))))

(defun xt-string-to-charlist (string place)
  (let ((codes (xt-string-to-codes string 0 (length string) place nil)))
    (xt-string-to-charlist* codes place)))

(defun xt-string-to-charlist* (codes place)
  (if (null codes)
      (add-place (mk-name-expr '|null|)
		 (vector (ending-row place) (ending-col place)
			 (ending-row place) (ending-col place)))
      (let* ((code (caar codes))
	     (cplace (cdar codes))
	     (aplace (vector (starting-row cplace) (starting-col cplace)
			     (ending-row place) (ending-col place)))
	     (scar (add-place
		    (mk-application (add-place (mk-name-expr '|char|) cplace)
		      (add-place (mk-number-expr (caar codes)) cplace))
		    cplace))
	     (scdr (xt-string-to-charlist* (cdr codes) place)))
	 (make-instance 'application
	   'operator (add-place (mk-name-expr '|cons|) cplace)
	   'argument (make-instance 'arg-tuple-expr
		       'exprs (list scar scdr)
		       'place aplace)
	   'place aplace))))

(defun xt-string-to-codes (string pos len place codes)
  (if (>= pos len)
      (nreverse codes)
      (multiple-value-bind (code npos)
	  (next-string-code string pos len place)
	(xt-string-to-codes string npos len place
			    (acons code
				   (vector (starting-row place)
					   (+ (starting-col place) pos)
					   (starting-row place)
					   (+ (starting-col place) npos))
				   codes)))))

(defun next-string-code (string pos len place)
  (let ((char (char string pos)))
    (if (or *tex-mode*
	    (char/= char #\\))
	(values (char-code char) (1+ pos))
	(if (>= (1+ pos) len)
	    (let ((cplace (vector (svref place 0) (+ (svref place 1) pos)
				  (svref place 0) (+ (svref place 1) pos))))
	      (parse-error cplace "String ends with a '\\'"))
	    (let ((echar (char string (1+ pos))))
	      (case echar
		(#\a (values (char-code #-gcl #\Bell #+gcl #\^G) (+ pos 2)))
		(#\b (values (char-code #\Backspace) (+ pos 2)))
		(#\f (values (char-code #\Page) (+ pos 2)))
		(#\n (values (char-code #\Newline) (+ pos 2)))
		(#\r (values (char-code #\Return) (+ pos 2)))
		(#\t (values (char-code #\Tab) (+ pos 2)))
		(#\v (values (char-code #-gcl #\VT #+gcl #\^K) (+ pos 2)))
		(#\' (values (char-code #\') (+ pos 2)))
		(#\" (values (char-code #\") (+ pos 2)))
		(#\? (values (char-code #\?) (+ pos 2)))
		(#\\ (values (char-code #\\) (+ pos 2)))
		((#\x #\X)
		 (if (digit-char-p (char string (+ pos 2)) 16)
		     (parse-integer string :radix 16 :start (+ pos 2))
		     (let ((cplace (vector (svref place 0)
					   (+ (svref place 1) pos 2)
					   (svref place 0)
					   (+ (svref place 1) pos 2))))
		       (parse-error cplace
			 "Illegal character after '\\X': must be a hex digit"
			 ))))
		(t (if (digit-char-p echar)
		       (parse-integer string :radix 8 :start (1+ pos))
		       (let ((cplace (vector (svref place 0)
					     (+ (svref place 1) pos 1)
					     (svref place 0)
					     (+ (svref place 1) pos 1))))
			 (parse-error cplace
			   "Illegal character after '\\'"))))))))))
		    

(defun xt-list-expr (expr)
  (let* ((place (term-place expr))
	 (last-place (when place
		       (vector (ending-row place) (ending-col place)
			       (ending-row place) (ending-col place))))
	 (lexpr (xt-list-expr* (term-args expr) last-place)))
    (setf (place lexpr) place)
    lexpr))

(defun xt-list-expr* (exprs last-place)
  (if exprs
      (let* ((ex (xt-expr (car exprs)))
	     (list (xt-list-expr* (cdr exprs) last-place)))
	(make-instance 'list-expr
	  'operator (make-instance 'name-expr
		      'id '|cons|
		      'place (place ex))
	  'argument (make-instance 'arg-tuple-expr
		      'exprs (list ex list)
		      'place (place ex))
	  'place (place ex)))
      (make-instance 'null-expr
	'id '|null|
	'place last-place)))

(defun xt-brack-expr (expr)
  (let ((args (term-args expr)))
    (make-instance 'bracket-expr
      'operator (make-instance 'name-expr
		  'id '[\|\|]
		  'place (term-place expr))
      'argument (xt-arg-expr args)
      'place (term-place expr))))

(defun xt-arg-expr (args)
  (if (cdr args)
      (let ((exprs (mapcar #'xt-expr args)))
	(make-instance 'arg-tuple-expr
	  'exprs exprs
	  'place (merge-places (place (car exprs))
			       (place (car (last exprs))))))
      (xt-expr (car args))))

(defun xt-name-expr (expr)
  (let ((name (xt-name (term-arg0 expr))))
    (if (typep name 'number-expr)
	name
	(let* ((upid (intern (string-upcase (string (id name)))))
	       (prindex (projection? upid)))
	  (if prindex
	      (cond ((actuals name)
		     (parse-error expr "Projection may not have actuals"))
		    ((mod-id name)
		     (parse-error expr "Projection may not have a theory id"))
		    ((library name)
		     (parse-error expr "Projection may not have a library id"))
		    (t (make-instance 'projection-expr
			 'id upid
			 'index prindex
			 'place (term-place expr))))
	      (progn (change-class name 'name-expr)
		     (setf (place name) (term-place expr))
		     name))))))

(defun projection? (id)
  (let ((str (string-upcase (string id))))
    (and (> (length str) 5)
	 (string= str "PROJ_" :end1 5)
	 (every #'digit-char-p (subseq str 5))
	 (parse-integer str :start 5))))

(defun xt-rec-expr (rec-expr)
  (make-instance 'record-expr
    'assignments (mapcar #'xt-assignment
			 (term-args (term-arg0 rec-expr)))
    'place (term-place rec-expr)))

(defun xt-tuple-expr (expr)
  (let ((exprs (mapcar #'xt-expr (term-args expr))))
    (cond ((null exprs)
	   (parse-error expr "Expression expected here"))
	  ((cdr exprs)
	   (make-instance 'tuple-expr
	     'exprs exprs
	     'place (term-place expr)))
	  (t (incf (parens (car exprs)))
	     (car exprs)))))

(defun xt-fieldappl (expr)
  (make-instance 'fieldappl
    'id (ds-id (term-arg1 expr))
    'argument (xt-expr (term-arg0 expr))
    'place (term-place expr)))

(defun xt-projappl (expr)
  (let ((index (ds-number (term-arg1 expr))))
    (make-instance 'projappl
      'index index
      'argument (xt-expr (term-arg0 expr))
      'place (term-place expr))))

(defun xt-term-expr (expr)
  (let* ((op (term-arg0 expr))
	 (opid (sim-term-op op))
	 (args (term-args (term-arg1 expr)))
	 (ne (make-instance 'name-expr
	       'id opid
	       'place (term-place op))))
    (make-instance 'infix-application
      'operator ne
      'argument (if (cdr args)
		    (let ((exprs (mapcar #'xt-expr args)))
		      (make-instance 'arg-tuple-expr
			'exprs exprs
			'place (merge-places (place (car exprs))
					     (place (car (last exprs))))))
		    (xt-expr (car args)))
      'place (term-place expr))))

(defun xt-unary-term-expr (uexpr)
  (let* ((op (term-arg0 uexpr))
	 (expr (term-arg1 uexpr))
	 (opex (mk-name-expr (sim-term-op op))))
    (setf (place opex) (term-place op))
    (make-instance 'unary-application
      'operator opex
      'argument (xt-expr expr)
      'place (term-place uexpr))))

(defun xt-coercion (coercion)
  (let* ((expr (term-arg0 coercion))
	 (type (term-arg0 (term-arg1 coercion))))
    (make-instance 'coercion
      'operator (mk-lambda-expr (list (mk-bind-decl '|x|
					(xt-type-expr type)))
		  (mk-name-expr '|x|))
      'argument (xt-expr expr)
      'place (term-place coercion))))

(defun xt-if-expr (expr)
  (let* ((cond (term-arg0 expr))
	 (then (term-arg1 expr))
	 (elsif (term-arg2 expr))
	 (else (term-arg3 expr))
	 (place (term-place expr))
	 (if-place (when place
		     (vector (starting-row place) (starting-col place)
			     (starting-row place) (+ (starting-col place) 2))))
	 (if-name (make-instance 'name-expr
		    'id 'if
		    'place if-place)))
    (make-instance 'if-expr
      'operator if-name
      'argument (make-instance 'arg-tuple-expr
		  'exprs (list (xt-expr cond)
			       (xt-expr then)
			       (xt-elsif-expr (term-args elsif) else))
		  'place (merge-places (term-place cond)
				       (term-place else)))
      'place (term-place expr))))

(defun xt-elsif-expr (elsifs else)
  (if (null elsifs)
      (xt-expr else)
      (let* ((tplace (term-place (car elsifs)))
	     (place (when tplace
		      (vector (starting-row tplace) (starting-col tplace)
			      (starting-row tplace) (+ (starting-col tplace) 5)))))
	(make-instance 'chained-if-expr
	  'operator (make-instance 'name-expr
		      'id 'if
		      'place place)
	  'argument (make-instance 'arg-tuple-expr
		      'exprs (list (xt-expr (term-arg0 (car elsifs)))
				   (xt-expr (term-arg1 (car elsifs)))
				   (xt-elsif-expr (cdr elsifs) else)))))))

(defun xt-application (expr)
  (let ((op (xt-expr (term-arg0 expr)))
	(args (term-args (term-arg1 expr))))
    (unless args
      (parse-error (term-arg1 expr) "argument expected here"))
    (if (typep op 'projection-expr)
	(make-instance 'projection-application
	  'id (id op)
	  'index (index op)
	  'argument (xt-arg-expr args)
	  'place (term-place expr))
	(make-instance 'application
	  'operator op
	  'argument (xt-arg-expr args)
	  'place (term-place expr)))))


(defun xt-bind-expr (bexpr)
  (let ((op (term-arg0 bexpr))
	(body (term-arg1 bexpr)))
    (make-xt-bind-expr (sim-term-op op) body bexpr)))

(defun xt-name-bind-expr (bexpr)
  (let ((op (term-arg0 bexpr))
	(body (term-arg1 bexpr)))
    (make-instance 'binding-application
      'operator (make-instance 'name-expr
		  'id (ds-id op)
		  'place (term-place op))
      'argument (make-xt-bind-expr 'lambda-expr body nil)
      'place (term-place bexpr))))

(defun xt-skovar (expr)
  (make-instance 'name-expr
    'id (makesym "~a!~d"
		 (ds-id (term-arg0 expr))
		 (ds-number (term-arg1 expr)))
    'place (term-place expr)))

(defun make-xt-bind-expr (class body save-as)
  (let* ((set-expr? (is-sop 'set-expr body))
	 (commas? (xt-lambda-formals-check (term-arg0 body)))
	 (bindings (xt-lambda-formals (term-arg0 body) commas?))
	 (rterm (unless set-expr? (term-arg1 body)))
	 (rtype (when (and rterm
			   (not (is-sop 'noreturntype rterm)))
		  (xt-not-enum-type-expr rterm)))
	 (expr (xt-expr (if set-expr?
			    (term-arg1 body)
			    (term-arg2 body)))))
    (make-instance class
      'bindings (xt-flatten-bindings (car bindings) (if set-expr? 1 0))
      'result-type rtype
      'expression (make-xt-bind-expr* (cdr bindings) class expr)
      'commas? commas?
      'place (term-place save-as))))


;;; Checks whether the formals all make sense, and returns whether the
;;; formals are separated by commas or whitespace.  Note that even with
;;; one formal, the separator is important; this is used to distinguish
;;; between (LAMBDA x: x) and (LAMBDA (x): x) --- returns T and NIL, resp.

(defun xt-lambda-formals-check (lambda-formals &optional comma-needed?)
  (if (or (is-sop 'lambda-formals lambda-formals)
	  (is-sop 'set-formals lambda-formals))
      (let ((lambda-formal (term-arg0 lambda-formals))
	    (comma (term-arg1 lambda-formals)))
	(xt-lambda-formal-check lambda-formal comma comma-needed?)
	(xt-lambda-formals-check (term-arg2 lambda-formals)
				 (if (is-sop 'comma comma) 'yes 'no)))
      (xt-lambda-formal-check lambda-formals nil comma-needed?)))

(defun xt-lambda-formal-check (lambda-formal comma comma-needed?)
  (when (or (and (eq comma-needed? 'yes)
		 comma
		 (is-sop 'nocomma comma))
	    (and (eq comma-needed? 'no)
		 comma
		 (is-sop 'idop lambda-formal)))
    (parse-error comma ", expected here"))
  (when (and (eq comma-needed? 'no)
	     comma
	     (is-sop 'comma comma))
    (parse-error comma ", not allowed here"))
  (when (and (eq comma-needed? 'no)
	     (is-sop 'idop lambda-formal))
    (parse-error lambda-formal "( expected here"))
  (when (is-sop 'adformals lambda-formal)
    (let ((notype-or-pred (find-if #'(lambda (adf)
				       (and (is-sop 'typed-ids adf)
					    (is-sop 'no-type-expr (term-arg1 adf))
					    (is-sop 'no-pred (term-arg2 adf))))
			    (term-args lambda-formal))))
      (when notype-or-pred
	(parse-error (car (term-args lambda-formal))
	  "Remove parentheses or give a type or predicate"))))
  (when (and (eq comma-needed? 'yes)
	     (is-sop 'adformals lambda-formal))
    (let* ((adformals (term-args lambda-formal))
	   (too-many-parens (find-if #'(lambda (adf)
					 (is-sop 'typed-ids adf))
			      adformals))
	   (has-pred (find-if-not #'(lambda (adf)
				      (is-sop 'no-pred (term-arg2 adf)))
		       adformals))
	   (has-type (find-if-not #'(lambda (adf)
				      (is-sop 'no-type-expr (term-arg1 adf)))
		       adformals)))
      (when too-many-parens
	(parse-error too-many-parens "id expected here"))
      (when (and has-pred
		 (cdr adformals))
	(parse-error has-pred
	  "Only a single id allowed when a predicate is given"))
      (when (and (null has-type) (null has-pred))
	(parse-error (car adformals)
	  "Remove parentheses or give a type or predicate"))
      (when (and has-type
		 (not (eq has-type (car (last adformals)))))
	(parse-error has-pred "Only the last identifier may specify a type"))))
  ;; If comma is nil, then we are at the last binding.  If
  ;; comma-needed? is also nil, then there is only one, and we return
  ;; t if it is an idop or a parenthesized typed-ids.  This is needed
  ;; in order to correctly reconstruct the bindings list (see
  ;; xf-lambda-formal in unparse.lisp.
  (if comma-needed?
      (eq comma-needed? 'yes)
      (xt-formal-at-level-one? lambda-formal)))

(defun xt-formal-at-level-one? (lambda-formal)
  (or (and (is-sop 'idop lambda-formal) t)
      (let ((adformals (term-args lambda-formal)))
	(and (every #'(lambda (adf) (is-sop 'typed-id adf))
		    adformals)
	     (every #'(lambda (adf)
			(and (is-sop 'no-type-expr (term-arg1 adf))
			     (is-sop 'no-pred (term-arg2 adf))))
		    (butlast adformals))
	     (not (and (is-sop 'no-type-expr
			       (term-arg1 (car (last adformals))))
		       (is-sop 'no-pred
			       (term-arg2 (car (last adformals))))))))))

(defun xt-lambda-formals (lambda-formals &optional commas? bindings lbindings)
  (if (or (is-sop 'lambda-formals lambda-formals)
	  (is-sop 'set-formals lambda-formals))
      (let* ((formals (xt-lambda-formal (term-arg0 lambda-formals)))
	     (lambda-formals-rest (term-arg2 lambda-formals))
	     (nbindings (cons formals bindings)))
	(xt-lambda-formals lambda-formals-rest
			   commas?
			   (when commas?
			     nbindings)
			   (if commas?
			       lbindings
			       (cons (nreverse nbindings) lbindings))))
      (let* ((formals (xt-lambda-formal lambda-formals))
	     (bindings (if commas?
			   (nreverse (cons (nreverse (cons formals bindings))
					   lbindings))
			   (nreverse (cons (list formals)
					   (if bindings
					       (cons bindings lbindings)
					       lbindings))))))
	bindings)))

(defun xt-lambda-formal (formal)
  (cond ((is-sop 'idop formal)
	 (make-instance 'bind-decl
	   'id (xt-idop formal)
	   'place (term-place formal)))
	((is-sop 'set-id formal)
	 (if (is-sop 'notype (term-arg1 formal))
	     (make-instance 'untyped-bind-decl
	       'id (xt-idop (term-arg0 formal))
	       'place (term-place formal))
	     (make-instance 'bind-decl
	       'id (xt-idop (term-arg0 formal))
	       'declared-type (xt-type-expr (term-arg1 formal))
	       'place (term-place formal))))
	(t (xt-lpdformals (term-args formal)))))

(defun xt-lpdformals (adformals)
  (xt-ladformals adformals))

(defun xt-ladformals (formals &optional bindings)
  (if (null formals)
      (nreverse bindings)
      (xt-ladformals
       (cdr formals)
       (cons (if (is-sop 'typed-id (car formals))
		 (xt-typed-id (car formals))
		 (xt-typed-ids (car formals)))
	     bindings))))


;;; The primary thing that flatten-bindings does is to flatten the
;;; bindings-structure into a flat list of bindings.  On the way, it looks
;;; ahead for default types of bindings, if necessary.  Thus in
;;; (LAMBDA (x,y,z: int): y), x and y get declared-type int associated
;;; with them, while in (LAMBDA (x,(y),z: int): y), only z gets int.
;;; Basically, for any bindings list, the declared type is set to whatever
;;; can be found before a parentheses is found, unless aleady set.

(defun xt-flatten-bindings (bindings-structure level)
  (xt-set-declared-types bindings-structure)
  (mapc #'(lambda (x) (xt-set-paren-level x level))
	bindings-structure)
  (xt-flatten-bindings* bindings-structure))

(defun xt-set-declared-types (bindings-structure)
  (when (and bindings-structure
	     (listp bindings-structure))
    (cond ((listp (car bindings-structure))
	   (xt-set-declared-types (car bindings-structure))
	   (xt-set-declared-types (cdr bindings-structure)))
	  ((typep (car bindings-structure) 'untyped-bind-decl)
	   (dolist (b (cdr bindings-structure))
	     (cond ((listp b) (return))
		   ((declared-type b)
		    (setf (declared-type (car bindings-structure))
			  (declared-type b))
		    (setf (chain? (car bindings-structure)) t)
		    (return))))
	   (xt-set-declared-types (cdr bindings-structure)))
	  (t (xt-set-declared-types (cdr bindings-structure))))))

(defun xt-set-paren-level (bindings-structure &optional (level 0))
  (cond ((null bindings-structure) nil)
	((listp bindings-structure)
	 (xt-set-paren-level (car bindings-structure) (1+ level))
	 (xt-set-paren-level (cdr bindings-structure) level))
	(t (setf (parens bindings-structure) (max 0 (1- level))))))

(defun xt-flatten-bindings* (bindings-structure)
  (when bindings-structure
    (if (listp bindings-structure)
	(nconc (xt-flatten-bindings* (car bindings-structure))
	       (xt-flatten-bindings* (cdr bindings-structure)))
	(list bindings-structure))))
	      
  

(defun make-xt-bind-expr* (bindings class expr)
  (if bindings
      (make-instance class
	'bindings (xt-flatten-bindings (car bindings) 0)
	'expression (make-xt-bind-expr* (cdr bindings) class expr)
	'chain? t)
      expr))

(defun xt-set-expr (set-expr)
  (let ((set-formals (term-arg0 set-expr))
	(expr (term-arg1 set-expr)))
    (make-xt-bind-expr 'set-expr set-expr set-expr)))


(defun xt-let-expr (lexpr)
  (let* ((let-bindings (term-arg0 lexpr))
	 (expr (xt-expr (term-arg1 lexpr)))
	 (bindings (xt-let-bindings let-bindings))
	 (args (apply #'xt-let-arguments (term-args let-bindings))))
    (xt-let-expr* expr bindings args lexpr t)))

(defun xt-let-expr* (expr bindings args &optional lexpr first?)
  (if (null bindings)
      expr
      (make-instance (if first? 'let-expr 'chained-let-expr)
	'operator (make-instance 'lambda-expr
		    'bindings (if (listp (car bindings))
				  (car bindings)
				  (list (car bindings)))
		    'expression (xt-let-expr* expr (cdr bindings) (cdr args)
					      lexpr)
		    'place (term-place lexpr))
	'argument (car args)
	'place (term-place lexpr))))

(defun xt-where-expr (lexpr)
  (let* ((let-bindings (term-arg0 lexpr))
	 (expr (xt-expr (term-arg1 lexpr)))
	 (bindings (xt-let-bindings let-bindings))
	 (args (apply #'xt-let-arguments (term-args let-bindings))))
    (xt-where-expr* expr bindings args lexpr)))

(defun xt-where-expr* (expr bindings args &optional lexpr)
  (if (null bindings)
      expr
      (make-instance (if lexpr 'where-expr 'chained-where-expr)
	'operator (make-instance 'lambda-expr
		    'bindings (if (listp (car bindings))
				  (car bindings)
				  (list (car bindings)))
		    'expression (xt-where-expr* expr (cdr bindings) (cdr args))
		    'place (term-place lexpr))
	'argument (car args)
	'place (term-place lexpr))))

(defun xt-let-bindings (let-bindings)
  (mapcar #'xt-let-bind (term-args let-bindings)))

(defun xt-let-bind (lbind)
  (let ((let-bind (term-arg0 lbind)))
    (if (is-sop 'simplebind let-bind)
	(xt-simplebind let-bind)
	(mapcar #'xt-simplebind (term-args let-bind)))))

(defun xt-simplebind (bind)
  (if (is-sop 'notype (term-arg2 bind))
      (make-instance 'untyped-bind-decl
	'id (xt-idop (term-arg0 bind))
	'place (term-place bind))
      (make-instance 'arg-bind-decl
	'id (xt-idop (term-arg0 bind))
	'declared-type (xt-type-expr (term-arg2 bind))
	'place (term-place bind))))

(defun xt-let-arguments (&rest let-binds)
  (mapcar #'(lambda (b) (xt-expr (term-arg1 b))) let-binds))

(defun xt-update-expr (uexpr)
  (let ((expr (term-arg0 uexpr))
	(assignments (term-arg1 uexpr)))
    (make-instance 'update-expr
      'expression (xt-expr expr)
      'assignments (mapcar #'xt-assignment (term-args assignments))
      'place (term-place uexpr))))

(defun xt-override-expr (oexpr)
  (make-instance 'override-expr
    'left (xt-expr (term-arg0 oexpr))
    'right (xt-expr (term-arg1 oexpr))
    'place (term-place oexpr)))

(defun xt-cases-expr (cexpr)
  (let ((expr (term-arg0 cexpr))
	(selections (term-arg1 cexpr))
	(else (term-arg2 cexpr)))
    (make-instance 'cases-expr
      'expression (xt-expr expr)
      'selections (xt-selections selections)
      'else-part (unless (is-sop 'expr-null-1 else)
		   (xt-expr else))
      'place (term-place cexpr))))

(defun xt-selections (sels)
  (mapcar #'xt-selection (term-args sels)))

(defun xt-selection (sel)
  (let* ((selector (term-arg0 sel))
	 (args (term-arg1 sel))
	 (expr (term-arg2 sel))
	 (constr (mk-name-expr (xt-idop selector))))
    (setf (place constr) (term-place selector))
    (make-instance 'selection
      'constructor constr
      'args (unless (is-sop 'selection-null-1 args)
	      (mapcar #'(lambda (a) (make-instance 'bind-decl
				      'id (xt-idop a)
				      'place (term-place a)))
		      (term-args args)))
      'expression (xt-expr expr)
      'place (term-place sel))))

(defun xt-cond-expr (expr)
  (let* ((cases (term-args (term-arg0 expr)))
	 (else (unless (is-sop 'noelse (term-arg1 expr))
		 (xt-expr (term-arg1 expr))))
	 (cond-expr (xt-cond-expr* cases else nil)))
    (if (typep cond-expr 'last-cond-expr)
	(change-class cond-expr 'single-cond-expr)
	(change-class cond-expr 'first-cond-expr))
    (setf (place cond-expr) (term-place expr))
    cond-expr))

(defun xt-cond-expr* (cases else conditions)
  (if cases
      (let* ((condition (xt-expr (term-arg0 (car cases))))
	     (then-part (xt-expr (term-arg1 (car cases))))
	     (else-part (xt-cond-expr* (cdr cases) else
				       (cons condition conditions)))
	     (ifname (mk-name-expr 'if)))
	(setf (place ifname) (term-place (car cases)))
	(if else-part
	    (make-instance 'cond-expr
	      'operator ifname
	      'argument (make-instance 'arg-tuple-expr
			  'exprs (list condition
				       then-part
				       else-part)
			  'place (place condition))
	      'place (term-place (car cases)))
	    (make-instance 'last-cond-expr
	      'operator ifname
	      'argument (make-instance 'arg-tuple-expr
			  'exprs (list condition
				       then-part
				       then-part)
			  'place (place condition))
	      'place (term-place (car cases)))))
      (when else
	(make-instance 'last-cond-expr
	  'operator (mk-name-expr 'if)
	  'argument (make-instance 'arg-tuple-expr
		      'exprs (list (mk-else-condition nil conditions)
				   else
				   else)
		      'place (place else))))))

(defun xt-table-expr (expr)
  (let ((row-expr (unless (is-sop 'norowvar (term-arg0 expr))
		    (xt-expr (term-arg0 expr))))
	(col-expr (unless (is-sop 'nocolvar (term-arg1 expr))
		    (xt-expr (term-arg1 expr))))
	(col-headings (unless (is-sop 'nocolheading (term-arg2 expr))
		       (xt-col-headings (term-arg2 expr)))))
    (multiple-value-bind (row-headings rows)
	(xt-table-entries* (term-args (term-arg3 expr)) col-headings)
      (unless (or (null col-expr)
		  col-headings)
	(parse-error (term-arg1 expr)
	  "Column headings must be given if the column expression is"))
      (unless (or (null row-expr)
		  row-headings)
	(parse-error (term-arg0 expr) "Row headings must be given"))
      (make-instance 'table-expr
	'row-expr row-expr
	'col-expr col-expr
	'row-headings row-headings
	'col-headings col-headings
	'table-entries rows
	'place (term-place expr)))))

(defun xt-col-headings (col-heading)
  (let ((expr (xt-expr (term-arg0 col-heading)))
	(exprs (mapcar #'xt-table-entry* (term-args (term-arg1 col-heading)))))
    (when (some #'(lambda (ch) (eq ch 'else)) (butlast exprs))
      (let ((term (find-if #'(lambda (te)
			       (is-sop 'else te))
		    (term-args (term-arg1 col-heading)))))
	(parse-error term "ELSE not allowed here")))
    (cons expr exprs)))

(defun xt-table-entries (table-entries)
  (xt-table-entries* (term-args table-entries) nil))

(defun xt-table-entries* (table-entries col-headings)
  (let* ((row-headings nil)
	 (row-entries (xt-row-entries table-entries))
	 (row-length (length (car row-entries)))
	 (col-heading-length (when col-headings (length col-headings))))
    ;; Can't have more than one row without having row-headings
    (when (and col-headings
	       (= col-heading-length row-length)
	       (cdr row-entries))
      (parse-error (car table-entries) "Row has the wrong length"))
    (unless (and col-headings
		 (= (length col-headings) (length (car row-entries))))
      (setq row-headings (mapcar #'car row-entries))
      (setq row-entries (mapcar #'cdr row-entries)))
    (when row-headings
      (xt-check-row-headings row-headings table-entries))
    (xt-check-table-entries row-entries table-entries)
    (values row-headings row-entries)))

(defun xt-check-row-headings (headings entries)
  (when (some #'null headings)
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'notableentry te))
		  entries)))
      (parse-error term "Empty entry not allowed in row headings")))
  (when (some #'(lambda (ch) (eq ch 'else)) (butlast headings))
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'else te))
		  entries)))
      (parse-error term "ELSE not allowed here"))))

(defun xt-check-table-entries (row-entries table-entries)
  (mapc #'xt-check-table-row row-entries table-entries))

(defun xt-check-table-row (row entries)
  (when (some #'(lambda (ch) (eq ch 'else)) row)
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'else te))
		  (term-args entries))))
      (parse-error term "ELSE not allowed here"))))
  
(defun xt-table-entry* (table-entry)
  (case (sim-term-op table-entry)
    (notableentry nil)
    (else 'else)
    (t (xt-expr table-entry))))

(defun xt-table-entry (table-entry)
  (mapcar #'xt-table-entry* (term-args table-entry)))

(defun xt-row-entries (row-entries &optional length rows)
  (if (null row-entries)
      (nreverse rows)
      (let* ((row-entry (mapcar #'xt-table-entry*
				(term-args (car row-entries))))
	     (row-length (length row-entry)))
	(unless (or (null length)
		    (= length row-length))
	  (parse-error (car row-entries)
	    "Length of this row is different from those above"))
	(xt-row-entries (cdr row-entries)
			row-length
			(cons row-entry rows)))))

(defun xt-assignment (ass)
  (let ((assign (term-arg0 ass))
	(expr (term-arg1 ass))
	(sep (ds-id (term-arg2 ass))))
    (when (cdr (term-args assign))
      (let ((bad-arg (find-if #'(lambda (x)
				  (memq (sim-term-op x)
					'(assign-id assign-num)))
		       (term-args assign))))
	(when bad-arg
	  (parse-error bad-arg "Parentheses expected here"))))
    (if (and (null (cdr (term-args assign)))
	     (memq (sim-term-op (term-arg0 assign)) '(assign-id assign-num)))
	(if (eq sep 'ceq)
	    (make-instance 'uni-assignment
	      'arguments (xt-assign assign)
	      'expression (xt-expr expr)
	      'place (term-place ass))
	    (make-instance 'uni-maplet
	      'arguments (xt-assign assign)
	      'expression (xt-expr expr)
	      'place (term-place ass)))
	(if (eq sep 'ceq)
	    (make-instance 'assignment
	      'arguments (xt-assign assign)
	      'expression (xt-expr expr)
	      'place (term-place ass))
	    (make-instance 'maplet
	      'arguments (xt-assign assign)
	      'expression (xt-expr expr)
	      'place (term-place ass))))))

(defun xt-assign (ass-args)
  (mapcar #'xt-assign* (term-args ass-args)))

(defun xt-assign* (ass-arg)
  (case (sim-term-op ass-arg)
    (field-assign (list (make-instance 'field-assign
			  'id (ds-id (term-arg0 ass-arg)))))
    (proj-assign (list (make-instance 'proj-assign
			 'number (ds-number (term-arg0 ass-arg)))))
    (assign-id (list (make-instance 'name-expr
		       'id (ds-id (term-arg0 ass-arg)))))
    (assign-num (list (make-instance 'number-expr
			'number (ds-number (term-arg0 ass-arg)))))
    (assign-tuple (mapcar #'xt-expr (term-args ass-arg)))))

(defun xt-assign-application (ass &optional args)
  (case (sim-term-op ass)
    (application (xt-assign-application
		  (term-arg0 ass)
		  (cons (mapcar #'xt-expr (term-args (term-arg1 ass)))
			args)))
    (tuple-expr (cons (mapcar #'xt-expr (term-args ass)) args))
    (t (parse-error ass "Parentheses expected here"))))

(defun xt-assign-args (ass)
  (mapcar #'(lambda (arg) (mapcar #'xt-expr (term-args arg)))
	  (term-args ass)))

(defun xt-assign-arg (assign-arg)
  (or (xt-assign-arg* assign-arg)
      (let ((expr (xt-expr assign-arg)))
	(setf (parens expr) 0)
	(if (typep expr 'tuple-expr)
	    (list (exprs expr))
	    (list (list expr))))))

(defun xt-assign-arg* (assign-arg &optional args)
  (cond ((is-id assign-arg)
	 (list (list (make-instance 'name-expr
		       'id (ds-id assign-arg)
		       'place (term-place assign-arg)))))
	((is-number assign-arg)
	 (list (list (make-instance 'number-expr
		       'number (ds-number assign-arg)
		       'place (term-place assign-arg)))))
	(t (case (sim-term-op assign-arg)
	     (field-assign
	      (list (list (make-instance 'field-assign
			    'id (ds-id (term-arg0 assign-arg))
			    'place (term-place (term-arg0 assign-arg))))))
	     (proj-assign
	      (list (list (make-instance 'proj-assign
			    'number (ds-number (term-arg0 assign-arg))
			    'place (term-place (term-arg0 assign-arg))))))
	     (tuple-expr
	      (cons (xt-expr assign-arg)
		    (mapcar #'(lambda (a)
				(cond ((is-id a)
				       (list
					(make-instance 'field-assign
					  'id (ds-id a)
					  'place (term-place a))))
				      ((is-number a)
				       (list
					(make-instance 'proj-assign
					  'number (ds-number a)
					  'place (term-place a))))
				      (t (mapcar #'xt-expr a))))
		      args)))
	     (application
	      (xt-assign-arg* (term-arg0 assign-arg)
			      (cons (term-args (term-arg1 expr)) args)))
	     (fieldappl
	      (xt-assign-arg* (term-arg0 assign-arg)
			      (cons (term-arg1 assign-arg) args)))
	     (projappl
	      (xt-assign-arg* (term-arg0 assign-arg)
			      (cons (term-arg1 assign-arg) args)))))))

(defmethod separator ((ass assignment))
  'ceq)

(defmethod separator ((ass maplet))
  'arr)


;;; Names

(defun xt-modname (modname)
  (let* ((id (term-arg0 modname))
	 (lib (term-arg1 modname))
	 (actuals (term-arg2 modname))
	 (mappings (term-arg3 modname)))
    (make-instance (if (is-sop 'noactuals actuals)
		       'modname 'full-modname)
      'id (ds-vid id)
      'library (unless (is-sop 'nolib lib)
		 (ds-vid lib))
      'actuals (unless (is-sop 'noactuals actuals)
		 (xt-actuals actuals))
      'mappings (unless (is-sop 'nomap mappings)
		  (xt-mappings mappings))
      'place (term-place modname))))

(defun xt-mappings (mappings)
  (break))

(defun xt-name (name)
  (let ((idop (term-arg0 name))
	(lib (term-arg1 name))
	(actuals (term-arg2 name))
	(idop2 (term-arg3 name)))
    (if (and (is-sop 'nomod idop2)
	     (is-sop 'nolib lib)
	     (is-sop 'noacts actuals)
	     (is-number (term-arg0 idop)))
	(make-instance 'number-expr
	  'number (ds-number (term-arg0 idop))
	  'place (term-place name))
	(make-instance 'name
	  'id (if (is-sop 'nomod idop2)
		  (xt-idop idop)
		  (xt-idop idop2))
	  'library (unless (is-sop 'nolib lib)
		     (ds-vid lib))
	  'actuals (unless (is-sop 'noacts actuals)
		     (xt-actuals actuals))
	  'mod-id (unless (is-sop 'nomod idop2)
		    (xt-idop idop))
	  'place (term-place name)))))

(defun xt-actuals (actuals)
  (mapcar #'xt-actual (term-args actuals)))

(defun xt-actual (act)
  (make-instance 'actual
    'expr (if (member (sim-term-op act)
		      '(subtype expr-as-type enum-or-subtype
			funtype ;; predtype
			recordtype))
	      (xt-not-enum-type-expr act)
	      (xt-expr act))
    'place (term-place act)))

(defun xt-idop (idop)
  (ds-vid (term-arg0 idop)))

(defun ds-vid (term)
  (let ((id (ds-id term)))
    (when (memq id '(|/\\| |\\/|))
      (pushnew id *escaped-operators-used*))
    (if (or (numberp id)
	    (valid-pvs-id id))
	id
	(parse-error term "Invalid id"))))

(defun valid-pvs-id (symbol)
  (or (not *valid-id-check*)
      *in-checker*
      sbrt::*sb-tex-mode*
      (or (assq symbol *pvs-operators*)
	  (valid-pvs-id* (string symbol)))))

(defmethod valid-pvs-id* ((sym symbol))
  (valid-pvs-id* (string sym)))
  
(defmethod valid-pvs-id* ((str string))
  (and (alpha-char-p (char str 0))
       (every #'(lambda (ch)
		  (or (alpha-char-p ch)
		      (digit-char-p ch)
		      (and *in-checker*
			   (char= ch #\!))
		      (member ch '(#\_ #\?) :test #'char=)))
	      (subseq str 1))))
