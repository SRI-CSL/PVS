;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unparse.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:43:38 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 18:44:52 1998
;; Update Count    : 35
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "PVS")

;;; Unparses the CLOS instances of the PVS language.  Must first
;;; translate from CLOS to the ERGO Abstract Syntax.  This is handled by
;;; functions with xf prefix (for translate-from).  This dual to this
;;; function is in parse.lisp

;;; Unparse calls the built-in prettyprinter (sbrt:unparse-term) for most
;;; classes; for declarations it first builds a theory-decl out of it.
;;; This is needed because of the separation of the declaration body from
;;; the identifiers, e.g. "a,b,c: VARIABLE int".  This creates three distinct
;;; declarations, which could be prettyprinted individually, or as a single
;;; line.

(defun unparse-all (mod &rest keys)
  (apply #'unparse-expanded mod keys))

(defvar *unparse-expanded* nil)

;;; Cache the expression precedence information.

(defparameter *expr-prec-info* (gethash 'expr pvs-prec-info)
  "The precedence information - a list of four hash tables, for initial,
left (medial), right (medial), and aggregate operators.  Each of these
hash tables gives a binding number for a given operator; higher numbers
bind tighter.")

(defun unparse-expanded (term &rest keys)
  (let ((*unparse-expanded* t))
    (apply #'unparse term keys)))

(defmethod unparse ((theories modules) &rest keys)
  (let ((*parsing-or-unparsing* t))
    (apply #'sbrt:unparse-term (xf theories) keys)))

(defmethod unparse ((theoryname string) &rest keys)
  (let ((theory (get-theory (ref-to-id theoryname))))
    (if theory
	(apply #'unparse theory keys)
	(error "Theory ~a does not exist" theoryname))))

(defmethod unparse ((term syntax) &rest keys)
  (let ((*parsing-or-unparsing* t))
    (apply #'sbrt:unparse-term (xf term) keys)))

(defmethod unparse ((term declaration) &rest keys)
  (let* ((*parsing-or-unparsing* t)
	 (uterm (cond ((and (formula-decl? term)
			    (eq (spelling term) 'ASSUMPTION))
		       (xf-assuming (list term)))
		      ((formal-decl? term)
		       (xf-theory-formal1 term))
		      (t (xf-theory (list term))))))
    (apply #'sbrt:unparse-term uterm keys)))

(defmethod unparse ((term module) &rest keys)
  (apply #'unparse (make-instance 'modules 'modules (list term)) keys))

(defmethod unparse ((term list) &rest keys)
  (if (every #'module? term)
      (apply #'unparse (make-instance 'modules 'modules term) keys)
      (error "Don't know how to unparse ~a" term)))

(defun unparse-decl (decl)
  (let ((ndecl (if (bind-decl? decl)
		   (mk-var-decl (id decl)
				(or (declared-type decl) (type decl)))
		   decl)))
    (string-trim '(#\Space #\Tab #\Newline)
		 (unparse ndecl
		   :string t
		   :char-width *default-char-width*))))

(defun unpindent (inst indent &key (width *default-char-width*) string comment?)
  (let* ((str (unparse inst
		:string t
		:char-width (- width indent (if comment? 2 0)))))
    (if string
	(with-output-to-string (*standard-output*)
	  (unpindent* str indent 0 (position #\linefeed str) comment? nil))
	(unpindent* str indent 0 (position #\linefeed str) comment? nil))))

(defun unpindent* (str indent start end comment? notfirst?)
  (format t "~v%~vT~:[~;% ~]~a"
    (if notfirst? 1 0)
    (if notfirst? indent 0)
    (and comment? notfirst?)
    (subseq str start end))
  (if end
      (unpindent* str indent (1+ end)
		  (position #\linefeed str :start (1+ end))
		  comment? t)))


;;; The xf (translate-from) method translates from CLOS to the ERGO
;;; Abstract Syntax.

(defmethod xf :around ((syn syntax))
  (let ((term (call-next-method)))
    (when (and (newline-comment syn)
	       (module? syn))
      (let ((cmt (getf (term:term-attr term) :comment)))
	(if cmt
	    (unless (member (newline-comment syn) cmt :test #'string=)
	      (nconc cmt (newline-comment syn)))
	    (setf (getf (term:term-attr term) :comment)
		  (newline-comment syn)))))
    term))


(defmethod xf ((mods modules))
  (mk-ergo-term 'adt-or-modules
    (mapcar #'xf (modules mods))))

(defmethod xf ((adt datatype))
  (mk-ergo-term* 'adt-or-module
    (mk-mod-id (id adt))
    (xf-theory-formals (formals adt))
    (mk-ergo-term* 'datatype
      (if (remove-some-decls (assuming adt))
	  (xf-assuming-part (assuming adt))
	  (mk-ergo-term* 'datatype-null-2))
      (if (importings adt)
	  (xf (importings adt))
	  (mk-ergo-term* 'datatype-null-1))
      (mk-ergo-term 'adtcases
	(mapcar #'xf (constructors adt)))
      (mk-mod-id (id adt)))))

(defmethod xf ((adt inline-datatype))
  (mk-ergo-term* 'datatype
    (if (remove-some-decls (assuming adt))
	(xf-assuming-part (assuming adt))
	(mk-ergo-term* 'datatype-null-2))
    (if (importings adt)
	(xf (importings adt))
	(mk-ergo-term* 'datatype-null-1))
    (mk-ergo-term 'adtcases
      (mapcar #'xf (constructors adt)))
    (mk-mod-id (id adt))))

(defmethod xf ((constr simple-constructor))
  (mk-ergo-term* 'adtcase
    (mk-ergo-term* 'constructor
      (xf-idop (id constr))
      (if (arguments constr)
	  (mk-ergo-term 'adtdecls
	    (mapcar #'xf-adtdecl (xf-chained-decls (arguments constr))))
	  (mk-ergo-term* 'constructor-null-1)))
    (xf-idop (recognizer constr))))

(defun xf-adtdecl (decls)
  (mk-ergo-term* 'adtdecl
    (xf-idops (mapcar #'id decls))
    (xf (declared-type (car decls)))))

(defmethod xf ((list list))
  (if (every #'(lambda (e) (typep e 'declaration)) list)
      (xf-theory list)))

(defmethod xf ((mod module))
  (mk-ergo-term* 'adt-or-module
    (mk-mod-id (id mod))
    (xf-theory-formals (formals mod))
    (mk-ergo-term* 'module
      (if (or (null (exporting mod))
	      (eq (kind (exporting mod)) 'DEFAULT))
	  (mk-ergo-term* 'noexp)
	  (xf (exporting mod)))
      (if (remove-some-decls (assuming mod))
	  (xf-assuming-part (remove-some-decls (assuming mod)))
	  (mk-ergo-term* 'noass))
      (if (theory mod)
	  (xf-theory-part (remove-some-decls (theory mod)))
	  (mk-ergo-term* 'notheory))
      (mk-mod-id (id mod)))))

(defun remove-some-decls (decls)
  (remove-if #'(lambda (d)
		 (and (not *unparse-expanded*)
		      (generated-by d)))
    decls))

(defun xf-theory-formals (formals)
  (if formals
      (mk-ergo-term 'theory-formals
	(xf-theory-formal (xf-chained-decls formals)))
      (mk-ergo-term* 'no-theory-formals)))

(defun xf-theory-formal (formals &optional result)
  (if (null formals)
      (nreverse result)
      (if (typep (car formals) 'importing)
	  (xf-theory-formal (cddr formals)
		      (cons (mk-ergo-term* 'theory-formal
			      (xf (car formals))
			      (xf-idops (mapcar #'id (cadr formals)))
			      (xf (caadr formals)))
			    result))
	  (xf-theory-formal (cdr formals)
		      (cons (mk-ergo-term* 'theory-formal
			      (mk-ergo-term* 'theory-formal-null-1)
			      (xf-idops (mapcar #'id (car formals)))
			      (xf (caar formals)))
			    result)))))

(defun xf-theory-formal1 (formal)
  (mk-ergo-term* 'theory-formal
    (mk-ergo-term* 'theory-formal-null-1)
    (xf-idops (list (id formal)))
    (xf formal)))

;(defun xf-modbody (mod)
;  (mk-ergo-term* 'modbody
;    (if (null (assuming mod))
;	(mk-ergo-term* 'modbody-null-1)
;	(xf-assuming-part (remove-if #'field-decl? (assuming mod))))
;    (xf-theory-part (remove-if #'field-decl? (theory mod)))))

(defun xf-assuming-part (assumings)
  (mk-ergo-term 'assuming-part
    (mapcar #'xf-assuming
	    (xf-chained-decls assumings))))

(defun xf-assuming (assuming)
  (if (typep assuming 'importing)
      (xf-using-elt assuming)
      (let ((idops (xf-idops (mapcar #'id assuming)))
	    (cmt (append (newline-comment (car assuming))
			 (when (and *comment-on-proof-status*
				    (formula-decl? (car assuming)))
			   (list (format nil "  % ~a"
				   (proof-status-string
				    (car assuming))))))))
	(when cmt
	  (setf (getf (term:term-attr idops) :comment)
		(mapcar #'(lambda (c) (list c -1 t)) cmt)))
	(mk-ergo-term* 'assuming
	  idops
	  (if (formals (car assuming))
	      (if (datatype? (car assuming))
		  (xf-theory-formals (formals (car assuming)))
		  (xf-pdformals (formals (car assuming))))
	      (mk-ergo-term* 'pdf))
	  (let ((decl (xf (car assuming))))
	    decl)
	  (mk-ergo-term* (if (semi (car assuming)) 'semic 'nosemic))))))

(defun xf-pdformals (formals)
  (mk-ergo-term 'pdf
    (mapcar #'(lambda (pdf)
		(xf-adformals pdf))
	    formals)))


(defun xf-adformals (formals)
  (mk-ergo-term 'adformals
    (mapcar #'xf-adformal
	    (xf-chained-decls formals))))

(defun xf-adformal (formal)
  (if (arg-bind-decl? (car formal))
      (xf (car formal))
      (xf-typed-ids formal)))


(defun xf-theory-part (theory)
  (mk-ergo-term 'theory-part
    (mapcar #'xf-theory
	    (xf-chained-decls
	     (presort-theory-part theory nil nil)))))

(defun presort-theory-part (decls chain sdecls)
  (if (null decls)
      (nreverse (nconc chain sdecls))
      (let* ((chain? (and (slot-exists-p (car decls) 'chain?)
			 (chain? (car decls))))
	     (tcc? (tcc? (car decls)))
	     (nchain (cond (chain? (cons (car decls) chain))
			   (tcc? chain)
			   (t nil)))
	     (nsdecls (cond (chain? sdecls)
			    (tcc? (cons (car decls) sdecls))
			    (t (cons (car decls) (nconc chain sdecls))))))
	(presort-theory-part (cdr decls) nchain nsdecls))))
			       

(defun xf-theory (theory)
  (cond ((typep theory 'importing)
	 (xf-using-elt theory))
	((typep (car theory) 'judgement)
	 (xf-judgement theory))
	((typep (car theory) 'conversion-decl)
	 (xf-conversion theory))
	(t (let ((idops (xf-idops (mapcar #'id theory)))
		 (cmt (append (newline-comment (car theory))
			      (when (and *comment-on-proof-status*
					 (formula-decl? (car theory)))
				(list (format nil "  % ~a"
					(proof-status-string
					 (car theory))))))))
	     (when cmt
	       (setf (getf (term:term-attr idops) :comment)
		     (mapcar #'(lambda (c) (list c -1 t)) cmt)))
	     (mk-ergo-term* 'theory
	       idops
	       (if (formals (car theory))
		   (if (datatype? (car theory))
		       (xf-theory-formals (formals (car theory)))
		       (xf-pdformals (formals (car theory))))
		   (mk-ergo-term* 'noformals))
	       (let ((decl (xf (car theory))))
		 (if (is-sop 'theory decl)
		     (term-arg2 decl)
		     decl))
	       (mk-ergo-term* (if (semi (car theory)) 'semic 'nosemic)))))))


;;; Given a flat list of usings and decls, returns a list of the form
;;;   ((decl1 ... decln) using1 ...), where the declarations are
;;; partitioned into chained declarations, and the usings are left
;;; separate.

(defun xf-chained-decls (decls &optional cdecls part)
  (if (null decls)
      (nreverse (if cdecls (cons (nreverse cdecls) part) part))
      (let ((chainp (and (typep (car decls) '(and (not arg-bind-decl)
					      (or declaration bind-decl)))
			 (chain? (car decls))
			 (or (not (typep (car decls)
					 '(or typed-declaration bind-decl)))
			     (declared-type (car decls))))))
	(xf-chained-decls
	 (cdr decls)
	 (when chainp
	   (cons (car decls) cdecls))
	 (cond (chainp
		part)
	       ((typep (car decls) 'importing)
		(cons (car decls)
		      (if cdecls (cons (nreverse cdecls) part) part)))
	       ((and cdecls
		     (or (and (typep (car cdecls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car cdecls))
			      (typep (car decls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car decls))
			      (ps-eq (declared-type (car cdecls))
				     (declared-type (car decls))))
			 (and (typep (car cdecls) 'type-decl)
			      (typep (car decls) 'type-decl))))
		(cons (nreverse (cons (car decls) cdecls)) part))
	       (t (cons (list (car decls))
			(if cdecls (cons (nreverse cdecls) part) part))))))))

(defun xf-using-elt (using)
  (mk-ergo-term* 'using-elt
    (xf using)
    (mk-ergo-term* (if (semi using) 'semic 'nosemic))))

(defmethod xf ((using importing))
  (mk-ergo-term 'using
    (mapcar #'xf (modules using))))

(defun xf-judgement (decls)
  (mk-ergo-term* 'judgement-elt
    (mk-ergo-term* 'judgement
      (mk-ergo-term 'namenums
	(mapcar #'(lambda (j)
		    (typecase j
		      (number-judgement
		       (mk-number (number (number j))))
		      (typed-judgement
		       (mk-ergo-term* 'ename
			 (xf-name (name j))
			 (xf (declared-name-type j))))
		      (subtype-judgement
		       (if (typep (declared-subtype j) 'type-name)
			   (mk-ergo-term* 'ename
			     (xf-name (declared-subtype j))
			     (mk-ergo-term* 'notype))
			   (xf (declared-subtype j))))
		      (t
		       (mk-ergo-term* 'ename
			 (xf-name (name j))
			 (mk-ergo-term* 'notype)))))
	  decls))
      (mk-ergo-term* (if (typep (car decls) 'subtype-judgement)
			 'subtype-of 'has-type))
      (xf (declared-type (car decls))))
    (mk-ergo-term* (if (semi (car decls)) 'semic 'nosemic))))

(defun xf-conversion (decls)
  (mk-ergo-term* 'conversion-elt
    (mk-ergo-term 'conversion
      (mapcar #'(lambda (c)
		  (typecase c
		    (typed-conversion-decl
		     (mk-ergo-term* 'ename
		       (xf-name (name c))
		       (xf (declared-type c))))
		    (t
		     (mk-ergo-term* 'ename
		       (xf-name (name c))
		       (mk-ergo-term* 'notype)))))
	decls))
    (mk-ergo-term* (if (semi (car decls)) 'semic 'nosemic))))


(defmethod xf ((exp exporting))
  (mk-ergo-term* 'exporting
    (cond ((eq (names exp) 'all)
	   (mk-ergo-term* 'exporting-all
	     (if (but-names exp)
		 (mk-ergo-term 'expbuts
		   (xf-exporting-list (but-names exp)))
		 (mk-ergo-term* 'noexpbuts))))
	  (t (mk-ergo-term 'expnames
	       (xf-exporting-list (names exp)))))
    (if (and (null (kind exp))
	     (null (modules exp)))
	(mk-ergo-term* 'noexportingmods)
	(case (kind exp)
	  (all (mk-ergo-term* 'expmodall))
	  (closure (mk-ergo-term* 'expmodclosure))
	  (t (mk-ergo-term 'modnames
	       (mapcar #'xf (modules exp))))))))

(defun xf-exporting-list (names)
  (mapcar #'xf names))

(defmethod xf ((n expname))
  (mk-ergo-term* 'expname
    (mk-ergo-term 'idop (list (mk-id (id n))))
    (case (kind n)
      ((nil) (mk-ergo-term* 'noexpkind))
      (type (mk-ergo-term* 'exptype))
      (formula (mk-ergo-term* 'expformula))
      (t (xf (kind n))))))

(defmethod xf ((decl typed-declaration))
  (mk-ergo-term* #-akcl (type-of decl) #+akcl (class-name (class-of decl))
    (xf (declared-type decl))))

(defmethod xf ((decl formal-type-decl))
  (mk-ergo-term* 'ftype-decl
    (mk-ergo-term* 'theory-formal-decl-null-1)))

(defmethod xf ((decl formal-nonempty-type-decl))
  (mk-ergo-term* 'fnetype-decl
    (mk-ergo-term* (keyword decl))
    (mk-ergo-term* 'theory-formal-decl-null-2)))

(defmethod xf ((decl formal-subtype-decl))
  (mk-ergo-term* 'ftype-decl
    (xf (type-expr decl))))

(defmethod xf ((decl formal-nonempty-subtype-decl))
  (mk-ergo-term* 'fnetype-decl
    (mk-ergo-term* (keyword decl))
    (xf (type-expr decl))))

(defmethod xf ((decl formal-const-decl))
  (mk-ergo-term* 'fconst-decl
    (xf (declared-type decl))))

(defmethod xf ((decl lib-decl))
  (mk-ergo-term* 'lib-decl
    (mk-ergo-term* 'noeq)
    (mk-string (lib-string decl))))

(defmethod xf ((decl lib-eq-decl))
  (mk-ergo-term* 'lib-decl
    (mk-ergo-term* 'eq)
    (mk-string (lib-string decl))))

(defmethod xf ((decl mod-decl))
  (mk-ergo-term* 'mod-decl
    (xf (modname decl))))

(defmethod xf ((decl type-decl))
  (mk-ergo-term* 'uninterp-type-decl))

(defmethod xf ((decl nonempty-type-decl))
  (mk-ergo-term* 'netype-decl
    (mk-ergo-term* (keyword decl))
    (mk-ergo-term* 'no-type-def)))

(defmethod xf ((decl type-def-decl))
  (mk-ergo-term* 'type-decl
    (mk-ergo-term* 'type-def
      (mk-ergo-term* (if (typep decl 'type-from-decl) 'from 'equal))
      (xf (type-expr decl)))))

(defmethod xf ((decl nonempty-type-eq-decl))
  (mk-ergo-term* 'netype-decl
    (mk-ergo-term* (keyword decl))
    (mk-ergo-term* 'type-def
      (mk-ergo-term* 'equal)
      (xf (type-expr decl)))))

(defmethod xf ((decl nonempty-type-from-decl))
  (mk-ergo-term* 'netype-decl
    (mk-ergo-term* (keyword decl))
    (mk-ergo-term* 'type-def
      (mk-ergo-term* 'from)
      (xf (type-expr decl)))))

(defmethod xf ((decl const-decl))
  (if (definition decl)
      (mk-ergo-term* 'const-decl
	(xf (declared-type decl))
	(xf (definition decl)))
      (mk-ergo-term* 'uninterp-const-decl
	(xf (declared-type decl)))))

(defmethod xf ((decl def-decl))
  (mk-ergo-term* 'def-decl
    (xf (declared-type decl))
    (xf (definition decl))
    (xf (declared-measure decl))
    (if (ordering decl)
	(xf (ordering decl))
	(mk-ergo-term* 'noexpr))))

(defmethod xf ((decl inductive-decl))
  (mk-ergo-term* 'ind-decl
    (xf (declared-type decl))
    (xf (definition decl))))

(defmethod xf ((decl adt-def-decl))
  (xf (change-class (copy decl) 'const-decl)))

(defmethod xf ((decl formula-decl))
  (if (eq (spelling decl) 'assumption)
      (mk-ergo-term* 'assumption
	(xf (definition decl)))
      (mk-ergo-term* 'formula-decl
	(mk-ergo-term* (spelling decl))
	(xf (definition decl)))))


;;; Type Expressions

(defmethod xf :around ((te type-expr))
  (if (print-type te)
      (xf (print-type te))
      (xf-tupletype (call-next-method) (parens te))))

(defun xf-tupletype (texpr parens)
  (assert (and (integerp parens) (not (minusp parens))))
  (if (zerop parens)
      texpr
      (xf-tupletype (mk-ergo-term* 'funtype
		      (mk-ergo-term* 'comp-type-expr-null-1)
		      (mk-ergo-term 'dom (list texpr))
		      (mk-ergo-term* 'comp-type-expr-null-2))
		    (1- parens))))

(defmethod xf ((te type-name))
  (mk-ergo-term* 'type-name
    (call-next-method)))

(defmethod xf ((te type-application))
  (mk-ergo-term* 'type-appl
    (xf-name (type te))
    (mk-ergo-term 'arguments
      (mapcar #'xf (parameters te)))))

(defmethod xf ((te subtype))
  (let ((set-expr (xf (if (typep (predicate te) 'binding-expr)
			  (change-class (copy (predicate te)) 'set-expr)
			  (let* ((id (make-new-variable '|x| te))
				 (bd (mk-bind-decl id
				       (or (print-type (supertype te))
					   (supertype te))))
				 (var (mk-name-expr id)))
			    (make-instance 'set-expr
			      'bindings (list bd)
			      'expression (mk-application (predicate te)
					    var)))))))
    (mk-ergo-term* 'subtype
      (term-arg0 set-expr)
      (term-arg1 set-expr)
      (if (contains te)
	  (xf (contains te))
	  (mk-ergo-term* 'nocontaining)))))

(defmethod xf ((te expr-as-type))
  (mk-ergo-term* 'expr-as-type
    (xf (expr te))
    (if (contains te)
	(xf (contains te))
	(mk-ergo-term* 'nocontains))))

(defmethod xf ((te enumtype))
  (mk-ergo-term* 'enumtype
    (xf-enumtype-args (constructors te))
    (mk-ergo-term* 'nocontaining)))

(defun xf-enumtype-args (constructors)
  (if (cdr constructors)
      (mk-ergo-term* 'set-formals
	(xf-enumtype-arg (id (car constructors)))
	(mk-ergo-term* 'comma)
	(xf-enumtype-args (cdr constructors)))
      (xf-enumtype-arg (id (car constructors)))))

(defun xf-enumtype-arg (id)
  (mk-ergo-term* 'set-id
    (xf-idop id)
    (mk-ergo-term* 'notype)))

(defmethod xf ((te recordtype))
  (mk-ergo-term* 'recordtype
    (mk-ergo-term 'fields
      (xf-fields (fields te)))))

(defun xf-fields (fields)
  (mapcar #'xf-field
	  (xf-chained-decls fields)))

(defun xf-field (fields)
  (mk-ergo-term* 'field-decls
    (mk-ergo-term 'ids (mapcar #'mk-id (mapcar #'id fields)))
    (xf (declared-type (car fields)))))

(defmethod xf ((te funtype))
  (multiple-value-bind (domain range)
      (xf-funtype (domain te) (range te))
    (mk-ergo-term* 'funtype
      (mk-ergo-term* (typecase te
		      (functiontype 'function)
		      (arraytype 'array)
		      (funtype 'comp-type-expr-null-1)))
      (mk-ergo-term 'dom domain)
      range)))

(defmethod xf-funtype (domain range)
  (values (list (xf domain)) (xf range)))

(defmethod xf-funtype ((domain domain-tupletype) range)
  (values (mapcar #'xf (types domain)) (xf range)))

(defmethod xf-funtype ((domain dep-binding) range)
  (if (typep (declared-type domain) 'dep-domain-tupletype)
      (let ((types (types (declared-type domain)))
	    (bindings (var-bindings (declared-type domain)))
	    (ctr 0)
	    (*parsing-or-unparsing* t))
	(values (mapcar #'(lambda (dt)
			    (incf ctr)
			    (if (typep dt 'dep-binding)
				(xf dt)
				(let ((var (car (rassoc ctr bindings))))
				  (if var
				      (xf (make-instance 'dep-binding
					    'id var
					    'declared-type dt))
				      (xf dt)))))
			types)
		(xf (gensubst range
		      #'(lambda (ex)
			  (let ((var (car (rassoc (index ex) bindings))))
			    (make-instance 'name-expr
			      'id var)))
		      #'(lambda (ex)
			  (and (typep ex 'projection-application)
			       (typep (argument ex) 'name-expr)
			       (eq (id (argument ex)) (id domain))))))))
      (values (list (xf domain)) (xf range))))

(defmethod xf ((te tupletype))
  (mk-ergo-term* 'funtype
    (mk-ergo-term* 'comp-type-expr-null-1)
    (mk-ergo-term 'dom
      (mapcar #'xf (types te)))
    (mk-ergo-term* 'comp-type-expr-null-2)))

(defmethod xf ((dep dep-binding))
  (xf-tupletype
   (mk-ergo-term* 'dep-binding
     (xf-idop (id dep))
     (xf (declared-type dep)))
   (parens dep)))

;(defmethod xf ((te predtype))
;  (mk-ergo-term* 'predtype
;    (mk-ergo-term* (if (typep te 'settype) 'setof 'pred))
;    (mk-ergo-term 'dom
;      (mapcar #'xf (domain te)))))


;;; Expressions

(defmethod xf :around ((expr expr))
  (if (typep expr 'binding)
      (call-next-method)
      (xf-tuple-expr (call-next-method) (parens expr))))

(defun xf-tuple-expr (expr parens)
  (assert (and (integerp parens) (>= parens 0)))
  (if (zerop parens)
      expr
      (xf-tuple-expr (mk-ergo-term* 'tuple-expr
		       expr)
		     (1- parens))))

(defmethod xf ((expr number-expr))
  (mk-ergo-term* 'number-expr
    (mk-number (number expr))))

(defmethod xf ((expr string-expr))
  (unless (string-value expr)
    (setf (string-value expr) (xf-string-expr (argument expr))))
  (mk-ergo-term* 'string-expr
    (mk-string (string-value expr))))

(defun xf-string-expr (charlist &optional list)
  (if (typep charlist 'name-expr)
      (coerce (nreverse list) 'string)
      (xf-string-expr (args2 charlist)
		      (nconc (xf-string-char (number (args1 (args1 charlist))))
			     list))))

(defun xf-string-char (code)
  (let ((char (code-char code)))
    (case char
      (#-gcl #\Bell #+gcl #\^G (list #\a #\\ #\\))
      (#\Backspace (list #\b #\\ #\\))
      (#\Page (list #\f #\\ #\\))
      (#\Newline (list #\n #\\ #\\))
      (#\Return (list #\r #\\ #\\))
      (#\Tab (list #\t #\\ #\\))
      (#-gcl #\VT #+gcl #\^K (list #\v #\\ #\\))
      (#\" (list #\" #\\ #\\))
      (#\\ (list #\\ #\\ #\\ #\\))
      (t (if (graphic-char-p char)
	     (list char)
	     (nreverse (cons #\\ (cons #\\ (coerce (format nil "~o" code)
						   'list)))))))))

(defmethod xf ((expr list-expr))
  (mk-ergo-term 'list-expr
    (xf-list-expr expr)))

(defmethod xf ((expr null-expr))
  (mk-ergo-term* 'list-expr))

(defmethod xf-list-expr ((expr list-expr))
  (cons (xf (args1 expr)) (xf-list-expr (args2 expr))))

(defmethod xf-list-expr ((expr name-expr))
  nil)

(defmethod xf ((expr bracket-expr))
  (mk-ergo-term 'brack-expr
    (mapcar #'xf (arguments expr))))

(defmethod xf ((expr name-expr))
  (mk-ergo-term* 'name-expr
    (call-next-method)))

;(defmethod xf ((expr function-expr))
;  (mk-ergo-term 'tuple-expr
;    (mapcar #'xf (assignments expr))))

(defmethod xf ((expr record-expr))
  (mk-ergo-term* 'rec-expr
    (mk-ergo-term 'assignments
      (mapcar #'xf (assignments expr)))))

(defmethod xf ((expr tuple-expr))
  (mk-ergo-term 'tuple-expr
    (mapcar #'xf (exprs expr))))

(defmethod xf ((expr projection-application))
  (mk-ergo-term* 'application
    (mk-ergo-term* 'name-expr
      (mk-ergo-term* 'name
	(mk-ergo-term* 'idop
	  (mk-id (id expr)))
	(mk-ergo-term* 'nolib)
	(mk-ergo-term* 'noacts)
	(mk-ergo-term* 'nomod)))
    (mk-ergo-term 'fun-arguments
      (mapcar #'xf (argument-list (argument expr))))))

(defmethod xf ((expr field-application))
  (mk-ergo-term* 'application
    (mk-ergo-term* 'name-expr
      (mk-ergo-term* 'name
	(mk-ergo-term* 'idop
	  (mk-id (id expr)))
	(mk-ergo-term* 'nolib)
	(mk-ergo-term* 'noacts)
	(mk-ergo-term* 'nomod)))
    (mk-ergo-term 'fun-arguments
      (mapcar #'xf (argument-list (argument expr))))))

(defmethod xf ((expr application))
  (mk-ergo-term* 'application
    (let ((oper (xf (operator expr))))
      (if (< (precedence (operator expr) 'left)
	     (gethash 'jux (second *expr-prec-info*)))
	  (xf-tuple-expr oper 1)
	  oper))
    (mk-ergo-term 'fun-arguments
      (mapcar #'xf (arguments expr)))))

(defmethod xf ((expr binding-application))
  (let ((lambda-term (xf (lcopy (args1 expr) 'parens 0))))
    (if (is-sop 'bind-expr lambda-term)
	(mk-ergo-term* 'name-bind-expr
	  (mk-id (id (operator expr)))
	  (term-arg1 lambda-term))
	(call-next-method))))

(defmethod xf ((expr infix-application))
  (if (and (typep (operator expr) 'name-expr)
	   (not (mod-id (operator expr)))
	   (member (id (operator expr)) *infix-operators*))
      (let* ((cargs (arguments expr))
	     (args (mapcar #'xf cargs))
	     (oper (sbst-symbol (id (operator expr))))
	     (oper-left-binding (gethash oper (second *expr-prec-info*)))
	     (oper-right-binding (gethash oper (third *expr-prec-info*))))
	(unless (= (length args) 2)
	  (let ((*debugging-print-object* t))
	    (break "Infix application with other than two args")))
	(mk-ergo-term* 'term-expr
	  (mk-ergo-term* (id (operator expr)))
	  (mk-ergo-term 'expr-args
	    (list
	     (if (< (precedence (first cargs) 'left) oper-left-binding)
		 (xf-tuple-expr (first args) 1)
		 (first args))
	     (if (< (precedence (second cargs) 'right) oper-right-binding)
		 (xf-tuple-expr (second args) 1)
		 (second args))))))
      (call-next-method)))

(defmethod xf ((expr unary-application))
  (if (and (typep (operator expr) 'name-expr)
	   (not (mod-id (operator expr)))
	   (member (id (operator expr)) *unary-operators*))
      (mk-ergo-term* 'unary-term-expr
	(mk-ergo-term* (id (operator expr)))
	(let ((arg (xf (car (arguments expr)))))
	  (if (< (precedence (argument expr) 'right)
		 (gethash (sbst-symbol (id (operator expr)))
			  (first *expr-prec-info*)))
	      (xf-tuple-expr arg 1)
	      arg)))
      (call-next-method)))

(defmethod xf ((expr when-expr))
  (let* ((cargs (reverse (arguments expr)))
	 (args (mapcar #'xf cargs))
	 (oper (sbst-symbol 'WHEN))
	 (oper-left-binding (gethash oper (second *expr-prec-info*)))
	 (oper-right-binding (gethash oper (third *expr-prec-info*))))
    (mk-ergo-term* 'term-expr
      (mk-ergo-term* 'WHEN)
      (mk-ergo-term 'expr-args
	(list
	 (if (< (precedence (first cargs) 'left) oper-left-binding)
	     (xf-tuple-expr (first args) 1)
	     (first args))
	 (if (< (precedence (second cargs) 'right) oper-right-binding)
	     (xf-tuple-expr (second args) 1)
	     (second args)))))))

(defmethod xf ((expr implicit-conversion))
  (if *show-conversions*
      (call-next-method)
      (xf (car (arguments expr)))))

(defmethod xf ((expr if-expr))
  (mk-ergo-term* 'if-expr
    (xf (car (arguments expr)))
    (xf (cadr (arguments expr)))
    (mk-ergo-term 'elsifs
      (xf-elsif (caddr (arguments expr))))
    (xf-else (caddr (arguments expr)))))

(defmethod xf-elsif ((expr chained-if-expr) &optional result)
  (xf-elsif (else-part expr)
	    (cons (mk-ergo-term* 'elsif
		    (xf (car (arguments expr)))
		    (xf (cadr (arguments expr))))
		  result)))

(defmethod xf-elsif (expr &optional result)
  (declare (ignore expr))
  (nreverse result))

(defmethod xf-else ((expr chained-if-expr))
  (xf-else (caddr (arguments expr))))

(defmethod xf-else (expr)
  (xf expr))

(defmethod xf ((expr coercion))
  (mk-ergo-term* 'coercion
    (xf (argument expr))
    (mk-ergo-term* 'type-coercion
      (xf (declared-type (car (bindings (operator expr))))))))

;(defmethod xf ((expr intype))
;  (mk-ergo-term* 'intype
;    (xf (expression expr))
;    (xf (declared-type expr))))

(defmethod xf ((expr binding-expr))
  (xf-binding-expr expr
		   #-akcl (type-of expr)
		   #+akcl (class-name (class-of expr))))

(defun xf-binding-expr (expr class)
  (multiple-value-bind (formals exp)
      (xf-lambda-formals expr)
    (mk-ergo-term* 'bind-expr
      (mk-ergo-term* class)
      (mk-ergo-term* 'lambda-body
	formals
	(if (result-type expr)
	    (xf (result-type expr))
	    (mk-ergo-term* 'noreturntype))
	exp))))

(defun xf-lambda-formals (expr)
  (let* ((set-expr? (typep expr 'set-expr))
	 (dbindings (mapcar #'add-declared-type-if-needed (bindings expr)))
	 (bindings (xf-lambda-formal (xf-chained-decls dbindings)
				     (commas? expr)
				     set-expr?))
	 (expression (expression expr)))
    (if (and (typep expression 'lambda-expr)
	     (chain? expression))
	(multiple-value-bind (ebindings eexpr)
	    (xf-lambda-formals expression)
	  (values (mk-ergo-term* (if set-expr? 'set-formals 'lambda-formals)
		    bindings
		    (mk-ergo-term* 'nocomma)
		    ebindings)
		  eexpr))
	(values bindings (xf expression)))))

(defmethod add-declared-type-if-needed ((decl untyped-bind-decl))
  (let ((dtype (or (declared-type decl)
		   (and (type decl) (print-type (type decl)))
		   (type decl))))
    (if dtype
	(change-class (copy decl 'declared-type dtype) 'bind-decl)
	decl)))

(defmethod add-declared-type-if-needed (decl)
  decl)

(defun xf-lambda-formal (bindings commas? &optional set-expr?)
  (if commas?
      (if (cdr bindings)
	  (mk-ergo-term* (if set-expr? 'set-formals 'lambda-formals)
	    (if (or (cdr (car bindings))
		    (and (declared-type (caar bindings))
			 (not (typep (caar bindings) 'untyped-bind-decl))))
		(xf-lambda-adformals (car bindings))
		(if set-expr?
		    (mk-ergo-term* 'set-id
		      (xf-idop (id (caar bindings)))
		      (if (and (declared-type (caar bindings))
			       (not (typep (caar bindings) 'untyped-bind-decl)))
			  (xf (declared-type (caar bindings)))
			  (mk-ergo-term* 'notype)))
		    (xf-idop (id (caar bindings)))))
	    (mk-ergo-term* 'comma)
	    (xf-lambda-formal (cdr bindings) commas? set-expr?))
	  (if (and set-expr?
		   (zerop (parens (caar bindings))))
	      (if (cdar bindings)
		  (xf-lambda-formal (mapcar #'list (car bindings))
				    commas? set-expr?)
		  (mk-ergo-term* 'set-id
		    (xf-idop (id (caar bindings)))
		    (if (declared-type (caar bindings))
			(xf (declared-type (caar bindings)))
			(mk-ergo-term* 'notype))))
	      (if (or (cdr (car bindings))
		      (declared-type (caar bindings)))
		  (xf-lambda-adformals (car bindings))
		  (xf-idop (id (caar bindings))))))
      (if (and set-expr?
	       (zerop (parens (caar bindings))))
	  (mk-ergo-term* 'set-id
	    (xf-idop (id (caar bindings)))
	    (if (declared-type (caar bindings))
		(xf (declared-type (caar bindings)))
		(mk-ergo-term* 'notype)))
	  (mk-ergo-term 'adformals
	    (mapcan #'(lambda (b)
			(if (zerop (parens (car b)))
			    (xf-lambda-adformals* b nil)
			    (list (xf-typed-ids b))))
		    bindings)))))

(defun xf-lambda-adformals (bindings)
  (mk-ergo-term 'adformals
    (xf-lambda-adformals* bindings nil)))

(defun xf-lambda-adformals* (bindings result)
  (if (null bindings)
      (nreverse result)
      (let ((nb (if (or (cdr bindings)
			(not (chain? (car bindings)))
			(not (typep (car bindings) 'untyped-bind-decl)))
		    (xf (car bindings))
		    (xf (change-class (copy (car bindings))
					       'bind-decl)))))
	(xf-lambda-adformals* (cdr bindings) (cons nb result)))))

(defmethod xf ((expr lambda-conversion))
  (if *show-conversions*
      (xf (change-class (copy expr) 'lambda-expr))
      (xf (expression expr))))

(defmethod xf ((expr argument-conversion))
  (if *show-conversions*
      (xf (change-class (copy expr) 'application))
      (xf (operator expr))))

(defmethod xf ((expr set-expr))
  (multiple-value-bind (formals exp)
      (xf-lambda-formals expr)
    (mk-ergo-term* 'set-expr formals exp)))

(defun xf-dformals (formals)
  (mk-ergo-term 'dformals
    (mapcar #'xf-typed-ids
	    (xf-chained-decls formals))))

(defun xf-typed-ids (formals)
  (if (or (declared-type (car formals))
	  (cdr formals))
      (let ((ids (mapcar #'(lambda (a) (mk-ergo-term 'idop (list (mk-id a))))
			 (mapcar #'id formals)))
	    (type (if (declared-type (car formals))
		      (if (pred-bind-decl? (car formals))
			  (if (supertype (declared-type (car formals)))
			      (xf (supertype (declared-type (car formals))))
			      (mk-ergo-term* 'no-type-expr))
			  (xf (declared-type (car formals))))
		      (mk-ergo-term* 'no-type-expr)))
	    (pred (if (pred-bind-decl? (car formals))
		      (xf (formula (declared-type (car formals))))
		      (mk-ergo-term* 'no-pred))))
	(mk-ergo-term* 'typed-ids
	  (mk-ergo-term 'idops ids)
	  type
	  pred))
      (mk-ergo-term* 'typed-id
	(mk-ergo-term* 'idop (mk-id (id (car formals))))
	(mk-ergo-term* 'no-type-expr)
	(mk-ergo-term* 'no-pred))))

(defmethod xf ((formal untyped-bind-decl))
  (mk-ergo-term* 'typed-id
    (xf-idop (id formal))
    (mk-ergo-term* 'no-type-expr)
    (mk-ergo-term* 'no-pred)))

(defmethod xf ((formal bind-decl))
  (let* ((type (if (declared-type formal)
		   (if (pred-bind-decl? formal)
		       (if (supertype (declared-type formal))
			   (xf (supertype (declared-type formal)))
			   (mk-ergo-term* 'no-type-expr))
		       (xf (declared-type formal)))
		   (mk-ergo-term* 'no-type-expr)))
	 (pred (if (pred-bind-decl? formal)
		   (xf (formula (declared-type formal)))
		   (mk-ergo-term* 'no-pred))))
    (if (zerop (parens formal))
	(mk-ergo-term* 'typed-id
	  (mk-ergo-term* 'idop (mk-id (id formal)))
	  type
	  pred)
	(mk-ergo-term* 'typed-ids
	  (xf-idops (list (id formal)))
	  type
	  pred))))

(defmethod xf-typed-id ((formal bind-decl))
  (let ((type (if (declared-type formal)
		  (if (pred-bind-decl? formal)
		      (if (supertype (declared-type formal))
			  (xf (supertype (declared-type formal)))
			  (mk-ergo-term* 'no-type-expr))
		      (xf (declared-type formal)))
		  (mk-ergo-term* 'no-type-expr)))
	(pred (if (pred-bind-decl? formal)
		  (xf (formula (declared-type formal)))
		  (mk-ergo-term* 'no-pred))))
    (mk-ergo-term* 'typed-id
      (mk-ergo-term* 'idop (mk-id (id formal)))
      type
      pred)))


(defmethod formula ((te subtype))
  (expression (predicate te)))

(defun xf-dformal (formals)
  (if (declared-type (car formals))
      (mk-ergo-term* 'dformdecl
	(mk-ergo-term 'ids
	  (mapcar #'mk-id (mapcar #'id formals)))
	(xf (declared-type (car formals))))
      (mk-ergo-term* 'dformid (mk-id (id (car formals))))))

(defmethod xf ((expr let-expr))
  (mk-ergo-term* 'let-expr
    (xf-let-expr-bindings (expression (operator expr))
			  (bindings (operator expr))
			  (argument expr))
    (let ((exp (xf (xf-let-expr (expression (operator expr))))))
      (if (< (precedence (expression (operator expr)) 'right)
	     (gethash (sbst-symbol 'in)
		      (fourth *expr-prec-info*)))
	  (xf-tuple-expr exp 1)
	  exp))))

(defun xf-let-expr-bindings (expr bindings arg &optional tbindings)
  (let ((lbindings (xf-let-binding bindings arg)))
    (if (chained-let-expr? expr)
	(xf-let-expr-bindings (expression (operator expr))
			      (bindings (operator expr))
			      (argument expr)
			      (cons lbindings tbindings))
	(mk-ergo-term 'let-bindings
	  (nreverse (cons lbindings tbindings))))))

(defun xf-let-expr (expr)
  (if (chained-let-expr? expr)
      (xf-let-expr (expression (operator expr)))
      expr))

(defmethod xf ((expr where-expr))
  (mk-ergo-term* 'where-expr
    (xf-where-expr-bindings (expression (operator expr))
			    (bindings (operator expr))
			    (argument expr))
    (let ((exp (xf (xf-where-expr (expression (operator expr))))))
      (if (< (precedence (expression (operator expr)) 'right)
	     (gethash (sbst-symbol 'in)
		      (fourth *expr-prec-info*)))
	  (xf-tuple-expr exp 1)
	  exp))))

(defun xf-where-expr-bindings (expr bindings arg &optional tbindings)
  (let ((lbindings (xf-let-binding bindings arg)))
    (if (chained-where-expr? expr)
	(xf-where-expr-bindings (expression (operator expr))
				(bindings (operator expr))
				(argument expr)
				(cons lbindings tbindings))
	(mk-ergo-term 'let-bindings
	  (nreverse (cons lbindings tbindings))))))

(defun xf-where-expr (expr)
  (if (chained-where-expr? expr)
      (xf-where-expr (expression (operator expr)))
      expr))

(defun xf-let-binding (bindings arg)
  (mk-ergo-term* 'bind
    (xf-adformal bindings)
    (xf arg)))

;(defmethod xf ((expr override-expr))
;  (mk-ergo-term* 'override-expr
;    (xf (left expr))
;    (xf (right expr))))

(defmethod xf ((expr update-expr))
  (mk-ergo-term* 'update-expr
    (let ((exp (xf (expression expr))))
      (if (< (precedence (expression expr) 'left)
	     (gethash (sbst-symbol 'with)
		      (second *expr-prec-info*)))
	  (xf-tuple-expr exp 1)
	  exp))
    (mk-ergo-term 'assignments
      (mapcar #'xf (assignments expr)))))

(defmethod xf ((ass assignment))
  (mk-ergo-term* 'assignment
    (xf-assargs (reverse (arguments ass)))
    (xf (expression ass))
    (mk-ergo-term* (separator ass))))

(defmethod xf ((ass uni-assignment))
  (mk-ergo-term* 'assignment
    (if (typep (caar (arguments ass)) '(or name-expr number-expr))
	(xf (caar (arguments ass)))
        (xf-assargs (reverse (arguments ass))))
    (xf (expression ass))
    (mk-ergo-term* (separator ass))))

(defun xf-assargs (args)
  (if (cdr args)
      (mk-ergo-term* 'application
	(xf-assargs (cdr args))
	(mk-ergo-term 'fun-arguments (mapcar #'xf (car args))))
      (mk-ergo-term 'tuple-expr
	(mapcar #'xf (car args)))))

(defmethod xf ((expr cases-expr))
  (mk-ergo-term* 'cases-expr
    (xf (expression expr))
    (mk-ergo-term 'args
      (mapcar #'xf (selections expr)))
    (if (else-part expr)
	(xf (else-part expr))
	(mk-ergo-term* 'expr-null-1))))

(defmethod xf ((sel selection))
  (mk-ergo-term* 'selection
    (xf-idop (id (constructor sel)))
    (if (null (args sel))
	(mk-ergo-term* 'selection-null-1)
	(mk-ergo-term 'idops (mapcar #'(lambda (a)
					(xf-idop (id a)))
				    (args sel))))
    (xf (expression sel))))

(defmethod xf ((expr cond-expr))
  (mk-ergo-term 'cond-expr
    (xf-cond-expr expr nil)))

(defmethod xf ((expr first-cond-expr))
  (mk-ergo-term 'cond-expr
    (xf-cond-expr expr nil)))

(defmethod xf ((expr last-cond-expr))
  (mk-ergo-term 'cond-expr
    (xf-cond-expr expr nil)))

(defmethod xf-cond-expr ((expr cond-expr) cases)
  (with-slots (argument) expr
    (let* ((exprs (exprs argument))
	   (case (xf-cond-case (car exprs) (cadr exprs))))
      (xf-cond-expr (caddr exprs) (cons case cases)))))

(defmethod xf-cond-expr ((expr first-cond-expr) cases)
  (with-slots (argument) expr
    (let* ((exprs (exprs argument))
	   (case (xf-cond-case (car exprs) (cadr exprs))))
      (xf-cond-expr (caddr exprs) (cons case cases)))))

(defmethod xf-cond-expr ((expr last-cond-expr) cases)
  (with-slots (argument) expr
    (let ((exprs (exprs argument)))
      (if (typep (car exprs) 'else-condition)
	  (list (mk-ergo-term 'args
		  (nreverse cases))
		(xf (cadr exprs)))
	  (let ((case (xf-cond-case (car exprs) (cadr exprs))))
	    (list (mk-ergo-term 'args
		    (nreverse (cons case cases)))
		  (mk-ergo-term* 'noelse)))))))

(defmethod xf-cond-expr ((expr expr) cases)
  (list (mk-ergo-term 'args
	  (nreverse cases))
	(xf expr)))

(defun xf-cond-case (ex1 ex2)
  (mk-ergo-term* 'cond-case (xf ex1) (xf ex2)))

(defmethod xf ((expr table-expr))
  (xf-table-expr expr))

(defmethod xf ((expr cond-table-expr))
  (xf-table-expr expr))

(defmethod xf ((expr cases-table-expr))
  (xf-table-expr expr))

(defmethod xf ((expr let-table-expr))
  (xf-table-expr expr))

(defun xf-table-expr (expr)
  (mk-ergo-term* 'table-expr
    (if (row-expr expr)
	(xf (row-expr expr))
	(mk-ergo-term* 'norowvar))
    (if (col-expr expr)
	(xf (col-expr expr))
	(mk-ergo-term* 'nocolvar))
    (if (col-headings expr)
	(xf-col-headings (col-headings expr))
	(mk-ergo-term* 'nocolheading))
    (mk-ergo-term 'table-entries
      (xf-table-entries (row-headings expr)
			(table-entries expr)))))

(defun xf-col-headings (col-headings)
  (mk-ergo-term* 'col-heading
    (xf (car col-headings))
    (mk-ergo-term 'colrest
      (mapcar #'(lambda (ch)
		  (if (eq ch 'else)
		      (mk-ergo-term* 'else)
		      (xf ch)))
	      (cdr col-headings)))))

(defun xf-table-entries (row-headings table-entries)
  (let ((entries (if row-headings
		     (mapcar #'cons row-headings table-entries)
		     table-entries)))
    (mapcar #'xf-table-entry entries)))

(defun xf-table-entry (table-entry)
  (mk-ergo-term 'table-entry
    (mapcar #'(lambda (te)
		(cond ((null te)
		       (mk-ergo-term* 'notableentry))
		      ((eq te 'else)
		       (mk-ergo-term* 'else))
		      (t (xf te))))
	    table-entry)))
  

(defmethod xf ((mname modname))
  (mk-ergo-term* 'modname
    (mk-mod-id (id mname))
    (if (library mname)
	(mk-mod-id (library mname))
	(mk-ergo-term* 'nolib))
    (if (actuals mname)
	(mk-ergo-term 'actuals
	  (mapcar #'xf (actuals mname)))
	(mk-ergo-term* 'noactuals))
    (if (mappings mname)
	(break)
	(mk-ergo-term* 'nomap))))

(defmethod xf ((name name))
  (xf-name name))

(defun xf-name (name)
  (mk-ergo-term* 'name
    (mk-ergo-term* 'idop
      (let ((id (or (mod-id name) (id name))))
	(if (memq id '(true false))
	    (mk-sim-term* id)
	    (mk-id id))))
    (if (library name)
	(mk-mod-id (library name))
	(mk-ergo-term* 'nolib))
    (if (actuals name)
	(mk-ergo-term 'actuals
	  (mapcar #'xf (actuals name)))
	(if (and nil *unparse-expanded*
		 (resolution name)
		 (actuals (module-instance (resolution name)))
		 (not (some #'null (actuals (module-instance (resolution name))))))
	    (mk-ergo-term 'actuals
	      (mapcar #'xf (actuals (module-instance (resolution name)))))
	    (mk-ergo-term* 'noacts)))
    (if (mod-id name)
	(mk-ergo-term* 'idop
	  (mk-mod-id (id name)))
	(if (and nil *unparse-expanded*
		 (resolution name))
	    (mk-ergo-term* 'idop
	      (mk-id (id (module-instance (resolution name)))))
	    (mk-ergo-term* 'nomod)))))

(defmethod xf ((act actual))
  (let* ((texpr (xf (xf-actual-expr (expr act))))
	 (opclass (sim-term-op texpr)))
    (cond ((eq opclass 'type-name)
	   (mk-ergo-term 'name-expr
	     (term-args texpr)))
	  ((eq opclass 'expr-as-type)
	   (mk-ergo-term* 'tuple-expr
	     (term-arg0 texpr)))
	  (t texpr))))

(defmethod xf-actual-expr ((act-expr expr-as-type))
  act-expr)

(defmethod xf-actual-expr ((act-expr subtype))
  (if (typep (predicate act-expr) 'lambda-expr)
      (change-class
       (copy (predicate act-expr))
       'set-expr)
      (let ((nex (change-class
		  (copy act-expr) 'expr-as-type)))
	(setf (expr nex) (predicate nex))
	nex)))

(defmethod xf-actual-expr ((act-expr type-application))
  (let ((name-expr (change-class (copy (type act-expr)) 'name-expr)))
    (mk-application name-expr (parameters act-expr))))

(defmethod xf-actual-expr (act-expr)
  act-expr)

	
(defun xf-idops (ids)
  (mk-ergo-term 'idops
    (mapcar #'(lambda (id) (mk-ergo-term* 'idop (mk-id id)))
	    ids)))

(defun xf-idop (id)
  (mk-ergo-term* 'idop
    (if (memq (intern (string id) :sbst) pvs-keyword-list)
	(mk-sim-term* id)
	(mk-id id))))


;;; Make mod ids, which are just regular ids with :no-translation in the
;;; attribute list, used by ergo-tex-fixes.lisp.

(defun mk-mod-id (id)
  (let ((modid (mk-id id)))
    (setf (getf (term:term-attr modid) :no-translation) t)
    modid))

(defun remove-unexpanded-decls (decls)
  (remove-if-not #'to-be-displayed? decls))

(defun to-be-displayed? (decl)
  (not (or (field-decl? decl)
	   (and nil ;*suppress-proved-tccs*
		(formula-decl? decl)
		(eq (kind decl) 'TCC)
		(eq (proof-status decl) 'proved)))))


;;; Given an operator symbol, return the equivalent symbol in the
;;; sbst package.

(defun sbst-symbol (sym)
  (or (get sym 'sbst-symbol)
      (setf (get sym 'sbst-symbol)
	    (intern (symbol-name sym) 'sbst))))

;;; Find the precedence of an expression.

(defmethod precedence :around ((expr expr) ctx)
  (declare (ignore ctx))
  (if (plusp (parens expr))
      most-positive-fixnum
      (call-next-method)))

;; Most types of expressions cannot be ambiguous (e.g. tuples, if-exprs).
(defmethod precedence ((expr expr) ctx)
  (declare (ignore ctx))
  most-positive-fixnum)

(defmethod precedence ((expr unary-application) ctx)
  (if (and (typep (operator expr) 'name-expr)
	   (member (id (operator expr)) *unary-operators*))
      (case ctx
	(left (gethash (sbst-symbol (id (operator expr)))
		       (first *expr-prec-info*)))
	(right most-positive-fixnum))
      (call-next-method)))

(defmethod precedence ((expr binding-expr) ctx)
  (case ctx
    (left (or (gethash (sbst-symbol '|\||)
		       (third *expr-prec-info*))
	      21))
    (right (or (gethash (sbst-symbol '|\||)
			(second *expr-prec-info*))
	       20))))
    
(defmethod precedence ((expr let-expr) ctx)
  (case ctx
    (left (gethash (sbst-symbol 'in)
		   (fourth *expr-prec-info*)))
    (right most-positive-fixnum)))

(defmethod precedence ((expr update-expr) ctx)
  (case ctx
    (left most-positive-fixnum)
    (right (gethash (sbst-symbol 'with)
		    (second *expr-prec-info*)))))
    
(defmethod precedence ((expr application) ctx)
  (case ctx
    (left most-positive-fixnum)
    (right (gethash 'jux
		    (second *expr-prec-info*)))))

(defmethod precedence ((expr infix-application) ctx)
  (if (and (typep (operator expr) 'name-expr)
	   (member (id (operator expr)) *infix-operators*))
      (case ctx
	(left (min (gethash (sbst-symbol (id (operator expr)))
			    (third *expr-prec-info*))
		   (if (not (zerop (parens (args2 expr))))
		       most-positive-fixnum
		       (precedence (second (arguments expr)) 'left))))
	(right (gethash (sbst-symbol (id (operator expr)))
			(second *expr-prec-info*))))
      (call-next-method)))

(defmethod precedence ((expr name-expr) ctx)
  (if (and (eq ctx 'left)
	   (memq (id expr) *unary-operators*))
      0
      (call-next-method)))
