;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gensubst.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Oct 20 01:35:37 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Apr 15 16:01:21 1998
;; Update Count    : 24
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defvar *gensubst-cache* (make-hash-table :test #'eq))

(defvar *dont-expand-adt-subtypes* nil)

(defvar *gensubst-reset-types* nil)

(defun gensubst (obj substfn testfn)
  (unwind-protect (gensubst* obj substfn testfn)
    (clrhash *gensubst-cache*)))

(defmethod gensubst* :around (obj substfn testfn)
  (or (and *gensubst-cache*
	   (gethash obj *gensubst-cache*))
      (let ((nobj (if (funcall testfn obj)
		      (funcall substfn obj)
		      (call-next-method))))
	(if *gensubst-cache*
	    (setf (gethash obj *gensubst-cache*) nobj)
	    nobj))))

(defmethod gensubst* ((obj module) substfn testfn)
  (let* ((formals (gensubst* (formals obj) substfn testfn))
	 (formals-sans-usings (if (eq formals (formals obj))
				  (formals-sans-usings obj)
				  (remove-if #'(lambda (ff)
						 (typep ff 'importing))
				    formals))))

	
    (lcopy obj
      'formals formals
      'formals-sans-usings formals-sans-usings
      'assuming (gensubst* (assuming obj) substfn testfn)
      'exporting (gensubst* (exporting obj) substfn testfn)
      'theory (gensubst* (theory obj) substfn testfn))))

(defmethod gensubst* ((obj recursive-type) substfn testfn)
  (let* ((formals (gensubst* (formals obj) substfn testfn))
	 (formals-sans-usings (if (eq formals (formals obj))
				  (formals-sans-usings obj)
				  (remove-if #'(lambda (ff)
						 (typep ff 'importing))
				    formals))))
    (lcopy obj
      'formals formals
      'formals-sans-usings formals-sans-usings
      'importings (gensubst* (importings obj) substfn testfn)
      'constructors (gensubst* (constructors obj) substfn testfn))))

(defmethod gensubst* ((obj simple-constructor) substfn testfn)
  (lcopy obj
    'arguments (gensubst* (arguments obj) substfn testfn)))

(defmethod gensubst* ((obj adtdecl) substfn testfn)
  (lcopy obj 'bind-decl (gensubst* (bind-decl obj) substfn testfn)))

(defmethod gensubst* ((decl formula-decl) substfn testfn)
  (lcopy (call-next-method)
    'definition (gensubst* (definition decl) substfn testfn)))

;(defmethod gensubst* :around ((obj syntax) substfn testfn)
;  (let ((nobj (call-next-method)))
;    (if (or (eq nobj obj)
;	    (null *copy-abstract-syntax*))
;	nobj
;	(progn (setf (abstract-syntax nobj) (abstract-syntax obj))
;	       nobj))))

(defmethod gensubst* :around ((te type-expr) substfn testfn)
  (let ((nte (call-next-method)))
    (if (or (eq te nte)
	    (null (print-type te))
	    ;; It's possible for the print-type to have been set if te is a
	    ;; datatype-subtype.  This comes from
	    ;;   gensubst* (subtype) -> gensubst* (type-name)
	    ;;                     -> adt-expand-positive-subtypes!
	    (not (eq (print-type nte) (print-type te))))
	nte
	(lcopy nte
	  'print-type (let ((*dont-expand-adt-subtypes* t))
			(gensubst* (print-type te) substfn testfn))))))

(defmethod gensubst* ((list list) substfn testfn)
  (let ((nlist (gensubst-list list substfn testfn nil)))
    (if (equal nlist list) list nlist)))

(defun gensubst-list (list substfn testfn nlist)
  (if (null list)
      (nreverse nlist)
      (let* ((ncar (gensubst* (car list) substfn testfn))
	     (ncdr (if (and (not *parsing-or-unparsing*)
			    (not (eq ncar (car list)))
			    (typep (car list) 'binding)
			    (declaration (car list)))
		       (substit (cdr list) (acons (car list) ncar nil))
		       (cdr list))))
	(if (listp ncdr)
	    (gensubst-list ncdr substfn testfn (cons ncar nlist))
	    (cons ncar (gensubst* ncdr substfn testfn))))))

(defmethod gensubst* ((obj exporting) substfn testfn)
  (lcopy obj
    'names (gensubst* (slot-value obj 'names) substfn testfn)
    'but-names (gensubst* (but-names obj) substfn testfn)
    'modules (gensubst* (modules obj) substfn testfn)))

(defmethod gensubst* ((using importing) substfn testfn)
  (lcopy using 'theory-name (gensubst* (theory-name using) substfn testfn)))

(defmethod gensubst* ((decl declaration) substfn testfn)
  (declare (ignore substfn testfn))
  decl)

(defmethod gensubst* ((decl typed-declaration) substfn testfn)
  (lcopy decl
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type decl) substfn testfn))
    'type (gensubst* (type decl) substfn testfn)))

(defmethod gensubst* ((decl type-def-decl) substfn testfn)
  (lcopy decl
    'type-value (gensubst* (type-value decl) substfn testfn)
    'type-expr  (gensubst* (type-expr decl) substfn testfn)
    'contains   (gensubst* (contains decl) substfn testfn)))

(defmethod gensubst* ((decl formal-subtype-decl) substfn testfn)
  (lcopy decl
    'type-value (gensubst* (type-value decl) substfn testfn)
    'type-expr  (gensubst* (type-expr decl) substfn testfn)))

(defmethod gensubst* ((decl mod-decl) substfn testfn)
  (lcopy decl 'modname (gensubst* (modname decl) substfn testfn)))

(defmethod gensubst* ((decl theory-abbreviation-decl) substfn testfn)
  (lcopy decl 'theory-name (gensubst* (theory-name decl) substfn testfn)))

(defmethod gensubst* ((decl const-decl) substfn testfn)
  (lcopy (call-next-method)
    'definition (gensubst* (definition decl) substfn testfn)))

(defmethod gensubst* ((decl def-decl) substfn testfn)
  (lcopy (call-next-method)
    'measure (gensubst* (measure decl) substfn testfn)))

(defmethod gensubst* ((type dep-binding) substfn testfn)
  (let ((ntype (if *visible-only*
		   (type type)
		   (gensubst* (type type) substfn testfn))))
    (if (and (not (null (type type)))
	     (eq ntype (type type))
	     (not *visible-only*))
	type
	(let ((dtype (let ((*dont-expand-adt-subtypes* t))
		       (gensubst* (declared-type type) substfn testfn))))
	  (lcopy type 'type ntype 'declared-type dtype)))))

(defmethod gensubst* ((te type-name) substfn testfn)
  (declare (ignore substfn testfn))
  (let ((nte (call-next-method)))
    (if (and (not *dont-expand-adt-subtypes*)
	     (resolution te)
	     (adt-expand-positive-subtypes? nte))
	(adt-expand-positive-subtypes! nte)
	nte)))

(defmethod gensubst* ((te type-application) substfn testfn)
  (let ((ntype (gensubst* (type te) substfn testfn))
	(nargs (gensubst* (parameters te) substfn testfn)))
    (lcopy te 'type ntype 'parameters nargs)))

(defmethod gensubst* ((te expr-as-type) substfn testfn)
  (let ((nexpr (gensubst* (expr te) substfn testfn)))
    (lcopy te
      'expr (if (or *parsing-or-unparsing*
		    *visible-only*
		    (eq nexpr (expr te)))
		nexpr
		(pseudo-normalize nexpr)))))

(defmethod gensubst* ((te subtype) substfn testfn)
  (let ((stype (gensubst* (supertype te) substfn testfn))
	(pred (gensubst* (predicate te) substfn testfn)))
    (lcopy te
      'supertype stype
      'predicate (if (or *parsing-or-unparsing*
			 *visible-only*
			 (eq pred (predicate te)))
		     pred
		     (pseudo-normalize pred)))))

(defmethod gensubst* ((te setsubtype) substfn testfn)
  (if (predicate te)
      (let ((nte (call-next-method)))
	(unless (or (eq nte te)
		    (null (predicate nte)))
	  (setf (formula nte) (expression (predicate nte))))
	nte)
      (let ((nform (gensubst* (formula te) substfn testfn)))
	(lcopy te 'formula nform))))

(defmethod gensubst* ((te funtype) substfn testfn)
  (let* ((types (list (domain te) (range te)))
	 (ntypes (gensubst* types substfn testfn)))
    (if (eq types ntypes)
	te
	(copy te 'domain (car ntypes) 'range (cadr ntypes)))))

(defmethod gensubst* ((te tupletype) substfn testfn)
  (let ((types (gensubst* (types te) substfn testfn)))
    (lcopy te 'types types)))

(defmethod gensubst* ((te cotupletype) substfn testfn)
  (let ((types (gensubst* (types te) substfn testfn)))
    (lcopy te 'types types)))

(defmethod gensubst* ((te recordtype) substfn testfn)
  (let ((fields (gensubst* (fields te) substfn testfn)))
    (if (eq fields (fields te))
	te
	(copy te
	  'fields (if *parsing-or-unparsing*
		      fields
		      (sort-fields fields
				   (dependent-fields? fields)))))))


(defmethod gensubst* ((fd field-decl) substfn testfn)
  (let ((ntype (if *visible-only*
		   (type fd)
		   (gensubst* (type fd) substfn testfn))))
    (if (and ntype
	     (eq ntype (type fd))
	     (not *visible-only*))
	fd
	(let ((dtype (let ((*dont-expand-adt-subtypes* t))
		       (gensubst* (declared-type fd) substfn testfn))))
	  (lcopy fd 'type ntype 'declared-type dtype)))))


;;; Expressions

(defmethod gensubst* :around ((ex expr) substfn testfn)
  (let ((nex (call-next-method)))
    (if *gensubst-reset-types*
	(unless (eq ex nex)
	  (setf (type ex) nil
		(types ex) nil)
	  (when (name-expr? ex)
	    (setf (resolutions ex) nil)))
	(unless (or (eq ex nex) (type ex))
	  (setf (types nex) (gensubst* (types ex) substfn testfn))))
    nex))

(defmethod gensubst* ((ex name-expr) substfn testfn)
  (declare (ignore substfn testfn))
  (let ((nex (call-next-method)))
    (if (or *parsing-or-unparsing*
	    *visible-only*
	    (eq ex nex))
	nex
	(let ((ntype (type (resolution nex))))
	  (if (eq ntype (type ex))
	      nex
	      (lcopy nex 'type ntype))))))

(defmethod gensubst* ((ex adt-name-expr) substfn testfn)
  (let ((nex (call-next-method)))
    (if (or *visible-only* (eq ex nex))
	nex
	(copy nex 'adt-type (gensubst* (adt-type ex) substfn testfn)))))

(defmethod gensubst* ((ex number-expr) substfn testfn)
  (declare (ignore substfn testfn))
  ex)

(defmethod gensubst* ((ex record-expr) substfn testfn)
  (let ((ass (gensubst* (assignments ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'assignments ass 'type ntype)))

(defmethod gensubst* ((ex tuple-expr) substfn testfn)
  (let ((exprs (gensubst* (exprs ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'exprs exprs 'type ntype)))

;(defmethod gensubst* ((ex coercion) substfn testfn)
;  (let ((nexpr (gensubst* (expression ex) substfn testfn))
;	(ntype (gensubst* (type ex) substfn testfn)))
;    (lcopy ex 'expression nexpr 'type ntype)))

;(defmethod gensubst* ((ex intype) substfn testfn)
;  (let ((nexpr (gensubst* (expression ex) substfn testfn))
;	(ntype (gensubst* (type-value ex) substfn testfn)))
;    (lcopy ex 'expression nexpr 'type-value ntype)))

(defmethod gensubst* ((ex cases-expr) substfn testfn)
  (let ((nexpr (gensubst* (expression ex) substfn testfn)))
    (if (or (eq nexpr (expression ex))
	    *parsing-or-unparsing*
	    (compatible? (type nexpr) (type (expression ex))))
	(let ((sels (gensubst* (selections ex) substfn testfn))
	      (else (gensubst* (else-part ex) substfn testfn))
	      (ntype (if (or *parsing-or-unparsing*
			     *visible-only*)
			 (type ex)
			 (gensubst* (type ex) substfn testfn))))
	  (lcopy ex
	    'expression nexpr
	    'selections sels
	    'else-part else
	    'type ntype))
	(gensubst* (translate-cases-to-if ex) substfn testfn))))

(defmethod gensubst* ((ex fieldex) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex projection-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex injection-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex injection?-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex extraction-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex projection-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex injection-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex injection?-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex extraction-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex field-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex application) substfn testfn)
  (let* ((nop (gensubst* (operator ex) substfn testfn))
	 (narg (gensubst* (argument ex) substfn testfn))
	 (ntype (cond ((or *parsing-or-unparsing*
			   *visible-only*
			   (and (eq nop (operator ex))
				(eq narg (argument ex))))
		       (type ex))
		      ((dep-binding? (domain (find-supertype (type nop))))
		       (substit (range (find-supertype (type nop)))
			 (acons (domain (find-supertype (type nop))) narg
				nil)))
		      (t (range (find-supertype (type nop)))))))
    (lcopy ex
      'operator (if (or (eq narg (argument ex))
			(not (eq nop (operator ex))))
		    nop
		    (copy nop))
      'argument narg
      'type ntype)))

(defmethod gensubst* :around ((ex table-expr) substfn testfn)
  (let ((nex (call-next-method)))
    (if (eq ex nex)
	ex
	(lcopy nex
	  'row-headings (gensubst* (row-headings nex) substfn testfn)
	  'col-headings (gensubst* (col-headings nex) substfn testfn)
	  'table-entries (gensubst* (table-entries nex) substfn testfn)))))

(defmethod gensubst* ((ex binding-expr) substfn testfn)
  (let* ((nexpr (gensubst* (expression ex) substfn testfn))
	 (nbindings (gensubst* (bindings ex) substfn testfn))
	 (ntype (if (or *visible-only*
			*parsing-or-unparsing*)
		    (type ex)
		    (gensubst* (type ex) substfn testfn))))
    (if (and (eq (expression ex) nexpr)
	     (eq (bindings ex) nbindings))
	(lcopy ex 'type ntype)
	(lcopy ex
	  'bindings nbindings
	  'expression (if (or *parsing-or-unparsing*
			      (eq (bindings ex) nbindings)
			      (not (declaration (car (bindings ex)))))
			  nexpr
			  (substit nexpr (pairlis (bindings ex) nbindings)))
	  'type ntype))))

(defmethod gensubst* ((ex bind-decl) substfn testfn)
  (let ((ntype (if *visible-only*
		   (type ex)
		   (gensubst* (type ex) substfn testfn)))
	(ndeclared-type (let ((*dont-expand-adt-subtypes* t))
			  (gensubst* (declared-type ex) substfn testfn))))
    (lcopy ex
      'type ntype
      'declared-type ndeclared-type)))

(defmethod gensubst* ((ex update-expr) substfn testfn)
  (let ((nexpr (gensubst* (expression ex) substfn testfn))
	(nass (gensubst* (assignments ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'expression nexpr 'assignments nass 'type ntype)))

(defmethod gensubst* ((ex assignment) substfn testfn)
  (let ((nargs (gensubst* (arguments ex) substfn testfn))
	(nexpr (gensubst* (expression ex) substfn testfn)))
    (lcopy ex 'arguments nargs 'expression nexpr)))

(defmethod gensubst* ((ex selection) substfn testfn)
  (let ((nconstr (gensubst* (constructor ex) substfn testfn))
	(nargs (gensubst* (args ex) substfn testfn))
	(nexpr (gensubst* (expression ex) substfn testfn)))
    (if (and (eq nconstr (constructor ex))
	     (eq nexpr (expression ex))
	     (eq nargs (args ex)))
	ex
	(lcopy ex
	  'constructor nconstr
	  'args nargs
	  'expression (if (and (not *parsing-or-unparsing*)
			       (args ex) (declaration (car (args ex))))
			  (substit nexpr (pairlis (args ex) nargs))
			  nexpr)))))

(defmethod gensubst* ((name name) substfn testfn)
  (if (or *parsing-or-unparsing*
	  *visible-only*
	  (null (resolutions name)))
      (lcopy name
	'actuals (gensubst* (actuals name) substfn testfn)
	'mappings (gensubst* (mappings name) substfn testfn))
      (let ((nres (gensubst* (resolutions name) substfn testfn)))
	(if (eq nres (resolutions name))
	    name
	    (copy name
	      'resolutions nres
	      'actuals (gensubst* (actuals name) substfn testfn)
	      'mappings (gensubst* (mappings name) substfn testfn))))))

(defmethod gensubst* ((map mapping) substfn testfn)
  (lcopy map
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type map) substfn testfn))
    'type (gensubst* (type map) substfn testfn)
    'lhs (gensubst* (lhs map) substfn testfn)
    'rhs (gensubst* (rhs map) substfn testfn)))

(defmethod gensubst* ((map mapping-with-formals) substfn testfn)
  (lcopy map
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type map) substfn testfn))
    'type (gensubst* (type map) substfn testfn)
    'lhs (gensubst* (lhs map) substfn testfn)
    'rhs (gensubst* (rhs map) substfn testfn)
    'formals (gensubst* (formals map) substfn testfn)))

(defmethod gensubst* ((res resolution) substfn testfn)
  (let ((nmi (gensubst* (module-instance res) substfn testfn)))
    (if (eq nmi (module-instance res))
	res
	(make-resolution (declaration res) nmi)))) 
	       

(defmethod gensubst* ((act actual) substfn testfn)
  (if (and (not *parsing-or-unparsing*)
	   (not *visible-only*)
	   (type-value act))
      (let ((ntype (gensubst* (type-value act) substfn testfn)))
	(if (eq ntype (type-value act))
	    act
	    (mk-actual ntype)))
      (let ((nexpr (gensubst* (expr act) substfn testfn)))
	(lcopy act
	  'expr (if (or *parsing-or-unparsing*
			*visible-only*
			(eq nexpr (expr act))
			(not (typep nexpr 'expr))
			(and (name-expr? (expr act))
			     (module? (declaration (expr act)))))
		    nexpr
		    (pseudo-normalize nexpr))))))

(defmethod gensubst* ((name modname) substfn testfn)
  (let ((nacts (gensubst* (actuals name) substfn testfn))
	(nmaps (gensubst* (mappings name) substfn testfn)))
    (lcopy name
      'actuals (if (and nacts
			(not (eq nacts (actuals name)))
			(memq (id name) '(|equalities| |notequal|)))
		   (list (mk-actual (find-supertype
				     (type-value (car nacts)))))
		   nacts)
      'mappings nmaps)))

(defmethod gensubst* ((obj symbol) substfn testfn)
  (declare (ignore substfn testfn))
  obj)


;;; Mapobject - maps a function fn down the abstract syntax.
;;;             Returns the object
;;;             If the function fn returns non-NIL, will not go down
;;;                subcomponents.
;;;  Notes: 1. If the type-value of an actual is set, will not go down
;;;            the expr.
;;;         2. Caches the result for each call to mapobject.

(defvar *mapobject-cache* (make-hash-table :test #'eq :size 37))

(defun mapobject (fn obj)
  (unwind-protect
      (mapobject* fn obj)
    (clrhash *mapobject-cache*))
  obj)

(defmethod mapobject* :around (fn obj)
  (unless (gethash obj *mapobject-cache*)
    (setf (gethash obj *mapobject-cache*) t)
    (unless (funcall fn obj)
      (call-next-method))))

(defmethod mapobject* (fn (obj null))
  (declare (ignore fn))
  nil)

(defmethod mapobject* (fn (obj cons))
  (mapobject* fn (car obj))
  (mapobject* fn (cdr obj)))

(defmethod mapobject* (fn (obj datatype-or-module))
  (mapobject* fn (formals obj))
  (mapobject* fn (assuming obj)))

(defmethod mapobject* (fn (obj module))
  (call-next-method)
  (mapobject* fn (exporting obj))
  (mapobject* fn (theory obj)))

(defmethod mapobject* (fn (obj exporting))
  (mapobject* fn (names obj))
  (mapobject* fn (but-names obj))
  (mapobject* fn (modules obj)))

(defmethod mapobject* (fn (obj importing))
  (mapobject* fn (theory-name obj)))

;;; Declarations

(defmethod mapobject* (fn (obj declaration))
  (mapobject* fn (formals obj)))

(defmethod mapobject* (fn (obj typed-declaration))
  (mapobject* fn (declared-type obj))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type obj)))
  (call-next-method))

(defmethod mapobject* (fn (obj mod-decl))
  (mapobject* fn (modname obj))
  (call-next-method))

(defmethod mapobject* (fn (obj type-decl))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type-value obj)))
  (call-next-method))

(defmethod mapobject* (fn (obj type-def-decl))
  (mapobject* fn (type-expr obj))
  (mapobject* fn (contains obj))
  (call-next-method))

(defmethod mapobject* (fn (obj const-decl))
  (mapobject* fn (definition obj))
  (call-next-method))

(defmethod mapobject* (fn (obj def-decl))
  (mapobject* fn (definition obj))
  (mapobject* fn (measure obj))
  (call-next-method))

(defmethod mapobject* (fn (obj formula-decl))
  (mapobject* fn (definition obj))
  ;;(mapobject* fn (justification obj))
  (call-next-method))

(defmethod mapobject* (fn (obj subtype-judgement))
  (mapobject* fn (subtype obj))
  (call-next-method))

(defmethod mapobject* (fn (obj name-judgement))
  (mapobject* fn (name obj))
  (call-next-method))

(defmethod mapobject* (fn (obj application-judgement))
  (mapobject* fn (name obj))
  (mapobject* fn (formals obj))
  (call-next-method))

;;; Type Expressions

(defmethod mapobject* :around (fn (te type-expr))
  (unless (and *parsing-or-unparsing*
	       (print-type te))
    (call-next-method))
  (mapobject* fn (print-type te)))

(defmethod mapobject* (fn (te subtype))
  (unless *parsing-or-unparsing*
    (mapobject* fn (supertype te)))
  (mapobject* fn (predicate te)))

(defmethod mapobject* (fn (te type-application))
  (call-next-method)
  (mapobject* fn (type te))
  (mapobject* fn (parameters te)))

(defmethod mapobject* (fn (te setsubtype))
  (call-next-method)
  ;;(mapobject* fn (formals te))
  (mapobject* fn (formula te)))

(defmethod mapobject* (fn (te expr-as-type))
  (call-next-method)
  (mapobject* fn (expr te)))

(defmethod mapobject* (fn (te funtype))
  (mapobject* fn (domain te))
  (mapobject* fn (range te)))

(defmethod mapobject* (fn (te tupletype))
  (mapobject* fn (types te)))

(defmethod mapobject* (fn (te cotupletype))
  (mapobject* fn (types te)))

(defmethod mapobject* (fn (te recordtype))
  (mapobject* fn (fields te)))

(defmethod mapobject* (fn (fd field-decl))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type fd)))
  (mapobject* fn (declared-type fd)))

;;; Expressions

(defmethod mapobject* :around (fn (ex expr))
  (call-next-method)
  (unless *parsing-or-unparsing*
    (mapobject* fn (type ex))
    (unless (type ex)
      (mapobject* fn (types ex)))))

;(defmethod mapobject* (fn (ex name-expr))
;  (declare (ignore fn))
;  (call-next-method)			; For names
;  )

(defmethod mapobject* (fn (ex number-expr))
  (declare (ignore fn))
  nil)

(defmethod mapobject* (fn (ex record-expr))
  (mapobject* fn (assignments ex)))

(defmethod mapobject* (fn (ex tuple-expr))
  (mapobject* fn (exprs ex)))

;(defmethod mapobject* (fn (ex intype))
;  (mapobject* fn (expression ex))
;  (mapobject* fn (type-value ex))
;  (mapobject* fn (declared-type ex)))

;(defmethod mapobject* (fn (ex coercion))
;  (mapobject* fn (expression ex))
;  ;;(mapobject* fn (type-value ex))
;  (mapobject* fn (declared-type ex)))

(defmethod mapobject* (fn (ex cases-expr))
  (mapobject* fn (expression ex))
  (mapobject* fn (selections ex))
  (mapobject* fn (else-part ex)))

(defmethod mapobject* (fn (ex projection-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex injection-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex injection?-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex extraction-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex projection-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex injection-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex injection?-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex extraction-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex field-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex application))
  (mapobject* fn (operator ex))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex binding-expr))
  (mapobject* fn (bindings ex))
  (mapobject* fn (expression ex)))

(defmethod mapobject* (fn (ex update-expr))
  (mapobject* fn (expression ex))
  (mapobject* fn (assignments ex)))

(defmethod mapobject* (fn (ex assignment))
  (mapobject* fn (arguments ex))
  (mapobject* fn (expression ex)))

(defmethod mapobject* (fn (ex selection))
  (mapobject* fn (constructor ex))
  (mapobject* fn (args ex))
  (mapobject* fn (expression ex)))

;;; Misc

(defmethod mapobject* (fn (obj simple-decl))
  (mapobject* fn (declared-type obj))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type obj)))
  (when (next-method-p) (call-next-method)))

;(defmethod mapobject* (fn (te dep-binding))
;  (declare (ignore fn))
;  (call-next-method))

(defmethod mapobject* (fn (obj bind-decl))
  (call-next-method)			; binding
  (mapobject* fn (actuals obj))
  ;;(mapobject* fn (resolutions obj))
  )

(defmethod mapobject* (fn (obj name))
  (when (next-method-p) (call-next-method)) ; Handles expr part of name-expr
  (mapobject* fn (actuals obj))
  (mapobject* fn (mappings obj)))

(defmethod mapobject* (fn (act actual))
  (if (and (not *parsing-or-unparsing*)
	   (type-value act))
      (mapobject* fn (type-value act))
      (mapobject* fn (expr act))))

(defmethod mapobject* (fn (map mapping))
  (mapobject* fn (lhs map))
  (mapobject* fn (rhs map)))

(defmethod mapobject* (fn obj)
  (declare (ignore fn obj))
  nil)


;;; Copy-untyped copies untyped expressions.

(defmethod copy-untyped* ((ex type-expr))
  (pc-parse (unparse ex :string t) 'type-expr))

(defun copy-untyped (expr)
  (copy-untyped* expr))

(defmethod copy-untyped* ((list list))
  (when list
    (cons (copy-untyped* (car list))
	  (copy-untyped* (cdr list)))))

(defmethod copy-untyped* ((ex name-expr))
  (with-slots (actuals) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex field-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex adt-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex constructor-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex null-expr))
  (with-slots (actuals) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex recognizer-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex accessor-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex name))
  (with-slots (actuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex bind-decl))
  (with-slots (declared-type) ex
    (copy ex
      'type nil
      'declared-type (copy-untyped* declared-type)
      'resolutions nil)))

(defmethod copy-untyped* ((ex number-expr))
  (copy ex
    'type nil))

(defmethod copy-untyped* ((ex tuple-expr))
  (with-slots (exprs) ex
    (copy ex
      'type nil
      'exprs (copy-untyped* exprs))))

(defmethod copy-untyped* ((ex record-expr))
  (with-slots (assignments) ex
    (copy ex
      'type nil
      'assignments (copy-untyped* assignments))))

(defmethod copy-untyped* ((ex cases-expr))
  (with-slots (expression selections else-part) ex
    (copy ex
      'type nil
      'expression (copy-untyped* expression)
      'selections (copy-untyped* selections)
      'else-part (copy-untyped* else-part))))

(defmethod copy-untyped* ((ex selection))
  (with-slots (constructor args expression) ex
    (copy ex
      'constructor (copy-untyped* constructor)
      'args (copy-untyped* args)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((ex projection-expr))
  (copy ex 'type nil))

(defmethod copy-untyped* ((ex injection-expr))
  (copy ex 'type nil))

(defmethod copy-untyped* ((ex injection?-expr))
  (copy ex 'type nil))

(defmethod copy-untyped* ((ex extraction-expr))
  (copy ex 'type nil))

(defmethod copy-untyped* ((ex projection-application))
  (with-slots (argument) ex
    (copy ex
      'type nil
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex injection-application))
  (with-slots (argument) ex
    (copy ex
      'type nil
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex injection?-application))
  (with-slots (argument) ex
    (copy ex
      'type nil
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex extraction-application))
  (with-slots (argument) ex
    (copy ex
      'type nil
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex field-application))
  (with-slots (id argument) ex
    (make-instance 'application
      'operator (make-instance 'name-expr 'id id)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex application))
  (with-slots (operator argument) ex
    (copy ex
      'type nil
      'operator (copy-untyped* operator)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex binding-expr))
  (with-slots (bindings expression) ex
    (copy ex
      'type nil
      'bindings (copy-untyped* bindings)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((ex update-expr))
  (with-slots (expression assignments) ex
    (copy ex
      'type nil
      'expression (copy-untyped* expression)
      'assignments (copy-untyped* assignments))))

(defmethod copy-untyped* ((ex assignment))
  (with-slots (arguments expression) ex
    (copy ex
      'arguments (copy-untyped* arguments)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((ex actual))
  (with-slots (expr) ex
    (copy ex
      'expr (copy-untyped* expr)
      'type-value nil)))

(defmethod copy-untyped* ((ex table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (make-instance 'table-expr
      'row-expr (copy-untyped* row-expr)
      'col-expr (copy-untyped* col-expr)
      'row-headings (copy-untyped* row-headings)
      'col-headings (copy-untyped* col-headings)
      'table-entries (copy-untyped* table-entries))))

(defmethod copy-untyped* ((ex let-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (make-instance 'table-expr
      'row-expr (copy-untyped* row-expr)
      'col-expr (copy-untyped* col-expr)
      'row-headings (copy-untyped* row-headings)
      'col-headings (copy-untyped* col-headings)
      'table-entries (copy-untyped* table-entries))))

(defmethod copy-untyped* ((ex argument-conversion))
  (with-slots (operator) ex
    (copy-untyped* operator)))

(defmethod copy-untyped* ((ex lambda-conversion))
  (with-slots (expression) ex
    (copy-untyped* expression)))

(defmethod copy-untyped* ((ex implicit-conversion))
  (with-slots (argument) ex
    (copy-untyped* argument)))

(defmethod copy-untyped* :around ((ex type-expr))
  (if (print-type ex)
      (copy-untyped* (print-type ex))
      (call-next-method)))

(defmethod copy-untyped* ((ex type-application))
  (with-slots (type parameters) ex
    (copy ex
      'type (copy-untyped* type)
      'parameters (copy-untyped* parameters)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex subtype))
  (with-slots (supertype predicate) ex
    (copy ex
      'supertype (copy-untyped* supertype)
      'predicate (copy-untyped* predicate)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex setsubtype))
  (with-slots (supertype predicate formals formula) ex
    (copy ex
      'supertype (copy-untyped* supertype)
      'predicate (copy-untyped* predicate)
      'formals (copy-untyped* formals)
      'formula (copy-untyped* formula)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex expr-as-type))
  (with-slots (supertype predicate expr) ex
    (copy ex
      'supertype (copy-untyped* supertype)
      'predicate (copy-untyped* predicate)
      'expr (copy-untyped* expr)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex funtype))
  (with-slots (domain range) ex
    (copy ex
      'domain (copy-untyped* domain)
      'range (copy-untyped* range)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex tupletype))
  (with-slots (types) ex
    (copy ex
      'types (copy-untyped* types)
      'generated? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex cotupletype))
  (with-slots (types) ex
    (copy ex
      'types (copy-untyped* types)
      'generated? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex recordtype))
  (with-slots (fields) ex
    (copy ex
      'fields (copy-untyped* fields)
      'dependent? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex type-name))
  (with-slots (actuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex field-decl))
  (with-slots (declared-type) ex
    (copy ex
      'declared-type (copy-untyped* declared-type)
      'type nil
      'resolutions nil)))
