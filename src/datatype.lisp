;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datatype.lisp -- Generation and typechecking of datatypes
;; Author          : Sam Owre
;; Created On      : Sun Dec 12 01:18:46 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Apr  3 12:43:21 1998
;; Update Count    : 114
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

;;; Generates the theory corresponding to the datatype.  The declarations
;;; generated are:
;;;   the uninterpreted datatype declaration
;;;   the recognizer declarations
;;;   the accessor declarations
;;;   the ord declaration (Not generated for now)
;;;   extensionality and eta axioms
;;;   accessor/constructor axioms
;;;   inclusive axiom
;;;   induction axiom
;;;   every definition - positive type parameters
;;;   some definition - positive type parameters
;;;   subterm definition
;;;   << definition
;;;   << well-founded axiom
;;;   reduce_nat definition
;;;   REDUCE_nat definition
;;;   reduce_ordinal definition
;;;   REDUCE_ordinal definition

(defvar *adt* nil
  "The datatype declaration being processed")

(defvar *adt-vars* nil
  "A hashtable that holds the variable names used for the adt, constructors,
and accessors")

(defvar *last-adt-decl* nil)

(defvar *generate-all-adt-axioms* t
  "Indicates whether all ADT axioms should be generated")

(defvar *ord-definition-cutoff* most-positive-fixnum
  "If the number of constructors is larger than this, its definition won't be
generated")

(defvar *negative-occurrence* nil)

(defvar *no-adt-subtypes* nil)


;;; Three new theories are built and typechecked; this macro wraps around
;;; the generation and typechecking of the declarations.

(defmacro build-adt-theory (tid adt &rest forms)
  (let ((vtid (gensym))
	(vadt (gensym)))
    `(let* ((,vtid ,tid)
	    (,vadt ,adt)
	    (*generating-adt* ,vadt)
	    (*current-theory* (make-instance 'module
				'id ,vtid
				'exporting (make-instance 'exporting
					     'kind 'DEFAULT)))
	    (*current-context* (make-new-context *current-theory*))
	    (*typechecking-module* t)
	    (*tccs* nil)
	    (*tccdecls* nil)
	    (*tccforms* nil))
       (setf (gethash ,vtid (if (from-prelude? ,vadt)
				*prelude*
				*pvs-modules*))
	     *current-theory*)
       ,@forms
       (setf (generated-by *current-theory*) (id ,vadt))
       (push 'typechecked (status *current-theory*))
       (maphash #'(lambda (id decls)
		    (let ((ndecls (remove-if #'formal-decl? decls)))
		      (when ndecls
			(mapc #'set-visibility ndecls)
			(setf (gethash id (current-declarations-hash))
			      ndecls))))
		(current-declarations-hash))
       (generate-xref *current-theory*)
       (check-exporting *current-theory*)
       (setf (all-usings *current-theory*)
	     (let ((imps nil))
	       (maphash #'(lambda (th thinsts)
			    (unless (from-prelude? th)
			      (push (cons th thinsts) imps)))
			(using-hash *current-context*))
	       imps))
       (setf (saved-context *current-theory*) *current-context*)
       *current-theory*)))

(defmethod typecheck ((adt datatype) &key expected context)
  (declare (ignore context))
  (let ((*subtype-of-hash* (make-hash-table :test #'eq)))
    (unwind-protect (typecheck* adt expected nil nil)
      (unless (typechecked? adt)
	(untypecheck-theory adt)))))

(defmethod typecheck* ((adt datatype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*adt* adt)
	(*adt-vars* (make-hash-table :test #'eq))
	(*generate-tccs* 'all))
    (setf (adt-type-name adt) nil)
    (mapc #'(lambda (c)
	      (setf (con-decl c) nil
		    (rec-decl c) nil
		    (acc-decls c) nil))
	   (constructors adt))
    (if *typechecking-module*
	(typecheck-inline-adt adt)
	(typecheck-top-level-adt adt))))

(defun typecheck-inline-adt (adt)
  (when (assuming adt)
    (type-error (car (assuming adt))
      "Assumings are not allowed for in-line datatypes"))
  (let ((*generating-adt* adt)
	(*last-adt-decl* (declaration *current-context*)))
    (generate-inline-adt adt))
  (put-decl adt (current-declarations-hash))
  (setf (typechecked? adt) t))

(defun generate-inline-adt (adt)
  (generate-adt-sections adt))

(defun typecheck-top-level-adt (adt)
  (tcdebug "~%Typecheck ADT ~a" (id adt))
  (setf (formals-sans-usings adt)
	(remove-if #'(lambda (d) (typep d 'importing)) (formals adt)))
  (let ((adt-theory (generate-adt-theory adt))
	(map-theory (generate-adt-map-theory adt))
	(reduce-theory (generate-adt-reduce-theory adt)))
    (setf (adt-theory adt) adt-theory
	  (adt-map-theory adt) map-theory
	  (adt-reduce-theory adt) reduce-theory)
    (save-adt-file adt)
    adt))


;;; We don't want to create a file unless we have to.  Occasionally a
;;; forced typecheck comes through, and then we want to save the file only
;;; if it is different from what is out there.  Thus we compare the old
;;; forms with the new.  Note that the file is never read from, it is
;;; simply created so that the generated theories may be viewed
;;; conveniently.  We want to generate a file whenever there is a
;;; difference, which can happen either because the DATATYPE that
;;; generated it has changed, or a new patch causes the old one to be
;;; invalid.

(defun save-adt-file (adt)
  (let* ((adt-file (concatenate 'string (string (id adt)) "_adt"))
	 (adt-path (make-specpath adt-file))
	 (file-exists? #+allegro (excl::filesys-inode (namestring adt-path))
		       #-allegro (probe-file adt-path))
	 (adt-theories (adt-generated-theories adt))
	 (mods (make-instance 'modules 'modules adt-theories))
	 (ce (unless *loading-prelude*
	       (get-context-file-entry (filename adt)))))
    (unless (and file-exists?
		 ce
		 (equal (ce-write-date ce)
			(file-write-date (make-specpath (filename adt)))))
      (when file-exists?
	(ignore-errors
	  (chmod "a+w" (namestring adt-path)))
	(ignore-file-errors
	 (delete-file (namestring adt-path))))
      (let* ((adtmod (let ((*unparse-expanded* t))
		       (unparse mods :string t)))
	     (condition (nth-value 1
			  (ignore-file-errors
			   (with-open-file (str adt-path :direction :output
						:if-exists :supersede)
			     (format str "~a~a~2%"
			       *adt-generated-string*
			       *current-file*)
			     (princ adtmod str))))))
	(if condition
	    (pvs-message "Error in writing out ~a: ~a"
	      (namestring adt-path) condition)
	    (let ((ntheories (parse :file adt-path))
		  (*copy-lex-exact* t))
	      (mapc #'copy-lex adt-theories ntheories)
	      (pvs-message "Wrote pvs file ~a" adt-file))))
      (chmod "a-w" (namestring adt-path)))
    (let ((fdate (file-write-date adt-path))
	  (ce2 (get-context-file-entry adt-file)))
      (setf (generated-file-date adt) fdate)
      (when ce2
	(setf (ce-write-date ce2) fdate)
	(setq *pvs-context-changed* t))
      (push 'typechecked (status adt))
      (dolist (th adt-theories)
	(setf (filename th) adt-file)
	(setf (path th) (make-specpath adt-file)))
      (setf (gethash adt-file *pvs-files*)
	    (cons fdate (adt-generated-theories adt))))))


(defun adt-generated-theories (adt)
  (when (adt-theory adt)
    (if (adt-map-theory adt)
	(list (adt-theory adt) (adt-map-theory adt) (adt-reduce-theory adt))
	(list (adt-theory adt) (adt-reduce-theory adt)))))

;;; The adt-theory

(defun generate-adt-theory (adt)
  (build-adt-theory (makesym "~a_adt" (id adt)) adt
    (generate-adt-sections adt)))

(defun generate-adt-sections (adt)
  (generate-adt-vars adt)
  (set-constructor-ord-numbers (constructors adt))
  (check-adt-name-uniqueness adt)
  (when (formals adt)
    (generate-adt-formals adt))
  (when (assuming adt)
    (generate-adt-assuming adt))
  (generate-adt-importing (importings adt))
  (generate-adt-type adt)
  (generate-adt-decls adt)
  (check-adt-types adt)
  (set-adt-nonemptiness adt)
  (when (formals adt)
    (set-adt-positive-formal-types adt))
  (when *generate-all-adt-axioms*
    (generate-adt-ord-function adt))
  (if (or (enumtype? adt)
	  (every #'(lambda (c) (null (arguments c))) (constructors adt)))
      (generate-inclusive-axiom adt)
      (generate-adt-axioms adt))
  (generate-adt-induction adt)
  (unless (or (enumtype? adt)
	      (every #'(lambda (c) (null (arguments c))) (constructors adt)))
    (when (and (positive-types adt)
	       (not (every #'null (positive-types adt))))
      (generate-adt-every adt)
      (generate-adt-some adt))
    (generate-adt-subterm adt)
    (generate-adt-reduce adt '|nat|)
    (when (and (not (eq (id adt) '|ordstruct|))
	       (get-theory "ordstruct_adt"))
      (generate-adt-reduce adt '|ordinal|)))
  (setf (adt-theory adt) *current-theory*))

(defun set-adt-nonemptiness (adt)
  (if (some #'(lambda (c)
		(every #'(lambda (a) (not (possibly-empty-type? (type a))))
		       (arguments c)))
	    (constructors adt))
      (setf (nonempty? (adt-type-name adt)) t)
      (pvs-warning "Datatype ~a may be empty" (id adt))))

(defun set-adt-positive-formal-types (adt)
  (setf (positive-types adt)
	(mapcar #'type-value
		(remove-if-not #'(lambda (ff)
				   (and (typep ff 'formal-type-decl)
					(occurs-positively? (type ff) adt)))
		  (formals-sans-usings *current-theory*)))))

(defun check-adt-name-uniqueness (adt)
  (let ((constrids (mapcar #'id (remove-if-not #'simple-constructor?
				  (constructors adt))))
	(recognids (mapcar #'recognizer (constructors adt)))
	(*current-theory* (if (typep adt 'inline-datatype)
			      *current-theory*
			      adt)))
    (when (duplicates? constrids)
      (type-error (duplicates? (constructors adt) :test #'same-id)
	"Duplicate ~a names are not allowed"
	(if (typep adt 'enumtype) "enumeration" "constructor")))
    (when (duplicates? recognids)
      (type-error (duplicates? (constructors adt) :key #'recognizer)
	"Duplicate recognizer names are not allowed"))
    (when (duplicates? (append constrids recognids))
      (type-error (find-if #'(lambda (c) (memq (id c) recognids))
		    (constructors adt))
	"~a may not be used as both a constructor and recognizer name"
	(duplicates? (append constrids recognids))))
    (when (duplicates? (cons (id adt) recognids))
      (type-error adt "Datatype name may not be used as a recognizer name"))
    (dolist (c (constructors adt))
      (let ((accessids (mapcar #'id (arguments c))))
	(when (duplicates? accessids)
	  (type-error (duplicates? (arguments c) :test #'same-id)
	    "May not have duplicate accessor names for a single constructor"))
	(when (duplicates? (cons (id adt) accessids))
	  (type-error (find-if #'(lambda (x) (eq (id x) (id adt)))
			      (arguments c))
	    "Datatype name should not be used as an accessor name"))))))

(defun check-adt-positive-types (adt)
  (setf (positive-types adt)
	(mapcar #'type-value
		(remove-if-not #'(lambda (ff)
				   (and (typep ff 'formal-type-decl)
					(occurs-positively? (type ff) adt)))
		  (formals-sans-usings *current-theory*)))))

(defun non-recursive-constructor (c type)
  (not (some #'(lambda (a) (occurs-in type (type a))) (arguments c))))

(defun generate-adt-formals (adt)
  (setf (formals *current-theory*) (mapcar #'copy (formals adt)))
  (setf (formals-sans-usings *current-theory*)
	(remove-if #'importing? (formals *current-theory*)))
  (mapc #'(lambda (afm cfm)
	    (let ((*adt-decl* afm))
	      ;;(setf (abstract-syntax cfm) nil)
	      (typecheck-adt-decl cfm nil)))
	(formals adt) (formals *current-theory*))
  (set-dependent-formals (formals-sans-usings *current-theory*)))

(defun generate-adt-assuming (adt)
  (setf (assuming *current-theory*) (mapcar #'copy (assuming adt)))
  (mapc #'(lambda (ad cd)
	    (let ((*adt-decl* ad))
	      (typecheck-adt-decl cd nil)))
	(assuming adt) (assuming *current-theory*)))

(defun generate-adt-importing (importings)
  (when importings
    (let ((cimporting (pc-parse (unparse (car importings) :string t)
			    'theory-elt))
	  (*adt-decl* (car importings)))
      (typecheck-adt-decl cimporting))))

(defun generate-adt-type (adt)
  (let ((*adt-decl* adt)
	(tdecl (mk-type-decl (id adt))))
    (typecheck-adt-decl tdecl)
    (setf (generated-by tdecl) (id adt))
    (setf (adt-type-name adt) (type-value tdecl))))

(defvar *common-accessor-types* nil)

(defun generate-adt-decls (adt)
  (let* ((ptype (mk-predtype (adt-type-name adt)))
	 (*common-accessor-types* nil)
	 (common-accessors (when (cdr (constructors adt))
			     (generate-common-accessors adt)))
	 (last (car (last (constructors adt)))))
    (dolist (fm (formals adt))
      (unless (typep fm 'importing)
	(setf (module fm) adt)))
    (dolist (c (constructors adt))
      (dolist (arg (arguments c))
	(setf (module arg) adt))
      (let ((rec (generate-adt-recognizer (recognizer c) c ptype)))
	(setf (chain? rec) (not (eq c last)))))
    (generate-adt-subtypes adt)
    (dolist (c (constructors adt))
      (generate-adt-constructor c)
      (generate-accessors c adt common-accessors))))


;;; Generate the subtype type declarations

(defmethod generate-adt-subtypes ((adt datatype))
  nil)

(defmethod generate-adt-subtypes ((adt datatype-with-subtypes))
  (dolist (subtype (subtypes adt))
    (let* ((pred-decl (mk-adt-subtype-pred subtype (constructors adt)))
	   (*adt-decl* pred-decl))
      (typecheck-adt-decl pred-decl)
      (put-decl pred-decl (current-declarations-hash))
      (let* ((sdecl (mk-adt-subtype-decl
		     subtype (mk-adt-subtype subtype (constructors adt))))
	     (*adt-decl* sdecl))
	(typecheck-adt-decl sdecl)
	(put-decl sdecl (current-declarations-hash))
	(typecheck* subtype nil nil nil)))))

(defun mk-adt-subtype-pred (subtype constructors &optional preds)
  (if (null constructors)
      (let* ((var (make-new-variable '|x| preds))
	     (bd (mk-bind-decl var (adt-type-name *adt*)))
	     (appreds (mapcar #'(lambda (p)
				  (mk-application p (mk-name-expr var)))
			      (nreverse preds))))
	(mk-adt-def-decl (id subtype) (copy *boolean*)
			 (mk-disjunction appreds) (list bd)))
      (mk-adt-subtype-pred subtype (cdr constructors)
			   (if (same-id subtype (subtype (car constructors)))
			       (cons (recognizer (car constructors)) preds)
			       preds))))

(defun mk-adt-subtype (subtype constructors &optional preds)
  (if (null constructors)
      (let* ((var (make-new-variable '|x| preds))
	     (bd (mk-bind-decl var (adt-type-name *adt*)))
	     (appreds (mapcar #'(lambda (p)
				  (mk-application p (mk-name-expr var)))
			      (nreverse preds))))
	(mk-setsubtype (adt-type-name *adt*)
		       (mk-lambda-expr (list bd)
			 (mk-disjunction appreds))))
      (mk-adt-subtype subtype (cdr constructors)
		      (if (same-id subtype (subtype (car constructors)))
				 (cons (recognizer (car constructors)) preds)
				 preds))))

(defun mk-adt-subtype-decl (subtype-name subtype)
  (mk-type-decl (id subtype-name) 'type-eq-decl subtype))


;;; Generate the recognizer type declarations

(defun generate-adt-recognizers (adt)
  (let* ((ptype (mk-predtype (adt-type-name adt)))
	 (recs (mapcar #'(lambda (c)
			   (generate-adt-recognizer (recognizer c) c ptype))
		       (constructors adt))))
    (mapc #'(lambda (r) (setf (chain? r) t)) recs)
    (setf (chain? (car (last recs))) nil)
    recs))

(defun generate-adt-recognizer (rec constr ptype)
  (let* ((rdecl (mk-adt-recognizer-decl rec ptype (ordnum constr)))
	 (*adt-decl* rdecl))
    (typecheck-adt-decl rdecl)
    (put-decl rdecl (current-declarations-hash))
    (setf (rec-decl constr) rdecl)))


;;; Generate the constructor constant declarations.

(defun generate-adt-constructors (adt)
  (mapc #'(lambda (c)
	    (generate-adt-constructor c))
	(constructors adt)))

(defun generate-adt-constructor (constr)
  (let* ((ftype (generate-adt-constructor-type constr))
	 (cdecl (mk-adt-constructor-decl (id constr) ftype (ordnum constr)))
	 (*adt-decl* constr)
	 (*generate-tccs* 'none))
    (typecheck-adt-decl cdecl)
    (setf (con-decl constr) cdecl)))

(defun generate-adt-constructor-type (constr)
  (if (arguments constr)
      (let ((type (mk-funtype (generate-adt-constructor-domain
			       (arguments constr))
			      (mk-expr-as-type
			       (mk-unique-name-expr
				(recognizer constr)
				(type (rec-decl constr)))))))
	(typecheck type)
	type)
      (mk-expr-as-type (mk-unique-name-expr (recognizer constr)
					    (type (rec-decl constr))))))

(defun mk-unique-name-expr (id type)
  (if (> (count-if #'(lambda (d) (eq (module d) (current-theory)))
		   (gethash id (current-declarations-hash)))
	 1)
      (make-instance 'coercion
	'operator (mk-lambda-expr (list (mk-bind-decl '|x| type))
		    (mk-name-expr '|x|))
	'argument (mk-name-expr id))
      (mk-name-expr id)))

(defun generate-adt-constructor-domain (accessors &optional result)
  (if (null accessors)
      (nreverse result)
      (let* ((acc (car accessors))
	     (occ? (id-occurs-in (id acc) (cdr accessors)))
	     (atype (if occ?
			(mk-dep-binding (id acc) (declared-type acc))
			(declared-type acc)))
	     (*adt-decl* (declared-type acc)))
	(assert atype)
	(let* ((ty (typecheck (pc-parse (unparse atype :string t)
					'dep-type-expr)))
	       (*bound-variables* (if occ?
				      (cons ty *bound-variables*)
				      *bound-variables*)))
	  (generate-adt-constructor-domain (cdr accessors)
					   (cons atype result))))))


;;; Generate the accessor constant declarations.

(defun generate-common-accessors (adt)
  (dolist (c (constructors adt))
    (dolist (a (arguments c))
      (let* ((copy-type (pc-parse (unparse (declared-type a) :string t)
				  'type-expr))
	     (type (with-no-type-errors (typecheck copy-type))))
	(when type (push (cons a type) *common-accessor-types*)))))
  (let* ((args (arguments (car (constructors adt))))
	 (common-args
	  (remove-if-not
	      #'(lambda (arg)
		  (and (assq arg *common-accessor-types*)
		       (every #'(lambda (c)
				  (member arg (arguments c)
					  :test #'same-adt-arg))
			      (cdr (constructors adt)))))
	    args)))
    (generate-common-accessors* common-args args adt)))

(defun same-adt-arg (a1 a2)
  (and (same-id a1 a2)
       (let ((t1 (cdr (assq a1 *common-accessor-types*)))
	     (t2 (cdr (assq a2 *common-accessor-types*))))
	 (and t1 t2 (tc-eq t1 t2)))))

(defun generate-common-accessors* (common-args args adt &optional decls)
  (if (null common-args)
      (nreverse decls)
      (let* ((arg (car common-args))
	     (dep? (some #'(lambda (a) (id-occurs-in (id a) arg))
			 (remove arg args)))
	     (dtype (if dep?
			(mk-dep-binding (get-adt-var adt)
					(mk-type-name (id adt)))
			(mk-type-name (id adt))))
	     (ftype (mk-funtype (list dtype)
				(cdr (assq arg *common-accessor-types*))))
	     (cdecl (mk-adt-accessor-decl (id arg) ftype)))
	(typecheck-adt-decl cdecl)
	(generate-common-accessors* (cdr common-args) args adt
				    (cons (cons arg cdecl) decls)))))

(defun generate-accessors (c adt comacc)
  (let* ((rtype (mk-recognizer-type (recognizer c) adt))
	 (accdecls (generate-accessors* (arguments c)
					(get-adt-var adt)
					rtype
					comacc)))
    (mapc #'add-adt-decl accdecls)
    (setf (acc-decls c) accdecls)))

(defun mk-recognizer-type (rec-id adt)
  (let* ((rname (mk-name-expr rec-id))
	 (rpred (if (> (count-if #'(lambda (d)
				     (eq (module d) (current-theory)))
				 (gethash rec-id (current-declarations-hash)))
		       1)
		    (make-instance 'coercion
		      'operator (mk-lambda-expr
				    (list (mk-bind-decl '|x|
					    (mk-funtype
					     (mk-type-name (id adt))
					     (copy *boolean*))))
				  (mk-name-expr '|x|))
		      'argument rname)
		    rname)))
    (mk-expr-as-type rpred)))

(defun generate-accessors* (args var rtype comacc &optional pargs result)
  (if (null args)
      (nreverse result)
      (let ((cacc (assoc (car args) comacc :test #'same-adt-arg)))
	(if cacc
	    (let* ((bd (typecheck* (mk-dep-binding (id (car args))
						   (declared-type (car args))
						   (type (car args)))
				   nil nil nil))
		   (*bound-variables* (cons bd *bound-variables*)))
	      (generate-accessors* (cdr args) var rtype comacc
				   (cons (car args) pargs)
				   (cons (cdr cacc) result)))
	    (let* ((dtype (declared-type (car args)))
		   (type (typecheck* (pc-parse (unparse dtype :string t)
					       'type-expr)
				     nil nil nil))
		   (ftype (make-accessor-funtype type pargs rtype dtype var
						 result))
		   (cdecl (mk-adt-accessor-decl (id (car args)) ftype)))
	      (typecheck-adt-decl cdecl 'no)
	      (let* ((bd (typecheck* (mk-dep-binding (id (car args)) dtype
						     (type (car args)))
				     nil nil nil))
		     (*bound-variables* (cons bd *bound-variables*)))
		(generate-accessors* (cdr args)
				     var rtype comacc
				     (cons (car args) pargs)
				     (cons cdecl result))))))))

(defun make-accessor-funtype (type pargs rtype dtype var accdecls)
  (if (freevars type)
      (let* ((*generate-tccs* 'none)
	     (dom (typecheck* (mk-dep-binding var rtype) nil nil nil))
	     (*bound-variables* (cons dom *bound-variables*))
	     (vname (mk-name-expr var nil nil
				  (make-resolution dom
				    (current-theory-name) rtype)
				  'variable)))
	(dolist (ad accdecls)
	  (put-decl ad (current-declarations-hash)))
	(let* ((rng (subst-dependent-accessor-type vname pargs dtype))
	       (ftype (mk-funtype (list dom) rng)))
; 	  (dolist (ad accdecls)
; 	    (setf (gethash (id ad) (declarations *current-theory*))
; 		  (remove ad
; 			  (gethash (id ad) (declarations *current-theory*)))))
	  ftype))
      (mk-funtype (list rtype) dtype)))

(defun subst-dependent-accessor-type (var pargs dtype)
  (let ((*parsing-or-unparsing* t))
    (gensubst dtype
      #'(lambda (x) (subst-dependent-accessor-type! x var pargs))
      #'(lambda (x) (subst-dependent-accessor-type? x var pargs)))))

(defmethod subst-dependent-accessor-type? ((ex name-expr) var pargs)
  (and (typep (declaration ex) 'dep-binding)
       (member (id ex) pargs :test #'same-id)))

(defmethod subst-dependent-accessor-type? (ex var pargs)
  (declare (ignore ex var pargs))
  nil)

(defmethod subst-dependent-accessor-type! ((ex name-expr) var pargs)
  (declare (ignore pargs))
  (typecheck* (pc-parse (unparse (mk-application ex var) :string t) 'expr)
	      (type ex) nil nil))


;;; Ord function

(defun generate-adt-ord-function (adt)
  (let ((var (make-new-variable '|x| adt))
	(len (length (constructors adt)))
	(*generate-tccs* 'none))
    (typecheck-adt-decl
     (mk-const-decl '|ord|
       (make-instance 'type-application
	 'type (mk-type-name '|upto|)
	 'parameters (list (mk-number-expr (1- len))))
       (when (and *generate-all-adt-axioms*
		  (<= len *ord-definition-cutoff*))
	 (mk-cases-expr (mk-name-expr var)
	   (generate-adt-ord-selections (constructors adt))
	   nil))
       (list (mk-arg-bind-decl var (mk-type-name (id adt))))))))

(defun set-constructor-ord-numbers (constructors &optional (num 0))
  (when constructors
    (setf (ordnum (car constructors)) num)
    (set-constructor-ord-numbers (cdr constructors) (1+ num))))

(defun generate-adt-ord-selections (constructors &optional result)
  (if (null constructors)
      (nreverse result)
      (let ((sel (generate-adt-ord-selection (car constructors))))
	(generate-adt-ord-selections (cdr constructors)
				     (cons sel result)))))

(defun generate-adt-ord-selection (c)
  (if (arguments c)
      (mk-selection (mk-name-expr (id c))
	(mapcar #'(lambda (a) (change-class (copy (get-adt-var a)) 'bind-decl))
		(arguments c))
	(mk-number-expr (ordnum c)))
      (mk-selection (mk-name-expr (id c)) nil
	(mk-number-expr (ordnum c)))))


;;; Done generating the recognizer, constructor, and accessor declarations,
;;; now check the types

(defun check-adt-types (adt)
  (let ((adt-type (adt-type-name adt)))
    (check-adt-constructor-types (constructors adt))
    (dolist (c (constructors adt))
      (check-adt-type-occurrences c adt-type))))


;;; This actually typechecks the accessor types, creating bind-decls and
;;; storing them in the accessor-bindings slot of each constructor.

(defun check-adt-constructor-types (constructors)
  (when constructors
    (check-adt-accessor-types (arguments (car constructors)))
    (check-adt-constructor-types (cdr constructors))))

(defun check-adt-accessor-types (accessors &optional result)
  (if (null accessors)
      (nreverse result)
      (let* ((a (car accessors))
	     (bd (mk-bind-decl (id a)
		   (pc-parse (unparse (declared-type a) :string t)
			     'type-expr)))
	     (*adt-decl* (declared-type a)))
	(typecheck* bd nil nil nil)
	(setf (type a) (type bd)
	      (bind-decl a) bd)
	(let ((*bound-variables* (cons bd *bound-variables*)))
	  (check-adt-accessor-types (cdr accessors) (cons bd result))))))

(defun check-adt-type-occurrences (c adt-type)
  (mapc #'(lambda (a)
	    (check-adt-type-occurrences* a adt-type))
	(arguments c)))

(defun check-adt-type-occurrences* (arg adt-type)
  (let ((atype (type arg)))
    (multiple-value-bind (pos? negocc)
	(occurs-positively? adt-type atype)
      (unless pos?
	(let ((*current-theory* *adt*))
	  (setf (filename *current-theory*) *current-file*)
	  (setf (path *current-theory*) (make-specpath *current-file*))
	  (type-error (or negocc (declared-type arg))
	    "Recursive uses of the datatype ~a may not appear in:~%  ~
             the domain of a function type,~%  ~
             as a non-positive parameter to another datatype,~%  ~
             or in the predicate of a subtype."
	    (id adt-type)))))))

(defun sequence-adt? (adt-type arg-type)
  (let ((atype (find-supertype arg-type)))
    (and (typep atype 'funtype)
	 (tc-eq (domain atype) *naturalnumber*)
	 (tc-eq (range atype) adt-type))))

(defun datatype-adt? (adt-type arg-type &optional poscheck?)
  (let ((atype (find-supertype arg-type)))
    (and (typep atype 'type-name)
	 (let* ((mi (module-instance (resolution atype)))
		(th (if (same-id mi *current-theory*)
			*current-theory*
			(get-theory mi))))
	   (assert th)
	   (and (not (eq th *current-theory*))
		(generated-by th)
		(typep (get-theory (generated-by th)) 'datatype)
		(if poscheck?
		    (pos-datatype-adt? adt-type (actuals mi)
				       (get-theory (generated-by th)))
		    (member adt-type (actuals mi)
			    :test #'(lambda (aty act)
				      (and (type-value act)
					   (tc-eq (type-value act)
						  aty))))))))))

(defun pos-datatype-adt? (adt-type actuals adt)
  (pos-datype-adt?* adt-type actuals (formals-sans-usings adt) adt))

(defun pos-datype-adt?* (adt-type acts formals adt)
  (or (null acts)
      (and (or (not (occurs-in adt-type (car acts)))
	       (and (tc-eq (type-value (car acts)) adt-type)
		    (member (car formals) (positive-types adt)
			    :test #'(lambda (x y)
				      (let ((fdecl (declaration y)))
					(and (typep fdecl 'formal-type-decl)
					     (same-id x fdecl))))))
	       (and (typep (find-supertype (type-value (car acts))) 'type-name)
		    (pos-datatype-adt?
		     adt-type
		     (actuals (find-supertype (type-value (car acts))))
		     adt)))
	   (pos-datype-adt?* adt-type (cdr acts) (cdr formals) adt))))

;(defun generate-positive-adt-decls (adt)
;  (let ((every-decl (generate-adt-every adt)))
;    (setf (module every-decl) (module *current-context*))
;    (typecheck* every-decl nil nil nil)
;    (add-decl every-decl))
;  (when (all-adt-types-are-positive adt)
;    (let ((map-theory (generate-adt-map-theory adt))
;	  (reduce-theory (generate-adt-reduce-theory adt)))
;      (break "Finish this")))
;  (pvs-message "Datatype respects subtyping"))

(defun all-adt-types-are-positive (adt)
  (every #'(lambda (fml)
	     (or (not (typep fml 'type-decl))
		 (member fml (positive-types adt)
			 :test #'(lambda (x y)
				   (tc-eq (type-value x) y)))))
	 (formals (adt-theory adt))))

(defun no-adt-subtypes (adt)
  (let ((*no-adt-subtypes* t))
    (occurs-positively? (adt-type-name adt) adt)))

(defun generate-adt-axioms (adt)
  (generate-adt-extensionalities (constructors adt) adt)
  (generate-adt-accessor-axioms (constructors adt) adt)
;	  (when (cdr (constructors adt))
;	    (list (generate-disjoint-axiom adt)))
  (generate-inclusive-axiom adt))

(defun generate-adt-extensionalities (constructors adt)
  (when constructors
    (generate-adt-extensionality-axioms (car constructors) adt)
    (generate-adt-extensionalities (cdr constructors) adt)))

(defun generate-adt-extensionality-axioms (c adt)
  (generate-adt-extensionality c adt)
  (when (arguments c)
    (generate-adt-eta c adt)))

(defun generate-adt-extensionality (c adt)
  (let* ((var (mk-name-expr (makesym "~a_var" (op-to-id (recognizer c)))))
	 (var2 (copy var 'id (makesym "~a2" (id var)))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_~a_extensionality"
			       (id adt) (op-to-id (id c)))
       (mk-forall-expr (list (mk-bind-decl (id var)
			       (mk-recognizer-type (recognizer c) adt))
			     (mk-bind-decl (id var2)
			       (mk-recognizer-type (recognizer c) adt)))
	 (if (arguments c)
	     (mk-application 'IMPLIES
	       (mk-conjunction
		(mapcar #'(lambda (a)
			    (mk-application '=
			      (mk-application (id a) var)
			      (mk-application (id a) var2)))
			(arguments c)))
	       (mk-application '= var var2))
	     (mk-application '= var var2)))
       'AXIOM))))

(defun generate-adt-eta (c adt)
  (let ((var (mk-name-expr (makesym "~a_var" (op-to-id (recognizer c))))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_~a_eta" (id adt) (op-to-id (id c)))
       (mk-forall-expr (list (mk-bind-decl (id var)
			       (mk-recognizer-type (recognizer c) adt)))
	 (mk-application '=
	   (apply #'mk-application (id c)
		  (mapcar #'(lambda (a)
			      (mk-application (id a) var))
			  (arguments c)))
	   var))
       'AXIOM))))

(defun generate-adt-accessor-axioms (constructors adt)
  (when constructors
    (when (arguments (car constructors))
      (generate-accessor-axioms (car constructors) adt))
    (generate-adt-accessor-axioms (cdr constructors) adt)))

(defun generate-accessor-axioms (c adt)
  (let ((bds (mapcar #'bind-decl (arguments c))))
    (dolist (a (arguments c))
      (let* ((vars (mapcar #'get-adt-var (arguments c)))
	     (cappl (mk-application* (id c) vars))
	     (consappl (if (> (count-if #'(lambda (d)
					    (eq (module d) (current-theory)))
					(gethash (id c)
						 (current-declarations-hash)))
			      1)
			   (make-instance 'coercion
			     'operator (mk-lambda-expr
					   (list (mk-bind-decl '|x|
						   (mk-expr-as-type
						    (mk-name-expr
							(recognizer c)))))
					 (mk-name-expr '|x|))
			     'argument cappl)
			   cappl)))
	(typecheck-adt-decl
	 (mk-formula-decl (makesym "~a_~a_~a"
				   (id adt)
				   (op-to-id (id a))
				   (op-to-id (id c)))
	   (mk-forall-expr (adt-forall-bindings vars bds)
	     (mk-application '=
	       (mk-application (id a) consappl)
	       (get-adt-var a)))
	   'AXIOM)
	 t nil)))))

(defun adt-forall-bindings (vars bds &optional result)
  (if (null vars)
      (nreverse result)
      (let* ((nbd (mk-bind-decl (id (car vars))
		   (declared-type (car bds)) (type (car bds))))
	     (var (mk-name-expr (id (car vars)) nil nil
				(make-resolution nbd
				  (current-theory-name) (type nbd))
				'variable)))
	(adt-forall-bindings (cdr vars)
			     (substit (cdr bds)
			       (acons (car bds) var nil))
			     (cons nbd result)))))

(defun generate-disjoint-axiom (adt)
  (let ((tvar (mk-name-expr (makesym "~a_var" (id adt)))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_disjoint" (id adt))
       (mk-forall-expr (list (mk-bind-decl (id tvar)
			       (mk-type-name (id adt))))
	 (mk-conjunction
	  (make-disjoint-pairs
	   (mapcar #'(lambda (c) (mk-application (recognizer c) tvar))
		   (constructors adt)))))
       'AXIOM))))

(defun make-disjoint-pairs (appls &optional result)
  (if (null (cdr appls))
      result
      (make-disjoint-pairs
       (cdr appls)
       (append result
	       (mapcar #'(lambda (a)
			   (mk-negation
			    (mk-conjunction (list (car appls) a))))
		       (cdr appls))))))

(defun generate-inclusive-axiom (adt)
  (let ((tvar (mk-name-expr (makesym "~a_var" (id adt)))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_inclusive" (id adt))
       (mk-forall-expr (list (mk-bind-decl (id tvar)
			       (mk-type-name (id adt))))
	 (mk-disjunction
	  (mapcar #'(lambda (c) (mk-application (recognizer c) tvar))
		  (constructors adt))))
       'AXIOM))))

(defun generate-adt-induction (adt)
  (typecheck-adt-decl
   (mk-formula-decl (makesym "~a_induction" (id adt))
     (mk-forall-expr (list (mk-bind-decl '|p|
			     (mk-predtype (adt-type-name adt))))
       (mk-implication (gen-induction-hypothesis adt)
		       (gen-induction-conclusion adt)))
     'AXIOM)))

(defmethod gen-induction-hypothesis ((adt datatype) &optional ign)
  (declare (ignore ign))
  (mk-conjunction
   (mapcar #'(lambda (c) (gen-induction-hypothesis c adt))
	   (constructors adt))))

(defmethod gen-induction-hypothesis ((c simple-constructor) &optional adt)
  (if (arguments c)
      (let* ((arghyps (acc-induction-hypotheses (arguments c) adt))
	     (ppappl (mk-application '|p|
		       (mk-application* (id c)
			 (mapcar #'get-adt-var-name (arguments c)))))
	     (indh (mk-forall-expr (adt-forall-bindings
				    (mapcar #'get-adt-var-name (arguments c))
				    (mapcar #'bind-decl (arguments c)))
		     (if arghyps
			 (mk-implication (mk-conjunction arghyps) ppappl)
			 ppappl))))
	(pc-parse (unparse indh :string t) 'expr))
      (mk-application '|p| (mk-name-expr (id c)))))

(defun get-adt-var-name (arg)
  (make-instance 'name-expr 'id (id (get-adt-var arg))))

(defun acc-induction-hypotheses (args adt &optional result)
  (if (null args)
      (nreverse result)
      (let ((hyp (acc-induction-hypothesis (car args) adt)))
	(acc-induction-hypotheses (cdr args) adt
				  (if hyp (cons hyp result) result)))))


;;; Generate the induction hypothesis according to the type of the argument.
;;; It is either the datatype itself, a sequence of the datatype, or occurs as
;;; a positive actual parameter to a different datatype.  In every other case,
;;; return NIL.

(defun acc-induction-hypothesis (arg adt)
  (let ((pred (acc-induction-hypothesis* (type arg) adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (mk-application pred (get-adt-var-name arg)))))

(defmethod acc-induction-hypothesis* ((te type-name) adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (mk-name-expr '|p|))
	((adt? te)
	 (let* ((acts (remove-if-not #'type-value
			(actuals (module-instance te))))
		(preds (mapcar #'(lambda (act)
				   (acc-induction-hypothesis*
				    (type-value act) adt))
			       acts)))
	   (if (every #'(lambda (p) (or (everywhere-true? p) (null p)))
		      preds)
	       (mk-everywhere-true-function te)
	       (mk-application* '|every|
		 (mapcar #'(lambda (p a)
			     (or p
				 (mk-everywhere-true-function (type-value a))))
		   preds acts)))))
	(t ;;(mk-everywhere-true-function te)
	 nil)))

(defmethod acc-induction-hypothesis* ((te subtype) adt)
  (acc-induction-hypothesis* (supertype te) adt))

(defmethod acc-induction-hypothesis* ((te funtype) adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))
			     'variable))
	 (pred (acc-induction-hypothesis*
		(if (typep (domain te) 'dep-binding)
		    (substit (range te) (acons (domain te) fvar nil))
		    (range te))
		adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (if (tc-eq (domain te) *naturalnumber*)
	  (mk-application '|every| pred)
	  (let* ((lid (make-new-variable '|f| (list te pred)))
		 (lbd (make-bind-decl lid te))
		 (lvar (mk-name-expr lid nil nil
				     (make-resolution lbd
				       (current-theory-name) te)
				     'variable)))
	    (mk-lambda-expr (list lbd)
	      (mk-forall-expr (list fbd)
		(mk-application pred
		  (mk-application lvar fvar)))))))))

(defmethod acc-induction-hypothesis* ((te recordtype) adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (preds (acc-induction-fields (fields te) rvar adt (dependent? te))))
    (unless (every #'(lambda (p) (or (null p) (everywhere-true? p))) preds)
      (mk-lambda-expr (list rbd)
	(mk-conjunction
	 (mapcan #'(lambda (fd pred)
		     (when pred
		       (list (mk-application pred
			       (mk-application (id fd) rvar)))))
	   (fields te) preds))))))

(defun acc-induction-fields (fields rvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-induction-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar adt dep?
       (cons (acc-induction-hypothesis* (copy-all (type (car fields))) adt)
	     result))))

(defmethod acc-induction-hypothesis* ((te tupletype) adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)
			     'variable))
	 (preds (acc-induction-tuples (types te) tvar adt)))
    (unless (every #'null preds)
      (mk-lambda-expr (list tbd)
	(mk-conjunction
	 (let ((num 0))
	   (mapcan #'(lambda (pred)
		       (incf num)
		       (when pred
			 (list (mk-application pred
				 (make-instance 'projection-application
				   'id (makesym "PROJ_~d" num)
				   'index num
				   'argument tvar)))))
	     preds)))))))

(defun acc-induction-tuples (types tvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-induction-tuples
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       tvar adt (1+ index)
       (cons (acc-induction-hypothesis* (car types) adt) result))))

(defmethod acc-induction-hypothesis* ((te dep-binding) adt)
  (acc-induction-hypothesis* (type te) adt))

(defmethod acc-induction-hypothesis* ((te type-expr) adt)
  (declare (ignore adt))
  ;;(mk-everywhere-true-function te)
  )

(defun acc-adt-ind-hyp-preds (adt-type acts &optional result)
  (if (null acts)
      (nreverse result)
      (if (type-value (car acts))
	  (let ((npred (if (tc-eq (type-value (car acts)) adt-type)
			   (mk-name-expr '|p|)
			   (mk-everywhere-true-function
			    (type-value (car acts))))))
	    (acc-adt-ind-hyp-preds adt-type (cdr acts) (cons npred result)))
	  (acc-adt-ind-hyp-preds adt-type (cdr acts) result))))

(defun mk-everywhere-true-function (type)
  (mk-lambda-expr (list (mk-bind-decl (make-new-variable '|x| type) type))
    (copy *true*)))

(defun mk-everywhere-false-function (type)
  (mk-lambda-expr (list (mk-bind-decl (make-new-variable '|x| type) type))
    (copy *false*)))

(defmethod gen-induction-conclusion ((adt datatype))
  (let ((tvar (mk-name-expr (makesym "~a_var" (id adt)))))
    (mk-forall-expr (list (mk-bind-decl (id tvar)
			      (mk-type-name (id adt))))
      (mk-application '|p| tvar))))


;;; Generate the every and some functions - only when some formal
;;; parameter occurs positively.  This means either no occurrence, direct
;;; occurrence, or a subtype of one of these two cases.

(defun generate-adt-every (adt)
  (generate-adt-predicate adt '|every|))

(defun generate-adt-some (adt)
  (generate-adt-predicate adt '|some|))

(defun generate-adt-predicate (adt function-id)
  (let* ((ptypes (positive-types adt))
	 (pvars (generate-adt-predicate-variables adt))
	 (avar (mk-name-expr (make-new-variable '|a| adt)))
	 (cases (mk-cases-expr avar
		  (mapcar #'(lambda (c)
			      (generate-adt-predicate-selection
			       c pvars ptypes adt function-id t))
			  (constructors adt))
		  nil))
	 (cases2 (mk-cases-expr avar
		   (mapcar #'(lambda (c)
			       (generate-adt-predicate-selection
				c pvars ptypes adt function-id nil))
			   (constructors adt))
		   nil))
	 (fargs (list (mapcar #'mk-arg-bind-decl
			      (mapcar #'id pvars)
			      (mapcar #'(lambda (pty)
					  (mk-type-name 'PRED
					    (list (mk-actual pty))))
				      ptypes))
		      (list (mk-arg-bind-decl (id avar)
					      (adt-type-name adt))))))
    (typecheck-adt-decl
     (mk-adt-def-decl function-id (copy *boolean*)
		      (pc-parse (unparse cases :string t) 'expr) fargs))
    (typecheck-adt-decl
     (mk-adt-def-decl function-id (copy *boolean*)
		      (pc-parse (unparse cases2 :string t) 'expr)
		      (append (apply #'append (butlast fargs))
			      (car (last fargs)))))))

(defun generate-adt-predicate-variables (adt)
  (let ((ptypes (positive-types adt)))
    (if (singleton? ptypes)
	(list (mk-name-expr (make-new-variable '|p| adt)))
	(let ((ctr 0))
	  (mapcar #'(lambda (p)
		      (declare (ignore p))
		      (mk-name-expr (make-new-variable '|p| adt (incf ctr))))
		  ptypes)))))

(defun generate-adt-predicate-selection (c pvars ptypes adt function-id
					   curried?)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var-name a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name) (type b))
					     'variable))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (let ((exprs (mapcar #'(lambda (v)
				   (acc-predicate-selection
				    v (type v)
				    pvars ptypes adt function-id curried?))
			 vars)))
	    (if (eq function-id '|every|)
		(mk-conjunction exprs)
		(mk-disjunction exprs)))))
      (mk-selection (mk-name-expr (id c)) nil
		    (copy (if (eq function-id '|every|) *true* *false*)))))


(defmethod acc-predicate-selection (arg (te type-name) pvars ptypes adt
					funid curried?)
  (cond ((member te ptypes :test #'tc-eq)
	 (let* ((pos (position te ptypes :test #'tc-eq))
		(pvar (nth pos pvars)))
	   (mk-application pvar (copy arg))))
	((tc-eq te (adt-type-name adt))
	 (if curried?
	     (mk-application (mk-application* funid pvars) arg)
	     (mk-application* funid (append pvars (list (copy arg))))))
	((adt? te)
	 (let ((funs (mapcar #'(lambda (act)
				 (acc-predicate-selection*
				  (type-value act) pvars ptypes adt funid))
			     (positive-actuals te))))
	   (if (if (eq funid '|every|)
		   (every #'everywhere-true? funs)
		   (every #'everywhere-false? funs))
	       (call-next-method)
	       (if curried?
		   (mk-application (mk-predicate-application funid te adt funs)
		     arg)
		   (mk-predicate-application
		    funid te adt (append funs (list (copy arg))))))))
	(t (call-next-method))))

(defmethod acc-predicate-selection (arg (te subtype) pvars ptypes adt
					funid curried?)
  (acc-predicate-selection arg (supertype te) pvars ptypes adt
			   funid curried?))

(defmethod acc-predicate-selection (arg (te funtype) pvars ptypes adt
					funid curried?)
  (declare (ignore curried?))
  (if (sequence? te)
      (let ((fun (acc-predicate-selection* (range te) pvars ptypes adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (mk-application (mk-application funid fun) (copy arg))))
      (let* ((fid (make-new-variable '|x| te))
	     (fbd (make-bind-decl fid (domain te)))
	     (fvar (mk-name-expr fid nil nil
				 (make-resolution fbd
				   (current-theory-name) (domain te))
				 'variable))
	     (fun (acc-predicate-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) fvar nil))
		       (range te))
		   pvars ptypes adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (let* ()
	      (if (eq funid '|every|)
		  (mk-forall-expr (list fbd)
		    (mk-application fun
		      (mk-application (copy arg) (copy fvar))))
		  (mk-exists-expr (list fbd)
		    (mk-application fun
		      (mk-application (copy arg) (copy fvar))))))))))

(defmethod acc-predicate-selection (arg (te recordtype) pvars ptypes adt
					funid curried?)
  (declare (ignore curried?))
  (let* ((rbd (make-bind-decl (id arg) te))
	 (rvar (mk-name-expr (id arg) nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (preds (acc-predicate-fields (fields te) rvar pvars ptypes adt funid
				      (dependent? te))))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-conjunction
	 (mapcan #'(lambda (fd pred)
		     (unless (if (eq funid '|every|)
				 (everywhere-true? pred)
				 (everywhere-false? pred))
		       (list (mk-application pred
			       (mk-application (id fd) (copy arg))))))
	   (fields te) preds)))))

(defun acc-predicate-fields (fields rvar pvars ptypes adt funid dep?
				    &optional result)
  (if (null fields)
      (nreverse result)
      (acc-predicate-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar pvars ptypes adt funid dep?
       (cons (acc-predicate-selection*
	      (copy-untyped (type (car fields))) pvars ptypes adt funid)
	     result))))

(defmethod acc-predicate-selection (arg (te tupletype) pvars ptypes adt
					funid curried?)
  (declare (ignore curried?))
  (let ((preds (acc-predicate-types (types te) arg pvars ptypes adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-conjunction
	 (let ((num 0))
	   (mapcan #'(lambda (pred)
		       (incf num)
		       (unless (if (eq funid '|every|)
				   (everywhere-true? pred)
				   (everywhere-false? pred))
			 (list (mk-application pred
				 (make-instance 'projection-application
				   'id (makesym "PROJ_~d" num)
				   'index num
				   'argument (copy arg))))))
	     preds))))))

(defun acc-predicate-types (types arg pvars ptypes adt funid
				  &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-predicate-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index arg)
		    nil))
	   (cdr types))
       arg pvars ptypes adt funid (1+ index)
       (cons (acc-predicate-selection* (car types) pvars ptypes adt funid)
	     result))))

(defmethod acc-predicate-selection (arg (te type-expr) pvars ptypes adt
					funid curried?)
  (declare (ignore arg pvars ptypes adt curried?))
  (copy (if (eq funid '|every|) *true* *false*)))

(defmethod acc-predicate-selection* ((te type-name) pvars ptypes adt funid)
  (cond ((member te ptypes :test #'tc-eq)
	 (let ((pos (position te ptypes :test #'tc-eq)))
	   (nth pos pvars)))
	((tc-eq te (adt-type-name adt))
	 (mk-application* funid pvars))
	((adt? te)
	 (let ((funs (mapcar #'(lambda (act)
				 (acc-predicate-selection* (type-value act)
							   pvars ptypes
							   adt funid))
			     (positive-actuals te))))
	   (if (if (eq funid '|every|)
		   (every #'everywhere-true? funs)
		   (every #'everywhere-false? funs))
	       (call-next-method)
	       (mk-predicate-application funid te adt funs))))
	(t (call-next-method))))

(defmethod acc-predicate-selection* ((te funtype) pvars ptypes adt funid)
  (if (sequence? te)
      (let ((fun (acc-predicate-selection* (range te) pvars ptypes
					   adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (mk-application funid fun)))
      (let* ((fid (make-new-variable '|x| te))
	     (fbd (make-bind-decl fid (domain te)))
	     (fvar (mk-name-expr fid nil nil
				 (make-resolution fbd
				   (current-theory-name) (domain te))
				 'variable))
	     (fun (acc-predicate-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) fvar nil))
		       (range te))
		   pvars ptypes adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (let* ((lid (make-new-variable '|f| (list te fun)))
		   (lbd (make-bind-decl lid te))
		   (lvar (mk-name-expr lid nil nil
				       (make-resolution lbd
					 (current-theory-name) te)
				       'variable)))
	      (mk-lambda-expr (list lbd)
		(if (eq funid '|every|)
		    (mk-forall-expr (list fbd)
		      (mk-application fun
			(mk-application lvar (copy fvar))))
		    (mk-exists-expr (list fbd)
		      (mk-application fun
			(mk-application lvar (copy fvar)))))))))))

(defmethod acc-predicate-selection* ((te recordtype) pvars ptypes adt funid)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (preds (acc-predicate-fields (fields te) rvar pvars ptypes adt funid
				      (dependent? te))))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-conjunction
	   (mapcan #'(lambda (fd pred)
		       (unless (if (eq funid '|every|)
				   (everywhere-true? pred)
				   (everywhere-false? pred))
			 (list (mk-application pred
				 (mk-application (id fd) rvar)))))
	     (fields te) preds))))))

(defmethod acc-predicate-selection* ((te tupletype) pvars ptypes adt funid)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)
			     'variable))
	 (preds (acc-predicate-types (types te) tvar pvars ptypes adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (mk-conjunction
	   (let ((num 0))
	     (mapcan #'(lambda (pred)
			 (incf num)
			 (unless (if (eq funid '|every|)
				     (everywhere-true? pred)
				     (everywhere-false? pred))
			   (list (mk-application pred
				   (make-instance 'projection-application
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument tvar)))))
	       preds)))))))

(defmethod acc-predicate-selection* ((te dep-binding) pvars ptypes adt funid)
  (acc-predicate-selection* (type te) pvars ptypes adt funid))

(defmethod acc-predicate-selection* ((te type-expr) pvars ptypes adt funid)
  (declare (ignore pvars ptypes adt))
  (if (eq funid '|every|)
      (mk-everywhere-true-function te)
      (mk-everywhere-false-function te)))


;;; Generate the map function - only when all parameters occur positively.
;;; This generates a new theory.

(defun generate-adt-map-theory (adt)
  (when (and (some #'(lambda (ff) (typep ff 'formal-type-decl))
		   (formals adt))
	     (or (all-adt-types-are-positive adt)
		 (pvs-warning
		  "No map is generated since some formal parameter is not positive"))
	     (or (no-adt-subtypes adt)
		 (pvs-warning
		  "No map generated - some accessor has type involving a subtype of the datatype.")))
    (build-adt-theory (makesym "~a_adt_map" (id adt)) adt
      (let ((fpairs (adt-map-formal-pairs (formals (adt-theory adt)))))
	(generate-adt-map-formals fpairs)
	(generate-adt-map-using adt)
	(generate-adt-map fpairs adt)))))

(defun adt-map-formal-pairs (formals &optional pairs)
  (if (null formals)
      (nreverse pairs)
      (if (typep (car formals) 'importing)
	  (let ((nusing (gensubst (car formals)
			  #'(lambda (x)
			      (copy x
				'id (id (cdr (assoc x pairs
						    :test #'same-id)))))
			  #'(lambda (x)
			      (and (typep x 'name)
				   (assoc x pairs
					  :test #'same-id))))))
	    (adt-map-formal-pairs (cdr formals)
				  (if nil ;(eq nusing (car formals))
				      pairs
				      (acons (car formals) nusing pairs))))
	  (let* ((fml (copy-adt-formals (car formals)))
		 (nid (make-new-variable (id fml)
			(cons formals pairs)))
		 (nfml (typecase fml
			 (formal-subtype-decl
			  (copy-adt-formals
			   (copy (car formals)
			     'id nid
			     'type-expr (adt-subst-declared-type
					 (type-expr fml) pairs))))
			 (formal-type-decl
			  (copy-adt-formals
			   (copy fml 'id nid)))
			 (formal-const-decl
			  (copy-adt-formals
			   (copy (car formals)
			     'id nid
			     'declared-type (adt-subst-declared-type
					     (declared-type fml) pairs))))
			 (t (break "Class not handled")))))
	    (adt-map-formal-pairs (cdr formals)
				  (acons fml nfml pairs))))))

(defun adt-subst-declared-type (dtype formal-pairs)
  (gensubst dtype
    #'(lambda (x) (copy x
		    'id (id (cdr (assoc x formal-pairs :test #'same-id)))))
    #'(lambda (x) (and (typep x 'name)
		       (assoc x formal-pairs :test #'same-id)))))

(defun generate-adt-map-formals (fpairs)
  (let ((formals (append (mapcar #'car fpairs)
			 (mapcar #'cdr (remove-if #'(lambda (x)
						      (eq (car x) (cdr x)))
					 fpairs)))))
    (setf (formals *current-theory*) formals)
    (setf (formals-sans-usings *current-theory*)
	  (remove-if #'importing? (formals *current-theory*)))
    (mapc #'(lambda (fm)
	      (let ((*adt-decl* fm))
		;;(setf (abstract-syntax fm) nil)
		(typecheck-adt-decl fm nil)))
	  formals)
    (set-dependent-formals (formals-sans-usings *current-theory*))))

(defun generate-adt-map-using (adt)
  (let* ((mod (mk-modname (makesym "~a_adt" (id adt))))
	 (imp (make-instance 'importing
		'theory-name mod)))
    (dolist (im (importings adt))
      (typecheck-adt-decl im))
    (typecheck-adt-decl imp)))

(defun generate-adt-map (fpairs adt)
  (let* ((fptypes (remove-if-not #'(lambda (fp) (typep (car fp) 'type-decl))
		    fpairs))
	 (ptypes (mapcar #'type-value (mapcar #'car fptypes)))
	 (fvars (generate-adt-map-fvars fptypes adt))
	 (avar (mk-name-expr (make-new-variable '|a| adt)))
	 (rtype (mk-type-name (id adt)
		  (mapcan #'(lambda (fp)
			      (unless (typep (car fp) 'importing)
				(list (mk-actual
				       (mk-name-expr (id (cdr fp)))))))
			  fpairs)))
	 (adtinst (mk-map-adtinst fpairs adt))
	 (curried-cases
	  (mk-cases-expr avar
	    (mapcar #'(lambda (c)
			(generate-adt-map-selection
			 c fvars ptypes fpairs adt adtinst t))
		    (constructors adt))
	    nil))
	 (uncurried-cases
	  (mk-cases-expr avar
	    (mapcar #'(lambda (c)
			(generate-adt-map-selection
			 c fvars ptypes fpairs adt adtinst nil))
		    (constructors adt))
	    nil))
	 (fargs (adt-map-formals-arguments fvars fptypes fpairs avar adt))
	 (cdecl (mk-adt-def-decl
		    '|map| rtype
		    (pc-parse (unparse curried-cases :string t) 'expr)
		    fargs))
	 (uncdecl (mk-adt-def-decl
		      '|map| rtype
		      (pc-parse (unparse uncurried-cases :string t) 'expr)
		      (append (apply #'append (butlast fargs))
			      (car (last fargs))))))
    (typecheck-adt-decl cdecl)
    (typecheck-adt-decl uncdecl)))

(defun mk-map-adtinst (fpairs adt)
  (typecheck (mk-type-name (id adt)
	       (mapcar #'(lambda (fp)
			   (mk-actual (mk-name-expr (id (car fp)))))
		       fpairs))))

(defun adt-map-formals-arguments (fvars fptypes fpairs avar adt)
  (list (mapcar #'mk-arg-bind-decl
		(mapcar #'id fvars)
		(mapcar #'(lambda (fp)
			    (mk-funtype (list
					 (copy (type-value (car fp))
					   'resolutions nil))
					(type-value (cdr fp))))
			fptypes))
	(list (mk-arg-bind-decl (id avar)
		(mk-type-name (id adt)
			      (mapcan #'(lambda (fp)
					  (unless (typep (car fp) 'importing)
					    (list (mk-actual
						   (mk-name-expr
						       (id (car fp)))))))
				      fpairs))))))

(defun generate-adt-map-fvars (fptypes adt)
  (if (singleton? fptypes)
	(list (mk-name-expr (make-new-variable '|f| adt)))
	(generate-adt-map-fvars* fptypes adt 0 nil)))

(defun generate-adt-map-fvars* (fptypes adt ctr fvars)
  (if (null fptypes)
      (nreverse fvars)
      (generate-adt-map-fvars*
       (cdr fptypes) adt (1+ ctr)
       (cons (mk-name-expr (make-new-variable '|f| (cons adt fvars)
						(incf ctr)))
	     fvars))))

(defun generate-adt-map-selection (c pvars ptypes fpairs adt adtinst curried?)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name) (type b))
					     'variable))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (mk-application* (id c)
	    (mapcar #'(lambda (a)
			(acc-map-selection a
					   (subst-mod-params 
					    (type a)
					    (module-instance adtinst))
					   pvars ptypes fpairs adt curried?))
	      vars))))
      (mk-selection (mk-name-expr (id c)) nil
	(mk-name-expr (id c)))))

(defun instantiate-adt-types (type adtinst)
  (let ((*parsing-or-unparsing* t))
    (gensubst type
      #'(lambda (x) (instantiate-adt-types! x adtinst))
      #'(lambda (x) (instantiate-adt-types? x adtinst)))))

(defmethod instantiate-adt-types? ((type type-name) adtinst)
  (same-id type adtinst))

(defmethod instantiate-adt-types? ((act actual) adtinst)
  (and (type-value act)
       (instantiate-adt-types? (type-value act) adtinst)))

(defmethod instantiate-adt-types? (obj adtinst)
  (declare (ignore obj adtinst)))

(defmethod instantiate-adt-types! ((type type-name) adtinst)
  (copy-all adtinst))

(defmethod instantiate-adt-types! ((act actual) adtinst)
  (let* ((ntype (instantiate-adt-types! (type-value act) adtinst))
	 (nexpr (mk-name-expr (id ntype) (actuals ntype))))
    (setf (types nexpr) (list 'type)
	  (resolutions nexpr) (list (resolution ntype)))
    (make-instance 'actual
      'type-value ntype
      'expr nexpr)))

(defmethod acc-map-selection (arg (te type-name) pvars ptypes fpairs
				  adt curried?)
  (cond ((member te ptypes :test #'corresponding-formals)
	 (let* ((pos (position te ptypes :test #'corresponding-formals))
		(pvar (nth pos pvars)))
	   (mk-application pvar (copy arg))))
	((same-declaration te (adt-type-name adt))
	 (if curried?
	     (mk-application (mk-application* '|map| pvars) (copy arg))
	     (mk-application* '|map| (append pvars (list (copy arg))))))
	((adt? te)
	 (let ((maps (mapcar #'(lambda (act)
				 (acc-map-selection* (type-value act)
						     pvars ptypes fpairs adt))
			     (positive-actuals te))))
	   (if (every #'identity-fun? maps)
	       (copy arg)
	       (if curried?
		   (mk-application (mk-map-application te fpairs adt maps)
		     (copy arg))
		   (mk-map-application te fpairs adt
				       (append maps (list (copy arg))))))))
	(t (copy arg))))

(defmethod corresponding-formals (t1 (t2 type-name))
  (and (eq (id t1) (id t2))
       (typep (declaration (car (resolutions t1))) 'formal-type-decl)
       (typep (declaration (car (resolutions t1))) 'formal-type-decl)))

(defmethod corresponding-formals (t1 (t2 formal-type-decl))
  (and (eq (id t1) (id t2))
       (typep (declaration (car (resolutions t1))) 'formal-type-decl)))

(defmethod corresponding-formals (t1 (t2 subtype))
  (and (print-type t2)
       (corresponding-formals t1 (print-type t2))))

(defmethod acc-map-selection (arg (te subtype) pvars ptypes fpairs
				  adt curried?)
  (acc-map-selection arg (supertype te) pvars ptypes fpairs adt curried?))

(defmethod acc-map-selection (arg (te funtype) pvars ptypes fpairs
				  adt curried?)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))
			     'variable))
	 (map (acc-map-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       pvars ptypes fpairs adt)))
    (cond ((identity-fun? map)
	   (copy arg))
	  ((sequence? te)
	   (if curried?
	       (mk-application (mk-map-application te fpairs adt (list map))
		 (copy arg))
	       (mk-map-application te fpairs adt
				   (list map (copy arg)))))
	  (t
	   (if (or curried?
		   (not (map-application? map)))
	       (mk-lambda-expr (list fbd)
		 (mk-application map 
		   (mk-application (copy arg) fvar)))
	       (mk-lambda-expr (list fbd)
		 (mk-map-application
		  (range te) fpairs adt
		  (append (arguments map)
			  (list (mk-application (copy arg) fvar))))))))))

(defmethod acc-map-selection (arg (te recordtype) pvars ptypes fpairs
				  adt curried?)
  (declare (ignore curried?))
  (assert (type arg))
  (let ((maps (acc-map-selection-fields
	       (fields te) arg pvars ptypes fpairs adt (dependent? te))))
    (if (every #'identity-fun? maps)
	(copy arg)
	(mk-record-expr
	 (mapcar #'(lambda (fd map)
		     (mk-assignment 'uni
		       (list (list (mk-name-expr (id fd))))
		       (if (identity-fun? map)
			   (mk-application (id fd) (copy arg))
			   (mk-application map
			     (mk-application (id fd) (copy arg))))))
	   (fields te) maps)))))

(defun acc-map-selection-fields (fields rvar pvars ptypes fpairs adt dep?
					&optional result)
  (if (null fields)
      (nreverse result)
      (acc-map-selection-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar pvars ptypes fpairs adt dep?
       (cons (acc-map-selection* (type (car fields)) pvars ptypes fpairs adt)
	     result))))

(defmethod acc-map-selection (arg (te tupletype) pvars ptypes fpairs
				  adt curried?)
  (declare (ignore curried?))
  (let ((maps (acc-map-selection-types
	       (types te) arg pvars ptypes fpairs adt)))
    (if (every #'identity-fun? maps)
	(copy arg)
	(mk-tuple-expr
	 (let ((num 0))
	   (mapcar #'(lambda (map)
		       (incf num)
		       (let ((proj (make-instance 'projection-application
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument (copy arg))))
			 (if (identity-fun? map)
			     proj
			     (mk-application map proj))))
	     maps))))))

(defmethod acc-map-selection (arg (te type-expr) pvars ptypes fpairs
				  adt curried?)
  (declare (ignore pvars ptypes fpairs adt curried?))
  (copy arg))

(defmethod acc-map-selection* ((te type-name) pvars ptypes fpairs adt)
  (cond ((member te ptypes :test #'corresponding-formals)
	 (let* ((pos (position te ptypes :test #'corresponding-formals))
		(pvar (nth pos pvars)))
	   pvar))
	((tc-eq te (adt-type-name adt))
	 (mk-application* '|map| pvars))
	((adt? te)
	 (let ((maps (mapcar #'(lambda (act)
				 (acc-map-selection* (type-value act)
						     pvars ptypes fpairs adt))
			     (positive-actuals te))))
	   (if (every #'identity-fun? maps)
	       (mk-identity-fun te)
	       (mk-map-application te fpairs adt maps))))
	(t (mk-identity-fun te))))

(defmethod acc-map-selection* ((te subtype) pvars ptypes fpairs adt)
  (acc-map-selection* (supertype te) pvars ptypes fpairs adt))

(defmethod acc-map-selection* ((te funtype) pvars ptypes fpairs adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))
			     'variable))
	 (map (acc-map-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       pvars ptypes fpairs adt)))
    (cond ((identity-fun? map)
	   (mk-identity-fun te))
	  ((sequence? te)
	   (mk-map-application te fpairs adt (list map)))
	  (t
	   (let* ((lid (make-new-variable '|f| (list te map)))
		  (lbd (make-bind-decl lid te))
		  (lvar (mk-name-expr lid nil nil
				      (make-resolution lbd
					(current-theory-name) te)
				      'variable)))
	     (mk-lambda-expr (list lbd)
	       (mk-lambda-expr (list fbd)
		 (mk-application map
		   (mk-application lvar fvar)))))))))

(defmethod acc-map-selection* ((te recordtype) pvars ptypes fpairs adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (maps (acc-map-selection-fields
		(fields te) rvar pvars ptypes fpairs adt (dependent? te))))
    (if (every #'identity-fun? maps)
	(mk-identity-fun te)
	(mk-lambda-expr (list rbd)
	  (mk-record-expr
	   (mapcar #'(lambda (fd map)
		       (mk-assignment 'uni
			 (list (list (mk-name-expr (id fd))))
			 (if (identity-fun? map)
			     (mk-application (id fd) rvar)
			     (mk-application map
			       (mk-application (id fd) rvar)))))
	     (fields te) maps))))))

(defmethod acc-map-selection* ((te tupletype) pvars ptypes fpairs adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)
			     'variable))
	 (maps (acc-map-selection-types
		(types te) tvar pvars ptypes fpairs adt)))
    (if (every #'identity-fun? maps)
	(mk-identity-fun te)
	(mk-lambda-expr (list tbd)
	  (mk-tuple-expr
	   (let ((num 0))
	     (mapcar #'(lambda (map)
			 (incf num)
			 (let ((proj (make-instance 'projection-application
				       'id (makesym "PROJ_~d" num)
				       'index num
				       'argument tvar)))
			   (if (identity-fun? map)
			       proj
			       (mk-application map proj))))
	       maps)))))))

(defun acc-map-selection-types (types tvar pvars ptypes fpairs adt
				      &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-map-selection-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       tvar pvars ptypes fpairs adt (1+ index)
       (cons (acc-map-selection* (car types) pvars ptypes fpairs adt)
	     result))))

(defmethod acc-map-selection* ((te dep-binding) pvars ptypes fpairs adt)
  (acc-map-selection* (type te) pvars ptypes fpairs adt))

(defmethod acc-map-selection* ((te type-expr) pvars ptypes fpairs adt)
  (declare (ignore pvars ptypes fpairs adt))
  (mk-identity-fun te))

;(defmethod acc-map-selection (arg (te tupletype) pvars ptypes adt)
;  (labels ((map-tup (types i forms)
;	      (if (null types)
;		  (mk-conjunction (nreverse forms))
;		  (let ((proj (make-projection arg i (car types) te)))
;		    (map-tup (cdr types)
;			       (1+ i)
;			       (cons (acc-map-selection
;				      proj (car types) pvars ptypes adt)
;				     forms))))))
;    (map-tup (types te) 1 nil)))

;(defmethod acc-map-selection (arg (te recordtype) pvars ptypes adt)
;  (labels ((map-rec (fields forms)
;	      (if (null fields)
;		  (mk-conjunction (nreverse forms))
;		  (let ((fappl (make-application* (id (car fields)) arg)))
;		    (map-tup (cdr fields)
;			       (cons (acc-map-selection
;				      fappl (type (car fields))
;				      pvars ptypes adt)
;				     forms))))))
;    (map-rec (fields te) nil)))

;(defmethod acc-map-selection (arg (te funtype) pvars ptypes adt)
;  ;; adt and type parameters must not occur in the domain
;  (let* ((dtypes (domain te))
;	 (bids (if (singleton? dtypes)
;		   (list (make-new-variable '|d| adt))
;		   (mapcar #'(lambda (d)
;			       (declare (ignore d))
;			       (make-new-variable '|d| adt 1))
;			   dtypes)))
;	 (bds (mapcar #'(lambda (bid dty)
;			  (typecheck* (mk-bind-decl bid dty dty) nil nil nil))
;		      bids dtypes))
;	 (vars (mapcar #'(lambda (bd)
;			   (mk-name-expr (id bd)
;			     nil nil (make-resolution bd nil (type bd))))
;		       bds))
;	 (appl (mk-application arg vars)))
;    (make-forall-expr bds
;      (acc-map-selection appl (range te) pvars ptypes adt))))


;;; Theory for the reduce function

(defun generate-adt-reduce-theory (adt)
  (build-adt-theory (makesym "~a_adt_reduce" (id adt)) adt
    (let ((ran (make-new-variable '|range| adt)))
      (generate-adt-reduce-formals adt ran)
      (let ((adtinst (generate-adt-reduce-using adt)))
	(generate-adt-reduce adt ran adtinst)))))

(defun generate-adt-reduce-formals (adt ran)
  (let ((formals (append (mapcar #'copy-adt-formals (formals adt))
			 (list (make-instance 'formal-type-decl
				 'id ran)))))
    (setf (formals *current-theory*) formals)
    (setf (formals-sans-usings *current-theory*)
	  (remove-if #'importing? (formals *current-theory*)))
    (mapc #'(lambda (fm)
	      (let ((*adt-decl* fm))
		;;(setf (abstract-syntax fm) nil)
		(typecheck-adt-decl fm nil)))
	  formals)
    (set-dependent-formals (formals-sans-usings *current-theory*))))

(defmethod copy-adt-formals ((formal importing))
  (pc-parse (unparse formal :string t) 'using))

(defmethod copy-adt-formals ((formal formal-type-decl))
  (car (pc-parse (unparse formal :string t) 'theory-formal)))

(defmethod copy-adt-formals ((formal formal-const-decl))
  (car (pc-parse (unparse formal :string t) 'theory-formal)))

(defun generate-adt-reduce-using (adt)
  (let* ((mod (mk-modname (makesym "~a_adt" (id adt))
		(mapcar #'(lambda (ff)
			    (mk-actual (mk-name-expr (id ff))))
		  (formals-sans-usings (adt-theory adt)))))
	 (imp (make-instance 'importing
		'theory-name mod)))
    (dolist (im (importings adt))
      (let ((nthname (copy-untyped (theory-name im))))
	(typecheck-adt-decl (copy im 'theory-name nthname))))
    (typecheck-adt-decl imp)
    (subst-mod-params (adt-type-name adt) mod)))

;;; Was generate-adt-recursion

(defun generate-adt-reduce (adt ran &optional adtinst)
  (let* ((rtype (typecheck* (mk-type-name ran) nil nil nil))
	 (atype (mk-type-name (id adt)))
	 (fname (case ran
		  (|nat| '|reduce_nat|)
		  (|ordinal| '|reduce_ordinal|)
		  (t '|reduce|)))
	 (fdoms (gen-adt-reduce-domains rtype adt adtinst))
	 (thinst (when adtinst (module-instance adtinst)))
	 (cdecl (mk-adt-def-decl fname
		  (mk-funtype (list atype) rtype)
		  (pc-parse (unparse (gen-adt-reduce-definition adt fname fdoms thinst)
			      :string t) 'expr)
		  (mapcar #'(lambda (c)
			      (mk-arg-bind-decl (makesym "~a_fun"
						     (op-to-id (recognizer c)))
				(gen-adt-reduce-funtype c rtype adt adtinst)))
			  (constructors adt))))
	 (fname2 (case ran
		  (|nat| '|REDUCE_nat|)
		  (|ordinal| '|REDUCE_ordinal|)
		  (t '|REDUCE|)))
	 (rdecl (mk-adt-def-decl fname2
		  (mk-funtype (list atype) rtype)
		  (pc-parse (unparse (gen-adt-reduce-definition2 adt fname2 fdoms thinst)
			      :string t) 'expr)
		  (mapcar #'(lambda (c)
			      (mk-arg-bind-decl (makesym "~a_fun"
						     (op-to-id (recognizer c)))
				(gen-adt-reduce-funtype2 c rtype adt adtinst)))
			  (constructors adt)))))
    (typecheck-adt-decl cdecl)
    (typecheck-adt-decl rdecl)))

(defun gen-adt-reduce-domains (rtype adt adtinst)
  (mapcar #'(lambda (c)
	      (gen-adt-reduce-domains* c rtype adt adtinst))
	  (constructors adt)))

(defun gen-adt-reduce-domains* (c rtype adt adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'bind-decl (arguments c)))
	     (free-params (free-params bindings))
	     (nbindings (if (every #'(lambda (fp)
				       (memq fp (formals *current-theory*)))
				   free-params)
			    bindings
			    (gen-adt-reduce-funtype-bindings bindings)))
	     (dom (gen-adt-reduce-domain
		   (adt-dependent-bindings
		    (mapcar #'get-adt-var-name (arguments c))
		    nbindings)
		   rtype adt adtinst)))
	(typecheck* dom nil nil nil)
	(set-adt-reduce-dom-types dom)
	dom)
      nil))

(defun set-adt-reduce-dom-types (dom)
  (when dom
    (set-type* (car dom) nil)
    (let ((*bound-variables* (if (typep (car dom) 'dep-binding)
				 (cons (car dom) *bound-variables*)
				 *bound-variables*)))
      (set-adt-reduce-dom-types (cdr dom)))))

(defun gen-adt-reduce-definition (adt fname fdoms adtinst)
  (let ((avar (mk-name-expr (makesym "~a_adtvar" (id adt))))
	(funlist (mapcar #'(lambda (c)
			     (let ((rid (op-to-id (recognizer c))))
			       (mk-name-expr (makesym "~a_fun" rid))))
			 (constructors adt))))
    (mk-lambda-expr (list (mk-bind-decl (id avar)
			      (mk-type-name (id adt))))
      (mk-cases-expr avar
	(mapcar #'(lambda (c fdom)
		    (gen-adt-reduce-selection c adt funlist fname fdom
					      adtinst))
		(constructors adt) fdoms)
	nil))))

(defun gen-adt-reduce-selection (c adt funlist fname fdom adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name) (type b))
					     'variable))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (mk-application* (makesym "~a_fun" (op-to-id (recognizer c)))
	    (mapcar #'(lambda (v fd)
			(acc-reduce-selection
			 v
			 (if adtinst
			     (subst-mod-params (type v) adtinst)
			     (type v))
			 funlist fname
			 (if (typep fd 'dep-binding)
			     (type fd)
			     fd)
			 (if adtinst
			     (subst-mod-params (adt-type-name adt) adtinst)
			     (adt-type-name adt))))
	      vars fdom))))
      (mk-selection
	  (mk-name-expr (id c)) nil
	  (mk-name-expr (makesym "~a_fun" (op-to-id (recognizer c)))))))

(defmethod acc-reduce-selection (arg (te type-name) funlist fname fdom adt)
  (cond ((tc-eq te adt)
	 (mk-application (mk-application* fname funlist) (copy arg)))
	((adt? te)
	 (let* ((acts (actuals (module-instance te)))
		(facts (if (typep fdom 'datatype-subtype)
			   (actuals (declared-type fdom))
			   (actuals fdom)))
		(funs (acc-reduce-sel-acts acts facts fname funlist adt)))
	   (if (every #'identity-fun? funs)
	       (copy arg)
	       (let* ((appname (mk-name-expr '|map|
				 (append (actuals te) facts)))
		      (pname (pc-parse (unparse appname :string t) 'expr)))
		 (mk-application (mk-application* pname funs)
		   (copy arg))))))
	(t (copy arg))))

(defun acc-reduce-sel-acts (acts facts fname funlist adt &optional result)
  (if (null acts)
      (nreverse result)
      (if (type-value (car acts))
	  (let ((nfun (acc-reduce-selection*
		       (type-value (car acts))
		       funlist fname (type-value (car facts)) adt)))
	    (acc-reduce-sel-acts (cdr acts) (cdr facts) fname funlist adt
				 (cons nfun result)))
	  (acc-reduce-sel-acts (cdr acts) (cdr facts) fname funlist adt
			       result))))

(defmethod acc-reduce-selection (arg (te subtype) funlist fname fdom adt)
  (acc-reduce-selection arg (supertype te) funlist fname
			(if (typep fdom '(and subtype (not datatype-subtype)))
			    (supertype fdom)
			    fdom)
			adt))

(defmethod acc-reduce-selection (arg (te funtype) funlist fname fdom adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain fdom)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain fdom))
			     'variable))
	 (fun (acc-reduce-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       funlist fname
	       (if (typep (domain fdom) 'dep-binding)
		   (substit (range fdom) (acons (domain fdom) fvar nil))
		   (range fdom))
	       adt)))
    (if (identity-fun? fun)
	(copy arg)
	(if (sequence? te)
	    (mk-application (mk-application '|map| fun) arg)
	    (mk-lambda-expr (list fbd)
	      (mk-application fun
		(mk-application (copy arg) fvar)))))))

(defmethod acc-reduce-selection (arg (te recordtype) funlist fname fdom adt)
  (assert (typep fdom 'recordtype))
  (let ((funs (acc-reduce-selection-fields
	       (fields te) (fields fdom) arg funlist fname fdom adt
	       (dependent? te) (dependent? fdom))))
    (if (every #'identity-fun? funs)
	(copy arg)
	(mk-record-expr
	 (mapcar #'(lambda (fd fun)
		     (mk-assignment 'uni
		       (list (list (mk-name-expr (id fd))))
		       (if (identity-fun? fun)
			   (mk-application (id fd) (copy arg))
			   (mk-application fun
			     (mk-application (id fd) (copy arg))))))
	   (fields fdom) funs)))))

(defun acc-reduce-selection-fields (fields dfields rvar funlist fname fdom adt
					   dep1? dep2? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-reduce-selection-fields
       (if dep1?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       (if dep2?
	   (substit (cdr dfields)
	     (acons (car dfields)
		    (make-field-application (car dfields) rvar)
		    nil))
	   (cdr dfields))
       rvar funlist fname fdom adt dep1? dep2?
       (cons (acc-reduce-selection*
	      (type (car fields)) funlist fname (type (car dfields)) adt)
	     result))))

(defmethod acc-reduce-selection (arg (te tupletype) funlist fname fdom adt)
  (assert (typep fdom 'tupletype))
  (let ((funs (acc-reduce-selection-types
	       (types te) (types fdom) arg funlist fname adt)))
    (if (every #'identity-fun? funs)
	(copy arg)
	(mk-tuple-expr
	 (let ((num 0))
	   (mapcar #'(lambda (fun)
		       (incf num)
		       (let ((proj (make-instance 'projection-application
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument (copy arg))))
			 (if (identity-fun? fun)
			     proj
			     (mk-application fun proj))))
	     funs))))))

(defmethod acc-reduce-selection (arg (te type-expr) funlist fname fdom adt)
  (declare (ignore funlist fname fdom adt))
  (copy arg))

(defmethod acc-reduce-selection* ((te type-name) funlist fname fte adt)
  (cond ((tc-eq te adt)
	 (mk-application* fname funlist))
	((adt? te)
	 (let* ((acts (actuals (module-instance te)))
		(funs (acc-reduce-sel-acts acts (actuals fte)
					   fname funlist adt)))
	   (if (every #'identity-fun? funs)
	       (mk-identity-fun te)
	       (mk-application* '|map| funs))))
	(t (mk-identity-fun fte))))

(defmethod acc-reduce-selection* ((te subtype) funlist fname fdom adt)
  (acc-reduce-selection* (find-supertype te) funlist fname
			 (find-declared-adt-supertype fdom) adt))

(defmethod acc-reduce-selection* ((te funtype) funlist fname fdom adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain fdom)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain fdom))
			     'variable))
	 (fun (acc-reduce-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       funlist fname
	       (if (typep (domain fdom) 'dep-binding)
		   (substit (range fdom) (acons (domain fdom) fvar nil))
		   (range fdom))
	       adt)))
    (cond ((identity-fun? fun)
	   (mk-identity-fun fdom))
	  ((sequence? te)
	   (mk-application '|map|
	     (mk-application* fun funlist)))
	  (t
	   (let* ((aid (make-new-variable '|a| (list te fun adt)))
		  (abd (make-bind-decl aid te))
		  (avar (mk-name-expr aid nil nil
				      (make-resolution abd
					(current-theory-name) te)
				      'variable)))
	     (mk-lambda-expr (list abd)
	       (mk-lambda-expr (list fbd)
		 (mk-application fun
		   (mk-application avar fvar)))))))))

(defmethod acc-reduce-selection* ((te recordtype) funlist fname fdom adt)
  (assert (typep fdom 'recordtype))
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (funs (acc-reduce-selection-fields
		(fields te) (fields fdom) rvar funlist fname fdom adt
		(dependent? te) (dependent? fdom))))
    (if (every #'identity-fun? funs)
	(mk-identity-fun fdom)
	(mk-lambda-expr (list rbd)
	  (mk-record-expr
	   (mapcar #'(lambda (fd fun)
		       (mk-assignment 'uni
			 (list (list (mk-name-expr (id fd))))
			 (if (identity-fun? fun)
			     (mk-application (id fd) (copy rvar))
			     (mk-application fun
			       (mk-application (id fd) (copy rvar))))))
	     (fields fdom) funs))))))

(defmethod acc-reduce-selection* ((te tupletype) funlist fname fdom adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)
			     'variable))
	 (funs (acc-reduce-selection-types
		(types te) (types fdom) tvar funlist fname adt)))
    (if (every #'identity-fun? funs)
	(mk-identity-fun te)
	(let* ()
	  (mk-lambda-expr (list tbd)
	    (mk-tuple-expr
	     (let ((num 0))
	       (mapcar #'(lambda (fun)
			   (incf num)
			   (let ((proj (make-instance 'projection-application
					 'id (makesym "PROJ_~d" num)
					 'index num
					 'argument tvar)))
			     (if (identity-fun? fun)
				 proj
				 (mk-application fun proj))))
		 funs))))))))

(defun acc-reduce-selection-types (types dtypes tvar funlist fname adt
					 &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (let ((ty1 (if (typep (car types) 'dep-binding)
		     (type (car types))
		     (car types)))
	    (ty2 (if (typep (car dtypes) 'dep-binding)
		     (type (car dtypes))
		     (car dtypes))))
      (acc-reduce-selection-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       (if (typep (car dtypes) 'dep-binding)
	   (substit (cdr dtypes)
	     (acons (car dtypes)
		    (make-projection-application index tvar)
		    nil))
	   (cdr dtypes))
       tvar funlist fname adt (1+ index)
       (cons (acc-reduce-selection* ty1 funlist fname ty2 adt) result)))))

(defmethod acc-reduce-selection* ((te type-expr) funlist fname fdom adt)
  (declare (ignore funlist fname fdom adt))
  (mk-identity-fun te))


(defun gen-adt-reduce-funtype (c rtype adt adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'bind-decl (arguments c)))
	     (free-params (free-params bindings))
	     (nbindings (if (every #'(lambda (fp)
				       (memq fp (formals *current-theory*)))
				   free-params)
			    bindings
			    (gen-adt-reduce-funtype-bindings bindings))))
	(mk-funtype (gen-adt-reduce-domain
		     (adt-dependent-bindings
		      (mapcar #'get-adt-var-name (arguments c))
		      nbindings)
		     rtype adt adtinst)
		    rtype))
      rtype))

(defun gen-adt-reduce-funtype-bindings (bindings)
  (let ((nbindings (mapcar #'(lambda (b)
			       (copy b
				 'type nil
				 'types nil
				 'resolutions nil
				 'declared-type
				 (pc-parse (unparse (declared-type b)
					     :string t)
					   'type-expr)))
			   bindings)))
    (typecheck* nbindings nil nil nil)))


(defun adt-dependent-bindings (vars bds &optional result)
  (if (null vars)
      (nreverse result)
      (if (occurs-in (car bds) (cdr bds))
	  (let* ((dep (mk-dep-binding (id (car vars)) (type (car bds))))
		 (var (mk-name-expr (id (car vars)) nil nil
				    (make-resolution dep
				      (current-theory-name) (type dep))
				    'variable)))
	    (adt-dependent-bindings (cdr vars)
				    (substit (cdr bds)
				      (acons (car bds) var nil))
				    (cons dep result)))
	  (adt-dependent-bindings (cdr vars)
				  (cdr bds)
				  (cons (type (car bds)) result)))))

(defun gen-adt-reduce-domain (args rtype adt adtinst &optional result)
  (if (null args)
      (nreverse result)
      (let ((atype (gen-adt-reduce-dom (car args) rtype adt adtinst)))
	(if (or (tc-eq atype (car args))
		(not (typep atype 'dep-binding)))
	    (gen-adt-reduce-domain (cdr args) rtype adt adtinst
				   (cons atype result))
	    (let ((var (mk-name-expr (id atype) nil nil
				     (make-resolution atype
				       (current-theory-name) (type atype))
				     'variable)))
	      (gen-adt-reduce-domain (substit (cdr args)
				       (acons (car args) var nil))
				     rtype adt adtinst
				     (cons atype result)))))))

(defun gen-adt-reduce-dom (atype rtype adt adtinst)
  (let ((adt-type (or adtinst (adt-type-name adt))))
    (gen-adt-reduce-dom* atype rtype adt-type)))

(defun gen-adt-reduce-dom* (atype rtype adt-type)
  (gensubst atype
    #'(lambda (ex) (gen-adt-reduce-dom! ex rtype adt-type))
    #'(lambda (ex) (gen-adt-reduce-dom? ex adt-type))))

(defmethod gen-adt-reduce-dom? ((ex type-name) adt-type)
  (same-declaration ex adt-type))

(defmethod gen-adt-reduce-dom! ((ex type-name) rtype adt-type)
  (declare (ignore adt-type))
  (copy rtype))

(defmethod gen-adt-reduce-dom? ((ex expr-as-type) adt-type)
  (declare (ignore adt-type))
  nil)

(defmethod gen-adt-reduce-dom? ((ex subtype) adt-type)
  (subtype-of? ex adt-type))

(defmethod gen-adt-reduce-dom! ((ex subtype) rtype adt-type)
  (declare (ignore adt-type))
  (copy rtype))

(defmethod gen-adt-reduce-dom? ((ex datatype-subtype) adt-type)
  (occurs-in adt-type ex))

(defmethod gen-adt-reduce-dom! ((ex datatype-subtype) rtype adt-type)
  (gen-adt-reduce-dom* (supertype ex) rtype adt-type))

(defmethod gen-adt-reduce-dom? (ex adt-type)
  (declare (ignore ex adt-type))
  nil)

(defun gen-adt-reduce-definition2 (adt fname fdoms adtinst)
  (let ((avar (mk-name-expr (makesym "~a_adtvar" (id adt))))
	(funlist (mapcar #'(lambda (c)
			     (let ((rid (op-to-id (recognizer c))))
			       (mk-name-expr (makesym "~a_fun" rid))))
			 (constructors adt))))
    (mk-lambda-expr (list (mk-bind-decl (id avar)
			      (mk-type-name (id adt))))
      (mk-cases-expr avar
	(mapcar #'(lambda (c fdom)
		    (gen-adt-reduce-selection2 c adt funlist fname avar fdom adtinst))
		(constructors adt) fdoms)
	nil))))

(defun gen-adt-reduce-selection2 (c adt funlist fname avar fdom adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name) (type b))
					     'variable))
		     bindings)))      
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (mk-application* (makesym "~a_fun" (op-to-id (recognizer c)))
	    (nconc (mapcar #'(lambda (v fd)
			       (acc-reduce-selection
				v
				(if adtinst
				    (subst-mod-params (type v) adtinst)
				    (type v))
				funlist fname
				(if (typep fd 'dep-binding)
				    (type fd)
				    fd)
				(if adtinst
				    (subst-mod-params (adt-type-name adt) adtinst)
				    (adt-type-name adt))))
		     vars fdom)
		   (list (copy avar))))))
      (mk-selection (mk-name-expr (id c)) nil
		    (mk-application
			(mk-name-expr (makesym "~a_fun"
					       (op-to-id (recognizer c))))
		      (copy avar)))))

(defmethod acc-reduce-selection2 (arg (te type-name) funlist fname adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (mk-application (mk-application* fname funlist) (copy arg)))
;	((member te ptypes :test #'corresponding-formals)
;	 (let* ((pos (position te ptypes :test #'corresponding-formals))
;		(fun (nth pos funlist)))
;	   (mk-application fun (copy arg))))
	((datatype-adt? (adt-type-name adt) te)
	 (let ((acts (actuals (module-instance (find-supertype te)))))
	   (mk-application
	       (mk-application* '|map|
		 (acc-reduce-sel-acts2 (adt-type-name adt) acts fname funlist))
	     (copy arg))))
	(t (copy arg))))

(defun acc-reduce-sel-acts2 (adt-type acts fname funlist &optional result)
  (if (null acts)
      (nreverse result)
      (if (type-value (car acts))
	  (let ((nfun (if (tc-eq (type-value (car acts)) adt-type)
			  (mk-application* fname funlist)
			  (mk-name-expr '|id|
			    (list (mk-actual (type-value (car acts))))))))
	    (acc-reduce-sel-acts2 adt-type (cdr acts) fname funlist
				 (cons nfun result)))
	  (acc-reduce-sel-acts2 adt-type (cdr acts) fname funlist result))))

(defmethod acc-reduce-selection2 (arg (te subtype) funlist fname adt)
  (acc-reduce-selection2 arg (supertype te) funlist fname adt))

(defmethod acc-reduce-selection2 (arg (te funtype) funlist fname adt)
  (if (sequence-adt? (adt-type-name adt) te)
      (mk-application
	  (mk-application '|map|
	    (mk-application* fname funlist))
	arg)
      (copy arg)))

(defmethod acc-reduce-selection2 (arg (te type-expr) funlist fname adt)
  (declare (ignore funlist fname adt))
  (copy arg))

(defun gen-adt-reduce-funtype2 (c rtype adt adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'bind-decl (arguments c)))
	     (free-params (free-params bindings))
	     (nbindings (if (every #'(lambda (fp)
				       (memq fp (formals *current-theory*)))
				   free-params)
			    bindings
			    (gen-adt-reduce-funtype-bindings bindings))))
	(mk-funtype (gen-adt-reduce-domain2
		     (adt-dependent-bindings
		      (mapcar #'get-adt-var-name (arguments c))
		      nbindings)
		     rtype adt adtinst)
		    rtype))
      (mk-funtype (list (copy-all (adt-type-name adt))) rtype)))

(defun adt-dependent-bindings2 (vars bds &optional result)
  (if (null vars)
      (nreverse result)
      (let ((ty (if (occurs-in (car bds) (cdr bds))
		    (mk-dep-binding (id (car vars)) (type (car bds)))
		    (type (car bds)))))
	(adt-dependent-bindings2 (cdr vars)
				 (substit (cdr bds)
				   (acons (car vars) (car bds) nil))
				 (cons ty result)))))

(defun gen-adt-reduce-domain2 (args rtype adt adtinst &optional result)
  (if (null args)
      (nreverse (cons (copy-all (adt-type-name adt)) result))
      (let ((atype (gen-adt-reduce-dom (car args) rtype adt adtinst)))
	(if (or (eq atype (car args))
		(not (typep atype 'dep-binding)))
	    (gen-adt-reduce-domain2 (cdr args) rtype adt adtinst
				    (cons atype result))
	    (let ((var (mk-name-expr (id atype) nil nil
				     (make-resolution atype
				       (current-theory-name) (type atype))
				     'variable)))
	      (gen-adt-reduce-domain2 (substit (cdr args)
					(acons (car args) var nil))
				      rtype adt adtinst
				      (cons atype result)))))))

(defun gen-adt-reduce-dom2 (atype rtype adt adtinst)
  (let ((adt-type (or adtinst (adt-type-name adt)))
	(arg-type (find-supertype atype)))
    (cond ((and (typep arg-type 'type-name)
		(same-declaration arg-type adt-type))
	   (copy rtype))
	  ((sequence-adt? adt-type arg-type)
	   (mk-type-name '|sequence| (list (mk-actual (copy rtype)))))
	  ((adt? arg-type)
	   (mk-type-name (id arg-type)
	     (mapcar #'(lambda (a)
			 (if (type-value a)
			     (let ((act (mk-actual (gen-adt-reduce-dom2
						    (type-value a)
						    rtype adt adtinst))))
			       (setf (type-value act) nil)
			       act)
			     (raise-actuals (copy-all a))))
		     (actuals (module-instance arg-type)))))
	  (t (raise-actuals (copy-all atype))))))


;;; subterm definition

(defun generate-adt-subterm (adt)
  (let* ((xid (make-new-variable '|x| adt))
	 (yid (make-new-variable '|y| adt))
	 (subterm-decl
	  (mk-adt-def-decl '|subterm|
	    *boolean*
	    (gen-adt-subterm-definition
	     adt (mk-name-expr xid) (mk-name-expr yid))
	    (list (mk-arg-bind-decl xid (adt-type-name adt))
		  (mk-arg-bind-decl yid (adt-type-name adt)))))
	 (<<-decl (mk-adt-def-decl '<<
		    *boolean*
		    (gen-adt-<<-definition
		     adt (mk-name-expr xid) (mk-name-expr yid))
		    (list (mk-arg-bind-decl xid (adt-type-name adt))
			  (mk-arg-bind-decl yid (adt-type-name adt)))))
	 (<<-wf-decl (mk-formula-decl (makesym "~a_well_founded" (id adt))
		       (mk-application
			   (mk-name-expr '|well_founded?|
			     (list (mk-actual (adt-type-name *adt*))))
			 (mk-name-expr '<<))
		       'AXIOM)))
    (typecheck-adt-decl subterm-decl)
    (typecheck-adt-decl <<-decl)
    (typecheck-adt-decl <<-wf-decl)))

(defun gen-adt-subterm-definition (adt xvar yvar)
  (if (every #'(lambda (c)
		 (non-recursive-constructor c (adt-type-name adt)))
	     (constructors adt))
      (mk-application '= xvar yvar)
      (mk-application 'OR
	(mk-application '= xvar yvar)
	(mk-cases-expr yvar
	  (mapcar #'(lambda (c) (gen-adt-subterm-selection c adt xvar))
		  (constructors adt))
	  nil))))

(defun gen-adt-subterm-selection (c adt xvar)
  (if (arguments c)
      (mk-selection (mk-name-expr (id c))
	(mapcar #'(lambda (a) (make-instance 'bind-decl
				'id (id (get-adt-var a))))
		(arguments c))
	(mk-disjunction
	 (mapcar #'(lambda (a)
		     (acc-subterm-selection (make-instance 'name-expr
					      'id (id (get-adt-var a)))
					    (type a) xvar adt))
		 (arguments c))))
      (mk-selection (mk-name-expr (id c)) nil *false*)))

(defun acc-subterm-selection (arg type xvar adt)
  (let ((fun (acc-subterm-selection* type xvar adt)))
    (if (everywhere-false? fun)
	(copy *false*)
	(mk-application fun arg))))

(defmethod acc-subterm-selection* ((te type-name) xvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (let ((bd (mk-bind-decl (make-new-variable '|z| te) te)))
	   (mk-lambda-expr (list bd)
	     (mk-application '|subterm| (copy xvar) (mk-name-expr (id bd))))))
	((adt? te)
	 (let ((subs (mapcar #'(lambda (act)
				 (acc-subterm-selection* (type-value act)
						     xvar adt))
			     (positive-actuals te))))
	   (if (every #'everywhere-false? subs)
	       (call-next-method)
	       (mk-predicate-application '|some| te adt subs))))
	(t (call-next-method))))

(defmethod acc-subterm-selection* ((te subtype) xvar adt)
  (acc-subterm-selection* (supertype te) xvar adt))

(defmethod acc-subterm-selection* ((te funtype) xvar adt)
  (let* ((zid (make-new-variable '|z| (list te xvar)))
	 (zbd (make-bind-decl zid (domain te)))
	 (zvar (mk-name-expr zid nil nil
			     (make-resolution zbd
			       (current-theory-name) (domain te))
			     'variable))
	 (sub (acc-subterm-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) zvar nil))
		   (range te))
	       xvar adt)))
    (if (everywhere-false? sub)
	(call-next-method)
	(if (sequence? te)
	    (mk-predicate-application '|some| te adt (list sub))
	    (let* ((fid (make-new-variable '|f| (list te sub)))
		   (fbd (make-bind-decl fid te))
		   (fvar (mk-name-expr fid nil nil
				       (make-resolution fbd
					 (current-theory-name) te)
				       'variable)))
	      (mk-lambda-expr (list fbd)
		(mk-exists-expr (list zbd)
		  (mk-application sub
		    (mk-application fvar zvar)))))))))

(defmethod acc-subterm-selection* ((te recordtype) xvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)
			     'variable))
	 (subs (acc-subterm-fields (fields te) xvar rvar adt (dependent? te))))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-disjunction
	   (mapcan #'(lambda (fd sub)
		       (unless (everywhere-false? sub)
			 (list (mk-application sub
				 (mk-application (id fd) rvar)))))
	     (fields te) subs))))))

(defun acc-subterm-fields (fields xvar rvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-subterm-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       xvar rvar adt dep?
       (cons (acc-subterm-selection* (type (car fields)) xvar adt)
	     result))))

(defmethod acc-subterm-selection* ((te tupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)
			     'variable))
	 (subs (acc-subterm-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0))
	    (mk-disjunction
	     (mapcan #'(lambda (sub)
			 (incf num)
			 (unless (everywhere-false? sub)
			   (list (mk-application sub
				   (make-instance 'projection-application
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument tvar)))))
	       subs)))))))

(defun acc-subterm-types (types xvar tvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-subterm-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       xvar tvar adt (1+ index)
       (cons (acc-subterm-selection* (car types) xvar adt) result))))

(defmethod acc-subterm-selection* ((te dep-binding) xvar adt)
  (acc-subterm-selection* (type te) xvar adt))

(defmethod acc-subterm-selection* ((te type-expr) xvar adt)
  (declare (ignore xvar adt))
  (mk-everywhere-false-function te))

(defun make-some-subterm-application (arg xvar adt)
  (mk-application
      (mk-application '|some|
	(let ((zvar (mk-name-expr (make-new-variable '|z| adt))))
	  (mk-lambda-expr (list (mk-bind-decl (id zvar) (adt-type-name adt)))
	    (mk-application '|subterm| (copy xvar) zvar))))
    (copy arg)))


(defun gen-adt-<<-definition (adt xvar yvar)
  (if (every #'(lambda (c)
		 (non-recursive-constructor c (adt-type-name adt)))
	     (constructors adt))
      (copy *false*)
      (mk-cases-expr yvar
	(mapcar #'(lambda (c) (gen-adt-<<-selection c adt xvar))
		(constructors adt))
	nil)))

(defun gen-adt-<<-selection (c adt xvar)
  (if (arguments c)
      (mk-selection (mk-name-expr (id c))
	(mapcar #'(lambda (a) (make-instance 'bind-decl
				'id (id (get-adt-var a))))
		(arguments c))
	(mk-disjunction
	  (mapcar #'(lambda (a)
		      (acc-<<-selection (make-instance 'name-expr
					      'id (id (get-adt-var a)))
					     (type a) xvar adt))
		  (arguments c))))
      (mk-selection (mk-name-expr (id c)) nil *false*)))

(defun acc-<<-selection (arg type xvar adt)
  (let ((fun (acc-<<-selection* type xvar adt)))
    (if (everywhere-false? fun)
	(copy *false*)
	(mk-application fun arg))))

(defmethod acc-<<-selection* ((te type-name) xvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (let ((bd (mk-bind-decl (make-new-variable '|z| te) te)))
	   (mk-lambda-expr (list bd)
	     (mk-application 'OR
	       (mk-application '= (copy xvar) (mk-name-expr (id bd)))
	       (mk-application '<< (copy xvar) (mk-name-expr (id bd)))))))
	((adt? te)
	 (let ((subs (mapcar #'(lambda (act)
				 (acc-<<-selection* (type-value act)
						     xvar adt))
			     (positive-actuals te))))
	   (if (every #'everywhere-false? subs)
	       (call-next-method)
	       (mk-predicate-application '|some| te adt subs))))
	(t (call-next-method))))

(defmethod acc-<<-selection* ((te subtype) xvar adt)
  (acc-<<-selection* (supertype te) xvar adt))

(defmethod acc-<<-selection* ((te funtype) xvar adt)
  (if (sequence? te)
      (let ((sub (acc-subterm-selection* (range te) xvar adt)))
	(if (everywhere-false? sub)
	    (call-next-method)
	    (mk-predicate-application '|some| te adt (list sub))))
      (let* ((zid (make-new-variable '|z| te))
	     (zbd (make-bind-decl zid (domain te)))
	     (zvar (mk-name-expr zid nil nil
				 (make-resolution zbd
				   (current-theory-name) (domain te))
				 'variable))
	     (sub (acc-subterm-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) zvar nil))
		       (range te))
		   xvar adt)))
	(if (everywhere-false? sub)
	    (call-next-method)
	    (let* ((fid (make-new-variable '|f| (list te sub)))
		   (fbd (make-bind-decl fid te))
		   (fvar (mk-name-expr fid nil nil
				       (make-resolution fbd
					 (current-theory-name) te)
				       'variable)))
	      (mk-lambda-expr (list fbd)
		(mk-exists-expr (list zbd)
		  (mk-application sub
		    (mk-application fvar zvar)))))))))

(defmethod acc-<<-selection* ((te type-expr) xvar adt)
  (declare (ignore xvar adt))
  (mk-everywhere-false-function te))

(defun make-some-<<-application (arg xvar adt)
  (mk-application
      (mk-application '|some|
	(let ((zvar (mk-name-expr (make-new-variable '|z| adt))))
	  (mk-lambda-expr (list (mk-bind-decl (id zvar) (adt-type-name adt)))
	    (mk-application '<< (copy xvar) zvar))))
    (copy arg)))


; (defmethod declarations ((adt datatype))
;   nil)


(defun typecheck-adt-decl (decl &optional (add? t) (reduce? t))
  (setf (declaration *current-context*) decl)
  (when (typep decl '(and declaration (not formal-decl)))
    (setf (generated-by decl) (id *adt*)))
  (when (typep *adt* 'inline-datatype)
    (push decl (generated *adt*)))
  (when (eq add? t)
    (setf (theory *current-theory*)
	  (if *last-adt-decl*
	      (let* ((theory-part (theory *current-theory*))
		     (rest (cdr (memq *last-adt-decl* theory-part))))
		(nconc (ldiff theory-part rest) (cons decl rest)))
	      (nconc (theory *current-theory*) (list decl))))
    (when *last-adt-decl*
      (setq *last-adt-decl* decl)))
  (typecase decl
    (declaration (setf (module decl) (theory *current-context*))
		 (typecheck* decl nil nil nil)
		 (setf (typechecked? decl) t)
		 (unless (or (eq add? 'no)
			     (and (typep decl 'type-def-decl)
				  (typep (type-expr decl) 'enumtype)))
		   (put-decl decl (current-declarations-hash)))
		 (when (and reduce?
			    (typep decl '(or const-decl formula-decl)))
		   (setf (definition decl)
			 (beta-reduce (definition decl)))))
    (importing (typecheck-using (theory-name decl)))
    (datatype (unwind-protect
		  (typecheck* decl nil nil nil)
		(cleanup-datatype decl)))))

(defun add-adt-decl (decl)
  (unless (and (typep decl 'type-def-decl)
	       (typep (type-expr decl) 'enumtype))
    (put-decl decl (current-declarations-hash)))
  (setf (theory *current-theory*)
	  (if *last-adt-decl*
	      (let* ((theory-part (theory *current-theory*))
		     (rest (cdr (memq *last-adt-decl* theory-part))))
		(nconc (ldiff theory-part rest) (cons decl rest)))
	      (nconc (theory *current-theory*) (list decl))))
  (when *last-adt-decl*
    (setq *last-adt-decl* decl)))


;;; Returns true if the given type occurs positively in the datatype.  All
;;; of the accessor types are checked, and if any occurrence of the type is
;;; direct, in the range of a function type (not recursively), or as a
;;; subtype of one of these two cases.

(defvar *simple-pos* nil
  "If set to T, then occurs-positively? will return nil if the type occurs in a
function, tuple, or record type")

(defun occurs-positively? (type1 type2)
  (let* ((*negative-occurrence* nil)
	 (pos? (occurs-positively?* type1 type2 nil)))
    (values pos? *negative-occurrence*)))

(defmethod occurs-positively?* (type (adt datatype) none)
  (and (occurs-positively?* type (formals-sans-usings adt) none)
       (occurs-positively?* type (constructors adt) none)))

(defmethod occurs-positively?* (type (list list) none)
  (or (null list)
      (and (occurs-positively?* type (car list) none)
	   (occurs-positively?* type (cdr list) none))))

(defmethod occurs-positively?* (type (con simple-constructor) none)
  (occurs-positively?* type
		       (mapcar #'(lambda (ac) (range (type ac)))
			       (acc-decls con))
		       none))

(defmethod occurs-positively?* (type (fm formal-type-decl) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (fm formal-const-decl) none)
  (occurs-positively?* type (declared-type fm) none))

(defmethod occurs-positively?* (type (decl const-decl) none)
  (occurs-positively?* type (type decl) none))

(defmethod occurs-positively?* :around (type (te type-expr) none)
  (if (tc-eq type te)
      (cond (none
	     (setq *negative-occurrence* te)
	     nil)
	    (t t))
      (call-next-method)))

(defmethod occurs-positively?* (type (te type-name) none)
  (if (adt? te)
      (let ((adt (adt te)))
	(if (typep adt 'inline-datatype)
	    (every #'(lambda (a)
		       (if (type-value a)
			   (occurs-positively?* type (type-value a) t)
			   (occurs-positively?* type (type (expr a)) t)))
		   (actuals te))
	    (every #'(lambda (a pos?)
		       (if (type-value a)
			   (occurs-positively?* type (type-value a)
						(or none
						    (unless pos? t)))
			   (occurs-positively?* type (type (expr a)) t)))
		   (actuals te)
		   (mapcar #'(lambda (ff)
			       (member ff (positive-types adt)
				       :test #'same-id))
			   (formals-sans-usings adt)))))
      (occurs-positively?* type (actuals te) t)))

(defmethod occurs-positively?* (type (te datatype-subtype) none)
  (occurs-positively?* type (supertype te) (or *no-adt-subtypes* none)))

(defmethod occurs-positively?* (type (te subtype) none)
  (and (occurs-positively?* type (supertype te) (or *no-adt-subtypes* none))
       (occurs-positively?* type (predicate te) none)))

(defmethod occurs-positively?* (type (te funtype) none)
  (and (occurs-positively?* type (range te) (or *simple-pos* none))
       (occurs-positively?* type (domain te) t)))

(defmethod occurs-positively?* (type (te tupletype) none)
  (occurs-positively?* type (types te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te recordtype) none)
  (occurs-positively?* type (fields te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (fd field-decl) none)
  (occurs-positively?* type (type fd) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te dep-binding) none)
  (occurs-positively?* type (type te) none))

(defvar *adt-recursive-names* nil)

(defmethod occurs-positively?* (type (ex name-expr) none)
  (or (member ex *adt-recursive-names* :test #'tc-eq)
      (not (occurs-in type (actuals (module-instance ex))))
      (let* ((res (resolution ex))
	     (decl (declaration res))
	     (*adt-recursive-names* (cons ex *adt-recursive-names*)))
	(when (and (typep decl '(or const-decl def-decl))
		   (definition decl))
	  (let* ((def (subst-mod-params (car (def-axiom decl))
					(module-instance res)))
		 (*adt-recursive-names*
		  (if (typep def 'binding-expr)
		      (append (mapcar #'make-variable-expr (bindings def))
			      *adt-recursive-names*)
		      *adt-recursive-names*)))
	    (occurs-positively?*
	     type
	     (if (typep def 'binding-expr)
		 (args2 (expression def))
		 (args2 def))
	     none))))))

(defmethod occurs-positively?* (type (ex adt-name-expr) none)
  (let ((adt (adt ex)))
    (occurs-positively-in-adt?
     type
     (actuals (module-instance (resolution (or (print-type adt) adt))))
     (formals-sans-usings (adt adt))
     (positive-types (adt adt))
     none)))

(defun occurs-positively-in-adt? (type acts formals postypes none)
  (or (null acts)
      (and (if (member (car formals) postypes
		       :test #'same-id
		       :key #'(lambda (x) (or (print-type x) x)))
	       (occurs-positively?* type (type-value (car acts)) none)
	       (occurs-positively?* type (expr (car acts)) t))
	   (occurs-positively-in-adt? type (cdr acts) (cdr formals)
				      postypes none))))

(defmethod occurs-positively?* (type (ex number-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex record-expr) none)
  (occurs-positively?* type (assignments ex) none))

(defmethod occurs-positively?* (type (ex tuple-expr) none)
  (occurs-positively?* type (exprs ex) none))

(defmethod occurs-positively?* (type (ex projection-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex field-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex application) none)
  (and (occurs-positively?* type (operator ex) none)
       (occurs-positively?* type (argument ex) none)))

(defmethod occurs-positively?* (type (ex binding-expr) none)
  (and (occurs-positively?* type (bindings ex) none)
       (occurs-positively?* type (expression ex) none)))

(defmethod occurs-positively?* (type (ex cases-expr) none)
  (and (occurs-positively?* type (expression ex) none)
       (occurs-positively?* type (selections ex) none)
       (occurs-positively?* type (else-part ex) none)))

(defmethod occurs-positively?* (type (ex selection) none)
  (occurs-positively?* type (expression ex) none))

(defmethod occurs-positively?* (type (ex update-expr) none)
  (and (occurs-positively?* type (expression ex) none)
       (occurs-positively?* type (assignments ex) none)))

(defmethod occurs-positively?* (type (ex assignment) none)
  (and (occurs-positively?* type (arguments ex) none)
       (occurs-positively?* type (expression ex) none)))

(defmethod occurs-positively?* (type (ex bind-decl) none)
  (occurs-positively?* type (type ex) none))

(defmethod occurs-positively?* (type (ex actual) none)
  (if (type-value ex)
      (occurs-positively?* type (type-value ex) none)
      (occurs-positively?* type (expr ex) none)))

(defun generate-adt-vars (adt)
  (clrhash *adt-vars*)
  (setf (gethash adt *adt-vars*) (make-new-variable '|v| adt))
  (dolist (c (constructors adt))
    (let ((num 0))
      (dolist (a (arguments c))
	(setf (gethash a *adt-vars*)
	      (mk-name-expr (makesym "~a~d_var"
				     (op-to-id (id c))
				     (incf num))))))))

(defun get-adt-var (obj)
  (let ((var (gethash obj *adt-vars*)))
    (if (syntax? var)
	(copy-all var)
	var)))

(defmethod sequence? ((te funtype))
  (tc-eq (domain te) *naturalnumber*))

(defmethod sequence? ((te type-expr))
  nil)

(defmethod sequence? ((te dep-binding))
  (sequence? (type te)))

(defmethod everywhere-true? (expr)
  (declare (ignore expr))
  nil)

(defmethod everywhere-true? ((expr lambda-expr))
  (tc-eq (expression expr) *true*))

(defmethod everywhere-false? (expr)
  (declare (ignore expr))
  nil)

(defmethod everywhere-false? ((expr lambda-expr))
  (tc-eq (expression expr) *false*))

(defun mk-identity-fun (te)
  (let ((act (mk-actual te)))
    (pc-parse (unparse (mk-name-expr '|id| (list act)) :string t)
	      'expr)))

(defmethod identity-fun? (expr)
  (declare (ignore expr))
  nil)

(defmethod identity-fun? ((expr name-expr))
  (eq (id expr) '|id|))

(defmethod mk-predicate-application (funid (te type-name) adt funs)
  (if (eq (adt te) adt)
      (mk-application* funid funs)
      (let* ((acts (actuals te))
	     (name (mk-name-expr funid acts))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname funs))))

(defmethod mk-predicate-application (funid (te type-expr) adt funs)
  (declare (ignore adt))
  (let ((name (mk-name-expr funid)))
    (mk-application* name funs)))

(defmethod mk-map-application ((te type-name) fpairs adt maps)
  (if (eq (adt te) adt)
      (mk-application* '|map| maps)
      (let* ((acts (actuals (module-instance te)))
	     (macts (subst-map-actuals acts fpairs))
	     (name (mk-name-expr '|map| (append acts macts)))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname maps))))

(defmethod mk-map-application ((te funtype) fpairs adt maps)
  (declare (ignore fpairs adt))
  (mk-application* '|map| maps))

(defun map-application? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'name-expr)
       (eq (id (operator expr)) '|map|)))

(defun subst-map-actuals (te fpairs)
  (gensubst te
    #'(lambda (x) (subst-map-actuals! x fpairs))
    #'(lambda (x) (subst-map-actuals? x fpairs))))

(defmethod subst-map-actuals? ((name name) fpairs)
  (and (resolution name)
       (assoc (declaration name) fpairs :test #'same-id)))

(defmethod subst-map-actuals? (obj fpairs)
  (declare (ignore obj fpairs)))

(defmethod subst-map-actuals! ((name name) fpairs)
  (let ((nres (subst-map-actuals! (resolution name) fpairs)))
    (lcopy name
      'id (id (declaration nres))
      'resolutions (list nres))))

(defmethod subst-map-actuals! ((res resolution) fpairs)
  (let ((ndecl (cdr (assoc (declaration res) fpairs :test #'same-id))))
    (make-resolution ndecl (current-theory-name))))

(defun positive-actuals (type-name)
  (let ((adt (adt? type-name)))
    (positive-actuals* (actuals (module-instance type-name))
		       (formals-sans-usings adt)
		       (positive-types adt))))

(defun positive-actuals* (acts formals pos-formals &optional result)
  (if (null acts)
      (nreverse result)
      (positive-actuals* (cdr acts) (cdr formals) pos-formals
			 (if (member (car formals) pos-formals
				     :test #'same-id)
			     (cons (car acts) result)
			     result))))

(defmethod subtypes ((adt datatype))
  nil)
