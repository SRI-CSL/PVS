;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datatype.lisp -- Generation and typechecking of datatypes
;; Author          : Sam Owre
;; Created On      : Sun Dec 12 01:18:46 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jul  2 18:51:55 1999
;; Update Count    : 125
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :pvs)

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
					     'kind 'default)))
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

(defmethod typecheck ((adt recursive-type) &key expected context)
  (declare (ignore context))
  (let ((*subtype-of-hash* (make-hash-table :test #'eq)))
    (unwind-protect (typecheck* adt expected nil nil)
      (unless (typechecked? adt)
	(untypecheck-theory adt)))))

(defmethod typecheck* ((adt recursive-type) expected kind arguments)
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
      "Assumings are not allowed for in-line (co)datatypes"))
  (let ((*generating-adt* adt)
	(*last-adt-decl* (declaration *current-context*)))
    (generate-inline-adt adt))
  (put-decl adt (current-declarations-hash))
  (setf (typechecked? adt) t))

(defun generate-inline-adt (adt)
  (generate-adt-sections adt))

(defun typecheck-top-level-adt (adt)
  (tcdebug "~%Typecheck (CO)DATATYPE ~a" (id adt))
  (setf (formals-sans-usings adt)
	(remove-if #'(lambda (d) (typep d 'importing)) (formals adt)))
  (let ((adt-theory (generate-adt-theory adt))
	(map-theory (generate-adt-map-theory adt))
	(reduce-theory (if (datatype? adt)
			   (generate-adt-reduce-theory adt)
			   (generate-codt-coreduce-theory adt))))
    (setf (adt-theory adt) adt-theory
	  (adt-map-theory adt) map-theory
	  (adt-reduce-theory adt) reduce-theory)
    (save-adt-file adt)
    (let* ((adt-file (concatenate 'string (string (id adt))
				  (if (datatype? adt) "_adt" "_codt")))
	   (proofs (read-pvs-file-proofs adt-file)))
      (restore-from-context adt-file adt-theory proofs)
      (when map-theory
	(restore-from-context adt-file map-theory proofs))
      (when reduce-theory
	(restore-from-context adt-file reduce-theory proofs)))
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
  (let* ((adt-file (concatenate 'string (string (id adt))
				(if (datatype? adt) "_adt" "_codt")))
	 (adt-path (make-specpath adt-file))
	 (file-exists? #+allegro (file-exists-p adt-path)
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
    (dolist (th adt-theories)
      (let* ((tot (car (tcc-info th)))
	     (prv (cadr (tcc-info th)))
	     (mat (caddr (tcc-info th)))
	     (obl (- tot prv mat)))
	(if (zerop tot)
	    (pvs-message "In DATATYPE theory ~a: No TCCs generated~
                          ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		  (id th) (length (warnings th)) (length (info th)))
	    (pvs-message
		    "In DATATYPE theory ~a: ~d TCC~:p, ~d proved, ~
                     ~d subsumed, ~d unproved~
                     ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		  (id th) tot prv mat obl
		  (length (warnings th)) (length (info th))))))
    (let ((fdate (file-write-date adt-path))
	  (ce2 (get-context-file-entry adt-file)))
      (setf (generated-file-date adt) fdate)
      (when ce2
	(setf (ce-write-date ce2) fdate)
	(setq *pvs-context-changed* t))
      (push 'typechecked (status adt))
      (dolist (th adt-theories)
	(setf (filename th) adt-file))
      (setf (gethash adt-file *pvs-files*)
	    (cons fdate (adt-generated-theories adt))))))


(defun adt-generated-theories (adt)
  (when (adt-theory adt)
    (if (adt-map-theory adt)
	(list (adt-theory adt) (adt-map-theory adt) (adt-reduce-theory adt))
	(list (adt-theory adt) (adt-reduce-theory adt)))))

;;; The adt-theory

(defmethod generate-adt-theory ((adt datatype))
  (build-adt-theory (makesym "~a_adt" (id adt)) adt
    (generate-adt-sections adt)))

(defmethod generate-adt-theory ((adt codatatype))
  (build-adt-theory (makesym "~a_codt" (id adt)) adt
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
  (generate-adt-uninterpreted-ord-function-and-axiom adt)
  (generate-adt-ord-function adt)
  (if (or (enumtype? adt)
	  (every #'(lambda (c) (null (arguments c))) (constructors adt)))
      (progn (generate-inclusive-axiom adt)
	     (generate-disjoint-axiom adt))
      (generate-adt-axioms adt))
  (generate-remaining-adt-sections adt)
  (setf (adt-theory adt) *current-theory*))

(defmethod generate-remaining-adt-sections ((adt datatype))
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
      (generate-adt-reduce adt '|ordinal|))))

(defmethod generate-remaining-adt-sections ((codt codatatype))
  (generate-bisimulation codt)
  (unless (or (enumtype? codt)
	      (every #'(lambda (c) (null (arguments c))) (constructors codt)))
    (when (and (positive-types codt)
	       (not (every #'null (positive-types codt))))
      (generate-adt-every codt)
      (generate-adt-some codt))
    ;;(generate-codt-subterm codt)
    ))

(defun generate-bisimulation (adt)
  (let* ((rvid (make-new-variable 'R adt))
	 (rtype (typecheck (mk-type-name 'PRED
			     (list (make-instance 'actual
				     'expr
				     (mk-tupletype
				      (list (mk-type-name (id adt))
					    (mk-type-name (id adt)))))))))
	 (rbd (make-bind-decl rvid rtype))
	 (rvar (make-variable-expr rbd))
	 (avid (make-new-variable '|x| adt))
	 (bvid (make-new-variable '|y| adt))
	 (atype (typecheck (mk-type-name (id adt))))
	 (btype (typecheck (mk-type-name (id adt))))
	 (abd (make-bind-decl avid atype))
	 (bbd (make-bind-decl bvid btype))
	 (avar (make-variable-expr abd))
	 (bvar (make-variable-expr bbd))
	 (cform (mk-forall-expr (list abd bbd)
		  (mk-implication
		   (mk-application* rvar (list avar bvar))
		   (mk-disjunction
		    (mapcar #'(lambda (c)
				(bisimulation-case c avar bvar rvar adt))
		      (constructors adt))))))
	 (cdecl (mk-adt-def-decl '|bisimulation?|
		  *boolean*
		  (pc-parse (unparse cform :string t) 'expr)
		  (list (list rbd))))
	 (bivid (make-new-variable 'B adt))
	 (bitype (mk-expr-as-type (mk-name-expr '|bisimulation?|)))
	 (bibd (mk-bind-decl bivid bitype))
	 (bivar (mk-name-expr bivid))
	 (fdecl (mk-formula-decl '|coinduction|
		  (mk-forall-expr (list bibd abd bbd)
		    (mk-application '=>
		      (mk-application bivar avar bvar)
		      (mk-application '= avar bvar))))))
    (typecheck-adt-decl cdecl)
    (typecheck-adt-decl fdecl)))

(defun bisimulation-case (c avar bvar rvar adt)
  (if (arguments c)
      (let ((avals
	     (mapcar #'(lambda (a)
			 (bisimulation-arg-value
			  (type a)
			  (mk-application (id a) avar)
			  (mk-application (id a) bvar)
			  rvar
			  adt))
	       (arguments c))))
	(mk-conjunction (cons (mk-application (mk-name-expr (recognizer c))
				avar)
			      (cons (mk-application (mk-name-expr (recognizer c))
				      bvar)
				    avals))))
      (mk-conjunction (list (mk-application (mk-name-expr (recognizer c))
			      avar)
			    (mk-application (mk-name-expr (recognizer c))
			      bvar)))))

(defmethod bisimulation-arg-value ((te type-name) avar bvar rvar adt)
  (cond ((same-declaration te (adt-type-name adt))
	 (mk-application rvar (copy avar) (copy bvar)))
	((adt? te)
	 (let ((rels (mapcar #'(lambda (act)
				 (bisimulation-rel (type-value act) rvar adt))
		       (positive-actuals te))))
	   (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=)))
		      rels)
	       (mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	       (mk-application (mk-simple-every-application te adt rels)
		 (copy avar) (copy bvar)))))
	(t (mk-application (mk-name-expr '=) (copy avar) (copy bvar)))))

(defmethod bisimulation-arg-value ((te subtype) avar bvar rvar adt)
  (bisimulation-arg-value (supertype te) avar bvar rvar adt))

(defmethod bisimulation-arg-value ((te funtype) avar bvar rvar adt)
  (let* ((fid (make-new-variable '|x| (list adt avar bvar rvar)))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (rel (bisimulation-rel
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       rvar adt)))
    (if (and (name-expr? rel) (eq (id rel) '=))
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-forall-expr (list fbd)
	  (mk-application rel
	    (mk-application (copy avar) fvar)
	    (mk-application (copy bvar) fvar))))))

(defmethod bisimulation-arg-value ((te recordtype) avar bvar rvar adt)
  (let ((rels (bisimulation-selection-fields
	       (fields te) avar bvar rvar adt (dependent? te))))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-conjunction
	 (mapcar #'(lambda (fd rel)
		     (mk-application rel
		       (mk-application (id fd) (copy avar))
		       (mk-application (id fd) (copy bvar))))
	   (fields te) rels)))))

(defun bisimulation-selection-fields (fields avar bvar rvar adt dep?
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
       avar bvar rvar adt dep?
       (cons (bisimulation-rel (type (car fields)) rvar adt)
	     result))))

(defmethod bisimulation-arg-value ((te tupletype) avar bvar rvar adt)
  (let ((rels (bisimulation-selection-types (types te) avar bvar rvar adt)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-conjunction
	 (let ((num 0))
	   (mapcar #'(lambda (rel)
		       (incf num)
		       (let ((aproj (make-instance 'projappl
				      'id (makesym "PROJ_~d" num)
				      'index num
				      'argument (copy avar)))
			     (bproj (make-instance 'projappl
				      'id (makesym "PROJ_~d" num)
				      'index num
				      'argument (copy bvar))))
			 (mk-application rel aproj bproj)))
	     rels))))))

(defun bisimulation-selection-types (types avar bvar rvar adt &optional result)
  (if (null types)
      (nreverse result)
      (bisimulation-selection-types
       (cdr types)
       avar bvar rvar adt
       (cons (bisimulation-rel (car types) rvar adt) result))))

(defmethod bisimulation-arg-value ((te type-expr) avar bvar rvar adt)
  (declare (ignore rvar adt))
  (mk-application (mk-name-expr '=) (copy avar) (copy bvar)))

(defmethod bisimulation-rel ((te type-name) rvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (copy rvar))
	((adt? te)
	 (let ((rels (mapcar #'(lambda (act)
				   (bisimulation-rel (type-value act)
							    rvar adt))
			 (positive-actuals te))))
	   (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	       (mk-name-expr '=)
	       (mk-simple-every-application te adt rels))))
	(t (mk-name-expr '=))))

(defmethod bisimulation-rel ((te subtype) rvar adt)
  (bisimulation-rel (supertype te) rvar adt))

(defmethod bisimulation-rel ((te funtype) rvar adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (rel (bisimulation-rel
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       rvar adt)))
    (if (and (name-expr? rel) (eq (id rel) '=))
	rel
	(let* ((lid (make-new-variable '|f| (list te rel)))
	       (lbd (make-bind-decl lid te))
	       (lvar (mk-name-expr lid nil nil
				   (make-resolution lbd
				     (current-theory-name) te))))
	  (mk-lambda-expr (list lbd)
	    (mk-forall-expr (list fbd)
	      (mk-application rel
		(mk-application lvar fvar))))))))

(defmethod bisimulation-rel ((te recordtype) rvar adt)
  (let* ((rxid (make-new-variable '|rx| te))
	 (rxbd (make-bind-decl rxid te))
	 (rxvar (make-variable-expr rxbd))
	 (ryid (make-new-variable '|ry| te))
	 (rybd (make-bind-decl ryid te))
	 (ryvar (make-variable-expr rybd))
	 (rels (bisimulation-rel-fields (fields te) rxvar ryvar rvar adt)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-name-expr '=)
	(mk-lambda-expr (list rxbd rybd)
	  (mk-conjunction rels)))))

(defun bisimulation-rel-fields (fields rxvar ryvar rvar adt &optional rels)
  (if (null fields)
      (nreverse rels)
      (let ((rel (bisimulation-arg-value
		  (type (car fields))
		  (make-field-application (car fields) rxvar)
		  (make-field-application (car fields) ryvar)
		  rvar
		  adt)))
	(bisimulation-rel-fields (cdr fields) rxvar ryvar rvar adt
				 (cons rel rels)))))

(defmethod bisimulation-rel ((te tupletype) rvar adt)
  (let* ((txid (make-new-variable '|tx| te))
	 (txbd (make-bind-decl txid te))
	 (txvar (make-variable-expr txbd))
	 (tyid (make-new-variable '|ty| te))
	 (tybd (make-bind-decl tyid te))
	 (tyvar (make-variable-expr tybd))
	 (rels (bisimulation-rel-types (types te) txvar tyvar rvar adt 1)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-name-expr '=)
	(mk-lambda-expr (list txbd tybd)
	  (mk-conjunction rels)))))

(defun bisimulation-rel-types (types rxvar ryvar rvar adt num &optional rels)
  (if (null types)
      (nreverse rels)
      (let ((rel (bisimulation-arg-value
		  (car types)
		  (make-projection-application num rxvar)
		  (make-projection-application num ryvar)
		  rvar
		  adt)))
	(bisimulation-rel-types (cdr types) rxvar ryvar rvar adt (1+ num)
				(cons rel rels)))))

(defmethod bisimulation-rel ((te dep-binding) rvar adt)
  (bisimulation-rel (type te) rvar adt))

(defmethod bisimulation-rel ((te type-expr) rvar adt)
  (declare (ignore rvar adt))
  (mk-name-expr '=))

;;;;;;;;;;;;;;

(defun set-adt-nonemptiness (adt)
  (if (some #'(lambda (c)
		(every #'(lambda (a) (not (possibly-empty-type? (type a))))
		       (arguments c)))
	    (constructors adt))
      (setf (nonempty? (adt-type-name adt)) t)
      (pvs-warning "Datatype ~a may be empty" (id adt)))
  (set-recognizer-nonemptiness adt))

(defun set-recognizer-nonemptiness (adt)
  (mapc #'set-recognizer-nonemptiness* (constructors adt)))

(defun set-recognizer-nonemptiness* (constructor)
  (if (arguments constructor)
      (unless (some #'(lambda (a) (possibly-empty-type? (type a)))
		    (arguments constructor))
	(set-nonempty-type (range (type (con-decl constructor)))))
      (set-nonempty-type (type (con-decl constructor)))))

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
	(*current-theory* (if (typep adt 'inline-recursive-type)
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
    (dolist (imp importings)
      (let* ((cimp (copy-importing imp))
	     (*adt-decl* imp))
	(typecheck-adt-decl cimp)))))

(defun copy-importing (imp)
  (make-instance 'importing
    'theory-name (pc-parse (unparse (theory-name imp) :string t) 'modname)))

(defun generate-adt-type (adt)
  (let ((*adt-decl* adt)
	(tdecl (mk-type-decl (id adt))))
    (typecheck-adt-decl tdecl)
    (setf (generated-by tdecl) (id adt))
    (setf (adt-type-name adt) (type-value tdecl))))

(defun generate-adt-decls (adt)
  (let* ((ptype (mk-predtype (adt-type-name adt)))
	 (last (car (last (constructors adt)))))
    (unless (inline-datatype? adt)
      (dolist (fm (formals adt))
	(unless (typep fm 'importing)
	  (setf (module fm) adt)))
      (dolist (c (constructors adt))
	(dolist (arg (arguments c))
	  (setf (module arg) adt))))
    (dolist (c (constructors adt))
      (let ((rec (generate-adt-recognizer (recognizer c) c ptype)))
	(setf (chain? rec) (not (eq c last)))))
    (generate-adt-subtypes adt)
    (dolist (c (constructors adt))
      (generate-adt-constructor c))
    (generate-accessors adt)))


;;; Generate the subtype type declarations

(defmethod generate-adt-subtypes ((adt recursive-type))
  nil)

(defmethod generate-adt-subtypes ((adt recursive-type-with-subtypes))
  (dolist (subtype (subtypes adt))
    ;; First generate the predicate
    (let* ((pred-decl (mk-adt-subtype-pred subtype (constructors adt)))
	   (*adt-decl* pred-decl))
      (typecheck-adt-decl pred-decl)
      (put-decl pred-decl (current-declarations-hash))
      ;; Then the (sub)type declaration
      (let* ((sdecl (mk-adt-subtype-decl
		     subtype (mk-adt-subtype subtype (constructors adt))))
	     (*adt-decl* sdecl))
	(typecheck-adt-decl sdecl)
	(put-decl sdecl (current-declarations-hash))
	(typecheck* subtype nil nil nil))))
  (let ((*generate-tccs* 'none))
    ;; Now make sure the subtype field of each constructor is typechecked
    (dolist (c (constructors adt))
      (typecheck* (subtype c) nil nil nil))
    ;; Finally set up judgements for subtypes with more than one constructor
    (dolist (c (constructors adt))
      (when (multiple-recognizer-subtypes? c (constructors adt))
	(let ((jdecl (make-instance 'subtype-judgement
		       'declared-subtype (mk-expr-as-type
					  (mk-name-expr (recognizer c)))
		       'declared-type (subtype c))))
	  (typecheck-adt-decl jdecl)
	  (put-decl jdecl (current-declarations-hash)))))))

(defun multiple-recognizer-subtypes? (c constructors)
  (some #'(lambda (cc)
	    (and (not (eq cc c))
		 (eq (id (subtype cc)) (id (subtype c)))))
	constructors))

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
      (if (cdr preds)
	  (let* ((var (make-new-variable '|x| preds))
		 (bd (mk-bind-decl var (adt-type-name *adt*)))
		 (appreds (mapcar #'(lambda (p)
				      (mk-application p (mk-name-expr var)))
			    (nreverse preds))))
	    (mk-setsubtype (adt-type-name *adt*)
			   (mk-lambda-expr (list bd)
			     (mk-disjunction appreds))))
	  (mk-expr-as-type (mk-name-expr (car preds))))
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
	(let* ((ty (when occ?
		     (typecheck (pc-parse (unparse atype :string t)
				  'dep-type-expr))))
	       (*bound-variables* (if occ?
				      (cons ty *bound-variables*)
				      *bound-variables*)))
	  (generate-adt-constructor-domain (cdr accessors)
					   (cons atype result))))))


;;; Generate the accessor constant declarations.

(defun generate-accessors (adt)
  (let* ((common-accessors (collect-common-accessors adt))
	 (acc-decls (mapcar #'(lambda (entry)
				(generate-accessor entry adt))
		      common-accessors)))
    (dolist (c (constructors adt))
      (setf (acc-decls c)
	    (find-corresponding-acc-decls
	     (arguments c) common-accessors acc-decls)))
    acc-decls))

(defun find-corresponding-acc-decls (accs common-accessors acc-decls
					  &optional result)
  (if (null accs)
      (nreverse result)
      (let ((corr-act (find-corresponding-acc-decl
		       (car accs) common-accessors acc-decls)))
	(assert corr-act)
	(find-corresponding-acc-decls
	 (cdr accs)
	 common-accessors
	 acc-decls
	 (cons corr-act result)))))

(defun find-corresponding-acc-decl (acc common-accessors acc-decls)
  (if (find-corresponding-acc-decl* acc (car common-accessors))
      (car acc-decls)
      (find-corresponding-acc-decl
       acc (cdr common-accessors) (cdr acc-decls))))

(defun find-corresponding-acc-decl* (acc common-accessors)
  (when common-accessors
    (or (memq acc (cdar common-accessors))
	(find-corresponding-acc-decl* acc (cdr common-accessors)))))

(defun generate-accessor (entry adt)
  ;; entry is of the form (((adtdecl range-type deps) adtdecl ...)
  ;;                       ((adtdecl range-type deps) adtdecl ...) ...)
  (let* ((domain (get-accessor-domain-type entry adt))
	 (range (get-accessor-range-type entry domain adt))
	 (acc-type (make-accessor-funtype domain range (caddr (caar entry))))
	 (acc-decl (mk-adt-accessor-decl (id (caaar entry)) acc-type)))
    (typecheck-adt-decl acc-decl)
    (when (cddr entry)
      (make-common-accessor-subtype-judgements (cdr entry) domain adt))
    acc-decl))

(defun make-common-accessor-subtype-judgements (adtdecls domain adt)
  (dolist (adtdecl adtdecls)
    (make-common-accessor-subtype-judgement adtdecl domain adt)))

(defun make-common-accessor-subtype-judgement (adtdecl domain adt)
  (let* ((*generate-tccs* 'none)
	 (constr (find adtdecl (constructors adt)
		      :key #'arguments :test #'memq))
	 (subtype (mk-expr-as-type (mk-name-expr (recognizer constr))))
	 (tsubtype (typecheck* subtype nil nil nil)))
    (unless (subtype-of? tsubtype domain)
      (let ((jdecl (make-instance 'subtype-judgement
		     'declared-subtype subtype
		     'declared-type domain)))
	(typecheck-adt-decl jdecl)
	(put-decl jdecl (current-declarations-hash))))))

(defun make-accessor-funtype (domain range deps)
  (if deps
      (let* ((tdom (typecheck* (copy domain) nil nil nil))
	     (dtype (if (dep-binding? tdom)
			tdom
			(mk-dep-binding (make-new-variable 'd
					  (list tdom range))
					tdom)))
	     (dep-name (make-variable-expr dtype))
	     (*bound-variables* (cons dtype *bound-variables*))
	     (subst-range (substit range
			    (pairlis
			     (mapcar #'declaration deps)
			     (mapcar #'(lambda (dep)
					 (typecheck* (mk-application (id dep)
						       dep-name)
						     (type dep) nil nil))
			       deps)))))
	(mk-funtype dtype subst-range))
      (mk-funtype domain range)))

(defun get-accessor-domain-type (entry adt)
  (let ((domain (get-accessor-domain-basic-type entry adt)))
    (if (cdr entry)
	(let* ((dep-id (make-new-variable 'd adt))
	       (dtype (typecheck* domain nil nil nil)))
	  (mk-dep-binding dep-id dtype))
	domain)))
  
(defun get-accessor-domain-basic-type (entry adt)
  (let ((acc-decls (mapappend #'cdr entry)))
    (if (= (length (mapappend #'cdr entry)) (length (constructors adt)))
	(mk-type-name (id adt))
	(if (subtypes adt)
	    (multiple-value-bind (subtypes recognizers)
		(get-accessor-covered-subtypes (copy-list acc-decls) adt)
	      (get-accessor-domain-type* subtypes recognizers))
	    (get-accessor-domain-type*
	     nil
	     (mapcan #'(lambda (c)
			 (when (some #'(lambda (acc) (memq acc acc-decls))
				     (arguments c))
			   (list (recognizer c))))
	       (constructors adt)))))))

(defun get-accessor-domain-type* (subtypes recognizers)
  (cond ((and (singleton? subtypes)
	      (null recognizers))
	 (car subtypes))
	((and (singleton? recognizers)
	      (null subtypes))
	 (let ((*generate-tccs* 'none))
	   (typecheck* (mk-expr-as-type (mk-name-expr (car recognizers)))
		       nil nil nil)))
	(t
	 (let* ((*generate-tccs* 'none)
		(preds (append subtypes recognizers))
		(var (make-new-variable 'x preds))
		(bd (mk-bind-decl var (adt-type-name *adt*)))
		(appreds (mapcar #'(lambda (p)
				     (if (subtype? p)
					 (mk-application (id (print-type p))
					   (mk-name-expr var))
					 (mk-application p
					   (mk-name-expr var))))
			   preds))
		(pred (mk-lambda-expr (list bd) (mk-disjunction appreds)))
		(subtype (mk-setsubtype (adt-type-name *adt*) pred)))
	   (typecheck* subtype nil nil nil)))))

(defun get-accessor-covered-subtypes (accs adt &optional subtypes recs)
  (if (null accs)
      (values (nreverse subtypes) (nreverse recs))
      (let ((constr (find (car accs) (constructors adt)
			  :key #'arguments :test #'memq)))
	(assert constr)
	(multiple-value-bind (subtype-accs subtype-recs covers?)
	    (get-accessors-for-subtype
	     (car accs) (subtype constr) (constructors adt))
	  (declare (ignore subtype-recs))
	  ;; subtype-accs has all accessors of that name for the given subtype
	  ;; covers? is true if every constructor of that subtype has an
	  ;; accessor of that name
	  (multiple-value-bind (in-accs out-accs)
	      (split-on #'(lambda (acc) (memq acc subtype-accs)) accs)
	    (if covers?
		;; We got it covered - can use the subtype
		(get-accessor-covered-subtypes
		 out-accs
		 adt
		 (cons (type-value (declaration (subtype constr))) subtypes)
		 recs)
		(get-accessor-covered-subtypes
		 out-accs
		 adt
		 subtypes
		 (append recs
			 (mapcan #'(lambda (c)
				     (when (some #'(lambda (acc)
						     (memq acc in-accs))
						 (arguments c))
				       (list (recognizer c))))
			   (constructors adt))))))))))

(defun get-accessors-for-subtype (acc subtype constructors
				      &optional accs recs (covered? t))
  (if (null constructors)
      (values (nreverse accs) (nreverse recs) covered?)
      (if (same-id (subtype (car constructors)) subtype)
	  (let ((sim-acc (find acc (arguments (car constructors))
			       :test #'same-id)))
	    (if sim-acc
		(get-accessors-for-subtype
		 acc subtype (cdr constructors)
		 (cons sim-acc accs)
		 (cons (recognizer (car constructors)) recs)
		 covered?)
		(get-accessors-for-subtype acc subtype (cdr constructors)
					   accs recs nil)))
	  (get-accessors-for-subtype acc subtype (cdr constructors)
				     accs recs covered?))))

(defun get-accessor-range-type (entry domain adt)
  (assert (or (null (cdr entry)) (dep-binding? domain)))
  (if (cdr entry)
      (let* (;;(dtype (if (dep-binding? domain) (type domain) domain))
	     (dvar (when (dep-binding? domain)
		     (make-variable-expr domain)))
	     (suptype (reduce #'compatible-type (mapcar #'cadar entry)))
	     (tid (make-new-variable 'x adt))
	     (tbd (make-bind-decl tid suptype))
	     (tvar (make-variable-expr tbd))
	     (pred (get-accessor-range-type-pred entry dvar tvar adt)))
	(mk-subtype suptype
	  (make-instance 'set-expr
	    'bindings (list tbd)
	    'expression pred)))
      (cadaar entry)))

(defun get-accessor-range-type-pred (entry dvar tvar adt)
  (let* ((vars (caddr (caar entry)))
	 (subst-alist
	      (pairlis
	       (mapcar #'declaration vars)
	       (mapcar #'(lambda (dep)
			   (typecheck* (mk-application (id dep) dvar)
				       (type dep) nil nil))
		 vars)))
	 (epred (subtype-pred (substit (cadaar entry) subst-alist)
			      (type tvar)))
	 (tpred (make!-application epred tvar)))
    (if (null (cdr entry))
	tpred
	(let* ((adtype (get-accessor-domain-type (list (car entry)) adt))
	       (dtype (if (dep-binding? adtype) (type adtype) adtype))
	       (pred (predicate dtype))
	       (cond (make!-application pred dvar))
	       (else (get-accessor-range-type-pred
		      (cdr entry) dvar tvar adt)))
	  (make!-if-expr cond tpred else)))))
  

;;; The common-accessors list is of the form
;;;  ( (((acc-decl type freevars) acc-decl ...)
;;;     ((acc-decl type freevars) acc-decl ...))
;;;    (((acc-decl type freevars) acc-decl ...)) ... )
(defun collect-common-accessors (adt)
  (let ((common-accessors nil))
    (dolist (c (constructors adt))
      (let ((*bound-variables* nil))
	(dolist (a (arguments c))
	  (let* ((copy-type (pc-parse (unparse (declared-type a) :string t)
			      'type-expr))
		 (type (with-no-type-errors (typecheck copy-type))))
	    (assert type)
	    (push (typecheck* (mk-dep-binding (id a) copy-type type)
			      nil nil nil)
		  *bound-variables*)
	    (let* ((arg&type (list a type
				   (sort-freevars (freevars type))))
		   (entry (assoc arg&type common-accessors
				 :test #'compatible-arg&type
				 :key #'car)))
	      (if entry
		  (let ((subentry (assoc arg&type entry
					 :test #'same-arg&type)))
		    (if subentry
			(nconc subentry (list a))
			(nconc entry (list (list arg&type a)))))
		  (setq common-accessors
			(nconc common-accessors
			       (list (list (list arg&type a)))))))))))
    common-accessors))

(defun compatible-arg&type (a&t1 a&t2)
  (and (same-id (car a&t1) (car a&t2))
       (multiple-value-bind (bindings mismatch?)
	   (collect-same-arg&type-bindings (caddr a&t1) (caddr a&t2))
	 (declare (ignore bindings))
	 (unless mismatch?
	   (compatible? (cadr a&t1) (cadr a&t2))))))

(defun same-arg&type (a&t1 a&t2)
  (and (same-id (car a&t1) (car a&t2))
       (let ((bindings (collect-same-arg&type-bindings
			(caddr a&t1) (caddr a&t2))))
	 (tc-eq-with-bindings (cadr a&t1) (cadr a&t2) bindings))))

(defun collect-same-arg&type-bindings (deps1 deps2 &optional bindings)
  ;; bindings are sorted already
  (if (null deps1)
      (if (null deps2)
	  bindings
	  (values nil t))
      (if (and deps2
	       (same-id (car deps1) (car deps2))
	       (tc-eq-with-bindings (type (car deps1)) (type (car deps2))
				    bindings))
	  (collect-same-arg&type-bindings
	   (cdr deps1) (cdr deps2)
	   (acons (declaration (car deps1)) (declaration (car deps2))
		  bindings))
	  (values nil t))))


(defun mk-recognizer-type (rec-id adt)
  (declare (ignore adt))
  (let ((rpred (mk-name-expr rec-id)))
    (mk-expr-as-type rpred)))

;;; Ord axiom

(defun generate-adt-uninterpreted-ord-function-and-axiom (adt)
  (generate-adt-uninterpreted-ord-function adt)
  (generate-adt-uninterpreted-ord-axiom adt))

(defun generate-adt-uninterpreted-ord-function (adt)
  (let ((len (length (constructors adt)))
	(fname (makesym "~a_ord" (id adt))))
    (typecheck-adt-decl
     (mk-const-decl fname
       (mk-funtype (mk-type-name (id adt))
		   (make-instance 'type-application
		     'type (mk-type-name '|upto|)
		     'parameters (list (mk-number-expr (1- len)))))))))

(defun generate-adt-uninterpreted-ord-axiom (adt)
  (let* ((fname (makesym "~a_ord" (id adt))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_ord_defaxiom" (id adt))
       (mk-conjunction
	(generate-adt-uninterpreted-ord-axiom-conjuncts
	 (constructors adt) fname 0))
       'AXIOM))))

(defun generate-adt-uninterpreted-ord-axiom-conjuncts (constrs fname num
							       &optional conjs)
  (if (null constrs)
      (nreverse conjs)
      (generate-adt-uninterpreted-ord-axiom-conjuncts
       (cdr constrs) fname (1+ num)
       (let ((conj (generate-adt-uninterpreted-ord-conjunct
		     (car constrs) fname num)))
	 (cons conj conjs)))))

(defun generate-adt-uninterpreted-ord-conjunct (c fname num)
  (if (arguments c)
      (let* ((bindings (mapcar #'bind-decl (arguments c)))
	     (vars (mapcar #'mk-name-expr bindings)))
	(mk-forall-expr (mapcar #'bind-decl (arguments c))
	  (mk-application '=
	    (mk-application fname
	      (mk-application* (id c) vars))
	    (mk-number-expr num))))
      (mk-application '=
	(mk-application fname (mk-name-expr (id c)))
	(mk-number-expr num))))

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
	(copy-lex (declared-type bd) (declared-type a))
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
		(typep (get-theory (generated-by th)) 'recursive-type)
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
				      (let ((fdecl (if (name? y)
						       (declaration y)
						       (declaration (print-type y)))))
					(and (typep fdecl 'formal-type-decl)
					     (same-id x fdecl))))))
	       (and (typep (find-supertype (type-value (car acts))) 'type-name)
		    (pos-datatype-adt?
		     adt-type
		     (actuals (find-supertype (type-value (car acts))))
		     adt)))
	   (pos-datype-adt?* adt-type (cdr acts) (cdr formals) adt))))

(defun all-adt-types-are-positive (adt)
  (every #'(lambda (fml)
	     (or (not (typep fml 'type-decl))
		 (member fml (positive-types adt)
			 :test #'(lambda (x y)
				   (tc-eq (type-value x) y)))))
	 (formals (adt-theory adt))))

(defun some-adt-type-is-positive (adt)
  (some #'(lambda (fml)
	    (and (typep fml 'type-decl)
		 (member fml (positive-types adt)
			 :test #'(lambda (x y)
				   (tc-eq (type-value x) y)))))
	 (formals (adt-theory adt))))

(defun generate-adt-axioms (adt)
  (generate-adt-extensionalities (constructors adt) adt)
  (generate-adt-accessor-axioms (constructors adt) adt)
  (generate-inclusive-axiom adt)
  (generate-disjoint-axiom adt))

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
	 (var2 (copy var 'id (makesym "~a2" (id var))))
	 (*generate-tccs* 'none))
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
			      (if (some #'(lambda (cc)
					    (eq (recognizer cc) (id a)))
					(constructors adt))
				  (mk-coercion (mk-application (id a) var)
					       (type a))
				  (mk-application (id a) var))
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
				  (current-theory-name) (type nbd)))))
	(adt-forall-bindings (cdr vars)
			     (substit (cdr bds)
			       (acons (car bds) var nil))
			     (cons nbd result)))))

(defparameter *disjoint-axiom-expansion-maxsize* 100)

(defun generate-disjoint-axiom (adt)
  (let* ((varid (makesym "~a_var" (id adt)))
	 (bd (make-bind-decl varid (adt-type-name adt)))
	 (var (make-variable-expr bd))
	 (*generate-tccs* 'none)
	 (appls (mapcar #'(lambda (c)
			    (let ((recname
				   (mk-name-expr (recognizer c)
				     nil nil
				     (make-resolution (rec-decl c)
				       (current-theory-name)
				       (type (rec-decl c))))))
			      (make!-application recname var)))
		  (constructors adt))))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_disjoint" (id adt))
       (make!-forall-expr (list bd)
	 (if (<= (length appls) *disjoint-axiom-expansion-maxsize*)
	     (make!-conjunction* (make-disjoint-pairs appls))
	     (let ((list-type (typecheck* (pc-parse "list[bool]" 'type-expr)
					  nil nil nil)))
	       (make-application (mk-name-expr '|pairwise_disjoint?|)
		 (make-list-expr appls list-type)))))
       'AXIOM))))

(defun make-disjoint-pairs (appls &optional result)
  (if (null (cdr appls))
      result
      (make-disjoint-pairs
       (cdr appls)
       (nconc result
	      (mapcar #'(lambda (a)
			   (make!-negation
			    (make!-conjunction (car appls) a)))
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
  (let ((indvar (make-new-variable '|p| adt)))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_induction" (id adt))
       (mk-forall-expr (list (mk-bind-decl indvar
			       (mk-predtype (adt-type-name adt))))
	 (mk-implication (gen-induction-hypothesis adt indvar)
			 (gen-induction-conclusion adt indvar)))
       'AXIOM))))

(defmethod gen-induction-hypothesis ((adt datatype) indvar &optional ign)
  (declare (ignore ign))
  (mk-conjunction
   (mapcar #'(lambda (c) (gen-induction-hypothesis c indvar adt))
	   (constructors adt))))

(defmethod gen-induction-hypothesis ((c simple-constructor) indvar
				     &optional adt)
  (if (arguments c)
      (let* ((subst-alist (adt-subst-alist (arguments c)))
	     (arghyps (acc-induction-hypotheses (arguments c) indvar adt
						subst-alist))
	     (ppappl (mk-application indvar
		       (mk-application* (id c)
			 (mapcar #'get-adt-var-name (arguments c)))))
	     (indh (mk-forall-expr (adt-forall-bindings
				    (mapcar #'get-adt-var-name (arguments c))
				    (mapcar #'bind-decl (arguments c)))
		     (if arghyps
			 (mk-implication (mk-conjunction arghyps) ppappl)
			 ppappl))))
	(pc-parse (unparse indh :string t) 'expr))
      (mk-application indvar (mk-name-expr (id c)))))

(defun adt-subst-alist (args)
  (adt-subst-alist* (mapcar #'get-adt-var-name args)
		    (mapcar #'bind-decl args)
		    (mapcar #'bind-decl args)))

(defun adt-subst-alist* (vars bds sbds &optional result)
  (if (null vars)
      (nreverse result)
      (let* ((nbd (mk-bind-decl (id (car vars))
		   (declared-type (car sbds)) (type (car sbds))))
	     (var (mk-name-expr (id (car vars)) nil nil
				(make-resolution nbd
				  (current-theory-name) (type nbd)))))
	(adt-subst-alist* (cdr vars)
			  (cdr bds)
			  (substit (cdr sbds)
			    (acons (car sbds) var nil))
			  (acons (car bds) var result)))))

(defun get-adt-var-name (arg)
  (make-instance 'name-expr 'id (id (get-adt-var arg))))

(defun acc-induction-hypotheses (args indvar adt subst-alist &optional result)
  (if (null args)
      (nreverse result)
      (let ((hyp (acc-induction-hypothesis (car args) indvar adt subst-alist)))
	(acc-induction-hypotheses (cdr args) indvar adt subst-alist
				  (if hyp (cons hyp result) result)))))


;;; Generate the induction hypothesis according to the type of the argument.
;;; It is either the datatype itself, a sequence of the datatype, or occurs as
;;; a positive actual parameter to a different datatype.  In every other case,
;;; return NIL.

(defun acc-induction-hypothesis (arg indvar adt subst-alist)
  (let ((pred (acc-induction-hypothesis* (substit (type arg) subst-alist)
					 indvar adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (mk-application pred (get-adt-var-name arg)))))

(defmethod acc-induction-hypothesis* ((te type-name) indvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (mk-name-expr indvar))
	((adt? te)
	 (let* ((acts (actuals-corresponding-to-positive-types
		       (actuals (module-instance te))
		       (adt? te)))
		(preds (mapcar #'(lambda (act)
				   (acc-induction-hypothesis*
				    (type-value act) indvar adt))
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

(defun actuals-corresponding-to-positive-types (acts adt)
  (actuals-corresponding-to-positive-types*
   acts (formals-sans-usings adt) (positive-types adt)))

(defun actuals-corresponding-to-positive-types* (acts formals postypes
						      &optional posacts)
  (if (null acts)
      (nreverse posacts)
      (actuals-corresponding-to-positive-types*
       (cdr acts)
       (cdr formals)
       postypes
       (if (member (car formals) postypes :test #'same-id)
	   (cons (car acts) posacts)
	   posacts))))

(defmethod acc-induction-hypothesis* ((te subtype) indvar adt)
  (acc-induction-hypothesis* (supertype te) indvar adt))

(defmethod acc-induction-hypothesis* ((te funtype) indvar adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (pred (acc-induction-hypothesis*
		(if (typep (domain te) 'dep-binding)
		    (substit (range te) (acons (domain te) fvar nil))
		    (range te))
		indvar adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (if (tc-eq (domain te) *naturalnumber*)
	  (mk-application '|every| pred)
	  (let* ((lid (make-new-variable '|f| (list te pred)))
		 (lbd (make-bind-decl lid te))
		 (lvar (mk-name-expr lid nil nil
				     (make-resolution lbd
				       (current-theory-name) te))))
	    (mk-lambda-expr (list lbd)
	      (mk-forall-expr (list fbd)
		(mk-application pred
		  (mk-application lvar fvar)))))))))

(defmethod acc-induction-hypothesis* ((te recordtype) indvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (preds (acc-induction-fields (fields te) rvar indvar adt
				      (dependent? te))))
    (unless (every #'(lambda (p) (or (null p) (everywhere-true? p))) preds)
      (mk-lambda-expr (list rbd)
	(mk-conjunction
	 (mapcan #'(lambda (fd pred)
		     (when pred
		       (list (mk-application pred
			       (make-field-application (id fd) rvar)))))
	   (fields te) preds))))))

(defun acc-induction-fields (fields rvar indvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-induction-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar indvar adt dep?
       (cons (acc-induction-hypothesis* (type (car fields))
					indvar adt)
	     result))))

(defmethod acc-induction-hypothesis* ((te tupletype) indvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (preds (acc-induction-tuples (types te) tvar indvar adt)))
    (unless (every #'null preds)
      (mk-lambda-expr (list tbd)
	(mk-conjunction
	 (let ((num 0))
	   (mapcan #'(lambda (pred)
		       (incf num)
		       (when pred
			 (list (mk-application pred
				 (make-instance 'projappl
				   'id (makesym "PROJ_~d" num)
				   'index num
				   'argument tvar)))))
	     preds)))))))

(defun acc-induction-tuples (types tvar indvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-induction-tuples
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       tvar indvar adt (1+ index)
       (cons (acc-induction-hypothesis* (car types) indvar adt) result))))

(defmethod acc-induction-hypothesis* ((te cotupletype) indvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (preds (acc-induction-tuples (types te) tvar indvar adt)))
    (unless (every #'null preds)
      (mk-lambda-expr (list tbd)
	(let ((num 0)
	      (varid (make-new-variable 'x adt)))
	  (mk-cases-expr tvar
	    (mapcar #'(lambda (pred type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  'id (makesym "IN_~d" num)
					  'index num))
			       (sel-expr (if pred
					     (mk-application pred var)
					     *true*)))
			  (mk-selection in-expr (list bd) sel-expr)))
	      preds (types te))
	    nil))))))

(defmethod acc-induction-hypothesis* ((te dep-binding) indvar adt)
  (acc-induction-hypothesis* (type te) indvar adt))

(defmethod acc-induction-hypothesis* ((te type-expr) indvar adt)
  (declare (ignore indvar adt))
  ;;(mk-everywhere-true-function te)
  )

(defun mk-everywhere-true-function (type)
  (make!-lambda-expr
      (list (mk-bind-decl (make-new-variable '|x| type) type type))
    (copy *true*)))

(defun mk-everywhere-false-function (type)
  (make!-lambda-expr
      (list (mk-bind-decl (make-new-variable '|x| type) type type))
    (copy *false*)))

(defmethod gen-induction-conclusion ((adt datatype) indvar)
  (let ((tvar (mk-name-expr (makesym "~a_var" (id adt)))))
    (mk-forall-expr (list (mk-bind-decl (id tvar)
			      (mk-type-name (id adt))))
      (mk-application indvar tvar))))


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
	 (cases2 (when (datatype? adt)
		   (mk-cases-expr avar
		     (mapcar #'(lambda (c)
				 (generate-adt-predicate-selection
				  c pvars ptypes adt function-id nil))
		       (constructors adt))
		     nil)))
	 (fargs (list (mapcar #'mk-arg-bind-decl
			(mapcar #'id pvars)
			(mapcar #'(lambda (pty)
				    (mk-type-name 'PRED
				      (list (mk-actual pty))))
			  ptypes))
		      (list (mk-arg-bind-decl (id avar)
					      (adt-type-name adt))))))
    (typecheck-adt-decl
     (if (datatype? adt)
	 (mk-adt-def-decl function-id (copy *boolean*)
			  (pc-parse (unparse cases :string t) 'expr) fargs)
	 (if (eq function-id '|every|)
	     (mk-coinductive-decl function-id (copy *boolean*)
				  (pc-parse (unparse cases :string t) 'expr)
				  fargs)
	     (mk-inductive-decl function-id (copy *boolean*)
				(pc-parse (unparse cases :string t) 'expr)
				fargs))))
    (when (datatype? adt) 
      (typecheck-adt-decl
       (mk-adt-def-decl function-id (copy *boolean*)
			(pc-parse (unparse cases2 :string t) 'expr)
			(append (apply #'append (butlast fargs))
				(car (last fargs))))))))

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
					       (current-theory-name)
					       (type b))))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (let ((exprs (acc-predicate-selections
			(arguments c) vars pvars ptypes adt
			function-id curried?)))
	    (if (eq function-id '|every|)
		(mk-conjunction exprs)
		(mk-disjunction exprs)))))
      (mk-selection (mk-name-expr (id c)) nil
		    (copy (if (eq function-id '|every|) *true* *false*)))))

(defun acc-predicate-selections (args vars pvars ptypes adt function-id
				      curried? &optional selections)
  (if (null vars)
      (nreverse selections)
      (let ((sel (acc-predicate-selection (car vars) (type (car vars))
					  pvars ptypes adt
					  function-id curried?))
	    (nvars (substit (cdr vars)
		     (acons (bind-decl (car args)) (car vars) nil))))
	(acc-predicate-selections (cdr args) nvars pvars ptypes adt
				  function-id curried?
				  (cons sel selections)))))


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
				   (current-theory-name) (domain te))))
	     (fun (acc-predicate-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) fvar nil))
		       (range te))
		   pvars ptypes adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (if (eq funid '|every|)
		(mk-forall-expr (list fbd)
		  (mk-application fun
		    (mk-application (copy arg) (copy fvar))))
		(mk-exists-expr (list fbd)
		  (mk-application fun
		    (mk-application (copy arg) (copy fvar)))))))))

(defmethod acc-predicate-selection (arg (te recordtype) pvars ptypes adt
					funid curried?)
  (declare (ignore curried?))
  (let* ((rbd (make-bind-decl (id arg) te))
	 (rvar (mk-name-expr (id arg) nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
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
				 (make-instance 'projappl
				   'id (makesym "PROJ_~d" num)
				   'index num
				   'argument (copy arg))))))
	     preds))))))

(defmethod acc-predicate-selection (arg (te cotupletype) pvars ptypes adt
					funid curried?)
  (declare (ignore curried?))
  (let ((preds (acc-predicate-types (types te) arg pvars ptypes adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(let ((num 0)
	      (varid (make-new-variable 'x adt)))
	  (mk-cases-expr arg
	    (mapcar #'(lambda (pred type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  'id (makesym "IN_~d" num)
					  'index num))
			       (sel-expr (if pred
					     (mk-application pred var)
					     (if (eq funid '|every|)
						 *true* *false*))))
			  (mk-selection in-expr (list bd) sel-expr)))
	      preds (types te))
	    nil)))))

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
				   (current-theory-name) (domain te))))
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
					 (current-theory-name) te))))
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
			       (current-theory-name) te)))
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
			       (current-theory-name) te)))
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
				   (make-instance 'projappl
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument tvar)))))
	       preds)))))))

(defmethod acc-predicate-selection* ((te cotupletype) pvars ptypes adt funid)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (preds (acc-predicate-types (types te) tvar pvars ptypes adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable 'x adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (pred type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    'id (makesym "IN_~d" num)
					    'index num))
				 (sel-expr (if pred
					       (mk-application pred var)
					       (if (eq funid '|every|)
						   *true* *false*))))
			    (mk-selection in-expr (list bd) sel-expr)))
		preds (types te))
	      nil))))))

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
	     (or (some-adt-type-is-positive adt)
		 (pvs-warning
		     "No map is generated since the datatype has no strictly ~
                      positive formal type parameters")))
    (build-adt-theory (makesym "~a_~a_map"
			       (id adt)
			       (if (datatype? adt) "adt" "codt"))
	adt
      (let* ((*generate-tccs* 'none)
	     (fpairs (adt-map-formal-pairs (formals (adt-theory adt)) adt)))
	(generate-adt-map-formals fpairs)
	(generate-adt-map-using adt)
	(generate-adt-map fpairs adt)
	(generate-adt-every-rel fpairs adt)))))

(defun adt-map-formal-pairs (formals adt &optional pairs)
  (if (null formals)
      (nreverse pairs)
      (cond ((importing? (car formals))
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
				     adt
				     (if nil ;(eq nusing (car formals))
					 pairs
					 (acons (car formals) nusing pairs)))))
	    ((or (formal-const-decl? (car formals))
		 (not (member (car formals)
			      (positive-types adt)
			      :test #'(lambda (x y)
					(let ((fdecl (if (name? y)
							 (declaration y)
							 (declaration (print-type y)))))
						(and (typep fdecl 'formal-type-decl)
						     (same-id x fdecl)))))))
	     (let ((nfml (copy-adt-formals (car formals))))
	       (adt-map-formal-pairs (cdr formals)
				     adt
				     (acons nfml nfml pairs))))
	    (t (let* ((fml (copy-adt-formals (car formals)))
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
						  (declared-type fml) pairs)))))))
		 (adt-map-formal-pairs (cdr formals) adt
				       (acons fml nfml pairs)))))))

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
  (let* ((mod (mk-modname (makesym "~a_~a" (id adt)
				   (if (datatype? adt) "adt" "codt"))))
	 (imp (make-instance 'importing
		'theory-name mod)))
    (dolist (im (importings adt))
      (typecheck-adt-decl im))
    (typecheck-adt-decl imp)))

(defun generate-adt-map (fpairs adt)
  (let* ((*generate-tccs* 'none)
	 (postype-pairs
	  (remove-if-not
	      #'(lambda (fp)
		  (member (car fp)
			  (positive-types adt)
			  :test #'(lambda (x y)
				    (let ((fdecl
					   (if (name? y)
					       (declaration y)
					       (declaration (print-type y)))))
				      (and (typep fdecl 'formal-type-decl)
					   (same-id x fdecl))))))
	    fpairs))
	 (ptypes (mapcar #'type-value (mapcar #'car postype-pairs)))
	 (fvars (generate-adt-map-fvars '|f| postype-pairs adt))
	 (avar (mk-name-expr (make-new-variable '|a| adt)))
	 (adt-subtypes (find-adt-subtypes adt))
	 (rtype (typecheck
		    (mk-type-name (id adt)
		      (mapcan #'(lambda (fp)
				  (unless (typep (car fp) 'importing)
				    (list (mk-actual
					   (mk-name-expr (id (cdr fp)))))))
			fpairs))))
	 (frtype (if (null adt-subtypes)
		     rtype
		     (generate-adt-map-subtypes-rangetype
		      adt-subtypes adt postype-pairs avar rtype)))
	 (adtinst (typecheck (mk-map-adtinst fpairs adt)))
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
	 (fargs (adt-map-formals-arguments fvars postype-pairs fpairs avar adt))
	 (cdecl (mk-adt-def-decl
		    '|map| frtype
		    (pc-parse (unparse curried-cases :string t) 'expr)
		    fargs))
	 (uncdecl (mk-adt-def-decl
		      '|map| frtype
		      (pc-parse (unparse uncurried-cases :string t) 'expr)
		      (append (apply #'append (butlast fargs))
			      (car (last fargs))))))
    (typecheck-adt-decl cdecl)
    (typecheck-adt-decl uncdecl)))

(defun generate-adt-map-subtypes-rangetype (adt-subtypes adt fpairs avar rtype)
  (let* ((xvar (mk-name-expr (make-new-variable '|x| adt)))
	 (xbd (mk-bind-decl (id xvar) rtype))
	 (srcinst (mk-map-adtinst-src fpairs adt))
	 (tgtinst (mk-map-adtinst-tgt fpairs adt))
	 (pred (generate-adt-map-subtypes-rangetype-pred
		adt-subtypes avar xvar srcinst tgtinst)))
    (mk-subtype rtype
      (make-instance 'set-expr
	'bindings (list xbd)
	'expression pred))))

(defun generate-adt-map-subtypes-rangetype-pred (adt-subtypes
						 avar xvar srcinst tgtinst
						 &optional preds)
  (if (null adt-subtypes)
      (mk-conjunction (nreverse preds))
      (generate-adt-map-subtypes-rangetype-pred
       (cdr adt-subtypes) avar xvar srcinst tgtinst
       (cons (generate-adt-map-subtypes-rangetype-pred*
	      (car adt-subtypes) avar xvar srcinst tgtinst)
	     preds))))

(defun generate-adt-map-subtypes-rangetype-pred* (adt-subtype avar xvar
							      srcinst tgtinst)
  (let ((pred
	 (mk-application (mk-name-expr '=>)
	   (mk-application (subst-mod-params (predicate adt-subtype) srcinst)
	     avar)
	   (mk-application (subst-mod-params (predicate adt-subtype) tgtinst)
	     xvar))))
    (setf (parens pred) 1)
    pred))

(defun mk-map-adtinst (fpairs adt)
  (typecheck (mk-type-name (id adt)
	       (mapcar #'(lambda (fp)
			   (mk-actual (mk-name-expr (id (car fp)))))
		       fpairs))))

(defun mk-map-adtinst-src (fpairs adt)
  (typecheck (mk-modname (makesym "~a_adt" (id adt))
	       (mapcar #'(lambda (fp)
			   (mk-actual (mk-name-expr (id (car fp)))))
		       fpairs))))

(defun mk-map-adtinst-tgt (fpairs adt)
  (typecheck (mk-modname (makesym "~a_adt" (id adt))
	       (mapcar #'(lambda (fp)
			   (mk-actual (mk-name-expr (id (cdr fp)))))
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
			      (mapcan #'adt-map-formals-actuals
				      fpairs))))))

(defun adt-map-formals-actuals (fp)
  (unless (importing? (car fp))
    (list (mk-actual (mk-name-expr (id (car fp)))))))

(defun generate-adt-map-fvars (id fptypes adt)
  (if (singleton? fptypes)
	(list (mk-name-expr (make-new-variable id adt)))
	(generate-adt-map-fvars* id fptypes adt 0 nil)))

(defun generate-adt-map-fvars* (id fptypes adt ctr fvars)
  (if (null fptypes)
      (nreverse fvars)
      (generate-adt-map-fvars*
       id (cdr fptypes) adt (1+ ctr)
       (cons (mk-name-expr (make-new-variable id (cons adt fvars)
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
					       (current-theory-name)
					       (type b))))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (mk-application* (id c)
	    (acc-map-selections vars (arguments c) pvars ptypes fpairs
				adt adtinst curried?))))
      (mk-selection (mk-name-expr (id c)) nil
	(mk-name-expr (id c)))))

(defun acc-map-selections (vars accs pvars ptypes fpairs adt adtinst curried?
				&optional sels)
  (if (null vars)
      (nreverse sels)
      (let ((sel (acc-map-selection (car vars)
				    (subst-mod-params 
				     (type (car vars))
				     (module-instance adtinst))
				    pvars ptypes fpairs adt curried?))
	    (nvars (mapcar #'(lambda (v)
			       (lcopy v
				 'type (gensubst (type v)
					 #'(lambda (ex)
					     (declare (ignore ex))
					     (car vars))
					 #'(lambda (ex)
					     (and (name-expr? ex)
						  (eq (declaration ex)
						      (bind-decl (car accs))))))))
		     (cdr vars))))
	(acc-map-selections nvars (cdr accs) pvars ptypes fpairs
			    adt adtinst curried? (cons sel sels)))))
  

;; (defun instantiate-adt-types (type adtinst)
;;   (let ((*parsing-or-unparsing* t))
;;     (gensubst type
;;       #'(lambda (x) (instantiate-adt-types! x adtinst))
;;       #'(lambda (x) (instantiate-adt-types? x adtinst)))))

;; (defmethod instantiate-adt-types? ((type type-name) adtinst)
;;   (same-id type adtinst))

;; (defmethod instantiate-adt-types? ((act actual) adtinst)
;;   (and (type-value act)
;;        (instantiate-adt-types? (type-value act) adtinst)))

;; (defmethod instantiate-adt-types? (obj adtinst)
;;   (declare (ignore obj adtinst)))

;; (defmethod instantiate-adt-types! ((type type-name) adtinst)
;;   (copy-all adtinst))

;; (defmethod instantiate-adt-types! ((act actual) adtinst)
;;   (let* ((ntype (instantiate-adt-types! (type-value act) adtinst))
;; 	 (nexpr (mk-name-expr (id ntype) (actuals ntype))))
;;     (setf (types nexpr) (list 'type)
;; 	  (resolutions nexpr) (list (resolution ntype)))
;;     (make-instance 'actual
;;       'type-value ntype
;;       'expr nexpr)))

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
				 (acc-map-selection* (raise-actuals (type-value act))
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
			       (current-theory-name) (domain te))))
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
		       (let ((proj (make-instance 'projappl
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument (copy arg))))
			 (if (identity-fun? map)
			     proj
			     (mk-application map proj))))
	     maps))))))

(defmethod acc-map-selection (arg (te cotupletype) pvars ptypes fpairs
				  adt curried?)
  (declare (ignore curried?))
  (let ((maps (acc-map-selection-types
	       (types te) arg pvars ptypes fpairs adt)))
    (if (every #'identity-fun? maps)
	(copy arg)
	(mk-cases-expr arg
	  (let ((num 0)
		(varid (make-new-variable 'x adt)))
	    (mapcar #'(lambda (map type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  'id (makesym "IN_~d" num)
					  'index num))
			       (sel-expr (make-instance 'injection-application
					   'id (makesym "IN_~d" num)
					   'index num
					   'actuals (subst-map-actuals
						     (list (mk-actual te))
						     fpairs)
					   'argument (mk-application map
						       var))))
			  (mk-selection in-expr (list bd) sel-expr)))
	      maps (types te)))
	  nil))))

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
			       (current-theory-name) (domain te))))
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
					(current-theory-name) te))))
	     (mk-lambda-expr (list lbd)
	       (mk-lambda-expr (list fbd)
		 (mk-application map
		   (mk-application lvar fvar)))))))))

(defmethod acc-map-selection* ((te recordtype) pvars ptypes fpairs adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
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
			       (current-theory-name) te)))
	 (maps (acc-map-selection-types
		(types te) tvar pvars ptypes fpairs adt)))
    (if (every #'identity-fun? maps)
	(mk-identity-fun te)
	(mk-lambda-expr (list tbd)
	  (mk-tuple-expr
	   (let ((num 0))
	     (mapcar #'(lambda (map)
			 (incf num)
			 (let ((proj (make-instance 'projappl
				       'id (makesym "PROJ_~d" num)
				       'index num
				       'argument tvar)))
			   (if (identity-fun? map)
			       proj
			       (mk-application map proj))))
	       maps)))))))

(defmethod acc-map-selection* ((te cotupletype) pvars ptypes fpairs adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (maps (acc-map-selection-types
		(types te) tvar pvars ptypes fpairs adt)))
    (if (every #'identity-fun? maps)
	(mk-identity-fun te)
	(mk-lambda-expr (list tbd)
	  (mk-cases-expr tvar
	    (let ((num 0)
		  (varid (make-new-variable 'x adt)))
	      (mapcar #'(lambda (map type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    'id (makesym "IN_~d" num)
					    'index num))
				 (sel-expr (make-instance 'injection-application
					     'id (makesym "IN_~d" num)
					     'index num
					     'actuals (subst-map-actuals
						       (list (mk-actual te))
						       fpairs)
					     'argument (mk-application map
							 var))))
			    (mk-selection in-expr (list bd) sel-expr)))
		maps (types te)))
	    nil)))))

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

(defun generate-adt-every-rel (fpairs adt)
  (let* ((postype-pairs
	  (remove-if-not
	      #'(lambda (fp)
		  (member (car fp)
			  (positive-types adt)
			  :test #'(lambda (x y)
				    (let ((fdecl
					   (if (name? y)
					       (declaration y)
					       (declaration (print-type y)))))
				      (and (typep fdecl 'formal-type-decl)
					   (same-id x fdecl))))))
	    fpairs))
	 (ptypes (mapcar #'type-value (mapcar #'car postype-pairs)))
	 (fvars (generate-adt-map-fvars 'R ptypes adt))
	 (avid (make-new-variable '|x| adt))
	 (bvid (make-new-variable '|y| adt))
	 (atype (typecheck
		    (mk-type-name (id adt)
		      (mapcar #'(lambda (fp)
				  (mk-actual (mk-name-expr (id (car fp)))))
			fpairs))))
	 (btype (typecheck
		    (mk-type-name (id adt)
		      (mapcar #'(lambda (fp)
				  (mk-actual (mk-name-expr (id (cdr fp)))))
			fpairs))))
	 (abd (make-bind-decl avid atype))
	 (bbd (make-bind-decl bvid btype))
	 (avar (make-variable-expr abd))
	 (bvar (make-variable-expr bbd))
	 (form
	  (mk-disjunction 
	   (mapcar #'(lambda (c)
		       (generate-adt-every-rel-case
			c fvars avar bvar ptypes fpairs adt))
	     (constructors adt))))
	 (fargs (adt-every-formals-arguments
		 fvars postype-pairs fpairs avar bvar adt))
	 (cdecl (mk-adt-def-decl
		    '|every| *boolean*
		    (pc-parse (unparse form :string t) 'expr)
		    fargs)))
    (typecheck-adt-decl cdecl)))

(defun adt-every-formals-arguments (fvars fptypes fpairs avar bvar adt)
  (list (mapcar #'mk-arg-bind-decl
		(mapcar #'id fvars)
		(mapcar #'(lambda (fp)
			    (mk-funtype (list
					 (copy (type-value (car fp))
					   'resolutions nil)
					 (copy (type-value (cdr fp))
					   'resolutions nil))
					*boolean*))
			fptypes))
	(list (mk-arg-bind-decl (id avar)
		(mk-type-name (id adt)
			      (mapcan #'(lambda (fp)
					  (unless (typep (car fp) 'importing)
					    (list (mk-actual
						   (mk-name-expr
						       (id (car fp)))))))
				      fpairs)))
	      (mk-arg-bind-decl (id bvar)
		(mk-type-name (id adt)
			      (mapcan #'(lambda (fp)
					  (unless (typep (car fp) 'importing)
					    (list (mk-actual
						   (mk-name-expr
						       (id (cdr fp)))))))
				      fpairs))))))

(defun generate-adt-every-rel-case (c pvars avar bvar ptypes fpairs adt)
  (let* ((arec (mk-application (mk-name-expr (recognizer c)) avar))
	 (brec (mk-application (mk-name-expr (recognizer c)) bvar))
	 (mpairs (mapcar #'(lambda (fp) (cons (find (car fp) (formals adt)
						    :test #'same-id)
					      (car fp)))
		   fpairs))
	 (abds (mapcar #'bind-decl (arguments c)))
	 (*bound-variables* (cons avar *bound-variables*))
	 (vals (generate-every-rel-conjuncts
		abds pvars avar bvar ptypes fpairs mpairs adt)))
    (mk-conjunction (cons arec (cons brec vals)))))

(defun generate-every-rel-conjuncts (abds pvars avar bvar ptypes fpairs mpairs
					  adt &optional conjuncts)
  (if (null abds)
      (nreverse conjuncts)
      (let* ((atype (typecheck
			(subst-map-actuals
			 (subst-mod-params (declared-type (car abds))
					   (module-instance (type avar)))
			 mpairs)))
	     (btype (subst-map-actuals atype fpairs))
	     (aex (typecheck (mk-application (id (car abds)) avar)
		    :expected atype))
	     (bex (typecheck (mk-application (id (car abds)) bvar)
		     :expected btype))
	     (conjunct (adt-every-rel
			(type (car abds)) pvars aex bex ptypes fpairs adt))
	     (nabds (gensubst (cdr abds)
		      #'(lambda (ex)
			  (declare (ignore ex))
			  aex)
		      #'(lambda (ex)
			  (and (name-expr? ex)
			       (eq (declaration ex) (car abds)))))))
	(generate-every-rel-conjuncts nabds pvars avar bvar
				 ptypes fpairs mpairs adt
				 (cons conjunct conjuncts)))))

(defmethod adt-every-rel ((te type-name) pvars avar bvar ptypes fpairs adt)
  (cond ((member te ptypes :test #'corresponding-formals)
	 (let* ((pos (position te ptypes :test #'corresponding-formals))
		(pvar (nth pos pvars)))
	   (mk-application pvar (copy avar) (copy bvar))))
	((same-declaration te (adt-type-name adt))
	 (mk-application (mk-application* '|every| pvars)
	   (copy avar) (copy bvar)))
	((adt? te)
	 (let ((everys (adt-every-positive-actuals
			(positive-actuals te)
			(positive-actuals (find-supertype (type avar)))
			(positive-actuals (find-supertype (type bvar)))
			pvars ptypes fpairs adt)))
	   (if (every #'everywhere-true? everys)
	       *true*
	       (mk-application (mk-simple-every-application te adt everys)
		 (copy avar) (copy bvar)))))
	(t (mk-application (mk-name-expr '=) (copy avar) (copy bvar)))))

(defun adt-every-positive-actuals (acts aacts bacts pvars ptypes fpairs adt
					&optional rels)
  (if (null acts)
      (nreverse rels)
      (let* ((avid (make-new-variable '|u| adt))
	     (bvid (make-new-variable '|v| adt))
	     (atype (raise-actuals (type-value (car aacts))))
	     (btype (raise-actuals (type-value (car bacts))))
	     (abd (make-bind-decl avid atype))
	     (bbd (make-bind-decl bvid btype))
	     (avar (make-variable-expr abd))
	     (bvar (make-variable-expr bbd))
	     (rel (adt-every-rel (type-value (car acts))
				 pvars avar bvar ptypes fpairs adt))
	     (lexp (mk-lambda-expr (list abd bbd) rel)))
	(adt-every-positive-actuals (cdr acts) (cdr aacts) (cdr bacts)
				    pvars ptypes fpairs adt
				    (cons lexp rels)))))
  
(defmethod adt-every-rel ((te subtype) pvars avar bvar ptypes fpairs adt)
  (adt-every-rel
   (supertype te) pvars avar bvar ptypes fpairs adt))

(defmethod adt-every-rel ((te funtype) pvars avar bvar ptypes fpairs adt)
  (let* ((fid (make-new-variable '|z| (list te avar bvar)))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (every (adt-every-rel
		 (if (typep (domain te) 'dep-binding)
		     (substit (range te) (acons (domain te) fvar nil))
		     (range te))
		 pvars
		 (make!-application (copy avar) fvar)
		 (make!-application (copy bvar) fvar)
		 ptypes fpairs adt)))
    (if (everywhere-true? every)
	*true*
	(mk-forall-expr (list fbd) every))))

(defmethod adt-every-rel ((te recordtype) pvars avar bvar ptypes fpairs adt)
  (let ((rels (adt-every-rel-fields
		 (fields te) pvars avar bvar ptypes fpairs adt
		 (dependent? te))))
    (if (every #'everywhere-true? rels)
	*true*
	(mk-conjunction rels))))

(defun adt-every-rel-fields (fields pvars avar bvar ptypes fpairs adt
				    dep? &optional everys)
  (if (null fields)
      (nreverse everys)
      (let ((favar (make-field-application (car fields) avar))
	    (fbvar (make-field-application (car fields) bvar)))
	(adt-every-rel-fields
	 (if dep?
	     ;; Need a substit that creates two substitutions and conjoins
	     ;; them at the predicate level; e.g., in
	     ;;  [# a: int, b: {x: int | x < a} #]
	     ;; the substit on cdr fields) should create
	     ;; (b: {x: int | x < favar and x < fbvar})
	     (every-rel-field-subst (car fields) favar fbvar (cdr fields))
	     (cdr fields))
	 pvars avar bvar ptypes fpairs adt dep?
	 (cons (adt-every-rel
		(type (car fields)) pvars favar fbvar ptypes fpairs adt)
	       everys)))))

(defun every-rel-field-subst (field var1 var2 fields)
  (let* ((alist1 (acons field var1 nil))
	 (alist2 (acons field var2 nil))
	 (sfields
	  (gensubst fields
	    #'(lambda (ex)
		(let* ((id (make-new-variable 'x (supertype ex)))
		       (bd (make-bind-decl id (supertype ex)))
		       (var (make-variable-expr bd))
		       (appl (make!-application (predicate ex) var))
		       (pred1 (substit appl alist1))
		       (pred2 (substit appl alist2))
		       (cpred (make!-conjunction pred1 pred2)))
		  (copy ex
		    'predicate (make!-set-expr (list bd) cpred)
		    'print-type nil)))
	    #'(lambda (ex)
		(and (subtype? ex)
		     (member field (freevars (predicate ex))
			     :key #'declaration))))))
    (mapc #'(lambda (sfld fld)
	      (unless (eq (type sfld) (type fld))
		(setf (declared-type sfld) (type sfld))))
	  sfields fields)
    sfields))

(defmethod adt-every-rel ((te tupletype) pvars avar bvar ptypes fpairs adt)
  (let ((everys (adt-every-rel-types
		 (types te) pvars avar bvar ptypes fpairs adt 1)))
    (if (every #'everywhere-true? everys)
	*true*
	(mk-conjunction everys))))

(defun adt-every-rel-types (types pvars avar bvar ptypes fpairs adt
				  num &optional everys)
  (if (null types)
      (nreverse everys)
      (let ((pavar (make-projection-application num avar))
	    (pbvar (make-projection-application num bvar)))
	(adt-every-rel-types
	 (if (dep-binding? (car types))
	     ;; Need a substit that creates two substitutions and conjoins
	     ;; them at the predicate level; e.g., in
	     ;;  [a: int, {x: int | x < a} ]
	     ;; the substit on cdr fields) should create
	     ;; ({x: int | x < favar and x < fbvar})
	     (every-rel-types-subst (car types) pavar pbvar (cdr types))
	     (cdr types))
	 pvars avar bvar ptypes fpairs adt (incf num)
	 (cons (adt-every-rel
		(car types) pvars pavar pbvar ptypes fpairs adt)
	       everys)))))

(defmethod adt-every-rel ((te cotupletype) pvars avar bvar ptypes fpairs adt)
  (let ((num 0))
    (mk-disjunction
     (mapcar #'(lambda (type)
		 (incf num)
		 (let* ((inid (makesym "IN?_~d" num))
			(ain? (make-instance 'injection?-application
				'id inid
				'index num
				'argument avar))
			(bin? (make-instance 'injection?-application
				'id inid
				'index num
				'argument bvar))
			(outid (makesym "OUT_~d" num))
			(aout (typecheck (make-instance 'extraction-application
					   'id outid
					   'index num
					   'argument avar)))
			(bout (typecheck (make-instance 'extraction-application
					   'id outid
					   'index num
					   'argument bvar)))
			(every-sub (adt-every-rel type pvars aout bout ptypes fpairs adt)))
		   (mk-conjunction (list ain? bin? every-sub))))
       (types te)))))

(defun every-rel-types-subst (dep var1 var2 types)
  (let ((alist1 (acons dep var1 nil))
	(alist2 (acons dep var2 nil)))
    (gensubst types
      #'(lambda (ex)
	  (let* ((id (make-new-variable 'x (supertype ex)))
		 (bd (make-bind-decl id (supertype ex)))
		 (var (make-variable-expr bd))
		 (appl (make!-application (predicate ex) var))
		 (pred1 (substit appl alist1))
		 (pred2 (substit appl alist2))
		 (cpred (make!-conjunction pred1 pred2)))
	    (copy ex
	      'predicate (make!-set-expr (list bd) cpred)
	      'print-type nil)))
      #'(lambda (ex)
	  (and (subtype? ex)
	       (member dep (freevars (predicate ex))
		       :key #'declaration))))))

(defmethod adt-every-rel ((te type-expr) pvars avar bvar ptypes fpairs adt)
  (declare (ignore pvars ptypes fpairs adt))
  (mk-application '= (copy avar) (copy bvar)))


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
  (car (pc-parse (unparse formal :string t) 'theory-elt)))

(defmethod copy-adt-formals ((formal formal-type-decl))
  (car (pc-parse (unparse formal :string t) 'theory-formal)))

(defmethod copy-adt-formals ((formal formal-const-decl))
  (car (pc-parse (unparse formal :string t) 'theory-formal)))

(defun generate-adt-reduce-using (adt)
  (let* ((mod (mk-modname (makesym "~a_~a" (id adt)
				   (if (datatype? adt) "adt" "codt"))
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
  (let* ((*generate-tccs* 'none)
	 (rtype (typecheck* (mk-type-name ran) nil nil nil))
	 (atype (mk-type-name (id adt)))
	 (fname (case ran
		  (|nat| '|reduce_nat|)
		  (|ordinal| '|reduce_ordinal|)
		  (t '|reduce|)))
	 (fdoms (gen-adt-reduce-domains rtype adt adtinst))
	 (thinst (when adtinst (module-instance adtinst)))
	 (fran (mk-funtype (list atype) (mk-type-name ran)))
	 (cdecl (mk-adt-def-decl fname
		  (mk-funtype (list atype) rtype)
		  (pc-parse (unparse (gen-adt-reduce-definition
				      adt fname fdoms fran thinst)
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
		  (pc-parse (unparse (gen-adt-reduce-definition2
				      adt fname2 fdoms fran thinst)
			      :string t) 'expr)
		  (mapcar #'(lambda (c)
			      (mk-arg-bind-decl (makesym "~a_fun"
						     (op-to-id (recognizer c)))
				(gen-adt-reduce-funtype2 c rtype adt adtinst)))
			  (constructors adt)))))
    (typecheck-adt-decl cdecl t nil)
    (typecheck-adt-decl rdecl t nil)))

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

(defun gen-adt-reduce-definition (adt fname fdoms ran adtinst)
  (let* ((avar (mk-name-expr (makesym "~a_adtvar" (id adt))))
	 (funlist (mapcar #'(lambda (c)
			      (let ((rid (op-to-id (recognizer c))))
				(mk-name-expr (makesym "~a_fun" rid))))
		    (constructors adt)))
	 (redid (make-new-variable '|red| adt))
	 (redvar (mk-name-expr redid))
	 (red (mk-application* fname funlist)))
    (mk-lambda-expr (list (mk-bind-decl (id avar) (mk-type-name (id adt))))
      (make-instance 'let-expr
	'operator (mk-lambda-expr (list (mk-bind-decl redid ran))
		    (mk-cases-expr avar
		      (mapcar #'(lambda (c fdom)
				  (gen-adt-reduce-selection
				   c adt redvar fname fdom adtinst))
			(constructors adt) fdoms)
		      nil))
	'argument red))))

(defun gen-adt-reduce-selection (c adt red fname fdom adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name)
					       (type b))))
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
			 red fname
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

(defmethod acc-reduce-selection (arg (te type-name) red fname fdom adt)
  (cond ((tc-eq te adt)
	 (mk-application red (copy arg)))
	((adt? te)
	 (let* ((acts (actuals-corresponding-to-positive-types
		       (actuals (module-instance te))
		       (adt? te)))
		(facts (if (typep fdom 'datatype-subtype)
			   (actuals (declared-type fdom))
			   (actuals fdom)))
		(funs (acc-reduce-sel-acts acts facts fname red adt)))
	   (if (every #'identity-fun? funs)
	       (copy arg)
	       (let* ((appname (mk-name-expr '|map|
				 (append (actuals te) facts)))
		      (pname (pc-parse (unparse appname :string t) 'expr)))
		 (mk-application (mk-application* pname funs)
		   (copy arg))))))
	(t (copy arg))))

(defun acc-reduce-sel-acts (acts facts fname red adt &optional result)
  (if (null acts)
      (nreverse result)
      (if (type-value (car acts))
	  (let ((nfun (acc-reduce-selection*
		       (type-value (car acts))
		       red fname (type-value (car facts)) adt)))
	    (acc-reduce-sel-acts (cdr acts) (cdr facts) fname red adt
				 (cons nfun result)))
	  (acc-reduce-sel-acts (cdr acts) (cdr facts) fname red adt
			       result))))

(defmethod acc-reduce-selection (arg (te subtype) red fname fdom adt)
  (acc-reduce-selection arg (supertype te) red fname
			(if (typep fdom '(and subtype (not datatype-subtype)))
			    (supertype fdom)
			    fdom)
			adt))

(defmethod acc-reduce-selection (arg (te funtype) red fname fdom adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain fdom)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain fdom))))
	 (fun (acc-reduce-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       red fname
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

(defmethod acc-reduce-selection (arg (te recordtype) red fname fdom adt)
  (assert (typep fdom 'recordtype))
  (let ((funs (acc-reduce-selection-fields
	       (fields te) (fields fdom) arg red fname fdom adt
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

(defun acc-reduce-selection-fields (fields dfields rvar red fname fdom adt
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
       rvar red fname fdom adt dep1? dep2?
       (cons (acc-reduce-selection*
	      (type (car fields)) red fname (type (car dfields)) adt)
	     result))))

(defmethod acc-reduce-selection (arg (te tupletype) red fname fdom adt)
  (assert (typep fdom 'tupletype))
  (let ((funs (acc-reduce-selection-types
	       (types te) (types fdom) arg red fname adt)))
    (if (every #'identity-fun? funs)
	(copy arg)
	(mk-tuple-expr
	 (let ((num 0))
	   (mapcar #'(lambda (fun)
		       (incf num)
		       (let ((proj (make-instance 'projappl
				     'id (makesym "PROJ_~d" num)
				     'index num
				     'argument (copy arg))))
			 (if (identity-fun? fun)
			     proj
			     (mk-application fun proj))))
	     funs))))))

(defmethod acc-reduce-selection (arg (te cotupletype) red fname fdom adt)
  (assert (typep fdom 'cotupletype))
  (let ((funs (acc-reduce-selection-types
	       (types te) (types fdom) arg red fname adt)))
    (if (every #'identity-fun? funs)
	(copy arg)
	(mk-cases-expr arg
	  (let ((num 0)
		(varid (make-new-variable 'x adt)))
	    (mapcar #'(lambda (fun type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  'id (makesym "IN_~d" num)
					  'index num))
			       (sel-expr (make-instance 'injection-application
					   'id (makesym "IN_~d" num)
					   'index num
					   'argument (mk-application fun
						       var))))
			  (mk-selection in-expr (list bd) sel-expr)))
	      funs (types te)))
	  nil))))

(defmethod acc-reduce-selection (arg (te type-expr) red fname fdom adt)
  (declare (ignore red fname fdom adt))
  (copy arg))

(defmethod acc-reduce-selection* ((te type-name) red fname fte adt)
  (cond ((tc-eq te adt)
	 red)
	((adt? te)
	 (let* ((acts (actuals-corresponding-to-positive-types
		       (actuals (module-instance te))
		       (adt? te)))
		(funs (acc-reduce-sel-acts acts (actuals (find-supertype fte))
					   fname red adt)))
	   (if (every #'identity-fun? funs)
	       (mk-identity-fun te)
	       (mk-application* '|map| funs))))
	(t (mk-identity-fun fte))))

(defmethod acc-reduce-selection* ((te subtype) red fname fdom adt)
  (acc-reduce-selection* (find-supertype te) red fname
			 (find-declared-adt-supertype fdom) adt))

(defmethod acc-reduce-selection* ((te funtype) red fname fdom adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain fdom)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain fdom))))
	 (fun (acc-reduce-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       red fname
	       (if (typep (domain fdom) 'dep-binding)
		   (substit (range fdom) (acons (domain fdom) fvar nil))
		   (range fdom))
	       adt)))
    (cond ((identity-fun? fun)
	   (mk-identity-fun fdom))
	  ((sequence? te)
	   (mk-application '|map|
	     red))
	  (t
	   (let* ((aid (make-new-variable '|a| (list te fun adt)))
		  (abd (make-bind-decl aid te))
		  (avar (mk-name-expr aid nil nil
				      (make-resolution abd
					(current-theory-name) te))))
	     (mk-lambda-expr (list abd)
	       (mk-lambda-expr (list fbd)
		 (mk-application fun
		   (mk-application avar fvar)))))))))

(defmethod acc-reduce-selection* ((te recordtype) red fname fdom adt)
  (assert (typep fdom 'recordtype))
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (funs (acc-reduce-selection-fields
		(fields te) (fields fdom) rvar red fname fdom adt
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

(defmethod acc-reduce-selection* ((te tupletype) red fname fdom adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (funs (acc-reduce-selection-types
		(types te) (types fdom) tvar red fname adt)))
    (if (every #'identity-fun? funs)
	(mk-identity-fun te)
	(mk-lambda-expr (list tbd)
	  (mk-tuple-expr
	   (let ((num 0))
	     (mapcar #'(lambda (fun)
			 (incf num)
			 (let ((proj (make-instance 'projappl
				       'id (makesym "PROJ_~d" num)
				       'index num
				       'argument tvar)))
			   (if (identity-fun? fun)
			       proj
			       (mk-application fun proj))))
	       funs)))))))

(defmethod acc-reduce-selection* ((te cotupletype) red fname fdom adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (funs (acc-reduce-selection-types
		(types te) (types fdom) tvar red fname adt)))
    (if (every #'identity-fun? funs)
	(mk-identity-fun te)
	(mk-lambda-expr (list tbd)
	  (mk-cases-expr tvar
	    (let ((num 0)
		  (varid (make-new-variable 'x adt)))
	      (mapcar #'(lambda (fun type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    'id (makesym "IN_~d" num)
					    'index num))
				 (sel-expr (make-instance 'injection-application
					     'id (makesym "IN_~d" num)
					     'index num
					     'argument (mk-application fun
							 var))))
			    (mk-selection in-expr (list bd) sel-expr)))
		funs (types te)))
	    nil)))))

(defun acc-reduce-selection-types (types dtypes tvar red fname adt
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
       tvar red fname adt (1+ index)
       (cons (acc-reduce-selection* ty1 red fname ty2 adt) result)))))

(defmethod acc-reduce-selection* ((te type-expr) red fname fdom adt)
  (declare (ignore red fname fdom adt))
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
				      (current-theory-name) (type dep)))))
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
				       (current-theory-name) (type atype)))))
	      (gen-adt-reduce-domain (substit (cdr args)
				       (acons (car args) var nil))
				     rtype adt adtinst
				     (cons atype result)))))))

(defun gen-adt-reduce-dom (atype rtype adt adtinst)
  (let ((adt-type (or adtinst (adt-type-name adt))))
    (gen-adt-reduce-dom* atype rtype (cons adt-type (subtypes adt)))))

(defun gen-adt-reduce-dom* (atype rtype adt-types)
  (gensubst atype
    #'(lambda (ex) (gen-adt-reduce-dom! ex rtype adt-types))
    #'(lambda (ex) (gen-adt-reduce-dom? ex adt-types))))

(defmethod gen-adt-reduce-dom? ((ex type-name) adt-types)
  (member ex adt-types :test #'same-declaration))

(defmethod gen-adt-reduce-dom! ((ex type-name) rtype adt-types)
  (declare (ignore adt-types))
  (copy rtype))

(defmethod gen-adt-reduce-dom? ((ex expr-as-type) adt-types)
  (declare (ignore adt-types))
  nil)

(defmethod gen-adt-reduce-dom? ((ex subtype) adt-types)
  (or (subtype-of? ex (car adt-types))
      (unless (recognizer-name-expr? (predicate ex))
	(let ((stype (find-supertype ex)))
	  (and (adt-type-name? stype)
	       (occurs-in (car adt-types) stype))))))

(defmethod gen-adt-reduce-dom! ((ex subtype) rtype adt-types)
  (if (subtype-of? ex (car adt-types))
      (copy rtype)
      (gen-adt-reduce-dom* (find-supertype ex) rtype adt-types)))

(defmethod gen-adt-reduce-dom? ((ex datatype-subtype) adt-types)
  (occurs-in (car adt-types) ex))

(defmethod gen-adt-reduce-dom! ((ex datatype-subtype) rtype adt-types)
  (gen-adt-reduce-dom* (supertype ex) rtype adt-types))

(defmethod gen-adt-reduce-dom? (ex adt-types)
  (declare (ignore ex adt-types))
  nil)

(defun gen-adt-reduce-definition2 (adt fname fdoms ran adtinst)
  (let* ((avar (mk-name-expr (makesym "~a_adtvar" (id adt))))
	 (funlist (mapcar #'(lambda (c)
			      (let ((rid (op-to-id (recognizer c))))
				(mk-name-expr (makesym "~a_fun" rid))))
		    (constructors adt)))
	 (redid (make-new-variable '|red| adt))
	 (redvar (mk-name-expr redid))
	 (red (mk-application* fname funlist)))
    (mk-lambda-expr (list (mk-bind-decl (id avar)
			      (mk-type-name (id adt))))
      (make-instance 'let-expr
	'operator (mk-lambda-expr (list (mk-bind-decl redid ran))
		    (mk-cases-expr avar
		      (mapcar #'(lambda (c fdom)
				  (gen-adt-reduce-selection2
				   c adt redvar fname avar fdom adtinst))
			(constructors adt) fdoms)
		      nil))
	'argument red))))

(defun gen-adt-reduce-selection2 (c adt red fname avar fdom adtinst)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var a))
				     (type a)))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name)
					       (type b))))
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
				red fname
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

(defmethod acc-reduce-selection2 (arg (te type-name) red fname adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (mk-application red (copy arg)))
;	((member te ptypes :test #'corresponding-formals)
;	 (let* ((pos (position te ptypes :test #'corresponding-formals))
;		(fun (nth pos funlist)))
;	   (mk-application fun (copy arg))))
	((datatype-adt? (adt-type-name adt) te)
	 (let ((acts (actuals (module-instance (find-supertype te)))))
	   (mk-application
	       (mk-application* '|map|
		 (acc-reduce-sel-acts2 (adt-type-name adt) acts fname red))
	     (copy arg))))
	(t (copy arg))))

(defun acc-reduce-sel-acts2 (adt-type acts fname red &optional result)
  (if (null acts)
      (nreverse result)
      (if (type-value (car acts))
	  (let ((nfun (if (tc-eq (type-value (car acts)) adt-type)
			  red
			  (mk-name-expr '|id|
			    (list (mk-actual (type-value (car acts))))))))
	    (acc-reduce-sel-acts2 adt-type (cdr acts) fname red
				 (cons nfun result)))
	  (acc-reduce-sel-acts2 adt-type (cdr acts) fname red result))))

(defmethod acc-reduce-selection2 (arg (te subtype) red fname adt)
  (acc-reduce-selection2 arg (supertype te) red fname adt))

(defmethod acc-reduce-selection2 (arg (te funtype) red fname adt)
  (declare (ignore fname))
  (if (sequence-adt? (adt-type-name adt) te)
      (mk-application
	  (mk-application '|map| red)
	arg)
      (copy arg)))

(defmethod acc-reduce-selection2 (arg (te type-expr) red fname adt)
  (declare (ignore red fname adt))
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
				       (current-theory-name) (type atype)))))
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
	 (wf-type (typecheck* (mk-expr-as-type
			       (mk-name-expr '|well_founded?|
				 (list (mk-actual (adt-type-name *adt*)))))
			      nil nil nil))
	 (<<-decl (mk-adt-def-decl '<<
		    (supertype wf-type)
		    (mk-lambda-expr
			(list (mk-bind-decl xid (adt-type-name adt))
			      (mk-bind-decl yid (adt-type-name adt)))
		      (gen-adt-<<-definition
		       adt (mk-name-expr xid) (mk-name-expr yid)))
		    nil
		    (print-type (supertype wf-type))))
	 (<<-wf-decl (mk-formula-decl (makesym "~a_well_founded" (id adt))
		       (mk-application
			   (mk-name-expr '|well_founded?|
			     (list (mk-actual (adt-type-name *adt*))))
			 (mk-name-expr '<<))
		       'AXIOM)))
    (typecheck-adt-decl subterm-decl)
    (typecheck-adt-decl <<-decl)
    (setf (type <<-decl) wf-type
	  (declared-type <<-decl) (print-type wf-type))
    (typecheck-adt-decl <<-wf-decl t nil)))

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
      (let ((subst (adt-subst-alist (arguments c))))
	(mk-selection (mk-name-expr (id c))
	  (mapcar #'(lambda (b) (declaration (cdr b))) subst)
	  (mk-disjunction
	   (mapcar #'(lambda (a)
		       (acc-subterm-selection (make-instance 'name-expr
						'id (id (get-adt-var a)))
					      (substit (type a) subst)
					      xvar adt))
	     (arguments c)))))
      (mk-selection (mk-name-expr (id c)) nil *false*)))

(defun acc-subterm-selection (arg type xvar adt)
  (let ((fun (acc-subterm-selection* type xvar adt)))
    (if (everywhere-false? fun)
	(copy *false*)
	(mk-application fun arg))))

(defmethod acc-subterm-selection* ((te type-name) xvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (let* ((bd (mk-bind-decl (make-new-variable '|z| te) te))
		(le (mk-lambda-expr (list bd)
		      (mk-application '|subterm| (copy xvar)
				      (mk-name-expr (id bd))))))
	   (setf (parens le) 1)
	   le))
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
			       (current-theory-name) (domain te))))
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
				     (current-theory-name) te))))
	  (mk-lambda-expr (list fbd)
	    (mk-exists-expr (list zbd)
	      (mk-application sub
		(mk-application fvar zvar))))))))

(defmethod acc-subterm-selection* ((te recordtype) xvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
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
			       (current-theory-name) te)))
	 (subs (acc-subterm-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(let ((le (mk-lambda-expr (list tbd)
		    (let ((num 0))
		      (mk-disjunction
		       (mapcan #'(lambda (sub)
				   (incf num)
				   (unless (everywhere-false? sub)
				     (list (mk-application sub
					     (make-instance 'projappl
					       'id (makesym "PROJ_~d" num)
					       'index num
					       'argument tvar)))))
			 subs))))))
	  (setf (parens le) 1)
	  le))))

(defmethod acc-subterm-selection* ((te cotupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (subs (acc-subterm-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable 'w adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (sub type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    'id (makesym "IN_~d" num)
					    'index num))
				 (sel-expr (mk-application sub var)))
			    (mk-selection in-expr (list bd) sel-expr)))
		subs (types te))
	      nil))))))

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
      (let ((subst (adt-subst-alist (arguments c))))
	(mk-selection (mk-name-expr (id c))
	  (mapcar #'(lambda (b) (declaration (cdr b))) subst)
	  (mk-disjunction
	   (mapcar #'(lambda (a)
		       (acc-<<-selection (make-instance 'name-expr
					   'id (id (get-adt-var a)))
					 (substit (type a) subst)
					 xvar adt))
	     (arguments c)))))
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
  (let* ((zid (make-new-variable '|z| (list te xvar)))
	 (zbd (make-bind-decl zid (domain te)))
	 (zvar (mk-name-expr zid nil nil
			     (make-resolution zbd
			       (current-theory-name) (domain te))))
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
				     (current-theory-name) te))))
	  (mk-lambda-expr (list fbd)
	    (mk-exists-expr (list zbd)
	      (mk-application sub
		(mk-application fvar zvar))))))))

(defmethod acc-<<-selection* ((te recordtype) xvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (subs (acc-<<-fields (fields te) xvar rvar adt (dependent? te))))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-disjunction
	   (mapcan #'(lambda (fd sub)
		       (unless (everywhere-false? sub)
			 (list (mk-application sub
				 (mk-application (id fd) rvar)))))
	     (fields te) subs))))))

(defun acc-<<-fields (fields xvar rvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-<<-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       xvar rvar adt dep?
       (cons (acc-<<-selection* (type (car fields)) xvar adt)
	     result))))

(defmethod acc-<<-selection* ((te tupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (subs (acc-<<-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(let ((le (mk-lambda-expr (list tbd)
		    (let ((num 0))
		      (mk-disjunction
		       (mapcan #'(lambda (sub)
				   (incf num)
				   (unless (everywhere-false? sub)
				     (list (mk-application sub
					     (make-instance 'projappl
					       'id (makesym "PROJ_~d" num)
					       'index num
					       'argument tvar)))))
			 subs))))))
	  (setf (parens le) 1)
	  le))))

(defmethod acc-<<-selection* ((te cotupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (subs (acc-<<-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable 'w adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (sub type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    'id (makesym "IN_~d" num)
					    'index num))
				 (sel-expr (mk-application sub var)))
			    (mk-selection in-expr (list bd) sel-expr)))
		subs (types te))
	      nil))))))

(defun acc-<<-types (types xvar tvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-<<-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       xvar tvar adt (1+ index)
       (cons (acc-<<-selection* (car types) xvar adt) result))))

(defmethod acc-<<-selection* ((te dep-binding) xvar adt)
  (acc-<<-selection* (type te) xvar adt))

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
  (when (typep *adt* 'inline-recursive-type)
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

(defmethod occurs-positively?* (type (adt recursive-type) none)
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
	(if (typep adt 'inline-recursive-type)
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
  (occurs-positively?* type (supertype te) none))

(defmethod occurs-positively?* (type (te subtype) none)
  (and (occurs-positively?* type (supertype te) none)
       (occurs-positively?* type (predicate te) none)))

(defmethod occurs-positively?* (type (te funtype) none)
  (and (occurs-positively?* type (range te) (or *simple-pos* none))
       (occurs-positively?* type (domain te) t)))

(defmethod occurs-positively?* (type (te tupletype) none)
  (occurs-positively?* type (types te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te cotupletype) none)
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

(defmethod occurs-positively?* (type (ex projection-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex injection-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex injection?-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex extraction-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex projection-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex injection-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex injection?-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex extraction-application) none)
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
      (let* ((acts (raise-actuals (actuals (module-instance te))))
	     (macts (raise-actuals (subst-map-actuals acts fpairs)))
	     (name (mk-name-expr '|map| (append acts macts)))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname maps))))

(defmethod mk-map-application ((te funtype) fpairs adt maps)
  (declare (ignore fpairs adt))
  (mk-application* '|map| maps))

(defmethod mk-map-application ((te subtype) fpairs adt maps)
  (mk-map-application (supertype te) fpairs adt maps))

(defun map-application? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'name-expr)
       (eq (id (operator expr)) '|map|)))

(defun every-application? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'name-expr)
       (eq (id (operator expr)) '|every|)))

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

(defmethod mk-every-application ((te type-name) fpairs adt everys)
  (if (eq (adt te) adt)
      (mk-application* '|every| everys)
      (let* ((acts (actuals (module-instance te)))
	     (macts (subst-map-actuals acts fpairs))
	     (name (mk-name-expr '|every| (append acts macts)))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname everys))))

(defmethod mk-simple-every-application ((te type-name) adt everys)
  (if (eq (adt te) adt)
      (mk-application* '|every| everys)
      (let* ((name (mk-name-expr '|every|))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname everys))))

(defmethod mk-every-application ((te funtype) fpairs adt everys)
  (declare (ignore fpairs adt))
  (mk-application* '|every| everys))

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

(defmethod subtypes ((adt recursive-type))
  nil)


(defun generate-codt-coreduce-theory (codt)
  (build-adt-theory (makesym "~a_codt_coreduce" (id codt)) codt
    (let ((dom (make-new-variable '|domain| codt)))
      (generate-adt-reduce-formals codt dom)
      (let* ((codtinst (generate-adt-reduce-using codt))
	     (struct (generate-codt-structure-datatype codt dom)))
	(typecheck-adt-decl struct)
	(generate-codt-coreduce codt dom codtinst struct)))))

(defmethod generate-codt-structure-datatype ((codt codatatype-with-subtypes) dom)
  (make-instance 'inline-datatype-with-subtypes
    'id (makesym "~a_struct" (id codt))
    'constructors (generate-codt-structure-constructors codt dom)
    'subtypes (generate-codt-structure-subtypes codt)))

(defmethod generate-codt-structure-datatype ((codt codatatype) dom)
  (make-instance 'inline-datatype
    'id (makesym "~a_struct" (id codt))
    'constructors (generate-codt-structure-constructors codt dom)))

(defun generate-codt-structure-constructors (codt dom)
  (generate-codt-structure-constructors* (constructors codt) codt dom))

(defun generate-codt-structure-constructors* (constructors codt dom
							   &optional result)
  (if (null constructors)
      (nreverse result)
      (generate-codt-structure-constructors*
       (cdr constructors)
       codt
       dom
       (cons (generate-codt-structure-constructor (car constructors) codt dom)
	     result))))

(defmethod generate-codt-structure-constructor (constructor codt dom)
  (make-instance 'simple-constructor
    'id (makesym "inj_~a" (id constructor))
    'arguments (mapcar #'(lambda (arg)
			   (generate-codt-structure-argument arg codt dom))
		 (arguments constructor))
    'recognizer (makesym "inj_~a" (recognizer constructor))))

(defmethod generate-codt-structure-constructor
    ((constructor constructor-with-subtype) codt dom)
  (make-instance 'constructor-with-subtype
    'id (makesym "inj_~a" (id constructor))
    'arguments (mapcar #'(lambda (arg)
			   (generate-codt-structure-argument arg codt dom))
		 (arguments constructor))
    'recognizer (makesym "inj_~a" (recognizer constructor))
    'subtype (make-instance 'type-name
	       'id (makesym "inj_~a" (id (subtype constructor))))))

(defun generate-codt-structure-argument (arg codt dom)
  (make-instance 'adtdecl
    'id (makesym "inj_~a" (id arg))
    'declared-type (generate-codt-structure-accessor-type
		    (pc-parse (unparse (declared-type arg) :string t)
		      'type-expr)
		    (typecheck (mk-type-name (id (adt-type-name codt))))
		    codt dom)))

(defun generate-codt-structure-accessor-type (type codtype codt dom)
  (gensubst type
    #'(lambda (te) (if (subtype-of? (typecheck te) codtype)
		       (typecheck (mk-type-name dom))
		       (generate-codt-structure-accessor-type
			(find-supertype (typecheck te)) codtype codt dom)))
    #'(lambda (te) (and (type-expr? te)
			(not (expr-as-type? te))
			(or (subtype-of? (typecheck te) codtype)
			    (and (subtype? te)
				 (adt-type-name? (find-supertype (typecheck te)))))))))

(defun generate-codt-structure-subtypes (codt)
  (mapcar #'(lambda (st)
	      (make-instance 'type-name
		'id (makesym "inj_~a" (id st))))
    (subtypes codt)))


(defun generate-codt-coreduce (codt dom codtinst struct)
  (let* ((dtype (typecheck* (mk-type-name dom) nil nil nil))
	 (coreduce-type (generate-coreduce-function-type dtype codt struct))
	 (opid (make-new-variable '|op| codt))
	 (opbd (make-bind-decl opid coreduce-type))
	 (opvar (make-variable-expr opbd))
	 (xvid (make-new-variable '|x| codt))
	 (xbd (make-bind-decl xvid dtype))
	 (xvar (make-variable-expr xbd))
	 (coreduce-op (mk-application '|coreduce| opvar))
	 (op-xvar (mk-application opvar xvar))
	 (dvid (make-new-variable '|c| codt))
	 (dbd (make-bind-decl dvid codtinst))
	 (dvar (make-variable-expr dbd))
	 (coreduce-range
	  (mk-subtype codtinst
	    (mk-lambda-expr (list dbd)
	      (mk-disjunction
	       (mapcar #'(lambda (c1 c2)
			   (mk-conjunction
			    (list (mk-application (recognizer c1)
				    op-xvar)
				  (mk-application (recognizer c2)
				    dvar))))
		 (constructors struct) (constructors codt))))))
	 (*adt-vars* (make-hash-table :test #'eq)))
    (generate-adt-vars struct)
    (let* ((precases (mapcar #'(lambda (c)
				 (generate-adt-map-selection
				  c (list coreduce-op) (list dtype)
				  (acons dtype codtinst nil) struct
				  (adt-type-name struct) t))
		       (constructors struct)))
	   (cases (mk-cases-expr op-xvar
		    (mapcar #'(lambda (sel c)
				(copy sel
				  'expression (if (application?
						   (expression sel))
						  (mk-application (id c)
						    (argument
						     (expression sel)))
						  (mk-name-expr (id c)))))
		      precases (constructors codt))
		    nil))
	   (decl (mk-adt-def-decl
		     '|coreduce| coreduce-range
		     (pc-parse (unparse cases :string t) 'expr)
		     (list (list opbd) (list xbd)))))
      (typecheck-adt-decl decl))))

(defun generate-coreduce-function-type (dtype codt struct)
  (let* ((ftype (mk-funtype (list dtype) (adt-type-name struct)))
	 (opname (make-new-variable '|op| codt))
	 (opbd (make-bind-decl opname ftype))
	 (opvar (make-variable-expr opbd))
	 (dname (make-new-variable '|x| codt))
	 (dbd (make-bind-decl dname dtype))
	 (type-alist (append (mapcar #'(lambda (x y)
					 (cons (typecheck x) (typecheck y)))
			       (subtypes codt) (subtypes struct))
			     (mapcar #'(lambda (x y)
					 (cons (typecheck
						   (mk-name-expr
						       (recognizer x)))
					       (typecheck
						   (mk-name-expr
						       (recognizer y)))))
			       (constructors codt) (constructors struct))))
	 (cases (generate-coreduce-funtype-cases opvar codt struct type-alist)))
    (if (every #'null cases)
	ftype
	(mk-subtype ftype
	  (mk-lambda-expr (list opbd)
	    (make-forall-expr (list dbd)
	      (typecheck
		  (mk-cases-expr (make-application opvar
				   (make-variable-expr dbd))
		    (remove-if #'null cases)
		    (when (some #'null cases)
		      *true*))
		:expected *boolean*)))))))

(defun generate-coreduce-funtype-cases (op codt struct type-alist)
  (mapcar #'(lambda (c cs)
	      (generate-coreduce-funtype-case c cs op codt struct type-alist))
    (constructors codt) (constructors struct)))

(defun generate-coreduce-funtype-case (c cs op codt struct type-alist)
  (when (arguments c)
    (let* ((bindings (mapcar #'(lambda (a as)
				   (make-bind-decl (id (get-adt-var a))
				     (type as)))
			 (arguments c) (arguments cs)))
	   (vars (mapcar #'(lambda (b)
			     (mk-name-expr (id b) nil nil
					   (make-resolution b
					     (current-theory-name)
					     (type b))))
		   bindings))
	   (value (generate-coreduce-funtype-selection-value
		    (arguments c) (arguments cs) vars op codt struct
		    type-alist)))
      (when value
	(mk-selection (mk-name-expr (id cs))
	  bindings
	  value)))))

(defun generate-coreduce-funtype-selection-value (args sargs vars op codt
						       struct type-alist
						       &optional result)
  (if (null args)
      result
      (let ((val (generate-coreduce-funtype-selection-value1
		  (car args) (car sargs) (car vars) op codt struct
		  type-alist)))
	(generate-coreduce-funtype-selection-value
	 (cdr args) (cdr sargs) (cdr vars) op codt struct type-alist
	 (if val
	     (if result
		 (make-conjunction (list result val))
		 val)
	     result)))))

(defun generate-coreduce-funtype-selection-value1 (arg sarg var op codt struct
						       type-alist)
  (generate-coreduce-funtype-selection-value*
   (type arg) (type sarg) var op codt struct type-alist))

(defmethod generate-coreduce-funtype-selection-value* ((te type-name) ste var op codt struct type-alist)
  (declare (ignore ste var op codt struct type-alist))
  nil)

(defmethod generate-coreduce-funtype-selection-value* ((te subtype) (ste type-name) var op codt struct type-alist)
  (cond ((subtype-of? te (adt-type-name codt))
	 (let ((struct-subtype (cdr (assoc te type-alist :test #'tc-eq))))
	   (assert struct-subtype)
	   (make-application (predicate struct-subtype)
	     (make-application op var))))
	(t (assert (adt ste))
	   (let* ((formals (formals-sans-usings (adt ste)))
		  (ptypes (positive-types (adt ste)))
		  (supte (find-supertype te))
		  (acts (actuals (module-instance
				  (resolution (or (print-type supte) supte)))))
		  (sacts (actuals (module-instance
				   (resolution (or (print-type ste) ste)))))
		  (vals (mapcar
			    #'(lambda (a sa fm)
				(when (member fm ptypes
					      :test #'same-id
					      :key #'(lambda (x)
						       (or (print-type x) x)))
				  (let* ((did (make-new-variable '|x| codt))
					 (dbd (make-bind-decl did (type-value sa)))
					 (dvar (make-variable-expr dbd))
					 (dval
					  (generate-coreduce-funtype-selection-value*
					   (type-value a) (type-value sa)
					   dvar op codt struct type-alist)))
				    (mk-lambda-expr (list dbd)
				      (or dval *true*)))))
			  acts sacts formals)))
	     (unless (every #'everywhere-true? vals)
	       (mk-application (mk-application* (mk-name-expr '|every|)
				 vals)
		 var))))))
					 
	     

(defmethod generate-coreduce-funtype-selection-value* ((te subtype) (ste subtype) var op codt struct type-alist)
  (declare (ignore op struct type-alist))
  (assert (not (subtype-of? te (adt-type-name codt))))
  nil)

(defmethod generate-coreduce-funtype-selection-value* ((te funtype) (ste funtype) var op codt struct type-alist)
  ;;(assert (tc-eq (domain te) (domain ste)))
  (let* ((dvarid (make-new-variable '|d| codt))
	 (dbd (make-bind-decl dvarid (if (dep-binding? (domain te))
					 (type (domain te))
					 (domain te))))
	 (dvar (make-variable-expr dbd))
	 (value (generate-coreduce-funtype-selection-value* (range te) (range ste) (make-application var dvar) op codt struct type-alist)))
    (when value
      (mk-forall-expr (list dbd)
	(if (dep-binding? (domain ste))
	    (substit value (acons (domain ste) dbd nil))
	    value)))))

(defmethod generate-coreduce-funtype-selection-value* ((te tupletype) (ste tupletype) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-tup-value*
   (types te) (types ste) var op codt struct type-alist))

(defmethod generate-coreduce-funtype-selection-value* ((te cotupletype) (ste cotupletype) var op codt struct type-alist)
  (break "generate-coreduce-funtype-selection-value* (cotuple)")
  (generate-coreduce-funtype-selection-tup-value*
   (types te) (types ste) var op codt struct type-alist))

(defun generate-coreduce-funtype-selection-tup-value* (types stypes var op codt struct type-alist &optional (num 1) result)
  (if (null types)
      result
      (let* ((proj (make!-projection-application num var))
	     (aval (generate-coreduce-funtype-selection-value*
		    (car types) (car stypes) proj op codt struct type-alist)))
	(generate-coreduce-funtype-selection-tup-value*
	 (cdr types)
	 (if (dep-binding? (car stypes))
	     (substit (cdr stypes)
	       (acons (car stypes) proj nil))
	     (cdr stypes))
	 var op codt struct type-alist (1+ num)
	 (if aval
	     (if result
		 (make-conjunction (list result aval))
		 aval)
	     result)))))

(defmethod generate-coreduce-funtype-selection-value* ((te dep-binding) (ste dep-binding) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-value* (type te) (type ste) var op codt struct type-alist))

(defmethod generate-coreduce-funtype-selection-value* ((te recordtype) (ste recordtype) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-rec-value*
   (fields te) (fields ste) var op codt struct type-alist))

(defun generate-coreduce-funtype-selection-rec-value* (fields sfields var op codt struct type-alist &optional result)
  (if (null fields)
      result
      (let* ((avar (make!-field-application (car sfields) var))
	     (aval (generate-coreduce-funtype-selection-value*
		   (type (car fields)) (type (car sfields)) avar op codt struct type-alist)))
	(generate-coreduce-funtype-selection-rec-value*
	 (cdr fields)
	 (substit (cdr sfields)
	   (acons (car fields) avar nil))
	 var op codt struct type-alist
	 (if aval
	     (if result
		 (make-conjunction (list result aval))
		 aval)
	     result)))))

(defvar *adt-subtypes* nil)

(defun find-adt-subtypes (adt)
  (let ((adt-type (adt-type-name adt))
	(*adt-subtypes* nil))
    (find-adt-subtypes* (constructors adt) adt-type)
    *adt-subtypes*))

(defmethod find-adt-subtypes* ((list list) adt-type)
  (dolist (x list)
    (find-adt-subtypes* x adt-type)))

(defmethod find-adt-subtypes* ((c adt-constructor) adt-type)
  (find-adt-subtypes* (arguments c) adt-type))

(defmethod find-adt-subtypes* ((a adtdecl) adt-type)
  (find-adt-subtypes* (type a) adt-type))

(defmethod find-adt-subtypes* (type adt-type)
  (mapobject #'(lambda (ex)
		 (cond ((and (subtype? ex)
			     (not (expr-as-type? ex))
			     (tc-eq adt-type (find-supertype ex)))
			(pushnew ex *adt-subtypes* :test #'tc-eq)
			t)
		       ((datatype-subtype? ex)
			(find-adt-subtypes* (declared-type ex) adt-type)
			nil)))
	     type))
