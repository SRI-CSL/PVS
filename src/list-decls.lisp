;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-decls.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Dec 19 16:12:09 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Oct 29 22:43:06 1998
;; Update Count    : 3
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ---------------------------------------------
;;; Support for the show-declaration, find-declaration,
;;; whereis-declaration-used, and list-declarations commands.  Note that
;;; all of these return a file name rather than the actual list; this is
;;; because the list could get large.


(in-package 'pvs)

(defvar *list-declarations* nil)

;;; Called by Emacs - show-expanded-form command

(defun show-expanded-form (oname origin pos1 &optional (pos2 pos1) all?)
  (if (or (equal origin "Declaration")
	  (typechecked-origin? oname origin))
      (multiple-value-bind (object *current-theory*)
	  (get-object-at oname origin pos1 pos2)
	(let ((*disable-gc-printout* t)
	      (*current-context* (context *current-theory*)))
	  (pvs-buffer "Expanded Form"
	    (unparse (full-name object nil (not all?)) :string t)
	    t))
	(place-list (place object)))))

;;; Called by Emacs - show-declaration command

(defvar *containing-type* nil)

(defun show-declaration (oname origin pos &optional x?)
  (if (or (equal origin "Declaration")
	  (typechecked-origin? oname origin))
      (multiple-value-bind (object *containing-type*)
	  (get-id-object-at oname origin pos)
	(let ((decl (get-decl-associated-with object)))
	  (if decl
	      (let* ((declstr (format nil "~a~@[ ~a~]"
				(unparse-decl decl)
				(when *containing-type*
				  (let ((kind (typecase decl
						(field-decl "field")
						(dep-binding "declaration"))))
				    (when kind
				      (format nil "(~a in ~a)" kind
					      *containing-type*)))))))
		(if x?
		    (pvs-wish-source
		     (with-output-to-temp-file
		      (format t "show-declaration ~a ~a ~a {~a}~%"
			(id object)
			(min (length declstr) *default-char-width*)
			(count #\Newline declstr)
			declstr)))
		    (pvs-buffer "Declaration"
		      declstr
		      'temp t)))
	      ;;(pvs-message "Could not find associated declaration")
	      )))
      (pvs-message "~a is not typechecked" oname)))


;;; Called by Emacs - goto-declaration command

(defun goto-declaration (oname origin pos)
  (if (typechecked-origin? oname origin)
      (let* ((object (get-id-object-at oname origin pos))
	     (decl (get-decl-associated-with object)))
	(when decl
	  (pvs-locate (module decl) decl)))
      (pvs-message "~a has not been typechecked" oname)))

(defun typechecked-origin? (name origin)
  (case (intern (string-upcase origin))
    ((ppe tccs) (get-theory name))
    ((prelude prelude-theory) t)
    (t (typechecked-file? name))))

(defmethod get-decl-associated-with ((obj datatype-or-module))
  (pvs-message "The cursor is at the declaration for this identifier")
  nil)

(defmethod get-decl-associated-with ((obj modname))
  (pvs-message "~a is a theory; to see it use C-c C-f ~a"
    (id obj) (filename (get-theory obj)))
  nil)

(defmethod get-decl-associated-with ((obj name))
  (cond ((resolution obj)
	 (declaration (resolution obj)))
	(t (pvs-message "~a has no resolution" (id obj))
	   nil)))

(defmethod get-decl-associated-with ((obj expname))
  (pvs-message "Show-declaration for EXPORTING names is not supported")
  nil)

(defmethod get-decl-associated-with ((obj declaration))
  (pvs-message "The cursor is at the declaration for this identifier")
  nil)

(defmethod get-decl-associated-with ((obj simple-decl))
  (pvs-message "The cursor is at the declaration for this identifier")
  nil)

(defmethod get-decl-associated-with ((obj simple-constructor))
  (pvs-message "The cursor is at the declaration for this identifier")
  nil)

(defmethod get-decl-associated-with ((obj field-application))
  (setq *containing-type* (find-supertype (type (argument obj))))
  (find (id obj) (fields *containing-type*) :key #'id))

(defmethod get-decl-associated-with ((obj field-assignment-arg))
  (if (and *containing-type*
	   (recordtype? *containing-type*))
      (find (id obj) (fields *containing-type*) :key #'id)
      (progn (pvs-message "Can't find associated record type")
	     nil)))

(defmethod get-decl-associated-with ((obj projection-expr))
  (pvs-message "Projections do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj injection-expr))
  (pvs-message "Injections do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj injection?-expr))
  (pvs-message "Injection recognizers do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj extraction-expr))
  (pvs-message "Extractions do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj projection-application))
  (pvs-message "Projections do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj injection-application))
  (pvs-message "Injections do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj injection?-application))
  (pvs-message "Injection recognizers do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj extraction-application))
  (pvs-message "Extractions do not have an associated declaration")
  nil)

(defmethod get-decl-associated-with ((obj null))
  (pvs-message "Not at a valid id")
  nil)

(defun get-object-at (oname origin pos1 pos2)
  (multiple-value-bind (objects theories)
      (get-syntactic-objects-for oname origin)
    (let ((theory (find-element-containing-pos theories pos1)))
      (if (or (equal pos1 pos2)
	      (within-place pos2 (place theory)))
	  (values (get-object-in-theory-at objects theory pos1 pos2) theory)
	  (pvs-message "Region may not cross theory boundaries")))))

(defun get-object-in-theory-at (objects theory pos1 pos2)
  (let* ((decls (all-decls theory))
	 (decl (find-element-containing-pos decls pos1)))
      (if (or (equal pos1 pos2)
	      (within-place pos2 (place decl)))
	  (get-object-in-declaration-at objects pos1 pos2)
	  (let ((decl2 (find-element-containing-pos decls pos2)))
	    (ldiff (memq decl decls) (cdr (memq decl2 decls)))))))

(defun find-element-containing-pos (list pos)
  (when list
    (if (within-place pos (place (car list)))
	(car list)
	(find-element-containing-pos (cdr list) pos))))

(defun get-object-in-declaration-at (decl pos1 pos2)
  (let ((object nil)
	(*parsing-or-unparsing* t))
    (mapobject #'(lambda (ex)
		   (or (and (syntax? ex)
			    ;;(or (place ex) (break "Place not set"))
			    (place ex)
			    (within-place pos1 (place ex))
			    (or (equal pos1 pos2)
				(within-place pos2 (place ex)))
			    (unless (and (infix-application? object)
					 (arg-tuple-expr? ex))
			      (setq object ex))
			    nil)
		       (when (and (place ex)
				  (typep ex '(and syntax
						  (not assignment)
						  (or funtype
						      tupletype
						      recordtype)))
				  (within-place pos1 (place ex))
				  (or (equal pos1 pos2)
				      (within-place pos2 (place ex))))
			 nil)))
	       decl)
    object))

(defun get-id-object-at (oname origin pos)
  (let ((objects (get-syntactic-objects-for oname origin))
	(containing-type nil)
	(object nil)
	(*parsing-or-unparsing* t))
    (mapobject #'(lambda (ex)
		   (or object
		       (and (syntax? ex)
			    ;;(or (place ex) (break "Place not set"))
			    (place ex)
			    (slot-exists-p ex 'id)
			    (within-place pos (id-place ex))
			    (setq object ex)
			    t)
		       (when (and (typep ex '(and syntax
						  (not assignment)
						  (or funtype
						      tupletype
						      recordtype)))
				  (within-place pos (place ex)))
			 (typecase ex
			   (expr (setq containing-type
				       (find-supertype (type ex))))
			   ((or funtype tupletype recordtype)
			    (setq containing-type ex)))
			 nil)
		       (when object t)))
	       objects)
    (values object containing-type)))

(defun get-syntactic-objects-for (name origin)
  (case (intern (string-upcase origin))
    (ppe (let ((theory (get-theory name)))
	   (when theory
	     (values (ppe-form theory) (list theory)))))
    (tccs (let ((theory (get-theory name)))
	    (when theory
	      (values (tcc-form theory) (list theory)))))
    (prelude (let ((theories (remove-if #'generated-by *prelude-theories*)))
	       (values theories theories)))
    (prelude-theory (let ((theory (get-theory name)))
		      (when theory
			(values theory (list theory)))))
    (t (let ((theories (typecheck-file name nil nil nil t)))
	 (values theories theories)))))


;;; Called by Emacs - find-declaration command

(defun find-declaration (string)
  (let ((declarations nil)
	(id (intern string)))
    (do-all-theories #'(lambda (th)
			 (setq declarations
			       (append (get-find-declaration-info id th)
				       declarations))))
    (write-declaration-info declarations)))


;;; Called by Emacs - list-declarations command

(defun list-declarations (theoryref)
  (let ((theory (get-typechecked-theory theoryref))
	(*list-declarations* nil)
	(*modules-visited* nil))
    (list-declarations* theory)
    (setq *list-declarations*
	  (sort *list-declarations* #'string<
		:key #'(lambda (x)
			 (if (importing? (car x))
			     (id (theory-name (car x)))
			     (id (car x))))))
    (let ((declarations (mapcar #'(lambda (idecl)
				    (format-decl-list (car idecl)
						      (ptype-of (car idecl))
						      (cdr idecl)))
				(remove-if-not #'place *list-declarations*))))
      (write-declaration-info declarations))))


;;; Called by Emacs - whereis-declaration-used command

(defun whereis-declaration-used (oname origin pos &optional x?)
  (declare (ignore x?))
  (if (or (equal origin "Declaration")
	  (typechecked-origin? oname origin))
      (let* ((object (get-id-object-at oname origin pos))
	     (decl (if (typep object '(or declaration importing))
		       object
		       (get-decl-associated-with object))))
	(if decl
	    (let ((declarations nil))
	      (do-all-theories
	       #'(lambda (th)
		   (setq declarations
			 (nconc
			  (mapcan
			      #'(lambda (d)
				  (when (memq decl
					      (if (typep d 'declaration)
						  (unless (generated-by d)
						    (refers-to d))
						  (collect-references d)))
				    (list (format-decl-list d (ptype-of d) th))))
			    (all-decls th))
			  declarations))))
	      (write-declaration-info declarations))
	    (pvs-message "Could not find associated declaration")))
      (pvs-message "~a is not typechecked" oname)))


(defun whereis-identifier-used (string)
  (let ((declarations nil)
	(sym (intern string)))
    (do-all-theories #'(lambda (th)
			 (setq declarations
			       (append (get-whereis-info sym th)
				       declarations))))
    (write-declaration-info declarations)))

(defun get-find-declaration-info (id theory)
  (mapcar #'(lambda (d)
	      (format-decl-list d (ptype-of d) theory))
	  (remove-if-not #'(lambda (d)
			     (and (typep d 'declaration)
				  (eq id (id d))
				  (place d)))
			 (append (formals theory)
				 (assuming theory)
				 (if (module? theory)
				     (theory theory)
				     (constructors theory))))))


;;; Returns a list of declaration information; each entry is a list
;;; consisting of:
;;;    decl-string	- the string to be displayed
;;;    declname		- the name of the declaration
;;;    theoryname		- the name of the theory containing the declaration
;;;    filename		- the name of the file containing the theory
;;;    location		- (startline startcol endline endcol)

(defun format-decl-list (decl type theory)
  (assert (or (place decl) (from-prelude? decl)))
  (list (format nil "~25A ~25A ~25A"
	  (struncate (if (typep decl 'importing)
			 (unparse decl :string t)
			 (id decl))
		     25)
	  (struncate type 25)
	  (struncate (id theory) 25))
	(if (typep decl 'importing)
	    (unparse decl :string t)
	    (string (id decl)))
	(string (id theory))
	(when (filename theory)
	  (pvs-filename theory))
	(place-list (place decl))
	(unparse-decl decl)))

(defmethod pvs-filename ((theory datatype-or-module))
  (namestring (filename theory)))

(defmethod pvs-filename ((theory library-theory))
  (namestring (format nil "~a~a" (library-path theory) (filename theory))))

(defmethod pvs-filename ((theory library-datatype))
  (namestring (format nil "~a~a" (library-path theory) (filename theory))))


(defun ptype-of (decl)
  (let ((*default-char-width* 1000000))
    (if (and (slot-exists-p decl 'declared-type)
	     (declared-type decl))
	(if (and (slot-exists-p decl 'formals)
		 (formals decl))
	    (unparse (type decl) :string t)
	    (unparse (declared-type decl) :string t))
	(typecase decl
	  ((or const-decl def-decl)
	   (if (funtype? (declared-type decl))
	       "function" "const"))
	  (module "theory")
	  (formula-decl (format nil "~(~a~)" (spelling decl)))
	  (type-decl "type")
	  (mod-decl "theory declaration")
	  (theory-abbreviation-decl "theory abbr")
	  (formal-const-decl "formal-const")
	  (formal-type-decl "formal-type")
	  (importing "IMPORTING")
	  (conversion-decl "CONVERSION")
	  (datatype "DATATYPE")
	  (adt-constructor "CONSTRUCTOR")
	  (t (error "decl ~a not recognized" decl))))))
  

(defun list-declarations* (theory)
  (unless (member theory *modules-visited*)
    (if (typechecked? theory)
	(setq *list-declarations*
	      (append
	       (mapcar #'(lambda (d) (cons d theory))
		 (remove-if #'(lambda (x)
				(or (typep x '(or var-decl
						  field-decl))
				    (generated-by x)))
		   (all-decls theory)))
	       *list-declarations*))
	(setq *list-declarations*
	      (append (mapcar #'(lambda (d) (cons d theory))
			      (remove-if #'(lambda (x)
					     (or (not (typep x 'declaration))
						 (typep x '(or var-decl
							    field-decl))))
					 (append (assuming theory) (theory theory))))
		      *list-declarations*)))
    (push theory *modules-visited*)
    (mapc #'(lambda (entry) (list-declarations* (car entry)))
	  (all-usings theory))))
      

(defun write-declaration-info (declarations)
  declarations)


(defun get-whereis-info (sym theory)
  (mapcar #'(lambda (d)
	      (format-decl-list d (ptype-of d) theory))
	  (when (parsed? theory)
	    (remove-if-not #'(lambda (d) (and (place d)
					      (whereis sym d)))
			   (append (formals theory)
				   (assuming theory)
				   (if (module? theory)
				       (theory theory)
				       (constructors theory)))))))

;;; Recurse down the declarations, looking for the symbol.  Quit when
;;; the symbol is found.

(defmethod whereis (sym (l list))
  (some #'(lambda (e) (whereis sym e)) l))

(defmethod whereis (sym (u importing))
  (whereis sym (theory-name u)))

(defmethod whereis (sym (adt datatype))
  (whereis sym (constructors adt)))

(defmethod whereis (sym (d simple-constructor))
  (or (eq sym (id d))
      (whereis sym (arguments d))
      (eq sym (recognizer d))))

(defmethod whereis (sym (d declaration))
  (eq sym (id d)))

(defmethod whereis (sym (d mod-decl))
  (or (eq sym (id d))
      (whereis sym (modname d))))

(defmethod whereis (sym (d type-decl))
  (eq sym (id d)))

(defmethod whereis (sym (d type-def-decl))
  (or (eq sym (id d))
      (whereis sym (type-expr d))))

(defmethod whereis (sym (te type-application))
  (or (whereis sym (type te))
      (whereis sym (parameters te))))

(defmethod whereis (sym (te subtype))
  (or (whereis sym (supertype te))
      (whereis sym (predicate te))))

(defmethod whereis (sym (te funtype))
  (or (whereis sym (domain te))
      (whereis sym (range te))))

(defmethod whereis (sym (te tupletype))
  (whereis sym (types te)))

(defmethod whereis (sym (te cotupletype))
  (whereis sym (types te)))

(defmethod whereis (sym (te recordtype))
  (whereis sym (fields te)))

(defmethod whereis (sym (d field-decl))
  (or (eq sym (id d))
      (whereis sym (declared-type d))))

(defmethod whereis (sym (d var-decl))
  (or (eq sym (id d))
      (whereis sym (declared-type d))))

(defmethod whereis (sym (d const-decl))
  (or (eq sym (id d))
      (whereis sym (declared-type d))
      (when (and (slot-boundp d 'definition)
		 (definition d))  
	(whereis sym (definition d)))))

(defmethod whereis (sym (d def-decl))
  (or (eq sym (id d))
      (whereis sym (declared-type d))
      (whereis sym (definition d))
      (whereis sym (measure d))))

(defmethod whereis (sym (d formula-decl))
  (or (eq sym (id d))
      (whereis sym (definition d))))

(defmethod whereis (sym (d dep-binding))
  (or (eq sym (id d))
      (whereis sym (type d))))

(defmethod whereis (sym (e number-expr))
  (equal sym (number e)))

(defmethod whereis (sym (e record-expr))
  (whereis sym (assignments e)))

(defmethod whereis (sym (e tuple-expr))
  (whereis sym (exprs e)))

;(defmethod whereis (sym (e intype))
;  (or (whereis sym (declared-type e))
;      (whereis sym (expression e))))

;(defmethod whereis (sym (e coercion))
;  (or (whereis sym (declared-type e))
;      (whereis sym (expression e))))

(defmethod whereis (sym (e cases-expr))
  (or (whereis sym (expression e))
      (whereis sym (selections e))
      (whereis sym (else-part e))))

(defmethod whereis (sym (e selection))
  (or (whereis sym (constructor e))
      (whereis sym (args e))
      (whereis sym (expression e))))

(defmethod whereis (sym (e projection-expr))
  (eq sym (id e)))

(defmethod whereis (sym (e injection-expr))
  (eq sym (id e)))

(defmethod whereis (sym (e injection?-expr))
  (eq sym (id e)))

(defmethod whereis (sym (e extraction-expr))
  (eq sym (id e)))

(defmethod whereis (sym (e projection-application))
  (or (eq sym (id e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e injection-application))
  (or (eq sym (id e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e injection?-application))
  (or (eq sym (id e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e extraction-application))
  (or (eq sym (id e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e field-application))
  (or (eq sym (id e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e application))
  (or (whereis sym (operator e))
      (whereis sym (argument e))))

(defmethod whereis (sym (e binding-expr))
  (or (whereis sym (bindings e))
      (whereis sym (expression e))))

(defmethod whereis (sym (e update-expr))
  (or (whereis sym (expression e))
      (whereis sym (assignments e))) )

(defmethod whereis (sym (a assignment))
  (or (whereis sym (expression a))
      (whereis sym (arguments a))))

(defmethod whereis (sym (d simple-decl))
  (or (eq sym (id d))
      (whereis sym (declared-type d))))

(defmethod whereis (sym (n name))
  (or (eq sym (mod-id n))
      (whereis sym (actuals n))
      (eq sym (id n))))

(defmethod whereis (sym (a actual))
  (whereis sym (expr a)))

(defmethod struncate ((str string) len)
  (if (> (length str) len)
      (concatenate 'string (subseq str 0 (1- len)) "$")
      str))

(defmethod struncate ((sym symbol) len)
  (struncate (string sym) len))

(defmethod struncate (obj len)
  (struncate (format nil "~a" obj) len))

(defun squote (str)
  (format nil "\"~a\"" str))


;;; Change-declaration-name

;(defun change-declaration-name (from-name to-id root-theory)
;  (let* ((from (pc-parse from-name 'name))
;	 (res (when from (resolve from nil nil nil))))
;    (cond ((cdr res)
;	   (pvs-message "~a is ambiguous" from-name))
;	  ((null res)
;	   (pvs-message "No resolution for ~a" from-name))
;	  (t 
