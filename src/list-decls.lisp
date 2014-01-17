;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-decls.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Dec 19 16:12:09 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Oct 29 22:43:06 1998
;; Update Count    : 3
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

;;; ---------------------------------------------
;;; Support for the show-declaration, find-declaration,
;;; whereis-declaration-used, and list-declarations commands.  Note that
;;; all of these return a file name rather than the actual list; this is
;;; because the list could get large.


(in-package :pvs)

(export '(collect-pvs-file-decls-info))

(defvar *list-declarations* nil)

;;; Called by Emacs - show-expanded-form command

(defun show-expanded-form (oname origin pos1 &optional (pos2 pos1) all?)
  (if (or (equal origin "Declaration")
	  (typechecked-origin? oname origin))
      (multiple-value-bind (object *current-theory*)
	  (get-object-at oname origin pos1 pos2)
	(when object
	  (let ((*disable-gc-printout* t)
		(*current-context* (context *current-theory*)))
	    (pvs-buffer "Expanded Form"
	      (unparse (full-name object nil (not all?)) :string t)
	      t))
	  (place-list (place object))))
      (pvs-message "~a is not typechecked" oname)))

;;; Called by Emacs - show-declaration command

(defvar *containing-type* nil)
(defvar *show-declarations-alist* nil)

(defun show-declaration (oname origin pos &optional x?)
  (if (or (equal origin "declaration")
	  (typechecked-origin? oname origin))
      (multiple-value-bind (object *containing-type* theory)
	  (get-id-object-at oname origin pos)
	(let ((decl (get-decl-associated-with object)))
	  (if decl
	      (let ((thname (format nil "~@[~a@~]~a"
			      (when (library-datatype-or-theory? (module decl))
				(let ((*current-context* (context theory)))
				  (assert *current-context*)
				  (get-lib-id (lib-ref (module decl)))))
			      (id (module decl)))))
		(multiple-value-bind (declstr place-hash)
		    (unparse-with-places
		     decl (decl-nt decl)
		     (format nil "% From theory ~a:" thname)
		     (when *containing-type*
		       (let ((kind (typecase decl
				     (field-decl "field")
				     (dep-binding "declaration"))))
			 (when kind
			   (format nil "% (~a in ~a)" kind
				   *containing-type*)))))
		  (let ((dname (format nil "~a.~a" thname (id decl))))
		    (setf *show-declarations-alist*
			  (acons dname (list decl place-hash)
				 (remove dname *show-declarations-alist*
					 :key #'car :test #'string=)))
		    (if x?
			(pvs-wish-source
			 (with-output-to-temp-file
			  (format t "show-declaration ~a ~a ~a {~a}~%"
			    (id object)
			    (min (length declstr) *default-char-width*)
			    (count #\Newline declstr)
			    declstr)))
			(pvs-buffer dname declstr 'popto t nil "Declaration"))))
	      ;;(pvs-message "Could not find associated declaration")
	      ))))
      (pvs-message "~a is not typechecked" oname)))

(defmethod decl-nt ((decl declaration))
  (let ((th (module decl)))
    (if th
	(cond ((memq decl (theory th)) 'theory-elt)
	      ((memq decl (assuming th)) 'assuming)
	      ((memq decl (formals th)) 'theory-formals)
	      (t (assert (binding? decl)) 'expr))
	'expr)))

(defmethod decl-nt ((decl simple-decl))
  'simplebind)

(defmethod decl-nt ((decl t))
  'expr)


;;; Called by Emacs - goto-declaration command

(defun goto-declaration (oname origin pos)
  (if (typechecked-origin? oname origin)
      (let* ((object (get-id-object-at oname origin pos))
	     (decl (get-decl-associated-with object)))
	(when decl
	  (pvs-locate (module decl) decl)))
      (pvs-message "~a has not been typechecked" oname)))

(defun typechecked-origin? (name origin)
  (case (intern (#+allegro string-downcase #-allegro string-upcase origin)
		:pvs)
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
	  (let ((object (get-object-in-theory-at objects theory pos1 pos2)))
	    (values object theory))
	  (pvs-message "Region may not cross theory boundaries")))))

(defun get-object-in-theory-at (objects theory pos1 pos2)
  (let* ((decls (all-decls theory))
	 (decl (find-element-containing-pos decls pos1)))
    (unless decl
      (setq decl (find-decl-after-pos decls pos1)))
    (if (or (equal pos1 pos2)
	    (within-place pos2 (place decl)))
	(or decl
	    (get-object-in-declaration-at objects pos1 pos2))
	(let ((decl2 (find-element-containing-pos decls pos2)))
	  (unless decl2
	    (setq decl2
		  (find-decl-before-pos decls pos2)))
	  (or (when (and decl decl2)
		(ldiff (memq decl decls) (cdr (memq decl2 decls))))
	      (when decl
		(list decl))
	      (when decl2
		(list decl2)))))))

(defun find-decl-after-pos (decls pos)
  (when decls
    (if (and (place (car decls))
	     (or (< (car pos) (starting-row (place (car decls))))
		 (and (= (car pos) (starting-row (place (car decls))))
		      (<= (cadr pos) (starting-col (place (car decls)))))))
	(car decls)
	(find-decl-after-pos (cdr decls) pos))))

(defun find-decl-before-pos (decls pos &optional prev)
  (when decls
    (if (and (place (car decls))
	     (or (< (car pos) (starting-row (place (car decls))))
		 (and (= (car pos) (starting-row (place (car decls))))
		      (<= (cadr pos) (starting-col (place (car decls)))))))
	prev
	(find-decl-before-pos (cdr decls) pos (car decls)))))

(defun find-element-containing-pos (list pos)
  (when list
    (if (and (place (car list))
	     (or (precedes-place pos (place (car list)))
		 (within-place pos (place (car list)))))
	(car list)
	(find-element-containing-pos (cdr list) pos))))

(defun get-object-in-declaration-at (decl pos1 pos2)
  (let ((object nil)
	(objects nil)
	(*parsing-or-unparsing* t))
    (mapobject #'(lambda (ex)
		   (or (and (syntax? ex)
			    ;;(or (place ex) (break "Place not set"))
			    (place ex)
			    (within-place pos1 (place ex))
			    (or (equal pos1 pos2)
				(within-place pos2 (place ex)))
			    (unless (arg-tuple-expr? ex)
			      (push ex objects)
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
    (values object objects)))

(defun get-id-object-at (oname origin pos)
  (multiple-value-bind (objects theories)
      (get-syntactic-objects-for oname origin)
    (let ((containing-type nil)
	  (object nil)
	  (theory (when (listp theories) (car theories)))
	  (*parsing-or-unparsing* t))
      (mapobject #'(lambda (ex)
		     (or object
			 (and (syntax? ex)
			      (not (eq ex objects))
			      ;;(or (place ex) (break "Place not set"))
			      (place ex)
			      (slot-exists-p ex 'id)
			      (if (hash-table-p theories)
				  (let ((epos (gethash ex theories)))
				    (and epos
					 (within-place pos epos)))
				  (within-place pos (id-place ex)))
			      (setq object ex)
			      t)
			 (when (and (typep ex '(and syntax
						    (not assignment)
						    (or funtype
							tupletype
							recordtype)))
				    (if (hash-table-p theories)
					(let ((epos (gethash ex theories)))
					  (and epos
					       (within-place pos epos)))
					(and (place ex)
					     (within-place pos (place ex)))))
			   (typecase ex
			     (datatype-or-module (setq theory ex))
			     (expr (setq containing-type
					 (find-supertype (type ex))))
			     ((or funtype tupletype recordtype)
			      (setq containing-type ex)))
			   nil)
			 (when object t)))
		 objects)
      (values object containing-type theory))))

(defun get-syntactic-objects-for (name origin)
  (case (intern (#+allegro string-downcase #-allegro string-upcase origin)
		:pvs)
    (ppe (let ((theory (get-theory name)))
	   (when theory
	     (values (ppe-form theory) (list theory)))))
    (tccs (let ((theory (get-theory name)))
	    (when theory
	      (values (tcc-form theory) (list theory)))))
    (declaration (let ((entry (assoc name *show-declarations-alist*
				     :test #'string=)))
		   (when entry
		     (values-list (cdr entry)))))
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
	(id (intern string :pvs)))
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
	(sym (intern string :pvs)))
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
  (list (format nil "~25A ~25A ~25A"
	  (struncate (if (typep decl '(or importing auto-rewrite-decl))
			 (unparse decl :string t :no-newlines? t)
			 (id decl))
		     25)
	  (struncate type 25)
	  (struncate (id theory) 25))
	(if (typep decl '(or importing auto-rewrite-decl))
	    (unparse decl :string t :no-newlines? t)
	    (string (id decl)))
	(string (id theory))
	(when (filename theory)
	  (pvs-filename theory))
	(if (place decl)
	    (place-list (place decl))
	    nil)
	(unparse-decl decl)))

(defmethod pvs-filename ((theory datatype-or-module))
  (namestring (filename theory)))

(defmethod pvs-filename ((theory library-theory))
  (namestring (format nil "~a~a"
		(libref-to-pathname (lib-ref theory))
		(filename theory))))

(defmethod pvs-filename ((theory library-datatype))
  (namestring (format nil "~a~a"
		(libref-to-pathname (lib-ref theory))
		(filename theory))))


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
	  (auto-rewrite-decl "AUTO_REWRITE")
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

(defvar *max-row* nil)

(defun names-info (pvs-file)
  (let ((info (collect-pvs-file-decls-info pvs-file))
	(json:*lisp-identifier-name-to-json* #'identity))
    (json:encode-json-to-string info)))

(defun collect-pvs-file-decls-info (pvs-file)
  (if (string= pvs-file "prelude")
      (collect-visible-decl-info *prelude-theories*)
      (let* ((file (make-specpath pvs-file))
	     (theories (get-theories pvs-file))
	     (*max-row* (when theories (svref (place (car (last theories))) 2))))
	(cond ((not (file-exists-p file))
	       (pvs-message "PVS file ~a is not in the current context" pvs-file))
	      ((null theories)
	       (pvs-message "PVS file ~a is not typechecked" pvs-file))
	      (t (collect-visible-decl-info theories))))))

;; This doesn't work
;; (defun collect-name-to-decl-alist (obj)
;;   (let ((alist nil))
;;     (mapobject #'(lambda (x)
;; 		   (when (and (name? x) (resolution x) (place x))
;; 		     ;;(format t "~%~a: ~a - ~a" x (declaration x) (place x))
;; 		     (push (list (place-list x)
;; 				 (str (declaration x))
;; 				 (theory-filename (module (declaration x)))
;; 				 (place-list (declaration x)))
;; 			    alist))
;; 		   (null (place x)))
;; 	       obj)
;;     (json:encode-json-to-string alist)))

(defmethod theory-filename ((obj datatype-or-module))
  (if (from-prelude? obj)
      (format nil "~a/lib/prelude.pvs" *pvs-path*)
      (format nil "~a.pvs" (filename obj))))

(defmethod theory-filename ((obj library-datatype-or-theory))
  (format nil "~a~a.pvs"
    (libref-to-pathname (lib-ref obj))
    (filename obj)))

(defvar *visible-decl-info*)

(defun collect-visible-decl-info (obj)
  (let ((*visible-decl-info* nil))
    (collect-visible-decl-info* obj)
    *visible-decl-info*))

(defmethod collect-visible-decl-info* ((list list))
  (when list
    (collect-visible-decl-info* (car list))
    (collect-visible-decl-info* (cdr list))))

(defmethod collect-visible-decl-info* :around ((obj datatype-or-module))
  (collect-visible-decl-info* (formals obj))
  (collect-visible-decl-info* (assuming obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj module))
  (collect-visible-decl-info* (theory obj)))

(defmethod collect-visible-decl-info* ((obj recursive-type))
  (collect-visible-decl-info* (importings obj))
  (collect-visible-decl-info* (constructors obj)))

(defmethod collect-visible-decl-info* ((obj simple-constructor))
  (collect-visible-decl-info* (arguments obj)))

(defmethod collect-visible-decl-info* ((obj exporting))
  (collect-visible-decl-info* (names obj)))

(defmethod collect-visible-decl-info* ((obj importing))
  (collect-visible-decl-info* (theory-name obj)))

(defmethod collect-visible-decl-info* ((obj declaration))
  (when (place obj)
    (collect-visible-decl-info* (formals obj))))

(defmethod collect-visible-decl-info* :around ((obj typed-declaration))
  (when (place obj)
    (collect-visible-decl-info* (declared-type obj))
    (call-next-method)))

(defmethod collect-visible-decl-info* :around ((obj type-decl))
  (collect-visible-decl-info* (type-value obj))
  (call-next-method))

(defmethod collect-visible-decl-info* :around ((obj type-def-decl))
  (collect-visible-decl-info* (type-expr obj))
  (collect-visible-decl-info* (contains obj))
  (call-next-method))

(defmethod collect-visible-decl-info* :around ((obj formal-theory-decl))
  (collect-visible-decl-info* (theory-name obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj mod-decl))
  (collect-visible-decl-info* (modname obj)))

(defmethod collect-visible-decl-info* ((obj theory-abbreviation-decl))
  (collect-visible-decl-info* (theory-name obj)))

(defmethod collect-visible-decl-info* :around ((obj const-decl))
  (collect-visible-decl-info* (definition obj))
  (call-next-method))

(defmethod collect-visible-decl-info* :around ((obj def-decl))
  (collect-visible-decl-info* (declared-measure obj))
  (collect-visible-decl-info* (ordering obj))
  (call-next-method))

(defmethod collect-visible-decl-info* :around ((obj formula-decl))
  (when (place obj)
    (collect-visible-decl-info* (definition obj))
    (call-next-method)))

(defmethod collect-visible-decl-info* ((obj subtype-judgement))
  (collect-visible-decl-info* (declared-subtype obj)))

(defmethod collect-visible-decl-info* ((obj number-judgement))
  (collect-visible-decl-info* (number-expr obj)))

(defmethod collect-visible-decl-info* ((obj name-judgement))
  (collect-visible-decl-info* (name obj)))

(defmethod collect-visible-decl-info* ((obj application-judgement))
  (collect-visible-decl-info* (name obj))
  (collect-visible-decl-info* (formals obj))
  (collect-visible-decl-info* (judgement-type obj)))

(defmethod collect-visible-decl-info* ((obj expr-judgement))
  (collect-visible-decl-info* (expr obj))
  (collect-visible-decl-info* (formals obj))
  (collect-visible-decl-info* (judgement-type obj)))

(defmethod collect-visible-decl-info* ((obj conversion-decl))
  (unless (generated-by obj)
    (collect-visible-decl-info* (expr obj))))

(defmethod collect-visible-decl-info* ((obj auto-rewrite-decl))
  (collect-visible-decl-info* (rewrite-names obj)))

(defmethod collect-visible-decl-info* ((obj constant-rewrite-name))
  (collect-visible-decl-info* (declared-type obj)))

(defmethod collect-visible-decl-info* :around ((obj type-expr))
  (cond ((print-type obj)
	 (collect-visible-decl-info* (print-type obj)))
	((or (place obj)
	     (typep obj 'domain-tupletype))
	 (call-next-method))))

(defmethod collect-visible-decl-info* ((obj type-application))
  (collect-visible-decl-info* (type obj))
  (collect-visible-decl-info* (parameters obj)))

(defmethod collect-visible-decl-info* ((obj subtype))
  (collect-visible-decl-info* (supertype obj))
  (collect-visible-decl-info* (predicate obj)))

(defmethod collect-visible-decl-info* ((obj setsubtype))
  (collect-visible-decl-info* (formals obj))
  (collect-visible-decl-info* (formula obj)))

(defmethod collect-visible-decl-info* ((obj expr-as-type))
  (collect-visible-decl-info* (expr obj)))

(defmethod collect-visible-decl-info* ((obj funtype))
  (collect-visible-decl-info* (domain obj))
  (collect-visible-decl-info* (range obj)))

(defmethod collect-visible-decl-info* ((obj tuple-or-struct-subtype))
  (collect-visible-decl-info* (types obj)))

(defmethod collect-visible-decl-info* ((obj cotupletype))
  (collect-visible-decl-info* (types obj)))

(defmethod collect-visible-decl-info* ((obj dep-domain-tupletype))
  (collect-visible-decl-info* (var-bindings obj))
  (collect-visible-decl-info* (types obj)))

(defmethod collect-visible-decl-info* ((obj record-or-struct-subtype))
  (collect-visible-decl-info* (fields obj)))

(defmethod collect-visible-decl-info* :around ((obj struct-sub-recordtype))
  (collect-visible-decl-info* (type obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj type-extension))
  (collect-visible-decl-info* (type obj))
  (collect-visible-decl-info* (extension obj)))

(defmethod collect-visible-decl-info* ((obj binding-type))
  (collect-visible-decl-info* (bindings obj))
  (collect-visible-decl-info* (type obj)))

(defmethod collect-visible-decl-info* :around ((obj typed-name-expr))
  (collect-visible-decl-info* (declared-type obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj fieldex))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj)))

(defmethod collect-visible-decl-info* ((obj field-application))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj projection-application))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj injection-application))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj injection?-application))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj extraction-application))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj tuple-expr))
  (collect-visible-decl-info* (exprs obj)))

(defmethod collect-visible-decl-info* ((obj record-expr))
  (collect-visible-decl-info* (assignments obj)))

(defmethod collect-visible-decl-info* ((obj cases-expr))
  (collect-visible-decl-info* (expression obj))
  (collect-visible-decl-info* (selections obj))
  (collect-visible-decl-info* (else-part obj)))

(defmethod collect-visible-decl-info* ((obj selection))
  (collect-visible-decl-info* (constructor obj))
  (collect-visible-decl-info* (args obj))
  (collect-visible-decl-info* (expression obj)))

(defmethod collect-visible-decl-info* ((obj application))
  (collect-visible-decl-info* (operator obj))
  (collect-visible-decl-info* (argument obj)))

(defmethod collect-visible-decl-info* ((obj implicit-conversion))
  (collect-visible-decl-info* (argument obj)))

;;; Tables should go here

(defmethod collect-visible-decl-info* ((obj binding-expr))
  (collect-visible-decl-info* (bindings obj))
  (collect-visible-decl-info* (expression obj)))

(defmethod collect-visible-decl-info* ((obj set-list-expr))
  (collect-visible-decl-info* (exprs obj)))

(defmethod collect-visible-decl-info* :around ((obj fieldex-lambda-expr))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj update-expr))
  (collect-visible-decl-info* (expression obj))
  (collect-visible-decl-info* (assignments obj)))

(defmethod collect-visible-decl-info* ((obj assignment))
  (collect-visible-decl-info* (arguments obj))
  (collect-visible-decl-info* (expression obj)))

(defmethod collect-visible-decl-info* ((obj simple-decl))
  (unless (untyped-bind-decl? obj)
    (collect-visible-decl-info* (declared-type obj)))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj actual))
  (collect-visible-decl-info* (expr obj)))

(defmethod collect-visible-decl-info* ((obj mapping))
  (collect-visible-decl-info* (lhs obj))
  (collect-visible-decl-info* (rhs obj))
  (collect-visible-decl-info* (declared-type obj)))

(defmethod collect-visible-decl-info* :around ((obj mapping-with-formals))
  (collect-visible-decl-info* (formals obj))
  (call-next-method))

(defmethod collect-visible-decl-info* :around ((obj mapping-lhs))
  (collect-visible-decl-info* (decl-formals obj))
  (call-next-method))

(defmethod collect-visible-decl-info* ((obj rational-expr))
  )

(defmethod collect-visible-decl-info* (obj)
  (break "Should define method for ~a" obj))

(defmethod collect-visible-decl-info* ((obj name))
  (collect-visible-decl-info* (actuals obj))
  (collect-visible-decl-info* (dactuals obj))
  (collect-visible-decl-info* (mappings obj))
  (collect-visible-decl-info* (target obj))
  (name-visible-decl-info obj (place obj)))

(defun field-appl-id-visible-decl-info (fappl place)
  (when place
    (push `(("id" . ,(id fappl))
	    ("place" . ,(id-place-list fappl place))
	    ("decl" . ,(format nil "FieldAccess: ~a" (type fappl))))
	  *visible-decl-info*)))

(defvar *tooltip-char-width* *default-char-width*)

(defun name-visible-decl-info (obj place)
  (when (and place
	     (or (resolution obj)
		 (simple-decl? obj)))
    (when (and *max-row* (> (svref place 2) *max-row*))
      (break "Something's wrong"))
    ;;(format t "~%~a: ~a - ~a" x (declaration x) (place x))
    (push `(("id" . ,(id obj))
	    ("place" . ,(id-place-list obj place))
	    ("decl" . ,(let ((*default-char-width* *tooltip-char-width*))
			   (if (resolution obj)
			       (if (and (simple-decl? (declaration obj))
					(null (declared-type (declaration obj))))
				   (format nil "~a: ~a"
				     (id (declaration obj))
				     (type (declaration obj)))
				   (if (datatype-or-module? (declaration obj))
				       (format nil "~a~a: theory"
					 (id (declaration obj))
					 (with-output-to-string (*standard-output*)
					   (pp-theory-formals (formals (declaration obj)))))
				       (concatenate 'string
					 (if (skolem-constant? obj)
					     "Skolem Constant: " "")
					 (str (declaration obj)))))
			       (if (and (declared-type obj)
					(not (untyped-bind-decl? obj)))
				   (str obj)
				   (format nil "~a: ~a" (id obj) (type obj))))))
	    ("decl-file" . ,(unless (skolem-constant? obj)
				   (let ((th (module (declaration obj))))
				     ;;(unless th (break "No module?"))
				     (when th (theory-filename th)))))
	    ("decl-place" . ,(unless (skolem-constant? obj)
				    (place-list (declaration obj)))))
	  *visible-decl-info*)))


(defun names-info-proof-formula (s-form &optional json?)
  (let* ((info (collect-visible-decl-info (cadr (view s-form)))))
    (if json?
	(let ((json:*lisp-identifier-name-to-json* #'identity))
	  (json:encode-json-to-string info))
	info)))

(defmethod collect-visible-decl-info* ((obj view-node))
  (let ((term (vnode-term obj)))
    (typecase term
      (name
       (name-visible-decl-info term (vnode-place obj)))
      (field-application
       (field-appl-id-visible-decl-info term (vnode-place obj))))
    (collect-visible-decl-info* (vnode-children obj))))

;;;

(defun get-subterms-at-place (pvs-file row col &optional json?)
  (if (string= pvs-file "prelude")
      (get-subterms-at *prelude-theories* (list row col))
      (let* ((file (make-specpath pvs-file))
	     (theories (get-theories pvs-file))
	     (*max-row* (when theories (svref (place (car (last theories))) 2))))
	(cond ((not (file-exists-p file))
	       (pvs-message "PVS file ~a is not in the current context" pvs-file))
	      ((null theories)
	       (pvs-message "PVS file ~a is not typechecked" pvs-file))
	      (t (let* ((subterms (get-subterms-at theories (list row col))))
		   (if json?
		       (let ((json:*lisp-identifier-name-to-json* #'identity))
			 (json:encode-json-to-string subterms))
		       subterms)))))))

(defun get-subterms-at (theories pos)
  (let ((*subterms-at-pos* pos)
	(*subterms-at* nil))
    (declare (special *subterms-at-pos* *subterms-at*))
    (get-subterms-at* theories)
    *subterms-at*))

(defmethod get-subterms-at* ((list list))
  (when list
    (get-subterms-at* (car list))
    (get-subterms-at* (cdr list))))

(defmethod get-subterms-at* ((obj datatype-or-module))
  (get-subterms-at* (formals obj))
  (get-subterms-at* (assuming obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj module))
  (get-subterms-at* (theory obj)))

(defmethod get-subterms-at* ((obj recursive-type))
  (get-subterms-at* (importings obj))
  (get-subterms-at* (constructors obj)))

(defmethod get-subterms-at* ((obj simple-constructor))
  (get-subterms-at* (arguments obj)))

(defmethod get-subterms-at* ((obj exporting))
  (get-subterms-at* (names obj)))

(defmethod get-subterms-at* ((obj importing))
  (get-subterms-at* (theory-name obj)))

(defmethod get-subterms-at* ((obj declaration))
  (when (place obj)
    (get-subterms-at* (formals obj))))

(defmethod get-subterms-at* ((obj typed-declaration))
  (when (place obj)
    (get-subterms-at* (declared-type obj))
    (call-next-method)))

(defmethod get-subterms-at* ((obj type-decl))
  (get-subterms-at* (type-value obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj type-def-decl))
  (get-subterms-at* (type-expr obj))
  (get-subterms-at* (contains obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj formal-theory-decl))
  (get-subterms-at* (theory-name obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj mod-decl))
  (get-subterms-at* (modname obj)))

(defmethod get-subterms-at* ((obj theory-abbreviation-decl))
  (get-subterms-at* (theory-name obj)))

(defmethod get-subterms-at* ((obj const-decl))
  (get-subterms-at* (definition obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj def-decl))
  (get-subterms-at* (declared-measure obj))
  (get-subterms-at* (ordering obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj formula-decl))
  (when (place obj)
    (get-subterms-at* (definition obj))
    (call-next-method)))

(defmethod get-subterms-at* ((obj subtype-judgement))
  (get-subterms-at* (declared-subtype obj)))

(defmethod get-subterms-at* ((obj number-judgement))
  (get-subterms-at* (number-expr obj)))

(defmethod get-subterms-at* ((obj name-judgement))
  (get-subterms-at* (name obj)))

(defmethod get-subterms-at* ((obj application-judgement))
  (get-subterms-at* (name obj))
  (get-subterms-at* (formals obj))
  (get-subterms-at* (judgement-type obj)))

(defmethod get-subterms-at* ((obj expr-judgement))
  (get-subterms-at* (expr obj))
  (get-subterms-at* (formals obj))
  (get-subterms-at* (judgement-type obj)))

(defmethod get-subterms-at* ((obj conversion-decl))
  (unless (generated-by obj)
    (get-subterms-at* (expr obj))))

(defmethod get-subterms-at* ((obj auto-rewrite-decl))
  (get-subterms-at* (rewrite-names obj)))

(defmethod get-subterms-at* ((obj constant-rewrite-name))
  (get-subterms-at* (declared-type obj)))

;;; Type expr subterms
(defmethod get-subterms-at* :around ((obj type-expr))
  (declare (special *subterms-at-pos* *subterms-at*))
  (cond ((print-type obj)
	 (get-subterms-at* (print-type obj)))
	((place obj)
	 (when (within-place *subterms-at-pos* (place obj))
	   (push obj *subterms-at*)
	   (call-next-method)))
	((typep obj 'domain-tupletype)
	 (call-next-method))))

(defmethod get-subterms-at* ((obj type-application))
  (get-subterms-at* (type obj))
  (get-subterms-at* (parameters obj)))

(defmethod get-subterms-at* ((obj subtype))
  (get-subterms-at* (supertype obj))
  (get-subterms-at* (predicate obj)))

(defmethod get-subterms-at* ((obj setsubtype))
  (get-subterms-at* (formals obj))
  (get-subterms-at* (formula obj)))

(defmethod get-subterms-at* ((obj expr-as-type))
  (get-subterms-at* (expr obj)))

(defmethod get-subterms-at* ((obj funtype))
  (get-subterms-at* (domain obj))
  (get-subterms-at* (range obj)))

(defmethod get-subterms-at* ((obj tuple-or-struct-subtype))
  (get-subterms-at* (types obj)))

(defmethod get-subterms-at* ((obj cotupletype))
  (get-subterms-at* (types obj)))

(defmethod get-subterms-at* ((obj dep-domain-tupletype))
  (get-subterms-at* (var-bindings obj))
  (get-subterms-at* (types obj)))

(defmethod get-subterms-at* ((obj record-or-struct-subtype))
  (get-subterms-at* (fields obj)))

(defmethod get-subterms-at* ((obj struct-sub-recordtype))
  (get-subterms-at* (type obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj type-extension))
  (get-subterms-at* (type obj))
  (get-subterms-at* (extension obj)))

(defmethod get-subterms-at* ((obj binding-type))
  (get-subterms-at* (bindings obj))
  (get-subterms-at* (type obj)))

(defmethod get-subterms-at* :around ((obj expr))
  (declare (special *subterms-at-pos* *subterms-at*))
  (when (place obj)
    (when (within-place *subterms-at-pos* (place obj))
      (unless (typep obj 'arg-tuple-expr)
	;;(when (string= (str obj) "sq(a)") (break "check"))
	(push obj *subterms-at*))
      (call-next-method))))

(defmethod get-subterms-at* ((obj typed-name-expr))
  (get-subterms-at* (declared-type obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj fieldex))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj)))

(defmethod get-subterms-at* ((obj field-application))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj projection-application))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj injection-application))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj injection?-application))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj extraction-application))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj tuple-expr))
  (get-subterms-at* (exprs obj)))

(defmethod get-subterms-at* ((obj record-expr))
  (get-subterms-at* (assignments obj)))

(defmethod get-subterms-at* ((obj cases-expr))
  (get-subterms-at* (expression obj))
  (get-subterms-at* (selections obj))
  (get-subterms-at* (else-part obj)))

(defmethod get-subterms-at* ((obj selection))
  (get-subterms-at* (constructor obj))
  (get-subterms-at* (args obj))
  (get-subterms-at* (expression obj)))

(defmethod get-subterms-at* ((obj application))
  (get-subterms-at* (operator obj))
  (get-subterms-at* (argument obj)))

(defmethod get-subterms-at* ((obj implicit-conversion))
  (get-subterms-at* (argument obj)))

;;; Tables should go here

(defmethod get-subterms-at* ((obj binding-expr))
  (get-subterms-at* (bindings obj))
  (get-subterms-at* (expression obj)))

(defmethod get-subterms-at* ((obj set-list-expr))
  (get-subterms-at* (exprs obj)))

(defmethod get-subterms-at* ((obj fieldex-lambda-expr))
  (get-subterms-at* (actuals obj))
  (get-subterms-at* (dactuals obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj update-expr))
  (get-subterms-at* (expression obj))
  (get-subterms-at* (assignments obj)))

(defmethod get-subterms-at* ((obj assignment))
  (get-subterms-at* (arguments obj))
  (get-subterms-at* (expression obj)))

(defmethod get-subterms-at* ((obj simple-decl))
  (unless (untyped-bind-decl? obj)
    (get-subterms-at* (declared-type obj)))
  (call-next-method))

(defmethod get-subterms-at* ((obj actual))
  (get-subterms-at* (expr obj)))

(defmethod get-subterms-at* ((obj mapping))
  (get-subterms-at* (lhs obj))
  (get-subterms-at* (rhs obj))
  (get-subterms-at* (declared-type obj)))

(defmethod get-subterms-at* ((obj mapping-with-formals))
  (get-subterms-at* (formals obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj mapping-lhs))
  (get-subterms-at* (decl-formals obj))
  (call-next-method))

(defmethod get-subterms-at* ((obj rational-expr))
  )

(defmethod get-subterms-at* (obj)
  (break "Should define method for ~a" obj))

(defmethod get-subterms-at* ((obj name))
  nil)
