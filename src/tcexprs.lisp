;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcexprs.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Dec  4 12:35:56 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Apr 12 14:56:18 2004
;; Update Count    : 50
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

(in-package :pvs)

;;; Typechecking expressions - the arguments for these methods are:
;;;   expr	- the expression being typechecked
;;;   expected  - the expected type of the expression
;;;   arguments - the arguments to the expression

;;; These methods determine the possible types of the expression.  When the
;;; expected type is provided, the type of the expression will be set, or
;;; there will be an error.

(defmethod typecheck* :around ((ex expr) expected kind arguments)
  (declare (ignore kind arguments))
  (cond ((and (eq *generate-tccs* 'none)
	      (type ex)))
	((type ex)
	 (when (and expected
		    (not (eq *generate-tccs* 'none)))
	   (let ((*no-conversions-allowed* t))
	     (if (eq *generate-tccs* 'all)
		 (check-for-tccs ex expected)
		 (check-for-subtype-tcc ex expected)))))
	(t (call-next-method)
	   (when expected
	     (set-type ex expected))))
  ex)


;;; Names - must set up the possible resolutions for the name, based on
;;; the USING list and the local declarations.  For each potential
;;; declaration, a set of bindings is kept which provides the instances
;;; of the formal parameters of the module to which the declaration
;;; belongs.  As typechecking progresses, the context of this name will
;;; cause some of the possibilities to be removed from the list.  When a
;;; point is reached at which no more information is available from the
;;; context, only one possibility should remain for all of the
;;; subexpressions, and it should be instantiated (i.e. not a generic
;;; module unless the name provides actual parameters)

(defmethod typecheck* ((expr name-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)			; This will set the resolutions
  (setf (types expr)
	(delete-duplicates (mapcar #'type (resolutions expr)) :test #'tc-eq))
  (assert (types expr)))


(defmethod typecheck* ((expr fieldex) expected kind argument)
  (declare (ignore expected kind argument))
  (cond ((actuals expr)
	 (unless (singleton? (actuals expr))
	   (type-error expr
	     "Field expression actuals must be a single type"))
	 (typecheck* (car (actuals expr)) nil 'type nil)
	 (unless (type-value (car (actuals expr)))
	   (type-error expr
	     "Field expression actual must be a type"))
	 (unless (recordtype? (find-supertype (type-value
					      (car (actuals expr)))))
	   (type-error expr
	     "Field expression actual must be a recordtype"))
	 (let* ((rtype (find-supertype (type-value (car (actuals expr)))))
		(field (find (id expr) (fields rtype) :key #'id)))
	   (unless field
	     (type-error expr
	       "Field ~a is not in record ~a" (id expr) rtype))
	   (if (dependent? rtype)
	       (let* ((id (make-new-variable '|r| rtype))
		      (db (mk-dep-binding id rtype))
		      (ne (make-dep-field-name-expr db rtype))
		      (ftype (field-application-type (id field) rtype ne)))
		 (setf (types expr) (list (mk-funtype db ftype))))
	       (setf (types expr)
		     (list (mk-funtype rtype (type field)))))))
	(t (setf (types expr)
		 (list (mk-funtype (make-instance 'rec-type-variable
				     :id (make-new-variable '|recT| expr))
				   (make-instance 'type-variable
				     :id (make-new-variable 'T expr))))))))

;;; Projection-exprs are created by the parser, and those that appear as
;;; operators to an application are then converted to
;;; projection-applications.

(defmethod typecheck* ((expr projection-expr) expected kind argument)
  (declare (ignore expected kind argument))
  (cond ((actuals expr)
	 (unless (singleton? (actuals expr))
	   (type-error expr
	     "Projection expression actuals must be a single type"))
	 (typecheck* (car (actuals expr)) nil 'type nil)
	 (unless (type-value (car (actuals expr)))
	   (type-error expr
	     "Projection expression actual must be a type"))
	 (unless (tupletype? (find-supertype (type-value
					      (car (actuals expr)))))
	   (type-error expr
	     "Projection expression actual must be a tupletype"))
	 (unless (<= (index expr)
		     (length (types (find-supertype (type-value
						     (car (actuals expr)))))))
	   (type-error expr
	     "Projection expression index ~d too large" (index expr)))
	 (let ((ttype (type-value (car (actuals expr)))))
	   (setf (types expr)
		 (list (make-projection-type expr ttype)))))
	(t (setf (types expr)
		 (list (mk-funtype (make-instance 'tup-type-variable
				     :id (make-new-variable '|tupT| expr))
				   (make-instance 'type-variable
				     :id (make-new-variable 'T expr))))))))

(defun make-projection-type (projection-expr &optional type)
  (let* ((ttype (or type (type projection-expr)))
	 (tuptype (find-supertype ttype))
	 (index (index projection-expr))
	 (first-dep-pos (position-if #'dep-binding? (types tuptype)))
	 (dep? (and first-dep-pos (< first-dep-pos (1- index)))))
    (if dep?
	(let* ((id (make-new-variable '|t| ttype))
	       (db (mk-dep-binding id ttype))
	       (dvar (make-variable-expr db)))
	  (mk-funtype db
		      (make!-projection-type* (types tuptype) index 1 dvar)))
	(mk-funtype ttype (nth (1- index) (types tuptype))))))
		      
	      
	 

(defmethod typecheck* ((expr injection-expr) expected kind argument)
  (declare (ignore kind expected argument))
  (cond ((actuals expr)
	 (unless (singleton? (actuals expr))
	   (type-error expr
	     "Injection expression actuals must be a single type"))
	 (typecheck* (car (actuals expr)) nil 'type nil)
	 (unless (type-value (car (actuals expr)))
	   (type-error expr
	     "Injection expression actual must be a type"))
	 (unless (cotupletype? (find-supertype (type-value
						(car (actuals expr)))))
	   (type-error expr
	     "Injection expression actual must be a cotupletype"))
	 (setf (types expr)
	       (list (mk-funtype
		      (nth (1- (index expr))
			   (types (find-supertype
				   (type-value (car (actuals expr))))))
		      (find-supertype (type-value (car (actuals expr))))))))
	(t (setf (types expr)
		 (list (mk-funtype (make-instance 'type-variable
				     :id (make-new-variable 'T expr))
				   (make-instance 'cotup-type-variable
				     :id (make-new-variable '|coT| expr))))))))

(defmethod typecheck* ((expr extraction-expr) expected kind argument)
  (declare (ignore kind expected argument))
  (cond ((actuals expr)
	 (unless (singleton? (actuals expr))
	   (type-error expr
	     "Extraction expression actuals must be a single type"))
	 (typecheck* (car (actuals expr)) nil 'type nil)
	 (unless (type-value (car (actuals expr)))
	   (type-error expr
	     "Extraction expression actual must be a type"))
	 (unless (cotupletype? (find-supertype (type-value
						(car (actuals expr)))))
	   (type-error expr
	     "Extraction expression actual must be a cotupletype"))
	 (setf (types expr)
	       (list (mk-funtype
		      (find-supertype (type-value (car (actuals expr))))
		      (nth (1- (index expr))
			   (types (find-supertype
				   (type-value (car (actuals expr))))))))))
	(t (setf (types expr)
		 (list (mk-funtype (make-instance 'cotup-type-variable
				     :id (make-new-variable '|coT| expr))
				   (make-instance 'type-variable
				     :id (make-new-variable 'T expr))))))))

(defmethod typecheck* ((expr injection?-expr) expected kind argument)
  (declare (ignore kind expected argument))
  (cond ((actuals expr)
	 (unless (singleton? (actuals expr))
	   (type-error expr
	     "Injection? recognizer actuals must be a single type"))
	 (typecheck* (car (actuals expr)) nil 'type nil)
	 (unless (type-value (car (actuals expr)))
	   (type-error expr
	     "Injection recognizer actual must be a type"))
	 (unless (cotupletype? (find-supertype (type-value
						(car (actuals expr)))))
	   (type-error expr
	     "Injection recognizer actual must be a cotupletype"))
	 (setf (types expr)
	       (list (mk-funtype
		      (find-supertype (type-value (car (actuals expr))))
		      *boolean*))))
	(t (setf (types expr)
		 (list (mk-funtype (make-instance 'cotup-type-variable
				     :id (make-new-variable '|coT| expr))
				   *boolean*))))))

(defmethod typecheck* ((expr projection-application) expected kind argument)
  (declare (ignore kind expected argument))
  (typecheck* (argument expr) nil nil nil)
  (let ((tuptypes (delete-if-not #'(lambda (ty)
				     (typep (find-supertype ty)
					    '(or tupletype struct-sub-tupletype)))
		    (ptypes (argument expr)))))
    (unless (or tuptypes
		*no-conversions-allowed*)
      (let ((cexpr (find-proj-application-conversion expr)))
	(when cexpr
	  (setf tuptypes (types cexpr)))))
    (unless tuptypes
      (type-error expr
	"The argument to a projection must be of a tuple type."))
    (let ((ptypes (delete-if-not #'(lambda (ty)
				     (>= (length (types (find-supertype ty)))
					 (index expr)))
		    tuptypes)))
      (unless ptypes
	(type-error expr
	  "The argument to ~a must be a tuple with length at least ~d"
	  (id expr) (max (index expr) 2)))
      (setf (types (argument expr)) ptypes)
      (let ((types (projection-application-types ptypes expr)))
	(assert types)
	(setf (types expr) types)))))

(defmethod typecheck* ((expr injection-application) expected kind argument)
  (declare (ignore kind expected argument))
  (when (actuals expr)
    (unless (singleton? (actuals expr))
      (type-error expr
	"Injection expression actuals must be a single type"))
    (typecheck* (car (actuals expr)) nil 'type nil)
    (unless (type-value (car (actuals expr)))
      (type-error expr
	"Injection expression actual must be a type"))
    (unless (cotupletype? (find-supertype (type-value (car (actuals expr)))))
      (type-error expr
	"Injection expression actual must be a cotupletype")))
    (typecheck* (argument expr) nil nil nil)
    (if (actuals expr)
	(let* ((intype (nth (1- (index expr))
			    (types (find-supertype
				    (type-value (car (actuals expr)))))))
	       (ptypes (remove-if (complement #'(lambda (ty)
						  (compatible? ty intype)))
			 (types (argument expr)))))
	  (if ptypes
	      (setf (types (argument expr)) ptypes)
	      (type-incompatible (argument expr)
				 (types (argument expr)) intype))
	  (setf (types expr)
		(list (find-supertype (type-value (car (actuals expr)))))))
	(setf (types expr)
	      (list (make-instance 'cotup-type-variable
		      :id (make-new-variable '|coT| expr))))))

(defmethod typecheck* ((expr extraction-application) expected kind argument)
  (declare (ignore kind expected argument))
  (when (actuals expr)
    (unless (singleton? (actuals expr))
      (type-error expr
	"Extraction expression actuals must be a single type"))
    (typecheck* (car (actuals expr)) nil 'type nil)
    (unless (type-value (car (actuals expr)))
      (type-error expr
	"Extraction expression actual must be a type"))
    (unless (cotupletype? (find-supertype (type-value (car (actuals expr)))))
      (type-error expr
	"Extraction expression actual must be a cotupletype")))
  (cond ((type (argument expr))
	 (let ((cotype (find-supertype (type (argument expr)))))
	   (unless (or (null (actuals expr))
		       (tc-eq (find-supertype
			       (type-value (car (actuals expr))))
			      cotype))
	     (type-error expr
	       "Extraction expression actual must match the argument type"))
	   (unless (<= (index expr) (length (types cotype)))
	     (type-error expr "Index is out of bounds"))
	   (setf (types expr) (list (nth (1- (index expr)) (types cotype))))))
	(t (let ((cotype (when (actuals expr)
			   (find-supertype
			    (type-value (car (actuals expr)))))))
	     (typecheck* (argument expr) cotype nil nil))
	   (let ((cotypes (if (actuals expr)
			      (ptypes (argument expr))
			      (remove-if (complement
					  #'(lambda (ty)
					      (cotupletype?
					       (find-supertype ty))))
				(ptypes (argument expr))))))
	     (if cotypes
		 (let ((lcotypes (remove-if
				     (complement
				      #'(lambda (ty)
					  (<= (index expr)
					      (length
					       (types (find-supertype ty))))))
				   cotypes)))
		   (if lcotypes
		       (setf (types (argument expr)) lcotypes)
		       (type-error expr "Index is out of bounds")))
		 (type-error expr
		   "Extraction argument must be a (known) cotuple type - may need to provide the type, e.g., OUT[[int + bool]](x)"))
	     (setf (types expr)
		   (mapcar #'(lambda (ty)
			       (nth (1- (index expr))
				    (types (find-supertype ty))))
		     cotypes))))))

(defmethod typecheck* ((expr injection?-application) expected kind argument)
  (declare (ignore kind expected argument))
  (when (actuals expr)
    (unless (singleton? (actuals expr))
      (type-error expr
	"Injection recognizer actuals must be a single type"))
    (typecheck* (car (actuals expr)) nil 'type nil)
    (unless (type-value (car (actuals expr)))
      (type-error expr
	"Injection recognizer actual must be a type"))
    (unless (cotupletype? (find-supertype (type-value (car (actuals expr)))))
      (type-error expr
	"Injection recognizer actual must be a cotupletype")))
  (let ((cotype (when (actuals expr)
		  (find-supertype (type-value (car (actuals expr)))))))
    (typecheck* (argument expr) cotype nil nil))
  (let ((cotypes (if (actuals expr)
		     (types (argument expr))
		     (remove-if (complement
				 #'(lambda (ty)
				     (cotupletype? (find-supertype ty))))
		       (types (argument expr))))))
    (if cotypes
	(let ((lcotypes (remove-if
			    (complement
			     #'(lambda (ty)
				 (<= (index expr)
				     (length (types (find-supertype ty))))))
			  cotypes)))
	  (if lcotypes
	      (setf (types (argument expr)) lcotypes)
	      (type-error expr "Index is out of bounds")))
	(type-error expr
	  "Injection? recognizer argument must be a cotuple type")))
  (setf (types expr) (list *boolean*)))

(defun projection-application-types (ptypes expr)
  (mapcar #'(lambda (pty)
	      (projection-application-type expr pty))
	  ptypes))

(defmethod typecheck* ((expr field-application) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (argument expr) nil nil nil)
  (let ((atypes (delete-if-not #'(lambda (pty)
				   (typep (find-supertype pty)
					  '(or recordtype struct-sub-recordtype)))
		  (ptypes (argument expr)))))
    (unless (or atypes
		*no-conversions-allowed*)
      (let ((cexpr (find-field-application-conversion expr)))
	(when cexpr
	  (setf atypes (types cexpr)))))
    (unless atypes
      (if (fieldappl? expr)
	  (type-error expr "Expression must be of a recordtype")
	  (type-error expr "Argument must be of a recordtype")))
    (let ((ptypes (delete-if-not #'(lambda (pty)
				     (member expr (fields (find-supertype pty))
					     :test #'same-id))
		    atypes)))
      (unless ptypes
	(type-error expr "Field does not occur in the given record."))
      (setf (types (argument expr)) ptypes)
      (setf (types expr)
	    (mapcar #'(lambda (pty)
			(let* ((*generate-tccs* 'none)
			       (*dont-worry-about-full-instantiations* t)
			       (targ (if (dependent? (find-supertype pty))
					 (typecheck* (copy-untyped
						      (argument expr))
						     pty nil nil)
					 (argument expr))))
			  (field-application-type (id expr) pty targ)))
	      ptypes)))))
    

(defmethod typecheck* ((expr number-expr) expected kind arguments)
  (declare (ignore expected kind))
  ;;(assert (typep *number* 'type-expr))
  (let ((reses (resolve* expr 'expr arguments)))
    (when reses
      (change-class expr 'name-expr
		    'id (number expr)
		    'resolutions reses))
    (assert *number_field*)
    (setf (types expr)
	  (cons (or *real* *number_field*) (mapcar #'type reses)))))

(defun available-numeric-type (num)
  (or (unless (zerop num) *posint*)
      *naturalnumber* *integer* *rational* *real* *number*
      (error "No type available for numerals")))

;;; Record-expr typechecking involves typechecking the assignments and
;;; setting the type to a new recordtype created based on the types of
;;; the assignments.  Dependencies and subtypes are handled in set-type.
;;; The resulting type is a newly constructed recordtype.

;;;               C |- a1:T1, ... , C |- an:Tn
;;;  ------------------------------------------------------------
;;;  C |- {# x1 := a1, ... , xn := an #}:[# x1:T1, ... , xn:Tn #]

(defmethod typecheck* ((expr record-expr) expected kind arguments)
  (declare (ignore expected kind))
  (when arguments
    (type-error expr
      "Record expressions may not be used as functions"))
  (let* ((fielddecls (typecheck-rec-assignments (assignments expr)))
	 (recfields (cartesian-product fielddecls))
	 (rectypes (mapcar #'(lambda (rf) (make-recordtype rf))
			   recfields)))
    (assert rectypes)
    ;;(set-possible-assignment-types (assignments expr) rectypes)
    (setf (types expr) rectypes)))

(defun set-possible-assignment-types (assigns rectypes)
  (when assigns
    (set-possible-assignment-types* (arguments (car assigns)) rectypes)
    (set-possible-assignment-types (cdr assigns) rectypes)))

(defun set-possible-assignment-types* (args rectypes)
  (let* ((arg (caar args))
	 (reses (possible-assignment-resolutions arg rectypes))
	 (types (mapcar #'type reses)))
    (setf (resolutions arg) reses
	  (types arg) types)))

(defun possible-assignment-resolutions (arg rectypes &optional reses)
  (if (null rectypes)
      reses
      (let* ((fld (find arg (fields (car rectypes)) :test #'same-id))
	     (res (make-resolution fld
		    (theory-name *current-context*)
		    (mk-funtype (list (car rectypes))
				(type fld)))))
	(possible-assignment-resolutions arg (cdr rectypes)
					 (cons res reses)))))



;;; Typecheck-rec-assignments recursively checks that each assignment of
;;; a record expression satisfies: the LHS is a name without further
;;; arguments (partial assignments are not allowed in record
;;; expressions).  The RHS is then typechecked.  Finally the types of
;;; the field name are set according to the RHS types.  Note that this
;;; processing is for record-exprs and is not the same as for
;;; update-assignments.

(defun typecheck-rec-assignments (assignments &optional fielddecls)
  (if (null assignments)
      (nreverse fielddecls)
      (let* ((ass (car assignments))
	     (fieldname (caar (arguments ass))))
	(when (maplet? ass)
	  (type-error ass
	    "Record expression assignments may not have maplets"))
	(when (cdr (arguments ass))
	  (type-error ass
	    "Record expression assignments must not have arguments"))
	(unless (name-expr? fieldname)
	  (type-error ass "Record expressions must have named fields"))
	(when (member fieldname fielddecls
		      :test #'(lambda (x y) (same-id x (car y))))
	  (type-error fieldname
	    "Duplicate field assignments are not allowed"))
	(typecheck* (expression ass) nil nil nil)
	(let* ((fdecls (mapcar #'(lambda (ty)
				   (mk-field-decl (id fieldname) ty ty))
			       (ptypes (expression ass))))
	       ;;(*bound-variables* (append fdecls *bound-variables*))
	       )
	  (typecheck-rec-assignments (cdr assignments)
				     (cons fdecls fielddecls))))))


;;; Tuple-expr

(defmethod typecheck* ((expr tuple-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (assert (cdr (exprs expr)))
  (typecheck* (exprs expr) nil nil nil)
  (setf (types expr)
	(if (singleton? (exprs expr))
	    (ptypes (car (exprs expr)))
	    (all-possible-tupletypes (exprs expr))))
  #+pvsdebug
  (assert (every #'(lambda (ty)
		     (every #'(lambda (tt)
				(typep tt '(or type-expr dep-binding)))
			    (types (find-supertype ty))))
		 (types expr))))

(defun all-possible-tupletypes (exprs)
  (mapcar #'mk-tupletype
	  (cartesian-product (mapcar #'ptypes exprs))))

(defun cartesian-product (list-of-lists &optional (result (list nil)))
  (if (null list-of-lists)
      result
      (cartesian-product
       (cdr list-of-lists)
       (mapcan #'(lambda (e)
		   (mapcar #'(lambda (r)
			       (append r (list e)))
			   result))
	       (car list-of-lists)))))


;;; Coercion is now handled by turning the form into an application of
;;; identity.  Thus a:T is changed to id[T](a) in parse.lisp.

(defmethod typecheck* ((expr coercion) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*in-coercion* expr))
    (call-next-method)))

;;; Intype does not exist in PVS

;;; If-expr is now an application.


;;; Cases-expr have the form
;;;   CASES expr OF appl1 : expr1, ... ENDCASES
;;; expr is first typechecked, and the non-adts are removed from its
;;; types.  If this results in a single type, then the selections are
;;; typechecked.  Finally, the types is set by collecting all types
;;; which are compatible to all selections (and the else-part, if
;;; specified).

(defmethod typecheck* ((expr cases-expr) expected kind arguments)
  (declare (ignore expected kind))
  (unless (type (expression expr))
    (typecheck* (expression expr) nil nil nil))
  (let ((atypes	(remove-if-not #'(lambda (ty)
				   (or (cotupletype? (find-supertype ty))
				       (adt? (find-supertype ty))))
		  (ptypes (expression expr)))))
    (unless (singleton? atypes)
      (if atypes
	  (type-ambiguity (expression expr))
	  (type-error (expression expr)
	    "Expression type must be a cotuple or datatype")))
    (setf (types (expression expr)) atypes)
    (let* ((type (car atypes))
	   (stype (find-supertype type)))
      (if (adt? stype)
	  (typecheck-selections expr (adt (find-supertype type))
				(find-declared-adt-supertype type) arguments)
	  (typecheck-coselections expr stype arguments))))
  (setf (types expr)
	(compatible-types
	 (nconc (mapcar #'(lambda (s) (ptypes (expression s)))
		  (selections expr))
		(when (else-part expr) (list (ptypes (else-part expr)))))))
  (unless (types expr)
    (type-error expr "Selections have incompatible types"))
  expr)

(defmethod find-adt-supertype ((te subtype))
  (find-adt-supertype (supertype te)))

(defmethod find-adt-supertype ((te datatype-subtype))
  te)

(defmethod find-adt-supertype (te)
  te)

(defmethod find-declared-adt-supertype ((te subtype))
  (find-declared-adt-supertype (supertype te)))

(defmethod find-declared-adt-supertype ((te datatype-subtype))
  (declared-type te))

(defmethod find-declared-adt-supertype ((te dep-binding))
  (find-declared-adt-supertype (type te)))

(defmethod find-declared-adt-supertype (te)
  te)

(defun compatible-types (list-of-types)
  (compatible-types* (cdr list-of-types) (car list-of-types)))

(defun compatible-types* (list-of-types types)
  (if (null list-of-types)
      (remove-duplicates types :test #'tc-eq)
      (compatible-types* (cdr list-of-types)
			 (compatible-types** (car list-of-types) types))))

(defun compatible-types** (types1 types2)
  (mapcan #'(lambda (t1)
	      (mapcan #'(lambda (t2)
			  (when (compatible? t1 t2)
			    (let ((ty (compatible-type-match t1 t2)))
			      (when ty (list ty)))))
		      types2))
	  types1))


(defun compatible-type-match (t1 t2)
  (if (fully-instantiated? t1)
      (if (fully-instantiated? t2)
	  (compatible-type t1 t2)
	  (let ((type (find-parameter-instantiation t2 t1)))
	    (assert (fully-instantiated? type))
	    (when (and type (compatible? type t1))
	      (compatible-type t1 type))))
      (if (fully-instantiated? t2)
	  (let ((type (find-parameter-instantiation t1 t2)))
	    (assert (fully-instantiated? type))
	    (when (and type (compatible? type t2))
	      (compatible-type t2 type)))
	  (ignore-lisp-errors (compatible-type t1 t2)))))

(defun typecheck-selections (expr adt type args)
  (when (duplicates? (selections expr) :test #'same-id :key #'constructor)
    (type-error expr "Selections must have a unique id"))
  (when (and (length= (selections expr) (constructors adt))
	     (else-part expr))
    (type-error-noconv (else-part expr) "ELSE part will never be evaluated"))
  (typecheck-selections* (selections expr) adt type args)
  (when (else-part expr)
    (typecheck* (else-part expr) nil nil args)))

(defmethod typecheck-coselections (expr (type cotupletype) args)
  (when (duplicates? (selections expr) :test #'same-id :key #'constructor)
    (type-error expr "Selections must have a unique id"))
  (when (and (length= (selections expr) (types type))
	     (else-part expr))
    (type-error (else-part expr) "ELSE part will never be evaluated"))
  (typecheck-coselections* (selections expr) type args)
  (when (else-part expr)
    (typecheck* (else-part expr) nil nil args)))

(defmethod typecheck-coselections* (selections (type cotupletype) args)
  (when selections
    (let* ((sel (car selections))
	   (constr (constructor sel))
	   (n (get-injection-number constr))
	   (in-type (when n (nth (1- n) (types type)))))
      (unless n
	(type-error sel
	  "~a should be of the form IN_i (or in_i) for some integer i."
	  (constructor sel)))
      (unless (<= n (length (types type)))
	(type-error sel "Cotuple type only has ~d components"
		    (length (types type))))
      (unless (= (length (args sel)) 1)
	(type-error sel "Only a single argument is allowed"))
      (setf (declared-type (car (args sel))) in-type)
      (typecheck* (car (args sel)) nil nil nil)
      (let ((ctype (mk-funtype in-type type)))
	(setf (type constr) ctype
	      (types constr) (list ctype)))
      (let* ((*bound-variables* (append (args sel) *bound-variables*)))
	(typecheck* (expression sel) nil nil args)))
    (typecheck-coselections* (cdr selections) type args)))

(defun get-injection-number (name)
  (let ((strid (string (id name))))
    (when (and (> (length strid) 3)
	       (string= "IN_" strid :end2 3)
	       (every #'digit-char-p (subseq strid 3)))
      (parse-integer strid :start 3))))

(defun typecheck-selections* (selections adt type args)
  (when selections
    (let* ((sel (car selections))
	   (constr (constructor sel))
	   (c (car (member (constructor sel) (constructors adt)
			   :test #'same-id))))
      (unless c
	(type-error-noconv sel
	    "No matching constructor found for ~a in datatype ~a"
	    (constructor sel) (id adt)))
      ;;(typecheck* constr nil nil nil)
      (unless (length= (args sel) (arguments c))
	(type-error-noconv sel "Wrong number of arguments"))
      (set-selection-types (args sel) type (arguments c))
      (typecheck* (args sel) nil nil nil)
      (let* ((*bound-variables* (append (args sel) *bound-variables*)))
	(typecheck* constr nil nil (cond ((null (args sel)) nil)
					 ((cdr (args sel)) 
					  (mk-tuple-expr (args sel)))
					 ((car (args sel)))))
	(let ((reses (remove-if-not #'(lambda (r)
					(eq (declaration r) (con-decl c)))
		       (resolutions constr))))
	  (if reses
	      (setf (resolutions constr) reses
		    (types constr) (mapcar #'type reses))
	      (type-error-noconv sel
		  "No matching constructor found for ~a in datatype ~a"
		  (constructor sel) (id adt))))
	(typecheck* (expression sel) nil nil args)))
    (typecheck-selections* (cdr selections) adt type args)))

(defun set-selection-types (selargs type arg-decls)
  (when selargs
    (let* ((rtype (declared-type (car arg-decls)))
	   (atype (if (type-name? rtype)
		      (copy rtype
			'actuals (or (actuals rtype)
				     (actuals (module-instance rtype))))
		      (copy rtype)))
	   (dtype (subst-mod-params atype (module-instance type)
				    (module (declaration type)))))
      (setf (declared-type (car selargs))
	    (if (typep dtype 'datatype-subtype)
		(declared-type dtype)
		dtype))
      (unless (fully-instantiated? (declared-type (car selargs)))
	(type-error (declared-type (car selargs))
	    "Could not determine the full theory instance"))
      (set-selection-types
       (cdr selargs)
       type
       (let ((bd (bind-decl (car arg-decls))))
	 (if (occurs-in bd (cdr arg-decls))
	     (let* ((ntype (typecheck* dtype nil nil nil))
		    (narg (mk-name-expr (id (car selargs))
			    nil nil (make-resolution (car selargs) nil ntype)))
		    (alist (acons bd narg nil)))
	       (mapcar #'(lambda (a)
			   (let ((stype (substit (type a) alist)))
			     (lcopy a
			       'type stype
			       'declared-type (or (print-type stype) stype))))
		       (cdr arg-decls)))
	     (cdr arg-decls)))))))


;;; Table-exprs will be transformed into one of these three, which will
;;; then call the appropriate method.

;(defmethod typecheck* ((expr cond-table-expr) expected kind arguments)
;  (call-next-method))
;
;(defmethod typecheck* ((expr cases-table-expr) expected kind arguments)
;  (call-next-method))
;
;(defmethod typecheck* ((expr let-table-expr) expected kind arguments)
;  (call-next-method))


;;; table-exprs - first typecheck the row and column exprs; these are used
;;; to determine if the table is converted to a cases-expr or a cond-expr.

(defmethod typecheck* ((expr table-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) expr
    (cond (row-expr
	   (typecheck-uniquely row-expr)
	   (unless (adt? (type row-expr))
	     (let ((expected (type row-expr))
		   (*generate-tccs* 'none))
	       (dolist (rh row-headings)
		 (unless (eq rh 'else)
		   (typecheck* rh expected nil nil))))))
	   (t (let ((*generate-tccs* 'none))
		(dolist (rh row-headings)
		 (unless (eq rh 'else)
		   (typecheck* rh *boolean* nil nil))))))
    (cond (col-expr
	   (typecheck-uniquely col-expr)
	   (unless (adt? (type col-expr))
	     (let ((expected (type col-expr))
		   (*generate-tccs* 'none))
	       (dolist (ch col-headings)
		 (unless (eq ch 'else)
		   (typecheck* ch expected nil nil))))))
	   (t (let ((*generate-tccs* 'none))
		(dolist (ch col-headings)
		 (unless (eq ch 'else)
		   (typecheck* ch *boolean* nil nil))))))
    (cond ((null row-headings)
	   ;; 1-dimensional horizontal table
	   (cond ((and col-expr
		       (adt? (type col-expr))
		       (has-selection-syntax? col-headings
					      (adt? (type col-expr))))
		  (make-cases-table-expr expr col-expr col-headings
					 (car table-entries)))
		 (t (make-cond-table-expr expr col-expr col-headings
					  (car table-entries)))))
	  ((null col-headings)
	   ;; 1-dimensional vertical table
	   (cond ((and row-expr
		       (adt? (type row-expr))
		       (has-selection-syntax? row-headings
					      (adt? (type row-expr))))
		  (make-cases-table-expr expr row-expr row-headings
					 (mapcar #'car table-entries)))
		 (t (make-cond-table-expr expr row-expr row-headings
					  (mapcar #'car table-entries)))))
	  (t ;; 2-dimensional table
	   (make-2d-table expr row-expr col-expr row-headings col-headings
			   table-entries)))
    (typecheck* expr nil nil nil)))

(defun make-2d-table (table-expr row-expr col-expr row-headings col-headings
				  table-entries)
  (let ((rows (if (and col-expr
		       (adt? (type col-expr))
		       (has-selection-syntax? col-headings
					      (adt? (type col-expr))))
		  (make-cases-row-exprs
		   col-expr col-headings table-entries)
		  (make-cond-row-exprs
		   col-expr col-headings table-entries))))
    (cond ;;((every #'(lambda (row)
		;;      (every #'(lambda (e) (not (null e)))
			;;     row))
		  ;;table-entries)
	   ;;(make-let-table-expr table-expr row-expr row-headings rows))
	  ((and row-expr
		(adt? (type row-expr))
		(has-selection-syntax? row-headings
				       (adt? (type row-expr))))
	   (make-cases-table-expr table-expr row-expr row-headings rows))
	  (t (make-cond-table-expr table-expr row-expr row-headings rows)))))

(defun make-let-table-expr (table-expr row-expr row-headings rows)
  (change-class table-expr 'let-table-expr)
  (let* ((row-bindings (make-new-row-bindings rows))
	 (row-vars (mapcar #'(lambda (rb)
			       (change-class (copy rb) 'name-expr))
			   row-bindings))
	 (expr (if (and row-expr
			(adt? (type row-expr))
			(has-selection-syntax? row-headings
					       (adt? (type row-expr))))
		   (make-cases-table-expr
		    nil row-expr row-headings row-vars)
		   (make-cond-table-expr
		    nil row-expr row-headings row-vars))))
    (setf (operator table-expr) (mk-lambda-expr row-bindings expr))
    (setf (argument table-expr) (mk-arg-tuple-expr* rows))))

(defun make-new-row-bindings (rows &optional rvars)
  (let ((new-rvars (if rvars
		       (mapcar #'(lambda (rv) (makesym "r~a" rv)) rvars)
		       (let ((i 0))
			 (mapcar #'(lambda (r)
				     (declare (ignore r))
				     (makesym "r~d" (incf i)))
				 rows)))))
    (if (some #'(lambda (rv) (id-occurs-in rv rows)) new-rvars)
	(make-new-row-bindings rows new-rvars)
	(mapcar #'(lambda (r)
		    (make-instance 'untyped-bind-decl
		      :id r))
		new-rvars))))
			       


(defun has-selection-syntax? (headings adt)
  (or (null headings)
      (and (or (eq (car headings) 'else)
	       (and (typep (car headings) 'name-expr)
		    (let ((constr (car (member (car headings)
					       (constructors adt)
					       :test #'same-id))))
		      (and constr
			   (null (arguments constr)))))
	       (and (typep (car headings) 'application)
		    (typep (operator (car headings)) 'name-expr)
		    (every #'(lambda (x) (typep x 'name-expr))
			   (arguments (car headings)))
		    (let ((constr (car (member (operator (car headings))
					       (constructors adt)
					       :test #'same-id))))
		      (and constr
			   (length= (arguments constr)
				    (arguments (car headings)))))))
	   (has-selection-syntax? (cdr headings) adt))))

(defun make-cases-table-expr (table-expr expr headings table-entries)
  (let* ((else? (eq (car (last headings)) 'else))
	 (selections (mapcar #'(lambda (ch te)
				 (when te
				   (if (typep ch 'name-expr)
				       (make-instance 'selection
					 :constructor ch
					 :expression te)
				       (make-instance 'selection
					 :constructor (operator ch)
					 :args (mapcar #'(lambda (a)
							   (change-class
							    (copy a)
							    'bind-decl))
						       (arguments ch))
					 :expression te))))
			     (if else?
				 (butlast headings)
				 headings)
			     (if else?
				 (butlast table-entries)
				 table-entries))))
    (cond (table-expr
	   (change-class table-expr 'cases-table-expr)
	   (setf (expression table-expr) expr)
	   (setf (selections table-expr) selections)
	   (when else?
	     (setf (else-part table-expr) (car (last table-entries))))
	   table-expr)
	  (t (make-instance 'cases-expr
	       :expression expr
	       :selections selections
	       :else-part (when else? (car (last table-entries))))))))

(defun make-cond-table-expr (table-expr expr headings table-entries)
  (let* ((condition (if (and expr
			     (not (typep (car headings) 'else-condition)))
			(mk-application '= expr (car headings))
			(car headings)))
	 (then-part (car table-entries))
	 (else-cond (when (eq (car (last headings)) 'else)
		      (mk-else-condition expr (butlast headings))))
	 (else-part (if else-cond
			(make-cond-table-expr*
			 expr
			 (append (butlast (cdr headings)) (list else-cond))
			 (cdr table-entries))
			(or (make-cond-table-expr* expr
						   (cdr headings)
						   (cdr table-entries))
			    then-part))))
    (cond (table-expr
	   (change-class table-expr 'cond-table-expr)
	   (cond (then-part
		  (setf (operator table-expr) (mk-name-expr 'IF))
		  (setf (argument table-expr)
			(make-instance 'arg-tuple-expr
			  :exprs (list condition then-part else-part))))
		 (t (setf (operator table-expr) (operator else-part))
		    (setf (argument table-expr) (argument else-part))))
	   table-expr)
	  (t (make-instance 'first-cond-expr
	       :operator (mk-name-expr 'IF)
	       :argument (make-instance 'arg-tuple-expr
			   :exprs (list condition then-part else-part)))))))

(defun make-cond-table-expr* (expr headings table-entries)
  (when headings
    (let ((condition (if (and expr
			      (not (typep (car headings) 'else-condition)))
			 (mk-application '= expr (car headings))
			 (car headings)))
	  (then-part (car table-entries))
	  (else-part (make-cond-table-expr* expr
					    (cdr headings)
					    (cdr table-entries))))
      (cond ((and then-part else-part)
	     (make-instance 'cond-expr
	       :operator (mk-name-expr 'IF)
	       :argument (make-instance 'arg-tuple-expr
			   :exprs (list condition
					then-part
					else-part)
			   :place (place condition))))
	    (then-part
	     (make-instance 'last-cond-expr
	       :operator (mk-name-expr 'IF)
	       :argument (make-instance 'arg-tuple-expr
			   :exprs (list condition
					then-part
					then-part)
			   :place (place condition))))
	    (else-part else-part)))))

(defun make-cases-row-exprs (expr headings table-entries &optional result)
  (if (null table-entries)
      (nreverse result)
      (let* ((row (car table-entries))
	     (else? (eq (car (last headings)) 'else))
	     (selections
	      (mapcar #'(lambda (ch te)
			  (if (typep ch 'name-expr)
			      (make-instance 'selection
				:constructor ch
				:expression te)
			      (make-instance 'selection
				:constructor (operator ch)
				:args (arguments ch)
				:expression te)))
		      (if else?
			  (butlast headings)
			  headings)
		      (if else?
			  (butlast row)
			  row))))
	(make-cases-row-exprs
	 expr headings (cdr table-entries)
	 (cons (make-instance 'cases-expr
		 :expression expr
		 :selections selections
		 :else-part (when else?
			      (car (last row))))
	       result)))))

(defun make-cond-row-exprs (expr headings table-entries &optional result)
  (if (null table-entries)
      (nreverse result)
      (let* ((row (car table-entries))
	     (else-cond (when (eq (car (last headings)) 'else)
			  (mk-else-condition expr (butlast headings))))
	     (cond-expr (if else-cond
			    (make-cond-table-expr*
			     expr
			     (append (butlast headings) (list else-cond))
			     row)
			    (make-cond-table-expr* expr headings row))))
	(make-cond-row-exprs
	 expr headings (cdr table-entries)
	 (if cond-expr
	     (cons (if (singleton? (remove-if #'null row))
		       (change-class cond-expr 'single-cond-expr)
		       (change-class cond-expr 'first-cond-expr))
		   result)
	     result)))))

(defun mk-else-condition (expr headings)
  (change-class (mk-negation
		 (mk-disjunction
		  (if expr
		      (mapcar #'(lambda (h) (mk-application '= expr h))
			      headings)
		      headings)))
		'else-condition))

;;; Application - First typecheck* the arguments.  Then typecheck* the
;;; operator with arguments.  Finally the types slot of the expr is set
;;; to the possible return types of the operator.

(defmethod typecheck* ((expr application) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; Can't do operator first - breaks when a field application is involved
  ;;(unless (ptypes (operator expr))
    ;;(typecheck* (operator expr) nil nil nil))
  (unless (ptypes (argument expr))
    (typecheck* (argument-list (argument expr)) nil nil nil))
  ;;(assert (every #'types (argument-list expr)))
  (when (lambda-expr? (operator expr))
    (if (typep expr '(or let-expr where-expr))
	(let ((*generate-tccs* 'none))
	  (typecheck* (argument expr) nil nil nil)
	  (typecheck-let-bindings (bindings (operator expr)) (argument expr)))
	(typecheck* (bindings (operator expr)) nil nil nil)))
  (unless (ptypes (operator expr))
    (typecheck* (operator expr) nil nil (argument-list (argument expr))))
  (set-possible-argument-types (operator expr) (argument expr))
  (unless (or (type (operator expr))
	      (typep (operator expr) 'name-expr))
    (let ((optypes (delete-if-not #'(lambda (opty)
				      (let ((stype (find-supertype opty)))
					(and (typep stype 'funtype)
					     (some #'(lambda (aty)
						       (compatible? (domain stype)
								    aty))
						   (ptypes (argument expr))))))
		     (types (operator expr)))))
      (if optypes
	  (setf (types (operator expr)) optypes)
	  (find-application-conversion expr))))
  (unless (type (argument expr))
    (let ((argtypes (delete-if-not
			#'(lambda (aty)
			    (some #'(lambda (oty)
				      (let ((sty (find-supertype oty)))
					(and (typep sty 'funtype)
					     (compatible? aty (domain sty)))))
				  (ptypes (operator expr))))
		      (types (argument expr)))))
      (if argtypes
	  ;; No conversion will be needed in this case
	  (setf (types (argument expr)) argtypes)
	  (when (and (typep (operator expr) 'name-expr)
		     (some #'(lambda (r)
			       (typep r 'lambda-conversion-resolution))
			   (resolutions (operator expr))))
	    (change-application-to-conversion expr)))))
  (unless (typep expr 'lambda-conversion)
    (let ((rtypes (application-range-types expr)))
      (cond (rtypes
	     (setf (types expr) rtypes))
	    ((and (not (type expr))
		  (typep (operator expr) 'name)
		  (some #'(lambda (r)
			    (typep r 'lambda-conversion-resolution))
			(resolutions (operator expr))))
	     (change-application-to-conversion expr))
	    (t (type-mismatch-error expr)))))
  #+pvsdebug (assert (every #'(lambda (ty)
				(let ((oty (find-supertype ty)))
				  (and (funtype? oty)
				       (some #'(lambda (ety)
						 (compatible? (range oty) ety))
					     (ptypes expr)))))
			    (ptypes (operator expr))))
  expr)

(defun set-possible-argument-types (op arg)
  (unless (ptypes arg)
    (set-possible-argument-types* (ptypes op) arg)
    (unless (ptypes arg)
      (setf (types arg)
	    (all-possible-tupletypes (exprs arg))))))

(defun set-possible-argument-types* (optypes arg &optional result)
  (if (null optypes)
      (setf (types arg) result)
      (let ((argtypes (get-possible-argument-types (car optypes) arg)))
	(set-possible-argument-types*
	 (cdr optypes) arg (nconc result argtypes)))))

(defmethod get-possible-argument-types (optype arg)
  (declare (ignore optype))
  (ptypes arg))
       
(defmethod get-possible-argument-types (optype (arg tuple-expr))
  (cond ((funtype? (find-supertype optype))
	 (let ((dtypes (domain-types optype)))
	   (if (length= dtypes (exprs arg))
	       (let ((atypes (mapcar #'(lambda (dty a)
					 (remove-if-not
					     #'(lambda (aty)
						 (compatible? aty dty))
					   (ptypes a)))
			       dtypes (exprs arg))))
		 (mapcar #'mk-tupletype
		   (cartesian-product atypes)))
	       (if (null (cdr dtypes))
		   (let ((stype (find-supertype (car dtypes))))
		     (if (and (typep stype 'tupletype)
			      (length= (types stype) (exprs arg)))
			 (let ((atypes (mapcar
					   #'(lambda (dty a)
					       (remove-if-not
						   #'(lambda (aty)
						       (compatible? aty dty))
						 (ptypes a)))
					 (types stype) (exprs arg))))
			   (mapcar #'mk-tupletype
			     (cartesian-product atypes)))
			 ;; This is possible only if there is a type mismatch;
			 ;; but we let this go to allow for conversions 
			 nil))
		   nil))))))


;;; Application-range-types takes an application and returns the list of
;;; possible types of that application.  In the simple cases, this is just
;;; the range of the possible types of the operator.  However,
;;; dependencies ruin this utopia.

(defmethod application-range-types ((expr application))
  (with-slots (operator argument) expr
    (let* ((op-types (or (types operator) (list (type operator))))
	   (arg-types (or (types argument) (list (type argument))))
	   (rtypes (application-range-types-op
		    op-types arg-types operator argument nil)))
      rtypes)))

(defun application-range-types-op (op-types arg-types op arg result)
  (if (null op-types)
      (delete-duplicates result :test #'tc-eq)
      (let ((rtypes (application-range-types-args
		     arg-types (car op-types) op arg nil)))
	(application-range-types-op (cdr op-types) arg-types op arg
				    (nconc rtypes result)))))

(defun application-range-types-args (arg-types op-type op arg result)
  (if (null arg-types)
      result
      (let ((rtype (application-range-type-arg arg op-type (car arg-types))))
	(application-range-types-args (cdr arg-types) op-type op arg
				      (if rtype
					  (cons rtype result)
					  result)))))

;;; This can come about through conversions
(defmethod application-range-type-arg (arg optype argtype)
  (declare (ignore arg optype argtype))
  nil)

(defmethod application-range-type-arg (arg (optype subtype) argtype)
  (with-slots (supertype) optype
    (application-range-type-arg arg supertype argtype)))

(defmethod application-range-type-arg (arg (optype funtype) argtype)
  (with-slots (domain range) optype
    (application-range-type-arg* arg domain range argtype)))

(defmethod application-range-type-arg* (arg (domain dep-binding) range argtype)
  (cond ((null (freevars range))
	 range)
	((type arg)
	 (substit range (acons domain arg nil)))
	(t (let ((atype (application-range-instantiated-argtype
			 (type domain) argtype)))
	     (when atype
	       (let* ((*generate-tccs* 'none)
		      (narg (with-no-type-errors
			     (typecheck* (copy-untyped arg) atype nil nil))))
		 (when (and narg
			    (compatible? (type narg) (type domain)))
		   #+pvsdebug (assert (fully-typed? narg))
		   (substit range (acons domain narg nil)))))))))

(defun application-range-instantiated-argtype (domain argtype)
  (if (fully-instantiated? argtype)
      argtype
      (find-parameter-instantiation argtype domain)))

(defmethod find-supertype-without-freevars ((type type-name))
  type)

(defmethod find-supertype-without-freevars ((type funtype))
  (if (freevars type)
      (mk-funtype (find-supertype-without-freevars (domain type))
		  (find-supertype-without-freevars (range type)))
      type))

(defmethod find-supertype-without-freevars ((type dep-binding))
  (find-supertype-without-freevars (type type)))

(defmethod find-supertype-without-freevars ((type subtype))
  (find-supertype-without-freevars (supertype type)))

(defmethod find-supertype-without-freevars ((type tupletype))
  (if (freevars type)
      (mk-tupletype (mapcar #'find-supertype-without-freevars (types type)))
      type))

(defmethod find-supertype-without-freevars ((type cotupletype))
  (if (freevars type)
      (mk-cotupletype (mapcar #'find-supertype-without-freevars (types type)))
      type))

(defmethod find-supertype-without-freevars ((type recordtype))
  (if (freevars type)
      (mk-recordtype (mapcar #'find-supertype-without-freevars (fields type))
		     nil)
      type))

(defmethod find-supertype-without-freevars ((fld field-decl))
  (if (freevars (type fld))
      (mk-field-decl (id fld) (find-supertype-without-freevars (type fld)))
      fld))


(defmethod application-range-type-arg* (arg domain range argtype)
  (declare (ignore arg))
  (if (or (fully-instantiated? range)
	  (not (fully-instantiated? argtype)))
      range
      (let ((theories (delete (current-theory)
			      (delete-duplicates (mapcar #'module
						   (free-params range)))))
	    (srange range))
	(dolist (th theories)
	  (let ((bindings (tc-match argtype domain
				    (mapcar #'(lambda (x) (cons x nil))
				      (formals-sans-usings th)))))
	    (when (every #'cdr bindings)
	      (setq srange
		    (subst-mod-params
		     srange
		     (mk-modname (id th)
		       (mapcar #'(lambda (a) (mk-res-actual (cdr a) th))
			 bindings))
		     th)))))
	srange)))

(defmethod application-range-type (arg (optype subtype))
  (with-slots (supertype) optype
    (application-range-type arg supertype)))

(defmethod application-range-type (arg (optype funtype))
  (with-slots (domain range) optype
    (application-range-type* arg domain range)))

(defmethod application-range-type* (arg (domain dep-binding) range)
  (substit range (acons domain arg nil)))

(defmethod application-range-type* (arg domain range)
  (declare (ignore arg domain))
  range)

(defmethod type-mismatch-error (expr)
  (let ((exprstr (unpindent expr 4 :string t)))
    (type-error expr
	 "Type mismatch in application~
          ~%    ~a~2%  Operator types: ~{~a~%~^~12T~}  Argument types: ~a"
	 exprstr
	 (ptypes (operator expr))
	 (mapcar #'(lambda (arg)
		     (car (ptypes arg)))
	   (arguments expr)))))

(defmethod type-mismatch-error ((expr coercion))
  (let ((exprstr (unpindent expr 4 :string t)))
    (type-error expr
      "Type mismatch in coercion~
          ~%    ~a~2%  Possible expression types: ~{~a~%~^~12T~}"
      exprstr
      (mapcar #'(lambda (arg)
		  (car (ptypes arg)))
	(arguments expr)))))

(defmethod type-mismatch-error ((expr let-expr))
  (if (lambda-expr? (operator expr))
      (let ((exprstr (unpindent expr 4 :string t)))
	(type-error expr
	  "Type mismatch in LET bindings~
           ~%    ~a~2%  Binding types: ~{~a~%~^~12T~}  Argument types: ~a"
	  exprstr
	  (mapcar #'type (bindings (operator expr)))
	  (mapcar #'(lambda (arg)
		      (car (ptypes arg)))
	    (arguments expr))))
      (call-next-method)))


;;; LET and WHERE expressions are handled specially wrt bindings without
;;; a declared-type.  The normal lambda expr looks for a global variable
;;; declaration of the same name.  In the LET (and WHERE) expression,
;;; the type of the binding is determined from the types of the binding
;;; expression, if it is uniquely determined.

(defun typecheck-let-bindings (bindings arg)
  (when (cdr bindings)
    (let ((atypes (remove-if-not
			#'(lambda (ty)
			    (let ((sty (find-supertype ty)))
			      (and (typep sty 'tupletype)
				   (length= bindings (types sty)))))
		      (types arg))))
      (if atypes
	  (setf (types arg) atypes)
	  (type-error arg "Wrong arity for ~d bindings" (length bindings)))))
  (typecheck-let-bindings* bindings arg 0))

(defun typecheck-let-bindings* (bindings arg anum &optional substs)
  (when bindings
    (let* ((bd (car bindings))
	   (dtype (get-let-binding-type bd bindings arg anum))
	   (type (substit (if (typep dtype 'dep-binding)
			      (type dtype)
			      dtype)
		   substs)))
      (setf (type bd) type)
      (unless (fully-instantiated? type)
	(type-error (car bindings)
	  "Could not determine the full theory instance"))
      (typecheck-let-bindings* (cdr bindings) arg (1+ anum)
			       (if (typep dtype 'dep-binding)
				   (acons dtype bd substs)
				   substs)))))

(defun get-let-binding-type (bd bindings arg anum)
  (if (declared-type bd)
      (prog1 (typecheck* (declared-type bd) nil nil nil)
	     (set-type (declared-type bd) nil))
      (let ((vdecl (find-if #'(lambda (v)
				(and (var-decl? v)
				     (eq (module v) (current-theory))))
		     (get-declarations (id bd)))))
	(cond ((and vdecl
		    (some #'(lambda (ty)
			      (compatible? (type vdecl) ty))
			  (types arg)))
	       (pvs-info "LET/WHERE variable ~a~@[~a~] is given ~
                          type~%  ~a from a preceding variable declaration."
		 (id bd)
		 (when (place bd)
		   (format nil " at line ~d, col ~d"
		     (starting-row (place bd)) (starting-col (place bd))))
		 (type vdecl))
	       (type vdecl))
	      (t (let ((type (get-let-binding-type-from-arg
			      bindings arg anum)))
		   (pvs-info "LET/WHERE variable ~a~@[~a~] is ~
                              given type~%  ~a from its value expression."
		     (id bd)
		     (when (place bd)
		       (format nil " at line ~d, col ~d"
			 (starting-row (place bd)) (starting-col (place bd))))
		     type)
		   type))))))
		    

(defun get-let-binding-type-from-arg (bindings arg anum)
  (if (or (cdr bindings)
	  (> anum 0))
      (let ((atypes (remove-duplicates
			(mapcar #'(lambda (aty)
				    (nth anum (types (find-supertype aty))))
			  (types arg))
		      :test #'tc-eq)))
	(if (cdr atypes)
	    (if (typep arg 'tuple-expr)
		(type-ambiguity (nth anum (exprs arg)))
		(type-ambiguity arg))
	    (car atypes)))
      (if (cdr (types arg))
	  (type-ambiguity arg)
	  (if (fully-instantiated? (car (types arg)))
	      (let ((carg (typecheck* (copy-untyped arg)
				      (car (types arg)) nil nil)))
		(car (judgement-types+ carg)))
	      (car (types arg))))))

(defun set-dep-projections (projections types)
  (when projections
    (setf (type (car projections))
	  (if (dep-binding? (car types))
	      (type (car types))
	      (car types)))
    (set-dep-projections
     (cdr projections)
     (if (dep-binding? (car types))
	 (substit (cdr types)
	   (list (cons (mk-name-expr (id (car types))
			 nil nil
			 (make-resolution (car types)
			   (theory-name *current-context*)))
		       (car projections))))
	 (cdr types)))))
    

(defun subst-range-type (rtype dtypes args)
  (if (freevars rtype)
      (flet ((test (e d) (and (dep-binding? d) (same-id e d))))
	(gensubst rtype
	  #'(lambda (ex)
	      (let* ((pos (position ex dtypes :test #'test))
		     (arg (copy-untyped (nth pos args)))
		     (*generate-tccs* 'none))
		(set-type arg (nth pos dtypes))
		arg))
	  #'(lambda (ex)
	      (and (typep ex '(and name (not binding) (not modname)))
		   (member ex dtypes :test #'test)))))
      rtype))


;;; This function implements the rule
;;;
;;;  C  |-  t: [ x_{1}:T1, ..., x_{n}:Tn ]
;;;  ----------------------------------------------------------
;;;  C  |-  proj_i(t): Ti[proj_1(t)/x_{1},...,proj_i-1(t)/x_{i-1}]
;;;
;;; The mapping is:
;;;   tup	- t
;;;   tuptype	- [ x_{1}:T1, ..., x_{n}:Tn ]
;;;   proj	- proj_i
;;;
;;; The function recurses over the projections of the tuple type
;;; performing the substitutions until the given projection is reached,
;;; at which point the resulting type is returned.  Since we are working
;;; in a type, the type of tup must be unique.

(defun subst-proj-applications (projname tup tuptype)
  (unless (singleton? (ptypes tup))
    (type-ambiguity tup))
  ;;(set-type tup (car (ptypes tup)) *current-context*)
  (subst-proj-applications* projname tup (tup-accessors tuptype)
			    (types tuptype) (types tuptype)))

(defun subst-proj-applications* (projname tup projs types substtypes)
  (if (same-id projname (car projs))
      (car substtypes)
      (subst-proj-applications*
       projname tup (cdr projs)
       (cdr types)
       (if (dep-binding? (car types))
	   (substit (cdr substtypes)
	     (list (cons (car types)
			 (make-proj-application (car projs) tup (car types)))))
	   (cdr substtypes)))))

(defun make-proj-application (proj tup type)
  (declare (ignore type))
  (typecheck (mk-application proj tup)))


;;; Binding-expr -

(defmethod typecheck* ((expr binding-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (bindings expr) nil nil nil)
  ;; XXX do something with arguments here
  (let ((*bound-variables* (append (bindings expr) *bound-variables*)))
    (typecheck* (expression expr) nil nil nil)))

(defmethod typecheck* ((expr lambda-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (bindings expr) nil nil nil)
  ;; XXX do something with arguments here
  (let ((*bound-variables* (append (bindings expr) *bound-variables*)))
    (typecheck* (expression expr) nil nil nil)
    (setf (types expr)
	  (mapcar #'(lambda (ty)
		      ;;(assert (fully-typed? (bindings expr)))
		      (make-formals-funtype (list (bindings expr)) ty))
	    (ptypes (expression expr))))))

(defmethod typecheck* ((expr set-list-expr) expected kind arguments)
  (declare (ignore expected kind arguments))
  (cond ((exprs expr)
	 (typecheck* (exprs expr) nil nil nil)
	 (let ((types (get-possible-set-list-types (exprs expr))))
	   (unless types
	     (type-error expr
	       "Could not find a compatible type for ~a" expr))
	   (setf (types expr)
		 (mapcar #'(lambda (ty) (mk-funtype ty *boolean*)) types))))
	(t (let ((tvar (make-instance 'type-variable
			 :id (make-new-variable 'T expr))))
	     (setf (types expr) (list tvar))))))

(defun get-possible-set-list-types (exprs)
  (get-possible-set-list-types*
   (ptypes (car exprs)) (cdr exprs)))

(defun get-possible-set-list-types* (types exprs &optional ptypes)
  (if (null types)
      (nreverse ptypes)
      (let ((type (get-possible-set-list-types** (car types) exprs)))
	(get-possible-set-list-types*
	 (cdr types) exprs
	 (if type
	     (cons type ptypes)
	     ptypes)))))

(defun get-possible-set-list-types** (type exprs)
  (if (null exprs)
      type
      (let ((compats (remove-if (complement #'(lambda (ty)
						(strict-compatible? ty type)))
		       (ptypes (car exprs)))))
	(when compats
	  (get-possible-set-list-types**
	   (reduce #'compatible-type compats :initial-value type)
	   (cdr exprs))))))
  

;;; Quant-expr -

(defmethod typecheck* ((expr quant-expr) expected kind arguments)
  (declare (ignore expected kind))
  (when arguments
    (type-error expr
      "Quantified expressions may not be used as functions"))
  (call-next-method expr nil nil nil)
  (setf (types expr) (list *boolean*)))


;;; Update-expr -

(defmethod typecheck* ((expr update-expr) expected kind arguments)
  (declare (ignore expected kind))
  (typecheck* (expression expr) nil nil arguments)
  (unless (type (expression expr))
    (setf (types (expression expr))
	  (remove-if
	      (complement
	       #'(lambda (ty)
		   (let ((sty (find-supertype ty)))
		     (and (not (from-conversion sty))
			  (typep sty '(or funtype tupletype recordtype
					  adt-type-name struct-subtype))))))
	    (ptypes (expression expr)))))
  (when (and (cdr (ptypes (expression expr)))
	     (name-expr? (expression expr)))
    (let ((res (find-best-name-resolution
		(expression expr) (resolutions (expression expr)) nil)))
      (setf (resolutions (expression expr)) (list res)
	    (types (expression expr)) (list (type res)))))
  ;; The following may be relaxed in the future.
  (unless (singleton? (ptypes (expression expr)))
    (if (cdr (types (expression expr)))
	;; FIXME - see if the assignments can determine the expression type
	;; before complaining about an ambiguity.
	(type-ambiguity (expression expr))
	(type-error (expression expr)
	  "Must resolve to a record, tuple, function, array, or datatype.")))
  (let ((etype (find-declared-adt-supertype (car (ptypes (expression expr))))))
    (typecheck-assignments (assignments expr) etype)
    (setf (types expr) (update-expr-types expr))))

(defun update-expr-types (expr)
  (let ((*generate-tccs* 'none))
    (if (some #'maplet? (assignments expr))
	(let ((*dont-worry-about-full-instantiations* t)
	      (*generate-tccs* 'none))
	  (find-update-commontypes expr))
	(find-update-commontypes expr))))

(defun find-update-commontypes (expr)
  (assert (singleton? (ptypes (expression expr))))
  (mapcar #'(lambda (ptype)
	      (find-update-commontype
	       ptype (expression expr) (assignments expr)))
    (ptypes (expression expr))))

(defun find-update-commontype (te expr assignments)
  (if (null assignments)
      te
      (let* ((assn (car assignments))
	     (args (arguments assn))
	     (value (expression assn)))
	(find-update-commontype
	 (find-update-commontype* te expr args value (maplet? assn))
	 expr
	 (cdr assignments)))))

(defmethod find-update-commontype* ((te funtype) expr args value maplet?)
  (if (null args)
      (call-next-method)
      (let* ((dtype (dep-binding-type (domain te)))
	     (dom (if (and maplet? (null (cdr args)))
		      (extend-domain-types (car args) dtype expr)
		      dtype)))
	(if (or (not (dep-binding? (domain te)))
		(tc-eq dom dtype))
	    (let ((ran (find-update-commontype*
			(range te)
			(typecheck* (copy-untyped
				     (mk-application* expr (car args)))
				    (range te) nil nil)
			(cdr args) value maplet?)))
	      (if (and (tc-eq dom dtype) (tc-eq ran (range te)))
		  te
		  (mk-funtype (if (tc-eq dom dtype) (domain te) dom) ran)))
	    ;; We have a dep-binding, and the domain type has changed.
	    ;; Note that the range may be lifted, and no longer have the
	    ;; dependency.  This normally happens unless the value is
	    ;; already in the range type, or the bound variable occurs
	    ;; in a domain type.
	    (let* ((*bound-variables* (cons (domain te) *bound-variables*))
		   (ran (find-update-commontype*
			 (range te)
			 (typecheck* (copy-untyped
				      (mk-application* expr (car args)))
				     (range te) nil nil)
			 (cdr args) value maplet?)))
	      (if (member (domain te) (freevars ran) :key #'declaration)
		  (let ((ndep (mk-dep-binding (id (domain te)) dom)))
		    (mk-funtype ndep
				(substit ran (acons (domain te) ndep nil))))
		  (mk-funtype dom ran)))))))

(defmethod find-update-commontype* ((te tupletype) expr args value maplet?)
  (if (null args)
      (call-next-method)
      (find-update-common-tupletype te expr args value maplet?)))

(defmethod find-update-commontype* ((te struct-sub-tupletype)
				    expr args value maplet?)
  (if (null args)
      (call-next-method)
      (find-update-common-tupletype te expr args value maplet?)))

(defun find-update-common-tupletype (te expr args value maplet?)
  (let* ((index (number (caar args)))
	 (dtype (nth (1- index) (types te)))
	 (mtype (unless dtype
		  ;; In a maplet - (cdr args) is nil, value has unique type
		  ;; and index = (1+ (length (type te)))
		  (car (ptypes value))))
	 (ptype (if (dep-binding? dtype) (type dtype) dtype))
	 (pappl (when ptype
		  (make-instance 'projappl
		    :id (makesym "PROJ_~d" index)
		    :index index
		    :argument expr)))
	 (tpappl (when ptype
		   (typecheck* (copy-untyped pappl) ptype nil nil)))
	 (ttype (when ptype
		  (find-update-commontype* ptype tpappl (cdr args)
					   value maplet?)))
	 (rtypes (if (some #'dep-binding? (types te))
		     (let ((texpr (typecheck* (copy-untyped expr)
					      te nil nil)))
		       (subst-tuptypes te texpr))
		     (types te)))
	 (stypes (if (or (null dtype)
			 (tc-eq ttype ptype))
		     rtypes
		     (let ((ctypes (copy-list rtypes)))
		       (setf (nth (1- index) ctypes) ttype)
		       ctypes)))
	 (etypes (if mtype
		     (append stypes (list mtype))
		     stypes)))
    (assert (every #'(lambda (ty) (not (dep-binding? ty))) etypes))
    (if (eq etypes (types te))
	te
	(mk-tupletype etypes))))

(defmethod find-update-commontype* ((te recordtype) expr (args cons)
				    value maplet?)
  (find-update-common-recordtype te expr args value maplet?))

(defmethod find-update-commontype* ((te struct-sub-recordtype) expr (args cons)
				    value maplet?)
  (find-update-common-recordtype te expr args value maplet?))

(defun find-update-common-recordtype (te expr args value maplet?)
  (let* ((sfields (subst-fields te
				(typecheck* (copy-untyped expr) te nil nil)))
	 (fdecl (find (caar args) sfields :test #'same-id))
	 (mdecl (unless fdecl
		  ;; If we don't find fdecl, we are in a maplet
		  (mk-field-decl (id (caar args))
				 (car (ptypes value))
				 (car (ptypes value)))))
	 (fappl (when fdecl
		  (make-instance 'fieldappl
		    :id (id fdecl)
		    :argument expr)))
	 (tfappl (when fdecl
		   (typecheck* (copy-untyped fappl) (type fdecl) nil nil)))
	 (ftype (when fdecl
		  (find-update-commontype*
		   (type fdecl) tfappl (cdr args) value maplet?)))
	 (nfdecl (when (and fdecl
			    (not (tc-eq ftype (type fdecl))))
		   (copy fdecl
		     'type ftype
		     'declared-type ftype)))
	 (sfields (if nfdecl
		      (substit sfields (acons fdecl nfdecl nil))
		      sfields))
	 (efields (if mdecl
		      (append sfields (list mdecl))
		      sfields)))
    (assert (every #'(lambda (fd)
		       (every #'(lambda (fv)
				  (or (not (field-decl? (declaration fv)))
				      (break)))
			      (freevars (type fd))))
		   efields))
    (if (equal efields (fields te))
	te
	(copy te
	  'fields efields
	  'dependent? nil
	  'print-type nil))))

(defmethod find-update-commontype* ((te datatype-subtype) expr (args cons)
				    value maplet?)
  (let* ((acc (caar args))
	 (cappl (typecheck* (mk-application (id acc) (copy-untyped expr))
			    (range (type acc)) nil nil))
	 (ctype (find-update-commontype* (type cappl) cappl (cdr args)
					 value maplet?)))
    (if (tc-eq ctype (range (type acc)))
	te
	(let* ((pdecls (mapcar #'declaration (positive-types (adt te))))
	       (cmatch (tc-match ctype (range (type (declaration acc)))
				 (mapcar #'list pdecls))))
	  (if (some #'cdr cmatch)
	      (let* ((nacts (mapcar #'(lambda (act fml)
					(if (cdr (assq fml cmatch))
					    (mk-actual (cdr (assq fml cmatch)))
					    act))
			      (actuals te) (formals (adt-theory (adt te)))))
		     (nte (typecheck* (copy (declared-type te) 'actuals nacts)
				      nil nil nil)))
		nte)
	      te)))))

(defmethod find-update-commontype* ((te adt-type-name) expr (args cons)
				    value maplet?)
  (declare (ignore expr value))
  te)

(defmethod find-update-commontype* ((te subtype) expr (args cons) value maplet?)
  (find-update-commontype* (supertype te) expr args value maplet?))

(defmethod find-update-commontype* ((te type-expr) expr (args null)
				    value maplet?)
  (declare (ignore expr))
  (let ((tvalue (typecheck* (copy-untyped value) te nil nil)))
    (assert (and tvalue (type tvalue)))
    (reduce #'compatible-type (cons te (judgement-types+ tvalue)))))


(defmethod update-expr-type (assignments expr (te tupletype))
  (let ((type (update-expr-type-types assignments expr
				      (copy-list (types te)))))
    (if (some #'null (types type))
	(let* ((pos (position nil (types type)))
	       (ass (find-if #'(lambda (a)
				 (> (number (caar (arguments a))) pos))
		      assignments)))
	  (type-error ass
	    "Need to include an assignment for ~d along with the assignment ~a"
	    (1+ pos) ass))
	type)))

(defun update-expr-type-types (assignments expr types)
  (if (null assignments)
      (mk-tupletype types)
      (let* ((assign (car assignments))
	     (index (number (caar (arguments assign)))))
	(if (typep assign 'maplet)
	    (let* ((dep (when (cdr (arguments assign))
			  (nth (1- index) types)))
		   (type (when dep
			   (if (dep-binding? dep) (type dep) dep))))
	      (if type
		  (let* ((ntype (update-expr-type-for-maplet
				 (cdr (arguments assign))
				 (expression assign)
				 (make-projection-application index expr)
				 type))
			 (ndep (unless (eq ntype type)
				 (if (dep-binding? dep)
				     (mk-dep-binding
				      (id dep)
				      ntype
				      (or (print-type ntype) ntype))
				     ntype))))
		    (update-expr-type-types
		     (cdr assignments) expr
		     (if ndep (substitute ndep type types) types)))
		  (if (cdr (types (expression assign)))
		      (type-ambiguity (expression assign))
		      (let ((etype (car (types (expression assign)))))
			(update-expr-type-types
			 (cdr assignments) expr
			 (cond ((>= (length types) index)
				(setf (nth (1- index) types) etype)
				types)
			       (t (append types
					  (make-list (- index (length types) 1))
					  (list etype)))))))))
	    (update-expr-type-types (cdr assignments) expr types)))))

(defmethod update-expr-type (assignments expr (te recordtype))
  (update-expr-type-fields assignments expr (fields te)))

(defun update-expr-type-fields (assignments expr fields)
  (if (null assignments)
      (mk-recordtype fields (dependent-fields? fields))
      (let ((assign (car assignments)))
	(if (typep assign 'maplet)
	    (let ((fld (when (cdr (arguments assign))
			 (car (member (caar (arguments assign)) fields
				      :test #'same-id)))))
	      (if fld
		  (let* ((ntype (update-expr-type-for-maplet
				 (cdr (arguments assign))
				 (expression assign)
				 (make-field-application fld expr)
				 (type fld)))
			 (nfld (unless (eq ntype (type fld))
				 (copy fld
				   'type ntype
				   'declared-type (or (print-type ntype)
						      ntype)))))
		    (update-expr-type-fields
		     (cdr assignments) expr
		     (if nfld (substitute nfld fld fields) fields)))
		  (if (cdr (types (expression assign)))
		      (type-ambiguity (expression assign))
		      (let ((etype (car (types (expression assign)))))
			(update-expr-type-fields
			 (cdr assignments) expr
			 (cons (mk-field-decl
				(id (caar (arguments assign)))
				(or (print-type etype) etype)
				etype)
			       (remove (id (caar (arguments assign)))
				       fields :key #'id)))))))
	    (update-expr-type-fields (cdr assignments) expr fields)))))

(defmethod update-expr-type (assignments expr (te funtype))
  ;;; Note that te may not be fully instantiated
  (let ((dom (if (dep-binding? (domain te))
		 (type (domain te))
		 (domain te))))
    (if (or (subtype? dom)
	    (and (tupletype? dom)
		 (some #'subtype? (types dom))))
	(update-expr-type-funtype assignments expr te)
	te)))

(defun update-expr-type-funtype (assignments expr funtype)
  (if (null assignments)
      funtype
      (let ((assign (car assignments)))
	(update-expr-type-funtype
	 (cdr assignments)
	 expr
	 (if (typep assign 'maplet)
	     (update-expr-type-for-maplet
	      (arguments assign) (expression assign) expr funtype)
	     funtype)))))

(defmethod update-expr-type-for-maplet ((arguments null) value expr te)
  (unless (some #'(lambda (ty) (compatible? ty te))
		(ptypes value))
    (type-incompatible value (ptypes value) te))
  (extend-domain-type value te expr))

(defmethod update-expr-type-for-maplet (arguments value expr (te recordtype))
  (cond ((member (caar arguments) (fields te) :test #'same-id)
	 (let* ((fld (find (caar arguments) (fields te) :test #'same-id))
		(fty (type fld))
		(nexpr (make-field-application fld expr))
		(ty (update-expr-type-for-maplet
		     (cdr arguments) value nexpr fty)))
	   (if (eq ty fty)
	       te
	       (let ((nfld (mk-field-decl (id fld) ty ty)))
		 (lcopy te
		   'print-type nil
		   'fields (substitute nfld fld (fields te)))))))
	(t (type-error (caar arguments)
	      "Field ~a not found in ~a~
               ~%  May not use nested arguments in extending records"
	      (id (caar arguments)) te))))

(defmethod update-expr-type-for-maplet (arguments value expr (te tupletype))
  (let ((types (types te))
	(index (number (caar arguments))))
    (cond ((<= index (length types))
	   (let* ((tty (nth (1- index) types))
		  (nexpr (make-projection-application index expr))
		  (ty (update-expr-type-for-maplet
		       (cdr arguments) value nexpr tty)))
	     (if (eq ty tty)
		 te
		 (lcopy te
		   'print-type nil
		   'types (substitute ty tty types)))))
	  (t (type-error (caar arguments)
	       "Index ~a out of range in ~a~
                ~%  May not use nested arguments in extending tuples"
	       (id (caar arguments)) te)))))

(defmethod update-expr-type-for-maplet (arguments value expr (te funtype))
  (let* ((dtype (extend-domain-types (car arguments) (domain te) expr))
	 (arg (mk-arg-tuple-expr* (car arguments)))
	 (rtype (update-expr-type-for-maplet
		 (cdr arguments) value
		 (make-application expr arg)
		 (if (or (eq dtype (domain te))
			 (not (typep (domain te) 'dep-binding)))
		     (range te)
		     (substit (range te) (acons (domain te) arg nil))))))
    (if (and (eq dtype (domain te))
	     (eq rtype (range te)))
	te
	(mk-funtype dtype rtype))))

(defmethod extend-domain-types (args (te tupletype) expr)
  (if (cdr args)
      (if (some #'dep-binding? (types te))
	  (extend-domain-type (mk-arg-tuple-expr* args) te expr)
	  (extend-domain-types* args (types te) expr))
      (extend-domain-type (car args) te expr)))

(defmethod extend-domain-types (args (te dep-binding) expr)
  (extend-domain-types args (type te) expr))

(defmethod extend-domain-types (args te expr)
  (if (cdr args)
      (let ((targ (mk-arg-tuple-expr* args)))
	(setf (types targ) (all-possible-tupletypes args))
	(extend-domain-type targ te expr))
      (extend-domain-type (car args) te expr)))

(defun extend-domain-types* (args types expr &optional ntypes)
  (if (null args)
      (mk-tupletype (nreverse ntypes))
      (let* ((type (if (typep (car types) 'dep-binding)
		       (type (car types))
		       (car types)))
	     (ntype (extend-domain-type (car args) type expr))
	     (dtype (if (typep (car types) 'dep-binding)
			(if (eq type ntype)
			    (car types)
			    (mk-dep-binding (id type) ntype))
			ntype)))
	(extend-domain-types*
	 (cdr args)
	 (if (and (not (eq (car types) dtype))
		  (typep (car types) 'dep-binding))
	     (substit (cdr types) (acons (car types) dtype nil))
	     (cdr types))
	 expr
	 (cons dtype ntypes)))))

(defmethod extend-domain-type (arg (type subtype) expr)
  (let ((stype (find-supertype type))
	(new-arg nil))
    (flet ((new-arg ()
	     (or new-arg
		 (setq new-arg
		       (let ((*generate-tccs* 'none))       
			 (typecheck* (copy-untyped arg) stype nil nil))))))
      (if (or (some #'(lambda (ty) (subtype-of? ty type)) (ptypes arg))
	      (some #'(lambda (jty) (subtype-of? jty type))
		    (judgement-types+ (new-arg))))
	  type
	  (let* ((carg (new-arg))
		 (stype (least-compatible-arg-judgement-type type carg))
		 (pred (subtype-pred type stype))
		 (vid (make-new-variable '|x| expr))
		 (vb (make-bind-decl vid stype))
		 (var (make-variable-expr vb))
		 (upred (make!-lambda-expr (list vb)
			  (make!-disjunction
			   (make!-application pred var)
			   (make!-equation var carg))))
		 (tpred (beta-reduce upred)))
	    (mk-subtype stype tpred))))))

(defun least-compatible-arg-judgement-type (type arg)
  (let ((jtypes (judgement-types+ arg)))
    (if (some #'(lambda (jty) (subtype-of? jty type)) jtypes)
	type
	(let ((jtype (find-if #'(lambda (jty) (subtype-of? type jty)) jtypes)))
	  (or jtype
	      (compatible-type type (car jtypes)))))))

(defmethod extend-domain-type (arg (type dep-binding) expr)
  (let ((ntype (extend-domain-type arg (type type) expr)))
    (if (eq ntype (type type))
	type
	(mk-dep-binding (id type) ntype))))

(defmethod extend-domain-type (arg (type tupletype) expr)
  (if (some #'(lambda (ty)
		(or (subtype-of? ty type)
		    (and (fully-instantiated? ty)
			 (let ((*generate-tccs* 'none)
			       (narg (typecheck* (copy-untyped arg)
						 ty nil nil)))
			   (some #'(lambda (jty) (subtype-of? jty type))
				 (judgement-types+ narg))))))
	    (types arg))
      type
      (let* ((*generate-tccs* 'none)
	     (vid (make-new-variable '|x| expr))
	     (vb (make-bind-decl vid type))
	     (var (make-variable-expr vb))
	     (carg (typecheck* (copy arg) type nil nil))
	     (upred (make!-lambda-expr (list vb)
		      (make!-equation var carg)))
	     (tpred (beta-reduce upred)))
	(mk-subtype type tpred))))

(defmethod extend-domain-type (arg type expr)
  (declare (ignore arg expr))
  type)

(defun make-update-expr-funtype (args value expr type)
  (if (every #'(lambda (arg)
		 (some #'(lambda (ty)
			   (subtype-of? ty (domain type)))
		       (types arg)))
	     args)
      type
      (let* ((*generate-tccs* 'none)
	     (stype (supertype (domain type)))
	     (pred (predicate (domain type)))
	     (var (mk-name-expr (make-new-variable '|x| expr)))
	     (vb (mk-bind-decl (id var) stype))
	     (upred (mk-lambda-expr (list vb)
		      (mk-disjunction
		       (cons (mk-application pred var)
			     (list (mk-application '=
				     var (mk-arg-tuple-expr* args)))))))
	     (tpred (beta-reduce
		     (typecheck* upred (mk-funtype (list stype) *boolean*)
				 nil nil)))
	     (vtype (find-if #'(lambda (ty)
				 (compatible? ty (range type)))
		      (types value))))
	(unless vtype
	  (type-incompatible value (types value) (range type)))
	(mk-funtype (list (mk-subtype stype tpred))
		    (compatible-type (range type) vtype)))))

(defun typecheck-assignments (assigns type)
  (when assigns
    (let ((assign (car assigns)))
      (when (and (maplet? assign)
		 (cdr (arguments assign)))
	(type-error assign "Maplet assignment may not be nested"))
      (typecheck-ass-args (arguments assign) type (typep assign 'maplet))
      (typecheck* (expression assign) nil nil nil)
      (typecheck-assignments (cdr assigns) type))))

(defmethod typecheck-ass-args (args (rtype subtype) maplet?)
  (typecheck-ass-args args (supertype rtype) maplet?))

(defmethod typecheck-ass-args (args (rtype dep-binding) maplet?)
  (typecheck-ass-args args (type rtype) maplet?))

(defmethod typecheck-ass-args (args (rtype recordtype) maplet?)
  (when args
    (unless (and (null (cdar args))
		 (name-expr? (caar args)))
      (type-error (caar args) "Field name expected"))
    (let ((fieldpos (position (caar args) (fields rtype) :test #'same-id)))
      (cond (fieldpos
	     (when (cdr args)
	       (typecheck-ass-args (cdr args)
				   (type (nth fieldpos (fields rtype)))
				   maplet?)))
	    ((and maplet?
		  (null (cdr args))))
	    (t (type-error (caar args) "Field ~a not found in ~a"
			   (id (caar args)) rtype))))))

(defmethod typecheck-ass-args (args (tuptype tupletype) maplet?)
  (when args
    (unless (and (null (cdar args))
		 (number-expr? (caar args)))
      (type-error (caar args) "Number expected"))
    (unless (or (<= (number (caar args)) (length (types tuptype)))
		maplet?)
      (type-error (caar args)
	"Number out of range for type ~a" tuptype))
    (when (cdr args)
      (typecheck-ass-args (cdr args)
			  (nth (1- (number (caar args))) (types tuptype))
			  maplet?))))

(defmethod typecheck-ass-args (args (ftype funtype) maplet?)
  (when args
    (unless (or (singleton? (car args))
		(length= (car args) (domain-types ftype)))
      (type-error (car args)
	"Wrong number of assignment arguments, ~d expected"
	(length (domain-types ftype))))
;    (mapc #'(lambda (a d) (typecheck* a d nil nil))
;	  args (domain ftype))
    (mapc #'(lambda (a) (typecheck* a nil nil nil)) (car args))
    (let ((dtypes (if (singleton? (car args))
		      (list (domain ftype))
		      (domain-types ftype))))
      (unless (= (length (car args)) (length dtypes))
	(type-error (car args) "Wrong number of arguments"))
      (check-compatible-funtype-ass-args (car args) dtypes)
      (when (cdr args)
	(typecheck-ass-args (cdr args) (range ftype) maplet?)))))

(defun check-compatible-funtype-ass-args (args dtypes)
  (when args
    (let* ((dty (dep-binding-type (car dtypes)))
	   (ctypes (remove-if (complement
			       #'(lambda (pty) (compatible? pty dty)))
		     (ptypes (car args)))))
      (unless ctypes
	(type-incompatible (car args) (ptypes (car args)) dty))
      (check-compatible-funtype-ass-args (cdr args) (cdr dtypes)))))

(defmethod typecheck-ass-args (args (type datatype-subtype) maplet?)
  (let ((accs (collect-datatype-assign-arg-accessors type (caar args))))
    (setf (resolutions (caar args)) (resolutions (car accs)))
    (setf (type (caar args)) (type (car accs)))
    (when (cdr args)
      (typecheck-ass-args (cdr args) (range (type (car accs))) maplet?))))

(defmethod typecheck-ass-args (args (type adt-type-name) maplet?)
  (let ((accs (collect-datatype-assign-arg-accessors type (caar args))))
    (setf (resolutions (caar args)) (resolutions (car accs)))
    (setf (type (caar args)) (type (car accs)))
    (when (cdr args)
      (typecheck-ass-args (cdr args) (range (type (car accs))) maplet?))))

(defun collect-datatype-assign-arg-accessors (dtype arg)
  (let ((accs nil))
    (dolist (constr (constructors dtype))
      (let ((acc (find (id arg) (accessors constr) :key #'id)))
	(when acc
	  (pushnew acc accs :test #'tc-eq))))
    (nreverse accs)))

(defmethod typecheck-ass-args (args (rtype struct-sub-recordtype) maplet?)
  (when args
    (unless (and (null (cdar args))
		 (name-expr? (caar args)))
      (type-error (caar args) "Field name expected"))
    (let ((fieldpos (position (caar args) (fields rtype) :test #'same-id)))
      (cond (fieldpos
	     (when (cdr args)
	       (typecheck-ass-args (cdr args)
				   (type (nth fieldpos (fields rtype)))
				   maplet?)))
	    ((and maplet?
		  (null (cdr args))))
	    (t (type-error (caar args) "Field ~a not found in ~a"
			   (id (caar args)) rtype))))))

(defmethod typecheck-ass-args (args (tuptype struct-sub-tupletype) maplet?)
  (when args
    (unless (and (null (cdar args))
		 (number-expr? (caar args)))
      (type-error (caar args) "Number expected"))
    (unless (or (<= (number (caar args)) (length (types tuptype)))
		maplet?)
      (type-error (caar args)
	"Number out of range for type ~a" tuptype))
    (when (cdr args)
      (typecheck-ass-args (cdr args)
			  (nth (1- (number (caar args))) (types tuptype))
			  maplet?))))

(defmethod typecheck-ass-args (args type maplet?)
  (declare (ignore type maplet?))
  (type-error (caar args)
    "The expression type is inconsistent with this set of arguments"))


(defmethod typecheck* ((decl bind-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (if (declared-type decl)
      (let* ((*generate-tccs* 'none)
	     (type (typecheck* (declared-type decl) nil nil nil)))
	(unless (fully-instantiated? type)
	  (type-error (declared-type decl)
	    "Could not determine the full theory instance"))
	(set-type (declared-type decl) nil)
	(setf (type decl) type))
      (let ((vdecls (remove-if-not #'(lambda (d)
				       (and (var-decl? d)
					    (eq (module d) (current-theory))))
		      (get-declarations (id decl)))))
	(cond ((null vdecls) 
	       (type-error decl
		 "Variable ~a not previously declared" (id decl)))
	      ((singleton? vdecls)
	       (setf (type decl) (type (car vdecls))
		     ;;(declared-type decl) (declared-type (car vdecls))
		     ))
	      (t (type-error decl "Variable ~a is ambiguous" (id decl))))))
  decl)
