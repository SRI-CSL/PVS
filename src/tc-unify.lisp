;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tc-unify.lisp -- Matches terms to get bindings for formal parameters
;; Author          : Sam Owre
;; Created On      : Fri Dec 17 02:44:21 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 17:05:57 1998
;; Update Count    : 10
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

;;; Find-compatible-bindings takes a set of arguments (all of whose
;;; types slot is set to the possible types), a set of formal types
;;; to the given constant declaration, and a binding of the form
;;; ((fid1) (fid2) ... ) where each fid is the formal parameter
;;; identifier for the module containing the constant declaration.  The
;;; result of this function is a set of bindings of the form
;;; (((fid1 . v1) (fid2 . v2) ...) ((fid1 . v1') (fid2 . v2') ...) ...)
;;; Not all of the vi's will necessarily be found; incomplete bindings
;;; will be weeded out in resolve.lisp - see create-compatible-modinsts.

(defun find-compatible-bindings (arguments formals binding)
  (let* ((args-types (mapcar #'get-compatible-binding-types arguments))
	 (types-lists (cartesian-product args-types)))
    (find-compatible-bindings* types-lists formals binding nil)))

(defun get-compatible-binding-types (obj)
  (if (type obj)
      (list (lift-constructor-type obj))
      (lift-constructor-types obj)))

(defmethod lift-constructor-type ((obj constructor-name-expr))
  (supertype (type obj)))

(defmethod lift-constructor-type ((obj application))
  (if (constructor-name-expr? (operator obj))
      (supertype (type obj))
      (car (judgement-types+ obj))))

(defmethod lift-constructor-type (obj)
  (type obj))

(defmethod lift-constructor-types ((obj name-expr))
  (let ((creses (remove-if (complement #'no-argument-constructor-resolution?)
		  (resolutions obj))))
    (if creses
	(delete-duplicates
	 (nconc (mapcar #'(lambda (r) (supertype (type r))) creses)
		(remove-if #'(lambda (ty)
			       (some #'(lambda (r) (tc-eq ty (type r)))
				     creses))
		  (types obj)))
	 :test #'tc-eq)
	(types obj))))

(defmethod lift-constructor-types ((obj application))
  (if (name-expr? (operator obj))
      (let* ((creses (remove-if (complement #'constructor-resolution?)
		       (resolutions (operator obj))))
	     (rem-types (when creses
			  (remove-if
			      #'(lambda (ty)
				  (member ty
					  (resolutions (operator obj))
					  :test #'tc-eq
					  :key #'(lambda (r)
						   (range (type r)))))
			    (types obj)))))
	(if creses
	    (delete-duplicates
	     (nconc (mapcar #'(lambda (r)
				(if (conversion-resolution? r)
				    (range (type r))
				    (find-adt-supertype (range (type r)))))
		      creses)
		    (mapcan #'(lambda (r)
				(unless (memq r creses)
				  (list (range (type r)))))
		      (resolutions (operator obj)))
		    rem-types)
	     :test #'tc-eq)
	    (types obj)))
      (types obj)))

(defmethod lift-constructor-types (obj)
  (types obj))

(defun constructor-resolution? (res)
  (adt-constructor-decl? (declaration res)))

(defun no-argument-constructor-resolution? (res)
  (and (adt-constructor-decl? (declaration res))
       (not (funtype? (type res)))))

(defun find-compatible-bindings* (types-lists formals binding result)
  (if (null types-lists)
      result
      (let* ((fbinding (mapcar #'list
			 (remove-if #'(lambda (fp)
					(or (not (decl-formal? fp))
					    (assq fp binding)
					    (memq fp (decl-formals
						      (current-declaration)))))
			   (free-params (car types-lists)))))
	     (cbinding (copy-tree binding))
	     (nbinding (if fbinding
			   (nconc cbinding fbinding)
			   cbinding))
	     (nbind (find-compatible-binding (car types-lists) formals nbinding)))
	(when nbind
	  (when fbinding
	    (if (every #'cdr fbinding)
		(setq nbind (subst-for-formals (ldiff nbind fbinding) fbinding))
		(setq nbind (ldiff nbind fbinding))))
	  ;; (let ((nnbind (mapcar #'(lambda (bd)
	  ;; 			    (cons (car bd)
	  ;; 				  (subst-for-formals (cdr bd) nbind)))
	  ;; 		  nbind)))
	  ;;   (setq nbind nnbind))
	  )
	(find-compatible-bindings* (cdr types-lists) formals binding
				   (if nbind (cons nbind result) result)))))

(defvar *tc-match-strictly* nil)

(defvar *tc-strict-matches* nil)

(defvar *formals-theory* nil)

(defvar *tc-match-boundvars* nil) ;;NSH: see below.

(defun find-compatible-binding (types formals binding)
  (let ((*formals-theory* (module (caar binding)))
	(*tc-strict-matches* nil))
    (find-compatible-binding* types formals binding)))

(defun find-compatible-binding* (types formals binding)
  (if (or (null types) (null binding))
      binding
      (let* ((*tc-match-strictly* nil)
	     (nbinding (tc-match* (car types) (car formals) binding)))
	(find-compatible-binding* (cdr types) (cdr formals) nbinding))))

;;; A marker to keep track of when an exact match is found; i.e., when a
;;; name provides all the actuals.  This is to stop tc-match from using
;;; compatible-type, since at this point the actuals are completely
;;; specified.

(defun tc-unify (t1 t2 bindings)
  (tc-match t1 t2 bindings))

(defun tc-match (t1 t2 bindings &optional strict-matches)
  (declare (type list bindings))
  #+pvsdebug (assert (every #'(lambda (b) (typep (car b) 'formal-decl))
			    bindings)
		     () "tc-match: bindings must be formal declarations")
  (when bindings
    (let* ((*formals-theory* (module (caar bindings)))
	   (*tc-strict-matches* strict-matches))
      (values (tc-match* t1 t2 bindings)
	      *tc-strict-matches*))))

(defmethod tc-match* (t1 t2 bindings)
  (declare (ignore t1 t2 bindings))
  #+pvsdebug (when #+scl (eq (type-of t1) (type-of t2))
		   #-scl (eq (class-of t1) (class-of t2))
		   (break "Match should handle ~a types"
			  #+scl (type-of t1)
			  #-scl (class-of t1)))
  nil)

(defmethod tc-match* ((args list) (fargs list) bindings)
  (declare (type list bindings))
  (cond ((or (null bindings) (null args))
	 bindings)
	((and (singleton? args)
	      (typep (car args) 'tupletype)
	      (not (singleton? fargs)))
	 (tc-match* (types (car args)) fargs bindings))
	((and (singleton? fargs)
	      (typep (car fargs) 'tupletype))
	 (tc-match* args (types (car fargs)) bindings))
	(t (tc-match* (cdr args) (cdr fargs)
		      (tc-match* (car args) (car fargs) bindings)))))

(defmethod tc-match* :around ((arg type-expr) farg bindings)
  (with-slots (print-type) arg
    (declare (type list bindings))
    (when bindings
      (if (tc-eq arg farg)
	  bindings
	  (let ((nbindings (if (or (free-params farg)
				   (free-params arg))
			       (call-next-method)
			       (when (and farg
					  (compatible? (find-supertype arg)
						       (find-supertype farg)))
				 bindings))))
	    (when nbindings
	      (tc-match-print-type print-type farg nbindings)))))))

(defmethod tc-match-print-type ((ptype name) farg nbindings)
  (declare (ignore farg))
  (let* ((res (car (resolutions ptype)))
	 (acts (actuals (module-instance res)))
	 (dacts (dactuals (module-instance res))))
    (or (if (and (or acts dacts)
		 (eq (module (declaration res)) *formals-theory*))
	    (let ((formals (formals-sans-usings (module (declaration res))))
		  (dformals (decl-formals (declaration res))))
	      (tc-match-actuals acts formals 
				(or (tc-match-actuals dacts dformals nbindings)
				    nbindings)))
	    (tc-match* ptype farg nbindings))
	nbindings)))

(defmethod tc-match-print-type ((ptype type-application) farg nbindings)
  (or (tc-match* ptype
		 (or (when (type-expr? farg)
		       (print-type farg))
		     farg)
		 nbindings)
      nbindings))

(defmethod tc-match-print-type (ptype farg nbindings)
  (declare (ignore ptype farg))
  nbindings)

(defmethod tc-match* ((arg type-expr) (farg type-name) bindings)
  (declare (type list bindings))
  (let ((binding (assq (declaration farg) bindings)))
    (declare (type list binding))
    (cond ((null binding)
	   nil)
	  ((null (cdr binding))
	   (unless (and (dependent-type? arg)
			(not (formal-type-appl-decl? (declaration farg))))
	     (when *tc-match-strictly*
	       (push arg *tc-strict-matches*))
	     (setf (cdr binding) arg))
	   bindings)
	  (t (set-tc-match-binding binding arg bindings)))))

(defun set-tc-match-binding (binding arg bindings &optional (last-attempt? t))
  (declare (type list bindings))
  (let ((nbindings (when (some #'(lambda (bd) (decl-formal? (car bd))) bindings)
		     (tc-match* arg (cdr binding) bindings)))
	(dtype (compatible-type (cdr binding) arg)))
    (declare (type list nbindings))
    (if dtype
	(let ((type (dep-binding-type dtype)))
	  (unless (or (and (dependent-type? type)
			   (not (has-type-vars? (cdr binding))))
		      (and (member (cdr binding) *tc-strict-matches*
				   :test #'tc-eq)
			   (or (fully-instantiated? (cdr binding))
			       (not (fully-instantiated? arg)))))
	    (cond (*tc-match-strictly*
		   (push arg *tc-strict-matches*)
		   (setf (cdr binding) arg))
		  (t (setf (cdr binding) type))))
	  (or nbindings bindings))
	(when last-attempt?
	  (tc-match-last-attempt (cdr binding) arg binding
				 (or nbindings bindings))))))

(defun tc-match-last-attempt (arg1 arg2 binding bindings)
  (declare (type list bindings))
  (if (fully-instantiated? arg1)
      (unless (fully-instantiated? arg2)
	(tc-match-last-attempt* arg1 arg2 binding bindings))
      (when (fully-instantiated? arg2)
	(tc-match-last-attempt* arg2 arg1 binding bindings))))

(defun tc-match-last-attempt* (arg1 arg2 binding bindings)
  (declare (type list bindings))
  (let ((nbindings (tc-match arg1 arg2 (mapcar #'list (free-params arg2)))))
    (declare (type list nbindings))
    (when (and nbindings (every #'cdr nbindings))
      (setf (cdr binding) arg1)
      bindings)))

(defun dependent-type? (type)
  (some #'(lambda (fv)
	    (and (typep (declaration fv) 'dep-binding)
		 (not (member fv *bound-variables*
			      :test #'same-declaration))))
	(freevars type)))

;(defmethod tc-match-formal-subtype-check ((farg formal-subtype-decl) arg
;					  bindings)
;  )

;(defmethod tc-match-formal-subtype-check ((farg formal-type-decl) arg
;					  bindings)
;  )


(defmethod tc-match* ((arg type-name) (farg type-name) bindings)
  (declare (type list bindings))
  (unless (null bindings)
    (or (call-next-method)
	(cond ((tc-eq arg farg)
	       bindings)
	      ((eq (id arg) (id farg))
	       (let* ((m1 (module-instance (resolution arg)))
		      (m2 (module-instance (resolution farg)))
		      (a1 (actuals m1))
		      (a2 (actuals m2))
		      (d1 (dactuals m1))
		      (d2 (dactuals m2)))
		 (let ((abindings (tc-match-type-name-actuals a1 a2 arg farg bindings)))
		   (declare (type list abindings))
		   (if (or d1 d2)
		       (tc-match-type-name-dactuals
			d1 d2 arg farg (or abindings bindings))
		       abindings))))
	      ((let ((binding (call-next-method farg arg bindings)))
		 binding))))))

(defun tc-match-type-name-dactuals (d1 d2 arg farg bindings)
  (declare (type list bindings))
  (cond ((and d1 d2)
	 (or (tc-match* d1 d2 bindings)
	     bindings))
	(d1
	 (tc-match-acts1
	  d1 (decl-formals (declaration (resolution farg)))
	  bindings))
	(d2
	 (tc-match-acts1
	  d2 (decl-formals (declaration (resolution arg)))
	  bindings))
	(t bindings)))

(defun tc-match-type-name-actuals (a1 a2 arg farg bindings)
  (declare (type list bindings))
  (or (cond ((and a1 a2)
	     (or (tc-match* a1 a2 bindings)
		 bindings))
	    (a1
	     (tc-match-acts1
	      a1 (formals-sans-usings
		  (module (declaration (resolution farg))))
	      bindings))
	    (a2
	     (tc-match-acts1
	      a2 (formals-sans-usings
		  (module (declaration (resolution arg))))
	      bindings))
	    (t (tc-match-names arg farg bindings)))
      (and (formal-type-decl? (declaration farg))
	   (not (decl-formal-type? (declaration farg)))
	   (not (assq (declaration farg) bindings))
	   bindings)))

(defmethod tc-match* ((arg type-application) (farg type-application) bindings)
  (declare (type list bindings))
  (unless (null bindings)
    (or (call-next-method)
	(and (tc-eq (type arg) (type farg))
	     (tc-match* (parameters arg) (parameters farg) bindings))
	bindings)))

;;; Called by match* (modname modname)
(defun tc-match-acts (acts formals bindings)
  (let ((*tc-match-strictly* t)
	(*tc-strict-matches* nil))
    (tc-match-acts1 acts formals bindings)))

(defun tc-match-acts1 (acts formals bindings)
  (declare (type list acts formals bindings))
  (when (length= acts formals)
    (tc-match-acts* acts formals bindings)))

(defun tc-match-acts* (acts formals bindings)
  (declare (type list acts formals bindings))
  (cond ((null bindings)
	 nil)
	((null acts)
	 bindings)
	(t (tc-match-acts* (cdr acts) (cdr formals)
			   (tc-match-act (car acts) (car formals) bindings)))))

(defun tc-match-act (act formal bindings)
  (declare (type list bindings))
  (typecase formal
    (formal-subtype-decl
     (when (type-value act)
       (let ((binding (assq formal bindings)))
	 (declare (type list binding))
	 (when binding
	   (if (and *tc-match-exact* (cdr binding))
	       (when (tc-eq (type-value act) (cdr binding)) bindings)
	       (tc-match* (type-value act) (type-value formal) bindings))))))
    (formal-type-decl
     (when (type-value act)
       (let ((binding (assq (declaration formal) bindings)))
	 (declare (type list binding))
	 (if (and *tc-match-exact* (cdr binding))
	     (when (tc-eq (type-value act) (cdr binding)) bindings)
	     (tc-match* (type-value act) (type-value formal) bindings)))))
    (formal-const-decl
     (let ((binding (assq (declaration formal) bindings)))
       (declare (type list binding))
       (cond ((null binding)
	      nil)
	     ((null (cdr binding))
	      (setf (cdr binding) (expr act))
	      bindings)
	     ((tc-eq (expr act) (cdr binding))
	      bindings))))
    (formal-theory-decl
     (when (theory-name-expr? (expr act))
       (let ((binding (assq formal bindings)))
	 (declare (type list binding))
	 (cond ((null binding)
		nil)
	       ((null (cdr bindings))
		(setf (cdr binding) (expr act))
		bindings)
	       ((tc-eq (expr act) (cdr binding))
		bindings)
	       (t (tc-match* (expr act) formal bindings))))))
    (t (break "Shouldn't get here"))))

(defmethod tc-match* (arg (farg dep-binding) bindings)
  (declare (type list bindings))
  (tc-match* arg (type farg) bindings))

(defmethod tc-match* ((arg dep-binding) farg bindings)
  (declare (type list bindings))
  (tc-match* (type arg) farg bindings))

(defmethod tc-match* ((arg subtype) farg bindings)
  (declare (type list bindings))
  (when bindings
    (or (call-next-method)
	(tc-match* (supertype arg) farg bindings))))

(defmethod tc-match* ((arg datatype-subtype) farg bindings)
  (declare (type list bindings))
  (when bindings
    (let ((nbindings
	   (if (and (typep farg 'type-name)
		    (let ((bind (cdr (assq (declaration farg)
					   bindings))))
		      (and bind
			   (fully-instantiated? bind))))
	       (let ((binding (assq (declaration farg) bindings)))
		 (set-tc-match-binding binding arg bindings))
	       (tc-match* (declared-type arg) farg bindings))))
      (declare (type list nbindings))
      (when nbindings
	(mapc #'(lambda (b)
		  (let ((cdrb (gensubst (cdr b)
				#'(lambda (ex) (declare (ignore ex)) arg)
				#'(lambda (ex) (eq ex (declared-type arg))))))
		    (unless (eq cdrb (cdr b))
		      (setf (cdr b) cdrb))))
	      nbindings)
	(unless (every #'cdr nbindings)
	  (call-next-method))
	nbindings))))

(defmethod tc-match* (arg (farg subtype) bindings)
  (declare (type list bindings))
  (when bindings
    (let ((fsubtype-binding
	   (assoc farg bindings
		  :test #'(lambda (x y)
			    (and (typep y 'formal-subtype-decl)
				 (tc-eq x (type-value y)))))))
      (declare (type list fsubtype-binding))
      (cond ((null fsubtype-binding)
	     (or (call-next-method)
		 (when (print-type farg)
		   (tc-match* arg (print-type farg) bindings))
		 (tc-match* arg (supertype farg) bindings)))
	    ((null (cdr fsubtype-binding))
	     (when *tc-match-strictly*
	       (push arg *tc-strict-matches*))
	     (setf (cdr fsubtype-binding) arg)
	     bindings)
	    ((tc-eq arg (cdr fsubtype-binding))
	     bindings)))))

(defmethod tc-match* ((arg subtype) (farg subtype) bindings)
  ;; This will only check for the predicates being tc-eq, not provably equal
  (declare (type list bindings))
  (unless (null bindings)
    (flet ((atest (x y)
	     (and (typep y 'formal-subtype-decl)
		  (tc-eq x (type-value y)))))
      (let ((binding (assoc farg bindings :test #'atest)))
	(if binding
	    (cond ((null (cdr binding))
		   (when *tc-match-strictly*
		     (push arg *tc-strict-matches*))
		   (setf (cdr binding) arg)
		   bindings)
		  (t (set-tc-match-binding binding arg bindings nil)))
	    (or (let ((nbind (tc-match* (supertype arg) (supertype farg)
					(tc-match* (predicate arg)
						   (predicate farg)
						   bindings))))
		  (completed-tc-match-bindings nbind))
		(let ((nbind (tc-match* arg (supertype farg) bindings)))
		  (completed-tc-match-bindings nbind))
		(tc-match* (supertype arg) farg bindings)))))))

(defun completed-tc-match-bindings (nbind)
  (when (every #'cdr nbind)
    nbind))

(defmethod tc-match* ((arg funtype) (farg funtype) bindings)
  (declare (type list bindings))
  (unless (null bindings)
    (tc-match* (range arg) (range farg)
	       (let ((*tc-match-strictly* t))
		 (tc-match* (domain arg) (domain farg) bindings)))))

(defmethod tc-match* ((arg tupletype) (farg tupletype) bindings)
  (declare (type list bindings))
  (tc-match* (types arg) (types farg) bindings))

(defmethod tc-match* ((arg tupletype) (farg struct-sub-tupletype) bindings)
  (declare (type list bindings))
  (when bindings
    (let ((fbinding (assoc farg bindings
			   :test #'(lambda (x y)
				     (and (typep y 'formal-struct-subtype-decl)
					  (tc-eq x (type-value y)))))))
      (declare (type list fbinding))
      (cond ((null fbinding)
	     (or (call-next-method)
		 (break "tc-match* (tupletype struct-sub-tupletype)")))
	    ((null (cdr fbinding))
	     (when *tc-match-strictly*
	       (push arg *tc-strict-matches*))
	     (setf (cdr fbinding) arg)
	     bindings)
	    ((tc-eq arg (cdr fbinding))
	     bindings)))))

(defmethod tc-match* ((arg cotupletype) (farg cotupletype) bindings)
  (declare (type list bindings))
  (tc-match* (types arg) (types farg) bindings))

(defmethod tc-match* ((arg recordtype) (farg recordtype) bindings)
  ;;; Make sure the fields are sorted before this
  ;;; Only should be needed if one is dependent and the other isn't
  (declare (type list bindings))
  (if (dependent? arg)
      (if (dependent? farg)
	  (tc-match* (fields arg) (fields farg) bindings)
	  (when (and (length= (fields arg) (fields farg))
		     (every #'(lambda (afld)
				(member (id afld) (fields farg) :key #'id))
			    (fields arg)))
	    (tc-match* (fields arg)
		       (mapcar #'(lambda (afld)
				   (find (id afld) (fields farg) :key #'id))
			 (fields arg))
		       bindings)))
      (if (dependent? farg)
	  (tc-match* (fields arg) (fields farg) bindings)
	  (when (and (length= (fields arg) (fields farg))
		     (every #'(lambda (afld)
				(member (id afld) (fields farg) :key #'id))
			    (fields arg)))
	    (tc-match* (mapcar #'(lambda (afld)
				   (find (id afld) (fields arg) :key #'id))
			 (fields farg))
		       (fields farg)
		       bindings)))))

(defmethod tc-match* ((fld field-decl) (ffld field-decl) bindings)
  (declare (type list bindings))
  (when (eq (id fld) (id ffld))
    (tc-match* (type fld) (type ffld) bindings)))

(defmethod tc-match* ((arg recordtype) (farg struct-sub-recordtype) bindings)
  (declare (type list bindings))
  (when bindings
    (let ((fbinding (assoc farg bindings
			   :test #'(lambda (x y)
				     (and (typep y 'formal-struct-subtype-decl)
					  (tc-eq x (type-value y)))))))
      (declare (type list fbinding))
      (cond ((null fbinding)
	     (or (call-next-method)
		 (break "tc-match* (recordtype struct-sub-recordtype)")))
	    ((null (cdr fbinding))
	     (when *tc-match-strictly*
	       (push arg *tc-strict-matches*))
	     (setf (cdr fbinding) arg)
	     bindings)
	    ((tc-eq arg (cdr fbinding))
	     bindings)))))

(defmethod tc-match-names ((n1 name) (n2 name) bindings)
  (declare (type list bindings))
  (if (tc-eq n1 n2)
      bindings
      (when (same-id n1 n2)
	(let* ((m1 (module-instance (resolution n1)))
	       (m2 (module-instance (resolution n2)))
	       (a1 (actuals m1))
	       (a2 (actuals m2)))
	  (cond ((and a1 a2)
		 (tc-match* a1 a2 bindings))
		(a1
		 (tc-match-acts1
		  a1 (formals-sans-usings (module (declaration n2)))
		  bindings))
		(a2
		 (tc-match-acts1
		  a2 (formals-sans-usings (module (declaration n1)))
		  bindings)))))))

(defmethod tc-match* ((a1 actual) (a2 actual) bindings)
  (declare (type list bindings))
  (if (type-value a1)
      (and (type-value a2)
	   (tc-match* (type-value a1) (type-value a2) bindings))
      (and (null (type-value a2))
	   (tc-match* (expr a1) (expr a2) bindings))))


;;; Expressions

(defmethod tc-match* ((arg number-expr) (farg number-expr) bindings)
  (declare (type list bindings))
  (with-slots ((anum number)) arg
    (with-slots ((fnum number)) farg
      (when (= anum fnum)
	bindings))))

(defmethod tc-match* ((A coercion) (B expr) bindings)
  (declare (type list bindings))
  (tc-match* (args1 A) B bindings))

(defmethod tc-match* ((A expr) (B coercion) bindings)
  (declare (type list bindings))
  (tc-match* A (args1 B) bindings))

(defmethod tc-match* ((A name-expr) (B coercion) bindings)
  (declare (type list bindings))
  (tc-match* A (args1 B) bindings))



(defmethod tc-match* ((arg expr) (farg name-expr) bindings)
  (let ((binding (assq (declaration farg) bindings)))
    (declare (type list binding))
    (cond ((null binding)
	   (when (and (typep arg 'name-expr)
		      (eq (declaration arg) (declaration farg))
		      (actuals (module-instance  arg))
		      (null (actuals (module-instance  farg))))
	     (mapc #'(lambda (act frm)
		       (unless (null bindings)
			 (let ((bind (assq frm bindings))
			       (type? (formal-type-decl? frm)))
			   (cond ((null bind)
				  (setq bindings nil))
				 ((null (cdr bind))
				  (setf (cdr bind)
					(if type?
					    (type-value act)
					    act)))
				 ((and type?
				       (tc-eq (cdr bind) (type-value act))))
				 ((and (not type?)
				       (tc-eq (cdr bind) act)))
				 (t (setq bindings nil))))))
		   (actuals (module-instance arg))
		   (formals-sans-usings (module (declaration arg))))
	     bindings))
	  ((null (cdr binding))
	   (setf (cdr binding) arg)
	   bindings)
	  (t (if (tc-eq arg (cdr binding)) bindings nil)))))

(defmethod tc-match* ((arg record-expr) (farg record-expr) bindings)
  (tc-match* (assignments arg) (assignments farg) bindings))

;(defmethod tc-match* ((arg coercion) (farg coercion) bindings)
;  (tc-match* (expression arg) (expression farg) bindings))

;(defmethod tc-match* ((arg intype) (farg intype) bindings)
;  (tc-match* (expression arg) (expression farg)
;	 (tc-match* (type arg) (type farg) bindings)))

(defmethod tc-match* ((arg projection-expr) (farg projection-expr) bindings)
  (with-slots ((i1 index)) arg
    (with-slots ((i2 index)) farg
      (when (= i1 i2)
	bindings))))

(defmethod tc-match* ((arg projection-expr) (farg name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg name-expr) (farg projection-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg injection-expr) (farg injection-expr) bindings)
  (with-slots ((i1 index)) arg
    (with-slots ((i2 index)) farg
      (when (= i1 i2)
	bindings))))

(defmethod tc-match* ((arg injection-expr) (farg name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg name-expr) (farg injection-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg injection?-expr) (farg injection?-expr) bindings)
  (with-slots ((i1 index)) arg
    (with-slots ((i2 index)) farg
      (when (= i1 i2)
	bindings))))

(defmethod tc-match* ((arg injection?-expr) (farg name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg name-expr) (farg injection?-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg extraction-expr) (farg extraction-expr) bindings)
  (with-slots ((i1 index)) arg
    (with-slots ((i2 index)) farg
      (when (= i1 i2)
	bindings))))

(defmethod tc-match* ((arg extraction-expr) (farg name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg name-expr) (farg extraction-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-match* ((arg projection-application)
		      (farg projection-application) bindings)
  (with-slots ((i1 index) (a1 argument)) arg
    (with-slots ((i2 index) (a2 argument)) farg
      (when (= i1 i2)
	(tc-match* a1 a2 bindings)))))

(defmethod tc-match* ((arg injection-application)
		      (farg injection-application) bindings)
  (with-slots ((i1 index) (a1 argument)) arg
    (with-slots ((i2 index) (a2 argument)) farg
      (when (= i1 i2)
	(tc-match* a1 a2 bindings)))))

(defmethod tc-match* ((arg injection?-application)
		      (farg injection?-application) bindings)
  (with-slots ((i1 index) (a1 argument)) arg
    (with-slots ((i2 index) (a2 argument)) farg
      (when (= i1 i2)
	(tc-match* a1 a2 bindings)))))

(defmethod tc-match* ((arg extraction-application)
		      (farg extraction-application) bindings)
  (with-slots ((i1 index) (a1 argument)) arg
    (with-slots ((i2 index) (a2 argument)) farg
      (when (= i1 i2)
	(tc-match* a1 a2 bindings)))))

(defmethod tc-match* ((arg field-application)
		      (farg field-application) bindings)
  (when (eq (id arg) (id farg))
    (tc-match* (argument arg) (argument farg) bindings)))

(defmethod tc-match* ((arg application) (farg application) bindings)
  (if (fully-instantiated? (operator farg))
      (if (tc-eq (operator arg) (operator farg))
	  (tc-match* (arguments arg) (arguments farg) bindings)
	  bindings)
      (tc-match* (operator arg) (operator farg)
		 (or (tc-match* (arguments arg) (arguments farg) bindings)
		     bindings))))

(defmethod tc-match* ((arg binding-expr) (farg binding-expr) bindings)
  (and (same-binding-op? arg farg)
       (let ((bind-bindings
	      (tc-match-bindings (bindings arg) (bindings farg) bindings)))
	 (when bind-bindings
	   (let ((*tc-match-boundvars*  
		  (nconc (pairlis (bindings arg)(bindings farg))
			 *tc-match-boundvars*)))
	     (tc-match* (expression arg) (expression farg)
		       bind-bindings))))))

(defun tc-match-bindings (argbinds fargbinds bindings)
  (cond ((null bindings) nil)
	((and (null argbinds)(null fargbinds)) bindings)
	((not (or  (null argbinds)(null fargbinds)))
	 (let ((*tc-match-boundvars*
		(cons (cons (car argbinds)(car fargbinds))
		      *tc-match-boundvars*)))
	   (tc-match-bindings (cdr argbinds)(cdr fargbinds)
			      (tc-match* (type (car argbinds))
					(type (car fargbinds))
					bindings))))
	(t nil)))

(defmethod tc-match* ((arg update-expr) (farg update-expr) bindings)
  (tc-match* (expression arg) (expression farg)
	 (tc-match* (assignments arg) (assignments farg) bindings)))

(defmethod tc-match* ((n1 name-expr) (n2 name-expr) bindings)
  (if (tc-eq n1 n2)
      bindings
      (let ((bound? (assoc (declaration n1) *tc-match-boundvars*
			   :key #'declaration)))
	(cond   ;;NSH: modified to treat bound variables.
	  (bound?
	   (when (eq (declaration n2)
		     (declaration (cdr bound?)))
	     bindings))
	  ((and (null (assq (declaration n2) bindings)) ;NSH: needs this.
		(same-id n1 n2)
		(same-id (module-instance (resolution n1))
			 (module-instance (resolution n2))))
	   (if (or (null (module (declaration n1)))
		   (null (module (declaration n2))))
	       bindings
	       (let* ((m1 (module-instance (resolution n1)))
		      (m2 (module-instance (resolution n2)))
		      (a1 (actuals m1))
		      (a2 (actuals m2))
		      (d1 (dactuals m1))
		      (d2 (dactuals m2)))
		 (let ((abindings (tc-match-type-name-actuals a1 a2 n1 n2 bindings)))
		   (if (or d1 d2)
		       (tc-match-type-name-dactuals
			d1 d2 n1 n2 (or abindings bindings))
		       abindings)))))
	  (t (call-next-method))))))

(defun tc-match-actuals (actuals formals bindings)
  (declare (list actuals formals))
  (when (length= actuals formals)
    (tc-match-actuals* actuals formals bindings)))

(defun tc-match-actuals* (actuals formals bindings)
  (if (null actuals)
      bindings
      (let ((binding (assq (car formals) bindings)))
	(cond ((null binding)
	       nil)
	      ((null (cdr binding))
	       (setf (cdr binding)
		     (or (type-value (car actuals))
			 (expr (car actuals))))
	       (tc-match-actuals* (cdr actuals) (cdr formals) bindings))
	      (t (when (if (type-value (car actuals))
			   (tc-eq (type-value (car actuals)) (cdr binding))
			   (tc-eq (expr (car actuals)) (cdr binding)))
		   (tc-match-actuals* (cdr actuals) (cdr formals) bindings)))))))

(defun collect-formals (expr)
  (let ((formals nil))
    (mapobject #'(lambda (ex)
		   (when (and (name? ex)
			      (formal-decl? (declaration ex)))
		     (pushnew (declaration ex) formals)))
	       expr)
    (mapcar #'(lambda (f) (list (id f))) formals)))
