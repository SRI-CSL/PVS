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
  (if (subtype? (type obj))
      (supertype (type obj))
      (type obj)))

(defmethod lift-constructor-type ((obj application))
  (if (constructor-name-expr? (operator obj))
      (if (subtype? (type obj))
	  (supertype (type obj))
	  (type obj))
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

(defvar *tc-match-boundvars* nil) ;;NSH: see below.

(defvar *tc-matching-domains* nil)

(defvar *tc-match-fixed-bindings* nil)

(defun find-compatible-binding (types formals binding)
  (let* ((*tc-strict-matches* nil)
	 (dbindings (tc-match-domains types formals (copy-tree binding))))
    ;; note that tc-match-domains doesn't check for consistent bindings;
    ;; still need to run find-compatible-binding* over the whole thing but
    ;; we make sure bindings set during tc-match-domain are not allowed to
    ;; change.
    (let ((*tc-match-fixed-bindings*
	   (delete-if-not #'cdr dbindings)))
      (find-compatible-binding* types formals binding))))

(defun external-formal-reference? (obj)
  (some #'(lambda (fp)
	    (and (not (memq fp (formals-sans-usings (current-theory))))
		 (not (memq fp (decl-formals (current-declaration))))))
	(free-params obj)))

(defun tc-match-domains (t1 t2 bindings)
  (let ((*tc-matching-domains* t)
	(dom-pairs (collect-domain-types t1 t2)))
    (values (tc-match-domains* dom-pairs bindings)
	    dom-pairs)))

(defun tc-match-domains* (dom-pairs bindings)
  (if (null dom-pairs)
      bindings
      (let ((nbindings (tc-match* (caar dom-pairs) (cdar dom-pairs) bindings)))
	(when nbindings
	  (tc-match-domains* (cdr dom-pairs) nbindings)))))

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

;;; tc-match takes two terms, and a formal-decls bindings alist, and matches
;;; the two terms, setting the bindings accordingly.

(defun tc-match (t1 t2 bindings &optional strict-matches)
  (declare (type list bindings))
  (when bindings
    ;; This assertion is not true in general - conversion declarations may
    ;; have free params that do not belong to the theory in which it was
    ;; declared.
    #+badassert (assert (every #'(lambda (b) (formal-not-in-context? (car b))) bindings))
    (let* ((*tc-strict-matches* strict-matches)
	   (dbindings (tc-match-domains t1 t2 (copy-tree bindings))))
      (let* ((*tc-match-fixed-bindings* (delete-if-not #'cdr dbindings))
	     (nbindings (tc-match* t1 t2 bindings)))
	(values nbindings *tc-strict-matches*)))))

;;; Don't call this with each tc-match, as it's possible to compose tc-matches, as
;;; in instantiate-operator-type.
(defun tc-match-check (bindings)
  "tc-match-check checks whether the bindings after tc-match* are all there, and
that no RHS has references to any of the bindings LHSs.  It's possible for this to
be violated during tc-match*, but at the end they should all be fully-instantiated
wrt the initial bindings."
  (every #'(lambda (fbd)
	     (and (cdr fbd)
		  (not (some #'(lambda (fm) (assq fm bindings))
			     (formals-not-in-context (cdr fbd))))))
	 bindings))

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

(defmethod tc-match* :around ((arg type-expr) (farg type-expr) bindings)
  (with-slots ((apt print-type)) arg
    (with-slots ((fpt print-type)) farg
      (declare (type list bindings))
      (when bindings
	(if (tc-eq arg farg)
	    bindings
	    (let ((nbindings (call-next-method)))
	      ;; In principal, print-type should not be needed, but the
	      ;; predicate is pseudo-normalized in the subtype.  For example,
	      ;; below(0) in normalizes to {s: nat | FALSE} rather than 0<0.
	      (when nbindings
		(if (or (null apt)
			(null fpt)
	    		(every #'cdr nbindings))
	    	    nbindings
	    	    (or (tc-match* apt fpt nbindings)
			nbindings)))))))))

(defmethod tc-match-print-type ((ptype name) farg nbindings)
  (assert (resolution ptype))
  (let* ((res (resolution ptype))
	 (thinst (module-instance res))
	 (acts (actuals thinst))
	 (dacts (dactuals thinst)))
    (or (if (or acts dacts)
	    (let* ((formals (formals-sans-usings (module (declaration res))))
		   (dformals (decl-formals (declaration res)))
		   (dbdgs (when dformals
			    (tc-match-actuals dacts dformals nbindings))))
	      (tc-match-actuals acts formals (or dbdgs nbindings)))
	    (if (every #'cdr nbindings)
		nbindings
		(tc-match* ptype farg nbindings)))
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

(defun tc-match-set-binding (binding arg)
  "Sets the binding to the arg.  Mostly to aid debugging, as this is done in
a number of places in tc-match."
  ;;(break "tc-match-set-binding ~a to ~a" (car binding) arg)
  (setf (cdr binding) arg))
  
(defmethod tc-match* ((arg type-expr) (farg type-name) bindings)
  ;; More specialized methods for type-name and subtype have been called,
  ;; but decided to call-next-method
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
	     (tc-match-set-binding binding arg))
	   bindings)
	  ;; ((and (fully-instantiated? arg)
	  ;; 	(fully-instantiated? (cdr binding)))
	  ;;  (when (compatible? (cdr binding) arg)
	  ;;    (when (and (not (tc-eq (cdr binding) arg))
	  ;; 		(not (tc-eq (compatible-type (cdr binding) arg)
	  ;; 			    (cdr binding))))
	  ;;      (break "should set to compatible-type?"))
	  ;;    bindings))
	  (t (set-tc-match-binding arg (cdr binding) binding bindings)))))

;;; Called when cdr binding has a value.  Need to make sure it is compatible.
;;; Sets the (cdr binding) by tc-match on the current value and the arg

(defmethod set-tc-match-binding ((arg type-expr) (barg type-expr) binding bindings)
  "barg is what a formal currently binds to.  binding is the binding, with car the
formal and cdr eq to barg.  Tries to match arg to barg, and update (cdr binding)
accordingly.  Returns nil if there's no compatible instance available, otherwise
returns the updated bindings."
  (declare (type list bindings))
  (if (tc-eq arg barg)
      bindings
      (when (compatible? arg barg)
	(cond (*tc-matching-domains*
	       (if (subtype-of? barg arg)
		   (tc-match-set-binding binding barg)
		   (tc-match-set-binding binding arg))
	       bindings)
	      ((assq (car binding) *tc-match-fixed-bindings*)
	       (let ((fixed (cdr (assq (car binding) *tc-match-fixed-bindings*))))
		 (if (tc-eq fixed barg)
		     bindings
		     (when (compatible? fixed barg)
		       (tc-match-set-binding binding fixed)
		       bindings))))
	      (t (let* ((ainst? (fully-instantiated? arg))
			(binst? (fully-instantiated? barg))
			(iarg (if (or ainst? (not binst?))
				  arg
				  (or (get-tc-match-instance barg arg)
				      arg)))
			(ibarg (if (or binst? (not ainst?))
				   barg
				   (or (get-tc-match-instance arg barg)
				       barg)))
			(dtype (if (or (fully-instantiated? ibarg)
				       (not (fully-instantiated? iarg)))
				   (compatible-type ibarg iarg)
				   (compatible-type iarg ibarg))))
		   (when dtype
		     ;; (when (member barg *tc-strict-matches* :test #'tc-eq)
		     ;;   (break "check on *tc-strict-matches*"))
		     (unless (or (tc-eq dtype barg)
				 (and (member barg *tc-strict-matches* :test #'tc-eq)
				      ;; This is suspicious - check
				      (or (has-type-vars? arg)
					  (not (has-type-vars? barg)))
				      (or binst? (not ainst?))))
		       (tc-match-set-binding binding dtype))
		     bindings)))))))

;;; Assuming either atype or etype is fully-instantiated,
;;; Instantiates the other, if necessary, and returns the compatible-type
(defun tc-match-compatible-type (atype etype)
  (let ((ainst? (fully-instantiated? atype))
	(einst? (fully-instantiated? etype)))
    (assert (or ainst? einst?))
    (cond ((and ainst? einst?)
	   (compatible-type atype etype))
	  (ainst?
	   (let ((einst (get-tc-match-instance atype etype)))
	     (when einst
	       (compatible-type atype einst))))
	  (t (tc-match-compatible-type etype atype)))))

;;; Given a fully-instantiated ainst, creates a tc-match binding from epat,
;;; calls tc-match to set the binding, and creates the instance of epat.
;;; Can return nil
(defun get-tc-match-instance (ainst epat)
  #+pvsdebug
  (assert (fully-instantiated? ainst))
  #+pvsdebug
  (assert (not (fully-instantiated? epat)))
  (let* ((bindings (mapcar #'list (formals-not-in-context epat)))
	 (abindings (tc-match ainst epat bindings)))
    ;; Remember that tc-match side-effects bindings, returns nil for failure
    (when (and abindings (every #'cdr abindings))
      (let ((einst (subst-for-formals epat abindings)))
	#+pvsdebug
	(assert (fully-instantiated? einst) ()
		"get-tc-match-instance: not fully instantiated after subst")
	(if (tc-eq einst ainst)
	    ainst
	    einst)))))


(defmethod reset-typevar-binding ((tvar tup-type-variable) arg bindings)
  (let ((pvar (proj-type-var tvar)))
    (gensubst bindings
      #'(lambda (x) (declare (ignore x)) (nth (1- (index pvar)) (types arg)))
      #'(lambda (x) (eq x pvar)))))

(defmethod reset-typevar-binding ((rvar rec-type-variable) arg bindings)
  (let ((fvar (field-type-var rvar))
	(rtype (find-supertype arg)))
    (when (recordtype? rtype)
      (let ((fld (find (field-id fvar) (fields rtype) :test #'same-id)))
	(when fld
	  (gensubst bindings
	    #'(lambda (x) (declare (ignore x)) (type fld))
	    #'(lambda (x) (eq x fvar))))))))

(defmethod reset-typevar-binding ((cvar cotup-type-variable) arg bindings)
  (let ((ovar (out-type-var cvar))
	(cotype (find-supertype arg)))
    (when (and (cotupletype? cotype)
	       (<= (index ovar) (length (types cotype))))
      (gensubst bindings
	#'(lambda (x) (declare (ignore x)) (nth (1- (index ovar)) (types cotype)))
	#'(lambda (x) (eq x ovar))))))

;; No way to find the cotuptype for in-type-variables here
;; (defmethod reset-typevar-binding ((ivar in-type-variable) arg bindings)
;;   (let ((covar (cotup-var ivar))
;; 	(intype (find-supertype arg)))
;;     (break "reset-typevar-binding in-type-variable")
;;     (when (and (cotupletype? cotype)
;; 	       (<= (length (types cotype)) (index ovar)))
;;       (gensubst bindings
;; 	#'(lambda (x) (declare (ignore x)) (nth (1- (index ovar)) (types cotype)))
;; 	#'(lambda (x) (eq x ovar))))))

(defmethod reset-typevar-binding (bnd arg bindings)
  (declare (ignore bnd arg))
  bindings)

;; If arg1 or arg2 is fully-instantiated, try to tc-match that way
;; 
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
      (break "Is tc-match-last-attempt really needed?")
      (tc-match-set-binding binding arg1)
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
	      (cond ((or (not (dependent? (car binding)))
			 (let ((new-fml (subst-acts-in-form (car binding) bindings)))
			   (strict-compatible? (type new-fml) (type (expr act)))))
		     #+pvsdebug
		     (assert (fully-instantiated? (expr act)))
		     (tc-match-set-binding binding (expr act))
		     bindings)
		    (t
		     ;;(pvs-message-with-context
		     ;;  act "CHECKTHIS: tc-match-act would have worked before")
		     nil)))
	     ((tc-eq (expr act) (cdr binding))
	      bindings))))
    (formal-theory-decl
     (when (theory-name-expr? (expr act))
       (let ((binding (assq formal bindings)))
	 (declare (type list binding))
	 (cond ((null binding)
		nil)
	       ((null (cdr bindings))
		#+pvsdebug
		(assert (fully-instantiated? (expr act)))
		(tc-match-set-binding binding (expr act))
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

(defmethod tc-match* ((arg datatype-subtype) (farg type-name) bindings)
  (declare (type list bindings))
  (let ((binding (assq (declaration farg) bindings)))
    (if (cdr binding)
	(set-tc-match-binding arg (cdr binding) binding bindings)
	(call-next-method))))

(defmethod tc-match* ((arg datatype-subtype) farg bindings)
  (declare (type list bindings))
  (let ((nbindings (tc-match* (declared-type arg) farg bindings)))
    (declare (type list nbindings))
    (when nbindings
      (dolist (b nbindings)
      	(let ((cdrb (gensubst (cdr b)
      		      #'(lambda (ex) (declare (ignore ex)) arg)
      		      #'(lambda (ex) (eq ex (declared-type arg))))))
      	  ;; Replaced declared-type with arg
      	  (unless (tc-eq cdrb (cdr b))
	    #+badassert
      	    (assert (fully-instantiated? cdrb))
	    (tc-match-set-binding b cdrb))))
      (unless (every #'(lambda (fp)
			 (cdr (assq fp nbindings)))
		     (free-params farg))
	(call-next-method))
      nbindings)))

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
	     ;; This test catches the situation where arg is a formal-type
	     ;; of the current theory, and farg is a subtype of a ground
	     ;; type.
	     (unless (and (fully-instantiated? (find-supertype farg))
			  (not (compatible? arg (find-supertype farg))))
	       (when *tc-match-strictly*
		 (push arg *tc-strict-matches*))
	       ;;(assert (fully-instantiated? arg))
	       (tc-match-set-binding fsubtype-binding arg)
	       bindings))
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
		   #+badassert
		   (assert (fully-instantiated? arg))
		   (tc-match-set-binding binding arg)
		   bindings)
		  (t (set-tc-match-binding arg (cdr binding) binding bindings)))
	    (let (;;(orig-bds (tc-match-subtypes-orig arg farg (copy-tree bindings)))
		  (bds (tc-match-subtypes arg farg bindings)))
	      ;; (unless (or (tc-eq orig-bds new-bds)
	      ;; 		  (and (name-expr? (predicate arg))
	      ;; 		       (name-expr? (predicate farg))
	      ;; 		       (not (eq (id (predicate arg)) (id (predicate farg)))))
	      ;; 		  (formal-type-name? (find-supertype farg)))
	      ;; 	(break "Why different?~%  arg = ~a~%  farg = ~a~%  orig: ~a~%  new:  ~a"
	      ;; 	       arg farg orig-bds new-bds))
	      bds))))))

(defun tc-match-subtypes (arg farg bindings)
  (let ((asup (find-adt-supertype arg))
	(fsup (find-adt-supertype farg)))
    (assert (and asup fsup))
    (if (or (and (formal-type-name? fsup)
		 (not (tc-eq asup fsup)))
	    (formal-type-name? asup))
	;; In this case, ignore the predicates, just match the formal-type to the arg
	(if (formal-type-name? fsup)
	    (tc-match* arg fsup bindings)
	    (tc-match* asup farg bindings))
	;; Otherwise, get the subtypes in sync, using the subtype-depth
	;; This keeps the system from doing, e.g., (int rat) -> (rat real) -> etc.
	;; Which misses opportunities to match on predicates.
	(let* ((adepth (subtype-depth arg))
	       (fdepth (subtype-depth farg))
	       (asub (if (> adepth fdepth)
			 (nth-supertype arg (- adepth fdepth))
			 arg))
	       (fsub (if (> fdepth adepth)
			 (nth-supertype farg (- fdepth adepth))
			 farg)))
	  (tc-match-subtypes* asub fsub bindings)))))
	  
(defmethod tc-match-subtypes* ((asub subtype) (fsub subtype) bindings)
  #+pvsdebug
  (assert (= (subtype-depth asub) (subtype-depth fsub)))
  (let ((pbind (tc-match* (predicate asub) (predicate fsub)
			  bindings)))
    (tc-match-subtypes* (supertype asub) (supertype fsub) (or pbind bindings))))
	  
(defmethod tc-match-subtypes* ((asub datatype-subtype) (fsub subtype) bindings)
  #+pvsdebug
  (assert (= (subtype-depth asub) (subtype-depth fsub)))
  (let* ((fsubdt (find-adt-supertype fsub))
	 (dbind (when fsubdt (tc-match* asub fsubdt bindings))))
    (or dbind bindings)))

(defmethod tc-match-subtypes* ((asub subtype) (fsub datatype-subtype) bindings)
  #+pvsdebug
  (assert (= (subtype-depth asub) (subtype-depth fsub)))
  (let* ((asubdt (find-adt-supertype asub))
	 (dbind (when asubdt (tc-match* asubdt fsub bindings))))
    (or dbind bindings)))

(defmethod tc-match-subtypes* ((asub datatype-subtype) (fsub datatype-subtype) bindings)
  #+pvsdebug
  (assert (= (subtype-depth asub) (subtype-depth fsub)))
  bindings)

(defmethod tc-match-subtypes* (atype ftype bindings)
  #+pvsdebug
  (assert (not (or (subtype? atype) (subtype? ftype))))
  (tc-match* atype ftype bindings))

(defmethod tc-match* ((arg expr-as-type) (farg type-name) bindings)
  (declare (type list bindings))
  (when bindings
    (let ((binding (assq (declaration farg) bindings)))
      (declare (type list binding))
      (if (and binding (cdr binding))
	  (when (compatible? arg (cdr binding))
	    bindings)
	  (if (supertype arg)
	      (call-next-method)
	      (tc-match* (domain (type (expr arg))) farg bindings))))))

(defmethod tc-match* ((arg expr-as-type) farg bindings)
  (declare (type list bindings))
  (when bindings
    (if (supertype arg)
	(call-next-method)
	(tc-match* (domain (type (expr arg))) farg bindings))))

(defmethod tc-match* (arg (farg expr-as-type) bindings)
  (declare (type list bindings))
  (when bindings
    (if (supertype farg)
	(call-next-method)
	(tc-match* arg (domain (type (expr farg))) bindings))))

(defmethod tc-match* ((arg expr-as-type) (farg expr-as-type) bindings)
  (declare (type list bindings))
  (when bindings
    (if (supertype arg)
	(if (supertype farg)
	    (call-next-method)
	    (tc-match* (supertype arg) (domain (type (expr farg))) bindings))
	(if (supertype farg)
	    (tc-match* (domain (type (expr arg))) (supertype farg) bindings)
	    (tc-match* (domain (type (expr arg))) (domain (type (expr farg))) bindings)))))

(defun tc-match-subtypes-orig (arg farg bindings)
  (or (let ((nbind (tc-match* (supertype arg) (supertype farg)
			      (tc-match* (predicate arg)
					 (predicate farg)
					 bindings))))
	(completed-tc-match-bindings nbind))
      (let ((nbind (tc-match* arg (supertype farg) bindings)))
	(completed-tc-match-bindings nbind))
      (tc-match* (supertype arg) farg bindings)))

(defun formal-type-name? (tn)
  (and (type-name? tn)
       (resolution tn)
       (formal-type-decl? (declaration tn))))

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
	     #+pvsdebug
	     (assert (fully-instantiated? arg))
	     (tc-match-set-binding fbinding arg)
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
	     #+pvsdebug
	     (assert (fully-instantiated? arg))
	     (tc-match-set-binding fbinding arg)
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
				  #+pvsdebug
				  (assert (fully-instantiated?
					   (if type? (type-value act) act)))
				  (tc-match-set-binding bind
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
	   #+badassert
	   (assert (fully-instantiated? arg))
	   (tc-match-set-binding binding arg)
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
	       (let ((val (or (type-value (car actuals)) (expr (car actuals)))))
		 #+pvsdebug
		 (assert (fully-instantiated? val))
		 (tc-match-set-binding binding val))
	       (tc-match-actuals* (cdr actuals) (cdr formals) bindings))
	      (t (when (if (type-value (car actuals))
			   (tc-eq (type-value (car actuals)) (cdr binding))
			   (tc-eq (expr (car actuals)) (cdr binding)))
		   (tc-match-actuals* (cdr actuals) (cdr formals) bindings)))))))

;;; tc-unify* is used when formals not in context occur on both sides.  The
;;; main reason for this is in name resolution.  For example, in sets_lemmas
;;; in the prelude there is the lemma
;;;     powerset_emptyset: LEMMA member(emptyset, powerset(a))

;;; member is in sets, with formal sets.T, and type [sets.T, set[sets.T] -> bool]
;;; the possible types for the emptyset arg includes set[sets.T], and the possible
;;; types for powerset(a) include set[set[sets_lemma.T]], which is
;;; fully-instantiated in this context.  tc-unify* then needs to find a way to unify
;;; the domain types of member (sets.T set[sets.T]) with the argument types
;;; (set[sets.T] set[set[sets_lemma.T]]), with bindings ((sets.T))

;;; It first unifies sets.T with set[sets.T], giving bindings ((sets.T
;;; . set[sets.T])) Note that the LHS occurs in the RHS.  Then it unifies the second
;;; arg, set[sets.T] with set[set[sets_lemma.T], which wants to bind sets.T to
;;; set[sets_lemma.T].  Since it's already bound, tc-unify is called recursively on
;;; set[sets.T] and set[sets_lemma.T] but with fresh bindings, to generate the
;;; instance sets[sets_lemma.T], which replaces the RHS of the first bindings.

(defun collect-formals (expr)
  (let ((formals nil))
    (mapobject #'(lambda (ex)
		   (when (and (name? ex)
			      (formal-decl? (declaration ex)))
		     (pushnew (declaration ex) formals)))
	       expr)
    (mapcar #'(lambda (f) (list (id f))) formals)))

(defun collect-domain-types (arg farg)
  (collect-domain-types* arg farg nil))

(defmethod collect-domain-types* ((arg funtype) (farg funtype) domain-pairs)
  (let ((adom (find-supertype-without-freevars (domain arg)))
	(fdom (find-supertype-without-freevars (domain farg))))
    ;; (unless (and (tc-eq adom (domain arg))
    ;; 		 (tc-eq fdom (domain farg)))
    ;;   (break "check this"))
    (if (assoc adom domain-pairs :test #'tc-eq)
	domain-pairs
	(acons adom fdom domain-pairs))))

(defmethod collect-domain-types* ((arg subtype) (farg subtype) domain-pairs)
  (let* ((asuptype1 (find-adt-supertype arg))
	 (asuptype2 (find-adt-supertype farg))
	 (suptype1 (if (subtype? asuptype1) (find-supertype arg) asuptype1))
	 (suptype2 (if (subtype? asuptype2) (find-supertype farg) asuptype2)))
    (collect-domain-types* suptype1 suptype2 domain-pairs)))

(defmethod collect-domain-types* ((arg subtype) (farg type-expr) domain-pairs)
  (collect-domain-types* (supertype arg) farg domain-pairs))

(defmethod collect-domain-types* ((arg type-expr) (farg subtype) domain-pairs)
  (collect-domain-types* arg (supertype farg) domain-pairs))
  
(defmethod collect-domain-types* ((arg tuple-or-struct-subtype)
				  (farg tuple-or-struct-subtype) domain-pairs)
  (collect-domain-types* (types arg) (types farg) domain-pairs))

(defmethod collect-domain-types* ((arg cotupletype) (farg cotupletype) domain-pairs)
  (collect-domain-types* (types arg) (types farg) domain-pairs))

(defmethod collect-domain-types* ((arg list) (farg list) domain-pairs)
  (if (or (null arg) (null farg))
      domain-pairs
      (collect-domain-types*
       (cdr arg) (cdr farg)
       (collect-domain-types* (car arg) (car farg) domain-pairs))))

(defmethod collect-domain-types* ((arg record-or-struct-subtype)
				  (farg record-or-struct-subtype) domain-pairs)
  (collect-domain-field-types (fields arg) (fields farg) domain-pairs))

(defun collect-domain-field-types (arg farg domain-pairs)
  (if (or (null arg) (null farg))
      domain-pairs
      (let* ((fld1 (car arg))
	     (fld2 (find (id fld1) farg :key #'id)))
	(if fld2
	    (collect-domain-field-types
	     (cdr arg) (remove fld2 (cdr farg))
	     (collect-domain-types* (type fld1) (type fld2) domain-pairs))
	    (collect-domain-field-types
	     (cdr arg) farg domain-pairs)))))

(defmethod collect-domain-types* ((arg dep-binding) (farg type-expr) domain-pairs)
  (collect-domain-types* (type arg) farg domain-pairs))

(defmethod collect-domain-types* ((arg type-expr) (farg dep-binding) domain-pairs)
  (collect-domain-types* arg (type farg) domain-pairs))

(defmethod collect-domain-types* ((arg dep-binding) (farg dep-binding) domain-pairs)
  (collect-domain-types* (type arg) (type farg) domain-pairs))

(defmethod collect-domain-types* ((arg type-expr) (farg type-expr) domain-pairs)
  domain-pairs)

(defmethod collect-domain-types* ((arg expr) (farg expr) domain-pairs)
  domain-pairs)

(defmethod collect-domain-types* ((arg actual) (farg actual) domain-pairs)
  domain-pairs)
