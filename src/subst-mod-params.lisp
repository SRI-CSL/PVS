;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst-mod-params.lisp -- performs substitution of actual parameters for
;;                          formals 
;; Author          : Sam Owre
;; Created On      : Thu Dec  9 13:10:41 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Tue May 25 17:58:34 2004
;; Update Count    : 74
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

(export '(make-resolution))

;;; Substitutes the actuals for the formals in an expression or
;;; type-expr.  The result is an expr with all resolutions fully
;;; resolved (except possibly the current module).  If nothing changed,
;;; then the original expr is returned, otherwise a copy is made.  The
;;; actuals of modinst may have name-exprs where a type-expr is
;;; expected; the method for type-name handles this case by creating a
;;; new type-name.

;;; Ultimately only names get substituted for.  There are three ways in which
;;; this could happen:
;;;  1. The name is a formal parameter of the specified modinst.
;;;  2. The name is declared in the theory being substituted for.
;;;  3. The actuals could be substituted for.

;;; Complications come about because of declarations which point to
;;; themselves, missing module instances, 


;;; This is the set of caches for subst-mod-params - it is a
;;; hash-table of hash-tables, indexed by module-instance.  This
;;; is initialized by reset-subst-mod-params-cache, and used and set by
;;; subst-mod-params.

(defvar *all-subst-mod-params-caches* nil)


;;; This is set by subst-mod-params to the cache found (or created) in
;;; looking up the modname in *all-subst-mod-params-caches*.

(defvar *subst-mod-params-cache* nil)
(defvar *subst-mod-params-eq-cache* nil)

;;; Flag indicating whether to update the cache.  Needed because the expr
;;; slot of an actual may be substituted for even if it has no type or
;;; resolution (because it is actually a type), but we still want it
;;; printed correctly.

(defvar *smp-dont-cache* nil)


;;; This indicates whether subst-mod-params should include actuals in a
;;; name whose resolution has had actuals substituted for it.  It is let
;;; to t by make-resolution, and used in subst-mod-params* (name).

(defvar *smp-include-actuals* nil)

(defvar *smp-have-real-bindings* nil)

(defvar *free-bindings* nil)

(defvar *smp-mappings* nil)

(defvar *subst-mod-params-module?* nil)

(defvar *subst-mod-free-params* nil)

(defvar *subst-mod-params-theory* nil)

(defvar *subst-mod-params-map-bindings*)

;;; This resets the *all-subst-mod-params-caches* hash table.  It is
;;; called from the parser whenever a newly parsed theory replaces the old
;;; theory, to ensure that the garbage collector can remove the objects.

(defun reset-subst-mod-params-cache ()
  (if *all-subst-mod-params-caches*
      (clrhash *all-subst-mod-params-caches*)
      (setq *all-subst-mod-params-caches*
	    (make-pvs-hash-table :strong-eq? t)))
;  (if *subst-mod-params-cache*
;      (clrhash *subst-mod-params-cache*)
;      (setq *subst-mod-params-cache*
;	    (make-pvs-hash-table :size 5)))
  )

(defun remove-subst-mod-params-cache ()
  (setq *all-subst-mod-params-caches* nil)
  (setq *subst-mod-params-cache* nil)
  (setq *subst-mod-params-eq-cache* nil))

(defun copy-subst-mod-params-cache ()
  (let ((new-hash (make-pvs-hash-table
		   :strong-eq? t
		   :size (floor (hash-table-size *all-subst-mod-params-caches*)
				1.5384616))))
    (maphash #'(lambda (modname hashes)
		 (setf (gethash modname new-hash)
		       (cons (copy (car hashes)) (copy (cdr hashes)))))
	     *all-subst-mod-params-caches*)
    new-hash))


;;; Gets the cache associated with the given modinst, or creates a new one
;;; and returns it if necessary.

(defun get-subst-mod-params-caches (modinst)
  (unless *all-subst-mod-params-caches*
    (reset-subst-mod-params-cache))
  (let ((caches (gethash modinst *all-subst-mod-params-caches*)))
    (or caches
	(let ((ncaches
	       (cons (make-pvs-hash-table :strong-eq? t)
		     (make-hash-table :test 'eq))))
	  (setf (gethash modinst *all-subst-mod-params-caches*) ncaches)
	  ncaches))))

(defmethod subst-theory-params (term (alist cons))
  ;;(assert (every #'(lambda (elt) (formal-decl? (car elt))) alist))
  (let ((new-alist (mapcan #'(lambda (elt)
			       (when (cdr elt)
				 (list (cons (car elt)
					     (if (actual? (cdr elt))
						 (cdr elt)
						 (mk-actual (cdr elt)))))))
		     alist))
	(theories (delete *current-theory*
			  (delete-duplicates
			   (mapcar #'(lambda (elt) (module (car elt)))
			     alist))))
	(sterm term))
    (dolist (th theories)
      (let* ((lib-id (get-lib-id th))
	     (thname
	      (mk-modname (id th)
		(mapcar #'(lambda (fp)
			    (or (cdr (assq fp new-alist))
				(let ((res (make-resolution fp
					     (mk-modname (id th)))))
				  (mk-res-actual
				   (if (type-decl? fp)
				       (mk-type-name (id fp) nil nil res)
				       (mk-name-expr (id fp) nil nil res))
				   th))))
		  (formals-sans-usings th))
		lib-id)))
	;; This is not necessarily true - we may be retypechecking within
	;; the library context, in which case it is already a
	;; library-theory.
	;; (assert (or lib-id (not (library-datatype-or-theory? th))))
	(setf sterm (subst-mod-params sterm thname th))))
    sterm))

(defmethod subst-theory-params (term (modinst modname))
  (subst-mod-params term modinst))


;;; The main entry point to subst-mod-params.

(defun subst-mod-params (obj modinst &optional theory)
  (assert *current-context*)
  (assert (modname? modinst))
  (assert (or (null theory) (eq (id theory) (id modinst))))
  (let* ((*subst-mod-params-theory* (or theory (get-theory modinst)))
	 (formals (formals-sans-usings *subst-mod-params-theory*))
	 (*subst-mod-free-params* nil))
    (if (or (mappings modinst)
	    (and (actuals modinst)
		 (or (some #'(lambda (ofp) (memq ofp formals))
			   (free-params obj))
		     (some #'(lambda (a)
			       (and (name-expr? (expr a))
				    (typep (declaration (expr a))
					   '(or module mod-decl
						formal-theory-decl
						theory-abbreviation-decl))))
			   (actuals modinst)))))
	(let* ((*generate-tccs* 'none)
	       (caches (get-subst-mod-params-caches modinst))
	       (*subst-mod-params-cache* (car caches))
	       (*subst-mod-params-eq-cache* (cdr caches))
	       (*smp-mappings* (mappings modinst))
	       (*subst-mod-params-map-bindings* nil) ; Collects map bindings
	       (init-bindings (initial-subst-mod-params-mappings
			       *subst-mod-params-theory*))
	       (bindings (make-subst-mod-params-bindings
			  modinst formals (actuals modinst) (mappings modinst)
			  init-bindings))
	       (nobj (subst-mod-params* obj modinst bindings)))
	  #+pvsdebug (assert (or (eq obj nobj) (not (tc-eq obj nobj))))
	  #+pvsdebug (assert (equal bindings
				    (pairlis formals (actuals modinst))))
	  #+pvsdebug (assert (or (typep nobj 'modname)
				 (fully-instantiated? nobj)))
	  ;; Note that it may be that nobj is tc-eq to obj, without being eq
	  ;; True even for strong-tc-eq.
	  (values nobj *subst-mod-params-map-bindings*))
	obj)))

(defun adt-modinst (modinst &optional theory)
  (let* ((th (or theory (get-theory modinst))))
    (if (rectype-theory? th)
	(adt-modinst* (positive-types th) (actuals modinst)
		      (formals-sans-usings th) modinst)
	modinst)))

(defun adt-modinst* (postypes acts formals modinst &optional nacts)
  (if (null acts)
      (let ((actuals (nreverse nacts)))
	(if (equal actuals (actuals modinst))
	    modinst
	    (change-class (copy modinst :actuals actuals)
			  'datatype-modname)))
      (adt-modinst* postypes (cdr acts) (cdr formals) modinst
		    (cons (if (and (not (formal-subtype-decl? (car formals)))
				   (member (car formals) postypes
					   :test #'same-id
					   :key #'(lambda (x)
						    (or (print-type x) x))))
			      (supertype-actual (car acts))
			      (car acts))
			  nacts))))

(defun supertype-actual (act)
  (let* ((aty (type-value act))
	 (ty (find-supertype aty)))
    (if (eq ty aty)
	act
	(mk-actual ty))))

(defmethod initial-subst-mod-params-mappings ((theory module))
  nil)

(defmethod initial-subst-mod-params-mappings ((theory theory-interpretation))
  (setq *subst-mod-params-map-bindings* (theory-mapping theory)))


;;; Create the formals to actuals bindings.  This would simply be a call
;;; to pairlis, but formal subtypes have an associated predicate that must
;;; be substituted for as well.

(defun make-subst-mod-params-bindings (modinst formals actuals mappings
					       bindings)
  (cond ((null formals)
	 #+pvsdebug (assert (every #'(lambda (d)
				       (typep (car d)
					      '(or declaration module)))
				   bindings))
	 (make-subst-mod-params-map-bindings modinst mappings bindings))
	(t (let ((pred-binding (make-subst-mod-params-pred-binding
				modinst (car formals) (car actuals) bindings))
		 (nbindings (make-subst-mod-params-binding
			     (car formals) (car actuals) bindings)))
	     (push (caar nbindings) *subst-mod-free-params*)
	     (when pred-binding
	       (push (car pred-binding) *subst-mod-free-params*))
	     (make-subst-mod-params-bindings
	      modinst
	      (cdr formals)
	      (cdr actuals)
	      mappings
	      (if pred-binding
		  (cons pred-binding nbindings)
		  nbindings))))))

(defmethod make-subst-mod-params-binding ((formal formal-theory-decl) actual
					  bindings)
  (acons formal actual
	 (let* ((thname (if (theory-abbreviation-decl?
			     (declaration (expr actual)))
			    (get-theory-alias (expr actual))
			    (expr actual)))
		(theory (generated-theory (declaration (expr actual))))
		(interpreted-theory (generated-theory formal))
		(source-theory (get-theory* (id (theory-name formal))
					    (or (library (theory-name formal))
						(get-lib-id (module formal)))))
		(pre-bindings (make-subst-mod-params-bindings
			       thname
			       (formals-sans-usings source-theory)
			       (actuals thname)
			       nil
			       (extended-mappings thname theory)))
		(inv-mappings
		 (inverse-mapping (theory-mapping interpreted-theory)
				  interpreted-theory)))
	   (assert interpreted-theory)
	   ;; Now we compose the inverses of the mappings of the interpreted
	   ;; theory and the source 
	   (append (compose-mappings inv-mappings
				     (theory-mapping theory))
		   (compose-mappings inv-mappings pre-bindings)
		   pre-bindings
		   bindings))))

(defun get-theory-alias (thabbr)
  (let* ((thabbr-decl (declaration thabbr))
	 (thname (theory-name thabbr-decl))
	 (containing-theory (module thabbr-decl)))
    (assert (fully-instantiated? (module-instance thabbr)))
    (subst-mod-params thname (module-instance thabbr) containing-theory)))

(defmethod generated-theory ((th module))
  th)

(defmethod generated-theory ((th theory-abbreviation-decl))
  (get-theory (theory-name th)))

;;; mapping1 is an alist of declaration to declaration mappings
;;; mapping2 is an alist of declaration to rhs-mappings
(defun compose-mappings (mapping1 mapping2 &optional composition)
  (if (null mapping1)
      (nreverse composition)
      (let ((map2 (assq (cdar mapping1) mapping2)))
	(compose-mappings
	 (cdr mapping1) mapping2
	 (if map2
	     (acons (caar mapping1) (cdr map2) composition)
	     composition)))))

;; Only works nicely for dotted pairs
(defun reverse-alist (alist)
  (mapcar #'(lambda (elt) (cons (cdr elt) (car elt))) alist))

(defun inverse-mapping (mapping theory &optional inverse)
  (if (null mapping)
      (nreverse inverse)
      (inverse-mapping
       (cdr mapping)
       theory
       (let* ((map (car mapping))
	      (val (cdr map)))
	 (typecase val
	   (mapping-rhs (if (type-value val)
			    (if (and (type-name? (type-value val))
				     (eq (module
					  (declaration (type-value val)))
					 theory))
				(acons (declaration (type-value val)) (car map)
				       inverse)
				inverse)
			    (if (and (name-expr? (expr val))
				     (eq (module (declaration (expr val)))
					 theory))
				(acons (declaration (expr val)) (car map)
				       inverse)
				inverse)))
	   (declaration (acons val (car map) inverse))
	   (t (break "What else is there?")))))))


;;; extended-mappings creates a bindings alist from lhs to rhs.
;;; There are a number of places the bindings can come from:
;;; 1. the given theory name (thname)
;;;    a. If it has a theory parameter, it needs to have it's bindings included
;;;    b. If it has a theory mapping, also needs bindings.
;;; 2. the theory may already be an interpretation, in which case those
;;;    bindings need to be brought out - (mapping theory) does this.

(defun extended-mappings (thname theory)
  (let ((all-mappings (nconc (mapcar #'(lambda (m)
					 (cons (declaration (lhs m)) (rhs m)))
			       (mappings thname))
			     (theory-mapping theory))))
    (dolist (amap all-mappings)
      (when (module? (car amap))
	(setq all-mappings
	      (append (extended-mappings (expr (cdr amap)) (car amap))
		      all-mappings))))
    all-mappings))


;;; Called from make-interpreted-copy
(defun get-interpreted-mapping (theory interpretation theory-name)
  (let* ((*subst-mod-params-map-bindings* nil)
	 (mapping (make-subst-mod-params-map-bindings
		   theory-name (mappings theory-name) nil))
	 (int-decls (when interpretation (all-decls interpretation))))
    (when interpretation
      (dolist (decl (interpretable-declarations theory))
	(unless (assq decl mapping)
	  (let ((fdecl (find decl int-decls
			     :test #'(lambda (x y)
				       (and (declaration? y)
					    (same-id x y))))))
	    (unless (eq decl fdecl)
	      (push (cons decl fdecl) mapping))))))
    #+pvsdebug (assert (every #'cdr mapping))
    mapping))

(defmethod theory-mapping (module)
  (declare (ignore module))
  nil)

(defmethod make-subst-mod-params-binding (formal actual bindings)
  (acons formal actual bindings))


(defmethod make-subst-mod-params-pred-binding (modinst (formal formal-subtype-decl)
					       actual bindings)
  (let* ((subtype (type-value actual))
	 (sformal (when (typep (type-expr formal) 'type-name)
		    (cdr (assoc (declaration (type-expr formal)) bindings)))))
    (if sformal
	(cons (find-if #'(lambda (c) (typep c 'const-decl))
		(generated formal))
	      (make-instance 'actual
		:expr (subtype-pred subtype (type-value sformal))))
	(cons (find-if #'(lambda (c) (typep c 'const-decl))
		(generated formal))
	      (make-instance 'actual
		:expr (subtype-pred subtype
				    (subst-mod-params* 
				     (supertype (type-value formal))
				     modinst bindings)))))))

(defmethod make-subst-mod-params-pred-binding (modinst formal actual bindings)
  (declare (ignore modinst formal actual bindings))
  nil)

(defun make-subst-mod-params-map-bindings (modinst mappings bindings)
  (if (null mappings)
      (reverse bindings)
      (let* ((decl (declaration (car (resolutions (lhs (car mappings))))))
	     (bind-rhs (rhs (car mappings)))
	     (nbindings (make-subst-mod-params-map-bindings*
			 decl bind-rhs bindings)))
	#+pvsdebug (assert (every #'(lambda (d)
				      (typep (car d) '(or declaration module)))
				  nbindings))
	(make-subst-mod-params-map-bindings
	 modinst
	 (cdr mappings)
	 nbindings))))

(defmethod make-subst-mod-params-map-bindings* ((theory module) rhs bindings)
  (let* ((thname (theory-ref (expr rhs)))
	 (pre-bindings (make-subst-mod-params-bindings
			thname (formals-sans-usings theory)
			(actuals thname)
			nil
			(extended-mappings thname theory))))
    #+pvsdebug (assert (every #'(lambda (d)
				  (typep (car d) '(or declaration module)))
			      pre-bindings))
    (setq *subst-mod-params-map-bindings*
	  (acons theory rhs
		 (append pre-bindings *subst-mod-params-map-bindings*)))
    (acons theory rhs
	   (append pre-bindings bindings))))

(defmethod make-subst-mod-params-map-bindings* ((decl mod-decl) rhs bindings)
  (let* ((thname (theory-ref (expr rhs)))
	 (interpreted-theory (generated-theory decl))
	 (source-theory (get-theory (theory-name decl)))
	 (pre-bindings (make-subst-mod-params-bindings
			thname (formals-sans-usings source-theory)
			(actuals thname)
			nil
			(extended-mappings thname interpreted-theory))))
    #+pvsdebug (assert (every #'(lambda (d)
				  (typep (car d) '(or declaration module)))
			      pre-bindings))
    (setq *subst-mod-params-map-bindings*
	  (acons decl rhs
		 (append pre-bindings *subst-mod-params-map-bindings*)))
    (acons decl rhs
	   (append (mapcar #'(lambda (x)
			       (let ((interp
				      (assq (car x)
					    (theory-mapping interpreted-theory))))
				 (if interp
				     (cons (cdr interp) (cdr x))
				     x)))
		     pre-bindings)
		   bindings))))



(defmethod make-subst-mod-params-map-bindings* (decl rhs bindings)
  (setq *subst-mod-params-map-bindings*
	(acons decl rhs *subst-mod-params-map-bindings*))
  (assert (typep decl '(or declaration module)))
  (assert (every #'(lambda (d) (typep (car d) '(or declaration module)))
		   bindings))
  (acons decl rhs bindings))

(defvar *caching-subst-mod-params?* t)

;;; This is the around method, which gets the expression from the cache,
;;; if possible, and otherwise calls the next method and adds the result
;;; to the cache.  In general, we try not to copy, so we check whether the
;;; hashed object is tc-eq to the original object, so we can return the
;;; original object.  Unfortunately, it is possible for an uninstantiated
;;; expression to be tc-eq to its instantiated form, so we also add a call
;;; to fully-instantiated? - we should try to fix this.  This can happen
;;; through projection applications and field applications, where tc-eq
;;; only cares about the argument type.

(defmethod subst-mod-params* :around (obj modinst bindings)
  (declare (type hash-table *subst-mod-params-cache*))
  (or (gethash obj *subst-mod-params-eq-cache*)
      (let ((hobj (gethash obj *subst-mod-params-cache*)))
	(or hobj
	    (let ((nobj (if (and (null (mappings modinst))
				 (not (some #'(lambda (b)
						(formal-theory-decl? (car b)))
					    bindings))
				 (no-matching-free-params
				  obj *subst-mod-free-params*))
			    obj
			    (call-next-method))))
	      (when (and (typep obj 'type-expr)
			 (typep nobj 'type-expr))
		(when (print-type obj)
		  (if (or (tc-eq obj nobj)
			  (and (free-params obj)
			       (tc-match nobj obj
					 (mapcar #'list (free-params obj)))))
		      (let* ((stype (subst-mod-params* (print-type obj)
						       modinst bindings))
			     (pte (or (print-type stype) stype)))
			(unless (tc-eq pte nobj)
			  (setq nobj (lcopy nobj :print-type pte))))
		      (setq nobj (lcopy nobj :print-type nil))))
		(when (from-conversion obj)
		  (let ((fconv (subst-mod-params* (from-conversion obj)
						  modinst bindings)))
		    (setq nobj (lcopy nobj :from-conversion fconv)))))
	      (if (and (null (freevars nobj))
		       (relatively-fully-instantiated? nobj))
		  (setf (gethash obj *subst-mod-params-cache*) nobj)
		  (setf (gethash obj *subst-mod-params-eq-cache*) nobj))
	      #+pvsdebug
	      (assert (every #'(lambda (fv)
				 (or (binding? fv)
				     (member fv (freevars obj)
					     :test #'same-declaration)
				     (member fv (freevars modinst)
					     :test #'same-declaration)))
			     (freevars nobj)))
	      nobj)))))

(defun no-matching-free-params (obj frees)
  (not (some #'(lambda (b) (memq b frees))
	     (free-params obj))))

(defun relatively-fully-instantiated? (obj &optional
					   (freeparms *subst-mod-free-params*))
  (let ((frees (set-difference (free-params obj) freeparms)))
    (or (null frees)
	(let ((formals (formals-sans-usings (current-theory))))
	  (every #'(lambda (fp) (memq fp formals)) frees)))))

(defmethod subst-mod-params* :around ((obj expr) modinst bindings)
  (let ((frees (free-parameters obj)))
    (cond ((or frees
	       bindings
	       (mappings modinst))
	   (let ((nobj (call-next-method)))
	     nobj))
	  (t #+pvsdebug (let ((nobj (call-next-method)))
			  (assert (eq nobj obj)))
	     obj))))

;;; Theory 

(defmethod subst-mod-params* ((th module) modinst bindings)
  (with-slots (formals assuming theory exporting) th
    (let* ((*subst-mod-params-module?* t)
	   (rformals (remove-if #'(lambda (d) (assq d bindings)) formals))
	   (rassuming (remove-if
			  #'(lambda (d) (substituted-map-decl d bindings))
			assuming))
	   (rtheory (remove-if
			#'(lambda (d) (substituted-map-decl d bindings))
		      theory)))
      (lcopy th
	:formals (subst-mod-params* rformals modinst bindings)
	:assuming (remove-if #'null
		    (subst-mod-params* rassuming modinst bindings))
	:theory (append (create-importings-for-bindings bindings)
			(remove-if #'null
			  (subst-mod-params* rtheory modinst bindings)))
	:exporting (subst-mod-params* exporting modinst bindings)))))

(defun substituted-map-decl (d bindings)
  (or (tcc? d)
      (and (formula-decl? d)
	   (nonempty-type-decl? (generated-by d))
	   (exists-expr? (definition d))
	   (tc-eq (expression (definition d)) *true*))
      (and (assq d bindings)
	   (not (mod-decl? d))
	   (find d *smp-mappings*
		 :key #'(lambda (m)
			  (and (mapping-subst? m)
			       (declaration (lhs m))))))))

(defun create-importings-for-bindings (bindings &optional importings)
  (if (null bindings)
      (nreverse importings)
      (let ((importing (create-importing-for-binding
			(caar bindings) (cdar bindings))))
	(create-importings-for-bindings
	 (cdr bindings)
	 (if (and importing
		  (not (member importing importings :test #'tc-eq)))
	     (cons importing importings)
	     importings)))))

(defmethod create-importing-for-binding ((decl formal-theory-decl) theory-name)
  (make-instance 'importing
    :theory-name theory-name))

(defmethod create-importing-for-binding ((decl mod-decl) theory-name)
  (unless (mapping-rhs-rename? theory-name)
    (make-instance 'importing
      :theory-name theory-name)))

(defmethod create-importing-for-binding (decl expr)
  (declare (ignore decl expr))
  nil)
  

(defmethod subst-mod-params* ((exp exporting) modinst bindings)
  (with-slots (names but-names) exp
    (lcopy exp
      :names (subst-mod-params* names modinst bindings)
      :but-names (subst-mod-params* but-names modinst bindings))))

(defmethod subst-mod-params* ((imp importing) modinst bindings)
  (with-slots (theory-name) imp
    (lcopy imp
      :theory-name (subst-mod-params* theory-name modinst bindings))))

(defmethod subst-mod-params* ((decl theory-abbreviation-decl) modinst bindings)
  (with-slots (theory-name) decl
    (lcopy decl
      :theory-name (subst-mod-params* theory-name modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((mn modname) modinst bindings)
  (with-slots (id actuals) mn
    (let ((entry (assoc id bindings
			:key #'(lambda (y)
				 (if (typep y
					    '(or module mod-decl
						 formal-theory-decl
						 theory-abbreviation-decl))
				     (id y)
				     ;; An id that can't possibly appear
				     ;; Note that nil won't work here.
				     '||)))))
      (if entry
	  (if (modname? (cdr entry))
	      (cdr entry)
	      ;; Binding could be an actual or mapping rhs
	      (let ((name (expr (cdr entry))))
		(mk-modname (id name)
		  (actuals name)
		  (library name)
		  (mappings name))))
	  (if (eq id (id modinst))
	      (if (library modinst)
		  modinst
		  (lcopy modinst :library (library mn)))
	      (let ((nacts (subst-mod-params* actuals modinst bindings)))
		(lcopy mn :actuals nacts)))))))

(defmethod subst-mod-params* ((decl declaration) modinst bindings)
  (declare (ignore modinst bindings))
  (lcopy decl :generated-by nil))

(defmethod subst-mod-params* ((decl type-decl) modinst bindings)
  (with-slots (type-value contains) decl
    (let ((map (find decl *smp-mappings*
		     :key #'(lambda (m)
			      (and (mapping-rename? m)
				   (declaration (lhs m)))))))
      (cond (map
	     (copy decl :id (id (expr (rhs map))) :semi t
		   :generated-by nil))
	    ((assq decl bindings)
	     (let ((ndecl (change-class (copy decl) 'type-eq-decl
					:type-expr (cdr (assq decl bindings))
					:generated-by nil)))
	       (setf (semi ndecl) t)
	       ndecl))
	    (t (let* ((val (when (theory-interpretation? (module decl))
			     (cdr (assq (generated-by-decl (module decl))
					bindings)))))
		 (if val
		     (let* ((thinst (expr val)) ; val is an actual
			    (ndecl (find (id decl)
					 (all-decls (get-theory thinst))))
			    (res (make-resolution ndecl thinst)))
		       (type res))
		     (lcopy decl
		       :type-value (subst-mod-params* type-value modinst
						      bindings)
		       :generated-by nil))))))))

(defmethod subst-mod-params* ((decl type-def-decl) modinst bindings)
  (with-slots (type-value type-expr contains) decl
    (lcopy decl
      :type-value (subst-mod-params* type-value modinst bindings)
      :type-expr (subst-mod-params* type-expr modinst bindings)
      :contains (subst-mod-params* contains modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl formal-theory-decl) modinst bindings)
  (with-slots (theory-name) decl
    (lcopy decl
      :theory-name (subst-mod-params* theory-name modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl mod-decl) modinst bindings)
  (with-slots (modname) decl
    (let ((adecl (cdr (assq decl bindings))))
      (if adecl
 	  (if (mapping-rhs-rename? adecl)
 	      (lcopy decl
 		:id (id (expr adecl)))
	      (make-instance 'importing
		:theory-name (expr adecl))
;; 	      (lcopy decl
;; 		:modname (module-instance (expr adecl))
;; 		:generated-by nil)
	      )
	  (lcopy decl
	    :modname (subst-mod-params* modname modinst bindings)
	    :generated-by nil)))))

(defmethod subst-mod-params* ((decl var-decl) modinst bindings)
  (with-slots (declared-type) decl
    (lcopy decl
      :declared-type (subst-mod-params* declared-type modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl const-decl) modinst bindings)
  (with-slots (formals declared-type type definition def-axiom) decl
    (let ((map (find decl *smp-mappings*
		     :key #'(lambda (m)
			      (and (or (mapping-rename? m)
				       ;;(mapping-def? m)
				       )
				   (declaration (lhs m)))))))
      (cond ((mapping-def? map)
	     (copy decl
	       :formals (subst-mod-params* formals modinst bindings)
	       :definition (subst-mod-params* (expr (rhs map)) modinst bindings)
	       :type (subst-mod-params* type modinst bindings)
	       :declared-type (subst-mod-params* declared-type modinst bindings)
	       :generated-by nil
	       :semi t))
	    ((mapping-rename? map)
	     (copy decl
	       :id (id (expr (rhs map)))
	       :formals (subst-mod-params* formals modinst bindings)
	       :type (subst-mod-params* type modinst bindings)
	       :declared-type (subst-mod-params* declared-type modinst bindings)
	       :generated-by nil
	       :semi t))
	    ((assq decl bindings)
	     (let ((nformals (subst-mod-params* formals modinst bindings)))
	       (copy decl
		 :formals nformals
		 :definition (if nformals
				 (make-applications
				  (expr (cdr (assq decl bindings)))
				  (mapcar #'(lambda (fms)
					      (mapcar #'mk-name-expr fms))
				    nformals))
				 (cdr (assq decl bindings)))
		 :type (subst-mod-params* type modinst bindings)
		 :declared-type (subst-mod-params* declared-type modinst bindings)
		 :generated-by nil
		 :semi t)))
	    (t (lcopy decl
		 :formals (subst-mod-params* formals modinst bindings)
		 :declared-type (subst-mod-params* declared-type modinst bindings)
		 :type (subst-mod-params* type modinst bindings)
		 :definition (subst-mod-params* definition modinst bindings)
		 :def-axiom (subst-mod-params* def-axiom modinst bindings)
		 :generated-by nil
		 :semi t))))))

(defmethod subst-mod-params* ((decl def-decl) modinst bindings)
  (with-slots (formals declared-type definition declared-measure ordering) decl
    (lcopy decl
      :formals (subst-mod-params* formals modinst bindings)
      :declared-type (subst-mod-params* declared-type modinst bindings)
      :definition (subst-mod-params* definition modinst bindings)
      :declared-measure (subst-mod-params* declared-measure modinst bindings)
      :ordering (subst-mod-params* ordering modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl formula-decl) modinst bindings)
  (with-slots (definition) decl
    (let ((ndef (subst-mod-params* definition modinst bindings)))
      (unless (and (axiom? decl)
		   (not (find-uninterpreted ndef modinst
					    *subst-mod-params-theory*
					    *subst-mod-params-map-bindings*)))
	(lcopy decl
	  :definition ndef
	  :generated-by nil)))))

(defmethod subst-mod-params* ((decl subtype-judgement) modinst bindings)
  (with-slots (declared-subtype) decl
    (lcopy decl
      :declared-subtype (subst-mod-params* declared-subtype modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl name-judgement) modinst bindings)
  (with-slots (name) decl
    (lcopy decl
      :name (subst-mod-params* name modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl application-judgement) modinst bindings)
  (with-slots (name) decl
    (lcopy decl
      :name (subst-mod-params* name modinst bindings)
      :generated-by nil)))

(defmethod subst-mod-params* ((decl conversion-decl) modinst bindings)
  (with-slots (expr) decl
    (lcopy decl
      :expr (subst-mod-params* expr modinst bindings)
      :generated-by nil)))


;;; Type Expressions

(defmethod subst-mod-params* ((list list) modinst bindings)
  (let ((nlist (if (some #'binding? list)
		   (subst-mod-params-bindings list modinst bindings)
		   (subst-mod-params-list list modinst bindings))))
    (if (equal list nlist)
	list
	nlist)))

(defun subst-mod-params-list (list modinst bindings &optional rlist)
  (if (null list)
      (nreverse rlist)
      (let* ((nelt (subst-mod-params* (car list) modinst bindings)))
	(subst-mod-params-list (cdr list) modinst bindings
			       (cons nelt rlist)))))

(defmethod subst-mod-params* ((type type-name) modinst bindings)
  (let* ((res (car (resolutions type)))
	 (decl (declaration res))
	 (act (cdr (assq decl bindings))))
    #+pvsdebug (assert (or (null act) (fully-instantiated? (type-value act))))
    (if act
	(type-value act) ;; sufficient since we know
	;; type name was found on the bindings, and the corresponding
        ;; actual is there.  Here we actally do the substitution.
	(if (and *subst-mod-params-module?*
		 (eq (module decl) (get-theory modinst)))
	    type
	    (or (mapped-theory-value (module decl) type modinst bindings)
		(let* ((mi (module-instance res))
		       (nacts (subst-mod-params*
			       (actuals mi) modinst bindings)))
		  #+pvsdebug (assert (not (and nacts
					       (eq (id mi) (id modinst)))))
		  #+pvsdebug (assert (fully-instantiated? nacts))
		  (if nacts
		      (if (eq (actuals mi) nacts)
			  type
			  ;; If modinst has a library, but mi does not, we
			  ;; may have to add a library to mi.  If (module
			  ;; decl) is a library theory, definitely need it,
			  ;; but how do we know it is the same library?
			  (let* ((lib
				  (when (and (library-datatype-or-theory?
					      (module decl))
					     (not (library-datatype-or-theory?
						   (current-theory))))
				    (car (rassoc (lib-ref (module decl))
						 (current-library-alist)
						 :test #'equal))))
				 (nmodinst (copy mi :actuals nacts
						 :library lib)))
			    #+pvsdebug
			    (assert (or lib
					(not (library-datatype-or-theory?
					      (module decl)))
					(from-prelude-library? decl)
					(library-datatype-or-theory?
					 (current-theory))
					(file-equal (lib-ref (module decl))
						    *pvs-context-path*)))
			    (subst-mod-params-type-name type nmodinst
							bindings)))
		      (if (eq (id mi) (id modinst))
			  (subst-mod-params-type-name type modinst bindings)
			  type))))))))

(defmethod mapped-theory-value ((th theory-interpretation)
				type modinst bindings)
  (let ((val (cdr (assq (generated-by-decl th) bindings))))
    (when val
      (let* ((thinst (expr val))	; val is an actual
	     (ndecl (find (id type) (all-decls (get-theory thinst)) :key #'id))
	     (res (make-resolution ndecl thinst)))
	(type res)))))

(defmethod mapped-theory-value ((th module) type modinst bindings)
  nil)

;;; just goes ahead and copies the type with the new module
;;; instance in the newly created resolution
(defun subst-mod-params-type-name (type modinst bindings)
  (assert (eq (id modinst) (id (module-instance (resolution type)))))
  (let* ((res (car (resolutions type)))
	 (decl (declaration res))
	 (nres (mk-resolution decl modinst nil))
	 (ntype (copy type
		  :resolutions (list nres)
		  :actuals (actuals (module-instance nres))
		  :print-type nil))
	 (rtype (typecase decl
		  (type-def-decl
		   (subst-mod-params* (copy (type-value decl) :print-type nil)
				      modinst bindings))
		  (type-decl ntype)
		  (mapping
		   (assert (type-value (rhs decl)))
		   (type-value (rhs decl)))
		  (t (error "bad type-decl")))))
    (setf (type nres) rtype)
    #+pvsdebug (assert (subsetp (free-params ntype) (free-params modinst)))
    ;;(assert (fully-instantiated? ntype))
    (adt-expand-positive-subtypes ntype)))

;;; Given a type such as list[int], this creates the datatype-subtype
;;;  {x: list[number] | every(...)(x)}

(defun adt-expand-positive-subtypes (type)
  (if (fully-instantiated? type)
      (gensubst type
	#'adt-expand-positive-subtypes!
	#'adt-expand-positive-subtypes?)
      type))

(defmethod adt-expand-positive-subtypes? ((ex type-name))
  #+lucid (restore-adt ex)
  (and (adt ex)
       (actuals ex)
       (some #'(lambda (a) (subtype? (type-value a))) (actuals ex))
       (positive-types (adt ex))
       (not (every #'null (positive-types (adt ex))))))

(defmethod adt-expand-positive-subtypes? (ex)
  (declare (ignore ex))
  nil)

(defmethod adt-expand-positive-subtypes! ((type type-name))
  (let ((ntype (find-supertype type)))
    (if (eq ntype type)
	type
	(let* ((bd (make-new-bind-decl ntype))
	       (var (make-variable-expr bd))
	       (preds (make!-conjunction*
		       (adt-compatible-preds ntype type var nil)))
	       (pred (make!-lambda-expr (list bd) preds))
	       (stype (make-instance 'datatype-subtype
			:supertype ntype
			:predicate pred
			:declared-type type)))
	  ;;(setf (tcc-status pred) (list (cons (type pred) 'none)))
	  (setf (print-type stype) type)
	  stype))))


(defmethod subst-mod-params* ((type type-application) modinst bindings)
  (let ((ntype (subst-mod-params* (type type) modinst bindings))
	(nparms (subst-mod-params* (parameters type) modinst bindings)))
    (lcopy type :type ntype :parameters nparms)))

(defmethod subst-mod-params* ((type dep-binding) modinst bindings)
  (let ((ntype (subst-mod-params* (type type) modinst bindings))
	(ndeclared-type (subst-mod-params* (declared-type type)
					   modinst bindings)))
    (if (and (eq (type type) ntype)
	     (eq (declared-type type) ndeclared-type))
	type
	(let ((ndep (copy type
		       :type ntype
		       :declared-type ndeclared-type)))
 	  (setf (resolutions ndep)
 		(list (mk-resolution ndep (current-theory-name) ntype)))
	  ndep))))

(defmethod subst-mod-params* ((type expr-as-type) modinst bindings)
  (let* ((ntype (call-next-method))
	 (nexpr (subst-mod-params* (expr ntype) modinst bindings)))
    (lcopy ntype :expr nexpr)))

(defmethod subst-mod-params* ((type datatype-subtype) modinst bindings)
  (lcopy (call-next-method)
    :declared-type (subst-mod-params* (declared-type type) modinst bindings)))

(defmethod subst-mod-params* ((type subtype) modinst bindings)
  (let ((act (cdr (assoc type bindings
			 :test #'formal-subtype-binding-match))))
    (if act
	(type-value act)
	(let ((stype (subst-mod-params* (supertype type) modinst bindings))
	      (spred (subst-mod-params* (predicate type) modinst bindings)))
	  (if (everywhere-true? spred)
	      stype
	      (lcopy type
		:supertype stype
		:predicate (if (eq spred (predicate type))
			       spred
			       (pseudo-normalize spred))))))))

(defmethod formal-subtype-binding-match (subtype (formal formal-subtype-decl))
  (tc-eq subtype (type-value formal)))

(defmethod formal-subtype-binding-match (subtype formal)
  (declare (ignore subtype formal))
  nil)

(defmethod subst-mod-params* ((type struct-sub-recordtype) modinst bindings)
  (let ((act (cdr (assoc type bindings
			 :test #'formal-struct-subtype-binding-match))))
    (if act
	(type-value act)
	(let ((ntype (subst-mod-params* (type type) modinst bindings))
	      (nfields (subst-mod-params* (fields type) modinst bindings)))
	  (lcopy type
	    :type ntype
	    :fields nfields)))))

(defmethod formal-struct-subtype-binding-match (type (formal formal-struct-subtype-decl))
  (tc-eq type (type-value formal)))

(defmethod formal-struct-subtype-binding-match (type formal)
  (declare (ignore type formal))
  nil)


(defmethod subst-mod-params* ((type funtype) modinst bindings)
  (with-slots ((dom domain) (ran range)) type
    (let* ((ftypes (list dom ran))
	   (ntypes (subst-mod-params-type-list ftypes modinst bindings)))
      (if (equal ftypes ntypes)
	  type
	  (copy type
	    :domain (car ntypes)
	    :range (cadr ntypes))))))

(defmethod subst-mod-params* ((type tupletype) modinst bindings)
  (let ((ntypes (subst-mod-params-type-list (types type) modinst bindings)))
    (if (equal ntypes (types type))
	type
	(copy type :types ntypes))))

(defmethod subst-mod-params* ((type struct-sub-tupletype) modinst bindings)
  (let ((act (cdr (assoc type bindings
			 :test #'formal-struct-subtype-binding-match))))
    (if act
	(type-value act)
	(let ((ntype (subst-mod-params* (type type) modinst bindings))
	      (ntypes (subst-mod-params* (types type) modinst bindings)))
	  (lcopy type
	    :type ntype
	    :types ntypes)))))

(defmethod subst-mod-params* ((type cotupletype) modinst bindings)
  (let ((ntypes (subst-mod-params-type-list (types type) modinst bindings)))
    (if (equal ntypes (types type))
	type
	(copy type :types ntypes))))

(defun subst-mod-params-type-list (types modinst bindings &optional ntypes)
  (if types
      (let ((ntype (subst-mod-params* (car types) modinst bindings)))
	(subst-mod-params-type-list
	 (if (typep ntype 'dep-binding)
	     (substit (cdr types) (acons (car types) ntype nil))
	     (cdr types))
	 modinst bindings (cons ntype ntypes)))
      (nreverse ntypes)))

(defmethod subst-mod-params* ((type recordtype) modinst bindings)
  (with-slots (fields) type
    (let ((nfields (subst-mod-params-fields fields modinst bindings)))
      (if (equal nfields fields)
	  type
	  (let ((ntype (copy type
			 :fields (sort-fields nfields
					      (dependent-fields? nfields)))))
	    ntype)))))

(defun subst-mod-params-fields (fields modinst bindings &optional nfields)
  (if fields
      (let ((nfield (subst-mod-params* (car fields) modinst bindings)))
	(subst-mod-params-fields
	 (substit (cdr fields) (acons (car fields) nfield nil))
	 modinst bindings (cons nfield nfields)))
      (nreverse nfields)))

(defmethod subst-mod-params* ((field field-decl) modinst bindings)
  (with-slots (type declared-type) field
    (let ((ntype (subst-mod-params* type modinst bindings)))
      (if (eq ntype type)
	  field
	  (let ((ndt (subst-mod-params* declared-type modinst bindings)))
	    (lcopy field :type ntype :declared-type ndt))))))

(defmethod subst-mod-params* ((conv conversion-result) modinst bindings)
  (with-slots (expr) conv
    (let ((nexpr (subst-mod-params* expr modinst bindings)))
      (lcopy conv :expr nexpr))))

;;; Expressions

(defmethod subst-mod-params* ((expr name-expr) modinst bindings)
  (declare (ignore modinst))
  (let* ((decl (declaration expr))
	 (act (cdr (assq decl bindings))))
    (if act
	(if (declaration? act)
	    (mk-name-expr (id act) nil nil
			  (mk-resolution act
			    (mk-modname (id (module act)))
			    (type act)))
	    (expr act))
	(let ((nexpr (call-next-method)))
	  (if (eq nexpr expr)
	      expr
	      (let ((ntype (type (resolution nexpr))))
		(lcopy nexpr :type ntype)))))))

(defmethod subst-mod-params* ((expr adt-name-expr) modinst bindings)
  (let ((nexpr (call-next-method)))
    (cond ((eq expr nexpr)
	   expr)
	  ((adt-name-expr? nexpr)
	   (setf (adt-type nexpr)
		 (subst-mod-params* (adt-type expr) modinst bindings))
	   nexpr)
	  (t nexpr))))

(defmethod subst-mod-params* ((expr constructor-name-expr) modinst bindings)
  (declare (ignore modinst bindings))
  (let ((nexpr (call-next-method)))
    (cond ((eq nexpr expr)
	   expr)
	  ((constructor-name-expr? nexpr)
	   (unless (adt (adt nexpr))
	     (restore-adt-slot (adt nexpr)))
	   (lcopy nexpr
	     :recognizer-name nil
	     :accessor-names 'unbound))
	  (t nexpr))))

(defmethod subst-mod-params* ((expr recognizer-name-expr) modinst bindings)
  (declare (ignore modinst bindings))
  (let ((nexpr (call-next-method)))
    (cond ((eq nexpr expr)
	   expr)
	  ((recognizer-name-expr? nexpr)
	   (lcopy nexpr
	     :constructor-name nil
	     :unit? 'unbound))
	  (t nexpr))))

(defmethod subst-mod-params* ((bd binding) modinst bindings)
  (let ((ntype (subst-mod-params* (type bd) modinst bindings))
	(ndeclared-type (unless *generating-mapped-axiom-tcc*
			  (subst-mod-params* (declared-type bd)
					     modinst bindings))))
    (if (and (eq (type bd) ntype)
	     (eq (declared-type bd) ndeclared-type))
	bd
	(let ((nbd (copy bd
		     :type ntype
		     :declared-type (or ndeclared-type
					(print-type ntype)
					ntype))))
	  (setf (resolutions nbd)
		(list (mk-resolution nbd (current-theory-name) ntype)))
	  nbd))))

(defmethod subst-mod-params* ((expr projection-expr) modinst bindings)
  (with-slots (actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :type ntype))))

(defmethod subst-mod-params* ((expr injection-expr) modinst bindings)
  (with-slots (actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :type ntype))))

(defmethod subst-mod-params* ((expr injection?-expr) modinst bindings)
  (with-slots (actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :type ntype))))

(defmethod subst-mod-params* ((expr extraction-expr) modinst bindings)
  (with-slots (actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :type ntype))))

(defmethod subst-mod-params* ((expr projection-application) modinst bindings)
  (with-slots (argument actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr injection-application) modinst bindings)
  (with-slots (argument actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr injection?-application) modinst bindings)
  (with-slots (argument actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr extraction-application) modinst bindings)
  (with-slots (argument actuals type) expr
    (let ((nacts (subst-mod-params* actuals modinst bindings))
	  (narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :actuals nacts :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr field-name-expr) modinst bindings)
  (declare (ignore modinst bindings))
  expr)

(defmethod subst-mod-params* ((expr field-application) modinst bindings)
  (with-slots (argument type) expr
    (let ((narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr number-expr) modinst bindings)
  (with-slots (number) expr
    (let ((map (assoc number bindings :key #'id)))
      (if map
	  (expr (cdr map))
	  (let ((ntype (subst-mod-params* (type expr) modinst bindings)))
	    (if (tc-eq (type expr) ntype)
		expr
		(let ((nres (mk-resolution (number-declaration number)
			      modinst ntype)))
		  (mk-name-expr number nil nil nres))))))))

(defmethod id ((rhs mapping-rhs))
  (when (name? (or (type-value rhs) (expr rhs)))
    (id (or (type-value rhs) (expr rhs)))))

(defvar *number-declarations* (make-hash-table :test 'eql))

(defun number-declaration (number)
  (or (gethash number *number-declarations*)
      (let ((decl (mk-const-decl number *number*)))
	(setf (module decl) (get-theory "numbers"))
	(setf (gethash number *number-declarations*) decl))))

(defmethod number-declaration? ((decl const-decl))
  (and (module decl)
       (eq (id (module decl)) '|numbers|)
       (integerp (id decl))))

(defmethod number-declaration? (obj)
  nil)
       

(defmethod subst-mod-params* ((expr record-expr) modinst bindings)
  (let ((ass (subst-mod-params* (assignments expr) modinst bindings))
	(type (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr :assignments ass :type type)))

(defmethod subst-mod-params* ((expr tuple-expr) modinst bindings)
  (let ((nexprs (subst-mod-params* (exprs expr) modinst bindings))
	(ntype (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr :exprs nexprs :type ntype)))

(defmethod subst-mod-params* ((expr cases-expr) modinst bindings)
  (if (some #'(lambda (sel) (assq (declaration (constructor sel))
				  bindings))
	    (selections expr))
      (subst-mod-params* (translate-cases-to-if expr) modinst bindings)
      (let ((nexpr (subst-mod-params* (expression expr) modinst bindings))
	    (nsels (subst-mod-params* (selections expr) modinst bindings))
	    (nelse (subst-mod-params* (else-part expr) modinst bindings))
	    (type (subst-mod-params* (type expr) modinst bindings)))
	(lcopy expr
	  :expression nexpr
	  :selections nsels
	  :else-part nelse
	  :type type))))

(defmethod subst-mod-params* ((expr application) modinst bindings)
  (with-slots (operator argument) expr
    (let* ((op (let ((*smp-include-actuals*
		      (and *smp-include-actuals*
			   (not (valid-infix-application? expr)))))
		 (subst-mod-params* operator modinst bindings)))
	   (arg (subst-mod-params* argument modinst bindings)))
      (if (and (eq op operator)
	       (eq arg argument))
	  expr
	  (if (and (implicit-conversion? expr)
		   (name-expr? op)
		   (or (and (eq (id op) '|extend|)
			    (eq (id (module-instance (resolution op)))
				'|extend|))
		       (and (eq (id op) '|restrict|)
			    (eq (id (module-instance (resolution op)))
				'|restrict|)))
		   (tc-eq (car (actuals (module-instance (resolution op))))
			  (cadr (actuals (module-instance (resolution op))))))
	      arg
	      (let* ((nop (if (and (implicit-conversion? op)
				   (name-expr? (operator op))
				   (eq (id (operator op)) '|restrict|)
				   (eq (id (module-instance
					    (resolution (operator op))))
				       '|restrict|))
			      (argument op)
			      op))
		     (optype (find-supertype (type op)))
		     (rtype (if (and (or (not (eq arg argument))
					 (not (eq op operator)))
				     (typep (domain optype) 'dep-binding))
				(substit (range optype)
				  (acons (domain optype) arg nil))
				(range optype)))
		     (nex (lcopy expr :operator nop :argument arg :type rtype)))
		;; Note: the copy :around (application) method takes care of
		;; changing the class if it is needed.
		nex))))))

(defmethod subst-mod-params* :around ((expr table-expr) modinst bindings)
  (let ((nexpr (call-next-method)))
    (if (eq expr nexpr)
	expr
	(lcopy nexpr
	  :row-expr (subst-mod-params* (row-expr nexpr) modinst bindings)
	  :col-expr (subst-mod-params* (col-expr nexpr) modinst bindings)
	  :row-headings (subst-mod-params* (row-headings nexpr)
					   modinst bindings)
	  :col-headings (subst-mod-params* (col-headings nexpr)
					   modinst bindings)
	  :table-entries (subst-mod-params* (table-entries nexpr)
					    modinst bindings)))))

(defmethod subst-mod-params* ((sym symbol) modinst bindings)
  (declare (ignore modinst bindings))
  sym)

(defmethod subst-mod-params* ((expr binding-expr) modinst bindings)
  (with-slots ((ebindings bindings) expression type) expr
    (let* ((nebindings (subst-mod-params-bindings ebindings modinst bindings))
	   (nbindings (if (equal nebindings ebindings)
			  ebindings
			  nebindings))
	   (alist (unless (eq nbindings ebindings)
		    (pairlis ebindings nbindings)))
	   (nexpr (if alist
		      (substit (subst-mod-params* expression modinst bindings)
			alist)
		      (subst-mod-params* expression modinst bindings)))
	   (ntype (subst-mod-params* type modinst bindings)))
      (lcopy expr
	:bindings nbindings
	:expression nexpr
	:type ntype))))

(defun subst-mod-params-bindings (ebindings modinst bindings
					    &optional nbindings)
  (if ebindings
      (let ((nbinding (subst-mod-params* (car ebindings) modinst bindings)))
	(subst-mod-params-bindings
	 (if (or (eq (car ebindings) nbinding)
		 (not (binding? (car ebindings))))
	     (cdr ebindings)
	     (substit (cdr ebindings) (acons (car ebindings) nbinding nil)))
	 modinst bindings (cons nbinding nbindings)))
      (nreverse nbindings)))

(defmethod subst-mod-params* ((expr update-expr) modinst bindings)
  (with-slots (expression assignments type) expr
    (let ((nexpr (subst-mod-params* expression modinst bindings))
	  (ass (subst-mod-params* assignments modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      (lcopy expr :expression nexpr :assignments ass :type ntype))))

(defmethod subst-mod-params* ((ass assignment) modinst bindings)
  (with-slots (arguments expression) ass
    (let ((args (subst-mod-params* arguments modinst bindings))
	  (expr (subst-mod-params* expression modinst bindings)))
      (lcopy ass :arguments args :expression expr))))

(defmethod subst-mod-params* ((expr field-assignment-arg) modinst bindings)
  (declare (ignore modinst bindings))
  expr)

(defmethod subst-mod-params* ((sel selection) modinst bindings)
  (with-slots (constructor expression args) sel
    (let* ((name (subst-mod-params* constructor modinst bindings))
	   (nbindings (subst-mod-params-bindings args modinst bindings))
	   (nargs (if (equal nbindings args) args nbindings))
	   (alist (unless (eq nargs args)
		    (pairlis args nargs)))
	   (nexpr (subst-mod-params* (if alist
					 (substit expression alist)
					 expression)
				     modinst bindings)))
      (lcopy sel
	:constructor name
	:args nargs
	:expression nexpr))))

;;; subst-mod-params* for a name works as follows, where A and B are
;;; sequences of actuals a_1,... and b_1,...; f_i is the ith formal
;;; parameter of m.
;;;
;;; smp(f_i, m[A])    = a_i
;;; smp(m[]!x, m[A])  = m[A]!x
;;; smp(n[B]!x, m[A]) = n[smp(B, m[A])]!x

(defmethod subst-mod-params* ((name name) modinst bindings)
  (let* ((res (car (resolutions name)))
	 (nres (subst-mod-params* res modinst bindings)))
    (if (eq nres res)
	name
	(copy name
	  :actuals (when (or *smp-include-actuals*
			     (actuals name))
		     (mapcar #'copy (actuals (module-instance nres))))
	  :resolutions (list nres)))))

(defmethod subst-mod-params* ((act actual) modinst bindings)
  (with-slots (expr type-value) act
    (if (and (typep expr 'name)
	     (assq (declaration expr) bindings))
	(let ((nact (cdr (assq (declaration expr) bindings))))
	  (if (or (and type-value (type-value nact))
		  (and (null type-value) (null (type-value nact))))
	      nact
	      (lcopy nact
		:expr (lcopy (expr nact) :parens (parens expr))
		:type-value (subst-mod-params* type-value modinst bindings))))
	(let ((ntype (when type-value
		       (subst-mod-params* type-value modinst bindings))))
	  (lcopy act
	    :expr (if ntype
		      (if (eq ntype type-value)
			  expr
			  (or (print-type ntype) ntype))
		      (let ((nexpr (subst-mod-params* expr modinst bindings)))
			(if (eq nexpr expr)
			    expr
			    (pseudo-normalize nexpr))))
	    :type-value ntype)))))


;;; Checks whether all actuals are formal parameters (of the current theory)

(defun actuals-are-formals? (actuals)
  (or (null actuals)
      (let ((ex (actual-value (car actuals))))
	(and (typep ex 'name)
	     (typep (declaration ex) 'formal-decl)
	     #+pvsdebug (null (assert (memq (declaration ex)
					    (formals (current-theory)))))
	     ;; Even if the first one is, the rest may not be.
	     (actuals-are-formals? (cdr actuals))))))


;;; For resolutions, first check whether there are any actuals; if not,
;;; and the module-instance matches modinst, then we can simply use
;;; modinst in a newly created resolution.  Subst-declaration ensures
;;; that the local bindings are handled properly.  

(defmethod subst-mod-params* ((res resolution) modinst bindings)
  (with-slots ((decl declaration) (mi module-instance) type) res
    (let ((acts (actuals mi)))
      #+pvsdebug (assert (not (assq decl bindings)))
      #+pvsdebug (assert mi)
      (cond ((tc-eq mi modinst)
	     res)
	    ((and (eq (id mi) (id modinst))
		  (or (null acts)
		      (actuals-are-formals? acts)))
	     ;; Note: mappings may change the type of a resolution for an
	     ;; uninterpreted constant.
	     (let ((ntype (subst-mod-params* type modinst bindings)))
	       (mk-resolution decl modinst ntype)))
	    (t (let* ((nacts (subst-mod-params* acts modinst bindings)))
		 (if (and (eq nacts acts)
			  (not (binding? decl))
			  (null (mappings modinst)))
		     res
		     (let ((ntype (subst-mod-params* type modinst bindings)))
		       (if (and (strong-tc-eq type ntype)
				(strong-tc-eq nacts acts)
				(not (memq (id mi) '(|equalities| |notequal|)))
				(not (library-datatype-or-theory?
				      (module decl))))
			   res
			   (let* ((rhs (cdr (assq (module decl) bindings)))
				  (nmappings (when rhs (mappings (expr rhs)))))
			     (mk-resolution decl
			       (mk-modname (id mi)
				 (if (memq (id mi) '(|equalities| |notequal|))
				     (list (mk-actual
					    (find-supertype
					     (type-value (car nacts)))))
				     nacts)
				 (when (library-datatype-or-theory?
					(module decl))
				   (get-lib-id (module decl)))
				 (or nmappings
				     (unless (or (eq type ntype)
					     (not (eq (id mi) (id modinst)))
					     (not (eq (library mi)
						      (library modinst))))
				       (mappings modinst))))
			       ntype)))))))))))

(defmethod make-resolution (decl modinst &optional type)
  (assert (modname? modinst))
  #+pvsdebug
  (assert (or (null type)
	      (not (fully-instantiated? modinst))
	      (fully-instantiated? type)))
  #+pvsdebug (assert (get-theory modinst))
  (let* ((*smp-include-actuals* t)
	 (theory (when (and (module decl)
			    (eq (id (module decl)) (id modinst)))
		   (module decl)))
	 (rtype (if type
		    (subst-mod-params type modinst theory)
		    (typecase decl
		      (type-decl
		       (let ((*free-bindings*
			      (append (apply #'append (formals decl))
				      *free-bindings*)))
			 (subst-mod-params (type-value decl) modinst theory)))
		      ((or expname typed-declaration simple-decl)
		       (subst-mod-params (type decl) modinst theory))))))
    (mk-resolution decl modinst rtype)))

(defmethod make-resolution ((decl binding) modinst &optional type)
  (assert (or modinst type))
  #+pvsdebug (assert (or (null type)
			 (eq modinst (current-theory-name))
			 (not (fully-instantiated? modinst))
			 (fully-instantiated? type)))
  (let ((dtype (or type (type decl))))
    (mk-resolution decl
      (or modinst (current-theory-name))
      (if (and modinst (not (eq modinst (current-theory-name))))
	  (subst-mod-params dtype modinst)
	  dtype))))

(defun theories-of-param-alist (alist)
  (let ((theories nil))
    (dolist (elt alist)
      (pushnew (module (car elt)) theories :test #'tc-eq))
    theories))

(defun theory-insts-of-param-alist (alist)
  (let ((theories (theories-of-param-alist alist)))
    (values (mapcar #'(lambda (th)
			(mk-modname (id th)
			  (mapcar #'(lambda (fm)
				      (let ((ex (cdr (assq fm alist))))
					(if ex
					    (mk-actual ex)
					    (mk-res-actual
					     (mk-name-expr (id fm)
					       nil nil
					       (make-resolution
						   fm (mk-modname (id th))))
					     th))))
			    (formals-sans-usings th))))
	      theories)
	    theories)))

(defun subst-mod-params-instances (ex theory-instances theories)
  (if (null theory-instances)
      ex
      (subst-mod-params-instances
       (subst-mod-params ex (car theory-instances) (car theories))
       (cdr theory-instances)
       (cdr theories))))

(defun subst-mod-params-alist (ex alist)
  (multiple-value-bind (instances theories)
      (theory-insts-of-param-alist alist)
    (subst-mod-params-instances ex instances theories)))

(defun subst-mod-params-substlist (alist inst theory)
  (let ((cars (subst-mod-params (mapcar #'car alist) inst theory)))
    (mapcar #'(lambda (x y) (cons x (cdr y))) cars alist)))
