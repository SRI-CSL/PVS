;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst-mod-params.lisp -- performs substitution of actual parameters for
;;                          formals 
;; Author          : Sam Owre
;; Created On      : Thu Dec  9 13:10:41 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 18 03:38:55 2012
;; Update Count    : 76
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2012, SRI International.  All Rights Reserved.

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


;;; (all-subst-mod-params-caches *workspace-session*) is the set of caches
;;; for subst-mod-params - it is a hash-table of hash-tables, indexed by
;;; module-instance.  This is initialized by reset-subst-mod-params-cache,
;;; and used and set by subst-mod-params.

;;; This is set by subst-mod-params to the cache found (or created) in
;;; looking up the modname in (all-subst-mod-params-caches *workspace-session*).

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

(defvar *subst-mod-params-declaration* nil)

(defvar *subst-mod-params-cur-decl* nil)

(defvar *subst-mod-params-freevars* nil)

(defvar *subst-mod-params-map-bindings*)

;;; This resets the (all-subst-mod-params-caches *workspace-session*) hash
;;; table.  It is called from the parser whenever a newly parsed theory
;;; replaces the old theory, to ensure that the garbage collector can remove
;;; the objects.

(defun reset-subst-mod-params-cache ()
  (assert *workspace-session*)
  (clrhash (all-subst-mod-params-caches *workspace-session*))
  (when *subst-mod-params-cache*
    (clrhash *subst-mod-params-cache*))
  (when *subst-mod-params-eq-cache*
    (clrhash *subst-mod-params-eq-cache*))
;  (if *subst-mod-params-cache*
;      (clrhash *subst-mod-params-cache*)
;      (setq *subst-mod-params-cache*
;	    (make-pvs-hash-table :size 5)))
  )

(defun remove-subst-mod-params-cache ()
  (assert *workspace-session*)
  (clrhash (all-subst-mod-params-caches *workspace-session*))
  (setq *subst-mod-params-cache* nil)
  (setq *subst-mod-params-eq-cache* nil))

(defun copy-subst-mod-params-cache ()
  (assert *workspace-session*)
  (unless (all-subst-mod-params-caches *workspace-session*)
    (reset-subst-mod-params-cache))
  (let ((new-hash (make-pvs-hash-table
		   :strong-eq? t
		   :size (floor (hash-table-size (all-subst-mod-params-caches *workspace-session*))
				1.5384616))))
    (maphash #'(lambda (modname hashes)
		 (setf (gethash modname new-hash)
		       (cons (copy (car hashes)) (copy (cdr hashes)))))
	     (all-subst-mod-params-caches *workspace-session*))
    new-hash))


;;; Gets the cache associated with the given modinst, or creates a new one
;;; and returns it if necessary.

(defun get-subst-mod-params-caches (modinst)
  ;; (assert (fully-typed? modinst))
  ;; (assert (every #'fully-typed?
  ;; 		 (get-hash-keys (all-subst-mod-params-caches *workspace-session*))))
  (let ((caches (gethash modinst (all-subst-mod-params-caches *workspace-session*))))
    (or caches
	(let ((ncaches
	       (cons (make-pvs-hash-table :strong-eq? t)
		     (make-hash-table :test 'eq))))
	  (setf (gethash modinst (all-subst-mod-params-caches *workspace-session*)) ncaches)
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
	(theories (delete-duplicates
		   (mapcar #'(lambda (elt) (module (car elt)))
		     alist)))
	(sterm term))
    (dolist (th theories)
      (let* ((lib-id (when (lib-datatype-or-theory? th)
		       (let* ((libpath (context-path th))
			      (libid (get-library-id libpath)))
			 (unless libid
			   (pvs-error "Library reference error"
			     (format nil "Couldn't find lib-id for ~a" libpath)))
			 libid)))
	     (actuals (unless (eq th (current-theory))
			(mapcar #'(lambda (fp)
				    (or (cdr (assq fp new-alist))
					(let* ((thname (mk-modname (id th)
							 nil lib-id))
					       (tres (mk-resolution th thname nil))
					       (res (make-resolution fp thname)))
					  (setf (resolutions thname) (list tres))
					  (mk-res-actual
					   (if (type-decl? fp)
					       (mk-type-name (id fp) nil nil res)
					       (mk-name-expr (id fp) nil nil res))
					   th))))
			  (formals-sans-usings th))))
	     (fdecl (let ((fbd (find-if #'decl-formal? new-alist :key #'car)))
		      (when fbd (associated-decl (car fbd)))))
	     (dactuals (when fdecl
			 (mapcar #'(lambda (fp) (cdr (assq fp new-alist)))
			   (decl-formals fdecl))))
	     (thname
	      (mk-modname (id th) actuals lib-id nil dactuals fdecl))
	     (tres (mk-resolution th thname nil)))
	(setf (resolutions thname) (list tres))
	(setf sterm (subst-mod-params sterm thname th fdecl))))
    sterm))

(defmethod subst-theory-params (term (modinst modname))
  (subst-mod-params term modinst))


;;; The main entry point to subst-mod-params.  Basically the modinst has
;;; actuals and dactuals that need to be substituted for in the term obj The
;;; actuals are associated with the theory parameters, while the dactuals
;;; are associates with the declaration parameters (decl-formals).

;;; The theory argument is the theory associated with the modinst, and the
;;; decl is the declaration associated with the dactuals.  The decl is
;;; necessary if there are dactuals.  The decl should belong to the theory.

;;; The theory arg is not needed, but may speed things up if given.

;;; Note that the theory and decl are generally (always?) different than the
;;; (current-theory) and (current-declaration), which are used for testing
;;; if something is fully-instantiated.

;;; The result is a new object with the actuals, deactuals, and mappings all
;;; substituted for.  The 

(defun subst-mod-params (obj modinst &optional theory decl)
  (assert *current-context*)
  (assert (modname? modinst))
  (assert (or (null theory)
	      (null (actuals modinst))
	      (same-id theory modinst)
	      (eq (generated-by theory) (id modinst))))
  (assert (or (null (dactuals modinst)) decl))
  (assert (not (decl-formal? decl)))
  ;; (assert (or (null decl)
  ;; 	      (mapping-lhs? decl)
  ;; 	      (null (decl-formals decl))
  ;; 	      (and theory (eq theory (module decl))
  ;; 		   (memq decl (all-decls theory)))))
  (assert (or (null decl)
	      (every #'(lambda (x)
			 (let ((d (declaration x)))
			   (or (not (decl-formal? d))
			       (memq d (decl-formals decl)))))
		     (freevars obj))))
  (assert (fully-typed? modinst))
  (let* ((*subst-mod-params-theory* (or theory
					(and (resolutions modinst) (declaration modinst))
					(get-theory modinst)))
	 (*subst-mod-params-declaration* decl)
	 (*subst-mod-params-cur-decl* (current-declaration)) ;; with-current-decl changes this
	 (*subst-mod-params-freevars* (or *subst-mod-params-freevars*
					  (freevars obj)))
	 (formals (unless (or (not (datatype-or-module? *subst-mod-params-theory*))
			      (eq (current-theory) *subst-mod-params-theory*))
		    (formals-sans-usings *subst-mod-params-theory*)))
	 (dformals (when decl (all-decl-formals decl))) ; includes mapping lhs
		     ;;(decl-formals decl)))
	 (*subst-mod-free-params* nil))
    (unless (resolution modinst)
      (setf (resolutions modinst)
	    (list (mk-resolution *subst-mod-params-theory* modinst nil))))
    (assert (typep *subst-mod-params-theory*
		   '(or module mod-decl theory-abbreviation-decl
		     datatype formal-theory-decl)))
    (if (or (module? obj)
	    (actuals modinst)
	    (dactuals modinst)
	    (mappings modinst)
	    (some #'formal-theory-decl? formals))
	(let* ((*generate-tccs* 'none)
	       (caches (get-subst-mod-params-caches modinst))
	       (*subst-mod-params-cache* (car caches))
	       (*subst-mod-params-eq-cache* (cdr caches))
	       (*smp-mappings* (mappings modinst))
	       (*subst-mod-params-map-bindings* nil) ; Collects map bindings
	       (bindings (make-subst-mod-params-bindings
			  modinst (append (when (actuals modinst)
					    formals)
					  (when (dactuals modinst)
					    dformals))
			  (append (actuals modinst)
				  ;; if decl is a mapping-lhs, include its dactuals
				  (all-dactuals decl modinst))
			  (mappings modinst) nil)))
	  (assert (every #'(lambda (fp)
			     (or (not (decl-formal? fp))
				 (memq fp (decl-formals (current-declaration)))
				 (assq fp bindings)))
			 (free-params obj)))
	  (unless (subsetp *subst-mod-params-map-bindings* bindings :test #'equal)
	    (break "subst-mod-params: check bindings"))
	  (multiple-value-bind (nobj nbindings)
	      (subst-mod-params* obj modinst bindings)
	    (unless nbindings (setq nbindings bindings))
	    #+pvsdebug (assert (or (eq obj nobj) (not (tc-eq obj nobj))))
	    #+pvsdebug (assert (equal nbindings (pairlis formals (actuals modinst))))
	    #+pvsdebug (assert (or (typep nobj 'modname) (fully-instantiated? nobj)))
	    ;; Note that it may be that nobj is tc-eq to obj, without being eq
	    ;; True even for strong-tc-eq.
	    ;; (assert (or (mappings modinst)
	    ;; 	       (subsetp (free-params nobj) (free-params modinst))))
	    (assert (or (mappings modinst)
			(every #'(lambda (fp) (or (memq fp (free-params modinst))
						  (memq fp (free-params obj))))
			       (free-params nobj))))
	    #+badassert ;; 
	    (assert (every #'(lambda (fp)
			       (and (not (memq fp formals))
				    (not (memq fp (when decl (decl-formals decl))))))
			   (free-params nobj)))
	    (assert (or (null decl)
			(every #'(lambda (x)
				   (let ((d (declaration x)))
				     (or (not (decl-formal? d))
					 (not (memq d (decl-formals decl))))))
			       (freevars obj))))
	    (values nobj nbindings)))
	obj)))

(defmethod all-decl-formals ((lhs mapping-lhs))
  (append (decl-formals (declaration lhs)) (decl-formals lhs)))

(defmethod all-decl-formals (decl)
  (decl-formals decl))

(defmethod all-dactuals ((lhs mapping-lhs) modinst)
  (append (dactuals lhs) (dactuals modinst)))

(defmethod all-dactuals (decl modinst)
  (declare (ignore decl))
  (dactuals modinst))

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


;;; Create the declaration to exprs association list
;;; It comes from 3 places in modinst
;;;  1. the formals are associated with the actuals
;;;     fairly straightforward, except for subtype formals and theory formals
;;;  2. the mappings lhs declarations are associated with the rhs
;;;     only complicated for theory-decl mappings
;;;  3. modinst itself may be a reference to a theory-decl or theory-abbreviation
;;;     needs recursive call
;;; Note that the arguments in principle could all be derived from modinst,
;;; except for bindings, which are initially nil, but may not be in recursive calls

(defun make-subst-mod-params-bindings (modinst formals actuals mappings bindings)
  (cond ((null formals)
	 #+pvsdebug
	 (assert (check-smp-bindings bindings))
	 (let ((mbindings (make-subst-mod-params-map-bindings modinst mappings bindings)))
	   ;;(make-subst-mod-params-theoryref-bindings modinst mbindings)
	   mbindings))
	(t (let ((pred-binding (make-subst-mod-params-pred-binding
				modinst (car formals) (car actuals) bindings))
		 (nbindings (make-subst-mod-params-binding
			     (car formals) (car actuals) bindings)))
	     (assert (or (not (decl-formal? (car formals)))
			 (mapping-lhs? *subst-mod-params-declaration*)
			 (memq (car formals) (decl-formals (current-declaration)))
			 (memq (car formals) (decl-formals *subst-mod-params-declaration*))))
	     #+pvsdebug
	     (assert (check-smp-bindings nbindings))
	     (push (caar nbindings) *subst-mod-free-params*)
	     (when pred-binding
	       #+pvsdebug
	       (assert (check-smp-bindings (cons pred-binding nil)))
	       (push (car pred-binding) *subst-mod-free-params*))
	     (make-subst-mod-params-bindings
	      modinst
	      (cdr formals)
	      (cdr actuals)
	      mappings
	      (if pred-binding
		  (cons pred-binding nbindings)
		  nbindings))))))

;;; In most cases, this is simply adding (formal . actual)  to bindings
;;; But when the formal is a theory-decl, its bindings need to be included
;;; Note that this is before doing the mappings of the original modinst.
(defmethod make-subst-mod-params-binding ((formal formal-theory-decl) actual
					  bindings)
  (acons formal actual
	 (make-subst-mod-params-theory-decl-bindings formal actual bindings)))

(defun make-subst-mod-params-theory-decl-bindings (formal actual bindings)
  (let* ((thname (if (typep (declaration (expr actual)) 'theory-reference)
		     (get-theory-alias (expr actual))
		     (expr actual)))
	 (amappings (when (typep (declaration (expr actual))
				 '(or mod-decl formal-theory-decl))
		      (theory-mappings (declaration (expr actual)))))
	 (decl (declaration (expr actual)))
	 (theory (if (module? decl)
		     decl
		     (get-theory (theory-name decl))))
	 (pre-bindings (make-subst-mod-params-bindings
			thname
			(formals-sans-usings theory)
			(actuals thname)
			nil
			(extended-mappings thname theory decl)))
	 (inv-mappings
	  (mapcar #'(lambda (da)
		      (cons (declaration (expr (cdr da))) (car da)))
	    (theory-mappings formal)))
	 ;; Now we compose the inverses of the mappings of the
	 ;; interpreted theory and the source
	 (comp (append (compose-mappings inv-mappings pre-bindings)
		       (compose-mappings inv-mappings amappings)
		       pre-bindings
		       bindings)))
    (dolist (imap inv-mappings)
      (unless (assq (car imap) comp)
	(push imap comp)))
    comp))

(defun get-theory-alias (thabbr)
  (let* ((thabbr-decl (declaration thabbr))
	 (thname (theory-name thabbr-decl))
	 (containing-theory (module thabbr-decl)))
    #+pvsdebug
    (assert (fully-instantiated? (module-instance thabbr)))
    (subst-mod-params thname (module-instance thabbr) containing-theory)))

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


;;; extended-mappings creates a bindings alist from lhs to rhs.
;;; There are a number of places the bindings can come from:
;;;  1. The explicit mappings in thname
;;;  2. The implicit mappings, if any, in thname, i.e., if thname is a
;;;     mod-decl or formal-theory-decl
;;;  3. Theory parameters of thname, need to get their mappings
;;;  4. The rhs mappings for a theory lhs of a mapping in 1.
;;; E.g., thname = th[th1]{{th' := th2, ...}} needs to include, besides
;;; the given bindings, any bindings coming (recursively) from th, th1 and th2

(defun extended-mappings (thname lhs-theory decl)
  ;; Shouldn't lhs-theory and rhs-theory be the same?
  (declare (ignore lhs-theory))
  (let* ((rhs-theory (declaration thname))
	 ;; extended-basic-mappings deals with explicit and implicit th mappings
	 (rhs-mappings (when rhs-theory (theory-mappings rhs-theory)))
	 ;; (bmappings (extended-basic-mappings (mappings thname) rhs-mappings
	 ;; 				     nil ;(theory-mappings lhs-theory)
	 ;; 				     ))
	 (emappings (extended-mappings* rhs-mappings decl))
	 (all-mappings (make-subst-mod-params-map-bindings
			thname (mappings thname) emappings)))
    (dolist (amap all-mappings)
      (typecase (car amap)
	(module
	 (setq all-mappings
	       (append (extended-mappings (expr (cdr amap)) (car amap) decl)
		       all-mappings)))
	(mod-decl
	 (when (mapping-rhs? (cdr amap))
	   (setq all-mappings
		 (append (extended-mappings (expr (cdr amap)) (car amap) decl)
			 all-mappings))))))
    all-mappings))

;;; We have mappings, and mod-decl has some in theory-mappings
;;; if the car of a theory-mapping is in mappings, ignore
;;; Otherwise, find what the cdr maps to (mcdr).  If not nil,
;;; add (car . mcdr) to mappings
(defmethod extended-mappings* (mappings (decl mod-decl))
  (nconc (mapcan #'(lambda (tmap)
		     (unless (or ;;(assq (car tmap) mappings) ; Already mapped
				 (not (interpretable? (car tmap))))
		       (let* ((mdecl (declaration (cdr tmap)))
			      (nmap (assq (car tmap) mappings)))
			 (when nmap
			   (list (cons mdecl (cdr nmap)))))))
	    (theory-mappings decl))
	  mappings))

(defmethod extended-mappings* (mappings (decl formal-theory-decl))
  (mapcan #'(lambda (tmap)
	      (let ((mdecl (declaration (cdr tmap)))
		    (nmap (assq (car tmap) mappings)))
		(when nmap
		  (list (cons mdecl (cdr nmap))))))
    (theory-mappings decl)))

(defmethod extended-mappings* (mappings (decl theory-abbreviation-decl))
  mappings)

(defmethod extended-mappings* (mappings (decl module))
  mappings)

(defun extended-basic-mappings (mappings rthmappings lthmappings &optional bmappings)
  (if (null mappings)
      (if (null rthmappings)
	  (if (null lthmappings)
	      (nreverse bmappings)
	      (extended-basic-mappings
	       mappings rthmappings (cdr lthmappings)
	       (if (and (mapping-rhs? (cdar lthmappings))
			(if (type-value (cdar lthmappings))
			    (type-name? (type-value (cdar lthmappings)))
			    (name-expr? (expr (cdar lthmappings)))))
		   (let* ((decl (declaration (or (type-value (cdar lthmappings))
						 (expr (cdar lthmappings)))))
			  (elt (assq (caar lthmappings) bmappings)))
		     (if elt
			 (acons decl (cdr elt) bmappings)
			 (acons decl (caar lthmappings) bmappings)))
		   bmappings)))
	  (extended-basic-mappings
	   mappings (cdr rthmappings) lthmappings
	   (let ((elt (assq (cdar rthmappings) bmappings)))
	     (if elt
		 (acons (caar rthmappings) (cdr elt) bmappings)
		 (cons (car rthmappings) bmappings)))))
      (extended-basic-mappings (cdr mappings)
			       rthmappings
			       lthmappings
			       (acons (declaration (lhs (car mappings)))
				      (rhs (car mappings))
				      bmappings))))


;;; Called from make-interpreted-copy
;;; For the uninterpreted decls of the interpretation theory, create a
;;; mapping from the corresponding theory decls.  This is stored in the
;;; theory-mapping of the interpretation theory by set-type-mapping.
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

(defmethod make-subst-mod-params-binding (formal actual bindings)
  (acons formal actual bindings))


(defmethod make-subst-mod-params-pred-binding (modinst (formal formal-subtype-decl)
						       actual bindings)
  (let* ((subtype (type-value actual))
	 (sformal (when (typep (type-expr formal) 'type-name)
		    (cdr (assoc (declaration (type-expr formal)) bindings)))))
    (if sformal
	(let ((spred (subtype-pred subtype (type-value sformal))))
	  (when spred
	    (unless (place spred)
	      (set-extended-place spred actual
				  "creating predicate between ~a and ~a"
				  subtype (type-value sformal)))
	    (cons (find-if #'(lambda (c) (typep c 'const-decl))
		    (generated formal))
		  (make-instance 'actual
		    :expr spred
		    :place (place spred)))))
	(let* ((fstype (subst-mod-params* 
			(supertype (type-value formal))
			modinst bindings))
	       (spred (when (compatible? subtype fstype)
			(subtype-pred subtype fstype))))
	  (when spred
	    (unless (place spred)
	      (set-extended-place spred actual
				  "creating predicate between ~a and ~a"
				  subtype fstype))
	    (cons (find-if #'(lambda (c) (typep c 'const-decl))
		    (generated formal))
		  (make-instance 'actual
		    :expr spred
		    :place (place spred))))))))

(defmethod make-subst-mod-params-pred-binding (modinst formal actual bindings)
  (declare (ignore modinst formal actual bindings))
  nil)


;;; Generate the bindings due to mappings

(defun make-subst-mod-params-map-bindings (modinst mappings bindings)
  (if (null mappings)
      (nreverse (remove-if #'null bindings :key #'cdr))
      (let* ((lhs (lhs (car mappings)))
	     (ldecl (if (mapping-lhs? lhs) ;; Has decl-formals, otherwise just a name
			lhs
			(declaration (car (resolutions lhs)))))
	     (rhs (rhs (car mappings)))
	     (nbindings (make-subst-mod-params-map-bindings*
			 ldecl rhs bindings)))
	#+pvsdebug
	(assert (check-smp-bindings nbindings))
	(make-subst-mod-params-map-bindings
	 modinst
	 (cdr mappings)
	 nbindings))))

(defun check-smp-bindings (bindings)
  (every #'(lambda (bd)
	     (typecase (car bd)
	       ((or type-decl formal-type-decl)
		(or (and (current-declaration) ;; may not be set when restoring
			 (memq (car bd) (decl-formals (current-declaration))))
		    (type-decl? (cdr bd))
		    (and (actual? (cdr bd)) (type-value (cdr bd)))
		    (break "No binding for type ~a" (car bd))))
	       ((or const-decl formal-const-decl)
		(or (const-decl? (cdr bd))
		    (and (actual? (cdr bd))
			 (null (type-value (cdr bd)))
			 (expr? (expr (cdr bd))))
		    (break "const")))
	       ((or formal-theory-decl module theory-reference)
		(or (and (actual? (cdr bd))
			 (null (type-value (cdr bd)))
			 (name-expr? (expr (cdr bd))))
		    (break "theory")))
	       (formula-decl
		(or (formula-decl? (cdr bd))
		    (break "formula-decl")))
	       (mapping-lhs
		(or (mapping-rhs? (cdr bd))
		    (break "mapping")))
	       (t (break "whats this?"))))
	 bindings))

;;; The lhs is generally a name, with optional declaration parameters.
;;; If declaration parameters are there, it is a mapping-lhs
;;; else the declaration of the name, which can be a module, 

(defmethod make-subst-mod-params-map-bindings* ((lhs-theory module) rhs bindings)
  ;; th := thref[a1,...]{{e := e1,...}}
  ;; thref may not be th - it could be a theory-abbreviation-decl or mod-decl
  ;; Need to expand it first.  Note that thref may itself have actuals and mappings,
  ;; but the actuals are relative to the theory where thref was declared
  (let* ((rhs-thname (expr rhs))
	 (rhs-decl (declaration rhs-thname))
	 (rhs-thinst (module-instance rhs-thname))
	 (rhs-theory (module rhs-decl))
	 (nbindings (make-subst-mod-params-bindings
		     rhs-thinst
		     (when (actuals rhs-thinst) (formals-sans-usings rhs-theory))
		     (actuals rhs-thinst)
		     (mappings rhs-thinst)
		     bindings)))
    #+pvsdebug
    (assert (or (eq (id rhs-thname) (id lhs-theory))
		(and (resolution rhs-thname)
		     (mod-decl? (declaration rhs-thname))
		     (eq (id (theory-name (declaration rhs-thname))) (id lhs-theory)))))
    ;;(assert (fully-instantiated? rhs-thname))
    #+pvsdebug (assert (every #'(lambda (d)
				  (typep (car d) '(or declaration module mapping-lhs)))
			      nbindings))
    (setq *subst-mod-params-map-bindings*
	  (acons lhs-theory rhs
		 (append nbindings *subst-mod-params-map-bindings*)))
    (acons lhs-theory rhs
	   (append nbindings bindings))))

(defmethod make-subst-mod-params-map-bindings* ((lhs-decl theory-reference) rhs bindings)
  (let* ((rthname (if (theory-abbreviation-decl? (declaration (expr rhs)))
		     (get-theory-alias (expr rhs))
		     (expr rhs)))
	 (rdecl (declaration (expr rhs)))
	 (theory (if (module? rdecl)
		     rdecl
		     (or (get-theory (theory-name lhs-decl))
			 (module rdecl))))
	 (extmaps (extended-mappings rthname theory lhs-decl))
	 (pre-bindings (make-subst-mod-params-bindings
			rthname (formals-sans-usings theory)
			(actuals rthname)
			nil
			extmaps))
	 (inv-mappings
	  (mapcar #'(lambda (da)
		      (cons (declaration (expr (cdr da))) (car da)))
	    (if (mod-decl? lhs-decl)
		(theory-mappings lhs-decl)
		(mappings (theory-name lhs-decl))))))
    #+pvsdebug (assert (every #'(lambda (d)
				  (typep (car d) '(or declaration module mapping-lhs)))
			      pre-bindings))
    (let ((sbindings (acons lhs-decl rhs
			    (union (compose-mappings inv-mappings pre-bindings)
				   pre-bindings
				   :test #'tc-eq))))
      (setq *subst-mod-params-map-bindings*
	    (union sbindings *subst-mod-params-map-bindings* :test #'tc-eq))
      (let ((result (union sbindings bindings :test #'tc-eq)))
	result))))

(defmethod make-subst-mod-params-map-bindings* ((decl declaration) rhs bindings)
  (setq *subst-mod-params-map-bindings*
	(acons decl rhs *subst-mod-params-map-bindings*))
  #+pvsdebug
  (assert (every #'(lambda (d) (typep (car d) '(or declaration module mapping-lhs)))
		 bindings))
  (acons decl rhs bindings))

(defmethod make-subst-mod-params-map-bindings* ((lhs mapping-lhs) rhs bindings)
  (setq *subst-mod-params-map-bindings*
	(acons lhs rhs *subst-mod-params-map-bindings*))
  #+pvsdebug
  (assert (every #'(lambda (d) (typep (car d) '(or declaration module mapping-lhs)))
		 bindings))
  (acons lhs rhs bindings))

;;; The bindings already include the actuals and mappings from modinst
;;; Now we want to deal with modinst when it is a theory-reference
;; (defun make-subst-mod-params-theoryref-bindings (modinst bindings)
;;   ;; (let ((decl (if (resolution modinst)
;;   ;; 		  (declaration modinst)
;;   ;; 		  (get-theory modinst))))
;;   ;;   (assert decl)
;;   ;;   (make-subst-mod-params-theoryref-bindings* decl bindings))
;;   bindings
;;   )

;; (defmethod make-subst-mod-params-theoryref-bindings* ((decl theory-reference) bindings)
;;   ;; Theory-reference is a mod-decl or theory-abbreviation-decl
;;   (let ((thinst (theory-name decl)))
;;     ;;(break "make-subst-mod-params-theoryref-bindings*")
;;     bindings))
    
;; (defmethod make-subst-mod-params-theoryref-bindings* ((decl module) bindings)
;;   bindings)

;; (defmethod make-subst-mod-params-theoryref-bindings* ((decl declaration) bindings)
;;   bindings)

;;; End of make-subst-mod-params-bindings functions

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
  (declare (type hash-table *subst-mod-params-cache* *subst-mod-params-eq-cache*))
  #+badassert
  (assert (or (module? obj)
	      (declaration? obj)
	      (mappings modinst)
	      (not (some #'(lambda (bdg)
			     (and (decl-formal? (car bdg))
				  (not (mapping-lhs? *subst-mod-params-declaration*))
				  (not (memq (car bdg) (decl-formals *subst-mod-params-declaration*)))
				  (not (memq (car bdg) (decl-formals (current-declaration))))))
			 bindings))))
  (let ((fixed-type (car (member obj (list *boolean* *number* *number_field*
					   *real* *rational* *integer* *naturalnumber*)
				 :test #'tc-eq))))
    (cond (fixed-type
	   (if (or (null (print-type obj))
		   (null (free-params (print-type obj))))
	       obj
	       (let ((pt (subst-mod-params* (print-type obj) modinst bindings)))
		 (lcopy obj :print-type pt))))
	  (t
	   (let ((hobj (or (gethash obj *subst-mod-params-eq-cache*)
			   (gethash obj *subst-mod-params-cache*))))
	     (if hobj
		 (progn (unless (fully-typed? hobj) (break "not fully-typed?"))
			(if (strong-tc-eq hobj obj) obj hobj))
		 (let ((nobj
			(if (and (null (mappings modinst))
				 (not (typep obj '(or module declaration
						   inline-recursive-type list)))
				 (not (some #'(lambda (b)
						(typep (car b)
						       '(or formal-theory-decl
							 decl-formal declaration)))
					    bindings))
				 (no-matching-free-params
				  obj *subst-mod-free-params*))
			    obj
			    (call-next-method))))
		   (when (and (typep obj 'type-expr)
			      (typep nobj 'type-expr))
		     (let ((nnobj (subst-mod-params-print-type obj nobj modinst bindings)))
		       (setq nobj nnobj))
		     (when (from-conversion obj)
		       (let ((fconv (subst-mod-params* (from-conversion obj)
						       modinst bindings)))
			 (setq nobj (lcopy nobj :from-conversion fconv)))))
		   (unless (some #'(lambda (fv) (member fv *subst-mod-params-freevars*
							:test #'same-declaration))
				 (freevars nobj))
		     (if (and (not (some #'decl-formal? (free-params nobj)))
			      (relatively-fully-instantiated? nobj))
			 (setf (gethash obj *subst-mod-params-cache*) nobj)
			 (setf (gethash obj *subst-mod-params-eq-cache*) nobj)))
		   #+pvsdebug
		   (assert (every #'(lambda (fv)
				      (or (binding? fv)
					  (member fv (freevars obj)
						  :test #'same-declaration)
					  (member fv (freevars modinst)
						  :test #'same-declaration)))
				  (freevars nobj)))
		   #+pvsdebug
		   (when (actual? obj)
		     (assert (actual? nobj)))
		   ;;(assert (or (not (strong-tc-eq obj nobj)) (eq obj nobj)))
		   #+badassert
		   (assert (or (not (fully-instantiated? modinst)) (fully-instantiated? nobj)))
		   nobj)))))))

(defmethod subst-mod-params-print-type ((obj type-expr) (nobj type-expr) modinst bindings)
  "subst-mod-params-print-type is called with the original obj, and nobj:
the result of subst-mod-params* on obj.  It basically decides what to set
the print-type of nobj to.
type-name, datatype-subtype, type-application, expr-as-type"
  (assert (typep (print-type obj) '(or null type-name expr-as-type type-application)))
  (let* ((opt (print-type obj))
	 (npt (print-type nobj)))
    (cond ((and npt
		;; May be here by lcopy, in which case we still need to process it.
		(not (eq opt npt)))
	   (assert (or (not (fully-instantiated? modinst))
		       (fully-instantiated? nobj)))
	   nobj)
	  ((and opt
		(or (null npt)
		    (mappings modinst)
		    (free-params npt)))
	   (if (or (mappings modinst)
		   (not (fully-instantiated? opt)))
	       (if (and (type-name? opt) (eq obj (type (resolution opt))))
		   (let* ((odecl (declaration opt))
			  (ndecl (if (declaration? (cdr (assq odecl bindings)))
				     (cdr (assq odecl bindings))
				     odecl))
			  (smi (subst-mod-params* (module-instance opt) modinst bindings))
			  (dacts (when (eq ndecl *subst-mod-params-declaration*)
				   (dactuals smi)))
			  (npty (lcopy opt :actuals (actuals smi) :dactuals dacts))
			  (nmi (copy smi :dactuals dacts)))
		     (if (and (eq npt opt)
			      (eq nmi (module-instance opt))
			      (eq npty opt)
			      (eq odecl ndecl))
			 nobj
			 (let* ((cnobj (if (eq obj nobj)
					   (copy obj) (copy nobj)))
				(nres (mk-resolution ndecl nmi cnobj)))
			   (setf (resolutions npty) (list nres))
			   (setf (print-type cnobj) npty)
			   (assert (eq cnobj (type (resolution (print-type cnobj)))))
			   #+badassert ;; update-known-subtypes violates this
			   (assert (or (not (fully-instantiated? modinst))
				       (fully-instantiated? cnobj)))
			   cnobj)))
      		   (let* ((stype (subst-mod-params* opt modinst bindings))
      			  (pte (or (print-type stype) stype)))
		     #+badassert ;; update-known-subtypes violates this
		     (assert (or (not (fully-instantiated? modinst))
				 (fully-instantiated? pte)))
      		     (if (or (occurs-in nobj pte)
			     (and (mappings modinst)
				  (not (compatible? obj nobj))))
      			 (lcopy nobj :print-type nil)
      			 (if (eq obj nobj)
      			     (lcopy nobj :print-type pte)
      			     (progn (setf (print-type nobj) pte)
				    (setf (free-parameters nobj) 'unbound)
      				    nobj)))))
      	       (if (eq obj nobj)
      		   nobj
      		   (lcopy nobj :print-type nil))))
	  (t nobj))))

(defmethod subst-mod-params-print-type ((obj type-expr) (nobj type-name) modinst bindings)
  (declare (ignore bindings modinst))
  nobj)
  
(defmethod subst-mod-params-print-type (obj nobj modinst bindings)
  (declare (ignore obj modinst bindings))
  nobj)

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
    ;; modinst should be fully-instantiated, so formals should be in bindings
    ;; (when (mappings modinst)
    ;;   (type-error th "Mappings for new theories only available with theory-declarations"))
    (assert (every #'(lambda (d) (assq d bindings)) (formals-sans-usings th)))
    (when (some #'theory-reference? (formals th))
      (break "subst-mod-params* (module) with theory-references in formals"))
    (let* ((*subst-mod-params-module?* th)
	   ;;(fthrefs (remove-if-not #'theory-reference? (formals th)))
	   (nth (copy th
		  :id (if *subst-mod-params-declaration*
			  (makesym "~a.~a"
				   (id (current-theory))
				   (id *subst-mod-params-declaration*))
			  (id th))
		  :formals nil ;; modinst should be fully-instantiated
		  :formals-sans-usings nil
		  :place nil
		  :filename nil
		  :info nil
		  :warnings nil
		  :conversion-messages nil
		  :all-declarations nil
		  :nonempty-types nil
		  :saved-context nil
		  :ppe-form nil
		  :tcc-form nil
		  :typecheck-time nil
		  :immediate-usings 'unbound)))
      (multiple-value-bind (nass abindings)
	  (subst-mod-params-decls assuming modinst bindings nth)
	(multiple-value-bind (rtheory tbindings)
	    (subst-mod-params-decls theory modinst abindings nth)
	  (setf (assuming nth) nass
		(theory nth) (append (create-importings-for-bindings bindings)
				     (remove-if #'null rtheory))
		(exporting nth) (subst-mod-params* exporting modinst bindings))
	  (dolist (decl (all-decls nth))
	    (when (declaration? decl)
	      (setf (module decl) nth)
	      (setf (refers-to decl) (regenerate-xref decl))))
	  (setf (all-usings nth)
		(let ((imps nil))
		  (maphash #'(lambda (th thinsts)
			       (unless (and (from-prelude? th)
					    (singleton? thinsts))
				 (push (cons th thinsts) imps)))
			   (lhash-table (current-using-hash)))
		  imps))
	  (when (or (instances-used th)
		    (dependent-known-subtypes th)
		    (macro-expressions th)
		    (macro-subtype-tcc-args-alist th))
	    (break "subst-mod-params* module"))
	  (values nth tbindings))))))

(defun subst-mod-params-decls (decls modinst bindings nth &optional ndecls)
  (cond ((null decls)
	 (dolist (ndecl ndecls)
	   (mapcar #'(lambda (gdecl)
		       (or (cdr (assq gdecl bindings))
			   (and (rassoc gdecl bindings) gdecl)
			   (break "generated")))
	     (generated ndecl)))
	 (values (nreverse ndecls) bindings))
	((typep (car decls) '(or tcc-decl))
	 ;; Skip TCCs; they can be assumed
	 (subst-mod-params-decls (cdr decls) modinst bindings nth ndecls))
	(t (multiple-value-bind (nfmls fbindings)
	       (subst-mod-params-formal-decls (decl-formals (car decls)) (car decls) modinst nil)
	     (let* ((ndecl (copy (car decls)
			     :decl-formals nfmls
			     :generated-by (car decls) :module nth :place nil
			     :pvs-sxhash-value nil :ir-type-value nil
			     :newline-comment nil :semi t))
		    (bpair (assoc (car decls) bindings
				  :key #'(lambda (x) (if (mapping-lhs? x) (declaration x) x))))
		    (bdecl (car bpair))
		    (bval (cdr bpair))
		    ;; nbindings is for recursive decls, and for the remaining decls
		    ;; Note that this will shadow the bpair binding
		    (nbindings (if (assq (car decls) bindings)
				   bindings
				   (acons (car decls) ndecl bindings)))
		    ;; rbindings comes from mappings with decl-formals
		    (rbindings (when (mapping-lhs? bdecl)
				 (mapcar #'cons (decl-formals bdecl) (decl-formals ndecl))))
		    ;; fbindings maps old decl-formals of decl to new
		    (lbindings (append rbindings fbindings ;;nbindings
				       bindings)))
	       (assert (or (not (mapping-lhs? bdecl)) (mapping-rhs? bval)))
	       (dolist (nfml nfmls)
		 (setf (associated-decl nfml) ndecl))
	       (when (and (id (car decls)) *subst-mod-params-declaration*)
		 (setf (id ndecl) (makesym "~a.~a"
					   (id *subst-mod-params-declaration*) (id (car decls)))))
	       (setf (generated ndecl) (remove-if #'tcc? (generated ndecl)))
	       ;;(when (generated ndecl) (break "subst-mod-params-decls with generated"))
	       (when bval
		 (if (mapping-rhs? bval)
		     (cond ((type-value bval)
			    (let* ((tval (type-value bval))
				   (texp (or (print-type tval) tval)))
			      (assert (type-decl? ndecl))
			      (if (type-def-decl? ndecl)
				  (setf (type-value ndecl) tval
					(type-expr ndecl) (or (print-type tval) tval))
				  (change-class ndecl (if (nonempty-type-decl? ndecl)
							  'nonempty-type-eq-decl
							  'type-eq-decl)
				    :type-value tval :type-expr texp))))
			   ((const-decl? ndecl)
			    (if (definition ndecl)
				(break "Need to set const definition")
				(setf (definition ndecl) (expr bval))))
			   ((typep ndecl '(or theory-abbreviation-decl))
			    (assert (theory-name-expr? (expr bval)))
			    (setf (theory-name ndecl) (expr bval)))
			   (t (break "subst-mod-params-decls new ndecl class")))
		     ;; (subst-mod-params-decls (cdr decls) modinst bindings nth
		     ;; 		   (cons (declaration (actual-value bval)) ndecls))
		     (break "What is bval?")))
	       ;; Use generated-by to access the original, and to tell if this is new,
	       ;; in code that could be recursively called in different contexts.
	       #+badassert
	       (assert (not (some #'(lambda (bdg)
				      (and (decl-formal? (car bdg))
					   (not (memq (car bdg) (decl-formals (car decls))))))
				  nbindings))
		       () "Bad nbindings before")
	       (with-current-decl ndecl
		 (subst-mod-params* ndecl modinst lbindings))
	       #+badassert
	       (assert (not (some #'(lambda (bdg)
				      (and (decl-formal? (car bdg))
					   (not (memq (car bdg) (decl-formals (car decls))))))
				  nbindings))
		       () "Bad dbindings after")
	       (regenerate-xref ndecl)
	       (subst-mod-params-decls (cdr decls) modinst nbindings nth (cons ndecl ndecls)))))))

;;; Create the importings needed to reference the bindings
(defun create-importings-for-bindings (bindings &optional importings)
  (if (null bindings)
      (nreverse importings)
      (let ((importing (create-importing-for-binding
			(caar bindings)
			(cond ((mapping-rhs-rename? (cdar bindings))
			       nil)
			      ((actual? (cdar bindings))
			       (expr (cdar bindings)))
			      (t (cdar bindings))))))
	(when importing
	  (assert (fully-typed? (theory-name importing))))
	(create-importings-for-bindings
	 (cdr bindings)
	 (if (and importing
		  (not (member importing importings :test #'tc-eq)))
	     (cons importing importings)
	     importings)))))

(defmethod create-importing-for-binding ((decl formal-theory-decl) theory-name)
  (assert (modname? theory-name))
  (make-instance 'importing
    :theory-name theory-name))

(defmethod create-importing-for-binding ((decl mod-decl) theory-name)
  (when (modname? theory-name)
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

;;; Declaration level, primarily for mod-decls; always creates new decls
;;; This should only be called from subst-mod-params-decls, with a new decl

(defmethod subst-mod-params* :around ((imp importing) modinst bindings)
  (declare (ignore modinst bindings))
  (let ((nimp (call-next-method)))
    (when (and nimp (not (eq imp nimp)))
      (setf (generated-by nimp) imp)
      (regenerate-xref nimp))
    nimp))

(defmethod subst-mod-params* ((imp importing) modinst bindings)
  (with-slots (theory-name) imp
    (assert (resolution theory-name))
    (unless (assq (declaration theory-name) bindings)
      (let ((thname (subst-mod-params* theory-name modinst bindings)))
	(assert (fully-typed? thname))
	(lcopy imp :theory-name thname)))))

(defmethod subst-mod-params* ((decl theory-abbreviation-decl) modinst bindings)
  (with-slots (theory-name) decl
    (let ((thname (subst-mod-params* theory-name modinst bindings)))
      (lcopy decl :theory-name thname))))


(defmethod subst-mod-params* ((nrectype inline-recursive-type) modinst bindings)
  (assert (generated-by nrectype))
  (let ((nconstr (subst-mod-params* (constructors nrectype) modinst bindings))
	(ntype-name (subst-mod-params* (adt-type-name nrectype) modinst bindings))
	(gen-theories (subst-mod-params* (generated nrectype) modinst bindings)))
    (break "check this")
    (setf (constructors nrectype) nconstr
	  (adt-type-name nrectype) ntype-name
	  (generated nrectype) gen-theories)
    nrectype))

(defmethod subst-mod-params* ((adt recursive-type) modinst bindings)
  (break "Needs work")
  (let ((cadt
	 (copy adt
	   :constructors (subst-mod-params* (constructors adt) modinst bindings)
	   :adt-type-name (subst-mod-params* (adt-type-name adt) modinst bindings)
	   ;; set :generated later
	   :generated-by nil
	   :place nil
	   :formals nil
	   :formals-sans-usings nil
	   )))
    (change-class cadt (inlined-version adt))))

(defmethod subst-mod-params* ((c simple-constructor) modinst bindings)
  (lcopy c
    :recognizer (subst-mod-params* (recognizer c) modinst bindings)
    :arguments (subst-mod-params* (arguments c) modinst bindings)
    :con-decl (subst-mod-params* (con-decl c) modinst bindings)
    :rec-decl (subst-mod-params* (rec-decl c) modinst bindings)
    :acc-decls (subst-mod-params* (acc-decls c) modinst bindings)))

(defmethod subst-mod-params* ((decl adtdecl) modinst bindings)
  (if (rassoc decl bindings)
      (break "subst-mod-params* adtdecl")
      (let* ((nbd (subst-mod-params* (bind-decl decl) modinst bindings))
	     (alist (unless (eq nbd (bind-decl decl)) (acons (bind-decl decl) nbd nil))))
	(copy decl
	  :module (current-theory)
	  :generated-by decl
	  :declared-type (subst-mod-params* (declared-type decl) modinst bindings)
	  :type (subst-mod-params* (type decl) modinst bindings)
	  :bind-decl nbd
	  :accessor-decl (substit (subst-mod-params* (accessor-decl decl) modinst bindings)
			   alist)))))


(defmethod subst-mod-params* ((decl declaration) modinst bindings)
  (declare (ignore modinst bindings))
  (break "More methods, please"))

(defmethod subst-mod-params* ((ndecl type-decl) modinst bindings)
  (with-slots (type-value contains) ndecl
    (assert (generated-by ndecl)) ;; Should be new
    (let ((ntypeval (subst-mod-params* (type-value ndecl) modinst bindings)))
      (assert (fully-instantiated? ntypeval))
      (setf (type-value ndecl) ntypeval))
    ndecl))

(defmethod subst-mod-params* ((decl type-def-decl) modinst bindings)
  (with-slots (type-value type-expr contains) decl
    (let* ((tv (subst-mod-params* type-value modinst bindings))
	   (te (subst-mod-params* type-expr modinst bindings))
	   (co (subst-mod-params* contains modinst bindings))
	   (ndecl (if (rassoc decl bindings)
		      decl
		      (copy decl :generated-by decl))))
      (unless (fully-instantiated? (print-type tv))
	(if (eq tv type-value)
	    (setq tv (copy tv :print-type nil :free-parameters 'unbound))
	    (setf (print-type tv) nil
		  (free-parameters tv) 'unbound)))
      (assert (fully-instantiated? tv))
      (setf (type-value ndecl) tv
	    (type-expr ndecl) te
	    (contains ndecl) co)
      ndecl)))

(defmethod subst-mod-params* ((decl formal-theory-decl) modinst bindings)
  (with-slots (theory-name) decl
    (if (rassoc decl bindings)
	(break "Shouldn't get here")
	(lcopy decl
	  :theory-name (subst-mod-params* theory-name modinst bindings)
	  :generated-by decl))))

(defmethod subst-mod-params* ((decl mod-decl) modinst bindings)
  (with-slots (modname) decl
    (let ((adecl (cdr (assq decl bindings))))
      (if adecl
 	  (if (mapping-rhs-rename? adecl)
	      (let ((rdecl (declaration (expr adecl))))
		(assert (generated-by rdecl))
		rdecl)
	      (lcopy decl
		:modname (if (modname? (expr adecl))
			     (expr adecl)
			     (change-class (copy (expr adecl)) 'modname))
		:generated-by decl)
;; 	      (lcopy decl
;; 		:modname (module-instance (expr adecl))
;; 		:generated-by decl)
	      )
	  (let ((ndecl (if (rassoc decl bindings)
			   decl
			   (copy decl :generated-by decl))))
	    (setf (modname ndecl)
		  (subst-mod-params* modname modinst bindings)))))))

(defmethod subst-mod-params* ((decl var-decl) modinst bindings)
  (with-slots (declared-type type) decl
    (let* ((ntype (subst-mod-params* type modinst bindings))
	   (ndtype (subst-mod-params* declared-type modinst bindings))
	   (ndecl (if (rassoc decl bindings)
		      decl
		      (copy decl :generated-by decl))))
      (setf (type ndecl) ntype
	    (declared-type ndecl) ndtype))))

(defmethod subst-mod-params* ((ndecl const-decl) modinst bindings)
  (with-slots (decl-formals formals declared-type type definition def-axiom) ndecl
    (assert (generated-by ndecl)) ;; this is a new decl, will directly modify
    ;; (assert (rassoc ndecl bindings))
    (multiple-value-bind (afmls alist)
	(apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
			   (formals ndecl))
      (let ((ntype (substit (subst-mod-params* (type ndecl) modinst bindings) alist))
	    (ndtype (substit (subst-mod-params* (declared-type ndecl) modinst bindings) alist))
	    (ndef (substit (subst-mod-params* (definition ndecl) modinst bindings) alist))
	    ;; No need to substit here, as this is closed
	    ;; (ndefax (subst-mod-params* (def-axiom ndecl) modinst bindings))
	    )
	(unless (fully-instantiated? (print-type ntype))
	  (if (eq type ntype)
	      (setq ntype (copy type :print-type nil
				:free-parameters 'unbound))
	      (setf (print-type ntype) nil
		    (free-parameters ntype) 'unbound)))
	(assert (or (tc-eq type ntype)
		    (with-current-decl ndecl (fully-instantiated? ntype))))
	(assert (or (tc-eq (declared-type ndecl) ndtype)
		    (with-current-decl ndecl (fully-instantiated? ndtype))))
	(assert (or (tc-eq (definition ndecl) ndef)
		    (with-current-decl ndecl (fully-instantiated? ndef))))
	;; (assert (or (tc-eq (def-axiom ndecl) ndefax)
	;; 	    (with-current-decl ndecl (fully-instantiated? ndefax))))
	(setf (formals ndecl) afmls
	      (declared-type ndecl) ndtype
	      (type ndecl) ntype
	      (definition ndecl) ndef
	      (def-axiom ndecl) nil)
	(make-def-axiom ndecl)
	ndecl))))

(defmethod subst-mod-params-mapping ((decl const-decl) mapping modinst bindings)
  (with-slots (decl-formals formals declared-type type definition def-axiom) decl
    (let* ((map (assoc decl bindings :key #'declaration))
	   ;; FIXME - This is overkill - but if 2 different LHS mappings map
	   ;; to the same RHS, the cache will give the wrong answer, and this
	   ;; is a quick fix.  See Cesar/2018-09-12/library4/bigops.pvs
	   (*subst-mod-params-cache* (make-pvs-hash-table :strong-eq? t))
	   (*subst-mod-params-eq-cache* (make-hash-table :test 'eq)))
      (assert map)
      (multiple-value-bind (dfmls dacts)
	  (new-decl-formals decl)
	(let ((ndecl (copy decl
		       :decl-formals dfmls
		       :module (current-theory)
		       :generated-by decl
		       :semi t)))
	  (dolist (dfml dfmls)
	    (setf (associated-decl dfml) ndecl
		  (module dfml) (current-theory)))
	  (with-current-decl ndecl
	    (let* ((mbindings (append (pairlis (decl-formals mapping) dacts)
				      bindings))
		   (nmodinst (copy modinst :dactuals dacts))
		   (ltype (if (mapping-lhs? (lhs mapping))
			      (type (lhs mapping))
			      (type (resolution (lhs mapping)))))
		   (ntype (subst-mod-params* ltype nmodinst mbindings))
		   (rexpr (expr (rhs mapping)))
		   (ndef (if (fully-instantiated? rexpr)
			     rexpr
			     (subst-mod-params* rexpr nmodinst mbindings))))
	      (assert (compatible? (type ndef) ntype))
	      (assert (fully-instantiated? ntype))
	      (assert (fully-instantiated? ndef))
	      #+pvsdebug
	      (assert (or (null ndef)
			  (every #'(lambda (fp) (memq fp dfmls)) (free-params ndef))))
	      (setf (type ndecl) ntype)
	      (setf (definition ndecl) ndef)
	      ;; (setf (declared-type ndecl)
	      ;; 	  (subst-mod-params* declared-type modinst mbindings))
	      (assert (every #'(lambda (d) (memq d (flatten-list (formals ndecl))))
			     (mapcar #'declaration (freevars (definition ndecl)))))
	      ndecl)))))))


(defmethod subst-mod-params-mapping-rename ((decl const-decl) mapping modinst bindings)
  (with-slots (decl-formals formals declared-type type definition def-axiom) decl
    ;; Reuse the declaration created in the rhs
    (assert (resolution (expr (rhs mapping))))
    (let ((rdecl (declaration (expr (rhs mapping))))
	  ;; FIXME - This is overkill - but if 2 different LHS mappings map
	  ;; to the same RHS, the cache will give the wrong answer, and this
	  ;; is a quick fix.  See Cesar/2018-09-12/library4/bigops.pvs
	  (*subst-mod-params-cache* (make-pvs-hash-table :strong-eq? t))
	  (*subst-mod-params-eq-cache* (make-hash-table :test 'eq)))
      ;; Set the formals, type, and declared-type
      (multiple-value-bind (nfmls alist)
	  (apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
			     formals)
	(setf (formals rdecl) nfmls)
	(let ((rtype (substit (subst-mod-params* type modinst bindings) alist)))
	  (unless (fully-instantiated? (print-type rtype))
	    (if (eq type rtype)
		(setq rtype (copy type :print-type nil :free-parameters 'unbound))
		(setf (print-type rtype) nil
		      (free-parameters rtype) 'unbound)))
	  (setf (type rdecl) rtype))
	(setf (declared-type rdecl)
	      (substit (subst-mod-params* declared-type modinst bindings) alist)))
      (setf (semi rdecl) t)
      (assert (every #'(lambda (d) (memq d (flatten-list (formals rdecl))))
		     (mapcar #'declaration (freevars (definition rdecl)))))
      rdecl)))


;; formals are a list of lists of bindings, and need to handle dependencies

(defun subst-mod-params-def-formals (formals modinst theory decl)
  (subst-mod-params-def-formals* (car formals) (cdr formals) modinst theory decl))

(defun subst-mod-params-def-formals* (fmls fmls-list modinst theory decl &optional nfmls nfmls-list)
  (if (null fmls)
      (if (null fmls-list)
	  (nreverse (cons (nreverse nfmls) nfmls-list))
	  (subst-mod-params-def-formals* (car fmls-list) (cdr fmls-list) modinst theory decl
					 nil (cons (nreverse nfmls) nfmls-list)))
      (let* ((nfml (subst-mod-params (car fmls) modinst theory decl))
	     (alist (acons (car fmls) nfml nil)))
	(subst-mod-params-def-formals*
	 (substit (cdr fmls) alist)
	 (substit fmls-list alist)
	 modinst theory decl
	 (cons nfml nfmls) nfmls-list))))

(defmethod subst-mod-params* ((decl def-decl) modinst bindings)
  (with-slots (formals declared-type type definition declared-measure ordering) decl
    (multiple-value-bind (nfmls alist)
	(apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
			   formals)
      (let ((ndtype (substit (subst-mod-params* declared-type modinst bindings) alist))
	    (ntype (substit (subst-mod-params* type modinst bindings) alist))
	    (ndef (substit (subst-mod-params* definition modinst bindings) alist))
	    (ndmeas (substit (subst-mod-params* declared-measure modinst bindings) alist))
	    (nord (substit (subst-mod-params* ordering modinst bindings) alist))
	    (ndecl (if (rassoc decl bindings)
		       decl
		       (copy decl :generated-by decl))))
	(setf (formals ndecl) nfmls
	      (declared-type ndecl) ndtype
	      (type ndecl) ntype
	      (definition ndecl) ndef
	      (declared-measure ndecl) ndmeas
	      (ordering ndecl) nord)
	ndecl))))

(defmethod subst-mod-params* ((decl tcc-decl) modinst bindings)
  (declare (ignore modinst bindings))
  nil)

(defun subst-mod-params-formal-decls (fdecls decl modinst bindings &optional nfdecls)
  (if (null fdecls)
      (values (nreverse nfdecls) bindings)
      (let ((nfdecl (subst-mod-params* (car fdecls) modinst bindings)))
	(assert (formal-decl? nfdecl))
	(setf (associated-decl nfdecl) decl)
	(subst-mod-params-formal-decls
	 (cdr fdecls)
	 decl
	 modinst
	 (acons (car fdecls) nfdecl bindings)
	 (cons nfdecl nfdecls)))))

(defmethod subst-mod-params* ((decl decl-formal-type) modinst bindings)
  (declare (ignore bindings))
  (with-slots (declared-type type-value) decl
    (when (typep decl '(or decl-formal-struct-subtype decl-formal-subtype
			decl-formal-type-appl))
      (break "Need more methods in decl-formal-type"))
    (let* ((ndecl (copy decl :generated-by decl))
	   (nmi (if (eq (id (module decl)) (id modinst))
		    modinst
		    (break "what now?")))
	   (tn (mk-type-name (id decl))))
      (setf (resolutions tn) (list (mk-resolution ndecl nmi tn)))
      (setf (type ndecl) tn)
      (setf (type-value ndecl) tn)
      (assert (equal (free-params tn) (list ndecl)))
      ndecl)))

;; (defmethod subst-mod-params* ((decl decl-formal-const) modinst bindings)
;;   (break "decl-formal-const needs definition"))

;; (defmethod subst-mod-params* ((decl decl-formal-theory-decl) modinst bindings)
;;   (break "decl-formal-theory-decl needs definition"))


(defmethod subst-mod-params* ((ndecl formula-decl) modinst bindings)
  (with-slots (definition closed-definition) ndecl
    (assert (generated-by ndecl)) ;; Should be new
    (assert (every #'(lambda (df) (assq df bindings)) (decl-formals (generated-by ndecl))))
    (let* (;;(nmodinst (copy modinst :dactuals dacts))
	   (ndef (subst-mod-params* definition modinst bindings))
	   (ncdef (if (freevars ndef)
		      (subst-mod-params* closed-definition modinst bindings)
		      ndef))
	   (ety (nonempty-formula-type ndef)))
      (setf (kind ndecl) nil
	    (default-proof ndecl) nil
	    (proofs ndecl) nil)
      (assert (fully-instantiated? ndef))
      (assert (fully-instantiated? ncdef))
      (setf (definition ndecl) ndef
	    (closed-definition ndecl) ncdef)
      (when (and ety (not (possibly-empty-type? ety)))
	(set-nonempty-type ety ndecl))
      (change-to-mapped-formula-decl ndecl)
      ;; generated-by will be set to the mod-decl that generated this.
      (setf (from-formula ndecl) (generated-by ndecl))
      ndecl)))

(defmethod change-to-mapped-formula-decl ((decl formula-decl))
  (change-class decl 'mapped-formula-decl :spelling 'AXIOM))

(defmethod change-to-mapped-assuming-decl ((decl formula-decl))
  (change-class decl 'mapped-assuming-decl :spelling 'AXIOM))

(defmethod change-to-mapped-tcc-decl ((decl formula-decl))
  (change-class decl 'mapped-tcc-decl :spelling 'AXIOM))

(defmethod subst-mod-params* ((decl subtype-judgement) modinst bindings)
  (with-slots (declared-subtype) decl
    (break "Fill this in")
    (copy decl
      :declared-subtype (subst-mod-params* declared-subtype modinst bindings))))

(defmethod subst-mod-params* ((decl name-judgement) modinst bindings)
  (with-slots (name) decl
    (break "Fill this in")
    (lcopy decl :name (subst-mod-params* name modinst bindings))))

(defmethod subst-mod-params* ((decl application-judgement) modinst bindings)
  (with-slots (name declared-type type judgement-type formals) decl
    (if *subst-params-decl*
	(let ((nname (subst-mod-params* name modinst bindings)))
	  (when (name? nname)
	    (multiple-value-bind (nformals alist)
		(apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
				   formals)
	      (let* ((ntype (substit (subst-mod-params* type modinst bindings) alist))
		     (ndecl-type
		      (cond ((tc-eq declared-type type) ntype)
			    ((and (print-type type)
				  (typep declared-type '(or type-application)))
			     (substit (subst-mod-params* (print-type type) modinst bindings) alist))
			    (t (substit (subst-mod-params* declared-type modinst bindings) alist))))
		     (jtype (substit (subst-mod-params* judgement-type modinst bindings) alist))
		     (ndecl (if (rassoc decl bindings) ;; already new
				 decl
				 (copy decl))))
		(setf (declared-type ndecl) ndecl-type
		      (type ndecl) ntype
		      (judgement-type ndecl) jtype
		      (name ndecl) nname
		      (formals ndecl) nformals)
		#+pvsdebug (assert (every #'(lambda (fv) (memq (declaration fv) nlist))
					  (freevars (substit ndecl-type alist))))
		#+pvsdebug (assert (every #'(lambda (fv) (memq (declaration fv) nlist))
					  (freevars (substit ntype alist))))
		#+pvsdebug (assert (every #'(lambda (fv) (memq (declaration fv) nlist))
					  (freevars (subst-mod-params* judgement-type modinst bindings))))
		(values ndecl alist)))))
	(progn
	  (break "Check this")
	  (copy decl :name (subst-mod-params* name modinst bindings))))))

(defmethod subst-mod-params* ((decl rec-application-judgement) modinst bindings)
  (multiple-value-bind (ndecl alist)
      (call-next-method)
    (setf (rewrite-formula ndecl)
	  (substit (subst-mod-params* (rewrite-formula decl) modinst bindings) alist))
    ndecl))

(defmethod subst-mod-params* ((decl expr-judgement) modinst bindings)
  (break "expr-judgement")
  (multiple-value-bind (nfmls alist)
      (apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
			 (formals decl))
    (let ((nexpr (substit (subst-mod-params* (expr decl) modinst bindings) alist))
	  (ncform (substit (subst-mod-params* (closed-form decl) modinst bindings) alist))
	  (jtype (substit (subst-mod-params* (judgement-type decl) modinst bindings) alist)))
      (copy decl
	:formals nfmls
	:expr nexpr
	:closed-form ncform
	:judgement-type jtype))))

(defmethod subst-mod-params* ((decl conversion-decl) modinst bindings)
  (with-slots (expr) decl
    (let ((nexpr (subst-mod-params* expr modinst bindings)))
      (assert (null (freevars nexpr)))
      (setf (expr decl) nexpr))))


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
	 (bdg (cdr (assq decl bindings)))
	 (lhsmatch (unless bdg
		     (assoc decl bindings
			    :test #'(lambda (d bd)
				      (and (mapping-lhs? bd) (eq d (declaration bd))))))))
    (assert (or (null bdg) (not (fully-instantiated? modinst)) (fully-instantiated? bdg)))
    (assert (subsetp (free-params bdg) (free-params modinst)))
    ;; (when (and (decl-formal? decl)
    ;; 	       (not (memq decl (decl-formals *subst-mod-params-declaration*)))
    ;; 	       (not (memq decl (decl-formals (current-declaration)))))
    ;;   (break "type-name"))
    (cond ((actual? bdg)
	   (if (strong-tc-eq (type-value bdg) type) type (type-value bdg))
	   ;; sufficient since we know
	   ;; type name was found on the bindings, and the corresponding
	   ;; actual is there.  Here we actally do the substitution.
	   )
	  ((decl-formal-type? bdg)
	   ;;(assert (memq bdg (decl-formals (current-declaration))))
	   ;;(assert (fully-instantiated? (type-value bdg)))
	   (type-value bdg))
	  ((decl-formal? bdg) (break "Have unhandled decl-formal"))
	  ((type-def-decl? bdg)
	   (let ((res (mk-resolution bdg modinst (type-value bdg))))
	     (mk-type-name (id bdg) nil nil res)))
	  (bdg (break "What is this?"))
	  (lhsmatch
	   (let ((stype
		  (subst-for-formals
		   (type-value (cdr lhsmatch))
		   (mapcar #'(lambda (x y)
			       (cons x (type-value y)))
		     (decl-formals (car lhsmatch))
		     (dactuals type)))))
	     (subst-mod-params* stype modinst bindings)))
	  ;; ((and *subst-mod-params-module?*
	  ;; 	(eq (module decl) *subst-mod-params-module?*))
	  ;;  type)
	  (t (let* ((mi (module-instance res))
		    (nacts (cond ((actuals mi)
				  (subst-mod-params*
				   (actuals mi) modinst bindings))
				 ((eq (id mi) (id modinst))
				  (actuals modinst))))
		    (ndacts (cond ((dactuals mi)
				   (subst-mod-params*
				    (dactuals mi) modinst bindings))
				  (t ;;(eq decl *subst-mod-params-declaration*)
				   (dactuals modinst))))
		    (pt (when (print-type type)
			  (subst-mod-params* (print-type type) modinst bindings))))
	       #+pvsdebug (assert (not (and nacts
					    (eq (id mi) (id modinst)))))
	       #+pvsdebug (assert (fully-instantiated? nacts))
	       #+pvsdebug
	       (assert (or (mappings modinst)
			   (not (fully-instantiated? modinst))
			   (and (every #'(lambda (fp) (not (assq fp bindings)))
				       (free-params nacts))
				(every #'(lambda (fp) (not (assq fp bindings)))
				       (free-params ndacts)))))
	       (if (or nacts ndacts)
		   (if (and (eq (actuals mi) nacts)
			    (eq (dactuals mi) ndacts))
		       (lcopy type :print-type pt)
		       ;; If modinst has a library, but mi does not, we
		       ;; may have to add a library to mi.  If (module
		       ;; decl) is a library theory, definitely need it,
		       ;; but how do we know it is the same library?
		       (let* ((th (module decl))
			      (lib-id
			       (when (and (not (from-prelude-library? th))
					  (lib-datatype-or-theory? th))
				 (or (get-library-id (context-path th))
				     (pvs-error "Library reference error"
				       (format nil "Library id not found for ~a" (context-path th))))))
			      (nmi (copy mi :actuals nacts
					 :dactuals ndacts
					 :library lib-id))
			      (ntype (subst-mod-params-type-name type nmi bindings modinst)))
			 #+pvsdebug
			 (assert (or lib-id
				     (not (lib-datatype-or-theory?
					   (module decl)))
				     (from-prelude-library? decl)
				     (lib-datatype-or-theory?
				      (current-theory))
				     (file-equal (context-path (module decl))
						 *default-pathname-defaults*)))
			 (assert (or (not (strong-tc-eq type ntype)) (eq type ntype)))
			 ntype))
		   (if (eq (id mi) (id modinst))
		       ;; mi is useless after this
		       (subst-mod-params-type-name type modinst bindings modinst)
		       (lcopy type :print-type pt))))))))

(defun possibly-mapped-theory? (th bindings)
  (some #'(lambda (bdg)
	    (and (theory-reference? (car bdg))
		 (eq th (declaration (theory-name (car bdg))))))
	bindings))

;;; In the simple case,  copies the type with the new module
;;; instance in the newly created resolution
;;; Note that the modinst here is not the same as the one provided to
;;; subst-mod-params
(defun subst-mod-params-type-name (type tn-thinst bindings modinst)
  (declare (ignore bindings modinst))
  (assert (eq (id tn-thinst) (id (module-instance (resolution type)))))
  (let* ((res (car (resolutions type)))
	 (decl (declaration res)))
    (if (decl-formal? decl)
	(let ((ntype (resolve (id decl) 'type nil)))
	  (assert (type-name? ntype))
	  (if (strong-tc-eq ntype type) type ntype))
	(let* (;;(dacts (dactuals (module-instance type)))
	       ;;(bdg (cdr (assq decl bindings)))
	       (thinst (if (or (decl-formals decl) (null (dactuals tn-thinst)))
			   tn-thinst
			   (copy tn-thinst :dactuals nil)))
	       (nres (mk-resolution decl thinst nil))
	       (ntype (copy type
			:resolutions (list nres)
			:actuals (actuals thinst)
			:dactuals (dactuals thinst)
			:print-type nil))
	       (rtype (typecase decl
			(type-def-decl
			 (subst-mod-params (copy (type-value decl) :print-type nil)
			     thinst (module decl) decl))
			(type-decl ntype)
			(mapping
			 (assert (type-value (rhs decl)))
			 (type-value (rhs decl)))
			(t (error "bad type-decl")))))
	  (setf (type nres) rtype)
	  #+pvsdebug
	  (assert (subsetp (free-params ntype) (cons decl (free-params thinst))))
	  ;;(assert (or (not (fully-instantiated? thinst)) (fully-instantiated? ntype)))
	  (if (strong-tc-eq type ntype)
	      type
	      (adt-expand-positive-subtypes ntype))))))

;;; Given a type such as list[int], this creates the datatype-subtype
;;;  {x: list[number] | every(...)(x)}

(defun adt-expand-positive-subtypes (type)
  (if (adt-expand-positive-subtypes? type)
      (adt-expand-positive-subtypes! type)
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
      (assert (null (print-type type)))
      (lcopy type :type ntype :parameters nparms)))

(defmethod subst-mod-params* ((dtype dep-binding) modinst bindings)
  (with-slots (type declared-type) dtype
    (let* ((ntype (subst-mod-params* type modinst bindings))
	   (ndeclared-type (if (eq declared-type type)
			       ntype
			       (subst-mod-params* declared-type modinst bindings))))
    (if (and (eq type ntype)
	     (eq declared-type ndeclared-type))
	dtype
	(let ((ndep (copy dtype
		       :type ntype
		       :declared-type ndeclared-type)))
 	  (setf (resolutions ndep)
 		(list (mk-resolution ndep (current-theory-name) ntype)))
	  ndep)))))

(defmethod subst-mod-params* ((type expr-as-type) modinst bindings)
  (let* ((ntype (call-next-method)) ;; subtype will be next
	 (nexpr (subst-mod-params* (expr ntype) modinst bindings)))
    (if (everywhere-true? nexpr)
	(domain (type nexpr))
	(lcopy ntype :expr nexpr))))

(defmethod subst-mod-params* ((type datatype-subtype) modinst bindings)
  (lcopy (call-next-method)
    :declared-type (subst-mod-params* (declared-type type) modinst bindings)))

(defmethod subst-mod-params* ((type setsubtype) modinst bindings)
  (let ((ntype (call-next-method)))
    (cond ((eq ntype type) ntype)
	  ((setsubtype? ntype)
	   (multiple-value-bind (nfmls alist)
	       (apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
				  (formals type))
	     (setf (formals ntype) nfmls)
	     (setf (formula ntype)
		   (substit (subst-mod-params* (formula type) modinst bindings) alist))
	     ntype))
	  (t ntype))))

(defmethod subst-mod-params* ((type subtype) modinst bindings)
  (let ((act (cdr (assoc type bindings
			 :test #'formal-subtype-binding-match))))
    (if act
	(if (strong-tc-eq (type-value act) type) type (type-value act))
	(let ((stype (subst-mod-params* (supertype type) modinst bindings))
	      (spred (beta-reduce (subst-mod-params* (predicate type) modinst bindings))))
	  (if (everywhere-true? spred)
	      stype
	      (let* ((pred (if (eq spred (predicate type))
			       spred
			       (pseudo-normalize spred)))
		     (ntype (lcopy type
			      :supertype stype
			      :predicate pred)))
		(assert (or (not (strong-tc-eq type ntype)) (eq type ntype)))
		ntype))))))

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
      (assert (cadr ntypes))
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
	 (if (and (not (eq ntype (car types)))
		  (typep ntype 'dep-binding))
	     (substit (cdr types) (acons (car types) ntype nil))
	     (cdr types))
	 modinst bindings (cons ntype ntypes)))
      (nreverse ntypes)))

(defmethod subst-mod-params* ((type recordtype) modinst bindings)
  (with-slots (fields) type
    (let ((nfields (subst-mod-params-fields fields modinst bindings)))
      (if (equal nfields fields)
	  type
	  (let* ((dfields (dependent-fields? nfields))
		 (ntype (copy type
			  :fields (sort-fields nfields dfields)
			  :dependent? dfields)))
	    ntype)))))

(defun subst-mod-params-fields (fields modinst bindings &optional nfields)
  (if fields
      (let ((nfield (subst-mod-params* (car fields) modinst bindings)))
	#+pvsdebug
	(assert (every #'(lambda (fp) (not (assq fp bindings)))
		       (free-params nfield)))
	(subst-mod-params-fields
	 (if (eq (car fields) nfield)
	     (cdr fields)
	     (substit (cdr fields) (acons (car fields) nfield nil)))
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
  #+pvsdebug
  (assert (tc-eq (type expr) (type (resolution expr))))
  (let* ((decl (declaration expr))
	 (dacts (dactuals (module-instance expr)))
	 (bdg (cdr (assq decl bindings)))
	 (lhsmatch (unless bdg
		     (assoc decl bindings
			    :test #'(lambda (d bd)
				      (and (mapping-lhs? bd)
					   (eq d (declaration bd))))))))
    (cond ((declaration? bdg)
	   (let* ((ndacts (subst-mod-params* dacts modinst bindings))
		  (mi (if (eq (id (module-instance expr)) (id modinst))
			  (lcopy modinst :dactuals ndacts)
			  (progn (when (formals (module bdg))
				   (break "name-expr no actuals available"))
				 (make-theoryname (module bdg)))))
		  ;;(alist (mapcar #'cons (decl-formals decl) (decl-formals bdg)))
		  (ntype (subst-mod-params* (type expr) modinst bindings)) ; not (type bdg)
		  ;; (res (make-resolution bdg mi))
		  (res (mk-resolution bdg mi ntype))
		  (nex (mk-name-expr (id bdg) (actuals mi) nil res :dactuals (dactuals mi))))
	     (assert (fully-instantiated? ntype))
	     nex))
	  ((actual? bdg)
	   (let* ((ex (expr bdg))
		  (adecl (when dacts
			   (if (name-expr? ex)
			       (declaration ex)
			       (break "dacts1a"))))
		  (dfmls (when dacts (decl-formals adecl)))
		  (sdacts (when dacts (subst-mod-params* dacts modinst bindings)))
		  (nex (if dacts
			   (subst-for-formals
			    ex
			    (mapcar #'(lambda (x y) (cons x (type-value y)))
			      dfmls sdacts))
			   ex)))
	     #+badassert
	     (assert (every #'(lambda (fp)
				(or (memq fp (decl-formals (current-declaration)))
				    (memq fp (decl-formals *subst-mod-params-declaration*))))
			    (free-params nex)))
	     nex))
	  (lhsmatch
	   #+pvsdebug
	   (assert (or (null (decl-formals (car lhsmatch)))
		       (dactuals (module-instance expr))))
	   (let* ((lexpr (subst-for-formals
			  (expr (cdr lhsmatch))
			  (mapcar #'(lambda (x y)
				      (cons x (type-value y)))
			    (decl-formals (car lhsmatch))
			    dacts)))
		  (sexpr (subst-mod-params* lexpr modinst bindings)))
	     #+badassert
	     (assert (with-context *subst-mod-params-declaration*
		       (fully-instantiated? sexpr)))
	     sexpr))
	  (t (let* ((res (car (resolutions expr)))
		    (nres (subst-mod-params* res modinst bindings)))
	       (if (and (eq res nres)
			(every #'(lambda (fp) (not (assq fp bindings)))
			       (free-params nres)))
		   expr
		   (let ((nacts (when (or *smp-include-actuals*
					  (actuals expr)
					  ;; (not (tc-eq (module-instance res)
					  ;; 	      (module-instance nres)))
					  )
				  (mapcar #'copy (actuals (module-instance nres)))))
			 (ndacts (when (dactuals expr)
				   (mapcar #'copy (dactuals (module-instance nres)))))
			 #+pvsdebug
			 (ntype (subst-mod-params* (type expr) modinst bindings)))
		     #+pvsdebug
		     (assert (or (tc-eq ntype (type nres))
				 (and (eq (id (declaration nres)) '=)
				      (subtype? (car (types (domain ntype))))
				      (tc-eq (find-supertype (car (types (domain ntype))))
					     (car (types (domain (type nres))))))))
		     (copy expr
		       :actuals nacts
		       :dactuals ndacts
		       :resolutions (list nres)
		       :type (type nres)))))))))

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
  (let* ((ntype (subst-mod-params* (type bd) modinst bindings))
	 (ndeclared-type (unless *generating-mapped-axiom-tcc*
			   (if (eq (type bd) (declared-type bd))
			       ntype
			       (subst-mod-params* (declared-type bd)
						  modinst bindings))))
	 (clash? (some #'(lambda (sbd) (var-occurs-in (id bd) (cdr sbd))) bindings)))
    (if (and (strong-tc-eq (type bd) ntype)
	     (strong-tc-eq (declared-type bd) ndeclared-type)
	     (not clash?))
	bd
	(let ((nbd (copy bd
		     :id (if clash?
			     (make-new-variable (id bd) (mapcar #'cdr bindings) 1)
			     (id bd))
		     :type ntype
		     :declared-type (or (and ndeclared-type
					     (print-type ndeclared-type))
					ndeclared-type
					(print-type ntype)
					ntype))))
	  (when (resolutions bd)
	    (setf (resolutions nbd)
		  (list (mk-resolution nbd (current-theory-name) ntype))))
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
  (let ((nres (subst-mod-params* (resolution expr) modinst bindings)))
    (if (eq nres (resolution expr))
	expr
	(lcopy expr :resolutions (list nres) :type (type nres)))))

(defmethod subst-mod-params* ((expr field-application) modinst bindings)
  (with-slots (argument type) expr
    (let ((narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      #+pvsdebug (assert (fully-instantiated? ntype))
      (lcopy expr :argument narg :type ntype))))

(defmethod subst-mod-params* ((expr rational-expr) modinst bindings)
  (declare (ignore modinst bindings))
  expr)

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

(deftype number-declaration ()
  '(and const-decl (satisfies number-declaration?)))

(defmethod number-declaration? ((decl const-decl))
  (and (module decl)
       (eq (id (module decl)) '|numbers|)
       (integerp (id decl))))

(defmethod number-declaration? (obj)
  (declare (ignore obj))
  nil)
       

(defmethod subst-mod-params* ((expr record-expr) modinst bindings)
  (let ((ass (subst-mod-params* (assignments expr) modinst bindings))
	(type (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr :assignments ass :type type)))

(defmethod subst-mod-params* ((expr tuple-expr) modinst bindings)
  (let ((nexprs (subst-mod-params* (exprs expr) modinst bindings))
	(ntype (subst-mod-params* (type expr) modinst bindings)))
    (if (and (equal nexprs (exprs expr))
	     (eq ntype (type expr)))
	expr
	;; Mappings may affect the nexprs, without changing the type
	;; Need to recognize this case, and not do the mapping
	;; bugs/835 maps number to complex, and 0 to (0, 0),
	;; But then "x > 0" maps to "x > (0, 0)",
	;; (if (or t (every #'(lambda (a ty) (compatible? (type a) ty))
	;; 		 nexprs (types (find-supertype ntype))))
	;;     (lcopy expr :exprs nexprs :type ntype)
	;;     expr)
	(lcopy expr :exprs nexprs :type ntype))))

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
      #+badassert ;; bugs/887
      (assert (or (mappings modinst)
		  (compatible? (domain (find-supertype (type op))) (type arg))))
      (cond ((or (and (eq op operator)
		      (eq arg argument))
		 (not (compatible? (domain (find-supertype (type op))) (type arg))))
	     expr)
	    ((and (implicit-conversion? expr)
		  (name-expr? op)
		  (or (and (eq (id op) '|extend|)
			   (eq (id (module-instance (resolution op)))
			       '|extend|))
		      (and (eq (id op) '|restrict|)
			   (eq (id (module-instance (resolution op)))
			       '|restrict|)))
		  (tc-eq (car (actuals (module-instance (resolution op))))
			 (cadr (actuals (module-instance (resolution op))))))
	     arg)
	    (t (let* ((nop (if (and (implicit-conversion? op)
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
    (multiple-value-bind (nebindings alist)
	(apply-to-bindings #'(lambda (bd) (subst-mod-params* bd modinst bindings))
			   ebindings)
      (let* ((nexpr (substit (subst-mod-params* expression modinst bindings) alist))
	     (ntype (substit (subst-mod-params* type modinst bindings) alist)))
	(if (lambda-expr-with-type? expr)
	    (let ((nrettype (substit (subst-mod-params* (return-type expr) modinst bindings) alist))
		  (ndrettype (substit (subst-mod-params* (declared-ret-type expr) modinst bindings) alist)))
	      (lcopy expr
		:bindings (if (equal ebindings nebindings) ebindings nebindings)
		:expression nexpr
		:type ntype
		:declared-ret-type ndrettype
		:return-type nrettype))
	    (let ((nexpr (lcopy expr
			   :bindings (if (equal ebindings nebindings) ebindings nebindings)
			   :expression nexpr
			   :type ntype)))
	      (if (strong-tc-eq nexpr expr) expr nexpr)))))))

(defun subst-mod-params-bindings (ebindings modinst bindings)
  (subst-mod-params-bindings* ebindings ebindings modinst bindings nil))

(defun subst-mod-params-bindings* (ebindings obindings modinst bindings
					     nbindings)
  (if ebindings
      (let ((nbinding (subst-mod-params* (car ebindings) modinst bindings)))
	(subst-mod-params-bindings*
	 (if (or (eq (car ebindings) nbinding)
		 (not (binding? (car ebindings))))
	     (cdr ebindings)
	     (substit (cdr ebindings)
	       (acons (car ebindings) nbinding
		      (acons (car obindings) nbinding nil))))
	 (cdr obindings) modinst bindings (cons nbinding nbindings)))
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
			     (actuals name)
			     ;; (not (tc-eq (module-instance res)
			     ;; 		 (module-instance nres)))
			     )
		     (mapcar #'copy (actuals (module-instance nres))))
	  :dactuals (when (dactuals name)
		     (mapcar #'copy (dactuals (module-instance nres))))
	  :resolutions (list nres)))))

(defmethod subst-mod-params* ((mn modname) modinst bindings)
  (with-slots (id actuals dactuals mappings) mn
    (assert (resolution modinst))
    ;;(assert (resolution mn))
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
	      (progn (assert (resolution (cdr entry)))
		     (cdr entry))
	      ;; Binding could be an actual or mapping rhs
	      (let* ((name (expr (cdr entry)))
		     (reses (remove-if-not #'(lambda (r)
					       (module? (declaration r)))
			      (resolutions name))))
		(cond ((null reses)
		       (mk-modname (id name)
			 (actuals name)
			 (library name)
			 (mappings name)))
		      ((cdr reses)
		       (break "subst-mod-params* modname multiple reses?"))
		      (t (module-instance (car reses))))))
	  (cond ((and (eq id (id modinst))
		      (null actuals)
		      (null mappings)
		      (null dactuals))
		 ;; Can't just take the modinst, as declaration actuals may be
		 ;; different
		 (if t ;; (library modinst)
		     modinst
		     (lcopy modinst :library (library mn) :dactuals nil)))
		(t (let* ((nacts (if actuals
				     (subst-mod-params* actuals modinst bindings)
				     (when (eq id (id modinst)) (actuals modinst))))
			  (ndacts (if dactuals
				      (subst-mod-params* dactuals modinst bindings)
				      (when (eq id (id modinst)) (dactuals modinst))))
			  (nmaps (subst-mod-params* mappings modinst bindings))
			  (thinst (lcopy mn :actuals nacts :dactuals ndacts :mappings nmaps))
			  (theory (if (resolution thinst)
				      (declaration (resolution thinst))
				      (or (get-theory thinst)
					  (break "subst-mod-params* modname problem"))))
			  (tres (mk-resolution theory thinst nil)))
		     (setf (resolutions thinst) (list tres))
		     thinst)))))))

(defmethod subst-mod-params* ((act actual) modinst bindings)
  (with-slots (expr type-value) act
    (if (and (typep expr 'name)
	     (assq (declaration expr) bindings))
	(let ((nact (cdr (assq (declaration expr) bindings))))
	  (typecase nact
	    (actual (if (or (and type-value (type-value nact))
			    (and (null type-value) (null (type-value nact))))
			nact
			(lcopy nact
			  :expr (lcopy (expr nact) :parens (parens expr))
			  :type-value (subst-mod-params*
				       type-value modinst bindings))))
	    (decl-formal-type (mk-actual (type-value nact)))
	    (decl-formal (break "need more cases"))
	    (type-decl (let* ((nthname (if (eq (declaration modinst) (module nact))
					   modinst
					   (make-theoryname (module nact))))
			      (nres (make-resolution nact nthname))
			      ;; (tname (mk-type-name (id nact) (actuals nthname) nil nres
			      ;; 			   :dactuals (dactuals nthname)))
			      )
			 (mk-res-actual (type nres) (module nact))))
	    (t (break "More needed"))))
	(let ((ntype (when type-value
		       (subst-mod-params* type-value modinst bindings))))
	  (lcopy act
	    :expr (if ntype
		      (if (eq ntype type-value)
			  expr
			  (or (print-type ntype)
			      (subst-mod-params* expr modinst bindings)
			      ntype))
		      (let ((nexpr (subst-mod-params* expr modinst bindings)))
			(if (eq nexpr expr)
			    expr
			    (pseudo-normalize nexpr))))
	    :type-value ntype)))))

(defmethod subst-mod-params* ((map mapping) modinst bindings)
  (with-slots (lhs rhs declared-type type) map
    ;; Does it make sense to substitute the lhs?
    (let ((ntype (subst-mod-params* type modinst bindings)))
      (lcopy map
	:lhs (lcopy lhs :resolutions (subst-mod-params* (resolutions lhs) modinst bindings))
	:rhs (subst-mod-params* rhs modinst bindings)
	:declared-type (if (eq declared-type type)
			   ntype
			   (subst-mod-params* declared-type modinst bindings))
	:type ntype))))

;;; A mapping-rhs is an actual
;; (defmethod subst-mod-params* ((rhs mapping-rhs) modinst bindings)
;;   (call-next-method))


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
    (let ((acts (actuals mi))
	  (dacts (dactuals mi))
	  (bdg (cdr (assq decl bindings))))
      #+pvsdebug (assert (not (assq decl bindings)))
      #+pvsdebug (assert mi)
      (cond ((and (null bdg) (binding? decl))
	     (let* ((ntype (subst-mod-params* type modinst bindings))
		    (nres (if (eq ntype type)
			      res
			      (mk-resolution decl modinst ntype))))
	       nres))
	    ((declaration? bdg)
	     (let* ((nmi (subst-mod-params* mi modinst bindings))
		    (ntype (unless (and (type-name? type)
					(eq (resolution type) res))
			     (subst-mod-params* type modinst bindings)))
		    (nres (if ntype
			      (mk-resolution bdg nmi ntype)
			      (make-resolution bdg nmi))))
	       (assert (or (not (fully-instantiated? nmi))
			   (fully-instantiated? ntype)))
	       (assert (or (not (fully-instantiated? nmi))
			   (fully-instantiated? nres)))
	       nres))
	    ((tc-eq mi modinst)
	     #+pvsdebug
	     (assert (subsetp (free-params res) (free-params modinst)) () "res1")
	     res)
	    ((and (eq (id mi) (id modinst))
		  (or (null acts)
		      (actuals-are-formals? acts)))
	     ;; Note: mappings may change the type of a resolution for an
	     ;; uninterpreted constant.
	     (let* ((ntype (subst-mod-params* type modinst bindings))
		    (libid (or (library mi) (library modinst)))
		    (nmi (if (decl-formals decl)
			     (if (dactuals mi)
				 (subst-mod-params* mi modinst bindings)
				 (lcopy modinst :library libid))
			     ;;(lcopy modinst :dactuals nil :library libid)
			     (if (or (actuals mi) (dactuals modinst))
				 (progn (assert (null (dactuals mi)))
					mi)
				 modinst)))
		    (nres (if (and (eq ntype type) (eq mi nmi))
			      res
			      (mk-resolution decl nmi ntype))))
	       (assert (or (not (strong-tc-eq type ntype)) (eq type ntype)))
	       (assert (or (not (strong-tc-eq mi nmi)) (eq mi nmi)))
	       (assert (or (not (strong-tc-eq res nres)) (eq res nres)))
	       #+pvsdebug
	       (assert (subsetp (remove-if #'(lambda (d)
					       (memq d (decl-formals (current-declaration))))
				  (free-params ntype))
				(free-params modinst)) () "res2")
	       #+pvsdebug
	       (assert (subsetp (remove-if #'(lambda (fp)
					       (or (memq fp (formals-sans-usings
							     *subst-mod-params-theory*))
						   (memq fp (decl-formals (current-declaration)))))
				  (free-params nres))
				(free-params modinst)) () "res2.5")
	       nres))
	    (t (let ((nacts (when acts (subst-mod-params* acts modinst bindings)))
		     (ndacts (when (decl-formals decl)
			       (if dacts
				   (subst-mod-params* dacts modinst bindings)
				   (when (length= (decl-formals decl) (dactuals modinst))
				     (dactuals modinst))))))
		 (assert (every #'actual? nacts))
		 (if (and (eq nacts acts)
			  (eq ndacts dacts)
			  (not (binding? decl))
			  (null (mappings modinst)))
		     (progn
		       ;; This assertion is too strong if subst-mod-params is called
		       ;; from e.g., instantiate-resolution
		       #+pvsdebug
		       (assert (subsetp (free-params res) (free-params modinst))
			       () "res3")
		       res)
		     (let ((ntype (subst-mod-params* type modinst bindings)))
		       ;; (assert (or (mappings modinst)
		       ;; 		   (subsetp (free-params ntype) (free-params modinst)))
		       ;; 	       () "res4")
		       (if (and (strong-tc-eq type ntype)
				(strong-tc-eq nacts acts)
				(strong-tc-eq ndacts dacts)
				(not (memq (id mi) '(|equalities| |notequal|)))
				(not (lib-datatype-or-theory? (module decl))))
			       ;; If mappings are present, the resolution may be
			       ;; mapped inconsistently with the declaration type
			       ;; bugs/835 shows this, mapping "i /= 0" to "i /= (0, 0)"
			       ;;(and (mappings modinst)
				;;    ;; Note that compatible? allows for formals matching actuals
				;;    (not (compatible? type ntype))))
			   (progn #+pvsdebug
				  (assert (or (mappings modinst)
					      (subsetp (free-params res)
						       (free-params modinst))) () "res5")
				  res)
			   (let* ((rhs (cdr (assq (module decl) bindings)))
				  (nmappings (when rhs (mappings (expr rhs))))
				  (eqtype (when (memq (id mi)
						      '(|equalities| |notequal|))
					    (find-supertype
					     (type-value (car nacts)))))
				  ;;(imps (get-importings (module (declaration res))))
				  (imps (gethash (module (declaration res))
						 (lhash-table (current-using-hash))))
				  (imp-mi (best-module-instance imps mi))
				  (mi-th (or (get-theory imp-mi)
					     (and (same-id (module decl) imp-mi)
						  (module decl))
					     (break "what now?")))
				  (nmi (mk-modname (id imp-mi)
					 (if eqtype
					     (list (mk-actual eqtype))
					     nacts)
					 (when (and (lib-datatype-or-theory? (module decl))
						    (not (from-prelude? (module decl))))
					   (get-library-id (context-path (module decl))))
					 (or nmappings
					     (mappings imp-mi)
					     (unless (or (eq type ntype)
							 (not (eq (id mi) (id modinst)))
							 (not (eq (library mi)
								  (library modinst))))
					       (mappings modinst)))
					 ndacts
					 decl))
				  (tres (mk-resolution mi-th nmi nil))
				  (nrtype (if eqtype
					      (mk-funtype (list eqtype eqtype) *boolean*)
					      ntype))
				  (nres (mk-resolution decl nmi nrtype)))
			     (setf (resolutions nmi) (list tres))
			     (if (strong-tc-eq nres res) res nres)))))))))))

(defun best-module-instance (imps thinst)
  "Finds the most mapped instance that includes any mappings from thinst"
  (if (null imps)
      thinst
      (let ((imp (car imps)))
	(if (and (> (length (mappings imp)) (length (mappings thinst)))
		 (every #'(lambda (map)
			    (member map (mappings imp)
				    :test #'(lambda (m1 m2)
					      (eq (declaration (lhs m1))
						  (declaration (lhs m2))))))
			(mappings thinst)))
	    (best-module-instance (cdr imps) imp)
	    (best-module-instance (cdr imps) thinst)))))

(defmethod make-resolution (decl modinst &optional type
				 (sdecl (current-declaration)))
  (declare (ignore sdecl))
  (assert (modname? modinst))
  #+pvsdebug
  (assert (or (null type)
	      (not (fully-instantiated? modinst))
	      (fully-instantiated? type)))
  ;;(assert (get-theory modinst))
  (let* ((*smp-include-actuals* t)
	 (theory (if (and (module decl)
			  (eq (id (module decl)) (id modinst)))
		     (module decl)
		     (get-theory modinst)))
	 (mi (cond ((lib-datatype-or-theory? theory)
		    (if (library modinst)
			modinst
			(lcopy modinst :library (get-library-id theory))))
		   (theory (lcopy modinst :library nil))
		   (t modinst)))
	 (rtype (if type
		    (subst-mod-params type mi theory decl)
		    (typecase decl
		      ((or type-decl formal-type-decl)
		       (let ((*free-bindings*
			      (append (apply #'append (formals decl))
				      *free-bindings*)))
			 (subst-mod-params (type-value decl) mi theory decl)))
		      ((or expname typed-declaration simple-decl)
		       (subst-mod-params (type decl) mi theory decl))))))
    ;;(assert (or (not (fully-instantiated? mi)) (fully-instantiated? rtype)))
    (assert (current-declaration))
    (assert theory)
    ;;(assert (get-theory mi))
    ;; (assert (let ((tifps (free-params modinst))
    ;; 		  (dfmls (decl-formals (current-declaration))))
    ;; 	      (every #'(lambda (fp) (or (memq fp tifps) (memq fp dfmls)))
    ;; 		     (free-params rtype))))
    (mk-resolution decl (lcopy mi :resolutions nil) rtype)))

(defmethod make-resolution ((decl decl-formal) modinst &optional type
			    (cdecl (current-declaration)))
  (assert (or modinst type))
  (assert (memq decl (decl-formals cdecl)))
  (if (memq decl (decl-formals (current-declaration)))
      (mk-resolution decl modinst (type-value decl))
      (break "Check make-resolution (decl-formal)")))

(defmethod make-resolution ((decl binding) modinst &optional type
			    (sdecl (current-declaration)))
  (assert (or modinst type))
  #+pvsdebug (assert (or (null type)
			 (eq modinst (current-theory-name))
			 (not (fully-instantiated? modinst))
			 (fully-instantiated? type)))
  (let* ((dtype (or type (type decl)))
	 (mi (or modinst (current-theory-name)))
	 (stype (subst-mod-params dtype mi nil sdecl)))
    ;; Not true when conversions are being applied
    ;;(assert (or (typep (current-declaration) '(or conversion-decl importing))
    ;;   (fully-instantiated? stype)))
    (mk-resolution decl
      mi
      (if (and modinst (not (eq modinst (current-theory-name))))
	  stype
	  dtype))))

(defun theories-of-param-alist (alist)
  (let ((theories nil))
    (dolist (elt alist)
      (pushnew (module (car elt)) theories :test #'tc-eq))
    theories))

(defun theory-insts-of-param-alist (alist)
  (let ((theories (theories-of-param-alist alist)))
    (values (mapcar #'(lambda (th)
			(let* ((thinst
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
			       (res (mk-resolution th thinst nil)))
			  (setf (resolutions thinst) (list res))
			  thinst))
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
