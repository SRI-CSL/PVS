;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typecheck.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 19:01:35 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Mon May 24 17:48:12 2004
;; Update Count    : 36
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top level typechecking functions

(defmethod typecheck ((m module) &key expected context tccs)
  (declare (ignore expected context tccs))
  (let ((*generate-tccs* 'all))
    (typecheck* m nil nil nil)))


;;; Typecheck, returning the original object.  The gen-tccs keyword indicates
;;; that all tccs must be generated, even if the expression is fully
;;; typechecked.  This is for the prover, which may be using the formula in
;;; different contexts.

(defmethod typecheck (obj &key expected (context *current-context*)
			  (tccs nil given))
  (assert context)
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (assert *generate-tccs*)
    (typecheck* obj expected nil nil))
  obj)


;;; Typecheck, returning the canonical form

(defmethod typecheck ((te type-expr) &key expected (context *current-context*)
		      (tccs nil given))
  (assert context)
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (typecheck* te expected 'type nil)))

(defmethod typecheck ((ex expr) &key expected (context *current-context*)
		      (tccs nil given))
  (assert context)
  ;;(assert (or (not given) expected (type ex)))
  (assert (memq tccs '(nil none all top)))
  (let* ((*current-context* context)
	 (*generate-tccs* (if given tccs *generate-tccs*)))
    (assert *generate-tccs*)
    (cond ((type ex)
	   (cond ((eq *generate-tccs* 'all)
		  (call-next-method))
		 (expected
		  (set-type ex (or expected (type ex))))))
	  (t (call-next-method)
	     (unless (or expected (type ex))
	       (let ((type (get-unique-type ex)))
		 (when type
		   (set-type ex type))))))
    ex))

(defvar *empty-expression-types* (make-hash-table :test 'eq))

(defmethod typecheck :around (obj &key expected context tccs)
  (declare (ignore expected context tccs))
  (protect-types-hash obj (call-next-method)))

(defmethod types ((ex expr))
  (gethash ex *expression-types*))

(defmethod (setf types) (types (ex expr))
  (if types
      (setf (gethash ex *expression-types*) types)
      (remhash ex *expression-types*)))

(defmethod get-unique-type ((ex name-expr))
  (let ((freses (if (cdr (resolutions ex))
		    (filter-local-resolutions (resolutions ex))
		    (resolutions ex))))
    (setf (resolutions ex) freses)
    (if (singleton? freses)
	(let ((type (type (car freses))))
	  (when (fully-instantiated? type)
	    type))
	(call-next-method))))

(defmethod get-unique-type ((ex expr))
  (let ((types (remove-duplicates (ptypes ex) :test #'tc-eq)))
    (if (and (singleton? types)
	     (fully-instantiated? (car types)))
	(car types)
	(if (every #'(lambda (ty)
		       (compatible? ty (car types)))
		   (cdr types))
	    (let ((ctype (reduce #'compatible-type types)))
	      (when (fully-instantiated? ctype)
		ctype))
	    (type-ambiguity ex)))))

(defun typecheck-uniquely (expr &key (tccs 'all given))
  (let ((*generate-tccs* (if given tccs *generate-tccs*)))
    (typecheck* expr nil nil nil)
    (cond ((and (null (type expr))
		(not (every #'(lambda (ty)
				(compatible? ty (car (types expr))))
			    (cdr (types expr)))))
	   (unless *suppress-printing*
	     (if (types expr)
		 (type-ambiguity expr)
		 (type-error expr
		   "~%Given expression does not typecheck uniquely.~%")))
	   (type-ambiguity expr))
	  ((not (fully-instantiated? (car (types expr))))
	   (unless *suppress-printing*
	     (type-error expr
	       "Could not determine the full theory instance")))
	  (t (set-type expr (car (types expr))))))
  expr)


;;; Typecheck* methods for theories - returns the theory

(defvar *subtype-of-hash*)

(defmethod typecheck* ((m module) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (and (memq 'typechecked (status m)) (typechecked? m))
    (let ((*subtype-of-hash* (make-hash-table :test #'equal))
	  (*subtype-names* nil)
	  (*assert-if-arith-hash* (make-hash-table :test #'eq))
	  (*bound-variables* *bound-variables*)
	  ;;(*set-type-exprs* nil)
	  )
      (reset-pseudo-normalize-caches)
      (reset-beta-cache)
      (if (boundp '*subst-fields-hash*)
	  (clrhash *subst-fields-hash*)
	  (setq *subst-fields-hash* (make-pvs-hash-table)))
      (tcdebug "~%Typecheck ~a" (id m))
      (setf (formals-sans-usings m)
	    (remove-if #'importing-param? (formals m)))
      (setf (all-imported-theories m) 'unbound)
      (let* ((*typechecking-module* t)
	     (*tccs* nil)
	     (*tccdecls* nil)
	     (*tccforms* nil)
	     (*exprs-generating-actual-tccs* nil)
	     (*current-context* (if (eq (current-theory) m)
				    *current-context*
				    (make-new-context m))))
	(tcdebug "~%  Processing formals")
	(typecheck-decls (remove-if #'generated-by (formals m)))
	(set-dependent-formals (formals-sans-usings m))
	(tcdebug "~%  Processing assuming")
	(when (and (assuming m)
		   (null (formals-sans-usings m)))
	  (type-error m
	    "Theory ~a has no formal parameters, hence no need for ASSUMING section"
	    (id m)))
	(typecheck-decls (remove-if #'generated-by (assuming m)))
	(tcdebug "~%  Processing theory")
	(typecheck-decls (remove-if #'generated-by (theory m)))
	(tcdebug "~%  Processing exporting")
	(generate-xref m)
	(assert (eq (current-theory) m))
	(setf (all-usings m)
	      (let ((imps nil))
		(maphash #'(lambda (th thinsts)
			     (unless (and (from-prelude? th)
					  (singleton? thinsts))
			       (push (cons th thinsts) imps)))
			 (lhash-table (current-using-hash)))
		imps))
	(check-exporting m)
	(setf (dependent-known-subtypes m)
	      (remove-if (complement
			  #'(lambda (elt)
			     (some #'(lambda (fp)
				       (memq (declaration fp)
					     (formals-sans-usings m)))
				   (free-params elt))))
		(current-known-subtypes)))
	(setf (saved-context m) *current-context*))
      (push 'typechecked (status m))
      m)))

(defun set-dependent-formals (formals)
  (dolist (fm formals)
    (typecase fm
      (formal-theory-decl
       (let ((fparms (free-params (theory-name fm))))
	 (when (some #'(lambda (fp)
			 (and (not (eq fp fm))
			      (memq fp formals)))
		     fparms)
	   (setf (dependent? fm) t))))
      (formal-subtype-decl
       (let ((fparms (free-params (type-value fm))))
	 (when (some #'(lambda (fp)
			 (and (not (eq fp fm))
			      (memq fp formals)))
		     fparms)
	   (setf (dependent? fm) t))))
      (formal-const-decl
       (let ((fparms (free-params (type fm))))
	 (when (some #'(lambda (fp)
			 (and (not (eq fp fm))
			      (memq fp formals)))
		     fparms)
	   (setf (dependent? fm) t)))))))

;(defun adt-generating-theory (theory)
;  (unless *generating-adt*
;    (let ((cmt (comment theory)))
;      (when (and cmt (string= cmt "% Generated from file " :end1 22))
;	(let ((*typechecking-module* nil))
;	  (typecheck-file (subseq cmt 22 (- (length cmt) 4))))))))

(defmethod typecheck* ((list list) expected kind args)
  (typecheck*-list list expected kind args))

(defun typecheck*-list (list expected kind args &optional result)
  (if (null list)
      (nreverse result)
      (let ((obj (typecheck* (car list) expected kind args))
	    (*bound-variables* (cond ((binding? (car list))
				      (cons (car list) *bound-variables*))
				     ((and (listp (car list))
					   (every #'binding? (car list)))
				      (append (car list) *bound-variables*))
				     (t *bound-variables*))))
	(typecheck*-list (cdr list) expected kind args
			 (cons obj result)))))


(defmethod typecheck* ((use importing) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck-using (theory-name use))
  ;; This may make restoring binfiles faster, but has problems otherwise
  ;; (let ((follows (cadr (memq use (all-decls (current-theory))))))
  ;;   (unless (or (null follows) (importing-entity? follows))
  ;;     (setf (saved-context use) (copy-context *current-context*))))
  (setf (saved-context use) (copy-context *current-context*))
  use)

(defun typecheck-using (theory-inst)
  (let ((lib-id (library theory-inst)))
    (when lib-id
      (let ((libpath (get-library-reference lib-id)))
	(unless libpath
	  (type-error theory-inst "Cannot find library ~a" lib-id))
	(when (file-equal libpath *default-pathname-defaults*)
	  (type-error theory-inst
	    "Library \"~a\" refers to the current PVS context - it must be external"
	    lib-id)))))
  (when (and (null (library theory-inst))
	     (eq (id theory-inst) (id (current-theory))))
    (type-error theory-inst "A theory may not import itself"))
  (when (and (not (theory-abbreviation-decl? (current-declaration)))
	     (member (cons (current-theory) (current-declaration)) *tc-theories*
		     :test #'equal))
    (type-error theory-inst
      "Circularity found in importings of theory ~a" theory-inst))
  (let* ( ;; Need to keep track of where we are for untypechecking
	 ;; Everything after this will be untypechecked if
	 ;; something changed underneath
	 (*tc-theories* (acons (current-theory) (current-declaration)
			       *tc-theories*))
	 (plib-context (prelude-context *workspace-session*))
	 (mod (get-typechecked-theory theory-inst)))
    (unless mod
      (type-error theory-inst
	"Cannot find theory ~a" theory-inst))
    ;; (let* ((*generate-tccs* 'none)
    ;; 	   (thname (if (recursive-type? mod)
    ;; 		       (copy theory-inst :id (id (adt-theory mod)))
    ;; 		       theory-inst))
    ;; 	   (reses (resolve thname 'module nil)))
    ;;   ;;(typecheck* thname nil nil nil)
    ;;   ;;(unless (eq theory-inst thname)
    ;;   ;;  (setf (resolutions theory-inst) (resolutions thname)))
    ;;   (when reses
    ;; 	(setf (resolutions theory-inst) reses)))
    ;; If typecheck* ended up loading a new prelude library,
    ;; we need to update the current context.
    (assert (saved-context mod))
    ;;(assert (or (null mod) (eq (id mod) (id (declaration theory-inst)))))
    (when (context-difference? plib-context (prelude-context *workspace-session*))
      (setf (lhash-next (using-hash *current-context*))
	    (using-hash (prelude-context *workspace-session*)))
      (setf (lhash-next (declarations-hash *current-context*))
	    (declarations-hash (prelude-context *workspace-session*))))
    (when (and *tc-add-decl*
	       ;; Check for circularities
	       (memq (current-theory) (all-importings mod)))
      (type-error theory-inst
	"Circularity found in importings of theory ~a" theory-inst))
    (typecheck-using* mod theory-inst)))

(defun context-difference? (old-ctx new-ctx)
  (if (null old-ctx)
      (not (null new-ctx))
      (not (eq old-ctx new-ctx))))

(defmethod typecheck-using* (obj inst)
  (declare (ignore obj))
  (type-error inst "Theory ~a not found" (id inst)))

(defmethod typecheck-using* ((th module) inst)
  "Typechecks the modname inst which should be an instance of module th.
inst has form lib@thid[acts]{{mappings}}:->target

First the actuals are typechecked, then the target and mappings are merged
and the resulting list is typechecked.  The resolution is set, and the inst
is added to the context importings.  When there are mappings, mapped axiom
TCCs are generated, and finally exportings are updated."
  (let* ((nthinst inst)
	 (*typecheck-using* inst))
    (when (actuals inst)
      (unless (length= (formals-sans-usings th) (actuals inst))
	(type-error inst "Wrong number of actuals in ~a" inst))
      (typecheck-actuals inst)
      ;; typecheck-mappings done by determine-implicit-mappings
      ;;(typecheck-mappings (mappings inst) inst)
      (setq nthinst (set-type-actuals inst th))
      (unless (if (actuals inst)
		  (fully-instantiated? (actuals inst))
		  (fully-instantiated? (copy inst :mappings nil)))
	(type-error inst "Importing actuals must be fully instantiated"))
      ;; set-type-actuals already does this
      ;;(check-compatible-params (formals-sans-usings th)
      ;;		    (actuals inst) nil)
      )
    (let* ((tgt-name (target inst))
	   (tgt-theory (when tgt-name (get-typechecked-theory tgt-name)))
	   (tgt-mappings (determine-implicit-mappings
			  th inst tgt-name tgt-theory)))
      (when (or tgt-mappings (mappings inst))
	(if tgt-mappings
	    ;; Note that tgt-mappings includes the (mappings inst)
	    (setq nthinst (set-type-maps (lcopy inst
					   :mappings tgt-mappings
					   :target nil)
					 th))
	    (setq nthinst (set-type-maps inst th)))))
    (unless (resolution inst)
      (setf (resolutions inst) (list (make-resolution th inst))))
    (unless (resolution nthinst)
      (setf (resolutions nthinst) (list (make-resolution th nthinst))))
    (unless (eq nthinst inst)
      (let ((theory (get-theory inst)))
	(assert (or (not (lib-datatype-or-theory? theory))
		    (library inst)))
	(put-importing inst theory)
	(setf (resolutions inst) (list (make-resolution th nthinst)))))
    (assert (fully-typed? nthinst))
    (add-to-using nthinst th)
    ;;     (when (some #'(lambda (m) (mod-decl? (declaration (lhs m))))
    ;; 		(mappings nthinst))
    ;;       (add-theory-mappings-importings th nthinst))
    ;;     (when (some #'formal-theory-decl? (formals th))
    ;;       (add-theory-parameters-importings th nthinst))
    (when (mappings nthinst)
      (let* ((*subst-mod-params-theory* th)
	     (*subst-mod-params-map-bindings* (mappings nthinst))
	     (map-alist (make-subst-mod-params-map-bindings
			 inst (mappings inst) nil)))
	;; Note that we're simply appending here - maybe should deal with
	;; duplicates somehow...
	(setf (theory-mappings (current-theory))
	      (append map-alist (theory-mappings (current-theory)))))
      (generate-mapped-axiom-tccs nthinst))
    (unless *ignore-exportings*
      (add-exporting-with-theories th nthinst t))
    (assert (resolution inst))))

(defun add-theory-parameters-importings (theory inst)
  (when (and (formals-sans-usings theory)
	     (actuals inst))
    (mapc #'(lambda (fm act)
	      (when (and (formal-theory-decl? fm)
			 (not (typep (declaration (expr act))
				     '(or theory-abbreviation-decl mod-decl))))
		(let* ((th (get-theory (expr act)))
		       (ninst (make-theoryname th
				(actuals (expr act))
				(library (expr act))
				(mappings (expr act)))))
		  (add-to-using ninst th))))
	  (formals-sans-usings theory)
	  (actuals inst))))

(defun add-theory-mappings-importings (theory inst)
  "Looks for mappings in inst with lhs a mod-decl and adds the rhs theory as
if imported."
  (declare (ignore theory))
  (dolist (map (mappings inst))
    (when (and (not (mapping-rename? map))
	       (mod-decl? (declaration (lhs map))))
      (let* ((thname (theory-ref (expr (rhs map))))
	     (rth (target-mapped-theory (declaration (expr (rhs map)))))
	     (mtheory rth)
	     (ninst (make-theoryname
		     mtheory
		     (or (actuals (expr (rhs map)))
			 (actuals thname))
		     (or (library (expr (rhs map)))
			 (library thname))
		     (or (mappings (expr (rhs map)))
			 (mappings thname)))))
	#+pvsdebug (assert (same-id (get-theory thname) mtheory))
	(add-to-using ninst mtheory)))))

(defmethod typecheck-using* ((adt recursive-type) inst)
  (let* ((th1 (adt-theory adt))
	 (th2 (adt-map-theory adt))
	 (th3 (adt-reduce-theory adt))
	 (use1 (copy inst :id (id th1) :resolutions nil))
	 (use2 (when th2 (copy inst :id (id th2) :actuals nil :mappings nil
			       :resolutions nil)))
	 (use3 (copy inst :id (id th3) :actuals nil :mappings nil
		     :resolutions nil))
	 (*typecheck-using* inst)
	 (*tc-theories* (remove-if #'(lambda (x)
				       (and (eq (car x) (current-theory))
					    (importing? (cdr x))
					    (eq (theory-name (cdr x)) inst)))
			  *tc-theories*)))
    (typecheck-using use1)
    (assert (resolution use1))
    (setf (resolutions inst) (resolutions use1))
    (let ((*ignore-exportings* t)
	  (supinst (if (mappings inst) use1 (adt-modinst use1))))
      (mapc #'typecheck-using
	    `(,@(unless (eq supinst inst) (list supinst))
		,@(when use2 (list use2))
		,use3)))))

(defmethod typecheck-using* ((decl theory-reference) inst)
  (declare (ignore inst))
  (let ((thname (theory-name decl)))
    (typecheck-using* (declaration thname) thname)))

;;; Handles EXPORTING WITH clauses.  For example,
;;;
;;; m: THEORY [t:TYPE, c:t]		m1: THEORY [s:TYPE, a:s]
;;;   USING m1[t,c]			   USING m2[s]
;;;					   EXPORTING ALL WITH m2[s]
;;;
;;; The using list generated for m should include (#m2 m2[t])
;;; The function will be called with (#m1 m1[t]), (#m2 m2[t])
;;; The theory is associated with the inst (i.e., they have the same id)

(defun add-exporting-with-theories (theory inst &optional skip-add-to-using?)
  (when (exporting theory)
    (dolist (entry (the list (closure (exporting theory))))
      (let* ((thname (car entry))
	     (itheory (cdr entry))
	     (ename (if (or (and (actuals inst)
				 (some #'(lambda (fp)
					   (memq (declaration fp) (formals-sans-usings theory)))
				       (free-params thname)))
			    (some #'(lambda (map)
				      (module? (declaration (lhs map))))
				  (mappings inst)))
			(subst-mod-params thname inst theory)
			(remove-indirect-formals-of-name thname)))
	     (iname (if (and (lib-datatype-or-theory? itheory)
			     (null (library ename)))
			(copy ename
			  :library (get-library-id (context-path itheory)))
			ename)))
	(assert itheory)
	(assert (or (not (lib-datatype-or-theory? itheory))
		    (library iname)
		    (from-prelude-library? itheory)
		    (file-equal (context-path itheory) *default-pathname-defaults*))
		()
		"Error in add-exporting-with-theory: itheory = ~a, iname = ~a, ~
                 itheory-path = ~a, default path = ~a"
		itheory iname (context-path itheory) *default-pathname-defaults*)
	#+pvsdebug (assert (or (null (actuals iname))
			       (fully-instantiated? iname)))
	(unless (and (formals-sans-usings itheory) (null (actuals iname)))
	  ;; Add this to the assuming-instances list if fully instantiated
	  (assert (theory-element? (current-declaration)))
	  (pushnew (list iname inst (current-declaration))
		   (assuming-instances (current-theory))
		   :test #'(lambda (x y)
				   (not (eq (simple-match (car y) x) 'fail)))))
	(unless skip-add-to-using?
	  (add-to-using iname itheory))))))

;; (defmethod expand-theory-name ((tn modname))
;;   (unless (resolutions tn)
;;     (typecheck* tn 'module nil nil))
;;   (assert (fully-instantiated? tn))
;;   (let ((decl (declaration tn)))
;;     (typecase decl
;;       (theory-abbreviation-decl
;;        (let ((stn (subst-mod-params (theory-name decl) tn (module tn) decl)))
	 
							  

;;; Returns all of the theorynames directly used by the specified
;;; theory, either through USINGs or MOD-DECLs.  Note that when a datatype
;;; is referenced, it is replaced by (instances of) its generated
;;; theories.

(defmethod get-immediate-usings ((theory module))
  (with-slots (immediate-usings formals assuming (theory-part theory)) theory
    (if (eq immediate-usings 'unbound)
	(get-immediate-usings* theory)
	immediate-usings)))

(defun get-immediate-usings* (theory)
  (let* ((alldecls (all-decls theory))
	 (usings (mapcar #'theory-name
		   (remove-if-not #'importing-entity? alldecls)))
	 (all-there? t)
	 (imm-usings
	  (mapcan #'(lambda (thname)
		      (let ((th (or (and (resolutions thname)
					 (declaration thname))
				    (get-theory thname))))
			(unless th (setq all-there? nil))
			(append
			 (when (target thname)
			   (list (target thname)))
			 (or (and (typep th 'datatype)
				  (datatype-instances thname))
			     (list thname)))))
	    usings)))
    ;; Would like to have the following invariant
    ;; (assert (or (not all-there?)
    ;; 		(every #'resolution imm-usings)))
    ;; But if the theory is currently being typechecked, this is only true
    ;; for importings preceding the current-declaration
    ;; Keep unbound if get-theory returns nil for some thname
    (if all-there?
	(setf (immediate-usings theory) imm-usings)
	imm-usings)))

(defmethod get-immediate-using-names ((theory module))
  (with-slots (immediate-usings formals assuming (theory-part theory)) theory
    (let* ((usings (mapcar #'theory-name
		     (remove-if-not #'mod-or-using?
		       (all-decls theory)))))
      usings)))

(defmethod get-immediate-context-usings ((theory module))
  (mapcan #'(lambda (thname)
	      (unless (library thname)
		(let ((th (get-theory thname)))
		  (or (and (typep th 'recursive-type)
			   (datatype-instances thname))
		      (list thname)))))
    (mapcar #'theory-name
      (remove-if-not #'mod-or-using?
	(all-decls theory)))))

(defmethod modname ((d formal-theory-decl))
  (theory-name d))

(defmethod theory-name ((mdecl mod-decl))
  (modname mdecl))

(defun datatype-instances (imported-adt)
  (let* ((adt (get-theory imported-adt))
	 (th1 (adt-theory adt))
	 (th2 (adt-map-theory adt))
	 (th3 (adt-reduce-theory adt)))
    (when th1
      (nconc (list (make-theoryname th1
		     (when (actuals imported-adt)
		       (ldiff (actuals imported-adt)
			      (nthcdr (length (formals-sans-usings adt))
				      (actuals imported-adt))))
		     (library imported-adt)))
	     (when th2
	       (list (make-theoryname th2 nil (library imported-adt))))
	     (when th3
	       (list (make-theoryname th3 nil (library imported-adt))))))))

(defmethod get-immediate-usings ((adt recursive-type))
  (append (mapcar #'theory-name
	    (remove-if-not #'mod-or-using?
	      (append (formals adt)
		      (assuming adt))))
	  (when (importings adt)
	    (mapcar #'theory-name (importings adt)))))

(defmethod get-immediate-context-usings ((adt recursive-type))
  (append (mapcar #'theory-name
	    (remove-if-not #'mod-or-using?
	      (append (formals adt)
		      (assuming adt))))
	  (when (importings adt)
	    (mapcan #'(lambda (imp)
			(unless (library (theory-name imp))
			  (list (theory-name imp))))
	      (importings adt)))))

(defun mod-or-using? (obj)
  (typep obj
	 '(or mod-decl theory-abbreviation-decl formal-theory-decl importing)))

(defmethod modules ((decl mod-decl))
  (list (modname decl)))

(defmethod modules ((decl theory-abbreviation-decl))
  (list (theory-name decl)))


;;; Perform the substitution.  In the above, would be called with
;;; (m1[t,c] #m1 m2[s]) and return m2[t].

(defun subst-actuals (inst theory target-inst)
  (assert (resolution inst))
  (assert (resolution target-inst))
  (let* ((etheory (subst-mod-params target-inst inst theory))
	 (actuals (subst-actuals* inst
				  (formals-sans-usings theory)
				  (actuals etheory)
				  nil)))
    (assert (resolution etheory))
    (if (equal actuals (actuals etheory))
	etheory
	(make-theoryname etheory actuals))))

(defun subst-actuals* (inst formals actuals result)
  (if (null actuals)
      (nreverse result)
      (let* ((pos (if (name-expr? (expr (car actuals)))
		      (position (expr (car actuals)) formals :test #'same-id)))
	     (nactual (or (and pos
			       (nth pos (actuals inst)))
			  (car actuals))))
	(subst-actuals* inst formals (cdr actuals) (cons nactual result)))))


;;; The using list of a context has the form
;;;   ((theory theoryname_1 ... theoryname_n) ... )
;;; where theory is a theory and the theoryname_i's are the theory instances
;;; This is the form that is most convenient in resolving names.

(defun add-to-using (theoryname &optional itheory)
  (assert *current-context*)
  (assert (resolution theoryname))
  (assert (fully-typed? theoryname))
  #+pvsdebug (assert (valid-importing-entry? theoryname))
  (let ((theory (or itheory (get-typechecked-theory theoryname))))
    (unless theory
      (type-error theoryname "Theory ~a not found" (id theoryname)))
    ;; Need to update using-hash, declarations-hash, 
    ;; known-subtypes, judgements, conversions, auto-rewrites, and
    ;; named-theories of current context from saved-context of theory.
    ;; Also need to update assuming-instances of current theory.
    (let ((entry (get-importings theory)))
      #+pvsdebug (assert (valid-importing-entries? entry))
      (unless (and entry (member theoryname entry :test #'tc-eq))
	(update-current-context theory theoryname)))))

(defun update-current-context (theory theoryname)
  (assert (saved-context theory))
  (assert (fully-typed? theoryname))
  (assert (resolution theoryname))
  (update-usings-hash theory theoryname)
  (update-declarations-hash theory theoryname)
  (update-known-subtypes theory theoryname)
  (update-judgements-of-current-context theory theoryname)
  (update-conversions-of-current-context theory theoryname)
  (update-auto-rewrites-of-current-context theory theoryname))

(defun update-usings-hash (theory theoryname)
  (let ((thimps (get-importings theory)))
    (setf (get-importings theory)
	  (append thimps (list theoryname))))
  (assert (saved-context theory))
  (maphash #'(lambda (th ithinsts)
	       (dolist (ith ithinsts)
		 (unless (resolution ith)
		   (setf (resolutions ith) (list (mk-resolution th ith nil)))))
	       (unless (or (eq th theory)
			   (unimported-mapped-theory? th theory theoryname))
		 (let* ((thinsts (exportable-theory-instances ithinsts theory))
			(curimps (get-importings th))
			(expinsts (get-exported-theory-instances
				   thinsts (closure (exporting theory))))
			(sthinsts (subst-theory-importings
				   th expinsts theoryname theory))
			(newinsts (remove-if #'(lambda (sth)
						 (member sth curimps
							 :test #'tc-eq))
				    sthinsts)))
		   #+pvsdebug
		   (assert (or (not (fully-instantiated? theoryname))
			       (valid-importing-entries? newinsts)))
		   (assert (every #'resolution curimps))
		   (when newinsts
		     (dolist (ith newinsts)
		       (unless (resolution ith)
			 (setf (resolutions ith) (list (mk-resolution th ith nil)))))
		     (setf (get-importings th) (append curimps newinsts))))))
	   (lhash-table (using-hash (saved-context theory)))))

(defun exportable-theory-instances (thinsts theory &optional expinsts)
  (if (null thinsts)
      (nreverse expinsts)
      (exportable-theory-instances
       (cdr thinsts)
       theory
       (if (every #'(lambda (x) (or (formal-decl? x) (exportable? x theory)))
		  (collect-references (actuals (car thinsts))))
	   (cons (car thinsts) expinsts)
	   expinsts))))

(defun get-exported-theory-instances (thinsts closure &optional insts)
  (if (null thinsts)
      (nreverse insts)
      (let ((inst (or (car (assoc (car thinsts) closure :test #'tc-eq))
		      (car (assoc (car thinsts) closure :test #'expinst-eq)))))
	(get-exported-theory-instances
	 (cdr thinsts) closure
	 (if inst
	     (cons (if (actuals inst)
		       inst
		       (car thinsts))
		   insts)
	     insts)))))

(defun expinst-eq (inst1 inst2)
  (and (eq (id inst1) (id inst2))
       (eq (library inst1) (library inst2))
       (or (null (actuals inst1))
	   (null (actuals inst2))
	   (tc-eq (actuals inst1) (actuals inst2)))
       (tc-eq (mappings inst1) (mappings inst2))))
	   

(defun subst-theory-importings (th thinsts theoryname theory)
  (let* ((mthinsts (if (mappings theoryname)
		       (mapcan #'(lambda (thinst)
				   (assert (resolution thinst))
				   (if (and (eq (id theoryname) (id thinst))
					    (eq (library theoryname)
						(library thinst))
					    (fully-instantiated?
					     (lcopy thinst :mappings nil)))
				       (list (copy thinst
					       :mappings (mappings theoryname)))
				       (unless (get-importings th)
					 (list (lcopy thinst
						 :library (get-library-id thinst))))))
			 thinsts)
		       thinsts)))
    (if (fully-instantiated? (lcopy theoryname :mappings nil))
	(mapcar #'(lambda (thinst)
		    (subst-theory-importing thinst theoryname theory))
	  mthinsts)
	(mapcar #'remove-indirect-formals-of-name mthinsts))))


(defun subst-theory-importing (thinst theoryname theory)
  (if (or (actuals thinst)
	  (mappings thinst))
      (subst-mod-params thinst theoryname theory)
      thinst))


(defun valid-importing-entries? (theory-names)
  (every #'valid-importing-entry? theory-names))

(defun valid-importing-entry? (theory-name)
  (or (and (null (actuals theory-name))
	   (null (mappings theory-name)))
      (fully-instantiated? theory-name)))


(defun update-declarations-hash (theory theoryname)
  (let ((dhash (current-declarations-hash)))
    (dolist (decl (theory-formal-decls theory))
      (when (and (declaration? decl)
		 (visible? decl)
		 (exportable? decl theory)
		 (not (unimported-mapped-theory?
		       (module decl) theory theoryname)))
	(let ((map (find decl (mappings theoryname)
			 :key #'(lambda (m)
				  (declaration (lhs m))))))
	  (unless (mapping-subst? map)
	    (check-for-importing-conflicts decl)
	    (pushnew decl (get-lhash (id decl) dhash) :test #'eq)))))
    (let ((mapped-decls nil))
      (maphash #'(lambda (id decls)
		   (dolist (decl decls)
		     (when (and (declaration? decl)
				(visible? decl)
				(exportable? decl theory)
				(not (unimported-mapped-theory?
				      (module decl) theory theoryname)))
		       (let ((map (unless (mod-decl? (current-declaration))
				    (find decl (mappings theoryname)
					  :key #'(lambda (m)
						   (declaration (lhs m)))))))
			 ;; IMPORTINGs with mapping-substs generate mapped decls
			 (cond ((mapping-subst? map)
				(unless (or (null map) (mapped-decl map))
				  (let ((mdecl (make-mapped-decl decl map theoryname theory)))
				    ;;(pushnew mdecl (get-lhash id dhash) :test #'eq)
				    ;; (add-decl mdecl)
				    (push mdecl mapped-decls)
				    #+badassert
				    (assert (or *in-checker* *in-evaluator*
						(memq mdecl (all-decls (current-theory))))))))
			       (t (check-for-importing-conflicts decl)
				  (pushnew decl (get-lhash id dhash) :test #'eq)))))))
	       (lhash-table (declarations-hash (saved-context theory))))
      (when mapped-decls
	(let ((mth (module (generated-by (car mapped-decls)))))
	  (assert (every #'(lambda (md) (eq (module (generated-by md)) mth))
			 (cdr mapped-decls)))
	  (let ((sorted (sort mapped-decls
			      #'(lambda (x y)
				  (memq (generated-by y)
					(memq (generated-by x) (all-decls mth)))))))
	    (dolist (mdecl sorted)
	      (add-decl mdecl))))))))

(defmethod make-mapped-decl ((decl mod-decl) map theory theoryname)
  (let ((mdecl (change-class
		   (copy decl
		     :place nil
		     :decl-formals (mapcar #'(lambda (df)
						(make-mapped-decl
						 df map theory theoryname))
				      (decl-formals decl))
		     :formals nil
		     :module (current-theory)
		     :refers-to nil
		     :generated nil
		     :generated-by decl
		     ;;:modname
		     :theory-mappings nil
		     :other-mappings nil)
		   'mapped-mod-decl)))
    ;; (generate-xref mdecl)
    (dolist (df (decl-formals mdecl))
      (setf (associated-decl df) mdecl))
    (setf (mapped-decl map) mdecl)
    mdecl))

(defmethod make-mapped-decl ((decl theory-abbreviation-decl) map theory theoryname)
  (let ((mdecl (change-class
		   (copy decl
		     :place nil
		     :decl-formals (mapcar #'(lambda (df)
						(make-mapped-decl
						 df map theory theoryname))
				      (decl-formals decl))
		     :module (current-theory)
		     :refers-to nil
		     :generated nil
		     :generated-by decl
		     ;;:theory-name
		     :saved-context nil
		     :theory-mappings nil
		     :other-mappings nil)
		   'mapped-theory-abbreviation-decl)))
    ;; (generate-xref mdecl)
    (dolist (df (decl-formals mdecl))
      (setf (associated-decl df) mdecl))
    (setf (mapped-decl map) mdecl)
    mdecl))

(defmethod make-mapped-decl ((decl type-decl) map theory theoryname)
  (let* ((typeval (subst-mod-params (type-value decl) theory theoryname))
	 (typex (type-value (rhs map)))
	 (mdecl (change-class (copy decl) 'mapped-type-decl
		  :place nil
		  :decl-formals (mapcar #'(lambda (df)
					    (make-mapped-decl
					     df map theory theoryname))
				  (decl-formals decl))
		  :formals nil
		  :module (current-theory)
		  :generated nil
		  :generated-by decl
		  :type-value typeval
		  :type-expr typex)))
    (let ((*generate-xref-declaration* mdecl))
      (generate-xref mdecl))
    (dolist (df (decl-formals mdecl))
      (setf (associated-decl df) mdecl))
    (setf (mapped-decl map) mdecl)
    mdecl))

(defmethod make-mapped-decl ((decl decl-formal-type) map theory theoryname)
  (declare (ignore map theory theoryname))
  (let ((fdecl (new-decl-formal decl)))
    (setf (place fdecl) nil)
    fdecl))

(defmethod make-mapped-decl ((decl const-decl) map theoryname theory)
  (declare (ignore theoryname theory))
  (let* ((rhs (expr (rhs map)))
	 (type (type rhs))
	 (dtype (or (print-type type) type))
	 ;;(dtype (subst-mod-params (declared-type decl) theoryname theory))
	 ;;(type (subst-mod-params (type decl) theoryname theory))
	 (dformals (decl-formals (lhs map)))
	 (mdecl (change-class
		    (copy decl
		      :place nil
		      :decl-formals dformals
		      :formals nil
		      :module (current-theory)
		      :generated nil
		      :generated-by decl
		      :declared-type dtype
		      :type type
		      :definition (expr (rhs map)))
		    'mapped-const-decl)))
    (let ((*generate-xref-declaration* mdecl))
      (generate-xref mdecl))
    (dolist (df (decl-formals mdecl))
      (setf (associated-decl df) mdecl))
    (assert (with-current-decl mdecl (fully-instantiated? type)))
    (make-def-axiom mdecl)
    (setf (mapped-decl map) mdecl)
    mdecl))
				

  
;;; Checks whether the given declaration is exportable from the given theory.
;;; to the current theory.
;;; Used mostly in merging judgements from one context to another.

;;; There are three theories involved here: the (module decl), the given
;;; theory, and the (current-theory).  The tricky bit is judgements, which
;;; when instantiated end up belonging to a different theory (see
;;; subst-params-decl).  
(defun exportable? (decl theory)
  (assert *current-context*)
  (let ((th (module decl)))
    (or (eq th (current-theory))
	(from-prelude? decl)
	(from-prelude-library? decl)
	(unless (or (from-prelude? theory)
		    (from-prelude-library? theory))
	  (let ((exp (exporting theory)))
	    (if (eq (module decl) theory)
		(or (eq (kind exp) 'default)
		    (if (eq (names exp) 'all)
			(not (member decl (but-names exp)
				     :test #'expname-test))
			(member decl (names exp) :test #'expname-test)))
		(or ;;(eq (kind exp) 'default)
		 (and (rassoc (module decl) (closure exp) :test #'eq)
		      (exportable? decl (module decl))))))))))

(defun remove-uninstantiated-repeated-importings (entry imps)
  (remove-if
      #'(lambda (imp)
	  (uninstantiated-repeated-importing imp imps))
    entry))

(defun uninstantiated-repeated-importing (imp imps)
  (and (actuals imp)
       (not (fully-instantiated? imp))
       (member (id imp) imps :key #'id)
       (not (member imp imps :test #'tc-eq))))

(defmethod check-for-importing-conflicts ((decl lib-decl))
  (let ((lib-ref (lib-ref decl)))
    (dolist (d (get-declarations (id decl)))
      (when (and (lib-decl? d)
		 (not (equal lib-ref (lib-ref d))))
	(if (= (locality d) (locality decl))
	    (pvs-warning
		"Library id ~a declared in imported theory ~a and ~a ~
               with a different path.~%References to this library id will ~
               lead to ambiguity errors."
	      (id decl) (id (module d)) (id (module decl)))
	    (pvs-warning 
		"Library id ~a declared in imported theories ~a and ~a ~
               with a different path.~%References to this library id will ~
               use the path~%  ~a"
	      (id decl) (id (module d)) (id (module decl))
	      (if (< (locality d) (locality decl))
		  (library d)
		  (library decl))))))))

(defmethod check-for-importing-conflicts (decl)
  (declare (ignore decl))
  nil)

(defun update-conversions-of-current-context (theory theoryname)
  (unless (and (not *loading-prelude*)
	       (from-prelude? theory))
    (let ((new-convs (get-new-imported-conversions
		      (mapcar #'(lambda (conv)
				  (if (eq (module conv) theory)
				      (subst-params-decl conv
							 theoryname theory)
				      conv))
			(conversions (saved-context theory)))
		      (current-conversions))))
      (dolist (conversion new-convs)
	(push conversion (conversions *current-context*))))
    (dolist (conversion (disabled-conversions (saved-context theory)))
      (when (memq conversion (conversions *current-context*))
	(setf (conversions *current-context*)
	      (remove conversion (conversions *current-context*))))
      (if (eq (module conversion) theory)
	  (pushnew (subst-params-decl conversion theoryname theory)
		   (disabled-conversions *current-context*)
		   :test #'eq)
	  (pushnew conversion (disabled-conversions *current-context*)
		   :test #'eq)))))

;; Note that this returns the new conversions in reverse order
(defun get-new-imported-conversions (imported-convs current-convs
						    &optional new-convs)
  (if (null imported-convs)
      new-convs
      (get-new-imported-conversions
       (cdr imported-convs)
       current-convs
       (if (member (car imported-convs) current-convs
		   :test #'subsumed-conversion)
	   new-convs
	   (cons (car imported-convs) new-convs)))))

(defun subsumed-conversion (imp-conv cur-conv)
  (and (eq (id imp-conv) (id cur-conv))
       (or (tc-eq (expr imp-conv) (expr cur-conv))
	   (let ((bindings (tc-match (expr cur-conv) (expr imp-conv)
				     (mapcar #'list (free-params imp-conv)))))
	     (and bindings (every #'cdr bindings))))))
		     

(defun update-auto-rewrites-of-current-context (theory theoryname)
  (declare (ignore theoryname))
  (dolist (r (auto-rewrites (saved-context theory)))
    (assert (auto-rewrite-decl? r))
    (pushnew r (auto-rewrites *current-context*)))
  (dolist (r (disabled-auto-rewrites (saved-context theory)))
    (assert (auto-rewrite-decl? r))
    (pushnew r (disabled-auto-rewrites *current-context*))))

(defmethod unimported-mapped-theory? (th theory theoryname)
  (declare (ignore th theory theoryname))
  nil)

(defun list-diff (l1 l2 &optional elts)
  (if (or (null l1) (equal l1 l2))
      (nreverse elts)
      (list-diff (cdr l1) l2 (cons (car l1) elts))))

(defmethod subst-params-decl ((c conversion-decl) modinst theory)
  (if (or (mappings modinst)
	  (and (memq theory (free-params-theories c))
	       (actuals modinst)
;; 	       (not (every #'(lambda (a)
;; 			       (if (type-value a)
;; 				   (and (type-name? (type-value a))
;; 					(memq (declaration (type-value a))
;; 					      (free-params (current-theory))))
;; 				   (and (name-expr? (expr a))
;; 					(memq (declaration (expr a))
;; 					      (free-params (current-theory))))))
;; 			   (actuals modinst)))
	       ))
      (let* ((nexpr (subst-mod-params (expr c) modinst theory))
	     (nc (lcopy c :expr nexpr)))
	(unless (eq c nc)
	  (setf (module nc)
		(if (fully-instantiated? modinst)
		    (current-theory)
		    (module c)))
	  (unless (freevars (expr nc))
	    (add-decl nc)))
	nc)
      c))
		    
;;; Remove formals that are not a part of the current module.  This
;;; handles the following circumstance:
;;;
;;; t1[t:TYPE]:THEORY     t2[tt:TYPE]:THEORY         t3: THEORY
;;;   ...                   EXPORTING ALL WITH t1      USING t2
;;;   ...                   USING t1[tt]
;;;
;;; So in typechecking t3, add-to-using is called with t1[tt] and
;;; returns t1.

(defun remove-indirect-formals-of-name (theoryname)
  (if (or (null (actuals theoryname))
	  (fully-instantiated? (actuals theoryname)))
      theoryname
      (copy theoryname :actuals nil :mappings nil)))

(defun check-compatible-params (formals actuals assoc)
  (or (null formals)
      (and (check-compatible-param (car formals) (car actuals) assoc)
	   (check-compatible-params
	    (cdr formals) (cdr actuals)
	    (if (formal-theory-decl? (car formals))
		(let ((formal (car formals)))
		  (append (theory-mappings formal) assoc))
		(acons (car formals)
		       (if (formal-type-decl? (car formals))
			   (type-value (car actuals))
			   (expr (car actuals)))
		       (if (formal-subtype-decl? (car formals))
			   (acons (find-if #'(lambda (c) (typep c 'const-decl))
				    (generated (car formals)))
				  (subtype-pred (type-value (car actuals))
						(subst-types
						 (supertype (type-value
							     (car formals)))
						 assoc))
				  assoc)
			   assoc)))))))

(defun check-compatible-param (formal actual assoc)
  (typecase formal
    (formal-type-decl
     (unless (type-value actual)
       (type-error actual "Expression provided where a type is expected"))
     (typecase formal
       (formal-subtype-decl
	(let ((type (subst-types (supertype (type-value formal)) assoc)))
	  (unless (strict-compatible? (type-value actual) type)
	    (type-error actual "~a Should be a subtype of ~a"
			(type-value actual) type))))
       (formal-struct-subtype-decl
	(let ((struct-type (type (resolution (type-expr formal))))
	      (act-type (type-value actual)))
	  (unless (sub-struct-type? act-type struct-type)
	    (type-error actual
	      "Not a structural subtype"))))))
    (formal-theory-decl
     (check-compatible-theory-param formal (expr actual) assoc))
    (t (let ((type (subst-types (type formal) assoc)))
	 (typecheck* (expr actual) type nil nil))))
  t)

(defun check-compatible-theory-param (formal act assoc)
  (unless (typep (declaration act)
		 '(or module mod-decl theory-abbreviation-decl
		      formal-theory-decl))
    (type-error act "Theory name expected here"))
  (check-compatible-theory-param*
   (get-theory (theory-name formal)) (declaration act) act assoc))

(defmethod check-compatible-theory-param* (ftheory (adecl module) act assoc)
  (declare (ignore assoc))
  (unless (eq ftheory adecl)
    (type-error act "Theory name should be (an alias of) ~a" (id ftheory))))

(defmethod check-compatible-theory-param* (ftheory
					   (adecl theory-abbreviation-decl)
					   act assoc)
  (check-compatible-theory-param* ftheory
				  (get-theory (theory-name adecl))
				  act assoc))

(defmethod check-compatible-theory-param* (ftheory (adecl formal-theory-decl)
						   act assoc)
  (check-compatible-theory-param* ftheory
				  (get-theory (modname adecl))
				  act assoc))

(defmethod check-compatible-theory-param* (ftheory (adecl mod-decl) act assoc)
  (check-compatible-theory-param* ftheory
				  (get-theory (modname adecl))
				  act assoc))

(defmethod sub-struct-type? ((t1 recordtype) (t2 recordtype))
  (let ((subfields (remove-if (complement
			       #'(lambda (fld)
				   (member (id fld) (fields t2) :key #'id)))
		     (fields t1))))
    (tc-eq subfields (fields t2))))

(defmethod sub-struct-type? ((t1 tupletype) (t2 tupletype))
  (every #'tc-eq (types t1) (types t2)))

(defmethod sub-struct-type? ((t1 type-expr) (t2 type-expr))
  nil)

(defun subst-types (type assoc)
  (if assoc
      (gensubst type
	#'(lambda (te) (actual-value (cdr (assoc (declaration te) assoc))))
	#'(lambda (te) (and (name? te)
			    (assoc (declaration te) assoc))))
      type))

(defmethod typecheck-mappings (mappings (thinst modname))
  (unless (or (not mappings)
	      (already-typed? mappings))
    (let ((lhs-context (mapping-lhs-theory-context thinst))
	  (rhs-context (mapping-rhs-theory-context thinst)))
      (unless lhs-context
	(type-error thinst "Theory reference ~a not found" thinst))
      (let* ((lhs-theory (theory lhs-context))
	     (lhs-theory-decls (interpretable-declarations lhs-theory)))
	(dolist (mapping mappings)
	  (unless (already-typed? mapping)
	    (typecheck-decl-formals (decl-formals mapping) (lhs mapping))
	    (typecheck-mapping-lhs mapping lhs-context lhs-theory lhs-theory-decls thinst)
	    ;; (when (let ((prev-mappings (ldiff mappings (memq mapping mappings))))
	    ;; 	    (member mapping prev-mappings :test #'same-mapping-lhs?))
	    ;;   (type-error mapping
	    ;; 	"Mapping has duplicate LHS: ~a" (lhs mapping)))
	    ;; lhs can be a name, or a mapping-lhs, which has decl-formals.
	    (when (mapping-lhs? (lhs mapping))
	      (setf (module (lhs mapping)) (current-theory)))
	    (cond ((declaration? (lhs mapping))
		   (break "How did we get here?")
		   (with-current-decl (declaration (lhs mapping))
		     (typecheck-mapping-rhs mapping)))
		  ((mapping-lhs? (lhs mapping))
		   ;; Using generated-by slot to keep (current-declaration)
		   (setf (generated-by (lhs mapping)) (current-declaration))
		   (with-current-decl (lhs mapping)
		     ;; Notice that this will use the lhs decl-formals,
		     ;; As we don't yet know which lhs resolution we'll use
		     (typecheck-mapping-rhs mapping)))
		  (t (with-context rhs-context
		       (typecheck-mapping-rhs mapping))))
	    (assert (or (type-value (rhs mapping))
			(name-expr? (expr (rhs mapping)))
			(ptypes (expr (rhs mapping)))))
	    ))))))

(defun same-mapping-lhs? (map1 map2)
  (let ((lhs1 (lhs map1))
	(lhs2 (lhs map2)))
    (same-declaration lhs1 lhs2)))

(defun typecheck-mapping-lhs (mapping lhs-context lhs-theory lhs-theory-decls thinst)
  "The lhs is basically a name, we get its possible resolutions as a type,
expr, number, or theory reference.  They are appended together, set-type
sorts it all out.  Note that the mapping may have a declared-type; this will
be typechecked when set-type selects a resolution."
  (let* ((*current-context* lhs-context)
	 ;; (ass (assert (eq (current-theory) lhs-theory)))
	 (*generate-tccs* 'none)
	 (dfmls (decl-formals mapping))
	 (lhs (lhs mapping))
	 (tres (unless (and (kind mapping)
			    (not (eq (kind mapping) 'type)))
		 (let ((tr (delete-if-not
			       #'(lambda (r)
				   (and (interpretable? (declaration r))
					(memq (declaration r) lhs-theory-decls)
					(length= (decl-formals (declaration r)) dfmls)))
			     (with-no-type-errors
				 (resolve* lhs 'type nil)))))
		   (if (cdr tr)
		       (or (remove-if-not
			       #'(lambda (res)
				   (memq (declaration res) lhs-theory-decls))
			     tr)
			   (remove-if-not
			       #'(lambda (r)
				   (id-prefix (id thinst) (id (declaration r))))
			     tr)
			   tr)
		       tr))))
	 (eres (unless (and (kind mapping)
			    (not (eq (kind mapping) 'expr)))
		 (let ((res (with-no-type-errors (resolve* lhs 'expr nil))))
		   (delete-if-not
		       #'(lambda (r)
			   (and (interpretable? (declaration r))
				(memq (declaration r) lhs-theory-decls)
				;; Need to make sure it's not already mapped
				;; This may already be done
				(length= (decl-formals (declaration r)) dfmls)
				(or (null (type mapping))
				    (tc-eq (type mapping) (type r)))))
		     res))))
	 (nres (unless (or ;; eres
			   (and (kind mapping)
				(not (eq (kind mapping) 'expr))))
		 (when (and (or (integerp (id lhs))
				(every #'digit-char-p
				       (string (id lhs))))
			    (or (null (mod-id lhs))
				(eq (mod-id lhs) '|numbers|)))
		   (let* ((num (if (integerp (id lhs))
				   (id lhs)
				   (parse-integer (string (id lhs)))))
			  (ndecl (number-declaration num)))
		     (list (mk-resolution ndecl
			     (make-theoryname (module ndecl))
			     (type ndecl)))))))
	 (thres (unless (and (kind mapping)
			     (not (eq (kind mapping) 'theory)))
		  (delete-if-not
		      #'(lambda (r)
			  (and ;;(memq (declaration r) lhs-theory-decls)
			       (length= (decl-formals (declaration r)) dfmls)
			       (interpretable? (declaration r))))
		    (with-no-type-errors
			(resolve* lhs 'module nil))))))
    (unless (or eres nres tres thres)
      (type-error lhs
	"Map lhs~%  ~a~%does not resolve to an uninterpreted ~
                   type, constant, or theory declaration in theory:~%  ~a~
          ~@[~%Note: ~a is not allowed to be interpreted~]"
	lhs (id lhs-theory)
	(car (memq (id lhs) '(boolean number)))))
    (if (cdr tres)
	(cond ((or eres nres)
	       (setf (resolutions lhs) (nconc eres nres)))
	      (t (setf (resolutions lhs) tres)
		 (type-ambiguity lhs)))
	(let* ((reses (nconc tres eres nres thres))
	       (mreses (remove-if-not #'(lambda (res)
					  (memq (declaration res) lhs-theory-decls))
			 reses)))
	  ;; (unless mreses (break "typecheck-mapping-lhs: why no mreses?"))
	  ;; have theory-abbreviation-decl in nasalib float
	  (setf (resolutions lhs) (or mreses reses))))
    ;; (when (and (name? lhs)
    ;; 	       (not (or (mod-id lhs)
    ;; 			(actuals lhs)
    ;; 			(dactuals lhs)
    ;; 			(mappings lhs)
    ;; 			(target lhs)))
    ;; 	       (valid-number? (string (id lhs)))
    ;; 	       (compatible? (type (resolution lhs)) *number*))
    ;;   (multiple-value-bind (num radix)
    ;; 	  (parse-number (string (id lhs)))
    ;; 	(if radix
    ;; 	    (change-class lhs 'number-expr-with-radix
    ;; 	      :number num
    ;; 	      :radix radix
    ;; 	      :type (get-expr-number-type num))
    ;; 	    (change-class lhs 'number-expr
    ;; 	      :number num
    ;; 	      :type (get-expr-number-type num)))))
    (assert (resolutions lhs))
    (when (mapping-rename? mapping)
      (if (cdr (resolutions lhs))
	  (type-ambiguity lhs)
	  (check-duplication (copy (declaration
				    (car (resolutions lhs)))
			       :id (id (expr (rhs mapping)))
			       :module 'unbound))))))

(defmethod decl-formals ((map mapping))
  (decl-formals (lhs map)))

(defmethod decl-formals ((name name))
  nil)

(defmethod resolve-lhs ((lhs mapping-lhs) kind)
  (assert (every #'decl-formal-type? (decl-formals lhs)))
  (typecheck-decl-formals (decl-formals lhs) lhs)
  (with-added-decls (decl-formals lhs)
    (resolve* lhs kind nil)))
  
(defmethod resolve-lhs ((lhs name) kind)
  (resolve* lhs kind nil))

;;; mapping-lhs-decl finds the declaration or theory in which the lhs is defined
;;; This is used to create a context, which must include all declarations
;;; given by renamings.

(defmethod mapping-lhs-theory-context ((thname modname))
  (unless (every #'typed? (actuals thname))
    (set-type-actuals thname (get-theory thname)))
  (assert (every #'typed? (actuals thname)))
  ;;(assert (null (resolutions thname)))
  (let ((res (resolve* (lcopy thname :mappings nil) 'module nil)))
    (when res
      (assert (null (cdr res)))
      (mapping-lhs-theory-context (declaration (car res))))))

(defmethod mapping-lhs-theory-context ((thdecl theory-abbreviation-decl))
  (assert (resolution (theory-name thdecl)))
  (mapping-lhs-theory-context (theory-name thdecl)))

(defmethod mapping-lhs-theory-context ((thdecl mod-decl))
  (assert (saved-context thdecl))
  (saved-context thdecl))

(defmethod mapping-lhs-theory-context ((thdecl formal-theory-decl))
  (assert (saved-context thdecl))
  (saved-context thdecl))

(defmethod mapping-lhs-theory-context ((th module))
  (context th))

(defmethod mapping-lhs-theory-context ((th datatype))
  (context th))

(defun mapping-rhs-theory-context (thname)
  "Extends the current context to include thname, removing any mapped
declarations"
  (assert (mappings thname))
  (let* ((nomap-thname (copy thname :mappings nil))
	 (res (let ((r (resolve nomap-thname 'module nil)))
		(or r (resolution-error thname 'module nil))))
	 ;; (thry (if (datatype-or-module? (declaration (car res)))
	 ;; 	   (declaration (car res))
	 ;; 	   (declaration (theory-ref (declaration (car res))))))
	 ;; (idecls (interpretable-declarations thry))
	 (ncontext (copy-context *current-context*)))
    (setf (resolutions nomap-thname) res)
    ;; Add the thry, and then remove those idecls that have been
    ;; interpreted, as they are no longer visible.
    (let ((*current-context* ncontext))
      (add-to-using nomap-thname)
      ;; (dolist (idecl idecls)
      ;; 	(let ((gdecls (get-declarations (id idecl))))
      ;; 	  (when (and (memq idecl gdecls)
      ;; 		     (some #'(lambda (mapping)
      ;; 			       (same-id (lhs mapping) idecl))
      ;; 			   (mappings thname)))
      ;; 	    (setf (get-declarations (id idecl))
      ;; 		  (remove idecl gdecls)))))
      )
    ncontext))
  

(defmethod already-typed? ((list list))
  (every #'already-typed? list))

(defmethod already-typed? ((name name))
  (resolution name))

(defmethod already-typed? ((ex number-expr))
  (type ex))

(defmethod already-typed? ((act actual))
  (or (type-value act)
      (already-typed? (expr act))))

(defmethod already-typed? ((map mapping))
  (and (already-typed? (lhs map))
       (already-typed? (rhs map))))

(defmethod already-typed? ((rhs mapping-rhs))
  (or (type-value rhs)
      (type (expr rhs))))

(defmethod typecheck-mappings (mappings (name name))
  ;; Used with a name that is not necessarily a modname
  (when mappings
    (if (mod-id name)
	(typecheck-mappings mappings (name-to-modname name))
	(dolist (mapping mappings)
	  (typecheck-mapping-rhs mapping)))))

(defmethod typecheck-mappings (mappings (n number-expr))
  (declare (ignore mappings))
  nil)

(defmethod typecheck-mapping-rhs ((mapping mapping))
  (when (declared-type mapping)
    (setf (type mapping) (typecheck* (declared-type mapping) nil nil nil)))
  (typecheck-mapping-rhs* (expr (rhs mapping))
			  (kind mapping)
			  (type mapping)
			  (rhs mapping)))

(defmethod typecheck-mapping-rhs ((mapping mapping-rename))
  (with-slots (lhs rhs declared-type type) mapping
    (unless (or (type-value rhs) (resolutions (expr rhs)))
      (when declared-type
	(setf type (typecheck* declared-type nil nil nil)))
      ;; We know that the rhs won't clash with any existing names of the
      ;; source theory, because the (unique) theory-decl id will be prepended
      (let* ((ldecl (declaration lhs))
	     ;; We create a new declaration and resolution for the RHS, that
	     ;; will be used for inlining later.
	     (rdecl (copy ldecl :id (id rhs)
			  :module (current-theory)
			  :generated-by ldecl)))
	(typecase ldecl
	  (type-decl
	   (let ((tn (make-self-resolved-type-name rdecl)))
	     ;;(when (adt-type-name? (type-value ldecl))
	     ;;(change-class tn 'adt-type-name :adt (adt (type-value ldecl))))
	     (setf (type-value rdecl) tn)
	     (setf (type-value rhs) tn)
	     (setf (resolutions (expr rhs)) (resolutions tn))
	     (setf (generated rdecl)
		   (remove-if #'(lambda (gd) (or (tcc? gd) (nonempty-formula-type gd)))
		     (generated ldecl)))))
	  (const-decl
	   (let ((res (make-resolution rdecl (current-theory-name) type)))
	     (setf (resolutions (expr rhs)) (list res))
	     (setf (type (expr rhs)) (type res))))
	  ((or mod-decl formal-theory-decl)
	   (let ((res (make-resolution rdecl (current-theory-name) type)))
	     (setf (resolutions (expr rhs)) (list res))
	     (change-class (expr rhs) 'theory-name-expr))))))))

(defmethod typecheck-mapping-rhs* ((ex name-expr) kind type rhs)
  (assert (or (null (type ex)) (null type) (compatible? (type ex) type)))
  (unless (type ex)
    (let* ((decls (get-declarations (id ex)))
	   (tres (unless (or (null decls)
			     (and kind (not (eq kind 'type))))
		   (with-no-type-errors (resolve* ex 'type nil))))
	   (eres (unless (or (null decls)
			     (and kind (not (eq kind 'expr))))
		   (if (or tres (null (mod-id ex)))
		       (with-no-type-errors (get-resolutions ex 'expr nil))
		       (get-resolutions ex 'expr nil))))
	   (thres (unless (or (mod-id ex)
			      (and kind
				   (not (eq kind 'theory))))
		    (let ((thname (name-to-modname ex)))
		      (if (or tres eres)
			  (with-no-type-errors (typecheck* thname nil 'module nil))
			  (typecheck* thname nil 'module nil))
		      (when (resolutions thname)
			;; Check that imported modname is visible
			(let ((vreses (visible-modname-resolutions thname)))
			  (unless (or tres eres vreses)
			    (type-error ex "~a is not a visible theory"
					(copy ex :mappings nil)))
			  vreses))))))
;;       (when type
;; 	(setf eres (delete-if-not #'(lambda (r) (compatible? (type r) type))
      ;; 		     eres)))
      (if (cdr tres)
	  (cond (eres
		 (setf (resolutions ex) eres))
		(t (setf (resolutions ex) tres)
		   (type-ambiguity ex)))
	  (setf (resolutions ex) (nconc tres eres thres)))
      (unless (resolutions ex)
	(type-error ex "No resolution for ~a as a type, expr, or theory" ex))
      (when eres
	(setf (types ex) (mapcar #'type eres))
	(when (and (plusp (parens ex))
		   (some #'(lambda (ty)
			     (let ((sty (find-supertype ty)))
			       (and (funtype? sty)
				    (tc-eq (range sty) *boolean*))))
			 (ptypes ex)))
	  (setf (type-value rhs)
		(typecheck* (make-instance 'expr-as-type :expr (copy-untyped ex))
			    nil nil nil))))
      (when tres
	(if (type-value rhs)
	    (unless (compatible? (type-value rhs) (type (car tres)))
	      (push (car tres) (resolutions ex))
	      (type-ambiguity ex))
	    (progn
	      (setf (type-value rhs) (type (car tres)))))))))

(defun visible-modname-resolutions (thname)
  ;; Context has to be for the modname being imported
  ;; i.e., in importing foo[a] {{ bar := bar[b] {{ ... }} }}
  ;; foo gives context, bar[b] is the thname
  (let ((th (get-theory-transitive thname)))
    (assert th)
    (if (null (formals-sans-usings th))
	(resolutions thname)
	(let ((imps (nth-value 1 (all-importings th)))
	      (cimps (when (and (name? (current-declaration))
				(resolution (current-declaration)))
		       (nth-value 1
			 (all-importings (get-theory
					  (module-instance (resolution
							    (current-declaration)))))))))
	  (if (or (some #'(lambda (x) (null (actuals x))) imps)
		  (some #'(lambda (x) (null (actuals x))) cimps))
	      ;; all instances are visible in this case
	      (resolutions thname)
	      (or (visible-modname-resolutions* (resolutions thname) imps nil)
		  (visible-modname-resolutions* (resolutions thname) cimps nil)))))))

(defun visible-modname-resolutions* (resolutions importings vis-reses)
  (if (null resolutions)
      (nreverse vis-reses)
      (visible-modname-resolutions*
       (cdr resolutions) importings
       (if (some #'(lambda (imp) (visible-modname-resolution
				  imp (car resolutions)))
		 importings)
	   (cons (car resolutions) vis-reses)
	   vis-reses))))

(defun visible-modname-resolution (imp res)
  (let ((mi (module-instance res)))
    (assert (eq (id imp) (id mi)))
    (assert (= (length (actuals imp)) (length (actuals mi))))
    (every #'(lambda (act mact)
	       (or (null (type-value act))
		   (compatible? (type-value act) (type-value mact))))
	   (actuals imp) (actuals mi))))
  

(defmethod typecheck-mapping-rhs* (ex kind type rhs)
  (declare (ignore kind type))
  (let ((typed-ex (typecheck* ex nil nil nil)))
    (when (type-expr? typed-ex)
      (setf (type-value rhs) typed-ex))
    typed-ex))

(defmethod interpretable-declarations ((theory module))
  (remove-if-not #'interpretable? (all-decls theory)))

(defmethod interpretable-declarations ((thref mod-decl))
  (remove-if-not #'(lambda (d) (eq (generated-by d) thref))
    (interpretable-declarations (module thref))))

(defmethod interpretable-declarations ((thref theory-abbreviation-decl))
  (interpretable-declarations (declaration thref)))

(defmethod interpretable? ((thref module))
  (interpretable-declarations thref))

(defmethod interpretable? ((ty recursive-type))
  t)

(defmethod interpretable? ((d type-decl))
  ;;(not (adt-type-name? (type-value d)))
  ;; (not (memq d (list (declaration *boolean*)
  ;; 		     (declaration *number*))))
  t)

(defmethod interpretable? ((d type-from-decl))
  t)

(defmethod interpretable? ((d type-def-decl))
  nil)

(defmethod interpretable? ((d mod-decl))
  (let* ((mn (modname d))
	 (th (or (declaration mn)
		 (get-theory mn))))
    (if th
	(some #'interpretable? (all-decls th))
	(let ((reses (resolve* (lcopy mn :mappings nil) 'module nil)))
	  (assert reses () "interpretable? (mod-decl) reses failed")
	  (assert (null (cdr reses)) () "interpretable? (mod-decl) ambiguous reses")
	  (interpretable? (declaration (car reses)))))))

(defmethod interpretable? ((d theory-abbreviation-decl))
  t)


(defmethod interpretable? ((d formal-type-decl))
  nil)

(defmethod interpretable? ((d const-decl))
  (null (definition d)))

(defmethod interpretable? ((d adt-constructor-decl))
  ;;nil
  t)

(defmethod interpretable? ((d adt-recognizer-decl))
  ;;nil
  t)

(defmethod interpretable? ((d adt-accessor-decl))
  ;;nil
  t)

(defmethod interpretable? ((imp importing))
  nil)

(defmethod interpretable? ((res resolution))
  (interpretable? (declaration res)))

(defmethod interpretable? ((decl declaration))
  nil)

(defmethod interpretable? ((bd binding))
  nil)

(defmethod interpretable? ((name name))
  (and (resolutions name)
       (interpretable? (car (resolutions name)))))

(defun check-mapping-lhs (lhs)
  (unless (interpretable? (resolution lhs))
    (type-error lhs "Must be uninterpreted to be used in a mapping.")))
	      

;;; check-exporting checks the names and theory instances being exported.

(defun check-exporting (theory)
  (check-exported-theories (modules (exporting theory)))
  (let* ((alldecls (collect-all-exportable-decls theory))
	 (edecls (cond ((eq (kind (exporting theory)) 'default)
			alldecls)
		       ((but-names (exporting theory))
			(set-difference
			 alldecls
			 (check-exported-names (but-names (exporting theory))
					       (all-decls theory)
					       nil)))
		       ((eq (names (exporting theory)) 'all)
			alldecls)
		       ((names (exporting theory))
			(check-exported-names (names (exporting theory))
					      (all-decls theory)
					      nil))
		       (t (error "Something's wrong with EXPORTINGs")))))
    (mapc #'set-visibility (theory-formal-decls theory))
    (mapc #'set-visibility edecls)
    (check-exported-completeness (exporting theory) edecls)))

(defun collect-all-exportable-decls (theory)
  (remove-if #'(lambda (d)
		 (typep d '(or importing var-decl field-decl recursive-type)))
	     (append ;;(theory-formal-decls theory)
		     (assuming theory)
		     (theory theory))))

(defun check-exported-names (expnames decls expdecls)
  (if (null expnames)
      expdecls
      (let ((expname (car expnames)))
	(when (type-expr? (kind expname))
	  (setf (type expname) (typecheck* (kind expname) 'type nil nil))
	  (set-type (kind expname) nil))
	(let* ((edecls (remove-if-not #'(lambda (d)
					  (and (declaration? d)
					       (eq (id d) (id expname))))
			 decls))
	       (kdecls (remove-if-not #'(lambda (d)
					  (correct-expkind d expname))
				      edecls))
	       (vdecls (remove-if #'(lambda (d)
				      (typep d '(or var-decl field-decl)))
				  kdecls)))
	  (unless edecls
	    (if (member expname (formals (current-theory)) :test #'same-id)
		(type-error expname "May not export formal parameters")
		(type-error expname "Name ~a is not declared in this theory"
			    expname)))
	  (unless kdecls
	    (type-error expname "Name ~a is not declared as ~a in this theory"
			expname (kind expname)))
	  (unless vdecls
	    (type-error expname "~a may not be exported" expname))
	  (check-exported-names
	   (cdr expnames) decls (append expdecls vdecls))))))

(defun expname-test (decl expname)
  (and (eq (id decl) (id expname))
       (correct-expkind decl expname)))

(defun correct-expkind (decl expname)
  (case (kind expname)
    ((nil) t)
    (type (type-decl? decl))
    (formula (formula-decl? decl))
    (t (tc-eq (type decl) (type expname)))))

(defun check-exported-completeness (exporting expdecls)
  (check-exported-internal-completeness (names exporting) expdecls)
  (case (kind exporting)
    ;; kind refers to what the modnames means:
    ;;  default == no exporting clause
    ;;  all     == EXPORTING names WITH all
    ((all default)
     (setf (closure exporting)
	   (mapcan #'(lambda (imp)
		       (mapcar #'(lambda (inst)
				   (assert (resolution inst))
				   (cons inst (car imp)))
			 (cdr imp)))
	     (all-usings (current-theory)))))
    ;;  closure == EXPORTING names WITH closure
    (closure
     (setf (closure exporting)
	   (let ((insts nil))
	     (mapobject #'(lambda (ex)
			    (when (external-name ex)
			      (pushnew (cons (module-instance ex)
					     (get-theory (module-instance ex)))
				       insts
				       :test #'tc-eq)))
			expdecls)
	     insts)))
    ;;  nil     == EXPORTING names    (e.g., no WITH)
    ;;  or the names were listed explicitely
    (t (check-exported-external-completeness
	(names exporting)
	(exporting-with-closure (modules exporting))
	expdecls)
       (setf (closure exporting)
	     (collect-all-exporting-with-theories
	      (modules exporting)
	      nil)))))

(defun collect-all-exporting-with-theories (theories sorted)
  (let ((collected (remove-duplicates
		       (mapcan #'(lambda (thinst)
				   (collect-all-exporting-with-theories*
				    thinst
				    (get-theory thinst)))
			 theories)
		     :test #'tc-eq)))
    (assert (or (null sorted)
		(every #'(lambda (c) (memq (cdr c) sorted)) collected)))
    (if sorted
	(mapcan #'(lambda (th)
		    (let ((entries
			   (remove-if #'(lambda (e) (not (eq (cdr e) th)))
			     collected)))
		      ;;(assert (or (recursive-type? th)
			;;	  (generated-by th) entries))
		      entries))
	  sorted)
	collected)))

(defmethod collect-all-exporting-with-theories* (thinst (theory module))
  (assert (or (not (lib-datatype-or-theory? theory))
	      (library thinst)
	      (file-equal (context-path theory) *default-pathname-defaults*)))
  (unless (resolution thinst)
    (setf (resolutions thinst) (list (mk-resolution theory thinst nil))))
  (let ((closure (if (library thinst)
		     (mapcar #'(lambda (entry)
				 (if (and (lib-datatype-or-theory?
					   (cdr entry))
					  (null (library (car entry)))
					  (file-equal (context-path (cdr entry))
						      (context-path theory)))
				     (cons (copy (car entry)
					     :library (library thinst))
					   (cdr entry))
				     entry))
		       (closure (exporting theory)))
		     (closure (exporting theory)))))
    (nconc (if (or (actuals thinst) (mappings thinst))
	       (mapcar #'(lambda (entry)
			   (let* ((thinst2 (car entry))
				  (nthinst2 (subst-mod-params
					     thinst2 thinst theory)))
			     (assert (or (null (actuals nthinst2))
					 (fully-instantiated? nthinst2)))
			     (assert (resolution nthinst2))
			     (if (eq thinst2 nthinst2)
				 entry
				 (cons nthinst2 (cdr entry)))))
		 closure)
	       (mapcar #'(lambda (entry)
			   (let* ((thinst2 (car entry))
				  (nthinst2 (remove-indirect-formals-of-name
					     thinst2)))
			     (assert (or (null (actuals nthinst2))
					 (fully-instantiated? nthinst2)))
			     (assert (resolution nthinst2))
			     (if (eq thinst2 nthinst2 )
				 entry
				 (cons nthinst2 (cdr entry)))))
		 closure))
	   (list (cons thinst theory)))))

(defmethod collect-all-exporting-with-theories* (thinst (adt recursive-type))
  (let ((th1 (adt-theory adt))
	(th2 (adt-map-theory adt))
	(th3 (adt-reduce-theory adt)))
    (nconc (collect-all-exporting-with-theories*
	    (make-theoryname th1
	      (actuals thinst))
	    th1)
	   (when th2
	     (collect-all-exporting-with-theories*
	      (make-theoryname th2) th2))
	   (collect-all-exporting-with-theories*
	    (make-theoryname th3) th3))))

(defmethod exporting ((adt recursive-type))
  (exporting (adt-theory adt)))

(defvar *theory-instances* nil)

(defun exporting-with-closure (instances)
  (let ((*theory-instances* nil))
    (mapc #'collect-exporting-with-theories instances)
    *theory-instances*))

(defun collect-exporting-with-theories (inst)
  (push inst *theory-instances*)
  (let ((theory (get-typechecked-theory inst)))
    (when (exporting theory)
      (dolist (etheory (modules (exporting theory)))
	(let ((itheory (subst-mod-params etheory inst theory)))
	  (collect-exporting-with-theories itheory))))))

(defun check-exported-internal-completeness (expnames expdecls)
  (dolist (edecl expdecls)
    (let ((rdecls (remove-if
		      #'(lambda (d)
			  (or (not (eq (module d) (current-theory)))
			      (memq d (theory-formal-decls (current-theory)))
			      (typep d '(or formal-decl importing type-decl
					    var-decl field-decl const-decl
					    recursive-type module
					    mapping-subst))
			      (and (const-decl? d)
				   (formal-subtype-decl?
				    (generated-by d)))
			      (member d expdecls :test #'eq)))
		    (refers-to edecl))))
      (when rdecls
	(let ((expname (if (consp expnames)
			   (car (member edecl expnames
					:test #'same-id))
			   edecl)))
	  (type-error expname
	    "~a may not be exported unless the following are also:~
                     ~%  ~{~a~^, ~}"
	    (if (consp expnames)
		expname
		(format nil "~a:~a" (id expname) (ptype-of expname)))
	    (mapcar #'(lambda (d) (format nil "~a:~a"
				    (id d) (ptype-of d)))
	      rdecls)))))))

(defun check-exported-external-completeness (expnames exptheories expdecls)
  (unless (null expdecls)
    (let ((decl (car expdecls)))
      (unless (and (judgement? decl)
		   (generated-by decl))
	(mapobject #'(lambda (ex)
		       (when (and (external-name ex)
				  (not (member (module-instance ex) exptheories
					       :test #'tc-eq)))
			 (let ((expname (if (consp expnames)
					    (car (member decl expnames
							 :test #'same-id))
					    decl)))
			   (type-error expname
			     "~a refers to~_ ~w~@[[~{~w~^, ~}]~].~w,~_ which must be exported"
			     (id decl) (id (module-instance ex))
			     (actuals (module-instance ex)) (id ex)))))
		   decl)))
    (check-exported-external-completeness expnames exptheories (cdr expdecls))))

(defun external-name (ex)
  (and (name? ex)
       (not (variable? ex))
       (not (freevars ex))
       (module-instance ex)
       (not (eq (module (declaration ex))
		(current-theory)))
       (not (from-prelude? (declaration ex)))))

(defun set-visibility (decl)
  (unless (or (typep decl '(or importing var-decl field-decl
			       inline-recursive-type))
	      (and (type-def-decl? decl)
		   (enumtype? (type-expr decl))))
    (setf (visible? decl) t)))

(defun check-exported-theories (theories)
  (unless (symbolp theories);; Handles NIL, ALL, and CLOSURE
    (let ((theory (get-theory (car theories))))
      (unless theory
	(type-error (car theories) "Theory ~a not found" (car theories)))
      (when (actuals (car theories))
	(typecheck-actuals (car theories))
	(set-type-actuals-and-maps (car theories) theory))
      (unless (member (car theories)
		      (get-importings theory)
		      :test #'check-exported-theories-test)
	(type-error (car theories)
	  "~a occurs in an EXPORTING WITH but is not in a IMPORTING clause"
	  (car theories))))
    (check-exported-theories (cdr theories))))

(defun check-exported-theories-test (u v)
  (and (same-id u v)
       (or (null (actuals v))
	   (tc-eq (actuals u) (actuals v)))))
