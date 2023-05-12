;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-tccs.lisp -- This checks for TCCs when everything has already
;;                        been typechecked.  Called by set-type.
;; Author          : Sam Owre
;; Created On      : Fri Oct 14 02:00:04 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Jul 20 18:36:08 1998
;; Update Count    : 7
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

(export '(check-for-tccs*))

(defvar *skip-tcc-check-exprs* nil)

(defvar *checking-implicit-conversion* nil)

;;; check-for-tccs is called by set-type when the type is already set, but
;;; TCCs need to be checked for.  Thus it is effectively called from the
;;; prover.  This is similar to what typecheck and set-type do, but
;;; theoretically should be simpler since the types and resolutions are
;;; already known.

(defun check-for-tccs (expr expected &optional skip-exprs)
  (unless (compatible? (type expr) expected)
    (type-incompatible expr (list (type expr)) expected))
  (let ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				*tccs-generated-for*))
	(*no-conversions-allowed* (not (memq expr *conversions-allowed*)))
	(*skip-tcc-check-exprs* skip-exprs))
    (check-for-tccs* expr expected)))

(defvar *inner-check-for-tccs* nil)

(defmethod check-for-tccs* :around (ex expected)
  (declare (ignore ex expected))
  (if (eq *generate-tccs* 'top)
      (unless *inner-check-for-tccs*
	(let ((*inner-check-for-tccs* t))
	  (call-next-method)))
      (call-next-method)))

(defmethod check-for-tccs* :around ((ex expr) expected)
  (unless (compatible? (type ex) expected)
    (if *checking-implicit-conversion*
	(type-error *checking-implicit-conversion*
	  "Incompatible types for ~a~%     Found: ~a~%  Expected: ~a"
	  ex (type ex) expected *checking-implicit-conversion*)
	(type-incompatible ex (list (type ex)) expected)))
  (let ((*set-type-expr* (if (place ex) ex *set-type-expr*)))
    (call-next-method)
    (unless (or (typep ex '(or branch lambda-expr update-expr
			    cases-expr let-expr where-expr))
		(memq ex *skip-tcc-check-exprs*))
      (if *added-recursive-def-conversion*
	  (let ((*added-recursive-def-conversion* nil))
	    (check-for-tccs ex (if (application? ex) (type ex) expected)))
	  (check-for-subtype-tcc ex expected)))
    (set-ghost-type ex expected)))

(defmethod check-for-tccs* ((ex modname) expected)
  (declare (ignore expected))
  (check-type-actuals-and-maps ex))

;; Only for theory instances - see check-type-actuals for expr and type names
(defun check-type-actuals-and-maps (thinst)
  (assert (resolution thinst))
  (let* ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				 *tccs-generated-for*))
	 (decl (declaration thinst))
	 (theory (if (module? decl) decl (module decl)))
	 (fmls (formals-sans-usings theory))
	 (dfmls (decl-formals decl))
	 (acts (actuals thinst))
	 (dacts (dactuals thinst)))
    (assert (or (null acts) (length= fmls acts)))
    (assert (or (null dacts) (length= dfmls dacts)))
    (when (or acts dacts)
      (unless (memq thinst *exprs-generating-actual-tccs*)
	(unless (or *in-checker* *in-evaluator*)
	  (push thinst *exprs-generating-actual-tccs*))
	(check-type-actuals* (if dacts (append acts dacts) acts)
			     (if dfmls (append fmls dfmls) fmls)
			     theory)
	(when acts
	  (generate-assuming-tccs thinst thinst theory)))))
  (check-type-maps thinst))

(defmethod check-for-tccs* ((ex name-expr) expected)
  (declare (ignore expected))
  (check-set-type-recursive-name ex)
  ;; ex may have been changed because of above
  (if (name-expr? ex)
      (check-type-actuals ex)
      (check-for-tccs* ex (type ex))))

;;; Similar to check-type-actuals-and-maps, but for arbitrary type and expr
;;; names, which can have actuals/dactuals both in the modinst and the expr
;;; itself, and they are not necessarily the same because of importings, etc.
(defun check-type-actuals (expr)
  (let* ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				 *tccs-generated-for*))
	 (modinst (module-instance expr))
	 (decl (declaration expr))
	 (theory (or (and (resolution modinst) (declaration modinst))
		     (if (and (module decl)
			      (eq (id (module decl)) (id modinst)))
			 (module decl)
			 (or (get-theory modinst)
			     (if (and (bind-decl? decl)
				      (null (actuals modinst)))
				 (current-theory)
				 (break "something is wrong"))))))
	 (acts (actuals modinst))
	 (dacts (dactuals modinst))
	 (*set-type-actuals-name* expr))
    (unless (memq expr *exprs-generating-actual-tccs*)
      (when (or acts dacts)
	(unless (or *in-checker* *in-evaluator*)
	  (push expr *exprs-generating-actual-tccs*)))
      (when (or acts dacts)
	(check-type-actuals* (append acts dacts)
			     (append (when acts (formals-sans-usings theory))
				     (when dacts (decl-formals decl)))
			     theory)
	(when acts
	  ;; Generate TCCs as instances of assumings dacts are not involved
	  (generate-assuming-tccs modinst expr theory)
	  ;; Finally, the expr may have actuals that differ from the
	  ;; module-instance actuals, deal with them here.
	  (generate-actuals-tccs (actuals expr) acts))
	(when dacts
	  (generate-actuals-tccs (dactuals expr) dacts))))))

;;; Run through the actuals and formals, substituting and generating TCCs
;;; accordingly.  For example, given a theory "th[T: TYPE, S: TYPE FROM T,
;;; c: S]" and an instance "th[int, nat, -1]", will substitute int for T,
;;; check that nat is a subtype of int and substitute nat for S, and finally
;;; check that -1 is a nat, generating a TCC.  Note that the actuals and
;;; formals include both theory and decl parameters
(defun check-type-actuals* (actuals formals theory &optional alist)
  (when actuals
    (multiple-value-bind (nform nalist)
	(subst-actuals-in-next-formal (car actuals) (car formals) alist)
      (check-type-actual (car actuals) nform theory)
      (check-type-actuals* (cdr actuals) (cdr formals) theory nalist))))

(defmethod check-type-actual (act (formal formal-type-decl) theory)
  (declare (ignore theory))
  (check-for-tccs* (type-value act) nil)
  (when (nonempty-type-decl? formal)
    (unless (eq *generate-tccs* 'none)
      ;; May need to generate existence TCC
      (check-nonempty-type (type-value act) (current-declaration)))))

(defmethod check-type-actual (act (formal formal-subtype-decl) theory)
  (declare (ignore theory))
  (call-next-method)
  (let* ((tact (type-value act))
	 (texp (type-value formal))
	 (vid (make-new-variable '|x| act))
	 (vb (make!-bind-decl vid tact))
	 (*tcc-conditions* (cons vb *tcc-conditions*))
	 (svar (mk-name-expr vid nil nil
			     (make-resolution vb nil tact))))
    (when (place act)
      (set-extended-place svar act "variable created from actual ~a" act))
    (check-for-subtype-tcc svar (supertype texp))))

(defmethod check-type-actual (act (formal formal-const-decl) theory)
  (declare (ignore theory))
  (check-for-tccs* (expr act) (type formal)))

(defmethod check-type-actual (act (formal formal-theory-decl) theory)
  (set-type-actuals-and-maps (expr act) theory))

(defun check-type-maps (name)
  (when (mappings name)
    (check-type-mappings name (get-theory name))))

(defun check-type-mappings (thinst theory)
  (when (mappings thinst)
    (let* ((athinst (if (recursive-type? theory)
                        (typecheck* (copy thinst
                                      :id (id (adt-theory theory))
                                      :resolutions nil)
                                    nil 'module nil)
                        thinst))
           (cthinst (copy athinst :mappings nil)))
      ;;(check-type-lhs-mappings (mappings thinst) cthinst) ; Not needed, can't generate TCCs
      (let* ((smappings (sort-mappings (mappings thinst)))
             (entry (get-importings theory))
             (there? (member thinst entry :test #'tc-eq))
             (*current-context* (if there?
                                    (copy-context *current-context*)
                                    *current-context*)))
;;      (unless there?
;;        (add-to-using thinst theory))
        (check-type-mappings* smappings cthinst)))))

(defun check-type-mappings* (mappings thinst &optional previous-mappings)
  (when mappings
    (let ((map (car mappings)))
      (check-type-mapping map thinst previous-mappings)
      (let ((smaps (cdr mappings)))
        (check-type-mappings* smaps thinst
                              (nconc previous-mappings (list map)))))))

(defmethod check-type-mapping ((map mapping) thinst previous-mappings)
  (let ((lhs (lhs map))
        (rhs (rhs map)))
    (assert (resolutions lhs))
    ;;(assert (ptypes (expr rhs)))
    ;;(determine-best-mapping-lhs lhs rhs)
    (if (decl-formals lhs)
        (let* ((ctn (current-theory-name))
               (cthinst (lcopy ctn :dactuals (dactuals lhs))))
          (setf (current-theory-name) cthinst)
          (unwind-protect 
              (with-current-decl lhs
                (check-type-mapping-rhs rhs lhs thinst previous-mappings))
            (setf (current-theory-name) ctn)))
        (check-type-mapping-rhs rhs lhs thinst previous-mappings))))

(defun check-type-mapping-rhs (rhs lhs thinst mappings)
  (typecase (declaration lhs)
    (type-decl
     (assert (type-value rhs))
     (check-for-tccs* (type-value rhs) nil))
    ((or theory-reference module)
     (when (or (actuals (expr rhs)) (mappings (expr rhs)))
       (check-type-actuals-and-maps (expr rhs))))
    (t ;; May need to perform two subst-mod-params because the lhs decl may not be directly
     ;; from the theory being mapped
     ;; First we substitute based on the lhs, then based on the thinst
     (let* ((theory (module (declaration lhs)))
	    (lthinst (module-instance lhs))
	    (ltype (subst-mod-params (type (declaration lhs)) lthinst theory lhs))
	    (mapthinst (lcopy thinst
			 :mappings (append mappings (mappings thinst))
			 :dactuals (dactuals lhs)))
	    ;; Need mapthinst to match theory
	    (stype (subst-mod-params ltype mapthinst (module (declaration mapthinst)) lhs))
	    (subst-type (subst-mod-params-all-mappings stype))
	    (subst-types (if (free-params stype)
			     (possible-mapping-subst-types
			      (ptypes (expr rhs)) stype)
			     (list stype)))
	    (etype (or (car subst-types) subst-type)))
       (assert (fully-instantiated? etype))
       (set-type (expr rhs) etype)
       (when (definition (declaration lhs))
	 (assert (def-axiom (declaration lhs)))
	 (let* ((lname-expr (mk-name-expr lhs))
		(slhs (subst-mod-params lname-expr mapthinst
			(module (declaration lhs))
			(declaration lname-expr))))
	   (generate-mapped-eq-def-tcc slhs (expr rhs) mapthinst)))))))

(defmethod check-type-mapping ((map mapping-rename) thinst previous-mappings)
  (declare (ignore thinst previous-mappings))
  nil)

(defmethod check-for-tccs* ((expr number-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr rational-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr tuple-expr) expected)
  (let* ((stype (find-supertype expected))
	 (types (types stype))
	 (exprs (exprs expr)))
    (check-tup-types exprs types)))

(defun check-tup-types (exprs expected)
  (when exprs
    (let ((type (if (dep-binding? (car expected))
		    (type (car expected))
		    (car expected))))
      (assert (type (car exprs)))
      (check-for-tccs* (car exprs) type)
      (check-tup-types (cdr exprs)
		       (if (dep-binding? (car expected))
			   (let* ((binding (car expected))
				  (nexpr (substit (cdr expected)
					   (acons binding (car exprs) nil))))
			     nexpr)
			   (cdr expected))))))


(defmethod check-for-tccs* ((expr cases-expr) expected)
  (check-for-tccs* (expression expr) (type (expression expr)))
  (let* ((atype (find-supertype (type (expression expr))))
	 (adt (when (adt? atype) (adt atype))))
    (dolist (s (selections expr))
      (check-for-tcc-selection s expr expected))
    (if (else-part expr)
	(let ((*tcc-conditions* (push-tcc-conditions
				 (make-else-cases-conditions expr)
				 *tcc-conditions*)))
	  (check-for-tccs* (else-part expr) expected))
	(generate-selections-tccs expr (constructors atype) adt))))

(defun check-for-tcc-selection (s expr expected)
  (let* ((equality (make-selection-equality s expr))
	 (*bound-variables* (append (args s) *bound-variables*))
	 (*tcc-conditions* (push-tcc-condition equality
					       (append (reverse (args s))
						       *tcc-conditions*))))
    (check-for-tccs* (expression s) expected)))

(defmethod check-for-tccs* ((expr projection-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr injection-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr injection?-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr extraction-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr projection-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr injection-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr injection?-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr extraction-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr field-application) expected)
  (declare (ignore expected))
  (with-slots (id argument) expr
    (check-for-tccs* (argument expr) (type argument))))

(defmethod check-for-tccs* :around ((expr implicit-conversion) expected)
  (declare (ignore expected))
  (let ((*checking-implicit-conversion* expr))
    (call-next-method)))

(defmethod check-for-tccs* ((expr application) expected)
  (with-slots (operator argument type) expr
    #+pvsdebug
    (assert (every #'(lambda (x) (or (not (consp x))
				     (and (bind-decl? (car x))
					  (memq (car x) *tcc-conditions*))))
		   *tcc-conditions*))
    (let ((optype (find-supertype (type operator))))
      (check-for-tccs* (argument expr) (domain optype))
      (when (some #'recursive-defn-conversion? (arguments expr))
	(let ((noptype
	       (adjust-application-operator-from-arg-judgements expr expected)))
	  (when noptype
	    (setq optype noptype)
	    (setf (type expr)
		  (application-range-type (argument expr) noptype)))))
      (let* ((*appl-tcc-conditions*
	      (cons (appl-tcc-conditions operator argument)
		    *appl-tcc-conditions*))
	     (*set-type-recursive-operator*
	      (check-set-type-recursive-operator expr)))
	(if (lambda-expr? expr)
	    ;; Was changed by the recursion conversion
	    (check-for-tccs* expr (type expr))
	    (if (typep expr '(or let-expr where-expr))
		(let ((*bound-variables* (append (let-bindings* expr) *bound-variables*))
		      (*tcc-conditions* (append (let-tcc-conditions* expr) *tcc-conditions*)))
		  (check-for-tccs* (let-expression* expr) expected))
		(check-for-tccs* (operator expr) optype))))))
  (check-for-recursive-tcc expr))

(defmethod check-for-tccs* ((ex list-expr) expected)
  (declare (ignore expected))
  (unless (type ex) (call-next-method)))

(defmethod check-for-tccs* ((ex conjunction) expected)
  (declare (ignore expected))
  (check-for-tccs* (args1 ex) *boolean*)
  (let ((*tcc-conditions* (push-tcc-condition (args1 ex) *tcc-conditions*)))
    (check-for-tccs* (args2 ex) *boolean*)))

(defmethod check-for-tccs* ((ex disjunction) expected)
  (declare (ignore expected))
  (check-for-tccs* (args1 ex) *boolean*)
  (let ((*tcc-conditions* (push-tcc-condition (make!-negation (args1 ex))
					     *tcc-conditions*)))
    (check-for-tccs* (args2 ex) *boolean*)))

(defmethod check-for-tccs* ((ex implication) expected)
  (declare (ignore expected))
  (check-for-tccs* (args1 ex) *boolean*)
  (let ((*tcc-conditions* (push-tcc-condition (args1 ex) *tcc-conditions*)))
    (check-for-tccs* (args2 ex) *boolean*)))

(defmethod check-for-tccs* ((expr branch) expected)
  (let ((econd (condition expr))
	(ethen (then-part expr))
	(eelse (else-part expr)))
    (check-for-tccs* econd *boolean*)
    (let ((*tcc-conditions* (push-tcc-condition econd *tcc-conditions*)))
      (check-for-tccs* ethen expected))
    (let ((*tcc-conditions* (push-tcc-condition (make!-negation econd)
					       *tcc-conditions*)))
      (check-for-tccs* eelse expected))))

(defmethod check-for-tccs* ((expr first-cond-expr) expected)
    (declare (ignore expected))
  (call-next-method)
  (generate-cond-tccs expr))

(defmethod check-for-tccs* ((expr single-cond-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (generate-cond-tccs expr))

(defmethod check-for-tccs* ((ex lambda-expr) expected)
  (let* ((*bound-variables* (append (bindings ex) *bound-variables*))
	 (sexpected (find-supertype expected))
	 (edomain (if (dep-binding? (domain sexpected))
		      (type (domain sexpected))
		      (domain sexpected)))
	 (erange (if (dep-binding? (domain sexpected))
		     (substit (range sexpected)
		       (acons (domain sexpected)
			      (let ((vars (mapcar #'make-variable-expr
					    (bindings ex))))
				(if (cdr vars)
				    (make!-tuple-expr vars)
				    (car vars)))
			      nil))
		     (range sexpected)))
	 (adomain (domain (find-supertype (type ex)))))
    (check-for-binding-expr-tccs (append (bindings ex)
					 (list (expression ex)))
				 (nconc (mapcar #'type (bindings ex))
					(list erange)))
    (unless (or (tc-eq adomain edomain)
		(recursive-defn-conversion? ex))
      (let ((epreds (equality-predicates adomain edomain)))
	(when epreds
	  (generate-subtype-tcc ex expected (list epreds)))))
    (let ((toppreds (compatible-preds sexpected expected ex)))
      (when toppreds
	(generate-subtype-tcc ex expected toppreds)))))

(defmethod check-for-tccs* ((ex set-list-expr) expected)
  (let ((est (find-supertype expected)))
    (cond ((null (exprs ex))
	   (unless (and (funtype? est)
			(tc-eq (range est) *boolean*))
	     (type-error ex "~a expected here" expected))
	   ;; Nothing really to do here - just check everything OK
	   (assert (bindings ex))
	   (assert (expression ex))
	   (assert (type ex)))
	  (t (dolist (e (exprs ex))
	       (check-for-tccs* e (domain est)))))))

(defun check-for-binding-expr-tccs (bindings expected-types)
  #+pvsdebug
  (assert (every #'(lambda (x) (or (not (consp x))
				   (and (bind-decl? (car x))
					(memq (car x) *tcc-conditions*))))
		 *tcc-conditions*))
  (if (cdr bindings)
      (let* ((dep? (typep (car expected-types) 'dep-binding))
	     (etype (if dep?
			(type (car expected-types))
			(car expected-types))))
	(check-for-tccs* (car bindings) etype)
	(let ((*bound-variables* (cons (car bindings) *bound-variables*))
	      (*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
	  #+pvsdebug
	  (assert (every #'(lambda (x) (or (not (consp x))
					   (and (bind-decl? (car x))
						(memq (car x) *tcc-conditions*))))
			 *tcc-conditions*))
	  (check-for-binding-expr-tccs
	   (cdr bindings)
	   (if dep?
	       (substit (cdr expected-types)
		 (acons (car expected-types) (car bindings) nil))
	       (cdr expected-types)))))
      (let ((*tcc-conditions*
	     (append (car *appl-tcc-conditions*) *tcc-conditions*)))
	#+pvsdebug
	(assert (every #'(lambda (x) (or (not (consp x))
					 (and (bind-decl? (car x))
					      (memq (car x) *tcc-conditions*))))
		       *tcc-conditions*))
	(check-for-tccs* (car bindings) (car expected-types)))))

(defmethod check-for-tccs* ((expr bind-decl) expected)
  (declare (ignore expected))
  (when (declared-type expr)
    (check-for-tccs* (declared-type expr) nil)))


(defmethod check-for-tccs* ((expr quant-expr) expected)
  (check-for-binding-expr-tccs (append (bindings expr)
				       (list (expression expr)))
			       (nconc (mapcar #'type (bindings expr))
				      (list expected))))

(defmethod check-for-tccs* ((expr record-expr) expected)
  (let* ((sexpected (find-supertype expected))
	 (ass (assignments expr))
	 (args-list (mapcar #'arguments ass))
	 (values (mapcar #'expression ass)))
    (check-assignment-arg-types args-list values nil expr sexpected)))

(defmethod check-for-tccs* ((expr update-expr) (expected recordtype))
  (with-slots (expression assignments) expr
    (let ((atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-for-tccs* expression
		       (if (some #'maplet? assignments)
			   (contract-expected expr atype)
			   expected))
      (check-assignment-arg-types args-list values expression expr expected))))

(defmethod check-for-tccs* ((expr update-expr) (expected struct-sub-recordtype))
  (with-slots (expression assignments) expr
    (let ((atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-for-tccs* expression
		       (if (some #'maplet? assignments)
			   (contract-expected expr atype)
			   expected))
      (check-assignment-arg-types args-list values expression expr expected))))

(defmethod check-for-tccs* ((expr update-expr) (expected tupletype))
  (with-slots (expression assignments) expr
    (check-for-tccs* expression
		     (if (some #'maplet? assignments)
			 (type expression)
			 expected))
    (let ((atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-assignment-arg-types args-list values expression expr atype))))

(defmethod check-for-tccs* ((expr update-expr) (expected funtype))
  (with-slots (expression assignments) expr
    (check-for-tccs* expression (if (some #'maplet? assignments)
				    (type expression)
				    expected))
    (let (;;(atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-assignment-arg-types args-list values expression expr expected))))

(defmethod check-for-tccs* ((expr update-expr) (expected datatype-subtype))
  (with-slots (expression assignments) expr
    (check-for-tccs* expression
		     (if (some #'maplet? assignments)
			 (type expression)
			 expected))
    (let ((atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-assignment-arg-types args-list values expression expr atype))))

(defmethod check-for-tccs* ((expr update-expr) (expected adt-type-name))
  (with-slots (expression assignments) expr
    (check-for-tccs* expression (type expression))
    (let ((atype (find-supertype (type expr)))
	  (args-list (mapcar #'arguments assignments))
	  (values (mapcar #'expression assignments)))
      (check-assignment-arg-types args-list values expression expr atype))))

(defun check-assignment-arg-types (args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (check-assignment-arg-types* args-list values ex expr expected))

(defmethod check-assignment-arg-types* (args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (when args-list
    (check-assignment-arg-type (car args-list) (car values) ex expr expected)
    (check-assignment-arg-types* (cdr args-list) (cdr values) ex expr expected)))

(defun check-assignment-arg-type (args value ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (check-assignment-arg-type* args value ex expr expected))

(defmethod check-assignment-arg-type* ((args null) value ex expr expected)
  (declare (ignore ex expr))
  ;;(assert (typep expr '(or record-expr update-expr)))
  (check-for-tccs* value expected))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected subtype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (let ((stype (find-adt-supertype expected)))
    (typecase stype
      ((or funtype recordtype tupletype adt-type-name datatype-subtype)
       (check-assignment-arg-types* args-list values ex expr stype)
       (let* ((assns (mapcan #'(lambda (a v)
				 (unless (null a)
				   (list (make-assignment a v))))
		       args-list values))
	      (updex (when assns (make!-update-expr ex assns))))
	 (when updex
	   (set-extended-place updex ex
			       "creating update-expr for subtype check")
	   (check-for-subtype-tcc updex expected)))
       (mapc #'(lambda (a v)
		 (unless a (check-for-subtype-tcc v expected)))
	     args-list values))
      (t (call-next-method)))))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected recordtype))
  (with-slots (fields) expected
    (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
    (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
    (if (every #'null args-list)
	(call-next-method)
 	(progn
	  (mapc #'(lambda (a v)
		    (unless a (check-for-tccs* v expected)))
		args-list values)
	  (multiple-value-bind (cargs-list cvalues)
	      (complete-assignments args-list values ex expr expected)
	    (check-assignment-rec-arg-types cargs-list cvalues ex fields expr))))))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected struct-sub-recordtype))
  (with-slots (fields) expected
    (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
    (if (every #'null args-list)
	(call-next-method)
 	(progn
	  (mapc #'(lambda (a v)
		    (unless a (check-for-tccs* v expected)))
		args-list values)
	  (multiple-value-bind (cargs-list cvalues)
	      (complete-assignments args-list values ex expr expected)
	    (check-assignment-rec-arg-types cargs-list cvalues ex fields expr))))))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected tupletype))
  (with-slots (types) expected
    (assert (typep expr '(or record-expr update-expr)))
    (if (every #'null args-list)
	(call-next-method)
	(multiple-value-bind (cargs-list cvalues)
	    (complete-assignments args-list values ex expr expected)
	  (check-assignment-tup-arg-types cargs-list cvalues ex types 1 expr)))))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected funtype))
  (with-slots (domain range) expected 
    (if (every #'null args-list)
	(call-next-method)
	(check-assignment-fun-arg-types args-list values ex expr expected))))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected datatype-subtype))
  (assert (typep expr '(or record-expr update-expr)))
  (if (every #'null args-list)
      (check-assignment-arg-type (car args-list) (car values) ex expr expected)
      (check-assignment-update-arg-types args-list values ex expr expected)))

(defmethod check-assignment-arg-types* (args-list values ex expr (expected adt-type-name))
  (assert (typep expr '(or record-expr update-expr)))
  (if (every #'null args-list)
      (call-next-method)
      (check-assignment-update-arg-types args-list values ex expr expected)))

(defun check-assignment-rec-arg-types (args-list values ex fields expr
				       &optional cargs cvalues)
  (assert (typep expr '(or record-expr update-expr)))
  (when args-list
    (let* ((pos (position (car fields) args-list :test #'same-id :key #'caar))
	   (args (when pos (nth pos args-list)))
	   (value (when pos (nth pos values)))
	   (rem-args (if args
			 (remove args args-list :count 1 :start pos)
			 args-list))
	   (rem-values (if value
			   (remove value values :count 1 :start pos)
			   values))
	   (done-with-field? (not (member (car fields) rem-args
					  :test #'same-id :key #'caar))))
      (multiple-value-bind (cdr-args cdr-vals)
	  (when done-with-field?
	    (rec-arg-types-cdrs (cons args cargs) (cons value cvalues) nil nil))
	(when args
	  (assert (field-assignment-arg? (caar args)))
	  (when done-with-field?
	    (let* ((fappl (when (and ex (some (complement #'null) cdr-args))
			    (make!-field-application (car fields) ex)))
		   (fldtype (type (car fields)))
		   (ftype (find-supertype fldtype)))
	      (when fappl
		(set-extended-place fappl ex
				    "creating field application from ~a and ~a"
				    ex (car fields)))
	      (check-assignment-arg-types* cdr-args cdr-vals fappl expr fldtype)
	      (when (and fappl
			 (funtype? (find-supertype ftype)))
		(let* ((bid (make-new-variable '|x| (cons ex cdr-args)))
		       (bd (make-bind-decl bid (domtype ftype)))
		       (bvar (make-variable-expr bd))
		       (deqn (if (cdr (caar cdr-args))
				 ;; Should have a tuple domtype with matching length
				 (let ((tup (make!-tuple-expr* (caar cdr-args))))
				   (set-extended-place tup (caaar cdr-args)
						       "creating tuple ~a for record type" tup)
				   (make!-disequation bvar tup))
				 (make!-disequation bvar (caaar cdr-args))))
		       (*tcc-conditions* (cons deqn (cons bd *tcc-conditions*)))
		       (app (make!-application fappl bvar))
		       (expected (if (dep-binding? (domain ftype))
				     (substit (range ftype)
				       (acons (domain ftype) bvar nil))
				     (range ftype))))
		  (set-extended-place bvar ex
				      "creating variable ~a for recordtype"
				      bvar)
		  (set-extended-place deqn (caaar cdr-args)
				      "creating disequation ~a for recordtype"
				      deqn)
		  (check-for-tccs* app expected))))))
	(let ((nfields (if done-with-field?
			   (if (some #'(lambda (fld)
					 (member (car fields) (freevars fld)
						 :key #'declaration))
				     fields)
			       (if (and ex (some (complement #'null) cdr-args))
				   (let* ((fapp (make!-field-application (car fields) ex))
					  (ass (make-assignment (car cdr-args) (car cdr-vals)))
					  (val (make!-update-expr fapp (list ass))))
				     (subst-rec-dep-type val (car fields) (cdr fields)))
				   (subst-rec-dep-type value (car fields) (cdr fields)))
			       (cdr fields))
			   fields)))
	  (if nfields
	      (check-assignment-rec-arg-types rem-args rem-values ex nfields expr
					      (unless done-with-field?
						(cons args cargs))
					      (unless done-with-field?
						(cons value cvalues)))
	      (check-assignment-rec-arg-maplet-types rem-args rem-values ex expr)))))))

(defun check-assignment-rec-arg-maplet-types (args-list values ex expr)
  (assert (typep expr '(or record-expr update-expr)))
  (when args-list
    (let ((args (car args-list))
	  (value (car values)))
      (assert (null (cdr args)))
      (assert (field-assignment-arg? (caar args)))
      (check-for-tccs* value (car (ptypes value))))
    (check-assignment-rec-arg-maplet-types (cdr args-list) (cdr values) ex expr)))

(defun check-assignment-tup-arg-types (args-list values ex types index expr
					       &optional cargs cvalues)
  (assert (typep expr '(or record-expr update-expr)))
  (when args-list
    (let* ((pos (position index args-list
			  :test #'eql :key #'(lambda (a)
					       (when a (number (caar a))))))
	   (args (when pos (nth pos args-list)))
	   (value (when pos (nth pos values)))
	   (rem-args (when args (remove args args-list)))
	   (rem-values (when value (remove value values)))
	   (done-with-index?
	    (not (member index rem-args
			 :test #'eql :key #'(lambda (a)
					      (when a (number (caar a))))))))
      (when args
	(when done-with-index?
	  (let ((nargs (nreverse (cons args cargs)))
		(projapp (make!-projection-application index ex)))
	    (set-extended-place projapp ex
				"creating projection ~a for a tuple type"
				projapp)
	    (check-assignment-arg-types*
	     (mapcar #'cdr nargs)
	     (nreverse (cons value cvalues))
	     projapp
	     expr
	     (car types)))))
      (check-assignment-tup-arg-types
       rem-args rem-values ex
       (if done-with-index?
	   (if (dep-binding? (car types))
	       (substit (cdr types) (acons (car types) value nil))
	       (cdr types))
	   types)
       (if done-with-index?
	   (1+ index)
	   index)
       expr
       (unless done-with-index?
	 (cons args cargs))
       (unless done-with-index?
	 (cons value cvalues))))))

(defun check-assignment-fun-arg-types (args-list values ex expr funtype)
  (when args-list
    (multiple-value-bind (cargs cvalues rem-args rem-values)
	(collect-same-first-fun-assignment-args args-list values)
      (let ((domtypes (domain-types* (domain funtype))))
	(dolist (arg cargs)
	  (when arg
		
	    (if (length= domtypes (car arg))
		(check-tup-types (car arg) domtypes)
		(check-for-tccs* (caar arg) (domain funtype))))))
      (let* ((arg (when (caar cargs) (make!-arg-tuple-expr* (caar cargs))))
	     (app (when (and ex arg)
		    (let ((app (make!-application ex arg)))
		      (set-extended-place app expr
					  "making application ~a" app)
		      app))))
	(when (and arg (null (place arg)) (tuple-expr? arg))
	    (set-extended-place arg (caaar cargs)
				"creating tuple from assignment args ~a" (caar cargs)))
	(when app
	  (set-extended-place app ex
			      "creating application from ~a and ~a" ex (caar cargs)))
	(check-assignment-arg-types*
	 (mapcar #'cdr cargs) cvalues app expr
	 (if arg
	     (if (dep-binding? (domain funtype))
		 (substit (range funtype) (acons (domain funtype) arg nil))
		 (range funtype))
	     funtype)))
      (check-assignment-fun-arg-types rem-args rem-values ex expr funtype))))

(defun check-assignment-update-arg-types (args-list values ex expr expected)
  (let* ((ass-accs (mapcar #'caar args-list))
	 (constrs (remove-if #'(lambda (c)
				 (not (every #'(lambda (a)
						 (member a (accessors c)
							 :test #'same-id))
					     ass-accs)))
		    (constructors expected))))
;; 	 (dep-accs (get-dependent-accessors ass-accs expected))
;; 	 (dep-assns (mapcar #'(lambda (da)
;; 				(make-datatype-assignment
;; 				 da (expression expr)))
;; 		      dep-accs)))
    (mapc #'(lambda (a v)
	      (unless a (check-for-tccs* v expected)))
	  args-list values)
    (check-assignment-update-arg-types* constrs args-list values ex expr)))

(defun check-assignment-update-arg-types* (constrs args-list values ex expr)
  (assert constrs)
  (check-constructors-update-arg-types constrs args-list values ex expr))

;; When multiple constructors are involved we need to collect the TCCs for
;; each one, and form the disjunction.
(defun check-constructors-update-arg-types (constrs args-list values ex expr
						  &optional tccs recs)
  (if (null constrs)
      (unless (eq *generate-tccs* 'none)
	(let* ((dtcc (add-tcc-conditions (make!-disjunction* (nreverse tccs))))
	       (type (make!-expr-as-type
		      (if (cdr recs)
			  (let* ((id (make-new-variable '|x| recs))
				 (bd (make-bind-decl id (type ex)))
				 (var (make-variable-expr bd)))
			    (make!-set-expr (list bd)
			      (make!-disjunction*
			       (mapcar #'(lambda (r) (make!-application r var))
				 (nreverse recs)))))
			  (car recs))))
	       (id (make-tcc-name dtcc))
	       (ndecl (mk-subtype-tcc id dtcc)))
	  (insert-tcc-decl 'subtype ex type ndecl)))
      (let* ((c (car constrs))
	     (accs (accessors c))
	     (*tccforms* nil))
	(multiple-value-bind (cargs-list cvalues)
	    (complete-constructor-assignments args-list values ex accs)
	  (let ((*collecting-tccs* t))
	    (check-assignment-accessor-arg-types cargs-list cvalues ex accs expr))
	  (let* ((cpred (make!-application (recognizer c) ex))
		 (ntccs (cons cpred
			      (remove cpred (mapcar #'tccinfo-formula
					      *tccforms*)
				      :test #'tc-eq)))
		 (tcc (make!-conjunction* ntccs)))
	    (setf (parens tcc) 1)
	    (check-constructors-update-arg-types
	     (cdr constrs) args-list values ex expr
	     (cons tcc tccs) (cons (recognizer c) recs)))))))

(defun check-assignment-accessor-arg-types (args-list values ex accessors expr
						    &optional cargs cvalues)
  (when args-list
    (let* ((pos (position (car accessors) args-list :test #'same-id :key #'caar))
	   (args (when pos (nth pos args-list)))
	   (value (when pos (nth pos values)))
	   (rem-args (if args
			 (remove args args-list)
			 args-list))
	   (rem-values (if value
			   (remove value values)
			   values))
	   (done-with-acc? (not (member (car accessors) rem-args
					:test #'same-id :key #'caar))))
      (when args
	(when done-with-acc?
	  (check-assignment-arg-types*
	   (mapcar #'cdr (nreverse (cons args cargs)))
	   (nreverse (cons value cvalues))
	   (when ex (make!-application (car accessors) ex))
	   expr
	   (if (dep-binding? (domain (type (car accessors))))
	       (substit (range (type (car accessors)))
		 (acons (domain (type (car accessors))) ex nil))
	       (range (type (car accessors)))))))
      (check-assignment-accessor-arg-types
       rem-args rem-values ex
       (if done-with-acc?
	   (subst-acc-dep-type value (car accessors) (cdr accessors))
	   accessors)
       expr
       (unless done-with-acc?
	 (cons args cargs))
       (unless done-with-acc?
	 (cons value cvalues))))))


(defmethod check-for-tccs* ((expr update-expr) (expected subtype))
  (let ((stype (find-supertype expected)))
    (check-for-tccs* expr stype)
    (unless (eq *generate-tccs* 'none)
      (let* ((id (make-new-variable '|x| (list expr expected)))
	     (bd (make-bind-decl id stype))
	     (var (make-variable-expr bd))
	     (cpreds (compatible-predicates (list stype) expected var))
	     (incs (beta-reduce (substit cpreds
				  (acons bd (copy expr 'parens 1) nil)))))
	(when incs
	  (generate-subtype-tcc expr expected incs))))))

(defmethod check-for-tccs* ((expr update-expr) (expected dep-binding))
  (check-for-tccs* expr (type expected)))

(defmethod check-for-tccs* ((ass assignment) expected)
  (break "shouldn't get here")
  (with-slots (arguments expression) ass
    (assert (typep expected '(or funtype recordtype tupletype
				 datatype-subtype adt-type-name)))
    (check-assignment-arg-types (list arguments) (list expression) nil nil expected)))


;;; Check-types for type expressions
(defmethod check-for-tccs* :around ((te type-expr) expected)
  (declare (ignore expected))
  (unless (and (type-name? (print-type te))
	       (not (eq (module (declaration (print-type te)))
			(current-theory))))
    (call-next-method)))

(defmethod check-for-tccs* ((te type-name) expected)
  (declare (ignore expected))
  (when (or (actuals (module-instance te))
	    (dactuals (module-instance te)))
    (check-type-actuals te)))

(defmethod check-for-tccs* ((te type-application) expected)
  (declare (ignore expected))
  (check-for-tccs* (type te) nil))

(defmethod check-for-tccs* ((te subtype) expected)
  (when (supertype te)
    (check-for-tccs* (supertype te) expected))
  (when (predicate te)
    (check-for-tccs* (predicate te) (type (predicate te)))))

(defmethod check-for-tccs* ((te expr-as-type) expected)
  (declare (ignore expected))
  (assert (type (expr te)))
  (check-for-tccs* (expr te) (type (expr te))))

(defmethod check-for-tccs* ((te funtype) expected)
  (with-slots (domain range) te
    (check-for-tccs* domain expected)
;;     (if (dep-binding? domain)
;; 	(let* ((bd (make-bind-decl (id domain) (type domain)))
;; 	       (*tcc-conditions* (cons bd *tcc-conditions*)))
;; 	  (check-for-tccs* (substit range (acons domain bd nil)) expected))
;; 	(check-for-tccs* range expected))
    (check-for-tccs* range expected)))

(defmethod check-for-tccs* ((te dep-binding) expected)
  (check-for-tccs* (type te) expected))

(defmethod check-for-tccs* ((te tupletype) expected)
  (mapc #'(lambda (ty) (check-for-tccs* ty expected)) (types te)))

(defmethod check-for-tccs* ((te cotupletype) expected)
  (mapc #'(lambda (ty) (check-for-tccs* ty expected)) (types te)))

(defmethod check-for-tccs* ((te recordtype) expected)
  (mapc #'(lambda (fd) (check-for-tccs* (declared-type fd) expected))
	(fields te)))

(defmethod check-for-tccs* ((te struct-sub-recordtype) expected)
  (mapc #'(lambda (fd) (check-for-tccs* (declared-type fd) expected))
	(fields te)))

(defmethod check-for-tccs* ((te struct-sub-tupletype) expected)
  (mapc #'(lambda (ty) (check-for-tccs* ty expected))
	(types te)))
