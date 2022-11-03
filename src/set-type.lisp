;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-type.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Oct 20 00:42:24 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Tue May 25 17:58:36 2004
;; Update Count    : 161
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

(export '(set-type set-type* kind-of-name-expr set-type-actuals
                   subst-actuals-in-next-formal set-lambda-dep-types
                   free-formals
                   subst-for-formals))

(defvar *dont-worry-about-full-instantiations* nil)

(defvar *ignore-for-tccs* nil)

(defvar *appl-tcc-conditions* nil)

(defmethod set-type ((ex expr) expected)
  (assert *current-context*)
  (assert *generate-tccs*)
  (let ((*added-recursive-def-conversion* nil)
	(*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				*tccs-generated-for*)))
    (cond ((or (not (type ex))
               (eq *generate-tccs* 'all))
           (set-type* ex expected))
          ((eq *generate-tccs* 'top)
           (check-for-subtype-tcc ex expected)))))

(defmethod set-type ((te type-expr) expected)
  (assert *current-context*)
  (let ((*added-recursive-def-conversion* nil)
	(*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				*tccs-generated-for*)))
    (set-type* te expected)))

(defmethod set-type ((te dep-binding) expected)
  (let ((*added-recursive-def-conversion* nil)
	(*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				*tccs-generated-for*)))
    (set-type* te expected)))

(defmethod typed? ((expr expr))
  (and ;;(not (memq *generate-tccs* '(all top)))
       (type expr)))

(defmethod typed? ((expr name-expr))
  (and (singleton? (resolutions expr))
       (or (typep (declaration expr)
                  '(or module mod-decl formal-theory-decl
                       theory-abbreviation-decl))
           (call-next-method))))

(defmethod typed? ((act actual))
  (or (type-value act)
      (and (expr? (expr act))
	   (typed? (expr act)))))


;;; This is the basic function for selecting the best type from the set of
;;; possible types, and setting the type slot accordingly.  After setting
;;; the type, subtype tccs are checked for.

(defun set-expr-type (expr expected)
  (unless (type expr)
    #+pvsdebug (assert (not (duplicates? (ptypes expr))))
    (let* ((ptypes (remove-duplicates (ptypes expr) :test #'tc-eq))
           (possibles
            (delete-if-not #'(lambda (ty)
                               (and (not (eq ty 'type))
                                    (compatible? ty expected)))
              ptypes)))
      (cond ((singleton? possibles)
             (setf (type expr)
                   (if (fully-instantiated? (car possibles))
                       (car possibles)
                       (instantiate-from (car possibles) expected expr)))
             #+pvsdebug (assert (fully-instantiated? (type expr))))
            ((null possibles)
             (type-incompatible expr ptypes expected))
            (t (type-ambiguity expr))))))

(defvar *has-tccs* nil
  "Used to keep track of whether an expression generated any TCCs that
required a context.")

(defmethod set-type* :around ((ex expr) expected)
  #+pvsdebug (assert (fully-typed? expected))
  #+pvsdebug (assert (fully-instantiated? expected))
  #+pvsdebug (assert (or (type ex) (types ex)))
  #+pvsdebug (assert (not (and (type ex) (types ex))))
  #+pvsdebug (assert (every #'type-expr? (ptypes ex)))
  #+pvsdebug (assert (or (null (type ex))
                         (typep (current-declaration) 'conversion-decl)
                         (fully-instantiated? ex)))
  (unless (eq *generate-tccs* 'none)
    (let ((tcc-args-list-entry (assq ex (macro-subtype-tcc-args-alist (current-theory)))))
      (when tcc-args-list-entry
        (dolist (tcc-args (reverse (cdr tcc-args-list-entry)))
          (apply #'generate-subtype-tcc tcc-args))
        (setf (macro-subtype-tcc-args-alist (current-theory))
              (delete tcc-args-list-entry
		      (macro-subtype-tcc-args-alist (current-theory)))))))
  (let ((*set-type-expr* (if (place ex) ex *set-type-expr*)))
    (cond ((and (type ex)
		(fully-instantiated? ex))
           (unless (compatible? (type ex) expected)
             (type-incompatible ex (list (type ex)) expected))
           (when (eq *generate-tccs* 'all)
             (check-for-tccs ex expected (list ex))))
          (t (cond ((some #'(lambda (ty)
                              (and (type-expr? ty)
                                   (not (from-conversion ty))
                                   (compatible? ty expected)))
                          (ptypes ex))
                    (call-next-method))
                   ((and (not *no-conversions-allowed*)
			 (some #'(lambda (ty)
                                   (and (from-conversion ty)
					(compatible? ty expected)))
                               (ptypes ex))
			 (change-to-conversion ex expected)))
                   ((and (not *no-conversions-allowed*)
			 (look-for-conversion ex expected)))
                   (t (call-next-method)
                      (check-type-incompatible ex expected)))
             (reset-types ex)))
    #+pvsdebug (assert (fully-typed? ex))
    (assert (fully-instantiated? ex))
    (unless (typep ex '(or branch lambda-expr update-expr
			cases-expr let-expr where-expr))
      (if *added-recursive-def-conversion*
          (let ((*added-recursive-def-conversion* nil))
            (check-for-tccs ex (if (application? ex) (type ex) expected)))
          (check-for-subtype-tcc ex expected)))
    (set-ghost-type ex expected)
    ))

(defmethod ghost? ((db dep-binding))
  (ghost? (type db)))

(defun check-type-incompatible (ex expected)
  (unless (type ex)
    (type-incompatible ex (ptypes ex) expected)))

(defvar *testing-ghosts* nil)

(defun set-ghost-type (ex expected)
  (when *testing-ghosts*
    (set-ghost-type* ex expected)))

(defmethod set-ghost-type* :around ((ex expr) expected)
  (call-next-method)
  (unless (eq (ghost? (type ex)) (ghost? expected))
    (type-error ex "~:[~;non-~]ghost type where ~:[~;non-~]ghost expected~%~
                    ~a: ~a~_ expected: ~a"
		(ghost? (type ex)) (ghost? expected) ex (type ex) expected)))

(defmethod set-ghost-type* ((ex name-expr) expected)
  (declare (ignore expected))
  (when (and (ghost? (type ex))
	     (not (variable? ex)))
    (setf (type ex) (copy (type ex) :ghost? nil))))

;;; adt-name-expr
;;; projection-expr
;;; injection-expr
;;; injection?-expr
;;; extraction-expr

(defmethod set-ghost-type* ((ex projection-application) expected)
  (declare (ignore expected))
  (when (ghost? (type (argument ex)))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex injection-application) expected)
  (declare (ignore expected))
  (when (ghost? (type (argument ex)))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex injection?-application) expected)
  (declare (ignore expected))
  (when (ghost? (type (argument ex)))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex extraction-application) expected)
  (declare (ignore expected))
  (when (ghost? (type (argument ex)))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex field-application) expected)
  (declare (ignore expected))
  (when (ghost? (type (argument ex)))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex lambda-expr) expected)
  (declare (ignore expected))
  (when (ghost? (type ex))
    (setf (type ex) (copy (type ex) :ghost? nil))))

(defmethod set-ghost-type* ((ex application) expected)
  (declare (ignore expected))
  (let* ((op (operator ex))
	 (optype (type op))
	 (dom (domain optype))
	 (arg (argument ex))
	 (argtype (type arg)))
    (if (ghost? dom)
	(when (and (not (ghost? (type ex)))
		   (ghost? (range optype)))
	  (setf (type ex) (copy (type ex) :ghost? t)))
	(when (and (not (ghost? (type ex)))
		   (or (ghost? argtype)
		       (ghost? (range optype))))
	  (setf (type ex) (copy (type ex) :ghost? t))))))

;; (defmethod set-ghost-type* ((ex cases-expr) expected)
;;   (break "set-ghost-type* cases-expr"))

(defmethod set-ghost-type* ((ex table-expr) expected)
  (declare (ignore expected))
  (break "set-ghost-type* table-expr")
  (when (or (ghost? (type (row-expr ex)))
	    (and (col-expr ex) (ghost? (type (col-expr ex))))
	    (some #'(lambda (rh) (ghost? (type rh))) (row-headings ex))
	    (some #'(lambda (ch) (ghost? (type ch))) (col-headings ex))
	    ;;; more thought needed here
	    )
    ))
  
(defmethod set-ghost-type* ((ex record-expr) expected)
  (declare (ignore expected))
  (when (some #'(lambda (assn)
		  (ghost? (type (expression assn))))
	      (assignments ex))
    (setf (type ex) (copy (type ex) :ghost? t))))

(defmethod set-ghost-type* ((ex t) expected)
  (declare (ignore expected))
  nil)

(defun reset-types (ex)
  (setf (types ex) nil))


;;; expand1 expands the top-level constant or application; it either
;;; returns the expanded form or the original expression if it was not
;;; expanded.  Nested applications are also handled, for example, given:
;;;   F(x, y: int)(z: int): int = x * y + z
;;; (expand1 "F(1,2)(3)") ==> 1 * 2 + 3
;;; (expand1 "F(1,2)")    ==> (LAMBDA (z:int): 1 * 2 + z)
;;; (expand1 "F")         ==> (LAMBDA (z:int): (LAMBDA (x,y:int): x * y + z))

;;; If you use expand1 in a recursive lisp function, it is always best to
;;; use it on the application before applying it to any operators.  This
;;; minimizes the need for beta-reduction.

;;; Most of this code is pretty straight-froward.  The only tricky part is
;;; that it is possible for a definition to use fewer arguments than are
;;; provided.  For example:
;;;   f: [int -> int]
;;;   F(x, y: int): [int -> int] = f
;;; (expand1 "F(1,2)(3)") ==> F(1,2)(3)
;;; This is checked for by comparing the argument-application-number and
;;; lambda-binding-number.

(defun expand1 (ex)
  (let ((op (operator* ex)))
    (if (and (typep op 'name-expr)
             (typep (declaration op) '(and const-decl (not def-decl)))
             (def-axiom (declaration op))
             (<= (argument-application-number ex)
                 (lambda-binding-number
                  (args2 (car (last (def-axiom (declaration op))))))))
        (expand1* ex
                  (subst-mod-params
                   (args2 (car (last (def-axiom (declaration op)))))
                   (module-instance op)
                   (module (declaration op))))
        ex)))

(defmethod expand1* ((ex application) (def lambda-expr))
  (expand1* (operator ex)
            (substit (expression def)
              (if (singleton? (bindings def))
                  (acons (car (bindings def)) (argument ex) nil)
                  (pairlis (bindings def)
                           (if (typep (argument ex) 'tuple-expr)
                               (exprs (argument ex))
                               (make-projections (argument ex))))))))

(defmethod expand1* ((ex expr) def)
  def)

(defmethod lambda-binding-number ((ex lambda-expr) &optional (num 0))
  (lambda-binding-number (expression ex) (1+ num)))

(defmethod lambda-binding-number ((ex expr) &optional (num 0))
  num)


(defmethod set-type* ((ex name-expr) expected)
  (assert (or (null (type ex)) (resolution ex)))
  (unless (and (type ex)
	       (fully-instantiated? ex))
    (let* ((creses (remove-if-not #'(lambda (r)
                                      (and (type r)
                                           (compatible? (type r) expected)))
                     (resolutions ex)))
           (sreses (remove-if-not
                       #'(lambda (r)
                           (strict-compatible? (type r) expected))
                     creses)))
      (if (and (null creses)
               (name-expr-from-number? ex)
               (compatible? expected *number_field*))
          (change-class ex 'number-expr :type (get-expr-number-type (number ex)))
          (set-type-name-expr ex expected
                              (or sreses creses (resolutions ex)))))))

(defun set-type-name-expr (ex expected creses)
  (assert creses)
  (setf (resolutions ex) creses)
  (let ((res (find-best-name-resolution ex creses expected)))
    (setf (resolutions ex) (list res))
    ;; Why not just check for actuals of the module-instance
    (unless (fully-instantiated? res)
      (let ((nres (instantiate-resolution ex res expected)))
        (assert nres)
        (if (or *dont-worry-about-full-instantiations*
                (fully-instantiated? nres))
            (setf res nres
                  (resolutions ex) (list nres))
            (type-error ex
              "Could not determine the full theory instance for~% ~a" ex))))
    (when (or (actuals (module-instance ex))
              (dactuals (module-instance ex))
              (mappings (module-instance ex)))
      (set-actuals-places ex)
      (setf (module-instance (resolution ex))
            (set-type-actuals-and-maps ex (module (declaration (resolution ex))))))
    (change-name-expr-class-if-needed (declaration res) ex)
    (let ((alias (cdr (assq (declaration res) (boolean-aliases)))))
      (when alias
        (setf (declaration res) alias)))
    (when (and (typep (declaration ex) 'bind-decl)
               (not (memq (declaration ex) *bound-variables*)))
      (type-error ex "Bound variable ~a outside of context" ex))
    (unless (compatible? (type res) expected)
      (if (integerp (id ex))
          (change-class ex 'number-expr :number (id ex)
                        :type (or *real* *number_field*))
          (type-incompatible ex (list (type res)) expected)))
    (set-type-name-expr* ex res)
    (check-set-type-recursive-name ex)))

(defun set-actuals-places (name &optional (res (resolutions name)))
  (when (place name)
    (dolist (r res)
      (if (and (actuals name) (every #'place (actuals name)))
	  (mapc #'(lambda (nact iact)
		    (cond ((string= (str nact) (str iact))
			   (setf (place iact) (place nact))
			   (setf (place (expr iact)) (place nact)))
			  (t (set-extended-place iact nact
						 "actual created from actual ~a" nact))))
		(actuals name) (actuals (module-instance r)))
	  (dolist (act (actuals (module-instance r)))
	    (unless (place act)
	      (set-extended-place act name "actual created for name ~a" name))))
      (if (and (dactuals name) (every #'place (dactuals name)))
	  (mapc #'(lambda (nact iact)
		    (cond ((string= (str nact) (str iact))
			   (setf (place iact) (place nact))
			   (setf (place (expr iact)) (place nact)))
			  (t (set-extended-place iact nact
						 "actual created from actual ~a" nact))))
		(dactuals name) (dactuals (module-instance r)))
	  (dolist (act (dactuals (module-instance r)))
	    (unless (place act)
	      (set-extended-place act name "actual created for name ~a" name)))))))

(defun check-set-type-recursive-name (ex)
  ;; No need to do anything if not in a recursive decl
  (when (and (not (eq *generate-tccs* 'none))
             (typep (current-declaration) '(and def-decl (not adt-def-decl)))
             (eq (declaration ex) (current-declaration))
             (not (memq ex *set-type-recursive-operator*)))
    (let* ((cex (copy ex))
           (nex (recursive-def-conversion (current-declaration))))
      ;; Add conversion
      (change-class ex 'recursive-defn-conversion)
      (setf (bindings ex) (bindings nex)
            (expression ex) (expression nex)
            (type ex) (type nex)
            (from-expr ex) cex)
      (add-conversion-info nex cex ex)
      (push ex *added-recursive-defn-conversions*)
      (setq *added-recursive-def-conversion* t))))

(defmethod set-type-name-expr* ((ex name-expr) res)
  (setf (type ex) (type res)))

(defmethod set-type-name-expr* ((ex number-expr) res)
  (declare (ignore res))
  nil)

(defmethod set-type* ((ex fieldex) expected)
  (assert (singleton? (ptypes ex)))
  (unless (compatible? (car (ptypes ex)) expected)
    (type-incompatible ex (ptypes ex) expected))
  (if (or (actuals ex) (dactuals ex))
      (setf (type ex) (car (ptypes ex)))
      (let ((etype (compatible-type expected (car (ptypes ex)))))
        (setf (type ex) etype)))
  (when (has-type-vars? (type ex))
    (type-error ex
      "Cannot determine the type associated with ~a:~%  Please provide more ~
       information, i.e., actual parameters or a coercion." ex))
  ;; Now convert to a lambda expression
  (let* (;;(cex (copy ex))
         (dtype (if (dep-binding? (domain (type ex)))
                    (type (domain (type ex)))
                    (domain (type ex))))
         (id (make-new-variable '|r| (list ex)))
         (bd (make-bind-decl id dtype))
         (var (make-variable-expr bd))
         (expr (make!-field-application (id ex) var)))
    (change-class ex 'fieldex-lambda-expr
      :bindings (list bd)
      :expression expr
      :type (make-formals-funtype (list (list bd)) (type expr)))))

(defmethod set-type* ((ex projection-expr) expected)
  (assert (ptypes ex))
  (let ((ctypes (remove-if (complement
                            #'(lambda (ty) (compatible? ty expected)))
                  (ptypes ex))))
    (unless ctypes
      (type-incompatible ex (ptypes ex) expected))
    (when (cdr ctypes)
      (type-ambiguity ex))
    (if (or (actuals ex) (dactuals ex))
        (setf (type ex) (car ctypes))
        (let ((etype (compatible-type expected (car ctypes))))
          (setf (type ex) etype)))))

(defmethod set-type* ((ex injection-expr) expected)
  (assert (singleton? (ptypes ex)))
  (unless (compatible? (car (ptypes ex)) expected)
    (type-incompatible ex (ptypes ex) expected))
  (let* ((etype (if (or (actuals ex) (dactuals ex))
                    (car (ptypes ex))
                    (compatible-type expected (car (ptypes ex)))))
	 (ftype (if (has-type-vars? etype)
		    (compatible-type etype expected)
		    etype))
         (cotuptype (range (find-supertype ftype)))
         (inrec (make-instance 'injection?-expr
                    :id (makesym "IN?_~d" (index ex))
                    :index (index ex)
                    :type (mk-funtype cotuptype *boolean*)))
         (insubtype (make!-expr-as-type inrec)))
    (set-extended-place inrec ex
			"creating injection recognizer ~a" inrec)
    (setf (type ex) (mk-funtype (domain (find-supertype ftype)) insubtype))))

(defmethod set-type* ((ex injection?-expr) expected)
  (assert (singleton? (ptypes ex)))
  (unless (compatible? (car (ptypes ex)) expected)
    (type-incompatible ex (ptypes ex) expected))
  (if (or (actuals ex) (dactuals ex))
      (setf (type ex) (car (ptypes ex)))
      (let* ((etype (compatible-type expected (car (ptypes ex))))
	     (ftype (if (has-type-vars? etype)
			(compatible-type etype expected)
			etype)))
        (setf (type ex) ftype))))

(defmethod set-type* ((ex extraction-expr) expected)
  (assert (singleton? (ptypes ex)))
  (let ((atype (car (ptypes ex))))
    (unless (compatible? atype expected)
      (type-incompatible ex (ptypes ex) expected))
    (let* ((etype (if (or (actuals ex) (dactuals ex))
                      atype
		      (compatible-type expected atype)))
	   (ftype (if (has-type-vars? etype)
		      (compatible-type etype expected)
		      etype))
           (cotuptype (domain (find-supertype ftype)))
           (inrec (make-instance 'injection?-expr
                    :id (makesym "IN?_~d" (index ex))
                    :index (index ex)
                    :type (mk-funtype cotuptype *boolean*)))
           (insubtype (make!-expr-as-type inrec)))
      (setf (type ex) (mk-funtype insubtype (range ftype))))))

;;; To keep the op from being changed by a macro in the name-expr case
(defvar *applied-operators* nil)

(defmethod set-type* :around ((ex application) expected)
  "Checks for possible macro expansions, i.e., the operator* has a
resolution with a macro matching the signature of the arguments."
  (declare (ignore expected))
  (let* ((op (operator* ex))
	 (macro? (and (name-expr? op)
		      (some #'(lambda (r)
				(and (macro-decl? (declaration r))
				     (not (eq (declaration r) (current-declaration)))))
			    (resolutions op))))
         (added? (and macro?
                      (not (memq op *applied-operators*))))
         (*applied-operators* (if added?
                                  (cons op *applied-operators*)
                                  *applied-operators*)))
    (if (and macro?
	     (not (eq *generate-tccs* 'none)))
	(let ((*generate-tccs* 'none))
	  (call-next-method))
	(call-next-method))
    ;; Note that the call-next-method could have modified ex
    (typecase ex
      (field-application
       (assert (not macro?))
       ;; Check for macro-expanded argument allowing beta reduction
       (when (from-macro (argument ex))
         ;;(cdr (assq (argument ex) (macro-expressions (current-theory))))
         (let ((orig (copy ex))
	       (nex (beta-reduce ex)))
           (unless (eq ex nex)
             (change-class ex (class-of nex))
             (copy-slots ex nex)
             (setf (from-macro ex) orig)
             ;;(push (cons orig ex) (macro-expressions (current-theory)))
             ))))
      (application
       (cond (added?
	      (let ((nop (operator* ex)))
		;; May have changed to no longer be a macro
		(when (and (name-expr? nop)
			   (macro-decl? (declaration nop))
			   (not (eq (declaration nop) (current-declaration))))
		  (assert (macro-decl? (declaration nop)))
		  (let* ((def (subst-mod-params
				  (args2 (car (last (def-axiom (declaration nop)))))
				  (module-instance nop)
				(module (declaration nop)) (declaration nop)))
			 (dappl (make!-applications def (argument* ex)))
			 (appl (beta-reduce dappl nil))
			 (orig (copy ex)))
                    (when (and (lambda-expr? def)
			       (eq *generate-tccs* 'none))
		      (check-for-application-macro-tccs (argument* ex) def ex))
                    (change-class ex (class-of appl))
		    (copy-slots ex appl)
                    (setf (from-macro ex) orig)
                    (set-extended-place ex orig
					"making lambda expr ~a from macro ~a.~a"
					ex (id (module (declaration nop)))
					(id (declaration nop)))
                    (assert (eq (from-macro ex) orig))))
		(unless (eq *generate-tccs* 'none)
		  (call-next-method))))
             ((and (accessor-name-expr? (operator ex))
                   (from-macro (argument ex)))
	      (let ((orig (copy ex))
                    (nex (beta-reduce ex)))
                (unless (eq ex nex)
                  (change-class ex (class-of nex))
                  (copy-slots ex nex)
                  (setf (from-macro ex) orig)
                  ))))))))

(defun check-for-application-macro-tccs (args def ex)
  (let ((bindings (lambda-bindings* def)))
    ;;(assert (<= (length args) (length bindings)))
    (check-for-application-macro-tccs* args bindings ex)))

(defun check-for-application-macro-tccs* (args bindings ex)
  (when (and args bindings)
    (let* ((arg (car args))
           (bnds (car bindings))
           (dom (if (cdr bnds)
                    (mk-tupletype (mapcar #'type bnds))
                    (type (car bnds))))
           (jtypes (judgement-types+ arg))
           (incs (compatible-predicates jtypes dom arg)))
      (when incs
        ;;(generate-subtype-tcc arg dom incs)
        ;; Don't generate subtype-tccs yet, wait till *tcc-conditions* is set up
        ;; See set-type* :around (expr)
        (setf (macro-subtype-tcc-args-list ex) (list arg dom incs)))
      (check-for-application-macro-tccs*
       (cdr args)
       (let ((sbindings (if (cdr bnds)
                            (pairlis bnds
                                     (if (tuple-expr? arg)
                                         (exprs arg)
                                         (make-projections arg)))
                            (acons (car bnds) arg nil))))
         (substit (cdr bindings) sbindings))
       ex))))

(defmethod set-type* :around ((ex projection-application) expected)
  (declare (ignore expected))
  (call-next-method)
  (when (from-macro (argument ex))
      ;;(cdr (assq (argument ex) (macro-expressions (current-theory))))
    (let ((orig (copy ex))
          (nex (beta-reduce ex)))
      (change-class ex (class-of nex))
      (copy-slots ex nex)
      (setf (from-macro ex) orig)
      ;;(push (cons orig ex) (macro-expressions (current-theory)))
      )))

(defmethod set-type* :around ((ex name-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  ;; Note that call-next-method can cause a conversion, so ex may no longer
  ;; be a name-expr
  (set-type-around-name-expr ex))

(defmethod set-type-around-name-expr ((ex name-expr))
  (when (and ;; Shouldn't be necessary - bug in CMU Lisp?
             #+(or cmu sbcl) (name-expr? ex)
             (macro-decl? (declaration ex))
             (not (eq (declaration ex) (current-declaration)))
             (not (memq ex *applied-operators*)))
    (let ((def (subst-mod-params (args2 (car (last (def-axiom
                                                     (declaration ex)))))
                                 (module-instance ex)
                                 (module (declaration ex))))
          (orig (copy ex)))
      (change-class ex (class-of def))
      (copy-slots ex def)
      (setf (from-macro ex) orig)
      ;;(push (cons orig ex) (macro-expressions (current-theory)))
      )))

(defmethod set-type-around-name-expr (ex)
  (declare (ignore ex))
  nil)

(defmethod set-type* :around ((ex cases-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (when (and (cases-expr? ex) ;; Conversions might have kicked in
             (from-macro (expression ex)))
      ;;(cdr (assq (expression ex) (macro-expressions (current-theory))))
    (let ((orig (copy ex))
          (nex (beta-reduce ex)))
      (unless (eq ex nex)
        (change-class ex (class-of nex))
        (copy-slots ex nex)
        (setf (from-macro ex) orig)
        ;;(push (cons orig ex) (macro-expressions (current-theory)))
        ))))

(defmethod change-name-expr-class-if-needed ((decl field-decl) expr)
  (change-class expr 'field-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-constructor-decl) expr)
  (change-class expr 'constructor-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-constructor-decl)
                                             (expr null-expr)))

(defmethod change-name-expr-class-if-needed ((decl adt-recognizer-decl) expr)
  (change-class expr 'recognizer-name-expr))

(defmethod change-name-expr-class-if-needed ((decl adt-accessor-decl) expr)
  (change-class expr 'accessor-name-expr))

(defmethod change-name-expr-class-if-needed (decl expr)
  (declare (ignore decl expr))
  nil)


(let ((boolean-alias-table nil))
  (defun boolean-aliases ()
    (or boolean-alias-table
        (let ((bth (get-theory "booleans")))
          (when bth
            (let ((bdecls (theory bth)))
              (setq boolean-alias-table
                    (list (cons (find '& bdecls :key #'id)
                                (find 'AND bdecls :key #'id))
                          (cons (find '∧ bdecls :key #'id)
                                (find 'AND bdecls :key #'id))
                          (cons (find '∨ bdecls :key #'id)
                                (find 'OR bdecls :key #'id))
                          (cons (find '=> bdecls :key #'id)
                                (find 'IMPLIES bdecls :key #'id))
                          (cons (find '⇒ bdecls :key #'id)
                                (find 'IMPLIES bdecls :key #'id))
                          (cons (find '<=> bdecls :key #'id)
                                (find 'IFF bdecls :key #'id))
                          (cons (find '⇔ bdecls :key #'id)
                                (find 'IFF bdecls :key #'id)))))))))
  (defun reset-boolean-aliases ()
    (setq boolean-alias-table nil)))

;;; The order of preference is those that are bound-variables, then the
;;; local resolutions (current theory, context, or library, in that
;;; order).  If a unique resolution is determined that way, then find the
;;; best resolution from the resolution and its associated judgements and
;;; return it.  Otherwise an ambiguity error is invoked.
        
(defun bound-variable-resolution (resolutions)
  (let ((breses (remove-if-not #'(lambda (r)
                                   (memq (declaration r) *bound-variables*))
                  resolutions)))
    (when breses
      (labels ((first-one (reses res rest-of-bvars)
                  (if (null reses)
                      res
                      (let ((nrest-of-bvars (memq (declaration (car reses))
                                                  rest-of-bvars)))
                        (first-one (cdr reses)
                                   (if nrest-of-bvars res (car reses))
                                   (if (and (cdr reses)
                                            (null nrest-of-bvars))
                                       (memq (declaration (car reses))
                                             *bound-variables*)
                                       rest-of-bvars))))))
        (first-one (cdr breses)
                   (car breses)
                   (memq (declaration (car breses))
                         *bound-variables*))))))

(defun find-best-name-resolution (ex resolutions expected)
  "Filters through the resolutions, trying to narrow it down to one."
  (if (cdr resolutions)
      (or (bound-variable-resolution resolutions)
          (let* ((mreses (when expected
                           (find-tc-matching-resolutions resolutions expected)))
                 (lreses (filter-local-resolutions (or mreses resolutions)))
		 (dreses (or (and (cdr lreses)
				  (remove-if-not #'fully-instantiated? lreses))
			     lreses))
                 (mreses (or (and (cdr dreses)
				  (remove-if #'from-datatype-modname? dreses))
			     dreses))
		 (freses (or (and (cdr mreses)
				  (remove-if-not #'instantiated-importing? mreses))
			     mreses))
		 (maxreses (or (and (cdr freses)
				    expected
				    (find-maximal-res-subtypes freses expected))
			       freses))
                 (ureses (or (and (cdr maxreses)
				  (remove-if #'instantiated-importing? maxreses))
			     maxreses))
		 (unpreses (or (and (cdr ureses)
				    (remove-if #'(lambda (res)
						   (formals (module (declaration res))))
				      ureses))
			       ureses)))
            (cond ((cdr unpreses)
                   (setf (resolutions ex) unpreses)
                   (type-ambiguity ex))
                  (t (car unpreses)))))
      (car resolutions)))

;;; Find the resolutions that are maximals subtypes of the expected type
(defun find-maximal-res-subtypes (resolutions expected)
  (let ((eqreses (remove-if
                     (complement #'(lambda (r) (tc-eq (type r) expected)))
                   resolutions)))
    (or eqreses
        (let ((subreses (or (remove-if
                                (complement #'(lambda (r)
                                                (subtype-of? (type r)
                                                             expected)))
                              resolutions)
                            resolutions)))
          (if (cdr subreses)
              (or (remove-if
                      #'(lambda (res)
                          (some #'(lambda (r)
                                    (and (not (eq r res))
                                         (subtype-of? (type res) (type r))))
                                subreses))
                    subreses)
                  subreses)
              subreses))
        resolutions)))
  

(defun instantiated-importing? (res)
  (find-if #'(lambda (x) (tc-eq x (module-instance res)))
    (get-importings (get-theory (module-instance res)))))

(defun from-datatype-modname? (res)
  (typep (find-if #'(lambda (x) (tc-eq x (module-instance res)))
           (get-importings (get-theory (module-instance res))))
         'datatype-modname))

(defun find-tc-matching-resolutions (resolutions expected &optional mreses)
  (cond ((null resolutions)
         (nreverse mreses))
        ((fully-instantiated? (car resolutions))
         (find-tc-matching-resolutions
          (cdr resolutions) expected
          (if (and (not (member (car resolutions) mreses :test #'tc-eq))
                   (compatible? (type (car resolutions)) expected))
              (cons (car resolutions) mreses)
              mreses)))
        (t (let* ((theory (get-theory (module-instance (car resolutions))))
                  (bindings (tc-match expected (type (car resolutions))
                                      (mapcar #'(lambda (x) (cons x nil))
                                              (formals-sans-usings theory))))
                  (mres (when (every #'cdr bindings)
                          (subst-mod-params (car resolutions)
                                            (car (create-compatible-modinsts
                                                  (module-instance (car resolutions))
                                                  (declaration (car resolutions))
                                                  (list bindings)
                                                  nil))
                                            theory))))
             (find-tc-matching-resolutions
              (cdr resolutions) expected
              (if (and mres
                       (not (member mres mreses :test #'tc-eq)))
                  (cons mres mreses)
                  mreses))))))

(defun kind-of-name-expr (expr)
  #+pvsdebug (assert (every #'declaration *bound-variables*))
  (or (kind expr)
      (if (or (memq (declaration expr) *bound-variables*)
              (typep (declaration expr) '(or var-decl bind-decl dep-binding)))
          'variable
          'constant)))


;;; should be split into two methods

(defun set-type-actuals-and-maps (name theory &optional decl)
  (assert theory)
  (let ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				*tccs-generated-for*)))
    (prog1 (set-type-actuals name theory decl)
      (set-type-maps name theory))))

(defmethod set-type-actuals ((thinst modname) &optional th decl)
  (assert (or (resolution thinst) th))
  (assert (or (null (dactuals thinst)) decl))
  (let* ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				 *tccs-generated-for*))
	 (tdecl (declaration thinst))
	 (theory (if tdecl
		     (if (declaration? tdecl) (module tdecl) tdecl)
		     th))
	 (acts (actuals thinst))
	 (dacts (dactuals thinst))
	 (fmls (when acts (formals-sans-usings theory)))
	 (dfmls (when decl (decl-formals decl))))
    ;;(assert (or (null th) (eq th theory)))
    (assert (or (null acts) (length= fmls acts)))
    (assert (or (null dacts) (length= dfmls dacts)))
    (when (or acts dacts)
      (set-type-actuals* (if dacts (append acts dacts) acts)
			 (if dfmls (append fmls dfmls) fmls))
      ;; Only true currently for acts whose exprs are names.
      #+badassert (assert (every #'typed? acts))
      )
    (when (or acts dacts)
      (let ((nthinst (simplify-modinst thinst)))
	(unless (eq *generate-tccs* 'none)
          (generate-assuming-tccs (lcopy nthinst :mappings nil) thinst theory))
	nthinst))))

(defmethod set-type-actuals ((expr name) &optional th adecl)
  (declare (ignore th adecl))
  #+bad-assert (assert (null *set-type-actuals-name*))
  (unless (module-instance expr)
    (type-ambiguity expr))
  (let* ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				 *tccs-generated-for*))
	 (thinst (module-instance expr))
	 (decl (declaration expr))
         (theory (if (module? decl) decl (module decl)))
         (*typecheck-using* nil)
         (*set-type-actuals-name* expr))
    (when (and (not *dont-worry-about-full-instantiations*)
               (or (some #'null (actuals thinst))
                   (some #'null (dactuals thinst))))
      (type-error expr
        "Could not determine the full theory instance for ~a~
         ~%  Theory instance: ~a" expr (full-name (module-instance expr))))
    ;; if modname was previously generated without allowing tccs
    ;; now we have to force tccs. (unless we don't want any tccs.)
    ;; see resolve.lisp for where modname-no-tccs are generated
    (let* ((*generate-tccs* (cond ((eq *generate-tccs* 'none) 'none)
                                  ((typep thinst 'modname-no-tccs) 'all)
                                  (t *generate-tccs*)))
	   (acts (actuals thinst))
	   (dacts (dactuals thinst))
	   (fmls (when acts (formals-sans-usings theory)))
	   (dfmls (when dacts (decl-formals decl))))
      (assert (length= fmls acts))
      (assert (length= dfmls dacts))
      (set-type-actuals* (if dacts (append acts dacts) acts)
			 (if dfmls (append fmls dfmls) fmls))
      (when (typep thinst 'modname-no-tccs)
	(change-class thinst 'modname))
      #+pvsdebug (assert (fully-typed? (actuals thinst)))
      #+pvsdebug (assert (fully-typed? (dactuals thinst)))
      ;; Actuals may need to be turned to dactuals
      ;; Keep in mind that the thinst has actuals/dactuals correctly set
      ;; Then the given expr actuals/dactuals need to be set-typed against the
      ;; thinst ones.
      (when (and (actuals expr)
		 (null (dactuals expr))
		 dacts)
	(setf (dactuals expr) (actuals expr))
	(setf (actuals expr) nil))
      ;; this only should occur in one of the two methods
      (when (actuals expr)
	(let ((*generate-tccs* 'none))
          (mapc #'(lambda (ea ma)
                    (cond ((type-value ma)
                           (unless (occurs-in ea ma)
                             (setf (type-value ea) (type-value ma))))
                          ((theory-name-expr? (expr ma))
                           (unless (and (theory-name-expr? (expr ea))
					(tc-eq (expr ma) (expr ea)))
                             (break "Look into this")))
                          (t (setf (type-value ea) nil)
                             (set-type* (expr ea) (type (expr ma))))))
		(actuals expr) acts)))
      #+pvsdebug (assert (fully-typed? (actuals expr)))
      (when (dactuals expr)
	(let ((*generate-tccs* 'none))
          (mapc #'(lambda (ea ma)
                    (cond ((type-value ma)
                           (setf (type-value ea) (type-value ma)))
                          ((theory-name-expr? (expr ma))
                           (unless (and (theory-name-expr? (expr ea))
					(tc-eq (expr ma) (expr ea)))
                             (break "Look into this")))
                          (t (setf (type-value ea) nil)
                             (set-type* (expr ea) (type (expr ma))))))
		(dactuals expr) dacts)))
      (let ((nthinst (simplify-modinst thinst)))
	(unless (eq nthinst thinst)
	  (when (print-type-name? expr)
	    (when (actuals expr) (setf (actuals expr) (actuals nthinst)))
	    (when (dactuals expr) (setf (dactuals expr) (dactuals nthinst))))
	  (let ((nres (copy (resolution expr) :module-instance nthinst)))
	    (setf (resolutions expr) (list nres))))
	(unless (eq *generate-tccs* 'none)
          (generate-assuming-tccs nthinst expr theory)
          ;; Compare the given actuals with those determined by the typechecker
          (when (and (eq theory (current-theory))
		     (formals-sans-usings theory))
            (if (and (actuals expr)
                     (null (actuals nthinst)))
		(check-local-actuals (actuals expr) (formals-sans-usings theory))
		(generate-actuals-tccs (actuals expr) (actuals nthinst)))))
	nthinst))))

(defmethod set-type-maps ((modinst modname) &optional theory)
  (when (mappings modinst)
    (unless (or (fully-instantiated? (actuals modinst))
                (fully-instantiated? (copy modinst 'actuals nil)))
      (type-error modinst
        "Actual parameters must be provided to include mappings"))
    (let ((nmodinst (simplify-modinst modinst))
          (thry (or theory (get-theory modinst))))
      (set-type-mappings nmodinst thry)
      ;; This is now done in typecheck-using* after doing add-to-using
      ;; (when (mappings modinst)
      ;;   (generate-mapped-axiom-tccs nmodinst))
      ;; Compare the given actuals with those determined by the typechecker 
      ;;(generate-actuals-tccs (actuals expr) (actuals nmodinst))
      nmodinst)))

(defmethod set-type-maps ((expr name) &optional theory)
  #+pvsdebug (assert (null *set-type-actuals-name*))
  (let* ((modinst (module-instance expr))
         (thry (or theory
                   (if (module? (declaration expr))
                       (declaration expr)
                       (module (declaration expr)))))
         (*typecheck-using* nil)
         (*set-type-actuals-name* expr))
    (when (mappings modinst)
      (set-type-mappings modinst thry)
      ;; this only should occur in one of the two methods
      (let ((nmodinst (simplify-modinst modinst)))
        (generate-mapped-axiom-tccs nmodinst)
        nmodinst))))

(defun check-local-actuals (actuals formals)
  (when actuals
    (let ((ex (or (type-value (car actuals)) (expr (car actuals)))))
      (unless (and (name? ex)
                   (eq (declaration ex) (car formals)))
        (type-error (car actuals)
          "May not instantiate current theory except with corresponding formals")))
    (check-local-actuals (cdr actuals) (cdr formals))))


;;; Sets the types of the actuals.

(defun set-type-actuals* (actuals formals &optional alist)
  (when actuals
    (let ((act (car actuals))
          (form (car formals)))
      (multiple-value-bind (nform nalist)
          (subst-actuals-in-next-formal act form alist)
        (set-type-actual act nform)
	;; Only true for acts whose exprs are names.
        #+badassert (assert (typed? act))
        (set-type-actuals* (cdr actuals) (cdr formals) nalist)))))

(defmethod set-type-actual :after (act formal)
  (declare (ignore formal))
  (when (expr? (expr act))
    (setf (types (expr act)) nil)))

;;; Type-value is used.

(defmethod set-type-actual (act (formal formal-type-decl))
  (unless (type-value act) 
    (type-error act "Expression provided where a type is expected"))
  (when (and (typep (expr act) 'name)
	     (zerop (parens (expr act)))
	     (resolutions (expr act))
	     ;;(actuals (expr act))
	     )
    (let ((reses (remove-if-not #'(lambda (res)
				    (typep (declaration res) '(or type-decl formal-type-decl)))
		   (resolutions (expr act)))))
      (assert (null (cdr reses)))
      (setf (resolutions (expr act)) reses)
      (when (name-expr? (expr act))
	(setf (type (expr act)) (type (car reses))))
      (let* ((*generate-tccs* (if (actuals (expr act)) *generate-tccs* 'none))
	     (rth (module (declaration (car reses))))
	     (thinst (set-type-actuals-and-maps (expr act) rth)))
	(setf (module-instance (resolution (expr act))) thinst)
	(set-type* (type-value act) nil))
      #+pvsdebug (assert (fully-typed? (actuals (expr act))))))
  ;; Note that (expr act) now has its actuals and maps set,
  ;; but that (type-value act) and act itself may still not be fully typed.
  ;;(assert (fully-typed? (type-value act)))
  (when (nonempty-type-decl? formal)
    ;; May need to generate existence TCC
    (unless (eq *generate-tccs* 'none)
      (check-nonempty-type (type-value act) (current-declaration)))))

(defmethod set-type-actual (act (formal formal-subtype-decl))
  (call-next-method)
  ;;(assert (fully-typed? (type-value act)))
  ;;(assert (fully-instantiated? (type-value act)))
  (let* ((tact (type-value act))
         (texp (type-value formal))
         (vid (make-new-variable '|x| act))
         (vb (mk-bind-decl vid tact tact))
         (*bound-variables* (cons vb *bound-variables*))
         (*tcc-conditions* (cons vb *tcc-conditions*))
         (svar (mk-name-expr vid nil nil
                             (make-resolution vb (current-theory-name) tact))))
    (when (place act)
      (set-extended-place svar act
			  "variable ~a as proxy for actual ~a"
			  svar act))
    (check-for-subtype-tcc svar (supertype texp))))

(defmethod set-type-actual (act (formal formal-struct-subtype-decl))
  (call-next-method)
  (let ((tact (find-supertype (type-value act)))
        (texp (type-value formal)))
    (when (recordtype? tact)
      (dolist (fld (fields texp))
        (unless (member (id fld) (fields tact) :key #'id)
          (type-error fld
            "Field ~a missing for structural subtype ~a"
            (id fld) (type-value act)))))))
        

(defmethod set-type-actual (act (formal formal-const-decl))
  #+pvsdebug (assert (and (type formal (fully-typed? (type formal)))))
  (unless (and (typep (expr act) 'expr)
               (or (type (expr act))
                   (setf (types (expr act))
                         (delete-if-not #'type-expr? (ptypes (expr act))))))
    (type-error act
      "Type provided where an expression is expected"))
  (set-type* (expr act) (type formal))
  (setf (type-value act) nil)
  #+pvsdebug (assert (fully-instantiated? (type formal)))
  #+pvsdebug (assert (fully-instantiated? (type (expr act))))
  #+pvsdebug (assert (fully-instantiated? (resolution (expr act))))
  #+pvsdebug (assert (fully-typed? (expr act))))

(defmethod set-type-actual (act (formal formal-theory-decl))
  (unless (name-expr? (expr act))
    (type-error act "Theory name expected here"))
  (setf (type-value act) nil)
  (let ((threses (remove-if-not
                     #'(lambda (r)
                         (typep (declaration r)
                                '(or module mod-decl formal-theory-decl
                                     theory-abbreviation-decl)))
                   (resolutions (expr act)))))
    (cond ((cdr threses)
           (type-error (expr act)
             "Theory name ~a is ambiguous" (expr act)))
          ((null threses)
           (type-error (expr act)
             "No resolution for theory name ~a" (expr act)))
          (t (setf (resolutions (expr act)) threses)
             (change-class (expr act) 'theory-name-expr)))
    (unless (eq (target-mapped-theory (declaration act))
                (declaration (theory-name formal)))
      (type-error act "Theory name should be (an alias of) ~a"
                  (id (declaration (theory-name formal)))))
    (set-type-actuals-and-maps (expr act) (declaration (theory-name formal)))))

(defun set-type-mappings (thinst theory)
  (when (mappings thinst)
    (let* ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				   *tccs-generated-for*))
	   (athinst (if (recursive-type? theory)
                        (typecheck* (copy thinst
                                      :id (id (adt-theory theory))
                                      :resolutions nil)
                                    nil 'module nil)
                        thinst))
           (cthinst (copy athinst :mappings nil)))
      (unless (fully-instantiated? cthinst)
        (type-error thinst
          "Actual parameters must be provided to include mappings"))
      (set-type-lhs-mappings (mappings thinst) cthinst)
      (let* ((smappings (sort-mappings (mappings thinst)))
             (entry (get-importings theory))
             (there? (member thinst entry :test #'tc-eq))
             (*current-context* (if there?
                                    (copy-context *current-context*)
                                    *current-context*)))
        (set-type-mappings* smappings cthinst)))))

(defun sort-mappings (mappings &optional sorted)
  ;; Need to sort theory mappings first, then declarations of top level
  (if (null mappings)
      (nreverse sorted)
      (let ((next (or (find-if
                          #'(lambda (map)
                              (and (module? (declaration (lhs map)))
                                   (not (some #'(lambda (m)
                                                  (and (not (eq m map))
                                                       (module? (declaration
                                                                 (lhs map)))
                                                       (theory-map-depends-on
                                                        map m)))
                                              mappings))))
                        mappings)
                      (find-if #'(lambda (map)
                                   (not (some #'(lambda (m)
                                                  (and (not (eq m map))
                                                       (map-depends-on map m)))
                                              mappings)))
                        mappings))))
        (assert next () "No next mapping?")
        (assert (or (module? (declaration (lhs next)))
                    (not (some #'(lambda (m) (module? (declaration (lhs m))))
                               mappings)))
                () "Should have chosen a theory mapping")
        (sort-mappings (remove next mappings) (cons next sorted)))))
        

;;; Returns t if theory of map1 depends on theory of map2
(defun theory-map-depends-on (map1 map2)
  (let ((th1 (declaration (lhs map1)))
        (th2 (declaration (lhs map2))))
    (cond ((from-prelude? th1)
           (and (from-prelude? th2)
                (memq th1 (memq th2 *prelude-theories*))
                t))
          ((from-prelude? th2)
           t)
          ((from-prelude-library? th1)
           (and (from-prelude-library? th2)
                (memq th2 (all-importings th1))
                t))
          ((from-prelude-library? th2)
           t)
          (t (memq th2 (all-importings th1))
             t))))

;;; Returns t if declaration of map1 depends on declaration of map2
(defun map-depends-on (map1 map2)
  (let ((d1 (declaration (lhs map1))))
    ;; uninterpreted type declarations can't possibly depend on anything
    (unless (typep d1 '(or type-decl datatype-or-module mod-decl
			theory-abbreviation-decl))
      (let ((d2 (declaration (lhs map2)))
            (refs (collect-references (type d1))))
        (and (memq d2 refs) t)))))
    

(defun set-type-mappings* (mappings thinst &optional previous-mappings)
  (when mappings
    (let ((map (car mappings)))
      (set-type-mapping map (lcopy thinst :mappings previous-mappings))
      (let ((smaps (cdr mappings)))
        (set-type-mappings* smaps thinst
                            (nconc previous-mappings (list map)))))))

(defun set-type-lhs-mappings (mappings thinst)
  (set-type-lhs-mappings* mappings thinst)
  (check-mapping-completeness mappings thinst))

(defun set-type-lhs-mappings* (mappings thinst)
  (when mappings
    (set-type-lhs-mapping (car mappings) thinst)
    (set-type-lhs-mappings* (cdr mappings) thinst)))

(defun set-type-lhs-mapping (map thinst)
  (let ((lhs (lhs map))
        (rhs (rhs map))
        (type (type map)))
    (assert (resolutions lhs))
    ;;(assert (ptypes (expr rhs)))
    ;; :Picks the best resolution
    (determine-best-mapping-lhs lhs rhs type thinst)
    (when (memq (id (module-instance (resolution lhs)))
                '(equalities notequal))
      (type-error lhs "May not map ~a" lhs))
    (when (mapping-lhs? lhs)
      (setf (dactuals lhs) (mk-dactuals (decl-formals lhs)))
      (with-current-decl lhs
	(let ((stype (subst-mod-params (type (resolution lhs))
			 (copy thinst :dactuals (dactuals lhs))
		       (module (declaration lhs)) (declaration lhs))))
	  (setf (type lhs) stype))))))

(defun determine-best-mapping-lhs (lhs rhs type thinst)
  (unless (singleton? (resolutions lhs))
    (let ((reses (determine-best-mapping-lhs-resolutions
                  (resolutions lhs) rhs type thinst)))
      (assert reses () "Determine-best-mapping-lhs failed")
      (setf (resolutions lhs) reses)
      (unless (singleton? reses)
        (type-ambiguity lhs)))))

(defun determine-best-mapping-lhs-resolutions (lhs-reses rhs type thinst)
  (let ((best (remove-if-not #'(lambda (r)
				 (determine-best-mapping-lhs-resolutions* r rhs type thinst))
		lhs-reses)))
    (filter-local-resolutions best)))

(defun determine-best-mapping-lhs-resolutions* (lhs-res rhs type thinst)
  (case (kind-of (declaration lhs-res))
    (type t)
    (module (and (name-expr? (expr rhs))
                 (not (type-value rhs))
                 (some #'(lambda (r) (eq (kind-of (declaration r)) 'module))
                       (resolutions (expr rhs)))))
    (expr (let ((rtype (subst-mod-params (type lhs-res)
                           thinst (module (declaration lhs-res))
                           (declaration lhs-res))))
            (if type
                (compatible? rtype type)
                (some #'(lambda (pty) (compatible? rtype pty))
                      (ptypes (expr rhs))))))
    (t (break "Strange kind-of"))))

(defun check-mapping-completeness (mappings thinst)
  ;; Check if the lhs is a complete set - this means if a constant is
  ;; mapped, all its uninterpreted types must be mapped
  (let* ((mth (get-theory thinst))
         (mapped-theories
          (cons mth
                (mapcan #'(lambda (m)
                            (when (module? (declaration (lhs m)))
                              (list (declaration (lhs m)))))
                  mappings))))
    (dolist (map mappings)
      (when (const-decl? (declaration (lhs map)))
        (let* ((lrefs (collect-references (type (resolution (lhs map)))))
               (rrefs (collect-references (ptypes (expr (rhs map)))))
               (unint (find-if #'(lambda (r)
                                   (and (memq (module r) mapped-theories)
                                        (not (generated-by r))
                                        (interpretable? r)
                                        (and (not (has-mapping? r mappings))
                                             (not (memq r rrefs)))))
                        lrefs)))
          (when unint
            (type-error (lhs map)
              "~a has a type that references ~a, which has not been mapped"
              (lhs map) (id unint))))))))

;; Checks to see if the obj has dependencies that are not properly mapped
;; (defun safe-mappings? (obj theory theoryname)
;;   (or (null (mappings theoryname))
;;       (let ((imdecls (invalid-mapping-decls theory theoryname))
;;          (refs (collect-references obj)))
;;      (some #'(lambda (r) (memq r imdecls)) refs))))


;;; Use the mappings of the theory and theoryname to determine which
;;; declarations of the given theory will cause problems for
;;; subst-mod-params.  For example,
;;;  T: TYPE
;;;  y, z: T
;;;  nz: TYPE = {x: T | x /= z}
;;;  ny: TYPE = {x: T | x /= y}
;;; If T and z are mapped (say to int and 0), then references to ny have
;;; problems since it is unclear what y maps to.

;; (defun invalid-mapping-references (theory theoryname)
;;   (when (mappings theoryname)
;;     (let ((decls (all-decls theory))
;;        (unmapped-constants nil)
;;        (unmapped-types nil)
;;        (invalid-refs nil))
;;       (dolist (decl decls)
;;      (typecase decl
;;        ((or formal-decl var-decl) nil)
;;        (const-decl (if (interpretable? decl)
;;                        (unless (member decl (mappings theoryname)
;;                                        :key #'(lambda (m)
;;                                                 (declaration (lhs m))))
;;                          (push decl unmapped-constants))
;;                        ())))))))
                            

(defun has-mapping? (decl mappings)
  (or (member decl mappings :key #'(lambda (m) (declaration (lhs m))))
      (let* ((dth (module decl))
             (tmap (find-if #'(lambda (m) (eq (declaration (lhs m)) dth))
                     mappings)))
        (when tmap
          (has-mapping? decl (mappings (expr (rhs tmap))))))))
        
  
(defmethod set-type-mapping ((map mapping) thinst)
  (let* ((lhs (lhs map))
         (rhs (rhs map))
	 (cthinst (lcopy thinst
		    :dactuals (when (mapping-lhs? lhs) (dactuals lhs)))))
    (assert (resolution lhs))
    ;;(assert (ptypes (expr rhs)))
    ;;(determine-best-mapping-lhs lhs rhs)
    (if (mapping-lhs? lhs)
	;; Temporarily reset the context to the lhs
        (let* ((ctn (current-theory-name))
	       (cdecl (current-declaration)))
	  (assert (dactuals lhs))
          (setf (current-theory-name) cthinst
		(current-declaration) lhs)
          (unwind-protect
               (set-type-mapping-rhs lhs rhs cthinst)
            (setf (current-theory-name) ctn
		  (current-declaration) cdecl))
	  (assert (every #'(lambda (fp)
			     (or (not (decl-formal? fp))
				 (memq fp (decl-formals lhs))))
			 (free-params rhs))))
        (set-type-mapping-rhs lhs rhs cthinst))))

(defmethod set-type-mapping ((map mapping-rename) thinst)
  (with-slots (lhs rhs) map
    (let ((ldecl (declaration lhs))
          (rdecl (declaration (or (type-value rhs) (expr rhs)))))
      (assert rdecl)
      (assert (name-eq (id rdecl) (id (expr rhs))))
      ;; For renames, the new declarations have already been set in the rhs,
      ;; but they are place holders - we still need to do the substitutions
      (typecase ldecl
        (type-decl
         (let ((subst-type (subst-mod-params (type-value rdecl) thinst (module ldecl))))
           ;;(assert (fully-instantiated? subst-type))
           (setf (type-value (rhs map)) subst-type)))
        (const-decl
         (let ((subst-type (subst-mod-params (type rdecl) thinst (module ldecl))))
           (setf (declared-type rdecl) (or (print-type subst-type) subst-type)
                 (type rdecl) subst-type)
           (setf (type (expr rhs)) subst-type)))
        (t ;; Theory-decl
         (let ((nmodname (subst-mod-params (modname rdecl) thinst (module ldecl))))
           (setf (modname rdecl) nmodname)))))))

(defun set-type-mapping-rhs (lhs rhs thinst)
  (set-type-mapping-rhs* (declaration lhs) lhs rhs thinst))

(defmethod set-type-mapping-rhs* ((ldecl type-decl) lhs rhs thinst)
  (declare (ignore lhs thinst))
  (unless (type-value rhs)
    (typecheck* rhs nil 'type nil))
  (if (type-value rhs)
      (set-type* (type-value rhs) nil)
      (type-error (expr rhs) "Type expected here"))
  (untypecheck* (expr rhs)))

(defmethod set-type-mapping-rhs* ((ldecl module) lhs rhs thinst)
  (set-type-mapping-rhs*-theoryref (id ldecl) lhs rhs thinst))

(defmethod set-type-mapping-rhs* ((ldecl theory-reference) lhs rhs thinst)
  (set-type-mapping-rhs*-theoryref (id (theory-name ldecl)) lhs rhs thinst))

(defun set-type-mapping-rhs*-theoryref (thid lhs rhs thinst)
  (declare (ignore lhs thinst))
  (let ((threses (remove-if
                     (complement
                      #'(lambda (r)
                          (and (typep (declaration r)
                                      '(or module theory-reference)))))
                   (resolutions (expr rhs)))))
    (cond ((cdr threses)
           (type-error (expr rhs)
             "Theory name ~a is ambiguous" (expr rhs)))
          ((null threses)
           (type-error (expr rhs)
             "~a is not an instance of ~a"
             (expr rhs) thid))
          (t (change-class (expr rhs) 'theory-name-expr)
             (setf (resolutions (expr rhs)) threses)
             (when (actuals (expr rhs))
               (set-type-actuals-and-maps (change-class (copy (expr rhs)) 'modname)
                                          (declaration (car threses))))
             (when (mappings (expr rhs))
               (set-type-mappings (name-to-modname (expr rhs))
                                  (declaration (expr rhs))))))))
  
(defmethod set-type-mapping-rhs* ((ldecl const-decl) (lhs name) rhs thinst)
  "Basically runs set-type on the rhs using the substituted type from the lhs.
The first arguement is just a method discriminator."
  (assert (eq ldecl (declaration lhs)))
  (setf (type-value rhs) nil)
  (unless (resolution thinst)
    (setf (resolutions thinst)
	  (list (mk-resolution (module ldecl) thinst nil))))
  (let ((stype (subst-mod-params (type (resolution lhs)) thinst (module ldecl) ldecl)))
    ;;(assert (compatible? (type (expr rhs)) stype))
    (set-type* (expr rhs) stype)))

(defmethod set-type-mapping-rhs* ((ldecl const-decl) (lhs mapping-lhs) rhs thinst)
  "Basically runs set-type on the rhs, using the substituted form of the
type of the lhs."
  ;; Note: lhs has dformals, but these are not the same as the dformals
  ;; of the (declaration lhs), The rhs references the former only.
  (assert (eq ldecl (declaration lhs)))
  (setf (type-value rhs) nil)
  (assert (every #'(lambda (fp)
		     (or (not (decl-formal? fp))
			 (memq fp (decl-formals lhs))))
		 (free-params (type lhs))))
  (let ((stype (subst-mod-params (type lhs) thinst (module ldecl) lhs)))
    (set-type* (expr rhs) stype))
  (assert (every #'(lambda (fp)
		     (or (not (decl-formal? fp))
			 (memq fp (decl-formals lhs))))
		 (free-params (expr rhs)))))

(defmethod target-mapped-theory ((decl module))
  decl)

(defmethod target-mapped-theory ((decl mod-decl))
  (target-mapped-theory (declaration (theory-name decl))))

(defmethod target-mapped-theory ((decl formal-theory-decl))
  (target-mapped-theory (declaration (theory-name decl))))

(defmethod target-mapped-theory ((decl theory-abbreviation-decl))
  (target-mapped-theory (declaration (theory-name decl))))

(defun possible-mapping-subst-types (types stype &optional stypes)
  (if (null types)
      (nreverse stypes)
      (let* ((bindings (tc-match stype (car types) 
                                 (mapcar #'list (formals-not-in-context stype)))))
        (possible-mapping-subst-types
         (cdr types) stype
         (if (and bindings
                  (every #'cdr bindings))
             (cons (subst-theory-params stype bindings) stypes)
             stypes)))))

(defun subst-mod-params-all-mappings (type)
  (let* ((refs (collect-references type))
         (theories (delete-duplicates (mapcar #'module refs)))
         (map-insts (get-unique-imported-mapping-instances theories)))
    (subst-mod-params-all-mappings* type map-insts)))

(defun subst-mod-params-all-mappings* (type map-insts)
  (if (null map-insts)
      type
      (subst-mod-params-all-mappings*
       (subst-mod-params type (cdar map-insts) (caar map-insts))
       (cdr map-insts))))

(defun get-unique-imported-mapping-instances (theories &optional thinsts)
  (if (null theories)
      thinsts
      (let ((insts (get-importings (car theories))))
        (get-unique-imported-mapping-instances
         (cdr theories)
         (append (mapcar #'(lambda (i) (cons (car theories) i))
                   (remove-if (complement #'mappings) insts))
                 thinsts)))))

(defun merge-mappings (mapped-name thname)
  (if (mappings thname)
      (progn (when (actuals thname) (break "Actuals in merge-mappings"))
      (merge-mappings* (mappings mapped-name) (mappings thname) mapped-name))
      mapped-name))

(defun merge-mappings* (maps1 maps2 thname)
  (if (null maps2)
      (copy thname :mappings maps1)
      (merge-mappings*
       (append (remove (car maps2) maps1
                       :test #'(lambda (m1 m2) (tc-eq (lhs m1) (lhs m2))))
               (list (car maps2)))
       (cdr maps2)
       thname)))

;;; theory-ref takes a modname and returns the expanded modname, where the
;;; modname may refer to a theory-decl, formal-theory-parameter, etc.  These
;;; have their own library, actuals, and mappings, and need to be taken into
;;; account.

(defmethod theory-ref ((thname modname))
  thname)

(defmethod theory-ref ((d mod-decl))
  (modname d))

(defmethod theory-ref ((d formal-theory-decl))
  (theory-name d))

(defmethod theory-ref ((d name-expr))
  (assert (resolution d))
  (let ((decl (declaration (resolution d))))
    (typecase decl
      (mod-decl (theory-ref (modname decl)))
      (theory-abbreviation-decl (theory-ref (theory-name decl)))
      (t (theory-ref (module-instance (resolution d)))))))


(defvar *simplify-actuals* t)

(defun simplify-modinst (modinst)
  (if (and *simplify-actuals*
           (or (actuals modinst) (dactuals modinst)))
      (let ((nactuals (simplify-actuals (actuals modinst)))
            (dactuals (simplify-actuals (dactuals modinst))))
        (if (and (equal nactuals (actuals modinst))
                 (equal dactuals (dactuals modinst)))
            modinst
	    (let* ((nmodinst (copy modinst :actuals nactuals :dactuals dactuals))
		   (nres (when (resolution modinst)
			   (copy (resolution modinst) :module-instance nmodinst))))
	      (setf (resolutions nmodinst) (list nres))
	      nmodinst)))
      modinst))

(defun simplify-actuals (actuals &optional result)
  (if (null actuals)
      (nreverse result)
      (simplify-actuals
       (cdr actuals)
       (cons (if (or (type-value (car actuals))
                     (and (name-expr? (expr (car actuals)))
                          (typep (declaration (car actuals))
                                 '(or module mod-decl formal-theory-decl
                                      theory-abbreviation-decl))))
                 (car actuals)
                 (let ((nexpr (pseudo-normalize (expr (car actuals)))))
                   (if (tc-eq nexpr (expr (car actuals)))
                       (car actuals)
                       (lcopy (car actuals)
                         :expr nexpr))))
             result))))
  


;;; subst-actuals-in-next-formal takes an actual, a corresponding formal,
;;; and an alist mapping formals to actuals and returns two values:
;;;   1. A substituted form of the formal
;;;   2. The new alist      

(defun subst-actuals-in-next-formal (act formal alist)
  (if (formal-theory-decl? formal)
      (values formal (append (make-mapping-alist act formal) alist))
      (let* ((pred (get-actual-subtype-predicate act formal alist))
             (nalist (if pred
                         (let ((fpred (find-if #'const-decl?
                                        (generated formal))))
                           (assert fpred)
                           (acons fpred pred alist))
                         alist))
             (nform (if (and alist (dependent? formal))
                        (subst-acts-in-form formal nalist)
                        formal)))
        (when (and (typep formal 'formal-subtype-decl)
                   (not (eq nform formal)))
          (setf (print-type (type-value nform)) nil))
        (values nform (acons nform act nalist)))))

(defun make-mapping-alist (act formal)
  (let* ((mdecl (declaration (resolution (expr act))))
         (fmappings (mapcar #'(lambda (m)
                                (if (actual? (cdr m))
                                    (cons (declaration (expr (cdr m))) (car m))
				    (cons (cdr m) (car m))))
                      (theory-mappings formal)))
         (tmappings (typecase mdecl
                      (theory-abbreviation-decl (mapping mdecl))
                      ((or formal-theory-decl mod-decl)
                       (theory-mappings mdecl))
                      (module nil)
                      (t (break "what now?"))))
         (amappings (let ((*subst-mod-params-map-bindings* nil))
                      (mapcar #'(lambda (mb)
                                  (cons (car mb)
                                        (or (type-value (cdr mb))
                                            (expr (cdr mb)))))
                        (make-subst-mod-params-map-bindings
                         (expr act) (mappings (expr act)) nil)))))
    ;;(compose-formal-to-actual-mapping
    ;; fmappings
    ;; (append amappings tmappings))
    (compose-mappings fmappings
                      (if amappings
                          (compose-mappings tmappings amappings)
                          tmappings))))

(defmethod mapping ((mdecl theory-abbreviation-decl))
  (get-interpreted-mapping
   (get-theory (theory-name mdecl)) nil (theory-name mdecl)))

;;; fmappings is the formal mappings from the original theory of the formal
;;; theory parameter to the theory created for the formal parameter.
;;; amappings is similar, but maps the same original theory to the
;;; corresponding actual theory.  The result of this is the mapping from the
;;; theory created for the formal parameter to the actual theory, appended
;;; to amappings.  Thus when the result is used in substitution, everything
;;; is mapped to the theory correcponding to the actual.
;; (defun compose-formal-to-actual-mapping (fmappings amappings
;;                                                 &optional famappings)
;;   (if (null fmappings)
;;       famappings
;;       ;; This assertion is too strong - see test2 in bugs/760
;;       ;;(assert (every #'(lambda (am) (assq (car am) fmappings)) amappings))
;;       (let* ((fmap (car fmappings))
;;           (amap (assq (car fmap) amappings)))
;;      (compose-formal-to-actual-mapping
;;       (cdr fmappings)
;;       amappings
;;       (if amap
;;           (acons (cdr fmap)
;;                  (if (mapping-rhs? (cdr amap))
;;                      (or (type-value (cdr amap))
;;                          (expr (cdr amap)))
;;                      (cdr amap))
;;                  famappings)
;;           fmappings)))))

(defmethod get-actual-subtype-predicate (act (formal formal-subtype-decl) alist)
  (let ((tact (type-value act)))
    (if tact
        (let ((tform (subst-acts-in-form (supertype (type-value formal))
                                         alist)))
          (if (compatible? tact tform)
              (let* ((*generate-tccs* 'none))
                (or (subtype-pred tact tform)
                    (gen-true-lambda tform)))
              (type-incompatible act (list tact) tform)))
        (type-error act
          "Expression provided where a type is expected"))))

(defmethod get-actual-subtype-predicate (act formal alist)
  (declare (ignore act formal alist))
  nil)

(defun subst-acts-in-form (formal alist)
  (gensubst formal
    #'(lambda (ex) (subst-acts-in-form! ex alist))
    #'(lambda (ex) (subst-acts-in-form? ex alist))))

(defmethod subst-acts-in-form? ((ex name) alist)
  (assoc (declaration ex) alist :test #'subst-formal-eq))

(defmethod subst-acts-in-form? (ex alist)
  (declare (ignore ex alist))
  nil)

(defmethod subst-acts-in-form! ((ex type-name) alist)
  (let ((act (cdr (assoc (declaration ex) alist :test #'subst-formal-eq))))
    (if (typep act 'actual)
        (or (type-value act)
            (expr act))
        act)))

(defmethod subst-acts-in-form! ((ex name-expr) alist)
  (let ((act (cdr (assoc (declaration ex) alist :test #'subst-formal-eq))))
    (if (typep act 'actual)
        (expr act)
        act)))

(defmethod subst-acts-in-form! ((ex actual) alist)
  (let ((act (cdr (assoc (declaration (expr ex)) alist
                         :test #'subst-formal-eq))))
    act))

(defun subst-formal-eq (formal1 formal2)
  (or (and (eq (class-of formal1) (class-of formal2))
           (same-id formal1 formal2)
           (eq (module formal1) (module formal2)))
      (and (formal-theory-decl? (generated-by formal2))
           (eq (cdr (assq formal1 (theory-mappings (generated-by formal2))))
               formal2))))

(defun gen-true-lambda (type)
  (let* ((id '|x|)
         (bd (typecheck* (mk-bind-decl id type type) nil nil nil))
         ;;(nvar (mk-name-expr id nil nil (make-resolution bd nil type)))
         )
    (make-lambda-expr (list bd) *true*)))


(defun generate-actuals-tccs (acts macts)
  (when acts
    (let ((act (lcopy (car acts)
                 :expr (if (type-value (car acts))
                           (expr (car acts))
                           (pseudo-normalize (expr (car acts)))))))
      (unless (tc-eq act (car macts))
        (generate-actuals-tcc act (car macts))))
    (generate-actuals-tccs (cdr acts) (cdr macts))))      


;;; Setting the type for number-exprs is complicated by the fact the there may
;;; now be judgements given on numbers.  If there is a single ptype, then
;;; there were no judgements.  If nat is a subtype of the expected type, then
;;; the judgements were unnecessary.  Otherwise we look for the "best"
;;; matching judgement type, and use that.

(defmethod set-type* ((ex rational-expr) expected)
  (unless (compatible? expected *number*)
    (type-incompatible ex (ptypes ex) expected))
  (setf (type ex) (get-expr-number-type (number ex))))

(defun check-for-subtype-tcc (ex expected)
  #+pvsdebug (assert (fully-instantiated? expected))
  (assert (or *checking-conversions*
              (typep (current-declaration) 'conversion-decl)
              (skolem-constant? ex)
              (fully-instantiated? ex)))
  #+pvsdebug (assert (fully-typed? ex))
  (unless (subtype-of? (type ex) expected)
    (let ((*tccs-generated-for* (when (boundp '*tccs-generated-for*)
				  *tccs-generated-for*))
	  (type (type ex)))
      (or (and (not (strict-compatible? type expected))
               (not (some #'(lambda (jty) (subtype-of? jty expected))
                          (judgement-types+ ex)))
               (find-funtype-conversion type expected ex))
          (unless (or (eq *generate-tccs* 'none)
		      (member (list ex expected) *tccs-generated-for*
			      :test #'(lambda (x y)
					(and (eq (car x) (car y))
					     (tc-eq (cadr x) (cadr y))))))
	    (push (list ex expected) *tccs-generated-for*)
            (let* ((jtypes (let ((*generate-tccs* 'none))
                             (judgement-types+ ex)))
                   (incs (let ((*generate-tccs* 'none))
                           (compatible-predicates jtypes expected ex))))
              (when incs
		(generate-subtype-tcc ex expected incs)
		  ;; A better warning is generated in judgements.lisp
                  ;; (when (assq ex *compatible-pred-reason*)
                  ;;   (pvs-warning "No TCC generated for judgement ~a~%~
                  ;;                 judgement is already known"
                  ;;     (ref-to-id (current-declaration))))
		)))))))

;;; Loop through the types (the judgement-types of the ex). Each of these is
;;; a minimal type, we want the one that leads to the most provable TCC.
;;; Since this is hard, we simply keep the first one we hit, unless one of
;;; the others returns nil.
;;; When types is nil (recursively), and we are in the prover, then we remove
;;; the incs that are in the current sequent.  Otherwise we simply return them.

(defun compatible-predicates (types expected ex &optional incs)
  (if (null types)
      (if *ps*
          (multiple-value-bind (ics there?)
              (gethash ex (typepred-hash *ps*))
            (if there?
                (remove-if #'(lambda (inc) (member inc ics :test #'tc-eq))
                  incs)
                (let ((ics (collect-implicit-type-constraints (list ex) *ps* nil t)))
                  (when (null (freevars ics))
                    (setf (gethash ex (typepred-hash *ps*)) ics))
                  (remove-if #'(lambda (inc) (member inc ics :test #'tc-eq))
                    incs))))
          (or incs
              (progn (add-tcc-comment 'subtype ex expected 'in-context)
                     nil)))
      (let* ((npreds (compatible-preds (car types) expected ex))
             (nincs (remove-if #'(lambda (x)
                                   (member x *tcc-conditions* :test #'tc-eq))
                      npreds)))
	(when nincs
	  (compatible-predicates
	   (cdr types) expected ex
	   (or incs nincs))))))

(defun uappend (list1 list2 &key (test #'eq))
  ;; returns the append of list1 and list2, without duplicates
  ;; (assuming no dups in list1)
  (cond ((null list2)
         list1)
        ((member (car list2) list1 :test test)
         (uappend list1 (cdr list2)))
        (t (uappend* (cons (car list2) (reverse list1)) (cdr list2) test))))

(defun uappend* (rlist1 list2 test)
  (if (null list2)
      (nreverse rlist1)
      (uappend* (if (member (car list2) rlist1 :test test)
                    rlist1
                    (cons (car list2) rlist1))
                (cdr list2)
                test)))


(defmethod set-type* ((expr tuple-expr) expected)
  (let ((stype (find-supertype expected))
        (exprs (exprs expr)))
    (unless (typep stype '(or tupletype struct-sub-tupletype))
      (type-incompatible expr (ptypes expr) expected))
    (let ((types (types stype)))
      (unless (= (length exprs) (length types))
        (type-error expr "Wrong arity: expect ~d subexpressions"
                    (length types)))
      (set-tup-types exprs types)
      (setf (type expr) (mk-tupletype (mapcar #'type exprs))))))

(defun set-tup-types (exprs etypes)
  (when exprs
    (let* ((dep? (typep (car etypes) 'dep-binding))
           (type (if dep?
                     (type (car etypes))
                     (car etypes))))
      (set-type* (car exprs) type)
      (set-tup-types (cdr exprs)
                     (if dep?
                         (substit (cdr etypes)
                           (acons (car etypes) (car exprs) nil))
                         (cdr etypes))))))


(defmethod set-type* ((expr cases-expr) expected)
  (set-type* (expression expr) (car (ptypes (expression expr))))
  (let* ((atype (find-declared-adt-supertype (type (expression expr))))
         (adt (when (adt? atype) (adt atype))))
    (set-type-selections (selections expr) expr atype expected)
    (if (else-part expr)
        (let ((*tcc-conditions* (push-tcc-conditions
                                 (make-else-cases-conditions expr)
                                 *tcc-conditions*)))
          (set-type* (else-part expr) expected))
        (if adt
            (generate-selections-tccs expr (constructors adt) adt)
            (generate-coselections-tccs expr))))
  (let* ((comptypes (if (cdr (ptypes expr))
			(remove-if #'(lambda (ty)
				       (not (compatible? ty expected)))
			  (ptypes expr))
			(ptypes expr)))
	 (fitypes (if (cdr comptypes)
		      (or (remove-if #'(lambda (ty)
					 (not (fully-instantiated? ty)))
			    comptypes)
			  comptypes)
		      comptypes))
	 (ctypes (if (cdr fitypes)
		     (let ((ctype (reduce #'compatible-type fitypes)))
		       (if ctype (list ctype) fitypes))
		     fitypes)))
    (cond ((singleton? ctypes)
	   (setf (type expr)
		 (if (fully-instantiated? (car ctypes))
		     (car ctypes)
		     (instantiate-from (car ctypes) expected expr))))
	  ((null ctypes)
	   (type-incompatible expr (ptypes expr) expected))
	  (t (type-ambiguity expr)))
    (assert (type expr))))

(defun set-type-selections (selections expr atype expected)
  (when selections
    (let ((sel (car selections)))
      (setf (resolutions (constructor sel))
            (set-sel-constr (constructor sel) (args sel) atype))
      (if (resolution (constructor sel))
          (let ((ctype (type (resolution (constructor sel)))))
            (assert (fully-instantiated? ctype))
            (setf (type (constructor sel)) ctype))
          (unless (injection-expr? (constructor sel))
            (type-ambiguity (constructor sel))))
      (unless (injection-expr? (constructor sel))
        (change-class (constructor sel) 'constructor-name-expr)
	(setf (constructor sel)
	      (subst-mod-params (constructor sel) (module-instance atype)
		(module (declaration atype))
		(declaration (constructor sel)))))
      (assert (fully-instantiated? (constructor sel)))
      (let* ((equality (make-selection-equality sel expr))
             (*bound-variables* (append (args sel) *bound-variables*))
             (*tcc-conditions* (cons equality
                                     (append (reverse (args sel))
                                             *tcc-conditions*))))
        (set-type* (expression sel) expected)))
    (set-type-selections (cdr selections) expr atype expected)))

(defun make-selection-equality (sel expr)
  (assert (fully-instantiated? (constructor sel)))
  (assert (fully-instantiated? (args sel)))
  (assert (fully-instantiated? (expression expr)))
  (assert (or *adt* (place sel)))
  (assert (or *adt* (place expr)))
  (let* ((*generate-tccs* 'none)
	 (rhs (typecheck*
	       (if (args sel)
		   (if (injection-expr? (constructor sel))
		       (let* ((arg (if (cdr (args sel))
				       (let ((tupex
					      (make-instance 'arg-tuple-expr
						:exprs (mapcar
							   #'(lambda (a)
							       (let ((nex (mk-name-expr a)))
								 (assert (or *adt* (place a)))
								 (unless *adt*
								   (set-extended-place
								    nex a
								    "creating arg ~a for selection ~a"
								    a sel))
								 nex))
							 (args sel)))))
					 (unless *adt*
					   (set-extended-place tupex sel "creating tuple for selection ~a" sel))
					 tupex)
				       (let ((nex (mk-name-expr (car (args sel)))))
					 (assert (or *adt* (place (car (args sel)))))
					 (unless *adt*
					   (set-extended-place nex (car (args sel))
							       "creating name for selection arg ~a" (car (args sel))))
					 nex)))
			      (injapp (make-instance 'injection-application
					:id (id (constructor sel))
					:index (index (constructor sel))
					:argument arg)))
			 (unless *adt*
			   (set-extended-place injapp sel
					       "creating injection application from selection ~a" sel))
			 injapp)
		       (let ((appl (mk-application* (copy (constructor sel))
				     (mapcar #'(lambda (a)
						 (assert (or *adt* (place a)))
						 (let ((nex (mk-name-expr a)))
						   (unless *adt*
						     (set-extended-place nex a
									 "creating name for selection arg ~a" a))
						   nex))
				       (args sel)))))
			 (unless *adt*
			   (set-extended-place appl sel
					       "creating application from selection ~a" sel))
			 appl))
		   (copy (constructor sel)))
	       (find-supertype (type (expression expr))) nil nil))
	 (eqn (make-equation (expression expr) rhs)))
    (unless *adt*
      (set-extended-place eqn expr
			  "creating equation for selection ~a" sel))
    eqn))

(defun make-else-cases-conditions (expr)
  (typecase (find-supertype (type (expression expr)))
    (cotupletype
     (mapcar #'(lambda (s)
                 (let ((inj? (make!-injection?-application
                              (index s) (expression expr))))
                   (make!-negation inj?)))
       (selections expr)))
    (t
     (mapcar #'(lambda (s)
                 (let* ((type (type (constructor s)))
                        (rtype (if (and (funtype? type)
                                        (adt (supertype (range type))))
                                   (range type)
                                   type))
                        (id (if (typep (predicate rtype) 'coercion)
                                (id (args1 (predicate rtype)))
                                (id (predicate rtype))))
			(ex (typecheck* (mk-application 'NOT
					  (mk-application id
					    (expression expr)))
					*boolean* nil nil)))
		   (set-extended-place ex s
				       "creating else case for selection ~a"
				       (constructor s))
		   ex))
       (selections expr)))))


(defun set-sel-constr (constructor args type)
  (assert (fully-instantiated? type))
  (instantiate-sel-resolutions type constructor args)
  (unless (injection-expr? constructor)
    (if (cdr (resolutions constructor))
        (let* ((reses (resolutions constructor))
               (rtypes (mapcar #'(lambda (r)
                                   (if args
                                       (range (type r))
                                       (type r)))
                         reses)))
          (multiple-value-bind (reses1 types1)
              (remove-nonconstructors-and-incompatibles reses rtypes type)
            (if (cdr reses1)
                (multiple-value-bind (reses2 types2)
                    (remove-unequal-resolution-types reses1 types1 type)
                  (if (cdr reses2)
                      (let ((reses3 (remove-smaller-adt-resolution-types
                                     reses2 types2)))
                        (if (cdr reses3)
                            (or (remove-if-not #'fully-instantiated? reses3)
                                reses3)
                            reses3))
                      reses2))
                reses1)))
        (progn (assert (fully-instantiated? (type (resolution constructor))))
               (resolutions constructor)))))

(defmethod instantiate-sel-resolutions (type constr args)
  (let ((reses (instantiate-sel-resolutions* (resolutions constr) type args)))
    (assert reses)
    (unless (equal reses (resolutions constr))
      (setf (resolutions constr) reses)
      (setf (types constr) (mapcar #'type reses)))))

(defun instantiate-sel-resolutions* (reses type args &optional ireses)
  (if (null reses)
      (nreverse ireses)
      (let ((res (instantiate-sel-resolution (car reses) type args)))
        (instantiate-sel-resolutions*
         (cdr reses) type args
         (if (and res
                  (not (member res ireses :test #'tc-eq)))
             (cons res ireses)
             ireses)))))

(defun instantiate-sel-resolution (res type args)
  (when (symbolp (adt type))
    ;; May happen after restoring from bin files
    (restore-adt-slot type))
  (let* ((constr (find (id (declaration res)) (constructors (adt type))
                       :key #'id))
         ;;(args? (arguments constr))
         )
    (when (if (arguments constr)
              args
              (null args))
      (if (fully-instantiated? res)
          res
          (multiple-value-bind (rtype thinsts)
              (find-parameter-instantiation (if args
                                                (range (type res))
                                                (type res))
                                            type)
            (when (and rtype
                       (singleton? thinsts)
                       (eq (id (car thinsts)) (id (module (declaration res))))
                       (compatible? rtype type))
              (make-resolution (declaration res) (car thinsts))))))))

(defmethod instantiate-sel-resolutions ((type cotupletype) constr args)
  (unless (injection-expr? constr)
    (type-error constr "IN_# expression expected here"))
  (unless (<= (index constr) (length (types type)) )
    (type-error constr "Index too large"))
  (let ((intype (nth (1- (index constr)) (types type)))
        (arg (if (cdr args)
                 (make-instance 'arg-tuple-expr
                   'exprs args)
                 (car args))))
    (when (cdr args)
      (break "Multiple args in instantiate-sel-resolutions"))
    (set-type* arg intype)))

(defun remove-nonconstructors-and-incompatibles (reses rtypes type)
  (multiple-value-bind (nreses ntypes)
      (remove-nonconstructors-and-incompatibles* reses rtypes type)
    (if nreses
        (values nreses ntypes)
        (values reses rtypes))))

(defun remove-nonconstructors-and-incompatibles* (reses rtypes type
                                                        &optional nreses ntypes)
  (if (null reses)
      (values (nreverse nreses) (nreverse ntypes))
      (let ((keep? (and (typep (declaration (car reses)) 'adt-constructor-decl)
                        (compatible? type (car rtypes)))))
        (remove-nonconstructors-and-incompatibles*
         (cdr reses) (cdr rtypes) type
         (if keep? (cons (car reses) nreses) nreses)
         (if keep? (cons (car rtypes) ntypes) ntypes)))))

(defun remove-unequal-resolution-types (reses rtypes type)
  (multiple-value-bind (nreses ntypes)
      (remove-unequal-resolution-types* reses rtypes type)
    (if nreses
        (values nreses ntypes)
        (values reses rtypes))))

(defun remove-unequal-resolution-types* (reses rtypes type
                                               &optional nreses ntypes)
  (if (null reses)
      (values (nreverse nreses) (nreverse ntypes))
      (let ((keep? (tc-eq (find-declared-adt-supertype (car rtypes)) type)))
        (remove-unequal-resolution-types*
         (cdr reses) (cdr rtypes) type
         (if keep? (cons (car reses) nreses) nreses)
         (if keep? (cons (car rtypes) ntypes) ntypes)))))
        
(defun remove-smaller-adt-resolution-types (reses rtypes)
  (or (remove-smaller-adt-resolution-types* reses rtypes nil nil)
      reses))

(defun remove-smaller-adt-resolution-types* (reses types nreses ntypes)
  (if (null reses)
      (nreverse nreses)
      (let ((remove? (or (some #'(lambda (ty)
                                   (subtype-of? (car types) ty))
                               (cdr types))
                         (some #'(lambda (ty)
                                   (subtype-of? (car types) ty))
                               ntypes))))
        (remove-smaller-adt-resolution-types*
         (cdr reses)
         (cdr types)
         (if remove? nreses (cons (car reses) nreses))
         (if remove? ntypes (cons (car types) ntypes))))))



(defmethod set-type* ((ex projection-application) expected)
  (let ((ptypes (remove-if-not
                    #'(lambda (pty)
                        (let ((types (types (find-supertype pty))))
                          (compatible? (nth (1- (index ex)) types)
                                       expected)))
                  (ptypes (argument ex)))))
    (cond ((null ptypes)
           (type-incompatible
            ex
            (projection-application-types (ptypes (argument ex)) ex)
            expected))
          ((cdr ptypes)
           (cond ((typep (argument ex) 'name-expr)
                  (let ((reses (filter-local-resolutions
                                (resolutions (argument ex)))))
                    (cond ((cdr reses)
                           (setf (resolutions (argument ex)) reses)
                           (setf (types (argument ex))
                                 (remove-duplicates (mapcar #'type reses)
                                   :test #'tc-eq))
                           (type-ambiguity (argument ex)))
                          (reses
                           (setf (resolutions (argument ex)) reses)
                           (setf (types (argument ex))
                                 (list (type (car reses))))
                           (set-type* (argument ex) (type (car reses)))
                           (setf (types ex)
                                 (list (projection-application-type
                                        ex (type (car reses))))))
                          (t (setf (types (argument ex)) ptypes)
                             (type-ambiguity (argument ex))))))
                 (t (setf (types (argument ex)) ptypes)
                    (type-ambiguity (argument ex)))))
          (t (set-type* (argument ex) (car ptypes))
             (setf (type ex)
                   (projection-application-type
                    ex (type (argument ex))))))))

(defmethod set-type* ((ex injection-application) expected)
  (let ((cotuptype (find-supertype expected)))
    (unless (cotupletype? cotuptype)
      (type-error ex "Cotuple not expected here"))
    (let ((intype (nth (1- (index ex)) (types cotuptype))))
      (set-type* (argument ex) intype))
    (let* ((inrec (make-instance 'injection?-expr
                    :id (makesym "IN?_~d" (index ex))
                    :index (index ex)
                    :type (mk-funtype cotuptype *boolean*)))
           (insubtype (make!-expr-as-type inrec)))
      (setf (type ex) insubtype))))

(defmethod set-type* ((ex injection?-application) expected)
  (unless (compatible? expected *boolean*)
    (type-error ex "Boolean not expected here"))
  (unless (singleton? (ptypes (argument ex)))
    (type-ambiguity (argument ex)))
  (unless (fully-instantiated? (car (ptypes (argument ex))))
    (type-error (argument ex)
      "Could not determine the full theory instance for ~a" (argument ex)))
  (set-type* (argument ex) (car (ptypes (argument ex))))
  (setf (type ex) *boolean*))

(defmethod set-type* ((ex extraction-application) expected)
  (unless (singleton? (ptypes (argument ex)))
    (type-ambiguity (argument ex)))
  (let ((cotuptype (find-supertype (car (ptypes (argument ex))))))
    (assert (cotupletype? cotuptype))
    (let* ((inrec (make-instance 'injection?-expr
                    :id (makesym "IN?_~d" (index ex))
                    :index (index ex)
                    :type (mk-funtype cotuptype *boolean*)))
           (insubtype (make!-expr-as-type inrec)))
      (set-type* (argument ex) insubtype))
    (let ((intype (nth (1- (index ex)) (types cotuptype))))
      (unless (compatible? intype expected)
        (type-incompatible ex (list intype) expected))
      (setf (type ex) intype))))

(defmethod set-type* ((expr field-application) expected)
  (unless (typed? expr)
    (let ((ptypes (remove-if-not
		      #'(lambda (pty)
			  (let ((fields (fields (find-supertype pty))))
			    (compatible? (type (find expr fields
						     :test #'same-id))
					 expected)))
		    (ptypes (argument expr)))))
      (cond ((null ptypes)
             (type-incompatible
              expr
              (mapcar #'(lambda (pty)
			  (type (find expr (fields (find-supertype pty))
				      :test #'same-id)))
		(ptypes (argument expr)))
              expected))
            ((and (cdr ptypes)
                  ;;(not (every #'(lambda (x) (compatible? x (car ptypes))) (cdr ptypes)))
                  )
             (setf (types (argument expr)) ptypes)
             (type-ambiguity (argument expr)))
            (t (set-type* (argument expr) (car ptypes))
               (set-expr-type expr expected))))))


;;; Applications

(defmethod set-type* ((ex application) expected)
  (with-slots (operator argument type) ex
    (let* ((types (ptypes ex))
           (ptypes1 (remove-if-not #'(lambda (ty) (tc-eq ty expected)) types))
           (ptypes (or ptypes1
                       (remove-if-not #'(lambda (ty) (compatible? ty expected))
                         types)))
           (ftypes (or (remove-if-not #'fully-instantiated? ptypes) ptypes)))
      (if (null ftypes)
          (type-incompatible ex types expected)
          (let ((optype (determine-operator-type operator argument expected ex)))
            ;;(assert (type-expr? optype))
            (if (and (typep argument 'tuple-expr)
                     (boolean-binop-type? optype)
                     (boolean-op? operator '(AND & ∧ IMPLIES => ⇒ OR ∨ WHEN)))
                (set-type-boolop-application ex (car ftypes))
                (set-type-nonboolop-application ex expected optype)))))
    #+pvsdebug (assert (fully-instantiated? ex))
    (check-for-recursive-tcc ex)))

(defvar *set-type-recursive-operator* nil)

;;; This is one of the more complicated set-type functions, as it involves
;;; and intricate interplay between the argument and the operator types,
;;; each being used to refine the other.  In addition, there is special
;;; handling of recursive types, and LET and WHERE expressions.
(defun set-type-nonboolop-application (ex expected foptype)
  #+pvsdebug (assert (fully-instantiated? foptype))
  (let ((operator (operator ex))
        (argument (argument ex)))
    ;; Set the argument type first.  Generate TCCs later as we need
    ;; to allow conversions to kick in on the operator.
    (let ((*generate-tccs* 'none))
      (set-type* argument (domain foptype)))
    (let ((optype (refine-optype-from-expected foptype ex expected)))
      (assert optype)
      (reset-let-bindings-from-arguments ex optype)
      (setf (type ex) (application-range-type argument optype))
      (let ((*generate-tccs* 'none))
        ;; Conversions may be added here and this could reset actuals, we
        ;; check this by comparing the optype to the (type operator)
        (assert (or (not (lambda-expr? operator))
                    (not (dep-binding? (domain optype)))
                    (subsetp (remove-if-not #'(lambda (x)
                                                (dep-binding? (declaration x)))
                               (freevars expected))
                             (freevars (range optype))
                             :test #'same-declaration)))
        (set-type* operator (if (lambda-expr? operator)
                                (mk-funtype (domain optype) expected)
                                optype))
        (unless (tc-eq (type operator) optype)
          (setf (type ex) (application-range-type argument (type operator)))))
      ;; Now generate the argument TCCs, unless in a recursive defn
      (if (def-decl? (current-declaration))
          ;; refine the arg type if the optype has changed
          ;; Still can't generate TCCs, until after
          ;; check-set-type-recursive-operator
          (unless (eq (domain foptype) (domain optype))
            (let ((*generate-tccs* 'none))
              (set-type* argument (domain optype))))
          (set-type* argument (domain optype)))
      ;; On to the operator
      (let* ((*appl-tcc-conditions*
              (cons (appl-tcc-conditions operator argument)
                    *appl-tcc-conditions*))
             (*set-type-recursive-operator*
              (check-set-type-recursive-operator ex)))
        (cond ((lambda-expr? ex)
               ;; Was changed by the recursion conversion
               (set-type* ex (type ex)))
              (t (if (let-expr? ex) ;; includes where-expr
                     (set-type-let-expr-body ex optype expected)
                     (set-type* (operator ex) (type (operator ex))))
                 (when (def-decl? (current-declaration))
                   (set-type* argument (domain (type (operator ex))))))))
      ;; Deal with recursive functions here.  This is not simple, because we
      ;; need to recognize when we have enough arguments to deal with the
      ;; recursion, or to add a conversion if there will never be enough
      ;; args.  Curried functions make this slightly more tricky, e.g., in
      ;; f(x)(y,z)(w) where the measure is on y, we want the recursion to
      ;; kick in at f(x)(y, z), and not at f(x) where a conversion would be
      ;; applied (which of course, we do want if that is all the args).  So
      ;; we set/check a global variable that is a stack of the operators we
      ;; are working on - need a stack for situations like
      ;; f(x)(f(y),z)(f(w)), where the argument is now being worked on.
      (cond ((and (typep operator 'field-name-expr)
                  (not (memq (declaration operator) *bound-variables*))
                  (typep (find-supertype (domain optype)) 'recordtype))
             (change-class ex 'field-application)
             (setf (id ex) (id operator)))
            (t (change-application-class-if-needed
                ex))))))

(defun check-set-type-recursive-operator (ex)
  ;; No need to do anything if not in a recursive decl
  (when (and (not (eq *generate-tccs* 'none))
             (typep (current-declaration) '(and def-decl (not adt-def-decl))))
    (let ((op* (operator* ex)))
      (if (and (name-expr? op*)
               (eq (declaration op*) (current-declaration)))
          (if (memq op* *set-type-recursive-operator*)
              ;; Already there, just return
              *set-type-recursive-operator*
              (let ((arglen (length (argument* ex)))
                    (depth (measure-depth (current-declaration))))
                (if (> arglen depth)
                    (if (= arglen (1+ depth))
                        (cons op* *set-type-recursive-operator*)
                        *set-type-recursive-operator*)
                    ;; Don't have enough args - need conversion
                    (let* ((cex (copy ex))
                           (recconv (recursive-def-conversion
                                     (current-declaration)))
                           (nex (make!-applications recconv (arguments* ex))))
                      ;; Add conversion
                      (assert (lambda-expr? nex))
                      (change-class ex 'recursive-defn-conversion)
                      (setf (bindings ex) (bindings nex)
                            (expression ex) (expression nex)
                            (type ex) (type nex)
                            (from-expr ex) cex)
                      (add-conversion-info nex cex ex)
                      (setq *added-recursive-def-conversion* t)
                      (push ex *added-recursive-defn-conversions*)
                      *set-type-recursive-operator*))))
          *set-type-recursive-operator*))))

(defun recursive-def-conversion (recdecl)
  (let* ((res (make-resolution recdecl
                (current-theory-name) (recursive-signature recdecl)))
         (name (make!-name-expr (id recdecl) nil nil res))
         (depth (measure-depth recdecl)))
    (set-extended-place name recdecl
			"making name for recursive defn conversion")
    (recursive-def-conversion* name (recursive-signature recdecl) depth)))

(defun recursive-def-conversion* (ex recsig depth)
  (assert (or (= depth -1) (funtype? recsig)))
  (if (= depth -1)
      ex
      (let* ((dtype (dep-binding-type (domain recsig)))
             (id (make-new-variable '|x| (cons ex dtype)))
             (bd (typecheck* (mk-bind-decl id dtype) nil nil nil))
             (var (mk-name-expr id nil nil (make-resolution bd nil dtype)))
	     (appl (make!-application ex var)))
	(set-extended-place var ex
			    "making name for recursive defn conversion")
	(set-extended-place appl ex
			    "making application ~a for recursive defn conversion" appl)
	(let* ((recdef (recursive-def-conversion* appl (range recsig) (1- depth)))
	       (lex (make!-lambda-expr (list bd) recdef)))
          (set-extended-place lex ex
			      "making lambda expression for recursive defn conversion")
	  lex))))


;; Here we set the operator type for LET and WHERE expressions.  These
;; are actually applications of lambda expressions to arguments, e.g.,
;; LET x = e IN f(x) \equiv (LAMBDA x: f(x))(e).  The problem is that the
;; optype is used instead of the expected, leading to TCCs on the entire
;; LET expression, rather than targeting the body.  The following
;; short-circuits this, applying set-type directly to the body.  We must
;; make certain to set *bound-variables* and *tcc-conditions* as if we
;; went through the setting of the LAMBDA expression.  The argument TCCs
;; are already handled above; so the only thing left is the bindings,
;; which can only generate TCCs through their types.
(defun set-type-let-expr-body (ex optype expected)
  (let ((*bound-variables* (append (let-bindings* ex) *bound-variables*))
        (*tcc-conditions* (append (let-tcc-conditions* ex) *tcc-conditions*))
        (lex (let-expression* ex))
        (dadt (find-declared-adt-supertype expected)))
    (assert (every #'(lambda (x) (or (not (consp x))
                                     (and (bind-decl? (car x))
                                          (memq (car x) *tcc-conditions*))))
                   *tcc-conditions*))
    ;; The original pass used the lambda type to reset the datatype
    ;; actuals, we now adjust it using the expected type.
    (when (and (declaration? (current-declaration))
               (application? lex)
               (name-expr? (operator lex))
               ;;(constructor-name-expr? (operator lex))
               (null (actuals (operator lex)))
               (actuals (module-instance (operator lex)))
               (typep dadt '(or adt-type-name datatype-subtype))
               (not (subtype-of? (type lex) dadt)))
      (let ((noptype (adjust-application-operator-from-adt-expected
                      lex optype expected)))
        (when noptype
          (setf (type lex) (application-range-type (argument lex) noptype)))))
    (set-type* lex expected)))


;; LET and WHERE expressions get adjusted according to the best guess from
;; the judgement types of the arguments.  Note that where-expr is a subclass
;; of let-expr, so no method is needed for it.
(defmethod reset-let-bindings-from-arguments ((ex let-expr) optype)
  (with-slots (operator argument) ex
    (let ((nbindings (set-let-bindings (bindings operator) argument)))
      (when nbindings
        (reset-let-bound-operator-type (bindings operator) optype)
        (reset-let-bound-name-exprs (expression operator) nbindings)))))

(defmethod reset-let-bindings-from-arguments (ex optype)
  (declare (ignore ex optype))
  nil)

(defun refine-optype-from-expected (optype ex expected)
  ;; The optype may need to be refined because of judgement types now
  ;; available for the argument, or to push a parameterized adt expected
  ;; on to an operator.
  (let ((op (operator ex))
        (arg (argument ex)))
    (if (and (declaration? (current-declaration))
             (name-expr? op)
             (null (actuals op))
             (actuals (module-instance op)))
        ;; For now, we prefer the adt type over the judgement type.  The
        ;; other direction doesn't work (bug # 981), but it's possible we
        ;; need to do both and choose the best.
        (let ((aoptype (adjust-application-operator-from-adt-expected
                        ex optype expected)))
          (or aoptype
              (let ((joptype
                     (adjust-application-operator-from-arg-judgements
                      ex expected)))
                (or joptype optype))))
        (let ((op* (operator* ex)))
          (if (and (declaration? (current-declaration))
                   (name-expr? op*)
                   (some #'recursive-defn-conversion? (argument* ex)))
              (determine-operator-type op arg expected ex)
              optype)))))

(defun set-type-boolop-application (ex optype)
  (setf (type ex) optype)
  (let* ((reses (delete-if-not
                    #'(lambda (r)
                        (and (eq (id (module-instance r)) '|booleans|)
                             (not (lambda-conversion-resolution? r))))
                  (resolutions (operator ex))))
         (alias (cdr (assq (declaration (car reses)) (boolean-aliases)))))
    (setf (resolutions (operator ex)) reses)
    (when alias (setf (declaration (car reses)) alias)))
  (set-conditional-arg-types (id (operator ex)) (args1 ex) (args2 ex))
  (setf (type (operator ex)) (type (resolution (operator ex))))
  (setf (types (operator ex)) nil)
  (setf (type (argument ex)) (mk-tupletype (list *boolean* *boolean*)))
  (setf (types (argument ex)) nil)
  (setf (type ex) *boolean*)
  (change-to-propositional-class ex)
  ;;(check-for-subtype-tcc ex expected)
  )

(defmethod let-expression* ((ex let-expr))
  ;;(let-expression* (expression (operator ex)))
  (expression (operator ex)))

(defmethod let-expression* ((ex expr))
  ex)

(defmethod let-bindings* ((ex let-expr))
;;   (append (bindings (operator ex))
;;        (let-bindings* (expression (operator ex))))
  (bindings (operator ex)))

(defmethod let-bindings* ((ex expr))
  nil)

(defmethod let-tcc-conditions* ((ex let-expr) &optional conds)
;;   (let-tcc-conditions* (expression (operator ex))
;;                     (append (let-tcc-condition-bindings
;;                              (bindings (operator ex)) (argument ex))
;;                             conds))
  (declare (ignore conds))
  (let-tcc-condition-bindings (bindings (operator ex)) (argument ex)))

(defmethod let-tcc-conditions* ((ex expr) &optional conds)
  (assert conds)
  conds)

(defun let-tcc-condition-bindings (bindings argument)
  (if (cdr bindings)
      (let ((name-exprs (mapcar #'(lambda (bd)
				    (make!-name-expr (id bd) nil nil
						     (make-resolution bd nil (type bd))))
			  bindings)))
	(if (tuple-expr? argument)
	    (let* ((eqns (mapcar #'make!-equation name-exprs (exprs argument)))
		   (conj (make!-conjunction* eqns)))
	      (cons conj (reverse bindings)))
	    (let* ((projs (make-projections argument))
		   (eqns (mapcar #'make!-equation name-exprs projs))
		   (conj (make!-conjunction* eqns)))
	      (cons conj (reverse bindings)))
	    ;; (let* ((tupex (make-tuple-expr name-exprs))
	    ;; 	   (eqn (make!-equation tupex argument)))
	    ;;   (cons eqn (reverse bindings)))
	    ))
      (let* ((bd (car bindings))
	     (name-expr (make!-name-expr (id bd) nil nil (make-resolution bd nil (type bd))))
	     (eqn (make!-equation name-expr argument)))
	(cons eqn bindings))))

;;; Similar to pairlis, but ensures results are in the same order
(defun opairlis (keys data &optional result)
  (assert (= (length keys) (length data)))
  (if (null keys)
      (nreverse result)
      (opairlis (cdr keys) (cdr data)
                (acons (car keys) (car data) result))))


;;; This is useful, for example, when given an expected of list[nat],
;;; ll: list[nat], and x: int, the form cons(x,ll) will normally be set
;;; to cons[int](x,ll) and generate the TCC every(nat)(cons(x,ll)).
;;; With the adjustment, the cons is reset to cons[nat](x,ll), and the
;;; TCC is simply nat(x).
(defun adjust-application-operator-from-adt-expected (ex optype expected)
  (let ((dadt (find-declared-adt-supertype expected))
        (op (operator ex))
        (arg (argument ex)))
    (when (and (typep dadt '(or adt-type-name datatype-subtype))
               (not (subtype-of? (application-range-type arg optype) dadt)))
      (let* ((eacts (actuals (module-instance dadt)))
             (res (let ((*resolve-error-info* nil))
                    (resolve* (copy op :resolutions nil :type nil :actuals eacts)
                              'expr (arguments ex)))))
        (when (and (singleton? res)
                   (not (tc-eq (car res) (resolution op))))
          (if (type op)
              (setf (type op) (type (car res)))
              (setf (types op) (list (type (car res)))))
          (setf (resolutions op) res)
          (determine-operator-type op arg expected ex))))))


;;; Adjust the operator based on the judgement-types of the argument.
;;; Does this by calling resolve, which takes judgements into account.
;;; If a different result is obtained, resets the operator type and
;;; resolution, and returns an updated optype.
(defun adjust-application-operator-from-arg-judgements (ex expected)
  (let* ((operator (operator ex))
         (argument (argument ex))
         (res (remove-if (complement
                          #'(lambda (r)
                              (compatible? (range (type r)) expected)))
                (resolve (copy operator :resolutions nil :type nil)
                         'expr (arguments ex)))))
    (when (and (singleton? res)
               (or (fully-instantiated? res)
                   (not (fully-instantiated? (resolution operator))))
               (not (tc-eq (car res)
                           (resolution operator))))
      ;; May need to rerun tc-match to get
      ;; proper actuals
      (setf (types operator)
            (list (type (car res))))
      (when (type operator)
        (setf (type operator)
              (type (car res))))
      (setf (resolutions operator) res)
      (determine-operator-type operator argument expected ex))))

(defun check-for-recursive-tcc (ex)
  (when (and (not *in-checker*)
             (not *in-evaluator*)
             (not *generating-adt*)
             (not (eq *generate-tccs* 'none))
             (typep (current-declaration) 'def-decl))
    (multiple-value-bind (name arguments)
        (rec-call-of-depth ex nil
                           (measure-depth (declaration *current-context*)))
      (when name
        (generate-recursive-tcc name arguments ex)))))

(defun set-let-bindings (bindings arg)
  (let ((reset nil))
    (if (cdr bindings)
        (if (tuple-expr? arg)
            (mapc #'(lambda (bd a)
                      (unless (declared-type bd)
                        (let ((btype (car (judgement-types+ a))))
                          (unless (tc-eq btype (type bd))
                            (setf (type bd) btype)
                            (push bd reset)))))
                  bindings (exprs arg))
            (let ((atype (car (judgement-types+ arg))))
              (unless (eq atype (type arg))
                (mapc #'(lambda (bd aty)
                          (unless (or (declared-type bd)
                                      (tc-eq (type bd) aty))
                            (setf (type bd) aty)
                            (push bd reset)))
                      bindings (types (find-supertype atype))))))
        (let ((bd (car bindings)))
          (unless (declared-type bd)
            (let ((btype (car (judgement-types+ arg))))
              (unless (tc-eq (type bd) btype)
                (setf (type bd) btype)
                (push bd reset))))))
    reset))

(defun reset-let-bound-operator-type (bindings optype)
  (if (dep-binding? (domain optype))
      (let ((dep (domain optype)))
        (cond ((cdr bindings)
               (setf (type dep) (mk-tupletype (mapcar #'type bindings)))
               (when (declared-type dep)
                 (setf (declared-type dep)
                       (mk-tupletype (mapcar #'(lambda (bd)
                                                 (or (declared-type bd)
                                                     (type bd)))
                                       bindings)))))
              (t (setf (type dep) (type (car bindings)))
                 (setf (declared-type dep)
                       (or (declared-type (car bindings))
                           (type (car bindings))))))
        (reset-let-bound-name-exprs (range optype) (list dep)))
      (setf (domain optype)
            (make-domain-type-from-bindings bindings))))


(defun reset-let-bound-name-exprs (expr nbindings)
  (mapobject #'(lambda (ex)
                 (when (name? ex)
                   (dolist (res (resolutions ex))
                     (when (memq (declaration res) nbindings)
                       (setf (type res) (type (declaration res)))
                       (when (and (name-expr? ex)
                                  (type ex))
                         (setf (type ex) (type (car (resolutions ex)))))))))
             expr))

(defun change-application-class-if-needed (ex)
  (let ((operator (operator ex))
        (argument (argument ex)))
    (cond ((and (typep operator 'name-expr)
                (boolean-op? operator
                             '(NOT ¬ AND & ∧ OR ∨ IMPLIES => ⇒ IFF <=> ⇔ WHEN)))
           (change-to-propositional-class ex))
          ((and (typep operator 'name-expr)
                (eq (id operator) '=)
                (eq (id (module-instance operator)) '|equalities|))
           (change-to-propositional-class ex))
          ((and (typep operator 'name-expr)
                (memq (id operator) '(/= ≠))
                (eq (id (module-instance operator)) '|notequal|))
           (change-to-propositional-class ex))
          ((and (typep operator 'name-expr)
                (eq (id operator) 'IF)
                (eq (id (module-instance operator)) '|if_def|)
                (not (typep ex 'branch)))
           (change-class ex 'branch)
           (unless (tuple-expr? (argument ex))
             (setf (argument ex)
                   (make!-projected-arg-tuple-expr*
                    (make-projections argument)))))
	  ((and (typep operator 'name-expr)
                (eq (id operator) '|bv|)
                (eq (id (module-instance operator)) '|bv_arithmetic_defs|)
		(actuals (module-instance operator))
		(typep (expr (car (actuals (module-instance operator)))) 'number-expr)
		(typep argument 'number-expr))
	   (if (typep ex 'implicit-conversion)
	       (change-class ex 'bitvector-conversion)
	       (change-class ex 'bitvector)))
          ((ground-arith-simplifiable? operator argument)
           (let ((num (apply (id operator)
                        (if (tuple-expr? argument)
                            (mapcar #'number (exprs argument))
                            (list (number argument))))))
             (change-expr-number-class ex num)))
	  ((and (name-expr? operator)
		(memq (id operator) '(+ -))
		(resolution operator)
		(eq (id (module-instance operator)) '|number_fields|)
		(number-expr? argument))
	   (let ((num (if (eq (id operator) '-)
			  (- (number argument))
			  (number argument))))
	     (change-class ex 'int-expr
	       :number num
	       :type (get-expr-number-type num))))
          ((typep operator 'injection-expr)
           (change-class ex 'injection-application
                         :index (index operator)
                         :id (id operator)))
          ((typep operator 'injection?-expr)
           (change-class ex 'injection?-application
                         :index (index operator)
                         :id (id operator))))))

(defun change-expr-number-class (ex num)
  (let ((class (get-expr-number-class ex num)))
    (assert class)
    (change-class ex class)
    (setf (number ex) num
          (type ex) (get-expr-number-type num))))

(defun get-expr-number-type (num)
  (if (integerp num)
      (if (null *int64*) ;; Must be loading the prelude
	  (or *integer* *rational* *real* *number_field*)
	  (if (minusp num)
	      (cond ((>= num -128) *int8*)
		    ((>= num -32768) *int16*)
		    ((>= num -2147483648) *int32*)
		    ((>= num -9223372036854775808) *int64*)
		    (t *integer*))
	      (cond ((< num 128) *uint8*)
		    ((< num 32768) *uint16*)
		    ((< num 2147483648) *uint32*)
		    ((< num 9223372036854775808) *uint64*)
		    (t *integer*))))
      (or *rational* *real* *number_field*)))

(defun get-expr-number-class (ex num)
  (if (integerp num)
      (if (decimal-integer? ex)
          'floatp
          (if (minusp num)
              'int-expr
              'number-expr))
      (if (decimal? ex)
          'floatp-expr
          (if (tuple-expr? (argument ex))
              (if (and (cond ((floatp-expr? (args1 ex))
                              (or (floatp-expr? (args2 ex))
                                  (number-expr? (args2 ex))))
                             ((floatp-expr? (args2 ex))
                              (number-expr? (args1 ex))))
                       (float-representable? num))
                  'floatp-expr
                  'rational-expr)
              (if (and (floatp-expr? (args1 ex))
                       (float-representable? num))
                  'floatp-expr
                  'rational-expr)))))

;;; A rational number is representable if its decimal fraction does not
;;; repeat, e.g., 1/8 is representable, but 1/7 is not.  This basically
;;; means the denominator is of the form 2^n * 5^m
(defun float-representable? (num)
  (or (integerp num)
      (decimal-factors (denominator num))))
    

(defmethod change-to-propositional-class ((ex propositional-application))
  nil)

(defmethod change-to-propositional-class ((ex equation))
  nil)

(defmethod change-to-propositional-class ((ex disequation))
  nil)

(defmethod change-to-propositional-class ((ex branch))
  nil)

(defmethod change-to-propositional-class ((ex unary-application))
  (case (id (operator ex))
    ((NOT ¬) (change-class ex 'unary-negation))))

(defmethod change-to-propositional-class ((ex infix-application))
  (change-to-infix-propositional-class ex))

(defun change-to-infix-propositional-class (ex)
  (case (id (operator ex))
    ((AND & ∧) (change-class ex 'infix-conjunction))
    ((OR ∨) (change-class ex 'infix-disjunction))
    ((IMPLIES => ⇒) (change-class ex 'infix-implication))
    ((IFF <=> ⇔) (change-class ex 'infix-iff))
    (WHEN (let ((op (operator ex)))
            (change-class ex 'infix-when-expr
              'operator (mk-implies-operator))
            (setf (place (operator ex)) (place op))
            (setf (exprs (argument ex)) (reverse (exprs (argument ex))))))
    (= (if (compatible? (type (args1 ex)) *boolean*)
           (change-class ex 'infix-boolean-equation)
           (change-class ex 'infix-equation)))
    ((/= ≠) (change-class ex 'infix-disequation)))
  (unless (tuple-expr? (argument ex))
    (setf (argument ex)
          (make!-projected-arg-tuple-expr* (make-projections (argument ex))))))

(defmethod change-to-propositional-class ((ex application))
  (change-to-appl-propositional-class ex))

(defun change-to-appl-propositional-class (ex)
  (case (id (operator ex))
    ((NOT ¬) (change-class ex 'negation))
    ((AND & ∧) (change-class ex 'conjunction))
    ((OR ∨) (change-class ex 'disjunction))
    ((IMPLIES => ⇒) (change-class ex 'implication))
    ((IFF <=> ⇔) (change-class ex 'iff))
    (WHEN (let ((place (place (operator ex))))
            (change-class ex 'when-expr)
            (setf (operator ex) (mk-implies-operator))
            (setf (place (operator ex)) place))
          (if (tuple-expr? (argument ex))
              (setf (exprs (argument ex)) (reverse (exprs (argument ex))))
              (setf (argument ex)
                    (make!-projected-arg-tuple-expr*
                     (reverse (make-projections (argument ex)))))))
    (= (if (compatible? (type (args1 ex)) *boolean*)
           (change-class ex 'boolean-equation)
           (change-class ex 'equation)))
    ((/= ≠) (change-class ex 'disequation)))
  (when (and (not (tuple-expr? (argument ex)))
             (not (memq (id (operator ex)) '(NOT ¬))))
    (setf (argument ex)
          (make!-projected-arg-tuple-expr* (make-projections (argument ex))))))

(defmethod change-to-propositional-class ((ex else-condition))
  ex)
    

(defmethod appl-tcc-conditions ((op lambda-expr) argument)
  (with-slots (bindings) op
    (let-tcc-condition-bindings bindings argument)))

(defmethod appl-tcc-conditions (op argument)
  (declare (ignore op argument))
  nil)

(defun and!+ (ex)
  (let ((*generate-tccs* 'none))
    (and+ ex)))

(defun set-conditional-arg-types (op arg1 arg2)
  (case op
    ((AND & ∧ IMPLIES => ⇒)
     (set-type* arg1 *boolean*)
     (let ((*tcc-conditions* (push-tcc-condition arg1 *tcc-conditions*)))
       (set-type* arg2 *boolean*)))
    ((OR ∨)
     (set-type* arg1 *boolean*)
     (let ((*tcc-conditions* (push-tcc-condition (make!-negation arg1)
                                                *tcc-conditions*)))
       (set-type* arg2 *boolean*)))
    ((WHEN)
     (set-type* arg2 *boolean*)
     (let ((*tcc-conditions* (push-tcc-condition arg2 *tcc-conditions*)))
       (set-type* arg1 *boolean*)))))


(defun determine-operator-type (operator argument expected expr)
  (if (type operator)
      (if (fully-instantiated? (type operator))
          (type operator)
          (instantiate-operator-type
           (type operator) operator (argument-list argument) expected))
      (let ((coptypes (delete-if-not #'(lambda (ty)
                                         (and (funtype? (find-supertype ty))
                                              (compatible?
                                               (range (find-supertype ty))
                                               expected)))
                        (ptypes operator))))
        (unless coptypes
          (type-incompatible expr (mapcar #'range (ptypes operator))
                             expected argument))
        (let* ((optypes1 (if (cdr coptypes)
                             (or (delete-if-not
                                     #'(lambda (oty)
                                         (let ((dty (domain
                                                     (find-supertype oty))))
                                           (some #'(lambda (aty)
                                                     (compatible? dty aty))
                                                 (ptypes argument))))
                                   coptypes)
                                 coptypes)
                             coptypes))
               (optypes2 (if (cdr optypes1)
                             (or (local-operator-types operator optypes1 argument)
                                 optypes1)
                             optypes1))
               (optypes3 (if (cdr optypes2)
                             (or (delete-if-not #'fully-instantiated? optypes2)
                                 optypes2)
                             optypes2))
               (optypes4 (if (cdr optypes3)
                             (or (delete-if-not
                                       #'(lambda (oty)
                                           (let* ((dty (domtype
                                                        (find-supertype oty))))
                                             (some #'(lambda (aty)
                                                       (tc-eq dty aty))
                                                   (ptypes argument))))
                                     optypes3)
                                   optypes3)
                               optypes3))
               (optypes5 (if (cdr optypes4)
                             (or (instantiable-operator-types
                                  operator optypes4 (argument-list argument)
                                  expected)
                                 optypes4)
                             optypes4))
               (optypes6 (if (cdr optypes5)
                             (or (preferred-argument-conversion
                                  (argument-list argument) optypes5)
                                 optypes5)
                             optypes5))
               (optypes7 (if (cdr optypes6)
                             (or (explicit-importings operator optypes6)
                                 optypes6)
                             optypes6))
               (optypes8 (if (cdr optypes7)
                             (or (max-strict-compatible-optypes
                                  optypes7 (argument-list argument))
                                 optypes7)
                             optypes7))
               (optypes9 (if (cdr optypes8)
                             (or (same-operator-instantiations
                                  optypes8 operator (argument-list argument)
                                  expected)
                                 optypes8)
                             optypes8))
               (optypes (if (cdr optypes9)
                            (or (maximal-domain-optypes optypes9)
                                optypes9)
                            optypes9)))
          (assert optypes)
          #+pvsdebug (assert (null (duplicates? optypes :test #'tc-eq)))
          (when (typep operator 'name-expr)
            (unless (singleton? (resolutions operator))
              (setf (resolutions operator)
                    (or (remove-if-not #'(lambda (r)
                                           (member (type r) optypes
                                                   :test #'tc-eq))
                          (resolutions operator))
                        (remove-if-not #'(lambda (r)
                                           (member (type r) optypes
                                                   :test #'compatible?))
                          (resolutions operator))))
              (assert (resolutions operator))))
          (when (cdr optypes)
            (setf (types operator) optypes)
            (if (name-expr? operator)
                (let ((reses (filter-local-resolutions
                              (resolutions operator))))
                  (cond ((equal reses (resolutions operator))
                         (type-ambiguity operator))
                        (t (setf (resolutions operator) reses)
                           (setf optypes (mapcar #'type reses))
                           (setf (types operator) optypes)
                           (when (cdr reses) (type-ambiguity operator)))))
                (type-ambiguity operator)))
          (cond ((has-type-vars? (car optypes))
                 (type-error operator
                   "The type of the operator ~a cannot be determined from the arguments"
                   operator))
                ((fully-instantiated? (car optypes))
                 (car optypes))
                (t (instantiate-operator-type (car optypes) operator
                                              (argument-list argument)
                                              expected)))))))

;; (defun compatible-optypes (operator expected expr)
;;   (let ((comp-res (delete-if-not #'(lambda (res)
;;                                   (let ((ty (type res)))
;;                                     (and (funtype? (find-supertype ty))
;;                                          (compatible?
;;                                           (range (find-supertype ty))
;;                                           expected))))
;;                  (resolutions operator))))
;;     (unless comp-res
;;       (type-incompatible expr (mapcar #'range (types operator))
;;                       expected argument))
;;     (mapcar #'(lambda (res)
;;              (subst-mod-params (type res)
;;                  (module-instance res)
;;                (module (declaration res))
;;                (declaration res)))
;;       comp-res)))


(defun has-type-vars? (type)
  (has-type-vars?* type))

(defmethod has-type-vars?* ((te type-var))
  t)

(defmethod has-type-vars?* ((te type-application))
  nil)

(defmethod has-type-vars?* ((te type-name))
  nil)

(defmethod has-type-vars?* ((te subtype))
  (has-type-vars?* (find-supertype te)))

(defmethod has-type-vars?* ((te funtype))
  (or (has-type-vars?* (domain te))
      (has-type-vars?* (range te))))

(defmethod has-type-vars?* ((te tupletype))
  (some #'has-type-vars?* (types te)))

(defmethod has-type-vars?* ((te cotupletype))
  (some #'has-type-vars?* (types te)))

(defmethod has-type-vars?* ((te recordtype))
  (some #'has-type-vars?* (fields te)))

(defmethod has-type-vars?* ((te struct-sub-recordtype))
  (or (has-type-vars?* (type te))
      (some #'has-type-vars?* (fields te))))

(defmethod has-type-vars?* ((fd field-decl))
  (has-type-vars?* (type fd)))

(defmethod has-type-vars?* ((bd dep-binding))
  (has-type-vars?* (type bd)))

(defmethod has-type-vars?* ((te type-extension))
  (or (has-type-vars?* (type te))
      (has-type-vars?* (extension te))))

(defun maximal-domain-optypes (optypes &optional max-optypes)
  (if (null optypes)
      (nreverse max-optypes)
      (maximal-domain-optypes
       (cdr optypes)
       (let ((optype-dom (domain (find-supertype (car optypes)))))
         (if (or (some #'(lambda (oty)
                           (subtype-of? optype-dom
                                        (domain (find-supertype oty))))
                       (cdr optypes))
                 (some #'(lambda (oty)
                           (subtype-of? optype-dom
                                        (domain (find-supertype oty))))
                       max-optypes))
             max-optypes
             (cons (car optypes) max-optypes))))))

(defun max-strict-compatible-optypes (optypes arglist &optional max result)
  (if (null optypes)
      (nreverse result)
      (let ((num (count-strict-compatible-domain-types
                  (if (cdr arglist)
                      (domain-types (find-supertype (car optypes)))
                      (list (domain (find-supertype (car optypes)))))
                  arglist)))
        (max-strict-compatible-optypes
         (cdr optypes) arglist
         (if (or (null max)
                 (>= num max))
             num
             max)
         (cond ((or (null max)
                    (= num max))
                (cons (car optypes) result))
               ((< num max)
                result)
               (t (list (car optypes))))))))

(defun count-strict-compatible-domain-types (doms args &optional (count 0))
  (if (null doms)
      count
      (count-strict-compatible-domain-types
       (cdr doms) (cdr args)
       (if (some #'(lambda (aty)
                     (strict-compatible? (car doms) aty))
                 (ptypes (car args)))
           (1+ count)
           count))))

(defun same-operator-instantiations (optypes operator arglist expected)
  (let* ((*dont-worry-about-full-instantiations* t)
         (noptypes (same-operator-instantiations*
                    optypes operator arglist expected nil nil)))
    (when (name-expr? operator)
      (setf (resolutions operator)
            (remove-if-not #'(lambda (r)
                               (member (type r) noptypes
                                       :test #'tc-eq))
              (resolutions operator))))
    noptypes))

(defun same-operator-instantiations* (optypes operator arglist expected
                                              &optional ioptypes result)
  (if (null optypes)
      (nreverse result)
      (let* ((ioptype (if (fully-instantiated? (car optypes))
                          (car optypes)
                          (instantiate-operator-type
                           (car optypes) operator arglist expected)))
             (same? (member ioptype ioptypes :test #'tc-eq)))
        (same-operator-instantiations*
         (cdr optypes) operator arglist expected
         (if same?
             ioptypes
             (cons ioptype ioptypes))
         (if same?
             result
             (cons (car optypes) result))))))

(defmethod explicit-importings ((op name-expr) optypes)
  (let ((reses (remove-if
                   (complement
                    #'(lambda (res)
                        (member (module-instance res)
                                (get-importings (module (declaration res)))
                                :test #'tc-eq)))
                 (resolutions op))))
    (or (remove-if
            (complement
             #'(lambda (oty)
                 (member oty reses :test #'tc-eq :key #'type)))
          optypes)
        optypes)))

(defmethod explicit-importings (op optypes)
  (declare (ignore op))
  optypes)

(defmethod local-operator-types ((op name-expr) optypes argument)
  (let* ((reses (remove-if-not #'(lambda (r)
                                   (member (type r) optypes :test #'tc-eq))
                  (resolutions op)))
         (lreses (local-resolutions reses))
         (loptypes (mapcar #'type lreses)))
    (if (cdr loptypes)
        (optypes-for-local-arguments argument loptypes)
        loptypes)))

(defun local-resolutions (reses)
  (or (let ((breses (remove-if-not #'(lambda (r)
                                       (memq (declaration r)
                                             *bound-variables*))
                      reses)))
        (when breses
          (list (nearest-bound-variable breses))))
      (remove-if-not #'(lambda (r)
                         (eq (module (declaration r))
                             (current-theory)))
        reses)
      (remove-if #'(lambda (r)
                     (let ((th (module (declaration r))))
                       (or (lib-datatype-or-theory? th)
                           (from-prelude? th))))
        reses)
      ;; (remove-if #'(lambda (r)
;;                   (let ((th (module (declaration r))))
;;                     (from-prelude? th)))
;;      reses)
      reses))

(defmethod local-operator-types ((op application) optypes argument)
  (declare (ignore argument))
  (let* ((opoptypes (remove-if-not #'(lambda (oty1)
                                       (let ((soty1 (find-supertype oty1)))
                                         (some #'(lambda (oty2)
                                                   (compatible? oty2
                                                                (range soty1)))
                                               optypes)))
                      (ptypes (operator op))))
         (doptypes (local-operator-types
                    (operator op) opoptypes (argument op))))
    (if (length= optypes doptypes)
        (call-next-method)
        (let ((rtypes (application-range-types-op
                       doptypes (ptypes (argument op))
                       (operator op) (argument op) nil)))
          (remove-if-not #'(lambda (oty2)
                             (some #'(lambda (oty1)
                                       (let ((soty1 (find-supertype oty1)))
                                         (tc-eq oty2 soty1)))
                                   rtypes))
            optypes)))))

(defmethod local-operator-types ((op lambda-expr) optypes argument)
  (declare (ignore argument))
  (let* ((*bound-variables* (append (bindings op) *bound-variables*))
         (pos (optypes-for-local-arguments* (expression op)
                                            (mapcar #'range optypes))))
    (if pos
        (list (nth pos optypes))
        optypes)))

(defmethod local-operator-types (op optypes argument)
  (declare (ignore op))
  (optypes-for-local-arguments argument optypes))

(defun optypes-for-local-arguments (arg optypes)
  (let ((pos (optypes-for-local-arguments*
              arg
              (mapcar #'(lambda (oty)
                          (let ((dom (domain (find-supertype oty))))
                            (if (typep dom 'dep-binding)
                                (type dom)
                                dom)))
                optypes))))
    (if pos
        (list (nth pos optypes))
        optypes)))

(defmethod optypes-for-local-arguments* ((ex name-expr) domtypes)
  (with-slots (resolutions) ex
    (when (cdr resolutions)
      (let* ((reses (local-resolutions resolutions)))
        (when (singleton? reses)
          (let* ((rtype (type (car reses)))
                 (dtypes (member rtype domtypes :test #'tc-eq)))
            (unless (member rtype (cdr dtypes) :test #'tc-eq)
              (position (car dtypes) domtypes :test #'tc-eq))))))))

(defmethod optypes-for-local-arguments* ((ex tuple-expr) domtypes)
  (when (every #'tupletype? domtypes)
    (let ((dtypes-list (mapcar #'types domtypes)))
      (optypes-for-local-arguments-list (exprs ex) dtypes-list))))

(defun dtypes-list (types-list)
  (when (car types-list)
    (cons (mapcar #'car types-list)
          (dtypes-list (mapcar #'cdr types-list)))))

(defun optypes-for-local-arguments-list (exprs dtypes-list &optional pos)
  (if (null exprs)
      pos
      (if (length= (ptypes (car exprs)) dtypes-list)
          (let ((apos (optypes-for-local-arguments*
                       (car exprs) (mapcar #'car dtypes-list))))
            (when (or (null apos)
                      (null pos)
                      (= pos apos))
              (optypes-for-local-arguments-list
               (cdr exprs) (mapcar #'cdr dtypes-list) (or apos pos))))
          (optypes-for-local-arguments-list
           (cdr exprs) (mapcar #'cdr dtypes-list) pos))))

(defmethod optypes-for-local-arguments* ((ex application) domtypes)
  (let ((dtypes (mapcar #'(lambda (dtype)
			    (or (find-if #'(lambda (opty)
					     (tc-eq (range (find-supertype opty)) dtype))
				  (ptypes (operator ex)))
				(find-if #'(lambda (opty)
					     (compatible? (range (find-supertype opty))
							  dtype))
				  (ptypes (operator ex)))))
		  domtypes)))
    (unless (some #'null dtypes)
      (optypes-for-local-arguments* (operator ex) dtypes))))

(defmethod optypes-for-local-arguments* ((ex update-expr) domtypes)
  (optypes-for-local-arguments* (expression ex) domtypes))

(defmethod optypes-for-local-arguments* (ex domtypes)
  (declare (ignore ex domtypes))
  nil)

(defun instantiable-operator-types (op optypes args expected &optional result)
  (if (null optypes)
      (nreverse result)
      (instantiable-operator-types
       op (cdr optypes) args expected
       (let* ((*dont-worry-about-full-instantiations* t)
              (optype (instantiate-operator-type
                       (car optypes) op args expected)))
         (if (and (eq optype (car optypes))
                  (not (fully-instantiated? optype)))
             result
             (cons (car optypes) result))))))

(defun instantiable-operator-type (optype op args expected)
  (declare (ignore op))
  (or (fully-instantiated? optype)
      *dont-worry-about-full-instantiations*
      (let* ((bindings ;;(instantiate-operator-bindings frees)
              (mapcar #'list (formals-not-in-context optype)))
             (domain (if (singleton? args)
                         (domain optype)
                         (domain-types optype)))
             (range (range optype)))
        (assert bindings)
        (let ((nbindings (tc-match-domain args domain
                                          (tc-match expected range bindings))))
          (and nbindings (every #'cdr nbindings))))))

;; (defmethod set-type-application (expr (operator lambda-expr) argument expected)
;;   (with-slots (bindings expression) operator
;;     (let ((args (if (cdr bindings)
;;                  (get-arguments-list argument)
;;                  (list argument))))
;;       (set-type-application-lambda expr operator args argument expected))))

;; (defmethod get-arguments-list ((arg tuple-expr))
;;   (with-slots (exprs) arg
;;     exprs))

;; (defmethod get-arguments-list ((arg expr))
;;   #+pvsdebug (assert (singleton? (types arg)))
;;   (let ((*generate-tccs* 'none))
;;     (typecheck* arg (car (types arg)) nil nil)
;;     (if (typep (find-supertype (type arg)) 'tupletype)
;;      (make!-projections arg)
;;      (list arg))))

;; (defun set-type-application-lambda (expr operator args argument expected)
;;   (let ((bindings (bindings operator)))
;;     (unless (type expr)
;;       (mapc #'(lambda (b) (when (declared-type b)
;;                          (set-type (declared-type b) nil)))
;;          bindings))
;;     (if (typep argument 'tuple-expr)
;;      (set-lambda-appl-types args
;;                             (mapcar #'type bindings)
;;                             bindings)
;;      (set-type* argument (if (cdr bindings)
;;                              (make-tupletype-from-bindings bindings)
;;                              (type (car bindings)))))
;;     (let* ((*tcc-conditions* (nconc (pairlis bindings args) *tcc-conditions*))
;;         (otype (determine-operator-type operator argument expected expr)))
;;       (set-type* operator otype)
;;       (unless (type argument)
;;      (let ((etype (domain (find-supertype (type operator)))))
;;        (set-expr-type argument etype)))
;;       (set-type-application* expr operator args argument expected otype))))

;; (defmethod set-type-application (expr operator argument expected)
;;   (let ((arguments (argument-list argument)))
;;     (set-type-application* expr operator arguments argument expected nil)))

;; (defun set-type-application* (expr operator args argument expected optype)
;;   (let* ((ptypes (ptypes expr))
;;       (nexpected (if (or (type expr)
;;                          (every #'(lambda (ty)
;;                                     (not (from-conversion ty)))
;;                                 ptypes)
;;                          (some #'(lambda (ty)
;;                                    (and (not (from-conversion ty))
;;                                         (compatible? ty expected)))
;;                                ptypes))
;;                      expected
;;                      (find-if #'(lambda (ty)
;;                                   (and (from-conversion ty)
;;                                        (compatible? ty expected)))
;;                        ptypes)))
;;       (otype (or optype
;;                  (determine-operator-type operator argument nexpected expr))))
;;     (set-type* operator otype)
;;     (cond ((and (typep operator 'field-name-expr)
;;              (not (memq (declaration operator) *bound-variables*))
;;              (typep (find-supertype (domain otype)) 'recordtype))
;;         (change-class expr 'field-application)
;;         (setf (id expr) (id operator))
;;         (set-type* argument (domain otype))
;;         (setf (types expr) (list (application-range-type argument otype)))
;;         #+pvsdebug (assert (types expr))
;;         (set-expr-type expr expected))
;;        (t (set-type-application-arguments expr args argument
;;                                           expected otype optype)))))

;; (defun set-type-application-arguments (expr args argument
;;                                          expected otype lambda?)
;;   #+pvsdebug (assert (every #'(lambda (a) (or (typep a 'binding) (types a)))
;;                          args))
;;   (cond ((or (conjunction? expr)
;;           (implication? expr))
;;       (set-type* (car args) *boolean*)
;;       (let ((*tcc-conditions* (push-tcc-condition (car args)
;;                                                  *tcc-conditions*)))
;;         (set-type* (cadr args) *boolean*)
;;         (setf (type argument) (mk-tupletype (mapcar #'type args)))
;;         (set-expr-type expr expected)))
;;      ((disjunction? expr)
;;       (set-type* (car args) *boolean*)
;;       (let ((*tcc-conditions* (push-tcc-condition (make!-negation (car args))
;;                                                  *tcc-conditions*)))
;;         (set-type* (cadr args) *boolean*)
;;         (setf (type argument) (mk-tupletype (mapcar #'type args)))
;;         (set-expr-type expr expected)))
;;      ((boolean-when-expr? expr)
;;       (set-type* (cadr args) *boolean*)
;;       (let ((*tcc-conditions* (push-tcc-condition (cadr args)
;;                                                  *tcc-conditions*)))
;;         (set-type* (car args) *boolean*)
;;         (setf (type argument) (mk-tupletype (mapcar #'type args)))
;;         (set-expr-type expr expected)))
;;      (lambda?
;;       (setf (types expr)
;;             (cons (application-range-type argument otype)
;;                   (delete-if-not #'from-conversion (ptypes expr))))
;;       (set-expr-type expr expected))
;;      (t (set-type* argument (if (typep (domain otype) 'dep-binding)
;;                                 (type (domain otype))
;;                                 (domain otype)))
;;         (setf (types expr)
;;               (cons (application-range-type argument otype)
;;                     (delete-if-not #'from-conversion (ptypes expr))))
;;         (set-expr-type expr expected)))
;;   (check-for-recursive-tcc expr))

(defmethod subst-dep-range ((optype funtype) expr)
  (with-slots (domain range) optype
    (subst-dep-range* domain range expr)))

(defmethod subst-dep-range ((optype subtype) expr)
  (with-slots (supertype) optype
    (subst-dep-range supertype expr)))

(defmethod subst-dep-range* ((domain dep-binding) range expr)
  (let* ((nres (make-resolution domain (theory-name *current-context*)))
         (nname (mk-name-expr (id domain) nil nil nres)))
    (break "subst-dep-range*")
    #+pvsdebug (assert (fully-typed? expr))
    (substit range (list (cons nname expr)))))

(defmethod subst-dep-range* ((domain type-expr) range expr)
  (declare (ignore expr))
  range)


(defun set-lambda-appl-types (exprs expected bindings)
  (when exprs
    (let ((type (car expected)))
      (set-type* (car exprs) type)
      (set-lambda-appl-types (cdr exprs)
                             (substit (cdr expected)
                               (acons (car bindings) (car exprs) nil))
                             (cdr bindings)))))

(defmethod from-conversion ((db dep-binding))
  (from-conversion (type db)))

(defun rec-call-of-depth (expr args depth)
  (when (typep expr 'application)
    (if (or (null depth) (zerop depth))
        (when (and (typep (operator expr) 'name-expr)
                   (eq (declaration (operator expr))
                       (declaration *current-context*)))
          (values (operator expr) (cons (arguments expr) args)))
        (rec-call-of-depth (operator expr)
                           (cons (arguments expr) args)
                           (1- depth)))))

(defmethod associated-decl ((fd formal-theory-decl))
  nil)

(defmethod associated-decl ((fd formal-const-decl))
  nil)

(defun instantiate-resolution (ex res type)
  (let* ((frees (free-params res))
         (bindings
          (mapcar #'list (remove-if #'(lambda (x)
                                        (or (memq x (formals (current-theory)))
                                            (memq x (decl-formals
                                                     (current-declaration)))))
                           frees)))
         (nbindings (tc-match type (type res) bindings)))
    (cond ((and nbindings
		(every #'cdr nbindings))
	   (dolist (bd nbindings)
	     (unless (place (cdr bd))
	       (set-extended-place (cdr bd) ex
				   "matching ~a formal ~a to the type ~a"
				   ex (id (car bd)) type)))
	   (let ((nres (subst-for-formals res nbindings)))
             (if (fully-instantiated? nres)
		 nres
		 (if *dont-worry-about-full-instantiations*
                     nres
                     (type-error ex
                       "Could not determine the full theory instance for ~a~
                     ~%  Theory instance: ~a"
                       ex (full-name (module-instance nres)))))))
	  (t (if *dont-worry-about-full-instantiations*
		 res
		 (type-error ex
		   "Could not determine the full theory instance for ~a~
                ~%  Theory instance: ~a"
		   ex (full-name (module-instance res))))))))

(defun all-formals-from-same-decls (formals &optional decl)
  (or (null formals)
      (and (or (not (decl-formal? (car formals)))
               (null decl)
               (eq (associated-decl (car formals)) decl)
               (memq (car formals) (decl-formals (current-declaration))))
           (all-formals-from-same-decls
            (cdr formals)
            (or decl
                (when (and (decl-formal? (car formals))
                           (not (memq (car formals)
                                      (decl-formals (current-declaration)))))
                  (associated-decl (car formals))))))))

(defun instantiate-resolution-from-bindings (res bindings)
  (multiple-value-bind (thinsts theory-decls)
      (bindings-to-modinsts bindings nil nil)
    ;; Could have multiple theories involved from actuals, mappings, etc.
    (assert (= (length thinsts) (length theory-decls)))
    (instantiate-resolution-from-bindings* res thinsts theory-decls)))

(defun instantiate-resolution-from-bindings* (res thinsts theory-decls)
  (if (null thinsts)
      res
      (instantiate-resolution-from-bindings*
       (subst-mod-params res (car thinsts) (caar theory-decls) (cdar theory-decls))
       (cdr thinsts)
       (cdr theory-decls))))

(defun instantiate-operator-type (optype op args expected)
  (let* ((bindings (mapcar #'list (formals-not-in-context optype)))
         (domain (if (singleton? args)
                     (list (domain optype))
                     (domain-types optype)))
         (range (range optype)))
    ;; (let ((th nil) (decl nil))
    ;;   (dolist (bd bindings)
    ;;  (cond ((null th) (setq th (module (car bd))))
    ;;        ((and (decl-formal? (car bd))
    ;;              (not (eq (module (car bd)) th)))
    ;;         (break "Check mixed optype params")))
    ;;  (when (decl-formal? (car bd))
    ;;    (cond ((null decl)
    ;;           (if (associated-decl (car bd))
    ;;               (setq decl (associated-decl (car bd)))
    ;;               (break "No associated decl")))
    ;;          ((not (eq (associated-decl (car bd)) decl))
    ;;           (break "Check mixed optype dactuals"))))))
    (let ((nbindings (tc-match-domain args domain
                                      (tc-match expected range bindings))))
      (or (and nbindings
               (every #'cdr nbindings)
               (let* ((dtype (subst-for-formals optype nbindings))
                      (type (subst-theory-params dtype nbindings)))
                 (assert (compatible? (range (find-supertype type)) expected))
                 (when (or *dont-worry-about-full-instantiations*
                           (fully-instantiated? type))
                   type)))
          (and (name-expr? op)
               (maybe-instantiate-from-decl-formals op)
               (type op))
          (if *dont-worry-about-full-instantiations*
              optype
              (type-error op
                "Could not determine the full theory instance for~_ ~a~
                 ~:[~;~%  May be a problem with splitting dependent types~]~
                 ~@[~%  Theory instance: ~a~]"
                op
                (some #'(lambda (a)
                          (some #'(lambda (aty)
                                    (and (typep (find-supertype aty)
                                                '(or funtype tupletype))
                                         (dependent? (find-supertype aty))))
                                (ptypes a)))
                      args)
                (when (typep op 'name-expr)
                  (full-name (module-instance op)))))))))

;;; Given a set of formals (usually the free-params of an expression),
;;; generate a bindings list that is ordered and complete, taking account of the
;;; current theory.
;;; Ordered means that all formals for a given theory/decl are in the declared order
;;; Complete in that missing ones are also inserted
;;; e.g., given th1[t1: type, c1: t1] with decl d1[td1: type],
;;;             th2[t2: type, c2: t2] with decl d2[td2: type],
;;;             th3[t3: type, c3: t3] with decl d3[td3: type],
;;;      the current theory is th1,
;;;    and the formals list is (c2 td3 td1)
;;; This produces (t2 c2 t3 c3 td3 td1)
;;; Note that the current theory formals (from th1) are omitted, and no decl formals
;;; are included for th2.

(defun instantiate-operator-bindings (formals)
  (let ((theories nil))
    (dolist (frm formals)
      (pushnew (module frm) theories :test #'eq))
    (let ((bindings nil))
      (dolist (th theories)
        (let ((decls nil))
          (dolist (frm formals)
            (when (and (decl-formal-type? frm)
                       (associated-decl frm)
                       (eq (module frm) th))
              (pushnew (associated-decl frm) decls)))
          (if (null decls)
              (unless (eq th (current-theory))
                (setq bindings
                      (nconc bindings
                             (mapcar #'list (formals-sans-usings th)))))
              (setq bindings
                    (nconc bindings
                           (mapcan #'(lambda (decl)
                                       (nconc (unless (eq th (current-theory))
                                                (mapcar #'list (formals-sans-usings th)))
                                              (mapcar #'list (decl-formals decl))))
                             decls))))))
      (assert (every #'(lambda (bd)
                         (and (consp bd) (formal-decl? (car bd))))
                     bindings))
      bindings)))

(defun instantiate-operator-from-bindings (optype bindings)
  (multiple-value-bind (thinsts theory-decls)
      (bindings-to-modinsts bindings nil nil)
    (assert (= (length thinsts) (length theory-decls)))
    (values (instantiate-operator-from-bindings* optype thinsts theory-decls)
            thinsts)))

(defun instantiate-operator-from-bindings* (optype modinsts theory-decls)
  (if (null modinsts)
      optype
      (instantiate-operator-from-bindings*
       (subst-mod-params
        optype (car modinsts) (caar theory-decls) (cdar theory-decls))
       (cdr modinsts)
       (cdr theory-decls))))

;;; The bindings here represent all theory and decl formals that cover the
;;; free-params of a given expression, as a flat list.
;;; Each element gives the theory, and after the theory formals will be the
;;; decl formal bindings.
(defun bindings-to-modinsts (bindings modinsts theories)
  (if (null bindings)
      (values modinsts theories)
      (let* ((theory (module (caar bindings)))
             (libid (when (lib-datatype-or-theory? theory)
                      (get-library-id (context-path theory))))
             (rem (if (eq theory (current-theory))
                      bindings
                      (nthcdr (length (formals-sans-usings theory)) bindings)))
             (decl (when (and rem (decl-formal-type? (caar rem)))
                     (associated-decl (caar rem))))
             (drem (if decl (nthcdr (length (decl-formals decl)) rem) rem)))
        (bindings-to-modinsts
         drem
         (cons (make-instance 'modname
                 :id (id theory)
                 :library libid
                 :actuals (mapcar #'(lambda (a)
                                      ;; mk-res-actual lifts equality
                                      (mk-res-actual (cdr a) theory))
                            (ldiff bindings rem))
                 :dactuals (mapcar #'(lambda (a)
				       (mk-actual (cdr a)))
                             (ldiff rem drem)))
               modinsts)
         (acons theory decl theories)))))

(defun tc-match-domain (args domain-types bindings)
  (cond ((every #'cdr bindings)
         bindings)
        ((null args)
         bindings)
        (t (tc-match-domain (cdr args) (cdr domain-types)
                            (tc-match-domain* (car args) (car domain-types)
                                              bindings)))))

(defun tc-match-domain* (arg dom-type bindings)
  (tc-match-domain** (reverse (ptypes arg)) dom-type bindings))

(defun tc-match-domain** (arg-types dom-type bindings)
  (if (or (null arg-types)
	  (every #'cdr bindings))
      bindings
      (tc-match-domain** (cdr arg-types) dom-type
                         (tc-match (car arg-types) dom-type bindings))))


;;; Returns the resolutions "closest" to the current theory.

(defun filter-local-resolutions (reses)
  (let ((resolutions
         (remove-if #'(lambda (vr)
                        (and (typep (declaration vr) 'var-decl)
                             (some #'(lambda (sr)
                                       (and (typep (declaration sr)
                                                   'skolem-const-decl)
                                            (strict-compatible? (type vr) (type sr))))
                                   reses)))
           reses)))
    (or (resolutions-of-current-theory* resolutions nil)
        (resolutions-of-current-context* resolutions nil)
        (resolutions-of-visible-library* resolutions nil)
        resolutions)))

(defun resolutions-of-current-theory* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-current-theory*
       (cdr resolutions)
       (if (and (not (declaration? (generated-by (declaration (car resolutions)))))
		(same-id (module-instance (car resolutions))
			 (current-theory-name)))
           (cons (car resolutions) result)
           result))))

(defmethod generated-by (obj)
  (declare (ignore obj))
  nil)

(defun resolutions-of-current-context* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-current-context*
       (cdr resolutions)
       (let ((th (get-theory (module-instance (car resolutions)))))
         (if (and th
                  (or (from-prelude? th)
                      (lib-datatype-or-theory? th)))
             result
             (cons (car resolutions) result))))))

(defun resolutions-of-visible-library* (resolutions result)
  (if (null resolutions)
      (nreverse result)
      (resolutions-of-visible-library*
       (cdr resolutions)
       (let ((th (get-theory (module-instance (car resolutions)))))
         (if (and th (from-prelude? th))
             result
             (cons (car resolutions) result))))))

(defun nearest-bound-variable (reses &optional res)
  (cond ((null reses)
         res)
        ((null res)
         (nearest-bound-variable (cdr reses) (car reses)))
        ((memq (declaration (car reses))
               (memq (declaration res) *bound-variables*))
         (nearest-bound-variable (cdr reses) res))
        (t (nearest-bound-variable (cdr reses) (car reses)))))

(defmethod set-type* ((ex list-expr) expected)
  (let ((etype (dep-binding-type expected)))
    (unless (or (type ex)
		(null (ptypes ex))
		(not (some #'(lambda (ty) (compatible? ty expected)) (ptypes ex))))
      (typecheck* ex etype nil nil))
    (or (type ex)
	(call-next-method))))

(defmethod set-type* ((ex string-expr) expected)
  (let ((*generate-tccs* (if (tc-eq expected (string-type))
                             'none
                             *generate-tccs*)))
    (call-next-method)))
  
;;; Set-type (if-expr)

(defvar *ignore-else-part-tccs* nil)

(defmethod set-type* ((ex if-expr) expected)
  #+pvsdebug (assert (fully-typed? (operator ex)))
  (let* ((op (operator ex))
         (optype (determine-operator-type op (argument ex) expected ex)))
    (set-type* op optype)
    (let ((reses (remove-if-not
                     #'(lambda (r)
                         (let ((stype (find-supertype (type r))))
                           (and stype
                                (typep stype 'funtype)
                                (compatible? (range stype) expected))))
                   (resolutions op))))
      (unless (singleton? reses)
        (if reses
            (type-ambiguity ex)
            (type-incompatible ex (ptypes ex) expected)))
      (if (eq (id (module-instance (car reses))) '|if_def|)
          (let ((econd (condition ex))
                (ethen (then-part ex))
                (eelse (else-part ex)))
            (set-type* econd *boolean*)
            (let ((*tcc-conditions* (push-tcc-condition econd
                                                       *tcc-conditions*)))
              (set-type* ethen expected))
            (let* ((*generate-tccs* (if *ignore-else-part-tccs*
                                        'none
                                        *generate-tccs*))
                   (*tcc-conditions*
                    (if (typep ex '(or first-cond-expr single-cond-expr
                                       cond-expr last-cond-expr))
                        *tcc-conditions*
                        (push-tcc-condition (make!-negation econd)
                                            *tcc-conditions*))))
              (set-type* eelse expected))
            (let* ((iftype (dep-binding-type
                            (compatible-type (type ethen) (type eelse)))))
              (assert iftype)
              ;;(set-type* op (mk-funtype (list *boolean* iftype iftype)
              ;;                        iftype))
              (let ((res (resolve (copy (operator ex)
                                    :resolutions nil
                                    :type nil)
                                  'expr (arguments ex))))
                (cond ((and (singleton? res)
                            (not (tc-eq (car res)
                                        (resolution (operator ex)))))
                       (setf (resolutions op) (list (car res)))
                       (setf (type op) (type (car res))))
                      (t (setf (resolutions op) reses)
                         #+pvsdebug
                         (assert (tc-eq (type (car reses))
                                        (mk-funtype (list *boolean* iftype iftype)
                                                    iftype)))
                         (setf (type op) (type (car reses))))))
              (unless (eq (id (current-theory)) '|if_def|)
                (setf (actuals (module-instance op))
                      (list (mk-actual iftype))))
              #+pvsdebug (assert (fully-instantiated? op))
              (setf (type (argument ex))
                    (mk-tupletype (list *boolean* iftype iftype)))
              (setf (type ex) iftype))
            (unless (typep ex 'branch)
              (if (typep ex 'chained-if-expr)
                  (change-class ex 'chained-branch)
                  (change-class ex 'mixfix-branch))))
          (call-next-method)))))

(defvar *generating-cond-tcc* nil)

(defmethod set-type* ((expr first-cond-expr) expected)
  (declare (ignore expected))
  (let ((*ignore-else-part-tccs* nil))
    (call-next-method)
    (generate-cond-tccs expr)))

(defmethod set-type* ((expr single-cond-expr) expected)
  (declare (ignore expected))
  (let ((*ignore-else-part-tccs* nil))
    (call-next-method)
    (generate-cond-tccs expr)))

(defun generate-cond-tccs (expr)
  (unless (or *generating-cond-tcc*
              (eq *generate-tccs* 'none))
    (let ((*generating-cond-tcc* t))
      (multiple-value-bind (conditions values else?)
          (collect-cond-expr-conditions expr nil nil)
        (let ((dj-conds (if else? (butlast conditions) conditions)))
          (when (cdr dj-conds)
            (unless (trivial-disjointness dj-conds)
              (generate-cond-disjoint-tcc expr dj-conds values))))
        (unless (or else? (trivial-cond-coverage conditions))
          (generate-cond-coverage-tcc expr conditions))))))

(defun trivial-disjointness (dj-conds)
  (let ((right-hand-sides (collect-disjoint-values-from-equations dj-conds)))
    (when right-hand-sides
      #+pvsdebug (assert (length= right-hand-sides dj-conds))
      (let ((dup (duplicates? right-hand-sides :test #'=)))
        (if dup
            (let ((edup (args2 (find dup dj-conds
                                     :test #'(lambda (x y)
                                               (= x (get-arithmetic-value
                                                     (args2 y))))))))
              (type-error edup "Duplicated entry: ~a" edup))
            t)))))

(defun collect-disjoint-values-from-equations (conditions &optional var rhss)
  (cond ((null conditions)
         (nreverse rhss))
        ((null var)
         (when (and (equation? (car conditions))
                    (typep (args1 (car conditions)) 'name-expr)
                    (ground-arithmetic-term? (args2 (car conditions))))
             (collect-disjoint-values-from-equations
              (cdr conditions)
              (args1 (car conditions))
              (list (get-arithmetic-value (args2 (car conditions)))))))
        ((and (equation? (car conditions))
              (tc-eq (args1 (car conditions)) var)
              (ground-arithmetic-term? (args2 (car conditions))))
         (collect-disjoint-values-from-equations
              (cdr conditions)
              var
              (cons (get-arithmetic-value (args2 (car conditions))) rhss)))))

(defmethod ground-arithmetic-term? (expr)
  (declare (ignore expr))
  nil)

(defmethod ground-arithmetic-term? ((expr rational-expr))
  t)

(defmethod ground-arithmetic-term? ((expr tuple-expr))
  (every #'ground-arithmetic-term? (exprs expr)))

(defmethod ground-arithmetic-term? ((expr application))
  (or (and (arithmetic-op? (operator expr))
           (ground-arithmetic-term? (argument expr)))
      (and (arithmetic-rel-op? (operator expr))
           (ground-arithmetic-term? (argument expr)))))

(defmethod ground-arithmetic-term? ((expr propositional-application))
  (ground-arithmetic-term? (argument expr)))

(defmethod ground-arithmetic-term? ((expr equation))
  (ground-arithmetic-term? (argument expr)))

(defmethod ground-arithmetic-term? ((expr disequation))
  (ground-arithmetic-term? (argument expr)))

(defmethod ground-arithmetic-term? ((expr branch))
  (ground-arithmetic-term? (argument expr)))

(defmethod arithmetic-op? ((ex name-expr))
  (and (memq (id ex) '(+ - * /))
       (eq (id (module-instance (car (resolutions ex)))) '|number_fields|)))

(defmethod arithmetic-op? (ex)
  (declare (ignore ex))
  nil)

(defmethod arithmetic-rel-op? ((ex name-expr))
  (and (memq (id ex) '(< <= > >=))
       (eq (id (module-instance (car (resolutions ex)))) '|reals|)))

(defmethod arithmetic-rel-op? (ex)
  (declare (ignore ex))
  nil)

(defmethod get-arithmetic-value ((expr rational-expr))
  (number expr))

(defmethod get-arithmetic-value ((expr application))
  (let ((a1 (get-arithmetic-value (args1 expr)))
        (a2 (when (args2 expr)
              (get-arithmetic-value (args2 expr)))))
    (case (id (operator expr))
      (+ (+ a1 a2))
      (- (if a2 (- a1 a2) (- a1)))
      (* (* a1 a2))
      (/ (/ a1 a2))
      (< (< a1 a2))
      (<= (<= a1 a2))
      (> (> a1 a2))
      (>= (>= a1 a2)))))

(defmethod get-arithmetic-value ((expr conjunction))
  (and (get-arithmetic-value (args1 expr))
       (get-arithmetic-value (args2 expr))))

(defmethod get-arithmetic-value ((expr disjunction))
  (or (get-arithmetic-value (args1 expr))
      (get-arithmetic-value (args2 expr))))

(defmethod get-arithmetic-value ((expr negation))
  (not (get-arithmetic-value (args1 expr))))

(defmethod get-arithmetic-value ((expr implication))
  (or (not (get-arithmetic-value (args1 expr)))
      (get-arithmetic-value (args2 expr))))

(defmethod get-arithmetic-value ((expr iff))
  (eq (get-arithmetic-value (args1 expr))
      (get-arithmetic-value (args2 expr))))

(defmethod get-arithmetic-value ((expr equation))
  (eql (get-arithmetic-value (args1 expr))
       (get-arithmetic-value (args2 expr))))

(defmethod get-arithmetic-value ((expr disequation))
  (not (eql (get-arithmetic-value (args1 expr))
            (get-arithmetic-value (args2 expr)))))

(defmethod get-arithmetic-value ((expr branch))
  (if (get-arithmetic-value (condition expr))
      (get-arithmetic-value (then-part expr))
      (get-arithmetic-value (else-part expr))))

(defun trivial-cond-coverage (conditions)
  (let ((last-cond (car (last conditions))))
    (and (typep last-cond 'else-condition)
         (= (1+ (length (disjuncts (args1 last-cond))))
            (length conditions)))))

(defmethod set-type* ((expr last-cond-expr) expected)
  (declare (ignore expected))
  (let ((*ignore-else-part-tccs* t))
    (call-next-method)))


(defmethod collect-cond-expr-conditions ((expr first-cond-expr) conditions
                                         values)
  (if conditions
      (values conditions values nil)
      (collect-cond-expr-conditions (else-part expr)
                                    (cons (condition expr) conditions)
                                    (cons (then-part expr) values))))

(defmethod collect-cond-expr-conditions ((expr single-cond-expr) conditions
                                         values)
  (values (cons (condition expr) conditions)
          (cons (then-part expr) values)
          nil))

(defmethod collect-cond-expr-conditions ((expr cond-expr) conditions
                                         values)
  (collect-cond-expr-conditions (else-part expr)
                                (cons (condition expr) conditions)
                                (cons (then-part expr) values)))

(defmethod collect-cond-expr-conditions ((expr last-cond-expr) conditions
                                         values)
  (values (nreverse (cons (condition expr) conditions))
          (nreverse (cons (then-part expr) values))
          (typep (condition expr) 'else-condition)))

(defmethod collect-cond-expr-conditions ((expr expr) conditions values)
  (values (nreverse conditions) (nreverse values) t))


;;; For lambda-exprs, we set the type of the body first to the range of
;;; the expected type.  We then check the types of the bind-decls against
;;; the expected types, they must be equal (or generate TCCs to that
;;; effect).  Throughout this process, we must allow for dependent types.
;;; Note that there is no need to check for function conversions here,
;;; they only apply if the lambda-expr is an operator of an application,
;;; in which case the expected here reflects the chosen conversion.

(defmethod set-type* ((ex lambda-expr) expected)
  (let ((sexpected (find-supertype expected)))
    (unless (typep sexpected 'funtype)
      (type-error ex "Type ~a~_ expected for LAMBDA expression~_ ~w"
                  expected ex))
    (let* ((edomain (if (dep-binding? (domain sexpected))
                        (type (domain sexpected))
                        (domain sexpected)))
           (adomain (get-lambda-expr-domain ex)))
      (unless (compatible? adomain edomain)
        (type-incompatible ex (list adomain) edomain))
      (let ((erange (if (dep-binding? (domain sexpected))
                        (substit (range sexpected)
                          (acons (domain sexpected)
                                 (let ((vars (mapcar #'make-variable-expr
                                               (bindings ex))))
                                   (if (cdr vars)
                                       (make!-tuple-expr vars)
                                       (car vars)))
                                 nil))
                        (range sexpected))))
        (set-binding-expr-types (append (bindings ex) (list (expression ex)))
                                (nconc (mapcar #'type (bindings ex))
                                       (if (lambda-expr-with-type? ex)
                                           (list (return-type ex) erange)
                                           (list erange))))
        (let* ((*bound-variables* (append (bindings ex) *bound-variables*))
	       (atype (make-formals-funtype (list (bindings ex))
                                           (if (lambda-expr-with-type? ex)
                                               (return-type ex)
                                               (type (expression ex))))))
          (setf (type ex) atype)
          (unless (tc-eq adomain edomain)
            (or (find-funtype-conversion atype sexpected ex)
                (unless (eq *generate-tccs* 'none)
                  (let ((epreds (equality-predicates adomain edomain)))
                    (when epreds
                      (generate-subtype-tcc ex expected (list epreds)))))))
          (unless (eq *generate-tccs* 'none)
            (let ((toppreds (compatible-preds sexpected expected ex)))
              (when toppreds
                (generate-subtype-tcc ex expected toppreds)))))))))


(defmethod get-lambda-expr-full-domain ((ex lambda-expr))
  (cons (get-lambda-expr-domain ex)
        (get-lambda-expr-full-domain (expression ex))))

(defmethod get-lambda-expr-full-domain (ex)
  (declare (ignore ex))
  nil)

(defun get-lambda-expr-domain (ex)
  (get-lambda-expr-domain* (bindings ex)))

(defun get-lambda-expr-domain* (bindings &optional types)
  (if (null bindings)
      (if (cdr types)
          (mk-tupletype (nreverse types))
          (car types))
      (let ((bind (car bindings)))
        (if (member bind (freevars (cdr bindings)) :key #'declaration)
            (let ((db (mk-dep-binding (id bind) (type bind)
                                      (declared-type bind))))
              (get-lambda-expr-domain*
               (substit (cdr bindings) (acons bind db nil))
               (cons db types)))
            (get-lambda-expr-domain* (cdr bindings)
                                     (cons (type bind) types))))))

;;; set-lambda-dep-types is called to handle lambda-expressions.  The
;;; input is a list of bindings and expected-types, where the
;;; expected-types may include dependencies.  There is no output, just the
;;; side-effect of checking for TCCs.  The expected-types gets substituted
;;; for in the recursive call if there is a dependent type.

(defun set-lambda-dep-types (bindings expected-types)
  (when bindings
    (let* ((dep? (typep (car expected-types) 'dep-binding))
           (etype (if dep?
                      (type (car expected-types))
                      (car expected-types)))
           (*bound-variables* (cons (car bindings) *bound-variables*))
           (*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
      (set-type* (car bindings) etype)
      (set-lambda-dep-types
       (cdr bindings)
       (if dep?
           (substit (cdr expected-types)
             (acons (car expected-types)
                    (make-variable-expr (car bindings))
                    nil))
           (cdr expected-types))))))


(defun add-bindings-to-tcc-conditions (bindings conditions)
  (if (null bindings)
      conditions
      (add-bindings-to-tcc-conditions
       (cdr bindings)
       (if (member (car bindings) conditions
                   :test #'(lambda (x y)
                             (and (consp y) (eq x (car y)))))
           conditions
           (cons (car bindings) conditions)))))


(defun lambda-expr-expected-domain (expr expected)
  (let* ((dexp (domain expected))
         (dtypes (if (singleton? (bindings expr))
                     (list dexp)
                     (if (and (null (cdr (domain-types expected)))
                              (typep (find-supertype
                                      (car (domain-types expected)))
                                     'tupletype)
                              (or (not (null (cdr (bindings expr))))
                                  (not (typep (find-supertype
                                               (type (car (bindings expr))))
                                              'tupletype))))
                         (types (domain-types (find-supertype (car dexp))))
                         (domain-types expected)))))
    (unless (or (singleton? (bindings expr))
                (length= dtypes (bindings expr)))
      (type-error expr "Wrong number of bound variables - ~d expected"
                  (length dtypes)))
    dtypes))

(defmethod set-type* ((ex set-list-expr) expected)
  (let ((est (find-supertype expected)))
    (cond ((null (exprs ex))
           (unless (and (funtype? est)
                        (tc-eq (range est) *boolean*))
             (type-error ex "~a expected here" expected))
           (let* ((id (make-new-variable '|x| ex))
                  (bd (make-bind-decl id (domain est)))
                  (var (make-variable-expr bd)))
             (setf (bindings ex) (list bd))
             (setf (expression ex) *false*)
             (setf (type ex) est)))
          (t (let ((ctypes (remove-if (complement
                                       #'(lambda (ty) (compatible? ty est)))
                             (ptypes ex))))
               (cond ((null ctypes)
                      (type-incompatible ex (ptypes ex) est))
                     ((cdr ctypes)
                      (setf (types ex) ctypes)
                      (type-ambiguity ex))
                     (t (dolist (e (exprs ex))
                          (set-type* e (domain est)))
                        (let* ((id (make-new-variable '|x| ex))
                               (bd (make-bind-decl id (domain (car ctypes))))
                               (var (make-variable-expr bd))
                               (dj (make!-disjunction*
                                    (mapcar #'(lambda (e)
                                                (make!-equation var e))
                                      (exprs ex)))))
                          (setf (bindings ex) (list bd))
                          (setf (expression ex) dj)
                          (setf (type ex) (car ctypes))))))))))
    

(defmethod set-type* ((ex array-expr) expected)
  (let ((est (find-supertype expected)))
    (cond ((null (exprs ex))
	   ;; Need to check that expected is essentially below(0)
           ;; (unless (and (funtype? est)
           ;;              (tc-eq (domain est) below(0)))
           ;;   (type-error ex "~a expected here" expected))
           (let* ((id (make-new-variable '|x| ex))
                  (bd (make-bind-decl id (domain est)))
                  (var (make-variable-expr bd)))
             (setf (bindings ex) (list bd))
             (setf (expression ex) *false*)
             (setf (type ex) est)))
          (t (let ((ctypes (remove-if (complement
                                       #'(lambda (ty) (compatible? ty est)))
                             (ptypes ex))))
               (cond ((null ctypes)
                      (type-incompatible ex (ptypes ex) est))
                     ((cdr ctypes)
                      (setf (types ex) ctypes)
                      (type-ambiguity ex))
                     (t (dolist (e (exprs ex))
                          (set-type* e (range est)))
			;; Need to convert to a lambda-expr:
			;; [: 13, 17, 19 :] => λ (x: below(3)): cases x of 0: 13, 
                        (let* ((id (make-new-variable '|x| ex))
                               (bd (make-bind-decl id (domain est)))
                               (var (make-variable-expr bd))
                               (if-expr (make-array-if-expr var (exprs ex))))
                          (setf (bindings ex) (list bd))
                          (setf (expression ex) if-expr)
                          (setf (type ex) (car ctypes))
			  (check-for-subtype-tcc ex expected)))))))))

(defun make-array-if-expr (var exprs)
  (cond ((null exprs)
	 (break))
	((null (cdr exprs))
	 (car exprs))
	(t (make-array-if-expr* var exprs (- (length exprs) 2) (car (last exprs))))))

(defun make-array-if-expr* (var exprs index else-expr)
  (if (< index 0)
      else-expr
      (let* ((cond-expr (make!-equation var (make!-number-expr index)))
	     (then-expr (nth index exprs))
	     (elsif-expr (if (zerop index)
			     (make!-if-expr cond-expr then-expr else-expr)
			     (make!-chained-if-expr cond-expr then-expr else-expr))))
	(make-array-if-expr* var exprs (1- index) elsif-expr))))

(defmethod set-type* ((ex bind-decl) expected)
  (declare (ignore expected))
  (when (declared-type ex)
    (set-type* (declared-type ex) nil)))

(defmethod set-type* ((ex quant-expr) expected)
  (set-binding-expr-types (append (bindings ex) (list (expression ex)))
                          (nconc (mapcar #'type (bindings ex))
                                 (list (find-supertype expected))))
  (unless (compatible? *boolean* expected)
    (type-incompatible ex (list *boolean*) expected))
  (setf (type ex) *boolean*))


;;; set-quant-dep-types is called to handle quant-exprs.  The input is a
;;; list of bindings and expected-types, where the expected-types may
;;; include dependencies.  There is no output, just the side-effect of
;;; checking for TCCs.  The expected-types gets substituted for in the
;;; recursive call if there is a dependent type.  The last element of the
;;; bindings list is actually an expression, and its type is set relative
;;; to the last expected-type, that has had all of its prior
;;; substitutions.  Otherwise, this is the same as set-lambda-dep-types.

(defun set-binding-expr-types (bindings expected-types)
  (if (cdr bindings)
      (let* ((dep? (dep-binding? (car expected-types)))
             (etype (if dep?
                        (type (car expected-types))
                        (car expected-types))))
        (set-type* (car bindings) etype)
        (let ((*bound-variables* (cons (car bindings) *bound-variables*))
              (*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
          (set-binding-expr-types
           (cdr bindings)
           (if dep?
               (substit (cdr expected-types)
                 (acons (car expected-types) (car bindings) nil))
               (cdr expected-types)))))
      (let ((*tcc-conditions*
             (append (car *appl-tcc-conditions*) *tcc-conditions*)))
        (set-type* (car bindings) (car expected-types))
        (when (cdr expected-types)
          ;; Only happens for lambda-expr with type
          ;; We have set-type for the lambda body against the return-type
          (check-for-tccs (car bindings) (cadr expected-types))))))


(defmethod set-type* ((expr record-expr) expected)
  (let ((sexpected (find-supertype expected))
        (ass (assignments expr)))
    (unless (typep sexpected '(or recordtype struct-sub-recordtype))
      (type-error expr "~a expected here" expected))
    (let ((fields (fields sexpected)))
      (unless (length= ass fields)
        (type-error expr "Wrong number of assignments - ~d expected"
                    (length fields)))
      (let ((badass (find-if-not #'(lambda (a)
                                     (member (id (caar (arguments a))) fields
                                             :test #'(lambda (x y)
                                                       (eq x (id y)))))
                      ass)))
        (when badass
          (type-error badass "Field not found")))
      (let ((args-list (mapcar #'arguments ass))
            (values (mapcar #'expression ass)))
        (set-assignment-arg-types args-list values nil expr sexpected)))
    (setf (type expr)
          (mk-recordtype
           (mapcar #'(lambda (a)
                       (let ((fld (caar (arguments a)))
                             (ty (car (judgement-types+ (expression a)))))
                         (mk-field-decl (id fld) ty ty)))
                   ass)
           nil))))


;;; Have to be careful here to make sure the right TCCs are generated.
;;;     r: [# x: int, y: int #]
;;;     s: [# x, y, z: nat #] = r WITH [`y := 2, `z |-> -5]
;;; Should only generate TCCs r`x >= 0 and -5 >= 0
;;; To do this, we need to compare with the expected type at the right time.

(defmethod set-type* ((expr update-expr) (expected recordtype))
  (assert (or *in-checker* *in-evaluator* (place expr)))
  (with-slots (expression assignments) expr
    (set-type-update-expr-recordtype expression assignments expr expected)))

(defmethod set-type* ((expr update-expr) (expected struct-sub-recordtype))
  (with-slots (expression assignments) expr
    (set-type-update-expr-recordtype expression assignments expr expected)))

(defun set-type-update-expr-recordtype (expression assignments expr expected)
  (assert (or *in-checker* *in-evaluator* (place expression)))
  (assert (or *in-checker* *in-evaluator* (null expr) (place expr)))
  (let ((etypes (collect-compatible-recordtypes (ptypes expr) expected)))
    (check-unique-type etypes expr expected)
    (let* ((stype (find-supertype (car etypes)))
           (atype (if (fully-instantiated? stype)
                      stype
                      (instantiate-from stype expected expr)))
           (args-list (mapcar #'arguments assignments))
           (values (mapcar #'expression assignments)))
      (set-type* expression
		 (if (some #'maplet? assignments)
		     (contract-expected expr atype)
		     expected))
      (set-assignment-arg-types args-list values expression expr expected)
      (setf (type expr) atype))))


(defmethod set-type* ((expr update-expr) (expected tupletype))
  (with-slots (expression assignments) expr
    (set-type-update-expr-tupletype expression assignments expr expected)))

(defmethod set-type* ((expr update-expr) (expected struct-sub-tupletype))
  (with-slots (expression assignments) expr
    (set-type-update-expr-tupletype expression assignments expr expected)))

(defun set-type-update-expr-tupletype (expression assignments expr expected)
  (let ((etypes (collect-compatible-tupletypes (ptypes expr) expected)))
    (check-unique-type etypes expr expected)
    (let* ((stype (find-supertype (car etypes)))
           (atype (if (fully-instantiated? stype)
                      stype
                      (instantiate-from stype expected expr)))
           (args-list (mapcar #'arguments assignments))
           (values (mapcar #'expression assignments)))
      (set-type* expression
		 (if (some #'maplet? assignments)
		     (contract-expected expr atype)
		     expected))
      (set-assignment-arg-types args-list values expression expr expected)
      (setf (type expr) atype))))

(defmethod set-type* ((expr update-expr) (expected funtype))
  (with-slots (expression assignments) expr
    (let ((etypes (collect-compatible-funtypes (ptypes expr) expected))
          (extypes (collect-compatible-funtypes (ptypes expression) expected)))
      (check-unique-type etypes expr expected)
      (check-unique-type extypes expression expected)
      (let* ((stype (find-supertype (car etypes)))
             (atype (if (fully-instantiated? stype)
                        stype
                        (instantiate-from stype expected expr)))
             (sxtype (find-supertype (car extypes)))
             (axtype (if (fully-instantiated? sxtype)
                         sxtype
                         (instantiate-from sxtype expected expr)))
             (args-list (mapcar #'arguments assignments))
             (values (mapcar #'expression assignments)))
        (set-type* expression
		   (if (some #'maplet? assignments) axtype expected))
        (set-assignment-arg-types args-list values expression expr expected)
        (setf (type expr) atype)))))

(defmethod set-type* ((expr update-expr) (expected datatype-subtype))
  (set-type-update-expr-datatype expr expected))

(defmethod set-type* ((expr update-expr) (expected adt-type-name))
  (set-type-update-expr-datatype expr expected))

(defun set-type-update-expr-datatype (expr expected)
  (let ((etypes (collect-compatible-adt-types (ptypes expr) expected)))
    (check-unique-type etypes expr expected)
    (let* ((stype (car etypes))
           (atype (if (fully-instantiated? stype)
                      stype
                      (instantiate-from stype expected expr)))
           (args-list (mapcar #'arguments (assignments expr)))
           (values (mapcar #'expression (assignments expr)))
           (dtypes (collect-compatible-adt-types (ptypes (expression expr))
                                                 expected))
           (dtype (if (fully-instantiated? (car dtypes))
                      (car dtypes)
                      (instantiate-from (car dtypes) expected
                                        (expression expr)))))
      (set-type* (expression expr)
		 (if (some #'maplet? (assignments expr))
		     dtype
		     expected))
      (set-assignment-arg-types args-list values (expression expr) expr atype)
      (setf (type expr) atype))))

(defmethod set-type* ((expr update-expr) (expected subtype))
  (assert (place expr))
  (let ((stype (find-update-supertype expected)))
    ;; Find-update-supertype walks up subtypes, until a datatype-subtype or
    ;; non-subtype is reached, e.g., list[int] rather than list[number]
    (set-type* expr stype)
    (unless (eq *generate-tccs* 'none)
      (let* ((id (make-new-variable '|x| (list expr expected)))
             (bd (make-bind-decl id stype))
             (var (make-variable-expr bd))
             (cpreds (compatible-predicates (list stype) expected var))
             (incs (beta-reduce (substit cpreds
                                  (acons bd (copy expr 'parens 1) nil)))))
        (when incs
          (generate-subtype-tcc expr expected incs))))))

(defmethod find-update-supertype ((te datatype-subtype))
  te)

(defmethod find-update-supertype ((te subtype))
  (find-update-supertype (supertype te)))

(defmethod find-update-supertype ((te type-expr))
  te)

(defmethod set-type* ((expr update-expr) (expected dep-binding))
  (set-type* expr (type expected)))

(defmethod set-type* ((expr update-expr) expected)
  (type-error expr "~a is not a record, tuple, function, array, or datatype"
              expected))



(defun collect-compatible-funtypes (ptypes expected)
  (let* ((cetypes (remove-if-not #'(lambda (ty) (compatible? ty expected))
                    ptypes))
         (etypes (or (remove-if-not #'(lambda (ty)
                                        (strict-compatible? ty expected))
                       cetypes)
                     cetypes))
;;       (fetypes (mapcar #'(lambda (ety)
;;                            (if (fully-instantiated? ety)
;;                                ety
;;                                (instantiate-from ety expected expression)))
;;                  etypes))
;;       (cutypes (remove-if-not #'(lambda (ty) (compatible? ty expected))
;;                  (ptypes expr)))
;;       (utypes (or (remove-if-not #'(lambda (ty)
;;                                      (strict-compatible? ty expected))
;;                     cutypes)
;;                   cutypes))
         )
    etypes))
  

(defun set-assignment-arg-types (args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (assert (type-expr? expected))
  (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (set-assignment-arg-types* args-list values ex expr expected))

(defmethod set-assignment-arg-types* (args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (assert (type-expr? expected))
  (when args-list
    ;; We're not at an updateable type, should have no more args
    (assert (null (car args-list)))
    (set-type* (car values) expected)
    (set-assignment-arg-types* (cdr args-list) (cdr values) ex expr expected)))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected subtype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (let ((stype (find-adt-supertype expected)))
    (typecase stype
      ((or funtype recordtype tupletype adt-type-name datatype-subtype)
       (set-assignment-arg-types* args-list values ex expr stype)
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
		   (unless a
		     (check-for-subtype-tcc v expected)))
             args-list values))
       (t (call-next-method)))))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected recordtype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (with-slots (fields) expected
    (if (every #'null args-list)
        (call-next-method)
        (set-assignment-arg-types-recordtype fields args-list values ex expr expected))))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected struct-sub-recordtype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (with-slots (fields) expected
    (if (every #'null args-list)
        (call-next-method)
        (set-assignment-arg-types-recordtype fields args-list values ex expr expected))))

;; Dependent record types are tricky
;; R: type = [# a: [nat -> nat], b: #]
;; r: R
;; r with [`a(0) := 3, `b

(defun set-assignment-arg-types-recordtype (fields args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (mapc #'(lambda (a v) (unless a (set-type* v expected))) args-list values)
  ;; This is wrong - if we're going to recurse we need to make all
  ;; arguments homogeneous, i.e., r WITH [`a`x := 3, `a := e] leads to e
  ;; being set-typed above, but then need to construct the args-list for
  ;; `a`x := e`x, etc.
  (multiple-value-bind (cargs-list cvalues)
      (complete-assignments args-list values ex expr expected)
    (set-assignment-rec-arg-types cargs-list cvalues ex fields fields 
				  (mapcar #'(lambda (a) (id (caar a))) args-list)
				  expr)))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected tupletype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (with-slots (types) expected
    (if (every #'null args-list)
        (call-next-method)
        (set-assignment-arg-types-tupletype types args-list values ex expr expected))))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected struct-sub-tupletype))
  (with-slots (types) expected
    (set-assignment-arg-types-tupletype types args-list values ex expr expected)))

(defun set-assignment-arg-types-tupletype (types args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  ;; recurse down the expected tuple types, which may be dependent..
  (multiple-value-bind (cargs-list cvalues)
      (complete-assignments args-list values ex expr expected)
    (set-assignment-tup-arg-types cargs-list cvalues ex types 1 expr)))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected funtype))
  (with-slots (domain range) expected
    (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
    (if (every #'null args-list)
        (call-next-method)
        (set-assignment-fun-arg-types args-list values ex expr expected))))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected datatype-subtype))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (if (every #'null args-list)
      (set-type* (car values) expected)
      (set-assignment-update-arg-types args-list values ex expr expected)))

(defmethod set-assignment-arg-types* (args-list values ex expr (expected adt-type-name))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (if (every #'null args-list)
      (call-next-method)
      (set-assignment-update-arg-types args-list values ex expr expected)))

(defun set-assignment-update-arg-types (args-list values ex expr expected)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (let* ((ass-accs (mapcar #'caar args-list))
         (constrs (remove-if #'(lambda (c)
                                 (not (every #'(lambda (a)
                                                 (member a (accessors c)
                                                         :test #'same-id))
                                             ass-accs)))
                    (constructors expected))))
;;       (dep-accs (get-dependent-accessors ass-accs expected))
;;       (dep-assns (mapcar #'(lambda (da)
;;                              (make-datatype-assignment
;;                               da (expression expr)))
;;                    dep-accs)))
    (mapc #'(lambda (a v)
              (unless a (set-type* v expected)))
          args-list values)
    (set-assignment-update-arg-types* constrs args-list values ex expr)))

(defun set-assignment-update-arg-types* (constrs args-list values ex expr)
  (assert constrs)
  (let ((cpreds (mapcar #'(lambda (c) (make!-application (recognizer c) ex))
                  constrs)))
    (set-constructors-update-arg-types constrs args-list values ex cpreds expr)))

;; When multiple constructors are involved we need to collect the TCCs for
;; each one, and form the disjunction.
(defun set-constructors-update-arg-types (constrs args-list values ex cpreds expr
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
             (*tccforms* nil)
             (jhash (judgement-types-hash (judgements *current-context*))))
        (multiple-value-bind (cargs-list cvalues)
            (complete-constructor-assignments args-list values ex accs)
          (unwind-protect
              (let ((*collecting-tccs* t)
                    (rectype (if (fully-instantiated? (range (type c)))
                                 (range (type c))
                                 (instantiate-from
                                  (range (type c)) (type ex) ex))))
                (multiple-value-bind (jtypes jdecls)
                    (judgement-types ex)
                  (setf (gethash ex jhash)
                        (list (cons rectype jtypes) jdecls)))
                (set-assignment-accessor-arg-types cargs-list cvalues ex accs expr))
            (let ((jtypes&jdecls (gethash ex jhash)))
              (setf (gethash ex jhash)
                    (list (cdr (car jtypes&jdecls)) (cadr jtypes&jdecls)))))
          (let* ((cpred (find (recognizer c) cpreds
                              :key #'operator :test #'tc-eq))
                 (stccs (gensubst (mapcar #'tccinfo-formula *tccforms*)
                         #'(lambda (x) (if (tc-eq x cpred) *true* *false*))
                         #'(lambda (x) (member x cpreds :test #'tc-eq))))
                 (ntccs (cons cpred
                              (remove *true* (pseudo-normalize stccs)
                                      :test #'tc-eq)))
                 (tcc (make!-conjunction* ntccs)))
            (setf (parens tcc) 1)
            (set-constructors-update-arg-types
             (cdr constrs) args-list values ex cpreds expr
             (cons tcc tccs) (cons (recognizer c) recs)))))))

;;; Set the assignment types.  This is complicated by 
;;; updates such as "x with [`f := lambda y: x`f(y) + 1, `f(0) := 0]"
;;; Where the "f" field is used more than once.
;;; args-list and values are expanded from the original update-expr,
;;; to include dependencies.

(defun rec-arg-types-cdrs (cdargs cdvals nargs nvals)
  (declare (ignore nargs nvals))
  (let ((cdr-args (mapcar #'cdr cdargs))
	(cdr-vals cdvals))
    (values cdr-args cdr-vals)))

;; (cond ((null cdargs) (values nargs nvals))
;; 	(nil ;(null (cdr (car cdargs)))
;; 	 (rec-arg-types-cdrs (cdr cdargs) (cdr cdvals) nargs nvals))
;; 	(t (rec-arg-types-cdrs (cdr (car cdargs)) (cdr cdvals)
;; 			       (cons (car cdargs) nargs) (cons (car cdvals) nvals)))))

(defun set-assignment-rec-arg-types (args-list values ex fields orig-fields changed-fields expr
				     &optional cargs cvalues)
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
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
           (done-with-field? (not (member (car fields) rem-args :test #'same-id :key #'caar))))
      (multiple-value-bind (cdr-args cdr-vals)
	  (when done-with-field?
	    (rec-arg-types-cdrs (cons args cargs) (cons value cvalues) nil nil))
	(when args
          (unless (field-assignment-arg? (caar args))
            (if (id-assign? (caar args))
		(change-class (caar args) 'field-assign)
		(change-class (caar args) 'field-assignment-arg)))
          (when done-with-field?
	    (let* ((mk-appl? (and ex (some (complement #'null) cdr-args)))
		   (fappl (when mk-appl? (make!-field-application (car fields) ex)))
		   (fldtype (type (car fields)))
		   (ftype (find-supertype fldtype)))
	      (when fappl
		(set-extended-place fappl ex
				    "creating field application from ~a and ~a"
				    ex (car fields)))
	      ;; different in check-assignment-rec-arg-types:
	      ;; should it be (nreverse (cons value cvalues)) instead of cdr-vals?
	      (set-assignment-arg-types* cdr-args cdr-vals fappl expr fldtype)
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
		  (set-type* app expected))))))
	(let* ((needs-fapp (some #'(lambda (fld) (member (car fields) (freevars fld) :key #'declaration)) fields))
	       (nfields (if done-with-field?
                            (if needs-fapp
				(if (and ex (some (complement #'null) cdr-args))
                                    (let* ((fapp (make!-field-application (car fields) ex))
                                           (ass (make-assignment (car cdr-args) (car cdr-vals)))
                                           (val (make-update-expr fapp (list ass))))
				      ;; (assert (place ex))
				      ;; ;; (push (list fapp ex 'set-assignment-rec-arg-types) *set-type-generated-terms*)
				      ;; ;; (push (list val ex 'set-assignment-rec-arg-types) *set-type-generated-terms*)
				      ;; (break)
				      (set-extended-place fapp ex "field-application created for ~a" ex)
                                      (subst-rec-dep-type val (car fields) (cdr fields)))
                                    (progn (set-type* value (type (car fields)))
					   (subst-rec-dep-type value (car fields) (cdr fields))))
				(cdr fields))
                            fields)))
          (if nfields
              (set-assignment-rec-arg-types rem-args rem-values ex nfields
					    (if done-with-field? (cdr orig-fields) orig-fields)
					    changed-fields expr
                                            (unless done-with-field? (cons args cargs))
                                            (unless done-with-field? (cons value cvalues)))
              (set-assignment-rec-arg-maplet-types rem-args rem-values ex expr)))))))

(defun set-assignment-rec-arg-maplet-types (args-list values ex expr)
  (when args-list
    (let ((args (car args-list))
          (value (car values)))
      (assert (null (cdr args)))
      (unless (field-assignment-arg? (caar args))
        (if (id-assign? (caar args))
            (change-class (caar args) 'field-assign)
            (change-class (caar args) 'field-assignment-arg)))
      (if (singleton? (ptypes value))
          (set-type* value (car (ptypes value)))
          (type-ambiguity value)))
    (set-assignment-rec-arg-maplet-types (cdr args-list) (cdr values) ex expr)))

(defun set-assignment-tup-arg-types (args-list values ex types index expr
                                               &optional cargs cvalues)
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
            (dolist (a nargs)
              (when (caar a)
                (setf (type (caar a)) *naturalnumber*)))
            (set-assignment-arg-types*
             (mapcar #'cdr nargs)
             (nreverse (cons value cvalues))
             projapp expr (dep-binding-type (car types))))))
      (set-assignment-tup-arg-types
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

(defun set-assignment-fun-arg-types (args-list values ex expr funtype)
  (when args-list
    (multiple-value-bind (cargs cvalues rem-args rem-values)
        (collect-same-first-fun-assignment-args args-list values)
      (let ((domtypes (domain-types* (domain funtype))))
        (dolist (arg cargs)
          (when arg
            (if (length= domtypes (car arg))
                (set-tup-types (car arg) domtypes)
                (set-type* (caar arg) (domain funtype))))))
      (let* ((arg (when (caar cargs) (make!-arg-tuple-expr* (caar cargs))))
	     (cdrs (mapcar #'cdr cargs))
	     (appl (when (and ex arg)
		     (let ((app (make!-application ex arg)))
		       (set-extended-place app expr
					   "making application ~a" app)
		       app)))
	     (ftype (if arg
			(if (dep-binding? (domain funtype))
			    (substit (range funtype) (acons (domain funtype) arg nil))
			    (range funtype))
			funtype)))
        (set-assignment-arg-types* cdrs cvalues appl expr ftype))
      (set-assignment-fun-arg-types rem-args rem-values ex expr funtype))))

(defun collect-same-first-fun-assignment-args (args-list values
                                                         &optional cargs cvalues)
  (if (or (null args-list)
          (and cargs
               ;; Can't use tc-eq here; not yet typechecked
               (not (ps-eq (caar args-list) (caar cargs)))))
      (values (nreverse cargs) (nreverse cvalues) args-list values)
      (collect-same-first-fun-assignment-args
       (cdr args-list) (cdr values)
       (cons (car args-list) cargs)
       (cons (car values) cvalues))))

(defun set-assignment-accessor-arg-types (args-list values ex accessors expr
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
        (unless (accessor-assignment-arg? (caar args))
          (if (id-assign? (caar args))
              (change-class (caar args) 'accessor-assign)
              (change-class (caar args) 'accessor-assignment-arg)))
        (when done-with-acc?
          (set-assignment-arg-types*
           (mapcar #'cdr (nreverse (cons args cargs)))
           (nreverse (cons value cvalues))
           (when ex (make!-application (car accessors) ex))
	   expr
           (if (dep-binding? (domain (type (car accessors))))
               (substit (range (type (car accessors)))
                 (acons (domain (type (car accessors))) ex nil))
               (range (type (car accessors)))))))
      (set-assignment-accessor-arg-types
       rem-args rem-values ex
       (if done-with-acc?
           (subst-acc-dep-type value (car accessors) (cdr accessors))
           accessors)
       expr
       (unless done-with-acc?
         (cons args cargs))
       (unless done-with-acc?
         (cons value cvalues))))))

(defmethod contract-expected (expr (expected recordtype))
  (contract-expected-recordtype expr expected))

(defmethod contract-expected (expr (expected struct-sub-recordtype))
  (contract-expected-recordtype expr expected))

(defun contract-expected-recordtype (expr expected)
  (let* ((new-ids (mapcar #'(lambda (a) (id (caar (arguments a))))
                    (remove-if-not #'maplet? (assignments expr))))
         (types (collect-compatible-recordtypes
                 (ptypes (expression expr)) expected new-ids)))
    (check-unique-type types (expression expr) expected)
    (let* ((fields (fields (find-supertype (car types))))
           ;; Remove those fields added by maplets, and replace fields changed
           ;; by maplets.
           (nfields (mapcar #'(lambda (fld)
                                (if (memq (id fld) new-ids)
                                    (car (member (id fld) fields :key #'id))
                                    fld))
                      (remove-if-not #'(lambda (fld)
                                         (member fld fields :test #'same-id))
                        (fields expected)))))
      (lcopy expected
        :fields nfields
        :dependent? (dependent-fields? nfields)))))

(defun check-unique-type (types expr expected)
  (cond ((cdr types)
         (setf (types expr) types)
         (type-ambiguity expr))
        ((null types)
         (type-incompatible expr (ptypes expr) expected))))

;;; collects those types that are compatible with the expected type
(defun collect-compatible-recordtypes (types expected &optional ignore-ids)
  (delete-if-not
      #'(lambda (ty)
          (let ((sty (find-supertype ty)))
            (and (typep sty '(or recordtype struct-sub-recordtype))
                 (or ignore-ids
                     (if (struct-sub-recordtype? expected)
                         (<= (length (fields expected)) (length (fields sty)))
                         (length= (fields sty) (fields expected))))
                 (every #'(lambda (fld)
                            (or (memq (id fld) ignore-ids)
                                (member fld (fields sty)
                                        :test #'(lambda (x y)
                                                  (and (eq (id x) (id y))
                                                       (compatible?
                                                        (type x) (type y)))))))
                        (fields expected)))))
    types))

(defmethod contract-expected (expr (expected tupletype))
  (contract-expected-tupletype expr expected))

(defmethod contract-expected (expr (expected struct-sub-tupletype))
  (contract-expected-tupletype expr expected))

(defun contract-expected-tupletype (expr expected)
  (let* ((new-indices (mapcar #'(lambda (a) (number (caar (arguments a))))
                        (remove-if-not #'maplet? (assignments expr))))
         (types (collect-compatible-tupletypes
                 (ptypes (expression expr)) expected new-indices)))
    (check-unique-type types (expression expr) expected)
    (let ((types (types (find-supertype (car types)))))
      ;; Remove those types added by maplets, and replace types changed
      ;; by maplets.
      (lcopy expected
        :types (let ((index 0))
                 (mapcar #'(lambda (ty ety)
                             (if (member (incf index) new-indices :test #'=)
                                 ty
                                 ety))
                   types (types expected)))))))

(defun collect-compatible-tupletypes (types expected &optional ignore-indices)
  (remove-if-not
      #'(lambda (ty)
          (let ((sty (find-supertype ty)))
            (and (typep sty '(or tupletype struct-sub-tupletype))
                 (or ignore-indices
                     (= (length (types sty)) (length (types expected))))
                 (let ((index 0))
                   (every #'(lambda (ety ty)
                              (or (member (incf index) ignore-indices
                                          :test #'=)
                                  (compatible? ety ty)))
                          (types expected) (types sty))))))
    types))


(defun get-corresponding-constructor-accessors (ass-accs constructors)
  (assert constructors)
  (let ((accessors (accessors (car constructors))))
    (if (every #'(lambda (a)
                   (member (declaration a) accessors :key #'declaration))
               ass-accs)
        accessors
        (get-corresponding-constructor-accessors
         ass-accs (cdr constructors)))))

(defun get-dependent-accessors (accs adt-type)
  (let ((dep-accs nil))
    (dolist (c (constructors adt-type))
      (dolist (acc (accessors c))
        (unless (or (member acc accs :test #'same-id)
                    (member acc dep-accs :test #'same-id))
          (when (or (some #'(lambda (a)
                              (or (occurs-in acc (type a))
                                  (occurs-in a (type acc))))
                          accs)
                    (some #'(lambda (a)
                              (or (occurs-in acc (type a))
                                  (occurs-in a (type acc))))
                          dep-accs))
            (push acc dep-accs)))))
    dep-accs))

(defun collect-compatible-adt-types (types expected)
  (remove-if (complement
              #'(lambda (ty)
                  (let ((sty (find-update-supertype ty)))
                    (and (typep sty '(or adt-type-name datatype-subtype))
                         (compatible? sty expected)))))
    types))

(defun subst-acc-dep-type (expr acc accessors &optional bindings result)
  (if (null accessors)
      (nreverse result)
      (multiple-value-bind (nacc nbindings)
          (subst-acc-dep-type* expr acc (car accessors) bindings)
        (subst-acc-dep-type expr acc (cdr accessors)
                            nbindings
                            (cons nacc result)))))

;;; Given accessor of type [x: (c?) -> p(x, acc(x))]
;;; Create accessor of type [x: (c?) -> p(x, expr)]
(defun subst-acc-dep-type* (expr acc accessor bindings)
  (if (dep-binding? (domain (type accessor)))
      (let* ((bd (domain (type accessor)))
             (var (mk-name-expr
                      (id bd) nil nil
                      (make-resolution bd (current-theory-name) (type bd))))
             (appl (make!-application acc var))
             (nbindings (acons appl expr bindings))
             (ran (gensubst (range (type accessor))
                    #'(lambda (ex) (cdr (assoc ex nbindings :test #'tc-eq)))
                    #'(lambda (ex) (assoc ex nbindings :test #'tc-eq)))))
	(break "subst-acc-dep-type*")
        (if (eq ran (range (type accessor)))
            (values accessor bindings)
            (let* ((dom (if (occurs-in bd ran)
                            bd
                            (type bd)))
                   (atype (mk-funtype dom ran))
                   (res (make-resolution (declaration (resolution accessor))
                          (module-instance (resolution accessor))
                          atype)))
              (values (copy accessor
                        :type atype
                        :resolutions (list res))
                      nbindings))))
      (values accessor bindings)))
  

;;; make-assignment-subst-expr takes an assignment of the form, e.g.,
;;;  (1)(x,y)(3) := true, and a type of the form [[t1, t2] -> [int -> bool]]
;;; and the expr associated with the update (e.g., the T in T WITH ...)
;;; and creates the expression
;;;  (LAMBDA (x1:[t1, t2]):
;;;     IF x1 = (x,y)
;;;        THEN (LAMBDA (x2:int):
;;;                 IF x2 = 3 THEN TRUE ELSE PROJ_1(T)(x1)(x2) ENDIF)
;;;        ELSE PROJ_1(T)(x1) ENDIF)

(defun make-assignment-subst-expr (ass type expr)
  (let ((ass-args (arguments ass))
        (ass-expr (expression ass)))
    (if (cdr ass-args)
        (let ((app (make-assignment-appl-expr (caar ass-args) expr)))
          (make-assignment-subst-expr* (cdr ass-args) ass-expr type app))
        ass-expr)))

(defmethod make-assignment-appl-expr ((arg number-expr) expr)
  (make!-projection-application (number arg) expr))

(defmethod make-assignment-appl-expr ((arg name-expr) expr)
  (make!-field-application arg expr))

(defmethod make-assignment-else-expr ((arg number-expr) expr)
  (make!-projection-application (number arg) expr))

(defmethod make-assignment-else-expr ((arg name-expr) expr)
  (make!-field-application arg expr))

(defun make-assignment-subst-expr* (args expr type proj)
  (multiple-value-bind (bindings vars)
      (make-assignment-bindings args type nil nil)
    (make-assignment-lambda-expr bindings vars args expr proj type)))

(defun make-assignment-bindings (args type bindings vars)
  (if (null args)
      (values (nreverse bindings) (nreverse vars))
      (let* ((stype (find-supertype type))
             (dom (domain stype))
             (dep? (typep dom 'dep-binding))
             (dtype (if dep? (type dom) dom))
             (ran (range stype))
             (id (make-new-variable '|x| (cons type bindings)))
             (bd (typecheck* (mk-bind-decl id dtype) nil nil nil))
             (var (mk-name-expr id nil nil (make-resolution bd nil dtype))))
	(break "make-assignment-bindings")
        (make-assignment-bindings (cdr args)
                                  (if dep?
                                      (substit ran (acons dom var nil))
                                      ran)
                                  (cons bd bindings)
                                  (cons var vars)))))

(defun make-assignment-equality (var arg)
  (let ((narg (if (listp arg)
                  (if (cdr arg)
                      (make!-tuple-expr arg)
                      (car arg))
                  arg)))
    (make!-equation var narg)))


(defun make-assignment-lambda-expr (bindings vars args expr proj type)
  (typecheck* (make-assignment-lambda-expr* bindings vars args expr proj)
              type nil nil))

(defun make-assignment-lambda-expr* (bindings vars args expr proj)
  (if (null bindings)
      expr
      (let ((nproj (mk-application proj (car vars))))
	(break "make-assignment-lambda-expr*")
        (mk-lambda-expr (list (car bindings))
          (add-parens
           (mk-if-expr (make-assignment-equality (car vars) (car args))
                       (make-assignment-lambda-expr*
                        (cdr bindings) (cdr vars) (cdr args) expr
                        nproj)
                       nproj))))))

(defun add-parens (expr)
  (setf (parens expr) 1)
  expr)

(defun make-assignment-projection (proj vars)
  (break "make-assignment-projection")
  (if (null vars)
      proj
      (make-assignment-projection (mk-application proj (car vars))
                                  (cdr vars))))

(defun subst-tup-dep-type (expr type types)
  (let ((rest (memq type types)))
    (assert rest)
    (let ((srest (substit (cdr rest) (acons type expr nil))))
      (if (eq srest (cdr rest))
          types
          (append (ldiff types (cdr rest)) srest)))))

(defmethod complete-assignments (args-list values ex expr (rtype recordtype))
  #+pvsdebug (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  ;; Used to check for dependent?, but we really need all assignments if
  ;; we're going to generate correct TCCs.
  ;; Since field-decls don't point to their associated recordtypes
  ;; (they can't, since they may appear in several different ones due
  ;; to copying and dependent substitutions), we create the
  ;; association list in *field-records*.
  (if (dependent? rtype)
      (let ((*field-records* (mapcar #'(lambda (fld) (cons fld rtype))
			       (fields rtype))))
	(complete-rec-assignments args-list values (fields rtype) ex expr nil nil))
      (values args-list values)))

(defmethod complete-assignments (args-list values ex expr (rtype struct-sub-recordtype))
  ;; Used to check for dependent?, but we really need all assignments if
  ;; we're going to generate correct TCCs.
  ;; Since field-decls don't point to their associated recordtypes
  ;; (they can't, since they may appear in several different ones due
  ;; to copying and dependent substitutions), we create the
  ;; association list in *field-records*.
  (let ((*field-records* (mapcar #'(lambda (fld) (cons fld rtype))
                           (fields rtype))))
    (complete-rec-assignments args-list values (fields rtype) ex expr nil nil)))

(defun complete-rec-assignments (args-list values fields ex expr cargs cvalues)
  #+pvsdebug (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (if (null fields)
      (values (append args-list (nreverse cargs))
              (append values (nreverse cvalues)))
      (let* ((pos (position (car fields) args-list :test #'same-id :key #'caar))
	     (args (if pos
		       cargs
		       (let ((fldass (make-instance 'field-assign :id (id (car fields)))))
			 (set-extended-place fldass expr "completing field assignment for ~a"
					     (id (car fields)))
			 (cons (list (list fldass)) cargs))))
	     (vals (if pos
		       cvalues
		       (let ((fappl (make!-field-application (car fields) ex)))
			 #+pvsdebug (assert (place ex))
			 (set-extended-place fappl ex "completing field application for ~a" fappl)
			 (cons fappl cvalues)))))
        (complete-rec-assignments args-list values (cdr fields) ex expr args vals))))

(defun make-rec-assignment (field expr)
  (break "make-rec-assignment")
  (mk-assignment 'uni
    (list (list (make-instance 'field-assign :id (id field))))
    (make!-field-application field expr)))

(defmethod complete-assignments (args-list values ex expr (type tupletype))
  (complete-tup-assignments args-list values (types type) ex 1 expr nil nil))

(defmethod complete-assignments (args-list values ex expr (type struct-sub-tupletype))
  (complete-tup-assignments args-list values (types type) ex 1 expr nil nil))

(defun complete-tup-assignments (args-list values types ex num expr cargs cvalues)
  (if (null types)
      (values (append args-list (nreverse cargs))
              (append values (nreverse cvalues)))
      (let ((arg (find num args-list
                       :test #'eql :key #'(lambda (a)
                                            (when a (number (caar a)))))))
        (complete-tup-assignments
         args-list values (cdr types) ex (1+ num) expr
         (if arg
             cargs
	     (let ((prass (make-instance 'proj-assign
			    :number num)))
	       (set-extended-place prass expr
				   "completing proj assignment for ~a"
				   num)
	       (cons (list (list prass)) cargs)))
         (if arg
             cvalues
	     (let ((prappl (make!-projection-application num ex)))
	       (set-extended-place prappl ex
				   "completing proj application for ~a"
				   prappl)
	       (cons prappl cvalues)))))))

(defun make-tup-assignment (expr num)
  (let ((type (nth (1- num) (types (find-supertype (type expr))))))
    (make-instance 'uni-assignment
      :arguments (list (list (make-number-expr num)))
      :expression (make-instance 'projection-application
                    :id (makesym "PROJ_~d" num)
                    :index num
                    :argument expr
                    :type type))))

(defun complete-constructor-assignments (args-list values ex accessors
                                                   &optional cargs cvalues)
  (cond ((null accessors)
         (values (append args-list (nreverse cargs))
                 (append values (nreverse cvalues))))
        ((member (car accessors) args-list :test #'same-id :key #'caar)
         (complete-constructor-assignments
          args-list values ex (cdr accessors) cargs cvalues))
        (t
         (complete-constructor-assignments
          args-list values ex (cdr accessors)
          (cons (list (list (car accessors))) cargs)
          (cons (make!-application (car accessors) ex) cvalues)))))

(defun make-datatype-assignment (acc expr)
  (break "make-datatype-assignment")
  (let* ((appl (make!-application acc expr)))
    (setf (place appl) (place expr))
    (mk-assignment 'uni (list (list acc)) appl)))

(defmethod dependent? ((type tupletype))
  (dependent? (types type)))

(defmethod dependent? ((type funtype))
  (or (dependent? (domain type))
      (dependent? (range type))))

(defmethod dependent? ((type dep-binding))
  t)

(defmethod dependent? ((type subtype))
  nil)

(defmethod dependent? ((type type-name))
  nil)

(defmethod dependent? ((c constructor-name-expr))
  (dependent? (accessors c)))

(defmethod dependent? ((list list))
  (some #'dependent? list))

(defmethod dependent? ((a accessor-name-expr))
  (dependent? (type a)))

(defun subst-rec-dep-type (expr fld fields)
  (let ((sub (list expr fld fields)))
    (or (let ((hres (gethash sub *subst-fields-hash*)))
	  ;; This assert isn't quite right - nested records will have different
	  ;; field-decls, which are not tc-eq
	  ;; (assert (or (null hres)
	  ;; 	      (every #'(lambda (fld1 fld2)
	  ;; 			 (tc-eq (type fld1) (type fld2)))
	  ;; 		     hres (subst-rec-dep-type* expr fld fields))))
	  hres)
	(let ((result (subst-rec-dep-type* expr fld fields)))
	  (setf (gethash sub *subst-fields-hash*) result)
	  result))))

(defun subst-rec-dep-type* (expr fld fields &optional bindings result)
  (if (null fields)
      (nreverse result)
      (let ((nfld (substit (car fields) (acons fld expr bindings))))
        (subst-rec-dep-type* expr fld (cdr fields)
			     (if (eq nfld (car fields))
				 bindings
				 (acons (car fields) nfld bindings))
			     (cons nfld result)))))

(defun maybe-instantiate-from-decl-formals (ex)
  (maybe-instantiate-from-decl-formals* (resolutions ex) ex))

(defun maybe-instantiate-from-decl-formals* (reses ex)
  (when reses
    (let* ((dfmls (decl-formals (current-declaration)))
           (fparams (when dfmls (free-params (car reses))))
           (adecl (let ((dfml (find-if #'decl-formal-type? fparams)))
                    (when dfml (associated-decl dfml)))))
      (or (when (and fparams (length= fparams dfmls)
                     (every #'(lambda (fp)
                                (and (decl-formal? fp)
                                     (member fp dfmls :test #'same-id)))
                            fparams))
            (assert (every #'(lambda (fp) (eq (associated-decl fp) adecl))
                           (cdr fparams)))
            (let* ((mi (mk-modname (id (current-theory)) nil nil nil (mk-dactuals dfmls)
                                   (current-declaration)))
                   (nex (subst-mod-params ex mi (current-theory) adecl)))
	      (break "maybe-instantiate-from-decl-formals*")
              (when (fully-instantiated? nex)
                (setf (resolutions ex) (resolutions nex)
                      (actuals ex) (actuals nex)
                      (dactuals ex) (dactuals nex)
                      (free-parameters ex) 'unbound)
                (assert (fully-instantiated? ex))
                ex)))
          (maybe-instantiate-from-decl-formals* (cdr reses) ex)))))

;;; set-type for types

(defmethod set-type* :around ((te type-expr) expected)
  (declare (ignore expected))
  (call-next-method)
  (when (print-type te)
    (unless (place (print-type te))
      (setf (place (print-type te)) (place te)))
    (let ((*generate-tccs* 'none))
      (set-type* (print-type te) nil)))
  (assert (fully-typed? (print-type te)))
  (assert (fully-instantiated? (print-type te))))

(defmethod set-type* ((te type-name) expected)
  (declare (ignore expected))
  (assert (resolution te))
  (when (and (or (actuals te) (dactuals te))
             (not (or (actuals (module-instance te))
                      (dactuals (module-instance te)))))
    (if (same-id (module-instance te) (current-theory))
        (unless (every #'(lambda (fml act)
                           (let ((aname (if (type-value act)
                                            (and (type-name? (type-value act))
                                                 (type-value act))
                                            (and (name-expr? (expr act))
                                                 (expr act)))))
                             (same-declaration fml aname)))
                       (formals-sans-usings (current-theory)) (actuals te))
          (type-error te
            "May not instantiate current theory except with corresponding formals"))
        (type-error te
          "May not specify actuals for this type name")))
;  (unless (or (same-id (module-instance te) (current-theory))
;             (from-prelude? (get-theory (module-instance te))))
;    (pushnew (module-instance te)
;            (instances-used (current-theory))
;            :test #'tc-eq))
  (when (or (actuals (module-instance te))
            (dactuals (module-instance te)))
    (setf (module-instance (resolution te))
          (set-type-actuals-and-maps te (module (declaration (resolution te))))))
  #+pvsdebug (fully-typed? te)
  (when (and (typep (type (resolution te)) 'type-name)
             (adt (type (resolution te))))
    (unless (and (boundp '*adt-type-name-pending*)
                 (memq te (assq (id te) *adt-type-name-pending*)))
      (let ((adt (adt (type (resolution te)))))
        (change-class te 'adt-type-name
          :adt adt
          :single-constructor? (singleton? (constructors adt))))))
  (unless (or *dont-worry-about-full-instantiations*
              (fully-instantiated? te))
    (maybe-instantiate-from-decl-formals te)
    (unless (fully-instantiated? te)
      (type-error te
        "Could not determine the full theory instance for ~a~
       ~%  Theory instance: ~a"
        te (full-name (module-instance te))))))

(defmethod set-type* ((te type-application) expected)
  (declare (ignore expected))
  (let ((typeslist (make-formals-type-app
                      (subst-mod-params (formals (declaration (type te)))
                                        (module-instance
                                         (resolution (type te)))
                                        (module (declaration (type te)))
                                        (current-declaration)))))
    (set-type-for-application-parameters (parameters te) (car typeslist))
    (set-type* (type te) nil)))

(defmethod set-type* ((te subtype) expected)
  ;;   (when (print-type te)
  ;;     (let ((*generate-tccs* 'none))
  ;;       (set-type* (print-type te) expected)))
  (let ((*set-type-subtype* (if (place te) te *set-type-subtype*)))
    (when (supertype te)
      (set-type* (supertype te) expected))
    (when (predicate te)
      (if (and (type (predicate te))
	       (fully-instantiated? (predicate te)))
          (when (eq *generate-tccs* 'all)
            (check-for-tccs (predicate te) (type (predicate te))))
          (set-type* (predicate te)
                     (mk-funtype (list (typecheck* (supertype te) nil 'type nil))
				 *boolean*))))))

(defmethod set-type* ((te expr-as-type) expected)
  (declare (ignore expected))
  (assert (type (expr te)))
  ;;(assert (funtype? (find-supertype (type (expr te)))))
  (set-type* (expr te) (type (expr te)))
  (assert (fully-instantiated? (expr te)))
  ;; expr-as-type should only be a print-type, never a canonical type
  ;; hence the following should not be used, and causes problems.
  ;; (unless (supertype te)
  ;;   (let* ((dtype (domain (find-supertype (type (expr te)))))
  ;; 	   (id (make-new-variable '|x| (cons (expr te) dtype)))
  ;; 	   (bd (typecheck* (mk-bind-decl id dtype) nil nil nil))
  ;; 	   (var (mk-name-expr id nil nil (make-resolution bd nil dtype)))
  ;; 	   (lex (make!-lambda-expr (list bd) (make!-application (expr te) var))))
  ;;     ;; E.g., if "p: [T -> bool]", then dtype for "(p)" is "T"
  ;;     (assert (fully-instantiated? dtype))
  ;;     (assert (fully-instantiated? lex))
  ;;     (setf (supertype te) dtype)
  ;;     (setf (predicate te) lex)))
  )

(defmethod set-type* ((te funtype) expected)
  (with-slots (domain range) te
    (set-type* domain expected)
;;     (if (dep-binding? domain)
;;      (let ((bd (make-bind-decl (id domain) (type domain))))
;;        (let ((*generate-tccs* 'none))
;;          (set-type* range expected))
;;        (let ((*tcc-conditions* (cons bd *tcc-conditions*)))
;;          (check-for-tccs (substit range (acons domain bd nil)) expected)))
;;      (set-type* range expected))
    (set-type* range expected)))

(defmethod set-type* ((te dep-binding) expected)
  (set-type* (type te) expected))

(defmethod set-type* ((te tupletype) expected)
  (mapc #'(lambda (ty) (set-type* ty expected)) (types te)))

(defmethod set-type* ((te cotupletype) expected)
  (mapc #'(lambda (ty) (set-type* ty expected)) (types te)))

(defmethod set-type* ((te recordtype) expected)
  (declare (ignore expected))
  (set-type-fields (fields te)))

(defmethod set-type* ((te struct-sub-recordtype) expected)
  (declare (ignore expected))
  (set-type-fields (fields te)))

(defun set-type-fields (field-decls)
  (when field-decls
    (set-type* (declared-type (car field-decls)) nil)
    (let ((*bound-variables* (cons (car field-decls) *bound-variables*)))
      (set-type-fields (cdr field-decls)))))

(defmethod set-type* ((te struct-sub-tupletype) expected)
  (declare (ignore expected))
  (set-type-types (types te)))

(defun set-type-types (types)
  (when types
    (set-type* (car types) nil)
    (let ((*bound-variables*
           (if (dep-binding? (car types))
               (cons (car types) *bound-variables*)
               *bound-variables*)))
      (set-type-types (cdr types)))))

(defmethod set-type* ((te type-extension) expected)
  (declare (ignore expected))
  (set-type* (type te) nil)
  (set-type* (extension te) nil))

(defmethod essentially-recordtype? ((te recordtype))
  t)

(defmethod essentially-recordtype? ((te type-name))
  (essentially-recordtype? (type (resolution te))))

(defmethod essentially-recordtype? ((te type-extension))
  (essentially-recordtype? (type te)))

(defmethod essentially-recordtype? ((te type-expr))
  nil)


;;; instantiate-from

(defun instantiate-from (type expected expr)
  #+pvsdebug (assert (fully-instantiated? expected))
  (or (find-parameter-instantiation type expected)
      (if *dont-worry-about-full-instantiations*
          type
          (type-error expr
            "Could not determine the full theory instance for ~a~
               ~:[~;~%  May be a problem with splitting dependent types~]~
               ~@[~%  Theory instance: ~a~]"
            expr
            (and (typep expr 'application)
                 (some #'(lambda (a)
                           (some #'(lambda (aty)
                                     (and (typep (find-supertype aty)
                                                 '(or funtype tupletype))
                                          (dependent? (find-supertype aty))))
                                 (ptypes a)))
                       (arguments expr)))
            (when (typep expr 'name-expr)
              (full-name (module-instance expr)))))))

(defun find-parameter-instantiation (type expected)
  (let* ((bindings (tc-match expected type
			     (instantiate-operator-bindings
			      (free-params type)))))
    (when (every #'cdr bindings)
      (instantiate-operator-from-bindings type bindings))))

(defun free-formals (obj)
  (let ((frees nil))
    (mapobject #'(lambda (ex)
                   (when (and (typep ex '(or name-expr type-name))
                              (resolution ex))
                     (mapc #'(lambda (act)
                               (when (free-actual act)
                                 (pushnew (act-value act) frees
                                          :test #'tc-eq)))
                           (actuals (module-instance ex)))))
               obj)
    frees))

(defun act-value (actual)
  (or (type-value actual)
      (expr actual)))

(defun free-actual (act)
  (let ((texp (or (type-value act)
                  (expr act))))
    (and (typep texp 'name)
         (typep (declaration texp) 'formal-decl)
         (not (eq (module (declaration texp)) (current-theory))))))

;(defun find-modinsts-matching (obj modids)
;  (let ((modinsts nil))
;    (mapobject #'(lambda (ex)
;                  (when (and (typep ex '(or name-expr type-name))
;                             (memq (id (module-instance ex)) modids))
;                    (pushnew (module-instance ex) modinsts
;                             :test #'tc-eq)))
;              obj)
;    modinsts))

(defun subst-for-formals (obj alist)
  (gensubst obj
    #'(lambda (ex) (subst-for-formals! ex alist))
    #'(lambda (ex) (subst-for-formals? ex alist))))

(defmethod subst-for-formals? ((ex name) alist)
  (assq (declaration ex) alist))

(defmethod subst-for-formals? ((ex subtype) alist)
  (and (type-name? (print-type ex))
       (assq (declaration (print-type ex)) alist)))

(defmethod subst-for-formals? ((ex modname) alist)
  (let ((mod (or (and (resolution ex)
		      (declaration ex))
		 (get-theory ex)
		 ;; Try again, but allow libraries to be searched
		 (let ((*current-context* nil))
		   (get-theory ex)))))
    (assert mod)
    (and (null (actuals ex))
         (formals-sans-usings mod)
         (every #'(lambda (fm) (assq fm alist))
                (formals-sans-usings mod)))))

(defmethod subst-for-formals? ((res resolution) alist)
  (let* ((mi (module-instance res))
         (th (or (module (declaration res))
                 (get-theory mi))))
    (or (typep (declaration res) 'field-decl)
        (and (null (actuals mi))
             (formals-sans-usings th)
             (every #'(lambda (fm) (assq fm alist))
                    (formals-sans-usings th)))
        (and (null (dactuals mi))
             (decl-formals (declaration res))
             (every #'(lambda (fm) (assq fm alist))
                    (decl-formals (declaration res)))))))

(defmethod subst-for-formals? (ex alist)
  (declare (ignore ex alist))
  nil)

(defmethod subst-for-formals! ((ex name) alist)
  (copy (cdr (assq (declaration ex) alist))))

(defmethod subst-for-formals! ((ex subtype) alist)
  (copy (cdr (assq (declaration (print-type ex)) alist))))

(defmethod subst-for-formals! ((ex modname) alist)
  (let ((mod (get-theory ex)))
    (copy ex
      :actuals (mapcar #'(lambda (fm)
                           (let ((exp (cdr (assq fm alist))))
                             (mk-actual exp)))
                       (formals-sans-usings mod)))))

(defmethod subst-for-formals! ((res resolution) alist)
  (let* ((th (module (declaration res)))
         (thinst (if (and (null (library (module-instance res)))
                          (lib-datatype-or-theory? th))
                     (lcopy (module-instance res)
                       :library (get-library-id (context-path th)))
                     (module-instance res)))
         (mi (subst-for-formals thinst alist))
         (decl (declaration res))
         (type (when (typep decl 'field-decl)
                 ;; Can't do this in general, or it loops - just let
                 ;; make-resolution take care of it
                 (subst-for-formals (type res) alist)))
         (dacts (cond ((dactuals mi)
                       (subst-for-formals? (dactuals mi) alist))
                      ((decl-formals decl)
                       (mapcar #'(lambda (fm)
                                   (let ((exp (cdr (assq fm alist))))
                                     (if exp
                                         (mk-actual exp)
                                         (break "Shouldn't happen"))))
                         (decl-formals decl)))))
         (dmi (if dacts
                  (change-class (copy mi) 'declparam-modname
                    :dactuals dacts :from-decl decl)
                  (lcopy mi :dactuals dacts))))
    (make-resolution decl dmi type)))

(defun push-tcc-conditions (conditions tcc-conditions)
  (if (null conditions)
      tcc-conditions
      (push-tcc-conditions (cdr conditions)
                          (push-tcc-condition (car conditions)
                                             tcc-conditions))))

(defun push-tcc-condition (condition tcc-conditions)
  (let ((conds (and!+ condition)))
    (push-tcc-condition* conds tcc-conditions)))

(defun push-tcc-condition* (conds tcc-conditions)
  (cond ((null conds)
         tcc-conditions)
        ((or (tc-eq (car conds) *true*)
             (member (car conds) tcc-conditions :test #'tc-eq))
         (push-tcc-condition* (cdr conds) tcc-conditions))
        (t (push-tcc-condition* (cdr conds)
				(cons (car conds) tcc-conditions)))))

(defun set-place-from (expr origin reason)
  (assert (place origin))
  (setf (place expr)
	(concatenate 'vector
	  (subseq (place origin) 0 4)
	  (list origin reason))))
