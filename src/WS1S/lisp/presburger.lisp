(in-package :pvs)

(defstep presburger (&optional (fnums *))
  (then* (presburger-to-ws1s :fnums fnums)
	 (ws1s :fnums fnums))
  "Decision procedure for Presburger arithmetic by
   reduction to WS1S."
  "Presburger arithmetic.")

(addrule 'presburger-to-ws1s nil ((fnums *))
    (presburger-to-ws1s-step fnums)
    "Translating Presburger formulas into WS1S.")

(defun presburger-to-ws1s-step (fnums)
  #'(lambda (ps)
      (let* ((sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(multiple-value-bind (signal newform)
	    (presburger-to-ws1s-sforms selected-sforms)
	  (case signal
	    (! (values '! nil))
	    (X (values 'X (current-goal ps)))
	    (? (values '? (list
			   (lcopy (current-goal ps)
			     's-forms (cons newform remaining-sforms))))))))))

(defun presburger-to-ws1s-sforms (sforms)
  (let* ((fmla (make!-disjunction* (mapcar #'formula sforms)))
	 (newfmla (presburger-fmla-to-ws1s fmla)))
    (if (null newfmla)
	(values 'X nil)
      (let ((new-sform (unless (or (tc-eq fmla newfmla)
				   (tc-eq newfmla *false*)
				   (and (negation? newfmla)
					(tc-eq (args1 newfmla) *true*)))
			 (make-instance 's-formula 'formula newfmla))))
	(if new-sform
	    (values '? new-sform)
	  (values 'X nil))))))

;; Translation from Presburger arithmetic to WS1S formulas. 
;; Returns [nil] if the argument is not translatable. As a side
;; effect [*current-context*] may be updated to include new Skolem
;; constants.

(defun presburger-fmla-to-ws1s (fmla)
  (catch 'not-a-presburger-fmla
    (let ((*boundvars*))               ; context of bound variables 
      (declare (special *boundvars*))
      (presburger-fmla-to-ws1s* fmla nil))))


;; Translating Presburger formulas

(defmethod presburger-fmla-to-ws1s* ((fmla expr) subst)
  (presburger-atom-to-ws1s* fmla subst))

(defmethod presburger-fmla-to-ws1s* ((fmla negation) subst)
  (multiple-value-bind (newargs1 newsubst)
      (presburger-fmla-to-ws1s* (args1 fmla) subst)
    (values (make!-negation newargs1) newsubst)))

(defmethod presburger-fmla-to-ws1s* ((fmla disequation) subst)
  (presburger-fmla-to-ws1s*
   (make!-negation (make!-equation (args1 fmla) (args2 fmla)))
   subst))

(defmethod presburger-fmla-to-ws1s* ((fmla conjunction) subst)
  (multiple-value-bind (newargs1 newargs2 newsubst)
      (presburger-binary-fmla-to-ws1s* fmla subst)
    (values (make!-conjunction newargs1 newargs2) newsubst)))

(defmethod presburger-fmla-to-ws1s* ((fmla disjunction) subst)
  (multiple-value-bind (newargs1 newargs2 newsubst)
      (presburger-binary-fmla-to-ws1s* fmla subst)
    (values (make!-disjunction newargs1 newargs2) newsubst)))

(defmethod presburger-fmla-to-ws1s* ((fmla implication) subst)
  (multiple-value-bind (newargs1 newargs2 newsubst)
      (presburger-binary-fmla-to-ws1s* fmla subst)
    (values (make!-implication newargs1 newargs2) newsubst)))
  
(defmethod presburger-fmla-to-ws1s* ((fmla iff-or-boolean-equation) subst)
  (multiple-value-bind (newargs1 newargs2 newsubst)
      (presburger-binary-fmla-to-ws1s* fmla subst)
    (values (make!-iff newargs1 newargs2) newsubst)))

(defun presburger-binary-fmla-to-ws1s* (fmla subst)
  (multiple-value-bind (newargs1 newsubst1)
      (presburger-fmla-to-ws1s* (args1 fmla) subst)
    (multiple-value-bind (newargs2 newsubst2)
	(presburger-fmla-to-ws1s* (args2 fmla) newsubst1)
      (values newargs1 newargs2 newsubst2))))

(defmethod presburger-fmla-to-ws1s* :around ((fmla quant-expr) subst)
  (if (bindings fmla)
      (call-next-method)
    (presburger-fmla-to-ws1s* (expression fmla) subst)))

(defmethod presburger-fmla-to-ws1s* ((fmla exists-expr) subst)
  (declare (special *boundvars*))
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fmla) :exclude *naturalnumber*)
    (if (some #'(lambda (type) (not (tc-eq type *naturalnumber*))) types)
	(call-next-method)
      (progn
	(setf *boundvars* (append vars *boundvars*))
	(multiple-value-bind (newbndngs newvars)
	    (presburger-fresh-bindings (mapcar #'id vars) (fset-of-nats))
	  (multiple-value-bind (newbody newsubst)
	      (presburger-fmla-to-ws1s*
	       (make!-conjunction (make!-conjunction* preds) (expression fmla))
	       (append (pairlis vars newvars) subst))
	    (values (make!-exists-expr newbndngs newbody)
		    newsubst)))))))

(defmethod presburger-fmla-to-ws1s* ((fmla forall-expr) subst)
  (presburger-fmla-to-ws1s*
   (make!-negation
    (make!-exists-expr (bindings fmla) (make!-negation (expression fmla))))
   subst))

;; Translating Presburger atoms

(defmethod presburger-atom-to-ws1s* ((atom expr) subst)
  (declare (special *boundvars*))
  (let ((lookup (assoc atom subst :test #'tc-eq)))
    (cond (lookup
	   (values (cdr lookup) subst))
	  ((some #'(lambda (var)             ; shielding
		     (occurs-in var atom))
		 *boundvars*)
	   (throw 'not-a-presburger-fmla nil))
	  (t
	   (let ((skoconst (presburger-fresh-skolem-const (presburger-name atom) *boolean*)))
	     (values skoconst (acons atom skoconst subst)))))))
	    
(defmethod presburger-atom-to-ws1s* ((atom equation) subst)
  (presburger-binary-atom atom #'make!-equation subst))

(defmethod presburger-atom-to-ws1s* ((atom application) subst)
  (let* ((op (operator atom)))
    (cond ((tc-eq op (less-operator))
	   (presburger-binary-atom atom #'presburger-lt subst))
	  ((tc-eq op (lesseq-operator))
	   (presburger-binary-atom atom #'presburger-le subst))
	  ((tc-eq op (greater-operator))
	   (presburger-binary-atom atom #'(lambda (p q) (presburger-lt q p)) subst))
	  ((tc-eq op (greatereq-operator))
	   (presburger-binary-atom atom #'(lambda (p q) (presburger-le q p)) subst))
	  (t
	   (call-next-method)))))
	  
(defun presburger-binary-atom (atom op subst)
  (multiple-value-bind (bndngs1 cnstrnts1 var1 subst1)
      (presburger-term-to-ws1s* (args1 atom) subst)
    (multiple-value-bind (bndngs2 cnstrnts2 var2 subst2)
	(presburger-term-to-ws1s* (args2 atom) subst1)
      (let* ((bndngs (append bndngs1 bndngs2))
	     (cnstrnts (make!-conjunction* (append cnstrnts1 cnstrnts2)))
	     (body (make!-conjunction cnstrnts (funcall op var1 var2))))
	(values (if bndngs (make-exists-expr bndngs body) body)
		subst2)))))


;; Translating Presburger terms

(defmethod presburger-term-to-ws1s* :around ((term expr) subst)
  (if (natural-expr? term)
      (call-next-method)
    (throw 'not-a-presburger-fmla nil)))

(defmethod presburger-term-to-ws1s* ((term expr) subst)
  (declare (special *boundvars*))
  (let ((lookup (assoc term subst :test #'tc-eq)))
    (cond (lookup
	   (values nil nil (cdr lookup) subst))
	  ((some #'(lambda (var)
		     (occurs-in var term))
		 *boundvars*)
	   (throw 'not-a-presburger-fmla nil))
	  (t
	   (let* ((name (presburger-name term))
		  (skoconst (presburger-fresh-skolem-const name (fset-of-nats))))
	     (values nil nil skoconst (acons term skoconst subst)))))))
	    
(defmethod presburger-term-to-ws1s* ((term number-expr) subst)
  (values nil nil (presburger-unsigned (number term)) subst))

(defmethod presburger-term-to-ws1s* ((term application) subst)
  (let ((op (operator term)))
    (cond ((tc-eq op (plus-operator))
	   (presburger-binary-term-to-ws1s* (args1 term) (args2 term)
					    #'presburger-add subst))
	  ((tc-eq op (minus-operator))
	   (presburger-binary-term-to-ws1s* (args1 term) (args2 term)
					    #'presburger-sub subst))
	  ((tc-eq op (times-operator))
	   (cond ((natural-number-expr? (args1 term))
		  (presburger-unary-term-to-ws1s* (args2 term)
						  #'(lambda (arg res)
						      (presburger-multq
						       (number (args1 term)) arg res))
						  subst))
		 ((natural-number-expr? (args2 term))
		  (presburger-unary-term-to-ws1s* (args1 term)
						  #'(lambda (arg res)
						      (presburger-multq
						       (number (args2 term)) arg res))
						  subst))
		 (t ;; nonlinear multiplication
		  (call-next-method))))
	  (t
	   (call-next-method)))))

(defun presburger-binary-term-to-ws1s* (arg1 arg2 ternary-rel subst)
  (multiple-value-bind (bndngs1 cnstrnts1 var1 subst1)
      (presburger-term-to-ws1s* arg1 subst)
    (multiple-value-bind (bndngs2 cnstrnts2 var2 subst2)
	(presburger-term-to-ws1s* arg2 subst1)
      (multiple-value-bind (newbndng newvar)
	  (presburger-fresh-binding 'new (fset-of-nats))
	(assert (and var1 var2 newvar))
	(let ((cnstrnt (funcall ternary-rel var1 var2 newvar)))
	  (values (cons newbndng (append bndngs1 bndngs2))
		  (cons cnstrnt (append cnstrnts1 cnstrnts2))
		  newvar
		  subst2))))))

(defun presburger-unary-term-to-ws1s* (arg1 binary-rel subst)
  (multiple-value-bind (bndngs1 cnstrnts1 var1 subst1)
      (presburger-term-to-ws1s* arg1 subst)
    (multiple-value-bind (newbndng newvar)
	(presburger-fresh-binding 'new (fset-of-nats))
      (assert (and var1 newvar))
      (let ((cnstrnt (funcall binary-rel var1 newvar)))
	(values (cons newbndng bndngs1)
		(cons cnstrnt cnstrnts1)
		newvar
		subst1)))))

;; Return a name for an expression

(defmethod presburger-name ((expr expr))
  (intern (format nil "@@@~a@@@" expr)))

(defmethod presburger-name ((expr name-expr))
  (id expr))

;; Return a binding and a corresponding variable with
;; unique identifier 

(defun presburger-fresh-binding (id type)
  (assert (symbolp id))
  (let* ((newid (presburger-fresh-id id))
	 (bndng (make!-bind-decl newid type))
	 (var (make-variable-expr bndng)))
    (values bndng var)))

(defun presburger-fresh-bindings (ids type &optional bndngs vars)
  (assert (every #'symbolp ids))
  (if (null ids)
      (values (nreverse bndngs) (nreverse vars))
    (multiple-value-bind (bndng var)
	(presburger-fresh-binding (car ids) type)
      (let ((newbndngs (cons bndng bndngs))
	    (newvars (cons var vars)))
	(presburger-fresh-bindings (cdr ids) type newbndngs newvars)))))

(let ((*counter* 0))

  (defun presburger-fresh-id (id)
    (declare (special *current-context*))
    (let ((newid (intern (format nil "~a@~d" id *counter*))))
      (setf *counter* (1+ *counter*))
      (if (declared? newid *current-context*)
	  (presburger-fresh-id id)
	newid)))

  (defun presburger-fresh-id-counter-reset ()
    (setf *counter* 0)))

;; Fresh Skolem constant

(defun presburger-fresh-skolem-const (id type)
  (declare (special *current-context*))
  (let ((fresh-id (presburger-fresh-id id)))
    (setf (declarations-hash *current-context*)
	  (copy (declarations-hash *current-context*)))
    (put-decl (make-instance
	       'skolem-const-decl
	       'id fresh-id
	       'type type
	       'module (module *current-context*))
	      (declarations-hash *current-context*))
    (typecheck
     (mk-name-expr fresh-id)
     :expected type 
     :context *current-context*)))


;; Encoding of the relation [P + Q = R] using a carry-lookahead adder
;;      EXISTS (C: finite_set[nat]): NOT(C(0)) AND
;;        FORALL (t: nat):
;;            (C(t + 1) = ((P(t) AND Q(t)) OR (P(t) AND C(t)) OR (Q(t) AND C(t)))) AND
;;         (R(t) = P(t) = Q(t) = C(t))

(defun presburger-add (p q r)
  (assert (and p q r))
  (multiple-value-bind (cbind cvar)
      (presburger-fresh-binding 'c (fset-of-nats))
    (multiple-value-bind (tbind tvar)
	(presburger-fresh-binding 't *naturalnumber*)
      (multiple-value-bind (sbind svar)
	  (presburger-fresh-binding 's *naturalnumber*)
	(let ((pt (make!-application p tvar))
	      (qt (make!-application q tvar))
	      (rt (make!-application r tvar))
	      (ct (make!-application cvar tvar)))
	  (make!-exists-expr
	   (list cbind)
	   (make!-conjunction
	    (make!-negation (make!-application cvar (make!-number-expr 0)))
	    (make!-forall-expr
	     (list tbind)
	     (make!-exists-expr
	      (list sbind)
	      (make!-conjunction*
	       (list (make!-equation svar (make!-plus tvar (make!-number-expr 1)))
		     (make!-equation
		      (make!-application cvar svar)
		      (make!-disjunction*
		       (list (make!-conjunction pt qt)
			     (make!-conjunction pt ct)
			     (make!-conjunction qt ct))))
		     (make!-equation rt
				     (make!-equation pt
						     (make-equation qt ct))))))))))))))


;; [P - Q = R] iff [R + Q = P]

(defun presburger-sub (p q r)
  (presburger-add r q p))

;; Encoding of arithmetic inequalities
;;    <=(P, Q): boolean = EXISTS R: P + R = Q;
;;     <(P, Q): boolean = (P <= Q) AND (P /= Q)

(defun presburger-le (p q)
  (multiple-value-bind (bndng var)
      (presburger-fresh-binding 'r (fset-of-nats))
    (make!-exists-expr (list bndng)
		       (presburger-add p q var))))

(defun presburger-lt (p q)
  (make!-conjunction (presburger-le p q)
		     (make!-disequation p q)))

;; Unsigned interpretation

(defun presburger-unsigned (n &optional (i 0))
  (assert (and (integerp n) (>= n 0)))
  (cond ((= n 0)
	 (emptyset-operator))
	((oddp n)
	 (make!-application
	  (add-operator)
	  (make!-number-expr i)
	  (presburger-unsigned (floor (/ (1- n) 2)) (1+ i))))
	(t
	 (presburger-unsigned (/ n 2) (1+ i)))))

;; Encoding of increment [ p + 1 = q]

(defun presburger-incr (p q)
  (presburger-add p (presburger-unsigned 1) q))

;; Encoding of linear multiplication
;;   multq(0,p,q) = 0 = q
;;   multq(1,p,q) = (p = q)
;;   multq(2,p,q) = add(p,p,q)
;;   multq(k,p,q) = EXISTS r: multq(k-1,p,r) & add(r,p,q)

(defun presburger-multq (k p q)
  (cond ((= k 0)
	 (presburger-zero? q))
	((= k 1)
	 (make!-equation p q))
	((= k 2)
	 (presburger-add p p q))
	(t
	 (multiple-value-bind (bndng r)
	     (presburger-fresh-binding 'r (fset-of-nats))
	   (make!-exists-expr
	    (list bndng)
	    (make!-conjunction
	     (presburger-multq (1- k) p r)
	     (presburger-add r p q)))))))

