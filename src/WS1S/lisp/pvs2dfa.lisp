;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makes.lisp -- 
;; Author          : Harald Ruess
;; Created On      : Tue Jan  4 23:17:39 1999
;; Last Modified By: Harald Ruess
;; Last Modified On: Thu Nov  5 15:11:36 2001
;; Update Count    : 27
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; Translation of PVS formulas to deterministic finite automata;

; Messages

(defmacro ws1s-msg (str &rest args)
  `(locally (declare (special *verbose*))
      (when *verbose*
        (format t ,str ,@args))))

(defun abort-translation (expr)
  (ws1s-msg (format nil  "Not translatable: ~a" expr))
  (throw 'not-ws1s-translatable nil))


;; WS1S type declarations

(defun ws1s-types ()
  (list *boolean* *naturalnumber* (fset-of-nats)))
  
(defun ws1s-type? (type)
  (member type (ws1s-types) :test #'tc-eq))

(defun first-order? (expr)
  (some #'(lambda (type)
	    (subtype-of? type *naturalnumber*))
	(judgement-types+ expr)))

(defun second-order? (expr)
  (some #'(lambda (type)
	    (subtype-of? type (fset-of-nats)))
	(judgement-types+ expr)))

(defun level (expr)
  (cond ((tc-eq (type expr) *boolean*) 0)
	((first-order? expr) 1)
	((second-order? expr) 2)))


;; Generating new indices

(let ((*index* 0))

  (defun fresh-index ()
    (setf *index* (1+ *index*)))

  (defun reset-index ()
    (setf *index* 0)))

;; Lookup from substitutions for both bound and free variables

(defun lookup (expr bound free)
  (cdr (or (assoc expr bound :test #'tc-eq)
	   (assoc expr free :test #'tc-eq))))

(defun var1? (expr bound free)
  (and (first-order? expr)
       (or (lookup expr bound free)
	   (name-expr? expr))))

(defun lookup-var1 (expr bound free)
  (let ((index (lookup expr bound free)))
    (if index
	(values index free)
      (let ((index (fresh-index)))
	(values index (acons expr index free))))))

(defun var2? (expr bound free)
  (and (second-order? expr)
       (or (lookup expr bound free)
	   (name-expr? expr))))

(defun lookup-var2 (expr bound free)
  (let ((index (lookup expr bound free)))
    (if index
	(values index free)
      (let ((index (fresh-index)))
	(values index (acons expr index free))))))


;; Translation of formulas has a formula argument, the currently set of bound variables,
;; and the current symbol table; the result is an automaton or nil if the formula is not
;; a translatable formula.

(defun fmla-to-dfa (fml)
  (catch 'not-ws1s-translatable
    (progn 
      (multiple-value-bind (dfa free)
	  (fmla-to-dfa* fml nil nil)
	(values (dfa-unrestrict dfa) free)))))
       
(defmethod fmla-to-dfa* ((fml expr) bound free)
  (atom-to-dfa* fml bound free))

(defmethod fmla-to-dfa* ((fml name-expr) bound free)
  (cond ((tc-eq fml *true*)
	 (values (dfa-true) free))
	((tc-eq fml *false*)
	 (values (dfa-false) free))
	(t
	 (call-next-method))))

(defmethod fmla-to-dfa* ((fml negation) bound free)
  (multiple-value-bind (dfa new-free)
      (fmla-to-dfa* (args1 fml) bound free)
    (values (dfa-negation dfa) new-free)))

(defmethod fmla-to-dfa* ((fml conjunction) bound free)
   (fmla-binary-connective-to-dfa* fml #'dfa-conjunction bound free))

(defmethod fmla-to-dfa* ((fml disjunction) bound free)
   (fmla-binary-connective-to-dfa* fml #'dfa-disjunction bound free))

(defmethod fmla-to-dfa* ((fml implication) bound free)
   (fmla-binary-connective-to-dfa* fml #'dfa-implication bound free))

(defmethod fmla-to-dfa* ((fml iff-or-boolean-equation) bound free)
   (fmla-binary-connective-to-dfa* fml #'dfa-iff bound free))

(defun fmla-binary-connective-to-dfa* (fml operator bound free)
  (multiple-value-bind (dfa1 free1)
      (fmla-to-dfa* (args1 fml) bound free)
    (multiple-value-bind (dfa2 free2)
	(fmla-to-dfa* (args2 fml) bound free1)
      (values (funcall operator dfa1 dfa2) free2))))

(defmethod fmla-to-dfa* ((fml disequation) bound free)
  (multiple-value-bind (dfa1 free1)
      (fmla-to-dfa* (make!-equation (args1 fml) (args2 fml))
		    bound
		    free)
    (values (dfa-negation dfa1) free1)))

(defmethod fmla-to-dfa* ((fml forall-expr) bound free)
  (fmla-to-dfa*
   (make!-negation
    (make!-exists-expr (bindings fml)
		       (make!-negation (expression fml))))
   bound
   free))

(defmethod fmla-to-dfa* ((fml exists-expr) bound free)
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fml) :exclude (ws1s-types))
    (if (some #'(lambda (type)
		   (not (member type (ws1s-types) :test #'tc-eq)))
	       types)
	(call-next-method)
      (let ((indices (loop for var in vars collect (fresh-index)))
	    (levels (loop for type in types
			  collect (cond ((tc-eq type *boolean*) 0)
					((tc-eq type *naturalnumber*) 1)
					((tc-eq type (fset-of-nats)) 2)
					(t (break))))))
	(multiple-value-bind (dfa-body new-free)
	    (fmla-to-dfa* (make!-conjunction (make!-conjunction* preds)
					     (expression fml))
			  (append (pairlis vars indices) bound)
			  free)
	  (values (dfa-exists* levels indices dfa-body)
		  new-free))))))

; Each atom is translated by first translating its arguments. Translation
; of an arithmetic arguments yields an index, a set of variables to be
; quantified existentially, a set of constraints, and a set of new substitutions.

(defmethod atom-to-dfa* ((atom expr) bound free)
  (declare (ignore bound)
	   (ignore free))
  (ws1s-msg (format nil "Not translatable: ~a" atom))
  (throw 'not-ws1s-translatable nil))

(defmethod atom-to-dfa* ((atom name-expr) bound free)
  (let ((index (lookup atom bound free)))
    (if index
	(values (dfa-var0 index) free)
      (let ((index (fresh-index)))
	(values (dfa-var0 index)
		(acons atom index free))))))

(defmethod atom-to-dfa* ((atom equation) bound free)
  (let ((lhs (args1 atom))
	(rhs (args2 atom)))
    (cond ((var1? lhs bound free)
	   (multiple-value-bind (i new-free)
	       (lookup-var1 lhs bound free)
	     (var1-eq-term1-to-dfa* i rhs bound new-free)))
	  ((var1? rhs bound free)
	   (multiple-value-bind (i new-free)
	       (lookup-var1 rhs bound free)
	     (var1-eq-term1-to-dfa* i lhs bound new-free)))
	  ((var2? lhs bound free)
	   (multiple-value-bind (i new-free)
	       (lookup-var2 lhs bound free)
	     (var2-eq-term2-to-dfa* i rhs bound new-free)))
	  ((var2? rhs bound free)
	   (multiple-value-bind (i new-free)
	       (lookup-var2 rhs bound free)
	     (var2-eq-term2-to-dfa* i lhs bound new-free)))
	  (t
	   (call-next-method)))))

(defun var1-eq-term1-to-dfa* (i term1 bound free)
  (cond ((number-expr? term1)                               ; p_i = n
	 (values (dfa-const (number term1) i) free))
	((var1? term1 bound free)                           ; p_i = p_j
	 (multiple-value-bind (j new-free)
	     (lookup-var1 term1 bound free)
	   (values (dfa-eq1 i j) new-free)))
	((and (tc-eq (operator term1) (plus-operator))      ; p_i = p_j + n
	      (var1? (args1 term1) bound free)
	      (number-expr? (args2 term1)))
	 (multiple-value-bind (j new-free)
	     (lookup-var1 (args1 term1) bound free)
	   (values (dfa-plus1 i j (number (args2 term1))) new-free)))
	((and (tc-eq (operator term1) (plus-operator))      ; p_i = n + p_j
	      (var1? (args2 term1) bound free)
	      (number-expr? (args1 term1)))
	 (multiple-value-bind (j new-free)
	     (lookup-var1 (args2 term1) bound free)
	   (values (dfa-plus1 i j (number (args1 term1))) new-free)))
	((and (tc-eq (operator term1) (minus-operator))     ; p_i = p_j - n
	      (var1? (args1 term1) bound free)
	      (number-expr? (args2 term1)))
	 (multiple-value-bind (j new-free)
	     (lookup-var1 (args1 term1) bound free)
	   (values (dfa-minus1 i j (number (args2 term1))) new-free)))
	(t
	 (throw 'not-ws1s-translatable nil))))
  
  
(defun var2-eq-term2-to-dfa* (i term2 bound free)
  (cond ((tc-eq (emptyset-operator) term2)                    ; P_i = empty
	 (values (dfa-empty i) free))
	((var2? term2 bound free)                             ; P_i = P_j
	 (multiple-value-bind (j new-free)
	     (lookup-var2 term2 bound free)
	   (values (dfa-eq2 i j) new-free)))
	((tc-eq (union-operator) (operator term2))             ; P_i = P_j union P_k
	 (let ((lhs (args1 term2))
	       (rhs (args2 term2)))
	   (cond ((and (tc-eq (emptyset-operator) lhs)         ; P_i = empty union empty
		       (tc-eq (emptyset-operator) rhs))
		  (values (dfa-empty i) free))
		 ((and (tc-eq (emptyset-operator) lhs)         ; P_i = empty union P_k
		       (var2? rhs bound free))
		  (multiple-value-bind (k new-free)
		      (lookup-var2 rhs bound free)
		    (values (dfa-eq2 i k) new-free)))
		 ((and (tc-eq (emptyset-operator) rhs)         ; P_i = P_j union empty
		       (var2? lhs bound free))
		  (multiple-value-bind (j new-free)
		      (lookup-var2 lhs bound free)
		    (values (dfa-eq2 i j) new-free)))
		 ((and (var2? lhs bound free)
		       (var2? rhs bound free))
		  (multiple-value-bind (j new-free)                    
		      (lookup-var2 lhs bound free)
		    (multiple-value-bind (k new-new-free)
			(lookup-var2 rhs bound new-free)
		      (values (dfa-union i j k) new-new-free))))
		 (t
		  (throw 'not-ws1s-translatable nil)))))
	((tc-eq (intersection-operator) (operator term2))
	 (cond ((or (tc-eq (emptyset-operator) lhs)       ; P_i = _ inter empty, P_i = empty inter _  
		    (tc-eq (emptyset-operator) rhs)) 
		(values (dfa-empty i) free))
	       ((and (var2? (args1 term2) bound free)     ; P_i = P_j inter P_k
		     (var2? (args2 term2) bound free))
		(multiple-value-bind (j new-free)
		    (lookup-var2 (args1 term2) bound free)
		  (multiple-value-bind (k new-new-free)
		      (lookup-var2 (args2 term2) bound new-free)
		    (values (dfa-intersection i j k) new-new-free))))
	       (t
		(throw 'not-ws1s-translatable nil))))
	(t
	 (throw 'not-ws1s-translatable nil))))

(defmethod atom-to-dfa* ((fml application) bound free)
  (let ((op (operator fml))
	(args (arguments fml)))
    (cond ((= (length args) 1)                                  ; p_i in P_j
	   (term1-in-term2-to-dfa* (first args) op bound free))
	  ((tc-eq op (less-operator))                           ; p_i < p_j
	   (term1-binrel-term1-to-dfa* (args1 fml) (args2 fml) #'dfa-less bound free))
	  ((tc-eq op (lesseq-operator))                         ; p_i <= p_j
	   (term1-binrel-term1-to-dfa* (args1 fml) (args2 fml) #'dfa-lesseq bound free))
	  ((tc-eq op (greater-operator))                        ; p_i > p_j
	   (term1-binrel-term1-to-dfa* (args1 fml) (args2 fml) #'dfa-greater bound free))
	  ((tc-eq op (greatereq-operator))                      ; p_i >= p_j
	   (term1-binrel-term1-to-dfa* (args1 fml) (args2 fml) #'dfa-greatereq bound free)) 
	  (t
	   (call-next-method)))))

(defun term1-in-term2-to-dfa* (var1 var2 bound free)
  (cond ((tc-eq (emptyset-operator) var2)                ; x in emptyset
	 (values (dfa-false) free))
	((tc-eq (fullset-operator) var2)                 ; x in fullset
	 (values (dfa-true) free))
        ((and (natural-number-expr? var1)                ; n in P_j
	      (var2? var2 bound free))
	 (multiple-value-bind (j new-free)
	     (lookup-var2 var2 bound free)
	   (let* ((i (fresh-index))
		  (dfa (dfa-exists1 i
			(dfa-conjunction
			 (dfa-const (number var1) i)
			 (dfa-in i j)))))
	     (values dfa new-free))))
	((and (tc-eq (plus-operator) (operator var1))    ; (p_i + n) in P_j
	      (var1? (args1 var1))
	      (natural-number-expr? (args2 var1))
	      (var2? var2))
	 (multiple-value-bind (i new-free)
	     (lookup-var1 (args1 var1) bound free)
	   (multiple-value-bind (j new-new-free)
	       (lookup-var2 var2 new-free)
	     (let* ((k (fresh-index))
		    (dfa (dfa-exists1 k
				      (dfa-conjunction
				       (dfa-plus1 k i (number (args2 var1)))
				       (dfa-in k j)))))
	       (values dfa new-new-free)))))
        ((and (var2? var2 bound free)                    ; p_i in P_j
	      (var1? var1 bound free))
	 (multiple-value-bind (i new-free)
	     (lookup-var1 var1 bound free)
	   (multiple-value-bind (j new-new-free)
	       (lookup-var2 var2 bound new-free)
	     (values (dfa-in i j) new-new-free))))
	(t
	 (throw 'not-ws1s-translatable nil))))

(defun term1-binrel-term1-to-dfa* (arg1 arg2 binrel bound free)
  (cond ((and (natural-number-expr? arg1)                               ; n binrel p_j
	      (var1? arg2 bound free))
	 (const1-binrel-var1-to-dfa* (number arg1) arg2 binrel bound free))
	((and (natural-number-expr? arg2)                               ; n binrel p_j
	      (var1? arg1 bound free))
	 (const1-binrel-var1-to-dfa* (number arg2) arg1 binrel bound free))
        ((and (var1? arg1 bound free)
	      (var1? arg2 bound free))
	 (multiple-value-bind (i new-free)
	     (lookup-var1 arg1 bound free)
	   (multiple-value-bind (j new-new-free)
	       (lookup-var1 arg2 bound new-free)
	     (values (funcall binrel i j) new-new-free))))
	(t
	 (throw 'not-ws1s-translatable nil))))

(defun const1-binrel-var1-to-dfa* (const1 var1 binrel bound free)
  (assert (and (integerp const1) (>= const1 0)))
  (assert (var1? var1 bound free))
  (multiple-value-bind (j new-free)
      (lookup-var1 var1 bound free)
    (let* ((i (fresh-index))
	   (dfa (dfa-exists1 i
			     (dfa-conjunction (dfa-const const1 i)
					      (funcall binrel i j)))))
      (values dfa new-free))))









