(in-package :pvs)

;; Translation of PVS formulas to deterministic finite automata;
;;
;; typically used as follows:
;;    (unwind-protect
;;       (progn
;;         (fml-to-dfa-init)
;;         (.... (catch 'not-ws1s-translatable (fml-to-dfa ....)))
;;      (fml-to-dfa-init))

; Initialization

(defun fml-to-dfa-init ()
  (symtab-init)
  (fml-to-dfa-hash-init))

(defvar *fml-to-dfa-table* nil)

(defun fml-to-dfa-hash-init ()
  (if *fml-to-dfa-table*
      (clrhash *fml-to-dfa-table*)
    (setf *fml-to-dfa-table* (make-hash-table :test 'eq))))

; Translation of Formulas

(defun fml-to-dfa (fml)           
  (fml-to-dfa* fml))

(defmethod fml-to-dfa* :around (fml)
   (if (freevars fml)
       (call-next-method)
       (or (gethash fml *fml-to-dfa-table*)
	   (let ((dfa (call-next-method)))
	     (setf (gethash fml *fml-to-dfa-table*) dfa)
	     dfa))))
       
(defmethod fml-to-dfa* ((fml expr))
  (cond ((shielding? fml)
	 (error-format-if "~%Formula ~a is shielding. Giving up..." fml)
	 (throw 'not-ws1s-translatable nil))
	(t (let ((i (symtab-index fml)))
	     (error-format-if "~%New Boolean parameter for ~a" fml)
	     (dfa-var0 i)))))

(defmethod fml-to-dfa* ((fml name-expr))
  (cond ((tc-eq fml *true*)
	 (dfa-true-val))
	((tc-eq fml *false*)
	 (dfa-false-val))
	((var0? fml)              
	 (dfa-var0 (symtab-index fml)))
	(t
	 (error-format-if "Name ~a not translatable" fml)
	 (call-next-method))))

(defmethod fml-to-dfa* ((fml negation))
  (dfa-negation (fml-to-dfa* (args1 fml))))

(defmethod fml-to-dfa* ((fml conjunction))
  (let* ((lhs (args1 fml))
	 (rhs (args2 fml))
	 (a1 (fml-to-dfa lhs)))
    (cond ((dfa-true? a1)
	   (fml-to-dfa rhs))        
	  ((dfa-false? a1)
	   (dfa-false-val))            
	  (t (dfa-conjunction a1 (fml-to-dfa rhs))))))

(defmethod fml-to-dfa* ((fml disjunction))
  (let* ((lhs (args1 fml))
	 (rhs (args2 fml))
	 (a1 (fml-to-dfa lhs)))
    (cond ((dfa-false? a1)
	   (fml-to-dfa rhs)) 
	  ((dfa-true? a1)
	   (dfa-true-val))
	  (t (dfa-disjunction a1 (fml-to-dfa rhs))))))

(defmethod fml-to-dfa* ((fml implication))
  (let* ((lhs (args1 fml))
	 (rhs (args2 fml))
	 (a1 (fml-to-dfa lhs)))
    (if (dfa-false? a1)
	(dfa-true-val)
      (let ((a2 (fml-to-dfa rhs)))              
	(dfa-implication a1 a2)))))

(defmethod fml-to-dfa* ((fml branch))
  (let ((c (fml-to-dfa (condition fml))))
    (cond ((dfa-true? c)                      
	   (fml-to-dfa (then-part fml)))
	  ((dfa-false? c)                 
	   (fml-to-dfa (else-part fml)))
	  (t (let ((a1 (fml-to-dfa (then-part fml)))
		   (a2 (fml-to-dfa (else-part fml))))
	       (dfa-ite c a1 a2))))))
 
(defmethod fml-to-dfa* ((fml iff-or-boolean-equation))
  (dfa-equivalence (fml-to-dfa (args1 fml))
		   (fml-to-dfa (args2 fml))))

(defmethod fml-to-dfa* ((fml equation))
  (let ((lhs (args1 fml))
	(rhs (args2 fml)))
    (cond ((and (1st-order? lhs) (1st-order? rhs))
	   (process-binrel1 lhs rhs #'dfa-eq1))
	  ((and (2nd-order? lhs) (2nd-order? rhs))
	   (process-binrel2 lhs rhs #'dfa-eq2))
	  (t
	   (error-format-if "Equation ~a not translatable" fml)
	   (call-next-method)))))

(defmethod fml-to-dfa* ((fml disequation))
  (let ((lhs (args1 fml))
	(rhs (args2 fml)))
    (cond ((and (1st-order? lhs) (1st-order? rhs))
	   (dfa-negation (process-binrel1 lhs rhs #'dfa-eq1)))
	  ((and (2nd-order? lhs) (2nd-order? rhs))
	   (dfa-negation (process-binrel2 lhs rhs #'dfa-eq2)))
	  (t
	   (error-format-if "Disequation ~a not translatable" fml)
	   (call-next-method)))))

(defmethod fml-to-dfa* ((fml application))
  (let ((op (operator fml))
	(args (arguments fml)))
    (cond ((and (= (length args) 1)   ; membership
		(2nd-order? op)
		(1st-order? (first args)))
	   (process-binrel12 (first args) op #'dfa-in))
	  ((and (= (length args) 2)
		(1st-order? (first args))
		(1st-order? (second args)))
	   (cond ((is? op '|<| '|reals|)
		  (process-binrel1 (first args) (second args) #'dfa-less))
		 ((is? op '|>| '|reals|)
		  (process-binrel1 (second args) (first args) #'dfa-less))
		 ((is? op '|<=| '|reals|)
		  (process-binrel1 (first args) (second args) #'dfa-lesseq))
		 ((is? op '|>=| '|reals|)
		  (process-binrel1 (second args) (first args) #'dfa-lesseq))
		 (t
		  (error-format-if "1st-order application ~a not translatable" fml)
		  (call-next-method))))
	  (t
	   (error-format-if "Application ~a not translatable" fml)
	   (call-next-method)))))

(defmethod fml-to-dfa* ((fml forall-expr))
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fml) :exclude (ws1s-types))
    (unwind-protect
	(let ((indices (symtab-shadow* (reverse vars)))
	      (levels  (mapcar #'(lambda (type)
				   (cond ((tc-eq type *boolean*) 0)
					 ((tc-eq type *naturalnumber*) 1)
					 ((tc-eq type (fset-of-nats)) 2)
					 (t
					  (error-format-if "~%Type ~a not WS1S translatable in ~a"
							   type fml)
					  (throw 'not-ws1s-translatable))))
			 types)))
	  (dfa-forall* levels
		       indices
		       (dfa-implication (dfa-conjunction* (mapcar #'fml-to-dfa preds))
					(fml-to-dfa (expression fml))))) 
      (symtab-unshadow* (length vars)))))
 
(defmethod fml-to-dfa* ((fml exists-expr))
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fml) :exclude (ws1s-types))
    (unwind-protect
	(let ((indices (symtab-shadow* (reverse vars)))
	      (levels  (mapcar #'(lambda (type)
				   (cond ((tc-eq type *boolean*) 0)
					 ((tc-eq type *naturalnumber*) 1)
					 ((tc-eq type (fset-of-nats)) 2)
					 (t
					  (error-format-if "~%Type ~a not WS1S translatable in ~a"
							   type fml)
					  (throw 'not-ws1s-translatable))))
			 types)))
	  (dfa-exists* levels
		       indices
		       (dfa-conjunction* (cons (fml-to-dfa (expression fml))
					       (mapcar #'fml-to-dfa preds)))))
      (symtab-unshadow* (length vars)))))
 
;; Processing of Relations

(defun process-binrel1 (lhs rhs combine)
  (multiple-value-bind (i xs a)
      (nat-to-dfa lhs)
    (multiple-value-bind (j ys b)
	(nat-to-dfa rhs)
      (dfa-exists1* (union xs ys :test #'=)
	 (dfa-conjunction* (list (dfa-op combine i j) a b))))))
   
(defun process-binrel2 (lhs rhs combine)
  (multiple-value-bind (i xs a)
      (fset-to-dfa lhs)
    (multiple-value-bind (j ys b)
	(fset-to-dfa rhs)
      (dfa-exists2* (union xs ys :test #'=)
	 (dfa-conjunction* (list (dfa-op combine i j) a b))))))

(defun process-binrel12 (lhs rhs combine)
  (multiple-value-bind (i xs a)
      (nat-to-dfa lhs)
    (multiple-value-bind (j ys b)
	(fset-to-dfa rhs)
     (dfa-exists1* xs
       (dfa-exists2* ys
	 (dfa-conjunction* (list (dfa-op combine i j) a b)))))))


; Processing of terms representing finite set of naturals

(defun fset-to-dfa (trm)
  (fset-to-dfa* trm))

(defmethod fset-to-dfa* ((trm expr))
  (if (shielding? trm)
      (progn
	(error-format-if "~%2nd-order term ~a
                            is shielding and can thus not be abstracted. Giving up..." trm)
	(throw 'not-ws1s-translatable nil))
      (let ((i (symtab-index trm)))
	(error-format-if "~%New 2nd-order parameter for ~a" trm)
	(values i nil (dfa-true-val)))))

(defmethod fset-to-dfa* ((trm lambda-expr))
  (unless (= (length (bindings trm)) 1)
    (error-format-if "~a has more than one binder" trm)
    (call-next-method))
  (multiple-value-bind (x supertype preds)
      (destructure-binding (car (bindings trm)) :exclude (list *naturalnumber*))
    (unless (tc-eq supertype *naturalnumber*)
      (error-format-if "Binding ~a is not first-order" trm)
	(call-next-method))
    (let ((i (symtab-shadow x)))
      (unwind-protect              
	(let* ((j (symtab-new-index))
	       (a (dfa-forall1 i
			       (dfa-equivalence (dfa-op #'dfa-in i j)
						(dfa-conjunction*
						 (mapcar #'fml-to-dfa
						   (cons (expression trm) preds)))))))
	  (values j (list j) a))
	(symtab-unshadow)))))

(defmethod fset-to-dfa* ((trm name-expr))
  (cond ((var2? trm)
	 (values (symtab-index trm) nil (dfa-true-val)))
	((emptyset2? trm)
	 (let ((i (symtab-new-index)))
	   (values i  (list i) (dfa-op #'dfa-empty i))))
	(t
	 (error-format-if "~a is not a 2nd-order variable" trm)
	 (call-next-method))))

(defun emptyset2? (trm)
  (tc-eq trm (emptyset-operator)))

(defmethod fset-to-dfa* ((fml branch))
  (let ((c (fml-to-dfa (condition fml))))
    (cond ((dfa-true? c)                            
	   (multiple-value-bind (j xs a1)
	       (fset-to-dfa (then-part fml))
	     (values j xs a1)))
	  ((dfa-false? c)
	   (multiple-value-bind (k ys a2)
	       (fset-to-dfa (else-part fml))
	     (values k ys a2)))
	  (t
	   (multiple-value-bind (j xs a1)
	       (fset-to-dfa (then-part fml))
	     (multiple-value-bind (k ys a2)
		 (fset-to-dfa (else-part fml))
	       (let* ((i (symtab-new-index))
		      (a (dfa-ite c (dfa-op #'dfa-eq2 i j)
				    (dfa-op #'dfa-eq2 i k))))
		 (values i
			 (cons i (union xs ys :test #'=))
			 (dfa-conjunction* (list a a1 a2))))))))))
    
(defmethod fset-to-dfa* ((trm application))
  (cond ((the2? trm)
	 (multiple-value-bind (X supertype preds)
	     (destructure-binding (car (bindings (argument trm))) :exclude (fset-of-nats))
	   (assert (tc-eq supertype (fset-of-nats)))
	   (let  ((j (symtab-shadow X)))
	     (unwind-protect
		 (let ((a (fml-to-dfa (expression (argument trm))))
		       (bs (mapcar #'fml-to-dfa preds)))			   
		   (values j (list j) (dfa-conjunction* (cons a bs))))
	       (symtab-unshadow)))))
	((union2? trm)
	 (multiple-value-bind (i xs a1)
	     (fset-to-dfa* (args1 trm))
	   (multiple-value-bind (j ys a2)
	       (fset-to-dfa* (args2 trm))
	     (let* ((k (symtab-new-index))
		    (a (dfa-op #'dfa-union k i j)))
		 (values k
			 (cons k (union xs ys :test #'=))
			 (dfa-conjunction* (list a a1 a2)))))))
	((intersection2? trm)
	 (multiple-value-bind (i xs a1)
	     (fset-to-dfa* (args1 trm))
	   (multiple-value-bind (j ys a2)
	       (fset-to-dfa* (args2 trm))
	     (let* ((k (symtab-new-index))
		    (a (dfa-op #'dfa-intersection k i j)))
		 (values k
			 (cons k (union xs ys :test #'=))
			 (dfa-conjunction* (list a a1 a2)))))))
	((difference2? trm)
	 (multiple-value-bind (i xs a1)
	     (fset-to-dfa* (args1 trm))
	   (multiple-value-bind (j ys a2)
	       (fset-to-dfa* (args2 trm))
	     (let* ((k (symtab-new-index))
		    (a (dfa-op #'dfa-difference k i j)))
		 (values k
			 (cons k (union xs ys :test #'=))
			 (dfa-conjunction* (list a a1 a2)))))))
	(t
	 (error-format-if "Application ~a is not 2nd-order" trm)
	 (call-next-method))))

(defun union2? (trm)
  (and (tc-eq (operator trm) (union-operator))
       (2nd-order? trm)))

(defun intersection2? (trm)
  (and (tc-eq (operator trm) (intersection-operator))
       (2nd-order? trm)))

(defun difference2? (trm)
  (and (tc-eq (operator trm) (set-difference-operator))
       (2nd-order? trm)))
  
(defun the2? (trm)
  (and (is? (operator trm) '|the| '|sets|)
       (2nd-order? trm)
       (typep (argument trm) 'lambda-expr)
       (= (length (bindings (argument trm)) 1))))

;; Translation of naturals

(defun nat-to-dfa (trm)
  (nat-to-dfa* trm))

(defmethod nat-to-dfa* ((trm expr))
  (cond ((shielding? trm)
	 (error-format-if "~%2nd-order term ~a is shielding and can thus not be abstracted.
                         Giving up..." trm)
	 (throw 'not-ws1s-translatable nil))
	(t (let ((i (symtab-index trm)))
	     (error-format-if "~%New 1st-order parameter for ~a" trm)
	     (values i (list i) (dfa-var1 i))))))

(defmethod nat-to-dfa* ((trm name-expr))
  (if (var1? trm)
      (values (symtab-index trm) nil (dfa-true-val))
    (progn
      (error-format-if "~a is not a 1st-order variable" trm)
      (call-next-method))))

(defmethod nat-to-dfa* ((trm number-expr))
  (if (natural-number-expr? trm) 
      (let ((i (symtab-new-index)))
	(values i (list i) (dfa-op #'dfa-const (number trm) i)))
    (progn
      (error-format-if "Number ~a is not a natural" trm)
      (call-next-method))))

(defmethod nat-to-dfa* ((trm branch))
  (let ((c (fml-to-dfa (condition trm))))
    (cond ((dfa-true? c)                            
	   (multiple-value-bind (j xs a1)
	       (nat-to-dfa (then-part trm))
	     (values j xs a1)))
	  ((dfa-false? c)
	   (multiple-value-bind (k ys a2)
	       (nat-to-dfa (else-part trm))
	     (values k ys a2)))
	  (t
	   (multiple-value-bind (j xs a1)
	       (nat-to-dfa (then-part trm))
	     (multiple-value-bind (k ys a2)
		 (nat-to-dfa (else-part trm))
	       (let* ((i (symtab-new-index))
		      (a (dfa-ite c (dfa-op #'dfa-eq1 i j)
				    (dfa-op #'dfa-eq1 i k))))
		 (values i
			 (cons i (union xs ys :test #'=))
			 (dfa-conjunction* (list a a1 a2))))))))))

(defmethod nat-to-dfa* ((trm application))
  (let ((op (operator trm)))
    (cond ((is? op '|-| '|reals|)                                ; p_i = p_j - n    
	   (let ((lhs (args1 trm))
		 (rhs (pseudo-normalize (args2 trm))))
	     (if (and (natural-number-expr? rhs)
		      (1st-order? lhs))
		 (multiple-value-bind (j xs b)
		     (nat-to-dfa lhs)
		   (let*  ((i (symtab-new-index))
			   (a (dfa-conjunction* (list (dfa-op #'dfa-minus1 i j (number rhs)) b))))
		     (values i (cons i xs) a)))
	       (progn
		 (error-format-if "Subtraction ~a not 1st-order" trm)
		 (call-next-method)))))
	  ((is? op '|+| '|reals|)                               ; p_i = p_j + n, p_i = n + p_j
	   (let* ((ntrm (pseudo-normalize trm))                 ; now canonized to p_i =  n + p_j   
		  (lhs (args1 ntrm))
		  (rhs (args2 ntrm)))
	     (if (and (natural-number-expr? lhs)
		      (1st-order? rhs))
		 (multiple-value-bind (j xs b)
		     (nat-to-dfa rhs)
		   (let*  ((i (symtab-new-index))
			   (a (dfa-conjunction* (list (dfa-op #'dfa-plus1 i j (number lhs)) b))))
		     (values i (cons i xs) a)))
		 (progn
		 (error-format-if "Addition ~a not 1st-order" trm)
		 (call-next-method)))))
	  ((the1? trm)
	   (let ((bndng (car (bindings (argument trm))))
		 (expr  (expression (argument trm))))
	     (multiple-value-bind (x supertype preds)
		 (destructure-binding bndng :exclude *naturalnumber*)
	       (unless (tc-eq supertype *naturalnumber*)
		 (error-format-if "Choice ~a is not 1st-order" trm)
		 (call-next-method))
	       (let ((j (symtab-shadow x)))
		 (unwind-protect
		     (let ((a (dfa-conjunction* (mapcar #'fml-to-dfa
						  (cons expr preds)))))
		       (values j (list j) a))
		   (symtab-unshadow))))))
	  (t
	   (error-format-if "Application ~a not translatable" trm)
	   (call-next-method)))))

(defun the1? (trm)
  (and (is? (operator trm) '|the| '|sets|)
       (1st-order? trm)
       (typep (argument trm) 'lambda-expr)
       (= (length (bindings (argument trm)) 1))))

(defun natural-number-expr? (expr)
  (and (typep expr 'number-expr)
       (integerp (number expr))
       (>= (number expr) 0)))



