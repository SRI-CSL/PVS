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
  (fml-to-dfa-hash-init)
  (known-judgements-hash-init)
  (unfold1-hash-init))

(defvar *fml-to-dfa-table* nil)
(defvar *unfold1-table* nil)
(defvar *known-judgements* nil)

(defun fml-to-dfa-hash-init ()
  (if *fml-to-dfa-table*
      (clrhash *fml-to-dfa-table*)
    (setf *fml-to-dfa-table* (make-hash-table :test 'eq))))

(defun unfold1-hash-init ()
  (if *unfold1-table*
      (clrhash *unfold1-table*)
    (setf *unfold1-table* (make-hash-table :test 'eq))))

(defun known-judgements-hash-init ()
  (if *known-judgements*
      (clrhash *known-judgements*)
    (setf *known-judgements* (make-hash-table :test 'eq))))

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
  (let ((expanded-fml (unfold1 fml)))
    (cond ((not (eq fml expanded-fml))  
	   (fml-to-dfa expanded-fml))
	  ((shielding? fml)
	   (ws1s-message "~%Formula ~a is shielding. Giving up..." fml)
	   (throw 'not-ws1s-translatable nil))
	  (t (multiple-value-bind (i signal)
		 (symtab-index fml)
	       (when (eq signal :new)
		 (ws1s-message "~%New Boolean parameter for ~a" fml))
	       (dfa-var0 i))))))

(defmethod fml-to-dfa* ((fml name-expr))
  (cond ((tc-eq fml *true*)
	 (dfa-true-val))
	((tc-eq fml *false*)
	 (dfa-false-val))
	((var0? fml)              
	 (dfa-var0 (symtab-index fml)))
	(t
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
	  (t (call-next-method)))))

(defmethod fml-to-dfa* ((fml disequation))
  (let ((lhs (args1 fml))
	(rhs (args2 fml)))
    (cond ((and (1st-order? lhs) (1st-order? rhs))
	   (dfa-negation (process-binrel1 lhs rhs #'dfa-eq1)))
	  ((and (2nd-order? lhs) (2nd-order? rhs))
	   (dfa-negation (process-binrel2 lhs rhs #'dfa-eq2)))
	  (t (call-next-method)))))

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
		 (t (call-next-method))))
	  (t (call-next-method)))))

(defmethod fml-to-dfa* ((fml forall-expr))
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fml) :exclude (ws1s-types))
    (unwind-protect
	(let ((indices (symtab-shadow* (reverse vars)))
	      (levels  (mapcar #'(lambda (type)
				   (cond ((tc-eq type *boolean*) 0)
					 ((tc-eq type *naturalnumber*) 1)
					 ((tc-eq type (fset-of-nats)) 2)
					 (t (break))))
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
					 (t (break))))
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
  (let ((new-trm (unfold1 trm)))
    (if (not (eq trm new-trm)) 
	(multiple-value-bind (i xs a)
	    (fset-to-dfa new-trm)
	  (values i xs a))
      (if (shielding? trm)
	  (progn
	    (ws1s-message "~%2nd-order term ~a
                            is shielding and can thus not be abstracted. Giving up..." trm)
	    (throw 'not-ws1s-translatable nil))
	(multiple-value-bind (i signal)
	    (symtab-index trm)
	  (when (eq signal :new)
	    (ws1s-message "~%New 2nd-order parameter for ~a" trm))
	  (values i nil (dfa-true-val)))))))

(defmethod fset-to-dfa* ((trm lambda-expr))
  (unless (= (length (bindings trm)) 1)
    (call-next-method))
  (multiple-value-bind (x supertype preds)
      (destructure-binding (car (bindings trm)) :exclude (list *naturalnumber*))
    (unless (tc-eq supertype *naturalnumber*)
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
  (if (var2? trm)
      (values (symtab-index trm) nil (dfa-true-val))
    (call-next-method)))

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
	(t (call-next-method))))

(defun the2? (trm)
  (and (is? (operator trm) '|the| '|sets|)
       (2nd-order? trm)
       (typep (argument trm) 'lambda-expr)
       (= (length (bindings (argument trm)) 1))))

;; Translation of naturals

(defun nat-to-dfa (trm)
  (nat-to-dfa* trm))

(defmethod nat-to-dfa* ((trm expr))
  (let ((new-trm (unfold1 trm)))
    (cond ((not (eq trm new-trm))     
	   (multiple-value-bind (i xs a)
	       (nat-to-dfa new-trm)
	     (values i xs a)))
	  ((shielding? trm)
	   (ws1s-message "~%2nd-order term ~a is shielding and can thus not be abstracted.
                         Giving up..." trm)
	   (throw 'not-ws1s-translatable nil))
	  (t (multiple-value-bind (i signal)
		 (symtab-index trm)
	       (when (eq signal :new)
		 (ws1s-message "~%New 1st-order parameter for ~a" trm))
	  (values i (list i) (dfa-var1 i)))))))

(defmethod nat-to-dfa* ((trm name-expr))
  (if (var1? trm)
      (values (symtab-index trm) nil (dfa-true-val))
    (call-next-method)))

(defmethod nat-to-dfa* ((trm number-expr))
  (if (natural-number-expr? trm) 
      (let ((i (symtab-new-index)))
	(values i (list i) (dfa-op #'dfa-const (number trm) i)))
    (call-next-method)))

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
	       (call-next-method))))
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
	       (call-next-method))))
	  ((the1? trm)
	   (let ((bndng (car (bindings (argument trm))))
		 (expr  (expression (argument trm))))
	     (multiple-value-bind (x supertype preds)
		 (destructure-binding bndng :exclude *naturalnumber*)
	       (unless (tc-eq supertype *naturalnumber*)
		 (call-next-method))
	       (let ((j (symtab-shadow x)))
		 (unwind-protect
		     (let ((a (dfa-conjunction* (mapcar #'fml-to-dfa
						  (cons expr preds)))))
		       (values j (list j) a))
		   (symtab-unshadow))))))
	  (t
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


;; Stepwise Unfolding

(defmethod unfold1 :around (expr)
  (or (gethash expr *unfold1-table*)
      (let ((nexpr (call-next-method)))
        (setf (gethash expr *unfold1-table*) nexpr)
	(when (and *babble* (not (eq expr nexpr)))
	  (ws1s-message "~%Unfolding: ~a" expr)
	  (ws1s-message "~%   ==> ~a" nexpr))
        nexpr)))

(defmethod unfold1 ((expr expr))
  expr)

(defmethod unfold1 ((expr name-expr))
  (if (is? expr '|the| '|sets|)
      expr
    (try-to-expand expr)))

(defmethod unfold1 ((expr application))
   (let* ((nop (unfold1 (operator expr)))
	  (narg (unfold1 (argument expr)))
	  (nexpr (if (lambda-expr? nop)
		     (let ((nexpr (make!-reduced-application nop narg)))
		       (if (branch? nexpr)
			   (unfold1 nexpr)
			  nexpr))
		   (lcopy expr 'operator nop 'argument narg))))
     (if (eq nexpr expr) expr
       (prog1
	 (pseudo-normalize nexpr)
	 (register-judgements nexpr expr)))))

(defun register-judgements (nexpr expr)
  "Register judgements of expr for an equivalent nexpr that
   may otherwise be lost."
  (setf (gethash nexpr *known-judgements*) 
	(union (gethash expr *known-judgements*)
	       (judgement-types+ expr)
	       :test #'tc-eq)))

(defmethod unfold1 ((expr branch))
  (let ((nexpr (unfold1 (condition expr))))
    (multiple-value-bind (signal bexpr)
	(assert-if nexpr)
      (declare (ignore signal))
      (cond ((tc-eq bexpr *true*)                      
	     (then-part expr))
	    ((tc-eq bexpr *false*)                
	     (else-part expr))
	    (t
	     (make!-if-expr bexpr (then-part expr) (else-part expr)))))))

(defmethod unfold1 ((expr let-expr))  ; structure sharing handled by hash table(s)
  (beta-reduce expr))

(defmethod unfold1 ((expr tuple-expr))
  expr)

(defmethod unfold1 ((expr coercion))
   (let ((nexpr (beta-reduce expr)))
     (when (not (eq expr nexpr))
       (register-judgements nexpr expr))
     nexpr))


(defmethod unfold1 ((expr update-expr))
  (unfold1 (translate-update-to-if expr))))

(defmethod unfold1 ((expr cases-expr))
  (unfold1 (translate-cases-to-if expr)))

(defmethod unfold1 ((list list))
  (let ((nlist (unfold1-list list)))
    (if (equal nlist list) list nlist)))

(defun unfold1-list (list &optional acc)
  (if (null list) (nreverse acc)
    (let ((nelt (unfold1 (car list)))) 
      (if (or (eq nelt (car list))
	      (not (typep (car list) 'binding)))
	  (unfold1-list (cdr list) (cons nelt acc))
	(unfold1-list (substit (cdr list)
				 (acons (car list) nelt nil))
			(cons nelt acc))))))
