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
    (setf *fml-to-dfa-table* (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))))

(defun unfold1-hash-init ()
  (if *unfold1-table*
      (clrhash *unfold1-table*)
    (setf *unfold1-table* (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))))

(defun known-judgements-hash-init ()
  (if *known-judgements*
      (clrhash *known-judgements*)
    (setf *known-judgements* (make-hash-table :hash-function 'pvs-sxhash :test 'eq))))


; Translation of Formulas

(defun fml-to-dfa (fml)           
  #+dbg(assert (boolean? fml))
  (fml-to-dfa* fml))

(defmethod fml-to-dfa* :around (fml)
  (if (freevars fml)   ; hash closed formulas only, since symtab does shadowing
      (call-next-method)
    (or (gethash fml *fml-to-dfa-table*)
	(let ((dfa (call-next-method)))
	  (setf (gethash fml *fml-to-dfa-table*) dfa)
	  dfa))))
       
(defmethod fml-to-dfa* ((fml expr))
  (let ((expanded-fml (unfold1 fml)))
    (if (not (eq fml expanded-fml))  
	(fml-to-dfa expanded-fml)
      (if (shielding? fml)
	  (progn
	    (format t "~%Formula ~a is shielding. Giving up..." fml)
	    (throw 'not-ws1s-translatable nil))
	(multiple-value-bind (i signal)
	    (symtab-index fml)
	  (when (eq signal :new)
	    (format t "~%New Boolean parameter for ~a" fml))
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
    (cond ((dfa-true? a1) (fml-to-dfa rhs))        
	  ((dfa-false? a1) (dfa-false-val))            
	  (t (dfa-conjunction a1 (fml-to-dfa rhs))))))

(defmethod fml-to-dfa* ((fml disjunction))
  (let* ((lhs (args1 fml))
	 (rhs (args2 fml))
	 (a1 (fml-to-dfa lhs)))
    (cond ((dfa-false? a1) (fml-to-dfa rhs)) 
	  ((dfa-true? a1)  (dfa-true-val))
	  (t (dfa-disjunction a1 (fml-to-dfa rhs))))))

(defmethod fml-to-dfa* ((fml implication))
  (let* ((lhs (args1 fml))
	 (rhs (args2 fml))
	 (a1 (fml-to-dfa lhs)))
    (if (dfa-false? a1) (dfa-true-val)
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
	   (call-next-method)))))

(defmethod fml-to-dfa* ((fml disequation))
  (let ((lhs (args1 fml))
	(rhs (args2 fml)))
    (cond ((and (1st-order? lhs) (1st-order? rhs))
	   (dfa-negation (process-binrel1 lhs rhs #'dfa-eq1)))
	  ((and (2nd-order? lhs) (2nd-order? rhs))
	   (dfa-negation (process-binrel2 lhs rhs #'dfa-eq2)))
	  (t
	   (call-next-method)))))

(defmethod fml-to-dfa* ((fml application))
  (let ((op   (operator fml))
	(args (arguments fml)))
    (cond ((lt1? fml)       
	   (process-binrel1 (first args) (second args) #'dfa-less))
	  ((gt1? fml)  
	   (process-binrel1 (second args) (first args) #'dfa-less))
	  ((le1? fml)
	   (process-binrel1 (first args) (second args) #'dfa-lesseq))
	  ((ge1? fml)
	   (process-binrel1 (second args) (first args) #'dfa-lesseq))
	  ((member12? op args)
	   (process-binrel12 (first args) op #'dfa-in))
	  (t
	   (call-next-method)))))

(defun member12? (op args)
  (and (= (length args) 1) (2nd-order? op) (1st-order? (first args))))

(defmethod fml-to-dfa* ((fml let-expr))
  (fml-to-dfa (beta-reduce fml)))

(defun supported? (bndngs)
  (every #'(lambda (bndng)
	     (let ((type (type bndng)))
	       (or (1st-order? bndng)
		   (2nd-order? bndng)
		   (subtype-of? type *boolean*))))
	 bndngs))

(defmethod fml-to-dfa* ((fml forall-expr))
  (let ((bndngs (bindings fml)))
    (if (supported? bndngs)
	(fml-to-dfa-quantor #'fml-to-dfa-forall bndngs (expression fml))
      (call-next-method))))

(defmethod fml-to-dfa* ((fml exists-expr))
  (let ((bndngs (bindings fml)))
    (if (supported? bndngs)
	(fml-to-dfa-quantor #'fml-to-dfa-exists (bindings fml) (expression fml))
      (call-next-method))))
  
(defun fml-to-dfa-quantor (fn bindings body)
   (if (null bindings)
       (fml-to-dfa body)
     (multiple-value-bind (x supertype preds)
	 (destructure-binding (car bindings) :exclude (list *boolean*
							    *naturalnumber*
							    (fset-of-nats)))
       (unwind-protect
	   (let* ((i (symtab-shadow x))
		  (a (fml-to-dfa-quantor fn (cdr bindings) body))
		  (b (dfa-conjunction* (mapcar #'fml-to-dfa preds))))
	     (funcall fn i supertype b a))
	 (symtab-unshadow x)))))

(defun fml-to-dfa-forall (i type r a)
  (assert type)
    (cond ((tc-eq type (fset-of-nats))
           (dfa-forall2 i a r))
          ((tc-eq type *naturalnumber*)
           (dfa-forall1 i a r))
          ((tc-eq type *boolean*)
           (dfa-forall0 i a r))
          (t
	   (error "~%Quantification with binding variable ~a of type ~a
                    not WS1S-translatable" (symtab-expr i) (type (symtab-expr i))))))

(defun fml-to-dfa-exists (i type r a)
  (assert type)
    (cond ((tc-eq type (fset-of-nats))
           (dfa-exists2 i a r))
          ((tc-eq type *naturalnumber*)
           (dfa-exists1 i a r))
          ((tc-eq type *boolean*)
           (dfa-exists0 i a r))
          (t
	   (error "Quantification with binding variable ~a of type ~a
                   not WS1S-translatable" (symtab-expr i) (type (symtab-expr i))))))

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
	    (format t "~%2nd-order term ~a
                        is shielding and can thus not be abstracted. Giving up..." trm)
	    (throw 'not-ws1s-translatable nil))
	(multiple-value-bind (i signal)
	    (symtab-index trm)
	  (when (eq signal :new)
	    (format t "~%New 2nd-order parameter for ~a" trm))
	  (values i nil (dfa-true-val)))))))

(defmethod fset-to-dfa* ((trm lambda-expr))
  (unless (= (length (bindings trm)) 1)
    (call-next-method))
  (multiple-value-bind (x supertype preds)
      (destructure-binding (car (bindings trm)) :exclude (list *boolean* *naturalnumber*
							       (fset-of-nats)))
    (unless (tc-eq supertype *naturalnumber*)
	(call-next-method))
    (unwind-protect              
	(let* ((i (symtab-shadow x))
	       (j (symtab-new-index))
	       (a (dfa-forall1 i
		     (dfa-equivalence (dfa-op #'dfa-in i j)
				      (dfa-conjunction* (mapcar #'fml-to-dfa
								(cons (expression trm) preds)))))))
	  (values j (list j) a))
      (symtab-unshadow x))))

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
  (if (the2? trm)
      (multiple-value-bind (X supertype preds)
	  (destructure-binding (car (bindings (argument trm))) :exclude (fset-of-nats))
	(assert (tc-eq supertype (fset-of-nats)))
	(unwind-protect                
	    (let ((j (symtab-shadow X)))
	      (values j
		      (list j)
		      (dfa-conjunction* (mapcar #'fml-to-dfa
						(cons (expression (argument trm)) preds)))))
	  (symtab-unshadow X)))
    (call-next-method)))

(defun the2? (trm)
  (and (the-op? (operator trm))
       (2nd-order? trm)
       (typep (argument trm) 'lambda-expr)
       (= (length (bindings (argument trm)) 1))))

(defmethod fset-to-dfa* ((trm let-expr))
  (multiple-value-bind (i xs a)
      (fset-to-dfa (beta-reduce trm))
    (values i xs a)))

;; Translation of naturals

(defun nat-to-dfa (trm)
  (nat-to-dfa* trm))

(defmethod nat-to-dfa* ((trm expr))
  (let ((new-trm (unfold1 trm)))
    (if (not (eq trm new-trm))     
	(multiple-value-bind (i xs a)
	    (nat-to-dfa new-trm)
	  (values i xs a))
      (if (shielding? trm)
	  (progn
	    (format t "~%2nd-order term ~a is shielding and can thus not be abstracted.
                         Giving up..." trm)
	    (throw 'not-ws1s-translatable nil))
	(multiple-value-bind (i signal)
	    (symtab-index trm)
	  (when (eq signal :new)
	    (format t "~%New 1st-order parameter for ~a" trm))
	  (values i (list i) (dfa-var1 i)))))))

(defmethod nat-to-dfa* ((trm let-expr))
  (multiple-value-bind (i xs a)
      (nat-to-dfa (beta-reduce trm))
    (values i xs a)))

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
    (cond ((and (or (minus-op? op) (plus-op? op))
		(1st-order? (args1 trm))
		(1st-order? (args2 trm)))
	   (let ((fn (if (minus-op? op) #'dfa-minus1 #'dfa-plus1))
		 (rhs (pseudo-normalize (args2 trm))))
	     (cond ((natural-number-expr? rhs)        ; p_i = p_j + n, p_i = p_j - n          
		    (multiple-value-bind (j xs b)
			(nat-to-dfa (args1 trm))
		      (let*  ((i (symtab-new-index))
			      (a (dfa-conjunction* (list (dfa-op fn i j (number rhs)) b))))
			(values i (cons i xs) a))))
		   ((plus-op? op)
		    (let ((lhs (pseudo-normalize (args1 trm))))
		      (unless (natural-number-expr? lhs)  ; p_i = n + p_j
			(call-next-method))
		      (multiple-value-bind (j xs b)
			  (nat-to-dfa rhs)
			(let* ((i (symtab-new-index))
			       (a (dfa-conjunction* (list (dfa-op #'dfa-plus1 i j (number lhs)) b))))
			  (values i (cons i xs) a)))))
		   (t
		    (call-next-method)))))
	  ((the1? trm)
	   (let ((bndng (car (bindings (argument trm))))
		 (expr  (expression (argument trm))))
	     (multiple-value-bind (x supertype preds)
		 (destructure-binding bndng :exclude *naturalnumber*)
	       (unless (tc-eq supertype *naturalnumber*)
		 (call-next-method))
	       (unwind-protect
		   (let ((j (symtab-shadow x))
			 (a (dfa-conjunction* (mapcar #'fml-to-dfa
						       (cons expr preds)))))
		     (values j (list j) a))
		 (symtab-unshadow x)))))
	  (t
	   (call-next-method)))))

(defun the1? (trm)
  (and (the-op? (operator trm))
       (1st-order? trm)
       (typep (argument trm) 'lambda-expr)
       (= (length (bindings (argument trm)) 1))))

(defun natural-number-expr? (expr)
  (and (typep expr 'number-expr)
       (integerp (number expr))
       (>= (number expr) 0)))

;; Normalization

(defun unfold1 (expr)
  (unfold1* expr))

(defmethod unfold1* :around (expr)
  (or (gethash expr *unfold1-table*)
      (let ((nexpr (call-next-method)))
        (setf (gethash expr *unfold1-table*) nexpr)
        nexpr)))

(defmethod unfold1* ((expr expr))
  expr)

(defmethod unfold1* ((expr name-expr))
  (if (the-op? expr) expr
    (let ((decl (declaration (resolution expr))))
      (if (typep decl 'const-decl)  
	  (let ((defs (def-axiom decl)))
	    (if defs
		(subst-mod-params (args2 (car (last defs)))
				  (module-instance expr))
	      expr))
	expr))))

(defmethod unfold1* ((expr application))
   (let* ((nop (unfold1 (operator expr)))
	  (narg (unfold1 (argument expr)))
	  (nexpr (if (lambda-expr? nop)
                     (unfold1
		      (make!-reduced-application nop narg))
		   (lcopy expr 'operator nop 'argument narg))))
     (if (eq nexpr expr) expr
       (prog1
	 (pseudo-normalize nexpr)     
	 (setf (gethash nexpr *known-judgements*)  ; ensure subject reduction for judgement types 
	       (union (gethash expr *known-judgements*)
		      (judgement-types+ expr)
		      :test #'tc-eq))))))

(defmethod unfold1* ((expr branch))
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

(defmethod unfold1* ((expr tuple-expr))
  (lcopy expr 'exprs (unfold1* (exprs expr))))

(defmethod unfold1* ((expr record-expr))
  (lcopy expr 'assignments (unfold1* (assignments expr))))

(defmethod unfold1* ((expr binding-expr))
  (call-next-method))

(defmethod unfold1* ((expr update-expr))
  (lcopy expr 'expression (unfold1* (expression expr))
	      'assignments (unfold1* (assignments expr))))

(defmethod unfold1* ((expr cases-expr))
  (unfold1 (translate-cases-to-if expr)))

(defmethod unfold1* ((expr let-expr)) 
  (lcopy expr 'argument (unfold1* (argument expr))))

(defmethod unfold1* ((list list))
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
