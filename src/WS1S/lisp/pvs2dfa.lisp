(in-package :pvs)

;; Translation of PVS formulas to deterministic finite automata;
;;
;; typically used as follows:
;;    (unwind-protect
;;       (progn
;;         (bool-to-dfa-init)
;;         (.... (catch 'not-ws1s-translatable (bool-to-dfa ....)))
;;      (bool-to-dfa-init))

; Messages

(defmacro ws1s-msg (str &rest args)
  `(locally (declare (special *verbose*))
      (when *verbose*
        (format t ,str ,@args))))

; Initialization

(defun bool-to-dfa-init ()
  (symtab-init)
  (bool-to-dfa-hash-init))

(defvar *bool-to-dfa-table* nil)

(defun bool-to-dfa-hash-init ()
  (if *bool-to-dfa-table*
      (clrhash *bool-to-dfa-table*)
    (setf *bool-to-dfa-table* (make-hash-table :test 'eq))))

;; WS1S type declarations

(defun ws1s-types ()
  (list *boolean* *naturalnumber* (fset-of-nats)))
  
(defun ws1s-type? (type)
  (member type (ws1s-types) :test #'tc-eq))

;; Recognizing Variables

(defun var? (expr) 
  (and (name-expr? expr)
    ;   (member (kind expr) '(variable constant))
    ;   (typep (declaration expr) '(and const-decl (not def-decl)))
       (not (def-axiom (declaration expr)))))

;; Assign level according to type information

(defun level (type)
  (cond ((tc-eq type *boolean*) 0)
	((subtype-of? type *number*)
	 (if *presburger* 2 1))
	(t
	 2)))
	;((subtype-of? type (fset-of-nats)) 2)
	;(t (break))))


;; Processing of binary relations

(defun combine-binary (trans-lhs lhs trans-rhs rhs combine symtab)
  (multiple-value-bind (idx1 exs1 dfa1 symtab1)
      (funcall trans-lhs lhs symtab)
    (assert (typep idx1 'integer))
    (multiple-value-bind (idx2 exs2 dfa2 symtab2)
	(funcall trans-rhs rhs symtab1)
      (assert (typep idx2 'integer))
      (let ((level-lhs (level (type lhs)))
	    (level-rhs (level (type rhs))))
      (values (dfa-exists*
	       (mapcar #'(lambda (x) level-lhs) exs1)
	       exs1
	       (dfa-exists*
		(mapcar #'(lambda (x) level-rhs) exs2)
		exs2
		(dfa-conjunction
		 (funcall combine idx1 idx2)
		 (dfa-conjunction dfa1 dfa2))))
		symtab2)))))


; Translation of formulas has a formula argument, the currently set of bound variables,
; and the current symbol table; the result is an automaton or nil if the formula is not
; a translatable formula.

(defun bool-to-dfa (fml symtab)
  (unwind-protect
      (progn 
	(symtab-init)
	(bool-to-dfa* fml symtab))
    nil))
       
(defmethod bool-to-dfa* ((fml expr) symtab)
  (cond ((symtab-shielding? fml symtab)
	 (ws1s-msg "~%Not abstractable: ~a" fml)
	 (throw 'not-ws1s-translatable nil))
	(t
	 (let ((idx0 (symtab-index fml symtab)))
	   (if idx0
	       (values (dfa-var0 idx0) symtab)
	       (multiple-value-bind (idx0 symtab0)
		   (symtab-add-free fml symtab)
		 (ws1s-msg "~%Abstracting boolean ~a" fml)   
		 (values (dfa-var0 idx0) symtab0)))))))

(defmethod bool-to-dfa* ((fml name-expr) symtab)
  (cond ((tc-eq fml *true*)
	 (values (dfa-true) symtab))
	((tc-eq fml *false*)
	 (values (dfa-false) symtab))
	(t
	 (let ((idx0 (symtab-index fml symtab)))
	   (if (not idx0)
	       (call-next-method)
	       (values (dfa-var0 idx0) symtab))))))

(defmethod bool-to-dfa* ((fml negation) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (values (dfa-negation dfa1) symtab1)))

(defmethod bool-to-dfa* ((fml conjunction) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (cond ((dfa-true? dfa1)
	   (bool-to-dfa* (args2 fml) symtab1))
	  ((dfa-false? dfa1)
	   (values (dfa-false) symtab1))            
	  (t (multiple-value-bind (dfa2 symtab2)
		 (bool-to-dfa* (args2 fml) symtab1)
	       (values (dfa-conjunction dfa1 dfa2) symtab2))))))

(defmethod bool-to-dfa* ((fml disjunction) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (cond ((dfa-false? dfa1)
	   (bool-to-dfa* (args2 fml)) symtab1) 
	  ((dfa-true? dfa1)
	   (values (dfa-true) symtab1))
	  (t (multiple-value-bind (dfa2 symtab2)
		 (bool-to-dfa* (args2 fml) symtab1)
	       (values (dfa-disjunction dfa1 dfa2) symtab2))))))

(defmethod bool-to-dfa* ((fml implication) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (if (dfa-false? dfa1)
	(values (dfa-true) symtab1)
	(multiple-value-bind (dfa2 symtab2)
	    (bool-to-dfa* (args2 fml) symtab1)          
	  (values (dfa-implication dfa1 dfa2) symtab2)))))

(defmethod bool-to-dfa* ((fml branch) symtab)
  (multiple-value-bind (dfa0 symtab0)
      (bool-to-dfa* (condition fml) symtab)
    (cond ((dfa-true? dfa0)                      
	   (bool-to-dfa* (then-part fml) symtab))
	  ((dfa-false? dfa0)                
	   (bool-to-dfa* (else-part fml) symtab))
	  (t (multiple-value-bind (dfa1 symtab1)
		 (bool-to-dfa* (then-part fml) symtab0)
	       (multiple-value-bind (dfa2 symtab2)
		   (bool-to-dfa* (else-part fml) symtab1)
		 (values (dfa-ite dfa0 dfa1 dfa2) symtab2)))))))

(defmethod bool-to-dfa* ((fml iff-or-boolean-equation) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (multiple-value-bind (dfa2 symtab2)
	(bool-to-dfa* (args2 fml) symtab1)
      (values (dfa-equivalence dfa1 dfa2) symtab2))))

(defmethod bool-to-dfa* ((fmls null) symtab)
  (values nil symtab))

(defmethod bool-to-dfa* ((fmls cons) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (first fmls) symtab)
    (multiple-value-bind (dfas2 symtab2)
	(bool-to-dfa* (rest fmls) symtab1)
      (values (cons dfa1 dfas2) symtab2))))

; Each atom is translated by first translating its arguments. Translation
; of an arithmetic arguments yields an index, a set of variables to be
; quantified existentially, a set of constraints, and a set of new substitutions.


(defmethod bool-to-dfa* ((fml equation) symtab)
  (let ((lhs (args1 fml))
	(rhs (args2 fml)))
    (cond ((or (boolean? lhs)
	       (boolean? rhs))
	   (bool-to-dfa* (make!-equivalence lhs rhs) symtab))
	  ((or (natural-expr? lhs)
	       (natural-expr? rhs))
	   (if *presburger*
	       (combine-binary #'presburger-to-dfa lhs
			       #'presburger-to-dfa rhs
			       #'dfa-eq2
			       symtab)
	       (combine-binary #'index-to-dfa lhs
			       #'index-to-dfa rhs
			       #'dfa-eq1
			       symtab)))
	  ((or (finite-set-of-nat? lhs)
	       (finite-set-of-nat? rhs))
	   (combine-binary #'nats-to-dfa lhs
			   #'nats-to-dfa rhs
			   #'dfa-eq2
			   symtab))
	  (t
	   (call-next-method)))))

(defmethod bool-to-dfa* ((fml disequation) symtab)
  (multiple-value-bind (dfa1 symtab1)
      (bool-to-dfa* (args1 fml) symtab)
    (values (dfa-negation dfa1) symtab1)))

(defmethod bool-to-dfa* ((fml application) symtab)
  (let ((op (operator fml))
	(args (arguments fml)))
    (cond ((and (= (length args) 1)   ; membership
		(finite-set-of-nat? op)
		(natural-expr? (first args)))
	   (combine-binary #'index-to-dfa (args1 fml)
			   #'nats-to-dfa op
			   #'dfa-in
			   symtab))
	  ((tc-eq op (less-operator))
	   (process-nat-binrel (args1 fml)
			       (args2 fml)
			       #'dfa-presburger-less
			       #'dfa-less
			       symtab))
	  ((tc-eq op (greater-operator))
	   (process-nat-binrel (args1 fml)
			       (args2 fml)
			       #'dfa-presburger-greater
			       #'(lambda (i j) (dfa-less j i))
			       symtab))
	  ((tc-eq op (lesseq-operator))
	   (process-nat-binrel (args1 fml)
			       (args2 fml)
			       #'dfa-presburger-lesseq
			       #'dfa-lesseq
			       symtab))
	  ((tc-eq op (greatereq-operator))
	   (process-nat-binrel (args1 fml)
			       (args2 fml)
			       #'dfa-presburger-greatereq
			       #'(lambda (i j) (dfa-lesseq j i))
			       symtab))
	  (t
	   (call-next-method)))))

(defun process-nat-binrel (lhs rhs combine-presburger combine-succ symtab)
  (if *presburger*
      (combine-binary #'presburger-to-dfa lhs
		      #'presburger-to-dfa rhs
		      #'(lambda (p q) (funcall combine-presburger p q #'symtab-fresh-index))
		      symtab)
      (combine-binary #'index-to-dfa lhs
		      #'index-to-dfa rhs
		      combine-succ
		      symtab)))

(defmethod bool-to-dfa* ((fml forall-expr) symtab)
  (bool-to-dfa*
   (make!-negation
    (make!-exists-expr (bindings fml)
		       (make!-negation (expression fml))))
   symtab))

(defmethod bool-to-dfa* ((fml exists-expr) symtab)
  (assert (consp symtab))
  (multiple-value-bind (vars types preds)
      (destructure-bindings (bindings fml) :exclude (ws1s-types))
    (multiple-value-bind (indices symtab1)
	(symtab-add-bounds vars symtab)
      (assert (consp symtab1))
      (multiple-value-bind (dfa-body symtab2)
	  (bool-to-dfa* (expression fml) symtab1)
	(multiple-value-bind (dfa-preds symtab3)
	    (bool-to-dfa* preds symtab2)
	  (values (dfa-exists* (mapcar #'level types)
			       indices
			       (dfa-conjunction* (cons dfa-body dfa-preds)))
		  (symtab-make (symtab-boundvars symtab)
			       (symtab-freevars symtab3))))))))


;; Translation of finite set of naturals


(defun nats-to-dfa (expr symtab)
  (nats-to-dfa* expr symtab))

(defmethod nats-to-dfa* ((expr expr) symtab)
  (when (symtab-shielding? expr symtab)
    (ws1s-msg "~%Not abstractable: ~a." expr)
    (throw 'not-ws1s-translatable nil))
  (multiple-value-bind (idx new-symtab)
      (symtab-add-free expr symtab)
    (ws1s-msg "~%Abstracting set ~a" expr)
    (values idx
	    nil
	    (dfa-true)
	    new-symtab)))

(defmethod nats-to-dfa* ((trm lambda-expr) symtab)
  (cond ((not (= (length (bindings trm)) 1))
	 (call-next-method))
	(t
	 (multiple-value-bind (var supertype preds)
	     (destructure-binding (car (bindings trm)) :exclude (list *naturalnumber*))
	   (cond ((not (tc-eq supertype *naturalnumber*))
		  (call-next-method))
		 (t
		  (multiple-value-bind (idx1 symtab1)
		      (symtab-add-bound var symtab)           
		    (let* ((idx2 (symtab-fresh-index))
			   (cnstrnt2 (dfa-forall1 idx1
						  (dfa-equivalence (dfa-in idx1 idx2)
								   (dfa-conjunction*
								    (bool-to-dfa (cons (expression trm) preds) symtab))))))
		      (values idx2
			      (list idx2)
			      cnstrnt2
			      (symtab-make (symtab-boundvars symtab)
					   (symtab-freevars symtab1)))))))))))
	
(defmethod nats-to-dfa* ((expr name-expr) symtab)
  (cond ((var? expr)
	 (let ((idx (symtab-index expr symtab)))
	   (if (null idx)
	       (call-next-method)
	       (values idx nil (dfa-true) symtab))))
	((tc-eq expr (emptyset-operator))
	 (let ((idx (symtab-fresh-index)))
	   (values idx  (list idx) (dfa-empty idx) symtab)))
	(t
	 (call-next-method))))

(defmethod nats-to-dfa* ((fml branch) symtab)
  (multiple-value-bind (c symtab1)
      (bool-to-dfa (condition fml))
    (cond ((dfa-true? c)                            
	   (nats-to-dfa (then-part fml) symtab))
	  ((dfa-false? c)
	   (nats-to-dfa (else-part fml) symtab))
	  (t
	   (multiple-value-bind (idx1 exs1 cnstrnt1 symtab1)
	       (nats-to-dfa (then-part fml) symtab)
	     (multiple-value-bind (idx2 exs2 cnstrnt2 symtab2)
		 (nats-to-dfa (else-part fml) symtab1)
	       (let* ((idx (symtab-fresh-index))
		      (cnstrnt (dfa-ite c (dfa-eq2 idx idx1) (dfa-eq2 idx idx2))))
		 (values idx
			 (cons idx (append exs1 exs2))
			 (dfa-conjunction cnstrnt (dfa-conjunction cnstrnt1 cnstrnt2))
			 symtab2))))))))
    
(defmethod nats-to-dfa* ((expr application) symtab)
  (assert (consp symtab))
  (let ((op (operator expr)))
    (cond ((tc-eq op (the2))
	   (the2-to-dfa (car (bindings (argument expr)))
			(expression (argument expr))
			symtab))
	  ((tc-eq op (union-operator))
	   (process-fset-binop (args1 expr)
			       (args2 expr)
			       #'dfa-union symtab))
	  ((tc-eq op (intersection-operator))
	   (process-fset-binop (args1 expr)
			       (args2 expr)
			       #'dfa-intersection symtab))
	  ((tc-eq op (set-difference-operator))
	   (process-fset-binop (args1 expr)
			       (args2 expr)
			       #'dfa-difference symtab)) 
	  ((tc-eq op (add-operator))
	   (process-nat-fset-binop (args1 expr)
				   (args2 expr)
				   #'dfa-union symtab))
	  ((tc-eq op (remove-operator))
	   (process-nat-fset-binop (args1 expr)
				   (args2 expr)
				   #'dfa-difference symtab))
	  ((tc-eq op (singleton-operator))
	   (singleton-to-dfa (argument expr) symtab))
	  (t
	   (call-next-method)))))

(defun singleton-to-dfa (expr symtab)
  (multiple-value-bind (idx1 exs1 cnstrnt1 symtab1)
      (index-to-dfa expr symtab)
    (let* ((idx (symtab-fresh-index))
	   (cnstrnt (dfa-single idx idx1)))
      (values idx
	      (cons idx exs1)
	      (dfa-conjunction cnstrnt cnstrnt1)
	      symtab1))))
	   
(defun process-nat-fset-binop (lhs rhs combine symtab)
  (multiple-value-bind (idx1 exs1 cnstrnt1 symtab1)
      (index-to-dfa lhs symtab)
    (multiple-value-bind (idx2 exs2 cnstrnt2 symtab2)
	(nats-to-dfa* rhs symtab1)
      (let* ((idx (symtab-fresh-index))
	     (idx3 (symtab-fresh-index))
	     (cnstrnt (dfa-conjunction (funcall combine idx idx3 idx2)
				       (dfa-single idx3 idx1))))
	(values idx
		(cons idx (cons idx3 (append exs1 exs2)))
		(dfa-conjunction cnstrnt (dfa-conjunction cnstrnt1 cnstrnt2))
		symtab2)))))

(defun the2-to-dfa (bndng body symtab)
  (assert (consp symtab))
  (multiple-value-bind (var supertype preds)
      (destructure-binding bndng :exclude (fset-of-nats))
    (assert (tc-eq supertype (fset-of-nats)))
    (multiple-value-bind (idx0 symtab0)
	(symtab-add-bound var symtab)
      (multiple-value-bind (dfa1 symtab1)
	  (bool-to-dfa body symtab0)
	(multiple-value-bind (dfas2 symtab2)
	    (bool-to-dfa preds symtab1)
	(let ((symtab3 (symtab-make (symtab-boundvars symtab)
				    (symtab-freevars symtab2))))
	  (values idx0
		  (list idx0)
		  (dfa-conjunction dfa1 (dfa-conjunction* dfas2))
		  symtab3)))))))

(defun process-fset-binop (lhs rhs combine symtab)
  (multiple-value-bind (idx1 exs1 cnstrnt1 symtab1)
      (nats-to-dfa lhs symtab)
    (multiple-value-bind (idx2 exs2 cnstrnt2 symtab2)
	(nats-to-dfa rhs symtab1)
      (let* ((idx (symtab-fresh-index))
	     (cnstrnt (funcall combine idx idx1 idx2)))
	(values idx
		(cons idx (append exs1 exs2))
		(dfa-conjunction cnstrnt (dfa-conjunction cnstrnt1 cnstrnt2))
		symtab2)))))


;; Translation of presburger expressions

(defun presburger-to-dfa (trm symtab)
  (presburger-to-dfa* trm symtab))

(defmethod presburger-to-dfa* ((trm expr) symtab)
  (when (symtab-shielding? trm symtab)
    (ws1s-msg "~%Not abstractable: ~a." trm)
    (throw 'not-ws1s-translatable nil))
  (let ((idx (symtab-index trm symtab)))
    (if idx
	(values idx nil (dfa-true) symtab)
	(multiple-value-bind (idx1 symtab1)
	    (symtab-add-free trm symtab)
	  (ws1s-msg "~%Abstracting natural ~a" trm)   
	  (values idx1
		  nil
		  (dfa-true)
		  symtab1)))))

(defmethod presburger-to-dfa* ((expr name-expr) symtab)
  (cond ((natural-expr? expr)
	 (let ((idx (symtab-index expr symtab)))
	   (if idx
	       (values idx nil (dfa-true) symtab)
	       (call-next-method))))
	(t
	 (ws1s-msg "~%Not abstractable: ~a." expr)
	 (throw 'not-ws1s-translatable nil))))

(defmethod presburger-to-dfa* ((expr number-expr) symtab)
  (cond ((not (natural-number-expr? expr))
	 (ws1s-msg "~%Not abstractable: ~a." expr)
	 (throw 'not-ws1s-translatable nil))
	(t
	 (multiple-value-bind (idx0 symtab0)
	     (symtab-add-free expr symtab)
	   (values idx0
		   nil
		   (dfa-presburger-const idx0 (number expr))
		   symtab0)))))

(defmethod presburger-to-dfa* ((trm application) symtab)
  (let ((op (operator trm)))
    (cond ((tc-eq op (minus-operator))
	   (let ((trm1 (make!-plus (args1 trm)
				   (make!-unary-minus (args2 trm)))))
	     (presburger-to-dfa* trm1 symtab)))
	  ((tc-eq op (plus-operator))
	   (plus-to-dfa (args1 trm) (args2 trm) symtab))
	  ((and (tc-eq op (times-operator))
		(natural-number-expr? (args1 trm)))
	   (linear-times-to-dfa (number (args1 trm)) (args2 trm) symtab))
	  (t
	   (call-next-method)))))

(defun plus-to-dfa (lhs rhs symtab)
  (multiple-value-bind (idx1 exs1 cnstrnts1 symtab1)
      (presburger-to-dfa* lhs symtab)
    (multiple-value-bind (idx2 exs2 cnstrnts2 symtab2)
	(presburger-to-dfa* rhs symtab1)
      (let* ((idx3 (symtab-fresh-index))
	     (exs (cons idx3 (append exs1 exs2)))
	     (cnstrnts3 (dfa-presburger-add idx1 idx2 idx3 #'symtab-fresh-index)) ; idx3 = idx2 + idx1
	     (cnstrnts (dfa-conjunction cnstrnts1
					(dfa-conjunction cnstrnts2 cnstrnts3))))
 	  (values idx3
		  (cons idx3 (append exs1 exs2))
		  cnstrnts
		  symtab2)))))

(defun linear-times-to-dfa (n trm symtab &optional exs (cnstrnts (dfa-true)) idx)
  (if (= n 0)
      (values idx exs cnstrnts symtab)
      (multiple-value-bind (new-idx new-exs new-cnstrnts new-symtab)
	  (plus-to-dfa trm trm symtab)
	(linear-times-to-dfa (1- n)
			     trm
			     new-symtab
			     (append exs new-exs)
			     (dfa-conjunction cnstrnts new-cnstrnts)
			     new-idx))))


;; Translation of index terms

(defun index-to-dfa (expr symtab)
  (index-to-dfa* expr symtab))

(defmethod index-to-dfa* ((expr expr) symtab)
  (when (symtab-shielding? expr symtab)
    (ws1s-msg "~%Not abstractable: ~a." expr)
    (throw 'not-ws1s-translatable nil))
  (multiple-value-bind (idx1 symtab1)
      (symtab-add-free expr symtab)
    (ws1s-msg "~%Abstracting natural ~a" expr)
    (assert (typep idx1 'integer))
    (values idx1
	    nil
	    (dfa-var1 idx1)
	    symtab1)))

(defmethod index-to-dfa* ((expr name-expr) symtab)
  (cond ((not (var? expr))
	 (call-next-method))
	(t
	 (let ((idx (symtab-index expr symtab)))
	   (if (null idx)
	       (call-next-method)
	       (values idx
		       nil
		       (dfa-true)
		       symtab))))))

(defmethod index-to-dfa* ((expr number-expr) symtab)
  (cond ((not (natural-number-expr? expr))
	 (call-next-method))
	(t
	 (let ((idx (symtab-fresh-index)))
	   (values idx
		   (list idx)
		   (dfa-const (number expr) idx)
		   symtab)))))
  
(defmethod index-to-dfa* ((expr branch) symtab)
  (multiple-value-bind (dfac symtab0)
      (bool-to-dfa (condition expr) symtab)
    (cond ((dfa-true? dfac)                            
	   (index-to-dfa (then-part expr) symtab0))
	  ((dfa-false? dfac)
	   (index-to-dfa (else-part expr) symtab0))
	  (t
	   (multiple-value-bind (idx1 exs1 cnstrnts1 symtab1)
	       (index-to-dfa (then-part expr) symtab0)
	     (multiple-value-bind (idx2 exs2 cnstrnts2 symtab2)
		 (index-to-dfa (else-part expr) symtab1)
	       (let* ((idx (symtab-fresh-index))
		      (cnstrnt (dfa-ite dfac (dfa-eq1 idx idx1) (dfa-eq1 idx idx2))))
		 (values idx
			 (cons idx (append exs1 exs2))
			 (dfa-conjunction cnstrnt
					  (dfa-conjunction cnstrnts1 cnstrnts2))
			 symtab2))))))))

(defmethod index-to-dfa* ((expr application) symtab)
  (let ((op (operator expr)))
    (cond ((tc-eq op (minus1))
	   (let ((nexpr (make!-minus
			 (pseudo-normalize (args1 expr)))))          ; now hopefully of the form p_i =  n + p_j
	     (cond ((not (natural-number-expr? (args1 nexpr)))
		    (call-next-method))
		   (t
		    (process-index-binop (number (args1 nexpr))
					 (args2 nexpr)
					 #'dfa-minus1
					 symtab)))))
	  ((tc-eq op (plus1))
	   (let ((nexpr (make!-plus
			 (pseudo-normalize (args1 expr))
			 (pseudo-normalize (args2 expr)))))
	     (cond ((not (natural-number-expr? (args1 nexpr)))
		    (call-next-method))
		   (t
		    (process-index-binop (number (args1 nexpr))
					   (args2 nexpr)
					   #'dfa-plus1
					   symtab)))))
	  ((and (tc-eq op (the1))
		(typep (argument expr) 'lambda-expr)
		(= (length (bindings (argument expr)) 1)))
	   (the1-to-dfa (car (bindings (argument expr)))
			(expression (argument expr))
			symtab))
	  (t
	   (call-next-method)))))

(defun the1-to-dfa (bndng body symtab)
  (multiple-value-bind (var supertype preds)
      (destructure-binding bndng :exclude *naturalnumber*)
    (cond ((not (tc-eq supertype *naturalnumber*))
	   (call-next-method))
	  (t
	   (multiple-value-bind (idx1 symtab1)
	       (symtab-add-bound var symtab)
	     (multiple-value-bind (dfas symtab2)
		 (fmla-to-dfa (cons body preds) symtab1)
	       (values idx1
		       (list idx1)
		       (dfa-conjunction* dfas)
		       (symtab-make (symtab-boundvars symtab)
				    (symtab-freevars symtab2)))))))))

(defun process-index-binop (num expr combine symtab)
  (multiple-value-bind (idx1 exs1 cnstrnts1 symtab1)
      (index-to-dfa expr symtab)
    (let* ((idx (symtab-fresh-index))
	   (cnstrnt (funcall combine idx idx1 num)))
      (values idx
	      (cons idx exs1)
	      (dfa-conjunction cnstrnt cnstrnts1)
	      symtab1))))










