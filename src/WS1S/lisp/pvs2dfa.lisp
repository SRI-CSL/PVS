(in-package :pvs)

;; Translation of PVS formulas to deterministic finite automata;

; Messages

(defmacro ws1s-msg (str &rest args)
  `(locally (declare (special *verbose*))
      (when *verbose*
        (format t ,str ,@args))))


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
	((subtype-of? type *number*) 1)
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

(defun bool-to-dfa (fml)
  (unwind-protect
      (progn 
	(symtab-init)
	(multiple-value-bind (dfa symtab)
	    (bool-to-dfa* fml (symtab-empty))
	  (values (dfa-unrestrict dfa) symtab)))
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
      (values (dfa-iff dfa1 dfa2) symtab2))))

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
	   (combine-binary #'index-to-dfa lhs
			   #'index-to-dfa rhs
			   #'dfa-eq1
			   symtab))
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
	   (combine-binary #'index-to-dfa (args1 fml)
			   #'index-to-dfa (args2 fml)
			   #'dfa-less
			   symtab))
	  ((tc-eq op (greater-operator))
	   (combine-binary #'index-to-dfa (args1 fml)
			   #'index-to-dfa (args2 fml)
			   #'(lambda (i j) (dfa-less j i))
			   symtab))
	  ((tc-eq op (lesseq-operator))
	   (combine-binary #'index-to-dfa (args1 fml)
			   #'index-to-dfa (args2 fml)
			   #'dfa-lesseq
			   symtab))
	  ((tc-eq op (greatereq-operator))
	   (combine-binary #'index-to-dfa (args1 fml)
			   #'index-to-dfa (args2 fml)
			   #'(lambda (i j) (dfa-lesseq j i))
			   symtab))
	  (t
	   (call-next-method)))))


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
						  (dfa-iff (dfa-in idx1 idx2)
								   (dfa-conjunction*
								    (bool-to-dfa* (cons (expression trm) preds) symtab))))))
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
      (bool-to-dfa* (condition fml))
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
	  (bool-to-dfa* body symtab0)
	(multiple-value-bind (dfas2 symtab2)
	    (bool-to-dfa* preds symtab1)
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
      (bool-to-dfa* (condition expr) symtab)
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
			 (pseudo-normalize (args1 expr)))))    ; now hopefully of the form p_i =  n + p_j
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










