(in-package dp3)
;; Recognizing sets

(defun regexp-p (trm)
  (declare (type node trm))
  #+dbg(assert (node-p trm))
  (or (eq (node-initial-type trm) *regexp*)
      (and (application-p trm)
	   (eq (the symbol (node-initial-type (funsym trm))) 'regexp-op))))

;; Canonization

(defun sig-regexp (trm cs)
  trm)

(defun sig-regexp<= (trm cs)
  (declare (type node trm))
  (with-regexp-environment ()
     (let* ((q (new-index))
	    (dfa1 (regexp-to-dfa (lhs trm) 0 q))
	    (dfa2 (regexp-to-dfa (rhs trm) 0 q))
	    (dfa (dfa-forall1 q (dfa-implies dfa1 dfa2))))
       (cond ((dfa-full-p dfa)  *true*)
	     ((dfa-empty-p dfa) *false*)
	     (t trm)))))

(defun sig-regexp= (trm cs)
  (declare (type node trm))
  (with-regexp-environment ()
     (let* ((q (new-index))
	    (dfa1 (regexp-to-dfa (lhs trm) 0 q))
	    (dfa2 (regexp-to-dfa (rhs trm) 0 q))
	    (dfa (dfa-forall1 q (dfa-iff dfa1 dfa2))))
       (cond ((dfa-full-p dfa)  *true*)
	     ((dfa-empty-p dfa) *false*)
	     (t trm)))))

;; Solving

(defun regexp-solve-neq (neq cs)
  (declare (type node neq)
	   (ignore cs))
  #+dbg(assert (disequality-p neq))
  (list neq))
  
(defun regexp-solve (eqn cs)
  (declare (type node eqn)
           (ignore cs))
  #+dbg(assert (equality-p eqn))
  (list eqn))

;; Translating a regular expression into a
;; minimal, deterministic finite-state automata (dfa).

(defun regexp-to-dfa (trm p q)
  (cond ((regexp-epsilon-p trm)
	 (dfa-eq1 p q))
	((regexp-conc-p trm)
	 (let ((r (new-index)))
	   (dfa-exists1 r (dfa-and (regexp-to-dfa (lhs trm) p r)
				   (regexp-to-dfa (lhs trm) r q)))))
	((regexp-union-p trm)
	 (dfa-or (regexp-to-dfa (lhs trm) p q)
		 (regexp-to-dfa (lhs trm) p q)))
	((regexp-intersection-p trm)
	 (dfa-and (regexp-to-dfa (lhs trm) p q)
		  (regexp-to-dfa (lhs trm) p q)))
	((regexp-complement-p trm)
	 (dfa-negation (regexp-to-dfa (arg 1 trm) p q)))
	((regexp-star-p trm)
	 (let ((S (new-index))
	       (r (new-index))
	       (r1 (new-index)))
	   (dfa-exists2 S (dfa-and (dfa-and (dfa-in p S) (dfa-in q S))
				   (dfa-forall1 r (dfa-forall1
				       (dfa-implies (consecutive-p r r1 S)
						    (regexp-to-dfa trm r r1))))))))
	(t (dfa-and (dfa-plus1 p q 1)
		    (is-p trm p)))))

(defun is-p (trm p)
  (let* ((i (get-index trm)))
     (dfa-var1 i)))

(defun consecutive-p (p q S)
  (dfa-and (dfa-and (dfa-and (dfa-less p q)
			     (dfa-in p S))
		    (dfa-in q S))
	   (let ((r (new-index)))
	     (dfa-forall1 r (dfa-implies
			     (dfa-and (dfa-less p r) (dfa-less (r q)))
			     (dfa-not r S))))))	
	
(defmacro with-regexp-environment (&body body)
  (let ((resultsym (gensym)))
    `(let ((*term-to-index-table* nil)
	   (*index* 0))
       (declare (special *term-to-index-table*)
		(special *index*))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defun get-index (trm)
  (declare (special *term-to-index-table*))
  (let ((res (assoc trm *term-to-index-table*)))
    (if res (cdr res)
	(let ((i (new-index)))
	  (setq *term-to-index-table* (acons trm i *term-to-index-table*))
	  i))))

(defun new-index ()
  (declare (special *index*))
  (setq *index* (1+ *index*)))

;; Interface to DFA package

(defmacro dfa-full-p (dfa) `(pvs::dfa-equivalence ,dfa (pvs::dfa-true-val)))
(defmacro dfa-empty-p (dfa) `(pvs::dfa-equivalence ,dfa (pvs::dfa-false-val)))

(defmacro dfa-var1 (i) `(pvs::dfa-var1 ,i))

(defmacro dfa-not (dfa) `(pvs::dfa-negation ,dfa))
(defmacro dfa-and (dfa1 dfa2) `(pvs::dfa-conjunction ,dfa1 ,dfa2))
(defmacro dfa-or (dfa1 dfa2) `(pvs::dfa-disjunction ,dfa1 ,dfa2))
(defmacro dfa-implies (dfa1 dfa2) `(pvs::dfa-implication ,dfa1 ,dfa2))
(defmacro dfa-iff (dfa1 dfa2) `(pvs::dfa-equivalence ,dfa1 ,dfa2))

(defmacro dfa-forall1 (i dfa) `(pvs::dfa-forall1 ,i ,dfa))
(defmacro dfa-exists1 (i dfa) `(pvs::dfa-exists1 ,i ,dfa))
(defmacro dfa-forall2 (i dfa) `(pvs::dfa-forall2 ,i ,dfa))
(defmacro dfa-exists2 (i dfa) `(pvs::dfa-exists2 ,i ,dfa))

(defmacro dfa-in (i P) `(pvs::mk-dfa-ptr (pvs::dfa-in ,i ,P)))
(defmacro dfa-eq1 (i j) `(pvs::mk-dfa-ptr (pvs::dfa-eq1 ,i ,j)))
(defmacro dfa-less (i j) `(pvs::mk-dfa-ptr (pvs::dfa-in ,i ,j)))




	
	
				   


       
     




