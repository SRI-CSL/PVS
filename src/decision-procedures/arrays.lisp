(in-package dp)

(defun update-p (term)
  (and (application-p term)
       (eq (funsym term) *update*)))

(defun applyupdate-p (term)
  (and (application-p term)
       (update-p (funsym term))))

(defmacro mk-update (array index val)
  `(mk-term (list *update* ,array ,index ,val)))

(defmacro update-array (term)
  `(arg 1 ,term))

(defmacro update-index (term)
  `(arg 2 ,term))

(defmacro update-value (term)
  `(arg 3 ,term))

(defmacro mk-apply (array index)
  `(mk-term (list ,array ,index)))

(defmacro applyupdate-array (term)
  `(funsym ,term))

(defmacro applyupdate-index (term)
  `(arg 1 ,term))

(defun test-equality (term1 term2 cong-state)
  "Cheap incomplete test for whether term1 = term2 is entailed by cong-state."
  (let* ((equality (mk-equality term1 term2))
	 (norm-equality (sigma equality cong-state)))
    (if (eq (dp-theory norm-equality) 'arith)
	(simplify-ineq-constraint norm-equality cong-state)
	norm-equality)))

(defun expr-< (term1 term2)
  (monom-< term1 term2))

(defun sigapplyupdate (term cong-state)
  (let* ((update (applyupdate-array term))
	 (apply-index (applyupdate-index term))
	 (array1 (update-array update))
	 (update-index (update-index update))
	 (val  (update-value update)))
    (let ((update-index_eq_apply-index
	   (test-equality  update-index apply-index cong-state)))
      (cond ((true-p update-index_eq_apply-index) val)
	    ((false-p update-index_eq_apply-index)
	     (sigma (mk-term (list array1 apply-index)) cong-state))
	    (t term)))))

(defun sigupdate (term cong-state)
  (let ((array0 (update-array term))
        (index0 (update-index term)))
    (cond ((update-p array0)
	   (let* ((array1 (update-array array0))
		  (index1 (update-index array0))
		  (val1  (update-value array0))
		  (index1_eq_index0 (test-equality index1 index0 cong-state)))
	     (cond ((true-p index1_eq_index0)
		    (mk-update array1 index0 (update-value term)))
		   ((false-p index1_eq_index0)
		    ;; DAC: 3-18-91
		    ;; when index1 > index0 have to call sigupdate recursively
		    ;; so that if there is an index' in array1
		    ;; s.t. index0 = index', that value at index'
		    ;; will be removed.
		    (if (expr-< index1 index0)
			term
			(mk-update
			 (sigupdate
			  (mk-update array1 index0 (update-value term))
			  cong-state)
			 index1 val1)))
		   (t term))))
	  (t term))))

(defun add-use-of-update (term cong-state)
  "If term is update(a, i, v). Then a syntactic use of
term is apply(update(a, i, v), i) which is simplified to v.
If apply(a, i) = v then apply(a, i) is in the use of
apply(update(a, i, v), i). To handle this possibility we put
apply(a, i) immediately on the use of udpate(a, i, v).
I think we alternatively could have put v on the use.
The bottom line is this allows the congruence closure algorithm
to automatically update the normal-forms whenever v is found
to be equivalent to something else."
  (let* ((index (update-index term))
	 (use-of-update (mk-term (list term index))))
    (add-use term use-of-update cong-state)))

(defun array-solve (eqn cong-state)
  (list eqn))

(defun array-solve-neq (neq cong-state)
  (list neq))
