(in-package 'dp)

(defun adt-p (term)
  (or (node-constructor? term)
      (and (application-p term)
	   (or (node-constructor? (funsym term))
	       (node-accessor-indices (funsym term))))))

(defun constructor-p (term)
  (and (application-p term)
       (node-constructor? (funsym term))))

(defun accessor-p (term)
  (and (application-p term)
       (node-accessor-indices (funsym term))))

(defun apply-interp-map (term map-eqns)
  (cond
   ((leaf-p term) (get-rhs-map term map-eqns))
   (t (let ((new-term (get-rhs-map term map-eqns)))
	(if (eq new-term term)
	    (cond
	     ((interp? term)
	      (mk-term (map-args-list #'apply-interp-map term map-eqns)))
	     (t term))
	    new-term)))))

(defun get-rhs-map (term map-eqns)
  (loop for eq in map-eqns
	for lhs = (arg 1 eq)
	for rhs = (arg 2 eq)
	when (eq lhs term) do (return rhs)
	finally (return term)))

(defun sig-adt (term cong-state)
  (cond
   ((leaf-p term) term)
   ((node-constructor? (funsym term))
    (sig-constructor term cong-state))
   ((node-accessor-indices (funsym term))
    (sig-accessor term cong-state))
   (t term)))
   

(defun sig-accessor (term cong-state)
  (declare (ignore cong-state))
  (let* ((accessor (funsym term))
	 (constructor-term (arg 1 term))
	 (constructor (when (application-p constructor-term)
			(funsym constructor-term)))
	 (index-alist (node-accessor-indices accessor))
	 (index (when (and constructor (node-constructor? constructor))
		  (cdr (assoc constructor index-alist)))))
    (if index (arg (1+ index) constructor-term)
	term)))

(defun sig-constructor (term cong-state)
  (declare (ignore cong-state))
  (let* ((accessor-terms (funargs term))
	 (common-arg
	  (loop with first-arg =
		(and (application-p (car accessor-terms))
		     (node-accessor-indices (funsym (car accessor-terms)))
		     (arg 1 (car accessor-terms)))
		for a in accessor-terms
		unless (and (application-p a)
			    (node-accessor-indices (funsym a))
			    (eq (arg 1 a) first-arg))
		do (return nil)
		finally (return first-arg))))
    (or common-arg term)))

(defun adt-solve (eqn cong-state)
  (let ((lhs (arg 1 eqn))
	(rhs (arg 2 eqn)))
    (adt-solve* lhs rhs cong-state)))

(defun adt-solve* (lhs rhs cong-state)
  (if (leaf-p lhs)
      (solve-base lhs rhs cong-state)
      (let ((funsym (funsym lhs)))
	(cond ((node-constructor? funsym)
	       (solve-constructor lhs rhs cong-state))
	      ((node-accessor-indices funsym)
	       (solve-accessor lhs rhs cong-state))
	      (t (solve-base lhs rhs cong-state))))))

(defun solve-base (lhs rhs cong-state)
  (cond
   ((not (occurs-under-interp lhs rhs))
    (list (mk-equality lhs rhs)))
   ((not (and (application-p rhs)
	      (node-constructor? (funsym rhs))))
    (list (mk-equality rhs lhs)))
   ((occurs-under-funsym lhs rhs (funsym rhs))
    (list (mk-equality rhs lhs)))
   (t
    (eliminate-var lhs rhs cong-state))))

(defun occurs-under-funsym (lhs rhs funsym)
  (cond
   ((constant-p rhs) nil)
   ((uninterp? rhs) nil)
   ((and (eq (funsym rhs) funsym)
	 (find lhs (funargs rhs) :test #'eq))
    t)
   (t (some #'(lambda (x) (occurs-under-funsym lhs x funsym))
	    (funargs rhs)))))

(defun eliminate-var (lhs rhs cong-state)
  (let ((accessors (node-constructor-accessors (funsym rhs))))
    (let* ((accessor-vars
	    (loop for a in accessors
		  collect (mk-fresh-constant)))
	   (accessor-applications
	    (loop for a in accessors
		  collect (mk-term (list a lhs))))
	   (accessor-map
	    (mapcar #'(lambda(x y) (mk-equality x y))
		    accessor-applications accessor-vars))
	   (new-rhs (apply-interp-map rhs accessor-map))
	   (solved-lhs-eqn (mk-equality lhs new-rhs)))
      (cons solved-lhs-eqn
	    (eliminate-accessor-vars accessor-vars new-rhs cong-state)))))

(defun eliminate-accessor-vars (accessor-vars rhs cong-state)
  (eliminate-accessor-vars* accessor-vars 1 rhs nil cong-state))

(defun eliminate-accessor-vars* (accessor-vars index rhs equalities cong-state)
  (if (null accessor-vars)
      equalities
      (let* ((accessor-var (car accessor-vars))
	     (new-rhs1 (arg index rhs))
	     (new-rhs (apply-interp-map new-rhs1 equalities))
	     (new-equalities (solve-base accessor-var new-rhs cong-state)))
	(eliminate-accessor-vars* (cdr accessor-vars) (1+ index) rhs
				  (union new-equalities equalities)
				  cong-state))))

(defun solve-accessor (lhs rhs cong-state)
  (let ((accessor-indices (node-accessor-indices (funsym lhs))))
    (if (cdr accessor-indices)
	;;dissallow bloody overloading of accessors
	(list (mk-equality lhs rhs))
	(let* ((constructor (car (car accessor-indices)))
	       (accessor-index (cdr (car accessor-indices)))
	       (accessors (node-constructor-accessors constructor))
	       (new-lhs (arg 1 lhs))
	       (new-rhs-args (mk-constructor-args rhs accessor-index
						  accessors))
	       (new-rhs (sig-constructor
			 (mk-term (cons constructor new-rhs-args))
			 cong-state)))
	  (adt-solve* new-lhs new-rhs cong-state)))))

(defun mk-constructor-args (rhs index accessors)
  (loop for i from 0
	for a in accessors
	collect
	(if (= i index) rhs
	    (mk-fresh-constant))))

(defun solve-constructor (lhs rhs cong-state)
  (if (member rhs (funargs lhs))
      (list (mk-equality lhs rhs))
      (let* ((accessors (node-constructor-accessors (funsym lhs))))
	(solve-constructor* accessors 1 lhs rhs nil cong-state))))

(defun solve-constructor* (accessors index lhs rhs equalities cong-state)
  (if (null accessors)
      equalities
      (let* ((accessor (car accessors))
	     (new-lhs1 (arg index lhs))
	     (new-lhs (apply-interp-map new-lhs1 equalities))
	     (new-rhs-arg (apply-interp-map rhs equalities))
	     (new-rhs (sig-accessor (mk-term (list accessor new-rhs-arg))
				    cong-state))
	     (new-equalities (adt-solve* new-lhs new-rhs cong-state)))
	(solve-constructor* (cdr accessors) (1+ index)
			    lhs new-rhs-arg
			    (union new-equalities equalities)
			    cong-state))))
