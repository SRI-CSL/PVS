(in-package dp)

(defun record-p (term)
  (and (application-p term)
       (record-op-p (funsym term))))

(defun tuple-p (term)
  (and (application-p term)
       (tuple-op-p (funsym term))))

(defun project-p (term)
  (and (application-p term)
       (project-op-p (funsym term))))

(defun sigproject (term &optional cong-state)
  (let ((record (arg 2 term))
	(index (arg 1 term)))
    (cond ((record-p record)
	   (arg (1+ (constant-id index)) record))
	  ((tuple-p record)
	   (arg (1+ (constant-id index)) record))
	  (t term))))

(defun record-solve (eqn cong-state)
  (let ((lhs (lhs eqn))
	(rhs (rhs eqn)))
    (cond
     ((record-p lhs)
      (record-solve-1 lhs rhs cong-state))
     ((record-p rhs)
      (record-solve-1 rhs lhs cong-state))
     (t (break "Should not be here.")))))

(defun record-solve-1 (lhs rhs cong-state)
  (let ((index 0))
  (labels
      ((mk-project-eqn (field1 record2)
         (prog1
	     (mk-equality field1
			  (sigproject
			   (mk-term
			       `(,*project*
				 ,(mk-constant index) ,record2))))
	   (incf index))))
  (let ((new-eqns (map-funargs-list #'mk-project-eqn lhs rhs)))
    new-eqns))))

(defun tuple-solve (eqn cong-state)
  (let ((lhs (lhs eqn))
	(rhs (rhs eqn)))
    (cond
     ((tuple-p lhs)
      (tuple-solve-1 lhs rhs cong-state))
     ((tuple-p rhs)
      (tuple-solve-1 rhs lhs cong-state))
     (t (break "Should not be here.")))))

(defun tuple-solve-1 (lhs rhs cong-state)
  (let ((index 0))
  (labels
      ((mk-project-eqn (field1 record2)
         (prog1
	     (mk-equality field1
			  (sigproject
			   (mk-term
			       `(,*project*
				 ,(mk-constant index) ,record2))))
	   (incf index))))
  (let ((new-eqns (map-funargs-list #'mk-project-eqn lhs rhs)))
    new-eqns))))
