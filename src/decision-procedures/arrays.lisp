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
  (let* ((equality (mk-equality term1 term2))
	 (norm-equality (sigma equality cong-state)))
    norm-equality))

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
