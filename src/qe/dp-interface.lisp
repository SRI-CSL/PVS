(in-package :pvs)

(defun apply-dp-func (dp-func dp-state &rest pvs-args)
  (let* ((dp-args (mapcar #'top-translate-to-dc pvs-args))
	 (dp-result (apply dp-func (append dp-args (list dp-state))))
	 (result (translate-from-dc-list dp-result)))
    result))

(defun dp-canon (expr dp-state)
  (apply-dp-func #'(lambda (expr dp-state)
		     (dp::canon expr dp-state 'no-mod))
		 dp-state expr))

(defun dp-find (expr dp-state)
  (apply-dp-func #'(lambda (expr dp-state)
		     (dp::dp-find expr dp-state))
		 dp-state expr))

(defmacro protecting-dp-state (((new-dp-state old-dp-state)) &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-dp-state (dp::push-new-cong-state ,old-dp-state))
	   (,resultsym nil))
       (declare (special ,new-dp-state))
       (unwind-protect
	   (progn
	     (setq ,resultsym
		   (multiple-value-list (progn ,@body)))
	     (values-list ,resultsym))
	 (dp::npop-cong-state ,new-dp-state)))))






