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

(defmacro protecting-dp-state (((new-dp-state old-dp-state)) &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-dp-state (dp::push-new-cong-state ,old-dp-state))
	   (,resultsym nil))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defun solve-for (expr bndng state)
  #+dbg(assert (not (negation? expr)))
  #+dbg(assert (not (null state)))
  #+dbg(assert (typep bndng 'bind-decl))
  (let* ((trm (top-translate-to-dc expr))
	 (canonized-trm (dp::canon trm state))
	 (x (top-translate-to-dc (make-variable-expr bndng))))
    (cond ((dp::false-p canonized-trm)
	   (throw 'unsatisfiable nil))
	  ((dp::true-p canonized-trm)
	   *true*)
	  (t
	   (if (and (dp::occurs-p x trm)
		    (not (dp::occurs-in-scope-of-uninterp-p x trm)))
	       (let ((solved-trm (dp::normineq canonized-trm state x)))
		 (unless (dp::well-formed-node-p solved-trm)
		   (error-format-if "Term ~a may contain nonlinearity"
				    (translate-from-dc canonized-trm))
		   (throw 'unable nil))
		 (translate-from-dc solved-trm))
	       expr)))))

(defun substitute-and-simplify (arg subst state)
    (dp-canon (substit arg subst) state))



