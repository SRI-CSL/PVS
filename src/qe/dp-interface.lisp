(in-package :pvs)


(defmacro protecting-dp-state (((new-dp-state old-dp-state)) &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-dp-state (new-cs ,old-dp-state))
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
		 (translate-from-dc solved-trm))
	       expr)))))

(defun substitute-and-simplify (arg subst state)
    (dp-canon (substit arg subst) state))



