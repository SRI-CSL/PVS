(in-package pvs)


(defun apply-dp-func (dp-func dp-state &rest pvs-args)
  (let* ((dp-args (mapcar #'top-translate-to-dc pvs-args))
	 (dp-result (apply dp-func (append dp-args (list dp-state))))
	 (result (translate-from-dc-list dp-result)))
    result))

(defun dp::arithsimp (term dp-state)
  (cond
   ((dp::leaf-p term) term)
   ((dp::interp? term)
    (let ((newterm
           (dp::sigma (dp::mk-term
		       (cons (dp::funsym term)
                        (loop for arg in (dp::funargs term)
                              collect (dp::arithsimp arg dp-state)))
		       (dp::node-type term))
		      dp-state)))
      newterm))
   (T term)))

(defun dp-arithsimp (expr dp-state)
  (let ((ground-expr (top-translate-to-dc expr)))
    (translate-from-dc (dp::arithsimp ground-expr dp-state))))

(defun dp-canon (expr dp-state)
  (apply-dp-func #'(lambda (expr dp-state)
		     (dp::canon expr dp-state 'no-mod))
		 dp-state expr))

(defun dp-solve (expr dp-state)
  (apply-dp-func #'dp::solve dp-state expr))

(defun dp-normineq (expr dp-state &optional (var nil))
  (apply-dp-func #'(lambda (expr var dp-state)
		     (dp::normineq expr dp-state var))
		 dp-state expr var))
