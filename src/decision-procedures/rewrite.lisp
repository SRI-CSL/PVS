(in-package dp)

(defvar *empy-rewrite-rules* (make-rewrite-rules))

(defun initial-rewrite-rules ()
  *empy-rewrite-rules*)

(defdpstruct (rewrite-rule
	      (:conc-name "RR-")
	      (:print-function
	       (lambda (rr s k)
		 (declare (ignore k))
		 (if (rr-condition rr)
		     (format s "<rr ~A: ~A IMPLIES ~A --> ~A>"
		       (rr-name rr) (rr-condition rr)
		       (rr-lhs rr) (rr-rhs rr))
		     (format s "<rr ~A: ~A --> ~A>"
		       (rr-name rr) (rr-lhs rr) (rr-rhs rr))))))
  name
  lhs
  rhs
  condition)

(defun print-rewrite-rule (rr s)
  (if (rr-condition rr)
      (format s "<rr ~A: ~A IMPLIES ~A --> ~A>"
	(rr-name rr) (rr-condition rr)
	(rr-lhs rr) (rr-rhs rr))
      (format s "<rr ~A: ~A --> ~A>"
	(rr-name rr) (rr-lhs rr) (rr-rhs rr))))

(defun clr-rewrite-rules (rewrite-rules)
  (setf (rewrite-rules-rules! rewrite-rules) nil)
  (setf (rewrite-rules-rules rewrite-rules) nil)
  (dp-clrhash (rewrite-rules-hash rewrite-rules))
  rewrite-rules)

(defun extend-subst (subst var term)
  (acons var term subst))

(defun subst-in-application (appl subst)
  (mk-term-array (map-args-array #'subst-in-node appl subst)))

(defun subst-in-var (var subst)
  (sublis subst var))

(defun subst-in-node (node subst)
  (cond
   ((application-p node) (subst-in-application node subst))
   ((constant-p node) node)
   ((dp-variable-p node) (subst-in-var node subst))
   (t (break))))

(defun apply-subst (term subst)
  (subst-in-node term subst))

(defun match (term lhs &optional (subst nil))
  (cond
   ;;((null lhs) (values subst (null term)))
   ;;((null term) (values subst nil))
   ((application-p lhs)
    (if (application-p term)
	(match-application term lhs subst)
	(values subst nil)))
   ((dp-numberp lhs) (if (and (dp-numberp term)
			      (eq lhs term))
			 (values subst t)
			 (values subst nil)))
   ((member lhs '(*true* *false*) :test #'eq)
    (if (eq term lhs)
	(values subst t)
	(values subst nil)))
   ((constant-p lhs)
    (if (eq term lhs)
	(values subst t)
	(values subst nil)))
   ((dp-variable-p lhs)
    (let ((real-lhs (apply-subst lhs subst)))
      (cond
       ((dp-variable-p real-lhs) (values (extend-subst subst lhs term) t))
       ((eq term real-lhs) (values subst t))
       (t (values subst nil)))))))

(defun match-application (term lhs subst)
  (if (eq term lhs) (values subst t)
      (match-args term lhs subst)))

(defun match-args (term lhs subst)
  (let ((term-arity (arity term))
	(lhs-arity (arity lhs)))
   (cond
    ((= term-arity lhs-arity)
     (labels
	 ((match-arg
	   (term-arg lhs-arg)
	   (multiple-value-bind (new-subst success)
	       (match term-arg lhs-arg subst)
	     (if success
		 (setq subst new-subst)
		 (throw 'fail (values subst nil))))))
       (catch 'fail
	 (map-args-args #'match-arg term lhs)
	 (values subst t))))
    (t (values subst nil)))))

(defvar *rew-hash* (make-hash-table :test #'equal))

(defun getrewhash (term cong-state)
  (gethash term (rewrite-rules-hash (rewrite-rules cong-state))))

(defun setf-getrewhash (term cong-state new-term)
  (setf (gethash term (rewrite-rules-hash (rewrite-rules cong-state)))
	new-term))

(defsetf getrewhash setf-getrewhash)

(defvar *print-rewrite* t)

(defun print-rewrite* (term new-term)
  (when *print-rewrite*
    (format t "~%Rewriting:")
    (ppr term)
    (format t "~%to~%")
    (ppr new-term)))


(defun normalize-term (term cong-state)
  (let ((hash-term (getrewhash term cong-state)))
    ;(break)
    (cond
     (hash-term (if (eq hash-term '*no-change*)
		    (values term nil)
		    (values hash-term t)))
     (t
      (multiple-value-bind (new-term change)
	  (normalize-term-no-memo term cong-state)
	(cond
	 (change (when *print-rewrite*
		   (print-rewrite* term new-term))
		 (when (equal ;'(IF (= (PC) (ACC)) (ACCP (S)) (ANYTHING))
			      '(BANYTHING)
			      new-term) (break))
		 (setf (getrewhash term cong-state)
		       new-term)
		 (values new-term t))
	 (t (setf (getrewhash term cong-state)
		  '*no-change*)
	    (values term nil))))))))

(defun normalize-term-no-memo (term cong-state)
  (cond
   ;;((null term) (values term nil))
   ((not (application-p term)) (rewrite-then-normalize term cong-state))
   (t (multiple-value-bind (new-term args-change)
	  (normalize-args term cong-state)
	(if args-change
	    (values (rewrite-then-normalize new-term cong-state)
		    t)
	    (rewrite-then-normalize term cong-state))))))

(defun rewrite-then-normalize (term cong-state)
  (multiple-value-bind (new-term change)
      (rewrite-term term (rewrite-rules-rules (rewrite-rules cong-state))
		    cong-state)
    (if change
	(values (normalize-term new-term cong-state) t)
	(values term nil))))

(defun rewrite-term (term rewrites cong-state)
  (if rewrites
      (multiple-value-bind (new-term change)
	  (rewrite-term-1 term (car rewrites) cong-state)
	(if change
	    (values new-term t)
	    (rewrite-term term (cdr rewrites) cong-state)))
      (simplify-term term cong-state)))

(defun simplify-term (term cong-state)
  (let ((new-term (sigma term cong-state)))
    (if (eq term new-term)
	(values term nil)
	(values new-term t))))

(defun simplify-= (term)
  (let ((arg1 (arg1 term))
	(arg2 (arg2 term)))
    (cond
     ((equal arg1 arg2) (values 'TRUE t))
     ((and (numberp arg1) (numberp arg2))
      (values 'FALSE t))
     ((some #'(lambda (distinct-list)
		(and (member arg1 distinct-list :test #'equal)
		     (member arg2 distinct-list :test #'equal)))
	    *distinct-lists*)
      (values 'FALSE t))
     (t (values term nil)))))

(defun rewrite-term-1 (term rewrite cong-state)
  (let ((lhs (rr-lhs rewrite))
	(rhs (rr-rhs rewrite)))
    (multiple-value-bind (subst succ)
	(match term lhs)
      (when (and (application-p term) (application-p lhs)
		 (eq (funsym term) (funsym lhs))
		 (eq (funsym term) 'aref))
	nil)
      (if succ
	  (values (apply-subst rhs subst) t)
	  (values term nil)))))

(defun normalize-args (term cong-state)
  (let ((change nil))
    (labels
	((normalize-arg
	  (arg)
	  (multiple-value-bind (new-arg change-arg)
	      (normalize-term arg cong-state)
	    (cond
	     (change-arg
	      (setq change t)
	      new-arg)
	     (t arg)))))
      (let ((new-term (mk-term-array (map-args-array #'normalize-arg term))))
	(values new-term change)))))

(defun big-size (term)
  (let ((*size-alist* nil))
    (declare (special *size-alist*))
    (big-size* term)))

(defun big-size* (term)
  (let ((ass (assq term *size-alist*)))
    (declare (special *size-alist*))
    (cond
     (ass (cdr ass))
     (t (let ((r (cond ((null term) 0)
		       ((consp term) (+ 1 (big-size* (car term))
					(big-size* (cdr term))))
		       (t 1))))
	  (setq *size-alist* (acons term r *size-alist*))
	  r)))))
