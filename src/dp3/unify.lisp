(in-package dp3)

(defvar *fail* :fail)

(defun fail? (subst)
  (eq subst *fail*))

;; Top-Level E-Unification

(defun E-unify (t1 t2 cs &optional subst)
  "Given a substituion and a congruence closure state,
   tries to unify terms t1 and t2. Returns *fail* if if
   does not succeed to do so; otherwise it returns an
   extended substitution that equalizes t1 and t2."
  #+dbg(assert (node-p t1))
  #+dbg(assert (node-p t2))
  #+dbg(assert (not (null cs)))
  #+dbg(assert (listp subst))
  (cond ((eq t1 t2) subst)
	((fail? subst) *fail*)
	(t (nprotecting-cong-state (cs cs)
	      (loop for (x . e) in subst
	         do (dp-union x e cs))
	      (catch 'unify  
	        (E-unify1  cs t1 t2 subst))))))

(defun E-unify1 (*cs* t1 t2 subst)
  (declare (special *cs*))
  (when (fail? subst)
    (throw 'unify *fail*))
  (let ((s1 (signature t1 *cs*))
	(s2 (signature t2 *cs*)))
    (cond ((eq s1 s2)
	   subst)
	  ((and (application-p s1)
		(application-p s2)
		(or (uninterpreted? (funsym s1)) (equality-p s1))
		(or (uninterpreted? (funsym s2)) (equality-p s2)))
	   (multiple-value-bind (g as)
	       (destructure-application s1)
	     (multiple-value-bind (h bs)
		 (destructure-application s2)
	       (if (eq g h)
		   (E-unify* as bs subst)
		   (throw 'unify *fail*)))))
	  (t (let ((res (try-to-solve s1 s2)))
	       (cond ((true-p res)
		      subst)
		     ((false-p res)
		      (throw 'unify *fail*))
		     ((solved-form-p res)
		      (dp-union (lhs res) (rhs res) *cs*)
		      (if (dp-variable-p (lhs res))
			  (add-to-subst (lhs res) (rhs res) subst)
			(add-to-subst (rhs res) (lhs res) subst)))
		     (t (throw 'unify *fail*))))))))

(defun uninterpreted? (sym)
  (not (node-interpreted? sym)))

(defun E-unify* (t1 t2 subst)
  (cond ((and (null t1) (null t2))
	 subst)
	((and (consp t1) (consp t2))
	 (E-unify* (cdr t1) (cdr t2)
		   (E-unify1 *cs* (car t1) (car t2) subst)))
	(t (throw 'unify *fail*))))

(defun try-to-solve (t1 t2)
  (try-to-solve* t1 t2 (union (vars-of t1)
			      (vars-of t2))))

(defun try-to-solve* (t1 t2 xs)
  (declare (special *cs*))
  (if (null xs) nil
    (let ((res (solving t1 t2 (car xs) *cs*)))
      (cond ((true-p res) *true*)
	    ((false-p res) *false*)
	    ((solved-form-p res) res)
	    (t (try-to-solve* t1 t2 (cdr xs)))))))

(defun add-to-subst (x e subst)
  (composition (acons x e nil) subst))

(defun composition (sigma tau)
  (multiple-value-bind (support-of-sigma subst1)
      (composition1 sigma tau)
    (let ((subst2 (composition2 tau support-of-sigma)))
      (append subst1 subst2))))
			    
(defun composition1 (sigma tau &optional support subst)
  (if (null sigma)
      (values (nreverse support) (nreverse subst))
    (destructuring-bind (x . trm)
	(car sigma)
      (composition1 (cdr sigma) tau (cons x support)
		    (let ((new-trm (replace-by trm tau)))
		      (if (eq x new-trm) subst
			  (acons x new-trm subst)))))))

(defun composition2 (tau support-of-sigma &optional subst)
  (if (null tau)
      (nreverse subst)
    (let* ((pair (car tau))
	   (new-subst (if (member (car pair) support-of-sigma) subst
			  (cons pair subst))))
      (composition2 (cdr tau) support-of-sigma new-subst))))

(defun subst= (subst1 subst2)
  (or (and (fail? subst1) (fail? subst2))
      (let ((d1 (dom subst1))
            (d2 (dom subst2)))
        (and (subsetp d1 d2)
             (subsetp d2 d1)
             (every #'(lambda (x)
                        (eq (cdr (lookup x subst1))
                            (cdr (lookup x subst2))))
                    d1)))))

(defun dom (subst)
  (mapcar #'car subst))
   
;; E-solving

(defun solving (t1 t2 x cs)
  "Solving an equality t1 = t2 for x; returns either *true*,
   *false*, or an equality which may not necessarily be solved for x;
   the congruence state may be changed."
  #+dbg(assert (node-p t1))
  #+dbg(assert (node-p t2))
  #+dbg(assert (dp-variable-p x))
  #+dbg(assert (not (null cs)))
  (let ((canonized (canon (mk-equality t1 t2) cs)))
    (if (or (true-p canonized) (false-p canonized))
	canonized
        (let ((solved (normineq canonized cs x)))
	  (if (well-formed-node-p solved) solved
	      canonized)))))

(defun solved? (eqn x)
  "Checks if the equation eqn is of the form x = e s.t.
   x does not occur in e."
  #+dbg(assert (node-p eqn))
  #+dbg(assert (dp-variable-p x))
  (and (equality-p eqn)
       (eq (lhs eqn) x)
       (not (occurs-p x (rhs eqn)))))


;(defun E-unify-some (*cs* t1 t2 subst)
;  (declare (special *cs*))
;  (E-unify-some1 (extensions t1 *cs*)
;		 (extensions t2 *cs*)
;		 subst))

;(defun E-unify-some1 (l1 l2 subst)
;  (if (null l1) *fail*
;     (let ((new-subst (E-unify-some2 (car l1) l2 subst)))
;        (if (fail? new-subst)
;            (E-unify-some1 (cdr l1) l2 subst)
;	  new-subst))))

;(defun E-unify-some2 (t1 l2 subst)
;  (if (null l2) *fail*
;     (let ((new-subst (catch 'unify
;			(E-unify1 t1 (car l2) subst))))
;       (if (fail? new-subst)                         ; only one subst for the time being
;	   (E-unify-some2 t1 (cdr l2) subst)
;	 new-subst))))
