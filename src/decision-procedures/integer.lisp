(in-package dp)

(defun dp-integer-atom-p (l cong-state)
  (or (eq (node-initial-type l) *integer*)
      (eq (dp-type l cong-state) *integer*)
      (dp-integerp l)
      (floor-p l)
      (and (times-p l)
	   (integer-times-p l cong-state))))

(defun integer-times-p (l cong-state)
  (loop for a in (funargs l)
	always (dp-integer-atom-p a cong-state)))

(defun integer-equality-p (lit cong-state)
  (let ((fract (and (or (arith-bool-p lit)
			(equality-p lit))
		    (dp-integer-atom-p (lhs lit) cong-state)
		    (fractpt (rhs lit) cong-state))))
    (and fract (zerop fract))))

(defun integer-inequality-p (lit cong-state)
  (and (not (equality-p lit))
       (integer-equality-p lit cong-state)))  

(defun strong-integercut (lit cong-state)
  (let ((diff (make-ineq-to-difference lit)))
    (if (plus-p diff)
	(let* ((lcm (mk-constant (apply #'lcm (plus-denoms diff))))
	       (normed-lit
		(mk-term (list (funsym lit)
			       (sigtimes (mk-times (list lcm (lhs lit))))
			       (sigtimes (mk-times (list lcm (rhs lit))))))))
	  (integercut normed-lit cong-state))
	lit)))

(defun integercut (lit cong-state)
  (let ((fract (and (or (arith-bool-p lit)
			(equality-p lit))
		    (dp-integer-atom-p (lhs lit) cong-state)
		    (fractpt (rhs lit) cong-state))))
    (cond
     ((null fract) lit)
     ((zerop fract) (zerocut lit))
     ((minusp fract) (minuscut lit fract))
     (t (poscut lit fract)))))

; performs cut for zero fractional part

(defun zerocut (ineq)
  (case (constant-id (funsym ineq))
    (LESSP
     (mk-term
	 `(,*lesseqp*
	   ,(lhs ineq)
	   ,(sigplus (mk-term `(,*plus* ,(mk-constant -1) ,(rhs ineq)))))))
    (GREATERP
     (mk-term `(,*greatereqp*
		,(lhs ineq)
		,(sigplus (mk-term `(,*plus* ,(mk-constant 1) ,(rhs ineq)))))))
    (t ineq)))

; performs cut for negative fractional part

(defun minuscut (ineq fract)
  (case (constant-id (funsym ineq))
    (= *false*)
    ((LESSP LESSEQP)
     (mk-term `(,*lesseqp*
		,(lhs ineq)
		,(sigplus (mk-term `(*plus*
				     ,(mk-constant (- (1+ fract)))
				     ,(rhs ineq)))))))
    (t
     (mk-term `(,*greatereqp* ,(lhs ineq)
		  ,(sigdifference
		    (mk-term `(,*difference* ,(rhs ineq) ,(mk-constant fract)))))))))

; performs cut for positive fractional part

(defun poscut(ineq fract)
  (case (constant-id (funsym ineq))
    (= *false*)
    ((LESSP LESSEQP)
     (mk-term `(,*lesseqp*
		,(lhs ineq)
		,(sigplus (mk-term `(,*plus* ,(mk-constant (- fract)) ,(rhs ineq)))))))
    (t
     (mk-term `(,*greatereqp*
		,(lhs ineq)
		,(sigplus
		  (mk-term `(,*plus* ,(mk-constant (- 1 fract)) ,(rhs ineq)))))))))

; returns the fractional part of expression l if one can be determined
; or nil otherwise

(defun num-fractpt (number)
  (multiple-value-bind (integer fract)
      (truncate number)
    (declare (ignore integer))
    fract))

(defun dp-fractpt (constant)
  (num-fractpt (constant-id constant)))

(defun fractpt (l cong-state)
  (cond
   ((dp-integer-atom-p l cong-state) 0)
   ((dp-numberp l) (dp-fractpt l))
   ((plus-p l) (plusfractpt l cong-state))
   ((times-p l) (timesfractpt l cong-state))
   (t nil)))

; fractpt for a plus expression

(defun plusfractpt (l cong-state)
  (let ((fract-sum 0))
    (labels ((sum-fract-pt
	      (atom)
	      (let ((atom-fract (fractpt atom cong-state)))
		(if atom-fract
		    (setq fract-sum (+ atom-fract fract-sum))
		    (throw 'fastexit nil)))))
      (catch 'fastexit
	(map-funargs #'sum-fract-pt l)
	(num-fractpt fract-sum)))))

; fractpt for a times expression

(defun timesfractpt (l cong-state)
  (labels ((times-fract-pt
	    (atom)
	    (let ((atom-fract (fractpt atom cong-state)))
	      (unless (and atom-fract (zerop atom-fract))
		(throw 'fastexit nil)))))
    (catch 'fastexit
      (map-funargs #'times-fract-pt l)
      0)))

(defun ineq-initial-type (ineq cong-state)
  (if (dp-integer-atom-p (lhs ineq) cong-state)
      (let ((frac (fractpt (rhs ineq) cong-state)))
	(if (and frac (zerop frac))
	    *integer*
	    *number*))
      *number*))
