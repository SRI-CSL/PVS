(in-package dp)

(defun dp-integer-atom-p (l)
  (or (eq (node-initial-type l) *integer*)
      (floor-p l)
      (and (times-p l)
	   (integer-times-p l))))

(defun integer-times-p (l)
  (loop for a in (funargs l)
	always (dp-integer-atom-p a)))

(defun integer-equality-p (lit)
  (let ((fract (and (or (arith-bool-p lit)
			(equality-p lit))
		    (dp-integer-atom-p (lhs lit))
		    (fractpt (rhs lit)))))
    (and fract (zerop fract))))
  
  

(defun integercut (lit)
  (let ((fract (and (or (arith-bool-p lit)
			(equality-p lit))
		    (dp-integer-atom-p (lhs lit))
		    (fractpt (rhs lit)))))
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

(defun fractpt (l)
  (cond
   ((dp-integer-atom-p l) 0)
   ((dp-numberp l) (dp-fractpt l))
   ((plus-p l) (plusfractpt l))
   ((times-p l) (timesfractpt l))
   (t nil)))

; fractpt for a plus expression

(defun plusfractpt (l)
  (let ((fract-sum 0))
    (labels ((sum-fract-pt
	      (atom)
	      (let ((atom-fract (fractpt atom)))
		(if atom-fract
		    (setq fract-sum (+ atom-fract fract-sum))
		    (throw 'fastexit nil)))))
      (catch 'fastexit
	(map-funargs #'sum-fract-pt l)
	(num-fractpt fract-sum)))))

; fractpt for a times expression

(defun timesfractpt (l)
  (labels ((times-fract-pt
	    (atom)
	    (let ((atom-fract (fractpt atom)))
	      (unless (and atom-fract (zerop atom-fract))
		(throw 'fastexit nil)))))
    (catch 'fastexit
      (map-funargs #'times-fract-pt l)
      0)))

(defun ineq-initial-type (ineq)
  (if (dp-integer-atom-p (lhs ineq))
      (let ((frac (fractpt (rhs ineq))))
	(if (and frac (zerop frac))
	    *integer*
	    *number*))
      *number*))
