;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Interval arithmetic core routines **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         19-Jan-2009,
;;;            last updated on  22-Nov-2009.
;;;

(in-package :rahd)

;;;
;;; I-<=: An interval version of `<=' operating as a binary relation on 
;;;       (Q \union {-inf, +inf}).  This is for constructing numerical
;;;       i-boxes.
;;;

(defun i-<= (x y)
  (cond ((not (and (or (rationalp x) (equal x '-inf) (equal x '+inf))
		   (or (rationalp y) (equal y '-inf) (equal y '+inf))))
	 (break "Arguments not in Q \union {-inf, +inf}. ~% X: ~A ~% Y: ~A"
		x y))
	((equal x y) t)
	((and (rationalp x) (rationalp y))
	 (<= x y))
	((or (equal x '-inf) (equal y '+inf)) t)
	(t nil)))
	
(defun i-> (x y)
  (and (not (equal x y))
       (i-<= y x)))

(defun i-< (x y)
  (and (not (equal x y))
       (i-<= x y)))

(defun i->= (x y)
  (i-<= y x))

(defun i-= (x y)
  (and (i-<= x y)
       (i->= x y)))

;;;
;;; MAKE-INTERVAL: Given left and right interval symbols L,R (either '[ or ']), and
;;;  lower and upper bounds LB, UB (in Q \union {-inf, +inf})), return a RAHD 
;;;  representation of the interval L LB, UB, R.
;;;
;;;  Example: (MAKE-INTERVAL '[ 1 2 ']) yields: '([ 1 2 ]), which is interpreted by
;;;   all interval routines to denote the closed real interval [1, 2].  Likewise,
;;;   (MAKE-INTERVAL '] '-inf 10 ']) yields: '(] -inf 10 ]), which is interpreted by
;;;   all interval routines to denote the half-open real interval ]-inf,10].  
;;;   (We use the European convention of ]-left and [-right denoting open boundaries.)
;;;   Also, we allow +-inf to appear as L/U-bound components of closed intervals.
;;;

(defun make-interval (l lb ub r)
  (if (not (and (or (equal l '[) (equal l ']))
		(or (equal r '[) (equal r ']))
		(or (rationalp lb) (equal lb '-inf) (equal lb '+inf))
		(or (rationalp ub) (equal ub '-inf) (equal ub '+inf))))

      (break "Invalid interval construction.~% L: ~A ~% R: ~A ~% LB: ~A ~% UB: ~A ~% LB<=UB?: ~A"
	     l r lb ub (i-<= lb ub))

	(list l lb ub r)))


;;;
;;; MAKE-CLOSED-INTERVAL: Given L,R as in MAKE-INTERVAL, make a RAHD representation for
;;;  the closed interval [L,R].
;;;

(defun make-closed-interval (l r)
  (make-interval '[ l r ']))

;;; 
;;;
;;; I-LB: Given a RAHD interval representation, return its lower-bound component.
;;;

(defun i-lb (i)
  (cadr i))

;;;
;;; I-UB: Given a RAHD interval representation, return its upper-bound component.
;;;

(defun i-ub (i)
  (caddr i))

;;;
;;; I-L: Given a RAHD interval representation, return its left boundary symbol.
;;;

(defun i-l (i)
  (car i))

;;;
;;; I-R: Given a RAHD interval representation, return its right boundary symbol.
;;;

(defun i-r (i)
  (cadddr i))

;;;
;;; I-BOUNDARY-TYPE: Given a RAHD interval representation, return n in {0,1,2,3} s.t.
;;;   I-BOUNDARY-TYPE(i) = 0 if i = [x,y] (closed),
;;;                        1 if i = [x,y[ (clopen-oR),
;;;                        2 if i = ]x,y] (clopen-oL),
;;;                        3 if i = ]x,y[ (open).
;;;

(defun i-boundary-type (i)
  (let ((x (list (i-l i) (i-r i))))
    (cond ((equal x '([ ])) 0)
	  ((equal x '([ [)) 1)
	  ((equal x '(] ])) 2)
	  ((equal x '(] [)) 3))))

;;;
;;; I-OL?: Is an interval's left boundary type open?
;;;

(defun i-ol? (i)
  (equal (i-l i) ']))

;;;
;;; I-OR?: Is an interval's right boundary type open?
;;;

(defun i-or? (i)
  (equal (i-r i) '[))

;;;
;;; I-CL-SEL: If the argument is T, return a closed left
;;;  boundary type (bt).  Otherwise, return an open left bt.
;;;

(defun i-cl-sel (x)
  (if x '[ ']))

;;;
;;; I-CR-SEL: If the argument is T, return a closed right
;;;  boundary type (bt).  Otherwise, return an open right bt.
;;;

(defun i-cr-sel (x)
  (if x '] '[))

;;;
;;; INF?: Is x in {-inf, +inf}?
;;;

(defun inf? (x)
  (or (equal x '-inf)
      (equal x '+inf)))

;;;
;;; NEGATE-INF: Given x in {-inf,+inf}, return (-1)*x.
;;;

(defun negate-inf (x)
  (cond ((equal x '-inf) '+inf)
	((equal x '+inf) '-inf)
	(t (break "Invalid infinite element: ~A" x))))

;;;
;;; QI-+: Add two elements of (Q \union {-inf, +inf}).
;;;       Note: -inf + +inf is undefined and causes an error.
;;;

(defun qi-+ (x y)
  (cond ((and (rationalp x) (rationalp y))
	 (+ x y))
	((and (rationalp x) (inf? y))
	 y)
	((and (inf? x) (rationalp y))
	 x)
	((and (inf? x) (inf? y) (equal x y))
	 x)
	(t (break "X (~A) and Y (~A) must be members of (Q \union {-inf, +inf}), s.t. X,Y in {-inf, +inf} ==> X = Y."
		  X Y))))

;;;
;;; QI--: Subtract two elements in (Q \union {-inf, +inf}).
;;;       Note: Subtraction of infinities of opposite signs
;;;        is permitted here, as it is caught and handled
;;;        soundly in the context of interval subtraction
;;;        in i---num below.
;;;

(defun qi-- (x y)
  (cond ((and (rationalp x) (rationalp y))
	 (- x y))
	((and (rationalp x) (inf? y))
	 (negate-inf y))
	((and (inf? x) (rationalp y))
	 x)
	((not (equal x y))
	 (if (equal x '+inf) '-inf '+inf))
	(t (break "X (~A) and Y (~A) must be in (Q \union {-inf, +inf}), s.t. X,Y in {-inf, +inf} ==> X != Y."
		  X Y))))

;;;
;;; QI-*: Multiply two elements in (Q \union {-inf, +inf}.
;;;       Note that this multiplication of +-inf's and 0 is sound
;;;       for its use in interval containment.
;;;

(defun qi-* (x y)
  (cond ((and (rationalp x) (rationalp y))
	 (* x y))
	((and (rationalp x) (inf? y))
	 (if (= x 0) 0
	   (if (< x 0) (negate-inf y) 
	     y)))
	((and (inf? x) (rationalp y))
	 (if (= y 0) 0
	   (if (< y 0) (negate-inf x)
	     x)))
	((and (inf? x) (inf? y))
	 (cond ((equal x y)
		'+inf)
	       (t '-inf)))))

;;;
;;; I-EMPTY?: Is an interval empty?
;;;

(defun i-empty? (i)
  (or (equal i 'empty)
      (let ((i-bt (i-boundary-type i))
	    (i-lb (i-lb i)) (i-ub (i-ub i)))

	;;
	;; There are two reasons an interval will be empty:
	;; 
	;; (a) i=[x,y] with
	;;                  (x>y) \/ (x=+-inf) \/ (y=-inf),
	;;
	;; (b) i=[x,y[ or i=]x,y] or i=]x,y[ with
	;;                 (x>=y) \/ (x=+-inf) \/ (y=-inf).
	;;

    (case i-bt
	  (0 (or (i-> i-lb i-ub)
		 (equal i-lb '+inf)
		 (equal i-ub '-inf)))
	  (otherwise
	   (or (i->= i-lb i-ub)
	       (equal i-lb '+inf)
	       (equal i-ub '-inf)))))))

;;;
;;; I-OP-COND$: A simple macro for conditionally operating on intervals.
;;;

(defmacro i-op-cond$ (&rest rst)
  `(let ((i0-bt (i-boundary-type i0)) (i1-bt (i-boundary-type i1))
	 (i0-lb (i-lb i0)) (i0-ub (i-ub i0))
	 (i1-lb (i-lb i1)) (i1-ub (i-ub i1))
	 (i0-ol? (i-ol? i0)) (i1-ol? (i-ol? i1))
	 (i0-or? (i-or? i0)) (i1-or? (i-or? i1))
	 (i0-empty? (i-empty? i0))
	 (i1-empty? (i-empty? i1)))
     (let ((i0-cl? (not i0-ol?)) (i1-cl? (not i1-ol?))
	   (i0-cr? (not i0-or?)) (i1-cr? (not i1-or?)))
     (cond ,@rst))))

;;;
;;; I-+-NUM: Add two numerical intervals.
;;;

(defun i-+-num (i0 i1)
  (if (or (i-empty? i0) 
	  (i-empty? i1))
      'empty
    (i-op-cond$
     (t (make-interval
	 (i-cl-sel (and i0-cl? i1-cl?))
	 (qi-+ i0-lb i1-lb)
	 (qi-+ i0-ub i1-ub)
	 (i-cr-sel (and i0-cr? i1-cr?)))))))

;;;
;;; I---Num: Subtract two numerical intervals.
;;;

(defun i---num (i0 i1)
  (if (or (i-empty? i0)
	  (i-empty? i1))
      'empty
    (i-op-cond$
     ((or i0-empty? i1-empty?) 'empty)
     (t (make-interval
	 (i-cl-sel (and i0-cl? i1-cr?))
	 (let ((q0 (qi-- i0-lb i1-ub)))
	   (if (inf? q0) '-inf q0))
	 (let ((q1 (qi-- i0-ub i1-lb)))
	   (if (inf? q1) '+inf q1))
	 (i-cr-sel (and i0-cr? i1-cl?)))))))

;;
;; A simple linear recursive min/max function.  This is used for
;; computing the bounds on a multiplication.
;;
;; Given a list of numbers, this function returns the minimal/maximal
;; element.
;;

(defun list-minmax (l g op)
  (cond ((endp l) g)
        (t (list-minmax
            (cdr l)
            (case op
              (min (min (car l) g))
              (max (max (car l) g))) op))))

(defun list-min (l)
  (list-minmax l (car l) 'min))

(defun list-max (l)
  (list-minmax l (car l) 'max))

;;;
;;; SORT-BCS: Sort boundary candidates.
;;;

(defun sort-bcs (bcs)
  (sort bcs #'(lambda (x y) 
		(let ((b0-v (car x))
		      (b1-v (car y)))
		  (i-< b0-v b1-v)))))

;;;
;;; MULT-CL?: Should the product of two boundaries result
;;;  in a closed boundary type?
;;;
;;;  This is true iff
;;; 
;;;          [closed(b0-bt) /\  closed(b1-bt)]
;;;       \/ [b0-v = 0      /\  closed(b0-bt)] 
;;;       \/ [b1-v = 0      /\  closed(b1-bt)].
;;;

(defun mult-cl? (b0-v b0-bt b1-v b1-bt)
  (or (and b0-bt b1-bt)
      (and (i-= b0-v 0) b0-bt)
      (and (i-= b1-v 0) b1-bt)))

;;;
;;; I-*-Num: Multiply two numerical intervals.
;;;

(defun i-*-num (i0 i1)
  (if (or (i-empty? i0) (i-empty? i1))
      'empty
    (i-op-cond$
     (t
   
      ;;
      ;; So, i0 * i1 = <min(lst), max(lst),
      ;;                 where
      ;;                lst = (i0-lb * i1-lb,
      ;;                       i0-lb * i1-ub,
      ;;                       i0-ub * i1-lb,
      ;;                       i0-ub * i1-ub), and
      ;;
      ;; `<' and '>' are determined by the logic below.
      ;;
    
      ;;
      ;; Compute a list of boundary candidates, together
      ;;  with their boundary type (closed=T, open=NIL).
      ;;

      (let ((c0-val (qi-* i0-lb i1-lb)) 
	    (c0-cl? (mult-cl? i0-lb i0-cl? i1-lb i1-cl?))
	    (c1-val (qi-* i0-lb i1-ub)) 
	    (c1-cl? (mult-cl? i0-lb i0-cl? i1-ub i1-cr?))
	    (c2-val (qi-* i0-ub i1-lb)) 
	    (c2-cl? (mult-cl? i0-ub i0-cr? i1-lb i1-cl?))
	    (c3-val (qi-* i0-ub i1-ub)) 
	    (c3-cl? (mult-cl? i0-ub i0-cr? i1-ub i1-cr?)))

	(let ((bc-lst
	       (list (cons c0-val c0-cl?)
		     (cons c1-val c1-cl?)
		     (cons c2-val c2-cl?)
		     (cons c3-val c3-cl?))))

	  (let* ((sorted-bc-lst (sort-bcs bc-lst))
		 (lbc (nth 0 sorted-bc-lst))
		 (oth-lbc (nth 1 sorted-bc-lst))
		 (oth-ubc (nth 2 sorted-bc-lst))
		 (ubc (nth 3 sorted-bc-lst)))
	  
	    (let ((lb-val (car lbc))
		  (lb-bt (if (i-= (car lbc) (car oth-lbc))
			     (or (cdr lbc) (cdr oth-lbc))
			   (cdr lbc)))
		  (ub-val (car ubc))
		  (ub-bt (if (i-= (car ubc) (car oth-ubc))
			     (or (cdr ubc) (cdr oth-ubc))
			   (cdr ubc))))

	      (make-interval (i-cl-sel lb-bt)
			     lb-val
			     ub-val
			     (i-cr-sel ub-bt))))))))))

;;
;; I-INT-CL?: Should the left boundary of intersected
;;  intervals be closed?
;;

(defun i-int-cl? (x y x-cl? y-cl?)
  (cond ((i-> x y) x-cl?)
	((i-> y x) y-cl?)
	(t (and x-cl? y-cl?))))

;;
;; I-INT-CR?: Should the right boundary of intersected
;;  intervals be closed?
;;

(defun i-int-cr? (x y x-cr? y-cr?)
  (cond ((i-< x y) x-cr?)
	((i-< y x) y-cr?)
	(t (and x-cr? y-cr?))))

;;
;; I-MAX: Given two extended rationals, return the max
;;  of them.
;;

(defun i-max (x y)
  (if (i->= x y) x y))

;;
;; I-MIN: Given two extended rationals, return the min
;;  of them.
;;

(defun i-min (x y)
  (if (i-<= x y) x y))

;;
;; I-INTERSECT-NUM: Intersect two numerical intervals.
;;

(defun i-intersect-num (i0 i1)
  (cond ((or (i-empty? i0) (i-empty? i1)) 'empty)
   (t

    ;;
    ;; We will act initially as if we are intersecting two
    ;;  closed intervals, and afterwards will adjust the
    ;;  boundary types of the resulting intersection.
    ;;
    ;; First, we form [x,y] INT [u,v].
    ;;
    ;; To be non-empty, we need:
    ;;  - [x,y] and [u,v] non-empty,
    ;;  - x <= u <= y   -OR-  u <= x <= v.
    ;;

    (i-op-cond$
     ((and (not (and (i-<= i0-lb i1-lb)
		     (i-<= i1-lb i0-ub)))
	   (not (and (i-<= i1-lb i0-lb)
		     (i-<= i0-lb i1-ub))))
      'empty)
     (t (let ((i-candidate 
	       (make-interval
		(i-cl-sel 
		 (i-int-cl? i0-lb i1-lb i0-cl? i1-cl?))
		(i-max i0-lb i1-lb)
		(i-min i0-ub i1-ub)
		(i-cr-sel 
		 (i-int-cr? i0-ub i1-ub i0-cr? i1-cr?)))))
	  (if (i-empty? i-candidate)
	      'empty
	    i-candidate)))))))
