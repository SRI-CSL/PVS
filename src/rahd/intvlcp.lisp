;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Interval arithmetic constraint propagation **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         08-Nov-2009,
;;;            last updated on  16-Nov-2009.
;;;

;;;
;;; Note that these functions are able to operate with all 
;;;  ordered-ring predicates: {=,<,>,<=,>=}, not just
;;;  equations and strict inequalities like most of the RAHD
;;;  CMFs.  This is because we wish interval constraint
;;;  propagation to be used even at the top *G* goal level
;;;  before non-strict expansion.
;;;
;;; * Should I have NOT's eliminated first?
;;;

;;;
;;; IHT-SEEK-BOUND: Given a hash-table of term/interval-bindings 
;;;  (IHT) and a term tm, return either the known binding of tm 
;;;  in IHT, or return ]-inf, +inf[.
;;;

(in-package :rahd)

(defun iht-seek-bound (tm iht)
  (multiple-value-bind
   (i e?)
   (gethash tm iht)
   (if (not e?)
       (make-interval '] '-inf '+inf '[)
     i)))

;;;
;;; IHT-BOUND-TERM:
;;;
;;; Given a term and a hash-table of interval bindings for
;;;  its variables, return an interval that contains the
;;;  value of the term.
;;;

(defun iht-bound-term (tm iht)
  (cond ((numberp tm)
	 (make-closed-interval tm tm))
	((varp tm)
	 (iht-seek-bound tm iht))
	(t (let ((op (car tm))
		 (t0 (cadr tm))
		 (t1 (caddr tm)))
	     (let ((i-t0 (iht-bound-term t0 iht))
		   (i-t1 (iht-bound-term t1 iht)))
	       (case op
		     (+ (i-+-num i-t0 i-t1))
		     (- (i---num i-t0 i-t1))
 		     (* (i-*-num i-t0 i-t1))))))))

;;;
;;; I-TO-STATUS: Given an interval, return :UNSAT if it is
;;;  empty, return :UNKNOWN otherwise.
;;;

(defun i-to-status (i)
  (if (i-empty? i) ':UNSAT ':UNKNOWN))

;;;
;;; IHT-UPDATE: Given a term and an interval, update IHT with
;;;  the interval for term and return the new IHT.
;;;

(defun iht-update (iht tm i)
  (fmt 10 ">> Interval tightened: ~A in ~A.~%"
       tm i)
  (setf (gethash tm iht) i)
  iht)

;;;
;;; I-EX-RATIONAL-BOUND?: Does an interval have a rational 
;;;  boundary component?
;;;

(defun i-ex-rational-bound? (i)
  (or (rationalp (i-lb i))
      (rationalp (i-ub i))))

;;;
;;; I-LE-RATIONAL-BOUND: The least rational bound of an interval,
;;;  with an error if one does not exist.  We return multiple
;;;  values of the form (lrb lrb-bt-closed?) with lrb the least
;;;  rational boundary and lrb-bt-closed? true iff the boundary
;;;  type associated with the lrb value is closed, giving preference
;;;  to the left.
;;;

(defun i-le-rational-bound (i)
  (let ((lb (i-lb i))
	(ub (i-ub i)))
    (let ((r-lb? (rationalp lb))
	  (r-ub? (rationalp ub)))
      (cond
       ((and r-lb? r-ub?) 
	(values (min lb ub) (not (i-ol? i))))
       (r-lb? (values lb (not (i-ol? i))))
       (r-ub? (values ub (not (i-or? i))))
       (t (break "The interval ~A has no rational boundaries." i))))))

;;;
;;; I-GR-RATIONAL-BOUND: The greatest rational bound of an interval,
;;;  with an error if one does not exist.  As above, we return an mv
;;;  form with the (closedness) of the boundary type of the contributing
;;;  rational interval, giving preference to the right.
;;;

(defun i-gr-rational-bound (i)
  (let ((lb (i-lb i))
	(ub (i-ub i)))
    (let ((r-lb? (rationalp lb))
	  (r-ub? (rationalp ub)))
      (cond
       ((and r-lb? r-ub?) 
	(values (max lb ub) (not (i-or? i))))
       (r-lb? (values lb (not (i-ol? i))))
       (r-ub? (values ub (not (i-or? i))))
       (t (break "The interval ~A has no rational boundaries." i))))))

;;;
;;; I-REFINE->=: Given x, y s.t. (x >= y) via intervals I(x),
;;;  I(y), return refined intervals for x and y taking the >=
;;;  relation into account.
;;;

(defun i-refine->= (i-x i-y)
  (let ((refined-i-x i-x)
	(refined-i-y i-y))

    ;;
    ;;  So, x >= y  
    ;;      |- I(x) \subseteq (I(x) \cap @lb(y), +inf[),
    ;;          where @ is the left boundary of I(y).
    ;;
    ;;  And, symmetrically:
    ;;      x >= y == y <= x
    ;;      |- I(y) \subseteq (I(y) \cap ]-inf, ub(x)@),
    ;;          where @ is the right boundary of I(x).
    ;;
    
    (setq refined-i-x
	  (i-intersect-num
	   i-x
	   (make-interval
	    (i-l i-y)
	    (i-lb i-y)
	    '+inf
	    '[)))

    (when (not (i-empty? refined-i-x))
      (let ((new-i-x-ub (i-ub refined-i-x)))
	(setq refined-i-y
	      (i-intersect-num
	       i-y
	       (make-interval
		']
		'-inf
		new-i-x-ub
		(i-r refined-i-x))))))

    (values refined-i-x refined-i-y)))

;;;
;;; I-REFINE->: Given x, y s.t. (x > y) via intervals I(x),
;;;  I(y), return refined intervals for x and y taking the >
;;;  relation into account.
;;;

(defun i-refine-> (i-x i-y)
  (let ((refined-i-x i-x)
	(refined-i-y i-y))

  ;;
  ;;  So, x > y  
  ;;      |- I(x) \subseteq (I(x) \cap ]lb(y), +inf[).
  ;;
  ;;  And, symmetrically:
  ;;      x > y == y < x
  ;;      |- I(y) \subseteq (I(y) \cap ]-inf, ub(x)[).
  ;;
  
  (setq refined-i-x
	(i-intersect-num 
	 i-x
	 (make-interval '] (i-lb i-y) '+inf '[)))
  
  (when (not (i-empty? refined-i-x))
    (setq refined-i-y
	  (i-intersect-num
	   i-y
	   (make-interval '] '-inf (i-ub i-x) '[))))

  (values refined-i-x refined-i-y)))
  
;;;
;;; IHT-TIGHTEN-BY-LIT:
;;;
;;; Given an IHT and a literal constraint, attempt to tighten
;;;  the intervals of terms in the constraint and return an
;;;  updated IHT.  Note, this function will do no solving for
;;;  variables.  Other functions will take care of that and
;;;  use this function as a subsidiary processor.
;;;
;;; This function returns a multiple-valued output of the form:
;;;  (v0=new-iht, v1=status) where status is :UNKNOWN or :UNSAT,
;;;  corresponding to whether or not the literal constraint is
;;;  either possibly satisfiable or surely unsatisfiable under 
;;;  the given IHT context.
;;;

(defun iht-tighten-by-lit (l iht)
  (let ((op (car l))
	(x (cadr l))
	(y (caddr l)))
    (if (and (rationalp x) (rationalp y))
	
	;;
	;; If the literal is strictly numeric (e.g., ground), then
	;;  no intervals in IHT will be contracted.  But, we still 
	;;  may recognise a possible inconsistency, so we check and 
	;;  return the appropriate status.
	;;

	(values iht 
		(if (case op
			  (>= (>= x y))
			  (<= (<= x y))
			  (= (= x y))
			  (> (> x y))
			  (< (< x y)))
		    ':UNKNOWN ':UNSAT))
		
      ;;
      ;; Otherwise, we have some non-numeric terms.  Let's see
      ;;  what we can learn about their containing intervals.
      ;;

      (let ((known-i-x (iht-seek-bound x iht))
	    (known-i-y (iht-seek-bound y iht)))

	(let ((new-i-x (iht-bound-term x iht))
	      (new-i-y (iht-bound-term y iht)))

	  (let ((intersected-i-x
		 (i-intersect-num known-i-x new-i-x))
		(intersected-i-y
		 (i-intersect-num known-i-y new-i-y)))

	    (if (or (i-empty? intersected-i-x)
		    (i-empty? intersected-i-y))
		(values (iht-update 
			 (iht-update iht y intersected-i-y)
			 x intersected-i-x)
			':UNSAT)
	      
	      ;;
	      ;; Now, let's use OP to further tighten our
	      ;;  term interval bounds.
	      ;;
	      ;; * Recall that `i0',`i1' need to be the interval
	      ;;   bindings for the i-op-cond$ macro.
	      ;;

	      (let ((i0 intersected-i-x)
		    (i1 intersected-i-y))
		
		(i-op-cond$

		  ;;
		  ;; We must be very careful here with the manipulation of
		  ;;  infinite interval boundaries.  In fact, if neither 
		  ;;  I(x) nor I(y) contain any rational boundaries, then
		  ;;  we will not be able to tighten any intervals beyond
		  ;;  what was done above, so we skip the processing logic
		  ;;  that follows.
		  ;;

		  ((or (i-ex-rational-bound? i0)
		       (i-ex-rational-bound? i1))

		    (case op

			  ;;
			  ;; If we're here, then we know
			  ;;  (i) both i0 and i1 are non-empty, and
			  ;; (ii) either i0 or i1 contain at least one rational
			  ;;       boundary (possibly both).
			  ;;

			  (>= (multiple-value-bind
			       (refined-i0 refined-i1)
			       (i-refine->= i0 i1)
			       (setq i0 refined-i0)
			       (setq i1 refined-i1)))

			  (<= (multiple-value-bind 
			       (refined-i1 refined-i0)
			       (i-refine->= i1 i0)
			       (setq i0 refined-i0)
			       (setq i1 refined-i1)))

			  ;;
			  ;; In the case of an equality, both intervals
			  ;;  can be simply refined to their intersection.
			  ;;

			  (= (let ((new-intersection
				    (i-intersect-num i0 i1)))
			       (setq i0 new-intersection)
			       (setq i1 new-intersection)))

			  (> (multiple-value-bind
			      (refined-i0 refined-i1)
			      (i-refine-> i0 i1)
			      (setq i0 refined-i0)
			      (setq i1 refined-i1)))

			  (< (multiple-value-bind
			      (refined-i1 refined-i0)
			      (i-refine-> i1 i0)
			      (setq i0 refined-i0)
			      (setq i1 refined-i1))))))

			  ;;
			  ;; Now, we have refined intervals for x and y.
			  ;;  Let's update these values in the IHT, and then
			  ;;  flag UNSAT in our mv return if either intervals
			  ;;  are empty.
			  ;;

			  (values
			   (iht-update (iht-update iht y i1) x i0)
			   (if (or (i-empty? i0) (i-empty? i1))
			       ':UNSAT ':UNKNOWN))))))))))


;;;
;;; IHT-UNSAT?: Is an IHT unsatisfiable?  We check by seeing
;;;  if it contains any empty intervals.
;;;


;;;
;;; IHT-CLONE: Given an IHT, produce a fresh clone of it.
;;;  Note, only the table itself is cloned, not its values.
;;;

(defun iht-clone (iht)
  (let ((new-iht (make-hash-table :test 'equal)))
    (maphash
     #'(lambda (key value)
	 (setf (gethash key new-iht) value))
     iht)
    new-iht))

;;;
;;; IHT-EQUAL?: Do two IHTs contain same intervals?
;;;

(defun iht-equal? (iht0 iht1)
  (if (not (and (hash-table-p iht0)
		(hash-table-p iht1)))
      nil
    (let ((e? t))
      (maphash 
       #'(lambda (key value)
	   (when (not (equal value
			     (gethash key iht1)))
	     (setq e? nil)))
       iht0)
      e?)))

;;;
;;; ICP-ON-CASE: Perform ICP on a conjunctive case, but
;;;  do it with a fresh (e.g., empty) IHT context unless one
;;;  is optionally given, *and* do ICP until we reach a fixed
;;;  point on the internal IHT.  This is because the order
;;;  that constraints are processed can lead boundaries to 
;;;  not be tightened as much as possible, and can thus cause
;;;  inconsistencies to be missed. So, we iterate until a
;;;  fixed point is reached.
;;;

(defun icp-on-case (c &optional iht &key max-contractions)
  (let ((internal-iht (or iht (make-hash-table :test 'equal)))
	(prev-iht nil)
	(icp-output nil)
	(max-cs (or max-contractions 1000))
	(count 0))
    (loop while (and (< count max-cs) 
		     (not (iht-equal? internal-iht prev-iht))) 
	  do
	  (setq prev-iht (iht-clone internal-iht))
	  (multiple-value-bind
	   (tightened-iht status)
	   (icp-on-case-with-iht c internal-iht 
				 :max-contractions max-cs
				 :count count)
	   (setq icp-output status)
	   (setq internal-iht tightened-iht)
	   (setq count (1+ count)))
	  (fmt 10 "[icp] contraction count: ~A~%" count))
    icp-output))

;;;
;;; ICP-ON-CASE-WITH-IHT: Given a conjunctive case and an IHT,
;;;  update IHT and return either ':UNKNOWN or ':UNSAT based
;;;  upon interval tightening upon literals.
;;;
;;; Note we return an mv here with (iht [c -or- :UNSAT just.]).
;;;

(defun icp-on-case-with-iht (c iht &key max-contractions count)
  (let ((tightened-iht iht)
	(unsat? nil)
	(icount count))
    
    (dolist (l c)
      
      (setq icount (1+ icount))
      
      ;;
      ;; Tighten the IHT based upon a literal constraint.
      ;;
      
      (multiple-value-bind
       (updated-iht status)
       (iht-tighten-by-lit l tightened-iht)

       (setq tightened-iht updated-iht)
       
       (when (and (not unsat?)
		  (equal status ':UNSAT))
	 (setq unsat? l)))

      (when (> icount max-contractions) 
	(return nil)))

    (values 
     tightened-iht
     (if unsat?
	 (let* ((lhs (cadr unsat?))
		(rhs (caddr unsat?))
		(lhs-interval (iht-bound-term lhs tightened-iht))
		(rhs-interval (iht-bound-term rhs tightened-iht)))
	   
	   `(:UNSAT :INTERVAL-CONTRACTION
		    :LITERAL-WITH-EMPTY-REALIZER
		    ,unsat?
		    :LHS-INTERVAL
		    ,lhs-interval
		    :RHS-INTERVAL
		    ,rhs-interval))
       c))))


;;;
;;; IHT-INFO: Given a term TM, print what we know about TM w.r.t. 
;;;  the active interval hash-tables.
;;;

(defun iht-info (tm &optional infix?)
  (fmt 0 "~%~% Interval Hash-Table Info:~%~% Term:    ~A,~% Interval: ~A.~%~%"
       (if infix? (poly-prover-rep-to-alg-rep tm) tm)
       (iht-seek-bound tm *i-boxes-num-local*))
  t)

;;;
;;; CLONE-I-BOXES-FOR-CASES: Given an IHT and a number k, make an
;;;  array of size k of cloned IHTs.  This is used to allow the local
;;;  case contexts to inherit their initial interval boxes from the
;;;  global IHT.
;;;

(defun clone-i-boxes-for-cases (iht k)
  (let ((cases-i-boxes-array 
	 (make-array k :element-type 'hash-table)))
    (loop for i from 0 to (1- k) do
	  (setf (aref cases-i-boxes-array i) 
		(iht-clone iht)))
    cases-i-boxes-array))
