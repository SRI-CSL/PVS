;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Quick SAT Checking based upon contracted term intervals **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         23-Nov-2009,
;;;            last updated on  23-Nov-2009.
;;;

;;;
;;; I-POINTS: Given an interval, return a list of points within it.
;;;

(defun i-points (i v)
  (cond ((i-empty? i) nil)
	(t (let ((lb (i-lb i)) (ub (i-ub i))
		 (l (i-l i)) (r (i-r i))
		 (pts nil))
	     (when (equal l '[)
	       (setq pts (list lb)))
	     (when (equal lb '-inf)
	       (if (rationalp ub)
		   (setq pts (union pts (list (- ub 100) (- ub 10) (1- ub))))
		   (setq pts (union pts '(0 -1 1 -100 100)))))
	     (when (equal ub '+inf)
	       (if (rationalp lb)
		   (setq pts (union pts (list (+ 100 lb) (+ 10 lb) (1+ lb))))
		   (setq pts (union pts '(0 -1 1 -100 100)))))
	     (when (not (equal lb ub))
	       (when (equal r '])
		 (setq pts (union pts (list ub))))
	       (when (and (rationalp lb)
			  (rationalp ub))
		 (setq pts (union pts (list (/ (+ lb ub) 2))))))
	     (if (member v *nz-terms*)
		 (remove-if #'(lambda (x) (= x 0)) pts)
		 pts)))))

;;;
;;; VERIFY-NONZERO: Given a list of terms, make sure none of them
;;;  are both ground and reduce to 0.
;;;

(defun verify-nonzero (tms)
  (cond ((endp tms) t)
	(t (let ((tm (car tms)))
	     (if (and (term-ground? tm)
		      (equal (eval tm) 0))
		 nil
		 (verify-nonzero (cdr tms)))))))


;;;
;;; EVAL-C: Given a case and a set of variable bindings, evaluate
;;;  c upon the bindings, returning the list of bindings only if
;;;  the case is satisfied by them (and NIL otherwise).
;;;
;;; Note: We check to make sure that no terms in *nz-terms* are
;;;  made 0 by the provided bindings.
;;;

(defun eval-c (c bindings)
  (let ((c* c)
	(nzts *nz-terms*))
    (dolist (b bindings)
      (let ((var (car b))
	    (val (cadr b)))
	(fmt 10 "Trying ~A=~A~%" var val)
	(setq c* (subst val var c*))
	(setq nzts (subst val var nzts))))
	(fmt 10 "NZ-terms instantiated: ~A" nzts)
    (let ((r (simplify-ground-lits c*))
	  (z-ok? (verify-nonzero nzts)))
      (if (and z-ok?
	       (reduce #'(lambda (x y) 
			   (and (equal x t) 
				(equal y t))) r :initial-value t))
	       bindings
	  nil))))

;;;
;;; SP-SEARCH: Given a case, a list of variables and corresponding
;;;  lists of sample points, check to see if any combination of the
;;;  sample points satisfies the formula c.
;;;
;;; If a deep-search is enabled, then after each binding is computed,
;;;  the entire waterfall is restarted upon the subproblem.
;;;  ** This isn't implemented yet.
;;;

(defun sp-search (c vs pts bindings &key deep-search?)
  (cond ((endp vs) (eval-c c bindings))
	(t (let ((model-found? nil))
	     (dolist (p (car pts))
	       (let ((new-bindings
		      (cons (list (car vs) p) bindings)))
		 (setq model-found? (sp-search 
				     c 
				     (cdr vs) 
				     (cdr pts) 
				     new-bindings))
	       (when model-found? (return model-found?))))))))

;;;
;;; QUICK-SAT-IHT:
;;;
;;; Given an IHT and a conjunctive case, check for simple rational solutions
;;;  by generating test points within the known variable interval bounds
;;;  contained within IHT.
;;;
;;; If an IHT is not given, we compute a new one 
;;;  (later will use *i-boxes-num-local*).
;;;
;;; If points found, we'll return (:SAT (:MODEL (v_0 r_0) ... (v_n r_n))).
;;;

(defun quick-sat-iht (c &key iht gen-pt-bound)
  (declare (ignore gen-pt-bound))
  (let ((iht* (if (not iht) 
		  (let ((iht** (make-hash-table :test 'equal)))
		    (icp-on-case c iht**)
		    iht**)
		  iht)))
  (let ((vs (all-vars-in-conj c)))
    (let ((vpt-pairs 
	   (mapcar 
	    #'(lambda (v) (let ((i (iht-seek-bound v iht*)))
			    (i-points i v)))
	    vs)))
      (let ((sps-found? (sp-search c vs vpt-pairs nil)))
	(when sps-found? 
	  `(:SAT (:MODEL 
		  ,(union (when *current-tactic-case*
			    (aref *gs* *current-tactic-case* 3))
			  (union *gs-vt-bindings* 
				 (reverse sps-found?)))))))))))

;;;
;;; QSI-ON-CASE: Given a case, return either :SAT (hopefully with QSI model)
;;;  or return the case unchanged.
;;;

(defun qsi-on-case (c &key iht gen-pt-bound)
  (let ((qsi (quick-sat-iht c :iht iht :gen-pt-bound gen-pt-bound)))
    (if qsi (setq *sat-model* qsi) c)))

;;;
;;; QSI-ON-GS-UNKNOWNS: If *GS* contains cases marked :UNKNOWN, then let's
;;;  go through them and check to see if any can be found to be SAT by
;;;  using QUICK-SAT-IHT.
;;;

(defun qsi-on-gs-unknowns ()
  (let ((model-found? nil))
    (if (equal *gs-unknown-size* 0) nil
	(loop for i from 0 to (1- *gs-size*) 
	      while (not model-found?) 
	      do
	      (let ((c (aref *gs* i 1))
		    (c-status (aref *gs* i 2)))
		(when (equal (car c-status) ':UNKNOWN)
		  (let ((qsi (quick-sat-iht c)))
		    (setq model-found? qsi))))))
    model-found?))

;;;
;;; SIMP-VAL: Given a VAL, a list of bindings, and a current binding
;;;  which binds a variable to VAL, if VAL is not a rational, then
;;;  we attempt to apply the other bindings in the list to VAL 
;;;  to make it a ground term, and then a rational.
;;;

(defun simp-val (val bs b)
  (let ((bs* (remove-if #'(lambda (x) (equal x b)) bs))
	(val* val))
    (dolist (b* bs*)
      (let ((svar (car b*))
	    (sval (cadr b*)))
	(when (rationalp sval) 
	  (setq val* (subst sval svar val*)))))
    (if (term-ground? val*) (eval val*) val*)))

;;;
;;; FORMAT-MODEL: Given a (:SAT (:MODEL ((X 0) ...))) form, prepare a 
;;;  nice string with the model.
;;;

(defun format-model (m)
  (let ((m* (reverse (cadadr m))))
    (format nil 
	    " model: [~A"
   (let ((out-str "")
	 (count 0)
	 (total (length m*)))
     (dolist (b m*)
       (let ((var (car b))
	     (val (cadr b)))
	 (setq out-str (format nil "~A~A" out-str
			       (format nil (if (= count 0) 
					       "~A=~A~A"
					       "         ~A=~A~A") 
				       var (if (rationalp val) 
					       val
					       (simp-val val m* b))
				       (if (= count (1- total))
					   (format nil "].~%")
					   (format nil ",~%"))))))
       (setq count (1+ count)))
     out-str))))
