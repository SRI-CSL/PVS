;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/24 21:51:32
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defvar *use-rewrite-hash* t)
(defvar *use-rewrite-index* t)
(defvar *use-match-automaton* nil)
(defvar *print-rewrite* t)
(defvar *destructive-rewrite* nil)

(defdpfield rewritten)
(defdpfield hash-consed)
(defdpfield normal-form)

(defdpstruct (rewrite-rules
	      (:print-function
	       (lambda (rr s k)
		 (declare (ignore k))
		 (format s "<~D Rewrite rules>"
		   (+ (length (rewrite-rules-rules rr))
		      (length (rewrite-rules-rules! rr)))))))
  (rules nil)
  (rules! nil)
  (hash (dp-make-eq-hash-table))
  (index-hash (dp-make-eq-hash-table)))

(defun initial-rewrite-rules ()
  (make-rewrite-rules))

(defun copy-rewrite-rules-and-hash (new-rewrite-rules old-rewrite-rules)
  (setf (rewrite-rules-rules new-rewrite-rules)
	(rewrite-rules-rules old-rewrite-rules))
  (setf (rewrite-rules-rules! new-rewrite-rules)
	(rewrite-rules-rules! old-rewrite-rules))
  (copy-hash-table (rewrite-rules-index-hash new-rewrite-rules)
		   (rewrite-rules-index-hash old-rewrite-rules))
  new-rewrite-rules)

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
  (condition *true*)
  (always? nil)
  (matching-automaton nil :type list)
  (matching-slots nil :type array)
  (rhs-automaton nil :type list)
  (rhs-loc nil :type integer)
  (condition-automaton nil :type list)
  (condition-loc nil :type integer)
  (if-cond-automaton nil :type list)
  (if-cond-loc nil :type integer)
  (if-then-automaton nil :type list)
  (if-then-loc nil :type integer)
  (if-else-automaton nil :type list)
  (if-else-loc nil :type integer))

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
  (clrhash (rewrite-rules-hash rewrite-rules))
  (clrhash (rewrite-rules-index-hash rewrite-rules))
  rewrite-rules)

(defun extend-subst (subst var term)
  (acons var term subst))

(defun subst-in-application (appl subst)
  (mk-term (map-args-list #'subst-in-node appl subst)))

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
      (multiple-value-bind (new-subst success)
	  (match-args term lhs subst)
	(if success (values new-subst t)
	    ;;; Put e-matching here
	    ;;; look for terms that are equivalent to term
	    ;;; that can possibly match lhs.
	    (match-arith term lhs subst)))))

(defun match-arith (term lhs subst)
  (if (and (plus-p lhs)
	   (dp-numberp (arg 1 lhs)))
      (let ((new-term (sigdifference (mk-difference term (arg 1 lhs))))
	    (new-lhs (sigdifference (mk-difference lhs (arg 1 lhs)))))
	(match new-term new-lhs subst))
      (values subst nil)))

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

(defun getrewhash (term cong-state)
  (gethash term (rewrite-rules-hash (rewrite-rules cong-state))))

(defun setf-getrewhash (term cong-state new-term)
  (setf (gethash term (rewrite-rules-hash (rewrite-rules cong-state)))
	new-term))

(defsetf getrewhash setf-getrewhash)

(defun print-rewrite* (term new-term)
  (when *print-rewrite*
    (format t "~%Rewriting:")
    (ppr term)
    (format t "~%to~%")
    (ppr new-term)))


(defun normalize-term-top (term cong-state)
  "Main entry point for rewriting."
  (normalize-term term cong-state))

(defun normalize-term (term cong-state)
  "Left-most, inner-most."
  (let ((hash-term (and *use-rewrite-hash*
			(getrewhash term cong-state))))
    (cond
     (hash-term (if (eq hash-term '*no-change*)
		    (values term nil)
		    (values hash-term t)))
     (t
      (multiple-value-bind (new-term change)
	  (normalize-term-no-memo term cong-state)
	;;; do not know that this assert is real.
	;;; might only be for debugging.
	(assert (not
		 (and (application-p new-term)
		      (not (record-p new-term))
		      (record-p (funsym new-term)))))
	(cond
	 (change (when *print-rewrite*
		   ;;; There might be better places to trace the rewriting.
		   ;;; or not.
		   (print-rewrite* term new-term))
		 (setf (getrewhash term cong-state)
		       new-term)
		 (values new-term t))
	 (t (setf (getrewhash term cong-state)
		  '*no-change*)
	    (values term nil))))))))

(defun normalize-term-no-memo (term cong-state)
  (cond
   ((not (application-p term)) (rewrite-then-normalize term cong-state))
   ((if-p term) (normalize-if-then-else term cong-state))
   (t (multiple-value-bind (new-term args-change)
	  (normalize-args term cong-state)
	(if args-change
	    (values (rewrite-then-normalize new-term cong-state)
		    t)
	    (rewrite-then-normalize term cong-state))))))

(defun normalize-if-then-else (term cong-state)
  (multiple-value-bind (new-cond cond-change)
      (normalize-term (if-cond term) cong-state)
    (cond
     ((true-p new-cond)
      (values (normalize-term (if-then term) cong-state) t))
     ((false-p new-cond)
     (values (normalize-term (if-else term) cong-state) t))
     (cond-change (values old-term t))
     ;;; There are different heuristics for how to handle this case.
     ;;; Choose one or use flags or make it different for different
     ;;; types of rewrite rules as in pvs.
     (cond-change (values (mk-if-then-else new-cond (if-then term)
					   (if-else term))
			  t))
     (t (values term nil)))))

(defun normalize-if-then-else-subst (old-term term subst cong-state)
  "Normalizes if-then-else when given a substitution.
The idea is not to apply the substitution to the then and else
branches unless necessary."
  (multiple-value-bind (new-cond cond-change)
      (normalize-term (apply-subst (if-cond term) subst) cong-state)
    (cond
     ((true-p new-cond)
      (values (normalize-term (apply-subst (if-then term) subst)
			      cong-state) t))
     ((false-p new-cond)
      (values (normalize-term (apply-subst (if-else term) subst)
			      cong-state) t))
     (cond-change (values old-term t))
     ;;; There are different heuristics for how to handle this case.
     ;;; Choose one or use flags or make it different for different
     ;;; types of rewrite rules as in pvs.
     (cond-change (values (mk-if-then-else new-cond
					   (apply-subst (if-then term) subst)
					   (apply-subst (if-else term) subst))
			  t))
     (t (values old-term nil)))))

(defun get-applicable-rewrites (term cong-state)
  (if *use-rewrite-index*
      (gethash (index-operator term)
	       (rewrite-rules-index-hash
		(rewrite-rules cong-state)))
      (append (rewrite-rules-rules (rewrite-rules cong-state))
	      (rewrite-rules-rules! (rewrite-rules cong-state)))))

(defun rewrite-then-normalize (term cong-state)
  "Either term is a leaf or its arguments have already been normalized."
  (multiple-value-bind (new-term change)
      (rewrite-term term (get-applicable-rewrites term cong-state)
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

(defun extended-dp-find (term cong-state)
  "Either does a find wrt cong-state or if term is an inequality
query's the inequality database of cong-state (polyhedron)
for the status of the inequality."
  (cond
   ((ineq-p term)
    (simplify-ineq-constraint term cong-state))
   (t (dp-find term cong-state))))

(defun simplify-wrt-cong-state (term cong-state)
  ;;; There is no need to do a canon here because we are alredy
  ;;; doing innermost simplification.
  (if (bool-p term)
      (nprotecting-cong-state (new-cong-state cong-state)
       (let ((result (dp::invoke-process term new-cong-state)))
	 (cond
	  ((true-p result) *true*)
	  ((false-p result) *false*)
	  (t (sigma (dp-find term cong-state) cong-state)))))
      (sigma (dp-find term cong-state) cong-state)))

(defun simplify-term (term cong-state)
  (let* ((new-term
	  (simplify-wrt-cong-state term cong-state)))
    (if (eq term new-term)
	(values term nil)
	(values new-term t))))

(defun match-rewrite (term rewrite)
  (if *use-match-automaton*
      (match-using-automaton term rewrite)
      (match term (rr-lhs rewrite))))

(defun rewrite-term-1 (term rewrite cong-state)
  (let ((lhs (rr-lhs rewrite))
	(rhs (rr-rhs rewrite))
	(condition (rr-condition rewrite)))
    (multiple-value-bind (subst succ)
	(match-rewrite term rewrite)
      (if succ
	  (if condition
	      (rewrite-term-1-condition term condition rhs subst
					cong-state)
	      (continue-rewrite-1 term rhs subst rewrite cong-state))
	  (values term nil)))))

(defun rewrite-term-1-condition (term condition rhs subst cong-state)
  (let ((norm-condition (normalize-term (apply-subst condition subst)
					cong-state)))
    (if (true-p norm-condition)
	(values (apply-subst rhs subst) t)
	(values term nil))))

(defun continue-rewrite-1 (term rhs subst rewrite cong-state)
  (if (if-p rhs)
      (normalize-if-then-else-subst term rhs subst cong-state)
      (values (normalize-term (apply-subst rhs subst) cong-state) t)))

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
      (let ((new-term (mk-term (map-args-list #'normalize-arg term))))
	(values new-term change)))))

(defun big-size (term)
  "Computes the size of the dag term if all structure sharing were removed."
  (let ((*size-alist* nil))
    (declare (special *size-alist*))
    (big-size* term)))

(defun big-size* (term)
  (let ((ass (assoc term *size-alist* :test #'eq)))
    (declare (special *size-alist*))
    (cond
     (ass (cdr ass))
     (t (let ((r (cond ((leaf-p term) 1)
		       ((application-p term)
			(+ 1 (loop for a in (application-arguments term)
				   sum (big-size* a))))
		       (t 1))))
	  (setq *size-alist* (acons term r *size-alist*))
	  r)))))
