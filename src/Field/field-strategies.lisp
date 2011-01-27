;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIELD is a simplification procedure for the field of reals.
;; Cesar Munoz (munoz@nianet.org)
;; National Institute of Aerospace, February 2003
;; ICASE-NASA LaRC
;; Version: Field.2b (for PVS 3.1 and Manip 1.1)
;; $Id: field-strategies,v 1.1 2007-09-14 19:35:53 owre Exp $
;;
;; Field was originally implemented with the collaboration of 
;; Micaela Mayero (Micaela.Mayero@inria.fr). 
;;

(defun get-const-divisors (n polynom)
  (if polynom
      (let ((na (str2int (caar polynom))))
	(cond ((and na (> (cdar polynom) 0))
	       (get-const-divisors (* n na) (cdr polynom)))
	      (na
	       (get-const-divisors (/ n na) (cdr polynom)))
	      (t (get-const-divisors n (cdr polynom)))))
    n))

(defun un-polynom (polynom e)
  (when polynom
    (let* ((a  (caar polynom))
	   (na (str2int a))
	   (ne (str2int (car e)))
	   (b (cdar polynom))
	   (r (cdr polynom)))
      (cond ((and na ne)
	     (let ((g (lcm na ne)))
	       (if (= g 1) (in-polynom r e)
		 (cons (expr2str g) 1))))
	    ((or na ne)
	     (un-polynom r e))
	    ((string= a (car e))
	     (cons a (max b (cdr e))))
	    (t (un-polynom r e))))))

(defun union-polynom (polynom1 polynom2 l)
  (cond (polynom1
	 (let ((e (un-polynom polynom2 (car polynom1))))
	   (if e
	       (union-polynom (cdr polynom1) polynom2 (cons e l))
	     (union-polynom (cdr polynom1) polynom2 (cons (car polynom1) l)))))
	(polynom2
	 (let ((e (un-polynom l (car polynom2))))
	   (if e
	       (union-polynom polynom1 (cdr polynom2) l)
	     (union-polynom polynom1 (cdr polynom2) (cons (car polynom2) l)))))
	(t l)))

(defun get-divisors-monoms (l monoms)
  (if monoms
      (let* ((divmonom  (remove-if #'(lambda (x) (> (cdr x) 0))
				   (cancel-monom nil (car monoms) 1)))
	     (divpos    (mapcar #'(lambda (x) (cons (car x) (* -1 (cdr x))))
				divmonom)))
	(get-divisors-monoms (union-polynom l divpos nil) (cdr monoms)))
    l))

(defun get-divisors-formula (formula)
  (when (is-relation formula)
    (let ((monoms (get-monoms-formula formula)))
      (get-divisors-monoms nil monoms))))

(defun makeprod (divs names)
  (when divs
    (cons (cons (car names) (cdar divs))
	  (makeprod (cdr divs) (cdr names)))))

(defun makecase-neq0 (names)
  (when names
    (cond ((= (length names) 1) (format nil "~A /= 0" (car names)))
	  (t (format nil "~A /= 0 AND ~A"
		     (car names)
		     (makecase-neq0 (cdr names)))))))
	
(defun get-monoms-expr (l expr)
  (cond ((or (is-infix-operator expr '+)
	     (is-infix-operator expr '-))
	 (get-monoms-expr (get-monoms-expr l (args1 expr))
			  (args2 expr)))
	((is-prefix-operator expr '-)
	 (get-monoms-expr l (args1 expr)))
	(t (cons expr l))))

(defun get-monoms-formula (formula)
  (when (is-relation formula)
    (get-monoms-expr (get-monoms-expr nil (args1 formula))
		     (args2 formula))))

(defun in-polynom (polynom e)
  (when polynom
    (let* ((a  (caar polynom))
	   (na (str2int a))
	   (ne (str2int (car e)))
	   (b (cdar polynom))
	   (r (cdr polynom)))
      (cond ((and ne (= ne 0)) e)
	    ((and na (= na 0)) e)
	    ((and na ne)
	     (let ((g (gcd na ne)))
	       (if (= g 1) (in-polynom r e)
		 (cons (expr2str g) 1))))
	    ((or na ne)(in-polynom r e))
	    ((string= a (car e))
	     (cons a (min b (cdr e))))
	    (t (in-polynom r e))))))

(defun inter-polynom (polynom1 polynom2 l)
  (if (and polynom1 polynom2)
      (let ((e (in-polynom polynom2 (car polynom1))))
	(if e
	    (inter-polynom (cdr polynom1) polynom2 (cons e l))
	  (inter-polynom (cdr polynom1) polynom2 l)))
    l))

;; e = string monom,
;; se is either multiplicity (>0) or divicity (<0)
(defun insert-polynom (polynom e se)
  (let ((ne (str2int e)))
    (cond
     ((and ne (= ne 1)) polynom)
     (polynom
      (let* ((a  (caar polynom))
	     (na (str2int a))
	     (sa (cdar polynom)))
	(cond ((and ne na)
	       (cond ((> (* se sa) 0)
		      (insert-polynom (cdr polynom)
				      (expr2str (* na ne))
				      sa))
		     ((= na ne) (cdr polynom))
		     (t (let ((g   (gcd ne na)))
			  (cond ((= na g)
				 (insert-polynom (cdr polynom)
						 (expr2str (/ ne g))
						 se))
				((= ne g)
				 (insert-polynom (cdr polynom)
						 (expr2str (/ na g))
						 sa))
				(t (cons (cons (expr2str (/ na g)) sa)
					 (insert-polynom (cdr polynom)
							 (expr2str (/ ne g))
							 se))))))))
	      (ne (cons (cons e se) polynom))
	      (na (cons (car polynom) (insert-polynom (cdr polynom) e se)))
	      ((string= e a)
	       (let ((count (+ se sa)))
		 (if (= count 0)
		     (cdr polynom)
		   (cons (cons e count) (cdr polynom)))))
	      ((> (length a) (length e))
	       (cons (cons e se) polynom))
	      (t (cons (car polynom) (insert-polynom (cdr polynom) e se))))))
     (t (list (cons e se))))))

(defun get-mults-monom (l expr)
  (cond ((is-infix-operator expr '*)
	 (get-mults-monom (get-mults-monom l (args1 expr)) (args2 expr)))
	((is-prefix-operator expr '-)
	 (get-mults-monom (insert-polynom l "-1" 1) (args1 expr)))
	(t (insert-polynom l (expr2str expr) 1))))

;; numden > 0 is numerator, numden < 0 is denominator
(defun cancel-monom (l expr numden)
  (cond ((is-infix-operator expr '/)
	 (cancel-monom (cancel-monom l (args1 expr) numden)
		       (args2 expr) (* -1 numden)))
	((is-infix-operator expr '*)
	 (cancel-monom (cancel-monom l (args1 expr) numden)
		       (args2 expr) numden))
	((is-prefix-operator expr '-)
	 (cancel-monom (insert-polynom l "-1" numden) (args1 expr) numden))
 	(t (insert-polynom l (expr2str expr) numden))))

(defun has-divisors (expr)
  (cond ((is-infix-operator expr '/) t)
	((or (is-infix-operator expr '+)
	     (is-infix-operator expr '-)
	     (is-infix-operator expr '*)
	     (is-relation expr))
	 (or (has-divisors (args1 expr))
	     (has-divisors (args2 expr))))
	((is-prefix-operator expr '-)
	 (has-divisors (args1 expr)))
	(t nil)))

(defun exp-n (e n)
  (cond ((= (abs n) 1) (format nil "~A" e))
	((> (abs n) 0) (format nil "~A * ~A" e
			       (exp-n e (- (abs n) 1))))
	(t "1")))

(defun normal-mult-parens (l)
  (cond ((= (length l) 1) (exp-n (caar l) (cdar l)))
	(t (format nil "~A * ~A"
		   (exp-n (caar l) (cdar l))
		   (normal-mult-parens (cdr l))))))

(defun normal-mult (l)
  (cond ((not l) "1")
        ((and (= (length l) 1)
	      (= (abs (cdar l)) 1))
	 (caar l))
	(t (format nil "(~A)" (normal-mult-parens l)))))

(defun normal-term (numden)
  (let ((num (mapcan #'(lambda (p) (and (> (cdr p) 0) (list p))) numden))
	(den (mapcan #'(lambda (p) (and (< (cdr p) 0) (list p))) numden)))
    (cond (numden
	   (cond (num
		  (cond (den (format nil "~A / ~A"
				     (normal-mult num)
				     (normal-mult den)))
			(t (normal-mult num))))
		 (t (format nil "1 / ~A" (normal-mult den)))))
	  (t "1"))))

(defun makecases-monoms (monoms)
  (when monoms
    (let ((monom  (car monoms)))
      (if (has-divisors monom)
	  (let ((cmonom (normal-term (cancel-monom nil monom 1))))
	    (if (string= (format nil "~A" monom) cmonom)
		(makecases-monoms (cdr monoms))
	      (cons (format nil "~A = ~A" monom cmonom)
		    (makecases-monoms (cdr monoms)))))
	(makecases-monoms (cdr monoms))))))

(defhelper cases-monoms__ (label cases)
  (if cases
      (let ((frel  (car cases))
	    (frels (cdr cases)))
	   (branch (case frel)
		   ((then (replaces -1 label)
			  (cases-monoms__$ label frels))
		    (then (delabel label :hide? T)
			  (grind-reals)))))
      (assert label))
  "[Field Package: Internal strategy]"
  ""
)

(defstrat simplify-monoms__ (label)
  (let ((flag (has-divisors (extra-get-formula label))))
       (when flag
	 (then (assert label)
	       (let ((formula (extra-get-formula label))
		     (monoms  (get-monoms-formula formula))
		     (cases   (makecases-monoms monoms)))
		    (cases-monoms__$ label cases)))))
  "[Field Package: Internal strategy]"
)

(defstep cancel-by (fnum expr &optional (theories "extra_tegies"))
  (let
   ((formula   (extra-get-formula fnum))
    (msg1      (no-formula fnum))
    (rel       (when formula (is-relation formula)))
    (msg2      (no-relation fnum))
    (term-expr (when rel (virt-ee-string (car (eval-ext-expr expr)))))
    (msg3      (no-suitable-expr expr)))
   (if (and rel term-expr)
       (let
	((div       (newname "CBdiv"))
	 (inv_div   (format nil "1 / ~A" div))
	 (nz_div    (format nil "~A = 0" div))
	 (gt0_div   (format nil "~A > 0" div)))
	(then
	 (relabel "CB:" fnum)
	 (else-cancel-by__
	  theories
	  (then@ (name-replace-label div term-expr :label "CBdiv:")
		 (name-distrib ("CBdiv:" "CB:") :prefix "NDC" :label "NDC:"))
	  (let
	   ((is_eq   (equation? (extra-get-formula "CB:"))))
	   (then
	    (branch
	     (case nz_div)
	     ((then (replace "CBdiv:" :dir rl)
		    (delabel "CBdiv:" :hide? t)
		    (real-props :theories theories)
		    (replaces "NDC:" :but "NDC:" :dir RL :hide? nil)
		    (delabel "NDC:" :hide? t))
	      (if is_eq
		  (then
		   (mult-by__ "CB:" inv_div)
		   (replace "CBdiv:" :dir rl)
		   (delabel "CBdiv:" :hide? t)
		   (simplify-monoms__ "CB:")
		   (replaces "NDC:" :but "NDC:" :dir RL :hide? nil)
		   (delabel "NDC:" :hide? t)
		   (real-props "CB:" :theories theories))
		  (branch
		   (case gt0_div)
		   ((then (try (cancel-by-guess__ theories)(fail)(skip))
			  (else-cancel-by__
			   theories
			   (mult-by__ "CB:" inv_div +)
			   (then (replaces "CBdiv:" :dir RL :hide? nil)
				 (delabel "CBdiv:" :hide? t)
				 (simplify-monoms__ "CB:")
				 (replaces "NDC:" :but "NDC:"
					   :dir RL :hide? nil)
				 (delabel "NDC:" :hide? t)
				 (real-props "CB:" :theories theories))))
		    (then (try (cancel-by-guess__ theories)(fail)(skip))
			  (else-cancel-by__
			   theories
			   (mult-by__ "CB:" inv_div -)
			   (then (replaces "CBdiv:" :dir RL :hide? nil)
				 (delabel "CBdiv:" :hide? t)
				 (simplify-monoms__ "CB:")
				 (replaces "NDC:" :but "NDC:"
					   :dir RL :hide? nil)
				 (delabel "NDC:" :hide? t)
				 (real-props "CB:" :theories theories)))))))))
	    (delabel "CB:")
	    (assert))))))
       (if (not formula)
	   (skip-msg msg1)
	   (if (not rel)
	       (skip-msg msg2)
	       (skip-msg msg3)))))
  "[Field Package] Cancels the common expression EXPR in the relational formula FNUM. Autorewrites with \"real_props\" and THEORIES when possible."
  "Canceling in formula ~A with ~A"
)

(defstrat my-guess__ (label name1 name2 theories)
  (then
   (delabel label :hide? t)
   (grind-reals :theories theories)
   (try (then (replaces name1 :dir RL :hide? nil)
	      (real-props :theories theories))
	(fail)
	(skip))
   (replaces name1 :but name1 :dir RL :hide? nil)
   (delabel name1 :hide? t)
   (replaces name2 :but name2 :dir RL :hide? nil)
   (delabel name2 :hide? t)
   (grind-reals :theories theories))
  "[Field Package: Internal strategy]"
)

(defhelper field-guess__ (theories)
  (my-guess__ "FD:" "X:" "NDF:" theories)
  "[Field Package: Internal strategy]"
  ""
)

(defhelper cancel-by-guess__  (theories)
  (my-guess__ "CB:" "CBdiv:" "NDC:" theories)
  "[Field Package: Internal strategy]"
  ""
)

(defstrat else-field__ (theories branch-step main-step)
  (branch branch-step
	  (main-step
	   (field-guess__ theories)))
  "[Field Package: Internal strategy]"
)

(defstrat else-cancel-by__ (theories branch-step main-step)
  (branch branch-step
	  (main-step
	   (cancel-by-guess__ theories)))
  "[Field Package: Internal strategy]"
)

(defstep cancel-formula (fnum &optional (theories "extra_tegies"))
  (let ((num     (extra-get-fnum fnum))
	(formula (extra-get-formula fnum))
	(msg1    (no-formula fnum))
	(rel     (when formula (is-relation formula)))
	(msg2    (no-relation fnum)))
       (if rel
	   (try (factor num)
		(let ((form (extra-get-formula fnum))
		      (l1   (get-mults-monom nil (args1 form)))
		      (l2   (get-mults-monom nil (args2 form)))
		      (l    (inter-polynom l1 l2 nil)))
		     (if l
			 (let ((cb   (normal-mult l)))
			      (cancel-by fnum cb :theories theories))
			 (skip)))
		(skip))
	   (if (not formula)
	       (skip-msg msg1)
	       (skip-msg msg2))))
  "[Field Package] Factorizes common terms in FNUM and then cancels them. Autorewrites with \"real_props\" and THEORIES when possible."
  "~%Canceling formula ~A"
)

(defstep field (&optional (fnum 1) (theories "extra_tegies") cancel?)
  (let ((formula (extra-get-formula fnum))
	(msg1    (no-formula fnum))
	(rel     (when formula (is-relation formula)))
	(msg2    (no-relation fnum)))
    (if rel
	(let ((flag (and (stringp fnum) (string= fnum "FD:"))))
	  (then@
	   (when-not flag
		     (relabel "FD:" fnum)
		     (else-field__ theories
				   (name-distrib "FD:" :prefix "NDF"
						 :label "NDF:")
				   (skip)))
	   (simplify-monoms__ "FD:")
	   (let ((form     (extra-get-formula "FD:"))
		 (is_eq    (equation? form))
		 (divs     (get-divisors-formula form))
		 (ndivs    (get-const-divisors 1 divs))
		 (edivs    (remove-if #'(lambda (x) (str2int (car x))) divs)))
	     (if divs
		 (let ((names    (newnames "X" (length edivs)))
		       (nameseq  (merge-names-exprs names
						    (mapcar #'(lambda(x) (car x))
						      edivs)))
		       (caseneq0 (makecase-neq0 names))
		       (eprod    (makeprod edivs names))
		       (prod     (normal-mult (if (= ndivs 1) eprod
						  (cons (cons (expr2str ndivs) 1)
							eprod))))
		       (prodgt0  (format nil "~A > 0" prod)))
		   (else-field__
		    theories
		    (name-replace-label* nameseq :label "X:")
		    (else-field__
		     theories
		     (if caseneq0 (case caseneq0) (skip))
		     (if is_eq
			 (else-field__ theories
				       (mult-by__ "FD:" prod)
				       (field$ "FD:" theories cancel?))
			 (branch (case prodgt0)
				 ((then
				   (try (field-guess__ theories)(fail)(skip))
				   (else-field__ theories
						 (mult-by__ "FD:" prod +)
						 (field$ "FD:" theories cancel?)))
				  (then
				   (try (field-guess__ theories)(fail)(skip))
				   (else-field__ theories
						 (mult-by__ "FD:" prod -)
						 (field$ "FD:" theories cancel?)))))))))
		 (try (replaces "X:" :but "X:" :dir RL :hide? nil)
		      (then (delabel "X:" :hide? t)
			    (field$ "FD:" theories cancel?))
		      (try (replaces "NDF:" :but "NDF:" :dir RL :hide? nil)
			   (then (delabel "NDF:" :hide? t)
				 (field$ "FD:" theories cancel?))
			   (then (try (assert)(fail)(skip))
				 (when cancel?
				   (assert "FD:")
				   (cancel-formula "FD:"))
				 (real-props "FD:" :distrib? nil)
				 (delabel "FD:"))))))))
	(if (not formula)
	    (skip-msg msg1)
	    (skip-msgmsg2))))
  "[Field Package] Applies a simplification procedure for the field of real numbers on the formula FNUM. FNUM must be a relational formula. Autorewrites with \"real_props\" and THEORIES when possible. If CANCEL? is t then it tries to cancel common terms once the expression is free of quotients."
  "~%Simplifying formula ~A with FIELD"
  )
