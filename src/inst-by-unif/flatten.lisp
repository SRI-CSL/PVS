(in-package :dp)

;; Propositional flattening, relabeling, and categorization

(defun flatten (ha hs hn ca cs cn)
  (format t "~%Flatten")
  (flatten* ha hs hn ca cs cn))

(defun flatten* (ha hs hn ca cs cn)
  (cond ((consp cn)
	 (flatten+ ha hs hn ca cs cn))
	((consp hn)
	 (flatten- ha hs hn ca cs cn))
	(t
	 (values ha hs ca cs))))

(defun flatten+ (ha hs hn ca cs cn)
  (assert (consp cn))
  (let* ((c1 (car cn))
	 (c  (fmla-of c1))
	 (l  (label-of c1))
	 (cn (cdr cn)))
    (cond ((negation-p c)
	   (flatten* ha hs (cons (mk-fmla l (arg 1 c)) hn)
		     ca cs cn))
	  ((disjunction-p c)
	   (flatten* ha hs hn
		     ca cs (cons (mk-fmla (once l) (lhs c))
				 (cons (mk-fmla (twice l) (rhs c))
				       cn))))
	  ((implication-p c)
	   (flatten* ha hs (cons (mk-fmla (once l) (lhs c)) hn)
		     ca cs (cons (mk-fmla (twice l) (rhs c)) cn)))
	  ((conjunction-p c)
	   (flatten* ha hs hn
		     ca (cons c1 cs) cn))
	  (t
	   (flatten* ha hs hn
		     (cons c1 ca) cs cn)))))

(defun flatten- (ha hs hn ca cs cn)
  (assert (consp hn))
  (let* ((h1 (car hn))
	 (h (fmla-of h1))
	 (l (label-of h1))
	 (hn (cdr hn)))
    (cond ((negation-p h)
	   (flatten* ha hs hn
		     ca cs (cons (mk-fmla l (arg 1 h)) cn)))
	  ((conjunction-p h)
	   (flatten* ha hs (cons (mk-fmla (once l) (lhs h))
				 (cons (mk-fmla (twice l) (rhs h))
				      hn))
		    ca cs cn))
	  ((or (disjunction-p h)
	       (implication-p h))
	   (flatten* ha (cons h1 hs) hn
		     ca cs cn))
	  (t
	   (flatten* (cons h1 ha) hs hn
		     ca cs cn)))))

