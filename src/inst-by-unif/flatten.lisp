(in-package :dp)

;; Propositional flattening, relabeling, and categorization

(defun flatten (ha hs hn ca cs cn)
  (assert (every #'node-p (append ha hs hn ca cs cn)))
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
  (let* ((c (car cn))
	 (cn (cdr cn)))
    (cond ((negation-p c)
	   (flatten* ha hs (cons (arg 1 c) hn)
		     ca cs cn))
	  ((disjunction-p c)
	   (flatten* ha hs hn
		     ca cs (cons (lhs c)
				 (cons (rhs c) cn))))
	  ((implication-p c)
	   (flatten* ha hs (cons (lhs c) hn)
		     ca cs (cons (rhs c) cn)))
	  ((conjunction-p c)
	   (flatten* ha hs hn
		     ca (cons c cs) cn))
	  (t
	   (flatten* ha hs hn
		     (cons c ca) cs cn)))))

(defun flatten- (ha hs hn ca cs cn)
  (assert (consp hn))
  (let* ((h (car hn))
	 (hn (cdr hn)))
    (cond ((negation-p h)
	   (flatten* ha hs hn
		     ca cs (cons (arg 1 h) cn)))
	  ((conjunction-p h)
	   (flatten* ha hs (cons (lhs h)
				 (cons (rhs h) hn))
		    ca cs cn))
	  ((or (disjunction-p h)
	       (implication-p h))
	   (flatten* ha (cons h hs) hn
		     ca cs cn))
	  (t
	   (flatten* (cons h ha) hs hn
		     ca cs cn)))))


