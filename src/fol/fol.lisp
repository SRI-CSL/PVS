(in-package :dp)

#-runtime

;; Labels L ::= (1 . L) | (2 . L) | (:f . L) | (:o . L) | nil

(defun label? (l)
  (and (listp l)
       (every #'(lambda (x)
		  (or (member x (list 1 2 :f :o) :test #'eql)))
	      l)))

(defvar *start-label* (list 1))

(defun frozen? (l) (eq (car l) :f))
(defun old?    (l) (eq (car l) :o))

(defun once   (l) (if (frozen? l) l (cons 1 l)))
(defun twice  (l) (if (frozen? l) l (cons 2 l)))
(defun freeze (l) (if (frozen? l) l (cons :f l)))
(defun old    (l) (cons :o l))

(defun label* (op fmlas)
  (assert (every #'fmla-p fmlas))
  (mapcar #'(lambda (fmla)
	      (mk-fmla (funcall op (label-of fmla))
		       (fmla-of fmla)))
	  fmlas))

(defun   old* (fmlas) (label* #'old fmlas))
(defun  once* (fmlas) (label* #'once fmlas))
(defun twice* (fmlas) (label* #'twice fmlas))

;; Formulas are represented by a label, a variable list, and the expression

(defstruct (fmla
	    (:print-function
		  (lambda (l s k)
		    (declare (ignore k))
		    (format s "~A[~{~a~^,~}]"
		      (fmla-fmla l) (fmla-label l)))))
  fmla
  label)

(defun label-of (fmla) (fmla-label fmla))
(defun  fmla-of (fmla) (fmla-fmla fmla))

(defun labels= (l1 l2)
  (equal l1 l2))

(defun in-label? (l ls)
  (member l ls :test #'labels=))

(defun fmla= (fmla1 fmla2)
  (and (eq (fmla-of fmla1) (fmla-of fmla2))
       (labels= (label-of fmla1) (label-of fmla2))))

(defun mk-fmla (tag fmla)
  (assert (label? tag))
  (assert (node-p fmla))
  (make-fmla :fmla fmla
	     :label tag))

;; Proof Search

(defun fol-search (fmlas state iterations &optional (k #'identity))
  (let ((*state* state)
	(*iterations* iterations))
    (declare (special *state*)
	     (special *iterations*))
    (unwind-protect
	(progn (frozen-vars-init)
	       (let ((lfmlas (mapcar #'(lambda (fmla)
					 (mk-fmla *start-label* fmla))
			       fmlas)))
		 (multiple-value-bind (ha hn ca cn)
		     (flatten () () () () () lfmlas)
		   (let ((unifier (prove1 ha ca
					  ha hn ()
					  ca cn ()
					  () (initial-unifier) k)))
		     (if (fail? unifier) :fail
			 (unifier-subst unifier))))))
      (frozen-vars-init))))
      
(defun prove1 (ha ca
	       ha1 hn1 h1
	       ca1 cn1 c1
	       labels unifier k)
  (try-to-unify1 ha ca1 labels unifier
		 #'(lambda (new-unifier)
		    (if (fail? new-unifier)
			(try-to-unify1 ca ha1 labels unifier
				       #'(lambda (new-unifier)
					   (if (fail? new-unifier)
					       (split ha1 hn1 h1
						      ca1 cn1 c1
						      labels unifier k)
					       (funcall k new-unifier))))
			(funcall k new-unifier)))))

;; Process two sequents in sequence
    
(defun prove2 (ha1 hs1 ca1 cs1
		   ha2 hs2 ca2 cs2
		   ha hs ca cs
		   label labels unifier k)
  (prove1 ha1 ca1
	  (union ha1 ha :test #'fmla=) hs1 (once* hs)
	  (union ca1 ca :test #'fmla=) cs1 (once* cs)
	  labels unifier #'(lambda (new-unifier)
			     (cond ((fail? new-unifier)
				    (funcall k *fail*))
				   ((not (in-label? label (unifier-labels new-unifier)))
				    (funcall k new-unifier))
				   (t
				    (prove1 ha2 ca2
					    (union ha2 ha :test #'fmla=) hs2 (twice* hs)
					    (union ca2 ca :test #'fmla=) cs2 (twice* cs)
					    labels
					    (filter-frozen new-unifier)
					    k))))))

(defun filter-frozen (unifier)
  "only substs for frozen vars need to be propagated"
  (with-slots (subst labels) unifier
    (let ((new-subst (remove-if #'(lambda (pair)
				    (not (frozen-variable? (car pair))))
		       subst)))
      (make-unifier :subst new-subst
		    :labels labels))))

;; Splitting a sequent

(defun split (ha ho hn ca co cn labels unifier k)
  (format t "~%Split")
  (split* ha ho hn ca co cn labels unifier k))

(defun split* (ha ho hn ca co cn labels unifier k)
  (declare (special *iterations*))
  (cond ((and (atom cn) (atom hn))
	 (if (or (consp ho) (consp co))
	     (if (> *iterations* 0)
		 (progn
		   (format t "Looping ~a..." *iterations*)
		   (setf *iterations* (1- *iterations*))
		   (split* ha ho (old* ho)
			  ca co (old* co)
			  labels unifier k))
		 (progn
		   (format t "~%Running out of steam...")
		   *fail*))
	    *fail*))
	((consp cn)
	 (split+ ha ho hn ca co cn labels unifier k))
	((consp hn)
	 (split- ha ho hn ca co cn labels unifier k))))
	 
(defun split+ (ha ho hn ca co cn labels unifier k)
  (let* ((c1 (car cn))
	 (l  (label-of c1))
	 (fl (freeze l))
	 (e  (fmla-of c1))
	 (cn (cdr cn)))
    (format t "~%Cfmla: ~a" c1)
    (if (conjunction-p e)
	(multiple-value-bind (ha1 hs1 ca1 cs1)
	    (flatten () hn ()
		     () cn (list (mk-fmla fl (lhs e))))
	  (multiple-value-bind (ha2 hs2 ca2 cs2)
	      (flatten () hn ()
		       () cn (list (mk-fmla fl (rhs e))))
	    (let ((new-co (if (frozen? l) co
			      (cons (mk-fmla (old l) e) co))))
	      (prove2 ha1 hs1 ca1 cs1
		      ha2 hs2 ca2 cs2
		      ha ho ca new-co
		      fl
		      (adjoin fl labels :test #'labels=)
		      unifier
		      k))))
    (funcall k *fail*))))

(defun split- (ha ho hn ca co cn labels unifier k)
  (declare (ignore cn))
  (let* ((h1 (car hn))
	 (l  (label-of h1))
	 (e  (fmla-of h1))
	 (fl (freeze l))
	 (hn (cdr hn))
	 (new-labels (adjoin fl labels :test #'labels=)))
    (format t "~%Hfmla: ~a" h1)
    (cond ((disjunction-p e)
	   (multiple-value-bind (ha1 hs1 ca1 cs1) 
	       (flatten () hn (list (mk-fmla fl (rhs e)))
			() () ())
	     (multiple-value-bind (ha2 hs2 ca2 cs2)
		 (flatten () hn (list (mk-fmla fl (lhs e)))
			  () () ())
	       (let ((new-ho (if (frozen? l) ho
				 (cons (mk-fmla (old l) e) ho))))
		 (prove2 ha1 hs1 ca1 cs1
			 ha2 hs2 ca2 cs2
			 ha new-ho ca co
			 fl
			 new-labels
			 unifier
			 k)))))
	  ((implication-p e)
	   (multiple-value-bind (ha1 hs1 ca1 cs1) 
	       (flatten () hn (list (mk-fmla fl (rhs e)))
			() () ())
	     (multiple-value-bind (ha2 hs2 ca2 cs2)
		 (flatten () hn ()
			  () () (list (mk-fmla fl (lhs e))))
	       (let ((new-ho (if (frozen? l) ho
				 (cons (mk-fmla (old l) e) ho))))
		 (prove2 ha1 hs1 ca1 cs1
			 ha2 hs2 ca2 cs2
			 ha new-ho ca co
			 fl
			 new-labels
			 unifier
			 k)))))
	  (t
	   (funcall k *fail*)))))

;; Search for Unifiers

(defun try-to-unify1 (hyps concs labels unifier k)
  (if (null concs) (funcall k *fail*)
    (try-to-unify2 hyps (car concs) labels unifier
		   #'(lambda (new-unifier)
		       (if (fail? new-unifier)
			   (try-to-unify1 hyps (cdr concs) labels unifier k)
			 (let ((new-new-unifier (funcall k new-unifier)))
			   (if (fail? new-new-unifier)
			       (try-to-unify1 hyps (cdr concs) labels unifier k)
			     new-new-unifier)))))))
	
(defun try-to-unify2 (hyps conc labels unifier k)
  (if (null hyps) (funcall k *fail*)
    (try-unify (car hyps) conc labels unifier
	       #'(lambda (new-unifier)
		   (if (fail? new-unifier)
		       (try-to-unify2 (cdr hyps) conc labels unifier k)
		     (let ((new-new-unifier (funcall k new-unifier)))
		       (if (fail? new-new-unifier)
			   (try-to-unify2 (cdr hyps) conc labels unifier k)
			 new-new-unifier)))))))

(defun try-unify (hyp conc labels unifier k)
  #+dbg(assert (fmla-p hyp))
  #+dbg(assert (fmla-p conc))
  #+dbg(assert (unifier-p unifier))
  (let ((new-unifier (lunify hyp
			     conc
			     labels
			     unifier)))
    (funcall k (if (fail? new-unifier) *fail* new-unifier))))

;; Labelled Unifiers

(defstruct (unifier
	    (:print-function
		  (lambda (l s k)
		    (declare (ignore k))
		    (format s "<~a, ~a>"
		      (unifier-subst l)
		      (unifier-labels l)))))
  subst
  labels)

(defun mk-unifier (labels subst)
  (make-unifier :labels labels
		:subst subst))

(defun subst-of (uni)
  (unifier-subst uni))

(defun labels-of (uni)
  (unifier-labels uni))

(let ((*initial-unifier* nil))
  (defun initial-unifier ()
    (or *initial-unifier*
	(make-unifier :subst nil :labels nil))))

(defun lunify (fmla1 fmla2 labels unifier)
  (declare (special *state*))
  #+dbg(assert (fmla-p fmla1))
  #+dbg(assert (fmla-p fmla2))
  #+dbg(assert (unifier-p unifier))
  (let ((res (E-unify (unlabel fmla1)
		      (unlabel fmla2)
		      *state*
		      (subst-of unifier))))
    (if (fail? res) :fail
	(let ((new-labels (union (list (label-of fmla1) (label-of fmla2)) labels
				 :test #'labels=)))
	  (make-unifier :subst res
			:labels new-labels)))))

(defvar *frozen-vars* nil)

(defun frozen-variable? (x)
  (gethash x *frozen-vars*))

(defsetf frozen (x) (entry)
  `(setf (gethash ,x ,*frozen-vars*) ,entry))

(defun frozen-vars-init ()
  (if *frozen-vars* (clrhash *frozen-vars*)
      (setf *frozen-vars*
	    (dp-make-hash-table :test 'equal-array
				:hash-function 'dp-sxhash))))

(defun unlabel (trm)
  (assert (fmla-p trm))
  (let ((*label* (label-of trm)))
    (declare (special *label*))
    (unlabel* (fmla-of trm))))

(defun unlabel* (trm)
  (declare (special *label*))
  (cond ((dp-variable-p trm)
	 (let* ((id (pvs::makesym "~a~a" (constant-id trm) *label*))
		(x (mk-variable id)))
	   (when (frozen? *label*)
	     (setf (gethash x *frozen-vars*) 'T))
	     ; (setf (frozen x) 'T))
	   x))
	((application-p trm)
	 (mk-term (mapcar #'unlabel*
		    (application-arguments trm))))
	(t trm)))

;; Propositional flattening, relabeling, and categorization

(defun flatten (ha hs hn ca cs cn)
  #+dbg(assert (every #'fmla-p (append ha hs hn ca cs cn)))
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
	 (rst (cdr cn)))
    (assert (node-p c))
    (cond ((negation-p c)
	   (flatten* ha hs (cons (mk-fmla l (arg 1 c)) hn)
		     ca cs rst))
	  ((disjunction-p c)
	   (flatten* ha hs hn
		     ca cs (cons (mk-fmla (once l) (lhs c))
				 (cons (mk-fmla (twice l) (rhs c))
				       rst))))
	  ((implication-p c)
	   (flatten* ha hs (cons (mk-fmla (once l) (lhs c))  hn)
		     ca cs (cons (mk-fmla (twice l) (rhs c)) rst)))
	  ((conjunction-p c)
	   (flatten* ha hs hn
		     ca (cons c1 cs) rst))
	  (t
	   (flatten* ha hs hn
		     (cons c1 ca) cs rst)))))

(defun flatten- (ha hs hn ca cs cn)
  (assert (consp hn))
  (let* ((h1 (car hn))
	 (h (fmla-of h1))
	 (l (label-of h1))
	 (rst (cdr hn)))
    (cond ((negation-p h)
	   (flatten* ha hs rst
		     ca cs (cons (mk-fmla l (arg 1 h)) cn)))
	  ((conjunction-p h)
	   (flatten* ha hs (cons (mk-fmla (once l) (lhs h))
				 (cons (mk-fmla (twice l) (rhs h))
				      rst))
		    ca cs cn))
	  ((or (disjunction-p h)
	       (implication-p h))
	   (flatten* ha (cons h1 hs) rst
		     ca cs cn))
	  (t
	   (flatten* (cons h1 ha) hs rst
		     ca cs cn)))))
