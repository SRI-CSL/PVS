;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diploma Thesis:
;;; "Solving Bit-Vector Equations 
;;;  - A Decision Procedure for Hardware Verifikation"
;;;
;;; M. Oliver M"oller
;;; University of Ulm
;;; Faculty of Computer Science (Informatik)
;;; AI Department (Abteilung f"ur k"unstliche Intelligenz)
;;; Supervising Professor:  Friedrich W. von Henke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ------------------------------------------------------------
;;; -              SOLVER FOR BITVECTOR-THEORY                 -
;;; -              ~~~~~~~~~~~~~~~~~~~~~~~~~~~                 -
;;; - WITH  +variable size                                     -
;;; -       +variable extraction                               -
;;; -       +composition                                       -
;;; -       +boolean operations                                -
;;; -       +(linar) arithmetic
;;; ------------------------------------------------------------
;;; -      Oliver M"oller (moeller@ki.informatik.uni-ulm.de)   -
;;; ------------------------------------------------------------
;;; - File:     bvec_bdd_solve.cl                              -
;;; -           [thanks to David Cyrluk]                       -
;;; - Purposes: solving equations over BDDS                    -
;;; -           composing BDD-nodes                            -
;;; -           apply boolean functions on nodes               -
;;; - USES:     bvec_structure.cl                              -
;;; ------------------------------------------------------------
;;;
;;; Allegro Common Lisp
;;; BEGUN:         December, 15th 1997
;;; Last Changes:  March, 16th    1998
;;; Version 2.0

(in-package 'bvec)


;; ******************************
;; ** MACRO DEFINITION
;; ** (Memoization)
;; ******************************

(defun real-args (args)
  (cond
   ((null args) nil)
   ((eq (car args) '&optional)
    (real-optional-rest-args (cdr args)))
   ((eq (car args) '&rest)
    (cdr args))
   (t (cons (car args) (real-args (cdr args))))))

(defun real-optional-rest-args (args)
  (cond
   ((null args) nil)
   ((listp (car args))
    (cons (car (car args)) (real-optional-rest-args (cdr args))))
   ((eq (car args) '&rest)
    (cdr args))
   (t (cons (car args) (real-optional-rest-args (cdr args))))))

(defmacro defun-hashed (fun args &rest body)
  `(defun ,fun ,args
     (if *hash*
	 (multiple-value-bind (hash-res found?)
	     (gethash (cons ',fun (list ,@(real-args args))) *bdd-hash*)
	   (if found?
	       (progn (incf *num-hits*) hash-res)
	       (let ((res (progn ,@body)))
		 (incf *num-mis*)
		 (setf (gethash (cons ',fun (list ,@(real-args args)))
				*bdd-hash*)
		       res)
		 res)))
	 (progn ,@body))))

;; --------------------
;; GLOBALS FOR MACRO
;; --------------------

(defvar *unique-table* (make-hash-table :test #'equal :size 100000))
(defvar *hash* t)
(defvar *bdd-hash* (make-hash-table :test #'equal :size 100000))
(defvar *num-mis* 0)
(defvar *num-hits* 0)

;; --------------------
;; GLOBAL DEFINITIONS
;; --------------------

(defconstant *bvec-var-prefix* "$FRESH") 

(defvar *bvec-node-max-label* 0)  ;; Counter for fresh variables

(defvar *true-node*)   ;; will be set before any apply / compose
(defvar *false-node*)  ;; to the right matching value


;(defun-hashed bdd-compute
;  (non-bdd-args bdd-args args-function termination-function leaf-function)
;  (when args-function
;    (setq bdd-args (apply args-function (append non-bdd-args bdd-args))))
;  (cond
;   ((and
;     termination-function
;     (apply termination-function (append non-bdd-args bdd-args))))
;   ((every #'leaf-node? bdd-args)
;    (apply leaf-function (append non-bdd-args bdd-args)))
;   (t
;    (let* ((min-bdd (smallest-bdd bdd-args))
;	   (min-var (node-variable min-bdd)))
;      (let* ((else-bdds (mapcar #'(lambda (bdd) (else-wrt-index bdd min-var))
;			       bdd-args))
;	     (then-bdds (mapcar #'(lambda (bdd) (then-wrt-index bdd min-var))
	;		       bdd-args))
;	     (node-else (bdd-compute non-bdd-args  else-bdds
;						 args-function nil
;						 leaf-function))
;	     (node-then (bdd-compute non-bdd-args  then-bdds
;						 args-function nil
;						 leaf-function)))
;	(if (eq node-then node-else)
;	    node-then
;	  
;	  (make-unique-node  :variable min-var
;			     :else     node-else
;			     :then     node-then)))))))


;;; ------------------------------
;;; Downward recursion
;;; ------------------------------                               

(defun then-wrt-index (bdd index)
  ;;; assume (>= (node-variable bdd) index)
  (if (equal index (node-variable bdd))
      (node-then bdd)
    bdd))

(defun else-wrt-index (bdd index)
  ;;; assume (>= (node-variable bdd) index)
  (if (equal index (node-variable bdd))
      (node-else bdd)
    bdd))



;;; ---------------------------------------------
;;;  APPLICATION OF BOOLEAN FUNCTIONS ON BDDs
;;; ---------------------------------------------

(defun bdd-apply-n (op args len)
  (init-leaf-nodes len)
  (case op
	(BV-NOT   (eval `(bdd-not ,@args)))
	(BV-AND   (eval `(bdd-and ,@args)))
	(BV-OR    (eval `(bdd-or ,@args)))
	(BV-XOR   (eval `(bdd-xor ,@args)))
	(BV-EQUIV (eval `(bdd-equiv ,@args)))
	(BV-ITE   (eval `(bdd-ite ,@args)))
	(t (error-misc "bdd-apply-n"
		       `(,op ,@args ,len) "Unknown Operator-type"))))

;------------------------------------------------------------


(defun ite-termination (nif nthen nelse)
  (cond
   ((true-node? nif) nthen)
   ((false-node? nif) nelse)
   ((eq nthen nelse) nthen)
   (t nil)))

(defun ite-leaf (nif nthen nelse)
  (cond
   ((true-node? nif) nthen)
   ((false-node? nif) nelse)
   ((eq nthen nelse) nthen)))

(defun bdd-ite (nif nthen nelse)
  (bdd-compute nil                 (list nif nthen nelse)
	       nil
	       #'ite-termination   #'ite-leaf))

(defun bdd-not (n1)
  (bdd-ite n1 *false-node* *true-node*))

(defun bdd-or (n1 n2)
  (if (smaller-node n1 n2)
      (bdd-ite n1 *true-node* n2)
      (bdd-ite n2 *true-node* n1)))

(defun bdd-and (n1 n2)
  (if (smaller-node n1 n2)
      (bdd-ite n1 n2 *false-node*)
    (bdd-ite n2 n1 *false-node*)))

(defun bdd-xor (n1 n2)
  (if (smaller-node n1 n2)
      (bdd-ite n1 (bdd-not n2) n2)
      (bdd-ite n2 (bdd-not n1) n1)))

(defun bdd-equiv (n1 n2)
  (if (smaller-node n1 n2)
      (bdd-ite n1 n2 (bdd-not n2))
      (bdd-ite n2 n1 (bdd-not n1))))

;;; ------------------------------------------
;;; - COMPOSE
;;; ------------------------------------------

(defun bdd-compose (n1 n2 var)
  ;; n1,n2   : node
  ;; var     : <variable>|<flat-extraction>
  ;; FUNCTION: replace var in n1 with n2 [var vanishes]
  ;; output  : node

  (bdd-compute (list var)
	       (list n1 n1 n2)
	       #'compose-arg-function
	       nil
	       #'bdd-compose-leaf))



(defun compose-arg-function (i nelse1 nthen1 n2)
  (list (if (and (not (leaf-node? nelse1))
		 (equal (node-variable nelse1) i))
	    (node-else nelse1)
	    nelse1)
	(if (and (not (leaf-node? nthen1))
		 (equal (node-variable nthen1) i))
	    (node-then nthen1)
	    nthen1)
	n2))

(defun bdd-compose-leaf (i nelse1 nthen1 n2)
  (declare (ignore i))
  (cond
   ((false-node? n2)
    nelse1)
   ((true-node? n2)
    nthen1)
   ((eq nelse1 nthen1)
    nelse1)))


;;------------------------------------------

(defun bdd-solve-triangle (n &key (fresh-var-call nil)
				  (fail nil))				  
  ;; Takes the variables in n starting from the lowest variable
  ;; and generates a triangular set of equations:
  ;; xi = bddi
  ;; where xi do not appear in any of the bddj (j > i) and
  ;; the conjuction of the equations are equivalent to
  ;; the input bdd n.
  ;; The set of equations is returned as an association list
  ;; ((xi . bddi) ...)
  ;; *true* *false* may also be members of the list.
  (cond
   ((true-node? n)  nil)
   ((false-node? n) (ifuncall fail) nil)
   ;;o;(return-from bdd-solve n))
   (t
    (let* ((x (node-variable n))
	   (width (bv-length x))
	   (delta (make-fresh-bv-var width :FRESH-VAR-CALL fresh-var-call))
	   (delta-node (make-unique-node
			:variable delta
			:else     *false-node*
			:then     *true-node*)))
      (acons x (bdd-ite (node-then n)
			(bdd-ite (node-else n) delta-node *true-node*)
			*false-node*)
	     (bdd-solve-triangle (bdd-or (node-then n) (node-else n))
				 :FRESH-VAR-CALL fresh-var-call))))))


(defun bdd-solve (n &key (fresh-var-call nil)
			 (fail nil))
  (let ((vars (vars-of-node n))
 	(triangle-list (bdd-solve-triangle n
					   :FRESH-VAR-CALL fresh-var-call
					   :FAIL fail)))
    (cond
     ((member *false-node* triangle-list :TEST #'equalp)
      (ifuncall fail) nil) 
     ((null vars)
      (loop for eq in triangle-list collect
	    (cons (car eq) (lift-bdd-if (cdr eq)))))
     (t (loop for eq in
	      (triangle-reduce
	       ;; ordinary solving
	       (sort (loop for v in vars collect
			   (cond ((assoc v triangle-list :TEST #'equal))
				 (t `(,v . ,(make-unique-node
					     :variable (make-fresh-bv-var
							(bv-length v)
							:FRESH-VAR-CALL fresh-var-call)
					     :else *false-node*
					     :then *true-node*)))))
		     #'(lambda (x y) (bv-term->= (car x) (car y))))
	       :FAIL fail) collect
	      (cons (car eq) (lift-bdd-if (cdr eq))))))))
	       


(defun triangle-reduce (triangle-list &key (fail nil))
 (if (null triangle-list)
     nil
   (let ((pair (car triangle-list)))
     (cond
      ((consp pair)
       (let ((var (car pair))
	     (bdd (cdr pair)))
	 
	 (acons var bdd
		(triangle-reduce 
		 (back-substitute (cdr triangle-list) var bdd)
		 :FAIL fail))))
      ((true-node? pair)
       (triangle-reduce (cdr triangle-list) :FAIL fail))
      ((false-node? pair)
       (ifuncall fail)
       ;; (return-from bdd-solve pair)
       nil)
      ))))
	

(defun back-substitute (triangle-list var bdd)
  ;; 'replace' var in triangle-list with bdd
  (loop for pair in triangle-list
	collect
	(cond
	 ((consp pair)
	  (cons (car pair) (bdd-compose (cdr pair) bdd var)))
	 (t pair))))

;; ------------------------------
;; -- Hash-functions:
;; ------------------------------ 

(defun get-unique (var-index node-else node-then)
  (gethash (list var-index node-else node-then)
	   *unique-table*))

(defun insert-unique (var-index node-else node-then node)
  (setf (gethash (list var-index node-else node-then)
		 *unique-table*)
	node))

;;----------------------


(defun make-unique-node (&key variable else then)
  ;; atf is not used in here
  (let ((new-node (get-unique variable else then)))
    (if new-node
	new-node
	(progn
	  (setq new-node (make-node :variable  variable
				    :then then :else else
				    :leaf-node? (and (null then) (null else))
				    :label (incf *bvec-node-max-label*)
				    :mark nil))
	  (insert-unique variable else then new-node)
	  new-node))))


(defun leaf-node? (node)
  (node-leaf-node? node))

(defun true-node? (node)
  (and (node-leaf-node? node)
       (rec-bv-const? (node-variable node))
       (= (bv-const-value (node-variable node))
	  (1- (expt 2 (bv-const-length (node-variable node)))))))

(defun false-node? (node)
  (and (node-leaf-node? node)
       (rec-bv-const? (node-variable node))
       (eq (bv-const-value (node-variable node)) 0)))


;;; ------------------------------------------------------------
;;;  BDD-Node-Traversion
;;; ------------------------------------------------------------

(defun traverse (node f)
  (setf (node-mark node) (not (node-mark node)))
  (apply f (list node))
  (when (node-then node)
    (unless (eq (node-mark node) (node-mark (node-then node)))
      (traverse (node-then node) f))
    (unless (eq (node-mark node) (node-mark (node-else node)))
      (traverse (node-else node) f)))
  node)

(defun unmark (node)
  (traverse node #'(lambda (node) (declare (ignore node)))))

; ------------------------------------------------------------


(defun bdd-size (node)
  (let ((node-count 0))
    (flet ((count-node (node)
		       (declare (ignore node))
		       (incf node-count)))
      (traverse node #'count-node)
      (unmark node)
      node-count)))

(defun bdd-leaf-size (node)
  (let ((node-count 0))
    (flet ((count-node (node)
		       (when (leaf-node? node)
			 (incf node-count))))
      (traverse node #'count-node)
      (unmark node)
      node-count)))

(defun bdd-leafs (node)
  (let ((leafs nil))
    (flet ((count-node (node)
		       (when (leaf-node? node)
			 (setq leafs (cons node leafs)))))
      (traverse node #'count-node)
      (unmark node)
      leafs)))


(defun initialize (node)
  (let ((node-label 0))
    (flet ((set-node-label (node)
			   (incf node-label)
			   (setf (node-label node) node-label)))
      (flet ((init-node (node)
			(set-node-label node)
			;;? (assign-atf-index node)
			))
	(traverse node #'init-node)
	(unmark node)))
    (setq *bvec-node-max-label* node-label)
    node))


(defun init-true-node (n) "True-node of length n"
  (let* ((true-term (make-bv-const (1- (expt 2 n)) n))
	 (node (make-node :leaf-node? t
			 :variable true-term
			 :label (incf *bvec-node-max-label*)
			 :mark nil)))
    (insert-unique true-term nil nil node)))

(defun init-false-node (n) "False node of length n"
  (let* ((false-term (make-bv-const 0 n))
	 (node (make-node :leaf-node? t
 			 :variable false-term
			 :label (incf *bvec-node-max-label*)
			 :mark nil)))
    (insert-unique false-term nil nil node)))


(defun make-true-node (n) "True-node of length n"
  (let* ((true-term (make-bv-const (1- (expt 2 n)) n))
	 (node (gethash `(,true-term nil nil) *unique-table*)))
    (or node
	(init-true-node n))))

(defun make-false-node (n) "False-node of length n"
  (let* ((false-term (make-bv-const 0 n))
	 (node (gethash `(,false-term nil nil) *unique-table*)))
    (or	node
	(init-false-node n))))

(defun init-leaf-nodes (n) "Prepare Tru-node and False-node of length n"
       (setf *true-node* (make-true-node n))
       (setf *false-node* (make-false-node n)))
       
      
       
    

;--

(defun init-bvec-bdd ()
  (setf *bvec-node-max-label* 0)
  (clrhash *unique-table*)
  (clr-gdd-hash))

;;; --------------------------------------------------
;;;  AUXILIARY FUNCTIONS
;;; --------------------------------------------------

(defun vars-of-node (node)
  (let ((erg nil))
    (traverse node
	      #'(lambda (x)
		  (unless (node-leaf-node? x)
			  (pushnew (node-variable x)
			   erg
			   :TEST #'equal))))
    (unmark node)
    (sort erg #'bv-term-<)))
	    



;; ******************************
;; ** <TERM> COMPARISON
;; ******************************


(defun bv-term-< (t1 t2) "equal length!"
;; only of flat terms (NOT on PVS-EBDDs)
  (let ((l1 (bv-length t1))
	(l2 (bv-length t2))
	(v1 (bv-term-level t1))
	(v2 (bv-term-level t2)))
    (cond
     ((< l1 l2) t)
     ((> l1 l2) nil)
     ((< v1 v2) t)
     ((> v1 v2) nil)
     ((= v1 0) ;; constants
      (< (bv-const-value t1) (bv-const-value t2)))
     ((= v1 1) ;; blank variables
      (bv-var-< t1 t2))
     ((= v1 2) ;; extract
      (let ((var1 (bv-extraction-bv t1))
	    (var2 (bv-extraction-bv t2))
	    (left1 (bv-extraction-left t1))
	    (left2 (bv-extraction-left t2)))
	(cond
	 ((< l1 l2) t)
	 ((> l1 l2) nil)
	 ((> left1 left2) t)
	 ((< left1 left2) nil)
	 (t (bv-term-< var1 var2)))))
     ;; very bad order for addition: (equal (bv-extraction-bv t1) (bv-extraction-bv t2)) 
     ((= v1 3) ;; composition
      (cond
       ((bv-term-< (bv-decompose-left t1)
		   (bv-decompose-left t2)) t)
       ((bv-term-< (bv-decompose-left t2)
		   (bv-decompose-left t1)) nil)  
       (t (bv-term-< (bv-decompose-right t1)
		     (bv-decompose-right t2)))))
     ((= v1 4)
      (node-< t1 t2))
     (t
      (< (sxhash t1) (sxhash t2))))))

(defun bv-term->= (x y)
  (not (bv-term-< x y)))

(defun bv-term-level (t1)
  (cond
   ((rec-bv-const? t1) 0)
   ((rec-bv-var? t1)   1)
   ((rec-bv-extraction? t1) 2)
   ((rec-bv-composition? t1) 3)
   ((rec-bdd? t1) 4)
   (t (error-misc "term-level" t1 "not caught."))))

(defun bv-var-< (t1 t2)
  (cond
   ((< (bv-var-length t1) (bv-var-length t2)) t)
   ((> (bv-var-length t1) (bv-var-length t2)) nil)
   (t (string-lex-< (princ-to-string (bv-var-name t1))
		    (princ-to-string (bv-var-name t2))))))

(defun string-lex-< (s1 s2)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (cond
     ((< l1 l2) t)
     ((> l1 l2) nil)
     (t (string< s1 s2)))))

;;; NODES

(defun node-< (n1 n2)
  (cond
   ((null n1) t)
   ((null n2) nil); shouldn't be called with this arguments!!
   ((and (leaf-node? n2)
	 (leaf-node? n1))
    (and (false-node? n1)
	 (true-node?  n2)))
   ((leaf-node? n1) nil)
   ((leaf-node? n2) t)
   (t (bv-term-< (node-variable n1)
		 (node-variable n2)))))

(defun smaller-node (n1 n2)
  (if (node-< n2 n1)
      n2
      n1))

(defun smallest-bdd (bdds)
  (reduce #'(lambda (n1 n2) (smaller-node n1 n2)) bdds))


(defun lift-to-bdd (bv)
  ;; bv is an extraction, variable or constant (or already a bdd)
  (cond
   ((node-p bv) bv)
   ((or (rec-bv-var? bv)
	(rec-bv-extraction? bv))
    (build-bv-node bv))
   ((rec-bv-const? bv)
    (make-bv-composition-from-list
     (mapcar #'bv-const-to-node
	       (bv-flat-term-content (flatten-bv-const bv)))))
   ((rec-bv-composition? bv)
    (make-bv-composition-from-list
     (mapcar #'lift-to-bdd (bv-composition-content bv))))    
   (t (error-misc "lift-to-bdd" bv "not caught."))))
   
(defun bv-const-to-node (const)
  #+dbg(assert (rec-bv-const? const))
  (let ((len (bv-const-length const))
	(val (bv-const-value const)))
    (cond
     ((= val 0) (make-false-node len))
     ((= val (1- (expt 2 len))) (make-true-node len))
     (t (error-misc "bv-const-to-node" const "illegal constant value")))))



(defun build-bv-node (term)
  (let ((n (bv-length term)))
    (make-unique-node :variable term
		      :else (make-false-node n)
		      :then (make-true-node n))))


;;; -----------
;;;  CLEANUP
;;; -----------

(defun clr-gdd-hash ()
  (clrhash *bdd-hash*)
  (setq *num-mis* 0)
  (setq *num-hits* 0)
)
