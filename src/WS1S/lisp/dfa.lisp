(in-package :pvs)

;; DFAs are packaged into a structure in order to finalize objects of this type

(defstruct (dfa-ptr
	    (:predicate dfa-ptr?)
	    (:constructor make-dfa-ptr (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "~a" (dfa-ptr-address p)))))
	    ;   (format s "#<S: ~a, N: ~a, T: ~a>"
		;       (number-of-states p)
		;       (number-of-bdd-nodes p)
		;       (number-of-transitions p)))))
  address)

(defun dfa-ptr= (p1 p2)
  (= (dfa-ptr-address p1) (dfa-ptr-address p2)))

(defun address (p)
  (dfa-ptr-address p))

(defun mk-dfa-ptr (a)
  (let ((p (make-dfa-ptr a)))
    (dfa-ptr-finalize! p)
    p))

(defun dfa-ptr-free! (p)
  (dfa-free! (dfa-ptr-address p)))

(defun dfa-ptr-finalize! (p)
    (excl:schedule-finalization p #'dfa-ptr-free!))


;; Constants

(let ((dfa-true-val nil))
  (defun dfa-true-val ()
    (or dfa-true-val
	(setq dfa-true-val (mk-dfa-ptr (dfa-true))))))

(let ((dfa-false-val nil))
  (defun dfa-false-val ()
    (or dfa-false-val
	(setq dfa-false-val (mk-dfa-ptr (dfa-false))))))

(defun dfa-true? (p)
  (dfa-ptr= p (dfa-true-val)))

(defun dfa-false? (p)
  (dfa-ptr= p (dfa-false-val)))

;; Variable

(defun dfa-var0 (i)
  (mk-dfa-ptr (dfa-boolvar i)))

(defun dfa-var1 (i)                     ; X(0)*1(0)*
  (mk-dfa-ptr (dfa-singleton i)))

;; DFA operations

(defun dfa-negation (p)
   (let ((a (dfa-copy (address p))))
     (dfa-negation! a)
     (let ((q (mk-dfa-ptr (dfa-minimize a))))
       (dfa-free! a)
       q)))

(defun dfa-cross-product (p1 p2 mode)
   (let* ((a (dfa-product (address p1) (address p2) mode))
	  (p (mk-dfa-ptr (dfa-minimize a))))
     (dfa-free! a)
     p))

(defun dfa-unrestrict (p)
  (let ((a (dfa-copy (address p))))
    (dfa-unrestrict! a)
    (mk-dfa-ptr a)))
    
(defun dfa-exists (i p)
  (let ((a (dfa-copy (address p))))
    (dfa-right-quotient! a i)
    (let* ((b (dfa-project a i)))
      (dfa-free! a)
      (mk-dfa-ptr (dfa-minimize b)))))

(defun dfa-op (op &rest l)
  (mk-dfa-ptr (apply op l)))

;; Derived Automaton Constructions
 
(defun dfa-conjunction (p1 p2) (dfa-cross-product p1 p2 *ANDmode*))
(defun dfa-disjunction (p1 p2) (dfa-cross-product p1 p2 *ORmode*))
(defun dfa-implication (p1 p2) (dfa-cross-product p1 p2 *IMPLmode*))

(defun dfa-equivalence (p1 p2)
  (if (dfa-ptr= p1 p2) (dfa-true-val)
    (dfa-cross-product p1 p2 *BIMPLmode*)))

(defun dfa-ite (c p1 p2)
  (dfa-disjunction (dfa-conjunction c p1)
		   (dfa-conjunction (dfa-negation c) p2)))

(defun dfa-conjunction* (as)
  (labels ((loop* (l acc)
	     (if (null l) acc
	       (let ((a (car l)))
		 (if (dfa-false? a)
		     (return-from dfa-conjunction* (dfa-false-val))
		   (loop* (cdr l) (dfa-conjunction acc a)))))))
    (loop* as (dfa-true-val))))

(defun dfa-disjunction* (as)
  (labels ((loop* (l acc)
	     (if (null l) acc
	       (let ((a (car l)))
		 (if (dfa-true? a)
		     (return-from dfa-disjunction* (dfa-true-val))
		   (loop* (cdr l) (dfa-disjunction acc a)))))))
    (loop* as (dfa-false-val))))

(defun dfa-exists2 (i p) (dfa-exists i p))
(defun dfa-exists1 (i p) (dfa-exists i (dfa-conjunction (dfa-var1 i) p)))
(defun dfa-exists0 (i p) (dfa-exists i (dfa-conjunction (dfa-var0 i) p)))

(defun dfa-forall2 (i p) (dfa-negation (dfa-exists i (dfa-negation p))))
(defun dfa-forall1 (i p) (dfa-negation (dfa-exists i (dfa-conjunction
						      (dfa-var1 i)
						      (dfa-negation p)))))
(defun dfa-forall0 (i p) (dfa-negation (dfa-exists i (dfa-conjunction
						      (dfa-var0 i)
						      (dfa-negation p)))))

(defun dfa-exists* (levels is a)
  (cond ((and (null levels) (null is))
	 a)
	((and (consp levels) (consp is))
	 (dfa-exists* (cdr levels)
		      (cdr is)
		      (let ((level (car levels)))
			(cond ((= level 2)
			       (dfa-exists2 (car is) a))
			      ((= level 1)
			       (dfa-exists1 (car is) a))
			      ((= level 0)
			       (dfa-exists0 (car is) a))
			      (t (error "Fatal Error: Unknown Level ~a" level))))))
	(t
	 (error "Fatal Error: List arguments not of equal length"))))

(defun dfa-forall* (levels is a)
  (dfa-negation (dfa-exists* levels is (dfa-negation a))))

(defun dfa-exists2* (is a)
  (if (null is) a
    (dfa-exists2 (car is) (dfa-exists2* (cdr is) a))))

(defun dfa-exists1* (is a)
  (if (null is) a
    (dfa-exists1 (car is) (dfa-exists1* (cdr is) a))))


;; Accessors, Recognizers

(defun number-of-states      (a) (dfa-ns (address a)))
(defun start-state           (a) (dfa-s (address a)))
(defun number-of-bdd-nodes   (a) (bdd-size (dfa-bddm (address a))))
(defun number-of-transitions (a) (transition-table-size (address a)))


;; Witnesses and Counterexamples

(defun dontcare? (ch) (eq ch #\X))
(defun set?      (ch) (eq ch #\1))
(defun unset?    (ch) (eq ch #\0))

(defun make-example (p num indices &key kind)
  (let ((char* (dfa-make-example (address p) kind num indices)))
    (if (= 0 char*) :null
      (let* ((str (ff:char*-to-string char*))
	     (len (/ (ff:char*-string-length char*) (1+ num))))   ; to do: free string
	#+dbg(assert (integerp len))
	(values str len)))))
		     
(defun dfa-witness (p num indices)
  (make-example p num indices :kind 1))
  
(defun dfa-counterexample (p num indices)
  (make-example p num indices :kind -1))

(defun dfa-full? (p)
  (= (dfaFull (address p)) 0))
  
(defun dfa-empty? (p)
  (= (dfaEmpty (address p)) 0))

(defun dfa-equiv (p1 p2)
  (dfa-empty? (dfa-conjunction p1 (dfa-negation p2))))
