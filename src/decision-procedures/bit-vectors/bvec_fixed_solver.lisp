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
;; ------------------------------------------------------------
;; -              SOLVER FOR BITVECTOR-THEORY                 
;; -              ~~~~~~~~~~~~~~~~~~~~~~~~~~~                 
;; - WITH  +fixed size                                        
;; -       +fixed extraction                                  
;; -       +composition                                       
;; -       +boolean operations                                
;; -       +artithmetic [via OBDDs]                           
;; -----------------------------------------------------------
;; -      Oliver M"oller (moeller@ki.informatik.uni-ulm.de)   
;; -----------------------------------------------------------
;; - File:     bvec_fixed_solver                              
;; - Purpose:  Provides canonizer   fixed-sigma               
;; -           and solver           fixed-bv-solve            
;; - Requires: bvec_structure.cl                              
;; -           bvec_bdd_solve.cl                              
;; -           bvec_arith.cl                                  
;; -           bvec_slicing.cl                                
;; -----------------------------------------------------------
;; - USAGE:                                                   
;; - Load and compile the required files in the listed order; 
;; - Call (fixed-bv-solve (BV-EQUAL <term1> <term2>))         
;; - [for term structure refer to bvec_structure.cl]          
;; - The function returns list, that contains                 
;; -  (a) the symbol 'TRUE  if the equation is a tautology    
;; -  (b) the symbol 'FALSE if the equation is unsatisfiable  
;; -  (c) else, a number of lists                             
;; -       (BV-EQUAL <original variable>  <bv-term>)            
;; -----------------------------------------------------------
;; NOTE: 
;;  The term notation is NOT identic with the one in the
;;   Diploma Thesis!
;;  Due to an (obsolete) design decision, the least significant
;;  bit is at the rightmost position and concatenations are the
;;  other way round as a consequence;
;;  eg. ( x{4} o y{4} ) ^ (1,0) = y{4} ^ (1,0)
;;
;; Allegro Common Lisp Version
;; BEGUN:         September, 1st 1997
;; Last Changes:  March, 14th    1998
;; Version 3.0    [as printed in Appendix C.2 of the Diploma Thesis]

;; ++++++++++++++++++++++++
;; + General Settings
;; ++++++++++++++++++++++++

(in-package bvec)

;;; observing

(eval-when (eval load compile)
  (defvar *obs-fs* nil)
  (defmacro obs-fs (arg)
    (if *obs-fs* arg)))

(defvar *heuristic-node-prior* 0)
(defvar *heuristic-node-post* 0)

;;; ** Loading **

(defun l1 () (load "./bvec_fixed_solver.lisp"))

(defun ll ()
  (load "./bvec_structure.lisp")
  (load "./bvec_bdd_solve.lisp")
  (load "./bvec_arith.lisp")
  (load "./bvec_slicing.lisp")
  (load "./bvec_fixed_solver.lisp")
  )

(defun cc ()
  (compile-file "./bvec_structure.lisp")
  (compile-file "./bvec_bdd_solve.lisp")
  (compile-file "./bvec_arith.lisp")
  (compile-file "./bvec_slicing.lisp")
  (compile-file "./bvec_fixed_solver.lisp")
  ;-
  (load "./bvec_structure.fasl")
  (load "./bvec_bdd_solve.fasl")
  (load "./bvec_arith.fasl")
  (load "./bvec_slicing.fasl")
  (load "./bvec_fixed_solver.fasl"))
  
  


;;;; -------------------------------------------------------------
;;;; -------------------------------------------------------------
;;;; THE CANONIZER ----------------------------------------------
;;;; -------------------------------------------------------------
;;;; -------------------------------------------------------------

(defun fixed-sigma (bv)
  ;; (ifassert (is-fixed-bv? bv))
  (fixed-beta (fixed-alpha bv)))

(defun fixed-alpha (bv)
  (cond
   ((or (rec-bv-var? bv)
	(rec-bv-const? bv))
    bv)
   ((rec-bv-composition? bv)
    (make-bv-composition
     (fixed-alpha (bv-decompose-left bv))
     (fixed-alpha (bv-decompose-right bv))))
   ((rec-bv-extraction? bv)
    (fixed-alpha-extraction bv))
   ((rec-bv-bool-apply? bv)
    (fixed-gamma bv))
   ((node-p bv)
    (lift-bdd-if bv))
   ((rec-bv-addition? bv)
    (fixed-delta bv))
   (t (error-misc "fixed-alpha" (list bv) "not caught."))))

(defun fixed-alpha-extraction (bv)
  (let* ((arg (fixed-alpha (bv-extraction-bv bv)))
	 (left (bv-extraction-left bv))
	 (right (bv-extraction-right bv))
	 (len (bv-length arg))
	 (times (1+ (- left right))))
    (cond
     ((eq len times) arg)
     ((rec-bv-extraction? arg)
      (fixed-alpha-extraction
       (make-bv-extraction
	(bv-extraction-bv arg)
	(+ left (bv-extraction-right arg))
	(+ right (bv-extraction-right arg)))))
     ((rec-bv-composition? arg)
      (let* ((bv-left (bv-decompose-left arg))
	     (bv-right (bv-decompose-right arg))
	     (lr (bv-length bv-right)))
	(cond
	 ((< left lr)
	  (fixed-alpha-extraction
	   (make-bv-extraction bv-right left right)))
	 ((>= right lr)
	  (fixed-alpha (make-bv-extraction bv-left (- left lr) (- right lr))))
	 (t (make-bv-composition
	     (fixed-alpha-extraction
	      (make-bv-extraction bv-left (- left lr) 0))
	     (fixed-alpha-extraction
	      (make-bv-extraction bv-right (- lr 1) right)))))))
     ((rec-bv-const? arg)
      (extract-bv-const (bv-const-value arg) left right))
     ((rec-bv-var? arg) (make-bv-extraction arg left right))
     ((rec-bv-bool-apply? arg)
      (fixed-gamma (make-bv-bool-apply (dp::funsym arg)
				       (mapcar #'(lambda (x) (fixed-alpha (make-bv-extraction x left right)))
					       (dp::funargs arg)))))

     ((node-p arg)
      (fixed-bdd-extraction arg left right))
     ;;--!-- tbc
     (t (error-misc "fixed-alpha-extraction" bv "not caught.")))))


(defun fixed-bdd-extraction (arg left right)
  ;; Extract in all nodes...
  (let ((content (fixed-alpha
		  (make-bv-extraction (node-variable arg) left right)))
	(n (1+ (- left right)))
	(then (node-then arg))
	(else (node-else arg)))
    (if (leaf-node? arg)
	(cond
	 ((true-node? arg) (make-true-node n))
	 ((false-node? arg) (make-false-node n))
	 (t (error-misc "fixed-bdd-extraction" arg "illegal leaf node.")))
      (make-unique-node
       :VARIABLE content
       :ELSE (fixed-bdd-extraction else left right)
       :THEN (fixed-bdd-extraction then left right)))))


(defun fixed-beta (bv)
  (if (rec-bv-composition? bv)
      (let* ((list (bv-composition-content bv))
	     (actual (car list))
	     (next (cadr list))
	     (rest (cddr list))
	     (res nil))
	(flet ((matches ()
		 (or (and (rec-bv-const? actual)
			  (rec-bv-const? next))
		     (and (rec-bv-extraction? actual)
			  (rec-bv-extraction? next)
			  (equal (bv-extraction-bv actual)
				 (bv-extraction-bv next))
			  (= (1+ (bv-extraction-left next))
			     (bv-extraction-right actual)))
		     (and (node-p actual)
			  (node-p next)
			  (nodes-match? actual next))))
	       (melt ()
		 (setf
		     actual (cond
			     ((rec-bv-const? actual)
			      (append-bv-const actual next))
			     ((rec-bv-extraction? actual)
			      (fixed-alpha (make-bv-extraction
					    (bv-extraction-bv actual)
					    (bv-extraction-left actual)
					    (bv-extraction-right next))))
			     ((node-p actual)
			      (attach-nodes actual next))
			     (t (error "[local]melt" actual "not caught.")))
		     next   (car rest)
		     rest   (cdr rest))))
	  (loop while next do
		(loop while (and next (matches)) do (melt))
		(push actual res)
		(setf actual next
		      next   (car rest)
		      rest   (cdr rest)))
	  (if actual (push actual res))
	  (make-bv-composition-from-list (reverse res))))
    bv))

(defun fixed-gamma (bv)
  ;; Turns boolan expressions into BDDs
  (cond
   ((node-p bv) bv)
   ((rec-bv-bool-apply? bv)
    (let* ((op (dp::constant-id (dp::funsym bv)))
	   (args (mapcar #'(lambda (x)
			     (fixed-alpha
			      (flatten-bv-constants x)))
			   (dp::funargs bv)))
	   (slicing (overlay-vector-list
		     (mapcar #'bv-term-to-slicing args)))
	   (arg-lists (ziplis (mapcar #'(lambda (x)
					  (mapcar #'lift-to-bdd
						    (slice-bv-term x slicing)))
				      args))))
      ;;(princ (format nil "ARGS    : ~a~%ARG-LISTS:~a~%" args arg-lists))
      (make-bv-composition-from-list
       (mapcar #'(lambda (x) (lift-bdd-if
			      (bdd-apply-n op x (bv-length (car x)))))
	       arg-lists))
      ))
   (t (error-misc "fixed-gamma" bv "not caught."))))
    
(defun fixed-delta (bv)
  ;; canonize an bv-addition (!)
  (let* ((len (bv-addition-modulo bv))
	 (args (bv-addition-args bv))
	 (nargs (length args)))
    (case nargs
     (0 (make-bv-const 0 len))
     (1 (fixed-alpha (car args)))
     (2 (let ((slicing (make-fullslice len)))
	  (init-leaf-nodes 1)
	  (make-bv-composition-from-list
	   (riple-carry-add (pairlis
			     (mapcar #'lift-to-bdd (slice-bv-term (first args)  slicing))
			     (mapcar #'lift-to-bdd (slice-bv-term (second args) slicing)))
			    *false-node*))))
     (t (fixed-delta
	 (make-bv-addition-from-list
	  len
	  (cons (fixed-delta
		 (make-bv-addition len (car args) (cadr args)))
		(cddr args))))))))

;;; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;;;          T H E         S O L V E R
;;; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

(defun fixed-bv-solve (eq &key (stream nil)
			       (heuristic-level 0)
			       (call-after-canon nil))
 ;;; Heuristics:
 ;;;  0:  No melting of BDDs in advance of solving
 ;;;  1:  Melting on connect= ONE node matches (factor oo)
 ;;;  3:  Melting on connect= factor 4
 ;;;  5:  Melting on connect= factor 2.5
 ;;;  7:  Melting on connect= factor 2
 ;;;  9:  Melting on connect= factor 2.4
  (ifassert (is-fixed-arith-bv-equation? eq))
  (ifassert (eq (bv-length (dp::lhs eq)) (bv-length (dp::rhs eq))))
  (setf *heuristic-node-prior* 0
	*heuristic-node-post*  0)
  (let ((term1 (flatten-bv-constants (fixed-sigma (dp::lhs eq))))
	(term2 (flatten-bv-constants (fixed-sigma (dp::rhs eq)))))
    (ifassert (eq (bv-length term1) (bv-length term2)))
    (ifuncall call-after-canon)
    (obs-fs (princ (format nil "Term1: ~a~%" term1) stream))
    (obs-fs (princ (format nil "Term2: ~a~%" term2) stream))
    (fail-closure :SET nil)

    (if (equalp term1 term2)
	'(TRUE)
      (let* (
	     (dummy (obs-fs (princ (format nil "################# Starting Csolve ###############~%") stream)))
	     (heuristically-combined nil)
	     (not-trivially-true nil)
	     (slice (overlay-vector-list
		     (list (bv-term-to-slicing term1)
			   (bv-term-to-slicing term2))))
	     (eqs (remove-duplicates
	       (pairlis
		(slice-bv-term term1 slice)
		(slice-bv-term term2 slice))
	       :TEST #'same-pair?))
	     (bool-eqs (if (= 1 (logand 1 heuristic-level))
			   (remove-if-not #'(lambda (x) (or (node-p (car x))
							    (node-p (cdr x))))
					  eqs)))
	     (eqs (if (= 1 (logand 1 heuristic-level))
		      (remove-if #'(lambda (x) (or (node-p (car x))
						   (node-p (cdr x))))
				 eqs)
		    eqs)) ;; non-bool-eqs
	     (vars (remove-duplicates
		    (append (loop for vs in (mapcar #'car eqs) append
				  (all-vars-in-bv-term vs))
			    (loop for vs in (mapcar #'cdr eqs) append
				  (all-vars-in-bv-term vs))
			    (loop for vs in (mapcar #'car bool-eqs) append
				  (all-vars-in-bv-term vs))
			    (loop for vs in (mapcar #'cdr bool-eqs) append
				  (all-vars-in-bv-term vs)))
		    :TEST #'equal))
	     (nvars (length vars))
	     (blocks (make-array (length vars) :initial-element nil))
	     (slices (loop for v in vars collect
			   (make-nullslice (bv-var-length v))))
	     (changes t)
	     (final nil))
	(declare (ignore dummy))
	(labels ((fresh-var-call (var)
		   (nconc vars   (list var))
		   (nconc slices (list (make-nullslice (bv-var-length var)))))
		 (var-pos (var) (position var vars :TEST #'equal))
	     (var-slice (var)
	       (obs-fs (princ (format nil "-- (var-slice ~a) --> " var) stream))
	       (let ((res
		      (cond
		       ((rec-bv-var? var) (nth (var-pos var) slices))
		       ((rec-bv-const? var)
			(make-nullslice (bv-const-length var)))
		       ((rec-bv-extraction? var)
			(extract-slice (nth (var-pos (bv-extraction-bv var)) slices)
				       (bv-extraction-left  var)
				       (bv-extraction-right var)))
		       ((and (node-p var)
			     (leaf-node? var))
			(make-nullslice (bv-length var)))
		       ((node-p var)
			(overlay-vector-list
			 (mapcar #'var-slice (vars-of-node var))))
		       (t (error-misc "(local) var-slice" var "not caught.")))))
		 (obs-fs (princ (format nil "~a~%" res) stream))
		 res))
	     (entry-slice-var (extr)
	       (obs-fs (princ (format nil "-- (entry-slice-var ~a) --> ~a~%" extr "?") stream))
	       (cond
		((rec-bv-const? extr))
		((rec-bv-var? extr))
		((node-p extr)
		 (mapcar #'entry-slice-var (all-bdd-nodes extr)))
		((rec-bv-extraction? extr)
		 (let ((var   (bv-extraction-bv extr))
		       (left  (bv-extraction-left extr)))
		   ;; only there is the new cut:
		   (setf (aref (var-slice var) left) t)))
		(t (error-misc "[local]entry-slice-var" extr "not caught."))))
	     ;;;(has-failed? () (not possibly-solvable))
	     ;;;(fail () (setf possibly-solvable nil))
	     )
      (obs-fs (princ (format nil "Original Vars: ~a~%" vars) stream))
      (loop for extr in (append (all-recognized-in-arith-bool-bv-term term1 #'(lambda (x) (rec-bv-extraction? x)))
				(all-recognized-in-arith-bool-bv-term term2 #'(lambda (x) (rec-bv-extraction? x)))) do
	    (entry-slice-var extr))
      (loop for e in eqs do
	    (obs-fs (princ (format nil "csolve[~a = ~a]~%" (car e) (cdr e)) stream))
	    (loop for b in (fixed-csolve (car e) (cdr e)
					 :FRESH-VAR-CALL #'fresh-var-call
					 :FAIL #'fail-closure) do
		  (obs-fs (princ (format nil " => ~a = ~a~%" (car b) (cdr b)) stream))
		  (setf not-trivially-true t)
		  (push (cdr b)
			(aref blocks (var-pos (car b)))))
	    (if (has-failed?) (return)))
      (setf heuristically-combined (heuristically-combine bool-eqs
					    :STREAM stream
					    :HEURISTIC-LEVEL heuristic-level))
      (if (some #'false-node? heuristically-combined)
	  (fail-closure)
	(loop for e in (heuristically-combine bool-eqs
					      :STREAM stream
					      :HEURISTIC-LEVEL heuristic-level)
	    do
	      (obs-fs (princ (format nil "csolve[~a = TRUE]~%" e) stream))
	      (loop for b in (fixed-csolve-bdd e
					       :FRESH-VAR-CALL #'fresh-var-call
					       :FAIL #'fail-closure) do
		    (obs-fs (princ (format nil " => ~a = ~a~%" (car b) (cdr b)) stream))
		    (setf not-trivially-true t)
		    (push (cdr b)
			  (aref blocks (var-pos (car b)))))
	      (if (has-failed?) (return))))
      (obs-fs (princ (format nil "Vars   : ~a~%Slices : ~a~%" vars slices) stream))
      ;;; ------------------------------
      (if (has-failed?)  (return-from fixed-bv-solve '(FALSE)))
      (unless not-trivially-true (return-from fixed-bv-solve '(TRUE)))
      ;;; ------------------------------
      ;;;  Coarsest Slicing
      ;;; ------------------------------
      (obs-fs (princ (format nil "################# Starting Coarsest Slicing ###############~%") stream))
      (loop while changes do
       (setf changes nil)	    
       (loop for i from 0 to (1- nvars) do
	     (let* ((sl     (nth i slices))
		    (sl-old (copy-slicing sl)))
	       (loop for term in (aref blocks i) do
		     (let ((slice-list (mapcar #'var-slice
					       (reverse (bv-flat-term-content term)))))
		       (or-of-slices!  sl     slice-list)
		       (and-of-slices! sl-old slice-list)))
	       (unless (equalp sl sl-old)
		 (setf
		     changes t
		     (aref blocks i)
		     (mapcar #'(lambda (x)
				 (make-bv-composition-from-list
				  (slice-bv-term x sl :ENTRY-SLICE-VAR #'entry-slice-var)))
			     (aref blocks i)))))))
      ;;; ------------------------------
      ;;; Propagation of Equality
      ;;; ------------------------------
      (obs-fs (princ (format nil "################## Starting Propagation #####################~%") stream))
      (obs-fs (princ (format nil "Vars   : ~a~%Slices : ~a~%Blocks : ~a~%" vars slices blocks) stream))
      (setf final
	(mapcar #'fixed-beta
	   (process-fixed-propagation
	    (loop for i from 0 to (1- nvars) collect
		  (let ((j 0)
			(columns (make-list (length (slicing-to-chop-list (nth i slices))) :initial-element nil)))
		    (loop for line in (aref blocks i) do
			  (setf j 0)
			  (loop for term in (bv-flat-term-content line) do
				(pushnew term (nth j columns)
					 :TEST #'equalp)
				(incf j)))
		    columns))
	    :FAIL   #'fail-closure
	    :HAS-FAILED? #'has-failed?
	    :STREAM stream)))
      ;;;--------------------
      ;;; END
      ;;;--------------------
      (obs-fs (loop for i from 0 to (1- nvars) do
		 (princ (format nil "~a  =  ~a~%" (nth i vars) (nth i final)) stream)))
      ;;;--------------------
      (if (has-failed?)
	  '(FALSE)
	  (loop for i from 0 to (1- nvars) collect
		(make-bv-equation
		 (nth i vars)
		 (nth i final))))
    )))))
   

;;; ******************************
;;;  Failing Closure
;;; ******************************

(defun fail-closure (&key (set t) (quest nil))
  (defvar *internal-fail-var*)
  (cond
   (quest *internal-fail-var*)
   (t (setf *internal-fail-var* set))))

(defun has-failed? ()
  (fail-closure :QUEST t))

;;; ---

(defun pair-starts-with-1? (x)
  (eq (car x) 1))

;;; ========================================


(defun fixed-csolve (t1 t2 &key (fresh-var-call nil)
				(fail #'break))
  ;;; Chunk-Solve
  ;;;  where t1 and t2 are flat terms
  ;;; when creating fresh variables, fresh-var-call is called with
  ;;;  the fresh var as an argument
  (cond
   ((equal t1 t2) nil)
   ((and (rec-bv-const? t1)
	 (rec-bv-const? t2))
    (ifuncall fail) nil)
   ((or (node-p t1)
	(node-p t2))
    (fixed-csolve-bdd (bdd-apply-n 'BV-EQUIV
				   `(,(lift-to-bdd t1) ,(lift-to-bdd t2))
				   (bv-length t1))
		      :FRESH-VAR-CALL fresh-var-call
		      :FAIL fail))
   ((rec-bv-const? t1)
    (list (cons (fixed-bv-content t2)
		(pad-fixed-bv-if t1 t2 :FRESH-VAR-CALL fresh-var-call))))
   ((rec-bv-const? t2)
    (list (cons (fixed-bv-content t1)
		(pad-fixed-bv-if t2 t1 :FRESH-VAR-CALL fresh-var-call))))
   ;;; Two non-constants!
   ((and (rec-bv-var-or-var-extract? t1)
	 (rec-bv-var-or-var-extract? t2))
    (fixed-csolve-var-var t1 t2 :FRESH-VAR-CALL fresh-var-call))

   (t (error-misc "fixed-csolve" (list t1 t2) "not caught."))))

(defun fixed-csolve-var-var (t1 t2 &key (fresh-var-call nil))
  (let ((v1 (fixed-bv-content t1))
	(v2 (fixed-bv-content t2)))
    (if (equal v1 v2)
	;;; csolve-same-var [extraction, neccessarily]
        (let* ((len (bv-var-length v1))
	       (l1 (bv-extraction-left t1))
	       (r1 (bv-extraction-right t1))
	       (l2 (bv-extraction-left t2))
	       (r2 (bv-extraction-right t2))
	       (hl (max l1 l2))
	       (ll (min l1 l2))
	       (hr (max r1 r2))
	       (lr (min r1 r2)))
	  (if (< ll hr)
	      (let ((common (make-fresh-bv-var (1+ (- hl hr))
					       :FRESH-VAR-CALL fresh-var-call)))
		(list
		 (cons
		  v1 (make-bv-composition-from-list
		      (list (make-fresh-bv-var (- len hl 1)
					       :FRESH-VAR-CALL fresh-var-call)
			    common
			    (make-fresh-bv-var (- hr ll 1)
					       :FRESH-VAR-CALL fresh-var-call)
			    common
			    (make-fresh-bv-var lr
					       :FRESH-VAR-CALL fresh-var-call))))))
	    (let* ((delta (- hr lr))
		   (overlap (1+ (- ll hr)))
		   (gamma (+ overlap delta delta))
		   (k-times (DIV gamma delta))
		   (lambda (MOD gamma delta))
		   (fresh1 (make-fresh-bv-var lambda
					      :FRESH-VAR-CALL fresh-var-call))
		   (fresh2 (make-fresh-bv-var (- delta lambda)
					      :FRESH-VAR-CALL fresh-var-call)))
	      (list
	       (cons v1
		     (fixed-alpha
		      (pad-fixed-bv-if
		       (make-bv-composition-from-list
			(cons fresh1 (loop for i from 1 to k-times append
					   (list fresh2 fresh1))))
		       (make-bv-extraction v1 hl lr)
		       :FRESH-VAR-CALL fresh-var-call)))))))
	
	 ;; -- different vars --
	 (let ((fresh (make-fresh-bv-var (bv-length t1) :FRESH-VAR-CALL fresh-var-call)))
	  (list
	   (cons v1 (pad-fixed-bv-if fresh t1 :FRESH-VAR-CALL fresh-var-call))
	   (cons v2 (pad-fixed-bv-if fresh t2 :FRESH-VAR-CALL fresh-var-call)))))))

(defun fixed-csolve-bdd (node &key (fresh-var-call nil)
				   (fail nil))
  (let* ((len  (bv-length node)))
    (init-leaf-nodes len)
    (loop for res in (bdd-solve node
				:FRESH-VAR-CALL fresh-var-call
				:FAIL fail) collect
	  (cons
	   (fixed-bv-content (car res))
	   (pad-fixed-bv-if (cdr res) (car res) :FRESH-VAR-CALL fresh-var-call)))))


(defun heuristically-combine (bool-eq-list &key (stream nil)
						(heuristic-level 1))
  ;;; Heuristic-Level:    1 -> weakly connected
  ;;;                     3 -> only strongly connected
  ;;;                     5 -> only heavy connected
  (setf *heuristic-node-prior* (length bool-eq-list))
  (unless (null bool-eq-list)
    (let ((phi (loop for b in bool-eq-list collect
		     (let ((node (bdd-apply-n 'BV-EQUIV
					      `(,(lift-to-bdd (car b))
						,(lift-to-bdd (cdr b)))
					      (bv-length (car b)))))
		       (cons node (all-bdd-nodes node)))))
	  (actual nil)
	  (res nil)
	  (changes nil))
      (flet ((connected? (a b)
	       (case heuristic-level
		 (1 (not (null (intersection (cdr a) (cdr b) :TEST #'equal))))
		 (3 (<= (+ (length (cdr a)) (length (cdr b)))
			(* 4 (length (intersection (cdr a) (cdr b) :TEST #'equal)))))
		 (5 (<= (+ (length (cdr a)) (length (cdr b)))
			(* 2.5 (length (intersection (cdr a) (cdr b) :TEST #'equal)))))
		 (7 (=  (+ (length (cdr a)) (length (cdr b)))
			(* 2 (length (intersection (cdr a) (cdr b) :TEST #'equal)))))
		 (9 (<= (+ (length (cdr a)) (length (cdr b)))
			(* 2.4 (length (intersection (cdr a) (cdr b) :TEST #'equal))))) ;; Testbed
		 (t (error-misc "heuristically combine" (list heuristic-level) "unknown heuristic")))))	       
	(loop while phi do
	      (setf actual (car phi)
		    phi    (cdr phi)
		    changes t)
	      (loop while changes do
		    (setf changes nil)
		    (loop for new in phi do
			  (if (connected? actual new)
			      (let ((new-node (bdd-apply-n 'BV-AND `(,(car actual) ,(car new)) (bv-length (car actual)))))
				(setf changes t
				      actual  (cons new-node (all-bdd-nodes new-node))
				      phi (delete new phi :TEST #'equalp))
				(return)))))
	      (push (car actual) res))
	(obs-fs (princ (format nil "Heuristically combine resulted in ~d Nodes:~%~a~%" (length res) res) stream))
	(setf *heuristic-node-post* (length res))
	res))))
		       

;;; ===============================

(defun process-fixed-propagation (llist &key (fail #'break)
					     (has-failed? #'break)
					     (stream nil))
  ;; Propagate equality within a list of lists of equivalence classes
  ;;  [each list representing one original variable]
  ;; builds assoc-list representing the union-find-structure
  ;; Returns a list of _terms_
  (let* ((alist nil)
	 (all-contents (loop for e in llist append
					       (apply #'append e)))
	 (flats (remove-if #'is-bv-const?
			   (loop for cont in all-contents
			       append
				 (if (node-p cont)
				     (all-bdd-nodes cont)
				   `(,cont)))))
	 (flat-times (mapcar #'(lambda (x) (count x flats :TEST #'equal))
			     flats))
	 (flat-zip (pairlis flat-times flats))
	 (one-timers
	  (mapcar #'cdr
		  (remove-if-not #'pair-starts-with-1? flat-zip)))
	 (bdds (remove-if-not #'node-p
			      (remove-duplicates all-contents :TEST #'equal)))
	 (n-bdds (length bdds))
	 (num-to-bdd (pairlis (count-up n-bdds) bdds))
	 (rep-llist (loop for var in llist collect
			  (loop for column in var collect
				(loop for entry in column collect
				      (if (node-p entry)
					  (find-via-rassoc entry num-to-bdd :TEST #'eq)
					entry)))))
	 (chunks (remove-duplicates flats :TEST #'equal))
	 (chunk-to-indices
	  (loop for c in chunks collect
		(cons c
		      (loop for b in bdds append
			    (if (member c (all-bdd-nodes b) :TEST #'equal)
				(list (car (rassoc b num-to-bdd))))))))
	 (classes
	  (loop for varcl in rep-llist append
		(loop for cl in varcl collect
		      (set-difference cl one-timers :TEST #'equal)))))
    (labels ((replace-in-indices (chunk bdd)
	       (if bdds
		   (loop for i in (cdr (assoc chunk chunk-to-indices :TEST #'equal)) do
			 (let* ((old-bdd (cdr (assoc i num-to-bdd)))
				(old-chks (all-bdd-nodes old-bdd))
				(new-bdd (bdd-compose old-bdd (lift-to-bdd bdd) chunk))
				(new-chks (all-bdd-nodes new-bdd))
				(obsolete (set-difference old-chks new-chks
							  :TEST #'equal))
				(newcomers (set-difference new-chks old-chks)))
			   (obs-fs (princ (format nil "Replacing [~d] ~a~%via ~a <- ~a~%by ~a~%" i old-bdd chunk bdd new-bdd) stream))
			   (setf num-to-bdd
			     (cons (cons i new-bdd)
				   (delete i num-to-bdd :TEST #'(lambda (x y) (eq x (car y))))))
;;;    The following, strangely, does not work
;;;		       (setf (cdr (assoc i num-to-bdd)) new-bdd)
		       (loop for c in obsolete do
			     (let ((find (assoc c chunk-to-indices :TEST #'equal)))
			       (if find
				   (setf (nth (position find chunk-to-indices :TEST #'equal)
					      chunk-to-indices)
				     (delete i find)))))
		      (obs-fs (princ (format nil "chunk-to-indices after deleting:~%~a~%" chunk-to-indices) stream))
		      (loop for c in newcomers do
			     (insert-index c i))))))
	     ;;;--------------------
	     (insert-index (c ind)
	       (let ((find (assoc c chunk-to-indices :TEST #'equal)))
		 (if find
		     (setf (nth (position c chunk-to-indices
					  :TEST #'(lambda (x y) (equal x (car y))))
				chunk-to-indices)
		       (nconc find `(,ind)))
		   (push (list c ind) chunk-to-indices))
		 (obs-fs (princ (format nil "New index for ~a: ~a~%Chunk-to-indices: ~a~%" c ind chunk-to-indices) stream))))
	     ;;; ----------------		   
	     (fs-merge (a b)
	       (let ((af (find-via-assoc a alist))
		     (bf (find-via-assoc b alist)))
		 (obs-fs (princ (format nil "-- merging: ~a~%            ~a~%" af bf) stream))
		 (cond
		  ((equal af bf))
		  ((and (rec-bv-const? af)
			(rec-bv-const? bf))
		   (ifuncall fail)
		   (return-from process-fixed-propagation nil))
		;;; --- here check for booleans:
		  ((or (numberp af)
		       (numberp bf))
		   (let* ((bdd1 (lift-to-bdd (find-via-assoc af num-to-bdd)))
			  (bdd2 (lift-to-bdd (find-via-assoc bf num-to-bdd)))
			  (n (bv-length bdd1))
			  (node (bdd-apply-n 'BV-EQUIV `(,bdd1 ,bdd2) n)))
		     (loop for eq in (bdd-solve node :FAIL fail) do
			   (obs-fs (princ (format nil "..merge ~a => ~a~%" (car eq) (cdr eq)) stream))
			   (if (ifuncall has-failed?) 
			       (return-from process-fixed-propagation nil))
			   (if (node-p (cdr eq))
			       (progn
				 (loop for c in (all-bdd-nodes (cdr eq)) do
				       (insert-index c n-bdds))
				 (replace-in-indices (car eq) (cdr eq))
				 (push (cons n-bdds (cdr eq)) num-to-bdd)
				 (push (cons (car eq) n-bdds) alist)
				 (incf n-bdds))
			     (progn
			       (replace-in-indices (car eq) (cdr eq))
			       (push eq alist))))))
		  ((rec-bv-const? af)
		   (setf alist (acons bf af alist))
		   (replace-in-indices bf af))
		  (t (setf alist (acons af bf alist))
		     (replace-in-indices af bf))))))	      
      (obs-fs (progn
		(princ (format nil "BDD-Embeddings:~%~a~%" num-to-bdd) stream)
		(princ (format nil "Chunks-to-Indices:~%~a~%" chunk-to-indices) stream)
		(princ (format nil "Equivalence Classes:~%") stream)
		(loop for cl in classes do
		      (princ (format nil "~a,~%" cl) stream))))
      (loop for c in classes do
	    ;; sometimes better: other order!
	    (unless (null c)
	      (let ((a (find-via-assoc (car c) alist)))
		(loop for b in (cdr c) do
		      (fs-merge a b)))))
      (obs-fs (princ (format nil "BDD-Embedding:~%~a~%Alist: ~a~%-> Combining result~%" num-to-bdd alist) stream))
      (let ((i -1))
	(loop for origc in rep-llist collect
	      (progn
		(obs-fs (princ (format nil "Column-list: ~a~%" origc) stream))
		(make-bv-composition-from-list
		 (loop for column in origc collect
		       (progn
			 (incf i)
			 (lift-bdd-if
			  (or (find-via-assoc (car (nth i classes)) (append alist num-to-bdd))
			      (make-fresh-bv-var (bv-length (car column))))))))))))))
     

(defun find-via-assoc (el alist &key (test #'equal))
  (let ((res (assoc el alist :TEST test)))
    (if (null res)
	el
      (find-via-assoc (cdr res) alist))))

(defun find-via-rassoc (el alist &key (test #'equal))
  (let ((res (rassoc el alist :TEST test)))
    (if (null res)
	el
      (find-via-rassoc (car res) alist))))


(defun fixed-bv-content (term)
  (cond
   ((rec-bv-var? term) term)
   ((rec-bv-extraction? term) (bv-extraction-bv term))
   (t (error-misc "fixed-bv-content" term "not caught."))))

(defun pad-fixed-bv-if (term pattern &key (fresh-var-call nil))
  ;;; returns a eventually padded bv-term with >term< in the middle
  ;;; according to pattern (possibly an extraction)
  (cond
   ;; no padding
   ((rec-bv-var? pattern)
    term)
   ((rec-bv-extraction? pattern)
    (let ((len   (bv-length (bv-extraction-bv pattern)))
	  (left  (bv-extraction-left  pattern))
	  (right (bv-extraction-right pattern)))
      (make-bv-composition-from-list
       (list (make-fresh-bv-var (- len left 1) :FRESH-VAR-CALL fresh-var-call)
	     term
	     (make-fresh-bv-var right :FRESH-VAR-CALL fresh-var-call)))))
   (t (error-misc "pad-fixed-bv-if" pattern "not caught"))))
       
(defun same-pair? (x y)
  (or (equalp x y)
      (and (equalp (car x) (cdr y))
	   (equalp (car y) (cdr x)))))


;;; ********************
;;;  Examples
;;; ********************

(defun ex-1 ()  ;;; TAUTOLOGY
   (fixed-bv-solve
   '(BV-EQUAL
     (bv-addition 4 (bv-var x 4) (bv-var y 4))
     (bv-addition 4 (bv-var y 4) (bv-var x 4)))))

(defun ex-2 ()  ;;; UNSATISFIABLE
  (fixed-bv-solve
   '(BV-EQUAL (bv-compose (bv-var x 16) (bv-extract (bv-var x 16) (tupcons 11 0)))
              (bv-compose (bv-var y 4) (bv-const 0 12) (bv-const 1 12)))))

(defun ex-3 () ;; Satisfiable but not valid
  (fixed-bv-solve
   '(bv-equal
     (bv-and (bv-var x 3)
             (bv-var y 3))
     (bv-var x 3))))
