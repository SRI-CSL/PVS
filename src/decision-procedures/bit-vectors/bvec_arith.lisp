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
;;; ------------------------------------------------------------
;;; -      Oliver M"oller (moeller@ki.informatik.uni-ulm.de)   -
;;; ------------------------------------------------------------
;;; - File:     bvec_structure.cl                              -
;;; - Purpose:  Constructor, Destructor, Recognizer            -
;;; -           of arithmetical <arith-bv>                     -
;;; - USES:     bvec_structure.cl                              -
;;; -           bvec_bdd_solve.cl                              -
;;; ------------------------------------------------------------
;;;
;;; Allegro Common Lisp
;;; Begun:         October, 28th 1997
;;; Last Changes:  March, 16th   1998
;;; Version 2.0

(in-package bvec)

;;; ----------------------------------------------------------------------
;;; - Access to the data-structures
;;; ----------------------------------------------------------------------
;;;
;;; <arith-bv>            ::= <bv> | <bv-addition>
;;; <bv-addition>         ::= '('bv-addition <posnat> {<arith_bv>}+ ')'
;;;
;;; <fixed-arith-bv>      ::= <fixed-bv> | <fixed-bv-addition>
;;; <fixed-bv-addition>   ::= '('bv-addition  <fixed-posnat>
;;;                                          {<fixed-arith-bvec>}+ ')'
;;;
;;; ------------------------------------------------------------


(defun is-arith-bv? (bv)
  (or (is-bv-addition? bv)
      (is-bv? bv)))

(defun is-bv-addition? (bv)
  (and (dp::application-p bv)
       (> (dp::arity bv) 1)
       (eq (dp::funsym bv) *bv-addition*)
       (is-nat? (dp::arg 1 bv))
       (every #'is-arith-bv? (dp::funargs bv))))

(defun is-fixed-arith-bv-equation? (eq)
  (and (dp::application-p eq)
       (eq (dp::funsym eq) *BV-EQUAL*)
       (is-fixed-arith-bv? (dp::arg 1 eq))
       (is-fixed-arith-bv? (dp::arg 2 eq))))

(defun is-fixed-arith-bv? (bv)
  (cond
   ((is-fixed-bv-addition? bv) t)
   ((rec-bv-composition? bv) (every #'is-fixed-arith-bv? (bv-composition-content bv))) 
   ((rec-bv-extraction? bv)
    (and (is-fixed-arith-bv? (bv-extraction-bv bv))
	 (is-fixed-nat? (bv-extraction-left bv))
	 (is-fixed-nat? (bv-extraction-right bv))))
   (t (is-fixed-bv? bv))))


(defun is-fixed-bv-addition? (bv)
  (and (dp::application-p bv)
       (> (dp::arity bv) 1)
       (eq (dp::funsym bv) *bv-addition*)
       (is-fixed-posnat? (dp::arg 1 bv))
       (every #'is-fixed-arith-bv? (cdr (dp::funargs bv)))))

(defun is-fixed-arith-bool-bv? (bv)
  (or  (is-fixed-bv-var? bv)
       (is-bv-const? bv)
       (is-fixed-arith-bool-extraction? bv)
       (is-fixed-arith-bool-composition? bv)
       (is-fixed-arith-bool-addition? bv)
       (and (dp::application-p bv)
	    (let* ((op    (bv-recognizer bv))
		   (args  (bv-bool-args bv))
		   (nargs (length args)))
	      (and (or (and (member op *bvec_bool_mops*)
			    (= nargs 1))
		       (and (member op *bvec_bool_binops*)
			    (= nargs 2)))
		   (every #'is-fixed-arith-bool-bv? args))))))

(defun is-fixed-arith-bool-extraction? (l)
  (and (dp::application-p l)
       (=  (dp::arity l) 2)
       (eq (dp::funsym l) *BV-EXTRACT*)
       (is-fixed-arith-bool-bv? (dp::arg 1 l))
       (dp::application-p (dp::arg 2 l))
       (= (dp::arity (dp::arg 2 l)) 2)
       (eq (dp::funsym (dp::arg 2 l)) *TUPCONS*)
       (is-fixed-nat? (bv-extraction-left l))
       (is-fixed-nat? (bv-extraction-right l))
       (< (bv-extraction-left l) (arith-bool-bv-length (dp::arg 1 l)))
       (<= (bv-extraction-right l) (bv-extraction-left l))))

(defun is-fixed-arith-bool-composition? (l)
  (and (dp::application-p l)
       (> (dp::arity l) 1)
       (equal (dp::funsym l) *BV-COMPOSE*)
       (every #'is-fixed-arith-bool-bv? (dp::funargs l))))

(defun is-fixed-arith-bool-addition? (bv)
  (and (dp::application-p bv)
       (> (dp::arity bv) 1)
       (eq (dp::funsym bv) *bv-addition*)
       (is-fixed-posnat? (dp::arg 1 bv))
       (every #'is-fixed-arith-bool-bv? (dp::funargs (dp::arg 1 bv)))))


;;; ------------------------------------------------------------
;;;  Recognizing Types
;;; ------------------------------------------------------------

(defmacro rec-arith-bv? (bv)
  `(or (rec-bv-addition? ,bv)
       (rec-bv? ,bv)))

(defmacro rec-fixed-arith-bv? (bv)
  `(or (rec-fixed-bv-addition? ,bv)
       (rec-fixed-bv? ,bv)))

(defmacro rec-fixed-bv-addition? (bv)
  `(and (dp::application-p ,bv)
	(eq ,(dp::funsym bv) *bv-addition*)))

(defmacro rec-nat-equation? (x)
  `(and (dp::application-p ,x)
	(eq (dp::funsym ,x) *NAT-EQUAL*)))

(defmacro rec-nat-leq? (x)
  `(and (dp::application-p ,x)
	(eq (dp::funsym ,x) *NAT-<=*)))

(defmacro rec-nat-less? (x)
  `(and (dp::application-p ,x)
	(eq (dp::funsym ,x) *NAT-<*)))


;;; Build BDDs representing an addition

(defun riple-carry-add (list carry)
  ;; input:  list of (cons) representing an addition (first = least sig. bit)
  ;; carry is the carry-BDD (at the moment)
  ;; output: list of BDDs   encoding this addition
  (if (null list)
      nil
    (let* ((a (caar list))
	   (b (cdar list))
	   (rest (cdr list)))
    (nconc
     (riple-carry-add rest
		      (bdd-or (bdd-or (bdd-and a b)
				      (bdd-and a carry))
			      (bdd-and b carry)))
     `(,(lift-bdd-if (bdd-xor a (bdd-xor b carry))))))))
  

;;; ----------------------------------------
;;; Destructors
;;; ----------------------------------------

(defun arith-bool-bv-length (bv)
  (cond
   ((rec-bv-addition? bv)
     (bv-addition-modulo bv))
    ((rec-bv-bool? bv)
     (arith-bool-bv-length
      (car (bv-bool-args bv))))
    ((rec-bv-composition? bv)
     (apply #'+ (mapcar #'arith-bool-bv-length (bv-composition-content bv))))
    (t
     (bv-length bv))))


;;; ----------------------------------------
;;; Analytic Funcions
;;; ----------------------------------------

(defun all-recognized-in-arith-bool-bv-term (bv recog?)
  ;; al subterms that satisfy the predicate recog?
  (cond
   ((funcall recog? bv) (list bv))
   ((rec-bv-const? bv) nil)
   ((rec-bv-var? bv)
    (all-recognized-in-nat-term (bv-var-length bv) recog?))
   ((rec-bv-extraction? bv)
    (all-recognized-in-arith-bool-bv-term (bv-extraction-bv bv) recog?))
   ((rec-bv-composition? bv)
    (loop for e in (bv-composition-content bv) append
	  (all-recognized-in-arith-bool-bv-term e recog?)))
   ((rec-bv-addition? bv)
    (loop for e in (bv-addition-args bv) append
	  (all-recognized-in-arith-bool-bv-term e recog?)))
   ((node-p bv)
    (loop for e in (all-bdd-nodes bv) append
	  (all-recognized-in-arith-bool-bv-term e recog?)))
   ((rec-bv-bool? bv)
    (loop for e in (bv-bool-args bv) append
	  (all-recognized-in-arith-bool-bv-term e recog?)))
   ((rec-bv-equation? bv)
    (loop for e in (bv-equation-args bv) append
	  (all-recognized-in-arith-bool-bv-term e recog?)))
   (t (error-misc "all-recognized-in-arith-bool-bv-term" bv "not caught."))))

(defun all-recognized-in-nat-term (nt recog?)
  (cond
   ((funcall recog? nt) (list nt))
   ((numberp nt) nil)
   ((rec-nat-var? nt) nil)
   ((consp nt)
    (loop for e in (cdr nt) append
	  (all-recognized-in-nat-term e recog?)))
   (t (error-misc "all-recognized-in-nat-term" nt "not caught."))))

(defun all-vars-in-nat-term (term)
  (all-recognized-in-nat-term term #'is-nat-var?))

;;; ------------------------------------------------------------
;;;  Synthetic Functions
;;; ------------------------------------------------------------

(defun arith-term-replace-via-alist (term alist)
  (if (null alist)
      term
    (let ((hit (assoc term alist :TEST #'equal)))
      (flet ((rec-rep (x) (arith-term-replace-via-alist x alist)))
	(cond
	 (hit (cdr hit))
	 ((rec-bv-var? term)
	  (make-bv-var (bv-var-name term)
		       (rec-rep (bv-var-length term))))
	 ((rec-nat-var? term) term)	
	 ((rec-addition? term)
	  (canonize-nat-term `(+ ,@(mapcar #'rec-rep (cdr term)))))
	 ((rec-subtraction? term)
	  (canonize-nat-term `(- ,@(mapcar #'rec-rep (cdr term)))))
	 ((rec-multiplication? term)
	  (canonize-nat-term `(* ,@(mapcar #'rec-rep (cdr term)))))
	 ((rec-div? term)
	  (canonize-nat-term `(DIV ,@(mapcar #'rec-rep (cdr term)))))
	 ((rec-mod? term)
	(canonize-nat-term `(MOD ,@(mapcar #'rec-rep (cdr term)))))
	 ((rec-division? term)
	  (canonize-nat-term `(/ ,@(mapcar #'rec-rep (cdr term)))))
	 ((numberp term)
	  term)
	 ((rec-bv-const? term) term)
	 ((rec-bv-extraction? term)
	  (make-bv-extraction (rec-rep (bv-extraction-bv term))
			      (rec-rep (bv-extraction-left term))
			      (rec-rep (bv-extraction-right term))))
	 ((rec-bv-composition? term)
	  (make-bv-composition-from-list
	   (mapcar #'rec-rep (bv-composition-content term))))
	 ((rec-bv-addition? term)
	  (make-bv-addition
	   (bv-addition-modulo term)
	   (mapcar #'rec-rep (bv-addition-args term))))
	 ((rec-bv-equation? term)
	  (make-bv-equation (rec-rep (second term))
			    (rec-rep (third  term))))
	 (t (error-misc "arith-term-replace-via-alist" term "not caught.")))))))
  

