;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; M. Oliver M"oller
;;; University of Ulm
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
;;; -           of <bvec_term> and subclasses                  -
;;; - USES:     -                                              -
;;; ------------------------------------------------------------
;;;
;;; Allegro Common Lisp
;;; Begun:         October, 12th 1997
;;; Last Changes:  March, 16th    1998
;;; Version 2.0
;;;
;;; ----------------------------------------------------------------------
;;; Data-structures
;;; ----------------------------------------------------------------------
;;;
;;; <bvec>               ::= <bv-var>|<bv-const>|<composition>|<extraction>
;;; <bv-var>             ::= '('BV-VAR <var_name> <nat> ')'
;;; <composition>        ::= '('BV-COMPOSE <bvec> {<bvec>}+ ')'
;;; <extraction>         ::= '('BV-EXTRACT <bvec> '(' <nat> <nat> ')'
;;;
;;; <bv-const>           ::= '('BV-CONST <fixed-nat>       <fixed-nat> ')'
;;;                                      Meaning the VALUE 
;;; 
;;; <bv-bdd>             ::= #NODE
;;; ----------------------------------------------------------------------
;;;
;;; <nat>                ::= <nat-var>|<fixed-nat>
;;; <nat-var>            ::= '('nat-var <var-name>')'
;;; <fixed-nat>          ::= {0,1,2,3, ...}
;;; <fixed-int>          ::= {... -2,-1,0,1,2, ...}
;;; <posnat>             ::= <nat-var>|<fixed-nat>
;;; <fixed-posnat>       ::= {  1,2,3, ...}
;;; 
;;; <fixed-bvec>         ::= <bv-fixed-var>|<bv-const>|<fixed-composition>|<fixed-extraction>
;;; <bv-fixed-var>       ::= '('BV-VAR <var_name> <fixed-nat> ')'
;;; <fixed-composition>  ::= '('BV-COMPOSE <fixed-bvec> {<fixed-bvec>}+ ')'
;;; <fixed-extraction>   ::= '('BV-EXTRACT <fixed-bvec> 
;;;                              '(TUPCONS' <fixed-nat> <fixed-nat> '))'
;;;
;;; ----------------------------------------------------------------------
;;;
;;;		      CONSTRUCTORS                   DESTRUCTORS	
;;; - <bv-var>         make-fresh-bv-var              bv-var-length
;;;                    make-bv-var                    bv-var-name
;;;                                          
;;; - <extraction>     make-bv-extraction             bv-extraction-length
;;;                                                   bv-extraction-bv
;;;                                                   bv-extraction-left
;;;                                                   bv-extraction-right
;;;				
;;; - <composition>    make-bv-composition            bv-decompose-left
;;;                    make-bv-composition-from-list  bv-decompose-right
;;;                                                   bv-composition-content
;;;
;;; - <bv-const>       make-bv-const                  bv-const-length
;;;                    append-bv-const                bv-const-value
;;;                    extract-bv-const
;;;
;;; - <bv-obdds>       (make-node)
;;;		       [see bvec_bdd_solve.cl]
;;;                    NOTE: BDD is a synonym to OBDD
;;;                          (Ordered Binary Decision Diagram)
;;; ----------------------------------------------------------------------
;;;
;;; - <bvec-fixed> :   SUBCLASS of <bvec>
;;;
;;; - <bv-equation>         make-bv-equal
;;; - <nat-equation>        make-nat-equal
;;; - <nat-leq>             make-nat-leq
;;; - <nat-less>            make-nat-less
;;;
;;; The theory also contains the empty bit-vector BV-EPSILON : bvec[0]
;;;------------------------------------------------------------------------

(in-package bvec)

;; Miscellaneous

(defconstant *inf* EXCL::*INFINITY-DOUBLE*)

(defvar *true_bit* dp::*TRUE*)
(defvar *false_bit* dp::*FALSE*)
(defvar *bvec_value_range* `(,*true_bit* ,*false_bit*))

(defun mk-bv-predicate-sym (sym)
  (let ((result (dp::mk-constant sym)))
    (setf (dp::node-type result) 'bv-predicate)
    (setf (dp::node-interpreted? result) t)
    result))

;; Signature

(defvar *bv-compose* (mk-bv-operator 'bv-compose))
(defvar *bv-extract* (mk-bv-operator 'bv-extract))
(defvar *bv-var* (mk-bv-operator 'bv-var))
(defvar *bv-const* (mk-bv-operator 'bv-const))
(defvar *bv-fill* (mk-bv-operator 'bv-fill))

(defvar *bv-not* (mk-bv-predicate-sym 'bv-not))
(defvar *bv-and* (mk-bv-predicate-sym 'bv-and))
(defvar *bv-or* (mk-bv-predicate-sym 'bv-or))
(defvar *bv-xor* (mk-bv-predicate-sym 'bv-xor))
(defvar *bv-ite* (mk-bv-predicate-sym 'bv-ite))
(defvar *bv-equiv* (mk-bv-predicate-sym 'bv-equiv))
(defvar *bv-equal* (mk-bv-predicate-sym 'bv-equal))

(defvar *nat2bv* (mk-bv-operator 'nat2bv))
(defvar *bv2nat* (mk-bv-operator 'bv2nat))


(defvar *bvec_recognizers*
  (list *BV-COMPOSE* *BV-EXTRACT* *BV-VAR* *BV-CONST* *BV-FILL*
	*BV-AND* *BV-OR* *BV-NOT* *BV-XOR* *BV-ITE* *BV-EQUIV* *BV-EQUAL*))

(defvar *bvec_bool_mops* `(,*BV-NOT*))

(defvar *bvec_bool_binops*
  `(,*BV-AND* ,*BV-OR* ,*BV-XOR* ,*BV-EQUIV* ,*BV-EQUAL*))

(defvar *bvec_bool_ternops*   '() ) ;;BV-ITE))
(defvar *bvec_bool_ops*
  (append *bvec_bool_mops* *bvec_bool_binops* *bvec_bool_ternops*))

; predicates

(defun is-bv? (l) 
  (or (eq (dp::node-type l) 'bv-op) ; added -hr
      (is-bv-epsilon? l)
      (is-bv-var? l)
      (is-bv-const? l)
      (is-bv-extraction? l)
      (is-bv-composition? l)
      (is-bdd? l)))

(defun is-bv-var? (trm) 
  (and (dp::application-p trm)
       (eq (dp::funsym var) *BV-VAR*)))

(defun is-bv-const? (l)
  (and (dp::application-p l)
       (eq (dp::funsym l) *bv-const*)
       (is-fixed-nat? (dp::arg 1 l))
       (is-fixed-posnat? (dp::arg 2 l))))

(defun is-bv-extraction? (l)
  (and (dp::application-p l)
       (eq (dp::funsym l) *BV-EXTRACT*)   
       (tuple-p (dp::arg 2 l))
       (= (dp::arity (dp::arg 2 l)) 2)
       (is-bv? (dp::arg 1 l))
       (is-nat? (dp::arg 1 (dp::arg 2 l)))
       (is-nat? (dp::arg 2 (dp::arg 2 l)))))

(defun is-bv-composition? (l) 
   (and (dp::application-p l)
        (> (dp::arity l) 1)
	(equal (dp::funsym l) *BV-COMPOSE*)
	(every #'is-bv? (dp::funargs l))))

(defvar *nat-var* (dp::mk-constant 'nat-var))
(defvar *+* (dp::mk-constant '+))
(defvar *-* (dp::mk-constant '-))
(defvar *times* (dp::mk-constant '*))
(defvar *mod* (dp::mk-constant 'mod))
(defvar *div* (dp::mk-constant 'div))
(defvar *expt2* (dp::mk-constant 'expt2))

(defun is-nat? (l)
  (or (is-nat-var? l)
      (is-fixed-nat? l)))

(defun is-nat-var? (l)
  (and (dp::application-p l)
       (= (dp::arity l) 1)
       (eq (dp::funsym l) *NAT-VAR*)))

(defun is-fixed-nat? (l)
  (and (dp::dp-integerp l)
   (>= (dp::constant-id l) 0)))

(defun is-fixed-int? (l)
  (or (dp::dp-integerp l)
      (eq (dp::constant-id l) *inf*)
      (eq (dp::constant-id l) (- 0 *inf*))))

(defmacro rec-fixed-int? (l)
  `(dp::dp-integerp ,l))

(defun is-fixed-posnat? (l)
  (and (dp::dp-integerp l)
       (> (dp::constant-id l) 0)))

;;; ------------------------------------------------------------
;;;  Recognizing Types
;;; ------------------------------------------------------------

(defmacro rec-bv-var? (bv)
  `(and (dp::application-p ,bv)
        (eq (dp::funsym ,bv) *BV-VAR*)))

(defmacro rec-bv-const? (bv)
  `(and (dp::application-p ,bv)
	(eq (dp::funsym ,bv) *BV-CONST*)))

(defmacro rec-bv-composition? (bv)
  `(and (dp::application-p ,bv)
        (eq (dp::funsym ,bv) *BV-COMPOSE*)))

(defmacro rec-bv-extraction? (bv)
  `(and (dp::application-p ,bv)
        (eq (dp::funsym ,bv) *BV-EXTRACT*)))

(defmacro rec-bv-addition? (bv)
  `(and (dp::application-p ,bv)
	(eq (dp::funsym ,bv) *bv-addition*)))

(defmacro rec-bv? (bv)
  `(or (rec-bv-epsilon? ,bv)
       (rec-bv-var? ,bv)
       (rec-bv-const? ,bv)
       (rec-bv-extraction? ,bv)
       (rec-bv-composition? ,bv)
       (rec-bdd? ,bv)))

;; ---

(defmacro rec-nat? (l)
  `(or (rec-fixed-nat? ,l)
       (rec-nat-var? ,l)))

(defmacro rec-nat-var? (l)
  `(and (dp::application-p ,l)
	(= (dp::arity ,l) 1)
	(eq (dp::funsym ,l) *NAT-VAR*)))

(defmacro rec-fixed-nat? (l)
  `(and (dp::dp-integerp ,l)
	(>= ,(dp::constant-id l) 0)))

(defmacro rec-fixed-posnat? (l)
  `(and (dp::dp-integerp ,l)
	(> ,(dp::constant-id l) 0)))

(defmacro rec-nat-term? (l)
  `(or (rec-fixed-nat? ,l)
       (and (dp::application-p ,l)
	    (member (dp::funsym ,l)
		    (list *+* *-* *times* *MOD* *DIV* *expt2* *nat-var*)))))

(defmacro rec-bv-bool? (l)
  `(or (node-p ,l)
       (and (dp::application-p ,l)
	    (member (dp::funsym ,l) *bvec_bool_ops*))))

(defmacro rec-bv-bool-apply? (l)
  `(and (dp::application-p ,l)
	(member (dp::funsym ,l) *bvec_bool_ops*)))

(defmacro rec-bv-negation? (l)
  `(and (dp::application-p ,l)
	(eq (dp::funsym ,l) 'bv-not)))

;;; Special: Constructs where the (natural) vars underneath cannot be
;;; extracted

(defun rec-var-hiding-construct? (l)
  (or (rec-expt2? l)
      (rec-div? l)
      (rec-mod? l)))

(defvar *cs-info* (dp::mk-constant 'cs-info))

(defmacro make-cs-info (x sl)
  `(dp::mk-term `(,*CS-INFO* ,,x ,,sl)))

(defmacro rec-cs-info? (x)
  `(and (dp::application-p ,x)
	(eq (dp::funsym ,x) *CS-INFO*)))

(defun is-cs-info? (x)
  (and (dp::application-p x)
       (eq (dp::funsym x) *CS-INFO*)))


;;; ****************************************
;;;  SubClass: fixed-bv
;;;  Contains only bv-variables of fixed width
;;;           and  extractions  of fixed size
;;; ****************************************

(defun is-fixed-bv-equation? (eq)
  (and (dp::application-p eq)
       (eq (dp::funsym eq) *BV-EQUAL*)
       (is-fixed-bv? (dp::arg 1 eq))
       (is-fixed-bv? (dp::arg 2 eq))))

(defun is-fixed-bv? (bv)
  (or  (is-fixed-bv-var? bv)
       (is-bv-const? bv)
       (is-fixed-bv-extraction? bv)
       (is-fixed-bv-boolop? bv)
       (is-fixed-bv-composition? bv)))

(defun is-fixed-bv-var? (var) 
  (and (dp::application-p var)
       (= (dp::arity var) 2)
       (eq (dp::funsym var) *BV-VAR*)
       (is-fixed-posnat? (dp::arg 2 var))))

(defun is-fixed-bv-extraction? (l)
  (and (dp::application-p l)
       (=  (dp::arity l) 2)
       (eq (dp::funsym l) *BV-EXTRACT*)
       (is-fixed-bv? (arg 1 l))
       (dp::tuple-p (arg 2 l))
       (= (dp::arity (arg 2 l)) 3)
       (is-fixed-nat? (bv-extraction-left l))
       (is-fixed-nat? (bv-extraction-right l))
       (< (bv-extraction-left l) (bv-length (arg 1 l)))
       (<= (bv-extraction-right l) (bv-extraction-left l))))

(defun is-fixed-bv-composition? (l) 
  (and (dp::application-p l)
       (> (dp::arity l) 1)
       (eq (dp::funsym l) *BV-COMPOSE*)
       (every #'is-fixed-bv? (dp::funargs l))))

(defun is-fixed-bv-boolop? (l)
  (or (and (node-p l)
           (every #'is-fixed-bv? (all-bdd-nodes l)))
      (and (dp::application-p l)
	   (or (and (= (dp::arity l) 1)
		    (member (dp::funsym l) *bvec_bool_mops*))
	       (and (= (dp::arity l) 2)
		    (member (dp::funsym l) *bvec_bool_binops*))
	       (and (= (dp::arity l) 3)
		    (member (dp::funsym l) *bvec_bool_ternops*)))
	   (every #'is-fixed-bv? (dp::funargs l)))))

;;; ----------------------------------------------------------------------
;;;  Constructors
;;; ----------------------------------------------------------------------

(defun make-bv-var (name len)
  #+dbg(assert (or (not (stringp name))
		   (not (string= (EXCL::substring-simple name 0 6)
				 "FRESH_"))))
  (dp::mk-term `(,*BV-VAR* ,name ,len)))

(defun make-bv-const (val len)
  (dp::mk-term `(,*BV-CONST* ,(dp::mk-dp-number val)
			     ,(dp::mk-dp-number len))))

(defun append-bv-const (c1 c2)
  (dp::mk-term `(,*BV-CONST* ,(dp::mk-dp-number
			       (+ (bv-const-value c2)
				  (* (bv-const-value c1)
				     (expt 2 (bv-const-length c2)))))
			     ,(dp::mk-dp-number
			       (+ (bv-const-length c1)
				  (bv-const-length c2))))))

(defun extract-bv-const (value left right)
  (dp::mk-term `(,*BV-CONST* ,(dp::mk-dp-number
			       (mod (div value (expt 2 right))
				    (expt 2 (1+ (- left right)))))
			     ,(dp::mk-dp-number
			       (1+ (- left right))))))

(defun flatten-bv-constants (bv)
  "Turn constants into >> fills <<"
  (let ((cont (bv-flat-term-content bv)))
    (make-bv-composition-from-list
     (loop for b in cont append
	   (cond
	    ((rec-bv-var? b) (list b))
	    ((rec-bv-const? b)
	     (bv-flat-term-content
	      (flatten-bv-const b)))
	    ((rec-bv-extraction? b) (list b))
	    ((rec-bv-bool? b)  (list b))
	    (t (error "flatten-bv ~a hit unexpected structure." bv)))))))
     
(defun flatten-bv-const (const)
  "turn into a composition of terms containing only 0s or 1s"
  #+dbg(assert (rec-bv-const? const))
  (let* ((bools (nat2bools (bv-const-value const) (bv-const-length const)))
	 (chunks nil)
	 (now (car bools))
	 (width 1))
    (loop for b in (append (cdr bools) '(stop)) do
	  (if (eq now b)
	      (incf width)
	    (progn
	      (push (make-bv-const (if now (1- (expt 2 width)) 0) width)
		    chunks)
	      (setf width 1
		    now b))))
    (make-bv-composition-from-list (nreverse chunks))))
  

(defun make-bv-extraction (bv left right)
  (dp::mk-term
   `(,*BV-EXTRACT* ,bv
		   ,(dp::mk-term
		     `(,dp::*TUPLE* ,(dp::mk-dp-number left)
				    ,(dp::mk-dp-number right))))))


(defun make-bv-composition (bv1 bv2)
  (cond
   ((rec-bv-epsilon? bv1)
    bv2)
   ((rec-bv-epsilon? bv2)
    bv1)
   ((rec-bv-composition? bv1)
    (if (rec-bv-composition? bv2)
	(append bv1 (cdr bv2))
      (append bv1 (list bv2))))
   ((rec-bv-composition? bv2)
    (dp::mk-term (cons *BV-COMPOSE* (cons bv1 (dp::funargs bv2)))))
   (t (dp::mk-term `(,*BV-COMPOSE* ,bv1 ,bv2)))))

(defun make-bv-composition-from-list (l)
  (let ((list (remove-if #'is-bv-epsilon? l)))
    (cond ((null list) *BV-EPSILON*)
	  ((null (cdr list)) (car list))
	  (t (dp::mk-term `(,*BV-COMPOSE* ,@list))))))
  
;;; Natural Variables

(defun make-nat-var (x)
  (dp::mk-term `(,*nat-var* ,x)))

;;; Boolean Operations

(defmacro make-bv-bool-apply (op args)
  `(dp::mk-term `(,,op ,,@args)))

(defmacro make-bv-and (x y)
  `(dp::mk-term `(,*BV-AND* ,,x ,,y)))

(defmacro make-bv-or (x y)
  `(dp::mk-term `(,*BV-OR* ,,x ,,y)))

(defmacro make-bv-not (x)
  `(dp::mk-term `(,*BV-NOT* ,,x)))

(defmacro make-bv-equal (x y)
  `(dp::mk-term `(,*BV-EQUAL* ,,x ,,y)))

(defvar *nat-var-between* (dp::mk-constant 'nat-var-between))
(defvar *bv-addition* (dp::mk-constant 'bv-addition))
(defvar *bv-epsilon* (dp::mk-constant 'bv-epsilon))
(defvar *nat-equal* (dp::mk-constant 'nat-equal))
(defvar *nat-<=* (dp::mk-constant 'nat-<=))
(defvar *nat-<* (dp::mk-constant 'nat-<))

(defmacro make-nat-between (var low up)
  `(dp::mk-term `(,*NAT-VAR-BETWEEN* ,,var ,,low ,,up)))

;;; ----------------------------------------
;;;  Arithmetic Constructors
;;; ----------------------------------------

(defmacro make-bv-addition (modulo &rest args)
  `(dp::mk-term `(,*bv-addition* ,(dp::mk-dp-number ,modulo) ,,@args)))

(defun make-bv-addition-from-list (modulo list)
  (dp::mk-term
   (cons *bv-addition*
	 (cons (dp::mk-dp-number modulo) list)))) ;;!!;

(defmacro make-nat-equal (t1 t2)
  `(dp::mk-term `(,*NAT-EQUAL* ,,t1 ,,t2)))

(defmacro make-nat-leq (t1 t2)
  `(dp::mk-term `(,*NAT-<=* ,,t1 ,,t2)))

(defmacro make-nat-leq-from-list (list)
  `(dp::mk-term `(,*NAT-<=* ,@,list)))

(defmacro make-nat-less (t1 t2)
  `(dp::mk-term `(,**NAT-<* ,,t1 ,,t2)))


(defmacro make-nat-less-from-list (list)
  `(dp::mk-term `(,*NAT-<* ,@,list)))

;;; -----------------------
;;; --- Fresh Variables ---
;;; -----------------------

(defvar *bv-fresh-var-counter* 0)

(defun make-fresh-bv-var (len &key (fresh-var-call nil))
  (cond
   ((numberp len)
    (cond
     ((< len 0) (error "make-fresh-bv-var ~a negative size" len))
     ((= len 0) *BV-EPSILON*)
     (t (let* ((name (dp::mk-constant
		      (intern
		       (CLOS::string-append
			"FRESH_"
			(princ-to-string (incf *bv-fresh-var-counter*))))))
	       (var (dp::mk-term `(,*BV-VAR* ,(dp::mk-constant name)
					     ,(dp::mk-dp-number len)))))
	  (ifuncall fresh-var-call var)
	  var))))
   (t (error "make-fresh-bv-var ~a not caught." len))))
	      

;;; ----------------------------------------------------------------------
;;;  Destructors
;;; ----------------------------------------------------------------------

(defun bv-length (l)
  ;; Returns a canonized Term;
  ;; if the argument is a fixed bv term, it will return a natural number
   (cond
    ((rec-bv-epsilon? l) 0)
    ((rec-bv-var? l)
     (bv-var-length l))
    ((rec-bv-const? l)
     (bv-const-length l))
    ((rec-bv-extraction? l)
     (+ 1 (bv-extraction-left l) (* -1 (bv-extraction-right l))))
    ((rec-bv-composition? l)
      (apply #'+ (mapcar #'bv-length (bv-composition-content l))))
    ((rec-bv-bool-apply? l)
     (bv-length (car (bv-bool-args l))))
    ((node-p l)
     (bv-length (node-variable l)))
    ((rec-bv-addition? l)
     (bv-addition-modulo l))
    
    (t (error "bv-length ~a not caught." l))))

;;-

(defun bv-var-name (var)
  (dp::arg 1 var))

(defun bv-var-length (var)
  (dp::constant-id (dp::arg 2 var)))

(defun nat-var-name (var)
  (dp::arg 1 var))


(defun bv-const-value (l)
  (dp::constant-id (dp::arg 1 l)))

(defun bv-const-length (l)
  (dp::constant-id (dp::arg 2 l)))

(defun bv-extraction-length (l)
  (canonize-nat-term 
   (dp::mk-term
    `(,*+* ,dp::*one* ,(bv-extraction-left l)
	   (,*times* ,dp::*neg-one* ,(bv-extraction-right l))))))

(defun bv-extraction-bv (l)
  (dp::arg 1 l))

(defun bv-extraction-left (l)
  (dp::constant-id (dp::arg 1 (dp::arg 2 l))))

(defun bv-extraction-right (l)
  (dp::constant-id (dp::arg 2 (dp::arg 2 l))))

;-

(defun bv-decompose-left (l)
  (dp::arg 1 l))
    
(defun bv-decompose-right (l)
  (let* ((rest (cdr (dp::funargs l)))
	 (len (length rest)))
  (case len
    (0 *BV-EPSILON*)
    (1 (car rest))
    (t (dp::mk-term `(,*BV-COMPOSE* ,@rest))))))

(defmacro bv-composition-content (l) "List of all bvecs in a composition"
  `(dp::funargs ,l))

(defmacro bv-flat-term-content (l)
  `(if (rec-bv-composition? ,l)
       (bv-composition-content ,l)
     (list ,l)))

(defmacro bv-bool-args (bv)
  `(dp::funargs ,bv))

(defmacro bv-recognizer (bv)
  `(dp::funsym ,bv))


(defmacro bv-addition-args (bv)
  `(cdr (dp::funargs ,bv)))

(defmacro bv-addition-modulo (bv)
  `(dp::constant-id (dp::arg 1 ,bv)))


;;; ****************************************
;;;  Browse Functions
;;; ****************************************

(defun all-vars-in-bv-term-unique (bv)
  (remove-duplicates
   (all-vars-in-bv-term bv)
   :TEST 'equal))

(defun all-vars-in-bv-term (bv)
  (cond
   ((rec-bv-const? bv) nil)
   ((rec-bv-var? bv) `(,bv))
   ((rec-bv-extraction? bv) (all-vars-in-bv-term (bv-extraction-bv bv)))
   ((rec-bv-composition? bv)
    (loop for e in (bv-composition-content bv) append
	  (all-vars-in-bv-term e)))
   ((node-p bv)
    (loop for n in (all-bdd-nodes bv) append
	  (all-vars-in-bv-term n)))
   ((rec-bv-bool-apply? bv)
    (loop for n in (cdr bv) append
	  (all-vars-in-bv-term n)))
   (t (error "all-vars-in-bv-term ~a not caught." bv))))
   
;;; ********************
;;;  Dealing Higher-Level Macros
;;; ********************

;;; Iteration

(defmacro rec-bv-var-or-var-extract? (term)
  `(and (dp::application-p ,term)
        (or (eq   (dp::funsym ,term) *BV-VAR*)
	    (and  (eq (dp::funsym ,term) *BV-EXTRACT*)
		  (eq (dp::funsym (bv-extraction-bv ,term)) *BV-VAR*)))))

(defvar *bv-iterate* (dp::mk-constant 'bv-iterate))

(defmacro make-bv-iteration (times bv)
  `(dp::mk-term `(,*BV-ITERATE* ,,times ,,bv)))

(defmacro bv-iteration-times (bv)
  `(dp::arg 1 ,bv))

(defmacro bv-iteration-bv (bv)
  `(dp::arg 2 ,bv))

(defun is-bv-iteration? (bv)
  (and (dp::application-p bv)
       (eq (dp::funsym bv) *BV-ITERATE*)
       (is-nat? (bv-iteration-times bv))
       (is-bv?  (bv-iteration-bv    bv))))

(defun is-bv-epsilon? (bv)
  (eq bv *BV-EPSILON*))

(defun rec-bv-iteration? (bv)
  `(and (dp::application-p ,bv)
	(eq (dp::funsym ,bv) *BV-ITERATE*)))


(defmacro rec-bv-epsilon? (bv)
  `(eq ,bv *BV-EPSILON*))


;;; **************************************************
;;;  Canonizing Natural Terms
;;; **************************************************
;;;
;;; Bring a (linear) term to the form
;;;  <nat> |
;;;  (* <fixed-int> <nat-var>) |
;;;  (+ <(<nat> | (* <fixed-nat> <nat-var>) )+2 )
;;;
;;; 
;;; The Cardinal Variables are assumed to appear in a order given by 
;;;  nat-var-<
;;;
;;; The Canonizer assumes that any argument is canonized as well.

(defun canonize-nat-term (term)
  ;; recursively (!)
  (cond
   ((is-fixed-int? term) term)
   ((dp::dp-numberp term) term)
   (t 
      (let ((op  (if (dp::application-p term) (dp::funsym term)))
	    (arg (if (dp::application-p term) (dp::funargs term))))
     (cond
      ((is-fixed-int? term) term)
      ((rec-nat-var? term) term)
      ((eq op *times*) (canonize-multiplication term))
      ((eq op *+*) (canonize-addition term))
      ((eq op *-*) (canonize-subtraction term))
      ((eq op *expt2*) (canonize-expt2-term term))
      ((eq op */*) (canonize-division-term term))
      ;; special constructs
      ((eq op *NAT-VAR-BETWEEN*)
       (make-nat-between (car arg)
			 (canonize-nat-term (cadr arg))
			 (canonize-nat-term (caddr arg))))
      ((eq op *NAT-MAX*)
       (symbolic-max (mapcar #'canonize-nat-term arg)))
      ((eq op *NAT-MIN*)
       (symbolic-min (mapcar #'canonize-nat-term arg)))
      ((eq op *CS-LIST*)
       (make-cs-list (mapcar #'canonize-nat-term arg)))

      (t (error "canonize-nat-term ~a not caught." term)))))))

(defun canonize-nat-equation (eq)
  #+dbg(assert (rec-nat-equation? eq))
  (make-nat-equal
   dp::*zero*
   (canonize-nat-term 
    (dp::mk-term `(,*-* ,(dp::arg 1 eq) ,(dp::arg 2 eq))))))

(defun canonize-nat-leq (leq)
  #+dbg(assert (rec-nat-leq? leq))
  (let* ((less (canonize-nat-term (dp::arg 1 leq)))
	 (more (canonize-nat-term (dp::arg 2 leq)))
	 (new-more (canonize-subtraction (dp::mk-term `(,*-* ,more ,less))))
	 (new-less dp::*zero*))
    (if  (and (rec-addition? new-more)
	      (rec-fixed-int? (dp::arg 1 new-more)))
	(setf new-less (dp::mk-dp-number
			(- 0 (dp::constant-id (dp::arg 1 new-more))))
	      new-more (let ((nn (cdr (dp::funargs new-more))))
			 (if (= (length nn) 1)
			     (car nn)
			   (dp::mk-term (cons *+* nn))))))
    (dp::mk-term `(,*NAT-<=* ,new-less ,new-more))))


(defun canonize-multiplication (term)
  (let ((fac1 (dp::lhs term))
	(fac2 (canonize-nat-term (dp::rhs term))))
    #+dbg(assert (dp::dp-numberp fac1))
    (cond ((rec-fixed-int? fac2) (dp::mk-dp-number
				  (* (dp::constant-id fac1)
				     (dp::constant-id fac2))))
	  ((dp::dp-zerop fac1) fac1)
	  ((= dp::*one* fac1) fac2)
	  ((and (application-p fac2)
		(eq (dp::funsym fac2) *times*))
	   (dp::mk-term `(,*times* ,(* (dp::constant-id fac1)
				       (dp::constant-id (dp::arg 1 fac2)))
				   ,(dp::arg 2 fac2))))
	  ((and (dp::application-p fac2)
		(eq (dp::funsym fac2) '+)
		(canonize-addition
		 (dp::mk-term `(,*+* ,@(mapcar #'(lambda (x) (dp::mk-term
							      `(,*times* ,fac1 ,x)))
					 (dp::funargs fac2)))))))
	  (t (dp::mk-term `(,*times* ,fac1 ,fac2))))))

(defun dp-+-2 (term1 term2)
  (dp::mk-dp-number (+ (dp::constant-id term1)
		       (dp::constant-id term1))))

(defun dp-+ (&rest args)
  (reduce #'dp-+-2 args :initial-value dp::*zero*))

(defun dp-*-2 (term1 term2)
  (dp::mk-dp-number (* (dp::constant-id term1)
		       (dp::constant-id term1))))

(defun dp-* (&rest args)
 (reduce #'dp-*-2 args :initial-value dp::*one*))

(defun dp-mod (term1 term2)
  (dp::mk-dp-number (mod (dp::constant-id term1)
			 (dp::constant-id term1))))

(defun dp-div (term1 term2)
  (dp::mk-dp-number (div (dp::constant-id term1)
			 (dp::constant-id term1))))

(defun dp-/ (&rest terms)
  (dp::mk-dp-number (apply #'/ (mapcar #'dp::constant-id terms))))

(defun dp-expt2 (term1)
  (dp::mk-dp-number (expt 2 (dp::constant-id term1))))

(defun canonize-addition (term)
  (let* ((cterm (dp::mk-term
		 `(,*+* ,@(mapcar #'canonize-nat-term (dp::funargs term)))))
	 (var-occ (mapcar #'(lambda (x) `(,x . 0))
			    (sort (all-visible-vars-in-nat-term cterm) #'nat-var-<)))
	 (other-occ (mapcar #'(lambda (x) `(,x . 0))
			    (all-other-in-nat-term cterm)))
	 (var-other-occ (nconc var-occ other-occ))
	 (flat-args (loop for e in (dp::funargs cterm) append
			  (cond
			   ((rec-addition? e) (dp::funargs e))
			   ((rec-primitive-multiplication? e) (list (eval e)))
			   (t (list e)))))
	 (int  (apply 'dp-+ (remove-if-not #'dp::dp-numberp flat-args)))
	 (non-int (remove-if #'dp::dp-numberp flat-args))
	 (dummy
	  (loop for e in non-int do
		(cond
		 ((rec-multiplication? e)
		  (incf (cdr (assoc (caddr e) var-other-occ :TEST 'equal))
			(cadr e)))
		 (t (incf (cdr (assoc e var-other-occ :TEST 'equal)))))))
	 (var-other-occ
	  (delete-if #'(lambda (x) (= 0 (cdr x))) var-other-occ))
	 (final-args
	  (append (if (dp::dp-zerop int)
		      nil
		    `(,int))
		  (mapcar #'(lambda (x)
			      (if (= 1 (cdr x))
				  (car x)
				(dp::mk-term `(* ,(dp::mk-dp-number (cdr x))
						 ,(car x)))))
			    var-other-occ))))
    (declare (ignore dummy))
    (case (length final-args)
      (0 dp::*zero*)
      (1 (car final-args))
      (t (dp::mk-term `(,*+* ,@final-args))))))

(defun canonize-subtraction (term)
  (let ((posarg (dp::arg 1 term))
	(negargs (cdr (dp::funargs term))))
    (canonize-addition
     (dp::mk-term
      `(,*+* ,posarg
	     ,@(mapcar #'(lambda (x)
			   (dp::mk-minus x))
		       negargs))))))
    

(defun canonize-mod-term (term)
  (let ((arg1 (canonize-nat-term (dp::arg 1 term)))
	(arg2 (canonize-nat-term (dp::arg 2 term))))
    (cond
     ((and (is-fixed-int? arg1)
	   (is-fixed-int? arg2))
      (dp-mod arg1 arg2))
     (t (dp::mk-term `(,*mod* ,arg1 ,arg2))))))

(defun canonize-div-term (term)
  (let ((arg1 (canonize-nat-term (dp::arg 1 term)))
	(arg2 (canonize-nat-term (dp::arg 2 term))))
    (cond
     ((and (is-fixed-int? arg1)
	   (is-fixed-int? arg2))
      (dp-div arg1 arg2))
     (t (dp::mk-term `(,*div* ,arg1 ,arg2))))))

(defun canonize-expt2-term (term)
  (let ((arg (canonize-nat-term (dp::arg 1 term))))
    (if (is-fixed-posnat? arg)
	(dp-expt2 arg)
      (dp::mk-term `(,*expt2* ,arg)))))

(defun canonize-division-term (term)
  (let ((args (mapcar #'canonize-nat-term (dp::funargs term))))
    (if (every #'dp::dp-numberp args)
	(eval `(dp-/ ,@args))
      (dp::mk-term `(,*/* ,@args)))))

(defun is-int? (l)
  (or (is-fixed-int? l)
      (is-addition? l)
      (is-multiplication? l)
      (is-nat? l)))

(defun is-addition? (term)
  (and (dp::application-p term)
       (eq (dp::funsym term) *+*)
       (every #'is-int? (dp::funargs term))))

(defun is-multiplication? (term)
  (and (dp::application-p term)
       (= (arity term) 2)
       (eq (dp::funsym term) *times*)
       (is-fixed-int? (dp::arg 1 term))
       (is-nat? (dp::arg 2 term))))

(defun is-expt2? (term)
  (and (dp::application-p term)
       (= (arity term) 1)
       (eq (dp::funsym term) *expt2*)
       (is-nat? (dp::arg 1 term))))


(defmacro rec-primitive-multiplication? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *times*)
	(is-fixed-int? (dp::arg 1 ,term))
	(is-fixed-int? (dp::arg 2 ,term))))

(defmacro rec-addition? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *+*)))

(defmacro rec-subtraction? (term)
  `(and (dp::application-p ,term)
        (eq (dp::funsym ,term) *-*)))

(defmacro rec-multiplication? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *times*)))

(defmacro rec-expt2? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *expt2*)))

(defmacro rec-div? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *div*)))

(defmacro rec-mod? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *mod*)))

(defmacro rec-division? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) */*)))

;; BV Equation

(defmacro make-bv-equation (t1 t2)
  `(dp::mk-term `(,*BV-EQUAL* ,,t1 ,,t2)))

(defmacro rec-bv-equation? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *bv-equal*)))

(defmacro bv-equation-args (term)
  `(dp::funargs ,term))

(defun all-other-in-nat-term (term)
  ;; any other terms but + , *
  ;; e.g. (expt2 .) (mod . .) (div . .)
  (cond
   ((is-fixed-int? term) nil)
   ((rec-nat-var? term)  nil)
   ((rec-addition? term)
    (remove-duplicates
     (loop for e in (dp::funargs term) append
	   (all-other-in-nat-term e))
     :TEST 'equal))
   ((rec-multiplication? term)
    (all-other-in-nat-term (dp::arg 2 term)))
   (t `(,term))))

(defun all-visible-vars-in-nat-term (term)
  (cond
  ((dp::dp-numberp term) nil)
   ((rec-nat-var? term) `(,term))
   ((rec-addition? term)
    (remove-duplicates
     (loop for e in (dp::funargs term) append
	   (all-visible-vars-in-nat-term e))
     :TEST 'equal))
   ((rec-multiplication? term)
    (all-visible-vars-in-nat-term (dp::arg 2 term)))
   ((rec-subtraction? term)
    (remove-duplicates
     (loop for e in (dp::funargs term) append
	   (all-visible-vars-in-nat-term e))
     :TEST #'equal))
   ;; Special Constructs
   ((rec-nat-between? term)
    (cons (dp::arg 1 term)
	  (append (all-visible-vars-in-nat-term (dp::arg 2 term))
		  (all-visible-vars-in-nat-term (dp::arg 3 term)))))
   ((or (rec-nat-max? term)
	(rec-nat-min? term))
    (loop for e in (cdr term) append
	   (all-visible-vars-in-nat-term e)))
   ((rec-var-hiding-construct? term)
    nil)
   (t (error "all-visible-vars-in-nat-term ~a not caught." term))))

;; Nat Equations

(defvar *max* (dp::mk-constant 'max))
(defvar *min* (dp::mk-constant 'min))

(defmacro rec-nat-max? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *MAX*)))

(defmacro rec-nat-min? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *MIN*)))

(defmacro rec-nat-between? (term)
  `(and (dp::application-p ,term)
	(eq (dp::funsym ,term) *NAT-VAR-BETWEEN*)))

(defmacro make-nat-min (list)
  `(dp::mk-term `(,*min* ,@,list)))

(defmacro make-nat-max (list)
  `(dp::mk-term `(,*max* ,@,list)))

(defmacro make-free-list (list)
  `(dp::mk-term `(,*free-list* ,@,list)))

(defmacro rec-free-list? (list)
  `(and (dp::application-p ,list)
	(eq (dp::funsym ,list) 'FREE-LIST)))

(defun is-free-list? (list)
  (and (dp::application-p list)
       (eq (dp::funsym list) 'FREE-LIST)))

;;; --------------------------------------------------
;;;  Auxiliary Functions & Macros
;;; --------------------------------------------------

(defmacro ifuncall (function &rest args)
  `(if ,function
       (funcall ,function ,@args)))

(defmacro ninsert-list (seq a list)
  ;; replace destructively the element a of seq with list
  `(let ((pos (position ,a ,seq :TEST #'equal)))
     (if (null pos)
	 (error "ninsert-list ~a not a member." a)
       (setf ,seq
	 (append (subseq ,seq 0 pos)
		 ,list
		 (subseq ,seq (1+ pos)))))))

    
(defun nat2bools (n &optional (nbits nil))
  (let ((res nil))
    (if nbits
	(loop for i from 0 to (1- nbits) do
	      (if (= 1 (mod n 2))
		  (push t res)
		(push nil res))
	      (setf n (div n 2)))
      (loop while (> n 0) do
	    (if (= 1 (mod n 2))
		(push t res)
	      (push nil res))
	    (setf n (div n 2))))
    res))
  

;;; ****************************************
;;;  Dealing with BV-BDDs
;;; ****************************************


(defstruct node
  (variable    nil) ;; contains also leaf-values...
  (label       -1  :TYPE integer)
  (leaf-node?  nil)
  (else        nil)
  (then        nil)
  (mark        nil)
  )      

(defun is-bdd? (term)
  (node-p term))

(defmacro rec-bdd? (term)
  `(node-p ,term))


;;; Functions on BDDs

(defun all-bdd-nodes (node)
  (vars-of-node node))

(defun lift-bdd-if (bdd)
  ;; turn a BDD into a flat term, if possible
  (if (node-p bdd)
      (cond
       ((leaf-node? bdd) (node-variable bdd))
       ((and (true-node? (node-then bdd))
	     (false-node? (node-else bdd)))
	(node-variable bdd))
       (t bdd)))
  bdd)

;;; ----------------------------------------

(defmacro numposp (n) "[positive-number-predicate] Is n an integer >=0?"
  `(and ,`(integerp ,n)
        ,`(>= ,n 0)))

(defmacro numneqp (n) "[postitive-non-zero-predicate] Is n an integer >0?"
  `(and ,`(integerp ,n) 
        ,`(> ,n 0)))

(defun nat-var-< (a b)
  (if (rec-nat-var? a)
      (if (rec-nat-var? b)
	  (string< (nat-var-name a)
		   (nat-var-name b))
	nil)
    (if (rec-nat-var? b)
	t
      (dp::arith-term-< a b))))

(defun nat-term-< (a b)
  (let ((a-l (nat-term-level a))
	(b-l (nat-term-level b)))
    (cond
     ((< a-l b-l) t)
     ((> a-l b-l) nil)
     ((dp::dp-numberp a)
      (< (dp::constant-id a) (dp::constant-id b)))
     ((rec-nat-var? a)
      (nat-var-< a b))
     (t
      (some #'(lambda (x) (nat-term-< (car x) (cdr x)))
	    (reverse (pairlis-pad (cdr a) (cdr b))))))))

(defun nat-term-level (term)
  (cond
   ((null term)    0)
   ((dp::dp-numberp term) 1)
   ((rec-nat-var? term) 2)
   ((rec-addition? term) 3)
   ((rec-subtraction? term) 4)
   ((rec-multiplication? term) 5)
   ((rec-mod? term) 6)
   ((rec-div? term) 7)
   ((rec-expt2? term) 8)
   (t (error "nat-term-level ~a not caught." term))))


;;; General AUX Functions

(defun div (a b)
  (/ (- a (mod a b)) b))

(defun pairlis-pad (a b)
  ;(ifassert (listp a))
  ;(ifassert (listp b))
  ;; like pairlis, but pads missing elements with nil at the list end
  (let* ((na (length a))
	 (nb (length b))
	 (n  (max na nb))
	 (pada (loop for i from 1 to (- n na) collect nil))
	 (padb (loop for i from 1 to (- n nb) collect nil)))
    (pairlis (nconc a pada) (nconc b padb))))

(defun ziplis (args)
  ;; similar to pairlis, but turns m lists of n elements
  ;; into n lists of m elements
  ;(ifassert (consp args))
  (let* ((len (length (car args)))
	 (n (length args))
	 (res (make-list len)))
    (loop for i from 0 to (1- len) do
	  (loop for j downfrom (1- n) to 0 do
		(push (nth i (nth j args))
		      (nth i res))))
    res))


(defun count-down (n) "Creates a n-ary list, starting with n-1"
  (let ((erg nil))
    (dotimes (i n erg) (push i erg))))

(defun count-up (n)
  (loop for i from 0 to (1- n)
	collect i))
