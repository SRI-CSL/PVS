;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreted-node-structures.lisp -- 
;; Author          : David Cyrluk, Harald Ruess
;; Created On      : 
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dp3)

(defvar *signature* nil)

(defun initial-term-hash ()
  (clrhash *term-hash*)
  (loop for symbol in *signature*
	do (setf (gethash symbol *term-hash*) symbol)))

(defmacro defsymbol (name &key arity
			       (kind :interpreted)
			       initial-type
			       constructor recognizer)
  (let ((varsym (intern (format nil "*~a*" name)))
	(constsym (or constructor (intern (format nil "MK-~a" name))))
	(recsym (or recognizer (intern (format nil "~a-P" name))))
	(const (gensym)))
    `(progn
        (when (boundp ',varsym)
	   (error "Defsymbol: ~a already in use" ,varsym))
	(let ((,const (mk-constant ',name ,initial-type)))
	  (when (and (eq ,kind :interpreted)
		     (not (eq ,arity :constant)))
	    (setf (node-interpreted? ,const) t))
	  (defvar ,varsym ,const)
	  (push ,varsym *signature*))
	(cond ((eq ,arity :constant)
	       (proclaim '(inline ,recsym))
	       (defun ,recsym (trm)
		 (declare (type node trm) (special ,varsym))
		 (eq trm (the constant ,varsym))))
	      (t (proclaim '(inline ,recsym))
		 (defun ,recsym (trm)
		   (declare (type node trm) (special ,varsym))
		   (and (application-p trm)
			(eq (funsym (the node trm))
			    (the constant ,varsym)))) 
		 (proclaim '(inline ,constsym))
		 (cond ((eq ,arity :unary)
			(defun ,constsym (trm)
			  (declare (type node trm) (special ,varsym))
			  (mk-term (list ,varsym trm))))
		       ((eq ,arity :binary)
			(defun ,constsym (trm1 trm2)
			  (declare (type node trm1) (type node trm2)
                                   (special ,varsym))
			  (mk-term (list ,varsym trm1 trm2))))
		       (t (defun ,constsym (trms)
			    (declare (type list trms) (special ,varsym))
			    (mk-term (cons ,varsym trms))))))))))

;; Miscellaneous

(defmacro interpsym? (sym)
  `(node-interpreted? ,sym))

(defun interp? (term)
  (declare (type node term))
  (or (true-p term)
      (false-p term)
      (dp-numberp term)
      (and (application-p term)
	   (or (interpsym? (funsym term))
	       (and (application-p (funsym term))
		    (interpsym? (funsym (funsym term))))))
      (applyupdate-p term)))

(defmacro uninterp? (term)
  `(not (interp? ,term)))

(defun occurs-under-interp (trm1 trm2)
  (declare (type node trm1)
	   (type node trm2))
  (cond ((eq trm1 trm2) t)
	((constant-p trm2) nil)
	((uninterp? trm2) nil)
	(t (some #'(lambda (x)
		     (occurs-under-interp trm1 x))
		 (funargs trm2)))))

(defun occurs-under-funsym (lhs rhs funsym)
  (cond ((constant-p rhs) nil)
	((uninterp? rhs) nil)
	((and (eq (funsym rhs) funsym)
	      (find lhs (funargs rhs) :test #'eq)) t)
	(t (some #'(lambda (x)
		     (occurs-under-funsym lhs x funsym))
		 (funargs rhs)))))

(defun occurs-in-scope-of-uninterp-p (x trm)
  (and (application-p trm)
       (if (or (node-interpreted? (funsym trm))
	       (equality-p trm))
	   (some #'(lambda (arg)
		     (occurs-in-scope-of-uninterp-p x arg))
		 (application-arguments trm))
         (some #'(lambda (arg)
	           (occurs-p x arg))
	       (application-arguments trm)))))

;; Equality

(defsymbol = :arity :binary
             :initial-type 'predicate
             :constructor mk-equality
	     :recognizer equality-p)

;; Booleans

(defsymbol true    :arity :constant :initial-type :predicate)
(defsymbol false   :arity :constant :initial-type :predicate)
(defsymbol not     :initial-type 'predicate
                   :arity :unary
                   :recognizer negation-p
	           :constructor mk-negation)
(defsymbol and     :initial-type 'predicate
                   :arity :binary
                   :recognizer conjunction-p
	           :constructor mk-conjunction
(defsymbol or      :initial-type 'predicate
                   :arity :binary
                   :recognizer disjunction-p
	           :constructor mk-disjunction)
(defsymbol implies :initial-type 'predicate
                   :arity :binary
                   :recognizer implication-p
		   :constructor mk-implication)
(defsymbol nequal  :initial-type 'predicate
                   :arity :binary)

(defmacro true-or-false-p (trm)
  `(or (true-p (the node ,trm))
       (false-p (the node ,trm))))

(defun mk-conjunction* (trms &optional (acc *true*))
  (declare (type list trms))
  #+dbg(assert (every #'node-p trms))
  (if (null trms) acc
      (let ((trm (car trms)))
	(cond ((true-p trm)
	       (mk-conjunction* (cdr trms) acc))
	      ((false-p trm)
	       *false*)
	      (t (mk-conjunction* (cdr trms)
				  (mk-conjunction trm acc)))))))

(defun mk-disjunction* (trms &optional (acc *false*))
  (declare (type list trms))
  #+dbg(assert (every #'node-p trms))
  (if (null trms) acc
      (let ((trm (car trms)))
	(cond ((false-p trm)
	       (mk-disjunction* (cdr trms) acc))
	      ((true-p trm)
	       *true*)
	      (t (mk-disjunction* (cdr trms)
				  (mk-disjunction trm acc)))))))

(defun bool-p (term)
  (declare (type node term))
  (if (application-p term)
      (let ((kind (node-initial-type (the node (funsym (the application term))))))
	(or (eq kind 'predicate)
	    (eq kind 'arith-pred)))
      (or (true-or-false-p term)
	  (eq (node-initial-type term) *boolean*))))

(defun simplified-negation (fml)
  #+dbg(assert (node-p fml))
  (cond ((true-p fml)
	 *false*)
	((false-p fml)
	 *true*)
	((equality-p fml)
	 (mk-disequality (lhs fml) (rhs fml)))
	((disequality-p fml) ; test before negation
	 (mk-equality (lhs fml) (rhs fml)))
	((negation-p fml)
	 (arg 1 fml))
	((less-p fml)
	 (mk-greatereq (lhs fml) (rhs fml)))
	((greatereq-p fml)
	 (mk-less (lhs fml) (rhs fml)))
	((greater-p fml)
	 (mk-lesseq (lhs fml) (rhs fml)))
	((lesseq-p fml)
	 (mk-greater (lhs fml) (rhs fml)))
	(t (mk-negation fml))))

(defun simplified-disjunction (lhs rhs)
  (cond ((or (true-p lhs) (true-p rhs))
	 *true*)
	((false-p lhs)
	 rhs)
	((false-p rhs)
	 lhs)
	((and (negation-p lhs) (eq (arg 1 lhs) rhs))
	 *true*)
	((and (negation-p rhs) (eq (arg 1 rhs) lhs))
	 *true*)
	((eq lhs rhs)
	 rhs)
	(t (mk-disjunction lhs rhs))))

(defun simplified-conjunction (lhs rhs)
  (cond ((or (false-p lhs) (false-p rhs))
	 *false*)
	((true-p lhs)
	 rhs)
	((true-p rhs)
	 lhs)
	((and (negation-p lhs) (eq (arg 1 lhs) rhs))
	 *false*)
	((and (negation-p rhs) (eq (arg 1 rhs) lhs))
	 *false*)
	((eq lhs rhs)
	 lhs)
	(t (mk-conjunction lhs rhs))))

(defun simplified-if (condition then-part else-part)
  (cond ((true-p condition) then-part)
	((false-p condition) else-part)
	((eq then-part else-part) then-part)
	(t (mk-if-then-else condition then-part else-part))))


;; Disequality (encoded as not(t1 = t2))


(defun mk-disequality (trm1 trm2)
  (declare (type node trm1)
	   (type node trm2))
  (mk-equality (mk-equality trm1 trm2) *false*))

(defun disequality-p (trm)
  (declare (type node trm))
  (and (equality-p trm)
       (false-p (rhs trm))
       (equality-p (lhs trm))))

(defun destructure-disequality (trm)
  (declare (type node trm))
  #+dbg(assert (disequality-p trm))
  (let ((arg (lhs trm)))
    (values (the node (lhs arg))
	    (the node (rhs arg)))))

(defmacro neq-p (term)
  `(disequality-p ,term))

;; Conditional

(defsymbol if :constructor mk-if-then-else
              :recognizer if-p)


(defun ite-p (trm)
  (if-p trm))

(defun if-cond (trm)
  (declare (type node trm))
  (arg 1 trm))

(defun if-then (trm)
  (declare (type node trm))
  (arg 2 trm))

(defun if-else (trm)
  (declare (type node trm))
  (arg 3 trm))

(defun destructure-ite (trm)
  (declare (type node term))
  (values (arg 1 trm) (arg 2 trm) (arg 3 trm)))

;; Arithmetic

(defsymbol plus :initial-type 'arith-op)
(defsymbol times :initial-type 'arith-op)
(defsymbol divide :initial-type 'arith-op :arity :binary)
(defsymbol minus :initial-type 'arith-op :arity :unary)
(defsymbol difference :initial-type 'arith-op :arity :binary)

(defsymbol sup     :kind :uninterpreted :initial-type 'arith-op)
(defsymbol inf     :kind :uninterpreted :initial-type 'arith-op)
(defsymbol floor   :kind :uninterpreted :initial-type 'arith-op)
(defsymbol ceiling :kind :uninterpreted :initial-type 'arith-op)
(defsymbol mod     :kind :uninterpreted :initial-type 'arith-op)
(defsymbol expt    :kind :uninterpreted :initial-type 'arith-op)

(defun arith-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'arith-op))

(defmacro coef (times) `(arg 1 ,times))
(defmacro numer (divide) `(arg 1 ,divide))
(defmacro denom (divide) `(arg 2 ,divide))

(defun square-p (var)
  (and (times-p var)
       (= (length (funargs var)) 2)
       (eq (arg 1 var) (arg 2 var))))

(defun plus-denoms (trm)
  (map-funargs-list #'number-term-denominator trm))

(defun number-term-denominator (trm)
  (denominator (number-term-coef trm)))

(defun number-term-coef (trm)
  (constant-id (term-coef trm)))

(defun plus-coefs (trm)
  (map-funargs-list #'number-term-coef trm))

(defsymbol lessp      :initial-type 'arith-pred
                      :arity :binary
                      :constructor mk-less
		      :recognizer less-p)
(defsymbol lesseqp    :initial-type 'arith-pred
                      :arity :binary
                      :constructor mk-lesseq
		      :recognizer lesseq-p)
(defsymbol greaterp   :initial-type 'arith-pred
                      :arity :binary
                      :constructor mk-greater
		      :recognizer greater-p)
(defsymbol greatereqp :initial-type 'arith-pred
                      :arity :binary
                      :constructor mk-greatereq
		      :recognizer greatereq-p)

(defun strict-ineq-p (trm)
  (declare (type node trm))
  (or (less-p trm) (greater-p trm)))

(defun nonstrict-ineq-p (trm)
  (declare (type node trm))
  (or (lesseq-p trm) (greatereq-p trm)))

(defun ineq-strict? (ineq)
  "Recognizes strict inequalities."
  (member (funsym ineq) (list *lessp* *greaterp*)))

(defun ineqfun-p (fun)
  "is fun an inequality symbol."
  (eq (node-initial-type fun) 'arith-pred))

(defun mk-ineq (ineq-pred arg1 arg2)
  "Builds inequality"
  (declare (type constant ineq-pred)
	   (type node arg1)
	   (type node arg2))
  (mk-term `(,ineq-pred ,arg1 ,arg2)))

(defun ineq-p (ineq)
  "Recognizer for inequalities"
  (and (application-p ineq)
       (ineqfun-p (funsym ineq))))

(defsymbol integer-pred  :kind :uninterpreted
                         :initial-type 'predicate)
(defsymbol rational-pred :kind :uninterpreted
                         :initial-type 'predicate)
(defsymbol number-pred   :kind :uninterpreted
                         :initial-type 'predicate)

(defun arith-p (term)
  "Is the top fun. symb. of term arithmetic."
  (declare (type node term))
  (cond ((application-p term)
	 (arith-op-p (the node (funsym (the application term)))))
	((leaf-p term)
	 (or (numberp (constant-id (the constant term)))
	     (eq (node-initial-type term)  *integer*)
	     (eq (node-initial-type term) *number*)))
	(t nil)))

(defun number-type-p (trm)
  (declare (type node trm))
  (and (application-p trm)
       (member (node-initial-type (funsym trm))
	     '(integer-pred rational-pred number-pred))))

(defun arith-pred-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'arith-pred))

(defun arith-bool-p (term)
  (declare (type node term))
  (and (application-p term)
       (arith-pred-p (the node (funsym (the application term))))))

(defun dp-numberp (term)
  (declare (type node term))
  (and (constant-p term)
       (numberp (constant-id (the constant term)))))

(defun dp-integerp (term)
  (declare (type node term))
  (and (constant-p term)
       (integerp (constant-id (the constant term)))))

(defmacro integer-p (trm)
  `(dp-integerp ,trm))

(defun number-p (trm)
 (let ((kind (node-initial-type trm)))
   (or (eq kind *integer*)
       (eq kind *number*))))

(defun integer-p (trm)
  (eq (node-initial-type trm) *integer*))
	
(defun mk-dp-number (num)
  (declare (type rational num))
  (mk-constant num))

(defsymbol 0 :arity :constant :initial-type *number* :recognizer zero-p)
(defsymbol 1  :arity :constant :initial-type *number* :recognizer one-p)
(defsymbol -1 :arity :constant :initial-type *number* :recognizer negone-p)

(defvar *zero* *0*)
(defvar *one* *1*)
(defvar *negone* *-1*)

(defsymbol epsilon :arity :constant :initial-type 'strict) ; infinitesimally small

(defmacro dp-minusp (const)
  `(minusp (constant-id (the constant ,const))))

(defun dp-rational-atom-p (term cs)
  (dp-integer-atom-p term cs))

(defun dp-real-atom-p (trm)
  (declare (ignore trm))
  t)

(let ((*arith-types*  (list *integer* *number*)))
  (defun number-theory-p (trm cs)
    (or (arith-p trm)
	(member (dp-type trm cs) *arith-types*))))
	    
(defun number-equality-p (eqn cs)
  (and (equality-p eqn)
       (number-theory-p (lhs eqn) cs)
       (number-theory-p (rhs eqn) cs)))

(defun num+ (t1 t2)
  (if (and (dp-number-p t1) (dp-number-p t2))
      (mk-constant (+ (constant-id t1) (constant-id t2)))
      (sigplus (mk-plus (list t1 t2)))))

(defun num* (t1 t2)
  (if (and (dp-numberp t1) (dp-numberp t2))
      (mk-constant (* (constant-id t1) (constant-id t2)))
      (sigtimes (mk-times (list t1 t2)))))

(defun num1+ (trm)
  (if (dp-number-p trm)
      (mk-constant (1+ (constant-id trm)))
      (sigplus (mk-plus (list trm *one*)))))

(defun num1- (trm)
  (if (dp-number-p trm)
      (mk-constant (1- (constant-id trm)))
      (sigplus (mk-difference (list trm *one*)))))

(defun num- (t1 t2)
  (if (and (dp-number-p t1) (dp-number-p t2))
      (mk-constant (- (constant-id t1) (constant-id t2)))
      (sigdifference (mk-difference (list t1 t2)))))


(defun num-div (t1 t2)
  (if (and (dp-numberp t1) (dp-numberp t2))
      (mk-constant (floor (/ (constant-id t1) (constant-id t2))))
      (mk-div t1 t2)))

(defun num-mod (t1 t2)
  (if (and (dp-numberp t1) (dp-numberp t2))
      (mk-constant (mod (constant-id t1) (constant-id t2)))
      (mk-mod t1 t2)))

(defun num-expt (t1 t2)
  (if (and (dp-numberp t1) (dp-number t2))
      (mk-constant (expt (constant-id t1) (constant-id t2)))
      (mk-expt t1 t2)))

(defun num= (t1 t2)
  (and (dp-number-p t1)
       (dp-number-p t2)
       (= (constant-id t1) (constant-id t2))))

(defun num< (t1 t2)
  (and (dp-number-p t1)
       (dp-number-p t2)
       (< (constant-id t1) (constant-id t2))))

(defun num> (t1 t2)
  (and (dp-number-p t1)
       (dp-number-p t2)
       (> (constant-id t1) (constant-id t2))))

(defun num<= (t1 t2)
  (and (dp-number-p t1)
       (dp-number-p t2)
       (<= (constant-id t1) (constant-id t2))))

;; Arrays

(defsymbol update :initial-type 'array-op)

(defun array-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'array-op))


;; Tuples

(defsymbol tuple :initial-type 'tuple-op)

(defun tuple-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'tuple-op))


;; Records

(defsymbol record  :initial-type 'record-op)
(defsymbol project :initial-type 'record-op)
(defsymbol th-app  :initial-type 'th-app-op)

(defun record-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'record-op))

(defun project-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'project-op))

(defun th-app-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'th-app-op))


;; Higher-Order Constants

(defsymbol lambda)
(defsymbol forall)
(defsymbol exists)

(defun mk-lambda* (vars trm)
  (mk-term (cons *lambda* (append vars (list trm)))))

(defun mk-exists* (vars trm)
  (mk-term (cons *exists* (append vars (list trm)))))

(defun mk-forall* (vars trm)
  (mk-term (cons *forall* (append vars (list trm)))))

(defun destructure-bndng (trm)
  (declare (type application trm))
  (let* ((args (funargs trm))
	 (vars (butlast args))
	 (body (car (last args))))
    #+dbg(assert (every #'dp-variable-p vars))
    #+dbg(assert (node-p body))
    (values vars body)))


;; Sets

(defsymbol emptyset     :initial-type 'set-op :arity :constant)
(defsymbol fullset      :initial-type 'set-op :arity :constant)
(defsymbol union        :initial-type 'set-op :arity :binary)
(defsymbol intersection :initial-type 'set-op :arity :binary)
(defsymbol complement   :initial-type 'set-op :arity :binary)

(defun set-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'set-op))

(defsymbol subset :initial-type 'predicate)


;; Bit-vectors

(defsymbol bv-const :initial-type 'bv-operator :kind :uninterpreted :arity :binary)
(defsymbol bv-zero  :initial-type 'bv-operator :kind :uninterpreted :arity :unary)
(defsymbol bv-one   :initial-type 'bv-operator :kind :uninterpreted :arity :unary)
(defsymbol bv-epsilon :initial-type 'bv-operator :arity :constant)
(defsymbol bv-concat :initial-type 'bv-operator :arity :binary)
(defsymbol bv-extract :initial-type 'bv-operator)
(defsymbol bv-and :initial-type 'bv-operator :arity :binary)
(defsymbol bv-or :initial-type 'bv-operator :arity :binary)
(defsymbol bv-xor :initial-type 'bv-operator :arity :binary)

(defsymbol nat2bv :kind :uninterpreted)
(defsymbol bv2nat :kind :uninterpreted)

(defun bv-op-p (constant)
  (declare (type node constant))
  (eq (node-initial-type constant) 'bv-operator))

(defun destructure-bv-extract (bv)
  "Destructure an extraction x[i, j] into pieces.
   Notice that i represent the lower bound and j
   the upper bound."
  #+dbg(assert (bv-extract-p bv))
  (values (arg 1 bv) (arg 3 bv) (arg 2 bv)))

(defun destructure-bv-constant (bv)
  (declare (type node bv))
  #+dbg(assert (bv-constant-p bv))
  (values (arg 1 bv) (arg 2 bv)))


(defun destructure-bv-concat (bv)
  (if (bv-concat-p bv)
      (values (lhs bv) (rhs bv))
      (values bv *bv-epsilon*)))

(let ((*bv-bitwise*
       (list *bv-not* *bv-and* *bv-or* *bv-xor* *bv-ite* *bv-iff*)))
  (defun bv-bitwise-p (bv)
    (declare (type node bv))
    (and (application-p bv)
	 (memq (funsym bv) *bv-bitwise*))))

;; Regular Expressions

(defsymbol regexp-epsilon :initial-type 'regexp-op :arity :constant)
(defsymbol regexp-conc :initial-type 'regexp-op :arity :binary)
(defsymbol regexp-union :initial-type 'regexp-op :arity :binary)
(defsymbol regexp-intersection :initial-type 'regexp-op :arity :binary)
(defsymbol regexp-complement :initial-type 'regexp-op :arity :binary)
(defsymbol regexp-star :initial-type 'regexp-op :arity :binary)
