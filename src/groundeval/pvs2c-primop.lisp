;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                PVS to C translator
;;
;;     Author: Gaspard ferey
;;
;;  -> https://github.com/Gaspi/pvs2c.git
;;  -> Please read  "main.lisp"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Functions handling translation of primitive function calls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defparameter *C-primitives* '(Eq Neq 1 0 pvsImplies pvsImplies == && && || 
 ! pvsWhen pvsIff pvsAdd pvsSub pvsTimes pvsDiv pvsNumberFieldPred < <= >
 >= pvsRealPred pvsIntegerPred pvsIntegerPred pvsRationalsPred pvsFloor pvsCeiling
 rem pvsNDiv isEven isOdd pvsCons hd tl isCons null isNull pvsRestrict
 length isMember pvsNth pvsAppend reverse))
(defparameter *pvsC-primitives-map*
  (pairlis *pvs2cl-primitives* *C-primitives*))
(defun pvs2C-primitive-op (name)
  (let ((entry (assoc name *pvsC-primitives-map* :test #'same-primitive?)))
    (when entry (cdr entry))))


;; Deprecated
;; (defun mk-C-funcall (fun args)
;;   (Cfuncall fun  (if (listp args) args (list args))))


;; Calling a mpz function (first argument is result)
(defun set-C-pointer (function args type)
  (list (Cfuncall-mp function
		     (cons (C-var type) (if (listp args) args (list args)))
		     type)))



;; ------ Primitive constant (now only boolean) --------
(defun pvs2C*-primitive-cste (expr bindings livevars)
  (let ((nop (pvs2C-primitive-op expr)))
    (if (boolean-primitive? nop)
	(pvs2C*-boolean-primitive nop)
      (C-expr (C-var *C-type*)  ;; If unimplemented ...
	      (format nil "[set](~~a, ~a);" nop)))))

(defun boolean-primitive? (name)
  (member name '(1 0)))
(defun pvs2C*-boolean-primitive (op)
  (C-expr (C-var *C-int* op t)))


(defun rem-application? (operator)
  (and (application? operator)
       (name-expr? (operator operator))
       (eql 'rem (pvs2C-primitive-op (operator operator)))))

;; --------- Primitive function call ----------------
(defun pvs2C*-primitive-app (expr bindings livevars)
  (let* ((op (pvs2C-primitive-op (operator expr)))
	 (args (arguments expr))
	 (type-args (pvs2C-type args))
	 (type-res  (pvs2C-type expr)))
    (cond ((equality-function? op args)
	     (pvs2C*-equality type-args args bindings livevars))
	  ((boolean-function? op)
	     (pvs2C*-boolean op type-args args bindings livevars))
	  ((negation-function? op args)
	     (pvs2C*-negation   type-args args bindings livevars))
	  ((add-function? op args)
	     (pvs2C*-add (car type-args) (cadr type-args) type-res args bindings livevars))
	  ((sub-function? op args)
	     (pvs2C*-sub (car type-args) (cadr type-args) type-res args bindings livevars))
	  ((times-function? op args)
	     (pvs2C*-times (car type-args) (cadr type-args) type-res args bindings livevars))
	  ((div-function? op args)
	     (pvs2C*-div (car type-args) (cadr type-args) type-res args bindings livevars))
	  ((rem-function? op)
	   (break "Do not use rem as a closure yet...")
	   (pvs2C*-rem (car type-args) args bindings livevars))  ;; to finish
	  (t
	   (break "Unknown primitive app")
	   (cons (pvs2C-type expr) ;; This is not even implemented correctly
		 (set-C-pointer op (pvs2C args bindings livevars type-args)))))))


(defun Cprocess (info args bindings livevars)
  (let* ((func (cdr info))  ;; This should be an instance of Cfuncall(-mpz) with a type
	 (type (type func))
	 (gmp? (Cfuncall-mp? func))
	 (C-args (pvs2C args bindings livevars (car info)))
	 (res (set-arguments func
			     (append (when gmp? (list (C-var type)))
				     (var C-args)))))
    (set-var   C-args (if gmp? (C-var type) res))
    (when gmp? (app-instr C-args res))
    C-args))


;; ------------- Boolean functions (2 arguments - number or bool / result bool) ----------
(defun boolean-function? (name)
  (member name '(== && || ! pvsWhen pvsImplies pvsIff
		    pvsNumberFieldPred < <= > >= 
		    pvsRealPred pvsIntegerPred pvsIntegerPred
		    pvsRationalsPred isEven isOdd isCons isNull isMember )))
(defun pvs2C*-boolean (op type-args args bindings livevars)
  (cond ((infix-primitive? op)
	 (pvs2C*-infix-primitive op args bindings livevars))
	((comparison-function? op)
	 (pvs2C*-comparison op type-args args bindings livevars))
	(t
	 (Cprocess (cons type-args (Cfuncall op nil *C-int*))
		   args bindings livevars))))


;; ------------ boolean operator (arguments int / result int ) ---------------
(defun infix-primitive? (name)
  (member name '(== && ||)))
(defun pvs2C*-infix-primitive (op args bindings livevars)
  (Cprocess (cons (list *C-int* *C-int*)
		  (Cfuncall (binop op) nil *C-int*))
	    args bindings livevars))

;; -------- Equality (2 arguments - could be anything / result int) --------
(defun equality-function? (name args)
  (and (eq name 'Eq) (= (length args) 2)))
(defun pvs2C*-equality (type-args args bindings livevars)
  (Cprocess (pvs2C-eq (car type-args) (cadr type-args))
	    args bindings livevars))
(defun pvs2C-eq (typeA typeB)
  (cond ((or (C-mpq? typeA) (C-mpq? typeB))
	 (cons (list *C-mpq* *C-mpq*) (Cfuncall (mp-cmp "mpq" '==) nil *C-int*)))
	((or (C-mpz? typeA) (C-mpz? typeB))
	 (cons (list *C-mpz* *C-mpz*) (Cfuncall (mp-cmp "mpz" '==) nil *C-int*)))
	((and (C-base? typeA) (C-base? typeB))
	 (cons (list typeA typeB) (Cfuncall (binop '==) nil *C-int*)))
	(t
	 (cons (list typeA typeB) (Cfuncall "pvsCmp" nil *C-int*)))))

;; -------- Comparison (2 arguments - numbers / result int) ---------------
(defun comparison-function? (name)
  (member name '(< <= > >=)))
(defun pvs2C*-comparison (op type-args args bindings livevars)
  (Cprocess (pvs2C-comp (car type-args) (cadr type-args) op)
	    args bindings livevars))
(defun pvs2C-comp (typeA typeB op)
  (cond ((or (C-mpq? typeA) (C-mpq? typeB))
	 (cons (list *C-mpq* *C-mpq*) (Cfuncall (mp-cmp "mpq" op) nil *C-int*)))
	((or (C-mpz? typeA) (C-mpz? typeB))
	 (cons (list *C-mpz* *C-mpz*) (Cfuncall (mp-cmp "mpz" op) nil *C-int*)))
	((and (C-base? typeA) (C-base? typeB))
	 (cons (list typeA typeB) (Cfuncall (binop op) nil *C-int*)))
	(t
	 (cons (list typeA typeB) (Cfuncall (mp-cmp "pvs" op) nil *C-int*)))))


;; ------------- Negation (1 argument - number / result number) ----------
(defun negation-function? (name args)
  (and (eq name 'pvsSub) (= (length args) 1)))
(defun pvs2C*-negation (type arg bindings livevars)
  (Cprocess (pvs2C-neg (car type))
	    arg bindings livevars))
(defun pvs2C-neg (type)
  (cond ((C-int? type)
	 (cons (list *C-int*) (Cfuncall *pvs-neg* nil *C-int*)))
	((or (C-uli? type) (C-mpz? type))
	 (cons (list *C-mpz*) (Cfuncall-mp "mpz_neg" nil *C-mpz*)))
	((C-mpq?)
	 (cons (list *C-mpq*) (Cfuncall-mp "mpq_neg" nil *C-mpq*)))
	(t
	 (cons (list type) (Cfuncall "pvsNeg" nil type)))))



;; -------- Arithmetic (2 arguments - numbers / result number) ---------------

;; ---------- Addition ------------
(defun add-function? (op args)
  (and (eq op 'pvsAdd) (eq (length args) 2)))
(defun pvs2C*-add (typeA typeB typeR args bindings livevars)
  (let ((info (pvs2C-add typeR typeA typeB args)))
    (Cprocess (cdr info) (car info) bindings livevars)))
(defun pvs2C-add (typeR typeA typeB args)
  (cond ((or (C-mpq? typeA) (C-mpq? typeB))
	 (cons args (cons
	       (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_add" nil *C-mpq*))))
	((or (C-mpz? typeA) (C-mpz? typeA) (C-mpz? typeR))
	 (cond ((C-unsignedlong-type? (cadr args))
		(cons args (cons
		      (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_add_ui" nil *C-mpz*))))
	       ((C-unsignedlong-type? (car args))
		(cons (reverse args) (cons
		      (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_add_ui" nil *C-mpz*))))
	       (t
		(cons args (cons
		      (list *C-mpz* *C-mpz*) (Cfuncall-mp "mpz_add" nil *C-mpz*))))))
	((or (C-int? typeR) (C-uli? typeR))
	 (cons args (cons
	       (list typeA typeB) (Cfuncall (binop '+) nil typeR))))
	(t
	 (cons args (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_add" nil *C-mpq*))))))


;; ---------- Substraction ------------
(defun sub-function? (op args)
  (and (eq op 'pvsSub) (eq (length args) 2)))
(defun pvs2C*-sub (typeA typeB typeR args bindings livevars)
  (Cprocess (pvs2C-sub typeR typeA typeB args)
	    args bindings livevars))
(defun pvs2C-sub (typeR typeA typeB args)
  (cond ((every #'C-base? (list typeA typeB typeR))
	 (cons (list typeA typeB) (Cfuncall (binop '-) nil typeR)))
	((and (C-integer? typeB) (C-unsignedlong-type? (car args)))
	 (cons (list *C-uli* *C-mpz*) (Cfuncall-mp "mpz_ui_sub" nil *C-mpz*)))
	((and (C-integer? typeA) (C-unsignedlong-type? (cadr args)))
	 (cons (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_sub_ui" nil *C-mpz*)))
	((and (C-integer? typeA) (C-integer? typeB))
	 (cons (list *C-mpz* *C-mpz*) (Cfuncall-mp "mpz_sub" nil *C-mpz*)))
	(t
	 (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_sub" nil *C-mpq*)))))


;; ---------- Multiplication ------------
(defun times-function? (op args)
  (and (eq op 'pvsTimes) (eq (length args) 2)))
(defun pvs2C*-times (typeA typeB typeR args bindings livevars)
  (let ((info (pvs2C-times typeR typeA typeB args)))
    (Cprocess (cdr info) (car info) bindings livevars)))
(defun pvs2C-times (typeR typeA typeB args)
  (cond ((or (C-mpq? typeA) (C-mpq? typeB))
	 (cons args (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_mul" nil *C-mpq*))))
	((or (C-mpz? typeA) (C-mpz? typeB) (C-mpz? typeR))
	 (cond ((C-unsignedlong-type? (cadr args))
		(cons args (cons (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_mul_ui" nil *C-mpz*))))
	       ((C-unsignedlong-type? (car args))
		(cons (reverse args) (cons (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_mul_ui" nil *C-mpz*))))
	       ((C-int-type? (cadr args))
		(cons args (cons (list *C-mpz* *C-int*) (Cfuncall-mp "mpz_mul_si" nil *C-mpz*))))
	       ((C-int-type? (car args))
		(cons (reverse args) (cons (list *C-mpz* *C-int*) (Cfuncall-mp "mpz_mul_ui" nil *C-mpz*))))
	       (t
		(cons args (cons (list *C-mpz* *C-mpz*) (Cfuncall-mp "mpz_mul" nil *C-mpz*))))))
	((C-base? typeR)
	 (cons args (cons (list typeA typeB) (Cfuncall (binop '*) nil typeR))))
	(t
	 (cons args (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_mul" nil *C-mpq*))))))


;; ---------- Division ------------ Problems here...
(defun div-function? (op args)
  (and (eq op 'pvsDiv) (eq (length args) 2)))
(defun pvs2C*-div (typeA typeB typeR args bindings livevars)
  (cond ((or (C-mpq? typeA) (C-mpq? typeB))
	 (Cprocess (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_div" nil *C-mpq*))))
	((every #'C-base? (list typeA typeB typeR))
	 (Cprocess (cons (list typeA typeB) (Cfuncall (binop '/) nil typeR))))
	((and (C-integer? typeA) (C-integer? typeR) (C-unsignedlong-type? (cadr args)))
	 (Cprocess (cons (list *C-mpz* *C-uli*) (Cfuncall-mp "mpz_divexact_ui" nil *C-mpz*))))
	((every #'C-integer? (list typeA typeB typeR))
	 (Cprocess (cons (list *C-mpz* *C-mpz*) (Cfuncall-mp "mpz_divexact" nil *C-mpz*))))
	((and (C-integer? typeA) (C-integer? typeB))
	 (let ((arg1 (pvs2C2 (car args)  bindings livevars
			     (Cfuncall "mpq_numref" (C-var *C-mpq*) *C-mpz*) nil))
	       (arg2 (pvs2C2 (cadr args) bindings livevars
			     (Cfuncall "mpq_denref" (C-var *C-mpq*) *C-mpz*) nil)))
	   (C-expr (C-var *C-mpq*)
		   (append (instr arg1) (instr arg2)
			   (list (Cfuncall-mp "mpq_canonicalize" (list (C-var *C-mpq*)))))
		   (append (destr arg1) (destr arg2)))))
	(t (Cprocess (cons (list *C-mpq* *C-mpq*) (Cfuncall-mp "mpq_div" nil *C-mpq*))))))


;; ---------- Modulo ------------
(defun rem-function? (op) (eq op 'rem))

;; Modulo function as a closure. Not handled right now...
(defun pvs2C*-rem (typeM args bindings livevars)
  (C-expr (C-var (make-instance 'C-closure))))

(defun pvs2C-remappli (expr bindings livevars)
  (let* ((argA (car (arguments expr)))
	 (argM (argument (operator expr)))
	 (args (list argA argM)))
    (Cprocess (pvs2C*-remappli (pvs2C-type argA)
			       (pvs2C-type argM)
			       args bindings livevars)
	      args bindings livevars)))
(defun pvs2C*-remappli (typeA typeM args bindings livevars)
  (cond ((and (C-base? typeA) (C-base? typeM))
	 (cons (list typeA typeM) (Cfuncall (binop "%") nil typeM)))
	((and (C-mpz? typeA) (C-unsignedlong-type? (cadr args)))
	 (cons (list *C-mpz* *C-uli*)
	       (Cfuncall (Cfun "modui" "mpz_mod_ui(null, ~{~a~^, ~})") nil *C-uli*)))
	(t (cons (list *C-mpz* *C-mpz*) (Cfuncall-mp "mpz_mod" nil *C-mpz*)))))






