;;===================================================
;; Make the polynomial operations defined in "prep"
;; and "nlsolver" callable from foreign code
;;====================================================

(defpackage "foreign-callable-polynomial-operations"
  (:nicknames "fcpo")
  (:export "polyrepVarToPoly" "polyrepConstToPoly" "polyrepSubPoly" "polyrepZero"
	   "polyrepOne" "polyrepMinusOne")
  (:use "common-lisp" "prep" "foreign-functions" "named-callbacks"))

(in-package "fcpo")



;;--------------------------------------------
;; Polynomial constructors missing from prep
;; Should be moved somewhere else!
;;--------------------------------------------

;; convert variable x to a polynomial = (1 * (x ^ 1))
(defun polyrepVarToPoly (x) 
  (assert (symbolp x))
  (list (cons 1 (list (cons x 1)))))

;; convert constant c to a polynomial
(eval-when (compile eval load) 
(defun polyrepConstToPoly (c) 
   (assert (rationalp c))
   (if (zerop c) nil (list (list c)))))

;; some constant polynomials
(defconstant polyrepZero nil)
(defconstant polyrepOne (polyrepConstToPoly 1))
(defconstant polyrepMinusOne (polyrepConstToPoly -1))

;; subtraction
(defun polyrepSubPoly (p q)
  (polyrepAddPoly p (polyrepNegativePoly q)))




;;-----------------------------------------------------------------
;; Each object known by the C-layer is registered as a lisp value
;; and is identified by its index in the corresponding table.
;; This function allows the C code to delete an object
;;-----------------------------------------------------------------

(defun-foreign-callable delete-object ((index :int))
  (unregister-lisp-value index))

(add-callback 'delete-object "delete-object")



;;-------------------------------------------------------
;; Conversion from C-level indices to Lisp objects
;; We use one function for each Lisp type we care about
;; but we don't check anything for now.
;;-------------------------------------------------------

;; return a variable based on its index
(defun get-var (id) (lisp-value id))

;; return a polynomial based on its index
(defun get-poly (id) (lisp-value id))

;; return a solver 
(defun get-solver (id) (lisp-value id))



;;----------------------------------------------------------
;; Variable creation:
;; - *var_prefix* is a the prefix used for variable names
;; - each variable is a symbol 'prefix<id>
;;----------------------------------------------------------

(defvar *var-prefix* "x")

;; set/change the prefix
;; s must be a native string in C's conventions
;; (i.e., array of char, terminated by '\0')
(defun-foreign-callable set-prefix ((s (* :char)))
  (setq *var-prefix* (excl:native-to-string s)))

(add-callback 'set-prefix "set-prefix")


;; create a variable with the given id
;; - the variable is added to the list maintained in prep
(defun-foreign-callable new-var ((id :int))
  (let ((x (intern (format nil "~a~a" *var-prefix* id))))
    (set-variables (cons x (get-variables))) ;; update prep's variable list    
    (register-lisp-value x)))

(add-callback 'new-var "new-var")


;; parameters are created internally by the gb solver
;; it's a good idea to delete them between calls to check
;; we also want to reset the bounds internal to gb
(defun-foreign-callable reset-parameters ()
  (gb:set-newU nil)
  (gb:set-newV nil)
  (gb:reset-bounds)
  (set-parameters nil))

(add-callback 'reset-parameters "reset-parameters")

;; delete all variables and parameters
(defun-foreign-callable delete-vars ()
  (gb:set-newU nil)
  (gb:set-newV nil)
  (set-variables nil)
  (set-parameters nil))

(add-callback 'delete-vars "delete-vars")


;;---------------------------
;; Polynomial construction
;;---------------------------

;; convert variable v to a polynomial
;; v must be the index of a variable (as returned by new-var)
(defun-foreign-callable var-to-poly ((v :int))
  (register-lisp-value (polyrepVarToPoly (get-var v))))

(add-callback 'var-to-poly "var-to-poly")

;; convert integer c to a polynomial
(defun-foreign-callable int-to-poly ((c :long))
  (register-lisp-value (polyrepConstToPoly c)))

(add-callback 'int-to-poly "int-to-poly")

;; convert quotient a/b to a polynomial
(defun-foreign-callable quotient-to-poly ((a :long) (b :unsigned-long))
  (register-lisp-value (polyrepConstToPoly (/ a b))))

(add-callback 'quotient-to-poly "quotient-to-poly")

;; convert string s to a polynomial
;; - s must be a C-style string that can be parsed to a rational
;; - can be used for large rationals
(defun-foreign-callable string-to-poly ((s (* :char)))
  (let ((str (excl:native-to-string s)))
    (register-lisp-value (polyrepConstToPoly (read-from-string str)))))

(add-callback 'string-to-poly "string-to-poly")


;; small constants
(defun-foreign-callable zero () (register-lisp-value polyrepZero))
(defun-foreign-callable one () (register-lisp-value polyrepOne))
(defun-foreign-callable minus-one () (register-lisp-value polyrepMinusOne))

(add-callback 'zero "zero")
(add-callback 'one "one")
(add-callback 'minus-one "minus-one")


;; operations: the arguments must be indices of polynomial objects
;; (as returned by the polynomial constructors)
(defun-foreign-callable add-poly ((p :int) (q :int))
  (register-lisp-value (polyrepAddPoly (get-poly p) (get-poly q))))

(defun-foreign-callable sub-poly ((p :int) (q :int))
  (register-lisp-value (polyrepSubPoly (get-poly p) (get-poly q))))

(defun-foreign-callable neg-poly ((p :int))
  (register-lisp-value (polyrepNegativePoly (get-poly p))))

(defun-foreign-callable mul-poly ((p :int) (q :int))
  (register-lisp-value (polyrepMultiplyPoly (get-poly p) (get-poly q))))

(add-callback 'add-poly "add")
(add-callback 'sub-poly "sub")
(add-callback 'neg-poly "neg")
(add-callback 'mul-poly "mul")

;; exponentiation d must be a positive integer
(defun-foreign-callable power-poly ((p :int) (d :int))
  (register-lisp-value (polyrepExpPolyCst (get-poly p) d)))

(add-callback 'power-poly "power")



;;------------------
;; Pretty printing
;;------------------

(defun-foreign-callable print-poly ((p :int))
  (format t "~a" (polyrepPrint (get-poly p))))

(add-callback 'print-poly "print")




;;--------------------------------
;; Solver construction and reset
;;--------------------------------

(defun-foreign-callable make-solver ()
  (register-lisp-value (nlsolver:create)))

(add-callback 'make-solver "make-solver")


(defun-foreign-callable reset-solver ((s :int))
  (nlsolver:reset (get-solver s)))

(add-callback 'reset-solver "reset-solver")




;;-----------------------------------------------------
;; Assertions
;; - each assertion has an id wich must be an integer
;; - if id < 0 it's ignored
;;-----------------------------------------------------

(defun get-id (id) (if (>= id 0) id))

;; p = 0
(defun-foreign-callable assert-zero ((s :int) (p :int) (id :int))
  (nlsolver:assert-zero (get-solver s) (get-poly p) (get-id id)))

(add-callback 'assert-zero "assert-zero")

;; p > 0
(defun-foreign-callable assert-pos ((s :int) (p :int) (id :int))
  (nlsolver:assert-pos (get-solver s) (get-poly p) (get-id id)))

(add-callback 'assert-pos "assert-pos")

;; p < 0
(defun-foreign-callable assert-neg ((s :int) (p :int) (id :int))
  (nlsolver:assert-neg (get-solver s) (get-poly p) (get-id id)))

(add-callback 'assert-neg "assert-neg")

;; p >= 0
(defun-foreign-callable assert-nonneg ((s :int) (p :int) (id :int))
  (nlsolver:assert-nonneg (get-solver s) (get-poly p) (get-id id)))

(add-callback 'assert-nonneg "assert-nonneg")

;; p <= 0
(defun-foreign-callable assert-nonpos ((s :int) (p :int) (id :int))
  (nlsolver:assert-nonpos (get-solver s) (get-poly p) (get-id id)))

(add-callback 'assert-nonpos "assert-nonpos")

;; p = q
(defun-foreign-callable assert-eq ((s :int) (p :int) (q :int) (id :int))
  (nlsolver:assert-eq (get-solver s) (get-poly p) (get-poly q) (get-id id)))

(add-callback 'assert-eq "assert-eq")


;; p > q
(defun-foreign-callable assert-gt ((s :int) (p :int) (q :int) (id :int))
  (nlsolver:assert-gt (get-solver s) (get-poly p) (get-poly q) (get-id id)))

(add-callback 'assert-gt "assert-gt")

;; p < q
(defun-foreign-callable assert-lt ((s :int) (p :int) (q :int) (id :int))
  (nlsolver:assert-lt (get-solver s) (get-poly p) (get-poly q) (get-id id)))

(add-callback 'assert-lt "assert-lt")

;; p >= q
(defun-foreign-callable assert-geq ((s :int) (p :int) (q :int) (id :int))
  (nlsolver:assert-geq (get-solver s) (get-poly p) (get-poly q) (get-id id)))

(add-callback 'assert-geq "assert-geq")

;; p <= q
(defun-foreign-callable assert-leq ((s :int) (p :int) (q :int) (id :int))
  (nlsolver:assert-leq (get-solver s) (get-poly p) (get-poly q) (get-id id)))

(add-callback 'assert-leq "assert-leq")



;;-------------------
;; Print assertions
;;-------------------

(defun-foreign-callable print-solver ((s :int))
  (nlsolver:print-solver (get-solver s)))

(add-callback 'print-solver "print-solver")


;;-----------------------
;; Check satisfiability
;;-----------------------

;; return 1 (true) for "unknown" (no conflict found)
;; return 0 (false) for "unsatisfiable"
(defun-foreign-callable check-solver ((s :int))
  (if (nlsolver:check (get-solver s)) 1 0))

(add-callback 'check-solver "check-solver")

;; incremental check
;; - update the solver state
;; return 1 (true) for "unknown" (no conflict found)
;; return 0 (false) for "unsatisfiable"
(defun-foreign-callable incremental-check ((s :int))
  (if (nlsolver:incremental-check (get-solver s)) 1 0))

(add-callback 'incremental-check "incremental-check")


;;---------------
;; Explanations
;;---------------

;; test whether the assertion of the given id in the 
;; unsatisfiability witness for s.
;; - id must be the id given in a previous assertion 
;; - so id must not be negative
;; return 1 (true) if yes
;; return 0 (false) otherwise
(defun-foreign-callable is-witness ((s :int) (id :int))
  (if (nlsolver:is-witness? (get-solver s) id) 1 0))

(add-callback 'is-witness "is-witness")


