;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ics-pvs.lisp -- 
;; Author          : Harald Ruess
;; Created On      : Tue May 14 11:32:58 PDT 2002
;; Last Modified By: Harald Ruess
;; Last Modified On: Tue May 14 11:32:58 PDT 2002
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; Wrapping and unwrapping ICS values in order to finalize 
;; objects of these types.

(defstruct (wrap
	    (:predicate wrap?)
	    (:constructor make-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
               (format t "<#wrap: ~a>" (wrap-address p)))))
  address)

(defun wrap (value)
  (assert (integerp value))
  (make-wrap value))

(defun unwrap (w)
  (assert (wrap? w))
  (wrap-address w))

(defun wrap= (w1 w2)
  (assert (wrap? w1))
  (assert (wrap? w2))
  (= (wrap-address w1) (wrap-address w2)))

(defun wrap-finalize! (w)
  (assert (wrap? w))
  (excl:schedule-finalization w 'ics-deregister-pointer))

(defun wrap-free! (w)
  (assert (wrap? w))
  (ics_deregister (unwrap w)))

(defun value-free! (v)
  (assert (integerp v))
  (ics_deregister v))

(defstruct (term-wrap
	    (:include wrap)
	    (:predicate term-wrap?)
	    (:constructor make-term-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#term: ~a>" (wrap-address p))))))

(defun term-wrap (value)
   (assert (integerp value))
   (make-term-wrap value))

(defun term-unwrap (w)
  (assert (term-wrap? w))
  (wrap-address w))

(defstruct (atom-wrap
	    (:include wrap)
	    (:predicate atom-wrap?)
	    (:constructor make-atom-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#atom: ~a>" (wrap-address p))))))

(defun atom-wrap (value)
   (assert (integerp value))
   (make-atom-wrap value))

(defun atom-unwrap (w)
  (assert (atom-wrap? w))
  (wrap-address w))

(defstruct (prop-wrap
	    (:include wrap)
	    (:predicate prop-wrap?)
	    (:constructor make-prop-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#prop: ~a>" (wrap-address p))))))

(defun prop-wrap (value)
   (assert (integerp value))
   (make-prop-wrap value))

(defun prop-unwrap (w)
  (assert (prop-wrap? w))
  (wrap-address w))

(defstruct (state-wrap
	    (:include wrap)
	    (:predicate state-wrap?)
	    (:constructor make-state-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#state: ~a>" (wrap-address p))))))

(defun state-wrap (value)
   (assert (integerp value))
   (make-state-wrap value))

(defun state-unwrap (w)
  (assert (state-wrap? w))
  (wrap-address w))


;; Check if ICS predicate holds

(defmacro holds (arg)
  `(plusp ,arg))


;; ICS-PVS error handling

(ff:defun-foreign-callable ocaml_error (fname msg)
  (error (format nil "~a: ~a" (excl:native-to-string fname)
		 (excl:native-to-string msg))))

(ff:def-foreign-call register_lisp_error_function (index))

;; PVS decision procedure interface

(defun ics-init (&optional full (verbose 0))
  (ics_caml_startup (if full 1 0) #(0))
  (register_lisp_error_function
   (nth-value 1 (ff:register-function `ocaml_error)))
  (ics_init verbose))

(defun ics-empty-state ()
  (let ((empty (state-wrap (ics_state_empty))))
    (wrap-finalize! empty)
    empty))

(defun ics-d-consistent (value)
  (assert (integerp value))
  (let ((state (state-wrap (ics_d_consistent value))))
    (wrap-finalize! state)
    state))

(defun ics-deregister-pointer (wrapper)
  (assert (wrap? wrapper))
  (wrap-free! wrapper))

(defun ics-process (state term)
  (assert (state-wrap? state))
  (assert (term-wrap? term))
  (ics_process (state-unwrap state) (term-unwrap term)))


;; Return a unique name for an expression

(defvar *pvs-to-ics-symtab* 
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))

(defvar *unique-name-counter* 0)
	
(defun unique-name (expr)
  (or (gethash expr *pvs-to-ics-symtab*)
      (let ((name (concatenate 'string 
			       (if (name-expr? expr) (symbol-name (id expr)) "new")
			       "@@"
			       (format nil "~d" *counter*))))
	(setf *counter* (1+ *counter*))
	(setf (gethash expr *pvs-to-ics-symtab*) name))))


;; Translating from PVS expressions to ICS terms

(defvar *pvs-to-ics-hash* 
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))

(defvar *ics-to-pvs-hash* 
  (make-hash-table :test 'eql))

(defvar *translate-type-predicates* nil)

(defun translate-to-ics (expr)
  (translate-fmla-to-ics* expr))

(defmethod translate-fmla-to-ics* :around ((expr expr))
  (or (gethash expr *pvs-to-ics-hash*)
      (let ((term (call-next-method)))
	(assert (integerp term))
	(let ((wrapper (prop-wrap term)))
	  (wrap-finalize! wrapper)
	  (setf (gethash expr *pvs-to-ics-hash*) wrapper)))))

(defmethod translate-to-ics* ((expr expr))
  (ics_mk_var (unique-name expr)))

(defmethod translate-to-ics* ((expr name-expr))
  (declare (special *true*))
  (declare (special *false*))
  (cond ((tc-eq expr *true*)
	 (ics_mk_true))
	((tc-eq expr *false*)
	 (ics_mk_false))
	(t
	 (call-next-method))))


(defmethod translate-to-ics* ((expr number-expr))
  (let ((q (ics_num_of_string (format nil "~d" (number expr)))))
    (ics_mk_num q)))

(defmethod translate-to-ics* ((num fixnum))
  num)

(defun translate-unary-to-ics (ics-constructor expr)
  (assert (typep expr 'expr))
  (funcall ics-constructor (term-unwrap (translate-to-ics* expr))))

(defun translate-binary-to-ics (ics-constructor expr1 expr2)
  (assert (typep expr1 'expr))
  (assert (typep expr2 'expr))
  (funcall ics-constructor
	   (term-unwrap (translate-to-ics* expr1))
	   (term-unwrap (translate-to-ics* expr2))))

(defun translate-ternary-to-ics (ics-constructor expr1 expr2 expr3)
  (funcall ics-constructor
	   (term-unwrap (translate-to-ics* expr1))
	   (term-unwrap (translate-to-ics* expr2))
	   (term-unwrap (translate-to-ics* expr3))))

(defmethod translate-to-ics* ((expr equation))
  (translate-binary-to-ics #'ics_atom_mk_equal (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr disequation))
  (translate-binary-to-ics #'ics_atom_mk_diseq (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr branch))
  (translate-ternary-to-ics #'ics_prop_mk_ite
			    (condition expr)
			    (then-part expr)
			    (else-part expr)))

(defmethod translate-to-ics* ((expr negation))
  (let ((arg (args1 expr)))
    (cond ((equation? arg)
	   (translate-binary-to-ics #'ics_atom_mk_diseq (args1 arg) (args2 arg)))
	  ((disequation? arg)
	   (translate-binary-to-ics #'ics_atom_mk_equal (args1 arg) (args2 arg)))
	  (t
	   (translate-unary-to-ics #'ics_atom_neg arg)))))

(defmethod translate-ics* ((expr iff-or-boolean-equation))
  (translate-binary-to-ics #'ics_prop_mk_iff (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr implication))
  (translate-binary-to-ics #'ics_prop_mk_imp (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr conjunction))
  (translate-binary-to-ics #'ics_prop_mk_conj (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr disjunction))
  (translate-binary-to-ics #'ics_prop_mk_disj (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr application))
  (declare (special *translate-type-predicates*))
  (let ((op (operator expr)))
    (cond ((update-expr? op)
	   (term-unwrap (translate-to-ics* (translate-update-to-if expr))))
	  ((tc-eq op (plus-operator))
	   (translate-binary-to-ics #'ics_term_mk_add2 (args1 expr) (args2 expr)))
	  ((tc-eq op (difference-operator))
	   (translate-binary-to-ics #'ics_term_mk_sub (args1 expr) (args2 expr)))
	  ((tc-eq op (unary-minus-operator))
	   (translate-unary-to-ics #'ics_term_mk_unary_minus (args1 expr)))
	  ((tc-eq op (times-operator))
	   (translate-binary-to-ics #'ics_term_mk_mult2 (args1 expr) (args2 expr)))
	  ((tc-eq op (divides-operator))
	   (translate-binary-to-ics #'ics_term_mk_div (args1 expr) (args2 expr)))
	  ((tc-eq op (greatereq-operator))
	   (translate-binary-to-ics #'ics_atom_mk_ge (args1 expr) (args2 expr)))
	  ((tc-eq op (greater-operator))
	   (translate-binary-to-ics #'ics_atom_mk_gt (args1 expr) (args2 expr)))
	  ((tc-eq op (less-operator))
	   (translate-binary-to-ics #'ics_atom_mk_lt (args1 expr) (args2 expr)))
	  ((tc-eq op (lesseq-operator))
	   (translate-binary-to-ics #'ics_atom_mk_le (args1 expr) (args2 expr)))
	  ((tc-eq op (integer_pred))
	   (if *translate-type-predicates*
	       (translate-unary-to-ics #'ics_atom_mk_int (args1 expr))
	     (ics_mk_true)))
	  ((or (tc-eq op (real_pred)) ; ICS does not distinguish between rationals and reals
               (tc-eq op (rational_pred)))
	   (if *translate-type-predicates*
	       (translate-unary-to-ics #'ics_atom_mk_real (args1 expr))
	     (ics_mk_true)))
	  (t
	   (translate-uninterp-to-ics* op (arguments expr))))))

(defun translate-uninterp-to-ics* (expr exprs)
  (let ((trm (term-unwrap (translate-to-ics* expr)))
	(trms (translate-list-to-ics* exprs)))
    (ics_mk_uninterp trm trms)))

(defmethod translate-to-ics* ((expr cases-expr))
  (translate-to-ics* (translate-cases-to-if expr)))

(defmethod translate-to-ics* ((expr let-expr))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-ics* reduced-expr))))

(defmethod translate-to-ics* ((expr record-expr))
  (let* ((exprs (mapcar #'expression
			(sort-assignments (assignments expr))))
	 (trms (translate-list-to-ics* exprs)))
    (ics_mk_tuple trms)))

(defun sort-assignments (assignments)
  (sort (copy-list assignments)
	#'string-lessp
	:key #'(lambda (assignment)
		 (id (caar (arguments assignment))))))

(defmethod translate-to-ics* ((expr tuple-expr))
  (let ((trms (translate-list-to-ics* (exprs expr))))
    (ics_mk_tuple trms)))
	
(defmethod translate-to-ics* ((expr coercion))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-ics* reduced-expr))))

(defun translate-list-to-ics* (exprs)
  (if (null exprs)
      (ics_nil)
    (let ((trm (translate-to-ics* (car exprs))))
      (ics_cons (term-unwrap trm) (translate-list-to-ics* (cdr exprs))))))

(defmethod translate-to-ics* ((expr projection-application))
  (let* ((arg (argument expr))
	 (width (width-of (type arg)))
	 (index (1- (index expr))))
    (ics_mk_proj index width (term-unwrap (translate-to-ics* arg)))))

(defmethod translate-to-ics* ((expr field-application))
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (pos (position id (sort-fields fields)
			  :test #'(lambda (x y) (eq x (id y)))))
	   (trm (unwrap (translate-to-ics* argument))))
      (ics_mk_proj pos (length fields) trm))))

(defmethod width-of ((type tupletype))
  (length (types type)))

