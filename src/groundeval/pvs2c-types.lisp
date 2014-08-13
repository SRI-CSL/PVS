;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                PVS to C translator
;;
;;     Author: Gaspard ferey
;;
;;  -> https://github.com/Gaspi/pvs2c.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This requires "pvs2c.lisp", "Cutils.lisp" and "Cprimop.lisp" files both available at
;;               https://github.com/Gaspi/pvs2c.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; Classes to represent C types
(defcl C-type ())
(defcl C-base    (C-type)) ;; represents int, long, ...
(defcl C-pointer (C-type) (bang))   ;; The bang type is 
(defcl C-gmp     (C-type))

(defcl C-number ())
(defcl C-integer (C-number) (range))

(defcl C-int (C-base C-integer))
(defcl C-uli (C-base C-integer))
(defcl C-mpz (C-gmp  C-integer))
(defcl C-mpq (C-gmp  C-number ))

(defcl C-array      (C-pointer) (target) (size))
(defcl C-struct     (C-pointer) (name) (args))
(defcl C-closure    (C-pointer)) ;; To be implemented...
(defcl C-named-type (C-pointer) (name)) ;; ???


;; -------- C constants (implementation dependent) --------
(defvar *min-C-int* (- (expt 2 15)))
(defvar *max-C-int* (- (expt 2 15) 1))
(defvar *min-C-uli* 0)
(defvar *max-C-uli* (- (expt 2 32) 1))

(defvar *C-int-range* (C-range (cons *min-C-int* *max-C-int*)))
(defvar *C-uli-range* (C-range (cons *min-C-uli* *max-C-uli*)))

;; Some instances of the classes above (to avoid over-instanciating)
(defvar *C-type* (make-instance 'C-type))  ;; Type undefined
(defvar *C-int* (make-instance 'C-int :range *C-int-range*))
(defvar *C-uli* (make-instance 'C-uli :range *C-uli-range*))
(defvar *C-mpz* (make-instance 'C-mpz))
(defvar *C-mpq* (make-instance 'C-mpq))
(defvar *C-struct* (make-instance 'C-struct :name nil :args nil :bang nil))



;; Type equality  (mainly used to know if conversion is needed)
(defmethod type= ((typeA C-int) (typeB C-int)) t)
(defmethod type= ((typeA C-uli) (typeB C-uli)) t)
(defmethod type= ((typeA C-mpz) (typeB C-mpz)) t)
(defmethod type= ((typeA C-mpq) (typeB C-mpq)) t)
(defmethod type= ((typeA C-array) (typeB C-array))
  (and (type= (target typeA) (target typeB))
       (eq    (size   typeA) (size   typeB))))
(defmethod type= ((typeA C-struct) (typeB C-struct))
  (string= (name typeA) (name typeB)))
(defmethod type= ((typeA C-named-type) (typeB C-named-type))
  (string= (name typeA) (name typeB)))
(defmethod type= ((typeA C-closure) (typeB C-closure)) t)
(defmethod type= (typeA typeB) nil)

;; Type printing in C syntax
(defmethod print-object ((obj C-int) out) (format out "int"))
(defmethod print-object ((obj C-uli) out) (format out (uli)))
(defmethod print-object ((obj C-mpz) out) (format out "mpz_t"))
(defmethod print-object ((obj C-mpq) out) (format out "mpq_t"))
(defmethod print-object ((obj C-array) out) (format out "~a*" (target obj)))
(defmethod print-object ((obj C-struct) out) (format out "~a" (name obj)))
(defmethod print-object ((obj C-named-type) out) (format out "~a" (name obj)))
(defmethod print-object ((obj C-closure) out) (format out "pvsClosure"))
(defmethod print-object ((obj C-type) out) (format out "[Abstract C type]"))

;; Translating PVS types to C types
(defmethod pvs2C-type ((type recordtype) &optional tbindings)
  (with-slots (print-type) type
     (if (type-name? print-type)
	(let ((entry (assoc (declaration print-type) *C-record-defns*)))
	  (if entry (make-instance 'C-struct :name (cadr entry) :args (cadddr entry))
	    (let* ((flds  (fields type))
		   (ids   (mapcar #'id flds))
		   (types (pvs2C-type (mapcar #'declared-type flds)))
		   (args (pairlis ids types))
		   (formatted-fields (loop for arg in args
					   collect (format nil "~a ~a;" (cdr arg) (car arg))))
		   (C-rectype-name (gentemp (format nil "~a" (id print-type))))
		   (C-rectype (format nil "~%struct struct_~a {~%~{  ~a~%~}};~%typedef struct struct_~a* ~a;"
				      C-rectype-name formatted-fields C-rectype-name C-rectype-name)))
	      (push (list (declaration print-type) C-rectype-name C-rectype args)
		    *C-record-defns*)
	      (make-instance 'C-struct
			     :name C-rectype-name
			     :args args ))))
       (pvs2C-error "~%Record type ~a must be declared." type))))

(defmethod pvs2C-type ((type tupletype) &optional tbindings)
  (with-slots (print-type) type
     (if (type-name? print-type)
	(let ((entry (assoc (declaration print-type) *C-record-defns*)))
	  (if entry (make-instance 'C-struct :name (cadr entry) :args (cadddr entry))
	    (let* ((flds  (types type))
		   (ids   (loop for n from 1 below (+ 1 (length flds))
				collect (format nil "f~a" n)))
		   (types (pvs2C-type flds))
		   (args (pairlis ids types))
		   (formatted-fields (loop for arg in args
					   collect (format nil "~a ~a;" (cdr arg) (car arg))))
		   (C-rectype-name (gentemp (format nil "~a" (id print-type))))
		   (C-rectype (format nil "~%struct struct_~a {~%~{  ~a~%~}};~%typedef struct struct_~a* ~a;"
				      C-rectype-name formatted-fields C-rectype-name C-rectype-name)))
	      (push (list (declaration print-type) C-rectype-name C-rectype args)
		    *C-record-defns*)
	      (make-instance 'C-struct
			     :name C-rectype-name
			     :args args ))))
       (pvs2C-error "~%Tuple type ~a must be declared." type))))

(defmethod pvs2C-type ((type funtype) &optional tbindings)
  (if (C-updateable? type)
      (let ((range (subrange-index (domain type))))
	(make-instance 'C-array
		       :target (pvs2C-type (range type))
		       :bang nil
		       :size
		         (when (and (car range)
				    (cadr range)
				    (not (eq (car range) '*))
				    (not (eq (cadr range) '*)))
			   (1+ (- (cadr range) (car range))))))
    (make-instance 'C-closure)))

(defmethod bang-type ((type C-pointer)) (setf (bang type) t) type)
(defmethod bang-type ((type C-type)) type)
(defmethod bang ((e C-type)) nil) ;; The default bang method

(defmethod pvs2C-type ((type subtype) &optional tbindings)
  (cond ((subtype-of? type *boolean*) *C-int*)
	((subtype-of? type *integer*)
	 (let ((range (C-range type)))
	   (cond ((range-included range *C-int-range*) *C-int*)
		 ((range-included range *C-uli-range*) *C-uli*)
		 (t *C-mpz*))))
	 ((subtype-of? type *number*) *C-mpq*)
	 (t (pvs2C-type (find-supertype type)))))


(defun C-type-args (operator)
  (let ((dom-type (domain (type operator))))
    (if (tupletype? dom-type)
	(pvs2C-type (types dom-type))
      (list (pvs2C-type dom-type)))))


(defun is-expr-subtype? (expr type)
  (some #'(lambda (jty) (subtype-of? jty type))
	(get-PVS-types expr)))
(defun C-integer-type? (expr)
  (is-expr-subtype? expr *integer*))

(defun C-unsignedlong-type? (expr)
  (and (C-integer-type? expr)
       (range-included (C-range expr) *C-uli-range*)))
(defun C-int-type? (expr)
  (and (C-integer-type? expr)
       (range-included (C-range expr) *C-int-range*)))


(defmethod pvs2C-type ((e lambda-expr) &optional tbindings)
  (with-slots (type expression) e
  (if (C-updateable? type)   ;; extend this function to work with more than below
      (let ((range (subrange-index (domain type))))
	(make-instance 'C-array
		       :target (pvs2C-type expression)
		       :bang nil
		       :size (1+ (- (cadr range) (car range)))))
    (make-instance 'C-closure))))

(defmethod pvs2C-type ((e expr) &optional tbindings)
  (if (C-integer-type? e)
      (let ((range (C-range e)))
	(cond ((range-included range *C-int-range*) *C-int*)
	      ((range-included range *C-uli-range*) *C-uli*)
	      (t *C-mpz*)))
    (pvs2C-type (type e))))

(defmethod pvs2C-type ((l list) &optional tbindings)
  (when (consp l)
    (cons (pvs2C-type (car l))
	  (pvs2C-type (cdr l)))))

(defmethod pvs2C-type ((type type-name) &optional tbindings)
  (if (eql (id type) 'boolean) *C-int*
    (pvs2C-type (type (car (resolutions type))))))
       ;; (or (call-next-method)
       ;; 	   (make-instance 'C-named-type
       ;; 			  :name (or (cdr (assoc type tbindings :test #'tc-eq))
       ;; 				    (id type)))))))

(defun get-PVS-types (expr)
  (let ((*generate-tccs* t))
    (cons (type expr) (judgement-types+ expr))))



(defmethod pointer? ((e expr)) (C-pointer? (pvs2C-type (type e))))



;; An expression to represent the memory size of a type
;; Deprecated
(defmethod m-size ((type C-array))
  (format nil "~a * sizeof(~a)" (size type) (target type)))
(defmethod m-size ((type C-type))
  (format nil "sizeof(~a)" type))




;; -------- Converting a C expression to an other type --------



;; Convert a (not unnamed) C-expr to an other with different type
(defun convert (type e)
  (let ((typeE (type e)))
    (cond ((type= type typeE) e)
	  ((and (C-int? type) (C-uli? typeE))
	   (set-var e (Cfuncall (Cfun "uli-to-int"           "(int) ~{~a~}") (var e) type)) e)
	  ((and (C-uli? type) (C-int? typeE))
	   (set-var e (Cfuncall (Cfun "int-to-uli" (uli "(~a) ~~{~~a~~}")) (var e) type)) e)
	  (t (let ((n (gen-C-var type "conv")))
	       (C-expr n
		       (append (instr e)
			       (list (Cdecl n) (Ccopy n (var e)))
			       (destr e))
		       (list (Cfree n))))))))


(defgeneric convertor (typeA typeB))
;; ---------- GMP library conversions ---------------
(defmethod convertor ((typeA C-mpz) (typeB C-mpz)) (list "mpz_set(~a, ~a);"))
(defmethod convertor ((typeA C-mpz) (typeB C-mpq)) (list "mpq_get_num(~a, ~a);"))
(defmethod convertor ((typeA C-mpz) (typeB C-uli)) (list "mpz_set_ui(~a, ~a);"))
(defmethod convertor ((typeA C-mpz) (typeB C-int)) (list "mpz_set_si(~a, (long) ~a);"))
(defmethod convertor ((typeA C-mpq) (typeB C-mpq)) (list "mpq_set(~a, ~a);"
							 "mpq_canonicalize(~a);"))
(defmethod convertor ((typeA C-mpq) (typeB C-mpz)) (list "mpq_set_z(~a, ~a);"
							 "mpq_canonicalize(~a);"))
(defmethod convertor ((typeA C-mpq) (typeB C-uli)) (list "mpq_set_d(~a, (double) ~a );"
							 "mpq_canonicalize(~a);"))
(defmethod convertor ((typeA C-mpq) (typeB C-int)) (list "mpq_set_d(~a, (double) ~a );"
							 "mpq_canonicalize(~a);"))
(defmethod convertor ((typeA C-mpz) typeB)
  (list (format nil "mpz_from_~a( ~~a, ~~a );" typeB)))
(defmethod convertor ((typeA C-mpq) typeB)
  (list (format nil "mpq_from_~a( ~~a, ~~a );" typeB)))
(defmethod convertor ((typeA C-uli) (typeB C-mpz)) "mpz_get_ui(~a)")
(defmethod convertor ((typeA C-int) (typeB C-mpz)) "( (int) mpz_get_si(~a) )")
(defmethod convertor ((typeA C-uli) (typeB C-mpq)) (uli "( (~a) mpq_get_d(~~a) )"))
(defmethod convertor ((typeA C-int) (typeB C-mpq)) "( (int) mpq_get_d(~a) )")
;; ---------- base types ------------------------
(defmethod convertor ((typeA C-int) (typeB C-uli)) "(int) ~a")
(defmethod convertor ((typeA C-uli) (typeB C-int)) (uli "(~a) ~~a"))
(defmethod convertor ((typeA C-uli) (typeB C-uli)) "~a")
(defmethod convertor ((typeA C-int) (typeB C-int)) "~a")
;; ---------- arrays pointer copy -----------------
(defmethod convertor ((typeA C-array) (typeB C-array))
  (format nil "(~a) ~~a" typeA))
;; ---------- struct pointer copy -----------------
(defmethod convertor ((typeA C-struct) (typeB C-struct))
  (format nil "(~a) ~~a" typeA))

;; ---------- other cases (basically unimplemented) --------------
(defmethod convertor (typeA typeB)
  (let ((func (if (type= typeA typeB)
		  (format nil "copy_~a" typeA)
		(format nil "~a_from_~a" typeA typeB))))
    (if (C-gmp? typeA)
	(list (format nil "~a(~~{~~a~~^, ~~});" func))
      (format nil "~a(~~a)" func))))


(defgeneric smaller-type (typeA typeB))
(defmethod smaller-type ((typeA C-mpq) typeB) typeB)
(defmethod smaller-type ((typeA C-mpz) (typeB C-mpq)) typeA)
(defmethod smaller-type ((typeA C-mpz) typeB) typeB)
(defmethod smaller-type ((typeA C-uli) (typeB C-gmp)) typeA)
(defmethod smaller-type ((typeA C-uli) typeB) typeB)
(defmethod smaller-type ((typeA C-int) typeB) typeA)


(defmethod smaller-type ((typeA C-closure) (typeB C-closure))
  (make-instance 'C-closure))

(defmethod smaller-type (typeA typeB)
  (format t "!!! Warning !!! smaller-type called with unexpected type.")
  typeA)


;; ----------- Functions to convert environment to (*void)[] -------------------

;; Return arrays of instructions
(defun save-void (var-ind name-arr)
  (save-void* (pvs2C-type (type (car var-ind)))
	      (format nil "~a[~d]" name-arr (cdr var-ind))
	      (id (car var-ind))))

;; Return arrays of instructions
(defun load-void (var-ind name-arr)
  (load-void* (pvs2C-type (type (car var-ind)))
	      (format nil "~a[~d]" name-arr (cdr var-ind))
	      (id (car var-ind))))

(defmethod save-void* (type name id)
  (list (format nil "~a = ~a;" name id)))
(defmethod load-void* (type name id)
  (list (format nil "~a ~a = (~a) ~a;" type id type name)))



