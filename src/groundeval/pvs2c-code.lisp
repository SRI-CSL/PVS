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
;; Defining here a intermediate language between C and PVS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; -------- Classe result of translating functions -------------------
(defcl C-expr () (var) (instr) (destr))
(defun C-expr (var &optional instr destr)
  (make-instance 'C-expr :var var :instr instr :destr destr))
(defun mk-C-expr (type name &optional instr destr)
  (C-expr (C-var type name) instr destr))

(defmethod type ((e C-expr)) (type (var e)))
(defmethod name ((e C-expr)) (name (var e)))
(defmethod var ((e list))
  (when (consp e) (cons (var (car e)) (var (cdr e)))))


(defun set-var (C-expr var) (setf (var C-expr) var))
(defmethod set-type ((C-expr C-expr) (type C-type))
  (setf (type (var C-expr)) type))
(defmethod set-name ((C-expr C-expr) name)
  (setf (name (var C-expr)) name))
(defmethod set-instr ((C-expr C-expr) instr)
  (setf (instr C-expr) instr))
(defmethod set-destr ((C-expr C-expr) destr)
  (setf (destr C-expr) destr))

(defmethod app-instr ((C-expr C-expr) instr &optional first)
  (let ((i (if (listp instr) instr (list instr))))
    (set-instr C-expr
	       (if first (append i (instr C-expr))
		         (append (instr C-expr) i)))))
(defmethod app-destr ((C-expr C-expr) destr &optional first)
  (set-destr C-expr
	     (if first
		 (append destr (destr C-expr))
	       (append (destr C-expr) destr))))

(defmethod unnamed? ((C-expr C-expr))
  (and (C-var? (var C-expr)) (unnamed? (var C-expr))))




;; --------------------------------------------------------------------
;;           CLOS supertypes representation for fragments of C code
;; --------------------------------------------------------------------

;; ------- Abstract supertype for C expressions --------
(defcl Ccode ())

;; --------------------------------------------------------------------
;;               CLOS representation of C expressions      
;; --------------------------------------------------------------------

;; -------- A C variable : is a C type and a name (string or hole) ----
(defcl C-var (Ccode) (type) (name) (safe))
(defun C-var (type &optional name (safe (C-base? type)))   ;; All C-base variables are safe
  (make-instance 'C-var :type type
		 :name (if (listp name) name
			 (format nil "~a" name)) ;; names must be strings
		 :safe safe))
(defun C-var-copy (v) (C-var (type v) (name v) (safe v)))
(defun C-var-list (types names)
  (when (consp types) (cons (C-var (car types) (car names))
			    (C-var-list (cdr types) (cdr names)))))
(defun gen-C-var (type prefix) (C-var type (gentemp (format nil "~a" prefix))))
(defmethod unnamed? ((C-var C-var))
  (or (not (name C-var))
      (and (stringp (name C-var))
	   (string= (name C-var) "~a"))))
;; Here are type functions that can be also applied to variables
(defmethod pointer? ((var C-var)) (C-pointer? (type var)))
(defmethod pointer? (obj) nil)
(defmethod bang-type ((variable C-var))
  (C-var (bang-type (type variable)) (name variable)))

(defmethod sign-var ((l list))
  (format nil "~{~a~^, ~}" (mapcar #'sign-var l)))
(defmethod sign-var (C-var)
  (format nil "~:[~;!~]~a ~a" (and *Cshow-bang* (bang? C-var)) (type C-var) C-var))

(defun safe-var (v &optional (safe t)) (C-var (type v) (name v) safe))

;; ------- Accessor to a record type var :  var.e ------
(defcl Crecord-get (Ccode) (var) (arg) (type))
(defun Crecord-get (var arg)
  (make-instance 'Crecord-get :var var :arg arg
		 :type (cdr (assoc arg (args (type var))))))


;; ------- Accessor to an array type var :  var[e] ------
(defcl Carray-get (Ccode) (var) (arg) (type))
(defun Carray-get (var arg)
  (make-instance 'Carray-get :var var :arg arg
		 :type (target (type var))))


;; -------- Function call : f(e1, ... , e2) ---------
;; May or may not have a valid type
(defcl Cfuncall (Ccode) (fun) (args) (type))
(defun Cfuncall (fun &optional args type)
  (set-arguments
   (make-instance 'Cfuncall :fun (Cfun fun) :args nil :type type)
   args))
(defmethod set-arguments ((fc Cfuncall) argts)
  (let ((args (if (listp argts) argts (list argts))))
    (setf (args fc) args) fc))


(defmethod safe ((e Crecord-get)) (safe (var e)))
(defmethod safe ((e Carray-get )) (safe (var e)))
(defmethod safe ((e Cfuncall)) t)
(defmethod safe (e) nil)

(defmethod eq-C-var ((x string) (y string)) (string= x y))
(defmethod eq-C-var ((x C-var)   y        ) (eq-C-var (name x) y))
(defmethod eq-C-var ( x         (y C-var) ) (eq-C-var x (name y)))
(defmethod eq-C-var ((x Carray-get ) (y Carray-get )) (eq-C-var (var x) (var y)))
(defmethod eq-C-var ((x Crecord-get) (y Crecord-get)) (eq-C-var (var x) (var y)))
(defmethod eq-C-var (x y) nil)




;; --------------------------------------------------------------------
;;           CLOS representation of a full line of C code 
;; --------------------------------------------------------------------

;; --------------------- Supertype for instructions -------------------
(defcl Cinstr (Ccode))

;; --------- Declaration of a variable : int* T; ----------------------
(defcl Cdecl (Cinstr) (var))
(defun Cdecl (var)
  (make-instance 'Cdecl :var var))



;; -------- Free a variable : GC_free(T);  mpz_clear(a); --------------
(defcl Cfree (Cinstr) (var))
(defun Cfree (var) (make-instance 'Cfree :var var))

;; -------- A = 2; ------------  Is not used...
(defcl Cset (Cinstr) (var) (expr))
(defun Cset (var expr)
  (make-instance 'Cset :var var :expr expr))

;; ------- MPZ function call (instruction) : mpz_?(res, ...);  --------
(defcl Cfuncall-mp (Cinstr) (Cfunc))
(defun Cfuncall-mp (fun &optional args type)
  (make-instance 'Cfuncall-mp :Cfunc (Cfuncall fun args type)))
(defmethod set-arguments ((fc Cfuncall-mp) argts)
  (set-arguments (Cfunc fc) argts)
  fc)
(defmethod type ((fc Cfuncall-mp)) (type (Cfunc fc)))

;; -------- If instructions ------------------------------------------
(defcl Cif (Cinstr) (var) (cond-part) (then-part) (else-part))
(defun Cif (cond-part then-part else-part)
  (make-instance 'Cif :cond-part cond-part
		      :then-part then-part
		      :else-part else-part))

;; -------- Array initialisation with lambda expr ----------
(defcl Carray-init (Cinstr) (var) (body) (i))
(defun Carray-init (var body i)
  (make-instance 'Carray-init :var var :body body :i i))

;; -------- Record initialisation with field spec ----------
(defcl Crecord-init (Cinstr) (var))
(defun Crecord-init (var)
  (make-instance 'Crecord-init :var var))



;; --------------- Return instruction ---------------
(defcl Creturn (Cinstr) (var))
(defun Creturn (var) (make-instance 'Creturn :var var))


;; ---- Creates a copy of varB and name it varA --------------
(defcl Ccopy (Cinstr) (varA) (varB) (safe))
(defun Ccopy (varA varB)
  (make-instance 'Ccopy :varA varA :varB varB :safe nil))


;; Takes a C instruction (list) with a hole and return a list of string
(defun create-loop (body size &optional (prefix "i") (start 0))
  (let* ((i (gen-C-var *C-int* prefix))
	 (for-body (get-C-instructions (define-name body i))))
    (when for-body
      (append
       (list (format nil "int ~a;" i)
	     (format nil "for(~a = ~a; ~a < ~a; ~a++) {" i start i size i))
       (indent for-body)
       (list "}")))))




;; --------------------------------------------------------------------
;;           CLOS representation of C functions
;; --------------------------------------------------------------------

(defcl Cfun () (name) (spec-op) (destr))
(defmethod Cfun ((f Cfun) &optional spec-op destr)
  (make-instance 'Cfun :name (name f) :spec-op (spec-op f)
		       :destr (or destr (destr f))))
(defmethod Cfun (name &optional spec-op destr)
  (make-instance 'Cfun :name name :spec-op spec-op :destr destr))

(defmethod print-object ((obj Cfun) out)
  (with-slots (name destr) obj
   (format out "~a"
      (if (const-decl? name)
	  (id (C-info-definition (gethash name
					  (if (destr obj)
					      *C-destructive-hash* *C-nondestructive-hash*))))
	name))))

(defmethod get-definition ((f Cfun) &optional (destr (destr f)))
  (when (const-decl? (name f))
    (C-info-definition (gethash (name f) (C-hashtable destr)))))
(defmethod get-definition ((fc Cfuncall) &optional (destr (destr (fun fc))))
  (get-definition (fun fc) destr))

;; --------- Generating functions ----------------
(defun mp-cmp (zq op)
  (Cfun (format nil "~a_cmp" zq)
	(format nil "(~a_cmp(~~{~~a~~^, ~~}) ~a 0)" zq op)))

(defun binop (op)
  (let ((bop (if (eql op '||) "||" op)))
    (Cfun (format nil "pvs_~a" bop)
	  (format nil "(~~{~~a~~^ ~a ~~})" bop))))

(defvar *pvs-cmp* (Cfun "pvsCmp"))
(defvar *pvs-neg* (Cfun "pvs-neg" "(-~{~a~})"))


;; --------------------------------------------------------------------
;;           Managing holes in expressions
;; --------------------------------------------------------------------
(defmacro rec-dn (v arg)
  `(setf (,arg ,v) (define-name (,arg ,v) name)))

(defmethod define-name ((C-expr C-expr) name)
  (when (unnamed? C-expr)
    (rec-dn C-expr var)
    (rec-dn C-expr instr)
    (rec-dn C-expr destr))
  C-expr)

(defmethod define-name ((v C-var) name)
  (if (unnamed? v)
      (if (Ccode? name) name
	(progn (setf (name v) (format nil "~a" name)) v))
    (progn (rec-dn v name) v)))

(defmethod define-name ((e Crecord-get) name)
  (rec-dn e var)
;;  (setf (var e) (define-name (var e) name))
  e)

(defmethod define-name ((e Carray-get) name)
  (rec-dn e var) (rec-dn e arg) e)

(defmethod define-name ((e Cfuncall) name)
  (rec-dn e args) e)

(defmethod define-name ((i Cfuncall-mp) name)
  (rec-dn i Cfunc) i)

(defmethod define-name ((i Cdecl) name)
  (rec-dn i var) i)

(defmethod define-name ((i Cfree) name)
  (rec-dn i var) i)

(defmethod define-name ((i Cset) name)
  (rec-dn i var) (rec-dn i expr) i)

(defmethod define-name ((e Ccopy) name)
  (rec-dn e varA) (rec-dn e varB) e)

(defmethod define-name ((e Cif) name)
  (rec-dn e cond-part) (rec-dn e then-part) (rec-dn e else-part) e)

(defmethod define-name ((e Carray-init) name)
  (rec-dn e i) (rec-dn e var) (rec-dn e body) e)

;; Should never be called...
(defmethod define-name ((e Creturn) name) (rec-dn e var) e)

(defmethod define-name ((str string) name)
  (format nil str name))

(defmethod define-name ((lst list) name)
  (when (consp lst)
    (cons (define-name (car lst) name)
	  (define-name (cdr lst) name))))

(defmethod define-name (e name) e)


;; --------------------------------------------------------------------
;;           C code generation
;; --------------------------------------------------------------------

(defmethod print-object ((obj C-var) out)
  (let ((format1 (if (and *Cshow-safe* (safe  obj)) "*~a*" "~a"))
	(format2 (if (and *Cshow-dupl* (dupl? obj)) "#~a#" "~a")))
  (format out "~a"
  (format nil format1
  (format nil format2
	  (if (name obj) (name obj) "~a"))))))

(defmethod print-object ((obj Carray-get) out)
  (format out "~a[~a]" (var obj) (arg obj)))

(defmethod print-object ((obj Crecord-get) out)
  (format out "~a->~a" (var obj) (arg obj)))

(defmethod print-object ((obj Cfuncall) out)
  (with-slots (fun args) obj
    (format out
	    (if (spec-op fun) (spec-op fun)
	      (format nil "~a(~~{ ~~a ~~^,~~})" fun))
	    (mapcar #'GC args))))

;; -------- Function to print an expression with GC increment if needed ------
(defun GC (v)
  (if (and (pointer? v)
	   (not (Cfuncall? v))
	   (not (and (C-var? v) (safe v))))
      (format nil "GC( ~a )" v)
    v))

;; ---- Default print function for Cinstr --------
(defmethod print-object ((obj Cinstr) out)
  (format out "~{~a~^~%~}" (get-C-instructions obj)))


;; ------------ C lines of code are generated here ----------
(defmethod get-C-instructions ((instr Cdecl))
  (let ((var (var instr)))
    (cons (format nil "~:[~;!~]~a ~a;" (and *Cshow-bang* (bang? var)) (type var) var)
	  (get-C-instructions (Cinit var)))))

;; Return a Cfuncall-mp instruction to init gmp var when needed
(defun Cinit (v)
  (cond ((C-mpz? (type v)) (Cfuncall-mp "mpz_init" v))
	((C-mpq? (type v)) (Cfuncall-mp "mpq_init" v))
	(t nil)))

;; ----------- C variables memory deallocation -------------
(defmethod get-C-instructions ((instr Cfree))
  (let* ((var (var instr))
	 (type (type var)))
    (cond ((C-mpz? type) (list (format nil "mpz_clear(~a);" var)))
	  ((C-mpq? type) (list (format nil "mpq_clear(~a);" var)))
	  ((C-pointer? type)
	   (let ((arg (if (C-struct? type)
			   (get-C-instructions
			    (mapcar #'(lambda (x) (Cfree (Crecord-get var (car x))))
				    (args type)))
			 (create-loop (Cfree (Carray-get var (C-var *C-int*)))
				      (size type)))))
	     (append (when arg (append (list (format nil "if( GC_count( ~a ) == 1 ) {" var))
				       (indent arg)
				       (list "}")))
		     (list (format nil "GC_free(~a);" var)))))
	  (t nil))))

(defmethod get-C-instructions ((instr Cfuncall-mp))
  (list (format nil "~a;" (Cfunc instr))))

(defmethod get-C-instructions ((instr Creturn))
  (list (format nil "return ~a;" (var instr))))

(defmethod get-C-instructions ((instr Cif))
  (append (list (format nil "if (~a) {" (cond-part instr)))
	  (indent (get-C-instructions (then-part instr)))
	  (list "} else {")
	  (indent (get-C-instructions (else-part instr)))
	  (list "}")))

(defmethod get-C-instructions ((instr Carray-init))
  (with-slots (body i) instr
  (let* ((var (var instr))
	 (type (type var))
	 (l (size type)))
    (append (list (format nil "~a = GC_malloc(~a, sizeof(~a) );" var l (target type))
		  (format nil "int ~a;" i)
		  (format nil "for(~a = 0; ~a < ~a; ~a++) {" i i l i))
	    (indent (get-C-instructions (list (Cinit (Carray-get var i))  body)))
	    (list "}")))))

(defmethod get-C-instructions ((instr Crecord-init))
  (let* ((var    (var instr))
	 (type   (type var)))
    (cons (format nil "~a = GC_malloc(1, sizeof( struct struct_~a ) );" var type)
	  (get-C-instructions (mapcar
			       #'(lambda (x) (Cinit (Crecord-get var (car x))))
			       (args type))))))

(defmethod get-C-instructions ((instr list))
  (when (consp instr)
    (append (get-C-instructions (car instr))
	    (get-C-instructions (cdr instr)))))

(defmethod get-C-instructions (instr)
  (list (format nil "Unknown instruction  : ~a" instr)))




;; --------------------------------------------------------------------
;;           C code generation for updates (Ccopy objects)
;; --------------------------------------------------------------------
(defmethod get-C-instructions ((instr Cset))
  (let* ((varA (var  instr)) (typeA (type varA))
	 (varB (expr instr)) (typeB (type varB)))
    (if (C-gmp? typeA)
	(mapcar #'(lambda (x) (format nil x varA varB)) (convertor typeA typeB))
      (list (format nil "~a = ~a;" varA
		    (format nil (convertor typeA typeB)
			    (GC varB)))))))

(defmethod get-C-instructions ((instr Ccopy))
  (let* ((varA (varA instr))
	 (typeA (type varA))
	 (varB (varB instr))
	 (typeB (type varB)))
    (cond ((C-gmp? typeA)
	   (mapcar #'(lambda (x) (format nil x varA varB)) (convertor typeA typeB)))
	  ((C-pointer? typeA)
	   (get-bang-copy typeA varA typeB varB instr))
	  (t (list (format nil "~a = ~a;" varA
			   (format nil (convertor typeA typeB) varB)))))))

(defmethod get-bang-copy ((typeA C-array) varA typeB varB instr)
  (cond ((not (safe varB))    ;; If we can't reuse B because it appears later in the code
	 (append (list (format nil "~a = GC_malloc(~a, sizeof(~a) );"
			       varA (size typeA) (target typeA)))
		 (create-loop (Ccopy (Carray-get varA (C-var *C-int*))
				     (Carray-get varB (C-var *C-int*)))
			      (size typeB))))
	;; ((bang? varB) ;; If B never appears later and is of type bang
	;;  (list (format nil "~a = GC( ~a );" varA varB)))  ;; Should not happend...
	(t ;; If B never appears later but is not bang (a priori). We check the GC
	 (append
	  (list (format nil "if ( GC_count( ~a ) == 1 )" varB)
		(format nil "  ~a = ~a;" varA  (GC varB))
		(format nil "else {")
		(format nil "  ~a = GC_malloc(~a, sizeof(~a) );"
			varA (size typeA) (target typeA)))
	  (indent (create-loop (list (Cinit (Carray-get varA (C-var *C-int*)))
				     (Cset  (Carray-get varA (C-var *C-int*))
					    (Carray-get varB (C-var *C-int*))))
			       (size typeB)))
	  (list "}")))))

(defmethod get-bang-copy ((typeA C-struct) varA typeB varB instr)
  (cond ((not (safe varB))    ;; If we can't reuse B because it appears later in the code
	 (get-C-instructions
	  (cons (Crecord-init varA)
		(loop for a in (args typeA) collect
		      (Ccopy (Crecord-get varA (car a))
			     (Crecord-get varB (car a)))))))
	;; ((bang varB) ;; If B never appears later and is of type bang
	;;  (list (format nil "~a = GC( ~a );" varA varB)))
	(t ;; If B never appears later but is not bang (a priori). We check the GC
	 (append
	  (list (format nil "if ( GC_count( ~a ) == 1 )" varB)
		(format nil "  ~a = ~a;" varA (GC varB))
		(format nil "else {")
		(format nil "  ~a = GC_malloc(1, sizeof( struct struct_~a ));" varA typeA))
	  (indent (get-C-instructions
		   (loop for a in (args typeA)
			 collect (Cinit (Crecord-get varA (car a)))
			 collect (Cset  (Crecord-get varA (car a))
				        (Crecord-get varB (car a))))))
	  (list "}")))))



(defun invisible? (e) (null (get-C-instructions e)))
