;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-

;------------------------------------------------------------------------------
; PRMACROS - Macros for the prover subsystem
;------------------------------------------------------------------------------


(defmacro newcontext (form)
  `(let ((sigalist sigalist) (findalist findalist) (usealist usealist))
     (catch 'context 
       ,form )))

(defmacro retfalse () `(throw 'context 'false))

; term accessor macros

(defmacro funsym (term) `(car ,term))

(defmacro arg1 (term) `(cadr ,term))

(defmacro arg2 (term) `(caddr ,term))

(defmacro arg3 (term) `(cadddr ,term))

(defmacro argsof (term) `(cdr ,term))

(defmacro lside (term) `(cadr ,term))

(defmacro rside (term) `(caddr ,term))

; getq:
; (getq arg alist) returns the item associated with arg in alist, or nil.

(defmacro getq (arg alist) `(cdr (assq ,arg ,alist)))

(defun prerr (&rest args)
  (apply #'error args))

(defconstant *truecons* '(true))

(defconstant *eqarithrels* '(greatereqp lesseqp))

(defconstant *ifops* '(IF IF*))

(defconstant *boolconstants* '(false true))

(defconstant *arithrels* '(lessp lesseqp greaterp greatereqp))

(defconstant *arithops* '(PLUS TIMES DIFFERENCE MINUS))

(defconstant *boolops* '(and or implies not IF iff))

(defmacro singleton? (obj)
  `(and (consp ,obj) (null (cdr ,obj))))


(defun add2pot (atoms)
  (setq s (append atoms s))
  nil)

(defmacro addneq2pot (atoms)
  `(add2pot ,atoms))

(defmacro addineq2pot (atoms)
  `(add2pot ,atoms))

(defmacro addprm2pot (atoms)
  `(add2pot ,atoms))

(defmacro arithlist (x)
  `(let ((xx ,x))
     (if (and (consp xx)
	   (qnumberp (car xx)))
      (cdr xx)
      xx)))
