;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 23:19:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:14:07 1998
;; Update Count    : 3
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defvar *use-pp* t)

;;; Controls whether to pay attention to chained declarations
(defvar *pretty-printing-decl-list* nil)

(defvar *pretty-printed-prefix* nil)

(defvar *unparse-expanded* nil)

;;; Unparse takes the following keywords:
;;; :string :stream :char-width :file :sb-tex :style

(defmethod unparse :around (obj &rest keys)
  (if *use-pp*
      (let ((*default-char-width* (or (cadr (memq :char-width keys))
				      *default-char-width*))
	    (*print-pretty* (or (not (memq :pretty keys))
				(cadr (memq :pretty keys)))))
	(cond ((memq :sb-tex keys)
	       (call-next-method))
	      ((memq :string keys)
	       (with-output-to-string (*standard-output*)
		 (pp obj)))
	      ((memq :stream keys)
	       (let ((*standard-output* (cadr (memq :stream keys))))
		 (pp obj)))
	      ((memq :file keys)
	       (with-open-file (*standard-output* (cadr (memq :file keys))
						  :direction :output)
		 (pp obj)))
	      (t (pp obj))))
      (call-next-method)))

(defun pp (obj)
  (let ((*print-escape* nil)
	(*print-level* nil)
	(*print-length* nil)
	(*print-right-margin* *default-char-width*))
    ;;(setf (slot-value *standard-output* 'excl::charpos) 0)
    (pp* obj)))

;(defmethod pp* :around ((syn syntax))
;  (call-next-method)
;  ;; need to deal with comments here
;  )

;;; Module level

(defmethod pp* ((theories modules))
  (pprint-logical-block (nil (modules theories))
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (terpri) (terpri))))

;(defmethod pp* ((mod datatype-or-module))
;  (call-next-method))

(defmethod pp* ((mod module))
  (with-slots (id formals exporting assuming theory) mod
    (pprint-logical-block (nil nil)
      (write id)
      (pp-theory-formals formals)
      (write-char #\:)
      (write-char #\space)
      (pprint-indent :block 2)
      (write 'THEORY)
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pp* exporting)
      (write 'BEGIN)
      (pprint-indent :block 2)
      (pprint-newline :mandatory)
      (pp-assuming assuming)
      (pp-theory theory)
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (write 'END)
      (write-char #\space)
      (write id))))

(defun pp-theory-formals (formals)
  (when formals
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block (nil formals :prefix "[" :suffix "]")
	(loop (let ((fml (pprint-pop)))
		(if (typep fml 'using)
		    (pprint-logical-block (nil nil :prefix "(" :suffix ")")
		      (pp* fml))
		    (pp* fml))
		(pprint-exit-if-list-exhausted)
		(unless (typep fml 'using)
		  (write-char #\,))
		(write-char #\space)
		(pprint-newline :fill)))))))

(defmethod pp* ((exp exporting))
  (with-slots (kind names but-names modules) exp
    (unless (eq kind 'default)
      (pprint-logical-block (nil nil)
	(write 'EXPORTING)
	(write-char #\space)
	(pprint-indent :current 0)
	(pp-exportings names but-names)
	(when (or kind modules)
	  (pprint-indent :block 4)
	  (pprint-newline :fill)
	  (pp-exportingmods kind modules))))))

(defun pp-exportings (names but-names)
  (pprint-logical-block (nil names)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill)))
  (when but-names
    (write-char #\space)
    (write 'BUT)
    (write-char #\space)
    (pprint-logical-block (nil but-names)
      (loop (pprint-indent :current 2)
	    (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)))))

(defun pp-exportingmods (kind modules)
  (pprint-logical-block (nil modules)
    (write-char #\space)
    (write 'WITH)
    (write-char #\space)
    (if kind
	(write kind)
	(loop (pprint-indent :current 2)
	      (pp* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)))))

(defmethod pp* ((name expname))
  (with-slots (id kind type) name
    (pprint-logical-block (nil nil)
      (write id)
      (when kind
	(write-char #\:)
	(write-char #\space)
	(if (symbolp kind)
	    (write kind)
	    (pp* type))))))

(defun pp-assuming (assuming)
  (when assuming
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block (nil assuming)
	(write 'ASSUMING)
	(pprint-indent :block 1)
	(pprint-newline :mandatory)
	(loop (pp* (pprint-pop))
	      (pprint-exit-if-list-exhausted))
	(write 'ENDASSUMING)))))

(defun pp-theory (theory)
  (let ((*pretty-printing-decl-list* t)
	(last-one (car (last theory))))
    (pprint-logical-block (nil theory)
      (pprint-newline :mandatory)
      (loop (let ((decl (pprint-pop)))
	      (pp* decl)
	      (unless (or (and (typep decl 'declaration)
			       (chain? decl))
			  (eq decl last-one))
		(pprint-newline :mandatory)))
	    (pprint-exit-if-list-exhausted)))))

(defmethod pp* ((dt datatype))
  (with-slots (id formals using assuming constructors) dt
    (pprint-logical-block (nil nil)
      (write id)
      (pp-theory-formals formals)
      (write-char #\:)
      (write-char #\space)
      (pprint-indent :block 2)
      (write 'DATATYPE)
      (pprint-indent :block 1)
      (when (typep dt 'datatype-with-subtypes)
	(write-char #\space)
	(write 'WITH)
	(write-char #\space)
	(write 'SUBTYPES)
	(write-char #\space)
	(pprint-logical-block (nil (subtypes dt))
	  (loop (pp* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space))))
      (pprint-newline :mandatory)
      (write 'BEGIN)
      (pprint-indent :block 2)
      (pprint-newline :mandatory)
      (when using
	(pp* using)
	(pprint-newline :mandatory))
      (pp-assuming assuming)
      (pp-constructors constructors)
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (write 'END)
      (write-char #\space)
      (write id))))

;;; (defmethod pp* ((dt datatype-with-subtype)))

;;; (defmethod pp* ((dt inline-datatype)))

;;; (defmethod pp* ((dt inline-datatype-with-subtype)))

(defun pp-constructors (constructors)
  (pprint-logical-block (nil constructors)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :mandatory))))

(defmethod pp* ((constr adt-constructor))
  (with-slots (id arguments recognizer) constr
    (pprint-logical-block (nil nil)
      (write id)
      (when arguments
	(pprint-logical-block (nil arguments :prefix "(" :suffix ")")
	  (loop (pp* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill))))
      (write-char #\:)
      (write-char #\space)
      (write recognizer)
      (when (typep constr 'constructor-with-subtype)
	(write-char #\:)
	(write-char #\space)
	(pp* (subtype constr))))))

(defmethod pp* ((ad adtdecl))
  ;; The around method for declarations would have already printed the id:
  (with-slots (declared-type) ad
    (pp* declared-type)))

;;; (defmethod pp* ((constr simple-constructor)))

;;; (defmethod pp* ((constr constructor-with-subtype)))


;;; Declarations

(defmethod pp* ((use using))
  (with-slots (modules) use
    (pprint-logical-block (nil modules)
      (write 'IMPORTING)
      (write-char #\space)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod pp* :around ((decl declaration))
  (with-slots (id formals chain? semi) decl
    (when (or *unparse-expanded*
	      *adt*
	      (not (generated-by decl)))
      (pprint-logical-block (nil nil)
	(cond ((and chain?
		    *pretty-printing-decl-list*)
	       (write id)
	       (write-char #\,)
	       (write-char #\space))
	      (t (when (newline-comment decl)
		   (write (car (newline-comment decl)))
		   (write-char #\space)
		   (pprint-newline :mandatory))
		 (when (and *comment-on-proof-status*
			    (tcc? decl))
		   (format t "  % ~a~%" (proof-status-string decl)))
		 (write id)
		 (pp-decl-formals formals)
		 (write-char #\:)
		 (write-char #\space)
		 (pprint-newline :miser)
		 (call-next-method)
		 (when semi (write-char #\;))
		 (unless (typep decl '(or formal-decl adtdecl))
		   (pprint-newline :mandatory))))))))

(defun pp-decl-formals (formals)
  (when formals
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block (nil formals)
	(loop (pp-lambda-formal (pp-chained-decls (pprint-pop)) nil)
	      (pprint-exit-if-list-exhausted)
	      (pprint-newline :fill))))))

;;; Need this as a primary method
(defmethod pp* ((decl declaration))
  nil)

;; (defmethod pp* ((decl nonempty-type-decl)))

(defmethod pp* ((decl type-decl))
  (with-slots (type-expr chain?) decl
    (if (typep decl 'nonempty-type-decl)
	(write (case (keyword decl)
		 (nonempty_type 'NONEMPTY_TYPE)
		 (t 'TYPE+)))
	(write 'TYPE))
    (when (typep decl 'type-def-decl)
      (write-char #\space)
      (pprint-newline :miser)
      (if (typep decl 'type-eq-decl)
	  (write-char #\=)
	  (write 'FROM))
      (write-char #\space)
      (pprint-newline :fill)
      (pp* type-expr))))
      

;; (defmethod pp* ((decl nonempty-type-def-decl)) )

;; (defmethod pp* ((decl type-eq-decl)) )

;; (defmethod pp* ((decl nonempty-type-eq-decl)) )

;; (defmethod pp* ((decl type-from-decl)) )

;; (defmethod pp* ((decl nonempty-type-from-decl)) )

;; (defmethod pp* ((decl formal-decl)) )

(defmethod pp* ((decl formal-type-decl))
  (with-slots (type-expr) decl
    (if (typep decl 'formal-nonempty-type-decl)
	(write (case (keyword decl)
		 (nonempty_type 'NONEMPTY_TYPE)
		 (t 'TYPE+)))
	(write 'TYPE))
    (when (typep decl 'formal-subtype-decl)
      (write-char #\space)
      (write 'FROM)
      (write-char #\space)
      (pp* type-expr))))

;; (defmethod pp* ((decl formal-nonempty-type-decl)) )

;; (defmethod pp* ((decl formal-subtype-decl)) )

;; (defmethod pp* ((decl formal-nonempty-subtype-decl)) )

(defmethod pp* ((decl formal-const-decl))
  (with-slots (declared-type) decl
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)))

(defmethod pp* ((decl lib-decl))
  (with-slots (lib-string) decl
    (write 'library)
    (when (typep decl 'lib-eq-decl)
      (write-char #\space)
      (write-char #\=))
    (write-char #\space)
    (pprint-newline :fill)
    (write lib-string)))

;; (defmethod pp* ((decl lib-eq-decl)) )

(defmethod pp* ((decl mod-decl))
  (with-slots (modname) decl
    (write 'library)
    (when (typep decl 'lib-eq-decl)
      (write-char #\=))
    (write-char #\space)
    (pprint-newline :fill)
    (pp* modname)))

(defmethod pp* ((decl var-decl))
  (with-slots (declared-type) decl
    (write 'VAR)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)))

(defmethod pp* ((decl const-decl))
  (with-slots (declared-type definition) decl
    (pprint-newline :fill)
    (pp* declared-type)
    (when definition
      (write-char #\space)
      (write #\=)
      (pprint-indent :block 4)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* definition)
      (pprint-indent :block 2))))

;; (defmethod pp* ((decl proj-decl)) )

(defmethod pp* ((decl def-decl))
  (with-slots (declared-type definition declared-measure ordering) decl
    (write 'RECURSIVE)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)
    (write-char #\space)
    (write #\=)
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* definition)
    (pprint-indent :block 3)
    (pprint-newline :mandatory)
    (write 'MEASURE)
    (write-char #\space)
    (pp* declared-measure)
    (when ordering
      (write-char #\space)
      (pprint-newline :fill)
      (write 'BY)
      (write-char #\space)
      (pp* ordering))
    (pprint-indent :block 2)))

(defmethod pp* ((decl adt-def-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)
    (when definition
      (write-char #\space)
      (write #\=)
      (pprint-indent :block 4)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* definition)
      (pprint-indent :block 2))))

(defmethod pp* ((decl inductive-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (write 'INDUCTIVE)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)
    (write-char #\space)
    (write #\=)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* definition)))

(defmethod pp* ((decl formula-decl))
  (with-slots (spelling definition) decl
    (write spelling)
    (write-char #\space)
    (pprint-indent :block 4)
    (pprint-newline :fill)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* definition)
    (pprint-indent :block 2)))

(defmethod pp* :around ((decl name-judgement))
  (with-slots (name chain? declared-type) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* name)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (write 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (pprint-indent :block 2)
	     (setq *pretty-printed-prefix* nil)))
    (pprint-newline :mandatory)))

(defmethod pp* :around ((decl application-judgement))
  (with-slots (name chain? declared-type) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* name)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (write 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (pprint-indent :block 2)
	     (setq *pretty-printed-prefix* nil)))
    (pprint-newline :mandatory)))

(defmethod pp* :around ((decl number-judgement))
  (with-slots (number chain? declared-type) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* number)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (write-char #\space)
	     (write 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (setq *pretty-printed-prefix* nil))))
    (pprint-newline :mandatory))

(defmethod pp* :around ((decl subtype-judgement))
  (with-slots (declared-subtype chain? declared-type) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* declared-subtype)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (write-char #\space)
	     (write 'SUBTYPE_OF)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (setq *pretty-printed-prefix* nil)))
    (pprint-newline :mandatory)))

(defmethod pp* :around ((decl conversion-decl))
  (with-slots (name chain?) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'CONVERSION)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* name)
    (when (typep decl 'typed-conversion-decl)
      (write-char #\:)
      (write-char #\space)
      (pp* (declared-type decl)))
    (when (and chain?
	       *pretty-printing-decl-list*)
      (write-char #\,)
      (write-char #\space))
    (pprint-newline :mandatory)))


;;; Type expressions

(defmethod pp* :around ((te type-expr))
  (if (print-type te)
      (pp* (print-type te))
      (progn (dotimes (p (parens te))
	       (write-char #\[))
	     (call-next-method)
	     (dotimes (p (parens te))
	       (write-char #\])))))

(defmethod pp* ((te type-application))
  (with-slots (type parameters) te
    (pprint-logical-block (nil nil)
      (pp* type)
      (pp-arguments parameters))))

(defun pp-arguments* (args)
  (pprint-logical-block (nil args)
    (loop (pprint-indent :current 2)
	  (let ((nextargs (pprint-pop)))
	    (if (and (singleton? nextargs)
		     (typep (car nextargs) '(or field-assign proj-assign)))
		(pp* (car nextargs))
		(pp-arguments nextargs)))
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :fill))))

(defmethod pp* ((ex field-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (write (id ex))))

(defmethod pp* ((ex proj-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (write (number ex))))

(defun pp-arguments (args)
  (pprint-logical-block (nil args :prefix "(" :suffix ")")
    (pprint-indent :current 1)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

(defmethod pp* ((te subtype))
  (with-slots (supertype predicate contains) te
    (let* ((bindings (if (typep (predicate te) 'binding-expr)
			 (bindings (predicate te))
			 (let* ((id (make-new-variable '|x| te))
				(bd (mk-bind-decl id
				      (or (print-type (supertype te))
					  (supertype te)))))
			   (list bd))))
	   (expr (if (typep (predicate te) 'binding-expr)
		     (expression (predicate te))
		     (let ((var (mk-name-expr (id (car bindings)))))
		       (mk-application (predicate te) var)))))
      (pprint-logical-block (nil nil :prefix "{" :suffix "}")
	(pp-bindings bindings)
	(pprint-indent :block 8)
	(write-char #\space)
	(write-char #\|)
	(write-char #\space)
	(pprint-newline :fill)
	(pp* expr))
      (when contains
	(write-char #\space)
	(write 'CONTAINING)
	(write-char #\space)
	(pp* contains)))))

(defmethod pp* ((te expr-as-type))
  (with-slots (expr contains) te
    (write-char #\()
    (pp* expr)
    (write-char #\))
    (when contains
      (write-char #\space)
      (write 'CONTAINING)
      (write-char #\space)
      (pp* contains))))

(defmethod pp* ((te recordtype))
  (with-slots (fields) te
    (pprint-logical-block (nil fields :prefix "[# " :suffix " #]")
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :linear)))))

(defmethod pp* ((te funtype))
  (with-slots (domain range) te
    (pprint-logical-block (nil nil :prefix "[" :suffix "]")
      (pprint-indent :current 2)
      (pp-funtype domain range)
      (pprint-indent :block 0))))

(defmethod pp-funtype (domain range)
  (pp* domain)
  (write-char #\space)
  (write "->")
  (write-char #\space)
  (pprint-newline :fill)
  (pp* range))

(defmethod pp-funtype ((domain domain-tupletype) range)
  (with-slots (types) domain
    (pprint-logical-block (nil types)
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))
    (write-char #\space)
    (write "->")
    (write-char #\space)
    (pprint-newline :fill)
    (pp* range)))

(defmethod pp-funtype ((domain dep-binding) range)
  (with-slots (id declared-type) domain
    (if (typep declared-type 'dep-domain-tupletype)
	(let* ((types (types declared-type))
	       (bindings (var-bindings declared-type))
	       (tbindings bindings)
	       (*parsing-or-unparsing* t))
	  (pprint-logical-block (nil types)
	    (pprint-indent :current 0)
	    (loop (let ((nty (pprint-pop))
			(nbd (pop tbindings)))
		    (if (typep nty 'dep-binding)
			(pp* nty)
			(pprint-logical-block (nil nil)
			  (write (car nbd))
			  (write-char #\:)
			  (write-char #\space)
			  (pprint-newline :fill)
			  (pp* nty))))
		  (pprint-exit-if-list-exhausted)
		  (write-char #\,)
		  (write-char #\space)
		  (pprint-newline :fill)))
	  (write-char #\space)
	  (write "->")
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp* (gensubst range
		 #'(lambda (ex)
		     (let ((var (car (rassoc (index ex) bindings))))
		       (make-instance 'name-expr
			 'id var)))
		 #'(lambda (ex)
		     (and (typep ex 'projection-application)
			  (typep (argument ex) 'name-expr)
			  (eq (id (argument ex)) (id domain)))))))
	(call-next-method))))

(defmethod pp* ((te tupletype))
  (with-slots (types) te
    (pprint-logical-block (nil types :prefix "[" :suffix "]")
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod pp-domain ((te domain-tupletype))
  (with-slots (types) te
    (pprint-logical-block (nil types)
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod pp-domain ((te dep-domain-tupletype))
  (with-slots (types) te
    (pprint-logical-block (nil types)
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))


;;; Expressions

(defmethod pp* :around ((ex expr))
  (if (typep ex 'binding)
      (call-next-method)
      (progn (dotimes (p (parens ex))
	       (write-char #\())
	     (call-next-method)
	     (dotimes (p (parens ex))
	       (write-char #\))))))

(defmethod pp* ((ex number-expr))
  (write (number ex)))

(defmethod pp* ((ex string-expr))
  (unless (string-value ex)
    (setf (string-value ex) (xf-string-expr (argument ex))))
  (write (string-value ex) :escape t))

(defmethod pp* ((ex list-expr))
  (pprint-logical-block (nil (exprs ex) :prefix "(: " :suffix " :)")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((ex null-expr))
  (write "(: :)"))

(defmethod pp* ((ex bracket-expr))
  (pprint-logical-block (nil (arguments ex) :prefix "[| " :suffix " |]")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((ex record-expr))
  (with-slots (assignments) ex
    (pprint-logical-block (nil assignments :prefix "(# " :suffix " #)")
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :linear)))))

(defmethod pp* ((ex tuple-expr))
  (with-slots (exprs) ex
    (pp-arguments exprs)))

(defmethod pp* ((ex projection-application))
  (with-slots (id argument) ex
    (write id)
    (pp-arguments (argument-list argument))))

(defmethod pp* ((ex projappl))
  (pprint-logical-block (nil nil)
    (pp* (argument ex))
    (write-char #\`)
    (write (index ex))))

(defmethod pp* ((ex field-application))
  (with-slots (id argument) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write id)
      (pprint-newline :fill)
      (pp-arguments (argument-list argument)))))

(defmethod pp* ((ex fieldappl))
  (pprint-logical-block (nil nil)
    (pp* (argument ex))
    (write-char #\`)
    (write (id ex))))

(defmethod pp* ((ex application))
  (with-slots (operator argument) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current (if (typep operator 'name-expr)
				  (min (- (length (string (id operator))) 2) 6)
				  6))
      (pp* operator)
      (pprint-newline :miser)
      (pp-arguments (argument-list argument)))))

(defvar *pp-infix-indents* 0)

(defmethod pp* ((ex infix-application))
  (with-slots (operator argument) ex
    (cond ((and (typep operator 'name-expr)
		(memq (id operator) *infix-operators*)
		(typep argument 'tuple-expr)
		(= (length (exprs argument)) 2))
	   (set-parens-if-needed ex)
	   (pprint-logical-block (nil nil)
	     (let ((*pp-infix-indents* (+ *pp-infix-indents* 2)))
	       (pprint-indent :block *pp-infix-indents*)
	       (let ((col (pp-column)))
		 (pp* (args1 ex))
		 (write-char #\space)
		 (unless (< (- (pp-column) col) *pp-infix-indents*)
		   (pprint-newline :fill))))
	     (pprint-logical-block (nil nil)
	       (pprint-indent :block 2)
	       (pp* (id operator))
	       (write-char #\space)
	       (if (>= (length (string (id operator))) 2)
		   (pprint-newline :mandatory)
		   (pprint-newline :miser))
	       (pp* (args2 ex)))))
	  (t (call-next-method)))))

(defun pp-column ()
  ;;(excl::charpos *standard-output*)
  ;;(slot-value *standard-output* 'excl::charpos)
  ;;(slot-value *standard-output* 'excl::buffer-offset)
  ;;(slot-value *standard-output* 'excl::buffpos)
  (slot-value *standard-output* 'excl::buffer-ptr)
  )

(defmethod set-parens-if-needed ((ex infix-application))
  (with-slots (operator argument) ex
    (let ((lhs (car (exprs argument)))
	  (rhs (cadr (exprs argument)))
	  (oper (sbst-symbol (id operator))))
      (when (and (zerop (parens lhs))
		 (< (precedence lhs 'left)
		    (gethash oper (second *expr-prec-info*))))
	(setf (parens lhs) 1))
      (when (and (zerop (parens rhs))
		 (< (precedence rhs 'right)
		    (gethash oper (third *expr-prec-info*))))
	(setf (parens rhs) 1)))))

(defmethod pp* ((ex unary-application))
  (with-slots (operator argument) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write (id operator))
      (when (valid-pvs-id* (id operator))
	(write-char #\space)
	(pprint-newline :miser))
      (pp* argument))))

(defmethod pp* ((ex binding-application))
  (with-slots (operator argument) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write (id operator))
      (write-char #\!)
      (write-char #\space)
      (pprint-newline :miser)
      (pp-lambda-formals argument)
      (write-char #\:)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* (expression argument)))))

(defmethod pp* ((ex when-expr))
  (with-slots (operator argument) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* (args2 ex))
      (write-char #\space)
      (pprint-newline :miser)
      (write operator)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* (args1 ex)))))

(defmethod pp* ((ex if-expr))
  (pprint-logical-block (nil nil)
    (pprint-indent :current 2)
    (write 'if)
    (write-char #\space)
    (pp* (condition ex))
    (write-char #\space)
    (pprint-newline :linear)
    (write 'then)
    (write-char #\space)
    (pp* (then-part ex))
    (write-char #\space)
    (pprint-indent :block 0)
    (pprint-newline :linear)
    (cond ((typep (else-part ex) 'chained-if-expr)
	   (write 'els))
	  (t (write 'else)
	     (write-char #\space)))
    (pp* (else-part ex))
    (unless (typep (else-part ex) 'chained-if-expr)
      (write-char #\space)
      (pprint-newline :linear)
      (write 'endif))))

(defmethod pp* ((ex chained-if-expr))
  (pprint-logical-block (nil nil)
    (pprint-indent :current 2)
    (write 'if)
    (write-char #\space)
    (pp* (condition ex))
    (write-char #\space)
    (pprint-newline :fill)
    (write 'then)
    (write-char #\space)
    (pp* (then-part ex))
    (write-char #\space)
    (pprint-indent :block 0)
    (pprint-newline :linear)
    (write 'else)
    (write-char #\space)
    (pp* (else-part ex))
    (write-char #\space)
    (pprint-newline :linear)
    (write 'endif)))

(defmethod pp* ((ex coercion))
  (with-slots (argument operator) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* argument)
      (write-char #\:)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* (declared-type (car (bindings operator)))))))

(defmethod pp* ((ex binding-expr))
  (let ((*pretty-printing-decl-list* t))
    (pprint-logical-block (nil nil)
      (unless (chain? ex)
	(write (operator ex))
	(write-char #\space)
	(pprint-indent :current 0)
	(pprint-newline :miser)
	(pp-lambda-formals ex)
	(write-char #\:)
	(write-char #\space))
      (pprint-indent :block 2)
      (pprint-newline :fill)
      (pp* (expression ex)))))

(defmethod pp* ((ex set-expr))
  (with-slots (bindings expression) ex
    (pprint-logical-block (nil nil :prefix "{" :suffix "}")
      (pprint-indent :current 2)
      (pp-bindings bindings)
      (write-char #\space)
      (pprint-newline :fill)
      (write-char #\|)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* expression))))

(defmethod pp* ((ex let-expr))
  (multiple-value-bind (let-bindings expr)
      (get-let-bindings ex)
    (pprint-logical-block (nil nil)
      (write 'let)
      (write-char #\space)
      (pp-let-bindings let-bindings)
      (write-char #\space)
      (pprint-indent :block 2)
      (pprint-newline :fill)
      (write 'in)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* expr))))

(defun pp-let-bindings (let-bindings)
  (pprint-logical-block (nil let-bindings)
    (loop (let ((lb (pprint-pop)))
	    (pp-bindings (car lb))
	    (write-char #\space)
	    (write #\=)
	    (write-char #\space)
	    (pp* (cdr lb)))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((ex where-expr))
  (multiple-value-bind (where-bindings expr)
      (get-where-bindings ex)
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* expr)
      (write-char #\space)
      (pprint-newline :fill)
      (write 'where)
      (write-char #\space)
      (pp-let-bindings where-bindings))))

(defmethod pp* ((ex update-expr))
  (with-slots (expression assignments) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* expression)
      (write-char #\space)
      (pprint-newline :fill)
      (write 'with)
      (write-char #\space)
      (pprint-logical-block (nil assignments :prefix "[" :suffix "]")
	(pprint-indent :current 0)
	(loop (pp* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :linear))))))

(defmethod pp* ((ex cond-expr))
  (pp-cond-expr ex))

(defmethod pp* ((ex first-cond-expr))
  (pp-cond-expr ex))

(defmethod pp* ((ex single-cond-expr))
  (pp-cond-expr ex))

(defmethod pp* ((ex last-cond-expr))
  (pp-cond-expr ex))

(defun pp-cond-expr (ex)
  (let ((pairs (collect-cond-expr-pairs ex nil)))
    (pprint-logical-block (nil nil)
      (write 'cond)
      (write-char #\space)
      (pprint-newline :miser)
      (pprint-logical-block (nil pairs)
	(loop (let ((pair (pprint-pop)))
		(pp* (car pair))
		(write-char #\space)
		(pprint-newline :miser)
		(write "->")
		(write-char #\space)
		(pprint-newline :miser)
		(pp* (cdr pair))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :linear))))
      (write-char #\space)
      (pprint-newline :linear)
      (write 'endcond))))

(defmethod pp* ((ex else-condition))
  (write 'else))

(defmethod collect-cond-expr-pairs ((ex cond-expr) pairs)
  (with-slots (argument) ex
    (with-slots (exprs) argument
      (collect-cond-expr-pairs (caddr exprs)
			       (acons (car exprs) (cadr exprs) pairs)))))

(defmethod collect-cond-expr-pairs ((ex first-cond-expr) pairs)
  (with-slots (argument) ex
    (with-slots (exprs) argument
      (collect-cond-expr-pairs (caddr exprs)
			       (acons (car exprs) (cadr exprs) pairs)))))

(defmethod collect-cond-expr-pairs ((ex single-cond-expr) pairs)
  (with-slots (argument) ex
    (with-slots (exprs) argument
      (nreverse (acons (car exprs) (cadr exprs) pairs)))))

(defmethod collect-cond-expr-pairs ((ex last-cond-expr) pairs)
  (with-slots (argument) ex
    (with-slots (exprs) argument
      (nreverse (acons (car exprs) (cadr exprs) pairs)))))

(defmethod collect-cond-expr-pairs (ex pairs)
  (nreverse (acons 'else ex pairs)))

(defmethod pp* ((ex cases-expr))
  (with-slots (expression selections else-part) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write 'cases)
      (write-char #\space)
      (pp* expression)
      (write-char #\space)
      (pprint-newline :fill)
      (write 'of)
      (write-char #\space)
      (pprint-logical-block (nil selections)
	(loop (pp* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :linear)))
      (when else-part
	(write-char #\space)
	(pprint-newline :linear)
	(write 'else)
	(write-char #\space)
	(pp* else-part))
      (write-char #\space)
      (pprint-newline :linear)
      (write 'endcases))))
      
(defmethod pp* ((sel selection))
  (with-slots (constructor args expression) sel
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* constructor)
      (when args
	(pprint-logical-block (nil args :prefix "(" :suffix ")")
	  (loop (write (id (pprint-pop)))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill))))
      (write-char #\:)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* expression))))

(defmethod pp* ((ass assignment))
  (with-slots (arguments expression) ass
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (typep ass 'uni-assignment)
	  (pp* (caar arguments))
	  (pp-arguments* arguments))
      (write-char #\space)
      (pprint-newline :miser)
      (write ":=")
      (write-char #\space)
      (pp* expression))))

(defmethod pp* ((ass maplet))
  (with-slots (arguments expression) ass
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (typep ass 'uni-assignment)
	  (pp* (caar arguments))
	  (pp-arguments* arguments))
      (write-char #\space)
      (pprint-newline :miser)
      (write "|->")
      (write-char #\space)
      (pp* expression))))

(defmethod separator ((ass assignment))
  'ceq)

(defmethod separator ((ass maplet))
  'arr)

(defmethod pp* ((ex table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-table-expr row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp* ((ex cond-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-table-expr row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp* ((ex cases-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-table-expr row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp* ((ex let-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-table-expr row-expr col-expr row-headings col-headings table-entries)))

(defun pp-table-expr (row-expr col-expr row-headings col-headings
			       table-entries)
  (pprint-logical-block (nil nil)
    (write 'table)
    (write-char #\space)
    (pprint-newline :miser)
    (when row-expr
      (pp* row-expr))
    (when col-expr
      (write-char #\,)
      (write-char #\space)
      (pprint-newline :miser)
      (pp* col-expr))
    (pprint-indent :block 2)
    (pprint-newline :mandatory)
    (let* ((rows (if row-headings
		     (mapcar #'cons row-headings table-entries)
		     table-entries))
	   (col-widths (compute-table-column-widths
			col-headings rows)))
      (when col-headings
	(when *pp-table-hrules*
	  (format t "~v<~>  %+~{~v,,,'-<~>--+~}+"
	    (car col-widths) (cdr col-widths))
	  (pprint-newline :mandatory))
	(let ((widths col-widths))
	  (format t "~va" (+ (pop widths) 2) " ")
	  (pprint-logical-block (nil col-headings :prefix "|[ " :suffix " ]|")
	    (loop (format t "~v:a"  (pop widths) (pprint-pop))
		  (pprint-exit-if-list-exhausted)
		  (write-char #\space)
		  (write-char #\|)
		  (write-char #\space)))
	  (pprint-newline :mandatory)
	  (when *pp-table-hrules*
	    (format t "%~{~v,,,'-<~>--+~}+" col-widths)
	    (pprint-newline :mandatory))))
      (pprint-logical-block (nil rows)
	(loop (let ((row (pprint-pop))
		    (widths col-widths))
		(pprint-logical-block (nil row :prefix "| " :suffix " ||")
		  (loop (format t "~v:a" (pop widths) (pprint-pop))
			(pprint-exit-if-list-exhausted)
			(write-char #\space)
			(write-char #\|)
			(write-char #\space)))
		(when *pp-table-hrules*
		  (pprint-newline :mandatory)
		  (format t "%~{~v,,,'-<~>--+~}+" col-widths))
		(pprint-exit-if-list-exhausted)
		(pprint-newline :mandatory))))
      (pprint-indent :block 0)
      (pprint-newline :mandatory)
      (write 'endtable))))

(defun compute-table-column-widths (col-headings rows)
  (compute-table-column-widths*
   rows
   (if col-headings
       (cons 0 (mapcar #'compute-column-width col-headings))
       (mapcar #'(lambda (x) 0) (car rows)))))

(defun compute-table-column-widths* (rows widths)
  (if (null rows)
      widths
      (compute-table-column-widths*
       (cdr rows)
       (mapcar #'(lambda (w e) (max w (compute-column-width e)))
	 widths (car rows)))))

(defun compute-column-width (ex)
  (length (unparse ex :string t :char-width 1000000)))

(defmethod exprs ((ex list-expr))
  (cons (args1 ex) (exprs (args2 ex))))

(defmethod exprs ((ex null-expr))
  nil)

(defun pp-bindings (bindings)
  (pprint-logical-block (nil bindings)
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((d simple-decl))
  (with-slots (id declared-type) d
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write id)
      (when declared-type
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :fill)
	(pp* declared-type)))))

(defmethod pp* ((ex name))
  (with-slots (library mod-id actuals id) ex
    (pprint-logical-block (nil (list ex))
      (when library
	(write library)
	(write-char #\@))
      (cond (mod-id
	     (write mod-id)
	     (when actuals
	       (pp-actuals actuals))
	     (write-char #\.)
	     (write id))
	    (t
	     (write id)
	     (when actuals
	       (pp-actuals actuals)))))))

(defmethod pp* ((list list))
  (let ((*pretty-printing-decl-list* t)
	(*pretty-printed-prefix* nil))
    (dolist (elt list)
      (pp* elt))))

(defun pp-actuals (actuals)
  (pprint-logical-block (nil actuals :prefix "[" :suffix "]")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((act actual))
  (with-slots (expr) act
    (pp* expr)))

;;; Utility functions for unparsing

(defmethod get-let-bindings ((ex let-expr))
  (with-slots (operator argument) ex
    (get-let-bindings* (expression operator) (bindings operator)
		       argument nil)))

(defmethod get-let-bindings* ((ex chained-let-expr) bindings arg lbindings)
  (with-slots (operator argument) ex
    (get-let-bindings* (expression operator)
		       (bindings operator)
		       argument
		       (cons (cons bindings arg) lbindings))))

(defmethod get-let-bindings* (ex bindings arg lbindings)
  (values (nreverse (cons (cons bindings arg) lbindings))
	  ex))

(defmethod get-where-bindings ((ex where-expr))
  (with-slots (operator argument) ex
    (get-where-bindings* (expression operator) (bindings operator)
		       argument nil)))

(defmethod get-where-bindings* ((ex chained-where-expr) bindings arg lbindings)
  (with-slots (operator argument) ex
    (get-where-bindings* (expression operator)
		       (bindings operator)
		       argument
		       (cons (cons bindings arg) lbindings))))

(defmethod get-where-bindings* (ex bindings arg lbindings)
  (values (nreverse (cons (cons bindings arg) lbindings))
	  ex))

(defun pp-lambda-formals (expr)
  (pp-lambda-formal (pp-chained-decls (bindings expr))
		    (commas? expr)
		    (set-expr? (typep expr 'set-expr)))
  (when (and (typep (expression expr) 'lambda-expr)
	     (chain? (expression expr)))
    (pp-lambda-formals (expression expr))))

(defun pp-lambda-formal (bindings commas? &optional set-expr?)
  (if commas?
      (if (cdr bindings)
	  (pprint-logical-block (nil nil)
	    (if (or (cdr (car bindings))
		    (and (declared-type (caar bindings))
			 (not (typep (caar bindings) 'untyped-bind-decl))))
		(pp-lambda-adformals (car bindings))
		(if set-expr?
		    (pprint-logical-block (nil nil)
		      (write (id (caar bindings)))
		      (when (and (declared-type (caar bindings))
				 (not (typep (caar bindings)
					     'untyped-bind-decl)))
			(write-char #\:)
			(write-char #\space)
			(pp (declared-type (caar bindings)))))
		    (if (typep (caar bindings) 'untyped-bind-decl)
			(pprint-logical-block (nil nil :prefix "(" :suffix ")")
			  (write (id (caar bindings))))
			(write (id (caar bindings))))))
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)
	    (pp-lambda-formal (cdr bindings) commas? set-expr?))
	  (if (and set-expr?
		   (zerop (parens (caar bindings))))
	      (if (cdar bindings)
		  (pp-lambda-formal (mapcar #'list (car bindings))
				    commas? set-expr?)
		  (pprint-logical-block (nil nil)
		    (write (id (caar bindings)))
		    (when (declared-type (caar bindings))
		      (write-char #\:)
		      (write-char #\space)
		      (pp (declared-type (caar bindings))))))
	      (if (or (cdr (car bindings))
		      (declared-type (caar bindings)))
		  (pprint-logical-block (nil nil :prefix "(" :suffix ")")
		    (pp-paren-adformals* (car bindings)))
		  (write (id (caar bindings))))))
      (if (and set-expr?
	       (zerop (parens (caar bindings))))
	  (pprint-logical-block (nil nil)
	    (write (id (caar bindings)))
	    (when (declared-type (caar bindings))
	      (write-char #\:)
	      (write-char #\space)
	      (pp (declared-type (caar bindings)))))
	  (pp-paren-adformals bindings))))

(defun pp-paren-adformals (bindings)
  (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
    (loop (pp-paren-adformals* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

(defun pp-paren-adformals* (b &optional (parens (parens (car b))))
  (if (zerop parens)
      (pprint-logical-block (nil nil)
	(mapl #'(lambda (bb)
		  (if (or (cdr bb)
			  (not (chain? (car bb)))
			  (not (typep (car bb) 'untyped-bind-decl)))
		      (pp* (car bb))
		      (pp-bind-decl (car bb)))
		  (when (cdr bb)
		    (write-char #\,)
		    (write-char #\space)
		    (pprint-newline :fill)))
	      b))
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
	(pp-paren-adformals* b (1- parens)))))

(defun pp-lambda-adformals (bindings)
  (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
    (loop (pprint-indent :current 2)
	  (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :fill))))

;(defun pp-typed-ids (bindings)
;  (pprint-logical-block (nil bindings)
;    (loop (pprint-indent :current 2)
;	  (write (id (pprint-pop)))
;	  (pprint-exit-if-list-exhausted)
;	  (write-char #\,)
;	  (write-char #\space)
;	  (pprint-newline :linear)))
;  (write-char #\:)
;  (write-char #\space)
;  (pp* (declared-type (car (last bindings)))))

(defmethod pp* ((bd untyped-bind-decl))
  (if *in-checker*
      (call-next-method)
      (write (id bd))))

(defmethod pp* ((bd bind-decl))
  (pp-bind-decl bd))

(defun pp-bind-decl (bd)
  (pprint-logical-block (nil nil)
    (write (id bd))
    (when (and (declared-type bd)
	       (not (chain? bd)))
      (cond ((pred-bind-decl? bd)
	     (when (supertype (declared-type bd))
	       (write-char #\:)
	       (write-char #\space)
	       (pprint-newline :linear)
	       (pp* (supertype (declared-type bd))))
	     (write-char #\space)
	     (write-char #\|)
	     (write-char #\space)
	     (pp* (formula (declared-type bd))))
	    (t (pprint-indent :block 2)
	       (write-char #\:)
	       (write-char #\space)
	       (pprint-newline :linear)
	       (pp* (declared-type bd)))))))

(defun pp-chained-decls (decls &optional cdecls part)
  (if (null decls)
      (nreverse (if cdecls (cons (nreverse cdecls) part) part))
      (let ((chainp (and (typep (car decls) '(and (not arg-bind-decl)
					      (or declaration bind-decl)))
			 (chain? (car decls))
			 (or (not (typep (car decls)
					 '(or typed-declaration bind-decl)))
			     (declared-type (car decls))))))
	(pp-chained-decls
	 (cdr decls)
	 (when chainp
	   (cons (car decls) cdecls))
	 (cond (chainp
		part)
	       ((typep (car decls) 'using)
		(cons (car decls)
		      (if cdecls (cons (nreverse cdecls) part) part)))
	       ((and cdecls
		     (or (and (typep (car cdecls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car cdecls))
			      (typep (car decls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car decls))
			      (ps-eq (declared-type (car cdecls))
				     (declared-type (car decls))))
			 (and (typep (car cdecls) 'type-decl)
			      (typep (car decls) 'type-decl))))
		(cons (nreverse (cons (car decls) cdecls)) part))
	       (t (cons (list (car decls))
			(if cdecls (cons (nreverse cdecls) part) part))))))))
  

(defvar *prover-lines* nil)
(defvar *print-lines* nil)

(defun display-sform (sform sfnum stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (cond (*report-mode*
	   (unless (and (memq sform par-sforms)
			(every #'(lambda (ps)
				   (memq sform
					 (s-forms (current-goal ps))))
			       *print-descendants*))
	     (format stream "~%~V@T" *prover-indent*)
	     (format stream "{~a}   ~a" sfnum
		     (unparse-sform sform))))
	  (t
	   (format stream "~%~V@T~6@<~:[{~a~@[,~a~]}~;[~a~@[,~a~]]~]~>~:[~;~%~6<~>~]"
	     *prover-indent* (memq sform par-sforms) sfnum
	     (label sform) (label sform))
	   (unparse (seq-formula sform) :stream stream)))))

(defun unparse-sform (sform)
  (let ((*print-lines* *prover-lines*))
    (unpindent (seq-formula sform)
	       (+ 6 *prover-indent*)
	       :string T)))
  
(defmethod pp* ((ex symbol))
  (write ex))
