;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 23:19:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Jan 26 18:34:54 1999
;; Update Count    : 8
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

(defvar *pp-compact* nil)

(defvar *pp-table-hrules* t)

(defvar *pp-print-parens* nil)

(defvar pvs-prec-info (make-hash-table :test #'eq))

(mapc #'(lambda (nt) (init-prec-info nt pvs-prec-info)) '(type-expr expr))
(mapc #'(lambda (entry)
          (set-prec-info (car entry) (cadr entry) (caddr entry) (cadddr entry)
                         pvs-prec-info))
      '((type-expr sbst::[ 0 10) (type-expr jux 1 20)
        (type-expr jux 2 21) (expr sbst::<> 0 200)
        (expr sbst::[] 0 200) (expr sbst::- 0 170)
        (expr sbst::~ 0 80) (expr sbst::NOT 0 80)
        (expr jux 1 230) (expr sbst::|`| 1 220)
        (expr sbst::^^ 1 210) (expr sbst::^ 1 210)
        (expr sbst::HAS_TYPE 1 190) (expr sbst::|::| 1 190)
        (expr sbst::|:| 1 190) (expr sbst::O 1 180)
        (expr sbst::// 1 160) (expr sbst::** 1 160)
        (expr sbst::/ 1 160) (expr sbst::* 1 160)
        (expr sbst::~ 1 150) (expr sbst::++ 1 150)
        (expr sbst::- 1 150) (expr sbst::+ 1 150)
        (expr sbst::\|\| 1 140) (expr sbst::|##| 1 140)
        (expr sbst::@@ 1 140) (expr sbst::|#| 1 130)
        (expr sbst::@ 1 130) (expr sbst::IN 1 120)
        (expr sbst::WHERE 1 120) (expr sbst::WITH 1 110)
        (expr sbst::\|> 1 100) (expr sbst::<\| 1 100)
        (expr sbst::>>= 1 100) (expr sbst::<<= 1 100)
        (expr sbst::>> 1 100) (expr sbst::<< 1 100)
        (expr sbst::>= 1 100) (expr sbst::> 1 100)
        (expr sbst::<= 1 100) (expr sbst::< 1 100)
        (expr sbst::== 1 90) (expr sbst::/= 1 90)
        (expr sbst::= 1 90) (expr sbst::ANDTHEN 1 71)
        (expr sbst::&& 1 71) (expr sbst::/\\ 1 71)
        (expr sbst::& 1 71) (expr sbst::AND 1 71)
        (expr sbst::ORELSE 1 61) (expr sbst::XOR 1 61)
        (expr sbst::\\/ 1 61) (expr sbst::OR 1 61)
        (expr sbst::WHEN 1 51) (expr sbst::=> 1 51)
        (expr sbst::IMPLIES 1 51) (expr sbst::<=> 1 41)
        (expr sbst::IFF 1 41) (expr sbst::\|= 1 31)
        (expr sbst::\|- 1 31) (expr sbst::\| 1 20)
        (expr jux 2 231) (expr sbst::|`| 2 221)
        (expr sbst::^^ 2 211) (expr sbst::^ 2 211)
        (expr sbst::HAS_TYPE 2 191) (expr sbst::|::| 2 191)
        (expr sbst::|:| 2 191) (expr sbst::O 2 181)
        (expr sbst::// 2 161) (expr sbst::** 2 161)
        (expr sbst::/ 2 161) (expr sbst::* 2 161)
        (expr sbst::~ 2 151) (expr sbst::++ 2 151)
        (expr sbst::- 2 151) (expr sbst::+ 2 151)
        (expr sbst::\|\| 2 141) (expr sbst::|##| 2 141)
        (expr sbst::@@ 2 141) (expr sbst::|#| 2 131)
        (expr sbst::@ 2 131) (expr sbst::IN 2 121)
        (expr sbst::WHERE 2 121) (expr sbst::WITH 2 111)
        (expr sbst::\|> 2 101) (expr sbst::<\| 2 101)
        (expr sbst::>>= 2 101) (expr sbst::<<= 2 101)
        (expr sbst::>> 2 101) (expr sbst::<< 2 101)
        (expr sbst::>= 2 101) (expr sbst::> 2 101)
        (expr sbst::<= 2 101) (expr sbst::< 2 101)
        (expr sbst::== 2 91) (expr sbst::/= 2 91)
        (expr sbst::= 2 91) (expr sbst::ANDTHEN 2 70)
        (expr sbst::&& 2 70) (expr sbst::/\\ 2 70)
        (expr sbst::& 2 70) (expr sbst::AND 2 70)
        (expr sbst::ORELSE 2 60) (expr sbst::XOR 2 60)
        (expr sbst::\\/ 2 60) (expr sbst::OR 2 60)
        (expr sbst::WHEN 2 50) (expr sbst::=> 2 50)
        (expr sbst::IMPLIES 2 50) (expr sbst::<=> 2 40)
        (expr sbst::IFF 2 40) (expr sbst::\|= 2 30)
        (expr sbst::\|- 2 30) (expr sbst::\| 2 21)
        (expr sbst::IN 3 10) (expr sbst::HAS_TYPE 3 10)))

(defparameter *expr-prec-info* (gethash 'expr pvs-prec-info)
  "The precedence information - a list of four hash tables, for initial,
left (medial), right (medial), and aggregate operators.  Each of these
hash tables gives a binding number for a given operator; higher numbers
bind tighter.")

;;; Unparse takes the following keywords:
;;; :string :stream :char-width :file :style

(defun unparse (obj &key string stream file char-width
		    length level lines (pretty t))
   (let ((*print-length* length)
	 (*print-level* level)
	 (*print-lines* lines)
	 (*print-pretty* pretty)
	 (*print-escape* nil)
	 (*print-right-margin* (or char-width *default-char-width*)))
     (cond (string
	    (decf *print-right-margin* 4)
	    (with-output-to-string (*standard-output*)
	      (pp obj)))
	   (stream
	    (let ((*standard-output* stream))
	      (pp obj)))
	   (file
	    (with-open-file (*standard-output* file :direction :output)
	      (pp obj)))
	   (t (decf *print-right-margin* 4)
	      (pp obj)))))

(defun unpindent (inst indent &key (width *default-char-width*)
		       length level lines string comment?)
  (let* ((str (unparse inst
		:string t :length length :level level :lines lines
		:char-width (- width indent (if comment? 2 0)))))
    (if string
	(with-output-to-string (*standard-output*)
	  (unpindent* str indent 0 (position #\linefeed str) comment? nil))
	(unpindent* str indent 0 (position #\linefeed str) comment? nil))))

(defun unpindent* (str indent start end comment? notfirst?)
  (format t "~v%~v,0T~:[~;% ~]~a"
    (if notfirst? 1 0)
    (if notfirst? indent (if comment? 1 0))
    (and comment? notfirst?)
    (subseq str start end))
  (if end
      (unpindent* str indent (1+ end)
		  (position #\linefeed str :start (1+ end))
		  comment? t)))

(defun unparse-decl (decl)
  (let ((*unparse-expanded* t)
	(ndecl (if (bind-decl? decl)
		   (mk-var-decl (id decl)
				(or (declared-type decl) (type decl)))
		   decl)))
    (string-trim '(#\Space #\Tab #\Newline)
		 (unparse ndecl
		   :string t
		   :char-width *default-char-width*))))

(defun pp (obj)
  (let ((*disable-gc-printout* t)
	(*pretty-printing-decl-list* nil)
	(*pretty-printed-prefix* nil))
    (pprint-logical-block (nil nil)
      (pp* obj))))

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
      (write ": ")
      (pprint-indent :block 2)
      (write 'THEORY)
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pp* exporting)
      (write 'BEGIN)
      (pprint-indent :block 2)
      (pprint-newline :mandatory)
      (pp-assuming (if *unparse-expanded*
		       assuming
		       (remove-if #'generated-by assuming)))
      (pp-theory (if *unparse-expanded*
		     theory
		     (remove-if #'generated-by theory)))
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (write 'END)
      (write-char #\space)
      (write id))))

(defun pp-theory-formals (formals)
  (when formals
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block (nil (check-chained-syntax formals)
				 :prefix "[" :suffix "]")
	(loop (let ((*pretty-printed-prefix* nil)
		    (elt (pprint-pop)))
		(when (typep elt 'importing)
		  (let ((imps (list elt)))
		    (loop while (chain? (car imps))
			  do (setq elt (pprint-pop))
			  do (push elt imps))
		    (pprint-logical-block (nil (nreverse imps)
					       :prefix "(" :suffix ")")
		      (write "IMPORTING")
		      (write #\space)
		      (pprint-indent :current 0)
		      (loop (pp* (pprint-pop))
			    (pprint-exit-if-list-exhausted)
			    (write-char #\,)
			    (write-char #\space)
			    (pprint-newline :fill))))
		  (write-char #\space)
		  (setq elt (pprint-pop)))
		(pp* elt)
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill)))))))

(defun check-chained-syntax (elements &optional nelts)
  (if (null elements)
      (nreverse nelts)
      (check-chained-syntax
       (cdr elements)
       (cons (if (and (chain? (car elements))
		      (or (null (cdr elements))
			  (not (compatible-chain? (car elements)
						  (cadr elements)))))
		 (copy (car elements) 'chain? nil)
		 (car elements))
	     nelts))))

(defmethod compatible-chain? ((decl1 type-decl) (decl2 type-decl))
  (eq (class-of decl1) (class-of decl2)))

(defmethod compatible-chain? ((decl1 type-def-decl) (decl2 type-def-decl))
  (and (eq (class-of decl1) (class-of decl2))
       (ps-eq (type-expr decl1) (type-expr decl2))))

(defmethod compatible-chain? ((decl1 typed-declaration)
			      (decl2 typed-declaration))
  (and (eq (class-of decl1) (class-of decl2))
       (ps-eq (declared-type decl1) (declared-type decl2))))

(defmethod compatible-chain? ((decl1 lib-decl) (decl2 lib-decl))
  (and (eq (class-of decl1) (class-of decl2))
       (string= (lib-string decl1) (lib-string decl2))))

(defmethod compatible-chain? ((decl1 mod-decl) (decl2 mod-decl))
  (and (eq (class-of decl1) (class-of decl2))
       (ps-eq (modname decl1) (modname decl2))))

(defmethod compatible-chain? ((decl1 theory-abbreviation-decl)
			      (decl2 theory-abbreviation-decl))
  (and (eq (class-of decl1) (class-of decl2))
       (ps-eq (theory-name decl1) (theory-name decl2))))

(defmethod compatible-chain? ((decl1 const-decl) (decl2 const-decl))
  (and (call-next-method)
       (ps-eq (definition decl1) (definition decl2))))

(defmethod compatible-chain? ((decl1 def-decl) (decl2 def-decl))
  (and (call-next-method)
       (ps-eq (declared-measure decl1) (declared-measure decl2))
       (ps-eq (ordering decl1) (ordering decl2))))

(defmethod compatible-chain? ((decl1 formula-decl) (decl2 formula-decl))
  (and (eq (class-of decl1) (class-of decl2))
       (eq (spelling decl1) (spelling decl2))
       (ps-eq (definition decl1) (definition decl2))))

(defmethod compatible-chain? ((imp1 importing) (imp2 importing))
  t)

(defmethod compatible-chain? ((imp1 importing) (imp2 theory-abbreviation-decl))
  t)

(defmethod compatible-chain? ((imp1 theory-abbreviation-decl) (imp2 importing))
  t)

; (defmethod compatible-chain? ((imp1 theory-abbreviation-decl)
; 			      (imp2 theory-abbreviation-decl))
;   t)

(defmethod compatible-chain? (x y)
  (declare (ignore x y))
  nil)

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
	  (pp-exportingmods kind modules)))
      (pprint-newline :mandatory))))

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
    (let ((*pretty-printing-decl-list* t)
	  (last-one (car (last assuming))))
      (pprint-logical-block (nil nil)
	(write 'ASSUMING)
	(pprint-indent :block 1)
	(pprint-newline :mandatory)
	(pprint-logical-block (nil (check-chained-syntax assuming))
	  (loop (let ((*pretty-printed-prefix* nil)
		      (decl (pprint-pop)))
		  (if (typep decl 'importing)
		      (let ((imps (list decl)))
			(loop while (chain? (car imps))
			      do (setq decl (pprint-pop))
			      do (push decl imps))
			(pprint-logical-block (nil (nreverse imps))
			  (write "IMPORTING")
			  (write #\space)
			  (pprint-indent :current 0)
			  (loop (pp* (pprint-pop))
				(pprint-exit-if-list-exhausted)
				(write-char #\,)
				(write-char #\space)
				(pprint-newline :fill)))
			(unless *pp-compact*
			  (pprint-newline :mandatory)))
		      (pp* decl))
		  (unless (or (chain? decl)
			      (eq decl last-one))
		    (pprint-newline :mandatory)
		    (unless *pp-compact*
		      (pprint-newline :mandatory))))
		(pprint-exit-if-list-exhausted)))
	(pprint-indent :block 0)
	(pprint-newline :mandatory)
	(write 'ENDASSUMING)
	(pprint-newline :mandatory)))))

(defun pp-theory (theory)
  (when theory
    (let ((*pretty-printing-decl-list* t)
	  (last-one (car (last theory))))
      (pprint-logical-block (nil (check-chained-syntax theory))
	(pprint-indent :block 0)
	(pprint-newline :mandatory)
	(loop (let ((*pretty-printed-prefix* nil)
		    (decl (pprint-pop)))
		(if (typep decl '(or importing theory-abbreviation-decl))
		    (let ((imps (list decl)))
		      (loop while (chain? (car imps))
			    do (setq decl (pprint-pop))
			    do (push decl imps))
		      (pprint-logical-block (nil (nreverse imps))
			(write "IMPORTING")
			(write #\space)
			(pprint-indent :current 0)
			(loop (pp* (pprint-pop))
			      (pprint-exit-if-list-exhausted)
			      (write-char #\,)
			      (write-char #\space)
			      (pprint-newline :fill))))
		    (pp* decl))
		(unless (or (chain? decl)
			    (eq decl last-one))
		  (pprint-newline :mandatory)
		  (unless *pp-compact*
		    (pprint-newline :mandatory))))
	      (pprint-exit-if-list-exhausted))))))

(defmethod pp* ((dt recursive-type))
  (with-slots (id formals importings assuming constructors) dt
    (pprint-logical-block (nil nil)
      (write id)
      (pp-theory-formals formals)
      (write-char #\:)
      (write-char #\space)
      (pprint-indent :block 2)
      (if (codatatype? dt)
	  (write 'CODATATYPE)
	  (write 'DATATYPE))
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
      (when importings
	(pprint-logical-block (nil importings)
	  (write 'IMPORTING)
	  (write-char #\space)
	  (pprint-indent :current 0)
	  (loop (pp* (theory-name (pprint-pop)))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill)))
	(pprint-newline :mandatory)
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
  (let ((*pretty-printing-decl-list* t))
    (pprint-logical-block (nil constructors)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory)))))

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

(defmethod pp* ((te enumtype))
  (pprint-logical-block (nil (constructors te) :prefix "{" :suffix "}")
    (pprint-indent :block 0)
    (loop (write (id (pprint-pop)))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
	  (pprint-newline :fill))))


;;; Declarations

(defmethod pp* ((imp importing))
  (with-slots (theory-name chain? semi) imp
    (unless *pretty-printing-decl-list*
      (write 'IMPORTING)
      (write-char #\space))
    (pp* theory-name)))

(defmethod pp* :around ((decl declaration))
  (with-slots (id formals chain? semi) decl
    (when (or *unparse-expanded*
	      *adt*
	      (not (generated-by decl)))
      (cond ((theory-abbreviation-decl? decl)
	     (call-next-method))
	    ((and chain?
		  *pretty-printing-decl-list*)
	     (write id)
	     (unless (typep decl '(or formal-decl adtdecl))
	       (write-char #\,)
	       (write-char #\space)
	       (pprint-newline :fill)))
	    (t (when (newline-comment decl)
		 (write (car (newline-comment decl)))
		 (write-char #\space)
		 (pprint-newline :mandatory))
	       (when (and *comment-on-proof-status*
			  (tcc? decl))
		 (format t "  % ~a~%" (proof-status-string decl)))
	       ;;(pprint-indent :block 2)
	       (write id)
	       (pp-decl-formals formals)
	       (write-char #\:)
	       (write-char #\space)
	       (pprint-newline :fill)
	       (call-next-method)
	       (when semi (write-char #\;)))))))

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
		 (nonempty-type 'NONEMPTY_TYPE)
		 (t 'TYPE+)))
	(write 'TYPE))))

(defmethod pp* ((decl type-def-decl))
  (with-slots (type-expr contains chain?) decl
    (if (typep decl 'nonempty-type-decl)
	(write (case (keyword decl)
		 (nonempty-type 'NONEMPTY_TYPE)
		 (t 'TYPE+)))
	(write 'TYPE))
    (write-char #\space)
    (pprint-newline :miser)
    (if (typep decl 'type-eq-decl)
	(write-char #\=)
	(write 'FROM))
    (write-char #\space)
    (pprint-newline :fill)
    (pp* type-expr)
    (when contains
	(write-char #\space)
	(pprint-newline :fill)
	(write 'CONTAINING)
	(write-char #\space)
	(pprint-newline :miser)
	(pp* contains))))

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
		 (nonempty-type 'NONEMPTY_TYPE)
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
    (pp* declared-type)))

(defmethod pp* ((decl formal-theory-decl))
  (with-slots (theory-name) decl
    (write 'THEORY)
    (write-char #\space)
    (pp* theory-name)))

(defmethod pp* ((decl lib-decl))
  (with-slots (lib-string) decl
    (write 'LIBRARY)
    (when (typep decl 'lib-eq-decl)
      (write-char #\space)
      (write-char #\=))
    (write-char #\space)
    (pprint-newline :fill)
    (write lib-string :escape t)))

;; (defmethod pp* ((decl lib-eq-decl)) )

(defmethod pp* ((decl mod-decl))
  (with-slots (modname) decl
    (write 'THEORY)
    (when (typep decl 'lib-eq-decl)
      (write-char #\space)
      (write-char #\=))
    (write-char #\space)
    (pprint-newline :fill)
    (pp* modname)))

(defmethod pp* ((decl theory-abbreviation-decl))
  (with-slots (id theory-name chain? semi) decl
    (unless *pretty-printing-decl-list*
      (write 'IMPORTING)
      (write-char #\space))
    (pp* theory-name)
    (write-char #\space)
    (write 'AS)
    (write-char #\space)
    (pp* id)))

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
      (pprint-indent :block 0))))

(defmethod pp* ((decl macro-decl))
  (with-slots (declared-type definition) decl
    (write 'MACRO)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)
    (write-char #\space)
    (write #\=)
    (pprint-indent :block 4)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* definition)
    (pprint-indent :block 0)))

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
    (pprint-indent :block 0)))

(defmethod pp* ((decl adt-def-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (pprint-newline :fill)
    (pp* declared-type)
    (when definition
      (write-char #\space)
      (write-char #\=)
      (pprint-indent :block 4)
      (write-char #\space)
      (pprint-newline :fill)
      (pp* definition)
      (pprint-indent :block 0))))

(defmethod pp* ((decl fixpoint-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (if (inductive-decl? decl)
	(write 'INDUCTIVE)
	(write 'COINDUCTIVE))
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
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (pp* definition)
    (pprint-indent :block 0)))

(defmethod pp* :around ((decl name-judgement))
  (with-slots (id name chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(write id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
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
	     (when semi (write-char #\;))
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp* :around ((decl application-judgement))
  (with-slots (id name formals chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(write id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* name)
    (pp-decl-formals formals)
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
	     (when semi (write-char #\;))
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp* :around ((decl number-judgement))
  (with-slots (id number-expr chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(write id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* number-expr)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (write 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (when semi (write-char #\;))
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp* :around ((decl subtype-judgement))
  (with-slots (id declared-subtype chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(write id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (write 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp* declared-subtype)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (write 'SUBTYPE_OF)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp* declared-type)
	     (when semi (write-char #\;))
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp* :around ((decl conversion-decl))
  (with-slots (expr chain?) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'CONVERSION)
      (typecase decl
	(conversionplus-decl (write '+))
	(conversionminus-decl (write '-)))
      (write-char #\space)
      (pprint-newline :miser))
    (pp* expr)
    (when (and chain?
	       *pretty-printing-decl-list*)
      (write-char #\,)
      (write-char #\space))
    (pprint-newline :mandatory)))

(defmethod pp* :around ((decl auto-rewrite-decl))
  (with-slots (name chain?) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (write 'AUTO_REWRITE)
      (typecase decl
	(auto-rewrite-plus-decl (write '+))
	(auto-rewrite-minus-decl (write '-)))
      (write-char #\space)
      (pprint-newline :miser))
    (pp* name)
    (when (typep decl 'typed-auto-rewrite-decl)
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
      (pp-arguments parameters)
      )))

(defun pp-arguments* (args)
  (pprint-logical-block (nil args)
    (loop (pprint-indent :current 2)
	  (let ((nextargs (pprint-pop)))
	    (if (and (singleton? nextargs)
		     (typep (car nextargs) 'quoted-assign))
		(pp* (car nextargs))
		(pp-arguments nextargs)))
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :fill))))

(defmethod pp* ((ex id-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (write (id ex))))

(defmethod pp* ((ex proj-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (write (number ex))))

(defun pp-arguments (args)
  (pprint-logical-block (nil args :prefix "(" :suffix ")")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

(defmethod pp* ((te subtype))
  (with-slots (supertype predicate) te
    (let* ((bindings (if (typep (predicate te) 'binding-expr)
			 (bindings (predicate te))
			 (let* ((id (make-new-variable '|x| te))
				(bd (mk-bind-decl id
				      (or (and (supertype te)
					       (print-type (supertype te)))
					  (supertype te)))))
			   (list bd))))
	   (expr (if (null (predicate te))
		     (formula te)
		     (if (typep (predicate te) 'binding-expr)
			 (expression (predicate te))
			 (let ((var (mk-name-expr (id (car bindings)))))
			   (mk-application (predicate te) var))))))
      (pprint-logical-block (nil nil :prefix "{" :suffix "}")
	(pp-bindings bindings)
	(pprint-indent :block 8)
	(write-char #\space)
	(write-char #\|)
	(write-char #\space)
	(pprint-newline :fill)
	(pp* expr)))))

(defmethod pp* ((te expr-as-type))
  (with-slots (expr) te
    (write-char #\()
    (pp* expr)
    (write-char #\))))

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
	       (ctr 0)
	       (*parsing-or-unparsing* t))
	  (pprint-logical-block (nil types)
	    (pprint-indent :current 0)
	    (loop (let ((nty (pprint-pop)))
		    (incf ctr)
		    (if (typep nty 'dep-binding)
			(pp* nty)
			(let ((var (car (rassoc ctr bindings))))
			  (if var
			      (pprint-logical-block (nil nil)
				(pp* var)
				(write-char #\:)
				(write-char #\space)
				(pprint-newline :fill)
				(pp* nty))
			      (pp* nty)))))
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

(defmethod pp* ((te cotupletype))
  (with-slots (types) te
    (pprint-logical-block (nil types :prefix "[" :suffix "]")
      (pprint-indent :current 0)
      (loop (pp* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\space)
	    (write-char #\+)
	    (write-char #\space)
	    (pprint-newline :fill)))))


;;; Expressions

(defmethod pp* :around ((ex expr))
  (if (and *ppmacros*
	   (from-macro ex))
      (pp* (from-macro ex))
      (if (typep ex 'binding)
	  (call-next-method)
	  (progn (dotimes (p (parens ex))
		   (write-char #\())
		 (call-next-method)
		 (dotimes (p (parens ex))
		   (write-char #\)))))))

(defmethod pp* :around ((ex infix-application))
  (cond ((and *pp-print-parens*
	      (zerop (parens ex)))
	 (write-char #\()
	 (call-next-method)
	 (write-char #\)))
	(t (call-next-method))))

(defmethod pp* ((ex number-expr))
  (write (number ex)))

(defmethod pp* ((ex string-expr))
  (unless (string-value ex)
    (setf (string-value ex) (pp-string-expr (argument ex))))
  (write (string-value ex) :escape t))

(defun pp-string-expr (charlist &optional list)
  (if (typep charlist 'name-expr)
      (coerce (nreverse list) 'string)
      (pp-string-expr (args2 charlist)
		      (nconc (pp-string-char (number (args1 (args1 charlist))))
			     list))))

(defun pp-string-char (code)
  (let ((char (code-char code)))
    (case char
      (#-gcl #\Bell #+gcl #\^G (list #\a #\\ #\\))
      (#\Backspace (list #\b #\\ #\\))
      (#\Page (list #\f #\\ #\\))
      (#\Newline (list #\n #\\ #\\))
      (#\Return (list #\r #\\ #\\))
      (#\Tab (list #\t #\\ #\\))
      (#-gcl #\VT #+gcl #\^K (list #\v #\\ #\\))
      (#\" (list #\" #\\ #\\))
      (#\\ (list #\\ #\\ #\\ #\\))
      (t (if (graphic-char-p char)
	     (list char)
	     (nreverse (cons #\\ (cons #\\ (coerce (format nil "~o" code)
						   'list)))))))))

(defmethod pp* ((ex list-expr))
  (if (valid-list-expr? ex)
      (pprint-logical-block (nil (list-arguments ex)
				 :prefix "(: " :suffix " :)")
	(pprint-indent :current 0)
	(loop (pp* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :fill)))
      (call-next-method)))

(defmethod valid-list-expr? ((ex list-expr))
  (and (tuple-expr? (argument ex))
       (= (length (exprs (argument ex))) 2)
       (valid-list-expr? (args2 ex))))

(defmethod valid-list-expr? ((ex null-expr))
  t)

(defmethod valid-list-expr? ((ex expr))
  nil)

(defmethod list-arguments ((ex list-expr))
  (cons (args1 ex) (list-arguments (args2 ex))))

(defmethod list-arguments ((ex null-expr))
  nil)

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

(defmethod pp* ((ex paren-vbar-expr))
  (pprint-logical-block (nil (arguments ex) :prefix "(| " :suffix " |)")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp* ((ex brace-vbar-expr))
  (pprint-logical-block (nil (arguments ex) :prefix "{| " :suffix " |}")
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

(defmethod pp* ((ex projection-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals)))))

(defmethod pp* ((ex injection-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals)))))

(defmethod pp* ((ex injection?-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals)))))

(defmethod pp* ((ex extraction-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals)))))

(defmethod pp* ((ex projection-application))
  (with-slots (id actuals argument) ex
    (if (and *pp-new-projection-forms*
	     (null actuals))
	(pprint-logical-block (nil nil)
	  (if (and (zerop (parens (argument ex)))
		   (< (precedence (argument ex) 'left)
		      (precedence 'sbst::|`| 'right)))
	      (progn (write-char #\()
		     (pp* (argument ex))
		     (write-char #\)))
	      (pp* (argument ex)))
	  (write-char #\`)
	  (write (index ex)))
	(pprint-logical-block (nil nil)
	  (write id)
	  (when actuals
	    (pp-actuals actuals))
	  (pp-arguments (argument-list argument))))))

(defmethod pp* ((ex projappl))
  (if (actuals ex)
      (call-next-method)
      (pprint-logical-block (nil nil)
	(if (and (zerop (parens (argument ex)))
		 (< (precedence (argument ex) 'left)
		    (precedence 'sbst::|`| 'right)))
	    (progn (write-char #\()
		   (pp* (argument ex))
		   (write-char #\)))
	    (pp* (argument ex)))
	(write-char #\`)
	(write (index ex)))))

(defmethod pp* ((ex injection-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals))
      (pp-arguments (argument-list argument)))))

(defmethod pp* ((ex injection?-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals))
      (pp-arguments (argument-list argument)))))

(defmethod pp* ((ex extraction-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (write id)
      (when actuals
	(pp-actuals actuals))
      (pp-arguments (argument-list argument)))))

(defmethod pp* ((ex field-application))
  (with-slots (id argument) ex
    (if *pp-new-projection-forms*
	(pprint-logical-block (nil nil)
	  (if (and (zerop (parens (argument ex)))
		   (< (precedence (argument ex) 'left)
		      (precedence 'sbst::|`| 'right)))
	      (progn (write-char #\()
		     (pp* (argument ex))
		     (write-char #\)))
	      (pp* (argument ex)))
	  (write-char #\`)
	  (write (id ex)))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (write id)
	  (pprint-newline :fill)
	  (pp-arguments (argument-list argument))))))

(defmethod pp* ((ex fieldappl))
  (pprint-logical-block (nil nil)
    (if (and (zerop (parens (argument ex)))
	     (< (precedence (argument ex) 'left)
		(precedence 'sbst::|`| 'right)))
	(progn (write-char #\()
	       (pp* (argument ex))
	       (write-char #\)))
	(pp* (argument ex)))
    (write-char #\`)
    (write (id ex))))

(defmethod pp* ((ex implicit-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp* (argument ex))))

(defmethod pp* ((ex argument-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp* (operator ex))))

(defmethod pp* ((ex lambda-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp* (expression ex))))

(defmethod pp* ((ex application))
  (let ((operator (get-pp-operator* (operator ex)))
	(args (get-pp-argument* (operator ex) (list (argument ex)))))
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (and (zerop (parens operator))
	       (< (precedence operator 'left)
		  (precedence (car args) 'right)))
	  (progn (write-char #\()
		 (pp* operator)
		 (write-char #\)))
	  (pp* operator))
      (pprint-indent :block 4)
      (if (simple-name? operator)
	  (pprint-newline :miser)
	  (pprint-newline :fill))
      (pp-arguments-list args))))

(defmethod get-pp-operator* ((expr application))
  (get-pp-operator* (operator expr)))

(defmethod get-pp-operator* ((expr infix-application))
  expr)

(defmethod get-pp-operator* ((expr unary-application))
  expr)

(defmethod get-pp-operator* ((expr expr))
  expr)

(defmethod get-pp-argument* ((expr application) &optional args)
  (get-pp-argument* (operator expr) (cons (argument expr) args)))

(defmethod get-pp-argument* ((expr infix-application) &optional args)
  args)

(defmethod get-pp-argument* ((expr unary-application) &optional args)
  args)

(defmethod get-pp-argument* ((expr expr) &optional args)
  args)


(defmethod simple-name? ((ex name))
  (and (not (mod-id ex))
       (not (actuals ex))
       (not (library ex))))

(defmethod simple-name? (ex)
  (declare (ignore ex))
  nil)

(defun pp-arguments-list (args)
  (pprint-logical-block (nil args)
    (loop (pp-arguments (pp-argument-list (pprint-pop)))
	  (pprint-exit-if-list-exhausted)
	  (pprint-newline :fill))))

(defmethod pp-argument-list ((arg projected-arg-tuple-expr))
  (if (and (every #'projection-application? (exprs arg))
	   (every #'(lambda (a)
		      (tc-eq (argument a) (argument (car (exprs arg)))))
		  (cdr (exprs arg))))
      (list (argument (car (exprs arg))))
      (call-next-method)))

(defmethod pp-argument-list (arg)
  (argument-list arg))

(defmethod pp* ((ex infix-application))
  (with-slots (operator argument) ex
    (if (valid-infix-application? ex)
	(let ((lhs (car (exprs argument)))
	      (rhs (cadr (exprs argument)))
	      (oper (sbst-symbol (id operator))))
	  (pprint-logical-block (nil nil)
	    (pprint-indent :block 1)
	    (if (and (zerop (parens lhs))
		     (< (precedence lhs 'left)
			(gethash oper (second *expr-prec-info*))))
		(progn (write-char #\()
		       (pp* lhs)
		       (write-char #\)))
		(pp* lhs))
	    (write-char #\space)
	    (pprint-newline :fill)
	    (if (eq (id operator) 'O)
		(pp* '|o|)
		(pp* (id operator)))
	    (write-char #\space)
	    (pprint-newline :fill)
	    (if (and (zerop (parens rhs))
		     (< (precedence rhs 'right)
			(gethash oper (third *expr-prec-info*))))
		(progn (write-char #\()
		       (pp* rhs)
		       (write-char #\)))
		(pp* rhs))))
	(call-next-method))))

(defmethod valid-infix-application? ((ex infix-application))
  (with-slots (operator argument) ex
    (and (typep operator 'name-expr)
	 (memq (id operator) *infix-operators*)
	 (typep argument 'tuple-expr)
	 (= (length (exprs argument)) 2)
	 (not (or (mod-id operator)
		  (library operator)
		  (actuals operator)
		  (mappings operator))))))

(defmethod valid-infix-application? (ex)
  nil)

(defmethod pp* ((ex infix-conjunction))
  (let ((conjuncts (collect-infix-conjuncts ex)))
    (if (<= (length conjuncts) 4)
	(call-next-method)
	(let ((op (id (operator ex))))
	  (pprint-logical-block (nil conjuncts)
	    (write "   ")
	    (when (eq op 'AND)
	      (write "  "))
	    (loop (pp* (pprint-pop))
		  (pprint-exit-if-list-exhausted)
		  (pprint-newline :fill)
		  (write " ")
		  (write op)
		  (write " ")))))))

(defmethod pp* ((ex infix-disjunction))
  (let ((disjuncts (collect-infix-disjuncts ex)))
    (if (<= (length disjuncts) 4)
	(call-next-method)
	(pprint-logical-block (nil disjuncts)
	  (write "    ")
	  (loop (pp* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(pprint-newline :fill)
		(write " OR "))))))

(defmethod collect-infix-conjuncts ((ex infix-conjunction))
  (nconc (collect-infix-conjuncts (args1 ex))
	 (collect-infix-conjuncts (args2 ex))))

(defmethod collect-infix-conjuncts (ex)
  (list ex))

(defmethod collect-infix-disjuncts ((ex infix-disjunction))
  (nconc (collect-infix-disjuncts (args1 ex))
	 (collect-infix-disjuncts (args2 ex))))

(defmethod collect-infix-disjuncts (ex)
  (list ex))

(defun sbst-symbol (sym)
  (or (get sym 'sbst-symbol)
      (setf (get sym 'sbst-symbol)
	    (intern (symbol-name sym) 'sbst))))

(defmethod pp* ((ex unary-application))
  (with-slots (operator argument) ex
    (if (and (typep operator 'name-expr)
	     (not (mod-id operator))
	     (not (library operator))
	     (memq (id operator) *unary-operators*))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (write (id operator))
	  (when (valid-pvs-id* (id operator))
	    (write-char #\space)
	    (pprint-newline :miser))
	  (if (>= (precedence argument 'right)
		      (gethash (sbst-symbol (id operator))
			       (first *expr-prec-info*)))
	      (pp* argument)
	      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
		(pp* argument))))
	(call-next-method))))

(defmethod pp* ((ex binding-application))
  (with-slots (operator argument) ex
    (if (lambda-expr? argument)
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
	  (pp* (expression argument)))
	(call-next-method))))

(defmethod pp* ((ex when-expr))
  (with-slots (operator argument) ex
    (if (real-projected-arg-tuple-expr? argument)
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (write "WHEN")
	  (write-char #\()
	  (pp* (argument (car (exprs argument))))
	  (write-char #\)))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp* (args2 ex))
	  (write-char #\space)
	  (pprint-newline :miser)
	  (write "WHEN")
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp* (args1 ex))))))

(defmethod real-projected-arg-tuple-expr? ((arg projected-arg-tuple-expr))
  (and (every #'projection-application? (exprs arg))
       (every #'(lambda (a)
		  (tc-eq (argument a) (argument (car (exprs arg)))))
	      (cdr (exprs arg)))))

(defmethod real-projected-arg-tuple-expr? (arg)
  (declare (ignore arg))
  nil)

(defmethod pp* ((ex if-expr))
  (pprint-logical-block (nil nil)
    (pprint-indent :current 2)
    (write 'IF)
    (write-char #\space)
    (pp* (condition ex))
    (write-char #\space)
    (pprint-indent :block 2)
    (pprint-newline :fill)
    (write 'THEN)
    (write-char #\space)
    (pp* (then-part ex))
    (write-char #\space)
    (pprint-indent :block 0)
    (pprint-newline :linear)
    (cond ((chained-else? (else-part ex))
	   (write 'ELSIF)
	   (pp-chained-if-expr (else-part ex) nil))
	  (t (write 'ELSE)
	     (write-char #\space)
	     (pp* (else-part ex))))
    (write-char #\space)
    (pprint-newline :linear)
    (write 'ENDIF)))

(defmethod chained-else? ((ex chained-branch))
  t)

(defmethod chained-else? ((ex chained-if-expr))
  t)

(defmethod chained-else? ((ex argument-conversion))
  (and (null *show-conversions*)
       (chained-else? (operator ex))))

(defmethod chained-else? ((ex lambda-conversion))
  (and (null *show-conversions*)
       (chained-else? (expression ex))))

(defmethod chained-else? (ex)
  (declare (ignore ex))
  nil)

(defmethod pp* ((ex chained-if-expr))
  (pprint-logical-block (nil nil)
    (pp-chained-if-expr ex t)))

(defun pp-chained-if-expr (ex print-if?)
  (when print-if?
    (write "   IF"))
  (write-char #\space)
  (pp* (condition ex))
  (write-char #\space)
  (pprint-indent :block 2)
  (pprint-newline :fill)
  (write 'THEN)
  (write-char #\space)
  (pp* (then-part ex))
  (write-char #\space)
  (pprint-indent :block 0)
  (pprint-newline :linear)
  (cond ((typep (else-part ex) 'chained-if-expr)
	 (write 'ELSIF)
	 (pp-chained-if-expr (else-part ex) nil))
	(t (write 'ELSE)
	   (write-char #\space)
	   (pp* (else-part ex)))))

(defmethod condition ((ex argument-conversion))
  (condition (operator ex)))

(defmethod then-part ((ex argument-conversion))
  (then-part (operator ex)))

(defmethod else-part ((ex argument-conversion))
  (else-part (operator ex)))

(defmethod condition ((ex lambda-conversion))
  (condition (expression ex)))

(defmethod then-part ((ex lambda-conversion))
  (then-part (expression ex)))

(defmethod else-part ((ex lambda-conversion))
  (else-part (expression ex)))

(defmethod pp* ((ex coercion))
  (with-slots (argument operator) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp* argument)
      (write "::")
      (write-char #\space)
      (pprint-newline :fill)
      (pp* (coercion-declared-type operator)))))

(defmethod coercion-declared-type ((ex lambda-expr))
  (declared-type (car (bindings ex))))

(defmethod coercion-declared-type ((ex implicit-conversion))
  (coercion-declared-type (argument ex)))

(defmethod pp* ((ex binding-expr))
  (let ((*pretty-printing-decl-list* t))
    (multiple-value-bind (bindings-list expr)
	(pp-unchain-binding-expr (expression ex) (list (bindings ex))
				 (operator ex))
      (pprint-logical-block (nil nil)
	(write (operator ex))
	(pprint-indent :current 1)
	(write " ")
	(pprint-newline :miser)
	(pprint-logical-block (nil bindings-list)
	  (loop (let ((bindings (pprint-pop)))
		  (pp-lambda-formal (pp-chained-decls bindings)
				    (commas? ex)
				    nil))
		(pprint-exit-if-list-exhausted)
		(pprint-newline :fill)))
	(pprint-indent :block 2)
	(write ": ")
	(pprint-newline :fill)
	(pp* expr)))))

(defmethod pp-unchain-binding-expr ((ex binding-expr) bindings op)
  (if (and (chain? ex)
	   (eq (operator ex) op))
      (pp-unchain-binding-expr (expression ex)
			       (cons (bindings ex) bindings)
			       op)
      (values (nreverse bindings) ex)))

(defmethod pp-unchain-binding-expr (ex bindings op)
  (declare (ignore op))
  (values (nreverse bindings) ex))
  

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
  (if (lambda-expr? (operator ex))
      (multiple-value-bind (let-bindings expr)
	  (get-let-bindings ex)
	(pprint-logical-block (nil nil)
	  (write 'LET)
	  (write-char #\space)
	  (pp-let-bindings let-bindings)
	  (write-char #\space)
	  (pprint-indent :block 2)
	  (pprint-newline :fill)
	  (write 'IN)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp* expr)))
      (call-next-method)))

(defun pp-let-bindings (let-bindings)
  (pprint-logical-block (nil let-bindings)
    (loop (let ((lb (pprint-pop)))
	    (if (cdr (car lb))
		(pprint-logical-block (nil nil :prefix "(" :suffix ")")
		  (pp-bindings (car lb)))
		(if (cadr lb)
		    (write (id (caar lb)))
		    (pp-bindings (car lb))))
	    (when (cadr lb)
	      (pp-decl-formals (cadr lb))
	      (when (declared-type (caar lb))
		(write ": ")
		(pp* (get-let-binding-range (declared-type (caar lb))
					    (cadr lb)))))
	    (write-char #\space)
	    (pprint-indent :block 2)
	    (pprint-newline :fill)
	    (write #\=)
	    (write-char #\space)
	    (pprint-newline :fill)
	    (pp* (caddr lb))
	    (pprint-indent :block 0))
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
      (write 'WHERE)
      (write-char #\space)
      (pp-let-bindings where-bindings))))

(defmethod pp* ((ex update-expr))
  (with-slots (expression assignments) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (and (zerop (parens expression))
	       (< (precedence expression 'left)
		  (precedence 'WITH 'right)))
	  (progn (write-char #\()
		 (pp* expression)
		 (write-char #\)))
	  (pp* expression))
      (write-char #\space)
      (pprint-newline :fill)
      (write 'WITH)
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

(defvar *pp-else-cond-exprs* nil)

(defun pp-cond-expr (ex)
  (let* ((pairs (collect-cond-expr-pairs ex nil))
	 (*pp-else-cond-exprs* (if (else-condition? (caar (last pairs)))
				   (cons (caar (last pairs))
					 *pp-else-cond-exprs*)
				   *pp-else-cond-exprs*)))
    (pprint-logical-block (nil nil)
      (write 'COND)
      (write-char #\space)
      (pprint-newline :miser)
      (pprint-logical-block (nil pairs)
	(loop (let ((pair (pprint-pop)))
		(pp* (car pair))
		(pprint-indent :block 2)
		(write-char #\space)
		(pprint-newline :fill)
		(write "->")
		(write-char #\space)
		(pprint-newline :fill)
		(pp* (cdr pair))
		(pprint-indent :block 0)
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :linear))))
      (write-char #\space)
      (pprint-newline :linear)
      (write 'ENDCOND))))

(defmethod pp* ((ex else-condition))
  (if (memq ex *pp-else-cond-exprs*)
      (write 'ELSE)
      (call-next-method)))

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
  (nreverse (acons 'ELSE ex pairs)))

(defmethod pp* ((ex cases-expr))
  (with-slots (expression selections else-part) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write 'CASES)
      (write-char #\space)
      (pp* expression)
      (write-char #\space)
      (pprint-newline :fill)
      (write 'OF)
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
	(write 'ELSE)
	(write-char #\space)
	(pp* else-part))
      (write-char #\space)
      (pprint-newline :linear)
      (write 'ENDCASES))))
      
(defmethod pp* ((sel selection))
  (with-slots (constructor args expression) sel
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (write (id constructor))
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
      (pprint-newline :fill)
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
      (pprint-newline :fill)
      (write "|->")
      (write-char #\space)
      (pp* expression))))

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
    (write "TABLE ")
    (pprint-newline :miser)
    (when row-expr
      (pp* row-expr))
    (when col-expr
      (write ", ")
      (pprint-newline :miser)
      (pp* col-expr))
    (pprint-indent :block 2)
    (pprint-newline :mandatory)
    (let* ((rows (if row-headings
		     (mapcar #'cons row-headings table-entries)
		     table-entries))
	   (col-widths (compute-table-column-widths
			col-headings row-headings rows)))
      (when col-headings
	(when *pp-table-hrules*
	  (if row-headings
	      (format t "~v<~>  %+~{~v,,,'-<~>--+~}+"
		(car col-widths) (cdr col-widths))
	      (format t "%~{~v,,,'-<~>--+~}+"
		 col-widths))
	  (pprint-newline :mandatory))
	(let ((widths col-widths)
	      (first? t))
	  (when row-headings
	    (format t "~va" (+ (pop widths) 2) " "))
	  (pprint-logical-block (nil col-headings
				     :prefix "|[ " :suffix " ]|")
	    (loop (let* ((entry (or (pprint-pop) ""))
			 (width (pop widths))
			 (str (format nil "~w" entry)))
		    (if (find #\newline str)
			(format t "~w" entry)
			(format t "~v:a" width str)))
		  (pprint-exit-if-list-exhausted)
		  (if (and first? (not row-headings))
		      (write "| ")
		      (write " | "))
		  (setq first? nil)
		  (pprint-indent :current 0)))
	  (pprint-newline :mandatory)))
      (when *pp-table-hrules*
	(format t "%~{~v,,,'-<~>--+~}+" col-widths)
	(pprint-newline :mandatory))
      (pprint-logical-block (nil rows)
	(loop (let ((row (pprint-pop))
		    (widths col-widths))
		(pprint-logical-block (nil row :prefix "| " :suffix " ||")
		  (loop (let* ((entry (or (pprint-pop) ""))
			       (width (pop widths))
			       (str (format nil "~w" entry)))
			  (if (find #\newline str)
			      (format t "~w" entry)
			      (format t "~v:a" width str)))
			(pprint-exit-if-list-exhausted)
			(write " | ")
			(pprint-indent :current 0)))
		(when *pp-table-hrules*
		  (pprint-newline :mandatory)
		  (format t "%~{~v,,,'-<~>--+~}+" col-widths))
		(pprint-exit-if-list-exhausted)
		(pprint-newline :mandatory))))
      (pprint-indent :block 0)
      (pprint-newline :mandatory)
      (write 'ENDTABLE))))

(defun compute-table-column-widths (col-headings row-headings rows)
  (compute-table-column-widths*
   rows
   (if col-headings
       (if row-headings
	   (cons 0 (mapcar #'compute-column-width col-headings))
	   (let ((col-widths (mapcar #'compute-column-width col-headings)))
	     (incf (car col-widths))
	     col-widths))
       (mapcar #'(lambda (x) (declare (ignore x)) 0) (car rows)))))

(defun compute-table-column-widths* (rows widths)
  (if (null rows)
      widths
      (compute-table-column-widths*
       (cdr rows)
       (mapcar #'(lambda (w e) (max w (compute-column-width e)))
	 widths (car rows)))))

(defun compute-column-width (ex)
  (let* ((str (unparse ex :string t :char-width 1000000))
	 (pos (position #\newline str)))
    (if pos
	(1- pos)
	(length str))))

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
  (with-slots (library mod-id actuals id mappings) ex
    (pprint-logical-block (nil (list ex))
      (when library
	(write library)
	(write-char #\@))
      (cond (mod-id
	     (write mod-id)
	     (when actuals
	       (pp-actuals actuals))
	     (when mappings
	       (pp-mappings mappings))
	     (write-char #\.)
	     (write id))
	    (t
	     (unless (or library (not (memq id '(|#| |##|))))
	       (write " "))
	     (write id)
	     (when actuals
	       (pp-actuals actuals))
	     (when mappings
	       (pp-mappings mappings)))))))

(defun pp-mappings (mappings)
  (pprint-logical-block (nil mappings :prefix "{{ " :suffix " }}")
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
	  (pprint-newline :fill))))

(defmethod pp* ((map mapping))
  (pprint-logical-block (nil nil)
    (pp* (lhs map))
    (when (kind map)
      (write ": ")
      (if (eq (kind map) 'expr)
	  (pp* (declared-type map))
	  (write (kind map))))	       
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (typecase map
      (mapping-def (write "="))
      (mapping-subst (write ":="))
      (mapping-rename (write "::=")))
    (write-char #\space)
    (if (rhs map)
	(pp* (expr (rhs map)))
	(write "NORHS"))))

(defmethod pp* ((map mapping-with-formals))
  (pprint-logical-block (nil nil)
    (pp* (lhs map))
    (pp-decl-formals (formals map))
    (when (kind map)
      (write ": ")
      (if (eq (kind map) 'expr)
	  (pp* (declared-type map))
	  (write (kind map))))
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (typecase map
      (mapping-def (write "="))
      (mapping-subst (write ":="))
      (mapping-rename (write "::=")))
    (write-char #\space)
    (if (rhs map)
	(pp* (remove-map-formals (formals map) (expr (rhs map))))
	(write "NORHS"))))

(defmethod remove-map-formals ((formals null) expr)
  expr)

(defmethod remove-map-formals (formals (expr lambda-expr))
  (if (null formals)
      expr
      (remove-map-formals
       (cdr formals) (expression expr))))

(defmethod remove-map-formals (formals (expr implicit-conversion))
  (if (null formals)
      expr
      (remove-map-formals
       (cdr formals) (expression (argument expr)))))

(defmethod pp* ((ex eager-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write "!")))

(defmethod pp* ((ex macro-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write "!!")))

(defmethod pp* ((ex constant-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write ": ")
    (pprint-newline :fill)
    (pp* (declared-type ex))))

(defmethod pp* ((ex formula-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write ": ")
    (pprint-newline :fill)
    (write (spelling ex))))

(defmethod pp* ((ex fnum-rewrite))
  (write (fnum ex)))

(defmethod pp* ((list list))
  (if (and list
	   (every #'declaration? list)
	   (every #'module list))
      (pp-theory list)
      (let ((*pretty-printing-decl-list* t)
	    (*pretty-printed-prefix* nil))
	(dolist (elt list)
	  (pp* elt)))))

(defun pp-actuals (actuals)
  (pprint-logical-block (nil actuals :prefix "[" :suffix "]")
    (pprint-indent :current 0)
    (loop (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

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
    (if (lambda-expr? operator)
	(get-let-bindings* (expression operator)
			   (bindings operator)
			   argument
			   (multiple-value-bind (formals arg-expr)
			       (get-let-formals-and-arg arg)
			     (cons (list bindings formals arg-expr) lbindings)))
	(call-next-method))))

(defmethod get-let-bindings* (ex bindings arg lbindings)
  (multiple-value-bind (formals arg-expr)
      (get-let-formals-and-arg arg)
    (values (nreverse (cons (list bindings formals arg-expr) lbindings))
	    ex)))

(defmethod get-let-formals-and-arg ((arg let-lambda-expr) &optional formals)
  (get-let-formals-and-arg (expression arg) (cons (bindings arg) formals)))

(defmethod get-let-formals-and-arg (arg &optional formals)
  (values (nreverse formals) arg))

(defun get-let-binding-range (type formals)
  (if (null formals)
      type
      (let ((nrange (get-let-binding-range* (domain type) (range type))))
	(get-let-binding-range nrange (cdr formals)))))

(defmethod get-let-binding-range* ((dom dep-binding) range)
  (if (dep-domain-tupletype? (declared-type dom))
      (let* ((bindings (var-bindings (declared-type dom)))
	     (*parsing-or-unparsing* t))
	(gensubst range
	  #'(lambda (ex)
	      (let ((var (car (rassoc (index ex) bindings))))
		(make-instance 'name-expr
		  'id var)))
	  #'(lambda (ex)
	      (and (typep ex 'projection-application)
		   (typep (argument ex) 'name-expr)
		   (eq (id (argument ex)) (id dom))))))
      range))

(defmethod get-let-binding-range* (dom range)
  (declare (ignore dom))
  range)

(defmethod get-where-bindings ((ex where-expr))
  (with-slots (operator argument) ex
    (get-where-bindings* (expression operator) (bindings operator)
			 argument nil)))

(defmethod get-where-bindings* ((ex chained-where-expr) bindings arg lbindings)
  (with-slots (operator argument) ex
    (get-where-bindings* (expression operator)
			 (bindings operator)
			 argument
			 (multiple-value-bind (formals arg-expr)
			   (get-let-formals-and-arg arg)
			 (cons (list bindings formals arg-expr) lbindings)))))

(defmethod get-where-bindings* (ex bindings arg lbindings)
  (multiple-value-bind (formals arg-expr)
      (get-let-formals-and-arg arg)
    (values (nreverse (cons (list bindings formals arg-expr) lbindings))
	    ex)))

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
		    (write (id (caar bindings)))))
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
  ;; These are the outermost binding parens
  (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
    (loop (let* ((next (pprint-pop))
		 (parens (if (zerop (parens (car next))) 0 1)))
	    (pp-paren-adformals* next parens)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod parens ((fd field-decl))
  0)

(defun pp-paren-adformals* (b &optional (parens (parens (car b))))
  (if (zerop parens)
      (pprint-logical-block (nil nil)
	(mapl #'(lambda (bb)
		  (pp-bind-decl (car bb) nil)
		  (when (cdr bb)
		    (write-char #\,)
		    (write-char #\space)
		    (pprint-newline :fill)))
	      b))
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
	(pp-paren-adformals* b 0))))

(defun pp-lambda-adformals (bindings)
  (pprint-logical-block (nil bindings :prefix "(" :suffix ")")
    (loop (pprint-indent :current 2)
	  (pp* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
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

(defun pp-bind-decl (bd &optional (parens? t))
  (let* ((pnum (if (and parens? (integerp (parens bd)))
		   (if (and (pred-bind-decl? bd)
			    (setsubtype? (declared-type bd)))
		       (max (parens bd) 1)
		       (parens bd))
		   0))
	 (pl (make-string pnum :initial-element #\())
	 (pr (make-string pnum :initial-element #\))))
    (pprint-logical-block (nil nil :prefix pl :suffix pr)
      (write (id bd))
      (when (and (declared-type bd)
		 (not (chain? bd)))
	(cond ((and (pred-bind-decl? bd)
		    (setsubtype? (declared-type bd)))
	       (when (supertype (declared-type bd))
		 (pprint-indent :block 2)
		 (write-char #\:)
		 (write-char #\space)
		 (pprint-newline :fill)
		 (pp* (supertype (declared-type bd))))
	       (pprint-indent :block 4)
	       (write-char #\space)
	       (pprint-newline :fill)
	       (write-char #\|)
	       (write-char #\space)
	       (pp* (formula (declared-type bd))))
	      (t (pprint-indent :block 2)
		 (write-char #\:)
		 (write-char #\space)
		 (pprint-newline :fill)
		 (pp* (declared-type bd))))))))

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
	       ((typep (car decls) 'importing)
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

(defmethod pp* ((ex symbol))
  (write ex))

;;; Find the precedence of an expression.

(defmethod precedence :around ((expr expr) ctx)
  (declare (ignore ctx))
  (if (plusp (parens expr))
      most-positive-fixnum
      (call-next-method)))

;; Most types of expressions cannot be ambiguous (e.g. tuples, if-exprs).
(defmethod precedence ((expr expr) ctx)
  (declare (ignore ctx))
  most-positive-fixnum)

(defmethod precedence ((expr unary-application) ctx)
  (if (and (typep (operator expr) 'name-expr)
	   (member (id (operator expr)) *unary-operators*))
      (case ctx
	(left (gethash (sbst-symbol (id (operator expr)))
		       (first *expr-prec-info*)))
	(right most-positive-fixnum))
      (call-next-method)))

(defmethod precedence ((expr binding-expr) ctx)
  (case ctx
    (left (or (gethash (sbst-symbol '|\||)
		       (third *expr-prec-info*))
	      21))
    (right (or (gethash (sbst-symbol '|\||)
			(second *expr-prec-info*))
	       20))))
    
(defmethod precedence ((expr let-expr) ctx)
  (case ctx
    (left (gethash (sbst-symbol 'IN)
		   (fourth *expr-prec-info*)))
    (right most-positive-fixnum)))

(defmethod precedence ((expr update-expr) ctx)
  (case ctx
    (left most-positive-fixnum)
    (right (gethash (sbst-symbol 'WITH)
		    (second *expr-prec-info*)))))
    
(defmethod precedence ((expr application) ctx)
  (case ctx
    (left most-positive-fixnum)
    (right (gethash 'jux
		    (second *expr-prec-info*)))))

(defmethod precedence ((expr infix-application) ctx)
  (if (and (typep (operator expr) 'name-expr)
	   (member (id (operator expr)) *infix-operators*)
	   (typep (argument expr) 'tuple-expr)
	   (= (length (exprs (argument expr))) 2))
      (case ctx
	(left (min (gethash (sbst-symbol (id (operator expr)))
			    (third *expr-prec-info*))
		   (if (not (zerop (parens (args2 expr))))
		       most-positive-fixnum
		       (precedence (second (arguments expr)) 'left))))
	(right (gethash (sbst-symbol (id (operator expr)))
			(second *expr-prec-info*))))
      (call-next-method)))

(defmethod precedence ((expr name-expr) ctx)
  (if (and (eq ctx 'left)
	   (memq (id expr) *unary-operators*))
      0
      (call-next-method)))

(defmethod precedence ((key symbol) ctx)
  (case ctx
    (left (gethash (sbst-symbol key) (third *expr-prec-info*)))
    (right (gethash (sbst-symbol key) (second *expr-prec-info*)))))

(defmethod chain? ((decl inline-datatype))
  nil)
