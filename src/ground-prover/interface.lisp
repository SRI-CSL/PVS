
(defvar *development-mode* nil)

(defvar *printerpdivide* 'yes)

(defvar *printerpmult* 'experimental)

(defvar prmode 'checking)

(proclaim '(special typealist applysymlist))

(defmacro ncons (x)
  `(cons ,x nil))

;from library-cl

(defmacro argument-form (arg)
  arg)

(defmacro same-printname-p (sym1 sym2)
  `(string= (string ,sym1) (string ,sym2)))

;(defmacro charlist-to-symbol (charlist)
;  `(values (intern (charlist-to-string ,charlist))))

;from library-cl
;used in process

(defmacro member$ (item list)
  `(member ,item ,list :test #'equal))

;from pr-top
;used in process

(defun db-index-name (num)
  (intern (format nil "*~a*" num)))

;from prpp
(defun is-db-lambda (form) ;;NSH(7.30.98: equal to >=)
  (and (is-lambda form)(>= (length form) 3)(numberp (arg1 form))))

;from tc-pr-interface
(defun is-lambda (form)
  (and (consp form)
       (eq (car form) 'lambda) ))

;from pr-top

(defun db-bump (exp up local-depth)
  (cond ((atom exp)
	 (if (lamvar? exp)
	     (if (<= (lamvar-index exp) local-depth)
		 exp
		 (db-index-name (+ (lamvar-index exp) up)))
	     exp))
	((is-db-lambda exp)
	 `(lambda ,(arg1 exp) ,(db-bump (arg2 exp) up (+ local-depth
						       (arg1 exp)))))
	(t (cons (funsym exp)
		 (loop for arg in (argsof exp) collect
		      (db-bump arg up local-depth))))))

(defun substandard (exp bindings depth)
  (cond
   ((atom exp)
    (cond
     ((lamvar? exp)
      (let ((ind (lamvar-index exp))
	    (bnd-len (length bindings)))
	(cond ((<= ind depth) exp)
	      ((> ind (+ depth bnd-len))
	       (db-index-name (- ind bnd-len)))
	      (t 
	       (db-bump (nth (- (lamvar-index exp) (+ depth 1)) bindings)
		     depth 0)))))
     (t exp)))
   ((eq (funsym exp) 'lambda)
    `(lambda ,(arg1 exp) ,(substandard (arg2 exp) bindings
				     (+ depth (arg1 exp)))))
   (t
    (cons (funsym exp)
	  (loop for arg in (argsof exp) collect
	       (substandard arg bindings depth))))))

(defun isapplylambda (exp)
  (and (listp (arg1 exp))
       (eq (funsym (arg1 exp)) 'lambda)
       (let ((result (is-apply-n-x-n (car exp))))
	 (and result (eq result (arg1 (arg1 exp)))))))

(defun lambdareduce (exp)
  (substandard (arg2 (arg1 exp)) (cddr exp) 0))

(defun lamvar? (sym)
  (when (symbolp sym)
    (let* ((symstring (string sym))
	   (symtrim (string-trim "*" symstring)))
      (every #'digit-char-p symtrim))))

(defun lamvar-index (sym)
    (let* ((symstring (symbol-name sym))
	   (symtrim (string-trim "*" symstring)))
      (do ((j 0 (+ j 1))
	   (n 0 (+ (* n 10)
		   (digit-char-p (char symtrim j)))))
	  ((= j (length symtrim)) n))))

(defun subst-expr (new old form)
  (cond ((eq form old) new)
	((atom form) form)
	((equal form old) new)
	(t (cons (subst-expr new old (car form))
		 (subst-expr new old (cdr form)) ))))

;from prpp
;used in process

;(defun is-apply-n-x (name)
;  (prog (chars first6)
;    (setq chars (symbol-to-charlist name))				
;    (setq first6 (loop for i from 1 to 6 for c in chars collect c))
;    (cond ((same-printname-p (charlist-to-symbol first6) 'apply- )	
;	   (return (nth 6 chars)))
;	  (t (return nil)))))

(defun is-apply-n-x (name)
  (and (symbolp name)
       (let ((str (string name)))
	 (and (>= (length str) 6)
	      (string= name "apply-" :end1 6)))))

(defun string->integer (string)
  (do ((j 0 (+ j 1))
       (n 0 (+ (* n 10)
	       (digit-char-p (char string j)))))
      ((= j (length string)) n)))

(defun is-apply-n-x-n (name)
  (and (symbolp name)
       (let ((str (string name)))
	 (and (>= (length str) 6)
	      (string= str "apply-" :end1 6)
	      (digit-char-p (char str 6))
	      (parse-integer str :start 6 :junk-allowed t)))))

(defun is-lambda-expr (form)
  (and (listp form)(eq (car form) 'lambda)))

;from library-cl
;used in arrays

(defun symbol-to-charlist (symbol)
  (map 'list
       #'(lambda (ch) (intern (string ch)))
       (princ-to-string symbol)))

;from prtop
;used in arrays

(defun applysym1 (type nargs)
  (prog (sym)
    (setq sym (combine-symbols 'apply- nargs '- type))
    (cond
     ((and type (not (assq sym typealist)))
      (push (cons sym type) typealist)))       ;add return type to alist 
    (push sym applysymlist)                    ;add sym to applysymlist
    (return sym)))

;from library-cl
(defun combine-symbols (&rest symbols)
  (let ((*print-radix* nil))
    (values (intern  
	     (eval 
	      (append '(concatenate 'string)
		      (map 'list	; use princ-to-string for numbers
			   #'(lambda (sym)
			       (if (listp sym) 
				   (charlist-to-string sym)
				   (princ-to-string sym)))
			   (argument-form symbols))))
	     ))))

(defun charlist-to-string (charlist)
  (map 'string                               ; (string ch) ensures type
       #'(lambda (ch) (char (string ch) 0))  ; if ch is not a single char
					     ; take only the first character
       charlist))

;from library-cl
;used in arith


(defun alphalessp (a b)
  (string< (arithstring a) (arithstring b)))

(defun arithstring (a)
  (case a
    (minus "MINUS")
    (times "TIMES")
    (divides "DIVIDES")
    (plus "PLUS")
    (difference "DIFFERENCE")
    (t (string a))))

;from library-cl
;used in q

(defmacro divide$ (num1 num2)
  `(values (truncate ,num1 ,num2)))

;from library-cl
;used in q

(defmacro remainder$ (num1 num2)
  `(cadr (multiple-value-list (truncate ,num1 ,num2))))


(defun load-ground-prover ()
  (load "~ehdm/prover/prmacros")
  (load "~ehdm/prover/prglobals")
  (load "~ehdm/prover/interface")
  (load "~ehdm/prover/process")
  (load "~ehdm/prover/arrays")
  (load "~ehdm/prover/tuples")
  (load "~ehdm/prover/arith")
  (load "~ehdm/prover/q")
  (load "~ehdm/prover/prove"))

(defun compile-prover ()
  (compile-file "~ehdm/prover/prmacros")
  (compile-file "~ehdm/prover/prglobals")
  (compile-file "~ehdm/prover/interface")
  (compile-file "~ehdm/prover/process")
  (compile-file "~ehdm/prover/arrays")
  (compile-file "~ehdm/prover/tuples")
  (compile-file "~ehdm/prover/arith")
  (compile-file "~ehdm/prover/q")
  (compile-file "~ehdm/prover/prove"))
