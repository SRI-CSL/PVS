;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-unparse.lisp	1.37 10/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Sat Aug  8 16:45:27 1987

(in-package "SB-RUNTIME")

(use-package '("OPER" "TERM" "SORT"))

(export '(*bracket-info* *prec-info* *pat-nesting*
	  *key-token-map* *lt-token-map* *key-esc-token-map* *lt-esc-token-map*
	  *case-sensitive* *escape-character* *restricted-chars*
	  *sb-print-length* *sb-print-depth*
	  *current-print-length* *current-print-depth*
	  *string-char* *keyword-char* *literal-char*
	  *unparse-style* *parens-off* *no-escapes*
 	  *uterm-bp-count* *uterm-son-count* *uterm-nt-name*
	  bracket-nesting-const
	  *as-stack* *uterm*
	  *apply-lt-dis-fun* *apply-lt-des-fun*
	  *apply-lt-token-cons-fun* apply-lt-token-constructor
	  dis-op get-term-args mk-unp-rept
	  init-as-stack as-peek as-pop as-push as-push-args
	  token
	  mk-keyword-token mk-seq-bp
	  uterm
	  make-uterm-lt init-nt-uterm queue-uterm-son
	  queue-uterm-bp make-spec-bp
	  set-bracket-info set-prec-info init-prec-info
	  unp-keyword unp-jux-keyword dis-lt unp-lt
	  nt-unp unp-uterm upats
	  unp-nt unp-ext-nt unp-opt
	  unp-alt unp-rept unp-double-rept unp-seq unp-jux 
	  match-id match-num match-str match-lit
	  unp-base-id unp-base-num unp-base-str unp-base-lit
	  unp-list unp-cons unp-bcons
	  unp-elist-const unp-null-const unp-list-const unp-term-const
	  unp-name unp-bind
          dis-opt unp-opt-aug dis-alt unp-alt-aug 
	  bracket-uterm
	  toss-name toss-rept

	  ellipsis-token cr-token unindent-token
	  tab-left-token tab-right-token untab-token

	  vnil
	  ))

(defvar vnil nil)			; HACK, to avoid warning message for
					; nonsense variables in generated
					; unparsers.



(defvar *unparse-style* nil
  "Unparsing pattern symbol to select for unparsing.")
(defvar *parens-off* nil
  "Disables the insertion of parens during unparsing.")
(defvar *no-escapes* nil
  "Disables the insertion of escape characters.")
(defvar *sb-print-length* nil
  "Behaves as Lisp's *print-length*, but for SB output.")
(defvar *sb-print-depth* nil
  "Behaves as Lisp's *print-depth*, but for SB output.")



(defvar *current-print-length* 0
  "Counts up to *sb-print-length*")
(defvar *current-print-depth* 0
  "Counts up to *sb-print-depth*")

(defconstant bracket-nesting-const 2)

(defvar *case-sensitive* nil)
(defvar *escape-character* nil
  "Character used for escape in the lexing of strings, identifiers, etc.")
(defvar *string-char* nil)
(defvar *literal-char* nil)
(defvar *keyword-char* nil)
(defvar *restricted-chars* nil
  "Restricted characters used for the reinsertion of escape characters.")

(defvar *bracket-info* nil)
(defvar *prec-info* nil)
(defvar *pat-nesting* 1)
(proclaim '(fixnum *pat-nesting*))
(proclaim '(ftype (function (t) fixnum) bp-value))

(defvar *key-token-map* nil)
(defvar *lt-token-map* nil)
(defvar *key-esc-token-map* nil)
(defvar *lt-esc-token-map* nil)

(defvar *uterm-nt-name* nil)

(defvar *apply-lt-dis-fun* nil)
(defvar *apply-lt-des-fun* nil)
(defvar *apply-lt-token-cons-fun* nil)

(defvar *as-stack* nil)
(defvar *uterm* nil)




;;; Just so we don't repeatedly cons identical tokens.

(defconstant ellipsis-token
  (make-token :kind :lt :subkind :string :value "#"))
(defconstant cr-token
  (make-token :kind :whitespace :subkind :cr))
(defconstant unindent-token
  (make-token :kind :whitespace :subkind :unindent))
(defconstant tab-left-token
  (make-token :kind :whitespace :subkind :tab-left))
(defconstant tab-right-token
  (make-token :kind :whitespace :subkind :tab-right))
(defconstant untab-token
  (make-token :kind :whitespace :subkind :untab))



;;; Generic macros. 


(eval-when (compile eval load)

(defmacro dis-op (op x)
  `(equal ,op
	  (oper:ds-sim-op (term:term-op ,x))))
  

(defmacro get-term-args (term)
  `(term:term-args ,term))

(defmacro mk-unp-rept (args)
  `(mk-sim-term ':unp-temp
		,args))


)
   ;; eval-when





;;; Stack routines for unparsing abstract syntax. 


(eval-when (compile eval load)

(defmacro init-as-stack ()
  nil)

(defmacro as-peek ()
  `(car *as-stack*))

(defmacro as-pop ()
  `(pop *as-stack*))

(defmacro as-push (x)
  `(push ,x *as-stack*))

(defmacro as-push-args (x)
  `(do ((rx (reverse ,x) (cdr rx))	; dest. reverse clobbers A.S.
	(nstack *as-stack* (cons (car rx) nstack)))
       ((null rx) (setf *as-stack* nstack))))

)
   ;; eval-when



(defun mk-seq-bp ()
  (make-bp :value *pat-nesting*))

(defun mk-keyword-token (symbol &optional (subkind nil))
  (make-token :kind :keyword
	      :subkind subkind
	      :value symbol
	      :str-value (if (not *no-escapes*)
			     (insert-escapes (symbol-name symbol)
					     :omit-first t))))
					     

(defun apply-lt-token-constructor (kind value)
  (ecase kind
    ((sbst::id sbst::identifier :id :identifier)
     (make-token :kind :lt
		 :subkind :identifier
		 :value value
		 :str-value (if (not *no-escapes*)
				(insert-escapes (symbol-name value)))))
    ((sbst::number :number)
     (make-token :kind :lt
		 :subkind :number
		 :value value))
    ((sbst::string :string)
     (make-token :kind :lt
		 :subkind :string
		 :str-value 
		 (concatenate 'string
		   (string *string-char*)
		   (if *no-escapes*
		       value
		       (insert-escapes value
				       :delimited `(,*string-char*)))
		   (string *string-char*))))
    ((sbst::literal :literal)		; Meta grammar
     (let ((str (symbol-name value)))
       (make-token :kind :lt
		   :subkind :string
		   :value value
		   :str-value
		   (concatenate 'string
		     (string *literal-char*)
		     (if *no-escapes*
			 str
			 (insert-escapes str))))))

    ((sbst::keyword :keyword)
     (make-token :kind :lt
		 :subkind :string
		 :value value
		 :str-value
		 (concatenate 'string
		   (string *keyword-char*)
		   (if *no-escapes*
		       value
		       (insert-escapes value :delimited `(,*keyword-char*)))
		   (string *keyword-char*))))))




;;; The following may cause side effects since the bp's are always regenerated. 
(defun make-spec-bp (bp)
  (setf (bp-value bp)
	(+ (the fixnum (bp-value bp)) *pat-nesting*))
  bp)





(defun make-uterm-lt (kind as value)
  (declare (type term as))
  (let ((sons (list value)))
    (make-uterm :name kind
		:term as
		:sons sons
		:sons-tl sons)))



(defvar *uterm-son-count* 0)		; used to insure correspondance of
					; these two lists.
(defvar *uterm-bp-count* 0)
(proclaim '(fixnum *uterm-son-count* *uterm-bp-count*))

(defun init-uterm (name as)
  (declare (type term as))
  (make-uterm :name name
	      :term as))


(defun queue-uterm-son (son)
  (let ((uterm *uterm*))
    (incf *uterm-son-count*)
    (cond ((null (uterm-sons uterm))
	   (setf (uterm-sons-tl uterm)
		 (setf (uterm-sons uterm)
		       (list son))))
	  (t
	   (uterm-son-insert-tail uterm son)))))

(defun uterm-son-insert-tail (uterm son)
  (setf (cdr (uterm-sons-tl uterm))
	(list son))
  (setf (uterm-sons-tl uterm)
	(cdr (uterm-sons-tl uterm))))


(defun uterm-son-insert-after-position (uterm son position)
  (cond ((eq position (uterm-sons-tl uterm))
	 (uterm-son-insert-tail uterm son))
	(t
	 (setf (cdr position)
	       (cons son
		     (cdr position))))))


(defun uterm-son-insert-before-position (uterm son position)
  (setf (cdr position)
	(cons (car position)
	      (cdr position)))
  (setf (car position)
	son)
  (cond ((eq (uterm-sons-tl uterm)
	     position)
	 (setf (uterm-sons-tl uterm)
	       (cdr position)))))


(defun queue-uterm-bp (bp)
  (let ((uterm *uterm*))
    (cond ((= *uterm-son-count* (1+ *uterm-bp-count*))
	   (incf *uterm-bp-count*)
	   (cond ((null (uterm-bps uterm))
		  (setf (uterm-bps-tl uterm)
			(setf (uterm-bps uterm)
			      (list bp))))
		 (t
		  (uterm-bp-insert-tail uterm bp))))
	  ((= *uterm-son-count* 0))	; no token to break on. 
	  ((= *uterm-son-count* *uterm-bp-count*)
	   (let* ((old-bp (car (uterm-bps-tl uterm))))
	     (setf (car (uterm-bps-tl uterm))
		   (merge-bps old-bp bp)))) ; rt-format routine!
	  (t
	   (error "Internal -- Mismatched bp and token sequences")))))
	  

(defun uterm-bp-insert-tail (uterm bp)
  (setf (cdr (uterm-bps-tl uterm))
	(list bp))
  (setf (uterm-bps-tl uterm)
	(cdr (uterm-bps-tl uterm))))




(defun uterm-bp-insert-position (uterm bp position)
  (setf (cdr position)
	(cons (car position)
	      (cdr position)))
  (setf (car position)
	bp)
  (cond ((eq (uterm-bps-tl uterm)
	     position)
	 (setf (uterm-bps-tl uterm)
	       (cdr position)))))





(defun non-nil-min (x y)
  (cond ((null x) y)
	((null y) x)
	(t
	 (min (the fixnum x) (the fixnum y)))))



(defun set-bracket-info (nt-name lbracket rbracket table)
  (setf (gethash nt-name table)
	(list (make-token :kind :keyword
			  :value lbracket)
	      (make-token :kind :keyword
			  :value rbracket))))

(defun get-left-bracket (nt)
  (car (gethash nt
		*bracket-info*)))
(defun get-right-bracket (nt)
  (cadr (gethash nt
		 *bracket-info*)))




(defun init-prec-info (nt-name table)
  ;;@@@ Need a better representation here!
  (setf (gethash nt-name table)
	(list (make-hash-table)
	      (make-hash-table)
	      (make-hash-table)
	      (make-hash-table))))

(defun set-prec-info (nt-name oper table-index value table)
  (setf (gethash oper
		 (nth table-index (gethash nt-name table)))
	value))



  
(defun is-initial? (token uterm)
  (and (token-p token)
       (eq :keyword (token-kind token))	; keyword
       (eq token (car (uterm-sons uterm)))))
					; initials must begin the nt
					; production.

(defun is-medial? (token uterm)
  (and (token-p token)
       (eq :keyword (token-kind token))	; keyword
       (uterm-p (car (uterm-sons uterm)))
       (eq (uterm-name uterm)
	   (uterm-name (car (uterm-sons uterm)))) ; left recursive
       (or (get-binding-power :medial-left token uterm)	; HACK!
	   (get-binding-power :medial-right token uterm))))

(defun is-aggregate? (token uterm)		; keyword
  (declare (ignore uterm))
  (and (token-p token)
       (eq :keyword (token-kind token))))
					; must be checked last of the three!



(defun get-binding-power (op-type token uterm)
  (cond ((not (eq (token-kind token) :keyword))
	 ());; only keywords have binding powers.
	((or (and (get-left-bracket (uterm-name uterm))
		  (eq (token-value token)
		      (token-value (get-left-bracket (uterm-name uterm)))))
	     (and (get-right-bracket (uterm-name uterm))
		  (eq (token-value token)
		      (token-value (get-right-bracket (uterm-name uterm))))))
	 ());; brackets have no binding power
	((gethash (uterm-name uterm) *prec-info*)
	 (cond ((gethash (token-value token)
			 (nth (case op-type
				(:initial 0)
				(:medial-left 1)
				(:medial-right 2)
				(:aggregate 3))
			      (gethash (uterm-name uterm) *prec-info*))))
	       (t 0)))
	(t 0)))




(defun bracket-nested-uterm (uterm nested-uterm-pos)
  (let* ((index (- (length (the list (uterm-sons uterm)))
		   (length (the list nested-uterm-pos))))
	 (bp-pos (nthcdr index (uterm-bps uterm))))
    (cond ((not (and (get-left-bracket  (uterm-name uterm))
		     (get-right-bracket (uterm-name uterm))))
	   (unparse-runtime-error
	    "Brackets needed but not found for nonterminal."
	    (uterm-name uterm)
	    t))
	  (t
	   (let ((left-bracket (get-left-bracket (uterm-name uterm)))
		 (right-bracket (get-right-bracket (uterm-name uterm)))
		 (left-bracket-bp
		  (make-bp :value (+ bracket-nesting-const
				     (the fixnum (bp-value (car bp-pos))))))
					; above value is a good guess?
		 (right-bracket-bp
		  (make-bp :value (+ bracket-nesting-const
				     (the fixnum
					  (bp-value (cond ((cadr bp-pos))
							  (t (car bp-pos)))))))))
	     (uterm-son-insert-after-position
	      uterm right-bracket nested-uterm-pos)
	     (uterm-son-insert-before-position
	      uterm left-bracket nested-uterm-pos)
	     (uterm-bp-insert-position uterm right-bracket-bp bp-pos)
	     (uterm-bp-insert-position uterm left-bracket-bp bp-pos))))))




(defun insert-brackets? (uterm nested-uterm
			       rbp-left-of-nested-uterm
			       lbp-right-of-nested-uterm)
  (and (null *parens-off*)
       (eq (uterm-name uterm)		; otherwise no brackets are necessary
	   (uterm-name nested-uterm))
       (> (length (the list (uterm-sons nested-uterm))) 1)
					; hack to prevent bracketing of silly
					; brackets 
       (or (and (uterm-lbp nested-uterm)
		rbp-left-of-nested-uterm
		(< (the fixnum (uterm-lbp nested-uterm))
		   (the fixnum rbp-left-of-nested-uterm)))
	   (and (uterm-rbp nested-uterm) 
		lbp-right-of-nested-uterm
		(< (the fixnum (uterm-rbp nested-uterm))
		   (the fixnum lbp-right-of-nested-uterm))))))



;;;  Note:  Bracket-uterm won't work if there are multiple 
;;;           consecutive operators because then a max and a min need 
;;; 	      to be calculated.

(defun bracket-uterm (uterm)
  (let ((start-marker (uterm-sons uterm))
        marker-left marker-right lbp rbp nested-uterms-to-bracket)
    (do ()
	((null start-marker))
      (let* ((next-uterm (do ((runner start-marker (cdr runner)))
			     ((or (null runner)
				  (uterm-p (car runner)))
			      runner)))
	     (next2-uterm (do ((runner (cdr next-uterm) (cdr runner)))
			      ((or (null runner)
				   (uterm-p (car runner)))
			       runner)))
	     (rbp-to-left nil)
	     (lbp-to-right nil))
	(setf marker-left start-marker)
	(do ()
	    ((eq marker-left next-uterm))
	  (setf rbp-to-left
		(non-nil-min
		 rbp-to-left
		 (cond ((is-initial? (car marker-left) uterm)
			(get-binding-power :initial (car marker-left) uterm))
		       ((is-medial? (car marker-left) uterm)
			(get-binding-power :medial-right
					   (car marker-left)
					   uterm))
		       ((is-aggregate? (car marker-left) uterm)
			(get-binding-power :aggregate
					   (car marker-left)
					   uterm)))))
	  (setf marker-left (cdr marker-left)))

	(setf marker-right (cdr next-uterm))
	(setf start-marker marker-right)
	(do ()
	    ((eq marker-right next2-uterm))
	  (setf lbp-to-right
		(non-nil-min lbp-to-right
			     (if (is-medial? (car marker-right) uterm)
				 (get-binding-power :medial-left
						    (car marker-right)
						    uterm))))
	  (setf marker-right (cdr marker-right)))
	
	(setf lbp (non-nil-min lbp lbp-to-right))
	(setf rbp (non-nil-min rbp rbp-to-left))
	
	(cond ((and next-uterm
		    (insert-brackets? uterm (car next-uterm)
				      rbp-to-left lbp-to-right))
	       (push next-uterm nested-uterms-to-bracket)))))

    (mapc #'(lambda (nested-uterm)
	      (bracket-nested-uterm uterm nested-uterm))
	  nested-uterms-to-bracket)
    (setf (uterm-lbp uterm) lbp)
    (setf (uterm-rbp uterm) rbp)
    uterm
    ))





(defun unp-keyword (symbol)		; reuse keyword tokens
  (let ((token-map (if *no-escapes*
		       *key-token-map*
		       *key-esc-token-map*)))
    (queue-uterm-son (cond ((gethash symbol token-map))
			   (t
			    (setf (gethash symbol token-map)
				  (mk-keyword-token symbol)))))))


(defun unp-jux-keyword (symbol)		; reuse jux keyword tokens
  (queue-uterm-son (cond ((gethash symbol *key-token-map*))
			 (t
			  (setf (gethash symbol *key-token-map*)
				(mk-keyword-token symbol :jux))))))



(defun unp-lt (kind as)			; reuse lex.term. tokens
  (let* ((token-map (if *no-escapes*
			*lt-token-map*
			*lt-esc-token-map*))
	 (value (funcall (the compiled-function *apply-lt-des-fun*) kind as))
	 (token (cond ((gethash (cons kind value) token-map))
		      (t
		       (setf (gethash (cons kind value) token-map)
			     (funcall (the compiled-function
					   *apply-lt-token-cons-fun*)
				      kind
				      value))))))
    (queue-uterm-son (make-uterm-lt kind as token))))



(eval-when (compile eval load)

(defmacro dis-lt (kind as)
  `(funcall *apply-lt-dis-fun*
	    ,kind
	    ,as))

(defmacro unp-nt (routine as)
  `(queue-uterm-son
    (,routine ,as)))

(defmacro unp-ext-nt (routine nt-name as)
  (declare (ignore nt-name))
  `(queue-uterm-son
    (,routine ,as)))


(defmacro unp-opt (flag body)
  `(let ((*pat-nesting* (1+ *pat-nesting*)))
     (cond ( ,flag
	     ,@body))))

(defmacro unp-alt (flag body)
  `(let ((*pat-nesting* (1+ *pat-nesting*)))
     (ecase ,flag
       ,@(do ((index 0 (1+ index))
	      (branches body (cdr branches))
	      (result ()
		      (cons (cons index
				  (car branches))
			    result)))
	     ((null branches)
	      result)
	   (declare (fixnum index))))))

(defmacro unp-rept (as body each-iter-slot bp-ws)
  `(let ((*pat-nesting* (1+ *pat-nesting*))
	 (*current-print-length* 1))
     (setf (uterm-kind *uterm*) :rept)
     (do ((rest (get-term-args ,as)
		(cdr rest)))
	 ((or (null rest)
	      (and *sb-print-length*
		   (>= *current-print-length* *sb-print-length*)))
	  (if rest
	      (queue-uterm-son ellipsis-token)))
       (setf ,each-iter-slot (car rest))
       ,@body
       (queue-uterm-bp (make-spec-bp ,bp-ws))
       (incf *current-print-length*))))


(defmacro unp-double-rept (as body key-body each-iter-slot
			      bp1-ws bp2-ws &optional (junk nil))
  (declare (ignore junk))		; kept for upward compatibility.
  `(let ((*pat-nesting* (1+ *pat-nesting*))
	 (*current-print-length* 1))
     (setf (uterm-kind *uterm*) :rept)
     (do ((rest (get-term-args ,as)
		(cdr rest)))
	 ((or (null rest)
	      (and *sb-print-length*
		   (>= *current-print-length* *sb-print-length*)))
	  (if rest
	      (queue-uterm-son ellipsis-token)))
       (setf ,each-iter-slot (car rest))
       ,@body
       (queue-uterm-bp (make-spec-bp ,bp1-ws))
       (cond ((cdr rest)
	      ,@key-body
	      (queue-uterm-bp (make-spec-bp ,bp2-ws))))
       (incf *current-print-length*))))
			     

(defmacro unp-seq (body bp-ws-list)
  `(let ((*pat-nesting* (1+ *pat-nesting*)))
     ,@(do ((sons body (cdr sons))
	    (bp-ws bp-ws-list (cdr bp-ws))
	    (result nil
		    (cons `(queue-uterm-bp
			    (make-spec-bp ,(car bp-ws)))
			  (append (reverse (car sons))
				  result))))
	   ((null sons)
	    (nreverse result)))))


(defmacro unp-jux (body1 jux body2 bp1-ws bp2-ws &optional (mid-ws nil))
  `(let ((*pat-nesting* (1+ *pat-nesting*)))  
     ,@body1
     (queue-uterm-bp (make-spec-bp ,bp1-ws))
     (unp-jux-keyword ,jux)
     (queue-uterm-bp
      (let ((ws ,mid-ws))
	(if ws
	    (make-spec-bp ws)
	    (mk-seq-bp))))
     ,@body2
     (queue-uterm-bp (make-spec-bp ,bp2-ws))))



  

(defmacro match-id (symbol term)
  `(let ((x ,term))
     (and (is-id x)
	  (eq (ds-id x) ,symbol))))

(defmacro match-num (number term)
  `(let ((x ,term))
     (and (is-number x)
	  (= (ds-number x) ,number))))

(defmacro match-str (string term)
  `(let ((x ,term))
     (and (is-string x)
	  (string= (ds-string x) ,string))))

(defmacro match-lit (symbol term)
  `(let ((x ,term))
     (and (is-literal x)
	  (eq (ds-literal x) ,symbol))))

(defmacro unp-base-id ()
  `(as-pop))

(defmacro unp-base-num ()
  `(as-pop))

(defmacro unp-base-str ()
  `(as-pop))

(defmacro unp-base-lit ()
  `(as-pop))


  
  
(defmacro unp-list ()
  `(let ((x (as-pop)))
     (as-push-args (get-term-args x))))

(defmacro unp-cons ()
  `(let ((x (get-term-args (as-pop))))
     (as-push-args (list (car x)
			 (mk-unp-rept (cdr x))))))

(defmacro unp-bcons ()
  `(let ((x (get-term-args (as-pop))))
     (as-push-args (list (mk-unp-rept (butlast x))
			 (car (last x))))))

(defmacro unp-elist-const ()
  `(as-pop))
(defmacro unp-null-const ()
  `(as-pop))
(defmacro unp-list-const ()
  ())

(defmacro unp-term-const (op-name)
  (declare (ignore op-name))
  `(as-push-args (get-term-args (as-pop))))

(defmacro unp-name (name slot)
  (declare (ignore name))
  `(setf ,slot
	 (as-pop)))

(defmacro toss-name (name)
  (declare (ignore name))
  `(as-pop))

(defmacro toss-rept ()
  `(as-pop))

(defmacro unp-bind (name slot)
  (declare (ignore name))
  `(as-push ,slot))


  

(defmacro dis-opt (key-slot discrim-fun1 discrim-fun2)
  `(setf ,key-slot
	 (cond ((,discrim-fun1 (as-peek))
		nil)
	       ((,discrim-fun2 (as-peek))
		t)
	       (t
		(unparse-runtime-error
		 "Optional not discriminated to either branch."
		 (as-peek))))))

(defmacro unp-opt-aug (key-slot body1 body2)
  `(cond (,key-slot ,@body2)
	 (t ,@body1)))

(defmacro dis-alt (key-slot discrim-funs)
  `(setf
    ,key-slot
    (cond
     ,@(do ((index 0 (1+ index))
	    (branches discrim-funs (cdr branches))
	    (result ()
		    (cons `((,(car branches) (as-peek))
			    ,index)
			  result)))
	   ((null branches)
	    (nreverse
	     (cons `(t
		     (unparse-runtime-error
		      "Alternation not discriminated to any branch."
		      (as-peek)))
		   result)))
	 (declare (fixnum index))))))

(defmacro unp-alt-aug (key-slot body-list)
  `(ecase ,key-slot 
     ,@(do ((index 0 (1+ index))
	    (branches body-list (cdr branches))
	    (result ()
		    (cons (cons index
				(car branches))
			  result)))
	   ((null branches)
	    result)
	 (declare (fixnum index)))))


  )					; eval-when



(eval-when (compile eval load)

(defmacro nt-unp (nt-name as body)
  `(let* ((*uterm-nt-name* ,nt-name)
	  (*as-stack* (init-as-stack))
	  (*uterm* (init-uterm *uterm-nt-name* ,as))
	  (*uterm-son-count* 0)
	  (*uterm-bp-count* 0)
	  (*pat-nesting* nesting-constant))
     ;; The above is just our starting value for *pat-nesting*,
     ;; since we don't want any real negative values for user's
     ;; relative prefernces. (nesting-constant is from rt-format).
     (setf (uterm-kind *uterm*) :nt)
     (cond ((and *sb-print-depth*
		 (>= *current-print-depth* *sb-print-depth*))
	    (queue-uterm-son ellipsis-token)
	    *uterm*)
	   (t
	    (incf *current-print-depth*)
	    ,@body
	    (decf *current-print-depth*)
	    (bracket-uterm *uterm*)
	    ;; cache UTERM in argument AS.  (or this occurs at another level).
	    (cond ((and (= (length (uterm-sons *uterm*)) 1)	
					; flatten uterm structure.
			(uterm-p (car (uterm-sons *uterm*)))
			(not (eq (uterm-kind *uterm*) :rept)))
		   (setf (uterm-name (car (uterm-sons *uterm*)))
			 ,nt-name)
		   (setf (uterm-term (car (uterm-sons *uterm*)))
			 ,as)
		   (car (uterm-sons *uterm*)))
		  (t
		   *uterm*))))))

(defmacro unp-uterm (as body)
  `(let ((new-uterm (init-uterm *uterm-nt-name* ,as)))
     (let ((*uterm* new-uterm)
	   (*uterm-son-count* 0)
	   (*uterm-bp-count* 0)
	   (*pat-nesting* nesting-constant))
       ;; The above is just our starting value for *pat-nesting*, since we
       ;; don't want any real negative values for user's relative prefernces.
       ;; (nesting-constant is from rt-format).
       ,@body
       (bracket-uterm *uterm*))
     ;; cache UTERM in argument AS.  (or this occurs at another level).
     (queue-uterm-son 
      (cond ((and (= (length (uterm-sons new-uterm)) 1) 
					; flatten uterm structure.
		  (uterm-p (car (uterm-sons new-uterm)))
		  (not (eq (uterm-kind new-uterm) :rept)))
	     (setf (uterm-term (car (uterm-sons new-uterm)))
		   ,as)
	     (car (uterm-sons new-uterm)))
	    (t
	     new-uterm)))))


(defmacro upats (&rest clauses)
  (let ((clause (car clauses)))
    (if (and (car clause)
	     (consp (car clause)))
	`(if (intersection *unparse-style* ',(car clause))
	     ,(cadr clause)
	     (upats ,@(cdr clauses)))
	(cadr clause))))



  )  
   ;; eval-when







(defun unparse-runtime-error (string object &optional (error? t))
  (format t "~%Unparse runtime error.~%")
  (format t string)
  (format t "~%Object: ~S~%" object)
  (if error? 
      (error "Can't continue from here.")))




