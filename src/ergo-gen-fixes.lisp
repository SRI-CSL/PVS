;;; Replaces a function in file ess/lang/sb-term/unparse-gen.lisp
;;; Makes jux operator symbol local to package of grammar, rather than
;;; package sbst.  Spacing info for jux is now obeyed.

(in-package :syntax-box)

(defun build-parse (gram-struct)
  (let (guts)
    (do ((nts *look-ahead-sets* (cdr nts)))
	((null nts))
      (push `(,(intern (symbol-name (caar nts))
		       (lang:lang-abs-syn-package *language*))
	      (,(make-function-name (caar nts)) 0)) guts))
    (push `(t (error "Unknown nonterminal ~A." nt)) guts)
    (save-function
     `(defun ,(sb-intern-nupcase (lang:lang-parse-routine-name *language*))
	(&key (nt
	       ',(nt-name-abs-package
		  (car (grammar-nonterminal-definitions
			gram-struct))))
	      error-threshold
	      ask-about-bad-tokens
	      (return-errors nil)
	      (stream nil streamp)
	      string
	      file
	      (exhaust-stream nil)
	      )
	(cond (stream)
	      (string (setf stream (make-string-input-stream string)))
	      (file (setf stream (open file)))
	      (t (error "Must provide an input means -- either :stream, ~
			:string, or :file.")))
	(setq ,*conc-keyword-table*
	      (init-keyword-table ,*conc-keyword-list*))
	(let ((*parser-error* nil)
	      (*parser-error-count* 0)
	      (*parser-return-errors* return-errors)
	      (*parser-abort-threshold* error-threshold)
	      (*ask-about-bad-tokens* ask-about-bad-tokens)
	      (*lexical-stream* (open-lexical-stream
				 (open-placestream stream)))
	      (*hold-a1* nil) (*hold-a2* nil) (*hold-a3* nil) (*hold-a4* nil)
	      (*hold-b1* nil) (*hold-b2* nil) (*hold-b3* nil) (*hold-b4* nil)
	      (*abs-syn-package* ,*conc-abs-syn-package*)
	      (*reader-fun* #'reader)
	      (*apply-lex-term-constr-fun* #'apply-lexical-terminal-constructor)
	      (*keyword-table* ,*conc-keyword-table*)
	      (*close-comment-char* ,*conc-close-comment-char*)
	      (*case-sensitive* ,*conc-case-sensitive*))
	  (init-lexer *lexical-stream*)
	  (multiple-value-bind
	      (abs-syntax error-string error-args error-place)
	      (unwind-protect
		  (catch 'parser-abort-catch
		    (prog1
			(case nt ,@(nreverse guts))
		      (when (or file string exhaust-stream)
			(clet* (((token ignore place) (peek-first)))
			  (unless (or (eq token :eof)  ; file.
				      (eq token 'eof)) ; string.
			    (do-syntax-error
			     "There is garbage at the end of your ~
			      file or string:~%~A"
			     place))))))
		(unless streamp (close stream)))
	    (if return-errors
		(values abs-syntax *parser-error* 
			error-place error-string error-args)
		(values abs-syntax *parser-error*))))))))

;;; Get rid of keyword args in an attempt to speed things up.

(defun gen-nt-unparse-routine ()
  `(defun ,(mk-unparse-name) (nt as &key (style *unparse-style*))
     (let* ((*unparser-op-list* ,*conc-all-operators-list*)
	    (*key-token-map* ,(mk-key-token-table-name))
	    (*key-esc-token-map* ,(mk-key-esc-token-table-name))
	    (*lt-token-map* ,(mk-lt-token-table-name))
	    (*lt-esc-token-map* ,(mk-lt-esc-token-table-name))
	    (*apply-lt-dis-fun* #'apply-lexical-terminal-discriminator)
	    (*apply-lt-des-fun* #'apply-lexical-terminal-destructor)
	    (*apply-lt-token-cons-fun* #'apply-lt-token-constructor)
	    (*bracket-info* ,(mk-bracket-table-name))
	    (*prec-info* ,(mk-prec-table-name))
	    (sort (sort:opsig-output
		   (sort:opsig-table-lookup (term:term-op as))))
	    (nt-name (cond (nt)
			   ((sort:is-sort-ttype sort)
			    (sort:ds-sort-ttype sort))
			   (t 
			    ',(nt-name-abs-package
			       (car (grammar-nonterminal-definitions
				     *grammar-term*))))))

	    (*case-sensitive* ,*conc-case-sensitive*)
	    (*escape-character* ,*conc-escape-char*)
	    (*restricted-chars* ,*conc-restricted-chars*)
	    (*string-char* ,*conc-string-char*)
	    (*literal-char* ,*conc-literal-char*)
	    (*keyword-char* ,*conc-keyword-char*)
	    (*unparse-style* (if (and style
				      (not (consp style)))
				 (list style)
				 style))
	    (*no-escapes* (or *no-escapes*
			      (null (insert-escapes?))))
	    (*current-print-depth* 1))

       (let ((uterm
	      (case nt-name
		    ,@(do ((runner (grammar-nonterminal-definitions
				    *grammar-term*)
 				   (cdr runner))
			   (result ()
				   (cons
				    `(,(nt-name-abs-package (car runner))
				      (,(mk-unparse-routine-name
					 (nt-name (car runner)))
				       as
				       t)) ; was :TOP-LEVEL? T
				    result)))
			  ((null runner)
			   (reverse
			    (cons `(t (unparse-runtime-error
				       "Nonterminal not unparsable:"
				       nt-name nil)
				      ());; return nil to avoid mapping above.
				  result)))))))
	    uterm))))

(defun gen-nt-attr (nt)
  (let ((nt-name (nt-name nt)))
    `(defun ,(mk-unparse-routine-name nt-name) (as
						&optional (top-level? nil))
       (memo-uterm as #',(mk-aux-unparse-routine-name nt-name)
		   top-level?))))


;;; Handle jux correctly

(defun get-spacing-symbol (term)
  (cond ((is-keyword term)
	 (sbst-intern-ncase (ds-keyword term)))
	((and (is-id term)
	      (memq (ds-id term)
		    '(arb op lt)))
	 (keyword-intern (ds-literal term)))
	((and (is-id term)
	      (is-lexical-terminal (sb-intern-case (ds-id term))
				   *grammar-term*))
	 (sbst-intern-case (ds-id term)))
	((is-id term)			; NT
	 (ds-id term))
	((eq (sim-term-op term) 'jux-op)
	 (if (is-sop 'null
		     (term-arg0 term))
	     'jux 
	     (mk-jux-name (term-arg0 term))))))
; Original ergo code:
;	 (sbst-intern-upcase (if (is-sop 'null
;					 (term-arg0 term))
;				 'jux 
;				 (mk-jux-name (term-arg0 term)))))))

;;; Fix mk-jux-name so it can be called with a number or symbol
;;; as its argument, as well as a number- or id-term.

(defun mk-jux-name (postfix)
  (if postfix
      (sb-intern-upcase
       (concatenate 'string
	 "JUX^"
	 (cond ((numberp postfix) (princ-to-string postfix))
	       ((symbolp postfix) (symbol-name postfix))
	       ((is-number postfix) (princ-to-string (ds-number postfix)))
	       (t (symbol-name (ds-id postfix))))))
      'jux))

;;; Next 4 fns replace code in collapse.lisp, from ess/lang/sb-term/rel.
;;; These fns produce the code to parse a non-terminal.  The
;;; modification wraps a "(let ((place (caddr (multiple-value-list 
;;; (peek-first))) ... ) around the code for the nt, so that place
;;; stores the current place, and can be used in calls to make-sb-term,
;;; so that lexical position can be stored with terms.

(defun collapse-i-only (fragment-numbers fragment-list number-of-slots 
			nt nt-as fs-list need-brackets)
  (let* (branch-list fragment
             ; list of the main fragments.		    
	 (fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  fragment))
		  fragment-numbers))

             ; optimize the main fragment look-ahead sets.
	 (opt-las-list (optimize-las-list fragments)))
    
        ; for each main branch
    (do ((branches fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

         ; build branch entry ((la-match optimized-LAS)
	 ;		           collapsed parsing code)
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    branch-list))
    	
        ; catch errors.
    (push `(t (initial-error (quote ,fs-list))) branch-list)
    (setq branch-list (cons 'initials-only (nreverse branch-list)))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))
	(declare (ignore rbp))
	,(if (not need-brackets) '(declare (ignore bracket-list)))
	(let ,(make-let-variables number-of-slots)

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))
	  ;; next line added to ergo code -- SJP 9/5/90
	  (let* ((peek (multiple-value-list (peek-first)))
		 (place (caddr peek))
		 (comment (cadddr peek)))
	    ,branch-list
	    ,nt-as
					; final result is always in slot 0
	    ,(slot-code 0)))))))



(defun collapse-bracket-i-only (fragment-numbers fragment-list number-of-slots 
						 nt nt-as fs-list gram-struct)

  (let* ((branch-list nil) (fragment nil)
	 (brackets (mapcar #'make-keyword
			   (reverse (get-nt-brackets nt gram-struct))))
					; revised srd
	 (leading-bracket (make-keyword (get-nt-left-bracket nt gram-struct)))
             ; get fragments for main branches
	 (fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  fragment))
		  fragment-numbers))

             ; optimize look ahead sets for main branches.
	 (opt-las-list (optimize-las-list fragments)))
    
        ; for each main branch
    (do ((branches fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

         ; build branch entry ((la-match optimized-LAS) 
	 ;		           collapsed parsing code)
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    branch-list))
    	
       ; catch errors -- do not want to fall out of the bottom.
    (push `(t (initial-error (quote ,fs-list))) branch-list)
    (setq branch-list (cons 'initials-only (nreverse branch-list)))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))
	(declare (ignore rbp))
	
	(let ,(make-let-variables number-of-slots)

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))
	  ;; next line added to ergo code -- SJP 9/5/90
	  (let* ((peek (multiple-value-list (peek-first)))
		 (place (caddr peek))
		 (comment (cadddr peek)))

	    ;; keep track of all the leading brackets.
	    (stack-brackets ,leading-bracket ,nt)

	    ,branch-list
	    ,nt-as
	    ;; eat all of the necessary closing brackets.
	    (eat-brackets ,brackets ,nt)

					; final result is always in slot 0
	    ,(slot-code 0)))))))



(defun collapse-im (initials medials number-of-slots fragment-list nt nt-as
			     gram-struct initial-fs-list medial-fs-list
			     need-brackets jux-branches)

  (let* ((init-list nil) (medial-list nil) (init-fragment nil)
	 (regular-ops nil) (lbp-code nil) (mvar 'mtemp)
	 (var-list (cons mvar (make-let-variables number-of-slots)))
	     ; left binding powers.
	 (lbps (get-nt-prec-medial-lefts nt gram-struct))
             ; initial branch fragments
	 (init-fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq init-fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  init-fragment))
		  initials))
             ; optimize initial look ahead sets
	 (opt-las-list (optimize-las-list init-fragments)))
    
         ; for each initial branch (This is identical to the code used
					 ; for collapse-I-only)
    (do ((branches init-fragments (cdr branches)) (fragment-las))
	((null branches))
	    
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

          ; save initial branch code
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    init-list))

    	
       ; catch errors
    (push `(t (initial-error (quote ,initial-fs-list))) init-list)
    (setq init-list (cons 'initials (nreverse init-list)))


        ; for each medial
    (do ((meds medials (cdr meds)) (jux-value) (fragment))
	((null meds))
	     
          ; get the fragment
      (setq fragment (assoc (car meds) fragment-list))
		   
      (cond ((null fragment) (my-error 28))

	    ((setq jux-value (assoc (get-number fragment) jux-branches))
	     (push (list (cadr jux-value)
			 (collapse (get-code fragment) fragment-list))
		   medial-list))

	    (t
	         ; add an entry for the medial case statement
	         ; Medials only have one symbol of look ahead.
	     (let ((medial-fragment-las
		    (make-medial-branch-las (get-fs fragment))))

	       (if (atom medial-fragment-las)
		   (setq regular-ops (union (list medial-fragment-las)
					    regular-ops))
		   (setq regular-ops (union medial-fragment-las regular-ops)))


	       (push (list medial-fragment-las
			   (medial-peep-hole
			    (collapse (get-code fragment) fragment-list)
			    medial-fragment-las))
		     medial-list)))))

    (push `(t (medial-error (quote ,medial-fs-list))) medial-list)
    (setq medial-list (append `(case ,mvar) (nreverse medial-list)))
	 

    (setq lbp-code (build-lbp-code mvar regular-ops jux-branches lbps))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))

	,(if (not need-brackets) '(declare (ignore bracket-list)))
	(let ,var-list

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))
	  ;; next line added to ergo code -- SJP 9/5/90
	  (let* ((peek (multiple-value-list (peek-first)))
		 (place (caddr peek))
		 (comment (cadddr peek)))

	    ,init-list
	    ,nt-as
	 
	    ;; pick up as many medials as possible -- as long as
	    ;; LBP(next token) > rbp.

	    (cond ((> ,lbp-code rbp)
		   (loop
					; set all the variables to nil.
		    (clean-variables ,(cddr var-list))
		    ,medial-list
		    ;; each time through we need to do the abstract syntax
		    ,nt-as
		    (when (<= ,lbp-code rbp) (return nil)))))
		   
					; final result is always in slot 0
	    ,(slot-code 0)))))))


;;; This collapse is also modified to have a local variable
;;; "operator-place", which is used by build-lbp-code to save the place
;;; of the operator, so that make-sb-term can grab it and put it in the
;;; name of the application corresponding to a term-expr.

(defun collapse-bracket-im (initials medials number-of-slots fragment-list nt
			    nt-as gram-struct initial-fs-list medial-fs-list
			    jux-branches)

  (let* ((init-list nil) (init-fragment nil) (medial-list nil)
	 (mvar 'mtemp) (lbp-code nil) (regular-ops nil)
	 (var-list (cons mvar (make-let-variables number-of-slots)))
	 ;; left binding powers
	 (lbps (get-nt-prec-medial-lefts nt gram-struct))
	 (brackets (mapcar #'make-keyword
			   (reverse (get-nt-brackets nt gram-struct))))
	 (leading-bracket (make-keyword (get-nt-left-bracket nt gram-struct)))
					; revised srd, list of main initial
					; fragments.
	 (init-fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq init-fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  init-fragment))
		  initials))
             ; optimize initial look ahead sets
	 (opt-las-list (optimize-las-list init-fragments)))
    
        ; for each initial branch
    (do ((branches init-fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las
	    (cadr (assoc (get-number (car branches)) opt-las-list)))

          ; save initial branch code
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    init-list))


        ; catch errors
    (push `(t (initial-error (quote ,initial-fs-list))) init-list)
    (setq init-list (cons 'initials (nreverse init-list)))

        ; for each medial
    (do ((meds medials (cdr meds)) (jux-value) (fragment))
	((null meds))
	     

          ; get fragment
      (setq fragment (assoc (car meds) fragment-list))

      (cond ((null fragment) (my-error 28))

	    ((setq jux-value (assoc (get-number fragment) jux-branches))
	     (push (list (cadr jux-value)
			 (collapse (get-code fragment) fragment-list))
		   medial-list))

	    (t
	         ; build entry for branch in the medial case statement.
	     (let ((medial-fragment-las
		    (make-medial-branch-las (get-fs fragment))))

	       (if (atom medial-fragment-las)
		   (setq regular-ops (union (list medial-fragment-las)
					    regular-ops))
		   (setq regular-ops (union medial-fragment-las regular-ops)))


	       (push (list medial-fragment-las
			   (medial-peep-hole
			    (collapse (get-code fragment) fragment-list)
			    medial-fragment-las))
		     medial-list)))))
    
        ; catch errors
    (push `(t (medial-error (quote ,medial-fs-list))) medial-list)
    (setq medial-list (append `(case ,mvar) (nreverse medial-list)))
	 

    (setq lbp-code (build-lbp-code mvar regular-ops jux-branches lbps))
    
    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))


	(let ,var-list

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))
	  ;; next line added to ergo code -- SJP 9/5/90
	  (let* ((peek (multiple-value-list (peek-first)))
		 (place (caddr peek))
		 (comment (cadddr peek)))

					; keep track of all leading brackets.
	    (stack-brackets ,leading-bracket ,nt)
	  
	    ,init-list
	    ,nt-as

					; eat as many brackets as necessary.
	    (eat-brackets ,brackets ,nt)

	    (cond ((> ,lbp-code rbp)
		   (loop
					; set all variables to nil
		    (clean-variables ,(cddr var-list))
		    ,medial-list
					; do the nt abstract syntax again
		    ,nt-as
					; eat as many brackets as necessary
		    (eat-brackets ,brackets ,nt)
		    (when (<= ,lbp-code rbp) (return nil)))))
		   
					; final result is always in slot 0
	    ,(slot-code 0)))))))



;;; Replaces function in file phase-three.lisp, from
;;; ess/lang/sb-term/rel.  This fn creates an intermediate form of the
;;; code to construct abstract syntax, that is to be contained in the
;;; parsing routine for a non-terminal.  The changes for pvs are to
;;; calls to the function make-sb-term, which originally took
;;; 2 arguments, now takes an optional third, which is the position of
;;; the lexical start position of the current non-terminal, that is to
;;; be included into the abstract syntax.  The end position is obtained
;;; by mk-term (which is called by make-sb-term), by a call to peek-first.

(defun as-parse-gen (as-pat gram-struct)

  (case (get-as-kind as-pat)
    (number
     `(mk-number ,(get-aug-leaf-value as-pat)))
    (string
     `(mk-string ,(get-aug-leaf-value as-pat)))
    (id
     `(mk-id ',(get-aug-leaf-value as-pat)))
    (literal
     `(mk-id ',(get-aug-leaf-value as-pat)))

    (null
     (error "Internal error: Attempting to generate code for a null." ))

    (list
     `(mk-temp-rept (list ,@(mapcar #'(lambda (x)
					(as-parse-gen x gram-struct))
				    (get-as-args as-pat)))))
    (cons
     `(mk-temp-rept
       (cons ,(as-parse-gen (car (get-as-args as-pat))
			    gram-struct)
	     (ds-temp-rept ,(as-parse-gen (cadr (get-as-args as-pat))
					  gram-struct)))))

    (bcons
     `(mk-temp-rept
       (append (ds-temp-rept ,(as-parse-gen (car (get-as-args as-pat))
					    gram-struct))
	       (list ,(as-parse-gen (cadr (get-as-args as-pat))
				    gram-struct)))))
			     

    (append
     `(mk-temp-rept
       (append (ds-temp-rept ,(as-parse-gen (car (get-as-args as-pat))
					    gram-struct))
	       (ds-temp-rept ,(as-parse-gen (cadr (get-as-args as-pat))
					    gram-struct)))))
     
     

    ;; term constructor call. 
    (term-const
     (cond ((has-list-arg? as-pat)	;  routine imported from sort-gen.
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   (ds-temp-rept
			    ,(as-parse-gen (cadr (get-as-args as-pat))
					   gram-struct))
			   place
			   comment))
	   ((or (has-null-arg? as-pat)	; routines imported from sort-gen.
		(has-elist-arg? as-pat))
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   nil
			   place
			   comment))
	   (t
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   (list ,@(mapcar #'(lambda (x)
					       (as-parse-gen  x gram-struct))
					   (cdr (get-as-args as-pat))))
			   place
			   comment))))

    
    ;; or it is a constructor call.  Deal with sons and slap function name onto
    ;; the front of the resulting term.


        ; If the type is name then we just need to return the
        ; value from that slot.
    ((name ext-name)
     (cond ((and (pattern-p (car (augment-path as-pat)))
		 (memq (pattern-kind (car (augment-path as-pat)))
		       '(star doublestar plus doubleplus)))
	    `(ck-rept-ref ,(slot-code (get-as-key as-pat))))
	   ;; srd revised.  This is a hack. It would be better if all these
	   ;; values were initialed properly, but this is hard because of the
	   ;; way the parser generator flattens things.  This is a much easier
	   ;; and not too expensive fix.
	   (t
	    (slot-code (get-as-key as-pat)))))


	       
        ; If the type is bind then evaluate the son and store
        ; the result in the appropriate slot (the slot specified in the bind)
    (bind 
     (let ((as-code (as-parse-gen (get-as-first-son as-pat) gram-struct)))
       (cond ((and (atom as-code)
		   (eq as-code (slot-code (get-as-result as-pat))))
	      nil)  ; statement has no effect, we are moving a slot to itself.
	     ((atom as-code)
	      `(value-to-slot ,(slot-code (get-as-result as-pat)) ,as-code))
	     (t
	      `(as-to-slot ,(slot-code (get-as-result as-pat)) ,as-code)))))
	       
	       
        ; internal-bind's only needed for unparser generation (finding
	; discriminators).  They do not really do anything for parser
	; generation. 
    (internal-bind nil)

    
    
    ;; internal as-type for unrolled loops.  Cons partial result into correct
    ;; slot for the loop.
    (piece-of-loop
     `(cons-to-slot ,(slot-code (get-as-second-son as-pat))
		    ,(slot-code (get-as-first-son as-pat))))
    ;; See documentation for gen-star-parse to understand why result will be a
    ;; term rather than a list.  (srd term revision, 12/86).
    
    ;; internal as-type -- we have unrolled a construct out of existence.
    ;; known-branch is used to set its flag slot.
    (known-branch
     `(value-to-slot ,(slot-code (get-as-second-son as-pat))
		     ,(get-as-first-son as-pat)))

        ; If the type is and --just deal with each son.
    (and
     (add-let (mapcar #'(lambda (x)
			  (as-parse-gen x gram-struct))
		      (get-as-args as-pat))))
       
        ; If the type is opt we have to build code that
        ; checks its key field to see which branch to
        ; call.
    (opt
     `(if (= ,(slot-code (get-as-key as-pat)) 0)
	  ,(as-parse-gen (get-as-first-son as-pat) gram-struct)
	  ,(as-parse-gen (get-as-second-son as-pat) gram-struct)))


        ; For an alt we have to check the key field and
        ; choose the right branch based in that value.   Some optimization
        ; here.
    (alt
     (let (branches as-code temp optimize-results)

       (do ((sons (get-as-args as-pat) (cdr sons)) (count 0 (+ count 1)))
	   ((null sons))

	 (setq as-code (as-parse-gen (car sons) gram-struct))

	     ; if as-code is an atom then we are just getting the value of some
	     ; slot.  Other branches may be doing the same thing so we want
	     ; to save the slot-name and the count. SO we can collapse all of
	     ; these branches into one.
	 (cond ((atom as-code)
		(setq temp (assoc as-code optimize-results))

		    ; if the slot-name has already been seen then just add 
		    ; the current branch number to the list keys.
		 (if temp (setf (cdr temp) (cons count (cdr temp)))
		          (push (list as-code count) optimize-results)))

		(t (push (list count as-code) branches))))

           ; special case -- if branches is empty then we only have slot name
	   ; references and if we only have one entry in optimize results
	   ; then everyone is refering to the same slot -- no need to build
	   ; a case.

       (cond ((and (null branches) (= (length optimize-results) 1))
	      (caar optimize-results))

	     (t (do ((entries optimize-results (cdr entries)))
		    ((null entries))

		  (if (= (length (car entries)) 2)
		      (push (reverse (car entries)) branches)
		      (push (list (cdar entries) (caar entries)) branches)))

		(append `(case ,(slot-code (get-as-key as-pat))) branches)))))

        ; The list operator says to build a default sequence 
        ; using the abstract syntax results from each son.

    ;; @@ Eventually this bracnh should go away
      
    (flatten
     `(let ((first ,(as-parse-gen (get-as-second-son as-pat) gram-struct))
	    (second ,(as-parse-gen (nth 2 (get-as-args as-pat)) gram-struct)))

	(make-term 
	 ,(get-term-const-op as-pat)
	 (append
	  (if (funcall ,(get-discriminator as-pat gram-struct) first)
	      (car (funcall ,(get-destructor as-pat gram-struct) first))
	      (list first))
	  (if (funcall ,(get-discriminator as-pat gram-struct) second)
	      (car (funcall ,(get-destructor as-pat gram-struct) second))
	      (list second))))))

    (t
     (sb-system-error))))


;;;; Replaces a function in lexer-gen.lisp.  The call to
;;;; check-comment-chars-with-ops has been taken out, so that the initial
;;;; comment chars '-', '(', and ')' don't generate warnings.  Overlap
;;;; problems are taken care of elsewhere, at runtime.  The comment
;;;; indicators are really -- and (* *), but ergo only allows a grammar
;;;; specification to give single character comment indicators.
;(defun generate-custom-lexer (grammar lexer-file-spec)
;  (let* ((keywords (mapcar #'(lambda (x)
;                               (sbst-intern-case x))
;                           (get-keywords *global-lexer-information*)))
;	 (operator-list (mapcar #'(lambda (x)
;                                    (sbst-intern-case x))
;                                (get-operator-list grammar keywords)))
;	 (parsed-oplist (parse-op operator-list))
;	 (open-comment-char
;	  (if (grammar-open-comment-char grammar)
;	      (schar (grammar-open-comment-char grammar) 0)))
;	 (close-comment-char
;	  (if (grammar-close-comment-char grammar)
;	      (schar (grammar-close-comment-char grammar) 0)))
;	 (new-line-comment-char
;	  (if (grammar-new-line-comment-char grammar)
;	      (schar (grammar-new-line-comment-char grammar) 0)))
;	 (escape-char
;	  (if (grammar-escape-character grammar)
;	      (schar (grammar-escape-character grammar) 0)))
;	 (case-sensitive (grammar-case-sensitive? grammar))
;	 single-char-ops multi-char-ops)
;;    (check-comment-chars-with-ops (list new-line-comment-char
;;					open-comment-char
;;					close-comment-char) 
;;				  operator-list)
;    (multiple-value-setq (single-char-ops multi-char-ops)
;      (parse-op-tree parsed-oplist))
;    (setf *global-operator-list* operator-list)
;    (with-open-file (s lexer-file-spec :direction :output :if-exists
;		       		       :new-version)
;      (sb-write
;       (format t "   writing file ~S.~%" lexer-file-spec)
;       (initialize-file s :lexer)
;       (dolist (x `((defparameter ,*conc-keyword-list* ',keywords)
;		    (defparameter ,*conc-single-char-op-list* ',single-char-ops)
;		    (defparameter ,*conc-multi-char-op-list*
;		      ,(if multi-char-ops `',(mk-char-ops multi-char-ops)))
;		    (defparameter ,*conc-all-operators-list*
;		      ',*global-operator-list*)
;		    (defparameter ,*conc-new-line-comment-char*
;		      ,new-line-comment-char)
;		    (defparameter ,*conc-open-comment-char*
;		      ,open-comment-char)
;		    (defparameter ,*conc-close-comment-char*
;		      ,close-comment-char)
;		    (defparameter ,*conc-escape-char* ,escape-char)
;		    (defparameter ,*conc-case-sensitive* ,case-sensitive)
;
;		    (defparameter ,*conc-string-char*
;		      ,(if (is-lexical-terminal 'string grammar)
;			   (schar (get-lt-delimiter 'string grammar) 0)))
;		    (defparameter ,*conc-keyword-char*
;		      ,(if (is-lexical-terminal 'keyword grammar)
;			   (schar (get-lt-delimiter 'keyword grammar) 0)))
;		    (defparameter ,*conc-literal-char*
;		      ,(if (is-lexical-terminal 'literal grammar)
;			   (schar (get-lt-delimiter 'literal grammar) 0)))
;		    
;		    (defparameter ,*conc-restricted-chars*
;		      (reduce #'(lambda (R S)
;				  (union R S :test #'char=))
;			      (list ,*conc-single-char-op-list*
;				    (if ,*conc-new-line-comment-char*
;					(list ,*conc-new-line-comment-char*))
;				    (if ,*conc-open-comment-char*
;					(list ,*conc-open-comment-char*))
;				    (if ,*conc-close-comment-char*
;					(list ,*conc-close-comment-char*))
;				    (if ,*conc-escape-char*
;					(list ,*conc-escape-char*))
;				    (if ,*conc-string-char*
;					(list ,*conc-string-char*))
;				    (if ,*conc-keyword-char*
;					(list ,*conc-keyword-char*))
;				    (if ,*conc-literal-char*
;					(list ,*conc-literal-char*)))))
;		    (defvar ,*conc-keyword-table* nil)))
;	 (pprint x s)))
;      (gen-defun-init-lexer s)
;      (dolist (op-tree multi-char-ops)
;	(gen-defun-lex s op-tree))
;      (terpri s))))


;;; From ess/lang/sb-term/rel/lexer-gen.lisp
;;; Fixed so that char= is not used on :eof

(defun mk-defun-lex-body (root prefix)
  (declare (type op-tree root)
	   (type simple-string prefix))
  (cond ((not (op-tree-children root)) (list `',(intern prefix *sbst-package*)))
	(t (list `(setf holdchar (lexical-read-char stream :eof))
		 `(if (and ,*conc-escape-char*
			   (eql holdchar ,*conc-escape-char*))
		      (setf holdchar (lexical-read-char stream :eof)))
		 `(cond ,@(let ((stuff nil)
				(new-prefix nil))
			    (dolist (child (op-tree-children root) stuff)
			      (setf new-prefix
				    (concatenate 'simple-string prefix
						 (string (op-tree-op child))))
			      (push `((eql holdchar ,(op-tree-op child))
				      ,@(mk-defun-lex-body child new-prefix))
				    stuff)))
			(t (lexical-unread-char stream)
			   ,@(if (op-tree-validp root)
				 (list `',(intern prefix *sbst-package*))
				 (list `(illegal-token-error ,prefix)
				       :illegal-token))))))))
