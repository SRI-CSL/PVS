;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)top-parse.lisp	1.27 10/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Anne Rogers 

;;; Revised, Scott Dietzen, Mon Oct 13 16:05:43 1986


(in-package :syntax-box)   (use-package :ergolisp)


;;; srd revised: moved global varaible definitions to globals.lisp 




;;; Main routine for the parser generator.  Pass it a grammar structure
;;; after parsing the grammar and running it through the intermediate phase.
(defun generate-parser (gram-struct file-spec)
  (setq *parser-gen-error* nil)
  (setq *current-nt* nil)
  (setq *global-function-list* nil)
  (generate-look-ahead gram-struct)
  (handle-brackets gram-struct *dependency-graph*)
  (process-grammar gram-struct)
  (handle-lexer-stuff gram-struct *keyword-graph*)
  (save-parser file-spec gram-struct))
	     



; These routines are for setting up the bracketing information list
; that we need in process-nonterminal.  The result of Handle-brackets
; is a list with entries of the form (NT-name . brackets).  For each 
; nonterminal there is a list of brackets that can be derived from it.
;
;      A ::= B, where A has brackets [ ] and B has brackets { }
;
; the resulting list will be
;	((A ([ ]) ({ })) (B ({ })))
;
; NOTE:  Given a grammar like A -> B ...., B -> A.  A and B 
;   must have the same bracket set; all members of a strongly connected
;   component in the dep. graph must have the same bracket set.

; (defvar *global-brackets* ())  ; in globals.lisp

;(defun get-bracketing-information (nt)  ; in aux-funs.lisp
;  (if (eq (car nt) 'nonterminal)
;      (assoc (cadr nt) *global-brackets*)
;      (assoc (nth 2 nt) (cadr (assoc (cadr nt) *ext-brackets*)))))

(defun handle-brackets (gram-struct b-graph)
           ; reduce each strongly connected component in the dep. graph
           ; to a single node.  
  (let ((reduced-graph (reduce-graph b-graph)))

    (setq *global-brackets* nil)
  
    (mapcar #'(lambda (x)
		(handle-component-brackets (car x) gram-struct reduced-graph))
	    reduced-graph)))


(defun handle-component-brackets (vertex-number gram-struct reduced-graph)
  (let* ((vertex (assoc vertex-number reduced-graph))
	 (components (cadr vertex))
	 (descendents (nth 2 vertex))
	 (temp (get-bracketing-information (car components))))
    ; if nt already has an entry in *global-brackets*, just return
    ; the result.  If one nt in the component has a entry, all the
    ; members of the component will.
    (if (or temp (eq (caar components) 'ext-nonterminal))
	(cadr temp)
	; ow.  Get brackets for all nonterminals in component
	(let ((brackets nil) (tb nil) (nt-info nil))
	  (do ((members components (cdr members)))
	      ((null members))
	    (setq nt-info (get-nt-info (cadar members) gram-struct))
	    (setq tb (if nt-info
			 (get-nt-brackets (nt-name nt-info) gram-struct)
			 nil))
	    (if tb (setq brackets (equal-union brackets (list tb)))))
	  ; The brackets for each descendant are must also be
	  ; handled as brackets by the parent.  Find all descendants
	  ; brackets and add them to the component bracket list.
	  (do ((guys descendents (cdr guys)))
	      ((null guys))
	    (setq tb (handle-component-brackets
		      (car guys) gram-struct reduced-graph))
	    (if tb (setq brackets (equal-union brackets tb))))
	  ; add entry to *global-brackets* for each nt in the 
	  ; component.
	  (do ((members components (cdr members)))
	      ((null members))
	    (push (list (cadar members) brackets) *global-brackets*))
	  brackets))))


; Save Parser Code

(defun save-parser (file-spec gram-struct)
  (let (temp)
    (cond (*parser-gen-error*
	   (write-string "Not writing file...errors") (terpri))
	  (t
	   (setq temp (open file-spec :direction :output
			    :if-exists :new-version))
	   (format t "   writing file ~S.~%" file-spec)
	   (initialize-file temp :parser)
	   (build-parse gram-struct)
	   (sb-write (pprint `(defparameter ,*conc-abs-syn-package*
				(find-package ,*abs-syn-package-spec*))
			     temp))
	   (sb-write (mapcar #'(lambda (x)
				 (terpri temp)
				 (pprint x temp)
				 (format temp "~|~%"))
			     *global-function-list*))
	   (terpri temp)
	   (close temp)))))



;;;      INITIALIZE-FILE outputs any stuff you want at the top of the file.
;;; File-type must be either :parser or :unparser, which causes the obvious
;;; effect.

(defun initialize-file (stream file-type)
  (sb-write
   (format stream ";;; -*- Mode: Lisp; Package: ~A -*-~%~
                  (in-package ~S)  ;; creates package for abstract syntax. ~%~%~
                  (in-package ~S)  ;; enters package for generated code.  ~%~%~
	          (use-package '~S)~3%"
     *code-package-spec*
     *abs-syn-package-spec*
     *code-package-spec*
     *use-packages-spec*)
   (format stream "(export '(")
   (mapc #'(lambda (symbol) (format stream " ~A " symbol))
     (case file-type
       (:unparser
	(append (if (eq (symbol-package
			 (lang:lang-unparse-routine-name *language*))
			(lang:lang-code-package *language*))
		    (list (lang:lang-unparse-routine-name *language*)))
		(if (eq (symbol-package
			 (lang:lang-win-unparse-routine-name *language*))
			(lang:lang-code-package *language*))
		    (list (lang:lang-win-unparse-routine-name *language*)))))
       (:parser
	(append (if (eq (symbol-package
			 (lang:lang-parse-routine-name *language*))
			(lang:lang-code-package *language*))
		    (list (lang:lang-parse-routine-name *language*)))))
       ((:sorts :lexer)
	())))
   (format stream "))~%")))



;;; Write main parsing routine.  
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
	      (*hold-a1* nil) (*hold-a2* nil) (*hold-b1* nil) (*hold-b2* nil)
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

(defun gen-var/const-names (conc-name)
    (setq conc-name (string-upcase conc-name))
    (macrolet ((save (place name-ending name-type)
		   `(setf ,place
			  (intern ,(if (eq name-type :special)
				       `(concatenate 'string
					    "*" conc-name
					    ,name-ending "*")
				       `(concatenate 'string
					    conc-name
					    ,name-ending))
				  *sb-package*))))
	(save *conc-keyword-list* "-KEYWORD-LIST" :constant)
	(save *conc-single-char-op-list* "-SINGLE-CHAR-OP-LIST" :constant)
	(save *conc-multi-char-op-list* "-MULTI-CHAR-OP-LIST" :constant)
	(save *conc-all-operators-list* "-ALL-OPERATORS-LIST" :constant)
	(save *conc-new-line-comment-char* "-NEW-LINE-COMMENT-CHAR"
	      :constant)
	(save *conc-open-comment-char* "-OPEN-COMMENT-CHAR" :constant)
	(save *conc-close-comment-char* "-CLOSE-COMMENT-CHAR" :constant)
	(save *conc-escape-char* "-ESCAPE-CHAR" :constant)
	(save *conc-case-sensitive* "-CASE-SENSITIVE" :constant)
	(save *conc-lexical-readtable* "-LEXICAL-READTABLE" :special)
	(save *conc-keyword-table* "-KEYWORD-TABLE" :special)
	(save *conc-abs-syn-package* "-ABS-SYN-PACKAGE" :constant)
	(save *conc-lexer-init-p*  "-LEXER-INIT-P" :special)
	(save *conc-restricted-chars*  "-RESTRICTED-CHARS" :constant)

	(save *conc-string-char* "-STRING-CHAR" :constant)
	(save *conc-literal-char* "-LITERAL-CHAR" :constant)
	(save *conc-keyword-char* "-KEYWORD-CHAR" :constant)
	))	

; Cleanup after parser generation.

(defun parser-clean-up ()
   (setq *look-ahead-sets* nil)
   (setq *global-function-list* nil)
   (setq *current-nt* nil)
   (setq *parser-gen-error* nil))



;(defvar *ext-look-ahead* nil)   ; srd revised, defined in globals. 
;(defvar *ext-first-sets* nil)
;(defvar *ext-lexer-information* nil)
;(defvar *ext-operators* nil)
;(defvar *ext-used-operators* nil)

; File structure
;   Package name
;   Look ahead sets     (nt-name LAS)*
;   first sets          (nt-name FS)*
;   bracketing information  (nt-name brackets)*
;   lexer information  (nt-name keywords)*

(defun get-external-grammars-info (lang)
  (setf *ext-used-operators* nil)
  (setf *ext-package-names* nil)
  (setf *ext-look-ahead* nil)
  (setf *ext-first-sets* nil)
  (setf *ext-brackets* nil)
  (setf *ext-lexer-information* nil)
  (setf *ext-operators* nil)
  (when (and (lang:lang-sub-languages lang)
	     (not (equal (lang:lang-sub-languages lang)
			 '("LEXICAL-TERMINALS"))))
    (format t "   Fetching external grammar information ...~%")
    (mapc #'(lambda (x)
	      (if (not (string= x "LEXICAL-TERMINALS"))
		  (get-external-grammars-info-aux x)))
	  (lang:lang-sub-languages lang))))

(defun get-external-grammars-info-aux (lang-name)
  (let ((info-file-name (lang:get-lang-info-file lang-name))
	(lang-name (if (stringp lang-name)
		       (intern #+(or allegro-v6.0 allegro-v6.2) (string-downcase lang-name)
			       #-(or allegro-v6.0 allegro-v6.2) (string-upcase lang-name)
			       *sb-package*)
		       lang-name)))
    (with-open-file (s info-file-name :if-does-not-exist nil)
      (if s
	  (let ((*package* *sb-package*))
	    (push (list lang-name (read s)) *ext-package-names*)
	    (push (list lang-name (read s)) *ext-look-ahead*)
	    (push (list lang-name (read s)) *ext-first-sets*)
	    (push (list lang-name (read s)) *ext-brackets*)
	    (push (list lang-name (read s)) *ext-lexer-information*)
	    (push (list lang-name (read s)) *ext-operators*))
	  (my-error 33 lang-name)))))




(defun gen-info-file-name (grammar-prefix working-dir)
  (merge-pathnames (concatenate 'string grammar-prefix "-info.txt")
                   working-dir))


; These routines are for setting up the lexical information for
; the aux-file.  The result of handle-ops is a list with entries
; of the form (NT-name . keywords).  For each 
; nonterminal there is a list of keywords that can be derived from it.
;
;      A ::= B, where A has keywords 'begin and 'end and
;      B has keyword 'foo
;
; the resulting list will be
;	((A (begin end foo)) (B (foo)))
;
; NOTE:  Given a grammar like A -> B ...., B -> A.  A and B 
;   must have the same keyword set; all members of a strongly connected
;   component in the dep. graph must have the same keyword set.

; (defvar *global-lexer-information* nil)


(defun get-lexer-information (nt)
  (if (eq (car nt) 'nonterminal)
      (assoc (cadr nt) *global-lexer-information*)
      (assoc (nth 2 nt) (cadr (assoc (cadr nt) *ext-lexer-information*)))))

(defun handle-lexer-stuff (gram-struct b-graph)
           ; reduce each strongly connected component in the dep. graph
           ; to a single node.  
  (let ((reduced-graph (reduce-graph b-graph)))

    (setq *global-lexer-information* nil)
    (setq *ext-used-operators* nil)
  
    (mapcar #'(lambda (x)
		(handle-component-LI (car x) gram-struct reduced-graph))
	    reduced-graph)))


(defun handle-component-LI (vertex-number gram-struct reduced-graph)
  (let* ((vertex (assoc vertex-number reduced-graph))
	 (components (cadr vertex))
	 (descendents (nth 2 vertex))
	 (temp (get-lexer-information (car components))))

       ; if nt already has an entry in *global-lexer-information*, just return
       ; the result.  (if one nt in the component has a entry, all the
       ; members of the component will.)  If it is an
       ; external nonterminal, it won't have an entry in the graph.
    (cond ((eq (caar components) 'ext-nonterminal)
	   (let ((comp (car components)))
	     (setq *ext-used-operators*
		   (union
		    *ext-used-operators*
		    (intersection (cadr temp)
				  (cadr (assoc (cadr comp) *ext-operators*)))))
	     (cadr temp)))
	  (temp (cadr temp))

	  (t
	       ; ow.  Get lexer-information for all nonterminals in component
	   (let ((ops nil) (t-ops nil) (nt-info nil))

	     (do ((members components (cdr members)))
		 ((null members))

	       (setq nt-info (get-nt-info (cadr (car members)) ; srd revised.
					  gram-struct))
	       (setq t-ops (if nt-info
			       (get-nt-keywords nt-info)
			       nil))	; srd revised
	       (if t-ops (setq ops (union ops t-ops))))


	         ; The operatos for each descendant are must also be
	         ; handled as ops by the parent.  Find all descendants
                 ; operators and add them to the component operator list.

	     (do ((guys descendents (cdr guys)))
		 ((null guys))

	       (setq t-ops (handle-component-LI
			    (car guys) gram-struct reduced-graph))
	       (if t-ops (setq ops (union ops t-ops))))

	         ; add entry to *global-lexer-information* for each nt in the 
                 ; component.
	     (do ((members components (cdr members)))
		 ((null members))

	       (push (list (cadr (car members)) ops) *global-lexer-information*))
	     ops)))))
