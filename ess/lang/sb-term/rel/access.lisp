;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)access.lisp	1.25 11/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.


;;; Access macros for the grammar abstract syntax (output of the
;;;    sb-parser). 
;;; Scott Dietzen, Fri Oct 10 14:48:20 1986
;;;
;;; fp , Mon Jan  2 11:02:12 1989
;;; Took out the memq macro, since it was defined as a function elsewhere.
;;; Now centralized.

;;(in-package "SB" :nicknames '("SYNTAX-BOX"))

(in-package "SYNTAX-BOX" :nicknames '("SB")) (use-package :ergolisp)


(use-package '("OPER" "TERM" "SORT" "SB-RUNTIME" "LANG"))


(defparameter *sb-package* (find-package "SB"))

(defvar *abs-syn-lisp-package*
  "Package of client abstract syntax.")

(defvar *code-lisp-package*
  "Package of client code.")


;;; Global variable set to the grammar structure.  (defined elsewhere)
(defvar *grammar-term* nil)

;;; Global variable whether target language is case-sensitive. (defined
;;; elsewhere) 
(defvar *grammar-case-sensitive?* nil)

(defvar *language*)

(import '(*sbst-package*))




;;; @@ HACK for destructive attributing. 

(defvar *acc-attr-table* (make-hash-table :test #'eq :size (expt 2 15)))

(defun init-sb-attr-table ()
  (clrhash *acc-attr-table*))

(defun get-sb-attr (node name)
  (let ((node-table (gethash node *acc-attr-table*)))
    (if node-table
	(gethash name node-table)
	nil)))

(defun set-sb-attr (node name value)
  (let ((node-table (gethash node *acc-attr-table*)))
    (if (not node-table)
	(setf node-table
	      (setf (gethash node *acc-attr-table*)
		    (make-hash-table :test #'eq :size (expt 2 5)))))
    (setf (gethash name node-table)
	  value)))






(defun sbst-intern-upcase (x)
  (if (and (symbolp x)
	   (eq (symbol-package x) *sbst-package*))
      x
      (let ((str (if (stringp x)
		     x
		     (symbol-name x))))
	(intern (string-upcase str)
		*sbst-package*))))

(defun sbst-intern-case (x)
  (if (and (symbolp x)
	   (eq (symbol-package x) *sbst-package*))
      x
      (let ((str (if (stringp x)
		     x
		     (symbol-name x))))
	(intern (if *grammar-case-sensitive?*
		    str
		    (string-upcase x))
		*sbst-package*))))

(defun sb-intern-upcase (x)
  (if (and (symbolp x)
	   (eq (symbol-package x) *sb-package*))
      x
      (let ((str (if (stringp x)
		     x
		     (symbol-name x))))
	(intern (string-upcase str)
		*sb-package*))))

(defun sb-intern-case (x)
  (if (and (symbolp x)
	   (eq (symbol-package x) *sb-package*))
      x
      (let ((str (if (stringp x)
		     x
		     (symbol-name x))))
	(intern (if *grammar-case-sensitive?*
		    str
		    (string-upcase x))
		*sb-package*))))


(eval-when (compile eval load)

  ;; The following were originally destructive versions of the above, but my
  ;; timing tests show that you don't gain that much and I'm worried about
  ;; introducing strange behavior. 

(defmacro sbst-intern-nupcase (x)
  `(sbst-intern-upcase ,x))
(defmacro sbst-intern-ncase (x)
  `(sbst-intern-case ,x))
(defmacro sb-intern-nupcase (x)
  `(sb-intern-upcase ,x))
(defmacro sb-intern-ncase (x)
  `(sb-intern-case ,x))


(defparameter sb-type-checks t)

(defmacro assert-sb (p)
  (if sb-type-checks
      `(if (not ,p)
	   (sb-system-error))))

  ;;  (defmacro memq (target list)
  ;;  `(member ,target ,list :test #'eq))

)   ;; eval-when




(defun grammar-name (grammar)
  (let ((name (term-arg0 (ck-term-sop 'grammar
			     grammar))))
    (if (is-id name)
	(ds-id name))))


;;; External grammar information access
(defun grammar-external-grammars (grammar)
  (let ((ext-grammars (term-arg1 (ck-term-sop 'grammar
				     grammar))))
    (cond ((get-sb-attr ext-grammars 'ext-grammars))
	  (t
	   (set-sb-attr ext-grammars 'ext-grammars
			(mapcar #'(lambda (x) (ds-id x))
				(if (is-sop 'ext-gram ext-grammars)
				    (term-args ext-grammars)
				    ())))))))


;;; Comment character information access. 

(defun grammar-comment-info (grammar)
  (term-arg2 (ck-term-sop 'grammar
		 grammar)))
(defun grammar-comment-chars (arg grammar)
  (let ((keyword (if (is-sop 'comment-character (grammar-comment-info grammar))
		     (term-argn (grammar-comment-info grammar)
				arg))))
    (if (is-keyword keyword)
	(ds-keyword keyword)
	nil)))


(eval-when (compile eval load)

(defmacro grammar-new-line-comment-char (grammar)
  `(grammar-comment-chars 0 ,grammar))
(defmacro grammar-open-comment-char (grammar)
  `(grammar-comment-chars 1 ,grammar))
(defmacro grammar-close-comment-char (grammar)
  `(grammar-comment-chars 2 ,grammar))

)  ; eval-when



;;; Escape character access. 

(defun grammar-escape-info (grammar)
  (term-argn (ck-term-sop 'grammar
		 grammar)
	     10))
  
(defun grammar-escape-character (grammar)
  (let ((keyword (if (is-sop 'escape-character (grammar-escape-info grammar))
		     (term-arg0 (grammar-escape-info grammar)))))
    (if (is-keyword keyword)
	(ds-keyword keyword)
	nil)))


;;; Case sensitive access. 

(defun grammar-case-info (grammar)
  (term-arg9 (ck-term-sop 'grammar
		 grammar)))

(defun grammar-case-sensitive? (grammar)
  (or (and (is-literal (grammar-case-info grammar))
	   (eq 'case (ds-literal (grammar-case-info grammar))))
      (and (is-id (grammar-case-info grammar))
	   (eq 'case (ds-id (grammar-case-info grammar))))))




;;; Operator information access
(defun grammar-operator-list (grammar)
  "Accesses operator list from grammar structure.  Flatterns to list
   of symbols."
  (let ((op-list (term-arg3 (ck-term-sop 'grammar
				grammar))))
    (cond ((get-sb-attr op-list 'op-list))
	  (t
	   (set-sb-attr op-list 'op-list
			(mapcar #'(lambda (x) (sbst-intern-ncase
                                               (ds-keyword x)))
				(if (is-sop 'op-info op-list)
				    (term-args op-list)
				    ())))))))



;;; Grammar lexical terminal information access.
(defun grammar-lexical-terminals (grammar)
  "Accesses lexical terminal list from grammar structure."
  (let ((lt-list (term-arg4 (ck-term-sop 'grammar
				grammar))))
    (cond ((get-sb-attr lt-list 'lt-list))
	  (t
	   (set-sb-attr lt-list 'lt-list
			(mapcar #'(lambda (x)
				    (if (is-id x)
					(ds-id x)
					(ds-id (term-arg0 x))))  ;; delimited.
				(if (is-sop 'lex-terms lt-list)
				    (term-args lt-list)
				    ())))))))

(defun is-lexical-terminal (name grammar)
  "Checks is NAME is a lexical terminal of GRAMMAR.  Returns name
    with exclamation points around it if true." 
    ;;; The explanation points are used as a reference name in parser
    ;;; generation.  Not the best place to due this, but no big deal. 
  (if (memq name (grammar-lexical-terminals grammar))
      (sbst-intern-ncase
       (concatenate 'string "!" (symbol-name name) "!"))))
					; The above is a name used by the
					; parser, and is just made here for
					; convenience.

(defun get-lt-delimiter (lex-term-id grammar)
  (let ((default (case lex-term-id
		   (string "\"")
		   (keyword "'")
		   (literal "`"))))
    (let* ((lt-list (term-arg4 (ck-term-sop 'grammar
				   grammar)))
	   (lt-list (if (is-sop 'lex-terms lt-list)
			(term-args lt-list)
			())))
      (do ((lts lt-list (cdr lts)))
	  ((or (null lts)
	       (let ((id (if (is-id (car lts))
			     (ds-id (car lts))
			     (ds-id (term-arg0 (car lts))))))
		 (eq id lex-term-id)))
	   (if lts
	       (if (is-sop 'delimiter (car lts))
		   (ds-keyword (term-arg1 (car lts)))
		   default)))))))
		    



;;; Nonterminal definition access

(defun grammar-nonterminal-definitions (grammar)
  "Accesses list of nonterminal information from grammar structure."
  (term-args (ck-term-sop 'nts
		 (term-arg8 (ck-term-sop 'grammar
				grammar)))))

(defun get-nt-info (nt-name grammar)
  "Gets particular nonterminal structure."
  (let* ((nt-list (grammar-nonterminal-definitions grammar))
	 (nt-entry (search-list-for-term-with-subterm
		    nt-list
		    #'(lambda (x)
			(and (is-id x)
			     (eq (ds-id x) nt-name)))
		    0)))
    (if nt-entry
	(ck-term-sop 'nt-def
	    nt-entry))))



(defun nt-name (nt-term)
  (ds-id (term-arg0 (ck-term-sop 'nt-def nt-term))))

(defun nt-name-abs-package (nt-term)
  (intern (symbol-name (nt-name nt-term))
	  (lang:lang-abs-syn-package *language*)))


(defun grammar-nt-names (grammar)
  (mapcar #'nt-name
	  (grammar-nonterminal-definitions grammar)))

(defun nt-pattern (nt-term)
  (term-arg1 (ck-term-sop 'nt-def nt-term)))

(defun get-nt-pattern (nt-name grammar)
  (nt-pattern (get-nt-info nt-name grammar)))

(defun set-nt-pattern (nt-term pat)    ;; side effecting!! @@This must be
				       ;; removed sometime!
  (ck-term-sop 'nt-def nt-term)
  (setf (cadr (term-args nt-term))
	pat))


(eval-when (compile eval load)

(defmacro get-nt-slot-total (nt-term)
  `(get-sb-attr ,nt-term 'slots))
(defmacro set-nt-slot-total (nt-term value)
  `(set-sb-attr ,nt-term 'slots ,value))

)  ; eval-when




(defun get-nt-keywords (nt-term)
  (cond ((get-sb-attr nt-term 'keywords))
	(t
	 (set-sb-attr nt-term
		      'keywords
		      (union 
		       (map-pattern (nt-pattern nt-term)
				    #'(lambda (x)
					(if (and (pattern-p x)
						 (eq (pattern-kind x)
						     'ukeyword))
					    (sbst-intern-ncase
					     (ds-keyword
					      (car (pattern-sons x)))))))
		       (get-nt-brackets (nt-name nt-term)
					*grammar-term*))))))
					
	 


(defun get-nt-lexical-terminals (nt-term)
  (declare (ignore nt-term)))  ;; not currently used. 





  ;;; Bracketing information access

(defun grammar-bracket-information (grammar)
  (let ((brackets (term-arg5 (ck-term-sop 'grammar
			      grammar))))
    (if (is-sop 'bracket-entries brackets)
	(term-args brackets))))


(defun find-bracket (nt-name grammar kind) 
  (let* ((br-list (grammar-bracket-information grammar))
	 (entry (search-list-for-term-with-subterm
		 br-list
		 #'(lambda (x)
		     (and (is-id x)
			  (eq (ds-id x) nt-name)))
		 0)))
    (if (termp entry)
	(sbst-intern-ncase
	 (ds-keyword
	  (term-argn (ck-term-sop 'bracket-entry entry)
		     (ecase kind
		       (left-bracket 1)
		       (right-bracket 2))))))))


(defun get-nt-bracket (nt-name grammar kind) ; This keeps redoing it for the
					     ; null case. 
  (cond ((get-sb-attr (get-nt-info nt-name grammar) kind))
	(t
	 (set-sb-attr (get-nt-info nt-name grammar) kind
		      (find-bracket nt-name grammar kind)))))

(defun get-nt-brackets (nt-name grammar) 
  (if (get-nt-bracket nt-name grammar 'left-bracket)
      (list (get-nt-bracket nt-name grammar 'left-bracket)
	    (get-nt-bracket nt-name grammar 'right-bracket))))

	 
	
(eval-when (compile eval load)
	
(defmacro get-nt-left-bracket (nt-name grammar)
  `(get-nt-bracket ,nt-name ,grammar 'left-bracket))
(defmacro get-nt-right-bracket (nt-name grammar)
  `(get-nt-bracket ,nt-name ,grammar 'right-bracket))

(defmacro get-nt-left-paren (nt-name grammar)
  `(get-nt-bracket ,nt-name ,grammar 'left-bracket))
(defmacro get-nt-right-paren (nt-name grammar)
  `(get-nt-bracket ,nt-name ,grammar 'right-bracket))
  
)  ;; eval-when


;;; Precedence information access

(defun grammar-precedence-information (grammar)
  "Accesses precedence information from grammar structure."
  (let ((prec (term-arg6 (ck-term-sop 'grammar
			      grammar))))
    (if (is-sop 'prec-entries prec)
	(term-args prec))))
  
(defun get-nt-prec (nt-name grammar)
  (cond ((get-sb-attr (get-nt-info nt-name grammar) 'prec))
	(t
	 (set-sb-attr (get-nt-info nt-name grammar) 'prec 
		      (process-nt-prec nt-name grammar)))))
(defun process-nt-prec (nt-name grammar)
  (let* ((pr-list (grammar-precedence-information grammar))
	 (entry (search-list-for-term-with-subterm
		 pr-list
		 #'(lambda (x)
		     (and (is-id x)
			  (eq (ds-id x) nt-name)))
		 0)))
    (if (termp entry)
	(process-nt-precedence
	 (term-argn (ck-term-sop 'prec-entry
			   entry)
		       1)))))





;;; Precedence access (con.)

(eval-when (compile eval load)
(defmacro get-nt-prec-initials (nt-name grammar)
  `(nth 0 (get-nt-prec ,nt-name ,grammar)))
(defmacro get-nt-prec-medial-lefts (nt-name grammar)
  `(nth 1 (get-nt-prec ,nt-name ,grammar)))
(defmacro get-nt-prec-medial-rights (nt-name grammar)
  `(nth 2 (get-nt-prec ,nt-name ,grammar)))
(defmacro get-nt-prec-aggregates (nt-name grammar)
  `(nth 3 (get-nt-prec ,nt-name ,grammar)))
)  ; eval-when


(defun get-nt-op-prec (op nt-name kind grammar)
  (ecase kind
    ((:initial initial)
     (cadr (assoc op (get-nt-prec-initials      nt-name grammar))))
    ((:left :medial-left medial-left left)
     (cadr (assoc op (get-nt-prec-medial-lefts  nt-name grammar))))
    ((:right :medial-right medial-right right)
     (cadr (assoc op (get-nt-prec-medial-rights nt-name grammar))))
    ((:aggregate aggregate)
     (cadr (assoc op (get-nt-prec-aggregates    nt-name grammar))))))

(defun mk-jux-name (postfix)
  (if postfix
      (sb-intern-upcase
       (concatenate 'string
	 "JUX^"
	 (if (is-number postfix)
	     (princ-to-string (ds-number postfix))
	     (symbol-name (ds-id postfix)))))
      'jux))

(defun get-jux-bp (tag left-or-right nt-name grammar)
  (ecase left-or-right
    (left
     (get-nt-op-prec (mk-jux-name tag) nt-name 'medial-left grammar))
    (right
     (get-nt-op-prec (mk-jux-name tag) nt-name 'medial-right grammar))))



;;; Precedence preprocessing (con.)
;;; Returns sexps rather than terms because information is easier to access. 
(defun process-nt-precedence (multiple-level)
  (let (initial-rbp medial-rbp medial-lbp aggregate-lbp)

    (do ((single-levels (term-args multiple-level)
                        (cdr single-levels))
         (count 10 (+ count 10)))
        ((null single-levels))

      (do ((single-level (term-args (car single-levels))
                         (cdr single-level)))
	  ((null single-level))

        (ck-term #'(lambda (x)
		     (or (is-sop 'initial x)
			 (is-sop 'medial x)
			 (is-sop 'aggregate x)))
	    (car single-level))
	(let* ((single-op-prec (car single-level))
               (type (sim-term-op single-op-prec))
               (subtype (if (eq type 'medial)
			    (ds-id
			     (term-arg1 single-op-prec))))
               (operator-term (term-arg0 single-op-prec))
               (operator
                (let ((subterm (term-arg0 operator-term)))
                  (ecase (sim-term-op operator-term)
                    (keyword-op (sbst-intern-case (ds-keyword subterm)))
                    (jux-op
                     (if (is-sop 'null subterm)
                         'jux
			 (mk-jux-name subterm)))))))
	  (ecase type
	    (initial
	     (push (list operator count) initial-rbp))
	    (medial
             (ecase subtype
               (left
                (push (list operator count) medial-lbp)
                (push (list operator (+ 1 count)) medial-rbp))
               (right
                (push (list operator (+ 1 count)) medial-lbp)
                (push (list operator count) medial-rbp))
               (lbind
                (push (list operator count) medial-lbp))
               (rbind
                (push (list operator count) medial-rbp))))
	    (aggregate
	     (push (list operator count) aggregate-lbp))))))
    
    (list initial-rbp medial-lbp medial-rbp aggregate-lbp)))



(defun grammar-spacing (grammar)
  (let ((spacing (term-arg7 (ck-term-sop 'grammar
				 grammar))))
    (if (is-sop 'spacing spacing)
	(term-args spacing))))



;;; Pattern access 

(defparameter pattern-op-list
  '(nonterminal ext-nonterminal ukeyword jux opt alt
    star plus doubleplus doublestar tag
    augment seq 
    format upattern
    just-as gen-star internal-seq))	; last three added by parse-gen

(defparameter composed-pats
  '(tag augment format upattern))

(defparameter pattern-ops (make-hash-table :test #'eq :size (expt 2 8)))

(eval-when (load eval)
  (mapcar #'(lambda (x)
	      (setf (gethash x pattern-ops) t))
	  pattern-op-list))


(defun pattern-p (pat)
  (and (termp pat)
       (gethash (sim-term-op pat) pattern-ops)))

(defun is-composed-pat? (pat)
  (and (termp pat)
       (member (sim-term-op pat) composed-pats :test #'eq)))


(defun pattern-leaf-p (pat)
  (and (pattern-p pat)
       (or (memq-sop pat '(nonterminal ext-nonterminal ukeyword))
           (and (is-composed-pat? pat)
                (pattern-leaf-p (term-arg0 pat)))
	   (and (is-sop 'seq pat)
		(term-arity1? pat)
		(pattern-leaf-p (term-arg0 pat))))))

(defun pattern-kind (pat)
  (assert-sb (pattern-p pat))
  (cond ((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-kind (term-arg0 pat)))
	(t
	 (sim-term-op pat))))

(defun pattern-name (pat)
  (assert-sb (pattern-p pat))
  (cond ((is-sop 'tag pat)
	 (let ((value (term-arg1 pat)))
	   (cond ((is-number value)
		  (ds-number value))
		 ((is-id value)
		  (ds-id value)))))
	((or (is-composed-pat? pat)	; not eq to tag
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-name (term-arg0 pat)))
	(t
	 ())))

(defun pattern-format (pat)
  (assert-sb (pattern-p pat))
  (cond ((is-sop 'format pat)
	 (term-arg1 pat))
	((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-format (term-arg0 pat)))
	(t
	 ())))

(defun pattern-augment (pat)
  (assert-sb (pattern-p pat))
  (cond ((is-sop 'augment pat)
	 (term-arg1 pat))
	((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-augment (term-arg0 pat)))
	(t
	 ())))

(defun pattern-upats (pat)
  (assert-sb (pattern-p pat))
  (cond ((is-sop 'upattern pat)
	 (term-args (ck-term-sop 'upats (term-arg1 pat))))
	((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-upats (term-arg0 pat)))
	(t
	 ())))


(defun upat-uids (upat)
  (assert-sb (is-sop 'upat upat))
  (mapcar #'(lambda (sym)
	      (intern (symbol-name sym) (find-package 'keyword)))
	  (mapcar #'ds-id 
		  (term-args (ck-term-sop 'uids (term-arg0 upat))))))

(defun upat-pattern (upat)
  (assert-sb (is-sop 'upat upat))
  (term-arg1 upat))

(defun map-upat-patterns (function upats &key (result nil))
  (if (null result)
      (mapc #'(lambda (upat)
		(funcall function (upat-pattern upat)))
	    upats)
      (mapcar #'(lambda (upat)
		  (funcall function (upat-pattern upat)))
	      upats)))	



(defun pattern-sons (pat)
  (assert-sb (pattern-p pat))
  (cond ((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-sons (term-arg0 pat)))
	((is-sop 'jux pat)
	 (list (term-arg0 pat)
	       (term-arg1 pat)))
					; pattern-son2 is whitespace info.
	(t
	 (term-args pat))))


(defun pattern-jux-ws (pat)
  (assert-sb (and (pattern-p pat)
		  (eq (pattern-kind pat) 'jux)))
  (cond ((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-jux-ws (term-arg0 pat)))
	(t
	 (term-arg2 pat))))


(defun pattern-leaf-ds (pat)
  (assert-sb (pattern-p pat))
  (cond ((or (is-composed-pat? pat)
	     (and (is-sop 'seq pat)
		  (term-arity1? pat)))
	 (pattern-leaf-ds (term-arg0 pat)))
	(t
	 (ecase (pattern-kind pat)
	   (nonterminal
	    (ds-id (term-arg0 pat)))
	   (ukeyword
	    (sbst-intern-ncase (ds-keyword (term-arg0 pat))))
	   (ext-nonterminal
	    (values (ds-id (term-arg0 pat))
		    (ds-id (term-arg1 pat))))))))


(eval-when (compile eval load)

(defmacro pattern-tag (pat)    
  `(pattern-name ,pat))
(defmacro pattern-args (pat)
  `(pattern-sons ,pat))


(defmacro pattern-son0 (pat)
  `(nth 0 (pattern-sons ,pat)))
(defmacro pattern-son1 (pat)
  `(nth 1 (pattern-sons ,pat)))
(defmacro pattern-son2 (pat)
  `(nth 2 (pattern-sons ,pat)))
(defmacro pattern-son3 (pat)
  `(nth 3 (pattern-sons ,pat)))
(defmacro pattern-son4 (pat)
  `(nth 4 (pattern-sons ,pat)))
(defmacro pattern-son5 (pat)
  `(nth 5 (pattern-sons ,pat)))
(defmacro pattern-son6 (pat)
  `(nth 6 (pattern-sons ,pat)))
(defmacro pattern-son7 (pat)
  `(nth 7 (pattern-sons ,pat)))
(defmacro pattern-son8 (pat)
  `(nth 8 (pattern-sons ,pat)))
(defmacro pattern-son9 (pat)
  `(nth 9 (pattern-sons ,pat)))


  ;; Attributes
(defmacro get-pattern-slot (pat)
  `(get-sb-attr ,pat 'slot))
(defmacro set-pattern-slot (pat value)
  `(set-sb-attr ,pat 'slot ,value))

(defmacro get-pattern-referenced (pat)
  `(get-sb-attr ,pat 'referenced))
(defmacro set-pattern-referenced (pat value)
  `(set-sb-attr ,pat 'referenced ,value))

(defmacro get-pattern-bound (pat)
  `(get-sb-attr ,pat 'bound))
(defmacro set-pattern-bound (pat value)
  `(set-sb-attr ,pat 'bound ,value))

(defmacro get-pattern-binding-aug (pat)
  `(get-sb-attr ,pat 'binding-aug))
(defmacro set-pattern-binding-aug (pat value)
  `(set-sb-attr ,pat 'binding-aug ,value))

(defmacro get-pattern-initial (pat)
  `(get-sb-attr ,pat 'initial))
(defmacro set-pattern-initial (pat value)
  `(set-sb-attr ,pat 'initial ,value))
(defmacro pattern-initial (pat)    
  `(get-pattern-initial ,pat))

(defmacro get-pattern-medial (pat)
  `(get-sb-attr ,pat 'medial))
(defmacro set-pattern-medial (pat value)
  `(set-sb-attr ,pat 'medial ,value)) 
(defmacro pattern-medial (pat)    
  `(get-pattern-medial ,pat))

(defmacro get-pattern-augment (pat)
  `(cond ((get-sb-attr ,pat 'augment))
	 (t
	  (pattern-augment ,pat))))
(defmacro set-pattern-augment (pat value)
  `(set-sb-attr ,pat 'augment ,value))

(defmacro get-pattern-ttype (pat)
  `(get-sb-attr ,pat 'ttype))
(defmacro set-pattern-ttype (pat value)
  `(set-sb-attr ,pat 'ttype ,value))

(defmacro get-pattern-is-discriminated (pat)
  `(get-sb-attr ,pat 'is-discriminated))
(defmacro set-pattern-is-discriminated (pat value)
  `(set-sb-attr ,pat 'is-discriminated ,value))

(defmacro get-pattern-expand-seq-rep (pat)
  `(get-sb-attr ,pat 'expand-seq-rep))
(defmacro set-pattern-expand-seq-rep (pat value)
  `(set-sb-attr ,pat 'expand-seq-rep ,value))

) ; eval-when



(defun is-pat-iterator (pat)
  (and (pattern-p pat)
       (memq (pattern-kind pat)
	     '(plus star doubleplus doublestar))))


(defun make-pattern (&key kind (name ()) (sons ()) (slot ())
			  (referenced ()) (bound ()) (initial ())
			  (medial ()) (augment ()) (format ()) (upats ()))
  (let* ((pat (mk-sim-term kind sons))
	 (pat (if name
		  (mk-sim-term 'tag
			       (list pat (if (numberp name)
					     (mk-number name)
					     (mk-id name))))
		  pat))
	 (pat (if format
		  (mk-sim-term 'format
			       (list pat format))
		  pat))
	 (pat (if upats
		  (mk-sim-term 'upattern
			       (list pat upats))
		  pat)))
    (set-pattern-slot pat slot)
    (set-pattern-referenced pat referenced)
    (set-pattern-bound pat bound)
    (set-pattern-initial pat initial)
    (set-pattern-medial pat medial)
    (set-pattern-augment pat augment)
    (assert-sb (pattern-p pat))
    pat))


(eval-when (compile eval load)
  ;; There is a compiler error for mapcan.
  (defun map-pattern (pat funct)
    (if (pattern-p pat)
	(union (if (funcall funct pat)
		   (list (funcall funct pat))
		   nil)
	       (if (not (pattern-leaf-p pat))
		   (mapcan #'(lambda (p)
			       (map-pattern p funct))
			   (pattern-sons pat)))))))



;;; $upattern revision

(defun set-augment-of-upats (pat)
  (assert-sb (pattern-p pat))
  (do ((upats (pattern-upats pat)
	      (cdr upats)))
      ((null upats))
    (if (pattern-augment pat)
	(let* ((new-aug (copy-augment (pattern-augment pat)))
	       (new-pat (term:mk-sim-term 'augment
					  (list (upat-pattern (car upats))
						new-aug))))
	  (assert-sb (and (augment-p new-aug)
			  (pattern-p new-pat)))
	     ;; Side-effecting is the easiest thing to do here.  Perhaps
	     ;; consider how to handle this wihtout side-effecting some other
	     ;; time.  This won't mess up other constructions as unparsers are
	     ;; the only ones to look at upatterns.
	  (setf (nth 1 (term-args (car upats)))	; i.e., (upat-pattern
					; (car upats))
		new-pat)))))





;;;  Augment-pattern access 

(defparameter augment-op-list
  '(string-aug number-aug literal-aug id-aug
    name ext-name null list cons bcons append 
    term-const star-aug plus-aug
    alt-aug opt-aug tag-aug
    bind and internal-bind		; added by inter-phase
    piece-of-loop known-branch))	; added by parse-gen

(defparameter augment-ops (make-hash-table :test #'eq :size (expt 2 8)))

(eval-when (load eval)
  (mapcar #'(lambda (x)
	      (setf (gethash x augment-ops) t))
	  augment-op-list))



(defun aug-convert (op)
  (case op
    (tag-aug  'tag)
    (alt-aug  'alt)
    (opt-aug  'opt)
    (star-aug 'star)
    (plus-aug 'plus)
    (string-aug 'string)
    (number-aug 'number)
    (literal-aug 'literal)
    (id-aug 'id)
    (t op)))

(defun aug-convert-back (op)
  (case op
    (tag  'tag-aug)
    (alt  'alt-aug)
    (opt  'opt-aug)
    (star 'star-aug)
    (plus 'plus-aug)
    (string 'string-aug)
    (number 'number-aug)
    (literal 'literal-aug)
    (id 'id-aug)
    (t op)))


(defun augment-p (aug)
  (and (termp aug)
       (gethash (sim-term-op aug) augment-ops)))

(defun augment-leaf-p (aug)
  (and (termp aug)
       (or (memq-sop aug '(name ext-name null 
				id-aug literal-aug string-aug number-aug))
	   (and (is-sop 'tag-aug aug)
		(augment-leaf-p (term-arg0 aug))))))

(defun augment-base-p (aug)
  (and (termp aug)
       (or (memq-sop aug '(id-aug literal-aug string-aug number-aug
				  null))
	   (and (is-sop 'tag-aug aug)
		(augment-base-p (term-arg0 aug))))))


(defun augment-kind (aug)
  (assert-sb (augment-p aug))
  (cond ((is-sop 'tag-aug aug)
	 (augment-kind (term-arg0 aug)))
	(t
	 (aug-convert (sim-term-op aug)))))

(defun augment-name (aug)
  (assert-sb (augment-p aug))
  (cond ((is-sop 'tag-aug aug)
	 (leaf-term-value (term-arg1 aug)))
	(t
	 ())))

(defun augment-sons (aug)
  (assert-sb (augment-p aug))
  (cond ((or (is-sop 'tag-aug aug)
	     (eq (augment-kind aug) 'opt)) ; son is an alt which we are not
					   ; interest in.
	 (augment-sons (term-arg0 aug)))
	(t
	 (term-args aug))))



(eval-when (compile eval load)

(defmacro augment-args (aug)
  `(augment-sons ,aug))

(defmacro augment-arg0 (aug)
  `(nth 0 (augment-args ,aug)))
(defmacro augment-arg1 (aug)
  `(nth 1 (augment-args ,aug)))
(defmacro augment-arg2 (aug)
  `(nth 2 (augment-args ,aug)))
(defmacro augment-arg3 (aug)
  `(nth 3 (augment-args ,aug)))
(defmacro augment-arg4 (aug)
  `(nth 4 (augment-args ,aug)))
(defmacro augment-arg5 (aug)
  `(nth 5 (augment-args ,aug)))
(defmacro augment-arg6 (aug)
  `(nth 6 (augment-args ,aug)))
(defmacro augment-arg7 (aug)
  `(nth 7 (augment-args ,aug)))
(defmacro augment-arg8 (aug)
  `(nth 8 (augment-args ,aug)))
(defmacro augment-arg9 (aug)
  `(nth 9 (augment-args ,aug)))




  ;; Attributes
(defmacro get-augment-key-slot (aug)
  `(get-sb-attr ,aug 'key-slot))
(defmacro set-augment-key-slot (aug value)
  `(set-sb-attr ,aug 'key-slot ,value)) 
(defmacro augment-key-slot (aug)    
  `(get-augment-key-slot ,aug))

(defmacro get-augment-result-slot (aug)
  `(get-sb-attr ,aug 'result-slot))
(defmacro set-augment-result-slot (aug value)
  `(set-sb-attr ,aug 'result-slot ,value)) 
(defmacro augment-result-slot (aug)    
  `(get-augment-result-slot ,aug))

(defmacro get-augment-path (aug)
  `(get-sb-attr ,aug 'path))
(defmacro set-augment-path (aug value)
  `(set-sb-attr ,aug 'path ,value)) 
(defmacro augment-path (aug)    
  `(get-augment-path ,aug))

(defmacro get-augment-ttype (aug)
  `(get-sb-attr ,aug 'ttype))
(defmacro set-augment-ttype (aug value)
  `(set-sb-attr ,aug 'ttype ,value))

  )  ;; eval-when



(defun is-aug-iterator (aug)
  (and (augment-p aug)
       (memq (augment-kind aug)
	     '(star plus))))


(defun make-augment (&key kind (leaf-value ())  (name ()) (args ())
			  (key-slot ()) (result-slot ()) (path ()))
  (let* ((args (case kind
		 (opt
		  (list (make-augment :kind 'alt
				      :args args)))
		 (string
		  (list (mk-string (car leaf-value))))
		 (number
		  (list (mk-number (car leaf-value))))
		 (literal
		  (list (mk-literal (car leaf-value))))
		 (id
		  (list (mk-id (car leaf-value))))
		 (t
		  args)))
	 (kind (aug-convert-back kind))
	 (aug (case kind 
		(name
		 (mk-sim-term 'name 
			      (list (mk-id (car leaf-value)))))
		(ext-name
		 (mk-sim-term 'ext-name 
			      (list (mk-id (car leaf-value))
				    (mk-id (cadr leaf-value)))))
		(null
		 (mk-sim-term 'null ()))
		(t
		 (mk-sim-term kind args))))
	 (aug (if name
		  (mk-sim-term 'tag-aug (list aug
					      (if (numberp name)
						  (mk-number name)
						  (mk-id name))))
		  aug)))
    (set-augment-key-slot aug key-slot)
    (set-augment-result-slot aug result-slot)
    (set-augment-path aug path)
    (assert-sb (augment-p aug))
    aug))


(defun copy-augment (aug)
  (assert-sb (augment-p aug))
  (make-augment :kind (augment-kind aug)
		:name (augment-name aug)
		:leaf-value (if (augment-leaf-p aug)
				(multiple-value-list
				 (augment-leaf-ds aug)))
		:key-slot ()		; ignored !
		:result-slot ()		; ignored !
		:path ()		; ignored !
		:args (if (not (augment-leaf-p aug))
			  (mapcar #'copy-augment
				  (augment-args aug)))))

       

(defun replace-augment-son (aug repl-aug index)	; @ Side effecting
  (assert-sb (augment-p aug))
  (cond ((is-sop 'tag-aug aug)
	 (replace-augment-son (term-arg0 aug) repl-aug index))
	(t
	 (case (augment-kind aug)
	   (opt
	    (assert-sb (eq (augment-kind (term-arg0 aug)) 'alt))
	    (replace-augment-son (term-arg0 aug) repl-aug index))
					; son is actually an alt which we don't
					; really care about. 
	   (t
	    (setf (nth index (term-args aug))
		  repl-aug))))))



(defun augment-leaf-ds (aug)
  (assert-sb (augment-p aug))
  (cond ((is-sop 'tag-aug aug)
	 (augment-leaf-ds (term-arg0 aug)))
	(t
	 (ecase (augment-kind aug)
	   ((name)
	    (leaf-term-value (term-arg0 aug)))
	   (ext-name
	    (values (ds-id (term-arg0 aug))
		    (ds-id (term-arg1 aug))))
	   (null
	    '(nil))
	   (id
	    (intern (symbol-name (ds-id (term-arg0 aug)))
		    *abs-syn-lisp-package*))
	   (string
	    (ds-string (term-arg0 aug)))
	   (number
	    (ds-number (term-arg0 aug)))
	   (literal
	    (intern (symbol-name (ds-literal (term-arg0 aug)))
		    *abs-syn-lisp-package*))))))


(defun augment-term-const-op (aug)
  (assert-sb (and (augment-p aug)
		  (eq 'term-const (augment-kind aug))))
  (augment-leaf-ds (augment-arg0 aug)))



;;; Discriminator/Destructor access. 

;;; @@@ NEEDS WORK

(defun get-discriminator (aug grammar)
  (declare (ignore grammar))
  `(function
    (lambda (as)
      (and (termp as)
	   (is-sop ',(let ()
		       (ck-term-sop 'term-const aug)
		       (cond ((is-id (term-arg0 aug))
			      (ds-id (term-arg0 aug)))
			     ((is-literal (term-arg0 aug))
			      (ds-literal (term-arg0 aug)))
			     (t
			      (sb-system-error))))
		   as)))))


(defun get-destructor (aug grammar)
  (declare (ignore grammar))
  (case (augment-kind aug)
    (t
     `(function
       (lambda (as)
	 (and (termp as)
	      (term-args as)))))))	; do we want multiple values here? 





(defun sb-system-error ()
  (format t "Internal error in SB ... see maintainer. ~%")
  (format t "  Integrity of grammar abstract syntax may have been violated. ~%")
  (break))


(defun sb-access-error (flag item)
  (format t "Access error in SB grammar structure: ~%")
  (ecase flag
    (comment-key-too-long
     (format t " Comment delimiter [ ~A ] may only be one character long. ~%"
	     item))))





