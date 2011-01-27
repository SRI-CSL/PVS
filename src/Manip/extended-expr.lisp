;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; extended-expr.lisp -- Extended expression functions for Manip package
;; Author          : Ben Di Vito <b.divito@nasa.gov>
;; Created On      : 19 Sep 2005 (excerpted from manip-strategies.lisp)
;; Last Modified By: 
;; Last Modified On: 22 Nov 2005 (v1.2-beta)
;; Last Modified On: 17 Nov 2007 (v1.2)
;; Status          : Stable
;; Version         : 1.2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :pvs)

;;; =============== Extended expression evaluation ===============

;;; The following functions implement a unified method of accessing
;;; PVS expressions using both textual pattern matching and indexed
;;; term referencing.

(defconstant syntax-match-symb   '~  )
(defconstant-if-unbound loc-ref-flat-symb   '!  )
(defconstant-if-unbound loc-ref-full-symb   '!! )
(defconstant pat-match-symb      '?  )
(defconstant deep-wild-all       '** )
(defconstant deep-wild-term      '-* )   ;; '<* ?
(defconstant deep-wild-nonterm   '*- )   ;; '*< ?
(defconstant goto-index-symb     '-> )
(defconstant goto-all-symb       '->* )
(defconstant all-but-symb        '^  )   ;; '*- ?
(defconstant all-but-symb-all    '0^ )
(defconstant all-but-ante        '-^ )   ;; '-- ?
(defconstant all-but-cons        '+^ )   ;; '+- ?
(defconstant rich-pat-char       #\@ )
(defconstant subst-symb-char     #\$ )

(defconstant-if-unbound loc-ref-symbols (list loc-ref-flat-symb loc-ref-full-symb))
(defconstant-if-unbound ext-expr-symbols
  (apply #'list syntax-match-symb pat-match-symb loc-ref-symbols))
(defconstant-if-unbound deep-wild-symbols
  (list deep-wild-all deep-wild-term deep-wild-nonterm))
(defconstant-if-unbound goto-index-symbols (list goto-index-symb goto-all-symb))
(defconstant-if-unbound all-but-symbols (list all-but-symb all-but-ante all-but-cons))
(defconstant-if-unbound all-but-symbols-loc (list all-but-symb all-but-symb-all))
(defconstant-if-unbound all-but-dict
  (list (cons all-but-symb '*) (cons all-but-ante '-) (cons all-but-cons '+)))


;;; ===============

;;; The structure for extended expression descriptors saves the
;;; expression's string representation, its formula number, and its
;;; PVS CLOS object.  Only the string is guaranteed to exist.  There
;;; is a kind of lazy evaluation implemented, however, wherein the
;;; string is not initially stored if the PVS object exists.  It
;;; will be generated (but not stored) on first access.  Use the
;;; function virt-ee-string rather than the slot accessor ee-string.

(defstruct (ee-descriptor (:conc-name ee-))
  (string nil) (fnum nil) (pvs-obj nil))

(defun virt-ee-string (descriptor)
  (if (ee-descriptor-p descriptor)
      (or (ee-string descriptor) (textify (ee-pvs-obj descriptor)))
      ""))

(defun ee-obj-or-string (descriptor)
  (if (ee-descriptor-p descriptor)
      (or (ee-pvs-obj descriptor) (ee-string descriptor))
      ""))

;;; Compute expression(s) specified by argument.  Return a list of
;;; expression descriptors, each of which is a structure of the form:
;;; #S(<expr string> <fnum> <CLOS object>).  Some descriptors have
;;; only a string component, allowing the others to be nil.

(defun eval-ext-expr (expr)
;  (cond ((stringp expr) (list (make-ee-descriptor :string expr)))
  (cond ((stringp expr)
	 (if (equal (elt expr 0) #\#)
	     (mapcan #'fnum->descriptor-list (map-fnums-arg (subseq expr 1)))
	     (list (make-ee-descriptor :string expr))))
	((numberp expr) (fnum->descriptor-list expr))
	((symbolp expr) (mapcan #'fnum->descriptor-list (map-fnums-arg expr)))
	((ee-descriptor-p expr) (list expr))
	((consp expr)
	 (cond ((eq (car expr) syntax-match-symb)
		(apply #'match-syntax-ee (cdr expr)))
	       ((eq (car expr) pat-match-symb)
		(let ((subexpr (eval-ext-expr (cadr expr))))
		  (mapcan #'(lambda (e) (match-one-expr e (cddr expr)))
			  subexpr)))
	       ((member (car expr) loc-ref-symbols)
		(let ((subexpr (eval-ext-expr (cadr expr)))
		      (flat (eq (car expr) loc-ref-flat-symb)))
		  (mapappend #'(lambda (e) (ref-one-expr e (cddr expr) flat))
			     subexpr)))
	       ((every #'ee-descriptor-p expr) expr)
	       ((member (car expr) all-but-symbols)
		(mapcan #'fnum->descriptor-list (map-fnums-arg expr)))
	       (t (mapappend #'eval-ext-expr expr))))
	(t nil)))

;;; Convert a formula number to a list of expression descriptors by
;;; constructing the string form of the formula.  Returns either a list
;;; of length 1 for a valid number or nil.

(defun fnum->descriptor-list (fnum)
  (let ((obj (manip-get-formula fnum)))
    (if obj
	(list (make-ee-descriptor ;; :string (textify obj)
	                          :fnum fnum :pvs-obj obj))
        nil)))

;;; Apply pattern matching and construct a list of expression descriptors
;;; for all captured strings resulting from the match.

(defun match-one-expr (expr patterns)
  (let* ((regexp-pat (percent-to-regexp-pattern patterns)) ;; compile pattern?
	 (fnum     (if (ee-descriptor-p expr) (ee-fnum expr) nil))
	 (match (match-expr regexp-pat expr)))
    (if (ee-descriptor-p match)
	(progn (setf (ee-fnum match) fnum) (list match))
        (and (car match)
	     (mapcar #'(lambda (m) (make-ee-descriptor :string m :fnum fnum))
		     (or (cddr match) '("")) )))))


;;; --------------- Locations references ----------------

;;; Recursively descend the expression tree rooted at input descriptor
;;; to arrive at the subexpression(s) specified by the list of indexes.
;;; Returns a list of descriptors.  If no object found during tree
;;; traversal, return nil.  Set FLAT to nil to suppress use of flattening
;;; index scheme.

(defun ref-one-expr (descriptor indexes flat)
  (let ((top-obj (ee-pvs-obj descriptor))
	(fnum    (ee-fnum descriptor)))
    (labels
      ((make-descriptor (obj)
	 (make-ee-descriptor ;; :string (textify obj)
			     :fnum fnum :pvs-obj obj))
       (ref-next-level (obj indices)
         (cond
	  ((null obj) nil)
	  ((null indices) (list (make-descriptor obj)))
	  (t (let* ((index (car indices))
		    (rest  (cdr indices))
		    (children (next-lower-objects obj flat))
		    (fn+children (append (and (typep obj 'application)
					      (list (operator obj)))
					 children)))
	       (cond
		((member index '(* 0*))
		 (mapappend #'(lambda (child) (ref-next-level child rest))
			    (if (eq index '*) children fn+children)))

		((or (stringp index)
		     (and (consp index) (every #'stringp index)))
		 (and (match-function-symbol obj index)
		      (ref-next-level obj rest)))

		((and (consp index) (member (car index) goto-index-symbols))
		 (cond ((null (cdr index)) (ref-next-level obj rest))
		       ((match-function-symbol obj (cadr index))
			;; when fn matches, remove from goto list and
			;; proceed to check all child nodes
			(if (cddr index)
			    (let ((other-indices
				   (cons (cons (car index) (cddr index))
					 rest)))
			      (if (eq (car index) goto-index-symb)
				  ;; -> : find first case
				  (loop for child in fn+children
			                when (ref-next-level child
							     other-indices)
					  return it)
				  ;; ->* : find all cases
				  (loop for child in fn+children
				        append (ref-next-level
						child other-indices))))
			    (ref-next-level obj rest)))
		       ((eq (car index) goto-index-symb)
			;; no match for ->, try fn at next level down
			(loop for child in fn+children
			      when (ref-next-level child indices)
			        return it))
		       (t ;; no match for ->* (goto all)
			(loop for child in fn+children
			      append (ref-next-level child indices)))))
;		       (t (mapappend       ;; no match for ->* (goto all)
;			    #'(lambda (child) (ref-next-level child indices))
;			    fn+children))))

		((member index deep-wild-symbols)
		 (let ((lower (mapappend #'(lambda (child)
					     (ref-next-level child indices))
					 fn+children))
		       (contin (ref-next-level obj rest)))
		   (cond
		     ((eq index deep-wild-term) (if fn+children lower contin))
		     ((eq index deep-wild-nonterm)
		      (and fn+children (append contin lower)))
		     ((eq index deep-wild-all) (append contin lower)))))

		((and (consp index) (member (car index) all-but-symbols-loc))
		 (let* ((base (if (eq (car index) all-but-symb) 1 0))
			(but-children (if (eq (car index) all-but-symb)
					  children
				          fn+children))
			(excl (mapcar #'(lambda (n)
					  (symbolic-index n but-children base))
				      (cdr index))))
		   (mapappend #'(lambda (child) (ref-next-level child rest))
			      (mapcar #'(lambda (n) (nth n but-children))
				      (bag-difference
				        (consec (length but-children))
				        excl)))))

		((and (consp index)
		      (every #'(lambda (x) (or (numberp x) (symbolp x)))
			     index))
		 (mapappend #'(lambda (child) (ref-next-level child rest))
			    (mapcar #'(lambda (n)
					(symbolic-nth n fn+children 0))
				    index)))

		((and (consp index) (member (car index) ext-expr-symbols))
		 ;; eval embedded EE on current object as path guard
		 (and (eval-ext-expr (cons (car index)
					   (cons (make-ee-descriptor
						   :pvs-obj obj)
						 (cdr index))))
		      (ref-next-level obj rest)))

		(t (ref-next-level (symbolic-nth index fn+children 0)
				   rest))))))))
      (ref-next-level top-obj indexes))))

;;; Try to match the function/operator symbol of an expression against
;;; a string or list of patterns.  For a string use simple equality.
;;; For a list do pattern matching.

(defun match-function-symbol (obj patterns)
  (handler-case
    (let ((op (textify (operator obj))))
      (if (consp patterns)
	  (match-one-expr op patterns)
	  (equal op patterns)))
    (error (condition) nil)))

;;; Translate symbolic, negative and 1-based indexes into proper numeric
;;; indexes, then apply nth to select an object.  BASE should 0 or 1 to
;;; indicate index base.

(defun symbolic-nth (index objects base)
  (let ((num-index (symbolic-index index objects base)))
    (and num-index (nth num-index objects))))

(defun symbolic-index (index objects base)
  (cond ((eq index 'l) (- 1 base))
	((eq index 'r) (- 2 base))
	((numberp index)
	 (let ((shifted-index
		(if (minusp index) (+ index (length objects)) (- index base))))
	   (and (>= shifted-index 0) shifted-index)))
	(t nil)))

;;; Descend to next lower objects in expression tree.  For certain
;;; associative and commutative operators, observe some tree
;;; "flattening" and allow index to select nth term from those
;;; at the same "level".  Feature is disabled by setting FLAT to nil.
;;; Returns a list of objects.

(defun next-lower-objects (obj flattening)
  (and obj
       (handler-case
	   (if (and flattening
		    (typep obj 'infix-application)
		    (member (id (operator obj)) '(+ - *)))
	       (if (eq (id (operator obj)) '*)
		   ;; include all factors regardless of type:
		   (collect-multiplicative-terms obj nil #'(lambda (a b) t))
		   (mapcar #'cadr (collect-additive-terms '+ obj)))
	       (let ((arg (argument obj)))
		 (cond ((typep arg 'arg-tuple-expr) (exprs arg))
		       ((listp arg) arg)
		       (t (list arg)))))
	 (error (condition) nil))))


;;; ------------------ Pattern matching ------------------

;; Map a pattern involving %-variables (capturing text fields) into a
;; regular expression suitable for matching and collecting substrings.
;; Return a list of regular expression pattern strings.

(defun percent-to-regexp-pattern (pattern)
  (let ((pattern-list (if (listp pattern) pattern (list pattern))))
    (mapcar #'(lambda (pat)
;		(replace-substrings "\\b*" " "  ;; arbitrary whitespace on " "
		(replace-substrings "\\s*" " "  ;; arbitrary whitespace on " "
				    (map-percent-fields pat)))
	    pattern-list)))

;; Escape regular expression meta-characters by prefixing them with "\\".

(defun escape-regexp-metachars (pattern)
  (map-string #'(lambda (c)
		  (if (member c *metachars-regexp*) (list #\\ c) (list c)))
	      pattern))

;; Regular expression metachars:
(defparameter *metachars-regexp*
  (map 'list #'(lambda (c) c) "][)(}{.|*+?$\\^"))
;  (map 'list #'(lambda (c) c) "][.*+$\\^"))

;; Map text field designators from pattern language into regular expression
;; syntax.  A pattern may be "simple", having only one field type (arbitrary
;; text) and one matching type (partial), or "rich", having multiple field
;; types and matching types.  A rich pattern begins with the character `@'
;; followed by the match type.  Simple patterns have neither.
;; Each rich field is 3 characters long having the format:
;; `% <digit> <field type>'.  If the digit is 0, it's a noncapturing field.
;; If it's the first occurrence of digit d, create a capturing field for it.
;; Otherwise, it's a reference to a previously captured field that it must
;; match.  Digits used must be consecutive (e.g., can't have "%1* %3*").
;; Simple fields are 2 characters long, `% <digit>'.

(defun map-percent-fields (pattern)
  (do* ((next-index 1) (posn t) (fragments '()) (edge 0)
	(len (length pattern))
	(is-rich (and (> len 0) (eql (char pattern 0) #\@))))
      ((not posn)
       (apply #'concatenate
	      'string
	      (reverse (cons (escape-regexp-metachars (subseq pattern edge))
			     fragments))))
    (setf posn (position #\% pattern :start edge))
    (when posn
      (push (escape-regexp-metachars (subseq pattern edge posn)) fragments)
      (let ((index-char (if (< posn (- len 1)) (char pattern (1+ posn)) #\_)))
	(push (cond ((eql index-char #\0)
		     (if is-rich
			 (multiple-value-bind (desig-len field-pattern)
			     (match-field-pattern pattern (+ posn 2) nil)
			   (setf edge (min len (+ posn desig-len)))
			   field-pattern)
		         (progn (setf edge (min len (+ posn 2)))
				".*")))
		    ((digit-char-p index-char)
		     (let ((index (parse-integer (string index-char))))
		       (cond ((< index next-index)
			      (setf edge (+ posn 2))
			      (format nil "\\~A" index))
			     (is-rich
			      (multiple-value-bind (desig-len field-pattern)
				  (match-field-pattern pattern (+ posn 2) t)
				(incf next-index)
				(setf edge (min len (+ posn desig-len)))
				field-pattern))
			     (t (incf next-index)
				(setf edge (min len (+ posn 2)))
				"(.*)"))))
;				"\\(.*\\)"))))
		    (t (setf edge (1+ posn)) "%"))
	      fragments)))))

;; Build sub-patterns for rich pattern string fields.
;; Field type characters include:
;;   * -- zero or more arbitrary characters
;;   + -- one or more arbitrary characters
;;   & -- one or more arbitrary characters, where first & last are
;;        non-whitespace characters; (doesn't handle length 1 case yet)
;;   i -- PVS identifier (allows ! for prover variables)
;;   # -- numeric field (digits only)
;;   s -- special symbols (not implemented yet)
;;   none of the above -- same as *
;; If a capturing field is requested, wrap it in parentheses.

(defun match-field-pattern (pattern posn capture)
  (let* ((field-type (if (< posn (length pattern)) (char pattern posn) nil))
	 (field-desig-len 3)
	 (field-pattern (case field-type
			  ((#\*) ".*")
			  ((#\+) ".+")
			  ;; alternation not working here in pregexp
			  ((#\&) "\\S.*\\S")
;			  ((#\&) "(?:\\S.*\\S)|\\S")  ;; preferred, if working
;			  ((#\&) "\\B.*\\B")
			  ((#\#) "[0-9]+")
			  ((#\i) "[a-zA-Z][a-zA-Z0-9?_!]*")
;;;;		              ((#\s) "")  ;; special symbols -- add later
			  (t (setf field-desig-len 2) ".*"))))
    (values field-desig-len
	    (if capture
		(format nil "(~A)" field-pattern)
;		(format nil "\\(~A\\)" field-pattern)
	        field-pattern))))

;; Match patterns in pattern list against a string or ee-descriptor.
;; Patterns are tried in order.  Search stops on first successful match.
;; Result is either a list form as returned by excl:match-regexp or an
;; ee-descriptor value.

(defun match-expr (patterns str-or-expr)
  (let ((expr (if (stringp str-or-expr)
		  (make-ee-descriptor :string str-or-expr)
		str-or-expr)))
    (loop for pat in patterns
          when (let ((match (if (> (length pat) 0)
				(match-expr-pattern pat expr)
			        (list t nil ""))))
		 (and (expr-match-success match) match))
	  return it)))

(defun expr-match-success (match)
  (or (ee-descriptor-p match) (car match)))

;; Match a pattern against a string using the second character of the
;; pattern to select the match type (for rich patterns) or using a partial
;; match (for simple patterns).  Types include:
;;   f       -- full string match
;;   p       -- partial string match (first substring to match pattern)
;;   s       -- partial match returning original string (obj) if successful
;;   t       -- top-down expression matching
;;   b       -- bottom-up expression matching
;;   <digit> -- top-down expr matching, skipping top-most <digit> levels
;;   none of the above -- partial string match
;; Pattern must be already converted to regexp format.
;; Returns either an ee-descriptor or a list value as returned by
;; excl:match-regexp.

(defun match-expr-pattern (pattern descriptor)
  (let* ((is-rich (eql (char pattern 0) rich-pat-char))
	 (match-type (if is-rich (char pattern 1) #\p))
	 (regexp-pat (if is-rich (subseq pattern 2) pattern)))
    (if (member match-type '(#\f #\p #\s))
	(let ((expr-text (virt-ee-string descriptor)))
	  (case match-type
	    ((#\f) 
	     (multiple-value-list
	      (match-regexp (format nil "^~A$" regexp-pat) expr-text)))
;	      (excl:match-regexp (format nil "^~A$" regexp-pat) expr-text)))
	    ((#\p) 
	     (multiple-value-list (match-regexp regexp-pat expr-text)))
;	     (multiple-value-list (excl:match-regexp regexp-pat expr-text)))
	    ((#\s) 
	     (let ((match (multiple-value-list
			   (match-regexp regexp-pat expr-text))))
;			   (excl:match-regexp regexp-pat expr-text))))
	       (if (car match)
		   (make-ee-descriptor ;; :string expr-text
		                       :pvs-obj (ee-pvs-obj descriptor))
		   match)))))
        (let ((expr-obj (ee-pvs-obj descriptor)))
	  (cond ((eql match-type #\t)
		 (top-down-expr-match regexp-pat expr-obj 0))
		((digit-char-p match-type) 
		 (top-down-expr-match regexp-pat expr-obj
				      (parse-integer (string match-type))))
		((eql match-type #\b)
		 (bottom-up-expr-match regexp-pat expr-obj))
            ;; none of the above -- do partial match, keep first pattern char
		(t (multiple-value-list
		    (match-regexp (subseq pattern 1)
				  (virt-ee-string descriptor)))))))))
;		    (excl:match-regexp (subseq pattern 1)
;				       (virt-ee-string descriptor)))))))))

;; Apply pattern to match subexpressions of an expression in top-down
;; fashion (pre-order traversal).  Skip top-most n layers while matching.
;; Currently handles basic function applications (infix and prefix).
;; Yet to be added is support for various syntactic forms of PVS.

(defun top-down-expr-match (pattern expr-obj skip)
  (labels ((expr-match (expr depth)
	     (let ((node-match
		    (and (>= depth skip)
			 (multiple-value-list
			  (match-regexp pattern (textify expr))))))
;			  (excl:match-regexp pattern (textify expr))))))
	       (if (car node-match)
		   node-match
		   (let ((expr-arg (handler-case (argument expr)
						 (error (condition) nil))))
		     (and expr-arg
			  (handler-case 
			    (do ((subexprs (exprs expr-arg) (cdr subexprs))
				 (match nil))
				((or (car match) (null subexprs)) match)
			      (setf match
				    (expr-match (car subexprs) (1+ depth))))
			    (error (condition)
				   (expr-match expr-arg (1+ depth))))))))))
    (expr-match expr-obj 0)))

;; Apply pattern to match subexpressions of an expression in bottom-up
;; fashion (post-order traversal).

(defun bottom-up-expr-match (pattern expr-obj)
  (labels ((expr-match (expr)
	     (let ((subtree-match
		    (let ((expr-arg (handler-case (argument expr)
						  (error (condition) nil))))
		      (and expr-arg
			   (handler-case 
			     (do ((subexprs (exprs expr-arg) (cdr subexprs))
				  (match nil))
				 ((or (car match) (null subexprs)) match)
			       (setf match (expr-match (car subexprs))))
			     (error (condition) (expr-match expr-arg)))))))
	       (if (car subtree-match)
		   subtree-match
		   (multiple-value-list
		    (match-regexp pattern (textify expr)))))))
;		    (excl:match-regexp pattern (textify expr)))))))
    (expr-match expr-obj)))


;;; ------------ Constants (depend on prior definitions) ------------

;; PVS expression strings not requiring surrounding parentheses are
;; filtered out using the following patterns.  Currently includes
;; numbers, identifiers and expressions with outermost parentheses.
;; Will add prefix function applications later when pattern language
;; is powerful enough to express it.

  (defparameter *non-infix-expr-patterns*
    (percent-to-regexp-pattern '("@f%1#" "@f%1i" "@f(%0*)")))
                                 ;;; "@f%1i(%0*)"))))  ;;; prefix funs

  (defparameter *pvs-identifier-pattern*
    (percent-to-regexp-pattern '("@f%1i")))
