;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-lex.lisp	1.8 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; This is the generic lexer routines.  It exports many specials for generated
;;; parsers to bind when calling exported routines.

;;; Written by Scott Dietzen and Bill Chiles

;;; Revised to return new base terms (from the uniform ADT proposal). 
;;; Scott Dietzen, Fri Oct 10 12:36:12 1986


(in-package "SB-RUNTIME")  (use-package :ergolisp)

(export '(*sbst-package* *abs-syn-package* *keyword-list* *single-char-op-list*
	  *multi-char-op-list* *new-line-comment-char* *open-comment-char*
	  *close-comment-char* *case-sensitive* 
	  *reader-fun* *apply-lex-term-constr-fun*
	  *apply-lex-term-discr-fun* *apply-lex-term-destr-fun*
	  *keyword-table* *lexical-stream* *hold-a1*
	  *delexical-stream* *escape-character* 
	  *unparse-style* *restricted-chars*
	  *hold-a2* *hold-a3* *hold-b1* *hold-b2* *hold-b3*
	  init-lexical-readtable init-keyword-table
	  gettoken gobble-token peek-first peek-second flush-lexer
	  illegal-token-error init-delexer close-delexer
          reader error-peek-token
	  insert-escapes?
	  read-keyword-string read-sb-string read-literal
	  ))


(defconstant possible-single-char-operators
  '(#\(  #\)  #\[  #\]  #\{  #\}  #\<  #\>  #\,  #\;  #\|  #\^  #\#  #\~  #\/
    #\!  #\@  #\$  #\&  #\_  #\-  #\?  #\%  #\'  #\:  #\*  #\+  #\`  #\=  #\\))


;;; The ":use '()" below is important because different common lisps will 
;;; export different symbols from the LISP package, so it is important that
;;; the SBST package does not use the LISP package, so we can get repeatable
;;; behavior on different machines.
(eval-when (compile load eval)
  (defvar *sbst-package* 
    (cond ((find-package "SBST"))
	  (t
	   (make-package "SBST"
			 :nicknames '("SB-ST" "SB-SYMBOL-TABLE") :use '())))))


;;; This is dynamically bound by generated parsers to their package, 
;;; so symbols get interned appropriately.
(defvar *abs-syn-package*)

(defvar *close-comment-char* nil
  "The character that ends comments the lexer should skip.")

(defvar *case-sensitive* nil
  "Is the language case sensitive?")

;;; This is dynamically bound by generated parsers.  Usually it is bound
;;; to the READER fun in this file, but if the application must have special
;;; lexical terminals, then the user has to copy READER and modify it.  Also,
;;; he must shadow the symbol in the lexer generated for him.
(defvar *reader-fun*)

;;; This is dynamically bound by generated parsers.  Usually it is bound
;;; to the APPLY-LEXICAL-TERMINAL-CONSTRUCTOR fun in this file, but if the
;;; application must have special lexical terminals, then the user may have
;;; to have such a function.  It must be a function of two arguments --
;;; a token type and a token.  Also, he must shadow the symbol in the lexer
;;; generated for him.
(defvar *apply-lex-term-constr-fun*)

;;; This is dynamically bound by generated unparsers.  Usually it is bound
;;; to the APPLY-LEXICAL-TERMINAL-DISCRIMINATOR fun in this file, but if the
;;; application must have special lexical terminals, then the user will have
;;; to have such a function.  It must be a function of two arguments --
;;; non-terminal name and abstract syntax.  Also, he must shadow the symbol
;;; in the lexer generated for him.
(defvar *apply-lex-term-discr-fun*)

;;; This is dynamically bound by generated unparsers.  Usually it is bound
;;; to the APPLY-LEXICAL-TERMINAL-DESTRUCTOR fun in this file, but if the
;;; application must have special lexical terminals, then the user will have
;;; to have such a function.  It must be a function of two arguments --
;;; non-terminal name and abstract syntax.  Also, he must shadow the symbol
;;; in the lexer generated for him.
(defvar *apply-lex-term-destr-fun*)

;;; This is dynamically bound by generated parsers.
(defvar *keyword-table*)

;;; This is dynamically bound by generated parsers.
(defvar *lexical-stream*)

(defvar *hold-a1*)
(defvar *hold-a2*)
(defvar *hold-a3*)
(defvar *hold-b1*)
(defvar *hold-b2*)
(defvar *hold-b3*)

;;; Lexical readtable routines
(defun init-lexical-readtable (lexstream &key
					 single-char-op-list
					 new-line-comment-char
					 open-comment-char
					 escape-char
					 multi-char-op-list)
  (dolist (x single-char-op-list) (lexical-make-macro lexstream x #'lex-op))
  (when new-line-comment-char
    (lexical-make-macro lexstream new-line-comment-char #'lex-newline-comment))
  (when open-comment-char
    (lexical-make-macro lexstream open-comment-char #'lex-comment))
  (when escape-char
    (lexical-make-escape lexstream escape-char))
  (dolist (pair multi-char-op-list)
    (let* ((char (car pair))
	   (fun (cdr pair)))
      (lexical-make-macro lexstream char (symbol-function fun)))))

(defun lex-op (stream op)
  (declare (ignore stream))
  (intern (if *case-sensitive*
	      (string op)
	      #+allegro-v6.0 (string-downcase (string op))
	      #-allegro-v6.0 (string-upcase (string op)))
	  *sbst-package*))

(defun lex-newline-comment (stream open-comment)
  (let ((*close-comment-char* #\newline))
    (lex-comment stream open-comment)))

(defun lex-comment (stream open-comment)
  (declare (ignore open-comment))
  (loop
   (let ((char (lexical-read-char stream *close-comment-char*)))
     (cond ((is-lexical-escape? stream char)
	    (lexical-read-char stream *close-comment-char*)) ; disgard.
	   ((equal char *close-comment-char*)
	    (return)))))
  (values))

;;; Keyword table routines

(defun init-keyword-table (keyword-list)
  (let ((keyword-table (make-hash-table :test #'eq)))
    (dolist (x keyword-list) (setf (gethash x keyword-table) t))
    keyword-table))

;;; Token fetching, advancing, and peeking.

;;; Note that the (gethash token *keyword-table*) branch of the COND returns
;;; the token type and the token in backwards order.  This is because the
;;; generated parsers expect the peekers (PEEK-FIRST PEEK-SECOND)
;;; to return what the parsers are trying to match.  What they are trying to
;;; match are either lexical terminal names (!id!, !string!, !number!)
;;; or explicit keywords.  This is necessary for the parser to know which
;;; branches to parse.
;(defun reader ()
;  (multiple-value-bind (token place) (lexical-read *lexical-stream* :eof)
;    (cond ((numberp token)
;	   (values 'sbst::!number! token place))
;	  ((stringp token)
;	   (values 'sbst::!string! token place))
;	  ((gethash token *keyword-table*)
;	   (values token :keyword-internal-flag place))
;	  ((eq token :eof) (values :eof nil place))
;	  ((eq token :illegal-token)
;	   (values :illegal-token :illegal-token place))
;	  (t (values 'sbst::!id!
;		     (intern (if *case-sensitive*
;				 (symbol-name token)
;				 (string-upcase (symbol-name token)))
;			     *abs-syn-package*)
;		     place)))))
;
;
;;; GETTOKEN is used to return the current token for insertion in
;;; abstract syntax.  Incidentally, this guy also moves to the next token.  
(defun gettoken ()
  (multiple-value-bind (type token)
		       (cond (*hold-b1*
			      (values (prog1 *hold-b1* (setf *hold-b1* nil))
				      *hold-b2*))
			     (*hold-a1*
			      (values (prog1 *hold-a1* (setf *hold-a1* nil))
				      *hold-a2*))
			     (t (funcall *reader-fun*)))
    (cond (;; This branch of the COND is completely meaningless because
	   ;; there is currently no way to cause a generated parser to return
	   ;; a lex'ed keyword (e.g., 'begin', 'then', ':=').  Parsers merely
	   ;; check that the correct keyword is there and then throws it away.
	   (eq type :keyword-internal-flag) token)
	  ((eq type :illegal-token) :illegal-token)
	  (t (funcall *apply-lex-term-constr-fun* type token)))))

;;; Consume a token from the input stream.
(defun gobble-token ()
  (cond (*hold-b1* (setq *hold-b1* ()))
	(*hold-a1* (setq *hold-a1* ()))
	(t (funcall *reader-fun*) nil)))

(defun values-a () (values *hold-a1* *hold-a2* *hold-a3*))

(defun values-b () (values *hold-b1* *hold-b2* *hold-b3*))

;;; Look ahead one token in the input stream.
(defun peek-first ()
  (cond (*hold-b1* (values-b))
	(*hold-a1* (values-a))
	(t (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3*)
	     (funcall *reader-fun*))
	   (values-a))))

(defun peek-second ()
  (cond (*hold-b1* (values-a))
	(*hold-a1*
	 (setf *hold-b1* *hold-a1*
	       *hold-b2* *hold-a2*
	       *hold-b3* *hold-a3*)
	 (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3*)
	   (funcall *reader-fun*))
	 (values-a))
	(t (multiple-value-setq (*hold-b1* *hold-b2* *hold-b3*)
	     (funcall *reader-fun*))
	   (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3*)
	     (funcall *reader-fun*))
	   (values-a))))

;;; Fiddling with hold-x2's here seems silly, since it is the hold-x1's that 
;;; determine whether the token is there or not.
(defun flush-lexer ()
  (setf *hold-a1* ())
  (setf *hold-a2* ())
  (setf *hold-b1* ())
  (setf *hold-b2* ()))

(defun illegal-token-error (string)
  (format t "   Illegal token found in input -- ~S~%" string))



;;;;  Delexer

(defvar *delexical-stream*)

(defvar *restricted-chars*)

(defvar *escape-character*)

(defvar *unparse-style*)

(defvar *no-escapes*)

(defparameter *spaces-to-indent-per-level* 1)   ;;  exported to formatter



(defun make-token-string-or-char (token)      ;; exported to formatter
  (case (token-kind token)
    (:keyword
     (if (eq (token-subkind token) :jux)
	 ""
	 (if *case-sensitive*
	     (string (token-value token))
	     (string-downcase (string (token-value token))))))
    ((:id :identifier)
     (if *case-sensitive*
	 (symbol-name (token-value token))
	 (string-downcase (symbol-name (token-value token)))))
    (:string
     (token-value token))
    (:number
     (princ-to-string (token-value token)))))





(defun insert-escapes? ()
  (and *escape-character*
       *print-escape*
       ;;(or (null *unparse-style*)
       ;;    (intersection *unparse-style* '(:parse :default) :test #'eq))
       ))
       

(defun insert-escapes (s &key (delimited nil)
			      (omit-first nil))
  (let ((r (make-string-output-stream)))
    (map nil 
	 #'(lambda (c)
	     (if (or (and (not delimited)
			  (or (member c *restricted-chars* :test #'char=)
			      (not (graphic-char-p c))
			      (char= c #\space)))
		     (and delimited
			  (or (member c delimited :test #'char=)
			      (char= c *escape-character*))))
		 (princ *escape-character* r))
	     (princ c r))
	 s)
    (let ((r (get-output-stream-string r)))
      (if (and omit-first
	       (char= (elt r 0) *escape-character*))
	  (subseq r 1)
	  r))))





;;;Specilization of general lexer listed previously.

(defun read-keyword-string (stream delimiter)
  (cons :keyword (string-lexer stream delimiter)))

(defun read-sb-string (stream delimiter)
  (cons :string (string-lexer stream delimiter)))

(defun read-literal (stream op)
  (declare (ignore op))
  (declare (ignore stream))
  (multiple-value-bind (literal-type literal)
		       (reader)
    (cond ((eq literal-type 'sbst::!id!)
	   (cons :literal (symbol-name literal)))
	  ((eq literal-type 'sbst::!string!)
	   (cons :literal literal))
	  ((eq literal :keyword-internal-flag)
	   (cons :literal (symbol-name literal-type)))
	  (t (illegal-token-error (symbol-name literal))
	     :illegal-token))))


;;; The following routines allow the additional lexical terminals of !literal!
;;; and !keyword!.  This routine used to shadow the previous definition.

(defun reader ()
  (multiple-value-bind (token place) (lexical-read *lexical-stream* :eof)
    (cond ((consp token)
	   (case (car token)
	     (:literal
	      (values 'sbst::!literal!
		      (let ((name (if (symbolp (cdr token))
				      (if *case-sensitive*
					  (symbol-name (cdr token))
					  #+allegro-v6.0
					  (string-downcase
					   (symbol-name (cdr token)))
					  #-allegro-v6.0
					  (string-upcase
					   (symbol-name (cdr token))))
				      (cdr token))))
			(intern name *abs-syn-package*))
		      place))
	     (:string
	      (values 'sbst::!string!
		      (cdr token)
		      place))
	     (:keyword
	      (values 'sbst::!keyword!
		      (cdr token)	; string. can't intern until we know
					; case sensitivity. 
		      place))
	     (t (error "Internal Error -- ~
			   SB lexer confused by its own literal types -- ~S"
		       token))))
	  ((numberp token)
	   (values 'sbst::!number! token place))
	  ((stringp token)
	   (values 'sbst::!string! token place))
	  ((gethash token *keyword-table*)
	   (values token :keyword-internal-flag place))
	  ((eq token :eof) (values :eof nil place))
	  ((eq token :illegal-token)
	   (values :illegal-token :illegal-token place))
	  (t (values 'sbst::!id!
		     (intern (if *case-sensitive*
				 (symbol-name token)
				 #+allegro-v6.0
				 (string-downcase (symbol-name token))
				 #-allegro-v6.0
				 (string-upcase (symbol-name token)))
			     *abs-syn-package*)
		     place)))))

