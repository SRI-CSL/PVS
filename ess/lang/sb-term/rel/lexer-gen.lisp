;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)lexer-gen.lisp	1.16 9/27/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; Ergo Project
;;; 
;;; Automatic Generation of the Lexer
;;;
;;; Robert L. Nord
;;; Carnegie-Mellon University
;;; July 1985
;;;
;;; This is the automatic lexer generation code for the SB.  It will create
;;; a custom lexer file that consists of:
;;;     a set of specials that contain application specific
;;;          information, (i.e. keywords, operators, comments)
;;;     the init-lexer routine that binds the above specials and calls routines
;;;          in the generic lexer to initialize the readtable and the keyword
;;;          table.
;;;     a set of lexical action routines to handle multiple character operators.

;;; Revised, Scott Dietzen, Mon Oct 13 17:00:52 1986
;;;   	Added check for comment character overlapping other operators. 
;;;	   (Routines check-comment-char(s)-with-op(S))


(in-package 'syntax-box)   (use-package :ergolisp)



;;; An op-tree is a parsed representation of an operator. 
(defstruct op-tree op validp children)

;;; Generate custom lexer will create a file 'lexer-file-spec' containing
;;; the information unique to the users application.  Grammar is a defstruct
;;; containing the operator list and comment characters.
(defun generate-custom-lexer (grammar lexer-file-spec)
  (let* ((keywords (mapcar #'(lambda (x)
                               (sbst-intern-case x))
                           (get-keywords *global-lexer-information*)))
	 (operator-list (mapcar #'(lambda (x)
                                    (sbst-intern-case x))
                                (get-operator-list grammar keywords)))
	 (parsed-oplist (parse-op operator-list))
	 (open-comment-char
	  (if (grammar-open-comment-char grammar)
	      (schar (grammar-open-comment-char grammar) 0)))
	 (close-comment-char
	  (if (grammar-close-comment-char grammar)
	      (schar (grammar-close-comment-char grammar) 0)))
	 (new-line-comment-char
	  (if (grammar-new-line-comment-char grammar)
	      (schar (grammar-new-line-comment-char grammar) 0)))
	 (escape-char
	  (if (grammar-escape-character grammar)
	      (schar (grammar-escape-character grammar) 0)))
	 (case-sensitive (grammar-case-sensitive? grammar))
	 single-char-ops multi-char-ops)
    (check-comment-chars-with-ops (list new-line-comment-char
					open-comment-char
					close-comment-char) 
				  operator-list)
    (multiple-value-setq (single-char-ops multi-char-ops)
      (parse-op-tree parsed-oplist))
    (setf *global-operator-list* operator-list)
    (with-open-file (s lexer-file-spec :direction :output :if-exists
		       		       :new-version)
      (sb-write
       (format t "   writing file ~S.~%" lexer-file-spec)
       (initialize-file s :lexer)
       (dolist (x `((defparameter ,*conc-keyword-list* ',keywords)
		    (defparameter ,*conc-single-char-op-list* ',single-char-ops)
		    (defparameter ,*conc-multi-char-op-list*
		      ,(if multi-char-ops `',(mk-char-ops multi-char-ops)))
		    (defparameter ,*conc-all-operators-list*
		      ',*global-operator-list*)
		    (defparameter ,*conc-new-line-comment-char*
		      ,new-line-comment-char)
		    (defparameter ,*conc-open-comment-char*
		      ,open-comment-char)
		    (defparameter ,*conc-close-comment-char*
		      ,close-comment-char)
		    (defparameter ,*conc-escape-char* ,escape-char)
		    (defparameter ,*conc-case-sensitive* ,case-sensitive)

		    (defparameter ,*conc-string-char*
		      ,(if (is-lexical-terminal 'string grammar)
			   (schar (get-lt-delimiter 'string grammar) 0)))
		    (defparameter ,*conc-keyword-char*
		      ,(if (is-lexical-terminal 'keyword grammar)
			   (schar (get-lt-delimiter 'keyword grammar) 0)))
		    (defparameter ,*conc-literal-char*
		      ,(if (is-lexical-terminal 'literal grammar)
			   (schar (get-lt-delimiter 'literal grammar) 0)))
		    
		    (defparameter ,*conc-restricted-chars*
		      (reduce #'(lambda (r s)
				  (union r s :test #'char=))
			      (list ,*conc-single-char-op-list*
				    (if ,*conc-new-line-comment-char*
					(list ,*conc-new-line-comment-char*))
				    (if ,*conc-open-comment-char*
					(list ,*conc-open-comment-char*))
				    (if ,*conc-close-comment-char*
					(list ,*conc-close-comment-char*))
				    (if ,*conc-escape-char*
					(list ,*conc-escape-char*))
				    (if ,*conc-string-char*
					(list ,*conc-string-char*))
				    (if ,*conc-keyword-char*
					(list ,*conc-keyword-char*))
				    (if ,*conc-literal-char*
					(list ,*conc-literal-char*)))))
		    (defvar ,*conc-keyword-table* nil)))
	 (pprint x s)))
      (gen-defun-init-lexer s)
      (dolist (op-tree multi-char-ops)
	(gen-defun-lex s op-tree))
      (terpri s))))

;;; Generate the init-lexer routine
(defun gen-defun-init-lexer (stream)
  (sb-write
   (pprint `(defun init-lexer-aux (lexstream)
		    (init-lexical-readtable
		     lexstream
		     :single-char-op-list   ,*conc-single-char-op-list*
		     :new-line-comment-char ,*conc-new-line-comment-char*
		     :open-comment-char     ,*conc-open-comment-char*
		     :escape-char           ,*conc-escape-char*
		     :multi-char-op-list    ,*conc-multi-char-op-list*))
	   stream)
   (terpri stream)
   (pprint `(defun init-lexer (lexstream)
	      (init-lexer-aux lexstream)
	      (if ,*conc-string-char*
		  (lexical-make-macro lexstream
				      ,*conc-string-char*
				      #'read-sb-string))
	      (if ,*conc-keyword-char*
		  (lexical-make-macro lexstream
				      ,*conc-keyword-char*
				      #'read-keyword-string))
	      (if ,*conc-literal-char*
		  (lexical-make-macro lexstream
				      ,*conc-literal-char*
				      #'read-literal)))
	   stream)
   (terpri stream)))



;;; Create the character operator pair list.  This consists of dotted pairs
;;; containing the leading character of the operator and the name of the 
;;; lexical action routine.  E.g if ':=' is an operator, the pair
;;; '#\: . lex-:' will be added to the list.
(defun mk-char-ops (parsed-oplist)
  (let ((char-op-list nil))
    (dolist (op-list parsed-oplist char-op-list)
      (push (cons (op-tree-op op-list)
		  (mk-fun-name (string (op-tree-op op-list))))
	    char-op-list))))

;;; parse-op will parse the operator list and return a list of op-tree's
;;; Each tree-op has three fields, op, validp, and children
;;;  The root op of each tree is the first character of each operator
;;; the op's of the children of the root are the possible second
;;; characters and so on.
;;; The validp field specifies whether the sequence of characters starting
;;; from the root and ending at that node is a valid operator or not.
(defun parse-op (oplist)
  (declare (type list oplist))
  (let ((parse-tree (make-op-tree :op :root :validp nil :children ())))
    (dolist (op oplist (op-tree-children parse-tree))
      (do* ((str-op (string op))
	    (sptr 0 (+ sptr 1))
	    (eptr (length str-op))
	    (old-child parse-tree new-child)
	    (new-child))
	   ((= sptr eptr) (setf (op-tree-validp old-child) t))
	(declare (simple-string str-op))
	(setf new-child
	      (car (member (schar str-op sptr) (op-tree-children old-child)
			   :test #'char= :key #'op-tree-op))) 
	(unless new-child
	  (do* ((eptr (- eptr 1) (- eptr 1))
		(ch (schar str-op eptr)(schar str-op eptr))
		(nw-op-tree (make-op-tree :op ch :validp t :children nil)
			    (make-op-tree :op ch :validp nil
					  :children (list nw-op-tree))))
	       ((= sptr eptr) (push nw-op-tree (op-tree-children old-child))))
	  (return))))))
	 
;;; parse-op-tree will take the parsed operator list generated from 
;;; parse op and return a list of the single character operators
;;; (that are not a prefix to a multiple character operator)
;;; and a list of the parse trees of the multiple character operators.
(defun parse-op-tree (parsed-oplist)
  (let ((single-char-ops nil)
	(multi-char-ops nil))
    (dolist (optree parsed-oplist (values single-char-ops multi-char-ops))
      (if (op-tree-children optree)
	  (push optree multi-char-ops)
	  (push (op-tree-op optree) single-char-ops)))))

;;; mk-defun-lex takes a parse tree generated from "parse-op" and
;;; constructs the lexical action routine associated with the root character
;;; mk-defun-lex calls "mk-defun-lex-body" which generates a cond statement
;;; the routine generated uses look-ahead to try to match the longest possible
;;; operator.
(defun gen-defun-lex (stream root)
  (declare (type op-tree root))
  (let ((prefix (string (op-tree-op root))))
    (sb-write
     (pprint `(defun ,(mk-fun-name prefix) (stream symbol)
		(declare (ignore symbol))
		(let (holdchar)
		  ,@(mk-defun-lex-body root prefix)))
	     stream))))

(defun mk-defun-lex-body (root prefix)
  (declare (type op-tree root)
	   (type simple-string prefix))
  (cond ((not (op-tree-children root)) (list `',(intern prefix *sbst-package*)))
	(t (list `(setf holdchar (lexical-read-char stream :eof))
		 `(if (and ,*conc-escape-char*
			   (char= holdchar ,*conc-escape-char*))
		      (setf holdchar (lexical-read-char stream :eof)))
		 `(cond ,@(let ((stuff nil)
				(new-prefix nil))
			    (dolist (child (op-tree-children root) stuff)
			      (setf new-prefix
				    (concatenate 'simple-string prefix
						 (string (op-tree-op child))))
			      (push `((char= holdchar ,(op-tree-op child))
				      ,@(mk-defun-lex-body child new-prefix))
				    stuff)))
			(t (lexical-unread-char stream)
			   ,@(if (op-tree-validp root)
				 (list `',(intern prefix *sbst-package*))
				 (list `(illegal-token-error ,prefix)
				       :illegal-token))))))))


(defun mk-fun-name (name)
  (declare (type simple-string name))
  (intern (concatenate 'simple-string "LEX-" name)
	  *sb-package*))

;;; Generate the operator list from the grammar defstruct.
;;; If no operator list is specified, then a default is generated
;;; from the keyword list consisting of those symbols that do not contain
;;; alphanumeric characters.
(defun get-operator-list (grammar keywords)
  (let* ((user-oplist (grammar-operator-list grammar))
	 (user-ext-oplist (union user-oplist *ext-used-operators* :test #'eq))
	 (default-oplist (get-default-operator-list keywords))
	 (op-list (union (or user-oplist default-oplist) *ext-used-operators*
			 :test #'eq))
	 (user-added (set-difference user-ext-oplist default-oplist :test #'eq))
	 (user-added-alphanum
	  (do* ((list user-added (cdr list))
		(elm (car list) (car list))
		(result nil))
	       ((null list) result)
	    (if (alphanumericp (schar (symbol-name elm) 0))
		(push elm result))))
	 (user-omitted
	  (if user-oplist
	       (set-difference default-oplist user-ext-oplist :test #'eq)))
	 (user-omitted-must-op
	  (get-must-ops (set-difference keywords op-list :test #'eq)
			op-list))
	 (r-user-omitted
	  (set-difference user-omitted user-omitted-must-op :test #'eq)))
    (if user-added-alphanum
	(format t "   Warning: Operators beginning with an alphanumeric ~
		~%   character will cause problems in lexing identifiers and ~
		~%   numbers.  The following keywords should not be operators:~
		~%   ~A~%"
		user-added-alphanum))
    (if r-user-omitted
	(format t "   Warning: The following keywords are permissible operators~
		~%   but are not declared as operators:~
		~%   ~A~%"
		r-user-omitted))
    (if user-omitted-must-op
	(progn
	 (setf op-list (append op-list user-omitted-must-op))
	 (if user-oplist
	     (format t "   Warning: The following keywords were added to the ~
		     ~%   operator list since an operator with the same prefix ~
		     ~%   was specified:~%   ~A~%"
		     user-omitted-must-op))))
    (unless user-oplist
      (format t "   Warning: no operator list has been specified.~
	  ~%   The following default has been derived from the keyword list:~
	  ~%   ~A~%"
	      op-list))
    op-list))

(defun get-must-ops (queryops ops)
  (let ((result nil)
	(char-ops (mapcar #'(lambda (x) (schar (symbol-name x) 0)) ops)))
    (dolist (op queryops result)
      (if (member (schar (symbol-name op) 0) char-ops :test #'char= )
	  (push op result)))))

;;; Constructs an operator list from the keyword list.
;;; An operator currently defaults to any non-alphanumeric word.
(defun get-default-operator-list (keyword-list)
  (let ((op-list nil))
    (dolist (key keyword-list op-list)
      (if (nonalphanumeric-p (string key)) (push key op-list)))))

(defun nonalphanumeric-p (str)
  (declare (simple-string str))
  (dotimes (index (length str) t)
    (if (alphanumericp (char str index)) (return nil))))


(defun get-keywords (lexer-info)
  (do ((nt-a-list lexer-info (cdr nt-a-list))
       (result nil (union result (cadar nt-a-list) :test #'eq)))
      ((null nt-a-list) result)))



(defun check-comment-chars-with-ops (comment-op-list op-list)
  (mapc (function (lambda (x)
		    (check-comment-char-with-ops x op-list)))
	comment-op-list))

(defun check-comment-char-with-ops (comment-char op-list)
  (mapc (function (lambda (x)
		    (if comment-char 
			(check-comment-char-with-op comment-char x))))
	op-list))

(defun check-comment-char-with-op (comment-char op)
  (if (char= comment-char
	     (elt (symbol-name op) 0))
      (format t "   Warning: A comment character [~c] overlaps one of your ~
		~%    operators [~A].  This operator will not be parsed ~
		~%    properly. One or the other should be changed! ~%"
	      comment-char
	      op)))

