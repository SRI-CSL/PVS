;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ergo-runtime-fixes.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat May 27 18:20:25 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr  5 00:43:24 1998
;; Update Count    : 2
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sbrt)

(export '(*num-keywords-skipped* *last-syntax* *last-newline-comment*
				 *hold-a4* *hold-b4*))

(defvar *end-place* nil)


;;; SBRT Parameters

;;; Turn off ergo auto-bracketing, otherwise ergo has an error when unparsing
;;; complicated expressions.  This variable is set to null while unparsing ada
;;; abstract syntax, as auto-bracketing is used there.

(setq sbrt:*parens-off* t)

;;; Set *rept-indent-units* to 0, otherwise first element of a rept is
;;; indented different from the rest (I don't know why).

(setq sbrt::*rept-indent-units* 0)

;;; Turn off indentation increasing with depth of nonterminals in the grammar
;(setq sbrt::*uterm-indent-units* 0)
;; testing on 9/18/90, if doesn't work then revert to above line:

(setq sbrt::indent-unit 0)


;;; PARSE: From here until UNPARSE: the functions are for parsing


;;; The following probably belongs in a different file, but must be compiled
;;; and loaded before the pvs-parser.lisp file, otherwise the built-in macro
;;; will be used instead.

(in-package :pvs)

(defparameter *infix-operators*
  '(O IFF <=> IMPLIES => WHEN OR \\/ AND /\\ & XOR ANDTHEN ORELSE
      ^ + - * / ++ ~ ** // ^^ \|- \|= <\| \|> = /= == < <= > >=
      << >> <<= >>= |#| @@ |##|))

(defparameter *unary-operators* '(NOT + - ~ [] <>))

(defvar *elsif-places* nil)

;;; Shadows a macro in rt-parse-mac.lisp, in dir ess/lang/sb-term/rel.  It
;;; took 2 args before, now third one is lexical position at start of
;;; non-terminal, given to mk-sim-term Operator-place is special for now,
;;; doesn't need to be if this function is a macro.  Operator-place saves the
;;; place of the operator to be saved as place information in the application
;;; that is constructed from a term-expr.  Similarly for operator-place-end.
;;; This function is invoked by the generated parser.

(defun make-sb-term (sim-op args &optional splace comment)
  (declare (ignore comment))
  ;;(assert splace)
  (let* ((result (term:mk-sim-term sim-op args))
	 (eplace (get-end-place sim-op args splace nil)))
    (when (and (eq sim-op 'ELSIF)
	       *elsif-places*)
      (setq splace (pop *elsif-places*)))
    (sbrt::save-place-and-comment-info result nil splace eplace)
    (case sim-op
      (TERM-EXPR (reset-operator-place args))
      (UNARY-TERM-EXPR (reset-unary-operator-place args)))
    ;;(assert (type-of result))
    result))

(defvar *last-end-place* nil)
(defvar *last-end-value* nil)

(defun get-end-place (sim-op args splace token?)
  (cond ((eq *last-end-place* sbrt::*end-place*)
	 *last-end-value*)
	(sbrt::*end-place*
	 (let ((eplace (caddr sbrt::*end-place*))
	       (token (if (eq (cadr sbrt::*end-place*) :keyword-internal-flag)
			  (car sbrt::*end-place*)
			  (cadr sbrt::*end-place*))))
	   (setq *last-end-place* sbrt::*end-place*)
	   (setq *last-end-value*
		 (sbrt::make-place
		  :linenumber (sbrt::place-linenumber eplace)
		  :charnumber (+ (the fixnum (sbrt::place-charnumber eplace))
				 (the fixnum (length (princ-to-string token))))))))
	(args
	 (let ((maxplace (maximal-endplace args)))
	   (sbrt::make-place
	    :linenumber (svref maxplace 2)
	    :charnumber (svref maxplace 3))))
	(t (sbrt::make-place
	    :linenumber (sbrt::place-linenumber splace)
	    :charnumber (+ (the fixnum (sbrt::place-charnumber splace))
			   (if token?
			       (the fixnum (length (princ-to-string sim-op)))
			       1))))))

(defun maximal-endplace (args &optional place)
  (cond ((null args)
	 place)
	((null place)
	 (maximal-endplace (cdr args) (term-place (car args))))
	(t (let ((aplace (term-place (car args))))
	     (maximal-endplace
	      (cdr args)
	      (if (or (< (the fixnum (svref place 2))
			 (the fixnum (svref aplace 2)))
		      (and (= (svref place 2) (svref aplace 2))
			   (< (svref place 3) (svref aplace 3))))
		  aplace
		  place))))))
		     
(defun reset-operator-place (args)
  (let* ((arg1 (term-arg0 (cadr args)))
	 (arg1-place (term-place arg1))
	 (arg2 (term-arg1 (cadr args)))
	 (arg2-place (term-place arg2))
	 (op (sim-term-op (car args)))
	 (oplace (find-if #'(lambda (oplace)
			      (and (eq (car oplace) op)
				   (place< arg1-place (cdr oplace))
				   (or (null arg2-place)
				       (place< (cdr oplace) arg2-place))))
		   *operator-places*)))
    (assert oplace)
    (setq *operator-places* (delete oplace *operator-places*))
    (setf (getf (term:term-attr (car args)) :place) (cdr oplace))))

(defun reset-unary-operator-place (args)
  (let ((op (sim-term-op (car args)))
	(oplace (getf (term:term-attr (car args)) :place)))
    (setf (svref oplace 2) (svref oplace 0))
    (setf (svref oplace 3) (+ (svref oplace 1) (length (string op))))))

(defun place< (place1 place2)
  (or (< (svref place1 2) (svref place2 0))
      (and (= (svref place1 2) (svref place2 0))
	   (<= (svref place1 3) (svref place2 1)))))
       
    
(defun reader ()
  (multiple-value-bind (token place comment)
      (lexical-read *lexical-stream* :eof)
    ;;(format t "~%reader: token = ~s, place = ~s, comment = ~s"
    ;;  token place comment)
    (cond ((consp token)
	   (case (car token)
	     (:literal
	      (values 'sbst::!literal!
		      (let ((name (if (symbolp (cdr token))
				      (if sbrt:*case-sensitive*
					  (symbol-name (cdr token))
					  (string-upcase
					   (symbol-name (cdr token))))
				      (cdr token))))
			(intern name *abs-syn-package*))
		      place comment))
	     (:string
	      (values 'sbst::!string!
		      (cdr token)
		      place comment))
	     (:keyword
	      (values 'sbst::!keyword!
		      (coerce (cdr token) 'string)
					; string. can't intern until we know
					; case sensitivity. 
		      place comment))
	     (t (error "Internal Error -- ~
			   SB lexer confused by its own literal types -- ~S"
		       token))))
	  ((numberp token)
	   (values 'sbst::!number! token place comment))
	  ((stringp token)
	   (values 'sbst::!string! token place comment))
	  ;; SO - don't care about case for keywords
	  ;; At some point should add another case-sensitivity option.
	  ((gethash (intern (string-upcase token) :sbst) *keyword-table*)
	   (let* ((upstr (string-upcase token))
		  (pvs-sym (intern upstr :pvs)))
	     (when (memq pvs-sym pvs::*infix-operators*)
	       (let ((oplace (vector (sbrt::place-linenumber place)
				     (sbrt::place-charnumber place)
				     (sbrt::place-linenumber place)
				     (+ (the fixnum
					  (sbrt::place-charnumber place))
					(the fixnum
					  (length (string token)))))))
		 (push (cons pvs-sym oplace) pvs::*operator-places*)))
	     (values (intern upstr 'sbst)
		     :keyword-internal-flag place comment)))
	  ((eq token :eof) (values :eof nil place comment))
	  ((eq token :illegal-token)
	   (values :illegal-token :illegal-token place))
	  (t (let ((id (intern (if *case-sensitive*
				 (symbol-name token)
				 (string-upcase (symbol-name token)))
			     *abs-syn-package*)))
	       (values 'sbst::!id! id place comment))))))

(in-package :sbrt)

;;; Sets *end-of-last-token* to the place in the lexical stream before
;;; stripping off white space. 

(defvar *end-of-last-token* nil "place where reading of the
  last token ended, points to beginning of white space before current
  first token in token stream")

(defvar *collect-comments* nil
  "Flag indicating that it is all right to collect comments.  Set by gettoken
and gobble-token.  Otherwise comments may be collected when just peeking.")

(defvar *hold-comments* nil
  "A place to keep comments until they can be processed")

(defvar *num-keywords-skipped* 0
  "Keeps track of the number of tokens skipped - this number is kept along
with the comment so as to put it in the proper place")

;;; gettoken and gobbletoken are used to get the next token; they both call
;;; reader.  These are the right places to do processing of all tokens, rather
;;; than reader, which is also called to do peek-first so may make multiple
;;; passes over the same token.

(defun gettoken ()
  (multiple-value-bind (type token place comment)
      (cond (*hold-b1*
	     (values (prog1 *hold-b1* (setf *hold-b1* nil))
		     *hold-b2* *hold-b3* *hold-b4*))
	    (*hold-a1*
	     (values (prog1 *hold-a1* (setf *hold-a1* nil))
		     *hold-a2* *hold-a3* *hold-a4*))
	    (t (funcall *reader-fun*)))
    ;;(format t "~%gettoken: type = ~s, token = ~s, place = ~s, comment = ~s"
    ;;  type token place comment)
    (setq *end-place* (list type token place))
    (setq *last-newline-comment* (nconc *last-newline-comment* comment))
    ;;(format t "~%gettoken comment = ~s" comment)
    (cond (;; This branch of the COND is completely meaningless because
	   ;; there is currently no way to cause a generated parser to return
	   ;; a lex'ed keyword (e.g., 'begin', 'then', ':=').  Parsers merely
	   ;; check that the correct keyword is there and then throws it away.
	   (eq type :keyword-internal-flag)
	   token)
	  ((eq type :illegal-token) :illegal-token)
	  (t (funcall *apply-lex-term-constr-fun*
		      type token place comment)))))


;;; Consume a token from the input stream.  The *hold-a1*, etc., values
;;; are the token type, token value, place, and comment, respectively.
(defun gobble-token ()
  (let ((*collect-comments* t))
    (incf *num-keywords-skipped*)
    ;;(format t "~%Skipping ~a - skipped ~d"
    ;;  (peek-first) *num-keywords-skipped*)
    (multiple-value-bind (v1 v2 v3 v4)
	(cond (*hold-b1*
	       (values (prog1 *hold-b1* (setq *hold-b1* nil))
		       *hold-b2* *hold-b3* *hold-b4*))
	      (*hold-a1*
	       (values (prog1 *hold-a1* (setq *hold-a1* nil))
		       *hold-a2* *hold-a3* *hold-a4*))
	      (t (funcall *reader-fun*)))
      ;;(format t "~%gobble-token: type = ~s, token = ~s, place = ~s, comment = ~s"
      ;;   v1 v2 v3 v4)
      (when (eq v1 'sbst::ELSIF)
	(push v3 pvs::*elsif-places*))
      (setq *end-place* (list v1 v2 v3))
      (setq *last-newline-comment*
	    (nconc *last-newline-comment* v4))
      nil)))


;;; This function comes from rt-format.  I changed it so that extra
;;; unindents don't cause it to bomb.  SJP 5/91.

(defun set-tinfo-indent-stack (start end)
  (let ((ns (if (>= start 1)
		(tinfo-indent-stack (aref *tinfo* (1- start)))
		'(0))))
    (do ((i start (1+ i)))
	((> i end))
      (let* ((tinfo (aref *tinfo* i))
	     (bp (tinfo-bp tinfo)))
	(do ((formats (bp-format bp) (cdr formats)))
	    ((null formats))
	  (ecase (token-kind (car formats))
	    (:whitespace
	     (ecase (token-subkind (car formats))
	       (:cr
		())
	       (:indent
		;; Changed here --- SJP 5/27/91
		(push (+ (or (car ns) 0)
			 (indent-width (token-value (car formats))))
		      ns))
	       ((:unindent :untab)
		(pop ns))
	       (:tab-left
		(push (+ (car ns)
			 (- (tinfo-width tinfo)
			    (if (>= start 1)
				(tinfo-width (aref *tinfo* (1- start)))
				0)))
		      ns))
	       (:tab-right
		(push (+ (car ns)
			 (tinfo-bp-spaces tinfo)
			 (- (tinfo-width tinfo)
			    (if (>= start 1)
				(tinfo-width (aref *tinfo* (1- start)))
				0)))
		      ns))))))))
    (setf (tinfo-indent-stack (aref *tinfo* end))
	  ns)))

;;; From rt-term.lisp - set the place and comments.  This is the value of
;;; *apply-lex-term-constr-fun*, invoked by gettoken.

(defun apply-lexical-terminal-constructor (type token &optional place
						comment)
  (declare (ignore comment))
  (let ((lterm (ecase type
		 (sbst::!id! (mk-id token))
		 (sbst::!string! (mk-string token))
		 (sbst::!number! (mk-number token))
		 (sbst::!literal! (mk-literal token))     
		 (sbst::!keyword! (mk-keyword token)))))
    (cond ((and lterm
		(memq type '(sbst::!id! sbst::!literal! sbst::!number!
					sbst::!string!)))
	   (save-place-and-comment-info lterm nil place
					(pvs::get-end-place token nil place t))
	   (if *last-syntax*
	       (incf *num-keywords-skipped*))
	   (setq *last-syntax* lterm)
	   ;;(setq *last-newline-comment*
	   ;;	 (append *last-newline-comment* *hold-comments*))
	   (setq *hold-comments* nil)
	   ;;(format t "~%Found ~a, resetting num-comments" token)
	   ;;(when *last-newline-comment*
	     ;;(format t "~%*hold-comments* = ~a" *last-newline-comment*))
	   (setq *num-keywords-skipped* 0)
	   lterm)
	  (t lterm))))

(defvar *newline-comments* nil
  "Used by lex-newline-comment to return the latest comments to
   lexical-read.")

(defvar *holding-char* nil)

;;; From rt-structs.lisp - only difference is that it sets *end-of-last-token*

(defun lexical-read (self eofval &aux char place
			  (readtable (lexical-stream-readtable self)))
  (let ((*newline-comments* nil))
    ;; This loop goes until we hit a character for which the macro returns
    ;; some values.
    (setq *end-of-last-token* (curplace (lexical-stream-stream self)))
    (loop
     ;; Scan till we hit non-whitespace or eof
     (loop
      (if *holding-char*
	  (setq char *holding-char*)
	  (setq char (lexical-read-char self :eof)))
      (when (or (equal char :eof)
		(not (null (elt readtable (char-code char)))))
	(return)))
     (unless *holding-char*
       (lexical-unread-char self))
     (setq place (curplace (lexical-stream-stream self))
	   char (multiple-value-list
		 (cond ((equal char :eof) eofval)
		       ((equal (elt readtable (char-code char)) :alphabetic)
			(let ((a (alpha-lexer self
					      (lexical-read-char self nil))))
			  (if (and (symbolp a)
				   (memq (intern (string-upcase a) 'sbst)
					 '(sbst::TYPE
					   sbst::CONVERSION
					   sbst::AUTO_REWRITE)))
			      (let ((nch (lexical-read-char self nil)))
				(cond ((and nch (char= nch #\+))
				       (case (intern (string-upcase a) 'sbst)
					 (sbst::TYPE 'sbst::TYPE+)
					 (sbst::CONVERSION 'sbst::CONVERSION+)
					 (sbst::AUTO_REWRITE 'sbst::AUTO_REWRITE+)
					 (t (lexical-unread-char self)
					    a)))
				      ((and nch (char= nch #\-))
				       (case (intern (string-upcase a) 'sbst)
					 (sbst::CONVERSION 'sbst::CONVERSION-)
					 (sbst::AUTO_REWRITE 'sbst::AUTO_REWRITE-)
					 (t (lexical-unread-char self)
					    a)))
				      (t (lexical-unread-char self)
					 a)))
			      a)))
		       ((is-lexical-escape? self char)
			(lexical-read-char self nil)
			(let ((char (lexical-read-char self :eof)))
			  (if (equal char :eof)
			      eofval
			      (alpha-lexer self char))))
		       (t (funcall (elt readtable (char-code char))
				   self
				   (if *holding-char*
				       (prog1 *holding-char*
					 (setq *holding-char* nil))
				       (lexical-read-char self nil)))))))
     (when char (return)))
    (values (car char) place *newline-comments*)))

(defun alpha-lexer (stream char &aux 
			        (buffer (lexical-stream-stringbuffer stream)))
  (setf (fill-pointer buffer) 0)
  (vector-push-extend char buffer)
  (loop
   (setq char (lexical-read-char stream :eof))
   (cond ((equal char :eof)
	  (return))
	 ((is-lexical-escape? stream char)
	  (setq char (lexical-read-char stream :eof)))
	 ((not (member (elt (lexical-stream-readtable stream)
			    (char-code char)) '(:alphabetic :number)))
	  (return)))
   (if (not (equal char :eof))
       (vector-push-extend char buffer)))
  (lexical-unread-char stream)		; Not quoted. 
  (let ((str #-(or lucid harlequin-common-lisp) buffer
	     #+(or lucid harlequin-common-lisp)
	     (coerce buffer 'simple-string)))
    (multiple-value-bind (integer length)
	(parse-integer str :junk-allowed t)
      (cond ((and integer
		  (= length (length str)))
	     (if (char= (char str 0) #\0)
		 (intern str *sbst-package*)
		 integer))
	    (integer
	     (format t "Error: integer contains illegal characters.~%")
	     :illegal-token)
	  ;; Because of a bug in Lucid common lisp, we have to make the buffer
	  ;; into a simple-string before we can intern it.
	    (t
	     (intern (if *case-sensitive*
			 str
			 #+(and allegro (version>= 6)) (string-downcase str)
			 #-(and allegro (version>= 6)) (string-upcase str))
		     *sbst-package*))))))

(defun open-lexical-stream (stream &aux result)
  (setq result (make-lexical-stream :stream stream))
  (setf (lexical-stream-readtable result) (make-array `(,char-code-limit)
						    :initial-element :alphabetic)
	(lexical-stream-stringbuffer result)
	(make-array '(20) :element-type 'character
		    :adjustable t
		    :fill-pointer t))
  (dolist (xa '(#\space #\tab #\newline #\page #\return #\^z))
    (setf (elt (lexical-stream-readtable result) (char-code xa)) nil))
  (lexical-make-macro result #\" #'string-lexer)
  result)

(defun save-place-and-comment-info (term lterm splace &optional eplace)
  ;;(peek-first)			;to make sure that next token is
					;looked at.  Kludgy, but
					;sometimes needed, eg in
					;bind-decl, eg lambda a: stuff,
					;then after a, ':' hasn't been
					;peeked.
  (set-term-place term splace (or eplace *end-of-last-token*))
  (when (and *last-newline-comment* 
	     (is-leaf-term lterm))
    ;;(format t "~%~a gets comment ~s" lterm comment)
    (setf (getf (term:term-attr term) :comment)
	  *last-newline-comment*)
    (setq *last-newline-comment* nil))
  term)

(defun set-term-place (term splace eplace)
  (let ((sline (place-linenumber splace))
	(scol (place-charnumber splace))
	(eline (place-linenumber eplace))
	(ecol (place-charnumber eplace)))
;     (format t "~%Term = ~s~%  Place = ~d, ~d, ~d, ~d"
;       term sline scol eline ecol)
    (setf (getf (term:term-attr term) :place)
	  (vector sline scol eline ecol))))

;;; The following two functions replace things in rt-lex.lisp.  Comments
;;; are put into *last-newline-comment*, and from there into *last-syntax*

(defvar *last-syntax* nil 
    "the last pvs object that can be given a comment")

(defvar *last-newline-comment* nil
  "collect newline comments to be plucked by syntactic objects
   as they are created")

(defun lex-newline-comment (stream open-comment)
  (if (eq *abs-syn-package* (find-package :pvs))
      (let ((newline? (check-for-newline stream))
	    (comment (collect-newline-comment-chars stream nil)))
	;;(format t "~%lex-newline-comment: *collect-comments* = ~a, comment = ~s"
	;;  *collect-comments* comment)
	(when *collect-comments*
	  (setq *newline-comments*
		(append *newline-comments*
			(list (list comment *num-keywords-skipped*
				    newline?)))))
;; 	(if *collect-comments*
;; 	    (setq *hold-comments*
;; 		  (append *hold-comments* (list (list comment 0 newline?))))
;; 	    (setq *last-newline-comment*
;; 		  (append *last-newline-comment*
;; 			  (list (list comment *num-keywords-skipped*
;; 				      newline?)))))
	(values))
      (let ((*close-comment-char* #\newline))
	(lex-comment stream open-comment))))

(defun check-for-newline (stream)
  (let* ((place (curplace (lexical-stream-stream stream)))
	 (col (place-charnumber place))
	 (text (place-linetext place)))
    (not (find-if-not #'pvs::white-space text :end (1- col)))))
    

(defun collect-newline-comment-chars (stream chars)
  (let ((char (lexical-read-char stream #\newline)))
    (cond ((is-lexical-escape? stream char)
	   (collect-newline-comment-chars stream chars))
	  ((equal char #\newline)
	   (coerce (cons #\% (nreverse chars)) 'string))
	  (t (collect-newline-comment-chars stream (cons char chars))))))
  

(defun lex-comment (stream open-comment)
  (declare (ignore open-comment))
  (loop
   (let ((char (lexical-read-char stream *close-comment-char*)))
     (cond ((is-lexical-escape? stream char)
	    (lexical-read-char stream *close-comment-char*)) ; disgard.
	   ((equal char *close-comment-char*)
	    (return)))))
  (values))

;;; Replaces a function in rt-lex.lisp.  Have moved the comment lexical
;;; makes to the end, so that they override the operator lexical-makes,
;;; so that '-' and open paren force a jump to the comment handling routines.
(defun init-lexical-readtable (lexstream &key
					 single-char-op-list
					 new-line-comment-char
					 open-comment-char
					 escape-char
					 multi-char-op-list)
  (dolist (x single-char-op-list) (lexical-make-macro lexstream x #'lex-op))
  (dolist (pair multi-char-op-list)
    (let* ((char (car pair))
	   (fun (cdr pair)))
      (lexical-make-macro lexstream char (symbol-function fun))))
  (when new-line-comment-char
    (lexical-make-macro lexstream new-line-comment-char #'lex-newline-comment))
  (when open-comment-char
    (lexical-make-macro lexstream open-comment-char #'lex-comment))
  (when escape-char
    (lexical-make-escape lexstream escape-char)))


;;; Adding *hold-a4* and *hold-b4* to contain comments, so they don't get
;;; out of sync.

(defvar *hold-a4*)

(defvar *hold-b4*)

(defun values-a () (values *hold-a1* *hold-a2* *hold-a3* *hold-a4*))

(defun values-b () (values *hold-b1* *hold-b2* *hold-b3* *hold-b4*))


;;; Look ahead one token in the input stream.
(defun peek-first ()
  (cond (*hold-b1* (values-b))
	(*hold-a1* (values-a))
	(t (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3* *hold-a4*)
	     (funcall *reader-fun*))
	   (values-a))))

;;; Look ahead to the second token which is (or will be) in values-a; the
;;; first token is in values-b.
(defun peek-second ()
  (cond (*hold-b1* (values-a))
	(*hold-a1*
	 (setf *hold-b1* *hold-a1*
	       *hold-b2* *hold-a2*
	       *hold-b3* *hold-a3*
	       *hold-b4* *hold-a4*)
	 (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3* *hold-a4*)
	   (funcall *reader-fun*))
	 (values-a))
	(t (multiple-value-setq (*hold-b1* *hold-b2* *hold-b3* *hold-b4*)
	     (funcall *reader-fun*))
	   (multiple-value-setq (*hold-a1* *hold-a2* *hold-a3* *hold-a4*)
	     (funcall *reader-fun*))
	   (values-a))))

(defun initial-error (fs-list &aux (formatstr
				    "~&Initial error.~%~
				    Found ~A when looking for ~A here:~%~A"))
  (clet* (((first ignore place) (peek-first))
	  (temp (assoc first fs-list)))
	 (if (null temp)
	     (do-syntax-error formatstr
			      first
			      (parse-error-list-string
			       (mapcar #'(lambda (x) (car x)) fs-list))
			      place)
	     (multiple-value-bind (second name place) (peek-second)
	       (declare (ignore name))
	       (do-syntax-error formatstr
				second
				(parse-error-list-string (cdr temp))
				place)))))

(defun lam-error (fs-list)
  (multiple-value-bind (type name place) (peek-first)
    (declare (ignore name))
    (do-syntax-error "Look ahead set match error.~%~
		     Missing ~A inserted here:~%~A~%Bad token is ~A~%"
		     (caar fs-list) type place)))

(defvar *sbst-infix-operators*
  (mapcar #'(lambda (s) (intern (string s) :sbst))
	  pvs::*infix-operators*))

(defvar *sbst-unary-operators*
  (mapcar #'(lambda (s) (intern (string s) :sbst))
	  pvs::*unary-operators*))

(defun parse-error-list-string (list)
  (let* ((binops? (every #'(lambda (e) (memq e list)) *sbst-infix-operators*))
	 (unops? (every #'(lambda (e) (memq e list)) *sbst-unary-operators*))
	 (nlist (remove-if #'(lambda (x)
			       (or (and binops?
					(memq x *sbst-infix-operators*))
				   (and unops?
					(memq x *sbst-unary-operators*))))
		  list)))
    (orify (substitute '|identifier| 'sbst::!ID!
		       (cond ((and binops? unops?)
			      (cons '|opsym| nlist))
			     (binops?
			      (cons '|binop| nlist))
			     (unops?
			      (cons '|unaryop| nlist))
			     (t nlist))))))

(defun string-lexer (stream char &aux (buffer (lexical-stream-stringbuffer
					       stream))
			    newchar)
  (setf (fill-pointer buffer) 0)
  (let ((place (curplace (lexical-stream-stream stream))))
    (loop
     (setq newchar (lexical-read-char stream :eof))
     (cond ((equal newchar :eof)
	    (return))
	   ((is-lexical-escape? stream newchar)
	    (setq newchar (lexical-read-char stream :eof)))
	   ((or (equal newchar :eof)
		(equal newchar char))
	    (return)))
     (vector-push-extend newchar buffer))
    (when (equal newchar :eof)
      (decf (place-charnumber place))
      (do-syntax-error
       "There is no ending quote for this string"
       place))
    #+lucid (coerce (copy-seq buffer) 'simple-string)
    #-lucid (copy-seq buffer)))
