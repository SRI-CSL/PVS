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

(defparameter *unary-operators* '(NOT - ~ [] <>))

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
    (when (eq sim-op 'TERM-EXPR)
      (reset-operator-place args))
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
				   (place< (cdr oplace) arg2-place)))
		   *operator-places*)))
    (assert oplace)
    (setq *operator-places* (delete oplace *operator-places*))
    (setf (getf (term:term-attr (car args)) :place) (cdr oplace))))

(defun place< (place1 place2)
  (or (< (svref place1 2) (svref place2 0))
      (and (= (svref place1 2) (svref place2 0))
	   (<= (svref place1 3) (svref place2 1)))))
       
    
(defun reader ()
  (multiple-value-bind (token place comment)
      (lexical-read *lexical-stream* :eof)
    ;; (format t "~%reader: token = ~s, place = ~s, comment = ~s"
    ;;   token place comment)
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

(in-package 'sbrt)

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
    ;; (format t "~%gettoken: type = ~s, token = ~s, place = ~s, comment = ~s"
    ;;   type token place comment)
    (setq *end-place* (list type token place))
    (setq *last-newline-comment* (nconc *last-newline-comment* comment))
    ;; (format t "~%gettoken comment = ~s" comment)
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
      ;; (format t "~%gobble-token: type = ~s, token = ~s, place = ~s, comment = ~s"
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
		;; (append *last-newline-comment* *hold-comments*))
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
      (setq char (lexical-read-char self :eof))
      (when (or (equal char :eof)
		(not (null (elt readtable (char-code char)))))
	(return)))
     (lexical-unread-char self)
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
				   (lexical-read-char self nil))))))
     (when char (return)))
    (values (car char) place *newline-comments*)))


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
    ;; (format t "~%~a gets comment ~s" lterm comment)
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
  (if (eq *abs-syn-package* (find-package 'pvs))
      (let ((newline? (check-for-newline stream))
	    (comment (collect-newline-comment-chars stream nil)))
	(when *collect-comments*
	  (setq *newline-comments*
		(append *newline-comments*
			(list (list comment *num-keywords-skipped*
				    newline?)))))
; 	(if *collect-comments*
; 	    (setq *hold-comments*
; 		  (append *hold-comments* (list (list comment 0 newline?))))
; 	    (setq *last-newline-comment*
; 		  (append *last-newline-comment*
; 			  (list (list comment *num-keywords-skipped*
; 				      newline?)))))
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


;;; UNPARSE: The rest of the functions in this file are for unparsing

;;; Replaces a function in rt-format.lisp, which does printing of output
;;; according to formatting that has been done earlier.  Whenever an pvs
;;; comment is seen it is stored on a list, and output when a whitespace
;;; including a cr is printed.  %-style comments are printed before the
;;; first cr.

(defvar *newline-comments-to-output* nil "list of %-style pvs comments
  that haven't been printed yet")

(defvar *white-space-up-to-last-newline* (make-string-output-stream)
"keep whitespace that has been output, so comments can be inserted therein")

(defvar *white-space-since-last-newline* (make-string-output-stream)
  "keep whitespace on current line, in case a comment needs to be inserted")

(defvar *newline-printed* nil)

(defun compute-octs-and-string (aw)
  (let ((*print-pretty* nil)		; To avoid weirdness.
	(*indent* '(0))
	(*s* (make-string-output-stream)))
    ;; Initialise buffers used for outputting comments in good places:
    (get-output-stream-string *white-space-up-to-last-newline*)
    (get-output-stream-string *white-space-since-last-newline*)
    (setq *newline-comments-to-output* nil)
    (setq *num-keywords-skipped* -1)
    (setq *last-syntax* nil)
    (compute-octs-and-string-aux aw 0 0 0 nil nil)
    (setf (aw-outstring aw) (get-output-stream-string *s*))))

(defun print-buffered-whitespace-and-comments ()
  (when *newline-comments-to-output*
    (unless pvs::*no-comments*
      ;;(format t "~%skipping ~d, comments = ~s"
	;;*num-keywords-skipped* *newline-comments-to-output*)
      (let ((first? t))
	(dolist (cmt *newline-comments-to-output*)
	  (when (= (second cmt) *num-keywords-skipped*)
	    ;;(let ((indent (floor (car *indent*))))
	      (format *s* "~V%~V,0@T~a~%~V,0@T"
		(if (or (third cmt) (not first?)) 0 0)
		(if (= (second cmt) -1) 0 2)
		(first cmt)
		0)
	      ;;) ;was indent
	    ;;(setq *newline-printed* t)
	    (setq *newline-comments-to-output*
		  (remove cmt *newline-comments-to-output*))))))
    ;;(setq *newline-comments-to-output* nil)
    )
  (let ((out (get-output-stream-string *white-space-up-to-last-newline*)))
    (unless (string= out "")
      (princ out *s*)))
  (let ((out (get-output-stream-string *white-space-since-last-newline*)))
    (unless (string= out "")
      (princ out *s*))))

;;; In the following, ;++ indicates added code, ;-- indicates changed code.

(defun compute-octs-and-string-aux (aw leftx topy topdent
				    more-on-topline? more-on-botline?)
  (declare (ignore leftx))
  ;;(assert (>= topdent leftx))
  (let ((x topdent)  
	(y topy)
	;;(s (make-string-output-stream))
	(minx topdent)
	(maxx 0)
	(height 1)
	(term (aw-term aw)))					   ;++
    (when (getf (term:term-attr term) :comment)		   ;++
      (let ((cmt (getf (term:term-attr term) :comment)))    ;++
	(setq *newline-comments-to-output*			   ;++
	      cmt
	      ;;(append *newline-comments-to-output* cmt)
	      )))	   ;++
    (dolist (son (aw-sons-form aw))				   ;++
      (when (and (token-p son)					   ;++
		 (memq (token-kind son) '(:keyword :lt))	   ;++
		 (memq (token-subkind son) '(:id :identifier)))  ;++
	(collect-comments term)
	))				   ;++
    (print-buffered-whitespace-and-comments)			   ;++

    (do ((sons (aw-sons-form aw) (cdr sons))
	 (rev-sons nil (cons (car sons) rev-sons)))
	((null sons) aw)
      (if (token-p (car sons))
	  (let ((token (car sons)))
	    ;;(assert (token-p token))
	    ;;(format t " ~A ~A ~A ~%" (token-kind token)
	    ;;      (token-subkind token) *indent*)
	    (ecase (token-kind token)
	      ((:keyword :lt)
	       (print-buffered-whitespace-and-comments)
	       (setf x (+ x (token-width token)))
	       ;;(when (member (token-subkind token) '(:id :identifier)) ;++
	       ;;  (collect-comments term))			   ;++
	       (ecase (token-subkind token)
		 ((:id :identifier nil)	; nil for keyword
		  (let ((str (if (token-str-value token)
				  (token-str-value token)
				  (symbol-name (token-value token)))))
		    (cond ((memq (token-subkind token) '(:id :identifier))
			   (let ((*num-keywords-skipped* -1))
			     (print-buffered-whitespace-and-comments))
			   (setq *num-keywords-skipped* 0))
			  (t (incf *num-keywords-skipped*)))
		    (princ (if (string-equal str "O")
			       "o"
			       (if *case-sensitive* ;--
				   str	;--
				   (case *print-case* ;--
				     (:downcase (string-downcase str)) ;--
				     (:upcase (string-upcase str)) ;--
				     (:capitalize (string-capitalize str)) ;--
				     (t str)))) ;--
			   *s*)
		    (print-buffered-whitespace-and-comments)	   ;++
		    ))
		 ((:string :number)
		  (print-buffered-whitespace-and-comments)	   ;++
		  (princ (if (token-str-value token)
			     (token-str-value token)
			     (token-value token))
			 *s*))
		 (:jux ())))

	      (:whitespace
	       (print-buffered-whitespace-and-comments)		   ;++
	       (ecase (token-subkind token)
		 (:sp (let* ((spaces (sb-pixels-to-chars (token-value token)))
			     (space (sb-chars-to-pixels spaces))) ; exact
			(setf x (+ x space))
			(do ((i spaces (1- i)))
			    ((<= i 0))
			  (write-char #\space			   ;--
				      *white-space-since-last-newline*)))) ;--
		 (:cr (let ((act-indent (sb-chars-to-pixels
					 (sb-pixels-to-chars (car *indent*)))))
			(setf maxx (max maxx x))
			(setf y (+ y (sb-lines-to-pixels 1)))
			(setf x act-indent)
			(setf minx (min minx x))
			(incf height)
			(if *newline-printed*
			    (setq *newline-printed* nil)
			    (terpri *white-space-since-last-newline*))  ;++
			(let ((out (get-output-stream-string
				    *white-space-since-last-newline*)))
			  (unless (string= out "")
			    (princ out
				   *white-space-up-to-last-newline*)))
			(do ((i (sb-pixels-to-chars act-indent) (1- i)))
			    ((<= i 0))
			  (write-char #\space			   ;--
				      *white-space-since-last-newline*)))) ;++
		 (:indent ;;(format t " indenting ~A ~A - ~A ~%"
			  ;;  token (indent-width (token-value token)) *indent*)
			  (push (+ (car *indent*)
				   (indent-width (token-value token)))
				*indent*))
		 (:unindent (if (cdr *indent*)
				(pop *indent*)))
		 (:tab-left (push x *indent*))
		 (:tab-right (push x *indent*))	; already ordered properly
					; with whitespace
		 (:untab (if (cdr *indent*)
			     (pop *indent*)))))))
	    
	  ;;  (aw-p (car sons))
	  (let ((aw (car sons))
		(new-leftx (car *indent*)))
	    (compute-octs-and-string-aux aw (min x new-leftx) y x
					 (is-more-on-line? more-on-topline?
							   rev-sons)
					 (is-more-on-line? more-on-botline?
							   (cdr sons)))
	    (setf maxx (max maxx
			    (rightx (aw-oct aw))))
	    ;;(write-string (aw-outstring aw) s)  ; for old 1 string per aw
	    (setf y (- (boty (aw-oct aw))
		       (sb-lines-to-pixels 1)))
	    (setf x (botdent (aw-oct aw))))))

    (setf maxx (max maxx x))
    ;;(setf (aw-outstring aw) (get-output-stream-string s))

    (setf (aw-oct aw)
	  (make-oct :leftx 	 minx
		    :topy 	 topy
		    :rightx	 maxx
		    :boty	 (+ y (sb-lines-to-pixels 1))
		    :topdent     (if (not more-on-topline?) minx topdent)
		    :botdent     (if (not more-on-botline?) maxx x)
		    ))))

(defun collect-comments (term)
  (dolist (sterm (term-args term))
    (let ((cmt (getf (term:term-attr sterm) :comment)))
      (when cmt
	(setq *newline-comments-to-output*
	      (append *newline-comments-to-output* cmt))))
    (collect-comments sterm)))

(defun show-comments (term)
  (let ((cmt (getf (term:term-attr term) :comment)))
    (when cmt
      (format t "~%~s - ~s" term cmt))
    (mapc #'show-comments (term-args term))
    nil))

;;; These two macros come from rt-unparse.lisp.  I have taken out the
;;; flattening feature.  This feature essentially removes single
;;; productions from the intermediate abstract syntax, by leaving out
;;; uterms with only one child.  This causes problems if the left out
;;; uterm contained a comment. 
;;; A more elegant solution would put the comment into the as or uterm
;;; above, so that the formatter would catch it again.
;;; This change should be evaluated for efficiency effects.
;;; 2/8/91: flattening only avoided when lower term is a syntactic object.

;(defvar *flatten-uterms* nil)		; Check texify macro if this changes
;(defvar *flatten-new-uterms* nil)	; Check texify macro if this changes

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
	      (queue-uterm-son length-ellipsis-token)))
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
	      (queue-uterm-son length-ellipsis-token)))
       (setf ,each-iter-slot (car rest))
       ,@body
       (queue-uterm-bp (make-spec-bp ,bp1-ws))
       (cond ((cdr rest)
	      ,@key-body
	      (queue-uterm-bp (make-spec-bp ,bp2-ws))))
       (incf *current-print-length*))))

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
	    (queue-uterm-son depth-ellipsis-token)
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
			(not (typep (uterm-term (car (uterm-sons *uterm*)))
				    'pvs::syntax))
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
		  (not (typep (uterm-term (car (uterm-sons new-uterm)))
			      'pvs::syntax))
		  (not (eq (uterm-kind new-uterm) :rept)))
	     (setf (uterm-term (car (uterm-sons new-uterm)))
		   ,as)
	     (car (uterm-sons new-uterm)))
	    (t
	     new-uterm)))))


(defun memo-uterm (term unp-function top-level?)
  (if (or *disable-caching*
	  (and *disable-nested-caching*
	       (null top-level?)))
      (funcall unp-function term)
      (newattr::get-gsyn theuterm
			 term
			 (list unp-function
			       *unparse-style*
			       *no-escapes*
			       *sb-print-depth*
			       *sb-print-length*
			       *formatting-off*))))

(defun should-break-intv? (start end)
  (let ((tend (tinfo-width (aref *tinfo* end)))
	(tstart (if (plusp start)
		    (tinfo-width (aref *tinfo* (1- start)))
		    0))
	(tindent (if (plusp start)
		     (or (car (tinfo-indent-stack (aref *tinfo* (1- start))))
			 0)
		     0))
	(slop 20))
;    (format t "~%tend = ~d, tstart = ~d, diff = ~d, tindent = ~d, break? = ~a"
;      tend tstart (- tend tstart) tindent
;      (and (> (- tend tstart) slop)
;	   (> (+ (- tend tstart) tindent) *allowed-width*)))
    (and (> (- tend tstart) slop);; SO - added this for slop
	 (> (+ (- tend tstart) tindent) *allowed-width*))))

(defun assign-between-token-whitespace ()
  (do ((i 0 (1+ i)))
      ((= i (1- (fill-pointer *tinfo*))))
    (let* ((tinfo (aref *tinfo* i))
	   (son (tinfo-son tinfo))
	   (bp (tinfo-bp tinfo))
	   (next-tinfo (aref *tinfo* (1+ i)))
	   (next-son (tinfo-son next-tinfo)))
      (assert (and (token-p son)
		   (token-p next-son)))
      (if (null (bp-spaces bp))
	  (let ((spaces (determine-between-token-spaces
			 son
			 next-son
			 (uterm-name (tinfo-uterm tinfo))
			 (uterm-name (tinfo-uterm next-tinfo)))))
	    (cond ((= spaces 0)
		   (setf (bp-spaces bp) 0))
		  ((< spaces 0)		     ; represent cr's from
		   (setf (bp-spaces bp) 0)   ; lang-special-spaces
		   (setf (bp-crs bp) (abs spaces))
		   (setf (bp-value bp) most-negative-fixnum))
		  ((> spaces 0)
		   (setf (bp-spaces bp) spaces))))
	  (setf (bp-spaces bp) (sb-chars-to-pixels (bp-spaces bp)))) 
					; Up till now, has been char spaces.

      (if (zerop (bp-spaces bp))
	  (setf (bp-value bp)
		(+ (bp-value bp) 
		   (* *zero-space-bp* nesting-constant))))
					; above is experiment, don't want to
					; break where no white space is
					; required . 

      (if (null (bp-crs bp))
	  ;;; Owre 9/12/92 - test before setting it to 1
	  (setf (bp-crs bp) (or (pvs-special-default-spaces son next-son) 1)))
      (insert-default-ws tinfo)))

  (let* ((tinfo (aref *tinfo* (1- (fill-pointer *tinfo*))))
	 (bp (tinfo-bp tinfo)))
    (if (null (bp-spaces bp))
	(setf (bp-spaces bp) 0))
    (if (null (bp-crs bp))
	(setf (bp-crs bp) 0))
    (insert-default-ws tinfo)))

(defun pvs-special-default-spaces (token1 token2)
  (cond ((and (eq (token-kind token2) :keyword)
	      (memq (token-value token2) *unparser-op-list*)
	      (memq (token-value token2)
		    '( SBST::|,|
		      SBST::|;|
		      SBST::|:|
		      SBST::|.|
		      SBST::|)|
		      SBST::|]|
		      SBST::|}| )))
	 0)
	((and (eq (token-kind token1) :keyword)
	      (memq (token-value token1) *unparser-op-list*)
	      (memq (token-value token1)
		    '(SBST::|(|
		      SBST::|[|
		      SBST::|{| )))
	 0)
        ((eq (get-token-space-kind token1) :jux)
	 0)
	((and (not (eq (token-kind token1) :keyword))
	      (eq (token-kind token2) :keyword)
	      (memq (token-value token2) *unparser-op-list*)
	      (memq (token-value token2)
		    '(SBST::|(| 
		      SBST::|[| )))
	 0)))


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
