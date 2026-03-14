;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-parser-runtime-fixes.lisp --
;;   changes to core ESS functions needed at runtime,
;;   primarily to keep track of places and comments
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2026, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :sbrt)

(export '( *hold-a4* *hold-b4*))

(defvar *end-place* nil)


;;; SBRT Parameters

;;; Turn off ergo auto-bracketing, otherwise ergo has an error when unparsing
;;; complicated expressions.  This variable is set to null while unparsing ada
;;; abstract syntax, as auto-bracketing is used there.

;;(setq sbrt:*parens-off* t)

;;; Set *rept-indent-units* to 0, otherwise first element of a rept is
;;; indented different from the rest (I don't know why).

;;(setq sbrt::*rept-indent-units* 0)

;;; Turn off indentation increasing with depth of nonterminals in the grammar
;(setq sbrt::*uterm-indent-units* 0)
;; testing on 9/18/90, if doesn't work then revert to above line:

(setq sbrt::indent-unit 0)


;;; Sets *end-of-last-token* to the place in the lexical stream before
;;; stripping off white space. 

(defvar *end-of-last-token* nil "place where reading of the
  last token ended, points to beginning of white space before current
  first token in token stream")

(defvar *hold-comments* nil
  "A place to keep comments until they can be processed")

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
    (when (and comment
	       (boundp 'pvs:*newline-comments*))
      ;; (format t "~%gettoken comment = ~s" comment)
      (push comment pvs:*newline-comments*))
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
    (when (and v4
	       (boundp 'pvs:*newline-comments*))
      ;; (format t "~%gettoken comment v4 = ~s" v4)
      (push v4 pvs:*newline-comments*))
    nil))


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
		(member type '(sbst::!id! sbst::!literal! sbst::!number! sbst::!string!)
			:test #'eq))
	   (save-place-and-comment-info lterm place
					(pvs::get-end-place token nil place t))
	   (setq *hold-comments* nil)
	   ;;(format t "~%Found ~a, resetting num-comments" token)
	   lterm)
	  (t lterm))))

(defvar *holding-char* nil)

;;; From rt-structs.lisp - only difference is that it sets *end-of-last-token*

(defun lexical-read (self eofval &aux char place
				   (readtable (lexical-stream-readtable self)))
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
		     ((eq (elt readtable (char-code char)) :alphabetic)
		      (let ((a (alpha-lexer self (lexical-read-char self nil))))
			(if (and (symbolp a)
				 (member (intern (string-upcase a) :sbst)
					 '(sbst::TYPE
					   sbst::CONVERSION
					   sbst::AUTO_REWRITE)
					 :test #'eq))
			    (let ((nch (lexical-read-char self nil)))
			      (cond ((and nch (char= nch #\+))
				     (case (intern (string-upcase a) :sbst)
				       (sbst::TYPE 'sbst::TYPE+)
				       (sbst::CONVERSION 'sbst::CONVERSION+)
				       (sbst::AUTO_REWRITE 'sbst::AUTO_REWRITE+)
				       (t (lexical-unread-char self)
					  a)))
				    ((and nch (char= nch #\-))
				     (case (intern (string-upcase a) :sbst)
				       (sbst::CONVERSION 'sbst::CONVERSION-)
				       (sbst::AUTO_REWRITE 'sbst::AUTO_REWRITE-)
				       (t (lexical-unread-char self)
					  a)))
				    (t (lexical-unread-char self)
				       a)))
			    a)))
		     ((is-lexical-escape? self char) ;; Currently no char satisfies this
		      (lexical-read-char self nil)
		      (let ((char (lexical-read-char self :eof)))
			(if (equal char :eof)
			    eofval
			    (alpha-lexer self char))))
		     (t (let ((rtfun (elt readtable (char-code char))))
			  (funcall rtfun
				   self
				   (if *holding-char*
				       (prog1 *holding-char*
					 (setq *holding-char* nil))
				       (lexical-read-char self nil))))))))
   (when char (return)))
  (values (car char) place))

(defun alpha-lexer (stream char &aux 
			          (buffer (lexical-stream-stringbuffer stream)))
  (setf (fill-pointer buffer) 0)
  (vector-push-extend char buffer)
  (loop
   (setq char (lexical-read-char stream :eof))
   (cond ((equal char :eof)
	  (return))
	 ((is-lexical-escape? stream char) ;; Never true in PVS
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
	    ((and integer
		  (= length 1)
		  (char= (char str 0) #\0)
		  (member (char str 1) '(#\x #\X #\o #\O #\b #\B) :test #'char=))
	     (let* ((radix (case (char str 1)
			     ((#\x #\X) 16)
			     ((#\o #\O) 8)
			     ((#\b #\B) 2)
			     (t 10))) ;; Added to satisfy SBCL
		    (len (nth-value 1
			   (parse-integer str :junk-allowed t :radix radix :start 2))))
	       (cond ((and integer
			   (= len (length str)))
		      (intern str *sbst-package*))
		     (t (format t "Error: integer contains illegal characters.~%")
			:illegal-token))))
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

(defun save-place-and-comment-info (term splace &optional eplace)
  ;;(peek-first)			;to make sure that next token is
					;looked at.  Kludgy, but
					;sometimes needed, eg in
					;bind-decl, eg lambda a: stuff,
					;then after a, ':' hasn't been
					;peeked.
  (set-term-place term splace (or eplace *end-of-last-token*))
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
;;; are put into *newline-comments*.

(defun lex-newline-comment (stream open-comment)
  (if (eq *abs-syn-package* (find-package :pvs))
      (let (;;(newline? (check-for-newline stream))
	    (buffer (lexical-stream-stringbuffer stream)))
	(setf (fill-pointer buffer) 0)
	(vector-push-extend #\% buffer)
	(multiple-value-bind (comment place)
	    (collect-newline-comment-chars stream buffer)
	  ;;(format t "~%lex-newline-comment: ~s at ~s" comment place)
	  (when (boundp 'pvs:*newline-comments*)
	    (push (list comment place) pvs:*newline-comments*)))
	(values))
      (let ((*close-comment-char* #\newline))
	(lex-comment stream open-comment))))

(defun check-for-newline (stream)
  (let* ((place (curplace (lexical-stream-stream stream)))
	 (col (place-charnumber place))
	 (text (place-linetext place)))
    (not (find-if-not #'pvs::white-space text :end (1- col)))))
    

(defun collect-newline-comment-chars (stream buffer)
  (let* ((splace (curplace (lexical-stream-stream stream)))
	 (line (place-linenumber splace))
	 (scol (1- (place-charnumber splace)))
	 (comment (collect-newline-comment-chars* stream splace buffer))
	 ;; Don't want to unread, instead we use length of the lastline
	 (ecol (length (placestream-lastline (lexical-stream-stream stream)))))
    (values comment (vector line scol line ecol))))

(defun collect-newline-comment-chars* (stream splace buffer)
  (let ((char (lexical-read-char stream #\newline)))
    (cond ((is-lexical-escape? stream char)
	   (collect-newline-comment-chars* stream splace buffer))
	  ((equal char #\newline)
	   (copy-seq buffer))
	  (t (vector-push-extend char buffer)
	     (collect-newline-comment-chars* stream splace buffer)))))
  

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
  (let* ((peek (multiple-value-list (peek-first)))
	 (first (nth 0 peek))
	 (place (nth 2 peek))
	 (temp (assoc first fs-list)))
    (if (null temp)
	(do-syntax-error formatstr
	  first
	  (parse-error-list-string
	   (if (> (length fs-list) 100)
	       (list 'EXPR)
	       (mapcar #'(lambda (x) (car x)) fs-list)))
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
  (let* ((binops? (every #'(lambda (e) (member e list)) *sbst-infix-operators*))
	 (unops? (every #'(lambda (e) (member e list)) *sbst-unary-operators*))
	 (nlist (remove-if #'(lambda (x)
			       (or (and binops?
					(member x *sbst-infix-operators*))
				   (and unops?
					(member x *sbst-unary-operators*))))
		  list)))
    (orify (substitute '|identifier| 'sbst::!ID!
		       (cond ((and binops? unops?)
			      (cons '|opsym| nlist))
			     (binops?
			      (cons '|binop| nlist))
			     (unops?
			      (cons '|unaryop| nlist))
			     (t nlist))))))

(defun string-lexer (stream char)
  ;; Simple rule - char is #\" or #\', keep all chars until another same one found,
  ;; unless preceded by a #\\
  (let ((place (curplace (lexical-stream-stream stream)))
	(buffer (lexical-stream-stringbuffer stream))
	(newchar (lexical-read-char stream :eof)))
    (setf (fill-pointer buffer) 0)
    (loop while (not (or (eql newchar char) (eql newchar :eof)))
	  do (cond ((eql newchar #\\)
		    (let ((nextchar (lexical-read-char stream :eof)))
		      (cond ((eql nextchar char)
			     (vector-push-extend char buffer)
			     (setq pvs:*last-escaped-quote-place*
				   (curplace (lexical-stream-stream stream)))
			     (setq newchar (lexical-read-char stream :eof)))
			    ((eql nextchar #\\)
			     (vector-push-extend #\\ buffer)
			     (if (char= char #\')
				 (setq newchar (lexical-read-char stream :eof))
				 (setq newchar nextchar)))
			    (t
			     (vector-push-extend #\\ buffer)
			     (vector-push-extend nextchar buffer)
			     (setq newchar (lexical-read-char stream :eof))))))
		   (t (vector-push-extend newchar buffer)
		      (setq newchar (lexical-read-char stream :eof)))))
    (when (equal newchar :eof)
      (decf (place-charnumber place))
      (do-syntax-error
	  "There is no ending quote for this string"
	place))
    (copy-seq buffer)))
