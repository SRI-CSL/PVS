;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-structs.lisp	1.11 9/27/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.

;;; Structure declarations for unparsing and formatting to windows
;;; 
;;; The declarations correspond closely to the "semantic domains"-part
;;; of the specification and should be compared with these
;;;
;;; New version 31 Aug 85  (Ulrik)
;;; Lexical-stream defstruct added by Tim Freeman, April 1986
;;; 
;;; Took out memq, since it was also defined elsewhere.  Now centralized.
;;; fp, Mon Jan  2 11:03:45 1989

(in-package "SB-RUNTIME") (use-package :ergolisp)

(export '(open-lexical-stream
	  open-placestream lexical-read lexical-clear-input
	  lexical-make-macro lexical-make-escape lexical-read-char
	  string-lexer is-lexical-escape?
	  lexical-unread-char placestream))

(defvar *sbst-package*)

(defvar *case-sensitive*)		; defined elsewhere. 

;;; Fix to make sure memq is defined.
;;; Now centralized.  fp.
;;; (defun memq (item list) (member item list :test #'eq))

;;; For now, make a lexical stream which is equivalent to the old lexical
;;; streams.  The user may not specify a readtable; the readtable must default.
;;; stream should be something for which the curplace, place-read-char, and 
;;; place-unread-char operators work
(defstruct (lexical-stream (:print-function
			   (lambda (ps stream depth)
			     (declare (ignore depth))
			     (format stream "<lexical-stream>"))))
  stream
  readtable
  stringbuffer)

(defun open-lexical-stream (stream &aux result)
  (setq result (make-lexical-stream :stream stream))
  (setf (lexical-stream-readtable result) (make-array `(,char-code-limit)
						    :initial-element :alphabetic)
	(lexical-stream-stringbuffer result)
	(make-array '(20) :element-type 'character
		    :adjustable t
		    :fill-pointer t))
  (dolist (xa '(#\space #\tab #\newline #\page))
    (setf (elt (lexical-stream-readtable result) (char-code xa)) nil))
  (lexical-make-macro result #\" #'string-lexer)
  result)

(defun lexical-unread-char (self)
  (place-unread-char (lexical-stream-stream self)))

(defun lexical-read-char (self eofval)
  (place-read-char (lexical-stream-stream self) eofval))

;;; Do a read operation on a lexical stream.  Two values returned:
;;; first is the token or whatever read in, or the eof value
;;; second is the place at which the token was found.
;;;
;;; srd revised, escape-character added.
;;;
(defun lexical-read (self eofval &aux char place
			  (readtable (lexical-stream-readtable self)))
  ;; This loop goes until we hit a character for which the macro returns
  ;; some values.
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
		      (alpha-lexer self (lexical-read-char self nil)))
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
  (values (car char) place))

;;; Read a symbol or a number from the stream.
;;;
;;; srd revised, escape-character added.
;;;
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
   
  (multiple-value-bind (integer length)
      (parse-integer #+(or lucid harlequin-common-lisp)
		     (coerce buffer 'simple-string)
		     #-(or lucid harlequin-common-lisp) buffer :junk-allowed t)
    (cond ((and integer
		(= length (length buffer)))
	   integer)
	  (integer
	   (format t "Error: integer contains illegal characters.~%")
	   :illegal-token)
	  ;; Because of a bug in Lucid common lisp, we have to make the buffer
	  ;; into a simple-string before we can intern it.
	  (t
	   (let ((str #- (or lucid harlequin-common-lisp) buffer
		      #+(or lucid harlequin-common-lisp)
		      (coerce buffer 'simple-string)))
	     (intern (if *case-sensitive*
			 str
			 #+(and allegro (version>= 6)) (string-downcase str)
			 #-(and allegro (version>= 6)) (string-upcase str))
		     *sbst-package*))))))



;;; Read a quoted string from the stream.  Presently, there is no way to put
;;; a quote character in a quoted string.  Oh well.
;;;
;;; srd revised, escape-character added.
;;;
(defun string-lexer (stream char &aux (buffer (lexical-stream-stringbuffer
					       stream))
			    newchar)
  (setf (fill-pointer buffer) 0)
  (loop
   (setq newchar (lexical-read-char stream :eof))
   (cond ((is-lexical-escape? stream newchar)
	  (setq newchar (lexical-read-char stream :eof)))
	 ((or (equal newchar :eof)
	      (equal newchar char))
	  (return)))
   (vector-push-extend newchar buffer))
  (when (equal newchar :eof)
    (error "Lexical stream encountered eof in the middle of a string!"))
  #+lucid (coerce (copy-seq buffer) 'simple-string)
  #-lucid (copy-seq buffer))




;;; Do a clear-input operation on a lexical stream.
(defun lexical-clear-input (stream)
  (place-clear-input (lexical-stream-stream stream)))

;;; This routine conceptually does a set-macro-character operation on the 
;;; readtable built into the given lexical stream.  Only terminating
;;; macro characters may be defined.
(defun lexical-make-macro (self char function)
  (setf (elt (lexical-stream-readtable self) (char-code char)) function))

;;; Define an escape character for a lexical stream, like "\" is for the
;;; standard readtable.  Not really implemented yet.
;;;  ... and now more implemented. 
(defun lexical-make-escape (self char)
  (setf (elt (lexical-stream-readtable self) (char-code char)) :escape))
(defun is-lexical-escape? (self char)
  (eq :escape (elt (lexical-stream-readtable self) (char-code char))))

;;; A place in a line.
(defstruct (place (:print-function place-printer))
	    (linetext "")
	    (linenumber 0)
	    (charnumber 0))

;;; Print out a place.  This would be nicer if ~& worked properly.
(defun place-printer (place stream level)
  (declare (ignore level))
  (if *print-escape*
      (print-struct place stream place linetext linenumber charnumber)
      (format stream
	  ;; This is a workaround for a bug in Lucid's format statement.
	  #-lucid "~&Line ~d:~12T~A~%~
	  ~VT^~%"
	  #+lucid "~&Line ~d:~12T~A~% ~
	  ~VT^~%"
	  (place-linenumber place)
	  (substitute #\space #\tab (place-linetext place))
	  (+ (place-charnumber place) 12))))

;;; Define a stream for which curplace works.  User is only allowed to 
;;; initialize the stream field.
(defstruct (placestream (:print-function
			   (lambda (ps stream depth)
			     (declare (ignore depth))
			     (format stream "<placestream>"))))
	    stream
	    (linetext "")
	    (linenumber 0)
	    (linelength 0)
	    (eof-at-eolp nil) ; True iff the last character of this line is
	                      ; the last character of the file.
	    (charnumber 0)
	    (unreadp nil)     ; True if the last operation was an 
                              ; place-unread-char
	    (lastch nil)      ; The last character read, or nil if we don't 
                              ; know.
	    (lastline nil)) 

;;; Make sure that the first line is read in when one of these is created.
(defun open-placestream (stream &aux self)
  (setq self (make-placestream :stream stream))
  (place-read-char self nil)
  self)

;;; This routine is the same as the system routine read-char, except it works on
;;; placestreams and some useless arguments (in this context) are omitted.
(defun place-read-char (self eof-value &aux linetext eof-at-eolp)
  (if (placestream-unreadp self)
      (setf (placestream-unreadp self) nil)
      (setf (placestream-lastch self)
	    (cond ((and (= (placestream-charnumber self)
			   (placestream-linelength self))
			(placestream-eof-at-eolp self))
		   :eof)
		  ((= (placestream-charnumber self)
		      (placestream-linelength self))
		   (setf (placestream-lastline self)
			 (placestream-linetext self))
		   (multiple-value-setq (linetext eof-at-eolp)
		     (read-line (placestream-stream self) nil :eof nil))
		   (when (equal linetext :eof)
		     (setq linetext ""
			   eof-at-eolp t))
		   (setf (placestream-linetext self) linetext
			 (placestream-eof-at-eolp self) eof-at-eolp
			 (placestream-linelength self) (length linetext)
			 (placestream-charnumber self) 0)
		   (incf (placestream-linenumber self))
		   #\newline)
		  (t (prog1 (elt (placestream-linetext self)
				 (placestream-charnumber self))
			    (incf (placestream-charnumber self)))))))
  (if (equal (placestream-lastch self) :eof)
      eof-value
      (placestream-lastch self)))

(defun place-unread-char (self)
  (when (placestream-unreadp self)
    (error "Can't unread twice from ~S." self))
  (setf (placestream-unreadp self) t))

(defun place-clear-input (self &aux ch)
  (loop
   (when (not (listen (placestream-stream self))) (return))
   (setq ch (place-read-char self nil))
   (when (or (null ch) (char= ch #\newline)) (return))))

;;; Determine the current place in a placestream.  Return something of type 
;;; place.  This guy points to the NEXT character that will be read from the 
;;; stream.
(defun curplace (self)
  (cond ((not (placestream-unreadp self))
	 (make-place :linetext (placestream-linetext self)
		     :charnumber (placestream-charnumber self)
		     :linenumber (placestream-linenumber self)))
	((= (placestream-charnumber self) 0)
	 (make-place :linetext (placestream-lastline self)
		     :charnumber (length (placestream-lastline self))
		     :linenumber (- (placestream-linenumber self) 1)))
	(t (make-place :linetext (placestream-linetext self)
		 :charnumber (- (placestream-charnumber self) 1)
		 :linenumber (placestream-linenumber self)))))

