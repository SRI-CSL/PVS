;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ergo-runtime-fixes.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat May 27 18:20:25 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr  5 00:43:24 1998
;; Update Count    : 2
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

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

;;; This file, must be compiled and loaded before the pvs-parser.lisp file,
;;; otherwise the built-in macro will be used instead.

(in-package :pvs)

(defvar *newline-comments*)

(defvar *elsif-places* nil)

;;; An association list of operators and their places.  The problem is
;;; that operators are thrown away, and later make-sb-term is called with
;;; just an id.  We thus keep all possible places associated with an id,
;;; and in make-sb-term set the place attribute if there is a unique one,
;;; otherwise we set a places attribute, and wait for parse to determine
;;; the right place from the argument places.

(defvar *operator-places* nil)

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
    (sbrt::save-place-and-comment-info result splace eplace)
    (case sim-op
      (TERM-EXPR (reset-operator-place args))
      (UNARY-TERM-EXPR (reset-unary-operator-place args)))
    ;;(assert (type-of result))
    result))

(defvar *last-end-place* nil)
(defvar *last-end-value* nil)

;;; Makes extensive use of the following functions from ERGO:
;;;	sim-term-op - returns the symbol which is the operator of a term
;;;	is-sop	    - checks whether a given symbol is the operator of a term
;;;	term-args   - returns the list of arguments of a term
;;;	ds-id       - returns the symbol of an id term

(defun term-place (absyn)
  (getf (term:term-attr absyn) :place))

(defsetf term-place (absyn) (place)
  `(setf (getf (term:term-attr ,absyn) :place) ,place))

(defun term-comment (absyn)
  (getf (term:term-attr absyn) :comment))

(defsetf term-comment (absyn) (comment)
  `(setf (getf (term:term-attr ,absyn) :comment) ,comment))

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

(defun white-space (c)
  (member c '(#\Space #\Tab)))
       
    
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
			(intern name (or *abs-syn-package* :pvs)))
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
	  ((or (gethash (intern (string-upcase token) :sbst) *keyword-table*)
	       (string= token "λ"))
	   (let* ((upstr (if (member token '(|λ|) :test #'string=)
			     (string token)
			     (string-upcase token)))
		  (pvs-sym (intern upstr :pvs)))
	     (when (member pvs-sym pvs::*infix-operators* :test #'eq)
	       (let ((oplace (vector (sbrt::place-linenumber place)
				     (sbrt::place-charnumber place)
				     (sbrt::place-linenumber place)
				     (+ (the fixnum
					  (sbrt::place-charnumber place))
					(the fixnum
					  (length (string token)))))))
		 (push (cons pvs-sym oplace) pvs::*operator-places*)))
	     (values (intern upstr :sbst)
		     :keyword-internal-flag place comment)))
	  ((eq token :eof) (values :eof nil place comment))
	  ((or (eq token :illegal-token)
	       (and (not (eq token 'sbst::_))
		    (char= (char (symbol-name token) 0) #\_)))
	   (values :illegal-token :illegal-token place))
	  (t (let ((id (intern (if (or *case-sensitive*
				       (member token '(λ) :test #'string=))
				   (symbol-name token)
				   (string-upcase (symbol-name token)))
			       (or *abs-syn-package* :pvs))))
	       (values 'sbst::!id! id place comment))))))
