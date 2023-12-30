;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-yices-to-pvs.lisp -- 
;; Author          : K. Nukala
;; Created On      : November 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 12/29/2023
;; Update Count    : 0
;; Status          : In-Progress
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

(require "uiop") ;; for uiop:run-program

(load "../makes.lisp")

;; global variables for temp/IO files
(defvar *yices-file* nil)
(defvar *temp-file* nil)

;; hash tables/contexts for mid-translation use
(defvar *type-context* (make-pvs-hash-table))
(defvar *uf-context* (make-pvs-hash-table))
(defvar *enum-fields-context* (make-pvs-hash-table))

;; hash table utilities
(defun hash-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun print-hash-entry (key value)
  (format t "(~S, ~S)" key value))

(defun print-hash-table (table)
  (format t "hash-table: ")
  (maphash #'print-hash-entry table))


;; query: THEOREM `conjoined *antecedents*` IMPLIES `disjoined *succedents*`
(defvar *antecedents* nil)
(defvar *succedents* nil)

;; collects all yices forms present in `yices-file`
(defun parse-yices-file (yices-file)
  ;; Since `x::t` isn't directly parseable in CL, I replace all occurrences of `::` with
  ;; a space. This info is used in the `define` branch of `translate-yices-command-to-pvs`,
  ;; where I handle `(define x::t)` forms.
  (let* ((colon-replace-command (format nil "sed -e \"s/::/ /g\" ~a > ~a" yices-file
					(ensure-directories-exist *temp-file*)))
	 (colon-replace-result (uiop:run-program colon-replace-command)))
    (with-open-file (stream *temp-file* :direction :input)
      (loop for expr = (read stream nil 'eof)
	    until (eq expr 'eof)
	    collect expr))))

(defun translate-yices-command-to-pvs (yices-term)
  (case (car yices-term)
    ('define-type (format t "=====> DEFINE-TYPE ~a~%" yices-term)
	(case (length yices-term)
	  (2 (translate-yices-typedef-to-pvs yices-term nil))
	  (3 (translate-yices-typedef-to-pvs yices-term t))))
    ('define (format t "=====> DEFINE ~a " yices-term)
        (case (length yices-term)
	  (3 (format t "UNINTERPRETED~%") ;; (define name type)
	     (translate-yices-var-decl-to-pvs yices-term))
	  (4 (format t "INTERPRETED~%") ;; (define name type term)
	     nil)))
    ('assert (format t "=====> ASSERT ~a~%" yices-term) ;; (assert formula)
	     (if (not (listp (cadr yices-term))) ;; edge case - atom `(assert false)`/`(assert true)`
		 (let* ((translated-yices-atom
			 (translate-yices-expr-to-pvs (cadr yices-term))))
		   (progn (setq *antecedents* (push translated-yices-atom *antecedents*))
			  nil))
		 (if (eq (caadr yices-term) 'not)
		     (let* ((translated-yices-term
			     (translate-yices-expr-to-pvs (cadadr yices-term))))
		       (progn (setq *succedents* (push translated-yices-term *succedents*))
			      nil))
		     (let* ((translated-yices-term
			     (translate-yices-expr-to-pvs (cadr yices-term))))
		       (progn (setq *antecedents* (push translated-yices-term *antecedents*))
			      nil)))))
    ;; not sure which of these we should support?
    ('exit (format t "EXIT~%") nil)
    ('check (format t "=====> CHECK~%") nil)
    ('check-assuming (format t "CHECK-ASSUMING~%") nil)
    ('ef-solve (format t "EF-SOLVE~%") nil)
    ('push (format t "PUSH~%") nil)
    ('pop (format t "POP~%") nil)
    ('reset (format t "RESET~%") nil)
    ('show-model (format t "SHOW-MODEL~%") nil)
    ('show-reduced-model (format t "SHOW-REDUCED-MODEL~%") nil)
    ('show-implicant (format t "SHOW-IMPLICANT~%") nil)
    ('show-unsat-core (format t "SHOW-UNSAT-CORE~%") nil)
    ('show-unsat-assumptions (format t "SHOW-UNSAT-ASSUMPTIONS~%") nil)
    ('eval (format t "EVAL~%") nil)
    ('echo (format t "ECHO~%") nil)
    ('include (format t "INCLUDE~%") nil)
    ('set-param (format t "SET-PARAM~%") nil)
    ('show-param (format t "SHOW-PARAM~%") nil)
    ('show-params (format t "SHOW-PARAMS~%") nil)
    ('show-stats (format t "SHOW-STATS~%") nil)
    ('reset-stats (format t "RESET-STATS~%") nil)
    ('set-timeout (format t "SET-TIMEOUT~%") nil)
    ('show-timeout (format t "SHOW-TIMEOUT~%") nil)
    ('export-to-dimacs (format t "EXPORT-TO-DIMACS~%") nil)
    ('dump-context (format t "=====> DUMP-CONTEXT~%") nil)
    ('help (format t "HELP~%") nil)))


;; Type/enum definitions manipulate *type-context* and *enum-fields-context*
(defun translate-yices-typedef-to-pvs (yices-typedef interpreted-p)
	    ; (format t "(translate-yices-typedef-to-pvs ~a)~%" yices-typedef)
  (if (not interpreted-p)
	    ; Looks like `(define-type id)`, turn into `id: TYPE`
      (progn (setf (gethash (cadr yices-typedef) *type-context*) (cadr yices-typedef))
	     (mk-type-decl (cadr yices-typedef)))
	    ; Looks like `(define-type id T)`, turn into `id: translate-to-pvs(T)`
      (let* ((name (symbol-name (cadr yices-typedef)))
	     (cleaned-name (substitute #\_ #\- name))
	     (cleaned-name-symb (intern cleaned-name))
	     (type (caddr yices-typedef)))
	(if (and (listp type) (eq (car type) 'scalar))
	    (let* ((pvs-scalar (pc-parse (format nil "~a: TYPE = {~{~a~^, ~}}"
						 cleaned-name-symb (cdr type))
					 'theory-elt)))
	      (progn
		(loop for enum-field in (cdr type)
		      do (setf (gethash enum-field *enum-fields-context*) enum-field))
		(setf (gethash cleaned-name-symb *type-context*) cleaned-name-symb)
		pvs-scalar))
	    (progn (setf (gethash cleaned-name-symb *type-context*)
			 (translate-yices-type-to-pvs type))
		   nil)))))


(defun translate-yices-type-to-pvs (yices-type)
  (if (not (listp yices-type))
      (case yices-type
	('int *integer*)
	('bool *boolean*)
	('real *real*)
	(otherwise (gethash yices-type *type-context*)))
      (case (car yices-type)
	('bitvector (progn (format t "UNIMPLEMENTED: BITVECTOR TRANSLATION~%") nil))
	('tuple (mk-tupletype (map 'list #'(lambda (x) (translate-yices-type-to-pvs x))
				   (cdr yices-type))))
	;; KN: doesn't use mk-funtype because arguments aren't type-exprs
	('-> (make-instance 'funtype
			    :domain (translate-yices-type-to-pvs (cadr yices-type))
			    :range (translate-yices-type-to-pvs (caddr yices-type)))))))

;; all the standard binops are binops only (no support for n-ary operations)
(defparameter *pvs-makes-table*
  '((|+| . "mk-addition")
    (|-| . "mk-subtraction")
    (|*| . "mk-multiplication")
    (|/| . "mk-division")
    (|->| . "mk-implication")
    (|<=>| . "mk-iff")
    (|<| . "mk-less")
    (|<=| . "mk-lesseq")
    (|>| . "mk-greater")
    (|>=| . "mk-greatereq")
    (|/=| . "mk-nequation")
    (|=| . "mk-equation")
    (|not| . "mk-negation")
    (|and| . "mk-conjunction")
    (|or| . "mk-disjunction")
    (|ite| . "mk-if-expr")))

(defun translate-yices-expr-to-pvs (yices-expr)
	    ; (format t "(translate-yices-expr-to-pvs ~a)~%" yices-expr)
  (cond ((symbolp yices-expr) (mk-name-expr yices-expr))
	((numberp yices-expr) (mk-number-expr yices-expr))
	((eq yices-expr 'false) *false*)
	((eq yices-expr 'true) *true*)
	((gethash yices-expr *enum-fields-context*) yices-expr)
	((gethash yices-expr *uf-context*) yices-expr)
	(t
	 (let* ((untranslated-fn-call (car yices-expr))
		(translated-args (map 'list #'(lambda (x)
						(if (not x) nil
						    (translate-yices-expr-to-pvs x)))
				      (cdr yices-expr))))
	   (let* ((uf-context-elem (gethash untranslated-fn-call *uf-context*)))
	     (if uf-context-elem
		 (mk-application* uf-context-elem translated-args) ;; found elem in uf-context
		 ;; KN: what if it's not in both *uf-context* and *pvs-makes-table*?
		 (let* ((pvs-makes-table-elem (assoc untranslated-fn-call *pvs-makes-table*)))
		   ;; Handle mk-conjunction/disjunction differently since they expect a list
		   (cond ((eq untranslated-fn-call 'and) (mk-conjunction translated-args))
			 ((eq untranslated-fn-call 'or) (mk-disjunction translated-args))
			 (t
			  (apply (find-symbol (cdr pvs-makes-table-elem) 'pvs)
				 translated-args))))))))))

(defun translate-yices-function-to-pvs (yices-function)
  (break "UNIMPLEMENTED - translate-yices-function-to-pvs~%"))

;; KN: This currently generates a new variable definition for every
;; `(define ...)` declaration, resulting in
;; x1: VAR typ1
;; x2: VAR typ1
;; ...
;; To make this more idiomatic PVS, we could group the declarations by type, enabling
;; x1, x2, ... : typ1
;; -------
;; Variable definitions manipulate *uf-context*
(defun translate-yices-var-decl-to-pvs (yices-var-decl)
  (let* ((name (cadr yices-var-decl))
	 (type (translate-yices-type-to-pvs (caddr yices-var-decl))))
    (if (and (listp (caddr yices-var-decl)) (eq (caaddr yices-var-decl) '->))
	(progn ; (format t "function type! adding to *uf-context*~%")
	  (setf (gethash name *uf-context*) name)
	  (mk-var-decl name type))
	(mk-var-decl name type))))

(defun translate-yices-binding-to-pvs (yices-binding)
  (break "UNIMPLEMENTED - translate-yices-binding-to-pvs~%"))

(defun translate-yices-immediate-value-to-pvs (yices-immediate-value)
  (break "UNIMPLEMENTED - translate-yices-immediate-value-to-pvs~%"))

(defun translate-yices-number-to-pvs (yices-number)
  (break "UNIMPLEMENTED - translate-yices-number-to-pvs~%"))

(defun translate-yices-assumptions-to-pvs (yices-assumptions)
  (break "UNIMPLEMENTED - translate-yices-assumptions-to-pvs~%"))

(defun translate-yices-commands-to-pvs (yices-terms)
  (loop for yices-term in yices-terms
	until (not yices-term)
	when (translate-yices-command-to-pvs yices-term) collect it))

;; emits the constructed PVS ast to a file with the same name as the input Yices file
(defun emit-pvs-to-file (pvs-ast output-filename)
	    ; (map 'list #'(lambda (x) (unparse x)) pvs-ast)
  (with-open-file (stream (ensure-directories-exist output-filename)
			  :direction :output :if-exists :supersede)
    (let* ((basename (substitute #\_ #\- (pathname-name output-filename)))
	   (theory-name (concatenate 'string basename "_theory"))
	   (conjoined-antecedents (mk-conjunction *antecedents*))
	   (disjoined-succedents (if (eq *succedents* nil) *false*
				     (mk-disjunction *succedents*)))
	   (ante-succ-query (mk-implication conjoined-antecedents disjoined-succedents))
	   (theorem-name (concatenate 'string basename "_thm")))
	    ; (format t "c-antecedents: ~a~%" conjoined-antecedents)
	    ; (format t "d-succedents (initially negated): ~a~%" disjoined-succedents)
	    ; (format t "final query: ~a~%" ante-succ-query)
      (format stream "~a: THEORY~%BEGIN~%~%" theory-name)

      (loop for pvs-term in pvs-ast
	    until (not pvs-term)
	    do (progn (unparse pvs-term :stream stream) (format stream "~%~%")))
      
      (format stream "~a: THEOREM ~a~%" theorem-name ante-succ-query)
      (format stream "~%~%END ~a" theory-name)
      (format t "Done emitting output to ~a~%" output-filename))))

;; cleans up local variables (hash tables, state modifications, etc)
(defun clear-locals ()
  (format t "Resetting local variables~%")
  (setq *antecedents* nil)
  (setq *succedents* nil)
  (clrhash *type-context*)
  (clrhash *uf-context*)
  (clrhash *enum-fields-context*))

;; cleans up temp files
(defun cleanup-files ()
  (format t "cleanup-files: removing ~a~%" *temp-file*)
  (let* ((remove-command (format nil "rm ~a" *temp-file*)))
    (uiop:run-program remove-command)))

(defun cleanup ()
  (clear-locals)
  (cleanup-files))

(defun translate-yices-commands-to-pvs-stub (yices-ast) yices-ast)

(defun translate-yices-to-pvs (yices-file output-loc)
  (format t "Translating yices query located at ~a~%" yices-file)
  (let* ((syices (setq *yices-file* yices-file))
	 (temp-file (concatenate 'string "~/" "tmp/"
				 (format nil "~a-modified.ycs"
					 (pathname-name yices-file))))
	 (stemp (setq *temp-file* temp-file))
	 (yices-ast (parse-yices-file yices-file))
	 (pvs-ast (translate-yices-commands-to-pvs yices-ast)))
    (emit-pvs-to-file pvs-ast output-loc)
    (cleanup)))
