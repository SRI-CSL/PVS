;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-yices-to-pvs.lisp -- 
;; Author          : K. Nukala
;; Created On      : November 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 3/08/2023
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
(defvar *def-var-context* (make-pvs-hash-table))
(defvar *type-context* (make-pvs-hash-table))
(defvar *uf-context* (make-pvs-hash-table))
(defvar *enum-fields-context* (make-pvs-hash-table))
(defvar *bound-var-context* (make-pvs-hash-table))

;; hash table utilities
(defun hash-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun print-hash-entry (key value)
  (format t "(~S, ~S)" key value))

(defun print-hash-table (table)
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
	  (4 (format t "INTERPRETED~%") ;; (define name type term) - TODO
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



;; turns (a b c d ...) into ((a b) (c d) ...)
(defun make-pairs-rec (list acc)
  (if (consp list)
      (make-pairs-rec (cddr list) (cons (list (car list) (cadr list)) acc))
      acc))

(defun make-pairs (list) (make-pairs-rec list nil))

(defun translate-record-field-decls (field-decls)
  (map 'list #'(lambda (x) (mk-field-decl (car x)
					  (translate-yices-type-to-pvs (cadr x))))
       (make-pairs field-decls)))

(defun translate-record-field-assns (field-assns)
  (map 'list #'(lambda (x) (mk-assignment nil (car x) (cadr x)))
       (make-pairs field-assns)))


;; Type/enum definitions manipulate *type-context* and *enum-fields-context*
;; TODO: do away with handcrafting declaration strings via `pc-parse`
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
	(cond ((and (listp type) (eq (car type) 'scalar))
	       (let* ((pvs-scalar (pc-parse (format nil "~a: TYPE = {~{~a~^, ~}}"
						    cleaned-name-symb (cdr type))
					    'theory-elt)))
		 (progn
		   (loop for enum-field in (cdr type)
			 do (setf (gethash enum-field *enum-fields-context*) enum-field))
		   (setf (gethash cleaned-name-symb *type-context*) cleaned-name-symb)
		   pvs-scalar)))
	      ((and (listp type) (eq (car type) 'datatype))
	       (break "typedef datatype unimplemented~%"))
	      ((and (listp type) (eq (car type) 'tuple))
	       (setf (gethash cleaned-name-symb *type-context*) cleaned-name-symb)
	       (pc-parse (format nil "~a: TYPE = ~a"
				 cleaned-name-symb
				 (mk-tupletype (map 'list #'(lambda (x) (translate-yices-type-to-pvs x))
						    (cdr type))))
			 'theory-elt))
	      ((and (listp type) (eq (car type) 'subtype))
	       (break "typedef subtype unimplemented~%"))
	      ((and (listp type) (eq (car type) 'record))
	       (setf (gethash cleaned-name-symb *type-context*) cleaned-name-symb)
	       (pc-parse (format nil "~a: TYPE = ~a"
				 cleaned-name-symb
				 (mk-recordtype (translate-record-field-decls (cdr type)) nil))
			 'theory-elt))
	      (t
	       (progn (setf (gethash cleaned-name-symb *type-context*)
			    (translate-yices-type-to-pvs type))
		      nil))))))


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
	('record (mk-recordtype (translate-record-field-decls (cdr yices-type)) nil))
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
    (|ite| . "mk-if-expr")
    (|forall| . "mk-forall-expr")
    (|exists| . "mk-exists-expr")))


(defun nary-addition (arguments)
  (if (= (length arguments) 1)
      (car arguments)
      (mk-addition (car arguments) (nary-addition (cdr arguments)))))

(defun translate-yices-expr-to-pvs (yices-expr)
  ; (format t "(translate-yices-expr-to-pvs ~a)~%" yices-expr)
  ; (format t "*enum-fields-context*: ~a~%" (hash-keys *enum-fields-context*))
  ; (format t "*uf-context*: ~a~%" (hash-keys *uf-context*))
  ; (format t "*bound-var-context*: ~a~%" (hash-keys *bound-var-context*))
  (cond ; ((symbolp yices-expr) (format t "symbolp ~%")(mk-name-expr yices-expr))
	((numberp yices-expr) (mk-number-expr yices-expr))
	((eq yices-expr 'false) *false*)
	((eq yices-expr 'true) *true*)
	((gethash yices-expr *enum-fields-context*) yices-expr)
	((gethash yices-expr *uf-context*)
	 ; (format t "translating uninterpreted function ~a~%" yices-expr)
	 (mk-name-expr yices-expr))
	((gethash yices-expr *def-var-context*)
	 ; (format t "translating defined variable ~a~%" yices-expr)
	 (mk-name-expr yices-expr))
	((gethash yices-expr *bound-var-context*)
	 ; (format t "translating bound variable ~a~%" yices-expr)
	 (mk-name-expr yices-expr))
	((eq (car yices-expr) 'lambda)
	 (let* ((var (caadr yices-expr))
		(type (cadadr yices-expr))
		(expr (prog2
			  (setf (gethash var *bound-var-context*) var)
			  (translate-yices-expr-to-pvs (caddr yices-expr))
			(remhash var *bound-var-context*))))
	   ; TODO: use mk-lambda-expr instead of handcrafting the string
	   (pc-parse (format nil "LAMBDA (~a: ~a): ~a~%" var type expr) 'expr))
	 ;; (format t "adding ~a to *bound-var-context*~%" (caadr yices-expr))
	 ;; (setf (gethash (caadr yices-expr) *bound-var-context*) (caadr yices-expr))
	 ;; (prog1 (mk-lambda-expr (list (caadr yices-expr))
	 ;; 		 (translate-yices-expr-to-pvs (caddr yices-expr)))
	 ;;   (format t "removing ~a from *bound-var-context*~%" (caadr yices-expr))
	 ;;   (remhash (car yices-expr) *bound-var-context*))
	 )
	; KN: Currently only supports a single update expr, rather than a list
	((eq (car yices-expr) 'update)
	 ; (update expr (idx) value)
	 (let* ((expr (cadr yices-expr))
		(idx (caaddr yices-expr))
		(value (cadddr yices-expr)))
	   ;; (mk-update-expr-1 expr idx value)
	   ; TODO: use mk-update-expr(-1) instead of handcrafting the string
	   (pc-parse (format nil "~a WITH [(~a) := ~a]" (translate-yices-expr-to-pvs expr) idx value)
		     'expr)
	   ))
	(t
	 (let* ((untranslated-fn-call (car yices-expr))
		(translated-args (map 'list #'(lambda (x)
						(if (not x) nil
						    (translate-yices-expr-to-pvs x)))
				      (cdr yices-expr))))
	   (let* ((uf-context-elem (gethash untranslated-fn-call *uf-context*)))
	     (if uf-context-elem
		 (mk-application* uf-context-elem translated-args) ;; found elem in uf-context
		 (let* ((pvs-makes-table-elem (assoc untranslated-fn-call *pvs-makes-table*)))
		   ;; Handle mk-conjunction/disjunction differently since they expect a list
		   (cond ((eq untranslated-fn-call 'and) (mk-conjunction translated-args))
			 ((eq untranslated-fn-call 'or) (mk-disjunction translated-args))
			 ((eq untranslated-fn-call 'mk-tuple) (mk-tuple-expr translated-args))
			 ; ((eq untranslated-fn-call 'lambda) (break "UNIMPLEMENTED - LAMBDA~%"))
			 ((eq untranslated-fn-call 'mk-record)
			  (mk-record-expr (translate-record-field-assns translated-args)))
			 ((eq untranslated-fn-call '+) (nary-addition translated-args))
			 ((eq untranslated-fn-call 'select)
			  (make-instance 'projappl
					 :id nil
					 :index (cadr translated-args)
					 :argument (car translated-args)))
			 (t
			  (apply (find-symbol (cdr pvs-makes-table-elem) 'pvs)
				 translated-args))))))))))

;; KN: This currently generates a new variable definition for every
;; `(define ...)` declaration, resulting in
;; x1: VAR typ1
;; x2: VAR typ1
;; ...
;; To make this more idiomatic PVS, we could group the declarations by type, enabling
;; x1, x2, ... : typ1
;; -------
;; Function variable definitions manipulate *uf-context*
;; Normal variable declarations manipulate *def-var-contextx*
(defun translate-yices-var-decl-to-pvs (yices-var-decl)
  (let* ((name (cadr yices-var-decl))
	 (type (translate-yices-type-to-pvs (caddr yices-var-decl))))
    (cond ((and (listp (caddr yices-var-decl)) (eq (caaddr yices-var-decl) '->))
	   (progn ; (format t "function type! adding ~a to *uf-context*~%" name)
	     (setf (gethash name *uf-context*) name)
	     (mk-var-decl name type)))
	  (t (setf (gethash name *def-var-context*) name)
	     (mk-var-decl name type)))))

(defun translate-yices-commands-to-pvs (yices-terms)
  (loop for yices-term in yices-terms
	until (not yices-term)
	when (translate-yices-command-to-pvs yices-term) collect it))

;; emits the constructed PVS ast to a file with the same name as the input Yices file
(defun emit-pvs-to-file (pvs-ast output-filename)
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
	    do (progn (unparse pvs-term :stream stream)
		      (format stream "~%~%")))
      
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
  (clrhash *enum-fields-context*)
  (clrhash *def-var-context*)
  (clrhash *bound-var-context*))

;; cleans up temp files
(defun cleanup-files ()
  (format t "Removing temp-file ~a~%" *temp-file*)
  (let* ((remove-command (format nil "rm ~a" *temp-file*)))
    (uiop:run-program remove-command)))

(defun cleanup ()
  (clear-locals)
  (cleanup-files))

;; (defun translate-yices-commands-to-pvs-stub (yices-ast) yices-ast)

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
