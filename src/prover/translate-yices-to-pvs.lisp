;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-yices-to-pvs.lisp -- 
;; Author          : K. Nukala
;; Created On      : November 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 12/19/2023
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

;; query: THEOREM `conjoined *antecedents*` IMPLIES `disjoined *succedents*`
(defvar *antecedents* nil)
(defvar *succedents* nil)

;; collects all yices forms present in `yices-file`
(defun parse-yices-file (yices-file)
  (let* ((colon-replace-command (format nil "sed -e \"s/::/##/g\" ~a > ~a" yices-file
					(ensure-directories-exist *temp-file*)))
	 (colon-replace-result (uiop:run-program colon-replace-command)))
  (with-open-file (stream *temp-file* :direction :input)
    (loop for expr = (read stream nil 'eof)
	  until (eq expr 'eof)
	  collect expr))))

(defun translate-yices-command-to-pvs (yices-term)
  (case (car yices-term)
    ('define-type (format t "DEFINE-TYPE ~a~%" yices-term)
	(case (length yices-term)
	  (2 (translate-yices-typedef-to-pvs yices-term nil))
	  (3 (translate-yices-typedef-to-pvs yices-term t))))
    ('define (format t "DEFINE ~a " yices-term)
        (case (length yices-term)
	  (2 (format t "UNINTERPRETED~%") ;; (define name :: type)
	     nil)
	  (3 (format t "INTERPRETED~%") ;; (define name :: type term)
	     nil)))
    ('assert (format t "ASSERT ~a~%" yices-term) ;; (assert formula)
	     (if (eq (caadr yices-term) 'not)
		 (let* ((translated-yices-term
			 (translate-yices-expr-to-pvs (cadadr yices-term))))
		       (progn (setq *succedents* (push translated-yices-term *succedents*))
			      nil))
		 (let* ((translated-yices-term
			 (translate-yices-expr-to-pvs (cadr yices-term))))
		   (progn (setq *antecedents* (push translated-yices-term *antecedents*))
			  nil))))
    ;; not sure which of these we should support?
    ('exit (format t "EXIT~%") nil)
    ('check (format t "CHECK~%") nil)
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
    ('dump-context (format t "DUMP-CONTEXT~%") nil)
    ('help (format t "HELP~%") nil)))


;; This function translates the two forms of (define-type ...) as follows:
;; 1) Uninterpreted `(define-type id)`
;; ==> For this case, simply create `id: TYPE` in PVS
;; 2) Interpreted `(define-type id type)`
;; ==> If it's a scalar, do as follows (otherwise, it can function as an alias)
;;     `(define-type id (scalar s1 s2 s3))`
;;     ==> Create datatype `id: DATATYPE
;;                          BEGIN
;;                            s1: s1?
;;                            s2: s2?
;;                            s3: s3?
;;                          END id`
(defun translate-yices-typedef-to-pvs (yices-typedef interpreted-p)
  (if (not interpreted-p)
      ; Looks like (define-type T)
      (mk-type-decl (cadr yices-typedef))
      (let* ((name (symbol-name (cadr yices-typedef)))
	     (cleaned-name (substitute #\_ #\- name))
	     (type (caddr yices-typedef)))
	(format t "interpreted typedef: name=~a, type=~a~%"
		  cleaned-name
		  type)
	(if (and (listp type) (eq (car type) 'scalar))
	    (mk-datatype cleaned-name nil nil (cdr type))
	    (progn (setf (gethash name *type-context*) (translate-yices-type-to-pvs type))
		   nil))
	; (mk-type-decl cleaned-name type)
	)))

(defun translate-yices-type-to-pvs (yices-type)
  (format t "yices-type: ~a~%" yices-type)
  (if (not (listp yices-type))
      (case yices-type
	('int *integer*)
	('bool *boolean*)
	('real *real*))
      (case (car yices-type)
	('bitvector (progn (format t "UNIMPLEMENTED: BITVECTOR TRANSLATION~%") nil))
	('tuple (mk-tupletype (map 'list #'(lambda (x) (translate-yices-type-to-pvs x))
				   (cdr yices-type))))
	('-> (mk-funtype (translate-yices-type-to-pvs (cadr yices-type))
			 (translate-yices-type-to-pvs (caddr yices-type)))))))

(defparameter *pvs-makes-table*
  '((|+| . "mk-plus")
    (|-| . "mk-difference")
    (|*| . "mk-times")
    (|/| . "mk-divides")
    (|->| . "mk-implication")
    (|<=>| . "mk-iff")
    (|<| . "mk-less")
    (|<=| . "mk-lesseq")
    (|>| . "mk-greater")
    (|>=| . "mk-greatereq")
    (|/=| . "mk-disequation")
    (|=| . "mk-equation")
    (|not| . "mk-negation")
    (|and| . "mk-conjunction")
    (|or| . "mk-disjunction")
    (|ite| . "mk-if-expr")))

(defun translate-yices-expr-to-pvs (yices-expr)
  (cond ((symbolp yices-expr) (mk-name-expr yices-expr))
	((numberp yices-expr) (mk-number-expr yices-expr))
	((eq yices-expr 'false) *false*)
	((eq yices-expr 'true) *true*)
	(t
	 (let* ((translated-args
		 (map 'list
		      #'(lambda (x)
			  (if (not x) nil (translate-yices-expr-to-pvs x)))
		      (cdr yices-expr))) 
		(mk-fn (cdr (assoc (car yices-expr) *pvs-makes-table*))))
	   (apply (find-symbol mk-fn 'pvs) translated-args)))))

(defun translate-yices-function-to-pvs (yices-function)
  (break "UNIMPLEMENTED - translate-yices-function-to-pvs~%"))

(defun translate-yices-var-decl-to-pvs (yices-var-decl)
  (break "UNIMPLEMENTED - translate-yices-var-decl-to-pvs~%"))

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
  (format t "(emit-pvs-to-file ~a)~%" pvs-ast)
  (with-open-file (stream output-filename :direction :output :if-exists :supersede)
    (let* ((basename (substitute #\_ #\- (pathname-name output-filename)))
	   (theory-name (concatenate 'string basename "_theory"))
	   (conjoined-antecedents (mk-conjunction *antecedents*))
	   (disjoined-succedents (if (eq *succedents* nil) *false*
				     (mk-disjunction *succedents*)))
	   (ante-succ-query (mk-implication conjoined-antecedents disjoined-succedents))
	   (theorem-name (concatenate 'string basename "_thm")))
      ; (format t "antecedents: ~a~%" conjoined-antecedents)
      ; (format t "succedents (initially negated): ~a~%" disjoined-succedents)
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
  (clrhash *uf-context*))

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
