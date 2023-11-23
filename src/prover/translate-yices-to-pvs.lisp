;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smt2.lisp -- 
;; Author          : K. Nukala
;; Created On      : November 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 11/22/2023
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

;; global variables for temp/IO files
(defvar *yices-file* nil)
(defvar *temp-file* nil)

;; hash tables/contexts for mid-translation use
(defvar *term-context* (make-pvs-hash-table))

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
    ('define-type (format t "DEFINE-TYPE ~a " yices-term)
	(case (length yices-term)
	  (2 (format t "UNINTERPRETED~%")
	     yices-term)
	  (3 (format t "INTERPRETED~%")
	     yices-term))
	yices-term)
    ('define (format t "DEFINE ~a " yices-term)
        (case (length yices-term)
	  (2 (format t "UNINTERPRETED~%") ;; (define name :: type)
	     yices-term)
	  (3 (format t "INTERPRETED~%") ;; (define name :: type term)
	     yices-term)))
    ('assert (format t "ASSERT ~a~%" yices-term) ;; (assert formula) 
     (translate-yices-expr-to-pvs (cdr yices-term)))

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

(defun translate-yices-typedef-to-pvs (yices-typedef)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-type-to-pvs (yices-type)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-expr-to-pvs (yices-expr)
  (format t "UNIMPLEMENTED! - translate-yices-expr-to-pvs~%")
  yices-expr)

(defun translate-yices-function-to-pvs (yices-function)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-var-decl-to-pvs (yices-var-decl)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-binding-to-pvs (yices-binding)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-immediate-value-to-pvs (yices-immediate-value)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-number-to-pvs (yices-number)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-assumptions-to-pvs (yices-assumptions)
  (break "UNIMPLEMENTED!"))

(defun translate-yices-commands-to-pvs (yices-terms)
  (loop for yices-term in yices-terms
	until (eq yices-term nil)
	when (translate-yices-command-to-pvs yices-term) collect it))

;; emits the constructed PVS ast to a file with the same name as the input Yices file
(defun emit-pvs-to-file (pvs-ast)
  (let* ((pvs-file (concatenate 'string "~/" "PVS/" "src/" "prover/" "yices-examples/"
				(format nil "~a.pvs" (pathname-name *yices-file*)))))
    (with-open-file (stream pvs-file :direction :output :if-exists :supersede)
      (format stream "UNIMPLEMENTED!")
      (format t "Done emitting output to ~a~%" pvs-file))))


;; cleans up local variables (hash tables, state modifications, etc)
(defun clear-locals ()
  (clrhash *term-context*))

;; cleans up temp files
(defun cleanup-files ()
  (format t "cleanup-files: removing ~a~%" *temp-file*)
  (let* ((remove-command (format nil "rm ~a" *temp-file*)))
    (uiop:run-program remove-command)))

(defun cleanup ()
  (clear-locals)
  (cleanup-files))

(defun translate-yices-commands-to-pvs-stub (yices-ast) yices-ast)

(defun translate-yices-to-pvs (yices-file)
  (format t "Translating yices query located at ~a~%" yices-file)
  (let* ((syices (setq *yices-file* yices-file))
	 (temp-file (concatenate 'string "~/" "tmp/"
				 (format nil "~a-modified.ycs"
					 (pathname-name yices-file))))
	 (stemp (setq *temp-file* temp-file))
	 (yices-ast (parse-yices-file yices-file))
	 (pvs-ast (translate-yices-commands-to-pvs yices-ast)))
    (emit-pvs-to-file pvs-ast)
    (cleanup)))
