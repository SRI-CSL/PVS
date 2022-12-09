
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

(in-package :pvs)

(defvar *pvs-eval-do-timing* nil)
(defvar *convert-back-to-pvs* nil)

(defun generate-lisp-for-prelude ()
  (with-open-file (output (format nil "~a/lib/prelude.lisp" *pvs-path*)
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format output ";;; Lisp file generated from PVS prelude~2%")
    (format output "(in-package :pvs)~%")
    (load-pvs-attachments)
    (dolist (theory *prelude-theories*)
      (pvs2cl-theory theory)
      (dolist (decl (theory theory))
	(when (and (const-decl? decl) (eval-info decl))
	  (write-decl-symbol-table decl output)))
      (dolist (decl (theory theory))
	(when (and (const-decl? decl) (eval-info decl))
	  (write-decl-defns decl output))))))

(defun generate-lisp-for-pvs-file (filename)
  (let ((theories (cdr (gethash filename (current-pvs-files)))))
    (with-open-file (output (format nil "~a.lisp" filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output ";;; Lisp file generated from ~a.pvs~2%" filename)
      (format output
	  ";;; In general for a definiton foo in an ~
               unparameterized~%;;; theory th, the names are:~
           ~%;;;    foo  - takes no arguments, returns a unary closure~
           ~%;;;   _foo  - the nondestructive version of the function~
           ~%;;;    foo! - the destructive version of the function")
      (format output
	  ";;; If the definition appears in a parameterized theory th, ~
               additional functions are generated ~%;;; that take arguments ~
               corresponding to the theory parameters, take names are:~
           ~%;;;    th_foo  - takes no arguments, returns a unary closure~
           ~%;;;   _th_foo  - the nondestructive version of the function~
           ~%;;;    th_foo! - the destructive version of the function")
      (format output
	  "~%;;; Function names must be unique, so a number may be appended, ~
            and the type~%;;; is included for functions associated with ~
            datatypes.~%;;; For these functions, the mappings are given here.")
      (load-pvs-attachments)
      (dolist (theory theories)
	(pvs2cl-theory theory))
      (dolist (theory theories)
	(dolist (decl (theory theory))
	  (when (and (const-decl? decl) (eval-info decl))
	    (write-decl-symbol-table decl output))))
      (format output "(in-package :pvs)~%")
      (dolist (theory theories)
	(dolist (decl (theory theory))
	  (when (and (const-decl? decl) (eval-info decl))
	    (write-decl-defns decl output)))))))
  

(defun generate-lisp-for-theory (theoryname)
  (let* ((theory (get-theory theoryname))
	 (basename (when theory
		     (format nil "~a~a" (context-path theory) (id theory)))))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  (t
	   (load-pvs-attachments)
	   (with-open-file (*standard-output* (format nil "~a.complog" basename)
					      :direction :output
					      :if-exists :supersede
					      :if-does-not-exist :create)
	     (let ((*error-output* *standard-output*))
	       (pvs2cl-theory theory)))
	   (print-lisp-defns theoryname (format nil "~a.lisp" basename) t)))))

(defun evaluation-mode (theoryref)
  (multiple-value-bind (dir file thname)
      (get-theory-ref theoryref)
    (declare (ignore file))
    (unless thname
      (error "No theory name found in ~s" theoryref))
    (with-workspace dir
      (let ((theory (get-typechecked-theory (or thname fname))))
	(unwind-protect
	     (let ((*generate-tccs* 'all)
		   (*current-context* (or (saved-context theory)
					  (context nil)))
		   (*in-evaluator* t)
		   (*pvs-eval-do-timing* t)
		   (*destructive?* t)
		   (*convert-back-to-pvs* t))
	       (load-pvs-attachments)
	       (format t "~%~%PVS Ground Evaluation.~%Enter a ground expression in quotes at the <GndEval> prompt.~%Type help for a list of commands.~%")
	       (format t "~%*CAVEAT*: evaluation of expressions which depend on unproven TCCs may be~%unsound, and result in the evaluator crashing into lisp, running out of~%stack, or worse.  If you crash into lisp, type (restore) to resume.~%")
	       (evaluate))
	  (pvs-message "Theory ~a is not typechecked" theoryref))
	(pvs-emacs-eval "(pvs-evaluator-ready)")))))

(defun gqread ()
  (format t "~%<GndEval> ")
  (force-output)
  (let ((input (ignore-errors (read))))
    (cond ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit? ")
	       (throw 'quit nil)
	       (gqread)))
	  ((and (consp input)
		(member (car input) '(lisp pvs::lisp)))  ;; allow for us to be interrupted
	   (format t "~%~s~%" (eval (cadr input)))
	   (gqread))
	  ((member input '(h help)
		   :test #'equal)
	   (format t "~%Evaluator commands:~%")
	   (format t "  h,help         : print this message~%")
	   (format t "  q,quit         : exit the evaluator~%")
	   (format t "  timing         : print timing information for each evaluation~%")
	   (format t "  notiming       : turn off printing of timing information~%")
	   (format t "  destructive    : use destructive evaluation where possible~%")
	   (format t "  nondestructive : don't use destructive evaluation~%")
	   (format t "  convert        : convert resulting expressions back to PVS syntax~%")
	   (format t "  noconvert      : don't convert resulting expressions~%")
	   (format t "  verbose        : enable verbose compilation messages~%")
	   (format t "  quiet          : disable verbose compilation messages~%")
	   (format t "~%Current values are: ")
	   (unless *pvs-eval-do-timing* (format t "no"))
	   (format t "timing, ")
	   (unless *destructive?* (format t "non"))
	   (format t "destructive, ")
	   (unless *convert-back-to-pvs* (format t "no"))
	   (format t "convert, ")
	   (if *eval-verbose* (format t "verbose.") (format t "quiet."))
	   (format t "~%~%Use M-x pvs-lisp-theory to see the generated lisp for a PVS theory")
	   (format t "~%~%Evaluator input should be enclosed in double quotes~%")
	   (gqread))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort? ")
	       (throw 'abort t)
	       (gqread)))
	  ((eq input 'timing)
	   (setq *pvs-eval-do-timing* t)
	   (format t "Enabled printing of timing information")
	   (gqread))
	  ((eq input 'notiming)
	   (setq *pvs-eval-do-timing* nil)
	   (format t "Disabled printing of timing information")
	   (gqread))
	  ((eq input 'destructive)
	   (format t "Using destructive evaluation where possible")
	   (setq *destructive?* t)
	   (gqread))
	  ((eq input 'nondestructive)
	   (format t "Disabled use of destructive evaluation")
	   (setq *destructive?* nil)
	   (gqread))
	  ((eq input 'convert)
	   (format t "Enabled conversion of result back to PVS syntax")
	   (setq *convert-back-to-pvs* t)
	   (gqread))
	  ((eq input 'noconvert)
	   (format t "Disabled conversion of result back to PVS syntax")
	   (setq *convert-back-to-pvs* nil)
	   (gqread))
	  ((eq input 'verbose)
	   (format t "Enabled verbose compiler messages")
	   (setq *eval-verbose* t)
	   (gqread))
	  ((eq input 'quiet)
	   (format t "Disabled verbose compiler messages")
	   (setq *eval-verbose* nil)
	   (gqread))
	  (t
	   input))))

(defparameter *default-random-test-size* 100)

(defparameter *default-random-test-dtsize* 10)

(defparameter *default-random-test-count* 10)

(defun evaluate ()
;  (load-pvs-attachments)
  (let ((result
	 (catch 'abort
	   (catch 'quit
	     (catch 'restore
	       (handler-case
		   (let* ((raw-input (ignore-errors (gqread)))
			  (test? (and (consp raw-input)
				      (eq (car raw-input) 'test)))
			  (input (if test?
				     (cadr raw-input)
				     raw-input))
			  (pr-input (pc-parse input 'expr))
			  (*tccforms* nil)
			  (tc-input (pc-typecheck pr-input)))
		     (when *evaluator-debug*
		       (format t "typechecks to:~%")
		       (show tc-input))
		     (when *tccforms*
		       (format t "~%Typechecking ~s produced the following TCCs:~%"
			 input)
		       (let ((unproved-tccs (evaluate-tccs)))
			 (when unproved-tccs
			   (format t "~%~%Evaluating in the presence of unproven TCCs may be unsound~%")
			   (unless (pvs-y-or-n-p "Do you wish to proceed with evaluation? ")
			     (throw 'abort t)))))
		     (when test?
		       (unless (or (null (cddr raw-input))
				   (posnat? (caddr raw-input)))
			 (format t "test count must be a positive integer")
			 (throw 'abort t))
		       (unless (or (null (cdddr raw-input))
				   (posnat? (cadddr raw-input)))
			 (format t "test size must be a positive integer")
			 (throw 'abort t)))
		     (if test?
			 (run-random-test
			  tc-input
			  (or (third raw-input) *default-random-test-count*)
			  (or (fourth raw-input) *default-random-test-size*)
			  (or (fifth raw-input) *default-random-test-dtsize*)
			  (sixth raw-input) ;; all?
			  (seventh raw-input) ;; verbose?
			  (eighth raw-input)) ;; instance
			 (let ((cl-input (pvs2cl tc-input)))
			   (when *evaluator-debug*
			     (format t "~a translates to~% ~s~%" tc-input cl-input))
			   (let* ((cl-eval (if *pvs-eval-do-timing*
					       (time (eval cl-input))
					       (eval cl-input)))
				  (clval (if *convert-back-to-pvs*
					     (cl2pvs cl-eval (type tc-input))
					     cl-eval)))
			     (format t "~%==> ~%")
			     (cond ((and clval *convert-back-to-pvs*)
				    (unparse clval))
				   (t
				    (when *convert-back-to-pvs*
				      (format t "Result not ground.  Cannot convert back to PVS."))
				    (format t "~%~a" cl-eval))))))
		     t)
		 (pvseval-error (condition)
		   (format t "~%~a" condition)
		   (throw 'abort t))
		 (cl2pvs-error (condition)
		   (format t "~%~a" condition)
		   (throw 'abort t))
		 (tcerror (condition)
		   (declare (ignore condition)))))))))
    (when result
      (evaluate))))

(defun evaluate-tccs ()
  (let ((unproved nil))
    (load-pvs-attachments)
    (dolist (tcc *tccforms*)
      (format t "~%~a TCC for ~a: ~a"
	(tccinfo-kind tcc) (tccinfo-expr tcc) (tccinfo-formula tcc))
      ;; Note that pvs2cl does not signal a groundeval error, it embeds
      ;; code that signals when evaluated.
      (let ((tcc-input (pvs2cl (tccinfo-formula tcc))))
	(when *evaluator-debug*
	  (format t "~a translates to~% ~s~%"
	    (tccinfo-formula tcc) tcc-input))
	(handler-case
	    (let* ((tcc-eval (eval tcc-input))
		   (tccval (cl2pvs tcc-eval *boolean*)))
	      (format t "~%==> ~%")
	      (unparse tccval)
	      (unless (tc-eq tccval *true*)
		(push tcc unproved)))
	  (groundeval-error (condition)
	    (format t "~%~a" condition)))
	(push tcc unproved)))
    unproved))

(defun evaluator-print-tccs (tccforms)
  (when tccforms
    (let ((tccform (car tccforms)))
      (format t "~%~a TCC for ~a: ~a"
	(tccinfo-kind tccform)
	(tccinfo-expr tccform)
	(tccinfo-formula tccform))
      (evaluator-print-tccs (cdr tccforms)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These operations evaluate a list of PVS expression strings and
;;print the values to a file.   Requested by Ajay Chander (3/23/03).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-to-file (theoryname list-of-exprs filename)
  (let ((theory (get-theory theoryname)))
    (load-pvs-attachments)
    (cond ((null theory)
	   (format t "~%Theory ~a is not typechecked." theoryname))
	  (t (let ((*current-context* (saved-context theory)))
	       (eval-to-file* list-of-exprs filename))))))

(defun eval-to-file* (exprs filename)
  (cond ((consp exprs)
	 (eval-expr-to-file (car exprs) filename)
	 (eval-to-file* (cdr exprs) filename))
	(t nil)))

(defun eval-expr-to-file (expr filename)      
  (let* ((*generate-tccs* 'all)
	 (*tccforms* nil)
	 (expr (pc-parse expr 'expr))
	 (expr (pc-typecheck expr)))
    (with-open-file
	(out filename
	     :direction :output
	     :if-exists :append
	     :if-does-not-exist :create)
      (load-pvs-attachments)
      (format out "~%Evaluating: ~a" expr)
      ;;NSH: can be turned on if TCCs must be printed.      
      ;; (when *tccforms*
      ;;  (format out "~%Generated TCCs: ")
      ;;  (evaluator-print-tccs *tccforms*))
      (handler-case 
	  (let* ((cl-input (pvs2cl expr))
		 (cl-eval (eval cl-input))
		 (pvsval (handler-case (cl2pvs cl-eval (type expr))
			   ;; Handle here, so cl-eval is available
			   (cl2pvs-error (condition)
			     (format out "~%Can't convert back to PVS.")
			     (format out "Common Lisp value: ~s" cl-eval)
			     (format out "~%~a" condition)
			     :no-value))))
	    (unless (eq pvsval :no-value)
	      (format out "~%Value: ~a~%~%" pvsval)))
	(groundeval-error (condition)
	  (format t "~%~a" condition))))))
