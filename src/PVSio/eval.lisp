;;
;; eval.lisp
;; Release: PVSio-6.0 (12/12/12)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/PVSio
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; List of strategies in PVSio: eval, eval-expr, and eval-formula
;;

(in-package :pvs)

(defparameter *pvsio-strategies* nil)

(pushnew "eval-formula" *pvsio-strategies* :test #'string=)
(pushnew "eval-expr"    *pvsio-strategies* :test #'string=)
(pushnew "eval"         *pvsio-strategies* :test #'string=)

(defun evalexpr (expr safe)
  (catch 'abort
    (catch '*pvsio-inprover*
      (catch 'tcerror
	(let* 
	    ((pr-input 
	      (cond ((stringp expr) (pc-parse expr 'expr))
		    ((and (listp expr)
			  (equal (car expr) '!))
		     (ee-pvs-obj (car (eval-ext-expr expr))))
		    ((expr? expr) expr)))
	     (*tccforms* nil)
	     (*generate-tccs* 'all)
	     (tc-input (pc-typecheck pr-input)))
	  (when (and *tccforms* safe)
	    (format t "~%Typechecking ~s produced TCCs:~%" expr)
	    (evaluator-print-tccs *tccforms*)
	    (throw 'abort 
		   (format nil 
			   "~%Use option :safe? nil if TCCs are provable.~%")))
	  (multiple-value-bind 
	   (cl-input error)
	   (catch 'no-defn 
	     (pvs2cl tc-input))
	   (when (eq cl-input 'cant-translate)
	     (throw 'abort (format nil 
				   "~s could not be translated:~%~a~%" 
				   expr error)))
	   (multiple-value-bind 
	    (cl-eval error)
	    (catch 'undefined
	      (eval cl-input))
	    (if (not error)
		(let ((clval 
		       (catch 'cant-translate
			 (cl2pvs cl-eval (type tc-input)))))
		  (or clval
		      (throw 'abort 
			     (format nil 
				     "Result not ground:~%~a~%" 
				     cl-eval))))
	      (throw 'abort (format nil "~%~a" error))))))))))

(defrule eval-expr (expr &optional safe? (auto? t))
  (let ((e      (extra-get-expr expr))
	(result (evalexpr (expr2str e) safe?)))
    (if (and result (stringp result))
	(printf result)
      (when result 
	(let ((casexpr (expr2str (make-equation e (pc-parse (expr2str result) 'expr)))))
	  (with-fnums
	   ((!eex))
	   (trust *PVSGroundEvaluator*
		  (discriminate (case casexpr) !eex)
		  ((skip) !
		   (when auto? (eval-formula !eex)))))))))
  "[PVSio] Adds the hypothesis expr=eval(EXPR) to the current goal, 
where eval(EXPR) is the ground evaluation of EXPR. If SAFE? is t and EXPR
generates TCCs, the expression is not evaluated. Otherwise, TCCs
are added as subgoals and the expression is evaluated. If AUTO? is t,
TCCs are ground evaluated. The strategy is sound in the sense that
user-defined semantic attachments are not evaluated. However, if SAFE? is nil,
the strategy may not terminate properly in the presence of unproven TCCs."
  "Evaluating expression ~a in the current sequent")

(defrule eval-formula (&optional (fnum 1) safe?)
  (let ((fexpr (extra-get-seqf fnum)))
    (when fexpr
      (let ((expr   (formula fexpr))
	    (result (evalexpr expr safe?)))
	(if (and result (stringp result))
	    (skip-msg result)
	  (when result 
	    (trust *PVSGroundEvaluator*
		   (case result)
		   (! (skip))))))))
  "[PVSio] Evaluates the formula FNUM in Common Lisp and adds the result to 
the antecedent of the current goal. If SAFE? is t and FNUM generates TCCs, 
the expression is not evaluated. The strategy is safe in the sense that 
user-defined semantic attachments are not evaluated. However, 
if SAFE? is nil, the strategy may not terminate properly in
the presence of unproven TCCs."
  "Evaluating formula ~a in the current sequent")

(defrule eval (expr &optional safe?)
  (let ((*in-evaluator* t)
	(e (format nil "~a" (extra-get-expr expr)))
	(result (evalexpr e safe?)))
    (if (stringp result)
	(skip-msg result)
      (if (expr? result)
	  (let ((mssg (format nil "~a = ~a~%" e result)))
	    (skip-msg mssg))
	(skip))))
  "[PVSio] Prints the evaluation of expression EXPR. If SAFE? is t and EXPR 
generates TCCs, the expression is not evaluated. This strategy evaluates
semantic attachments. Therefore, it may not terminate properly."
  "Printing the evaluation of ~a")

(defstrat pvsio-about ()
  (let ((version *pvsio-version*)
	(strategies *pvsio-strategies*))
    (printf "%--
% ~a 
% http://shemesh.larc.nasa.gov/people/cam/PVSio
% Strategies in PVSio: ~{~a~^, ~}
%--~%" version strategies))
  "[PVSio] Prints PVSio's about information.")

