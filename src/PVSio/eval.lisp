;; eval.lisp
;; Definition of evaluation strategies 

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

(defrule eval-expr (expr &optional (safe? t) (auto? t))
  (let ((e (extra-get-expr expr))
	(result (evalexpr (format nil "~a" e) safe?)))
    (if (and result (stringp result))
	(skip-msg result)
      (when result 
	(let ((casexpr (make-equation e (pc-parse (format nil "~a" result) 'expr))))
	  (branch (case casexpr)
		  ((skip)
		   (__miracle__)
		   (when auto? (let ((flag (tcc-sequent? *goal*))
				     (tcc (when flag (tcc *goal*))))
				 (when flag (eval-expr tcc) (assert))))))))))
  "[PVSio] Adds the antecedent expr=eval(EXPR) to the current goal, 
where eval(x) is the ground evaluation of EXPR. If SAFE? is T and EXPR
generates TCCs, the expression is not evaluated. Otherwise, TCCs
are added as subgoals and the expression is evaluated. If AUTO? is T,
TCCs are ground evaluated. The strategy is sound in the sense that
user-defined semantic attachments are not evaluated.
However, the strategy may fail or loop in the presence of unproven TCCs."
  "Evaluating expression ~a in the current sequent")

(defrule eval-formula (&optional (fnum 1) safe?)
  (let ((fexpr (car (select-seq (s-forms *goal*) fnum))))
    (when fexpr
      (let ((expr   (formula fexpr))
	    (result (evalexpr expr safe?)))
	(if (and result (stringp result))
	    (skip-msg result)
	  (when result 
	    (spread (case result)
		    ((__miracle__))))))))
  "[PVSio] Evaluates the formula FNUM in Common Lisp and adds the result to 
the antecedent of the current goal. If SAFE? is T and FNUM generates TCCs, 
the expression is not evaluated. The strategy is safe in the sense that 
user-defined semantic attachments are not evaluated. However, 
the strategy may fail in the presence of unproven TCCs. "
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
  "[PVSio] Prints the evaluation of expression EXPR. If SAFE? is T and EXPR 
generates TCCs, the expression is not evaluated."
  "Printing the evaluation of ~a")

(defstrat pvsio-about ()
  (let ((version *pvsio-version*)
	(strategies *pvsio-strategies*))
    (printf "%--
% ~a 
% http://shemesh.larc.nasa.gov/people/cam/PVSio
% Strategies: ~{~a~^, ~}
%--~%" version strategies))
  "[PVSio] Prints PVSio's about information.")

