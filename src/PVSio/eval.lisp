;; eval.lisp
;; Strategies 
;; Release : PVSio-2.c (09/16/05)

(in-package :pvs)

(defparameter *pvsio-strategies* nil)
(pushnew "eval-formula" *pvsio-strategies* :test #'string=)
(pushnew "eval-expr"    *pvsio-strategies* :test #'string=)
(pushnew "eval"         *pvsio-strategies* :test #'string=)

(defhelper __miracle__ ()
  (let ((thetrue (make-instance 
		  's-formula 
		  'formula (pc-typecheck (pc-parse "TRUE" 'expr))
		  'label nil 
		  'new? nil 
		  'asserted? nil))
	(thefalse (setf (s-forms *goal*) (cons thetrue (s-forms *goal*)))))
    (propax))
  "[PVSio] Internal strategy. WARNING: DO NOT USE (unless you know what 
you are doing)" 
  "")

(defun evalexpr (expr safe)
  (catch 'abort
    (catch '*pvsio-inprover*
      (catch 'tcerror
	(let* 
	    ((pr-input 
	      (cond ((stringp expr) (pc-parse expr 'expr))
		    (safe           (pc-parse (format nil "~a" expr) 'expr))
		    (t              expr)))
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
  (let ((result (evalexpr expr safe?)))
    (if (and result (stringp result))
	(skip-msg result)
      (if result 
	  (let ((casexpr (format nil "(~a) = (~a)" expr result)))
	    (branch (case casexpr)
		    ((skip)
		     (__miracle__)
		     (if auto?
			 (eval-formula)
		       (skip)))))
	(skip))))
  "[PVSio] Adds the antecedent expr=eval(EXPR) to the current goal, 
where eval(x) is the Common Lisp evaluation of EXPR. If SAFE? is T and EXPR
generates TCCs, the expression is not evaluated. Otherwise, TCCs
are added as subgoals and the expression is evaluated. If AUTO? is T,
TCCs are ground evaluated. The strategy is safe in the sense that
user-defined semantic attachments are not evaluated.
However, the strategy may fail or loop in the presence of unproven TCCs."
  "Evaluating expression ~a in the current sequent")

(defrule eval-formula (&optional (fnum 1) safe?)
  (let ((fexpr (car (select-seq (s-forms *goal*) fnum))))
    (if fexpr
	(let ((expr   (formula fexpr))
	      (result (evalexpr expr safe?)))
	  (if (and result (stringp result))
	      (skip-msg result)
	    (if result 
		(spread (case result)
			((__miracle__)))
	      (skip))))
      (skip)))
  "[PVSio] Evaluates the formula FNUM in Common Lisp and adds the result to 
the antecedent of the current goal. If SAFE? is T and FNUM generates TCCs, 
the expression is not evaluated. The strategy is safe in the sense that 
user-defined semantic attachments are not evaluated. However, 
the strategy may fail in the presence of unproven TCCs. "
  "Evaluating formula ~a in the current sequent")

(defrule eval (expr &optional safe?)
  (let ((*in-evaluator* t)
	(result (unwind-protect
		    (evalexpr expr safe?)
		  (setq *in-evaluator* nil)))
	(*in-evaluator* nil))
    (if (stringp result)
	(skip-msg result)
      (if (expr? result)
	(let ((mssg (format nil "~a = ~a~%" expr result)))
	  (skip-msg mssg))
	(skip))))
  "[PVSio] Prints the evaluation of expression EXPR. If SAFE? is T and EXPR 
generates TCCs, the expression is not evaluated."
  "Printing the evaluation of ~a")

(defstrat pvsio-about ()
  (let ((msg (format nil "%--
% Release: ~a. 
% http://research.nianet.org/~Amunoz/PVSio
% Contact and Bugs: Cesar A. Munoz (munoz@nianet.org)
% NIA - NASA LaRC
% Strategies: ~{~a~#[~:;, ~]~}
%--~%" *pvsio-version* "~" *pvsio-strategies*)))
    (skip-msg msg))
  "[PVSio] Prints PVSio's about information.")

(defstrat reload-pvsio ()
  (let ((a (libload "PVSio/eval.lisp"))
	(msg (format nil "Loading: ~A" *pvsio-version*)))
    (skip-msg msg))
  "[PVSio] Reloads PVSio's strategies.")
