
;;; ============== Debug utilities for prover strategies =============

;; Load after other strategies.

;; To construct the debug form of an existing strategy, invoke
;;    (lisp (make-debug-step S))
;; at the prover prompt.  (S is usually given as a quoted symbol.)
;; This Lisp function will create a redefined helper strategy derived
;; from S and having the same name.  Invoking S via
;;    (trace-strat (S <args>) <optional-depth>)
;; causes internal values to be printed during strategy execution.
;; Values printed include the strategy arguments, LET variables from
;; any top-level LET strategy in the definition, and the final step
;; that gets evaluated (labeled <STEP> in the output).  Any strategies
;; called (indirectly) by S that are also in debug form will be traced
;; as well.

;; To construct debug forms of all existing strategies, use
;;    (lisp (make-debug-steps))

;; Tracing occurs down to "depth" levels in the strategy-calling
;; hierarchy (only debug forms are counted).  If a strategy is
;; redefined, its debug form is overwritten.  (make-debug-steps)
;; needs to be rerun to restore the debug form(s).  Doing so will
;; not alter definitions that have already been converted to debug
;; format.  The pvs-strategies file can be reloaded to restore
;; definitions to their original (non-debug) format.

;; Loading the corresponding Emacs Lisp file adds TAB shortcuts for
;; invoking the trace feature (optional).

(defvar *strat-trace-max* 0)
(defvar *strat-trace-depth* 0)
(defvar *trace-excluded-names* '(trace-strat))

(defun make-debug-steps ()
  (let ((rules '())
	(rule-names (append *pvs-rulebase-names* *trace-excluded-names*)))
    (maphash #'(lambda (key val) (push val rules)) *rules*)
    (loop for e in rules
          unless (member (name e) rule-names)
	    do (make-debug-strat e))))

(defun make-debug-step (strat)
  (make-debug-strat (gethash strat *rules*)))

(defun make-debug-strat (entry)
  (unless (equal (subseq (format-string entry) 1 6) "DEBUG")
    (let* ((name (name entry))
	   (entry-string
	     (format nil "~%~%>>>>> Entering strategy ~A~%  Arguments:" name))
	   (exit-string  (format nil "~%<<<<< Leaving strategy ~A" name))
	   (formals (formals entry))
	   (formal-names (loop for f-arg in formals
			       unless (member f-arg '(&optional &rest))
			         collect (if (consp f-arg) (car f-arg) f-arg)))
	   (get-arg-vals `(stringify-pvs-objects (list ,@formal-names)))
	   (defn (defn entry))
	   (let-form? (and (consp defn) (eq (car defn) 'let)))
	   (let-vars (if let-form? (cadr defn) nil))
	   (value-form (if let-form? (caddr defn) defn))
	   (new-lets (mapappend #'make-debug-let-var let-vars))
	   (new-value (make-debug-let-var `(<step> ',value-form))))
      (eval `(defhelper ,name ,formals
	       (if (< *strat-trace-depth* *strat-trace-max*)
		   (then (let ((,(gensym) (incf *strat-trace-depth*))) (skip))
			 (let ((,(gensym) (princ ,entry-string))
			       (,(gensym) (format t " ~S" ,get-arg-vals))
			       (,(gensym) (format t "~%LET variables:"))
			       ,@new-lets ,@new-value
			       (,(gensym) (princ ,exit-string)))
			   ,value-form)
			 (let ((,(gensym) (decf *strat-trace-depth*))) (skip)))
		   ,defn)
	       "" "DEBUG")))))

(defun make-debug-let-var (var-val)
  (list var-val
	(list (gensym)
	      `(format t ,(format nil "~A~A~A" "~%  " (car var-val) " : ~S")
		       (stringify-pvs-objects ,(car var-val))))))

(defun stringify-pvs-objects (obj)
  (cond ((consp obj)               ;; handles dotted lists 
	 (cons (stringify-pvs-objects (car obj))
	       (stringify-pvs-objects (cdr obj))))
	((typep obj 'standard-object) (format nil "~A" obj))
	(t obj)))

(defhelper trace-strat (strat-call &optional (depth 1000))
  (then (let ((init (setf *strat-trace-max* depth  *strat-trace-depth* 0)))
	  (skip))
	strat-call
	(let ((final (setf *strat-trace-max* 0  *strat-trace-depth* 0)))
	  (skip)))
  "" "")

;;;;;;;;;;;
