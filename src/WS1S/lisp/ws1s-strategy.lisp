(in-package :pvs)

(defvar *output-examples* nil)
(defvar *output-automaton* nil)
(defvar *output-traces* nil)

(defstep ws1s (&optional (fnums *) (examples T) (automaton nil) (traces nil))
  (let ((cuth *current-theory*)
	(cuthstr (string (id cuth))))
    (then* (skolem!)
	   (install-rewrites :defs ! :exclude "sets.the")
	   (rewrite-msg-off)
	   (skip-msg "Expanding definitions..." :force-printing? T)
	   (assert :cases-rewrite? T)
	   (skip-msg "WS1S decision procedure..." :force-printing? T)
	   (ws1s-simp fnums examples automaton traces)))
  "Expands definitions in the formulas specified by FNUMS and tries to decide then
   using the WS1S decision procedures (based on the Mona package developed
   at BRICS (http://www.brics.dk/~mona).  If EXAMPLES is true a witness
   and a counterexample are being displayed if these formulas turn out to be satisfiable
   but not valid, setting the flag AUTOMATON causes the strategy to display the
   constructed automaton, TRACES displays a trace version of the witness."
  "By rewriting and WS1S decision procedure")

(addrule 'ws1s-simp nil ((fnums *) (examples T) (automaton nil) (traces nil))
	 (ws1s-step fnums examples automaton traces)
	 "WS1S Decision Procedure.")

(defun ws1s-step (fnums examples automaton traces)
  #'(lambda (ps)
      (let* ((*output-examples* examples)
	     (*output-automaton* automaton)
	     (*output-traces* traces)
	     (sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(declare (special *output-examples* *output-automaton*
			  *output-traces*))
	(multiple-value-bind (signal newform)
	    (ws1s-sforms selected-sforms)
	  (case signal
	    (! (values '! nil))
	    (X (values 'X (current-goal ps)))
	    (? (values '? (list
			   (lcopy (current-goal ps)
			     's-forms (cons newform remaining-sforms))))))))))

(defun ws1s-sforms (sforms)
  (let* ((fmla (make!-disjunction* (mapcar #'formula sforms)))
	 (newfmla (ws1s-simplify fmla))
	 (new-sform (unless (or (tc-eq fmla newfmla)
				(tc-eq newfmla *false*)
				(and (negation? newfmla)
				     (tc-eq (args1 newfmla) *true*)))
		      (make-instance 's-formula 'formula newfmla))))
    (if new-sform
	(values '? new-sform)
	(values 'X nil))))

(defun ws1s-simplify (fmla)
  (unwind-protect
      (progn
	(fml-to-dfa-init)
	(let ((main (catch 'not-ws1s-translatable
		      (fml-to-dfa fmla))))
	  (if (not main) fmla
	    (multiple-value-bind (symtab num offsets fvars types)
		(symtab-strip)
	      (let* ((restr (dfa-conjunction* (assertions symtab)))
		     (impl  (dfa-implication restr main))
		     (conj  (dfa-conjunction restr main)))
		(multiple-value-bind (counterex length-of-counterex)
		    (dfa-counterexample impl num offsets)
		  (multiple-value-bind (witness length-of-witness)
		      (dfa-witness conj num offsets)
		    (let ((newfmla (cond ((eq counterex :null) *true*)
					 ((eq witness :null)   *false*)
					 (t fmla))))
		      (ws1s-output fmla newfmla)
		      (when (and (not (eq newfmla *true*)) (not (eq newfmla *false*)))
			(ws1s-example-output "Counterexample: "
					     counterex length-of-counterex num types fvars)
			(ws1s-example-output "Witness: "
					     witness length-of-witness num types fvars))
		      (ws1s-automaton-output conj num fvars offsets)
		      (format t "~%")
		      newfmla))))))))
    (fml-to-dfa-init)))

(defun assertions (symtab &optional acc)
  (if (null symtab)
      (nreverse acc)
    (let* ((entry (car symtab))
	   (newacc (if (1st-order? (cdr entry))
		       (cons (dfa-var1 (car entry)) acc)
		     acc)))
      (assertions (cdr symtab) newacc))))
 
(defun ws1s-output (fmla newfmla)
  (format t "~2%Formula (main) ~%    ~a" fmla)
  (cond ((tc-eq newfmla *true*)
	 (format t "~%is valid." fmla))
	((tc-eq newfmla *false*)
	 (format t "~%is unsatisfiable." fmla))
	(t
	 (format t "~%is neither valid nor unsatisfiable." fmla))))

(defun ws1s-example-output (str example length num types fvars)
  (when (and *output-examples* (not (eq example :null)) (> num 0))
    (format t "~2%~a~%" str)
    (loop for i from 0 below num do
	  (format t "~%~a = ~a"
		  (elt fvars i)
		  (let ((level (elt types i)))
		    (cond ((eq level #\0) (val0 example length i))
			  ((eq level #\1) (val1 example length i))
			  ((eq level #\2) (val2 example length i))
			  (t (error "Unknown level ~a" level))))))))

(defun val0 (example length i)
  (if (set? (elt example (* i length))) *true* *false*))
			   
(defun val1 (example length i)
  (labels ((loop* (j)
	     (if (and (unset? (elt example (+ (* i length) j 1))) (< j length))
		 (loop* (1+ j))
	       j)))       
    (make-number-expr (loop* 0))))

(defun val2 (example length i)
  (labels ((loop* (j acc)
	     (if (= j length) acc
	       (let ((newacc (if (set? (elt example (+ (* i length) j 1)))
				 (make!-application (add-to-fset)
						    (list (make!-number-expr j) acc))
			       acc)))
		 (loop* (1+ j) newacc)))))
    (loop* 0 (empty-fset-of-nats))))

(defun ws1s-automaton-output (p num fvars offsets)
  (when *output-automaton*
    (format t "~2%Free vars:~2%" fvars)
    (dfa-print (address p) num fvars offsets)
    (format t "~%")))
