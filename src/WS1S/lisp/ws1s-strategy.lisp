(in-package :pvs)

(defvar *output-examples* nil)
(defvar *output-automaton* nil)
(defvar *output-traces* nil)

(defstep ws1s (&optional (fnums *)
			 (examples? T)
			 (automaton? nil)
			 (traces? nil)
			 (verbose? T)
			 (defs !)
			 theories
			 rewrites
			 exclude)
  (let ((cuth *current-theory*)
	(rewrite-msg *rewrite-msg-off*)
	(cuthstr (string (id cuth)))
	(exclude (if (listp exclude) exclude (list exclude)))
	(all-exclude (append exclude (list "sets[nat].singleton"
			   "sets.the"
			   "sets[nat].add"
			   "sets[nat].remove"
			   "sets[nat].union"
			   "sets[nat].intersection"
			   "sets[nat].difference"
			   "sets[nat].emptyset"))))
    (then* (auto-rewrite-theory cuthstr :defs defs :exclude exclude)
	   (auto-rewrite-theory "sets[nat]" :defs defs)
           (install-rewrites :defs defs
			     :theories theories
			     :rewrites rewrites
			     :exclude all-exclude)
           (rewrite-msg-off)
	   (expand* expand)
	   (skip-msg "Expanding definitions..." :force-printing? T)
	   (assert fnums :cases-rewrite? T)
	   (if rewrite-msg (skip) (rewrite-msg-on)) ; restore if necessary
	   (skip-msg "WS1S decision procedure..." :force-printing? T)
	   (ws1s-simp fnums examples? automaton? traces? verbose?)))
  "Decision procedure for Weak Second-order monadic logic of 1 Successor (WS1S)
   based on the Mona package developed at BRICS (http://www.brics.dk/~mona).
   Expands definitions in the formulas specified by FNUMS, applies Boolean
   abstraction for formulas outside the scope of WS1S, and constructs an
   automata that recognizes the language of interpretations of these formulas.

   The argument FNUMS restricts the focus of the strategy to the thereby specified sequent
   formulas. If EXAMPLES? is true, a witness and a counterexample are being displayed if
   these formulas turn out to be satisfiable but not valid. Setting the flag AUTOMATON?
   causes the strategy to display the constructed automaton, TRACES? displays a
   trace version of the witness. For a description of the remaining flags
   DEFS, THEORIES, REWRITES, and EXCLUDE see e.g. (help install-rewrites).

   The supported fragment includes boolean expressions, arithmetic on the
   natural numbers restricted to addition/subraction with/from a constant, and
   operations on finite sets over the naturals like union, intersection, set
   difference, addition and removal of a natural.  Predicates include arithmetic
   comparisons like <, <=, >, >=, equality, disequality, the subset relation,
   and membership in the form P(i). There is quantification over the booleans,
   the natural numbers, finite sets of naturals, and predicate subtypes of the
   aforementioned types built from formulas in the set just being described.
   Furthermore, this strategy tries to apply boolean abstraction for non-WS1S formulas,
   and natural numbers, and finite sets of natural numbers may also be described
   using the 'the' operator; e.g. ripple-carry addition may be defined as:
     +(P, Q: finite_set[nat]): finite_set[nat] = 
       the({R: finite_set[nat] | 
         EXISTS (C: finite_set[nat]): NOT(C(0)) AND
           FORALL (t: nat):
             (C(t + 1) = ((P(t) AND Q(t)) OR (P(t) AND C(t)) OR (Q(t) AND C(t)))) AND
                 (R(t) = P(t) = Q(t) = C(t))});"
  "By rewriting and WS1S decision procedure")


(addrule 'ws1s-simp nil ((fnums *) (examples? T) (automaton? nil) (traces? nil) (verbose? T))
	 (ws1s-step fnums examples? automaton? traces? verbose?)
	 "WS1S Decision Procedure.")


(defun ws1s-step (fnums examples automaton traces verbose)
  #'(lambda (ps)
      (let* ((*output-examples* examples)
	     (*output-automaton* automaton)
	     (*output-traces* traces)
	     (*verbose* verbose)
	     (sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(declare (special *output-examples* *output-automaton*
			  *output-traces* *verbose*))
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
  (declare (special *output-examples* *output-automaton*))
  (multiple-value-bind (dfa symtab)
      (fmla-to-dfa fmla)
    (if (not dfa)
	(progn
	  (ws1s-msg (format nil "Not WS1S translatable ~a" fmla))
	  fmla)
      (let* ((status (dfa-status dfa))
	     (newfmla (cond ((eq status :valid) *true*)
			    ((eq status :inconsistent) *false*)
			    (t fmla))))
	(ws1s-output fmla newfmla)
	(when (or *output-examples* *output-automaton*)
	  (multiple-value-bind (symtab num offsets fvars types)
	      (symtab-strip symtab)
	    (multiple-value-bind (counterex length-of-counterex)
		(dfa-counterexample dfa num offsets)
	      (multiple-value-bind (witness length-of-witness)
		  (dfa-witness dfa num offsets)
		(when *output-examples*
		  (ws1s-example-output "Counterexample: " counterex length-of-counterex num types fvars)
		  (ws1s-example-output "Witness: " witness length-of-witness num types fvars))
		(when *output-automaton*
		  (ws1s-automaton-output dfa num fvars offsets))))))
	newfmla))))
 
(defun ws1s-output (fmla newfmla)
  (format t "~2%Formula ")
  (cond ((tc-eq newfmla *true*)
	 (format t "is valid."))
	((tc-eq newfmla *false*)
	 (format t "is unsatisfiable."))
	(t
	 (format t "is satisfiable but not valid."))))

(defun ws1s-example-output (str example length num types fvars)
  (when (and (not (eq example :null)) (> num 0))
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
				 (make!-application* (add-operator)
						     (list (make!-number-expr j) acc))
			       acc)))
		 (loop* (1+ j) newacc)))))
    (loop* 0 (emptyset-operator))))

(defun ws1s-automaton-output (p num fvars offsets)
  (format t "~2%Free vars:~2%" fvars)
  (dfa-print p num fvars offsets)
  (format t "~%"))

;; Analyze symbol table

(defun symtab-strip (symtab)
  (let* ((size (length symtab))
	 (offsets (make-array size :element-type 'fixnum))
	 (fvars   (make-array size :element-type 'string))
	 (types   (make-string size))
	 (i       0))
    (mapc  #'(lambda (bndng)
	       (let ((idx (cdr bndng))
		     (expr (car bndng)))
		 (setf (elt offsets i) idx)
		 (setf (elt fvars i) (format nil "~a" expr))
		 (setf (elt types i)
		       (let ((level (level expr)))
			 (assert level)
			 (cond ((eql level 0) #\0)
			       ((eql level 1) #\1)
			       ((eql level 2) #\2))))
		 (setf i (1+ i))))
      symtab)
    (values symtab
	    size
	    offsets
	    fvars
	    types)))
