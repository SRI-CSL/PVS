;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies.lisp -- This file contains all the basic PVS strategies.
;; Author          : N. Shankar
;; Created On      : Thu Jan  8 15:02:25 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 29 17:24:08 1999
;; Update Count    : 3
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

(eval-when (eval compile load)
  (defun check-formals (formals &optional opt-flag)
    (or (null formals)
	(if (eq (car formals) '&optional)
	    (if (member '&optional (cdr formals))
		(format t "~%'&optional occurs twice")
		(check-formals (cdr formals) t))
	    (if (eq (car formals) '&rest)
		(if (not (singleton? (cdr formals)))
		    (format t "~%Exactly one &rest argument allowed.")
		    (if (not (symbolp (cadr formals)))
			(format t "~%&rest argument should be a symbol.")
			t))
		(and (or (symbolp (car formals))
			 (and (consp (car formals))
			      opt-flag
			      (symbolp (caar formals)))
			 (format t "~%Formals must be symbols or pairs (when optional)"))
		     (check-formals (cdr formals) opt-flag))))))
  (defun check-prover-macro-args (name args body doc format)
    (declare (ignore body))
    (cond ((not (symbolp name))
	   (error "Name ~a must be a symbol." name))
	  ((not (check-formals args))
	   (error "Arguments ~a must be a list of the form~
                 ~%  (arg1 arg2... &optional... &rest argn)."
		  args))
	  ((not (stringp doc))
	   (error "Documentation ~a is not a string (in double quotes)."
		  doc))
	  ((not (stringp format))
	   (error "Format-string ~a is not a string (in double quotes)."
		  format))))
  (defun extract-lisp-exprs-from-strat-body (body &optional lispexprs)
    (cond ((null body)
	   (nreverse lispexprs))
	  ((consp body)
	   (cond ((eq (car body) 'if)
		  (extract-lisp-exprs-from-strat-body
		   (cddr body) (cons (cadr body) lispexprs)))
		 ((eq (car body) 'let)
		  (let ((nexprs (extract-lisp-exprs-from-strat-body
				 (cddr body))))
		    (nreverse (cons `(let* ,(cadr body)
				       ,@(cons `(list ,@(mapcar #'car
							  (cadr body)))
					       nexprs))
				    lispexprs))))
		 (t (extract-lisp-exprs-from-strat-body
		     (cdr body)
		     (extract-lisp-exprs-from-strat-body (car body) lispexprs)))))
	  (t (nreverse lispexprs))))
  )

	    
(defun defgen* (name formals definition docstring format-string
		     entry-type rules-or-steps)
  (let ((primitive (gethash name *rulebase*))
	(rule (gethash name *rules*))
	(strat (gethash name *steps*)))
    (cond (primitive
	   (format t "~%~a exists as a primitive rule.  It cannot be redefined." name))
	  (rule
	   (format t "~%~a exists as a defined rule." name))
	  (strat
	   (format t "~%~a exists as a strategy." name)))
    (cond (primitive (format t "~%No change. "))
	  (t  (if (or rule strat)
		  (format t "~%Redefining ~a. " name)
		  (format t "~%Defining ~a. " name))
	      (if rule (remhash name *rules*))
	      (if strat (remhash name *steps*))
	      #+lucid (record-source-file name 'strategy)
	      (let ((old (assoc name *prover-keywords*))
		    (has-rest? (when (memq '&rest formals) t)))
		(if old
		    (setf (cdr old) (cons has-rest? (make-prover-keywords formals)))
		    (push (cons name (cons has-rest? (make-prover-keywords formals)))
				*prover-keywords*)))
	      (add-symbol-entry name
				(make-instance entry-type
				  'name name
				  'formals formals
				  'defn definition
				  'docstring docstring
				  'format-string format-string)
				rules-or-steps)))))


(defun defstrat* (name formals definition
		     &optional docstring format-string)
  (defgen* name formals definition docstring format-string
		 'defrule-entry *steps*))

(defun defrule* (name formals definition
		     &optional docstring format-string)
  (defgen* name formals definition docstring format-string
		 'defrule-entry *rules*))

(defun defstep* (name formals definition
		     &optional docstring format-string)
  (defgen* name formals definition docstring format-string
		 'defstep-entry *rules*)
  (defgen* (intern (format nil "~a$" name))
    formals definition docstring format-string
		 'defstep-entry *steps*))

(defun defhelper* (name formals definition
		     &optional docstring format-string)
  (defgen* name formals definition docstring format-string
		 'defhelper-entry *rules*)
  (defgen* (intern (format nil "~a$" name))
    formals definition docstring format-string
		 'defhelper-entry *steps*))


(defmacro defrule (name args body doc format)
  (check-prover-macro-args name args body doc format)
  (let ((lbody (extract-lisp-exprs-from-strat-body body)))
    (if lbody
	(let ((largs (mapcar #'(lambda (a) (if (consp a) (car a) a))
		       (remove-if #'(lambda (a)
				      (memq a '(&optional &rest)))
			 args))))
	  `(progn (defun ,(makesym "(DEFRULE) ~a" name) ,largs
		    ,@lbody
		    (list ,@largs))
		  (defrule* ',name ',args ',body
		    (format nil "~s :~%    ~a"
		      (cons ',(makesym "~a/$" name) ',args)
		      ,doc)
		    (format nil "~%~a," ,format))))
	`(defrule* ',name ',args
	   ',body
	   (format nil "~s:~%    ~a" (cons ',name ',args) ,doc)
	   (format nil "~%~a," ,format)))))

(defmacro defstrat (name args body doc &optional format)
  (check-prover-macro-args name args body doc (or format ""))
  (let ((lbody (extract-lisp-exprs-from-strat-body body)))
    (if lbody
	(let ((largs (mapcar #'(lambda (a) (if (consp a) (car a) a))
		       (remove-if #'(lambda (a)
				      (memq a '(&optional &rest)))
			 args))))
	  `(progn (defun ,(makesym "(DEFSTRAT) ~a" name) ,largs
		    ,@lbody
		    (list ,@largs))
		  (defstrat* ',name ',args ',body
		    (format nil "~s :~%    ~a"
		      (cons ',(makesym "~a/$" name) ',args)
		      ,doc)
		    (format nil "~%~a," ,format))))
	`(defstrat* ',name ',args
	   ',body
	   (format nil "~s:~%    ~a" (cons ',name ',args) ,doc)
	   (format nil "~%~a," ,format)))))

(defmacro defstep (name args body doc format)
  (check-prover-macro-args name args body doc format)
  (let ((lbody (extract-lisp-exprs-from-strat-body body)))
    (if lbody
	(let ((largs (mapcar #'(lambda (a) (if (consp a) (car a) a))
		       (remove-if #'(lambda (a)
				      (memq a '(&optional &rest)))
			 args))))
	  `(progn (defun ,(makesym "(DEFSTEP) ~a" name) ,largs
		    ,@lbody
		    (list ,@largs))
		  (defstep* ',name ',args ',body
		    (format nil "~s :~%    ~a"
		      (cons ',(makesym "~a/$" name) ',args)
		      ,doc)
		    (format nil "~%~a," ,format))))
	`(defstep* ',name ',args ',body
	   (format nil "~s :~%    ~a"
	     (cons ',(makesym "~a/$" name) ',args)
	     ,doc)
	   (format nil "~%~a," ,format)))))


(defmacro defhelper (name args body doc format)
  (check-prover-macro-args name args body doc format)
  (let ((lbody (extract-lisp-exprs-from-strat-body body)))
    (if lbody
	(let ((largs (mapcar #'(lambda (a) (if (consp a) (car a) a))
		       (remove-if #'(lambda (a)
				      (memq a '(&optional &rest)))
			 args))))
	  `(progn (defun ,(makesym "(DEFHELPER) ~a" name) ,largs
		    ,@lbody
		    (list ,@largs))
		  (defhelper* ',name ',args ',body
		    (format nil "~s :~%    ~a"
		      (cons ',(makesym "~a/$" name) ',args)
		      ,doc)
		    (format nil "~%~a," ,format))))
	`(defhelper* ',name ',args
	   ',body
	   (format nil "~s :~%    ~a"
	     (cons ',(makesym "~a/$" name) ',args)
	     ,doc)
	   (format nil "~%~a," ,format)))))


(defstrat try (strategy then else)
  ()
  "Tries strategy STRATEGY on current goal, and if successful, applies
strategy THEN to the subgoals.  If no change, then the ELSE strategy is
applied to current goal.
E.g., (try (skip)(flatten)(skolem!)) is just (skolem!)
      (try (lemma \"assoc\")(beta)(flatten)) is just lemma followed
           by beta if lemma assoc exists."
  "")

(defstrat let (bindings body)
  ()
  "Allows variables in body to be bound to the results of Lisp
computations.  E.g.,
  (let ((x (car *new-fmla-nums*)))
      (then (inst? x)(split x)))."
  "")


(defstep assert  (&optional (fnums *) rewrite-flag
			    flush? linear? cases-rewrite? (type-constraints? T)
			    ignore-prover-output?) 
	 (simplify
	  fnums T T rewrite-flag flush? linear? cases-rewrite? type-constraints? ignore-prover-output?) 
 "Simplifies/rewrites/records formulas in FNUMS using decision
procedures.  Variant of SIMPLIFY with RECORD? and REWRITE? flags set
to T. If REWRITE-FLAG is RL(LR) then only lhs(rhs) of equality
is simplified. If FLUSH? is T then the current asserted facts are
deleted for efficiency.  If LINEAR? is T, then multiplication and
division are uninterpreted.  If CASES-REWRITE? is T, then
the selections and else parts of a CASES expression are simplified,
otherwise, they are only simplified when simplification selects
a case.  See also SIMPLIFY, RECORD, DO-REWRITE.
Examples:
 (assert):  Simplifies, rewrites, and records all formulas.
 (assert -1 :rewrite-flag RL): Apply assert to formula -1 leaving
    RHS untouched if the formula is an equality.
 (assert :flush? T :linear? T): Apply assert with fully uninterpreted
  nonlinear arithmetic after flushing existing decision procedure
  database."
"Simplifying, rewriting, and recording with decision procedures")

(defstep record (&optional (fnums *) rewrite-flag
			    flush? linear? (type-constraints? T)
			    ignore-prover-output?)
	 (simplify
	  fnums T NIL rewrite-flag flush? linear? type-constraints?
	  ignore-prover-output?)
	 "Uses decision procedures to simplify and record the formulas
in FNUMS for further simplification.   Variant of SIMPLIFY with RECORD?
flag set to T and REWRITE? flags set to NIL. If REWRITE-FLAG is
RL(LR) then only lhs(rhs) is simplified.  If FLUSH? is T then the
current asserted facts are deleted for efficiency.  If LINEAR? is T,
then multiplication and division are uninterpreted.  Example:
 (record - :flush? T): flushes database and records antecedent formulas."
	 "Simplifying and recording with decision procedures")

(defstep do-rewrite (&optional (fnums *) rewrite-flag
			    flush? linear? cases-rewrite? (type-constraints? T))
	 (simplify
	  fnums NIL T rewrite-flag flush? linear? cases-rewrite? type-constraints?) 
	 "Uses decision procedures to rewrite the formulas in FNUMS.
Variant of SIMPLIFY with RECORD? flag set to NIL and REWRITE? flags set to
T. If REWRITE-FLAG is RL(LR) then only lhs(rhs) is simplified.  If FLUSH?
is T then the current asserted facts are deleted for efficiency.  If
LINEAR? is T, then multiplication and division are uninterpreted.  If
CASES-REWRITE? is T, then the selections and else parts of a CASES
expression are simplified, otherwise, they are only simplified when
simplification selects a case.
Examples: (do-rewrite): Simplify every sequent formula with rewriting.
(do-rewrite :rewrite-flag RL :linear? T): Apply rewriting only to left-hand
          sides of any equalities with uninterpreted nonlinear arithmetic."
 "Simplifying and recording with decision procedures")


(defstep auto-rewrite!! (&rest names)
  (let ((names
	 (loop for name in names
	       collect (if (consp name)
			   (if (consp (car name))
			       name (list name))
			   (list (list name))))))
    (auto-rewrite :names names))
  "Similar to auto-rewrite! except that explicit (non-recursive)
definitions are always expanded even when all the arguments
do not occur in the expression.   Examples:
 (auto-rewrite!! \"compose\"):  Installs the rewrite so that compose(f, g)
  is expanded.  "
  "Installing !! rewrites from: ~@{~%  ~a~}")

(defstep auto-rewrite! (&rest names)
  (let ((names
	 (loop for name in names
	       collect (if (consp name) name (list name)))))
    (auto-rewrite :names names))
	 " Similar to auto-rewrite except that the rewrites of lemmas and
explicit (non-recursive) definitions always occur independent of the
righthand-side.  Examples:
 (auto-rewrite! \"assoc\" -3 \"distrib\" \"square\"):  Installs rewrites.
 (auto-rewrite! :names (\"assoc\" -3 \"distrib\" \"square\")): Same."
	 "Installing automatic rewrites from: ~@{~%  ~a~}")

(defun defn-decl? (x)
  (or (const-decl? x)(def-decl? x)))

(defstep auto-rewrite-theory (name &optional exclude defs ;defs-only? 
				   always? tccs?)
  (let ((name (pc-parse name 'modname))
	(current? (eq (id name)(id *current-theory*)))
	(theory-name (resolve-theory-name name))
	(theory (get-theory theory-name))
	(exclude (if (listp exclude) exclude (list exclude)))
	(exclude (mapcar #'(lambda (x) (pc-parse x 'name)) exclude))
	(okay?  (and theory
		     (or (not current?)
			 (null (actuals theory-name))))))
    (if theory
	(if okay?
	    (let ((all-decls (when theory
			       (append (assuming theory)
				       (theory theory))))
		  (exclude-current
		   (when current?
		     (memq (declaration *top-proofstate*)
			   all-decls)))
		  (names
		   (loop for decl in all-decls
			 when
			 (and  (or (defn-decl? decl)
				   (formula-decl? decl))
			       (if defs  ;;-only?
				   (if (eq defs 'explicit)
				       (const-decl? decl)  
				       (defn-decl? decl))
				   (or tccs?
				       (not (tcc? decl))))
			       (or (null current?)
				    (not (memq decl
					       exclude-current)))
			       (not (member (id decl)
					    exclude
					    :test #'same-id)))
			 collect
			 (let ((name (mk-name (id decl)
				       (actuals theory-name)
				       (id theory-name))
				       ))
			   (if (eq always? '!!)
			       (list (list name))
			       (if always? (list name) name)))))
		  (exclude-names
		   (loop for name in exclude
			 collect
			 (mk-name (id name)
			   (actuals theory-name)
			   (id theory-name))))
		  (rule `(auto-rewrite ,@names))
		  (stop-rule (if exclude-names
				 `(stop-rewrite ,@exclude-names)
				 '(skip))))
	      (then rule stop-rule))
	    (if current?
		(skip-msg "Current theory cannot be given actuals.")
		(skip-msg "Need theory actuals to install rewrites.")))
	(skip-msg "No such theory in current context.")))
	 "Installs an entire theory (or only (explicit) definitions
if DEFS is T(EXPLICIT)) as auto-rewrites.  In the case of a parametric
theory, unless the DEFS flag is T or EXPLICIT, the actual parameters
must be given.  If ALWAYS? is T the rewrites are installed also
so that any rewrite other than a recursive definition always
takes effect (see auto-rewrite!).  (Added 2.10.97: If ALWAYS? is
set to !!, then the non-recursive definitions are always rewritten
even when only a few of the curried arguments have been provided.)
Declarations named in EXCLUDE are not introduced and any current
rewrite rules in the EXCLUDE list are disabled. 
By default, TCCs in the theory are excluded but they can be included
when the TCC? flag is T. 
Examples:
 (auto-rewrite-theory \"sets\" :exclude (\"union\" \"subset?\")
                      :defs explicit)
 (auto-rewrite-theory \"lists[nat]\" :always? T)
 (auto-rewrite-theory \"connectives\" :always? !!)."
	 "Rewriting relative to the theory: ~a")

(defstep auto-rewrite-theories (&rest theories)
  (if (null theories)
      (skip)
      (let ((theory (car theories))
	    (rule (if (consp theory)
		      `(auto-rewrite-theory ,@theory)
		      `(auto-rewrite-theory ,theory)))
	    (rest (cdr theories)))
	(then
	 (try rule (skip)
	      (let ((string (format nil "Warning: Auto-rewrite-theory on ~a failed."
			      theory)))
		(skip-msg string :force-printing? T)))
	 (auto-rewrite-theories$ :theories rest))))
  "Iterated version of auto-rewrite-theory.  Each entry in
THEORIES must either be a theory name or a list in the form of
the arguments to AUTO-REWRITE-THEORY. 
E.g.: (auto-rewrite-theories \"real_props\" (\"sets\" :defs explicit))."
  "Installing rewrites from theories: ~{~% ~a~}")

(defstep auto-rewrite-theory-with-importings
  (name &optional exclude-theories importchain? exclude defs
	always? tccs?) 
  (let ((name (pc-parse name 'modname))
	(theory-name (resolve-theory-name name))
	(exclude-theories (listify exclude-theories))
	(exclude-theory-names
	 (mapcar #'(lambda (x) (pc-parse x 'modname))
	   exclude-theories))
	(theory (get-theory theory-name))
	(usings (if importchain?
		    (apply #'append
		      (mapcar #'cdr (all-usings theory)))
		    (immediate-usings theory)))
	(included-usings
	 (loop for z in (cons theory-name usings)
	       unless (member z exclude-theory-names
				 :test
				 #'(lambda (u v)
				     (if (actuals v)
					 (ps-eq u v)
					 (same-id u v))))
	       collect z))
	(theories
	 (loop for x in included-usings
	       collect
	       (list x :exclude exclude :defs defs :always? always? :tccs? tccs?))))
    (auto-rewrite-theories$ :theories theories))
  "Installs rewrites in theory NAME and along with any theories
imported by NAME.  The full import chain of theories can be
installed by supplying the IMPORTCHAIN? flag as T.  Theories named
in EXCLUDE-THEORIES are ignored.  The other arguments are similar
to those of auto-rewrite-theory and apply uniformly to each of
the theories to be installed."
  "Rewriting with ~a and imported theories therein")

(defstep stop-rewrite-theory (&rest theories)
  (if (consp theories)
      (let ((theory (car theories))
	    (rest-theories (cdr theories))
	    (theory-name (typecheck (pc-parse theory 'modname)
			      :context *current-context*))
	    (module (get-theory theory-name))
	    (decls (append (assuming module)(theory module)))
	    (names (loop for decl in
			 decls
			 when (typep decl '(or formula-decl
					       const-decl def-decl))
			 collect
			 (format nil "~a.~a" theory-name
				 (id decl)))))
	(then (stop-rewrite :names names)
	      (stop-rewrite-theory :theories rest-theories)))
      (skip))
  "Turns off rewriting for entire theories given their names.
If THEORIES contains a generic theory, this turns off the
generic definitional rewrite rules installed by AUTO-REWRITE,
AUTO-REWRITE-THEORY, or AUTO-REWRITE-THEORIES. E.g.,
 (stop-rewrite-theory \"real_proofs\" \"sets[nat]\" \"sequences\")."
  "~%Turning off automatic rewriting for theories: ~{~%   ~a~}")


(defhelper subtype-tcc ()
  (tcc$ explicit)
  "The strategy used for subtype TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper termination-tcc ()
  (tcc$ !)
  "The strategy used for termination TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper existence-tcc ()
  (tcc$ explicit)
  "The strategy used for existence TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper assuming-tcc ()
  (tcc$ explicit)
  "The strategy used for assuming TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper cases-tcc ()
  (tcc$ explicit)
  "The strategy used for cases TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper well-founded-tcc ()
  (tcc$ explicit)
  "The strategy used for well-founded TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper same-name-tcc ()
  (tcc$ explicit)
  "The strategy used for same-name TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper cond-disjoint-tcc ()
  (tcc$ explicit)
  "The strategy used for cond-disjoint TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defhelper cond-coverage-tcc ()
  (tcc$ explicit)
  "The strategy used for cond-coverage TCCs"
  "Trying repeated skolemization, instantiation, and if-lifting")

(defstep tcc (&optional (defs !))
  (grind$ :defs defs)
  "The guts of the tcc-strategy defined as (GRIND :DEFS DEFS).
Does auto-rewrite-explicit, then applies skolem!, inst?, lift-if,
bddsimp, and assert, until nothing works.  DEFS is either
 NIL: The definitions in the statement are not installed as auto-rewrites
 T: All defns are installed as conditional rewrites
 !: All defns are installed, but with explicit defns as
    unconditional rewrites
 explicit: Only explicit defns installed as conditional rewrites
 explicit!: Only explicit defns installed as unconditional rewrites."
    "Trying repeated skolemization, instantiation, and if-lifting")

;;NSH(12.15.94): For backward compatibility.
(defstep tcc-bdd (&optional (defs !))
  (grind$ :defs defs)
  "Obsolete - subsumed by (TCC)."
  "Trying repeated skolemization, instantiation, and if-lifting")

(defstep bash (&optional (if-match T)(updates? T) polarity? (instantiator inst?))
  (then (assert)(bddsimp)
	(if if-match (let ((command (generate-instantiator-command
				     if-match polarity? instantiator)))
		       command)(skip))
	(repeat (then (skolem-typepred)(flatten)))
	(lift-if :updates? updates?))
  "Core of REDUCE.  Does ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, 
FLATTEN, and LIFT-IF once and in that order.  The IF-MATCH option
can be NIL, T, ALL, or BEST for no, some, all, or the best instantiation.
If the UPDATES? option is NIL, update applications are not if-lifted.
When the POLARITY? flag is T, INST? matches templates against
complementary subexpressions.
The INSTANTIATOR argument can be used to specify use of an alternative
instantiation mechanism.  This defaults to the (INST?) strategy."
  "Simplifying with decision procedures, rewriting, propositional
reasoning, quantifier instantiation, skolemization, if-lifting.")

(defun generate-instantiator-command (if-match polarity? instantiator &optional fnum)
  (let* ((instcmd (if (consp instantiator)
		      (car instantiator)
		      instantiator))
	 (givenargs (when (listp
			   instantiator)
		      (cdr instantiator)))
	 (okkeys (or (assq instcmd *prover-keywords*)
		     (format t "~a is not a valid prover command" instcmd)
		     (restore)))
	 (okargs (cddr okkeys))
	 (instargs (append (when (and (memq :if-match okargs)
				      (not (memq :if-match givenargs)))
			     (list :if-match if-match))
			   (when (and (memq :polarity? okargs)
				      (not (memq :if-match givenargs)))
			     (list :polarity? polarity?))))
	 (fnum    (when fnum (list fnum)))
	 (command (append (list instcmd) fnum givenargs instargs)))
    (if (check-arguments command)
	command
	'(fail))))

	 
(defstep reduce (&optional (if-match T)(updates? T) polarity? (instantiator inst?))
    (repeat* (try (bash$ :if-match if-match :updates? updates?
			 :polarity? polarity? :instantiator instantiator)
               (replace*)
               (skip)))
"Core of GRIND (ASSERT, BDDSIMP, INST?, SKOLEM-TYPEPRED, FLATTEN,
LIFT-IF, i.e., BASH then REPLACE*) without reestablishing all the rewrites.
See BASH for more explanation."
"Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization,
 if-lifting and equality replacement")

(defstep smash (&optional (updates? T))
  (repeat* (then (bddsimp)(assert)(lift-if :updates? updates?)))
  "Repeatedly tries BDDSIMP, ASSERT, and LIFT-IF.  If the UPDATES?
option is NIL, update applications are not if-lifted."
  "Repeatedly simplifying with BDDs, decision procedures, rewriting,
and if-lifting")

(defstep install-rewrites (&optional defs theories rewrites
				     exclude-theories exclude)
  (then 
   (if (eq defs '!!)
       (auto-rewrite-defs :always? !!)
       (if (eq defs '!)
	   (auto-rewrite-defs :always? T)
	   (if (eq defs 'explicit)
	       (auto-rewrite-defs :explicit? T)
	       (if (eq defs 'explicit!!)
		   (auto-rewrite-defs :always? !! :explicit? T)
		   (if (eq defs 'explicit!)
		       (auto-rewrite-defs :always? T :explicit? T)
		       (if defs (auto-rewrite-defs) (skip)))))))
   (auto-rewrite-theories :theories theories)
   (auto-rewrite :names rewrites)
   (if exclude-theories
       (stop-rewrite-theory :theories exclude-theories)
       (skip))
   (if exclude (stop-rewrite :names exclude)(skip)))
  "Sets up auto-rewrites from definitions of operators in the statement,
THEORIES and REWRITES, and stops rewriting on EXCLUDE-THEORIES
and EXCLUDE.   DEFS is either
   NIL:  defns in the statement are not installed as auto-rewrites
   T: All defns are installed as conditional rewrites
   !: All defns are installed, but with explicit defns as
      unconditional rewrites
   !!: All defns are installed, but with explicit defns as
       unconditional rewrites even when partially applied
   explicit: Only explicit defns installed as conditional rewrites
   explicit!: Only explicit defns installed as unconditional rewrites
   explicit!!: Only explicit defns installed as !! rewrites.
THEORIES is a list of theories to be rewritten in the format expected by
          AUTO-REWRITE-THEORY.
REWRITES is a list of rewrite rules definitions/lemmas/assumptions
in the format expected by the AUTO-REWRITE rule.
EXCLUDE-THEORIES is a list of theories.
EXCLUDE is a list of rewrite rules. "
  "Installing rewrite rules from~
~@[~% definitions (~a) in the sequent~]~
~@[,~% theories: ~a~]~
~@[,~% rewrites: ~a~]~
~@[,~%and excluding theories: ~a~]~
~@[,~%and excluding rewrites: ~a~]")

(defstep grind (&optional (defs !); NIL, T, !, explicit, or explicit!
			  theories
			  rewrites
			  exclude
			  (if-match T)
			  (updates? T)
			  polarity?
			  (instantiator inst?))
  (then
   (install-rewrites$ :defs defs :theories theories
		     :rewrites rewrites :exclude exclude)
    (then (bddsimp)(assert))
    (replace*)
    (reduce$ :if-match if-match :updates? updates?
	     :polarity? polarity? :instantiator instantiator))
    "A super-duper strategy.  Does auto-rewrite-defs/theories,
auto-rewrite then applies skolem!, inst?, lift-if, bddsimp, and
assert, until nothing works. Here
 DEFS is either
   NIL:  defns in the statement are not installed as auto-rewrites
   T: All defns are installed as conditional rewrites
   !: All defns are installed, but with explicit defns as
      unconditional rewrites
   explicit: Only explicit defns installed as conditional rewrites
   explicit!: Only explicit defns installed as unconditional rewrites.
 THEORIES is a list of theories to be rewritten in the format expected 
          by AUTO-REWRITE-THEORY,
 REWRITES is a list of rewrite rules in AUTO-REWRITE format,
 EXCLUDE is a list of rewrite rules to be disabled (by STOP-REWRITE),
 IF-MATCH is either NIL (no instantiation), T (yes instantiation),
     ALL (all instances) or BEST (best instance) depending on which version
     of INST? is required.
 If the UPDATES? option is NIL, update applications are not if-lifted.
 POLARITY? if T, a positively occurring template is only matched against
     negatively occuring subexpressions, and less-than term occurrences
     are matched against greater-than occurrences.
The INSTANTIATOR argument can be used to specify use of an alternative
instantiation mechanism.  This defaults to the (INST?) strategy."
    "Trying repeated skolemization, instantiation, and if-lifting")


(defstep simplify-with-rewrites
  (&optional (fnums *) defs theories rewrites exclude-theories exclude)
  (then (install-rewrites :defs defs :theories theories
			  :rewrites rewrites
			  :exclude-theories exclude-theories
			  :exclude exclude)
    (assert fnums)
    (let ((decls (collect-referenced-decls
		  (declaration *top-proofstate*)
		  *ps*
		  (memq defs '(explicit explicit!))
		  nil))
	  (names (loop for decl in decls
		       collect (mk-name-expr (id decl) nil (id (module decl))))))
      (stop-rewrite :names names))
    (let ((theories (if (listp theories) theories (list theories)))
	  (theories (loop for entry in theories
			  collect
			  (if (consp entry)
			      (car entry)
			      entry))))
      (stop-rewrite-theory :theories theories))
    (stop-rewrite :names rewrites))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums), and
then turns off all the installed rewrites.  Examples:
 (simplify-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (simplify-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, and disabling installed rewrites")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The query strategy is the basic interactive strategy where the user
;; is alway queried.

(defun query*-step ()  '(if *proving-tcc* (quit)(query*)))

(defstrat query* ()
  (if (or *proving-tcc* *in-apply*)
      (postpone)
    (let ((input (let ((input (qread "Rule? ")))
		     (setf (current-input *ps*)
			   input)
		     input))
	    (rule (retypecheck-sexp
		   (unformat-rule input)
		   *ps*)))
	(try rule (query*) (query*))))
  "The basic strategy that queries the user for the next step.")


;;The else strategy

(defstrat else (step1 step2)
  (try step1 (skip) step2)
  "If step1 fails, then try step2, otherwise behave like step1" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;repeat strategy

(defstrat repeat (step)
  (try step (if (equal (get-goalnum *ps*) 1)
			  (repeat step)
			  (skip))
    (skip))
  "Successively apply STEP along main branch until it does nothing.")

(defstrat repeat* (step)
  (try step (repeat* step) (skip))
  "Successively apply STEP until it does nothing.")

(defstep prop ()
  (try (flatten) (prop$) (try (split)(prop$) (skip)))
  "A black-box rule for propositional simplification."
  "Applying propositional simplification")

(defrule quit ()
  (lisp (throw 'quit nil))
  "Terminates the proof attempt."
  "Quitting proof.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstrat then (&rest steps)
  (let ((x (when steps (car steps)))
	(y (when steps (cons 'then (cdr steps))))
	(z steps))
    (if steps (if (cdr steps)(try x y y) x) (skip)))
  "Successively applies the steps in STEPS.")

(defstrat then* (&rest steps)
  (then :steps steps)
  "Obsolete -see (THEN).")


(defstrat spread (step steplist)
  (try step
       (let ((goalnum (get-goalnum *ps*))
	     (par-ps (parent-proofstate *ps*))
	     ;;no dummy as opposed to spread@.
	      (x (if (consp steplist)
			  (nth (1- goalnum)
			       steplist)
			  '(skip))))
	       x)
    (let ((x (if (consp steplist)(car steplist) '(postpone T)))) x))
  "Applies STEP and then pairs the steps in STEPLIST with the subgoals")

(defstrat spread@ (step steplist)
  (try step
       (let ((goalnum (get-goalnum *ps*))
	     (par-ps (parent-proofstate *ps*))
	     (dummy
	      (when (eql goalnum 1)
		(cond ((> (1+ (length (remaining-subgoals par-ps)))
			  (length steplist))
		       (format t "~%***Warning: ~
Fewer subproofs (~s) than subgoals (~s)"
			 (length steplist)
			 (1+ (length (remaining-subgoals par-ps)))))
		      ((< (1+ (length (remaining-subgoals par-ps)))
			  (length steplist))
		       (format t "~%***Warning: ~
Fewer subgoals (~s) than subproofs (~s)"
			 (1+ (length (remaining-subgoals par-ps)))
			 (length steplist))))))
	      (x (if (consp steplist)
			  (nth (1- goalnum)
			       steplist)
			  '(skip))))
	       x)
    (let ((x (if (consp steplist)(car steplist) '(postpone T)))) x))
  "Like SPREAD, applies STEP and then pairs the steps in STEPLIST with
the subgoals, but generates warnings if number of subgoals do not match
the number of subproofs.")



(defstrat spread! (step steplist)
  (try (try  step
	     (let ((goalnum (get-goalnum *ps*))
		   (par-ps (parent-proofstate *ps*))
		   (mismatch?
		    (when (eql goalnum 1)
		      (cond ((> (1+ (length (remaining-subgoals par-ps)))
				(length steplist))
			     (format t "~%***Error: ~
Fewer subproofs (~s) than subgoals (~s)"
			       (length steplist)
			       (1+ (length (remaining-subgoals par-ps))))
			     T)
			    ((< (1+ (length (remaining-subgoals par-ps)))
				(length steplist))
			     (format t "~%***Error: ~
Fewer subgoals (~s) than subproofs (~s)"
			       (1+ (length (remaining-subgoals par-ps)))
			       (length steplist))
			     T))))
		   (x (if (consp steplist)
			  (nth (1- goalnum)
			       steplist)
			  '(skip))))
	       (if mismatch? (fail) x))
	     (query*))
       (query*)
       (query*))
  "Like SPREAD, applies STEP and then pairs the steps in STEPLIST with
the subgoals, but generates an error and queries the user if the number
of subgoals do not match the number of subproofs.")

(defstrat branch (step steplist)
  (try step (let ((x (if (consp steplist)
			  (nth (1- (min (length steplist)
					(get-goalnum *ps*)))
			       steplist)
			  '(skip))))
	       x)
    (let ((x (if (consp steplist)(car steplist) '(skip)))) x))
  "Like SPREAD, applies STEP then pairs steps
in STEPLIST with the resulting subgoals, using the last step in STEPLIST
for any excess subgoals.  If STEP does nothing, the first step in STEPLIST
is applied.")

(defstrat try-branch (step steplist else-step)
  (try step (let ((x (if (consp steplist)
			  (nth (1- (min (length steplist)
					(get-goalnum *ps*)))
			       steplist)
			  '(skip))))
	       x)
    else-step)
  "Like BRANCH, tries STEP and then pairs the steps in STEPLIST to the
resulting subgoals.  The last step is used for any excess subgoals.
If STEP does nothing, then ELSE-STEP is applied.")

(defstep ground ()
  (try (flatten)(ground$)(try (split)(ground$)(assert)))
  "Does propositional simplification followed by the use of decision procedures."
  "Applying propositional simplification and decision procedures")


(defstrat rerun (&optional proof recheck? break?)
  (let ((x (rerun-step (cond  ((null proof)
				(justification *ps*))
			       ((null (check-edited-justification proof))
				(revert-justification proof))
			       (t (error-format-if "~%Given proof is not well-formed")
				  '(skip)))
			recheck?
			break?)))
    x)
  "Strategy to rerun existing or supplied proof. The RECHECK? flag when T and
used to rerun an entire proof causes an expanded proof using only primitive
proof steps to be rerun."
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstep skosimp (&optional (fnum *) preds?)
  (then (if preds? (skolem-typepred fnum)
	    (skolem! fnum))
	(flatten))
  "Skolemize (with typepreds on skolem constants when PREDS? is T)
then disjunctively simplify.  "
  "Skolemizing and flattening")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep skosimp* (&optional preds?)
  (repeat (then (if preds? (skolem-typepred)(skolem!))
		(flatten)))
  "Repeatedly Skolemizes (with typepreds on skolem constants when PREDS?
is T) and disjunctively simplifies."
  "Repeatedly Skolemizing and flattening")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;simple induction strategy

(defun predtype? (type)
  (let ((type (find-supertype type)))
    (and (funtype? type)
	 (tc-eq (find-supertype (range type)) *boolean*))))

(defun upfrom-res ()
  (car (resolve (mk-type-name '|upfrom| nil '|integers|) 'type nil)))

(defun below-res ()
  (car (resolve (mk-type-name '|below| nil '|naturalnumbers|) 'type nil)))

(defun upto-res ()
  (car (resolve (mk-type-name '|upto|  nil '|naturalnumbers|) 'type nil)))

(defun subrange-res ()
  (car (resolve (mk-type-name '|subrange| nil '|integers|) 'type nil)))

(defun above-res ()
  (car (resolve (mk-type-name '|above| nil '|integers|) 'type nil)))

(defun upfrom? (type)
  (let ((subst (match! (type-value (declaration (upfrom-res)))
		      type nil nil)))
    (when (not (eq subst 'fail))
      (cdr (assoc '|i| subst :test #'same-id)))))

(defun upto? (type)
  (let ((subst (match! (type-value (declaration (upto-res)))
		      type nil nil)))
    (when (not (eq subst 'fail))
      (cdr (assoc '|i| subst :test #'same-id)))))

(defun below? (type)
  (let ((subst (match! (type-value (declaration (below-res)))
		      type nil nil)))
    (when (not (eq subst 'fail))
      (cdr (assoc '|i| subst :test #'same-id)))))

(defun subrange? (type)
  (let ((subst (match! (type-value (declaration (subrange-res)))
		      type nil nil)))
    (when (not (eq subst 'fail))
      (cons (cdr (assoc '|i| subst :test #'same-id))
	    (cdr (assoc '|j| subst :test #'same-id))))))

(defun above? (type)
  (let ((subst (match! (type-value (declaration (above-res)))
		      type nil nil)))
    (when (not (eq subst 'fail))
      (cdr (assoc '|i| subst :test #'same-id)))))

(defun bounded-int-type? (type)
  (and (subtype? type)
       (or (when (tc-eq type *naturalnumber*)
	     (intern
		(format nil
		    "naturalnumbers.nat_induction")))
	(let ((upfrom? (upfrom? type)))
	     (when upfrom?
	       (intern
		(format nil
		    "bounded_int_inductions[~a].upfrom_induction"
		  upfrom?))))
	   (let ((upto? (upto? type)))
	     (when upto?
	       (intern
		(format nil
		    "bounded_nat_inductions[~a].upto_induction"
		  upto?))))
	   (let ((below? (below? type)))
	     (when below?
	       (intern
		(format nil
		    "bounded_nat_inductions[~a].below_induction"
		  below?))))
	   (let ((above? (above? type)))
	     (when above?
	       (intern
		(format nil
		    "bounded_int_inductions[~a].above_induction"
		  above?))))
	   (let ((subrange? (subrange? type)))
	     (when subrange?
	       (intern
		(format nil
		    "subrange_inductions[~a,~a].subrange_induction"
		  (car subrange?)
		  (cdr subrange?)))))
	   (bounded-int-type? (supertype type)))))

(defun get-induction-domain-type (induction-name actual-type actual-var)
  (if induction-name  
      (let* ((name-expr (pc-parse induction-name 'name))
	     (resolutions (resolve name-expr 'formula nil *current-context*)))
	(if (singleton? resolutions)
	    (let ((forms (create-formulas (car resolutions))))
	      (if (and (singleton? forms)
		       (forall-expr? (car forms))
		       (predtype? (type (car (bindings (car forms))))))
		  (let ((type (domain (type (car (bindings (car forms)))))))
		    (if (compatible? type actual-type)
			(if (fully-instantiated? type)
			    type
			    (instantiate-from type actual-type actual-var))
			(progn
			  (error-format-if
			   "Induction predicate type: ~a~%~
                            does not match type of variable: ~a"
			   type actual-type)
			  nil)))
		  actual-type))
	    actual-type))
      (if (compatible? actual-type *naturalnumber*)
	  (when (tc-eq (compatible-type actual-type *naturalnumber*)
		       *naturalnumber*)
	    *naturalnumber*)
	  (let ((supertype (find-supertype actual-type)))
	    (when (adt? supertype)
	      supertype)))))

(defstep simple-induct (var fmla &optional name)
  (let ((var (pc-parse var 'expr));;get var name-expr
	(fmla (typecheck (pc-parse fmla 'expr);;typecheck fmla
		:expected *boolean*)))
    (if (and fmla;;if fmla is sensible
	     (forall? fmla)
	     (member var (bindings fmla)
		     :test #'(lambda (x y)
			       (or (format-equal x (id y))
				   (format-equal x
						 (symbol-prefix (id y)))))))
	(let ((actual-var  (or (find var (bindings fmla)
				     :test #'(lambda (x y)(format-equal x (id y))))
			       (find var (bindings fmla);;get var in fmla
				     :test
				     #'(lambda (x y)
					 (format-equal
					  x (symbol-prefix (id y)))))))
	      (remaining-vars (remove actual-var (bindings fmla)
				      :test #'eq)));;get non-var vars
	  (if (not (null (freevars (type actual-var))))
	      ;;type of actual-var should not contain
	      ;;free occurrences of other bound vars.
	      (skip-msg "Type of induction variable contains bound variables.")
	      (let ((body;;make body of induction predicate
		     (if remaining-vars
			 (lcopy fmla 'bindings
				remaining-vars)
			 (expression fmla)))
		    (actual-type (type actual-var));;type/supertype
		    (actual-supertype;;of actual-var
		     (find-declared-adt-supertype actual-type))
		    ;;NSH(2.16.97): was find-supertype
		    (induction-name  
		     (if name name;;get induction-name from
			 ;;given name or actual-(super)type
			 (or (bounded-int-type? actual-type)
			     (if (adt?  actual-supertype)
				 (intern (format nil "~a.~a_induction"
					   (module-instance (resolution
							     actual-supertype))
					   (id actual-supertype)))
				 (if (compatible? actual-type
						  *naturalnumber*)
				     (if (tc-eq  (compatible-type actual-type
								  *naturalnumber*)
						 *naturalnumber*)
					 '|nat_induction|
					 nil)
				     nil)))))
		    (type;;get domain type for induction predicate
		     (get-induction-domain-type induction-name actual-type actual-var))
		    (new-bound-var
		     (if (and type (not (tc-eq type actual-type)))
			 (make-bind-decl (id actual-var) type)
			 actual-var))
		    (new-var (make-variable-expr new-bound-var))
		    (subtype-constraints
		     (if (not (eq new-bound-var actual-var))
			 (compatible-preds type actual-type new-var)
			 nil));;compatible? has been checked above.
		    (predicate
		     (when type
		       (if subtype-constraints
			   (let* ((new-body
				   (make-implication
				    (make-conjunction subtype-constraints)
				    (substit body
				      (acons actual-var new-var nil)))))
			     (make-lambda-expr (list new-bound-var)
			       new-body))
			   (make-lambda-expr (list actual-var) body)))))
		(if induction-name
		    (if predicate
			(let ((rule `(then (lemma ,induction-name)(inst -1 ,predicate))))
			  rule)
			(skip-msg "Could not construct induction predicate"))
		    (skip-msg "Given variable does not have type natural number or datatype.")))))
	(skip-msg "Formula does not universally quantify given variable.")))
  "Selects an induction scheme according to the type of VAR in FMLA and uses FMLA
 to formulate  an induction predicate.  The name of the induction
formula can be explicitly supplied using the optional NAME argument.
See also INDUCT."
  "Inducting on ~a with formula ~a")
 
(defstep induct (var &optional (fnum 1) name)
  (let ((fmla (let* ((sforms (select-seq (s-forms (current-goal *ps*))
					 (list fnum))))
		(when sforms
		  (formula (car sforms)))))
	(var (pc-parse var 'name))
	(new-var-symbol (new-sko-symbol var *current-context*))
	(skolem-list (if (forall? fmla)
			 (loop for x in (bindings fmla)
			       collect (if (format-equal var (id x))
					   new-var-symbol
					   "_"))
			 nil)))
    (if fmla
	(try (simple-induct var fmla name)
	     (if *new-fmla-nums*  ;;NSH(4.25.97) record fnum here
		 (let ((fnum (find-sform (s-forms (current-goal *ps*))
					 '+
					 #'(lambda (sform)
					     (eq (formula sform)
						 fmla)))))
		   (then (beta)
			 (let ((fmla ;;NSH(4.25.97) record fmla here
				(let ((sforms (select-seq
					       (s-forms (current-goal *ps*))
					       (list fnum))))
				  (when sforms (formula (car sforms))))))
			   (then (let ((x (car *new-fmla-nums*)))
				   (then (inst? x)
					 (split x)))
				 (let ((num (find-sform
					     (s-forms (current-goal *ps*))
					     '+
					     #'(lambda (sform)
						 (eq (formula sform)
						     fmla)))))
				   (if (eql num fnum)
				       (then (prop)
					     (skolem fnum skolem-list)
					     (inst - new-var-symbol)
					     (prop))
				       (if num (delete num)
					   (let ((newnums
						  (loop for n
							in *new-fmla-nums*
							when (and (> n 0)
								  (<= n fnum))
							collect n))
						 (newfnum (+ fnum
							     (length newnums))))
					     (delete newfnum)))))))))
		 (skip))
	     (skip-msg "Could not find suitable induction scheme."))
	(let ((msg (format nil "No formula corresponding to fnum ~a"
		     fnum)))
	  (skip-msg msg))))
  "Selects an induction scheme according to the type of VAR in FNUM and uses
formula FNUM to formulate an induction predicate, then simplifies yielding base
and induction cases.  The induction scheme can be explicitly supplied
as the optional NAME argument.
 (induct \"i\"): If i has type nat and occurs outermost universally quantified
   in formula FNUM, the nat_induction scheme is instantiated with a predicate
   constructed from formula FNUM, and beta-reduced and simplified to yield base
   and induction subcases.  If i has type that is a datatype, then the
   induction scheme for that datatype is used by default.
 (induct \"x\" :fnum 2 :name \"below_induction[N]\"): The below_induction
   scheme is instantiated with an induction predicate constructed from fnum 2."
  "Inducting on ~a~@[ on formula ~a~]~@[ using induction scheme ~a~]")

(defstep induct-and-simplify (var &optional (fnum 1) name
				  (defs T)
				  (if-match best)
				  theories
				  rewrites
				  exclude
				  (instantiator inst?)
				  )
  (then
   (install-rewrites$ :defs defs :theories theories
		      :rewrites rewrites :exclude exclude)
   (try (induct var fnum name)
	(then
	 (skosimp*)
	 (assert);;To expand the functions in the induction conclusion
	 (repeat (lift-if));;To lift the embedded ifs,
	 ;;then simplify, split, then instantiate
	 ;;the induction hypothesis.  
	 (repeat* (then (assert)
			(bddsimp)
			(skosimp*)
			(if if-match
			    (let ((command (generate-instantiator-command if-match nil instantiator)))
			      command)
			    (skip))
			(lift-if))))
	(skip)))
  "Inducts on VAR in formula number FNUM using the induction
scheme named NAME, then simplifies using rewrite rules taken
from THEORIES and REWRITES.
 DEFS is either
   NIL:  defns in the statement are not installed as auto-rewrites
   T: All defns are installed as conditional rewrites
   !: All defns are installed, but with explicit defns as
      unconditional rewrites
   explicit: Only explicit defns installed as conditional rewrites
   explicit!: Only explicit defns installed as unconditional rewrites.
 IF-MATCH is either all, best, or T, as in INST?,
             or NIL meaning don't use INST?.
 THEORIES is a list of theories to be rewritten in format expected by
          AUTO-REWRITE-THEORY,
 REWRITES is a list of rewrite rules in AUTO-REWRITE format.
 EXCLUDE is a list of rewrite rules on which rewriting must be stopped.
The INSTANTIATOR argument can be used to specify use of an alternative
instantiation mechanism.  This defaults to the (INST?) strategy.

  (induct-and-simplify \"i\" :defs ! :theories \"real_props\"
               :rewrites \"assoc\" :exclude (\"div\_times\" \"add\_div\")):
    If i has type nat, then the natural number induction
    scheme is instantiated with a predicate constructed from sequent
    formula 1, and the resulting cases are simplified using
    definitions in the given sequent (unconditionally expanding
    explicit definitions), the rewrites in the  prelude theory 
    real_props but excluding div_times and add_div,
    and the rewrite rule assoc."
  "By induction on ~a, and by repeatedly rewriting and simplifying")
  
(defstep induct-and-rewrite (var &optional (fnum 1)  &rest rewrites)
  (induct-and-simplify$ var fnum :defs nil :rewrites rewrites) 
  "Performs induction according to the type of VAR using FNUM
to formulate induction predicate, and then simplifies using the given 
REWRITES.
 (induct-and-rewrite \"x\" 1 \"append\" \"reverse\"): Inducts on x in formula 1,
   then simplifies the base and induction using the definitions append and
   reverse. "
  "By induction on ~a and rewriting")

(defstep induct-and-rewrite! (var &optional (fnum 1)  &rest rewrites)
  (induct-and-simplify$ var fnum :defs ! :rewrites rewrites)
  "Performs induction according to the type of VAR using FNUM
to formulate induction predicate, and then simplifies using the given 
REWRITES while expanding all definitions.
 (induct-and-rewrite! \"x\"): Inducts on x then simplifies the base and
   induction using the definitions of functions appearing in the sequent."
  "By induction on ~a and rewriting")

(defstep name-induct-and-rewrite (var &optional (fnum 1) name
				     &rest rewrites)
  (induct-and-simplify$ var fnum name :rewrite rewrites)
  "Performs induction according to the type of VAR or the induction scheme
named NAME, using FNUM to formulate induction predicate, and then
simplifies using given REWRITES. "
  "By induction on ~a and rewriting")

(defun domain-compatible? (type1 type2)
  (if (and (consp type1)(tupletype? type2))
      (compatible? type1 (types type2))
      (if (and (tupletype? type1)(consp type1))
	  (compatible? (types type1) type2)
	  (compatible? type1 type2))))


(defstep simple-measure-induct (measure vars &optional  (fnum 1)
					order)
  (let ((fmla (let* ((sforms (select-seq (s-forms (current-goal *ps*))
					  (list fnum))))
		 (when sforms
		   (formula (car sforms)))))
	(vars (if (consp vars) vars (list vars))))
    ;;select induction formula and variables
    (if (and fmla
	     (forall? fmla)
	     (subsetp vars (bindings fmla)
		     :test #'(lambda (x y)(format-equal x (id y)))))
	;;if induction formula universally quantifies induction
	;;variables
	(let ((actual-vars  (loop for var in vars
				  collect
				  (find var (bindings fmla)
					:test
					#'(lambda (x y)
					    (format-equal x (id y))))))
	      (remaining-vars (loop for var in (bindings fmla)
				    when (not (memq var actual-vars))
				    collect var)))
	  (if (not (subsetp
		    (freevars (mapcar #'type actual-vars))
		    actual-vars
		    :test #'same-declaration))
	  (skip-msg "Type(s) of induction variable(s) contains bound variables.")
	  (let ((predicate
		 (let ((body
			(if remaining-vars
			    (lcopy fmla 'bindings
				   remaining-vars)
			    (expression fmla))))
		   (make-lambda-expr actual-vars body)))
		(domain-type (make-domain-type-from-bindings actual-vars))
		(measure (pc-parse measure 'expr))
		(measure (mk-lambda-expr actual-vars measure))
		(measure (typecheck measure))
		(measure-types (loop for typ in
				     (if (type measure)
					 (list (type measure))
					 (types measure))
				     when (and (funtype? typ)
					       (domain-compatible?
						(domain typ)
						domain-type))
				     collect typ))
		(range-type (if (member *naturalnumber* measure-types
					:test #'(lambda (x y)
						  (compatible? x (range y))))
				*naturalnumber*
				(if (member *ordinal* measure-types
					    :test #'(lambda (x y)
						      (compatible? x
								   (range
								    y))))
				    *ordinal*
				    (if measure-types
					;; SO 1997-06-25: took the range
					(range (car measure-types))
					nil))))
		(measure (when range-type ;;NSH(5.8.99)
			   (pc-typecheck measure
			     :expected (mk-funtype domain-type range-type))))
		(measure-freevars (when range-type (freevars measure)))
		(order-type (when range-type 
			      (make-predtype (make-tupletype
							 (list range-type
							       range-type)))))
		(order (when range-type
			 (if order
			     (typecheck (pc-parse order 'expr)
			       :expected order-type)
			     (typecheck (pc-parse
					 ;; CRW(8/8/94): changed <= to < in
					 ;; string below, to match new defn.
					 ;; of well_founded?
					 (format nil
					     "(LAMBDA (m, n: ~a): m<n)"
					   range-type)
					 'expr)
			       :expected order-type))))
		(induction-name
		 (when range-type 
		   (mk-name (intern "measure_induction")
			  (mapcar #'mk-actual (list domain-type range-type measure order)))))
		)
	    (if measure-freevars
		(let ((msg (format nil "Given measure contains free variables ~a" measure-freevars)))
		  (skip-msg msg))
		(if range-type
		(if induction-name
		    (if (tc-eq range-type *naturalnumber*)
			(let ((rule `(spread (lemma ,induction-name)
				      ((instantiate -1 ,predicate)
				       (then (lemma "naturalnumbers.wf_nat")
					(prop))))))
			  rule)
			(if (tc-eq range-type *ordinal*)
			(let ((rule `(spread (lemma ,induction-name)
				      ((instantiate -1 ,predicate)
				       (then (lemma "ordinals.well_founded_le")
					(prop))))))
			  rule)
			(let ((rule `(spread (lemma ,induction-name)
					     ((instantiate -1 ,predicate)))))
			  rule)))
		    (skip-msg "Given variable does not have type natural
number or datatype,"))
		(skip-msg "Given measure does not map to a natural number or
ordinal."))))))
	(skip-msg "Formula does not universally quantify given variable,")))
  "Selects and insert as an antecedent, an instance of measure
induction with measure MEASURE containing only free variables from
VARS using formula FNUM to formulate an induction predicate.
Uses ORDER as the well-founded relation; if not specified defaults
to < on nats or ordinals.

Example: (simple-measure-induct \"i+j\" (\"i\" \"j\")).
See also SIMPLE-MEASURE-INDUCT."  
  "Inducting with measure ~a on ~a")

(defhelper measure-induct (measure vars &optional (fnum 1) order)
  (try-branch (simple-measure-induct$ measure vars fnum order)
	      ((if *new-fmla-nums*
		   (branch
		    (then (beta)
			  (let ((x (car *new-fmla-nums*)))
			    (split x)))
		    ((then (let ((skoterms
				  (fill-up-terms fnum nil *ps*))
				 (bndvars (bindings
					   (formula
					    (car (select-seq
						  (s-forms
						   *goal*)
						  (list fnum))))))
				 (vars (if (consp vars) vars
					   (list vars))) 
				 (var-skoterms
				  (loop for var in vars
					collect
					(loop for bvar in bndvars
					      as skosymb in skoterms
					      thereis
					      (and (same-id bvar var)
						   skosymb))))
				 (var-skoterm
				  (if (singleton? var-skoterms)
				      (car var-skoterms)
				      (format nil "(~{~a~^,~})" var-skoterms)))
				 (rest-skoterms
				  (loop for bvar in bndvars
					as skosymb in skoterms
					when (not (member bvar vars
							  :test #'same-id))
					collect skosymb))
				 (inst-rule1 `(inst -
					      ,var-skoterm))
				 (inst-rule2 `(inst -
						    ,@rest-skoterms)))
			     (then (skolem fnum skoterms)
				   inst-rule1
				   inst-rule2
				   (beta)
				   (prop)
				   ))
			   (skip))
		     (let ((incfnum ;;NSH(5.23.99)
			    (length (loop for x in *new-fmla-nums*
					  when (and (> x 0)
						    (<= x fnum))
					  collect x)))
			   (delfnum (+ fnum incfnum)))
		       (then (delete delfnum)
			     (if (and (consp vars);;NSH(8.28.96)
				      (> (length vars) 1))
				 (detuple-boundvars 1 :singles? T)
				 (skip))))))
		   (skip))
	       (skip))
    (skip-msg "Could not find suitable induction scheme,"))
  "This is a helper strategy; use MEASURE-INDUCT+ instead."
  "Inducting on ~a")

(defstep measure-induct+ (measure vars &optional (fnum 1) order)
  (then (measure-induct$ measure vars fnum order)
	(skosimp)
	(let ((ihnum (if *new-fmla-nums*
			 (apply #'min *new-fmla-nums*)
			 0))
	      (ih (unless (>= ihnum 0)
		    (args1
		     (formula
		      (car (select-seq (s-forms *goal*) (lisp ihnum))))))))
	  (if ih
	      (let ((outerbvars (when (forall-expr? ih)
				  (bindings ih)))
		    (body (when (forall-expr? ih)
			    (expression ih)))
		    (measure-ineq (when (implication? body)
				    (args1 body)))
		    (innerbvars (when (and measure-ineq
					   (forall-expr? (args2 body)))
				  (bindings (args2 body))))
		    (innerbody (when innerbvars (expression (args2 body)))))
		(if innerbvars
		    (let 
			((new-outers (make-new-bindings outerbvars nil))
			 (new-measure-ineq (substit measure-ineq
					     (pairlis new-outers
						      outerbvars)))
			 (new-ih (make-forall-expr new-outers
				   (make-forall-expr innerbvars
				     (make-implication new-measure-ineq
						       innerbody)))))
		      (branch (case new-ih)
			      ((let ((ihfnum (1- ihnum)))
				 (hide ihfnum))
			       (let ((outer-skonames
				      (fill-up-terms 1 nil *ps*)))
				 (then (skolem 1 outer-skonames)
				       (instantiate ihnum
						    outer-skonames)
				       (let ((inner-skonames
					      (fill-up-terms 1 nil *ps*)))
					 (then
					  (skolem 1 inner-skonames)
					  (branch (split ihnum)
						  ((instantiate ihnum inner-skonames)
						   (skip)))
					  (prop))))))))
		    (skip)))
	      (skip))))
  "Uses MEASURE-INDUCT but recasts induction hypothesis in a more usable form,
i.e. (FORALL x, w: m(x) < m(y) IMPLIES p(x, w)) rather than
     (FORALL x: m(x) < m(y) IMPLIES (FORALL w: p(x, w))).
This form should always be preferred over MEASURE-INDUCT.

Suitably instantiates and uses the measure induction scheme in the
PVS prelude.  Selects an instance of measure induction with measure
MEASURE containing only free variables from VARS using formula FNUM to
formulate an induction predicate.   Uses ORDER as the well-founded
relation; if not specified defaults to < on nats or ordinals.
Simplifies out the result to yield the induction goal.

Example:  (measure-induct+ \"length(x) + length(y)\" (\"x\" \"y\"))."
  "Inducting on measure: ~a, ~% with variables: ~a" )

(defstep measure-induct-and-simplify
  (measure vars &optional (fnum 1) order expand (defs T)
	   (if-match best)
	   theories
	   rewrites
	   exclude
	   (instantiator inst?)
	   )
  (then
   (install-rewrites$ :defs defs :theories theories
		      :rewrites rewrites :exclude exclude)
   (try (measure-induct+$ measure vars fnum order)
	(then
	 (let ((expands
		(if (consp expand)
		    (loop for x in expand
			  collect `(expand ,x :fnum +))
		    `((expand ,expand :fnum +))))
	       (command `(then ,@expands)))
	   command)
	 (skosimp*)
	 (assert);;To expand the functions in the induction conclusion
	 (repeat (lift-if));;To lift the embedded ifs,
	 ;;then simplify, split, then instantiate
	 ;;the induction hypothesis.  
	 (repeat* (then (assert)
			(bddsimp)
			(skosimp*)
			(if if-match
			    (let ((command (generate-instantiator-command
					    if-match nil instantiator)))
			      command)
			    (skip))
			(lift-if))))
	(skip)))
  "Invokes MEASURE-INDUCT+ on MEASURE, VARS, FNUM, and ORDER, and then
repeatedly expands definition(s) taken from EXPAND in the succedent,
skolemizes, simplifies using rewrite rules taken from THEORIES and
REWRITES, instantiates, and lifts IFs.

 ORDER is a well-founded relation;
       if not specified defaults to < on nats or ordinals
 EXPAND is either a single definition name, or a list of names
 DEFS is either
   NIL:  defns in the statement are not installed as auto-rewrites
   T: All defns are installed as conditional rewrites
   !: All defns are installed, but with explicit defns as
      unconditional rewrites
   explicit: Only explicit defns installed as conditional rewrites
   explicit!: Only explicit defns installed as unconditional rewrites.
 IF-MATCH is either all, best, or T, as in INST?,
             or NIL meaning don't use INST?.
 THEORIES is a list of theories to be rewritten in format expected by
          AUTO-REWRITE-THEORY,
 REWRITES is a list of rewrite rules in AUTO-REWRITE format.
 EXCLUDE is a list of rewrite rules on which rewriting must be stopped.
 INSTANTIATOR argument can be used to specify use of an alternative
       instantiation mechanism.  This defaults to the (INST?) strategy.

Example:
    (measure-induct-and-simplify \"size(x)\" (\"x\") :expand \"unfold\" :if-match all)."
  "Invoking measure induction and simplifying")

(defstep replace-extensionality (f g &optional expected keep?)
  (let ((tt (when expected (typecheck (pc-parse expected 'type-expr)
				       :context *current-context*))))
    (let ((ff (pc-typecheck (pc-parse f 'expr)
			 :expected tt))
	  (gg (pc-typecheck (pc-parse g 'expr)
			 :expected tt)))
      (let ((tf (type ff))
	    (tg (type gg)))
	(try (if tt (extensionality tt)
		 (try (extensionality tf)(skip)
		      (extensionality tg)))
	     (branch (inst - ff gg)
		     ((branch (split -1)
			   ((then (replace -1)
				  (if keep? (skip)
				      (delete -1)))
			    (then* (skolem! 1);;NSH(5.19.95)
				   (beta 1);;changed from + to 1.
				   (assert 1))))
		      (assert)))
	     (skip)))))
  "Uses the extensionality axiom on the type of F (or with
EXPECTED as the type when given) to replace F by G.  Retains the
extensionality axiom scheme if KEEP? is T, and discards it otherwise.
See also EXTENSIONALITY, APPLY-EXTENSIONALITY."
  "Replacing ~a by ~a using extensionality")

(defstep apply-extensionality (&optional  (fnum +) keep? hide?)
  (let ((sforms (select-seq (s-forms (current-goal *ps*))
			     (if (memq fnum '(* + -)) fnum
				       (list fnum))))
	 (fmla (loop for sf in sforms thereis
		     (when (equation? (formula sf))
		       (formula sf))))
	 (lhs (when fmla (args1 fmla)))
	 (rhs (when fmla (args2 fmla))))
    (if fmla
	(try (replace-extensionality$  lhs rhs :keep? keep?)
	     (then
	      (let ((fnums (find-all-sformnums (s-forms
						(current-goal *ps*))
					       '+
					       #'(lambda (x)
						   (eq x fmla))))
		    (fnum (if fnums (car fnums) nil)))
		(if (and hide? fnum) (delete fnum) (skip)))
	      (assert))
	     (skip-msg "Couldn't find a suitable extensionality rule."))
	(skip-msg "Couldn't find suitable formula for applying extensionality.")))
  "Tries to prove an equality indicated by FNUM via extensionality.
If KEEP? is T, the equality is retained as an antecedent.
If HIDE? is T, the equality formula to which extensionality is applied,
is hidden.
See also EXTENSIONALITY."
  "Applying extensionality")

(defstep eta (type)
  (let ((type (typecheck (pc-parse type 'type-expr))))
    (if (adt? (find-supertype type))
	(if (subtype? type)
	    (if (recognizer? (predicate type))
		(let ((name (format nil "~a_~a_eta~@[[~a]~]"
			 (id (find-supertype type))
			 (id (constructor (predicate type)))
			 (actuals (module-instance (supertype type))))))
		  ;;NSH(2.25.97):was find-supertype but that
		  ;;now finds list[number] rather than list[nat].
		  ;;find-declared-adt-supertype is not needed
		  ;;since the supertype will be right anyway.
		  (lemma name))
		(if (datatype-subtype? type)
		    (skip-msg "Need a constructor subtype to generate an eta axiom.")
		    (let ((supertype
			   (supertype type)))
		      (eta supertype))))
	    (skip-msg "No relevant eta axiom for ~a"
		      type))
	(let ((eta-formula (make-eta-formula type)))
	  (if eta-formula
	      (try (extensionality type)
		   (branch (case eta-formula)
			  ((delete -2)
			  (then* (skolem! 1)
				 (inst? -1 :where 1)
				 (ground))))
		   (skip-msg "No suitable extensionality axiom for given type. "))
	      (skip-msg "No suitable eta formula for given type")))))
  "Introduces Eta version of extensionality axiom for given TYPE "
  "Introducing Eta axiom for type ~a")

(defmethod make-eta-formula ((type subtype))
  (make-eta-formula (supertype type)))

(defmethod make-eta-formula ((type recordtype))
  (let* ((var (generate-variable (gentemp "r_") type
				*current-context*))
	 (fields (fields type))
	 (assignments (loop for fld in fields
			    collect
			    (mk-assignment 'uni
			      (list (list (mk-field-name-expr
					   (id fld)
					   (resolution fld))))
			      (make-field-application
			       (id fld) var))))
	 (record-expr (make-record-expr assignments type)))
    (close-freevars (make-equality record-expr var) *current-context* (list var))))

(defmethod make-eta-formula ((type tupletype))
  (let* ((var (generate-variable (gentemp "u_") type
				*current-context*))
	 (lhs 
	  (make-tuple-expr
	       (loop for index from 1 to (length (types type))
		     collect
		     (make-projection-application index  var)))))
    (close-freevars (make-equality lhs var)
		    *current-context*
		    (list var))))

(defmethod make-eta-formula ((type funtype))
  (let* ((var (generate-variable (gentemp "u_") type
				*current-context*))
	 (bvar (generate-variable (gentemp "x_")
				  (domain type)
				*current-context*))
	 (lhs (quant-to-lambda
	       (close-freevars (make-application var bvar)
			       *current-context*
			       (list bvar)))))
    (close-freevars (make-equality lhs var) *current-context*
		    (list var))))

(defmethod make-eta-formula ((type type-expr))
  nil)

(defun pc-type (type)
  (with-slots (print-type) type
    (if print-type print-type type)))

(defstep replace-eta (term &optional type keep?)
  (let ((type (if type
		(typecheck (pc-parse type 'type-expr))
		nil))
	(term (if type
		  (pc-typecheck (pc-parse term 'expr)
		    :expected type)
		  (typecheck (pc-parse term 'expr))))
	(type (if type (pc-type type)
		  (if (type term) (pc-type (type term)) nil))))
    (if type
	(try (eta type)
	     (then (inst -1 term)
		   (replace -1)
		   (if keep? (skip)
		       (delete -1)))
	     (skip-msg "No suitable eta axiom scheme found."))
	(skip-msg "Please supply optional type argument.")))
  "Instantiates eta axiom scheme for type TYPE or type of TERM with TERM
See also ETA, APPLY-ETA."
  "Applying eta axiom scheme to ~a and does replace to eta-reduce")


(defstrat then@ (&rest steps)
  (if steps
    (let ((x (car steps))
	  (y (cdr steps))
	  (rest-step `(then@ ,@y)))
      (spread x (rest-step)))
    (skip))
  "Applies first step to the current goal and remaining
steps to the first subgoal, postponing remaining subgoals.")


(defstep case-replace (formula)
  (then@ (case formula)
	 (replace -1))
  "Case splits on a given FORMULA lhs=rhs and replaces lhs by rhs.
See also CASE, CASE*"
  "Assuming and applying ~a")

;;NSH(8.20.93): Strategy for timing proof steps.
(defstrat time (strat)
  (let ((init (get-internal-run-time)))
    (then strat
	  (let ((string
		 (format nil "~%~%Run time for ~s~%  = ~,2F secs."
			    strat
			    (/ (- (get-internal-run-time) init)
			       internal-time-units-per-second))))
	    (skip-msg string))))
  "Times a strategy from initial point to leaf nodes."
  "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(12.16.93): replace* strategy does LR replacement on fnums.

(defstep replace* (&rest fnums)
  (let ((fnums (find-all-sformnums (s-forms *goal*)
				   (if (null fnums) '* fnums)
				   #'always-true))
	(x `(then* ,@(loop for y in fnums collect `(replace ,y)))))
    x)
  "Apply left-to-right replacement with formulas in FNUMS."
  "Repeatedly applying the replace rule")
				   
(defstep skolem! (&optional (fnum *) keep-underscore? )
  (let ((sformnum (find-!quant fnum *ps*)))
    (let ((newterms (fill-up-terms sformnum nil *ps* keep-underscore?)))
      (skolem sformnum newterms)))
  "Skolemizes by automatically generating skolem constants.
When KEEP-UNDERSCORE? is T, a bound variable x_1 is replaced by skolem constant
x_1!n rather than x!n, for some number n."
  "Skolemizing") 

(defun check-inst-quant (fmla terms pos?)
  (let ((terms-length (length terms)))
    (or (zerop terms-length)
	(and (if pos? (exists-expr? fmla)(forall-expr? fmla))
	     (let ((bind-length  (length (bindings fmla))))
	       (or (eql bind-length terms-length)
		   (and (< bind-length terms-length)
			(let* ((rest-terms (nthcdr bind-length terms))
			       (current-terms (ldiff terms rest-terms)))
			  (and  (or (not
				     (some #'(lambda (x)
					       (or (and (symbolp x)
							(eq x '_))
						   (and (stringp x)
							(equal x "_"))
						   (and (name-expr? x)
							(eq (id x) '_))))
					   current-terms))
				    (error-format-if "~%Misplaced underscore in given term list."))
			       (check-inst-quant (expression fmla)
						 rest-terms
						 pos?))))))))))

(defstep instantiate-one (fnum terms &optional copy?)
  (else (try (instantiate fnum terms copy?)
	     (let ((new-sforms
		     (select-seq (s-forms (current-goal *ps*))
				 *new-fmla-nums*))
		    (rest (delete-seq (s-forms (current-goal *ps*))
				 *new-fmla-nums*)))
		(if (subsetp new-sforms rest
			       :test #'tc-eq
			       :key #'formula)
		    (then (skip-msg "Avoiding duplicate instantiation")(fail))
		  (skip)))
	     (skip))
	(skip))
  "Same as INSTANTIATE but treated as a SKIP if the instantiation would
introduce a duplicate formula."
  "~%Instantiating the top quantifier in ~a with the terms: ~% ~a,")

(defstep inst (fnum &rest terms)
  (let ((terms (if (listp terms) terms (list terms)))
	(fnum (find-sform (s-forms (current-goal *ps*)) fnum
			   #'(lambda (sform)
			      (if (negation? (formula sform))
				  (check-inst-quant (args1 (formula sform))
						    terms NIL)
				  (check-inst-quant (formula sform)
						    terms T)))))
	(bindings (let* ((sforms (select-seq (s-forms (current-goal *ps*))
					   fnum))
			(fmla (when sforms (formula (car sforms)))))
		    (when fmla
		      (if (negation? fmla)
			  (bindings (args1 fmla))
			  (bindings fmla))))))
    (if fnum
	(if (eql (length terms)(length bindings))
	    (instantiate-one$ fnum terms)
	    (if (< (length bindings)(length terms))
		(let ((current-terms (loop for x in terms as nil in bindings
					   collect x))
		      (remaining-terms (nthcdr (length bindings) terms)))
		  (try (instantiate-one$ fnum current-terms)
		       (inst fnum :terms remaining-terms)
		       (skip)))
		(skip-msg "Not enough terms given.")))
	(skip-msg "No quantified formula matching given number of terms.")))
  "Instantiates the top quantifier in formula FNUM. See INST-CP for copying
quantified formula." 
  "Instantiating the top quantifier in ~a with the terms: ~% ~{~a~^, ~}")

(defstep inst-cp (fnum &rest terms)
  (let ((terms (if (listp terms) terms (list terms)))
	(fnum (find-sform (s-forms (current-goal *ps*)) fnum
			   #'(lambda (sform)
			      (if (negation? (formula sform))
				  (check-inst-quant (args1 (formula sform))
						    terms NIL)
				  (check-inst-quant (formula sform)
						    terms T)))))
	(bindings (let* ((sforms (select-seq (s-forms (current-goal *ps*))
					   fnum))
			(fmla (when sforms (formula (car sforms)))))
		    (when fmla
		      (if (negation? fmla)
			  (bindings (args1 fmla))
			  (bindings fmla))))))
    (if fnum
	(if (eql (length terms)(length bindings))
	    (instantiate-one$ fnum terms T)
	    (if (< (length bindings)(length terms))
		(let ((current-terms (loop for x in terms as nil in bindings
					   collect x))
		      (remaining-terms (nthcdr (length bindings) terms)))
			  (try (instantiate-one$ fnum current-terms T)
			       	(let ((fnum
				       (if (minusp fnum)(1- fnum)(1+ fnum))))
				  (inst fnum :terms remaining-terms))
			       (skip)))
			(skip-msg "Not enough terms given.")))
	(skip-msg "No quantified formula matching given number of terms.")))
  "Instantiates the top quantifier in formula FNUM but retains a copy of the
quantified formula.
See INST for a non-copying version." 
  "Instantiating (with copying) the top quantifier in ~a with the terms:
~{~a~^, ~}")

(defstep inst? (&optional (fnums *) subst (where *)
		   copy? if-match polarity?)
  (let (;(sformnum (find-?quant fnum subst *ps*))
	 (sforms (remove-if-not
		     #'exists-sform?
		   (select-seq (s-forms (current-goal *ps*))
				 fnums)))
	           
	 (search (find-quant-terms sforms subst where
				     if-match polarity? *ps*))
         (sformnum (when search (car search)))
	 (newterms (when search (cdr search))))
    (if newterms
	(let ((x (make-inst?-rule sformnum newterms copy? if-match)))
	      x)
	(if  (all-or-best? if-match)
	    ;;try again with no polarity
	    (let ((dummy (format-if "~%Trying instantiation with ~
IF-MATCH set to NIL~%"))
		       (search (find-quant-terms sforms subst where
					    NIL polarity? *ps*))
		      (sformnum (when search (car search)))
		      (newterms (when search (cdr search)))
		      (x (make-inst?-rule sformnum newterms copy? NIL)))
		  x)
		(skip))))
  "Tries to automatically instantiate a quantifier:
FNUMS indicates which quantified formula:  *,-, +, or (n1, n2,..)
SUBST is a partial substitution for the bound variable names.
WHERE indicates which fnums to search for a match.
COPY? if T, the quantified formula is copied.
IF-MATCH if all, all possible instantiations of a chosen template 
                 subexpression containing all the instantiable variables
                 of the chosen quantified formula are found, and if
                 this fails, then it tries INST? with IF-MATCH set to NIL,
         if best, the instantiation from the all case that generates
                  the fewest TCCs is chosen,
         if T, the instantiation only occurs if the match succeeds,
               otherwise the given partial substitution is used.
         if first*, all possible instantiations of the
                    first successful template are chosen.
POLARITY? if T, a positively occurring template is only matched against
                negatively occuring subexpressions, and less-than
                term occurrences are matched against greater-than
                occurrences. "
  "Instantiating quantified variables")

 (defstep quant? (&optional (fnums *) subst (where *)
		   copy? if-match)
  (inst? fnums subst where copy? if-match)
  "Use INST? instead.  Tries to automatically instantiate a quantifier:
FNUM indicates which quantified formula: n, *,-, +.
SUBST is a partial substitution for the bound variable names.
WHERE indicates which fnums to search for a match.
COPY? if T, the quantified formula is copied.
IF-MATCH if all, all possible instantiations of a chosen template 
                 subexpression of the chosen quantified formula are
                 found,
         if best, the instantiation from the all case that generates
                  the fewest TCCs is chosen,
         if T, the instantiation only occurs if the match succeeds,
               otherwise the given partial substitution is used."
  "Instantiating quantified variables")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule track-rewrite (&rest names)
  (let ((dummy (loop for name in names
		     do
		     (if (has-id? name) ;;NSH(4.23.97)
			 (pushnew name *track-rewrites* :test #'same-id)
			 (error-format-if "~%Bad name: ~a" name))))
	(msg (format nil "Tracking rewrites: ~{~a, ~}" names))) 
    (skip-msg msg))
  "Tracks the named rewrite rules during rewriting when the rule fails to
apply.  Behaves like a (SKIP), otherwise." 
  "Tracking ~a")


(defrule untrack-rewrite (&rest names)
  (if (null  names)
      (let ((dummy (setq *track-rewrites* nil))
	    (msg "Untracking all rewrite rules"))
	(skip-msg msg))
      (let ((names (loop for name in names
			 when (or (has-id? name) ;;NSH(4.23.97)
				  (error-format-if "~%Bad name: ~a" name))
			 collect name))
	    (dummy (setq *track-rewrites*
			 (set-difference *track-rewrites*  names
					 :test #'same-id)))
	    (msg (format nil "Untracking ~{~a, ~}" names)))
	(skip-msg msg)))
  "(See TRACK-REWRITE) Turns off tracking for all rewrite rules when NAMES is
empty, and for the named rewrite rules, otherwise.  Behaves like (SKIP) otherwise."
  "Turning off tracking")

(defrule rewrite-msg-off ()
  (let ((dummy (setq *rewrite-msg-off* T)))
    (skip-msg "Turning off rewriting commentary,"))
  "Turns off listing of applied auto rewrites and skips."
  "")
(defrule rewrite-msg-on ()
  (let ((dummy (setq *rewrite-msg-off* nil)))
    (skip-msg "Turning on rewriting commentary,"))
  "Turns on list of applied auto rewrites and skips."
  "")

(defstep auto-rewrite-defs (&optional explicit? always?
				      exclude-theories)
    (let ((exclude-theories (if (listp exclude-theories)
				exclude-theories
				(list exclude-theories)))
	  (exclude-theories
	   (loop for name in exclude-theories
		 collect (get-theory
			  (typecheck (pc-parse name 'modname)))))
			   
	  (decls (collect-referenced-decls (declaration *top-proofstate*)
					    *ps*
					  explicit?
					  exclude-theories))
	   (rule-list (loop for decl in decls
			    collect
			    (let ((name (mk-name-expr (id decl)
					  nil
					  (id (module decl)))))
			      (if (eq always? '!!)
				  `(auto-rewrite!! ,name)
				  (if (and always?
					   (const-decl? decl))
				      `(auto-rewrite! ,name)
				      `(auto-rewrite ,name))))))
	   (rule `(then* ,@rule-list)))
      rule)
	 "Installs all the definitions used directly or indirectly in the
original statement as auto-rewrite rules.  If the explicit? flag is T, the
recursive definitions are not installed and only the explicit ones are.
If always? is !!, it uses auto-rewrite!!, and if it 
is T, then it uses auto-rewrite! on explicit definitions,
else auto-rewrite." 
	 "Auto-rewriting with all the definitions relevant to conjecture")

(defstep auto-rewrite-explicit (&optional always?)
  (auto-rewrite-defs T always?)
  "Installs all the explicit definitions in the original statement.
If always? is T, then it uses auto-rewrite! else auto-rewrite."
  "Auto-rewriting with all the explicit definitions relevant to statement")

(defhelper rewrite-directly-with-fnum (fnum  &optional (fnums *) (dir LR))
  (then (beta fnum dir)
	  (branch (split fnum)
		  ((if *new-fmla-nums*
		       (let ((newnum  (car *new-fmla-nums*)))
			 (let ((newnums (list newnum)))
			   (then (assert newnums dir)
				 (try (replace newnum
					       fnums
					       dir)
				      (delete newnums)
				      (skip)))))
		       (then (assert fnum dir)
			     (try (replace fnum
					   fnums
					   dir)
				  (delete fnum)
				  (skip))))
		   (then (beta *)
			 (assert *)))))
  "Beta-reduces, splits, and simplifies FNUM, and does a replacement in FNUMS
corresponding to dir (left-to-right when LR, and right-to-left when RL)."
  "Rewriting directly with ~a")

(defun pc-parse-subst (subst)
  (loop for (x . y) in
	(make-alist subst)
	collect (cons (pc-parse x 'name)
		      (pc-parse y 'expr))))

(defun check-subst-wrt-formula (fmla subst)
  (let* ((in-subst (pc-parse-subst subst))
	 (outervars (substitutable-vars fmla))
	 (check (loop for (x . nil) in in-subst
		      always
		      (member x outervars :test #'same-id)))
	 (subvars (when check
		    (loop for x in outervars
			  when (member x in-subst
				       :test #'(lambda (y z)
						 (same-id y (car z))))
			  collect x)))
	 (temp-subst
	  (when check
	    (loop for x in subvars
		  collect (cons x (cdr (assoc x in-subst :test #'same-id))))))
	 (in-subst (if check
		       (let ((*tccforms* *tccforms*));;protecting
					;NSH(11/17/93: too strong a check
					;(tc-alist temp-subst)
			 (loop for (nil . y) in temp-subst
			       do (typecheck y))
			 temp-subst)
		       'fail)))
    in-subst))

(defstep rewrite-with-fnum (fnum &optional subst (fnums *) (dir LR))
  (let ((fnum (find-sform (s-forms (current-goal *ps*)) (list fnum)))
	;;NSH(5.9.99): numeralizes labels.
	 (sforms (select-seq (s-forms (current-goal *ps*))
			    (list fnum))))
    (if sforms
	(let ((fmla (formula (car sforms)))
	      (form (if (negation? fmla)
			(args1 fmla)
			(if (exists-expr? fmla)
			    (negate (change-class (copy fmla)
						  'forall-expr))
			    fmla)))
	      (in-subst (check-subst-wrt-formula form subst))
	      (fnums-sforms (select-seq (s-forms (current-goal *ps*))
					fnums))
	      (fnums-sforms (if (memq (car sforms) fnums-sforms)
				(append;;moves fnum to the end
				 (remove (car sforms) fnums-sforms)
				 (list (car sforms)))
				fnums-sforms))
	      (out-subst (if (equal in-subst 'fail) 'fail
			     (let ((lhs (split-rewrite form
						       (mapcar #'car in-subst)
						       dir)))
			       (find-match-list lhs 
						(mapcar #'formula
						  fnums-sforms)
						in-subst
						T))))
	      (subs (unless (eq out-subst 'fail)
		      (if (negation? fmla)
			  (quant-subs* form
				       out-subst nil nil)
			  (quant-subs* fmla out-subst
				       T nil)))))
	  (if (eq out-subst 'fail)
	      (skip-msg "No matching substitution found")
	      (let ((fnum1 (inc-fnum fnum))
		    (rules
		     (when subs
		       (cons `(inst-cp ,fnum :terms ,(car subs))
			     (loop for sub in (cdr subs)
				   collect
				   `(inst ,fnum1 :terms ,sub))))))
		(then (then :steps rules)
		      (rewrite-directly-with-fnum fnum1 fnums dir)))))
	(skip-msg "No rewritable FNUM found.")))
  "Rewrites using the formula named in FNUM given input substitution
SUBST, target FNUMS where rewrites are to occur, and the rewrite direction
DIR (LR for left-to-right, and RL, otherwise)."
  "Rewriting with ~a")

(defstep rewrite-lemma (lemma subst &optional (fnums *)
			  (dir LR))
  (let ((in-sformnums (if (consp fnums)
			   (loop for x in fnums
				 collect (if (and (integerp x)
						  (< x 0))
					     (1- x)
					     x))
			   (if (and (integerp fnums)(< fnums 0))
			       (1- fnums)
			       fnums))))
	  (try-branch (lemma lemma subst)
	   ((if *new-fmla-nums*
		(let ((num (car *new-fmla-nums*)))
		  (rewrite-directly-with-fnum num fnums dir))
		(skip))
	    (then (beta *)(assert *)))
	   (skip )))
  "Rewrites using the given (conditional) FNUM or rewrite rule and
 substitution in the given sequent formulas. (See REWRITE.)"
   "Rewriting using ~a~@[ where~{~%   ~a gets ~a~^,~}~]")

(defstep rewrite (lemma-or-fnum &optional (fnums *)  subst (target-fnums *)
		    (dir LR) (order IN) ) ;;(hash-rewrites? T) NSH(9.21.95)
  (if (integerp lemma-or-fnum)
      (rewrite-with-fnum lemma-or-fnum subst target-fnums dir)
  (let ((x (rewrite-step lemma-or-fnum fnums subst
			  target-fnums dir order)))
    x))
  "Rewrites using LEMMA-OR-FNUM (lemma name or fnum) of the form
H IMPLIES L = R by finding match L' for L and replacing L' by R' with
subgoal proof obligations for H'.  A lemma H IMPLIES L is treated as
H IMPLIES L = TRUE, and also H can be empty.
FNUMS constrains where to search for a match,
SUBST takes a partial substitution and tries to find a match extending this,
TARGET-FNUMS constrains where the rewriting occurs,
DIR is left-to-right(LR) or right-to-left(RL),
ORDER is inside-out(IN) or outside-in (OUT)."
  "Rewriting using ~a~@[, matching in ~a~]~@[ where~{~%  ~a gets ~a~^,~}~]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstrat skip-msg (msg &optional force-printing?)
  (let ((dummy (if force-printing?
		   (format t "~%~a" msg)
		   (error-format-if "~%~a" msg))))
    (skip))
  "Prints the given string but behaves like a skip.
Useful for generating error messages in strategies."
  "")

(defun forward-fnum? (fnum)
  (or (integerp fnum)
      (eq fnum '-)
      (eq fnum '*)
      (and (listp fnum)
	   (every #'forward-fnum? fnum))))

(defstep forward-chain (lemma-or-fnum)
  (if (forward-fnum? lemma-or-fnum);;added (7.17.98)
      (chain-antecedent$ lemma-or-fnum)
      (let ((lemma lemma-or-fnum);;NSH(7.17.98): dummy to preserve previous name
	    (name (pc-parse lemma 'name))
	    (resolutions (resolve name 'formula nil *current-context*)))
	(if resolutions
	    (if (singleton? resolutions)
		(let ((res (car resolutions))
		      (info (check-forward-lemma res)))
		  (if info
		      (let ((conc (car info))
			    (antec-fmlas (cdr info))
			    (formlist (append *-*
					      (mapcar #'negate *+*)))
			    (sub (forward-match res conc antec-fmlas
						formlist)))
			(if (eq sub 'fail)
			    (skip-msg "No forward match for given lemma.")
			    (let ((fsub (flatten-sub sub)))
			      (then* (lemma lemma fsub)
				     (let ((x (car *new-fmla-nums*)))
				       (split x))
				     (flatten)))))
		      (skip-msg
		       "Given lemma is not in a form suitable for forward-chaining.
The correct form is A1 & ... & An => B, where free variables in B
occur among the free variables in the Ai.")))
		(skip-msg "Given lemma does not resolve uniquely."))
	    (skip-msg "No resolution for given lemma."))))
      "Forward chains on given lemma or antecedent formula of the form
   A1 & A2 & A3 IMPLIES B
by introducing the antecedent instance B' when the corresponding
instances A1', A2', and A3' can be found as antecedents in the
current goal.  If the instance B' already exists as an antecedent,
this rule backtracks to find a fresh instance.  Note that the free variables
in B must occur among the free variables in the Ai."  
      "Forward chaining on ~a")


(defhelper chain-antecedent (fnum)
  (let ((fnums (gather-fnums (s-forms (current-goal *ps*))
			      fnum '+)))
    (chain-antecedent*$ fnums))
  "Forward chains on antecedent formula chosen by FNUM (which can be
- or *)."
  "Forward chaining on antecedent formula ~a")

(defhelper chain-antecedent* (fnums)
  (if (consp fnums) ;;fnums has to be a list from chain-antecedent
      (let ((fnum (car fnums))
	    (fmla1 (formula (car (select-seq (s-forms (current-goal *ps*))
				    fnum))))
	    (nfmla1 (negate fmla1))
	    (rest-fnums (cdr fnums))
	    (info (when (forall-expr? nfmla1)
		    (check-forward-formula nfmla1))))
	(if info
	    (let ((conc (car info))
		  (antec-fmlas (cdr info))
		  (formlist (remove fmla1
				    (append *-*
					    (mapcar #'negate *+*))))
		  (sub (forward-match* conc antec-fmlas
				      formlist formlist nil)))
	      (if (eq sub 'fail)
		  (chain-antecedent* rest-fnums)
		  (let ((check (loop for var in
				      (bindings nfmla1)
				      always
				      (assoc var sub
					     :test #'same-declaration)))
						    
			(fsub (loop for var in
				    (bindings nfmla1)
				    collect
				    (cdr (assoc var sub
						:test
						#'same-declaration)))))
		    (if check
			(then* (inst  fnum :terms fsub)
			       (let ((x (car *new-fmla-nums*)))
				 (split x))
			       (flatten))
			(skip-msg "Could not instantiate all the forward-chain variables")))))
		  (chain-antecedent* rest-fnums)))
      (skip-msg "Could not find a matching forward-chaining antecedent
formula."))
  "Forward chains on antecedent formulas given by FNUMS until a match is
found. "
  "Forward chaining on antecedent formulas ~a")

(defun forward-match (res conc antec-fmlas formlist)
  (let* ((mod-inst (module-instance res))
	 (theory (get-theory mod-inst))
	 (current-mod? (eq theory *current-theory*))
	 (actuals (unless current-mod? (actuals mod-inst)))
	 (formals (unless current-mod?
		    (formals-sans-usings theory)))
	 (*modsubst*
	  (if formals (if actuals T
			  (mapcar #'(lambda (x) (list x)) formals))
	      T)))
    (forward-match* conc antec-fmlas formlist formlist nil)))

(defun forward-match* (conc antec-fmlas formlist all-formlist subst)
  (let ((*modsubst* (if *modsubst* *modsubst* T)))
    (forward-match*-rec conc antec-fmlas formlist all-formlist subst)))

(defun forward-match*-rec (conc antec-fmlas formlist all-formlist subst)
  (cond ((null antec-fmlas)
	 (if (subsetp (and+ (substit conc subst)) all-formlist
		     :test #'tc-eq)
	     'fail
	     subst))
	((null formlist) 'fail)
	(t (let* ((antec1 (car antec-fmlas))
		  (fmla1 (car formlist))
		  ;;(*modsubst* T)
		  (sub1 (match antec1 fmla1 nil subst)))
	     (if (eq sub1 'fail)
		 (forward-match*-rec conc antec-fmlas (cdr formlist)
				 all-formlist subst)
		 (let ((result (forward-match*-rec conc (cdr antec-fmlas)
					       all-formlist
					       all-formlist sub1)))
		   (if (eq result 'fail)
		       (forward-match*-rec conc antec-fmlas (cdr formlist)
				       all-formlist subst)
		       result)))))))

(defun find-trans-match (info)
  (let ((resolution (car info))
	(A (cadr info))
	(B (caddr info))
	(C (cadddr info)))
  (loop for F1 in *-* thereis
	(let* ((mod-inst (module-instance resolution))
	       (theory (get-theory mod-inst))
	       (current-mod? (eq theory *current-theory*))
	       (actuals (unless current-mod? (actuals mod-inst)))
	       (formals (unless current-mod?
			  (formals-sans-usings theory)))
	       (*modsubst*
		(if formals (if actuals T
				(mapcar #'(lambda (x) (list x)) formals))
		    T))
	       (subst1 (match A F1 nil nil)))
	    (unless (eq subst1 'fail)
	      (loop for F2 in *-*
		    thereis
		    (and (not (eq F1 F2))
		    (let ((subst2 (match B F2 nil subst1)))
		      (unless (or (eq subst2 'fail)
				  (member (substit C subst2) *-*
					  :test #'tc-eq))
			subst2)))))))))
			  

(defun conjuncts (fmla)
  (cond ((negation? fmla)
	 (mapcar #'negate  (disjuncts (args1 fmla))))
	((conjunction? fmla)
	 (nconc (conjuncts (args1 fmla))
		 (conjuncts (args2 fmla))))
	(t (list fmla))))

(defun disjuncts (fmla)
  (cond ((negation? fmla)
	 (mapcar #'negate (conjuncts (args1 fmla))))
	((disjunction? fmla)
	 (nconc (disjuncts (args1 fmla))(disjuncts (args2 fmla))))
	((implication? fmla)
	 (nconc (conjuncts (args1 fmla))(disjuncts (args2 fmla))))
	(t (list fmla))))
	 

(defun check-forward-lemma (res)
  (let* ((fmlas (create-formulas res))
         (fmla (when fmlas (car fmlas)))
	 )
    (check-forward-formula fmla)))
    
(defun check-forward-formula (fmla)
  (let* ((body (forall-body* fmla))			      
	 (antec (when (implication? body)
		  (args1 body)))
	 (antec-fmlas (when antec (conjuncts antec)))
	 (conc (if (implication? body) (args2 body) body)))
    (if (subsetp (freevars conc)(freevars antec-fmlas)
		 :test #'tc-eq)
	(cons conc antec-fmlas)
	nil)))


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;NSH(5.15.94): detupling

(defmethod detuple* ((expr binding-expr))
  (with-slots (bindings expression) expr
    (let* ((lists
	    (if (or (null *detuple-singletons?*)
		    (and (singleton? bindings)
			 (or (tupletype? (find-supertype (type (car bindings))))
			     (recordtype? (find-supertype (type (car bindings)))))))
		(make-new-detupled-bindings bindings nil nil)
		(cons nil bindings))) ;;NSH(9.19.96) was bindings.
	   (alist (car lists))
	   (new-bvars (cdr lists)))
      (if (equal bindings new-bvars)
	  (lcopy expr 'expression (detuple* expression))
	  (lcopy expr
	    'bindings new-bvars
	    'expression (detuple* (substit expression alist)))))))
      
(defun make-new-detupled-binding-list (boundvar types alist)
  (if (null types) nil
      (let* ((type1 (car types))
	     (type11 (if (dep-binding? type1) (type type1) type1))
	     (new-id 
	      (new-boundvar-id
	       (id boundvar)))
	     (stype11 (substit type11 alist))
	     (new-bvar
	      (copy boundvar
		'id new-id
		'type stype11
		'types (list stype11)
		'declared-type (or (print-type stype11) stype11))))
	(setf (resolutions new-bvar)
	      (list (make-resolution new-bvar nil
				     (type new-bvar))))
	(if (dep-binding? type1)
	    (cons new-bvar
		  (make-new-detupled-binding-list
		   boundvar
		   (cdr types)
		   (cons (cons type1 new-bvar) alist)))
	    (cons new-bvar
		  (make-new-detupled-binding-list
		   boundvar
		   (cdr types) alist))))))

(defmethod detuple* ((expr application))
  (let ((op (detuple* (operator expr)))
	(args (detuple* (arguments expr))))
	(lcopy expr 'operator op 'argument (make-arg-tuple-expr args))))

(defmethod detuple* ((expr projection-application))
       (let ((result (detuple* (argument expr))))
	 (if (tuple-expr? result)
	       (nth (1- (index expr))
		    (exprs result))
	       (lcopy expr 'argument result))))

(defmethod detuple* ((expr field-application))
  (cond ((record-expr? (argument expr))
;;	(break "dt*")
      (detuple*
       (expression (find (id expr)
			 (assignments (argument expr))
			 :test #'(lambda (x y)
				   (eq x (id (caar (arguments y)))))))))
	(t (let ((result (detuple* (argument expr))))
	(if (record-expr? result)
	    (expression (find (id expr)
			 (assignments result)
			 :test #'(lambda (x y)
				   (eq x (id (caar (arguments y)))))))
	(lcopy expr 'argument result))))))

(defun make-new-detupled-bindings (bindings alist bvars)
  (cond ((null bindings) (cons alist bvars))
	((or (tupletype? (type (car bindings)))
	     (recordtype? (type (car bindings))))
	 (let* ((bind1 (car bindings))
		(type (find-supertype (type bind1)))
		(types (if (recordtype? type)
			   (mapcar #'type (fields type))
			   (types type)))
		(new-bvars (make-new-detupled-binding-list
			    bind1 types alist))
		(new-vars (loop for x in new-bvars
				collect (make!-name-expr
					 (id x) nil nil
					 (make-resolution x nil (type x)))))
		(new-tuple (if (recordtype? type)
			       (make-record-expr
				(loop for fld in (fields type)
				      as var in new-vars
				      collect
				      (mk-assignment 'uni
					(list (list (mk-field-name-expr
						     (id fld)
						     (resolution fld))))
					var))
				type)
			       (make-tuple-expr new-vars type))))
	   (make-new-detupled-bindings (cdr bindings)
				       (cons (cons bind1
						   new-tuple)
					     alist)
				       (nconc bvars new-bvars))))
	(t (make-new-detupled-bindings (cdr bindings)
				       (cons (cons (car bindings)
						   (car bindings))
					     alist)
				       (append bvars (list (car bindings)))))))

(defmethod detuple* ((expr record-expr))
  (lcopy expr
    'assignments (detuple* (assignments expr))))

(defmethod detuple* ((expr assignment))
  (lcopy expr
    'arguments (detuple* (arguments expr))
    'expression (detuple* (expression expr))))


(defmethod detuple* ((expr tuple-expr))
  (lcopy expr
    'exprs (detuple* (exprs expr))))


(defmethod detuple* ((expr cases-expr))
  (lcopy expr
    'expression (detuple* (expression expr))
    'selections (detuple* (selections expr))
    'else-part (detuple* (else-part expr))))

(defmethod detuple* ((expr update-expr))
  (lcopy expr
    'expression (detuple* (expression expr))
    'assignments (detuple* (assignments expr))))

(defmethod detuple* ((expr list))
  (let ((new-expr (mapcar #'detuple* expr)))
    (if (every #'eq expr new-expr)
	expr
	new-expr)))

(defmethod detuple* ((expr T))
  expr)


(defstep detuple-boundvars (&optional (fnums *) singles?)
  (let ((sforms (select-seq (s-forms (current-goal *ps*)) fnums))
	(formulas (mapcar #'formula sforms))
	(formulas (mapcar #'(lambda (x) (if (negation? x) (args1 x) x))
			  formulas)))
    (detuple-boundvars-in-formulas formulas singles?))
  "Replaces formulas given by FNUMS by equivalent formulas where any
bound variable of tuple type is replaced by a list of variables.
When SINGLES? is T, detupling is restricted to those bound variables
of tuple/record type that are the sole bound variables of a binding operator,
i.e. those x of the form (FORALL x: ...), (EXISTS x: ...),
or (LAMBDA x: ...)."
  "De-tupling formulas")

(defhelper detuple-boundvars-in-formulas (formulas singles?)
  (if (consp formulas)
      (let ((fmla (car formulas))
	    (de-fmla (let ((*detuple-singletons?* T))
		       (detuple* fmla)))
	    (equality (make-equality fmla de-fmla))
	    (fmlas (cdr formulas)))
	(then (case-replace equality)
	      (detuple-boundvars-in-formulas fmlas singles?)
	      (delete -1)))
      (skip))
  "Replaces given formulas by equivalent formulas where any bound
variable of tuple type is replaced by a list of variables.
When SINGLES? is T, detupling is restricted to those bound variables
of tuple type that are the sole bound variables of a binding operator,
i.e. those x of the form (FORALL x: ...), (EXISTS x: ...),
or (LAMBDA x: ...)."
  "De-tupling the given formulas")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(7.27.94): skolem then typepred the skolem constants.

(defstep skolem-typepred (&optional (fnum *))
  (let ((sformnum (find-!quant fnum *ps*))
	(newterms (fill-up-terms sformnum nil *ps*))
	(X `(typepred ,@newterms))) 
    
    (then (skolem sformnum newterms)
	  X))
  "Skolemizes and then introduces type-constraints of the Skolem
constants.
See also SKOLEM!, TYPEPRED."
  "Skolemizing (with typepred on new Skolem constants)")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstep name-replace (name expr &optional (hide? T))
  (try (name name expr)
       (then (replace -1)
	     (if hide? (hide -1)(skip)))
       (skip-msg "NAME step in NAME-REPLACE failed."))
  "Uses NAME, REPLACE, and HIDE to abbreviate an expression with a
newly chosen name"
  "Using ~a to name and replace ~a")

(defstep name-replace* (name-and-exprs &optional (hide? T))
  (if (consp name-and-exprs)
      (if (consp (cdr name-and-exprs))
	  (let ((name (car name-and-exprs))
		(expr (cadr name-and-exprs))
		(rest (cddr name-and-exprs)))
	    (then (name-replace name expr hide?)
		  (name-replace* rest hide?)))
	  (skip-msg "Odd number of name/expr entries."))
      (skip))
  "Iterates NAME-REPLACE.  The NAME-AND-EXPRS argument must be
of the form (<name1> <expr1> <name2> <expr2>...).  The command
replaces each expri in the sequent with the corresponding namei."
  "Letting ~{~%  ~a name ~a~^,~}")

(defstep copy (fnum)
  (let ((sforms (select-seq (s-forms (current-goal *ps*)) fnum)))
    (if sforms
	(let ((fmla (formula (car sforms)))
	      (negfmla (negate fmla)))
	  (then (case negfmla)(skip)))
	(let ((msg
	       (format nil "Could not find formula number ~a" fnum)))
	  (skip-msg msg))))
  "Introduces a copy of formula number FNUM as the first antecedent
or succedent formula in the sequent."
  "Copying formula number: ~a")

    
(defstep trace (&rest names)
  (let ((dummy (loop for name in names do (pushnew name *ruletrace*)))
	(msg (format nil "Tracing ~a" (format-list-of-items names))))
    (skip-msg msg))
  "Traces rules or proof steps given by NAMES."
  "")



(defstep untrace (&rest names)
  (let ((msg (if names
		 (format nil "Untracing ~a" (format-list-of-items names))
		 (format nil "Untracing everything: ~a"
		   (format-list-of-items *ruletrace*))))
	(dummy (if names
		   (loop for name in names do (setq *ruletrace*
						    (delete name *ruletrace*)))
		   (setq *ruletrace* nil))))
    (skip-msg msg))
  "Turns off tracing on the rules and proof steps given in NAMES."
  "")

;;NSH(5.27.95) : From JMR
;;added exclude argument to grind and if-match argument to use.
(defstep use (lemma &optional subst (if-match best) (instantiator inst?))
  (then@ (lemma lemma subst)
	 (if *new-fmla-nums*
	     (let ((fnum (car *new-fmla-nums*))
		   (command (generate-instantiator-command if-match nil instantiator fnum)))
	       (then 
		(beta fnum)
		(repeat command)))
	     (skip)))
  "Introduces lemma LEMMA, then does BETA and INST? (repeatedly) on
 the lemma.  The INSTANTIATOR argument may be used to specify an alternative
 to INST?."
  "Using lemma ~a")

(defstep use* (&rest names)
  (if (consp names)
      (let ((first (car names))
            (rest (cdr names)))
        (then (use$ first)
              (use*$ :names rest)))
      (skip))
  "Iterating USE for a list of lemmas names NAMES."
  "Iterated lemma application on ~@{~%   ~a, ~}")

(defun collect-constant-names (expr)
  (let ((*constant-names* nil))
    (mapobject #'(lambda (x) (when (name-expr? x)
			       (pushnew x *constant-names*
					:test #'tc-eq)))
	       expr)
    *constant-names*))

(defun collect-subterms (expr pred)
  (let ((*collect-subterms* nil))
    (mapobject #'(lambda (x) (or (typep x 'actual)
				 (when (funcall pred x)
				   (pushnew x *collect-subterms*
					    :test #'tc-eq))))
	       expr)
    *collect-subterms*))

(defstep merge-fnums (fnums)
  (let ((seq (s-forms (current-goal *ps*)))
	(sforms (select-seq seq fnums))
	(antec-fmlas
	 (loop for sf in sforms when (negation? (formula sf))
	       collect (args1 (formula sf))))
	(conseq-fmlas
	 (loop for sf in sforms when (not (negation? (formula sf)))
	       collect (formula sf)))
	(fmla (if antec-fmlas
		  (make-implication
		   (make-conjunction antec-fmlas)
		   (make-disjunction conseq-fmlas))
		  (make-disjunction conseq-fmlas)))) 
    (if (or antec-fmlas conseq-fmlas)
	(if (and (null antec-fmlas)
		 (singleton?  conseq-fmlas))
	    (skip-msg "Nothing to merge.")
	    (spread (case fmla)
		    ((prop)
		     (let ((oldfnums
			    (find-all-sformnums
			     (s-forms (current-goal *ps*))
			     '* 
			     #'(lambda (x);;must be a pred on formulas
				 (member x sforms
					 :key #'formula)))))
		       (delete oldfnums)))))
	(skip-msg "Failed to find mergeable fnums.")))
  "Merges indicated FNUMS into a single formula in the sequent."
  "Merging formulas: ~a")

(defmethod pc-typecheck ((expr expr) &key expected (fnums '*) (uniquely? T))
  ;;assumption is that expr is pc-parsed, and expected is typechecked.
  (typecheck expr :expected expected)
  (cond ((type expr)
	 expr)
	((singleton? (types expr))
	 (set-type expr (car (types expr)))
	 expr)
	(t (let* ((sforms
		   (when *ps*
		     (select-seq (s-forms (current-goal *ps*)) fnums)))
		  (forms (mapcar #'formula sforms))
		  (terms (collect-subterms forms
					   #'(lambda (x)
					       (print-equal x expr))))
		  (term (loop for tm in terms
			      thereis (and (null (freevars tm))
					   (or (null expected)
					       (strict-compatible?
						expected
						(type tm)))
					   tm)))
		  (type (if term
			    (type term)
			    expected)))
	     (if type
		 (let ((ptypes (remove-if-not #'(lambda (pty)
						  (compatible? pty type))
				 (ptypes expr))))
		   (set-type expr
			     (if (singleton? ptypes)
				 (car ptypes)
				 type))
		   expr)
		 (if uniquely?
		     (type-ambiguity expr)
		     expr))))))

(defmethod pc-typecheck ((expr T) &key expected (fnums '*) (uniquely? T))
  (declare (ignore expected fnums uniquely?))
  (typecheck expr))

(defstep generalize (term var  &optional type (fnums *)
			  (subterms-only? T))
  (branch (merge-fnums fnums)
	  ((let ((sforms
		  (select-seq (s-forms (current-goal *ps*)) '(1)))
		 (sform (car sforms))
		 (form (when sform (formula sform)))
		 (var (pc-parse var 'name))
		 (vtype (when type
			 (typecheck (pc-parse type 'type-expr))))
		 (term (pc-parse term 'expr))
		 (term (pc-typecheck term :expected vtype :fnums '(1)))
		 (vtype (if vtype vtype (type term)))
		 (var-decl (when (and term vtype)
			     (make-bind-decl
				 (id var) vtype
				 )))
		 (var (when (and term var-decl)
			(make-variable-expr var-decl)))
		 (newform (when term
			    (if subterms-only?
				(termsubst form
					   #'(lambda (x)
					       (declare (ignore x))
					       var)
					   #'(lambda (x) (tc-eq x term)))
				(gensubst form
				  #'(lambda (x) (declare (ignore x)) var)
				  #'(lambda (x) (tc-eq x term))))))
		 (newform (when term (universal-closure newform)))
		 (subst (when term (list var term))))
	     (if vtype
		 (if (valid-pvs-id (id var))
		     (if term
			 (spread (case newform)
				 ((then (inst? -1 :subst subst
					       :where nil)
					(prop))
				  (hide 2)))
			 (skip-msg "No such generalizable term found in given fnums"))
		     (skip-msg "Given variable is not a valid PVS identifier"))
		 (skip-msg "Please supply the type argument.")))
	     (skip)))
  "Generalizes TERM in FNUMS by VAR of type TYPE.  By default, only the
subterm occurrences of TERM will be generalized and not those occurrences
in types or actuals.  If the command is invoked with the SUBTERMS-ONLY? flag
set to the NIL, then every occurrence of TERM will be generalized.  This
might go too far so if a more delicate generalization of selected occurrences
is needed, the best option is to use CASE."
  "Generalizing ~a by ~a")

(defvar *terms* nil)

(defmethod mapobject* (fn (ps proofstate))
  (mapobject* fn (s-forms (current-goal ps))))

(defmethod mapobject* (fn (sf s-formula))
  (mapobject* fn (formula sf)))

(defun collect-terms (pred obj)
  (let* ((*terms* nil))
    (mapobject #'(lambda (x)(collect-terms-fun pred x)) obj)
    *terms*))

(defmethod collect-terms-fun (pred (expr expr))
  (cond  ((funcall pred expr)
	  (push expr *terms*)
	  nil)
	 (t nil)))

(defmethod collect-terms-fun (pred (ps proofstate))
  (declare (ignore pred))
  nil)

(defmethod collect-terms-fun (pred (sf s-formula))
  (declare (ignore pred))
  nil)

(defmethod collect-terms-fun (pred (list list))
  (declare (ignore pred))
  nil)

(defmethod collect-terms-fun (pred (obj t))
  (declare (ignore pred))
  t)

(defun collect-applications-of-fun (id obj)
  (collect-terms
    #'(lambda (x) (and (application? x)(eq (id (operator* x)) id)))
    obj))

(defun skolem-constant? (expr)
  (and (name-expr? expr)(skolem-const-decl? (declaration expr))))

(defun sko-symbol-prefix (id)
  (let* ((string (string (op-to-id id)))
	 (pos (position #\! string :from-end T))
	 (prefix (if pos (subseq string 0 pos)
		     string))
	 (index (when pos  ;;NSH(9.20.95)
		  (parse-integer string :start (1+ pos) :junk-allowed T))))
    (if index (intern prefix) id)))

(defun make-constant-bind-decl-alist (constants done-alist)
  ;;constants must be sorted according to occurrence as done in
  ;;generalize-skolem-constants defstep.
  (if (null constants)
      (nreverse done-alist)
      (make-constant-bind-decl-alist
       (cdr constants)
       (cons (cons (car constants)
		   (make-bind-decl
			   (new-boundvar-id
			    (sko-symbol-prefix (id (car constants))))
		     (gensubst
		        (type (car constants))
		       #'(lambda (x)
			   (make-variable-expr
			    (cdr (assoc x done-alist :test #'tc-eq))))
		       #'(lambda (x)(assoc x done-alist :test #'tc-eq)))))
	     done-alist))))

(defstep generalize-skolem-constants (&optional (fnums *))
  (then (merge-fnums fnums)
	(if (or *new-fmla-nums*
		(singleton? (s-forms (current-goal *ps*))))
	    (let ((fnums (if *new-fmla-nums* *new-fmla-nums* fnums))
		  (sforms (select-seq (s-forms (current-goal *ps*)) fnums))
		  (sform (car sforms))
		  (form (formula sform))
		  (skolem-constants
		   (collect-subterms form
				     #'skolem-constant?))
		  (skolem-constants
		   (sort skolem-constants
			 #'(lambda (x y)
			     (member x (collect-subterms y #'skolem-constant?)))))
		  (constant-bind-decl-alist
		   (make-constant-bind-decl-alist skolem-constants nil))
		  (constant-variable-alist
		   (loop for (x . y) in constant-bind-decl-alist
			 collect (cons x (make-variable-expr y))))
		  (newform (gensubst form
			     #'(lambda (x)
				 (cdr (assoc x constant-variable-alist
					     :test #'tc-eq)))
			     #'(lambda (x)
				 (assoc x constant-variable-alist
					:test #'tc-eq))))
		  (newform (universal-closure newform))
		  (instantiation-list
		   (when (forall? newform)
		     (loop for bd in (bindings newform)
			   collect
			   (let ((entry (rassoc bd constant-bind-decl-alist
						:test #'tc-eq)))
			     (if entry (car entry) "_"))))))
	      (branch
	       (case newform)
	       ((then (inst -1 :terms instantiation-list)
		      (prop))
		(hide 2))))
	    (skip-msg "Merge-fnums failed.")))
  "Merges the formulas and universally generalizes the skolem constants
in the given fnums."
  "Merging and generalizing")

(defun cleanup-fnums (fnums)
  (cond ((consp fnums)(loop for fnum in fnums
			    collect (if (stringp fnum)
					(intern fnum)
					fnum)))
	((stringp fnums) (list (intern fnums)))
	((memq fnums '(* + -)) fnums)
	(t (list fnums))))

(defun gather-fnums (sforms yesnums nonums
		       &optional (pred #'always-t) (pos 1) (neg -1))
  (let ((yesnums (cleanup-fnums yesnums))
	(nonums (cleanup-fnums nonums)))
    (gather-fnums* sforms yesnums nonums pred pos neg)))

(defun gather-fnums* (seq yesnums nonums
		       pred pos neg)
   (cond ((null seq) nil)
	 ((negation? (formula (car seq)))
	  (if (and (in-sformnums? (car seq) pos neg yesnums)
		   (not (in-sformnums? (car seq) pos neg nonums))
		   (funcall pred (car seq)))
	      (cons neg
		    (gather-fnums* (cdr seq) yesnums nonums pred
				pos (1- neg)))
	      (gather-fnums* (cdr seq) yesnums nonums pred pos (1- neg)))) 
	 (t (if (and (in-sformnums? (car seq) pos neg yesnums)
		     (not (in-sformnums? (car seq) pos neg nonums))
		     (funcall pred (car seq)))
	      (cons pos
		    (gather-fnums* (cdr seq) yesnums nonums
				pred (1+ pos) neg))
	      (gather-fnums* (cdr seq) yesnums nonums pred
			  (1+ pos)  neg)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(9.3.97): integrating Sam's decompose-equality strategy
;;for converting (dis)equalities on compound types to their
;;component equalities.

(defun disequality? (expr)(disequation? expr))

(defun decomposable-equality? (fmla)
  (and (or (equation? fmla)
	   (disequality? fmla))
       (or (typep (find-supertype
		   (type (args1 fmla)))
		  '(or funtype recordtype tupletype))
	   (adt? (find-supertype
		  (type (args1 fmla)))))))

(defstep decompose-equality (&optional (fnum *) (hide? t))
  (let ((sforms (select-seq (s-forms (current-goal *ps*))
			    (if (memq fnum '(* + -)) fnum
				(list fnum))))
	(fm (find-if
		#'(lambda (sf)
		    (or (decomposable-equality? (formula sf))
			(and (negation? (formula sf))
			     (decomposable-equality? (args1 (formula sf))))))
	      sforms))
	(ffm (when fm (formula fm)))
	(equality? (when fm
		     (or (equation? ffm)
			 (and (negation? ffm)
			      (disequation? (args1 ffm))))))
	(fmla (when fm
		(if (negation? ffm)
		    (args1 ffm)
		    ffm)))
	(lhs (when fmla (args1 fmla)))
	(rhs (when fmla (args2 fmla)))
	(comp-equalities (when (and fmla (not equality?))
			   (component-equalities
			    lhs rhs
			    (find-declared-adt-supertype (type lhs))))))
    (if fmla
	(if equality?
	    (apply-extensionality :hide? hide)
	    (branch (case comp-equalities)
		    ((then (let ((fnums *new-fmla-nums*))
			     (simplify fnums))
			   (let ((fnums (find-all-sformnums
					 (s-forms (current-goal *ps*))
					 '* #'(lambda (x) (eq x ffm))))
				 (fnum (if fnums (car fnums) nil)))
			     (if (and hide? fnum) (delete fnum) (skip)))
			   (flatten))
		     (then (flatten) (replace*)
			   (grind :defs nil :if-match nil)))))
	(skip-msg "Couldn't find a suitable equation")))
  "Decomposes an equality or disequality to the component equalities.
This only works for equalities between functions, records, or tuples.  If
HIDE? is T, the original (dis)equality is hidden.  If it is an equality in
the consequents or a disequality in the antecedents then this simply
invokes apply-extensionality.  Otherwise it decomposes the
 (dis)equality into its component equalities."
  "Applying decompose-equality")

(defmethod component-equalities (lhs rhs (te recordtype))
;  (make-negation
   (make-conjunction
    (mapcar #'(lambda (fld)
		(make-equality (make-field-application fld lhs)
			       (make-field-application fld rhs)))
      (fields te))));)

(defmethod component-equalities (lhs rhs (te tupletype))
;  (make-negation
   (make-conjunction
    (loop for i from 1 to (length (types te))
	  collect (make-equality (make-projection-application i lhs)
				 (make-projection-application i rhs)))));)

(defmethod component-equalities (lhs rhs (te funtype))
;  (make-negation
   (let* ((id (make-new-variable '|x| (list te lhs rhs)))
	  (dom (domain te))
	  (bd (typecheck* (mk-bind-decl id dom dom) nil nil nil))
	  (nvar (mk-name-expr id nil nil (make-resolution bd nil dom))))
     (make!-forall-expr
      (list bd)
      (make-equality (make-application lhs nvar)
		      (make-application rhs nvar)))));)

(defmethod component-equalities (lhs rhs (te type-name))
  (make-disjunction
   (mapcar #'(lambda (r c)
	       (make-conjunction
		(cons (make-application r lhs)
		      (cons (make-application r rhs)
			    (mapcar #'(lambda (a)
					(make-equality
					 (make-application a lhs)
					 (make-application a rhs)))
			      (accessors c))))))
     (recognizers te) (constructors te))))

;;checks if formula is a well-formed induction scheme
;;returns p(x, y, z) where p is the induction predicate.
(defun inductive-scheme? (formula)
  (if (forall-expr? formula)
      (inductive-scheme? (expression formula))
      (and (implication? formula)
	   (inductive-conclusion? (args2 formula)))))

;;checks if conclusion of formula is induction conclusion.
(defun inductive-conclusion? (formula)
  (if (forall-expr? formula)
      (inductive-conclusion? (expression formula))
      (and (implication? formula)
	   (inductive-predicate? (operator* (args1 formula)))
	   (args1 formula))))

;;checks if name is an inductively defined predicate.
(defun inductive-predicate? (name)
  (inductive-decl? (declaration name)))

;;Returns resolution for inductive induction scheme given
;;rel or induction name.
(defun get-inductive-scheme-res (rel induction)
  (if induction
      (let* ((name (pc-parse induction 'name))
	    (resolutions (resolve name 'formula nil *current-context*)))
	(car resolutions))
      (let* ((name (pc-parse (format nil "~a.~a_weak_induction"
			      (module-instance (resolution rel))
			      (id rel))
		    'name))
	    (resolutions (resolve name 'formula nil *current-context*)))
	(car resolutions))))

;;returns flattened (uncurried) list of arguments
(defun arguments! (expr)
  (apply #'append (arguments* expr)))

;;prints the fully resolved name from a resolution
(defun print-resolution (res)
  (format nil "~a.~a" (module-instance res) (id res)))

;;checks if a formula is of the form rel(....).
(defun inductive-antecedent? (rel)
  #'(lambda (x)
      (let ((xf (formula x)))
	(and (negation? xf)
	     (name-expr? (operator* (args1 xf)))
	     (same-id (operator* (args1 xf))
		      rel)
	     (inductive-predicate?
	      (operator* (args1 xf)))))))

(defstep rule-induct-step (rel &optional (fnum -) name)
  (let ((sforms (s-forms (current-goal *ps*)))
	(rel (pc-parse rel 'expr))
	(pred-sforms
	 (gather-seq sforms
		     fnum nil
		     (inductive-antecedent? rel))))
    (if (null pred-sforms)
	(skip-msg "No appropriate inductive antecedent in the goal sequent.")
	(let ((pred-sform  (car pred-sforms))
	      (pred-application (args1 (formula pred-sform)))
	      (rel (operator* pred-application))
	      (ind-res (when rel
			 (get-inductive-scheme-res rel name)))
	      (ind-scheme-name (print-resolution ind-res))
	      (ind-scheme (car (create-formulas ind-res)))
	      (ind-pred (inductive-scheme? ind-scheme))
	      (ind-scheme (when (inductive-scheme? ind-scheme)
			    ind-scheme)))
	  (if (null ind-scheme)
	      (let ((format-string
		     (format nil "No induction scheme associated with relation ~a" rel)))
		(skip-msg format-string))
	      (let ((ind-bindings (apply #'append (bindings* ind-scheme)))
		    (pred-fixed
		     (loop for x in (arguments! ind-pred)
			   as y in (arguments! pred-application)
			   when (member x ind-bindings
					:test #'same-declaration)
			   nconc (list (id x) y)))
		    (pred-varying
		     (loop for x in (arguments! ind-pred)
			   as y in (arguments! pred-application)
			   when (not (member x ind-bindings
					     :test #'same-declaration))
			   collect y)))
		(if (not (every #'skolem-constant? pred-varying))
		    (let ((format-string
			   (format nil
			       "Inductive relation:~%~a~%must be over ~
skolem constants for the induction scheme to make sense."
			     pred-application)))
		      (skip-msg format-string))
		    (if (duplicates? pred-varying :test #'tc-eq)
			(let ((format-string
			       (format nil "Duplicates in induction parameters: ~a"
				 pred-varying)))
			  (skip-msg format-string))
			(let ((pred-constants 
			       (sort pred-varying
				     #'(lambda (x y)
					 (member x
						 (collect-subterms
						  y #'constant?)))))
			      (all-antec-fmlas
			       (loop for sf in sforms
				     when (and (negation? (formula sf))
					       (intersection
						pred-constants
						(collect-subterms (formula sf)
								  #'constant?)
						:test #'tc-eq))
				     collect (args1 (formula sf))))
			      (antec-fmlas (delete pred-application
						   all-antec-fmlas))
			      (conseq-fmlas
			       (loop for sf in sforms
				     when (and (not (negation? (formula sf)))
					       (intersection
						pred-constants
						(collect-subterms (formula sf)
								  #'constant?)
						:test #'tc-eq))
				     collect (formula sf)))
			      (form (if antec-fmlas
					(make-implication
					 (make-conjunction antec-fmlas)
					 (make-disjunction conseq-fmlas))
					(make-disjunction conseq-fmlas)))
			      (constant-bind-decl-alist
			       (make-constant-bind-decl-alist pred-constants nil))
			      (constant-variable-alist
			       (loop for (x . y) in constant-bind-decl-alist
				     collect (cons x (make-variable-expr y))))
			      (newform (gensubst form
					 #'(lambda (x)
					     (cdr (assoc x constant-variable-alist
							 :test #'tc-eq)))
					 #'(lambda (x)
					     (assoc x constant-variable-alist
						    :test #'tc-eq))))
			      (induction-pred
			       (if constant-bind-decl-alist
				   (make-lambda-expr
				       (mapcar #'cdr constant-bind-decl-alist)
				     newform)
				   newform))
			      (ind-var (car (last ind-bindings)))
			      (ind-subst (cons (id ind-var)
					       (cons induction-pred
						     pred-fixed))))
			  (then (lemma ind-scheme-name
				       :subst ind-subst)
				(beta)
				(branch (prop)
					((then (inst?)(prop))
					 (let ((old-fmlas (append all-antec-fmlas conseq-fmlas))
					       (fnums (find-all-sformnums
						       (s-forms (current-goal *ps*))
						       '*
						       #'(lambda (x)
							   (or (and (negation? x)
								    (memq (args1 x) old-fmlas))
							       (memq x old-fmlas))))))
					   (hide :fnums fnums)))))))))))))
  "Applies rule induction over an inductive relation REL in
   order to prove a sequent of the form
     ..., REL(x1,...,xn) ... |-- ... .
   RULE-INDUCT-STEP searches for an antecedent formula of this form
   but this can also be given explicitly as FNUM.  The induction
   predicate is formulated using all the sequent formulas containing
   x1,...,xn.   The strategy uses the default induction scheme but can be
   told to use weak induction by giving REL_weak_induction as the NAME
   argument."
  "Applying rule induction over ~a")

(defstep rule-induct (rel &optional (fnum +) name)
  (then (repeat (skolem! fnum))
	(try (flatten)
	     (let ((fnum *new-fmla-nums*))
	       ;;note rule-induct recursively, not rule-induct-step
	       ;;to deal with embedded induction predicates.
	       (rule-induct$ rel :fnum + :name name))
	     (rule-induct-step$ rel :fnum - :name name)))
    "Applies rule induction over an inductive relation REL in
   order to prove a sequent of the form
      ...|- (FORALL ...: REL(...) IMPLIES ... or
     ..., REL(x1,...,xn) ... |-- ... 
   RULE-INDUCT invokes first applies repeated skolemization  and flattening
   to the specified FNUM (or the first positive, skolemizable consequent)
   before invoking RULE-INDUCT-STEP.   The strategy uses the default
   weak induction scheme but can be told to use strong induction by giving
   REL_strong_induction as the NAME argument."
  "Applying rule induction over ~a")


(defmethod bindings* ((expr binding-expr))
  (cons (bindings expr)(bindings* (expression expr))))

(defmethod bindings* ((expr T))
  nil)

(defstep apply-eta (term &optional type)
  (let ((type (if type
		(typecheck (pc-parse type 'type-expr))
		nil))
	(term (if type
		  (pc-typecheck (pc-parse term 'expr)
		    :expected type)
		  (typecheck (pc-parse term 'expr))))
	(type (if type (pc-type type)
		  (if (type term) (pc-type (type term)) nil))))
    (if type
	(try (eta type)
	     (inst -1 term)
	     (skip-msg "No suitable eta axiom scheme found."))
	(skip-msg "Please supply optional type argument.")))
  "Instantiates eta axiom scheme for type TYPE or type of TERM with TERM
See also ETA"
  "Applying eta axiom scheme to ~a")

(defun ineq? (expr)
  (and (application? expr)
       (let ((op (operator expr)))
	 (and (name-expr? op)
	      (interpreted? op)
	      (memq (id op) '(= < <= > >=))
	      op))))

(defun find-ineq-conjunction (fmla)
  (if (conjunction? fmla)
      (let ((conjuncts (and+ fmla)))
	(if (every #'ineq? conjuncts)
	    fmla
	    (loop for x in (arguments fmla)
		  thereis (find-ineq-conjunction x))))
	    (if (propositional-application? fmla)
		(loop for x in (arguments fmla)
		      thereis (find-ineq-conjunction x))
		(if (ineq? fmla) fmla nil))))
	       

(defstep both-sides (op term &optional (fnum 1))
  (let ((op (pc-parse op 'name))
	(term (pc-parse term 'expr))
	(sforms (select-seq (s-forms (current-goal *ps*)) fnum))
	(fmla (when sforms (formula (car sforms))))
	(ineq-conjunction (find-ineq-conjunction fmla))
	(new-ineq-conjunction
	 (when ineq-conjunction
	   (let* ((ineq-conjuncts (and+ ineq-conjunction))
		  (new-conjuncts
		   (loop for conj in ineq-conjuncts	
		 collect
			 (make-application
			     (operator conj)
			   (typecheck
			       (mk-application op
				 (args1 conj) term)
			     :expected
			     (if *integer*
				 (compatible-type
				  (type (args1 conj))
				  *integer*)
				 (type (args1 conj))))
			   (typecheck
			       (mk-application op
				 (args2 conj) term)
			     :expected
			     (compatible-type (type (args2 conj))
					      *integer*))))))
	     (make-conjunction new-conjuncts))))
	(case-fmla (when new-ineq-conjunction
		     (make-equality ineq-conjunction new-ineq-conjunction))))
    (if case-fmla
	(spread (case-replace case-fmla)
		((hide -1)
		 (then
		  (try (typepred term)
		       (let ((fnums *new-fmla-nums*))
			 (then (assert fnums)
			       (hide :fnums fnums)))
		       (skip))
		  (auto-rewrite-theory "real_props")
		  (do-rewrite :fnums 1)
		  (stop-rewrite-theory "real_props"))
		 ))
	(skip-msg "Failed to find inequality/equality chain")))
  
  "Applies OP to TERM uniformly over a conjunction of inequalities.
Example: (both-sides \"*\" \"2\") multiplies both sides of the target
         inequalities by 2"
  "Applying ~a ~a to both sides of an inequality/equality conjunction")

(defstep case* (&rest formulas)
  (if (consp formulas)
      (let ((first (car formulas))
	    (rest (cdr formulas)))
	(then (case first)
	      (case*$ :formulas rest)))
      (skip))
  "Complete version of CASE command where all the formulas are case split
along every branch."
  "Case-splitting fully on ~@{~%   ~a, ~}")

(defhelper expand1* (names)
  (if (null names)
      (skip)
      (let ((name1 (car names))
	    (rest-names (cdr names)))
	(then (expand name1) (expand1* rest-names))))
  "Expands all the given names and simplifies.
See also EXPAND"
  "Expanding the definition(s) of ~a")

(defstrat checkpoint ()
  (query*)
  "A synonym for (query*): inserting (checkpoint) an edited proof and
 rerunning it causes the non-checkpointed subproofs to be installed
 (using JUST-INSTALL-PROOF) so that the proof is only run up to the
checkpoint.  "
 " ")

(defhelper label-fnums (labels fnums &optional push?)
  (if (consp labels)
      (if (consp (cdr labels))
	  (if (consp fnums)
	      (let ((label (car labels))
		    (cdr-labels (cdr labels))
		    (fnum (car fnums))
		    (cdr-fnums (cdr fnums)))
		(then (label label fnum :push? push?)
		      (label-fnums cdr-labels cdr-fnums :push? push?)))
	      (skip))
	  (let ((label (car labels)))
	    (label label fnums)))
      (label labels fnums :push? push?))
  "Labels a list of formulas from FNUMS with corresponding labels
taken from LABELS so that the last label is applied to any
remaining fnums.  If PUSH? is T, the new label is added to existing
labels, otherwise, the old labels are deleted."
  "Labelling some formulas")

(defstep with-labels (rule labels &optional push?)
  (then rule
	(let ((fnums *new-fmla-nums*)
	      (labels (if (consp labels) labels (list labels)))
	      (current-labels
	       (nth-or-last (subgoalnum *ps*) labels)))
	  (label-fnums current-labels fnums :push? push?)))
  "If RULE generates subgoal sequents S1...Sn where each Si has
new formulas, i.e., those numbered with {}, numbered fi1..fim, then
if LABELS is a list of list of labels ((l11 ... l1k)...(ln1...lnm)),
then each formula fij is labelled with label lij.  As usual, any of
the lists can be replaced a single label, and if there are not enough
labels in a list, then the last label is applied to the remaining
fnums."
  "Applying ~a and labelling new subgoal formulas with ~a")

(defstep hide-all-but (&optional keep-fnums (fnums *))
  (let ((fnums (gather-fnums (s-forms (current-goal *ps*))
			     fnums keep-fnums)))
    (hide :fnums fnums))
  "Hides all sequent formulas from FNUMS except those listed in
KEEP-FNUMS.  Useful when all but a few formulas need to be hidden."
  "Keeping ~a and hiding ~a")

(defstep skolem_inst (&optional (sk_fnum *) (inst_fnum *))
  (let ((sk_fnum (find-!quant sk_fnum *ps*))
	(sforms (select-seq (s-forms (current-goal *ps*))(list sk_fnum)))
	(bindings (when sforms (seq-form-bindings (formula (car sforms)))))
	(newterms (fill-up-terms sk_fnum nil *ps*))
	(inst_fnums (gather-fnums
		     (s-forms (current-goal *ps*))
		     inst_fnum nil
		     #'(lambda (sform)
			 (and (exists-sform? sform)
			      (eql (length (seq-form-bindings (formula sform)))
				   (length newterms))
			      (subsetp (seq-form-bindings (formula sform))
				       bindings
				       :test #'same-id)))))
	(inst_fnum (when inst_fnums (car inst_fnums))))
    (if inst_fnums
	(then (skolem sk_fnum newterms)
	      (inst inst_fnum :terms newterms))
	(skip-msg "Couldn't find matching top-level quantifiers.")))
    ""
    "Simultaneously skolemizing and instantiating with skolem constants")

(defstep flatten (&rest fnums) (flatten-disjunct fnums nil)
 "Disjunctively simplifies chosen formulas.  It simplifies 
top-level antecedent conjunctions, equivalences, and negations, and
succedent disjunctions, implications, and negations from the sequent."
 "Applying disjunctive simplification to flatten sequent")

(defstep model-check (&optional (dynamic-ordering? T) (cases-rewrite? T)
				defs	; NIL, T, !, explicit, or explicit!
				theories
				rewrites
				exclude
				irredundant?)
  (let ((cuth *current-theory*)
	(cuthstr (string (id cuth)))
	(init-real-time (get-internal-real-time))
	(init-run-time (get-run-time)))
    (then* (skolem!)
	   (install-rewrites$ :defs defs :theories theories
			      :rewrites rewrites :exclude exclude)
	   (auto-rewrite-theory cuthstr :always? T)
	   (auto-rewrite-theory "ctlops" :defs T :always? !!)
	   (auto-rewrite-theory "fairctlops" :defs T :always? !!)
	   (auto-rewrite-theory "Fairctlops" :defs T :always? !!)
	   (auto-rewrite "/=")
	   (stop-rewrite "mucalculus.mu" "mucalculus.nu"
			 "Reachable.Reachable")
	   (rewrite-msg-off)
	   (assert :cases-rewrite?  cases-rewrite?)
	   (musimp :dynamic-ordering? dynamic-ordering?
		   :irredundant? irredundant?)
	   (skip-msg
	    (format nil
		"~%Finished model-checking in ~,2,-3f real, ~,2,-3f cpu seconds"
	      (realtime-since init-real-time) (runtime-since init-run-time)))))
  "Rewrites temporal operators into mu/nu expressions, and
simplifies using mu-calculus checker.  If DYNAMIC-ORDERING? is T,
the BDD package uses dynamic ordering to minimize the BDD size.
If CASES-REWRITE is NIL, this turns off rewriting within the
selections of unsimplified CASES expressions.  The optional arguments
DEFS, THEORIES, REWRITES, and EXCLUDE are as in INSTALL-REWRITES."
  "By rewriting and mu-simplifying")

(defstep expand* (&rest names)
  (expand1* names)
  "Expands all the given names and simplifies. "
  "Expanding the definition(s) of ~a")

(defhelper auto-rewrite-theory-always (thlist)
  (if (or (stringp thlist)
	  (and (listp thlist)
	       (every #'stringp thlist)))
      (if (null thlist)
	  (skip)
	  (let ((hd (if (listp thlist) (car thlist) thlist))
		(tl (if (listp thlist) (cdr thlist) nil)))
	    (then
	     (auto-rewrite-theory hd :always? T)
	     (auto-rewrite-theory-always tl))))
      (let ((dummy (error-format-if
		    "Argument to auto-rewrite-theory-always must be a~%  ~
                     theory name or list of theory names")))
	(skip)))
  "Applies (auto-rewrite-theory :always? T) on a given list of theories."
  "Auto-rewriting given theories ~a with :always? T option")
