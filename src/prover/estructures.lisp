;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; estructures.lisp -- Basic PVS prover syntax defstructs
;; Author          : Sam Owre
;; Created On      : Sat Oct 31 02:24:43 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:30:29 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;Syntax taken from CLOS definitions in ~ehdm/6/src/classes.lisp
;and makes.lisp    

;;NSH(7.27.94) commented out make-apply.
;;;;an application consists of a result type, [ope]rator and [ope]rand.
;;;;mk-application takes op and &rest args.
;(defun make-apply (op args)
;  (make-instance (if (built-in? op)
;		     (if (= (length args) 2)
;			 'infix-application
;			 'unary-application)
;		     'application)
;    'operator (if (typep op 'expr) op (mk-name-expr op))
;    'arguments args))



;;;;an abstraction consists of a result type, list of formals and a
;;;;body.  It's not clear that using a list of formals, rather than a
;;;;single formal is a wise choice, and should be reconsidered when some
;;;;real proofs are carried out.

;(defmacro make-lambda (formals  body)
;  `(mk-lambda-expr ,formals ,body))
;
;(defun mk-negation (arg)
;  (typecheck (make-apply 'NOT (list arg))
;	     :context (context nil) :expected *boolean*))
;
;;;;;a quantification consists of a formals type (the target type is
;;;;;automatically boolean, and a list of formals, and the body.
;
;(defmacro make-exists (formals  body)
;  `(make-quant-expr  'exists ,formals ,body))
;
;
;(defmacro make-forall (formals  body)
;  `(make-quant-expr  'forall ,formals ,body))



;;;That completes the syntax of expressions.  The symbol-table
;;;won't be necessary since each symbol contains its own
;;;declaration.

;;;Next, I would like a proof state to consist of 
;;;a current goal which is a list of sequents,
;;;a label to number the cases with,
;;;a list of pending label/sequent pairs
;;;a substitution for the Herbrand variables,
;;;and the next proof state.

;;;A sequent has  left and right parts, and an induction part.
;;;An s-formula consists of the formula and its governing 
;;;Herbrand variables.

;;(NSH:4-10-91) I'm making an important change by throwing out skolem
;;variables.  It does not seem that important to allow these in an
;;interactive prover.  The advantages of not having skolem variables are
;;that  rules like generalization, and induction are easier to
;;justify.  
(defcl s-formula ()
  formula
  ;;(printout :initform nil :ignorc t)
  (label :initform nil)
  (new? :initform nil :ignorc t)
  (asserted? :initform nil))


(defcl sequent ()
;;  (label :initform (format nil ""))
  (s-forms :initform nil)
  (hidden-s-forms :initform nil)
  (info :initform nil))

(defcl dpinfo ()       ;;decision proc. information.
  (dpinfo-sigalist :initform nil)
  (dpinfo-findalist :initform nil)
  (dpinfo-usealist :initform nil)
;;  (typealist :initform primtypealist) ;;NSH(2.16.94):it is now global across
                                        ;;  a proof.
  )

;;(NSH:4-10-91) modified to get rid of substitutions and out-context
(defcl proofstate ()
  (label :initform " ")
  current-goal         ;;is a sequent
  (current-rule :initform nil)
  (alists :initform nil)       ;dpinfo
  (dp-state :initform *init-dp-state*)
  (done-subgoals :initform nil)
  (pending-subgoals :initform nil)
  current-subgoal      
  (remaining-subgoals :initform nil)
  (status-flag :initform nil)
  (subgoalnum :initform 0)
  (justification :initform nil)
  (current-input :initform nil)
  (printout :initform nil)
  (comment :initform nil)
  strategy
  (context :initform nil)
  (parent-proofstate :initform nil)
  (proof-dependent-decls :initform nil);;collects decls seen so far
  (dependent-decls :initform nil)
  (current-auto-rewrites :initform nil)
  (tcc-hash :initform
	    (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
  (subtype-hash :initform (make-hash-table :hash-function 'pvs-sxhash
					   :test 'tc-eq))
  (rewrite-hash :initform (make-hash-table :hash-function 'pvs-sxhash
					      :test 'tc-eq))
  (current-xrule :initform nil))


(defcl tcc-sequent (sequent)
 tcc expr type reason kind) ;;NSH(8.3.94) (formula) 

(defcl tcc-proofstate (proofstate))

(defcl rewrite ()
  (lhs :initform nil)
  (rhs :initform nil)
  (hyp :initform nil)
  (res :initform nil))

(defcl auto-rewrites-info () %fills current-auto-rewrites slot.
  rewrites
  all-rewrites-names
  auto-rewrites-names
  auto-rewrites!-names
  macro-names)

(defmethod rewrites ((proofstate proofstate))
  (rewrites (current-auto-rewrites proofstate)))

(defmethod rewrites ((proofstate T))
  nil)

(defmethod macro-names ((proofstate proofstate))
  (macro-names (current-auto-rewrites proofstate)))

(defmethod all-rewrites-names ((proofstate proofstate))
  (all-rewrites-names (current-auto-rewrites proofstate)))

(defmethod all-rewrites-names ((x T)) nil)

(defmethod auto-rewrites-names ((proofstate proofstate))
  (auto-rewrites-names (current-auto-rewrites proofstate)))

(defmethod auto-rewrites-names ((x T)) nil)

(defmethod auto-rewrites!-names ((x T)) nil)

(defmethod auto-rewrites!-names ((proofstate proofstate))
  (auto-rewrites!-names (current-auto-rewrites proofstate)))

(defun mk-auto-rewrites-info (x x! x* all-names rewrites original)
  (if (and (eq (auto-rewrites-names original) x)
           (eq (auto-rewrites!-names original) x!)
	   (eq (macro-names original) x*)
	   (eq (all-rewrites-names original) all-names)
	   (eq (rewrites original)  rewrites))
      original
      (make-instance 'auto-rewrites-info
	'rewrites rewrites
	'auto-rewrites-names x
	'auto-rewrites!-names x!
	'macro-names x*
	'all-rewrites-names all-names)))

(defcl top-proofstate (proofstate)
    (in-justification :initform nil)
    declaration
)

(defcl strat-proofstate (proofstate))


(defcl strategy ()
  topstep
  (subgoal-strategy :initform nil)
  (failure-strategy :initform nil))


;(defcl genrule ()
;  rule-input) ;;for generating the print representation.
;
;(defcl rule (genrule)
;  ;;rule will have subclasses for various situations. 
;  rule-part   
;  ;;rule-part is a function  (args -> (proofstate -> values))
;  )

(defcl rulemacro () ;(genrule)
  (rule-list :initform nil))
;;the idea of a rule macro is that it applies the first rule,
;;then applies the second rule to each of the subgoals, and so on.
;;The updates will be accumulated along each branch.

(defcl entry ()
  name
  required-args
  optional-args
  docstring)

(defcl rule-entry (entry)
  rule-function
  (format-string :initform nil))

(defcl rule-instance ()
  rule
  rule-input
  (rule-format :initform nil))

(defcl defrule-entry ()
  name formals defn docstring format-string)

(defcl defstep-entry (defrule-entry))
(defcl defhelper-entry (defstep-entry))

(defcl strategy-instance ()
  strategy-fun
  strategy-input)

(defcl rulefun-entry (entry)
  rulefun)

(defcl strategy-entry (entry)
  strategy-fun)

(defcl justification ()
  label   ;;the label is needed since proofs might get out of order.
  rule
  subgoals
  xrule   ;;this is the expanded rule in primitive terms
  comment
  )


;used in makeskoconst and needed to avoid chasing references for skolem
;constants. 
(defcl skolem-const-decl (const-decl))

;;;9-18-90: I need an equality predicate for expressions as eequal,
;;;which is the automatic equality pred. one gets from defcl checks more
;;;than is necessary.  Sam tells me he could modify eequal to behave in
;;;the expected manner, but I think I might want an equality predicate
;;;on expressions to go further and check for alpha-equivalence.  Sam
;;;says he might find alpha-equivalence useful in the typechecker as
;;;well to test for redundant tccs.

(defun exequal (x y &optional bind-alist )
  (tc-eq-with-bindings x y bind-alist))

(defun check-ids (xlist ylist)
  (cond ((null xlist)(null ylist))
	((null ylist) nil)
	(t (and (member (car xlist) ylist
			:test #'(lambda (x y)(eq (id x)(id y))))
		(check-ids (cdr xlist)
			   (remove (car xlist) ylist
				   :test #'(lambda (x y)(eq (id x)(id y)))))))))
		      


(defmethod copy ((list list) &rest args)
  (cond ((null list) list)
	(t (cons (apply #'copy (car list) args)
		 (apply #'copy (cdr list) args)))))
