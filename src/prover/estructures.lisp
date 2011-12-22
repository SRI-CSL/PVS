;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; estructures.lisp -- Basic PVS prover syntax defstructs
;; Author          : Sam Owre
;; Created On      : Sat Oct 31 02:24:43 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 21:19:22 2004
;; Update Count    : 2
;; Status          : Stable
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

(in-package :pvs)

(defcl s-formula ()
  formula
  (label :initform nil)
  (new? :initform nil :ignore t)
  (asserted? :initform nil))


(defcl sequent ()
  (s-forms :initform nil)
  (p-sforms :initform 'unbound :ignore t)
  (n-sforms :initform 'unbound :ignore t)
  (hidden-s-forms :initform nil)
  (info :initform nil))

(defcl dpinfo ()       ;;decision proc. information.
  (dpinfo-sigalist :initform nil)
  (dpinfo-findalist :initform nil)
  (dpinfo-usealist :initform nil))

(defcl proofstate ()
  (label :initform " ")
  current-goal         ;;is a sequent
  (current-rule :initform nil)
  (dp-state :initform *init-dp-state*)
  (done-subgoals :initform nil)
  (pending-subgoals :initform nil)
  current-subgoal      
  (remaining-subgoals :initform nil)
  (status-flag :initform nil)
  (subgoalnum :initform 0)
  (justification :initform nil)
  (current-input :initform nil)
  (parsed-input :initform nil)
  (printout :initform nil)
  (comment :initform nil)
  strategy
  (context :initform nil)
  (parent-proofstate :initform nil)
  (proof-dependent-decls :initform nil);;collects decls seen so far
  (dependent-decls :initform nil)
  (current-auto-rewrites :initform nil)
  (tcc-hash :initform (make-pvs-hash-table))
  (subtype-hash :initform (make-pvs-hash-table))
  (rewrite-hash :initform (make-pvs-hash-table))
  (typepred-hash :initform (make-pvs-hash-table))
  (current-xrule :initform nil))

(defcl apply-proofstate (proofstate)
  (apply-parent-proofstate :initform nil))


(defcl tcc-sequent (sequent)
  tcc
  expr
  type
  reason
  kind)

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

(defmethod rewrites ((proofstate t))
  nil)

(defmethod macro-names ((proofstate proofstate))
  (macro-names (current-auto-rewrites proofstate)))

(defmethod all-rewrites-names ((proofstate proofstate))
  (all-rewrites-names (current-auto-rewrites proofstate)))

(defmethod all-rewrites-names ((x t)) nil)

(defmethod auto-rewrites-names ((proofstate proofstate))
  (auto-rewrites-names (current-auto-rewrites proofstate)))

(defmethod auto-rewrites-names ((x t)) nil)

(defmethod auto-rewrites!-names ((x t)) nil)

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
  declaration)

(defcl strat-proofstate (proofstate))

(defcl strategy ()
  topstep
  (subgoal-strategy :initform nil)
  (failure-strategy :initform nil))

(defcl rulemacro ()
  (rule-list :initform nil))

(defcl entry ()
  name
  required-args
  optional-args
  docstring)

(defcl rule-entry (entry)
  rule-function
  (format-string :initform nil))

(defcl rule-instance ()
  (rule :fetch-as nil)
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

(defun count-proofstates (ps &optional (num 0))
  (count-proofstates* (children ps) (1+ num)))

(defun count-proofstates* (list num)
  (if list
      (count-proofstates* (cdr list) (count-proofstates (car list) num))
      num))
