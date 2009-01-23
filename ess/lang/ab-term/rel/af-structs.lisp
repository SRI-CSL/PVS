;;; -*- Mode: Lisp; Package: Analysis-Facility -*-
;;;
;;; Sccs Id @(#)af-structs.lisp	1.4 9/28/89");
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; AF-STRUCTS.LISP - Ergo Analysis Facility 
;;;
;;; ABSTRACT
;;; 
;;;; Compile-time dependency analysis and code generation structures
;;;
;;; AUTHOR
;;;
;;;	William Maddox
;;;
;;; HISTORY
;;;
;;;     07-22-87         rln    Initial development and release.

(in-package :analysis-facility)

;;; A DP-EVAL structure describes an attribute which is defined by an expression.
;;; An expression always defines exactly one attribute.

(defstruct dp-eval
  expression
  attrsused )

(defstruct dp-eval-iterate
  expression
  attrsused
  initial
  test)
  
;;; A DP-VISIT structure describes an attribute which is defined by a visit.
;;; Such a visit defines all of the synthesized attributes of the child, thus
;;; thus the DEFINITION field of all of these attribute structures will point
;;; to the same DP-VISIT structure.

(defstruct dp-visit
  nonterminal
  selector
  inherited
  synthesized
  optsort
  position
  demand
  family
  tlist )

(defstruct dp-iterate
  nonterminal
  inherited
  synthesized
  optsort
  position
  demand
  family
  initial
  test )

;;; The compile-time dependency analysis schedules evaluations and visits
;;; during the graph-building process.  The function SCHEDULE constructs a list
;;; of instructions of the following structure types, which is then translated
;;; to Lisp code by ENCODE.

(defstruct cd-eval		; Evaluate expression and bind ATTRIBUTE
  attribute
  arguments
  expression )

(defstruct cd-eval-iterate
  attribute
  arguments
  expression
  argcode
  initial
  test)

(defstruct cd-visit		; Visit child node and bind RESULTS
  function
  selector
  arguments
  argcode
  results 
  optsort
  position
  family
  tlist )

(defstruct cd-iterate		; Visit child node and bind RESULTS
  function
  arguments
  argcode
  results 
  optsort
  position
  family
  initial
  test )

(defstruct cd-check             ; Add entry to checklist to check constraint
  predicate
  message
  args )

;;; An attribute reference, on either side of a production.

(defstruct attribute
  name			; The attribute name
  direction		; Propagation direction, one of {INH, SYN}
  tlist			; 
  			; ** SEMANTIC INFO - Filled in after parsing **
  			; ** Used only for attributes of iterated nonterminals **
  tag 			; Tag for bucket-brigade attributes
  mate			; Name of attribute with matching tag, if any, else NIL
  kind			; One of {DIST, LIST, FIRST, LAST}
			; ** Used only for left-hand-side attributes **
  final			; True if attribute is final
  iterate		; True if attribute may participate in cicularities
  initial		; Initial approximation for circular attributes (expr)
  test       )		; Convergence test for circular attributes (symbol)


;;;; Attribute definition environment.

;;; The attribute definition environment is an alist of pairs (name . info),
;;; where info is an ATTRDEF structure.

(defstruct attrdef
  definition		; expression defining attribute value
  fixups      )		; list of forward references to patch when defined

