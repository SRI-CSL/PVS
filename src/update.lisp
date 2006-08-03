;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Feb 27 01:30:33 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Mar 31 22:53:34 1994
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


;;; Given a typechecked theory (omod) and a parsed theory (nmod) of the
;;; same id, this function first compares the theories, updates outside
;;; references, and merges the two.  The comparison can result in simple
;;; lexical changes (such as adding a comment), declaration level
;;; changes (changing a signature or modifying a definition), or theory
;;; level changes (reordering declarations, adding a new using, etc.)
;;; For lexical changes, no outside references need to be changed, and
;;; merging simply copies the lexical differences from nmod to omod.
;;; Declaration level changes require that references be updated; this
;;; is done using the referred-by information kept with the declaration.
;;; For signature changes, all references are followed and the
;;; associated declarations are marked as not typechecked.  For body
;;; changes, only proof declarations are so marked.  Finally, for theory
;;; level changes, all declaration references are marked as
;;; untypechecked and the new theory simply replaces the old in the
;;; context.

(defvar *last-diffs* nil "For debugging purposes")

;;; Must return the theory

;(defun update-module (omod nmod forced?)
;  (if (and omod (or (typechecked? omod) (partially-typechecked? omod)))
;      (let ((diffs (unless forced? (compare-modules omod nmod))))
;	(setq *last-diffs* diffs)
;	(cond ((or forced?
;		   (module-differences? diffs))
;	       (untypecheck (used-by omod))
;	       (dolist (use (all-usings omod))
;		 (setf (used-by (car use)) (remove omod (used-by (car use)))))
;	       (setf (gethash (id omod) *pvs-modules*) nmod)
;	       nmod)
;	      (t (progress-msg "Updating theory")
;		 (update omod nmod diffs))))
;      (progn (setf (gethash (id nmod) *pvs-modules*) nmod)
;	     nmod)))
;
;(defun module-differences? (diffs)
;  (some #'(lambda (diff)
;	    (and (not (eq (car (last diff)) 'lexical))		 
;		 (or (not (typep (car diff) 'declaration))
;		     (typep (car diff) 'formal-decl)
;		     (typep (car diff) 'mod-syn))))
;	 diffs))

;;; Udates

;;; Updating a datatype-or-module - the first four updates are only for lexical
;;; differences, otherwise the entire module is replaced (see above).

(defmethod update ((omod datatype-or-module) (nmod datatype-or-module)
		   &optional diffs)
  (when *tcdebug*
    (format t "~%Differences:")
    (mapc #'(lambda (d)
	      (format t "~%  ~a ~a - ~a"
		(type-of (car d)) (id (car d)) (caddr d)))
	  diffs))
  (setf
   ;; id
   (formals omod) (update (formals omod) (formals nmod) diffs)
   (assuming omod) (update-decls (assuming omod) (assuming nmod) diffs)
   ;; filename
   (status omod) (remove 'typechecked (status omod))
   ;; generated
   ;; generated-by
   )
  omod)


;;; Updating a module - the first four updates are only for lexical
;;; differences, otherwise the entire module is replaced (see above).

(defmethod update ((omod module) (nmod module) &optional diffs)
  (setf
   ;; declarations
   ;; types
   ;; implementing
   (exporting omod) (update (exporting omod) (exporting nmod) diffs)
   ;; all-usings
   ;; used-by
   (theory omod)    (update-decls (theory omod) (theory nmod) diffs)
   ;; tcc-info
   )
  omod)

(defmethod update ((ousing importing) (nusing importing) &optional diffs)
  (let ((diff (assoc ousing diffs)))
    (when diff
      (update (theory-name ousing) (theory-name nusing) (car (last diff))))
    ousing))

(defmethod update ((oexp exporting) (nexp exporting) &optional diffs)
  (let ((diff (assoc oexp diffs)))
    (when diff
      (if (eq (type-of (names oexp)) (type-of (names nexp)))
	  (update (names oexp) (names nexp) (car (last diff)))
	  (setf (names oexp) (names nexp)))
      (update (modules oexp) (modules nexp) (car (last diff))))
    oexp))

(defmethod update ((list1 list) (list2 list) &optional diffs)
  (mapcar #'(lambda (e1 e2) (update e1 e2 diffs))
	  list1 list2))

(defmethod update-decls (decls1 decls2 diffs &optional result collect-tccs?)
  (cond ((null decls1)
	 (nreverse result))
	((typep (car decls1) 'setvar)
	 (update-decls (cdr decls1) decls2 diffs result collect-tccs?))
	((typep (car decls2) 'setvar)
	 (update-decls decls1 (cdr decls2) diffs (cons (car decls2) result)
		       collect-tccs?))
	((generated-by (car decls1))
	 (update-decls (cdr decls1) decls2 diffs
		       (if collect-tccs? (cons (car decls1) result) result)
		       collect-tccs?))
	(t (let ((ndecl (update (car decls1) (car decls2) diffs)))
	     (update-decls (cdr decls1) (cdr decls2) diffs
			   (cons ndecl result)
			   (and (typep ndecl 'declaration)
				(typechecked? ndecl)))))))

;;; Declarations - note that we don't actually untypecheck the given
;;; declaration, since it is going to be replaced anyway.

(defmethod update :around ((odecl declaration) (ndecl declaration)
			   &optional diffs)
  (let ((diff (assoc odecl diffs)))
    (case (car (last diff))
      (signature (untypecheck-references odecl 'signature)
		 ndecl)
      (body (untypecheck-references odecl 'body)
	    ndecl)
      (lexical (call-next-method)
	       odecl)
      (t odecl))))


;;; If we are in the stuff below, then we only have lexical differences.
;;; In general, we will always simply copy rather than testing before
;;; copying; in fact, this is faster for strings.

(defmethod update ((odecl formal-decl) (ndecl formal-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (separator-symbol odecl) (separator-symbol ndecl)))

(defmethod update ((odecl mod-syn) (ndecl mod-syn) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (module-instance odecl) (module-instance ndecl))
  (setf (iseq odecl) (iseq ndecl)))

(defmethod update ((odecl type-decl) (ndecl type-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (type-expr odecl) (type-expr ndecl))
  (setf (spelling odecl) (spelling ndecl)))

(defmethod update ((odecl typed-declaration) (ndecl typed-declaration) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (unless (eq (declared-type odecl) 'type)
    (update (declared-type odecl) (declared-type ndecl))))

(defmethod update ((odecl var-decl) (ndecl var-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (spelling odecl) (spelling ndecl)))

(defmethod update ((odecl defining-decl) (ndecl defining-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (definition odecl) (definition ndecl)))

(defmethod update ((odecl const-decl) (ndecl const-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (kind odecl) (kind ndecl))
  (update (changes odecl) (changes ndecl))
  (update (from-object-decl? odecl) (from-object-decl? ndecl)))

(defmethod update ((odecl def-decl) (ndecl def-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (measure odecl) (measure ndecl)))

(defmethod update ((odecl formula-decl) (ndecl formula-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (spelling odecl) (spelling ndecl))
  (update (kind odecl) (kind ndecl))
  (update (definition odecl) (definition ndecl))
  (update (changes odecl) (changes ndecl)))

(defmethod update ((odecl proof-decl) (ndecl proof-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (kind odecl) (kind ndecl))
  (if (length= (conclusions odecl) (conclusions ndecl))
      (update (conclusions odecl) (conclusions ndecl))
      (progn (break "Update proof-decl")
	     (setf (conclusions odecl) (conclusions ndecl)
		   (typechecked? odecl) nil)))
  (if (length= (premises odecl) (premises ndecl))
      (update (premises odecl) (premises ndecl))
      (setf (premises odecl) (premises ndecl)
	    (typechecked? odecl) nil))
  (update (localvars odecl) (localvars ndecl)))

(defmethod update ((oinst instance) (ninst instance) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (lname oinst) (lname ninst))
  (update (name oinst) (name ninst))
  (setf (suffix oinst) (suffix ninst))
  (update (quoted? oinst) (quoted? ninst))
  (update (substitutions oinst) (substitutions ninst)))

(defmethod update ((oipc ipc-instance) (nipc ipc-instance) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (matching oipc) (matching nipc)))

(defmethod update ((oipc ipc-instantiate) (nipc ipc-instantiate)
		   &optional ign)
  (declare (ignore ign))
  (update (substitutions oipc) (substitutions nipc))
  (update (matching oipc) (matching nipc)))

(defmethod update ((oipc ipc-rewrite) (nipc ipc-rewrite) &optional ign)
  (declare (ignore ign))
  (update (instance oipc) (instance nipc))
  (unless (eq (occurrence oipc) (occurrence nipc))
    (setf (occurrence oipc) (occurrence nipc)))
  (setf (matching oipc) (matching nipc))
  (setf (targets oipc) (targets nipc)))

(defmethod update ((oipc ipc-split) (nipc ipc-split) &optional ign)
  (declare (ignore ign))
  (setf (formids oipc) (formids nipc)))

(defmethod update ((oipc ipc-lemma) (nipc ipc-lemma) &optional ign)
  (declare (ignore ign))
  (update (expr oipc) (expr nipc)))

(defmethod update ((oipc ipc-copy) (nipc ipc-copy) &optional ign)
  (declare (ignore ign))
  (setf (formids oipc) (formids nipc)))

(defmethod update ((oipc ipc-hide) (nipc ipc-hide) &optional ign)
  (declare (ignore ign))
  (setf (formids oipc) (formids nipc)))

(defmethod update ((oipc ipc-reveal) (nipc ipc-reveal) &optional ign)
  (declare (ignore ign))
  (setf (formids oipc) (formids nipc)))

(defmethod update ((oipc ipc-end) (nipc ipc-end) &optional ign)
  (declare (ignore ign))
  nil)


(defmethod update ((osubst substitution) (nsubst substitution) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (variable osubst) (variable nsubst))
  (update (expression osubst) (expression nsubst)))

(defmethod update ((odecl field-decl) (ndecl field-decl) &optional ign)
  (declare (ignore ign))
  (call-next-method))


;;; Type expressions

(defmethod update ((ote type-expr) (nte type-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method))

(defmethod update ((ote enumtype) (nte enumtype) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (elements ote) (elements nte))
  (setf (open-bracket ote) (open-bracket nte))
  (setf (close-bracket ote) (close-bracket nte)))

(defmethod update ((ote type-name) (nte type-name) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (name ote) (name nte)))

(defmethod update ((ote subtype) (nte subtype) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (supertype ote) (supertype nte))
  (update (predicate ote) (predicate nte)))

(defmethod update ((ote funtype) (nte funtype) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (kind ote) (kind nte))
  (update (domain ote) (domain nte))
  (update (range ote) (range nte)))

(defmethod update ((ote statetype) (nte statetype) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (stored-type ote) (stored-type nte)))

(defmethod update ((ote recordtype) (nte recordtype) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (fields ote) (fields nte))
  (setf (ending ote) (ending nte)))

(defmethod update ((oexpr expr) (nexpr expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (setf (parens? oexpr) (parens? nexpr)))

(defmethod update ((oexpr number-expr) (nexpr number-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method))

(defmethod update ((oexpr name-expr) (nexpr name-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (name oexpr) (name nexpr)))

(defmethod update ((oexpr literal-expr) (nexpr literal-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (assignments oexpr) (assignments nexpr)))

;(defmethod update ((oexpr coercion) (nexpr coercion) &optional ign)
;  (declare (ignore ign))
;  (call-next-method)
;  (update (expression oexpr) (expression nexpr))
;  (update (declared-type oexpr) (declared-type nexpr)))

;(defmethod update ((oexpr intype) (nexpr intype) &optional ign)
;  (declare (ignore ign))
;  (call-next-method)
;  (update (expression oexpr) (expression nexpr))
;  (update (declared-type oexpr) (declared-type nexpr)))

(defmethod update ((oexpr if-expr) (nexpr if-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (condition oexpr) (condition nexpr))
  (update (then-part oexpr) (then-part nexpr))
  (update (else-part oexpr) (else-part nexpr))
  (setf (chain? oexpr) (chain? nexpr))
  (setf (case-expr? oexpr) (case-expr? nexpr))
  (setf (endif oexpr) (endif nexpr)))

(defmethod update ((oexpr application) (nexpr application) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (operator oexpr) (operator nexpr))
  (update (arguments oexpr) (arguments nexpr))
  (setf (built-in? oexpr) (built-in? nexpr))
  (setf (from oexpr) (from nexpr))
  (setf (field-selection? oexpr) (field-selection? nexpr)))

(defmethod update ((oexpr binding-expr) (nexpr binding-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (bindings oexpr) (bindings nexpr))
  (update (expression oexpr) (expression nexpr)))

(defmethod update ((oexpr lambda-expr) (nexpr lambda-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method))

(defmethod update ((oexpr quant-expr) (nexpr quant-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method))

(defmethod update ((oexpr update-expr) (nexpr update-expr) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (expression oexpr) (expression nexpr))
  (update (assignments oexpr) (assignments nexpr)))

(defmethod update ((oass assignment) (nass assignment) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (arguments oass) (arguments nass))
  (update (expression oass) (expression nass))
  (setf (parens-added? oass) (parens-added? nass)))

(defmethod update ((oop block-op) (nop block-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (declarations oop) (declarations nop))
  (update (operations oop) (operations nop)))

(defmethod update ((oop opspec) (nop opspec) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (op-call oop) (op-call nop))
  (update (cases oop) (cases nop))
  (update (else-part oop) (else-part nop))
  (setf (ending oop) (ending nop)))

(defmethod update ((oop hoare-sentence) (nop hoare-sentence) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (precondition oop) (precondition nop))
  (update (operations oop) (operations nop))
  (update (postcondition oop) (postcondition nop)))

(defmethod update ((oop assign-op) (nop assign-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (lhs oop) (lhs nop))
  (update (rhs oop) (rhs nop)))

(defmethod update ((oop if-op) (nop if-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (condition oop) (condition nop))
  (update (then-part oop) (then-part nop))
  (update (else-part oop) (else-part nop))
  (setf (endif oop) (endif nop)))

(defmethod update ((oop loop-op) (nop loop-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (condition oop) (condition nop))
  (update (op-sequence oop) (op-sequence nop)))

(defmethod update ((oop exit-op) (nop exit-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (condition oop) (condition nop)))

(defmethod update ((oop assert-op) (nop assert-op) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (assertion oop) (assertion nop)))

(defmethod update ((oname name) (nname name) &optional ign)
  (declare (ignore ign))
  (call-next-method)
  (update (actuals oname) (actuals nname)))

(defmethod update ((oact actual) (nact actual) &optional ign)
  (declare (ignore ign))
  (update (expr oact) (expr nact))
  (setf (param-separator oact) (param-separator nact)))

(defmethod update ((osobj syntactic-object) (nsobj syntactic-object)
		   &optional ign)
  (declare (ignore ign))
  (setf (lexical-start osobj) (lexical-start nsobj)
	(lexical-end osobj) (lexical-end nsobj)
	;;(newline-comment osobj) (newline-comment nsobj)
	(delimited-comment osobj) (delimited-comment nsobj)))

(defmethod update ((osvar setvar) (nsvar setvar) &optional ign)
  (declare (ignore ign))
  (setf (var osvar) (var nsvar)
	(value osvar) (value nsvar)
	(separator osvar) (separator nsvar)))
