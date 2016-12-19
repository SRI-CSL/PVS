;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; judgements.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 22:40:53 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Sep  3 04:19:25 2004
;; Update Count    : 11
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

;;; The context has a judgements slot, which consists of a judgements
;;; instance, which in turn has slots:
;;;   judgement-types-hash: maps expressions to judgement types
;;;   number-judgements-hash:
;;;     eql hashtable on numbers; returns a list of judgements
;;;   name-judgements-hash:
;;;     eq hashtable on declarations; returns a name-judgements instance
;;;     with slots:
;;;       minimal-judgements: a list of fully-instantiated judgements
;;;       generic-judgements: an assoc list of theory instances to judgements
;;;   application-judgements-hash:
;;;     eq hashtable on declarations; returns a application-judgements instance
;;;     with slots:
;;;       argtype-hash: maps argument type lists to judgement types
;;;       judgements-graph: a graph of the fully-instantiated judgements
;;;       generic-judgements: an assoc list of theory instances to judgements

;;; There are three places where judgement declarations are added to the
;;; context:
;;;   1. when typechecking a judgement declaration: add-judgement-decl
;;;   2. when typechecking a new theory: copy-judgements
;;;   3. when importing a theory that has judgements: merge-judgements
;;; When the prover gets a new context, the judgements do not need to be
;;; copied.

;;; There are two times that judgements are looked up in the context:
;;;   1. during set-type, and
;;;   2. when collecting typepred/record-constraints.
;;; The judgements function is used for these.

;;; set-type.lisp uses the following:
;;;   find-best-type: judgement-types
;;; assert.lisp:
;;;   assert-if-inside-sign: judgement-types
;;;   collect-type-constraints-step: judgement-types
;;;   assert-if (application): judgement-types
;;; proofrules.lisp:
;;;   collect-typepreds: judgement-types

;;; Functions for the application-judgement graph.  The
;;; application-judgements-hash of the judgements of a context maps
;;; operator declarations to application-judgements instances, and the
;;; judgements-graph of one of these instances is a bottom node, with a
;;; null judgement-decl.  All judgement declarations for a given operator
;;; may be found in the transitive closure of the parents of this node.

;;; To add a node, we need to find all nodes immediately below the given
;;; one, add the new node to the parents of each of those nodes, 
;;; and add any parents of those nodes that should be above the new node
;;; to the parents of the new node, removing them from the found nodes.

;; (defstruct (judgement-type (:conc-name jt-)
;; 			   (:print-function
;; 			    (lambda (jt stream depth)
;; 			      (declare (ignore depth))
;; 			      (format stream "#(JT ~a: ~a)"
;; 				(jt-type jt) (jt-decl jt)))))
;;   type
;;   decl)

(defvar *nodes-seen* nil)

(defvar *judgements-added* nil)

(defvar *judgement-bound-vars* nil)

(defun add-appl-judgement-node (jdecl graph)
  (assert (application-judgement? jdecl))
  (add-appl-judgement-node* jdecl graph (list jdecl) nil nil))

(defun add-appl-judgement-node* (jdecl graph new-node new-graph ignore)
  (cond ((null graph)
	 (if (memq new-node new-graph)
	     (nreverse new-graph)
	     (cons new-node (nreverse new-graph))))
	((memq (caar graph) ignore)
	 (add-appl-judgement-node*
	  jdecl (cdr graph)
	  new-node
	  (cons (car graph) new-graph)
	  (append (cdar graph) ignore)))
	((judgement-lt jdecl (caar graph))
	 (add-appl-judgement-node*
	  jdecl (cdr graph)
	  (nconc new-node (list (caar graph)))
	  (if (memq new-node new-graph)
	      (cons (car graph) new-graph)
	      (cons (car graph) (cons new-node new-graph)))
	  (append (cdar graph) ignore)))
	((and (judgement-lt (caar graph) jdecl)
	      (not (some #'(lambda (jd) (judgement-lt jd jdecl))
			 (cdar graph))))
	 (multiple-value-bind (lt not-lt)
	     (split-on #'(lambda (jd) (judgement-lt jdecl jd)) (cdar graph))
	   (add-appl-judgement-node*
	    jdecl (cdr graph)
	    (nconc new-node lt)
	    (cons (cons (caar graph) not-lt) new-graph)
	    (append lt ignore))))
	(t (add-appl-judgement-node*
	    jdecl (cdr graph)
	    new-node
	    (cons (car graph) new-graph)
	    ignore))))

(defmethod judgement-eq ((d1 application-judgement) (d2 application-judgement))
  (or (eq d1 d2)
      (tc-eq (judgement-type d1) (judgement-type d2))))

(defmethod judgement-eq ((d1 number-judgement) (d2 number-judgement))
  (or (eq d1 d2)
      (tc-eq (type d1) (type d2))))

(defmethod judgement-eq ((d1 name-judgement) (d2 name-judgement))
  (or (eq d1 d2)
      (and (tc-eq (name d1) (name d2))
	   (tc-eq (type d1) (type d2)))))

(defmethod judgement-lt ((d1 number-judgement) (d2 number-judgement))
  (and (not (eq d1 d2))
       (not (tc-eq (type d1) (type d2)))
       (subtype-of? (type d1) (type d2))))

(defmethod judgement-lt ((d1 name-judgement) (d2 name-judgement))
  (and (not (eq d1 d2))
       (not (tc-eq (type d1) (type d2)))
       (subtype-of? (type d1) (type d2))))

(defmethod judgement-lt ((d1 application-judgement) (d2 application-judgement))
  (and (not (eq d1 d2))
       (judgement-lt (judgement-type d1) (judgement-type d2))))

;;; Should this take dependent types into account?
(defmethod judgement-lt ((t1 funtype) (t2 funtype))
  (and (not (tc-eq t1 t2))
       (subtype-of? (range t1) (range t2))
       (subtype-of? (domain t1) (domain t2))))

(defmethod judgement-subsumes ((d1 application-judgement)
			       (d2 application-judgement))
  (judgement-subsumes (judgement-type d1) (judgement-type d2)))

;;; Should this take dependent types into account?
(defmethod judgement-subsumes ((t1 funtype) (t2 funtype))
  (and (subtype-of? (range t1) (range t2))
       (subtype-of? (domain t2) (domain t1))))
	

;;; Accessors and update functions for the above

(defun number-judgements (number)
  (cdr (assoc number (number-judgements-alist (current-judgements))
	      :test #'=)))

;; (defsetf number-judgements (number) (number-judgements)
;;   (let ((numjs (gensym)))
;;     `(let ((,numjs ,number-judgements))
;;        (push (cons ,number ,numjs)
;; 	     (number-judgements-alist (current-judgements)))
;;        ,numjs)))

(defun name-judgements (name)
  (cdr (assq (declaration name) (name-judgements-alist (current-judgements)))))

(defsetf name-judgements (name) (name-judgements)
  `(progn (push (cons (declaration ,name) ,name-judgements)
		(name-judgements-alist (current-judgements)))
	  ,name-judgements))

(defun application-judgements (application)
  (let ((op (operator* application)))
    (when (typep op 'name-expr)
      (let ((currynum (argument-application-number application)))
	(declare (fixnum currynum))
	(aref (cdr (assq (declaration op)
			 (application-judgements-alist (current-judgements))))
	      (1- currynum))))))


;;; add-judgement-decl (subtype-judgement) is invoked from
;;; typecheck* (subtype-judgement)

(defmethod add-judgement-decl :around (decl &optional quiet?)
  (declare (ignore quiet?))
  (when (call-next-method)
    (pushnew decl (judgement-declarations (current-judgements)) :test #'eq)))

(defmethod add-judgement-decl ((decl subtype-judgement) &optional quiet?)
  ;; Note that this can be called by prove-decl before *in-checker* is t
  (declare (ignore quiet?))
  (assert (not *in-checker*))
  (add-to-known-subtypes (subtype decl) (type decl)))

;;; Invoked from typecheck* (number-judgement)

(defmethod add-judgement-decl ((decl number-judgement) &optional quiet?)
  ;; Note that this can be called by prove-decl before *in-checker* is t
  (declare (ignore quiet?))
  (assert (not *in-checker*))
  (clrhash (judgement-types-hash (current-judgements)))
  (let* ((num (number (number-expr decl)))
	 (entry (assoc num (number-judgements-alist (current-judgements))
		       :test #'=)))
    (if entry
	(unless (member decl (cdr entry) :key #'type :test #'tc-eq)
	  (setf (number-judgements-alist (current-judgements))
		(acons num (cons decl (cdr entry))
		       (remove* entry
				(number-judgements-alist
				 (current-judgements))))))
	(setf (number-judgements-alist (current-judgements))
	      (acons num (cons decl (cdr entry))
		     (number-judgements-alist (current-judgements)))))))

;;; Invoked from typecheck* (name-judgement)

(defmethod add-judgement-decl ((jdecl name-judgement) &optional quiet?)
  ;; Note that this can be called by prove-decl before *in-checker* is t
  (declare (ignore quiet?))
  (assert (not *in-checker*))
  (let* ((decl (declaration (name jdecl)))
	 (entry (assq decl (name-judgements-alist (current-judgements)))))
    (let ((sjdecl (when entry
		    (find-if #'(lambda (jd)
				 (subtype-of? (type jd) (type jdecl)))
		      (minimal-judgements (cdr entry))))))
      (cond (sjdecl
;; 	     (pvs-warning
;; 		 "Judgement ~a.~a (line ~d) is not needed;~%~
;;                   it is subsumed by ~a.~a (line ~d)"
;; 	       (id (module jdecl)) (ref-to-id jdecl) (line-begin (place jdecl))
;; 	       (id (module sjdecl)) (ref-to-id sjdecl)
;; 	       (line-begin (place sjdecl)))
	     )
	    (t (clrhash (judgement-types-hash (current-judgements)))
	       ;; Need to copy the entry - can assume (current-judgements)
	       ;; is already a copy, but name-judgements-alist may be eq to
	       ;; prelude.
	       (setf (name-judgements-alist (current-judgements))
		     (acons decl
			    (if entry
				(copy (cdr entry)
				  'minimal-judgements
				  (cons jdecl
					(delete-if
					    #'(lambda (jd)
						(subtype-of? (type jdecl)
								    (type jd)))
					  (minimal-judgements (cdr entry)))))
				(make-instance 'name-judgements
				  :minimal-judgements (list jdecl)))
			    (remove* entry
				     (name-judgements-alist
				      (current-judgements))))))))))

;;; Invoked from typecheck* (application-judgement) and from decl-context

(defmethod add-judgement-decl ((jdecl application-judgement) &optional quiet?)
  ;; Note that this can be called by prove-decl before *in-checker* is t
  (assert (not *in-checker*))
  (let* ((decl (declaration (name jdecl)))
	 (currynum (length (formals jdecl)))
	 (applalist (application-judgements-alist (current-judgements))))
    ;; Add to the tree pointed to by the vector
    (let* ((entry (assq decl applalist))
	   (vector (cdr entry))
	   (ventry (when (and vector
			      (< (1- currynum) (length vector)))
		     (aref vector (1- currynum))))
	   (new-ventry (if ventry
			   (add-judgement-decl-to-graph jdecl ventry quiet?)
			   (make-instance 'application-judgements
			     :judgements-graph (list (list jdecl))))))
      (unless (or (null new-ventry)
		  (eq ventry new-ventry))
	(let* ((new-length (max (length vector) currynum))
	       (new-vector (make-array new-length :initial-element nil)))
	  (dotimes (i new-length)
	    (if (= i (1- currynum))
		(setf (aref new-vector i) new-ventry)
		(when (and vector (< i (length vector)))
		  (setf (svref new-vector i) (svref vector i)))))
	  (setf (application-judgements-alist (current-judgements))
		(acons decl new-vector (remove* entry applalist)))
	  (clrhash (judgement-types-hash (current-judgements))))))))

(defun add-judgement-decl-to-graph (jdecl entry &optional quiet?)
  (unless (some-judgement-subsumes jdecl (judgements-graph entry) quiet?)
    (copy entry
      'judgements-graph 
      (add-application-judgement jdecl (judgements-graph entry)))
    ;;(format t "~%Judgements for ~a:" (id (declaration (name jdecl))))
    ;;(show-judgements-graph* (parents bottom-node))
    ))

(defun add-application-judgement (jdecl graph)
  ;; This does not work:
  ;;   (assert (or (eq (module jdecl) (current-theory))
  ;; 	      (from-prelude? (module jdecl))
  ;; 	      (assq (module jdecl) (all-usings (current-theory)))))
  (add-appl-judgement-node jdecl
			   (remove-subsumed-application-nodes jdecl graph)))

(defun remove-subsumed-application-nodes (jdecl graph)
  (let ((*nodes-seen* nil))
    (remove-subsumed-application-nodes* jdecl graph)))

(defun remove-subsumed-application-nodes* (jdecl graph &optional new-graph)
  (if (null graph)
      (nreverse new-graph)
      (remove-subsumed-application-nodes*
       jdecl
       (cdr graph)
       (if (judgement-subsumes jdecl (caar graph))
	   new-graph
	   (acons (caar graph)
		  (remove-if #'(lambda (jd) (judgement-subsumes jdecl jd))
		    (cdar graph))
		  new-graph)))))

(defun some-judgement-subsumes (jdecl graph quiet?)
  (let ((sjdecl (car (find-if #'(lambda (adjlist)
				  (judgement-subsumes (car adjlist) jdecl))
		       graph))))
    (when sjdecl
      (unless quiet?
	(pvs-warning
	    "Judgement ~a.~a (line ~d) is not needed;~%~
             it is subsumed by ~a.~a (line ~d)"
	  (id (module jdecl)) (ref-to-id jdecl) (line-begin (place jdecl))
	  (id (module sjdecl)) (ref-to-id sjdecl) (line-begin (place sjdecl))))
      t)))

(defun show-judgements-graph (decl)
  (let ((appl-entry
	 (assq decl (application-judgements-alist (current-judgements)))))
    (if (null appl-entry)
	(format t "~%No application judgements for ~a" (id decl))
	(show-judgements-graph* (judgements-graph appl-entry)))))

(defun show-judgements-graph* (nodes)
  (format t "~{~%~a~}" nodes))

(defmethod add-judgement-decl ((decl expr-judgement) &optional quiet?)
  (declare (ignore quiet?))
  (assert (not *in-checker*))
  (clrhash (judgement-types-hash (current-judgements)))
  ;; For now keep things simple - just a list of decls
  ;; Could keep an index, e.g., top operator of expr
  (push decl (expr-judgements-alist (current-judgements))))


;;; Called from add-to-using -> update-current-context

(defun update-judgements-of-current-context (theory theoryname)
  (merge-imported-judgements (judgements (saved-context theory))
			     theory theoryname)
;;   (assert (judgements-all-visible? *current-context*) ()
;; 	  "After merge")
  )

;;; judgement-rewrites

;;; Returns a list of formulas corresponding to the judgements for the given
;;; expression.  This is generally just the judgement-tcc, except for
;;; recursive judgements, where it is the TCC that would have been generated
;;; if it was a plain judgement.

(defun judgement-rewrites (expr)
  (let ((jdecls (nth-value 1 (judgement-types-expr expr))))
    (mapcar #'judgement-tcc-formula jdecls)))

(defmethod judgement-tcc-formula (jdecl)
  (let ((tcc (find-if #'judgement-tcc? (generated jdecl))))
    (assert tcc)
    (closed-definition tcc)))
  
(defmethod judgement-tcc-formula ((jdecl rec-application-judgement))
  (rewrite-formula jdecl))

;;; judgement-types

(defmethod judgement-types+ (expr)
  (let ((jtypes (judgement-types expr)))
    (if (consp jtypes)
	(if (some #'(lambda (jty) (subtype-of? jty (type expr)))
		  jtypes)
	    jtypes
	    (cons (type expr) jtypes))
	(list (type expr)))))

(defmethod judgement-types+ ((expr lambda-expr-with-type))
  (let ((jtypes (judgement-types expr)))
    (if (consp jtypes)
	;; Have judgement types
	(if (some #'(lambda (jty) (subtype-of? jty (type expr))) jtypes)
	    ;; Don't return the expr type, deal with return-type
	    (if (some #'(lambda (jty)
			  (let* ((alist (when (dep-binding? (domain jty))
					  (mk-subst-alist (domain jty) (bindings expr))))
				 (jrtype (substit (range jty) alist)))
			    (subtype-of? jrtype (return-type expr))))
		      jtypes)
		jtypes
		(let* ((alist (when (dep-binding? (domain (type expr)))
				(mk-subst-alist (domain (type expr)) (bindings expr))))
		       (lrtype (substit (return-type expr) alist))
		       (ftype (mk-funtype (domain (type expr)) lrtype)))
		  #+pvsdebug (assert (null (freevars ftype)))
		  (cons ftype jtypes)))
	    (let* ((alist (when (dep-binding? (domain (type expr)))
			    (mk-subst-alist (bindings expr) (domain (type expr)))))
		   (lrtype (substit (return-type expr) alist)))
	      (if (subtype-of? lrtype (range (type expr)))
		  (let ((ftype (mk-funtype (domain (type expr)) lrtype)))
		    #+pvsdebug (assert (null (freevars ftype)))
		    (cons ftype jtypes))
		  (if (subtype-of? (range (type expr)) lrtype)
		      (cons (type expr) jtypes)
		      (let ((ftype (mk-funtype (domain (type expr)) lrtype)))
			#+pvsdebug (assert (null (freevars ftype)))
			(cons ftype (cons (type expr) jtypes)))))))
	(list (type expr)))))

(defmethod judgement-types ((ex expr))
  (multiple-value-bind (jtypes jdecls)
      (judgement-types-expr ex)
    #+pvsdebug
    (assert (and (listp jtypes) (every #'type-expr? jtypes)))
    #+pvsdebug
    (assert (and (listp jdecls)
		 (every #'(lambda (jd)
				      (if (listp jd)
					  (every #'judgement? jd)
					  (judgement? jd)))
				  jdecls)))
    ;;(when (and (consp jtypes)
    ;;   (member (type ex) jtypes :test #'tc-eq))
    ;;  (break "Why is the real type a judgement type"))
    (when (and *in-checker* *ps*)
      (mapcar #'(lambda (jd)
		  (pushnew jd (dependent-decls *ps*)))
	(jdecls-list jdecls)))
    (values jtypes jdecls)))

;;; A jdecls list is generally a list, but for tuples and records, a vector
;;; of lists is kept instead, corresponding to the vector of types.  This
;;; basically flattens everything into a single list.
(defun jdecls-list (jdecls &optional result)
  (cond ((null jdecls) result)
	((vectorp jdecls) (jdecls-list (coerce jdecls 'list) result))
	((atom jdecls) (if (member jdecls result :test #'tc-eq)
			   result
			   (cons jdecls result)))
	(t (jdecls-list (car jdecls) (jdecls-list (cdr jdecls) result)))))

;;; Most exprs return a list of types, the result of applying judgements to
;;; the expr But for tuple-exprs and record-exprs, the types returned is a
;;; vector E.g., for tuple expr (e1, ..., en), if each ei returns a list of
;;; types Ti, to return the proper list means taking the cartesian product
;;; of the Ti, which can get big.  So we use vectors #(T1 ... Tn) until the
;;; end.
;;; Hence judgement-types-expr returns a form
;;;   J := '(' T* ')' | '#(' J+ ')' | '#(' F+ ')'
;;;   F := '(' id J+ ')'
;;;   Where T is a type-expr, and F is of the form

(defvar *jte-cnt*)

(defun judgement-types-expr (ex)
  (let ((jhash (judgement-types-hash (current-judgements)))
	(*jte-cnt* (if (boundp '*jte-cnt*) (1+ *jte-cnt*) 0)))
    (when (> *jte-cnt* 100) (break "Why so deep?"))
    (multiple-value-bind (jtypes&jdecls there?)
	(gethash ex jhash)
      (if there?
	  (when jtypes&jdecls
	    (remove-judgement-types-of-type
	     (type ex) (car jtypes&jdecls) (cadr jtypes&jdecls)))
	  (multiple-value-bind (types jdecls)
	      (judgement-types* ex)
	    (setf (gethash ex jhash) (list types jdecls))
	    (values types jdecls))))))

(defmethod judgement-types ((ex tuple-expr))
  nil)

(defmethod judgement-types ((ex record-expr))
  nil)

(defmethod judgement-types* :around (ex)
  (let ((jhash (judgement-types-hash (current-judgements))))
    (multiple-value-bind (jtypes&jdecls there?)
	(gethash ex jhash)
      (if there?
	  (values-list jtypes&jdecls)
	  (multiple-value-bind (njtypes njdecls)
	      ;; This gets number, name, and application judgements
	      (call-next-method)
	    (assert (length= njtypes njdecls))
	    (multiple-value-bind (ejtypes ejdecls)
		;; This gets expr-judgements
		(expr-judgement-types ex njtypes)
	      (assert (length= ejtypes ejdecls))
	      (let ((jtypes (if ejtypes (append njtypes ejtypes) njtypes))
		    (jdecls (if ejdecls (append njdecls ejdecls) njdecls)))
		(assert (length= jtypes jdecls))
		;(when (cdr jtypes) (break "Multiple judgement-types"))
		#+pvsdebug (assert (or (and (name-expr? ex)
					    (skolem-const-decl? (declaration ex)))
				       (vectorp jtypes)
				       (not (member (type ex) jtypes :test #'tc-eq))))
		#+pvsdebug (assert (valid-judgement-type-value jtypes))
		#+pvsdebug (assert (or (and (listp njtypes) (listp jdecls))
				       (and (vectorp njtypes)
					    (or (null jdecls)
						(vectorp jdecls)))))
		(setf (gethash ex jhash)
		      (list jtypes jdecls))
		(values jtypes jdecls))))))))

(defvar *in-expr-judgement-types* nil)

(defun expr-judgement-types (ex jtypes)
  (unless (memq ex *in-expr-judgement-types*)
    (let ((*in-expr-judgement-types* (cons ex *in-expr-judgement-types*)))
      (expr-judgement-types* ex (expr-judgements-alist (current-judgements))
			     jtypes nil nil))))

(defun expr-judgement-types* (ex expr-jdecls jtypes ejtypes ejdecls)
  (if (null expr-jdecls)
      (values ejtypes ejdecls)
      (let ((ejdecl (car expr-jdecls)))
	(if (some #'(lambda (jty) (subtype-of? jty (type ejdecl)))
		  (if (vectorp jtypes)
		      ejtypes
		      (append ejtypes jtypes)))
	    ;; No new information
	    (expr-judgement-types* ex (cdr expr-jdecls) jtypes ejtypes ejdecls)
	    (let* ((ejex (if (forall-expr? (closed-form ejdecl))
			     (car (expression (closed-form ejdecl)))
			     (car (closed-form ejdecl))))
		   (ejty (if (forall-expr? (closed-form ejdecl))
			     (cadr (expression (closed-form ejdecl)))
			     (cadr (closed-form ejdecl))))
		   (bindings (simple-match ejex ex)))
	      (if (or (eq bindings 'fail)
		      (and (forall-expr? (closed-form ejdecl))
			   (not (every #'(lambda (bd) (assq bd bindings))
				       (bindings (closed-form ejdecl))))))
		  (expr-judgement-types* ex (cdr expr-jdecls)
					 jtypes ejtypes ejdecls)
		  (let ((ejtype (substit ejty bindings)))
		    #+pvsdebug
		    (assert (every #'(lambda (fv)
				       (member fv (freevars ex) :test #'same-declaration))
				   (freevars ejtype)))
		    (expr-judgement-types* ex (cdr expr-jdecls) jtypes
					   (cons ejtype ejtypes)
					   (cons ejdecl ejdecls)))))))))

(defun valid-judgement-type-value (types)
  (if (listp types)
      (every #'type-expr? types)
      (and (vectorp types)
	   (if (symbolp (car (aref types 0)))
	       (every #'(lambda (tys)
			  (and (symbolp (car tys))
			       (valid-judgement-type-value (cdr tys))))
		      types)
	       (every #'valid-judgement-type-value types)))))

(defmethod judgement-types* ((ex number-expr))
  ;; Want the least type defined in the prelude
  ;; *even_int* *odd_int* *even_nat* *even_posnat* *odd_posnat*
  ;; 0 - *even_nat* *even_int* ...
  ;; 1 - *odd_posnat* *odd_int* ...
  ;; 2 - *even_posnat*
  (let ((jdecls (cdr (assoc (number ex)
			    (number-judgements-alist (current-judgements))
			    :test #'=))))
    (values
     (append (mapcar #'type jdecls)
	     (list (least-available-numeric-type (number ex))))
     ;; TODO: make a judgement-decl for this
     ;; nil is a placeholder for now
     (append jdecls (list nil)))))

(defmethod judgement-types* ((ex rational-expr))
  (let ((jdecls (cdr (assoc (number ex)
			    (number-judgements-alist (current-judgements))
			    :test #'=))))
    (values
     (append (mapcar #'type jdecls)
	     (list (least-available-numeric-type (number ex))))
     (append jdecls (list nil)))))

(defun least-available-numeric-type (num)
  (assert (rationalp num))
  (if (integerp num)
      (if (and *even_int* *odd_int*)
	  (if (and *even_nat* *even_posnat* *odd_posnat*
		   *even_negint* *odd_negint*)
	      (cond ((zerop num) *even_nat*)
		    ((plusp num)
		     (if (evenp num) *even_posnat* *odd_posnat*))
		    (t (if (evenp num) *even_negint* *odd_negint*)))
	      (if (evenp num) *even_int* *odd_int*))
	  (available-numeric-type num))
      ;; rational, but not integer
      (available-numeric-type num)))

(defmethod judgement-types* :around ((ex name-expr))
  (when (or (assoc (declaration ex) *judgement-bound-vars*
		   :key #'declaration)
	    (not (variable? ex)))
    (call-next-method)))

(defmethod judgement-types* ((ex name-expr))
  (let ((entry (name-judgements ex)))
    (when entry
      (let* ((mjudgements (remove-if (complement
				      #'(lambda (mj)
					  (and (not (subtype-of? (type ex) (type mj)))
					       (strict-compatible? (type mj) (type ex)))))
			    (minimal-judgements entry)))
	     (mtypes (mapcar #'type mjudgements)))
	#+pvsdebug (assert (not (member (type ex) mtypes :test #'tc-eq)))
	(if (generic-judgements entry)
	    (multiple-value-bind (inst-types gjdecls)
		(instantiate-generic-judgement-types
		 ex (generic-judgements entry))
	      #+pvsdebug (assert (or (null inst-types) gjdecls))
	      #+pvsdebug (assert (or (skolem-const-decl? (declaration ex))
				     (vectorp inst-types)
				     (not (member (type ex) inst-types :test #'tc-eq))))
	      (values 
	       (append inst-types mtypes)
	       (append gjdecls mjudgements)))
	    (values mtypes mjudgements))))))

(defun instantiate-generic-judgement-types (name judgements
						 &optional types jdecls)
  (if (null judgements)
      (values types jdecls)
      (let* ((type (instantiate-generic-judgement-type name (car judgements)))
	     (add? (and type (fully-instantiated? type)
			(not (subtype-of? (type name) type))
			(compatible? type (type name)))))
	(instantiate-generic-judgement-types
	 name
	 (cdr judgements)
	 (if add? (cons type types) types)
	 (if add? (cons (car judgements) jdecls) jdecls)))))

(defun instantiate-generic-judgement-type (name judgement)
  (let ((bindings (tc-match name (name judgement)
			    (mapcar #'list
			      (formals-sans-usings (module judgement))))))
    (cond ((every #'cdr bindings)
	   (let ((jthinst (mk-modname (id (module judgement))
			    (mapcar #'(lambda (a) (mk-actual (cdr a)))
			      bindings))))
	     (subst-mod-params (type judgement) jthinst (module judgement))))
	  ((some #'cdr bindings)
	   (let ((stype (subst-theory-params (type judgement) bindings)))
	     (when (fully-instantiated? stype)
	       stype))))))

(defmethod judgement-types* ((ex application))
  (let ((op (operator* ex)))
    (typecase op
      (name-expr
       (let* ((decl (declaration op))
	      (vector (cdr (assq decl
				 (application-judgements-alist
				  (current-judgements))))))
	 (when vector
	   (let* ((currynum (argument-application-number ex))
		  (entry (when (<= currynum (length vector))
			   (aref vector (1- currynum)))))
	     (when entry
	       (multiple-value-bind (gtypes ijdecls)
		   (compute-application-judgement-types
		    ex (judgements-graph entry))
		 (assert (length= gtypes ijdecls))
		 (multiple-value-bind (jtypes jdecls)
		     (generic-application-judgement-types
		      ex (generic-judgements entry) gtypes ijdecls)
		   (assert (length= jtypes jdecls))
		   #+pvsdebug (assert (or (vectorp jtypes)
					  (not (member (type ex) jtypes :test #'tc-eq))))
		   (values jtypes jdecls))))))))
      (lambda-expr
       (multiple-value-bind (ljtypes ljdecls)
	   ;; For lambda expressions, we get the judgement-types of the args,
	   ;; and temporarily add new ones to the associated bindings, then
	   ;; get the judgement types of the expression of the lambda-expr.
	   ;; Note that this is not the same as the judgements of the beta-reduced
	   ;; form, which loses information.
	   (let ((*judgement-bound-vars* *judgement-bound-vars*)
		 (jbvars nil))
	     (unwind-protect
		  (let ((*bound-variables* (append (bindings (operator* ex))
						   *bound-variables*)))
		    (setq jbvars (set-binding-judgements-from-arg-judgements ex))
		    (setq *judgement-bound-vars* (append jbvars *judgement-bound-vars*))
		    (judgement-types* (expression* op)))
	       (dolist (jbvar jbvars)
		 (setf (gethash (car jbvar) (judgement-types-hash
					     (current-judgements)))
		       (cdr jbvar)))))
	 (when ljtypes
	   (let* ((bndlist (bindings* op))
		  (arglist (argument* ex))
		  (sljtypes (subst-judgement-types ljtypes bndlist arglist)))
	     (remove-judgement-types-of-type (type ex) sljtypes ljdecls))))))))

;; (defmethod judgement-types* ((ex list-expr))
;;   (let* ((list-args (list-arguments ex))
;; 	 (etype (type-value (car (actuals (module-instance (operator ex))))))
;; 	 (jtypes (reduce #'(lambda (t1 t2) (join-compatible-types t1 t2 etype))
;; 			 (mapcar #'judgement-types* list-args))))
;;     (mapcar #'(lambda (jty) 
		

(defun subst-judgement-types (jtypes bndlist arglist)
  (if (null bndlist)
      jtypes
      (subst-judgement-types
       (substit jtypes
	 (if (singleton? (car bndlist))
	     (acons (caar bndlist) (car arglist) nil)
	     (pairlis (car bndlist)
		      (if (typep (car arglist) 'tuple-expr)
			  (exprs (car arglist))
			  (make-projections (car arglist))))))
       (cdr bndlist)
       (cdr arglist))))

(defun set-binding-judgements-from-arg-judgements (ex)
  (let* ((op (operator* ex))
	 (args (argument* ex)))
    (set-binding-judgements-from-arg-judgements* op args)))

(defun set-binding-judgements-from-arg-judgements* (op argslist
						       &optional jbvars)
  (if (null argslist)
      (nreverse jbvars)
      (let* ((bindings (bindings op))
	     (args (if (cdr bindings)
		       (argument-list (car argslist))
		       (list (car argslist))))
	     (jhash (judgement-types-hash (current-judgements))))
	(if (length= bindings args)
	    (mapc #'(lambda (bd arg)
		      (let* ((btype (type bd))
			     (atypes (judgement-types+ arg))
			     (ntypes (remove-if #'(lambda (aty)
						    (subtype-of? btype aty))
				       atypes)))
			(when ntypes
			  (let* ((nm (make-variable-expr bd))
				 (entry (gethash nm jhash)))
			    (push (cons nm entry) jbvars)
			    (setf (gethash nm jhash)
				  (list (append ntypes (car entry))
					(append (make-list (length ntypes)
							   :initial-element
							   nil)
						(cadr entry))))))))
		  bindings args)
	    ;; In this case, we have multiple bindings and one argument We
	    ;; get the judgement types for that argument, then collect the
	    ;; components of the tupletype.
	    (let* ((jtypes (judgement-types+ (car argslist)))
		   (ajtypes-list
		    (collect-component-arg-judgement-types jtypes)))
	      #+pvsdebug (assert (or (null bindings)
				     (null ajtypes-list)
				     (length= ajtypes-list bindings)))
	      (when ajtypes-list
		(mapc #'(lambda (bd ajtypes)
			  (let* ((btype (type bd))
				 (ntypes (remove-if #'(lambda (aty)
							(subtype-of? btype aty))
					   ajtypes)))
			    (when ntypes
			      (let* ((nm (make-variable-expr bd))
				     (entry (gethash nm jhash)))
				(push (cons nm entry) jbvars)
				(setf (gethash nm jhash)
				      (list (append ntypes (car entry))
					    (append (make-list (length ntypes)
							       :initial-element
							       nil)
						    (cadr entry))))))))
		      bindings ajtypes-list))))
	(set-binding-judgements-from-arg-judgements*
	 op (cdr argslist) jbvars))))

(defun collect-component-arg-judgement-types (jtypes &optional ajtypes-list)
  (if (null jtypes)
      (mapcar #'nreverse ajtypes-list)
      (let ((tuptype (find-supertype (car jtypes))))
	(collect-component-arg-judgement-types
	 (cdr jtypes)
	 (if (tupletype? tuptype)
	     (if (null ajtypes-list)
		 (mapcar #'list (types tuptype))
		 (mapcar #'(lambda (ty ajtypes)
			     (if (member ty ajtypes :test #'tc-eq)
				 ajtypes
				 (cons ty ajtypes)))
		   (types tuptype) ajtypes-list))
	     ajtypes-list)))))

(defmethod expression* ((ex binding-expr) &optional bex)
  (if (or (null bex) (same-binding-op? ex bex))
      (expression* (expression ex) ex)
      ex))

(defmethod expression* (ex &optional bex)
  (declare (ignore bex))
  ex)

;; (defun argtypes-subtype-of-domain-type (atypes dtype)
;;   (let ((sdtype (find-supertype dtype)))
;;     (if (vectorp atypes)
;; 	(if (tupletype? sdtype)
;; 	    (every #'argtypes-subtype-of-domain-type* atypes (types sdtype))
;; 	    ;; Must be a recordtype
;; 	    (every #'(lambda (a)
;; 		       (arg-field-types-subtype-of-domain-type
;; 			a (fields sdtype)))
;; 		   atypes))
;; 	(some #'(lambda (aty) (subtype-of? aty sdtype)) atypes))))

;; (defun argtypes-subtype-of-domain-type* (atypes type)
;;   (some #'(lambda (aty) (subtype-of? aty type)) atypes))

;; (defun arg-field-types-subtype-of-domain-type (aentry fields)
;;   (let* ((id (car aentry))
;; 	 (fldtype (type (find id fields :key #'id))))
;;     (some #'(lambda (aty) (subtype-of? aty fldtype)) (cdr aentry))))

;; (defun simple-application-judgement-types (ex)
;;   (let ((op (operator* ex)))
;;     (when (and (declaration? (current-declaration))
;; 	       (name-expr? op)
;; 	       ;; Check that actuals were derived
;; 	       (null (actuals op))
;; 	       (actuals (module-instance op)))
;;       (let ((ajtypes (mapcar #'judgement-types* (argument* ex))))
;; 	))))

(defmethod judgement-types* ((ex branch))
  (multiple-value-bind (then-types then-jdecls)
      (judgement-types* (then-part ex))
    (multiple-value-bind (else-types else-jdecls)
	(judgement-types* (else-part ex))
      (when (or then-types else-types)
	(cond ((tc-eq then-types else-types)
	       (values then-types then-jdecls))
	      ((and (vectorp then-types) (vectorp else-types))
	       (let ((comp-types (join-compatible-vector-types
				  then-types else-types (type ex))))
		 (when comp-types
		   (values comp-types
			   (cond ((null then-jdecls)
				  else-jdecls)
				 ((null else-jdecls)
				  then-jdecls)
				 (t (map 'vector #'jdecls-union
					 then-jdecls else-jdecls)))))))
	      ;; May have to convert a vector, might be better the other way
	      (t (let* ((ttypes (jtypes-to-types then-types (then-part ex)))
			(etypes (jtypes-to-types else-types (else-part ex)))
			(comp-types (join-compatible-types ttypes etypes (type ex))))
		   (when (and comp-types
			      (or then-jdecls else-jdecls))
		     (let ((comp-jdecls (make-list (length comp-types))))
		       (dotimes (i (length comp-types))
			 (let ((pos (position (elt comp-types i)
					      (if then-types ttypes etypes)
					      :test #'tc-eq)))
			   (when pos
			     (setf (elt comp-jdecls i)
				   (elt (if then-types then-jdecls else-jdecls) pos)))))
		       (values comp-types comp-jdecls))))))))))

;; Want the least types and corresponding judgement decls that cover all
;; exprs in the list 
(defmethod judgement-types* ((expr-list cons))
  #+pvsdebug (assert (every #'expr? expr-list))
  (multiple-value-bind (types jdecls)
      (judgement-types* (car expr-list))
    (judgement-types-list (cdr expr-list) (type (car expr-list)) types jdecls)))

;;; Recursively determing the judgement types and corresponding judgement
;;; decls for a list of expressions, that are intended to have a common type
;;; Mostly for if-exprs, cond-exprs, etc.  But not tuple exprs

;;; The type is the least connon actual type of the expressions so far - no
;;; point in looking higher than this.  The types and jdecls are the
;;; judgement types found so far.  Note that even if they are empty, we may
;;; need judgements on the remaining expressions, in order to not go above
;;; the type.
(defun judgement-types-list (expr-list type types jdecls)
  (cond ((null expr-list)
	 (values types jdecls))
	((null types)
	 ;; Shouldn't produce anything smaller than type; may have to get larger
	 (if (subtype-of? (type (car expr-list)) type)
	     ;; No use to get judgements in this case
	     (judgement-types-list (cdr expr-list) type nil nil)
	     (multiple-value-bind (ntypes njdecls)
		 (judgement-types* (car expr-list))
	       (if (null ntypes)
		   ;; Still no judgements available
		   (judgement-types-list (cdr expr-list)
					 (compatible-type type (type (car expr-list)))
					 nil nil)
		   ;; Look for an ntype that is tc-eq to type
		   (let ((pos (position-if #'(lambda (nty) (tc-eq nty type)) ntypes)))
		     (if pos
			 (judgement-types-list
			  (cdr expr-list)
			  type (list (elt ntypes pos)) (list (elt njdecls pos)))
			 ;; OW look for all judgements that are a subtype of type
			 (let ((stypes nil) (sjdecls nil))
			   (dotimes (i (length ntypes))
			     (when (subtype-of? (elt ntypes i) type)
			       (push (elt ntypes i) stypes)
			       (push (elt njdecls i) sjdecls)))
			   ;; Note that stypes could be empty
			   (if (null stypes)
			       (judgement-types-list
				(cdr expr-list)
				(compatible-type type (type (car expr-list)))
				nil nil)
			       (judgement-types-list
				(cdr expr-list) type (nreverse stypes) (nreverse sjdecls))))))))))
	(t ;; Have types - this is more difficult to reconcile
	 (multiple-value-bind (ntypes njdecls)
	     (judgement-types* (car expr-list))
	   (if (null ntypes)
	       (if (some #'(lambda (ty) (subtype-of? (type (car expr-list)) ty))
			 types)
		   (judgement-types-list (cdr expr-list) type types jdecls)
		   (break "types, null ntypes, not subtype"))
	       ;; Have ntypes - if one is equal to type, keep it and remove the rest
	       ;; else if any are subtypes of type, keep those that are
	       (break "types, ntypes"))))))

(defun jdecls-union (jdecls1 jdecls2)
  (let ((ldecls1 (jdecls-list jdecls1))
	(ldecls2 (jdecls-list jdecls2)))
    (union ldecls1 ldecls2 :test #'tc-eq)))

(defun jtypes-to-types (jtypes ex)
  (typecase jtypes
    (vector (jvector-to-types jtypes))
    (null (when ex (list (type ex))))
    (t jtypes)))

;;; Converts the internal representation of the set of judgement
;;; types for a given expr
(defun jvector-to-types (jtypes)
  (let ((tlist (map 'list #'(lambda (x) (coerce x 'list)) jtypes)))
    #+pvsdebug (assert (every #'consp tlist))
    (if (symbolp (caar tlist))
	;; These are field ids - create recordtypes
	(let* ((field-decls (mapcar #'(lambda (fld&types)
					(mapcar #'(lambda (ty)
						    (mk-field-decl
						     (car fld&types) ty ty))
					  (jtypes-to-types (cdr fld&types) nil)))
			      tlist))
	       (recfields (cartesian-product field-decls)))
	  (mapcar #'(lambda (rf) (make-recordtype rf))
	    recfields))
	;; otherwise tupletypes
	(mapcar #'mk-tupletype (cartesian-product tlist)))))

(defun jvector-to-decls (jdecls)
  (let ((dlist (map 'list #'(lambda (x) (coerce x 'list)) jdecls)))
    (cartesian-product dlist)))

(defun join-compatible-vector-types (types1 types2 type)
  (assert (= (length types1) (length types2)))
  (let ((ntypes (make-array (length types1) :initial-element nil)))
    (dotimes (i (length types1))
      (let ((t1 (svref types1 i)))
	(if (and (car t1) (symbolp (car t1)))
	    ;; This is for a record type
	    (let ((t2 (find (car t1) types2 :key #'car))
		  (fld (find (car t1) (fields type) :key #'id)))
	      (assert t2)
	      (assert fld)
	      (setf (aref ntypes i)
		    (if (tc-eq t1 t2)
			t1
			(cons (car t1) (join-minimal-types (cdr t1) (cdr t2))))))
	    ;; Else a tuple type
	    (setf (aref ntypes i)
		  (join-compatible-types t1 (svref types2 i) (nth i (types type)))))))
    ntypes))

;; types1 and types2 are both minimal types lists, and are compatible
;; result is new list of minimal types.
;; 2 has mintypes (even_int, posint), 3 has (odd_int, posint)
;; the result is (posint), as even_int is not a subtype of anything in 
;; Suppose types1 = (a1 b1 c1), so the given ex1 has these minimal types
;;     and types2 = (a2 b2 c2), ex2 has these minimal types
(defun join-minimal-types (types1 types2 &optional rem min)
  (if (null types1)
      (join-minimal-types* types2 min rem)
      (if (member (car types1) types2 :test #'tc-eq)
	  (join-minimal-types (cdr types1)
			      (remove (car types1) types2 :test #'tc-eq)
			      rem (cons (car types1) min))
	  (let ((ty2 (find-if #'(lambda (ty2) (strict-subtype-of? (car types1) ty2)) types2)))
	    (if ty2
		(join-minimal-types (cdr types1) (remove ty2 types2)
				    rem (cons ty2 min))
		(join-minimal-types (cdr types1) types2
				    (if (null rem)
					(car types1)
					(let ((ctype (compatible-type rem (car types1))))
					  (unless (some #'(lambda (mty) (subtype-of? mty ctype))
							min)
					    ctype)))
				    min))))))

(defun join-minimal-types* (types2 min rem)
  (if (null types2)
      (if rem
	  (cons rem min)
	  min)
      (if (and rem (subtype-of? rem (car types2)))
	  (join-minimal-types* (cdr types2) min rem)
	  (join-minimal-types* (cdr types2) min
			       (if (null rem)
				   (car types2)
				   (let ((ctype (compatible-type rem (car types2))))
				      (unless (some #'(lambda (mty) (subtype-of? mty ctype))
						    min)
					ctype)))))))

(defun join-compatible-types (types1 types2 type &optional compats)
  (if (null types1)
      (delete type compats :test #'tc-eq)
      (join-compatible-types
       (cdr types1)
       types2
       type
       (join-compatible-types* (car types1) types2 compats))))

(defun join-compatible-types* (type types compats)
  (if (null types)
      compats
      (let ((ctype (compatible-type type (car types))))
	(join-compatible-types*
	 type (cdr types)
	 (if (some #'(lambda (ty) (subtype-of? ty ctype)) compats)
	     compats
	     (cons ctype (delete-if #'(lambda (ty)
					(subtype-of? ty ctype))
			   compats)))))))

(defun generic-application-judgement-types (ex gen-judgements jtypes jdecls)
  (assert (length= jtypes jdecls))
  (if (null gen-judgements)
      (values jtypes jdecls)
      (let* ((jdecl (car gen-judgements))
	     (jtype (instantiate-generic-appl-judgement-type ex jdecl)))
	(if jtype
	    (let* ((arguments (argument* ex))
		   (argtypes (mapcar #'judgement-types* arguments))
		   (domains (operator-domain* jtype arguments nil))
		   (range (operator-range* jtype arguments))
		   (rdomains (operator-domain ex)))
	      (multiple-value-bind (jrange rjdecl)
		  (when (length= argtypes (formals jdecl))
		    (compute-appl-judgement-range-type
		     arguments argtypes rdomains domains range jdecl))
		(let* ((dont-add? (or (null jrange)
				      (subtype-of? (type ex) jrange)
				      (some #'(lambda (jty)
						(subtype-of? jty jrange))
					    jtypes)))
		       (rtypes nil)
		       (rdecls nil)
		       (otypes (if dont-add?
				   jtypes
				   (cons jrange
					 (remove-if #'(lambda (jty)
							(subtype-of? jrange jty))
					   jtypes)))))
		  (if dont-add?
		      (setq rtypes jtypes
			    rdecls jdecls)
		      (progn
			(mapc #'(lambda (jty jd)
				  (unless (subtype-of? jrange jty)
				    (push jty rtypes)
				    (push jd rdecls)))
			      jtypes jdecls)
			(push jrange rtypes)
			(push rjdecl rdecls)))
		  (assert (length= rtypes otypes))
		  (assert (length= rtypes rdecls))
		  (generic-application-judgement-types
		   ex
		   (cdr gen-judgements)
		   rtypes
		   rdecls))))
	    (generic-application-judgement-types
	     ex
	     (cdr gen-judgements)
	     jtypes
	     jdecls)))))

(defun instantiate-generic-appl-judgement-types (ex judgements &optional types)
  (if (null judgements)
      types
      (let ((type (instantiate-generic-appl-judgement-type
		   ex (car judgements))))
	(instantiate-generic-appl-judgement-types
	 ex
	 (cdr judgements)
	 (if type (cons type types) types)))))

(defun instantiate-generic-appl-judgement-type (ex judgement)
  (let* ((bindings1 (tc-match (operator* ex) (name judgement)
			     (mapcar #'list
			       (formals-sans-usings (module judgement)))))
	 (bindings (when bindings1
		     (if (or (null (formals judgement))
			     (every #'cdr bindings1))
			 bindings1
			 (tc-match (car (judgement-types+ (argument ex)))
				   (if (cdr (car (formals judgement)))
				       (mk-tupletype
					(mapcar #'type
					  (car (formals judgement))))
				       (type (caar (formals judgement))))
				   bindings1)))))
    (when (and bindings (every #'cdr bindings))
      (let* ((th (module judgement))
	     (lib-id (when (library-datatype-or-theory? th)
		       (car (rassoc (lib-ref th) (current-library-alist)
				    :test #'equal))))
	     (jthinst (mk-modname (id th)
			(mapcar #'(lambda (a) (mk-actual (cdr a)))
			  bindings)
			lib-id)))
	;;(assert (fully-instantiated? jthinst))
	(subst-mod-params (judgement-type judgement) jthinst th)))))

(defmethod no-dep-bindings ((te funtype))
  (no-dep-bindings (domain te)))

(defmethod no-dep-bindings ((te tupletype))
  (every #'no-dep-bindings (types te)))

(defmethod no-dep-bindings ((te cotupletype))
  (every #'no-dep-bindings (types te)))

(defmethod no-dep-bindings ((te dep-binding))
  nil)

(defmethod no-dep-bindings ((te type-expr))
  t)

(defmethod judgement-arg-types ((ex application) &optional argtypes)
  (judgement-arg-types (operator ex)
		       (cons (judgement-types* (argument ex)) argtypes)))

(defmethod judgement-arg-types ((ex name-expr) &optional argtypes)
  argtypes)

(defun compute-application-judgement-types (ex graph)
  #+pvsdebug (assert (every #'fully-instantiated? (mapcar #'car graph)))
  (let* ((args-list (argument* ex))
	 (cgraph (remove-if (complement
			     #'(lambda (x)
				 (compatible? (type (car x)) (type ex))))
		   graph))
	 (op (operator* ex))
	 (thinst (module-instance op))
	 (theory (module (declaration op))))
    (compute-appl-judgement-types
     args-list
     (mapcar #'judgement-types* args-list) ;; Not judgement-types+
     (operator-domain ex)
     (type ex) thinst theory cgraph)))

(defun operator-domain (ex)
  (operator-domain* (type (operator* ex)) (argument* ex) nil))

(defmethod operator-domain* ((te funtype) (args cons) domains)
  (operator-domain* (range te) (cdr args) (cons (domain te) domains)))

(defmethod operator-domain* ((te subtype) (args cons) domains)
  (operator-domain* (supertype te) args domains))

(defmethod operator-domain* ((te dep-binding) (args cons) domains)
  (operator-domain* (type te) args domains))

(defmethod operator-domain* ((te type-expr) (args null) domains)
  (nreverse domains))

(defun operator-range (ex)
  (operator-range* (type (operator* ex)) (argument* ex)))

(defmethod operator-range* ((te funtype) (args cons))
  (operator-range* (range te) (cdr args)))

(defmethod operator-range* ((te subtype) (args cons))
  (operator-range* (supertype te) args))

(defmethod operator-range* ((te dep-binding) (args cons))
  (operator-range* (type te) args))

(defmethod operator-range* ((te type-expr) (args null))
  te)

(defun compute-appl-judgement-types (arguments argtypes rdomains extype
					       thinst theory graph
					       &optional jtypes exclude jdecls)
  (if (null graph)
      (values (nreverse jtypes) (nreverse jdecls))
      (let* ((excluded? (memq (caar graph) exclude)))
	(multiple-value-bind (range rjdecl)
	    (unless excluded?
	      (compute-appl-judgement-types*
	       arguments argtypes rdomains thinst theory
	       (caar graph)))
	  (let* ((dont-add? (or (null range)
				;; TODO: vectors@closest_approach_2D.gt_D_t1_lt_t2 doesn't
				;; prove with this uncommented - not sure why
				;; (subtype-of? extype range)
				(some #'(lambda (r) (subtype-of? r range))
				      jtypes)))
		 (rtypes nil)
		 (rdecls nil)
		 (otypes (if dont-add?
			     jtypes
			     (cons range
				   (remove-if #'(lambda (r) (subtype-of? range r))
				     jtypes)))))
	    (assert (length= jtypes jdecls))
	    (if dont-add?
		(setq rtypes jtypes
		      rdecls jdecls)
		(progn 
		  (mapc #'(lambda (jty jd)
			    (unless (subtype-of? range jty)
			      (push jty rtypes)
			      (push jd rdecls)))
			jtypes jdecls)
		  (push range rtypes)
		  (push rjdecl rdecls)))
	    (assert (length= otypes rtypes))
	    (assert (or rtypes (null jtypes)))
	    (assert (length= rtypes rdecls))
	    (compute-appl-judgement-types
	     arguments argtypes rdomains extype thinst theory
	     (cdr graph)
	     rtypes
	     (if (or range excluded?)
		 (append (car graph) exclude)
		 exclude)
	     rdecls))))))

(defun compute-appl-judgement-types* (arguments argtypes rdomains thinst theory jdecl)
  (let* ((jtype (subst-mod-params (judgement-type jdecl)
		    thinst theory jdecl))
	 (domains (operator-domain* jtype arguments nil))
	 (range (operator-range* jtype arguments)))
    (when (length= argtypes (formals jdecl))
      ;;(assert (length= argtypes domains))
      ;;(assert (length= argtypes rdomains))
      ;;(assert (length= argtypes arguments))
      (compute-appl-judgement-range-type
       arguments argtypes rdomains domains range jdecl))))


;;; argtypes here is a list of either
;;;  1. vectors of types, if the argument is a record or tuple expr.
;;;  2. lists of types, otherwise

(defun compute-appl-judgement-range-type (arguments argtypes rdomains domains
					  range jdecl)
  (if (null arguments)
      (values range jdecl)
      (when (judgement-arguments-match?
	     (car arguments) (car argtypes) (car rdomains) (car domains))
	(let* ((srdoms (if (typep (car rdomains) 'dep-binding)
			   (substit (cdr rdomains)
			     (acons (car rdomains) (car arguments) nil))
			   (cdr rdomains)))
	       (sdoms (if (typep (car domains) 'dep-binding)
			  (substit (cdr domains)
			    (acons (car domains) (car arguments) nil))
			  (cdr domains)))
	       (rbindings (mapcan #'(lambda (odom ndom)
				      (when (and (dep-binding? odom)
						 (not (eq odom ndom)))
					(list (cons odom ndom))))
			    (cdr rdomains) srdoms))
	       (bindings (mapcan #'(lambda (odom ndom)
				     (when (and (dep-binding? odom)
						(not (eq odom ndom)))
				       (list (cons odom ndom))))
			   (cdr domains) sdoms)))
	  (compute-appl-judgement-range-type
	   (cdr arguments)
	   (cdr argtypes)
	   (substit srdoms rbindings)
	   (substit sdoms bindings)
	   (if (typep (car domains) 'dep-binding)
	       (substit range (acons (car domains) (car arguments) bindings))
	       (substit range bindings))
	   jdecl)))))

(defun judgement-arguments-match? (argument argtypes rdomain jdomain)
  ;;(assert (fully-instantiated? rdomain))
  ;;(assert (fully-instantiated? jdomain))
  ;;(assert (every #'fully-instantiated? argtypes))
  (and (compatible? rdomain jdomain)
       (if (null argtypes)
	   (judgement-arguments-match*?
	    argument (list (type argument)) rdomain jdomain)
	   (judgement-arguments-match*?
	    argument argtypes rdomain jdomain))))


;;; argtypes here is either
;;;  1. a vector of types, if the argument is a record or tuple expr.
;;;  2. a list of types, otherwise

(defun judgement-arguments-match*? (argument argtypes rdomain jdomain)
  (if (listp argtypes)
      ;; Need to pass argument to judgement-list-arguments-match?
      (judgement-list-arguments-match? argument argtypes rdomain jdomain)
      (if (recordtype? (find-supertype rdomain))
	  (judgement-record-arguments-match?
	   (make!-field-applications argument)
	   (delete-duplicates (coerce argtypes 'list) :from-end t :key #'car)
	   (fields (find-supertype rdomain)) (fields (find-supertype jdomain)))
	  (judgement-vector-arguments-match?
	   (if (= (length argtypes) 1)
	       (list argument)
	       (argument-list argument))
	   argtypes (types (find-supertype rdomain))
	   (types (find-supertype jdomain)) 0))))

(defmethod types ((te dep-binding))
  (types (type te)))

(defun judgement-list-arguments-match? (argument argtypes rdomain jdomain)
  (or (judgement-list-arguments-match*? argument argtypes rdomain jdomain)
      (argtype-intersection-in-judgement-type? argtypes jdomain)))

(defun judgement-list-arguments-match*? (argument argtypes rdomain jdomain)
  (when argtypes
    (or (subtype-wrt? argument (car argtypes) jdomain rdomain)
	(judgement-list-arguments-match*? argument
					  (cdr argtypes)
					  rdomain jdomain))))

(defun judgement-record-arguments-match? (args argflds rfields jfields)
  (or (null rfields)
      (let ((atypes (cdr (assq (id (car rfields)) argflds))))
	(and atypes
	     (compatible? (type (car args)) (type (car rfields)))
	     (eq (id (car rfields)) (id (car jfields)))
	     (judgement-arguments-match?
	      (car args) atypes (type (car rfields)) (type (car jfields)))
	     (judgement-record-arguments-match?
	      (cdr args) argflds (cdr rfields) (cdr jfields))))))

(defun judgement-vector-arguments-match? (args argtypes rdomain jdomain num
					  &optional bindings)
  (or (>= num (length argtypes))
      (and (or (judgement-vector-arguments-match*?
		(aref argtypes num) (car args)
		(car rdomain) (car jdomain) bindings)
	       (argtype-intersection-in-judgement-type?
		(aref argtypes num) (car jdomain)))
	   (judgement-vector-arguments-match?
	    (cdr args) argtypes
	    (if (dep-binding? (car rdomain))
		(substit (cdr rdomain) (acons (car rdomain) (car args) nil))
		(cdr rdomain))
	    (if (dep-binding? (car jdomain))
		(substit (cdr jdomain) (acons (car jdomain) (car args) nil))
		(cdr jdomain))
	    (1+ num)
	    (if (and (typep (car rdomain) 'dep-binding)
		     (typep (car jdomain) 'dep-binding))
		(acons (car jdomain) (car rdomain) bindings)
		bindings)))))

(defmethod judgement-vector-arguments-match*? ((argtypes list) args
					       rtype jtype bindings)
  (when argtypes
    (or (subtype-wrt? args (car argtypes) jtype rtype bindings)
	(judgement-vector-arguments-match*? (cdr argtypes) args
					    rtype jtype bindings))))

(defun argtype-intersection-in-judgement-type? (argtypes jtype)
  (if (listp argtypes)
      (let* ((types (cons jtype argtypes))
	     (ctype (reduce #'compatible-type types)))
	(when ctype
	  (let* ((bid '%)
		 (bd (make!-bind-decl bid ctype))
		 (bvar (make-variable-expr bd))
		 (jpreds (collect-judgement-preds jtype ctype bvar))
		 (apreds (collect-intersection-judgement-preds
			  argtypes ctype bvar)))
	    (and jpreds
		 (subsetp jpreds apreds :test #'tc-eq)))))
      (if (recordtype? (find-supertype jtype))
	  (argtype-intersection-in-record-judgement-type?
	   (delete-duplicates (coerce argtypes 'list) :from-end t :key #'car)
	   (fields (find-supertype jtype)))
	  (argtype-intersection-in-vector-judgement-type?
	   argtypes (types (find-supertype jtype)) 0))))

(defun argtype-intersection-in-record-judgement-type? (argflds jfields)
  (or (null jfields)
      (let ((atypes (cdr (assq (id (car jfields)) argflds))))
	(and (argtype-intersection-in-judgement-type?
	      atypes (type (car jfields)))
	     (argtype-intersection-in-record-judgement-type?
	      argflds (cdr jfields))))))

(defun argtype-intersection-in-vector-judgement-type? (argtypes jdomain num)
  (or (>= num (length argtypes))
      (and (argtype-intersection-in-judgement-type?
	    (aref argtypes num) (car jdomain))
	   (argtype-intersection-in-vector-judgement-type?
	    argtypes (cdr jdomain) (1+ num)))))

(defun collect-judgement-preds (type supertype var)
  (let ((preds (nth-value 1 (subtype-preds type supertype))))
    (make-preds-with-same-binding* var preds)))

(defun collect-intersection-judgement-preds (types supertype var
						   &optional preds)
  (if (null types)
      preds
      (collect-intersection-judgement-preds
       (cdr types) supertype var
       (nunion (collect-judgement-preds (car types) supertype var) preds
	       :test #'tc-eq))))

(defmethod judgement-vector-arguments-match*? ((argtypes vector) args
					       rtype jtype bindings)
  (declare (ignore bindings))
  (let ((stype (find-supertype rtype)))
    (when (and (tupletype? stype)
	       (length= argtypes (types stype)))
      (judgement-vector-arguments-match?
       (typecase args
	 (tuple-expr (exprs args))
	 (list args)
	 (t (list args)))
       argtypes (types stype) (types (find-supertype jtype)) 0))))

(defmethod judgement-types* ((ex field-application))
  (if (record-expr? (argument ex))
      (multiple-value-bind (jtypes jdecls)
	  (judgement-types* (beta-reduce ex))
	(remove-judgement-types-of-type (type ex) jtypes jdecls))
      (multiple-value-bind (ajtypes ajdecls)
	  (judgement-types* (argument ex))
	(when ajtypes
	  (if (vectorp ajtypes)
	      (let ((pos (position-if #'(lambda (x) (eq (car x) (id ex)))
				      ajtypes)))
		(assert pos)
		(let ((entry (aref ajtypes pos)))
		  (remove-judgement-types-of-type
		   (type ex) (cdr entry) (when ajdecls
					   (if (vectorp ajdecls)
					       (aref ajdecls pos)
					       ajdecls)))))
	      (remove-judgement-types-of-type
	       (type ex)
	       (field-application-types ajtypes ex)
	       ajdecls))))))

(defun remove-judgement-types-of-type (type jtypes jdecls &optional njtypes njdecls)
  (if (vectorp jtypes)
      (values jtypes jdecls)
      (if (null jtypes)
	  (values (nreverse njtypes) (nreverse njdecls))
	  (if (subtype-of? type (car jtypes))
	      (remove-judgement-types-of-type
	       type (cdr jtypes) (cdr jdecls) njtypes njdecls)
	      (remove-judgement-types-of-type
	       type (cdr jtypes) (cdr jdecls)
	       (cons (car jtypes) njtypes) (cons (car jdecls) njdecls))))))

(defmethod judgement-types* ((ex projection-expr))
  nil)

(defmethod judgement-types* ((ex injection-expr))
  nil)

(defmethod judgement-types* ((ex injection?-expr))
  nil)

(defmethod judgement-types* ((ex extraction-expr))
  nil)

(defmethod judgement-types* ((ex projection-application))
  (if (tuple-expr? (argument ex))
      (multiple-value-bind (jtypes jdecls)
	  (judgement-types* (beta-reduce ex))
	(remove-judgement-types-of-type (type ex) jtypes jdecls))
      (multiple-value-bind (ajtypes ajdecls)
	  (judgement-types* (argument ex))
	(when ajtypes
	  (if (vectorp ajtypes)
	      (remove-judgement-types-of-type
	       (type ex)
	       (aref ajtypes (1- (index ex)))
	       (if (vectorp ajdecls)
		   (aref ajdecls (1- (index ex)))
		   (nth (1- (index ex)) ajdecls)))
	      (remove-judgement-types-of-type
	       (type ex)
	       (projection-application-types ajtypes ex)
	       ajdecls))))))

(defmethod judgement-types* ((ex injection-application))
  nil)

(defmethod judgement-types* ((ex injection?-application))
  nil)

(defmethod judgement-types* ((ex extraction-application))
  nil)

(defmethod judgement-types* ((ex tuple-expr))
  (let* ((exprs (exprs ex))
	 (jtypes&decls (mapcar #'(lambda (ex)
				   (multiple-value-list
				    (judgement-types* ex)))
			 exprs)))
    (unless (every #'(lambda (jt&d) (null (car jt&d))) jtypes&decls)
      (let* ((len (length exprs))
	     (tvec (make-array len :initial-element nil))
	     (jvec (unless (every #'(lambda (jt&d) (null (cadr jt&d)))
				  jtypes&decls)
		     (make-array len :initial-element nil))))
	(dotimes (i len)
	  (setf (aref tvec i)
		(or (car (nth i jtypes&decls))
		    (list (type (nth i exprs)))))
	  (when jvec
	    (setf (aref jvec i)
		  (cadr (nth i jtypes&decls)))))
	#+pvsdebug (assert (valid-judgement-type-value tvec))
	(values tvec jvec)))))

(defvar *max-judgement-types-size* 20)

;;; Here we create a vector of types, rather than a set of tuple types.
;;; Thus if ex`i has n_i types, we create one vector where the ith element
;;; consists of n_i types, rather than n_1 * ... * n_m tuple types.
(defmethod new-judgement-types* ((ex tuple-expr))
  (let* ((exprs (exprs ex))
	 (jtypes-list nil)
	 (jdecls-list nil))
    (dolist (expr exprs)
      (multiple-value-bind (jtypes jdecls)
	  (judgement-types* expr)
	(push jtypes jtypes-list)
	(push jdecls jdecls-list)))
    (unless (every #'null jtypes-list)
      (setq jtypes-list (nreverse jtypes-list))
      (dotimes (i (length jtypes-list))
	(when (null (nth i jtypes-list))
	  (setf (nth i jtypes-list) (list (type (nth i exprs))))))
      (setq jdecls-list (nreverse jdecls-list))
      ;; Get the cardinality of the list, before constructing it
      (let ((card (reduce #'* jtypes-list :key #'length :initial-value 1)))
	(if (> card *max-judgement-types-size*)
	    (pvs-message "tuple judgement types not generated for~%  ~a~%~
                          because it would generate ~d types"
	      ex card)
	    (let* ((jtypes (mapcar #'mk-tupletype (cartesian-product jtypes-list)))
		   (jdecls (make-list (length jtypes))
		    ;; (mapcar #'merge-jdecls-list
		    ;;   (cartesian-product jdecls-list t)))
		    )
		  )
	      #+pvsdebug
	      (assert (and (listp jtypes) (every #'type-expr? jtypes)))
	      #+pvsdebug
	      (assert (and (listp jdecls)
			   (every #'(lambda (jd)
				      (or (judgement? jd)
					  (and (listp jd)
					       (every #'judgement? jd))))
				  jdecls)))
	      #+pvsdebug
	      (assert (length= jtypes jdecls))
	      (values jtypes jdecls)))))))

(defun merge-jdecls-list (jdecls-list &optional jdecls)
  (if (null jdecls-list)
      (if (listp jdecls)
	  (nreverse jdecls)
	  jdecls)
      (merge-jdecls-list
       (cdr jdecls-list)
       (if (null jdecls)
	   (car jdecls-list)
	   (merge-jdecls (car jdecls-list) jdecls)))))

(defun merge-jdecls (jdecls1 jdecls2)
  ;; jdecls ::= jdecl | '(' jdecl* ')'
  #+pvsdebug
  (assert (or (judgement? jdecls1)
	      (and (listp jdecls1)
		   (every #'judgement? jdecls1))))
  #+pvsdebug
  (assert (or (judgement? jdecls2)
	      (and (listp jdecls2)
		   (every #'judgement? jdecls2))))
  (cond ((null jdecls1)
	 jdecls2)
	((null jdecls2)
	 jdecls1)
	((listp jdecls2)
	 (if (listp jdecls1)
	     (dolist (jdecl (reverse jdecls1))
	       (pushnew jdecl jdecls2))
	     (pushnew jdecls1 jdecls2))
	 jdecls2)
	(t ;; jdecsl2 is a judgement
	 (if (listp jdecls1)
	     (append jdecls1 (list jdecls2))
	     (if (eq jdecls1 jdecls2)
		 jdecls2
		 (list jdecls1 jdecls2))))))

;;; As with tuple-exprs, we keep the types as a vector of types, one for
;;; each field, rather than creating the full set of recordtypes.  The only
;;; difference is that the field id is included as the car of the types.
(defmethod judgement-types* ((ex record-expr))
  (let* ((exprs (mapcar #'expression (assignments ex)))
	 (args (mapcar #'arguments (assignments ex)))
	 (jtypes-list nil)
	 (jdecls-list nil))
    (dolist (expr exprs)
      (multiple-value-bind (jtypes jdecls)
	  (judgement-types* expr)
	(push jtypes jtypes-list)
	(push jdecls jdecls-list)))
    (unless (every #'(lambda (jtypes arg)
		       (every #'(lambda (jty)
				  (let ((fld (find (id (caar arg))
						   (fields (find-supertype
							    (type ex)))
						   :key #'id)))
				    (subtype-of? jty (type fld))))
			      jtypes))
		   jtypes-list args)
      (setq jtypes-list (nreverse jtypes-list))
      (dotimes (i (length jtypes-list))
	(when (null (nth i jtypes-list))
	  (setf (nth i jtypes-list) (list (type (nth i exprs))))))
      (setq jdecls-list (nreverse jdecls-list))
      ;; Get the cardinality of the list, before constructing it
      (let ((card (reduce #'* jtypes-list :key #'length :initial-value 1)))
	(if (> card *max-judgement-types-size*)
	    (pvs-message "record judgement types not generated for~%  ~a~%~
                          because it would generate ~d types"
	      ex card)
	    (let ((jtypes (mapcar #'(lambda (ftypes)
				      (mk-recordtype		
				       (mapcar #'(lambda (a fty)
						   (mk-field-decl (id (caar a))
								  fty fty))
					 args ftypes)
				       nil))
			    (cartesian-product jtypes-list)))
		  (jdecls (mapcar #'merge-jdecls-list
			    (cartesian-product jdecls-list t))))
	      #+pvsdebug
	      (assert (and (listp jtypes) (every #'type-expr? jtypes)))
	      #+pvsdebug
	      (assert (and (listp jdecls)
			   (every #'(lambda (jd)
				      (or (judgement? jd)
					  (and (listp jd)
					       (every #'judgement? jd))))
				  jdecls)))
	      #+pvsdebug
	      (assert (length= jtypes jdecls))
	      (values jtypes jdecls)))))))

(defmethod judgement-types* ((ex quant-expr))
  nil)

(defmethod judgement-types* ((ex lambda-expr))
  (multiple-value-bind (ejtypes ejdecls)
      (let ((*bound-variables* (append (bindings ex) *bound-variables*)))
	(judgement-types* (expression ex)))
    ;; Any freevars in ejtypes should refer to (bindings ex)
    (assert (length= ejtypes ejdecls))
    #+pvsdebug (assert (every #'(lambda (fv)
				  (or (memq (declaration fv) (bindings ex))
				      (memq (declaration fv) *bound-variables*)
				      (var-decl? (declaration fv))))
			      (freevars ejtypes)))
    (assert (or (listp ejtypes) (vectorp ejtypes)))
    (let* ((depbdg (cond ((dep-binding? (domain (type ex)))
			  (domain (type ex)))
			 ((some #'(lambda (fv) (memq (declaration fv) (bindings ex)))
				(freevars ejtypes))
			  (if (cdr (bindings ex))
			      (let ((tupty (make-tupletype-from-bindings (bindings ex))))
				(mk-dep-binding (make-new-variable '|d| ex) tupty))
			      (mk-dep-binding
			       (id (car (bindings ex)))
			       (type (car (bindings ex)))
			       (declared-type (car (bindings ex))))))))
	   (alist (when depbdg (mk-subst-alist (bindings ex) depbdg)))
	   (drtypes (substit (if (vectorp ejtypes)
				 (jvector-to-types ejtypes)
				 ejtypes)
		      alist))
	   (drjdecls (if (vectorp ejdecls)
			 (jvector-to-decls ejdecls)
			 ejdecls))
	   (rtypes nil)
	   (rjdecls nil))
      (mapc #'(lambda (ety ejd)
		(unless (subtype-of? (range (type ex)) ety)
		  (push ety rtypes)
		  (push ejd rjdecls)))
	   drtypes drjdecls)
      (setq rtypes (nreverse rtypes))
      (setq rjdecls (nreverse rjdecls))
      (assert (length= rtypes rjdecls))
      (let ((ftypes (mapcar #'(lambda (rty)
				(let ((dom (if (member depbdg (freevars rty)
						       :key #'declaration)
					       depbdg
					       (dep-binding-type (domain (type ex))))))
				  (mk-funtype dom rty)))
		      rtypes)))
	#+pvsdebug (assert (every #'(lambda (fv)
				      (or (memq (declaration fv) *bound-variables*)
					  (var-decl? (declaration fv))))
				  (freevars ftypes)))
	(values ftypes rjdecls)))))

;; ex is a lambda expression, type is a new range type, e.g., after
;; substit, def expansion, etc.  If any of the lambda bindings occur in
;; the type, we need to construct a dep-binding for the domain and substitute
;; accordingly.  Otherwise it is simply the type(s) associated with the lambda
;; bindings.
(defun get-lambda-expr-funtype-for-range (ex range)
  (assert (lambda-expr? ex))
  (assert (type-expr? range))
  ;; Any freevars in range type should be from the bindings of ex
  #+pvsdebug (assert (every #'(lambda (fv) (memq (declaration fv) (bindings ex)))
			    (freevars range)))
  ;; (assert (and (null (cdr (freevars range)))
  ;; 	       (dep-binding? (declaration (car (freevars range))))
  ;; 	       (dep-binding? (domain (find-supertype (type ex))))
  ;; 	       (eq (domain (find-supertype (type ex)))
  ;; 		   (declaration (car (freevars range)))))))
  (let* ((depbdg (cond ((dep-binding? (domain (type ex)))
			(domain (type ex)))
		       ((some #'(lambda (fv) (memq (declaration fv) (bindings ex)))
			      (freevars range))
			(if (cdr (bindings ex))
			    (let ((tupty (make-tupletype-from-bindings (bindings ex))))
			      (mk-dep-binding (make-new-variable '|d| ex) tupty))
			    (mk-dep-binding
			     (id (car (bindings ex)))
			     (type (car (bindings ex)))
			     (declared-type (car (bindings ex))))))))
	 (alist (when depbdg (mk-subst-alist (bindings ex) depbdg)))
	 (rtype (substit range alist))
	 (dom (or depbdg (domain (type ex)))))
    (mk-funtype dom rtype)))

(defun get-lambda-expr-funtype-for-range* (obindings sbindings range
					   &optional (idx 1) substs types)
  (if (null obindings)
      (make-lambda-expr-funtype-for-range range substs types)
      (let* ((obind (car obindings))
	     (sbind (car sbindings))
	     (nsubsts (if (member obind (freevars range) :key #'declaration)
			  (acons obind idx substs)
			  substs)))
	(if (member obind (freevars (cdr sbindings)) :key #'declaration)
	    (let ((db (mk-dep-binding (id sbind) (type sbind)
				      (declared-type sbind))))
	      (get-lambda-expr-funtype-for-range*
	       (cdr obindings)
	       (substit (cdr sbindings) (acons obind db nil))
	       range (1+ idx) nsubsts (cons db types)))
	    (get-lambda-expr-funtype-for-range*
	     (cdr obindings) (cdr sbindings) range
	     (1+ idx) nsubsts (cons (type sbind) types))))))

;; This should only be called from get-lambda-expr-funtype-for-range*
;; If the substs is null, the range type does not depend on any bindings.
;; If there is only one binding, then the substs is a singleton, and there
;; is no need to create projections.
;; The tricky one is, e.g, a lambda-expr λ (x, y: T): e(x, y)
;; with a range type R(x, y); in this case the substs would be
;; ((y . 2) (x . 1)) and the types (T T).  The resulting funtype is
;; [z: [T, T] -> R(z`1, z`2)]
(defun make-lambda-expr-funtype-for-range (range substs types)
  (let* ((dom (if (cdr types)
		  (mk-tupletype (reverse types))
		  (car types)))
	 (dep-dom (if substs
		      (if (cdr types)
			  (mk-dep-binding
			   (make-new-variable '|z| (cons range types)) dom)
			  (if (dep-binding? dom)
			      dom
			      (progn (assert (singleton? substs))
				     (mk-dep-binding (id (caar substs)) dom))))
		      dom)))
    (if substs
	(if (cdr types)
	    (let ((dvar (mk-name-expr (id dep-dom) nil nil
				      (make-resolution dep-dom
					(current-theory-name) dom))))
	      (mk-funtype dep-dom
			  (substit range
			    (mapcar #'(lambda (subst)
					(cons (car subst)
					      (make-projection-application
					       (cdr subst) dvar)))
			      substs))))
	    (mk-funtype dep-dom
			(substit range (acons (caar substs) dep-dom nil))))
	(mk-funtype dom range))))

(defmethod judgement-types* ((ex cases-expr))
  nil)

(defmethod judgement-types* ((ex update-expr))
  nil)

(defun subst-params-decls (decls theory theoryname)
  (let ((jdecls (remove-if (complement #'(lambda (jd)
					    (exportable? jd theory)))
		   decls)))
    (if (and (null (actuals theoryname))
	     (null (mappings theoryname)))
	jdecls
	(let ((njdecls (subst-params-decls* jdecls theoryname theory)))
	  (if (equal njdecls jdecls)
	      jdecls
	      njdecls)))))

(defun subst-params-decls* (jdecls theoryname theory &optional sjdecls)
  (if (null jdecls)
      (nreverse sjdecls)
      (let ((sjdecl (subst-params-decl (car jdecls) theoryname theory)))
	(subst-params-decls* (cdr jdecls) theoryname theory
			     (if sjdecl
				 (cons sjdecl sjdecls)
				 sjdecls)))))


(defmethod subst-params-decl ((j subtype-judgement) thname theory)
  (if (or (mappings thname)
	  (memq theory (free-params-theories j)))
      (let* ((nj (lcopy j
		   'declared-type (subst-mod-params (declared-type j)
						    thname theory)
		   'declared-subtype (subst-mod-params (declared-subtype j)
						       thname theory)
		   'type (subst-mod-params (type j) thname theory)
		   'name (subst-mod-params (subtype j) thname theory)))
	     (oj (car (member nj
			      (remove-if-not
				  #'(lambda (d)
				      (eq (module d) (current-theory)))
				(get-declarations (id nj)))
			      :test #'add-decl-test))))
	(cond (oj oj)
	      (t
	       (add-decl nj t t nil t)
	       (unless (eq j nj) (setf (place nj) nil))
	       (setf (generated-by nj) (or (generated-by j) j))
	       nj)))
      j))

(defmethod subst-params-decl ((j number-judgement) thname theory)
  (if (or (mappings thname)
	  (memq theory (free-params-theories j)))
      (let* ((smphash (cdr (get-subst-mod-params-caches thname)))
	     (hj (gethash j smphash)))
	(or hj
	    (let* ((nj (lcopy j
			 'declared-type (subst-mod-params (declared-type j)
							  thname theory)
			 'type (subst-mod-params (type j) thname theory)))
		   (oj (car (member nj
				    (remove-if-not
					#'(lambda (d)
					    (eq (module d) (current-theory)))
				      (get-declarations (id nj)))
				    :test #'add-decl-test))))
	      (cond (oj
		     #+pvsdebug
		     (assert (memq oj (all-decls (current-theory))))
		     oj)
		    (t (setf (gethash j smphash) nj)
		       (unless (eq j nj)
			 (add-decl nj t t nil t)
			 (setf (place nj) nil)
			 (setf (generated-by nj) (or (generated-by j) j))
			 (setf (module nj)
			       (if (fully-instantiated? thname)
				   (current-theory)
				   (module j))))
		       nj)))))
      j))

(defmethod subst-params-decl ((j name-judgement) thname theory)
  (if (or (mappings thname)
	  (memq theory (free-params-theories j)))
      (let* ((smphash (cdr (get-subst-mod-params-caches thname)))
	     (hj (gethash j smphash)))
	(if (and hj
		 (exportable? hj theory))
	    hj
	    (let* ((nj (lcopy j
			 'type (subst-mod-params (type j) thname theory)
			 'declared-type (subst-mod-params (declared-type j)
					    thname theory)
			 'name (subst-mod-params (name j) thname theory)))
		   (oj (car (member nj
				    (remove-if-not
					#'(lambda (d)
					    (eq (module d) (current-theory)))
				      (get-declarations (id nj)))
				    :test #'add-decl-test))))
	      (cond (oj
		     #+pvsdebug
		     (assert (memq oj (all-decls (current-theory))))
		     oj)
		    ((or (eq j nj)
			 (exportable? nj theory))
		     (setf (gethash j smphash) nj)
		     (cond ((judgement-eq j nj)
			    ;;(break "judgement-eq")
			    j)
			   (t (add-decl nj t t nil t)
			      (setf (place nj) nil)
			      (setf (refers-to nj) nil)
			      (setf (module nj)
				    (if (fully-instantiated? thname)
					(current-theory)
					(module j)))
			      (setf (generated-by nj) (or (generated-by j) j))
			      nj)))))))
      j))

(defmethod subst-params-decl ((j application-judgement) thname theory)
  (if (or (mappings thname)
	  (and (actuals thname)
	       (memq theory (free-params-theories j))))
      (let* ((smphash (cdr (get-subst-mod-params-caches thname)))
	     (hj (gethash j smphash)))
	(or hj
	    (let* ((*subst-params-decl* t)
		   (nj (subst-mod-params j thname theory))
		   (oj (when nj
			 (car (member nj
				      (remove-if-not
					  #'(lambda (d)
					      (eq (module d) (current-theory)))
					(get-declarations (id nj)))
				      :test #'add-decl-test)))))
	      (when nj
		(cond (oj
		       #+pvsdebug
		       (assert (memq oj (all-decls (current-theory))))
		       oj)
		      (t
		       #+pvsdebug
		       (assert (or (eq nj j)
				     (let ((ofp (free-params j)))
				       (every #'(lambda (fp)
						  (or (eq (module fp)
							  (current-theory))
						      (memq fp ofp)))
					      (free-params nj)))))
			 (setf (gethash j smphash) nj)
			 (unless (eq j nj)
			   (add-decl nj t t nil t)
			   (setf (place nj) nil)
			   (setf (refers-to nj) nil)
			   #+pvsdebug
			   (assert (or (null *insert-add-decl*)
				       (memq nj (all-decls (current-theory)))))
			   (setf (module nj)
				 (if (fully-instantiated? thname)
				     (current-theory)
				     (module j)))
			   (setf (generated-by nj) (or (generated-by j) j))
			   #+pvsdebug
			   (assert (eq (module nj) (current-theory))))
			 nj))))))
      j))

(defun free-params-theories (jdecl)
  (if (eq (free-parameter-theories jdecl) 'unbound)
      (setf (free-parameter-theories jdecl)
	    (delete-duplicates (mapcar #'module (free-params jdecl))
			       :test #'eq))
      (free-parameter-theories jdecl)))

;; Returns true when type1 is a subtype of type2, given that it must be
;; of type reltype, e.g., (subtype-wrt? rat nzrat nzreal) is true.
;; Another way to put this is that type1 intersected with reltype is a
;; subtype of type2.  The typical use is for things like (a / b), where
;; a, b: rat We would like to use the judgement rat_div_nzrat_is_rat,
;; and simply get the TCC "b /= 0", rather than disallow all judgements
;; and get the TCC "rational_pred(b) AND b /= 0".
(defun subtype-wrt? (arg type1 type2 reltype &optional bindings)
  (subtype-wrt?* type1 type2 reltype arg bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 type-expr) reltype arg bindings)
  (declare (ignore reltype arg bindings))
  (subtype-of? te1 te2))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 subtype) (reltype subtype)
			  arg bindings)
  (or (subtype-of? te1 te2)
      (and (same-predicate? te2 reltype bindings)
	   (subtype-wrt?* te1 (supertype te2) (supertype reltype)
			  arg bindings))))

(defmethod subtype-wrt?* ((te1 dep-binding) (te2 type-expr) reltype
			  arg bindings)
  (subtype-wrt?* (type te1) te2 reltype arg bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 dep-binding) reltype
			  arg bindings)
  (subtype-wrt?* te1 (type te2) reltype arg bindings))

(defmethod subtype-wrt?* ((te1 dep-binding) (te2 dep-binding) reltype
			  arg bindings)
  (subtype-wrt?* (type te1) (type te2) reltype arg bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 type-expr) (reltype dep-binding)
			  arg bindings)
  (subtype-wrt?* te1 te2 (type reltype) arg bindings))

(defmethod subtype-wrt?* ((te1 funtype) (te2 funtype) reltype arg bindings)
  (let* ((id (gentemp))
	 (bd (make-bind-decl id (dep-binding-type (domain te1))))
	 (var (make-variable-expr bd))
	 (app (make!-application arg var)))
    (and (subtype-wrt?* (domain te1) (domain te2) (domain reltype) var bindings)
	 (subtype-wrt?* (range te1)
			(if (dep-binding? (domain te2))
			    (substit (range te2)
			      (acons (domain te2) app nil))
			    (range te2))
			(if (dep-binding? (domain reltype))
			    (substit (range reltype)
			      (acons (domain reltype) app nil))
			    (range reltype))
			app
			(if (and (dep-binding? (domain te2))
				 (dep-binding? (domain reltype)))
			    (acons (domain te2) (domain reltype) bindings)
			    bindings)))))

(defmethod subtype-wrt?* ((te1 tupletype) (te2 tupletype) reltype arg bindings)
  (subtype-wrt?-list (types te1) (types te2) (types reltype)
		     (if (typep arg 'tuple-expr)
			 (exprs arg)
			 (make-projections arg))
		     bindings))

(defmethod subtype-wrt?* ((te1 cotupletype) (te2 cotupletype) reltype
			  arg bindings)
  (subtype-wrt?-list (types te1) (types te2) (types reltype)
		     (if (typep arg 'tuple-expr)
			 (exprs arg)
			 (make-projections arg))
		     bindings))

(defun subtype-wrt?-list (types1 types2 reltypes args bindings)
  (or (null types1)
      (and (subtype-wrt?* (car types1) (car types2) (car reltypes) (car args)
			  bindings)
	   (subtype-wrt?-list (cdr types1)
			      (if (dep-binding? (car types2))
				  (substit (cdr types2)
				    (acons (car types2) (car args) nil))
				  (cdr types2))
			      (if (dep-binding? (car reltypes))
				  (substit (cdr reltypes)
				    (acons (car reltypes) (car args) nil))
				  (cdr reltypes))
			      (cdr args)
			      (if (and (dep-binding? (car types2))
				       (dep-binding? (car reltypes)))
				  (acons (car types2) (car reltypes) bindings)
				  bindings)))))

(defmethod subtype-wrt?* ((te1 recordtype) (te2 recordtype) reltype
			  arg bindings)
  (subtype-wrt?-fields (fields te1) (fields te2) (fields (find-supertype reltype))
		       (make!-field-applications arg te1)
		       bindings))

(defmethod subtype-wrt?* ((te1 recordtype) (te2 recordtype)
			  (reltype dep-binding) arg bindings)
  (subtype-wrt?* te1 te2 (type reltype) arg bindings))

(defun subtype-wrt?-fields (flds1 flds2 relflds args bindings)
  (or (null flds1)
      (and (eq (id (car flds1)) (id (car flds2)))
	   (eq (id (car flds1)) (id (car relflds)))
	   (subtype-wrt?* (type (car flds1)) (type (car flds2))
			  (type (car relflds)) (car args) bindings)
	   (subtype-wrt?-fields (cdr flds1) (cdr flds2) (cdr relflds)
				(cdr args)
				(acons (car flds2) (car relflds) bindings)))))

(defmethod same-predicate? ((t1 subtype) (t2 subtype) bindings)
  (same-predicate? (predicate t1) (predicate t2) bindings))

(defmethod same-predicate? ((p1 lambda-expr) (p2 lambda-expr) bindings)
  (with-slots ((ex1 expression) (b1 bindings)) p1
    (with-slots ((ex2 expression) (b2 bindings)) p2
      (tc-eq-with-bindings ex1 ex2 (acons (car b1) (car b2) bindings)))))

(defmethod same-predicate? (p1 p2 bindings)
  (tc-eq-with-bindings p1 p2 bindings))

(defun minimal-types (types &optional mintypes)
  (cond ((null types)
	 mintypes)
	((or (some #'(lambda (ty) (subtype-of? ty (car types))) (cdr types))
	     (some #'(lambda (ty) (subtype-of? ty (car types))) mintypes))
	 (minimal-types (cdr types) mintypes))
	(t (minimal-types (cdr types) (cons (car types) mintypes)))))

(defmethod all-domain-types ((ex application) &optional domtypes)
  (all-domain-types
   (operator ex)
   (cons (domain-types (type (operator ex))) domtypes)))

(defmethod all-domain-types (ex &optional domtypes)
  (declare (ignore ex))
  (nreverse domtypes))

(defmethod argument-types ((ex application) &optional argtypes)
  (argument-types
   (operator ex)
   (cons (argument-types* (argument ex)) argtypes)))

(defmethod argument-types* ((arg tuple-expr))
  (mapcar #'(lambda (a)
	      (if (some #'(lambda (jty) (subtype-of? jty (type a)))
			(judgement-types a))
		  (judgement-types a)
		  (cons (type a) (judgement-types a))))
    (exprs arg)))

(defmethod argument-types* (arg)
  (if (some #'(lambda (jty) (subtype-of? jty (type arg)))
	    (judgement-types arg))
      (list (judgement-types arg))
      (list (cons (type arg) (judgement-types arg)))))

(defmethod argument-types (ex &optional argtypes)
  (declare (ignore ex))
  (nreverse argtypes))

(defvar *subtypes-seen* nil)

(defun type-constraints (ex &optional all?)
  (let* ((jtypes (judgement-types+ ex))
	 (*subtypes-seen* nil)
	 (preds (type-constraints* jtypes ex nil all?)))
    #+pvsdebug (assert (subsetp (freevars preds)
				(union (freevars ex) *bound-variables*)
				:key #'declaration))
    (delete-duplicates preds :test #'tc-eq)))

(defmethod type-constraints* ((list cons) ex preds all?)
  (let ((car-preds (type-constraints* (car list) ex nil all?)))
    (type-constraints* (cdr list) ex (nconc car-preds preds) all?)))

;;; all? can be nil :none, :funcs, or t, defaults to nil
;;;  nil - don't include ignored type constraints (currently nat and up)
;;;        unless there are no other preds; excludes function constraints
;;;        for backward compatibility
;;;  :none - don't include ignored in any case; excludes function
;;;  :funcs - include function constraints
;;;  t - include all type constraints
(defmethod type-constraints* ((te subtype) ex preds all?)
  (cond ((everywhere-true? (predicate te))
	 (type-constraints* (supertype te) ex preds all?))
	((or (member te *subtypes-seen* :test #'tc-eq)
	     (and (not (eq all? t))
		  (or preds (eq all? :none))
		  (ignored-type-constraint te)))
	 (nreverse preds))
	(t (push te *subtypes-seen*)
	   (let ((pred (make!-reduced-application (predicate te) ex)))
	     (type-constraints* (if (and (not all?)
					 (tc-eq te *real*))
				    (find-supertype te)
				    (supertype te))
				ex
				(nconc (and+ pred) preds) all?)))))

(defmethod type-constraints* ((te datatype-subtype) ex preds all?)
  (if all?
      (let ((acts (actuals (module-instance te)))
	  (fmls (formals-sans-usings (adt te)))
	  (ptypes (positive-types (adt te))))
	(adt-type-constraints acts fmls te ptypes ex preds all?))
      (call-next-method)))

(defun adt-type-constraints (acts fmls te postypes ex preds all?
				  &optional pospreds)
  (cond ((null acts)
	 (if (some #'cdr pospreds)
	     (cons (make-compatible-every-pred (reverse pospreds) te acts ex)
		   preds)
	     preds))
	((member (car fmls) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration (or (print-type y) y))))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (let* ((act (car acts))
		(npreds (type-predicates (type-value act) all?)))
	   (adt-type-constraints
	    (cdr acts) (cdr fmls) te postypes ex preds all?
	    (cons (cons act npreds) pospreds))))
	(t (adt-type-constraints
	    (cdr acts) (cdr fmls) te postypes ex preds all? pospreds))))

(defmethod type-constraints* ((te dep-binding) ex preds all?)
  (type-constraints* (type te) ex preds all?))

(defmethod type-constraints* ((te funtype) ex preds all?)
  (if (memq all? '(:funcs t))
      (let ((npreds (loop for tp in (type-predicates te all?)
		       collect (make!-application tp ex))))
	(nconc npreds (nreverse preds)))
      (call-next-method)))

(defmethod type-constraints* (te ex preds all?)
  (declare (ignore te ex all?))
  (nreverse preds))

(let ((ignored-type-constraints
       (when *naturalnumber*
	 (list *naturalnumber* *integer* *rational* *real* *number_field*))))
  (pushnew 'clear-ignored-type-constraints *load-prelude-hook*)
  (defun push-ignored-type-constraints (te)
    (pushnew te ignored-type-constraints))
  (defun ignored-type-constraint (type)
    (member type ignored-type-constraints :test #'tc-eq))
  (defun clear-ignored-type-constraints ()
    (setq ignored-type-constraints nil)))

(defun type-predicates (ex &optional all?)
  (let ((*subtypes-seen* nil))
    (type-predicates* ex nil all?)))

(defmethod type-predicates* ((list cons) preds all?)
  (let ((car-preds (type-predicates* (car list) nil all?)))
    (type-predicates* (cdr list) (nconc car-preds preds) all?)))

(defmethod type-predicates* ((te subtype) preds all?)
  (if (or (member te *subtypes-seen* :test #'tc-eq)
	  (and (null all?)
	       ;;preds
	       (ignored-type-constraint te)))
      preds
      (let ((pred (predicate te)))
	(push te *subtypes-seen*)
	(type-predicates* (supertype te)
			  (if (and (consp all?)
				   (member pred all? :test #'tc-eq))
			      preds
			      (cons pred preds))
			  all?))))

(defmethod type-predicates* ((te dep-binding) preds all?)
  (type-predicates* (type te) preds all?))

(defmethod type-predicates* ((te funtype) preds all?)
  (let ((rpreds (type-predicates* (range te) nil all?)))
    (if rpreds
	(let* ((fid (make-new-variable 'F te))
	       (fbd (make-bind-decl fid
		      (mk-funtype (domain te) (find-supertype* (range te) all?))))
	       (fvar (make-variable-expr fbd))
	       (opred (type-predicates-funtype* rpreds te fvar)))
	  (cons opred preds))
	preds)))

(defun find-supertype* (te all?)
  (if all?
      (find-supertype te)
      (if (or (not (subtype? te))
	      (ignored-type-constraint te))
	  te
	  (find-supertype* (supertype te) all?))))

(defun type-predicates-funtype* (rpreds te fvar)
  (let* ((vid (make-new-variable '|x| te))
	 (bd (make-bind-decl vid (domain te)))
	 (var (make-variable-expr bd))
	 (appl (make!-application fvar var)))
    (make!-lambda-expr (list (declaration fvar))
      (make!-forall-expr (list bd)
	(make!-conjunction*
	 (mapcar #'(lambda (rpred)
		     (if (dep-binding? (domain te))
			 (make!-application (substit rpred
					      (acons (domain te) var nil))
			   appl)
			 (make!-application rpred appl)))
	   rpreds))))))

(defmethod type-predicates* ((te tupletype) preds all?)
  (let ((cpreds (mapcar #'(lambda (ct) (type-predicates* ct nil all?))
		  (types te))))
    (if (every #'null cpreds)
	preds
	(type-predicates-tupletype cpreds (types te) te preds all?))))

(defun type-predicates-tupletype (cpreds types te preds all?)
  (let* ((vid (make-new-variable 'T te))
	 (bd (make-bind-decl vid (mk-tupletype
				  (mapcar #'(lambda (ty)
					      (find-supertype* ty all?))
				    (types te)))))
	 (var (make-variable-expr bd))
	 (opred (type-predicates-tupletype* cpreds types te var)))
    (cons opred preds)))

(defun type-predicates-tupletype* (cpreds types te var
					  &optional (index 1) opreds)
  (if (null cpreds)
      (make!-lambda-expr (list (declaration var))
	(make!-conjunction* (nreverse opreds)))
      (let ((projappl (make!-projection-application index var)))
	(type-predicates-tupletype*
	 (if (dep-binding? (car types))
	     (substit (cdr cpreds) (acons (car types) projappl nil))
	     (cdr cpreds))
	 (if (dep-binding? (car types))
	     (substit (cdr types)
	       (acons (car types) projappl nil))
	     (cdr types))
	 te var (1+ index)
	 (if (null (car cpreds))
	     opreds
	     (cons (make!-conjunction*
		    (mapcar #'(lambda (cpred)
				(make!-application cpred projappl))
		      (car cpreds)))
		   opreds))))))

(defmethod type-predicates* ((te recordtype) preds all?)
  (let ((cpreds (mapcar #'(lambda (ct) (type-predicates* ct nil all?))
		  (fields te))))
    (if (every #'null cpreds)
	preds
	(type-predicates-recordtype cpreds (fields te) te preds all?))))

(defun type-predicates-recordtype (cpreds fields te preds all?)
  (let* ((vid (make-new-variable 'R te))
	 (lrectype (lift-recordtype-field-types te all?))
	 (bd (make-bind-decl vid te))
	 (var (make-variable-expr bd))
	 (lbd (make-bind-decl vid lrectype))
	 (lvar (make-variable-expr lbd))
	 (opred (type-predicates-recordtype* cpreds fields te var lvar)))
    (cons opred preds)))

(defun lift-recordtype-field-types (te all?)
  (lift-recordtype-field-types* (fields te) te all?))

(defun lift-recordtype-field-types* (fields te all? &optional nfields)
  (if (null fields)
      (let ((rfields (nreverse nfields)))
	(mk-recordtype  rfields (dependent-fields? rfields)))
      (let* ((nftype (find-supertype* (type (car fields)) all?))
	     (nfield (if (eq nftype (type (car fields)))
			 (car fields)
			 (mk-field-decl (id (car fields)) nftype nftype))))
	(lift-recordtype-field-types*
	 (if (eq nftype (type (car fields)))
	     (cdr fields)
	     (let ((nvar (mk-field-name-expr
			  (id nfield)
			  (make-resolution nfield
					   (current-theory-name)
					   nftype))))
	       (substit (cdr fields) (acons (car fields) nvar nil))))
	 te
	 all?
	 (cons nfield nfields)))))

(defun type-predicates-recordtype* (cpreds fields te var lvar &optional opreds)
  (if (null cpreds)
      (make!-lambda-expr (list (declaration lvar))
	(make!-conjunction* (substit (nreverse opreds)
			      (acons (declaration var) lvar nil))))
      (let ((fldappl (make!-field-application (car fields) var)))
	(type-predicates-recordtype*
	 (if (dependent? te)
	     (substit (cdr cpreds) (acons (car fields) fldappl nil))
	     (cdr cpreds))
	 (cdr fields)
	 te var lvar
	 (if (null (car cpreds))
	     opreds
	     (cons (make!-conjunction*
		    (mapcar #'(lambda (cpred)
				(make!-application cpred fldappl))
		      (car cpreds)))
		   opreds))))))

(defmethod type-predicates* ((te struct-sub-recordtype) preds all?)
  (let ((cpreds (mapcar #'(lambda (ct) (type-predicates* ct nil all?))
		  (fields te))))
    (if (every #'null cpreds)
	preds
	(type-predicates-recordtype cpreds (fields te) te preds all?))))

(defmethod type-predicates* ((te field-decl) preds all?)
  (type-predicates* (type te) preds all?))

(defmethod type-predicates* ((te cotupletype) preds all?)
  (let ((cpreds (mapcar #'(lambda (ct) (type-predicates ct all?))
		  (types te))))
    (if (every #'null cpreds)
	preds
	(type-predicates-cotupletype cpreds (types te) te preds all?))))

;;; Given a cotuple [S1 + ... + Sn] with corresponding supertypes
;;; [T1 + ... + Tn], cpreds is of the form
;;; ((p11 ... p1m_1) ... (pn1 ... pnm_n)), and the result is
;;; lambda (x: [T1 + ... + Tn]):
;;;    CASES x OF
;;;      IN_1(y): p11(y) and ... and p1m_1(y)
;;;      ...
;;;      IN_n(y): pn1(y) and ... and pnm_n(y)
;;;    ENDCASES
(defun type-predicates-cotupletype (cpreds types te preds all?)
  (let* ((vid (make-new-variable 't te))
	 (bd (make-bind-decl vid (mk-cotupletype
				  (mapcar #'(lambda (ty)
					      (find-supertype* ty all?))
				    (types te)))))
	 (var (make-variable-expr bd))
	 (opred (type-predicates-cotupletype* cpreds types te var)))
    (cons opred preds)))

(defun type-predicates-cotupletype* (cpreds types te var
				     &optional (index 1) sels)
  (if (null cpreds)
      (make!-lambda-expr (list (declaration var))
	(make!-cases-expr var (nreverse sels)))
      (let* ((id (make-new-variable '|x| (cons (car cpreds) (car types))))
	     (bd (typecheck* (mk-bind-decl id (find-supertype (car types)))
			     nil nil nil))
	     (svar (mk-name-expr id nil nil (make-resolution bd nil (car types))))
	     (sel (make!-in-selection index (type var) (list bd)
				      (make!-conjunction*
				       (mapcar #'(lambda (cpred)
						   (make!-application cpred svar))
					 (car cpreds))))))
	(type-predicates-cotupletype*
	 (cdr cpreds)
	 (cdr types)
	 te var (1+ index)
	 (cons sel sels)))))


(defmethod type-predicates* ((te type-name) preds all?)
  (declare (ignore all?))
  (nreverse preds))

(defmethod type-predicates* (te preds all?)
  (declare (ignore te all?))
  (break "Should not get here")
  (nreverse preds))

(defmethod argument-application-number ((ex application) &optional (num 0))
  (argument-application-number (operator ex) (1+ num)))

(defmethod argument-application-number ((ex expr) &optional (num 0))
  num)


(defun set-prelude-context-judgements (judgements)
  (make-instance 'judgements
    :judgement-declarations (judgement-declarations judgements)
    :number-judgements-alist
    (set-prelude-number-judgements (number-judgements-alist judgements))
    :name-judgements-alist
    (set-prelude-name-judgements (name-judgements-alist judgements))
    :application-judgements-alist
    (set-prelude-application-judgements (application-judgements-alist
					 judgements))))

(defun set-prelude-number-judgements (alist)
  alist)

(defun set-prelude-name-judgements (alist)
  (let ((nalist (set-prelude-name-judgements* alist nil)))
    (if (every #'eq alist nalist)
	alist
	nalist)))

(defun set-prelude-name-judgements* (alist nalist)
  (if (null alist)
      (nreverse nalist)
      (set-prelude-name-judgements*
       (cdr alist)
       (cons (set-prelude-name-judgements-elt (car alist))
	     nalist))))

(defun set-prelude-name-judgements-elt (elt)
  #+pvsdebug
  (assert (every #'judgement? (minimal-judgements (cdr elt))))
  #+pvsdebug
  (assert (every #'judgement? (generic-judgements (cdr elt))))
  (if (fully-instantiated? (minimal-judgements (cdr elt)))
      elt
      (cons (car elt)
	    (make-instance 'name-judgements
	      :minimal-judgements nil
	      :generic-judgements (append (minimal-judgements (cdr elt))
					  (generic-judgements (cdr elt)))))))
    
(defun set-prelude-application-judgements (alist)
  (let ((nalist (set-prelude-application-judgements* alist nil)))
    (if (every #'eq alist nalist)
	alist
	nalist)))

(defun set-prelude-application-judgements* (alist nalist)
  (if (null alist)
      (nreverse nalist)
      (set-prelude-application-judgements*
       (cdr alist)
       (cons (set-prelude-application-judgements-elt (car alist))
	     nalist))))

;; elt consists of a (decl . vector)
(defun set-prelude-application-judgements-elt (elt)
  (let ((nvect (set-prelude-application-judgements-vector (cdr elt))))
    (if (eq nvect (cdr elt))
	elt
	(cons (car elt) nvect))))

(defun set-prelude-application-judgements-vector (vect)
  (let ((nvect (make-array (length vect) :initial-element nil)))
    (dotimes (i (length vect))
      (let ((ajs (svref vect i)))
	(when ajs
	  (setf (svref nvect i)
		(if (fully-instantiated? (judgements-graph ajs))
		    ajs
		    (let ((ngenerics (copy-judgements-graph-to-generics
				      (judgements-graph ajs)
				      (generic-judgements ajs))))
		      #+pvsdebug
		      (assert (every #'judgement? ngenerics))
		      (make-instance 'application-judgements
			:generic-judgements ngenerics
			:judgements-graph nil)))))))
    (if (every #'eq vect nvect)
	vect
	nvect)))

(defun copy-judgements-graph-to-generics (graph generics)
  (if (null graph)
      generics
      (copy-judgements-graph-to-generics
       (cdr graph)
       (append (car graph) generics))))
					 


;;; Merging judgement structures; needed when typechecking an
;;; IMPORTING The from-judgements are from the saved-context of the
;;; imported theory, the to-judgements are from the current context.

;;; Note that the merge may need to be done, even if the declaration
;;; is not in the theory, or if the theoryname has no actuals.

(defmethod merge-imported-judgements (from-judgements theory theoryname)
  (let ((to-judgements (current-judgements))
	(*judgements-added* nil))
    ;; Can't skip these, even if from- and to-judgements are eq,
    ;; because we may need to move minimal judgements to generic lists
    ;; Need to set the current-judgements to a new instance if any of
    ;; the number-, name-, or application-judgements changes
    (let* ((number-judgements
	    (merge-number-judgements
	     (number-judgements-alist from-judgements)
	     (number-judgements-alist to-judgements)
	     theory theoryname))
	   (name-judgements
	    (merge-name-judgements
	     (name-judgements-alist from-judgements)
	     (name-judgements-alist to-judgements)
	     theory theoryname))
	   (application-judgements
	    (merge-application-judgements
	     (application-judgements-alist from-judgements)
	     (application-judgements-alist to-judgements)
	     theory theoryname))
	   (new-judgements
	    (lcopy (current-judgements)
	      'number-judgements-alist number-judgements
	      'name-judgements-alist name-judgements
	      'application-judgements-alist application-judgements)))
      (unless (eq new-judgements (current-judgements))
	(setf (current-judgements) new-judgements)
	(dolist (jdecl (judgement-declarations from-judgements))
	  (pushnew jdecl (judgement-declarations to-judgements) :test #'eq))
	(clrhash (judgement-types-hash to-judgements))))))

;; Must return an eq to-alist if nothing has been changed
;; Note: numbers overloaded as names are in name-judgements.
;; Substitutions may still be needed because of types involved.
(defun merge-number-judgements (from-alist to-alist theory theoryname)
  (cond ((null from-alist)
	 to-alist)
	((and (null to-alist)
	      (null (actuals theoryname))
	      (null (mappings theoryname)))
	 from-alist)
	(t
	 ;; May need substitutions
	 (dolist (entry from-alist)
	   (let* ((num (car entry))
		  (from-jdecls (cdr entry))
		  (to-jdecls (cdr (assoc num to-alist :test #'=)))
		  (subst-from-jdecls
		   (subst-params-decls from-jdecls theory theoryname)))
	     (unless (eq from-jdecls to-jdecls)
	       (let* ((new-from-jdecls
		       (remove-if #'(lambda (fj)
				      (member fj to-jdecls
					      :test #'judgement-eq))
			 subst-from-jdecls))
		      (min-jdecls
		       (minimal-judgement-decls new-from-jdecls to-jdecls)))
		 (setq to-alist
		       (acons num min-jdecls
			      (remove num to-alist :key #'car)))))))
	 to-alist)))

(defun merge-name-judgements (from-alist to-alist theory theoryname)
  (dolist (from-entry from-alist)
    (let* ((decl (car from-entry))
	   (from-name-judgements (cdr from-entry))
	   (from-min (subst-params-decls
		      (minimal-judgements from-name-judgements)
		      theory theoryname))
	   (from-gen (subst-params-decls
		      (generic-judgements from-name-judgements)
		      theory theoryname))
	   (to-entry (assq decl to-alist))
	   (to-name-judgements (cdr to-entry))
	   (to-min (when to-name-judgements
		     (minimal-judgements to-name-judgements)))
	   (to-gen (when to-name-judgements
		     (generic-judgements to-name-judgements))))
      (unless (and to-name-judgements
		   (equal from-min to-min)
		   (fully-instantiated? to-min)
		   (equal from-gen to-gen))
	(multiple-value-bind (new-min new-gen)
	    (merge-name-judgements* (append from-min from-gen) to-min to-gen)
	  (unless (and (eq new-min to-min)
		       (eq new-gen to-gen))
	    (setq to-alist
		  (acons decl
			 (make-instance 'name-judgements
			   :minimal-judgements new-min
			   :generic-judgements new-gen)
			 (if (memq to-entry to-alist)
			     (remove* to-entry to-alist)
			     to-alist))))))))
  to-alist)

;;; to-name-judgements may be nil.  Note: in merging, some generic
;;; judgements may become instantiated and moved to the minimal
;;; judgements list, and it is also possible for a minimal judgement
;;; to become generic.  That's why we have a single list of
;;; from-name-judgements.
(defun merge-name-judgements* (from-name-judgements to-min to-gen)
  (dolist (jdecl from-name-judgements)
    (with-current-decl jdecl
      (if (fully-instantiated? (type jdecl))
	  (unless (some #'(lambda (jd) (subtype-of? (type jd) (type jdecl)))
			to-min)
	    (setq to-min
		  (cons jdecl
			(remove-if #'(lambda (jd)
				       (subtype-of? (type jdecl) (type jd)))
			  to-min))))
	  (unless (or (memq jdecl to-gen)
		      (judgement-uninstantiable? jdecl)
		      (some #'(lambda (jd) (subtype-of? (type jd) (type jdecl)))
			    to-gen))
	    (setq to-gen
		  (cons jdecl
			(remove-if #'(lambda (jd)
				       (subtype-of? (type jdecl) (type jd)))
			  to-gen)))))))
  (values to-min to-gen))

(defun minimal-judgement-decls (newdecls olddecls &optional mindecls)
  (cond (olddecls
	 (minimal-judgement-decls
	  newdecls
	  (cdr olddecls)
	  (if (some #'(lambda (nd) (judgement-lt nd (car olddecls))) newdecls)
	      mindecls
	      (cons (car olddecls) mindecls))))
	(newdecls
	 (minimal-judgement-decls
	  (cdr newdecls)
	  nil
	  (if (or (some #'(lambda (nd) (judgement-lt nd (car newdecls)))
			mindecls)
		  (some #'(lambda (nd) (judgement-lt nd (car newdecls)))
			(cdr newdecls)))
	      mindecls
	      (cons (car newdecls) mindecls))))
	(t (nreverse mindecls))))

(defun merge-application-judgements (from-alist to-alist theory theoryname)
  ;; Note: from-alist and to-alist may be eq
  (let ((prelude-alist (application-judgements-alist
			(judgements *prelude-context*))))
    (unless to-alist
      (setq to-alist prelude-alist))
    (dolist (from-entry from-alist)
      (unless (or (memq from-entry prelude-alist)
		  (not (exportable? (car from-entry) theory)))
	(let* ((decl (car from-entry))
	       (from-vector (cdr from-entry))
	       (to-entry (assq decl to-alist))
	       (to-vector (cdr to-entry)))
	  (with-current-decl decl
	    (assert (or (null to-vector) (vectorp to-vector)))
	    ;; Note: from-vector and to-vector may be eq
	    (if (null to-vector)
		(let ((exp-from-vector
		       (exportable-from-vector-judgement from-vector theory)))
		  (if exp-from-vector
		      (if (or (formals-sans-usings theory)
			      (mappings theoryname))
			  (setq to-alist
				(acons decl
				       (subst-appl-judgements-vector
					exp-from-vector theory theoryname)
				       to-alist))
			  (setq to-alist
				(acons decl exp-from-vector to-alist)))
		      to-alist))
		(let ((new-vector
		       (merge-appl-judgement-vectors from-vector to-vector
						     theory theoryname)))
		  (assert (vectorp new-vector))
		  (unless (eq new-vector to-vector)
		    (assert (not (eq new-vector from-vector)))
		    (setq to-alist
			  (acons decl new-vector
				 (remove* from-entry to-alist)))))))))))
  to-alist)

(defun exportable-from-vector-judgement (from-vector theory)
  (if (every #'(lambda (elt) (exportable-application-judgement? elt theory))
	     from-vector)
      from-vector
      (let ((exp-elts nil))
	(dotimes (i (length from-vector))
	  (let ((exp-jdgs (exportable-application-judgements
			   (aref from-vector i) theory)))
	    (when exp-jdgs
	      (push (cons i exp-jdgs) exp-elts))))
	#+pvsdebug
	(assert (not (and (= (length from-vector)
			     (length exp-elts))
			  (every #'(lambda (ee)
				     (eq (aref from-vector (car ee)) (cdr ee)))
				 exp-elts))))
	(when exp-elts
	  (let ((new-vector (make-array (length from-vector)
					:initial-element nil)))
	    (dotimes (i (length from-vector))
	      (setf (aref new-vector i)
		    (cdr (assoc i exp-elts :test #'=))))
	    new-vector)))))

(defun exportable-application-judgement? (from-entry theory)
  (or (null from-entry)
      (and (every #'(lambda (elt) (exportable? elt theory))
		  (generic-judgements from-entry))
	   (every #'(lambda (ge) (exportable? (car ge) theory))
		  (judgements-graph from-entry)))))

(defun exportable-application-judgements (from-entry theory)
  (when from-entry
    (let ((gens (remove-if (complement #'(lambda (elt)
					   (exportable? elt theory)))
		  (generic-judgements from-entry)))
	  (graph (exportable-judgement-graph
		  (judgements-graph from-entry) theory)))
      (when (or gens graph)
	(make-instance 'application-judgements
	  :generic-judgements gens
	  :judgements-graph graph)))))

(defun subst-appl-judgements-vector (vector theory theoryname)
  (let ((new-elts nil))
    (dotimes (i (length vector))
      (let ((elt (aref vector i)))
	(when elt
	  (let ((sj (subst-appl-judgements elt theory theoryname)))
	    (unless (eq sj elt)
	      (push (cons i sj) new-elts))))))
    (if new-elts
	(let ((new-vector (make-array (length vector) :initial-element nil)))
	  (dotimes (i (length vector))
	    (setf (aref new-vector i)
		  (or (cdr (assoc i new-elts :test #'=))
		      (aref vector i))))
	  new-vector)
	vector)))

(defun subst-appl-judgements (from-entry theory theoryname)
  (multiple-value-bind (gen-jdecls graph)
      (merge-appl-judgements-entries*
       (generic-judgements from-entry)
       (judgements-graph from-entry)
       nil nil theory theoryname)
    (lcopy from-entry
      'generic-judgements gen-jdecls
      'judgements-graph graph)))

(defun update-instantiated-appl-judgements (subst-jdecls graph
							 &optional gen-jdecls)
  (if (null subst-jdecls)
      (values (nreverse gen-jdecls) graph)
      (if (fully-instantiated? (type (car subst-jdecls)))
	  (update-instantiated-appl-judgements
	   (cdr subst-jdecls)
	   (if (some-judgement-subsumes (car subst-jdecls) graph t)
	       graph
	       (add-application-judgement (car subst-jdecls) graph))
	   gen-jdecls)
	  (update-instantiated-appl-judgements
	   (cdr subst-jdecls)
	   graph
	   (cons (car subst-jdecls) gen-jdecls)))))

(defun merge-appl-judgement-vectors (from-vector to-vector theory theoryname)
  ;; from-vector and to-vector may be eq
  (let ((new-elts nil))
    (dotimes (i (length from-vector))
      (when (aref from-vector i)
	(if (and (< i (length to-vector))
		 (aref to-vector i))
	    (let ((new-elt (merge-appl-judgements-entries
			    (aref from-vector i) (aref to-vector i)
			    theory theoryname)))
	      (unless (eq new-elt (aref to-vector i))
		(push (cons i new-elt) new-elts)))
	    (push (cons i (subst-appl-judgements
			   (aref from-vector i) theory theoryname))
		  new-elts))))
    (if new-elts
	(let ((new-vector (make-array (max (length from-vector)
					   (length to-vector))
				      :initial-element nil)))
	  (dotimes (i (length from-vector))
	    (setf (aref new-vector i)
		  (or (cdr (assoc i new-elts :test #'=))
		      (aref to-vector i))))
	  new-vector)
	to-vector)))

;;; Entry here is an instance of application-judgements Need to substitute
;;; generics of the from-entry, and update the generics and graph of the
;;; to-entry.  Note that the graph may need substitution as well, since it
;;; may be fully-instantiated only in the theory in which it was declared.
(defun merge-appl-judgements-entries (from-entry to-entry theory theoryname)
  ;; Note: from-entry and to-entry may be eq
  (multiple-value-bind (gen-jdecls graph)
      (merge-appl-judgements-entries*
       (generic-judgements from-entry)
       (judgements-graph from-entry)
       (generic-judgements to-entry)
       (judgements-graph to-entry)
       theory theoryname)
    (lcopy to-entry
      'generic-judgements gen-jdecls
      'judgements-graph graph)))

;;; from-gens are the generic judgements from the theory
;;; from-graph is the judgement graph from the theory - fully-instantiated wrt theory
;;; to-gens are the current generic judgements
;;; from-graph is the current fully-instantiated judgement graph
;;; Roughly speaking, we are instantiating the from- judgements with theoryname instance
;;; and merging them into the to- judgements.

;;; If the theory has no formals, merging is direct:
;;;   from-gen -> to-gen, from-graph -> to-graph wo instantiation
;;; If the theoryname has no actuals, it is imported generically:
;;;   from-gen -> to-gen directly
;;;   from-graph needs to be checked: some judgements
;;;     may be uninstantiated wrt current (they move to to-gen), while others
;;;     may be instantiated (they move to to-graph).
;;; If the theoryname has actuals, it is fully instantiated.
;;;   from-gen are instantiated, but they will not become fully instantiated,
;;;     and are merged into to-gen.
;;;   from-graph should become fully-instantiated

;;; Both of these are used in getting judgement-types
;;; Note that a judgement may appear in both the judgement-graph and generic-judgements

(defun merge-appl-judgements-entries* (from-gens from-graph to-gens
						 to-graph theory theoryname)
  ;; Note: from-graph and to-graph may be eq, from-gens and to-gens may be eq
  (multiple-value-bind (new-to-gens new-to-graph)
      (merge-appl-judgement-graphs from-graph to-graph to-gens
				   theory theoryname)
    ;;(assert (every #'(lambda (x) (every #'judgement? x)) new-to-graph))
    ;;(assert (every #'(lambda (g) (fully-instantiated? (car g))) new-to-graph))
    (merge-appl-judgement-generics
     from-gens
     (if (equal new-to-gens to-gens)
	 to-gens
	 new-to-gens)
     (if (equal new-to-graph to-graph)
	 to-graph
	 new-to-graph)
     theory theoryname)))

(defun merge-appl-judgement-graphs (from-graph to-graph to-gens
					       theory theoryname)
  ;; Note: from-graph and to-graph may be eq,
  ;; and to-gens should not be destructively modified
  (cond ((null from-graph)
	 (values to-gens to-graph))
	((and (null to-graph)
	      (null (free-params from-graph))
	      (null (mappings theoryname)))
	 #+pvsdebug (assert (every #'(lambda (g) (fully-instantiated? (car g)))
				   from-graph))
	 (values to-gens (exportable-judgement-graph from-graph theory)))
	(t (multiple-value-bind (new-to-gens new-to-graph)
	       (merge-appl-judgement-graphs* from-graph to-graph to-gens
					     theory theoryname)
;; 	     (assert (or (not (equal new-to-gens to-gens))
;; 			 (eq  new-to-gens to-gens)))
;; 	     (assert (or (not (equal new-to-graph to-graph))
;; 			 (eq  new-to-graph to-graph)))
	     #+pvsdebug (assert (every #'(lambda (g)
					   (fully-instantiated? (car g)))
				       new-to-graph))
	     (values new-to-gens new-to-graph)))))

(defun exportable-judgement-graph (from-graph theory)
  (if (every #'(lambda (entry) (exportable? (car entry) theory)) from-graph)
      from-graph
      (exportable-judgement-graph* from-graph theory)))

(defun exportable-judgement-graph* (from-graph theory &optional exp-graph)
  (if (null from-graph)
      (nreverse exp-graph)
      (exportable-judgement-graph*
       (cdr from-graph)
       theory
       (if (exportable? (caar from-graph) theory)
	   (let ((exp-entry (remove-if (complement
					#'(lambda (elt)
					    (exportable? elt theory)))
			      (cdar from-graph))))
	     (if exp-entry
		 (acons (caar from-graph) exp-entry exp-graph)
		 exp-graph))
	   exp-graph))))
      
(defun merge-appl-judgement-graphs* (from-graph to-graph to-gens
						theory theoryname)
  ;; Note: from-graph and to-graph may be eq,
  ;; and to-gens should not be destructively modified
  (if (null from-graph)
      (values to-gens to-graph)
      (let ((from-jdecl (caar from-graph)))
	(if (or (assq from-jdecl to-graph)
		(assoc from-jdecl to-graph :test #'judgement-eq)
		(not (exportable? from-jdecl theory)))
	    (merge-appl-judgement-graphs*
	     (cdr from-graph) to-graph to-gens theory theoryname)
	    (if (fully-instantiated? from-jdecl)
		(merge-appl-judgement-graphs*
		 (cdr from-graph) 
		 (add-application-judgement from-jdecl to-graph)
		 to-gens theory theoryname)
		(let ((subst-from-jdecl
		       (when (exportable? from-jdecl theory)
			 (subst-params-decl from-jdecl theoryname theory))))
		  (if (and subst-from-jdecl
			   (exportable? subst-from-jdecl theory))
		      (if (fully-instantiated? subst-from-jdecl)
			  (if (and (not (eq subst-from-jdecl from-jdecl))
				   (assoc subst-from-jdecl to-graph
					  :test #'judgement-eq))
			      (merge-appl-judgement-graphs*
			       (cdr from-graph) to-graph to-gens
			       theory theoryname)
			      (merge-appl-judgement-graphs*
			       (cdr from-graph)
			       (add-application-judgement
				subst-from-jdecl to-graph)
			       to-gens theory theoryname))
			  (merge-appl-judgement-graphs*
			   (cdr from-graph) to-graph
			   (if (member subst-from-jdecl to-gens
				       :test #'judgement-eq)
			       to-gens
			       (cons subst-from-jdecl to-gens))
			   theory theoryname))
		      (merge-appl-judgement-graphs*
		       (cdr from-graph) to-graph to-gens
		       theory theoryname))))))))

(defun merge-appl-judgement-generics (from-gens to-gens to-graph
						theory theoryname)
  (let ((subst-from-gens (subst-params-decls from-gens theory theoryname)))
    (merge-appl-judgement-generics* subst-from-gens to-gens to-graph)))

(defun merge-appl-judgement-generics* (from-jdecls to-gens to-graph)
  ;; should not destructively modify to-gens or to-graph
  (if (or (null from-jdecls)
	  (equal from-jdecls to-gens))
      (values to-gens to-graph)
      (if (fully-instantiated? (car from-jdecls))
	  (if (assoc (car from-jdecls) to-graph :test #'judgement-eq)
	      (merge-appl-judgement-generics*
	       (cdr from-jdecls) to-gens to-graph)
	      (merge-appl-judgement-generics*
	       (cdr from-jdecls) to-gens
	       (add-application-judgement (car from-jdecls) to-graph)))
	  (merge-appl-judgement-generics*
	   (cdr from-jdecls)
	   (if (member (car from-jdecls) to-gens :test #'judgement-eq)
	       to-gens
	       (cons (car from-jdecls) to-gens))
	   to-graph))))
      

;;; A judgement declaration is uninstantiable if it's name does not
;;; include all the formal parameters of the judgement declarations'
;;; theory.

(defun judgement-uninstantiable? (jdecl)
  (let ((nfrees (free-params (cons (name jdecl) (formals jdecl))))
	(tfrees (formals-sans-usings (module jdecl))))
    (some #'(lambda (tf) (not (memq tf nfrees))) tfrees)))
  

;;; Copying judgement structures

(defmethod copy-judgements ((from context))
  (copy-judgements (judgements from)))

(defmethod copy-judgements ((from judgements))
  (make-instance 'judgements
    :judgement-declarations (judgement-declarations from)
    :number-judgements-alist (number-judgements-alist from)
    :name-judgements-alist (name-judgements-alist from)
    :application-judgements-alist (application-judgements-alist from)))

(defmethod copy-number-judgements-alist (number-alist)
  number-alist)

(defmethod copy-name-judgements-alist (name-alist)
  name-alist)

(defun copy-application-judgements-alist (appl-alist)
  appl-alist)

(defun copy-appl-judgement-vectors (decl from-vector)
  (map 'simple-vector
       #'(lambda (x)
	   (when x
	     (copy-application-judgements decl x)))
       from-vector))

(defun copy-application-judgements (decl appl-judgement)
  (let ((nappl-judgement
	 (make-instance 'application-judgements
;	   :argtype-hash (copy (argtype-hash appl-judgement))
	   :generic-judgements (copy-list
				(generic-judgements appl-judgement)))))
    (if (formals-sans-usings (module decl))
	(copy-judgements-graph (reverse (judgements-graph appl-judgement))
			       nappl-judgement)
	(setf (judgements-graph nappl-judgement)
	      (copy-tree (judgements-graph appl-judgement))))
    nappl-judgement))

(defun copy-judgements-graph (graph appl-judgement)
  (when graph
    (if (fully-instantiated? (judgement-type (caar graph)))
	(push (remove-if-not #'(lambda (jd)
				 (fully-instantiated? (judgement-type jd)))
		(car graph))
	      (judgements-graph appl-judgement))
	(unless (or (memq (caar graph) (generic-judgements appl-judgement))
		    (judgement-uninstantiable? (caar graph)))
	  (push (caar graph) (generic-judgements appl-judgement))))
    (copy-judgements-graph (cdr graph) appl-judgement)))


;;; Used by add-name-step in prover/proofrules.lisp
(defun update-judgements-with-new-name (name expr context)
  (let ((judgements-copied? nil))
    (multiple-value-bind (jtypes jdecls)
	(judgement-types expr)
      (when jtypes
	(let ((jthash (make-pvs-hash-table #-cmu :weak-keys? #-cmu t)))
	  (setf (gethash name jthash)
		;; This doesn't work
		;;(remove-judgement-types-of-name-type (type name) jtypes jdecls)
		(list jtypes jdecls))
	  (setf (judgements context)
		(copy (judgements context) 'judgement-types-hash jthash))
	  (setq judgements-copied? t)))
      (when (name-expr? expr)
	(let* ((applalist (application-judgements-alist (judgements context)))
	       (appl-judgements (cdr (assq (declaration expr) applalist))))
	  (when appl-judgements
	    (let ((capplalist (acons (declaration name) appl-judgements
				     applalist)))
	      (if judgements-copied?
		  (setf (application-judgements-alist (judgements context))
			capplalist)
		  (setf (judgements context)
			(copy (judgements context)
			  'application-judgements-alist capplalist))))))))))

(defun remove-judgement-types-of-name-type (ntype jtypes jdecls
					    &optional njtypes njdecls)
  (if (null jtypes)
      (list (nreverse njtypes) (nreverse njdecls))
      ;; (if (and nil
      ;; 	       (or (subtype-of? ntype (car jtypes))
      ;; 		   (not (subtype-of? (car jtypes) ntype))))
      ;; 	  (remove-judgement-types-of-name-type
      ;; 	   ntype (cdr jtypes) (cdr jdecls) njtypes njdecls)
	  (remove-judgement-types-of-name-type
	   ntype (cdr jtypes) (cdr jdecls)
	   (cons (car jtypes) njtypes) (cons (car jdecls) njdecls))
	  ;;)
      ))
      

;;; Subtype judgement handling


;;; add-to-known-subtypes updates the known-subtypes of the current
;;; context.  It first checks to see whether the subtype relation is
;;; already known, in which case it does nothing.

(defun add-to-known-subtypes (atype etype)
  (let* ((aty (if (typep atype 'dep-binding) (type atype) atype))
	 (ety (if (typep etype 'dep-binding) (type etype) etype))
	 (entry (get-known-subtypes aty)))
    (unless (member ety (cdr entry) :test #'tc-eq)
      (remove-compatible-subtype-of-hash-entries etype)
      ;; Note we don't modify the original known-subtypes, we simply
      ;; shadow the entry.
      (setf (current-known-subtypes)
	    (cons (cons aty (cons ety (cdr entry)))
		  (remove entry (current-known-subtypes)))))))


;;; We remove the compatible *subtype-of-hash* that return nil, as the
;;; new information may yield t for these.  Note that we don't need to
;;; change the true ones, as adding more known subtypes cannot change
;;; this.  We need to use compatible, as it may be that T0 <: T1 and
;;; T2 <: T3, but NOT T1 <: T2 in the subtype hash, and this is
;;; precisely what we're adding.  Hence we need to deal with the T0,
;;; T3 entry, even though neither of these types is mentioned.
;;; However, they must be compatible with the given types.  The
;;; *strong-tc-eq* flag is needed as compatible? has a much looser
;;; definition without it, and will return t wherever a formal is
;;; matched with another type.

(defun remove-compatible-subtype-of-hash-entries (type)
  (let ((*strong-tc-eq-flag* t))
    (maphash #'(lambda (pair val)
		 (when (and (null val)
			    (compatible? (car pair) type))
		   (remhash pair *subtype-of-hash*)))
	     *subtype-of-hash*)))

(defun get-known-subtypes (aty)
  (assoc aty (known-subtypes *current-context*) :test #'tc-eq))

;;; Called from update-current-context, merges the theory instances of
;;; the known-subtypes into the *current-context*
(defun update-known-subtypes (theory theoryname)
  (assert (saved-context theory))
  (let ((theory-known-subtypes (known-subtypes (saved-context theory))))
    (dolist (elt theory-known-subtypes)
      (when t ;(safe-mappings? elt theory theoryname)
	(let ((th-subtype-elt
	       (if (or (and (memq elt (dependent-known-subtypes theory))
			    (actuals theoryname))
		       (mappings theoryname))
		   (subst-mod-params elt theoryname theory)
		   elt)))
	  (unless (member th-subtype-elt (current-known-subtypes)
			  :test #'equal)
	    (let* ((cur-elt (get-known-subtypes (car th-subtype-elt)))
		   (new-elt (if (or (null cur-elt)
				    (member cur-elt theory-known-subtypes
					    :test #'equal))
				th-subtype-elt
				(merge-known-subtypes-elts
				 cur-elt th-subtype-elt)))
		   (refs (collect-references new-elt)))
	      (when (every #'(lambda (r) (exportable? r theory)) refs)
		(remove-compatible-subtype-of-hash-entries (car new-elt))
		(setf (current-known-subtypes)
		      (cons new-elt
			    (remove cur-elt (current-known-subtypes))))))))))))

(defmethod dependent-known-subtypes ((dt recursive-type))
  nil)

(defun merge-known-subtypes-elts (cur-elt new-elt)
  (merge-known-subtypes-elts* (reverse new-elt) (cdr cur-elt)))

(defun merge-known-subtypes-elts* (new-elts elts)
  (if (null new-elts)
      elts
      (merge-known-subtypes-elts*
       (cdr new-elts)
       (pushnew (car new-elts) elts :test #'tc-eq))))

(defun known-subtype-of? (t1 t2)
  (unless (or (simple-subtype-of? t2 t1)
	      (and (null (free-params t1))
		   (null (free-params t2))
		   (not (strict-compatible? t1 t2))))
    (let ((it (cons t1 t2)))
      (unless (member it *subtypes-seen* :test #'tc-eq)
	(let ((*subtypes-seen* (cons it *subtypes-seen*)))
	  (check-known-subtypes t1 t2))))))

(defmethod simple-subtype-of? ((t1 subtype) t2)
  (or (tc-eq t1 t2)
      (simple-subtype-of? (supertype t1) t2)))

(defmethod simple-subtype-of? (t1 t2)
  (declare (ignore t1 t2))
  nil)

(defun check-known-subtypes (t1 t2)
  (check-known-subtypes* t1 t2 (known-subtypes *current-context*)))

(defun check-known-subtypes* (t1 t2 known-subtypes)
  (when known-subtypes
    (or (subtype-of-test t1 t2 (car known-subtypes))
	(check-known-subtypes* t1 t2 (cdr known-subtypes)))))

(defvar *subtype-of-tests* nil)

;;; ksubtypes is of the form (T T1 T2 T3 ...) and represents that T is a
;;; known subtype of Ti.  We check if tt1 matches T, where match means
;;; that substitutions can be found for the free variables and free
;;; parameters occuring in T s.t. tt1 is tc-eq to the substituted form of
;;; T.  If it does match, then for each Ti the same substitution is done,
;;; and it is checked if this is a subtype-of*? tt2.
(defun subtype-of-test (tt1 tt2 ksubtypes)
  (when (compatible? tt1 (car ksubtypes))
    (let* ((fparams (when (fully-instantiated? tt1)
		      (free-params (car ksubtypes))))
	   (theory (when fparams (module (car fparams))))
	   (frees (unless (or (null fparams)
			      (eq theory (current-theory)))
		    (mapcan #'(lambda (x) (list (list x)))
		      (formals-sans-usings (module (car fparams))))))
	   (bindings (when frees (tc-match tt1 (car ksubtypes) frees)))
	   (thinst (subst-theory-inst-from-free-params theory bindings))
	   (kt (if frees
		   (when thinst
		     (subst-mod-params (car ksubtypes) thinst theory))
		   (car ksubtypes))))
      (when kt
	(let ((subst (when (freevars kt) (simple-match kt tt1))))
	  (when (if subst
		    (not (eq subst 'fail))
		    (tc-eq tt1 kt))
	    (flet ((findone (ks)
		     (let* ((subst2 (when (freevars ks)
				      (simple-match ks tt2)))
			    (kss (unless (eq subst2 'fail)
				   (substit ks subst2))))
		       (when kss
			 (subtype-of*?
			  (substit (if thinst
				       (subst-mod-params ks thinst theory)
				       kss)
			    subst)
			  tt2)))))
	      (some #'findone (cdr ksubtypes)))))))))

(defun subst-theory-inst-from-free-params (th bindings)
  (when (and bindings (every #'cdr bindings))
    (let* ((libid (get-lib-id th))
	   (nbindings (mapcar #'(lambda (elt)
				  (cons (car elt)
					(if (actual? (cdr elt))
					    (cdr elt)
					    (mk-actual (cdr elt)))))
			bindings))
	   (thinst (mk-modname (id th)
		     (mapcar #'cdr nbindings)
		     libid)))
      (when (fully-instantiated? thinst)
	(values thinst th)))))

(defvar *simple-match-hash*)

(defun simple-match (ex inst)
  
  (simple-match* ex inst nil nil))

(defmethod simple-match* ((ex type-name) (inst type-name) bindings subst)
  (if (tc-eq-with-bindings ex inst bindings)
      subst
      'fail))

(defmethod simple-match* ((ex subtype) (inst subtype) bindings subst)
  (let ((nsubst (simple-match* (supertype ex) (supertype inst)
			       bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (predicate ex) (predicate inst) bindings nsubst))))

(defmethod simple-match* ((ex funtype) (inst funtype) bindings subst)
  (let ((nsubst (simple-match* (domain ex) (domain inst) bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (range ex) (range inst)
		       (if (typep (domain ex) 'dep-binding)
			   (acons (domain ex) (domain inst) bindings)
			   bindings)
		       nsubst))))

(defmethod simple-match* ((ex tupletype) (inst tupletype) bindings subst)
  (simple-match* (types ex) (types inst) bindings subst))

(defmethod simple-match* ((ex cotupletype) (inst cotupletype) bindings subst)
  (simple-match* (types ex) (types inst) bindings subst))

(defmethod simple-match* ((ex list) (inst list) bindings subst)
  (simple-match-list ex inst bindings subst))

(defun simple-match-list (list ilist bindings subst)
  (cond ((null list)
	 (if (null ilist) subst 'fail))
	((null ilist) 'fail)
	(t (let ((nsubst (simple-match* (car list) (car ilist)
					bindings subst)))
	     (if (or (eq nsubst 'fail)
		     (and (binding? (car list))
			  (not (subtype-of? (type (car ilist))
					    (type (car list))))))
		 'fail
		 (simple-match-list (cdr list) (cdr ilist)
				    (if (typep (car list) 'binding)
					(acons (car list) (car ilist) bindings)
					bindings)
				    nsubst))))))

(defmethod simple-match* ((ex recordtype) (inst recordtype) bindings subst)
  (simple-match* (fields ex) (fields inst) bindings subst))

(defmethod simple-match* ((ex binding) (inst binding) bindings subst)
  (if (eq (id ex) (id inst))
      (simple-match* (type ex) (type inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex name-expr) (inst expr) bindings subst)
  (if (variable? ex)
      (let ((binding (assq (declaration ex) bindings)))
	(if binding
	    (if (if (and (typep (cdr binding) 'binding)
			 (typep inst 'name-expr))
		    (eq (cdr binding) (declaration inst))
		    (tc-eq (cdr binding) inst))
		subst
		'fail)
	    (let ((sub (assq (declaration ex) subst)))
	      (if sub
		  (if (tc-eq (cdr sub) inst)
		      subst
		      'fail)
		  (if (assq (declaration ex) bindings)
		      subst
		      (if (some #'(lambda (jty)
				    (subtype-of? jty (type (declaration ex))))
				(judgement-types+ inst))
			  (acons (declaration ex) inst subst)
			  'fail))))))
      (if (and (typep inst 'name-expr)
	       (eq (declaration ex) (declaration inst)))
	  (simple-match* (module-instance ex) (module-instance inst)
			 bindings subst)
	  'fail)))

(defmethod simple-match* ((ex field-application) (inst field-application)
			  bindings subst)
  (if (eq (id ex) (id inst))
      (simple-match* (argument ex) (argument inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex projection-expr) (inst projection-expr)
			  bindings subst)
  (declare (ignore bindings))
  (if (= (index ex) (index inst))
      subst
      'fail))

(defmethod simple-match* ((ex injection-expr) (inst injection-expr)
			  bindings subst)
  (declare (ignore bindings))
  (if (= (index ex) (index inst))
      subst
      'fail))

(defmethod simple-match* ((ex injection?-expr) (inst injection?-expr)
			  bindings subst)
  (declare (ignore bindings))
  (if (= (index ex) (index inst))
      subst
      'fail))

(defmethod simple-match* ((ex extraction-expr) (inst extraction-expr)
			  bindings subst)
  (declare (ignore bindings))
  (if (= (index ex) (index inst))
      subst
      'fail))

(defmethod simple-match* ((ex projection-application)
			  (inst projection-application)
			  bindings subst)
  (if (= (index ex) (index inst))
      (simple-match* (argument ex) (argument inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex injection-application)
			  (inst injection-application)
			  bindings subst)
  (if (= (index ex) (index inst))
      (simple-match* (argument ex) (argument inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex injection?-application)
			  (inst injection?-application)
			  bindings subst)
  (if (= (index ex) (index inst))
      (simple-match* (argument ex) (argument inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex extraction-application)
			  (inst extraction-application)
			  bindings subst)
  (if (= (index ex) (index inst))
      (simple-match* (argument ex) (argument inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex number-expr) (inst number-expr) bindings subst)
  (declare (ignore bindings))
  (if (= (number ex) (number inst))
      subst
      'fail))

(defmethod simple-match* ((ex tuple-expr) (inst tuple-expr) bindings subst)
  (simple-match* (exprs ex) (exprs inst) bindings subst))

(defmethod simple-match* ((ex record-expr) (inst record-expr) bindings subst)
  (simple-match* (assignments ex) (assignments inst) bindings subst))

(defmethod simple-match* ((ex cases-expr) (inst cases-expr) bindings subst)
  (simple-match* (selections ex) (selections inst) bindings subst))

(defmethod simple-match* ((ex application) (inst application) bindings subst)
  (let ((nsubst (simple-match* (operator ex) (operator inst) bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (argument ex) (argument inst) bindings nsubst))))

(defmethod simple-match* ((ex forall-expr) (inst forall-expr) bindings subst)
  (let ((nsubst (simple-match* (bindings ex) (bindings inst) bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (expression ex) (expression inst)
		       (pairlis (bindings ex) (bindings inst) bindings)
		       nsubst))))

(defmethod simple-match* ((ex exists-expr) (inst exists-expr) bindings subst)
  (let ((nsubst (simple-match* (bindings ex) (bindings inst) bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (expression ex) (expression inst)
		       (pairlis (bindings ex) (bindings inst) bindings)
		       nsubst))))

(defmethod simple-match* ((ex lambda-expr) (inst lambda-expr) bindings subst)
  (let ((nsubst (simple-match* (bindings ex) (bindings inst) bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (expression ex) (expression inst)
		       (pairlis (bindings ex) (bindings inst) bindings)
		       nsubst))))

(defmethod simple-match* ((ex update-expr) (inst update-expr) bindings subst)
  (let ((nsubst (simple-match* (expression ex) (expression inst)
			       bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (assignments ex) (assignments inst) bindings nsubst))))

(defmethod simple-match* ((ex assignment) (inst assignment) bindings subst)
  (let ((nsubst (simple-match* (arguments ex) (arguments inst)
			       bindings subst)))
    (if (eq nsubst 'fail)
	'fail
	(simple-match* (expression ex) (expression inst) bindings nsubst))))

(defmethod simple-match* ((ex modname) (inst modname) bindings subst)
  (if (eq (id ex) (id inst))
      (let ((amatch (simple-match* (actuals ex) (actuals inst) bindings subst)))
	(if (eq amatch 'fail)
	    'fail
	    (simple-match* (mappings ex) (mappings inst) bindings subst)))
      'fail))

(defmethod simple-match* ((ex actual) (inst actual) bindings subst)
  (if (type-value ex)
      (if (type-value inst)
	  (simple-match* (type-value ex) (type-value inst) bindings subst)
	  'fail)
      (if (type-value inst)
	  'fail
	  (simple-match* (expr ex) (expr inst) bindings subst))))

(defmethod simple-match* ((ex mapping) (inst mapping) bindings subst)
  (let ((lmatch (simple-match* (lhs ex) (lhs inst) bindings subst)))
    (if (eq lmatch 'fail)
	'fail
	(simple-match* (rhs ex) (rhs inst) bindings subst))))

(defmethod simple-match* (ex inst bindings subst)
  (declare (ignore ex inst bindings subst))
  'fail)

(defun describe-judgements (&optional (context *current-context*))
  (let ((judgements (judgements context)))
    (format t "~%~d number-judgements"
      (length (number-judgements-alist judgements)))
    (format t "~%~d name-judgements"
      (length (name-judgements-alist judgements)))
    (format t "~%~d application-judgements"
      (length (application-judgements-alist judgements)))
    (let ((histu nil) (histg nil))
      (dolist (el (application-judgements-alist judgements))
	(dotimes (i (length (cdr el)))
	  (when (aref (cdr el) i)
	    (let* ((aj (aref (cdr el) i))
		   (ul (length (generic-judgements aj)))
		   (uent (assoc ul histu :test #'=))
		   (gl (length (judgements-graph aj)))
		   (gent (assoc gl histg :test #'=)))
	      (if uent
		  (incf (cdr uent))
		  (push (cons ul 1) histu))
	      (if gent
		  (incf (cdr gent))
		  (push (cons gl 1) histg))))))
      (format t "~%  generic hist: ~a~%  graph hist: ~a" histu histg))
    (format t "~%~d subtype-judgements"
      (count-if #'subtype-judgement? (judgement-declarations judgements)))))
