;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; judgements.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 22:40:53 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jan 28 18:16:43 1999
;; Update Count    : 10
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

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

(defvar *nodes-seen* nil)

(defvar *judgements-added* nil)

(defun add-appl-judgement-node (jdecl graph)
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
       (not (tc-eq (judgement-type d1) (judgement-type d2)))
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
	(aref (cdr (assq (declaration op)
			 (application-judgements-alist (current-judgements))))
	      (1- currynum))))))


;;; add-judgement-decl (subtype-judgement) is invoked from
;;; typecheck* (subtype-judgement)

(defmethod add-judgement-decl ((decl subtype-judgement))
  ;; Note that this can be called by prove-decl before *in-checker* is t
  (assert (not *in-checker*))
  (add-to-known-subtypes (subtype decl) (type decl)))

;;; Invoked from typecheck* (number-judgement)

(defmethod add-judgement-decl ((decl number-judgement))
  ;; Note that this can be called by prove-decl before *in-checker* is t
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

(defmethod add-judgement-decl ((jdecl name-judgement))
  ;; Note that this can be called by prove-decl before *in-checker* is t
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
				  'minimal-judgements (list jdecl)))
			    (remove* entry
				     (name-judgements-alist
				      (current-judgements))))))))))

;;; Invoked from typecheck* (application-judgement)

(defmethod add-judgement-decl ((jdecl application-judgement))
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
			   (add-judgement-decl-to-graph jdecl ventry)
			   (make-instance 'application-judgements
			     'judgements-graph (list (list jdecl))))))
      (unless (eq ventry new-ventry)
	(let* ((new-length (max (length vector) currynum))
	       (new-vector (make-array new-length)))
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


;;; Called from add-to-using -> update-current-context

(defun update-judgements-of-current-context (theory theoryname)
  (merge-imported-judgements (judgements (saved-context theory))
			     theory theoryname)
;;   (assert (judgements-all-visible? *current-context*) ()
;; 	  "After merge")
  )

;;; judgement-types

(defun judgement-types+ (expr)
  (let ((jtypes (judgement-types expr)))
    (if (consp jtypes)
	(if (some #'(lambda (jty) (subtype-of? jty (type expr)))
		  jtypes)
	    jtypes
	    (cons (type expr) jtypes))
	(list (type expr)))))

(defmethod judgement-types ((ex expr))
  (judgement-types-expr ex))

(defun judgement-types-expr (ex)
  (let ((jhash (judgement-types-hash (current-judgements))))
    (multiple-value-bind (jtypes there?)
	(gethash ex jhash)
      (if there?
	  jtypes
	  (let ((types (judgement-types* ex)))
	    (setf (gethash ex jhash) types))))))

(defmethod judgement-types ((ex tuple-expr))
  nil)

(defmethod judgement-types ((ex record-expr))
  nil)

(defmethod judgement-types* :around (ex)
  (let ((jhash (judgement-types-hash (current-judgements))))
    (multiple-value-bind (jtypes there?)
	(gethash ex jhash)
      (if there?
	  jtypes
	  (let ((njtypes (call-next-method)))
	    (setf (gethash ex jhash) njtypes))))))

(defmethod judgement-types* ((ex number-expr))
  (append (mapcar #'type
	    (cdr (assoc (number ex)
			(number-judgements-alist (current-judgements))
			:test #'=)))
	  (cons (available-numeric-type (number ex))
		(when (and *even_int* *odd_int*)
		  (list (if (evenp (number ex)) *even_int* *odd_int*))))))

(defmethod judgement-types* ((ex name-expr))
  (let ((entry (name-judgements ex)))
    (when entry
      (delete-if-not #'(lambda (ty) (compatible? ty (type ex)))
	(if (generic-judgements entry)
	    (let ((inst-types (instantiate-generic-judgement-types
			       ex (generic-judgements entry))))
	      (minimal-types
	       (nconc (delete-if (complement #'fully-instantiated?) inst-types)
		      (mapcar #'type (minimal-judgements entry)))))
	    (mapcar #'type (minimal-judgements entry)))))))

(defun instantiate-generic-judgement-types (name judgements &optional types)
  (if (null judgements)
      types
      (let ((type (instantiate-generic-judgement-type name (car judgements))))
	(instantiate-generic-judgement-types
	 name
	 (cdr judgements)
	 (if type (cons type types) types)))))

(defun instantiate-generic-judgement-type (name judgement)
  (let ((bindings (tc-match name (name judgement)
			    (mapcar #'list
			      (formals-sans-usings (module judgement))))))
    (assert (every #'cdr bindings))
    (let ((jthinst (mk-modname (id (module judgement))
		     (mapcar #'(lambda (a) (mk-actual (cdr a)))
		       bindings))))
      (subst-mod-params (type judgement) jthinst (module judgement)))))

(defmethod judgement-types* ((ex application))
  (let* ((op (operator* ex)))
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
	       (let* (;;(argtypes (judgement-types+ (argument ex)))
		      (gtypes (compute-application-judgement-types
			       ex
			       (judgements-graph entry)))
		      (jtypes (generic-application-judgement-types
			       ex (generic-judgements entry) gtypes)))
		 jtypes))))))
      (lambda-expr
       (judgement-types* (beta-reduce ex))))))

(defmethod judgement-types* ((ex branch))
  (let ((then-types (judgement-types+ (then-part ex)))
	(else-types (judgement-types+ (else-part ex))))
    (join-compatible-types then-types else-types)))

(defun join-compatible-types (types1 types2 &optional compats)
  (if (null types1)
      compats
      (join-compatible-types
       (cdr types1)
       types2
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

(defun generic-application-judgement-types (ex gen-judgements jtypes)
  (if (null gen-judgements)
      jtypes
      (let* ((jdecl (car gen-judgements))
	     (jtype (instantiate-generic-appl-judgement-type ex jdecl)))
	(if jtype
	    (let* ((arguments (argument* ex))
		   (argtypes (mapcar #'judgement-types* arguments))
		   (domains (operator-domain* jtype arguments nil))
		   (range (operator-range* jtype arguments))
		   (rdomains (operator-domain ex))
		   (jrange (when (length= argtypes (formals jdecl))
			     (compute-appl-judgement-range-type
			      arguments argtypes rdomains domains range))))
	      (generic-application-judgement-types
	       ex
	       (cdr gen-judgements)
	       (if (or (null jrange)
		       (some #'(lambda (jty) (subtype-of? jty jrange)) jtypes))
		   jtypes
		   (cons jrange
			 (delete-if #'(lambda (jty)
					(subtype-of? jrange jty)) jtypes)))))
	    (generic-application-judgement-types
	     ex
	     (cdr gen-judgements)
	     jtypes)))))

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
  (let ((args-list (argument* ex)))
    (compute-appl-judgement-types
     args-list
     (mapcar #'judgement-types* args-list) ;; Not judgement-types+
     (operator-domain ex)
     graph)))

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

(defun compute-appl-judgement-types (arguments argtypes rdomains graph
					       &optional jtypes exclude)
  (if (null graph)
      (nreverse jtypes)
      (let* ((excluded? (memq (caar graph) exclude))
	     (range (unless excluded?
		      (compute-appl-judgement-types*
		       arguments
		       argtypes
		       rdomains
		       (caar graph)))))
	(compute-appl-judgement-types
	 arguments
	 argtypes
	 rdomains
	 (cdr graph)
	 (if (or (null range)
		 (some #'(lambda (r) (subtype-of? r range)) jtypes))
	     jtypes
	     (cons range
		   (delete-if #'(lambda (r) (subtype-of? range r)) jtypes)))
	 (if (or range excluded?)
	     (append (car graph) exclude)
	     exclude)))))

(defun compute-appl-judgement-types* (arguments argtypes rdomains jdecl)
  (let* ((jtype (judgement-type jdecl))
	 (domains (operator-domain* jtype arguments nil))
	 (range (operator-range* jtype arguments)))
    (when (length= argtypes (formals jdecl))
      ;;(assert (length= argtypes domains))
      ;;(assert (length= argtypes rdomains))
      ;;(assert (length= argtypes arguments))
      (compute-appl-judgement-range-type
       arguments argtypes rdomains domains range))))


;;; argtypes here is a list of either
;;;  1. vectors of types, if the argument is a record or tuple expr.
;;;  2. lists of types, otherwise

(defun compute-appl-judgement-range-type (arguments argtypes rdomains domains
						    range)
  (if (null arguments)
      range
      (when (judgement-arguments-match?
	     (car arguments) (car argtypes) (car rdomains) (car domains))
	(compute-appl-judgement-range-type
	 (cdr arguments)
	 (cdr argtypes)
	 (if (typep (car rdomains) 'dep-binding)
	     (substit (cdr rdomains)
	       (acons (car rdomains) (car arguments) nil))
	     (cdr rdomains))
	 (if (typep (car domains) 'dep-binding)
	     (substit (cdr domains) (acons (car domains) (car arguments) nil))
	     (cdr domains))
	 (if (typep (car domains) 'dep-binding)
	     (substit range (acons (car domains) (car arguments) nil))
	     range)))))

(defun judgement-arguments-match? (argument argtypes rdomain jdomain)
  ;;(assert (fully-instantiated? rdomain))
  ;;(assert (fully-instantiated? jdomain))
  ;;(assert (every #'fully-instantiated? argtypes))
  (and (compatible? rdomain jdomain)
       (if (null argtypes)
	   (judgement-arguments-match*? (list (type argument)) rdomain jdomain)
	   (judgement-arguments-match*? argtypes rdomain jdomain))))


;;; argtypes here is either
;;;  1. a vector of types, if the argument is a record or tuple expr.
;;;  2. a list of types, otherwise

(defun judgement-arguments-match*? (argtypes rdomain jdomain)
  (if (listp argtypes)
      (judgement-list-arguments-match? argtypes rdomain jdomain)
      (if (recordtype? (find-supertype rdomain))
	  (judgement-record-arguments-match?
	   (delete-duplicates (coerce argtypes 'list) :from-end t :key #'car)
	   (fields (find-supertype rdomain)) (fields (find-supertype jdomain)))
	  (judgement-vector-arguments-match?
	   argtypes (types (find-supertype rdomain))
	   (types (find-supertype jdomain)) 0))))

(defmethod types ((te dep-binding))
  (types (type te)))

(defun judgement-list-arguments-match? (argtypes rdomain jdomain)
  (or (judgement-list-arguments-match*? argtypes rdomain jdomain)
      (argtype-intersection-in-judgement-type? argtypes jdomain)))

(defun judgement-list-arguments-match*? (argtypes rdomain jdomain)
  (when argtypes
    (or (subtype-wrt? (car argtypes) jdomain rdomain)
	(judgement-list-arguments-match*? (cdr argtypes) rdomain jdomain))))

(defun judgement-record-arguments-match? (argflds rfields jfields)
  (or (null rfields)
      (let ((atypes (cdr (assq (id (car rfields)) argflds))))
	(and atypes
	     (eq (id (car rfields)) (id (car jfields)))
	     (judgement-arguments-match?
	      nil atypes (type (car rfields)) (type (car jfields)))
	     (judgement-record-arguments-match?
	      argflds (cdr rfields) (cdr jfields))))))

(defun judgement-vector-arguments-match? (argtypes rdomain jdomain num
						   &optional bindings)
  (or (>= num (length argtypes))
      (and (or (judgement-vector-arguments-match*?
		(aref argtypes num) (car rdomain) (car jdomain) bindings)
	       (argtype-intersection-in-judgement-type?
		(aref argtypes num) (car jdomain)))
	   (judgement-vector-arguments-match?
	    argtypes (cdr rdomain) (cdr jdomain) (1+ num)
	    (if (and (typep (car rdomain) 'dep-binding)
		     (typep (car jdomain) 'dep-binding))
		(acons (car jdomain) (car rdomain) bindings)
		bindings)))))

(defmethod judgement-vector-arguments-match*? ((argtypes list)
					       rtype jtype bindings)
  (when argtypes
    (or (subtype-wrt? (car argtypes) jtype rtype bindings)
	(judgement-vector-arguments-match*? (cdr argtypes) rtype jtype
					    bindings))))

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

(defmethod judgement-vector-arguments-match*? ((argtypes vector)
					       rtype jtype bindings)
  (declare (ignore bindings))
  (let ((stype (find-supertype rtype)))
    (when (and (tupletype? stype)
	       (length= argtypes (types stype)))
      (judgement-vector-arguments-match?
       argtypes (types stype) (types (find-supertype jtype)) 0))))

(defmethod judgement-types* ((ex field-application))
  (if (record-expr? (argument ex))
      (judgement-types* (beta-reduce ex))
      (let ((atypes (judgement-types* (argument ex))))
	(when atypes
	  (if (vectorp atypes)
	      (let ((ftypes nil))
		(dotimes (i (length atypes))
		  (let ((entry (aref atypes i)))
		    (when (eq (car entry) (id ex))
		      (setq ftypes (cdr entry)))))
		ftypes)
	      (field-application-types atypes ex))))))

(defmethod judgement-types* ((ex projection-expr))
  nil)

(defmethod judgement-types* ((ex injection-expr))
  nil)

(defmethod judgement-types* ((ex injection?-expr))
  nil)

(defmethod judgement-types* ((ex extraction-expr))
  nil)

(defmethod judgement-types* ((ex projection-application))
  (let ((atypes (judgement-types* (argument ex))))
    (when atypes
      (if (vectorp atypes)
	  (aref atypes (1- (index ex)))
	  (projection-application-types atypes ex)))))

(defmethod judgement-types* ((ex injection-application))
  nil)

(defmethod judgement-types* ((ex injection?-application))
  nil)

(defmethod judgement-types* ((ex extraction-application))
  nil)

(defmethod judgement-types* ((ex tuple-expr))
  (let* ((exprs (exprs ex))
	 (jtypes (mapcar #'judgement-types* exprs)))
    (unless (every #'null jtypes)
      (let* ((len (length exprs))
	     (vec (make-array len)))
	(dotimes (i len)
	  (setf (aref vec i)
		(or (nth i jtypes)
		    (list (type (nth i exprs))))))
	vec))))

(defmethod judgement-types* ((ex record-expr))
  (let* ((exprs (mapcar #'expression (assignments ex)))
	 (args (mapcar #'arguments (assignments ex)))
	 (jtypes (mapcar #'judgement-types* exprs)))
    (unless (every #'null jtypes)
      (let* ((len (length exprs))
	     (vec (make-array len)))
	(dotimes (i len)
	  (setf (aref vec i)
		(cons (id (caar (nth i args)))
		      (or (nth i jtypes)
			  (list (type (nth i exprs)))))))
	vec))))

(defmethod judgement-types* ((ex quant-expr))
  nil)

(defmethod judgement-types* ((ex lambda-expr))
  (let ((jtypes (judgement-types* (expression ex))))
    (when (consp jtypes)
      (let ((dom (domain (type ex))))
	(mapcar #'(lambda (jty) (mk-funtype dom jty)) jtypes)))))

(defmethod judgement-types* ((ex cases-expr))
  nil)

(defmethod judgement-types* ((ex update-expr))
  nil)

(defun subst-params-decls (jdecls theory theoryname)
  (if (and (null (actuals theoryname))
	   (null (mappings theoryname)))
      jdecls
      (let ((njdecls (mapcar #'(lambda (jd)
				 (subst-params-decl jd theoryname theory))
		       jdecls)))
	(if (equal njdecls jdecls)
	    jdecls
	    njdecls))))

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
	       (add-decl nj)
	       (setf (generated-by nj) (or (generated-by j) j))
	       nj)))
      j))

(defmethod subst-params-decl ((j number-judgement) thname theory)
  (if (or (mappings thname)
	  (memq theory (free-params-theories j)))
      (let* ((smphash (cdr (get-subst-mod-params-caches thname)))
	     (hj (gethash j smphash)))
	(if hj
	    (if (and (fully-instantiated? thname)
		     (eq (module hj) (current-theory)))
		hj
		(let* ((nj (copy hj 'module (current-theory)))
		       (oj (car (member nj
					(remove-if-not
					    #'(lambda (d)
						(eq (module d)
						    (current-theory)))
					  (get-declarations (id nj)))
					:test #'add-decl-test))))
		  (cond (oj oj)
			(t (add-decl nj)
			   (assert (or (null *insert-add-decl*)
				       (memq nj (all-decls (current-theory)))))
			   nj))))
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
		     (assert (memq oj (all-decls (current-theory))))
		     oj)
		    (t (setf (gethash j smphash) nj)
		       (unless (eq j nj)
			 (add-decl nj)
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
	(if hj
	    (if (and (fully-instantiated? thname)
		     (eq (module hj) (current-theory)))
		hj
		(let* ((nj (copy hj 'module (current-theory)))
		       (oj (car (member nj
					(remove-if-not
					    #'(lambda (d)
						(eq (module d)
						    (current-theory)))
					  (get-declarations (id nj)))
					:test #'add-decl-test))))
		  (cond (oj oj)
			(t (add-decl nj)
			   (assert (or (null *insert-add-decl*)
				       (memq nj (all-decls (current-theory)))))
			   nj))))
	    (let* ((nj (lcopy j
			 'declared-type (subst-mod-params (declared-type j)
							  thname theory)
			 'type (subst-mod-params (type j) thname theory)
			 'name (subst-mod-params (name j) thname theory)))
		   (oj (car (member nj
				    (remove-if-not
					#'(lambda (d)
					    (eq (module d) (current-theory)))
				      (get-declarations (id nj)))
				    :test #'add-decl-test))))
	      (cond (oj
		     (assert (memq oj (all-decls (current-theory))))
		     oj)
		    (t (setf (gethash j smphash) nj)
		       (unless (eq j nj)
			 (add-decl nj)
			 (setf (generated-by nj) (or (generated-by j) j))
			 (setf (module nj)
			       (if (fully-instantiated? thname)
				   (current-theory)
				   (module j))))
		       nj)))))
      j))

(defmethod subst-params-decl ((j application-judgement) thname theory)
  (if (or (mappings thname)
	  (memq theory (free-params-theories j)))
      (let* ((smphash (cdr (get-subst-mod-params-caches thname)))
	     (hj (gethash j smphash)))
	(if hj
	    (if (and (fully-instantiated? thname)
		     (eq (module hj) (current-theory)))
		hj
		(let* ((nj (copy hj 'module (current-theory)))
		       (oj (car (member nj
					(remove-if-not
					    #'(lambda (d)
						(eq (module d)
						    (current-theory)))
					  (get-declarations (id nj)))
					:test #'add-decl-test))))
		  (cond (oj
			 (assert (memq oj (all-decls (current-theory))))
			 oj)
			(t (add-decl nj)
			   (assert (or (null *insert-add-decl*)
				       (memq nj (all-decls (current-theory)))))
			   nj))))
	    (let* ((nj (lcopy j
			 'declared-type (subst-mod-params (declared-type j)
							  thname theory)
			 'type (subst-mod-params (type j) thname theory)
			 'judgement-type (subst-mod-params (judgement-type j)
							   thname theory)
			 'name (subst-mod-params (name j) thname theory)
			 'formals (subst-mod-params (formals j) thname theory)))
		   (oj (car (member nj
				    (remove-if-not
					#'(lambda (d)
					    (eq (module d) (current-theory)))
				      (get-declarations (id nj)))
				    :test #'add-decl-test))))
	      (cond (oj
		     (assert (memq oj (all-decls (current-theory))))
		     oj)
		    (t (setf (gethash j smphash) nj)
		       (unless (eq j nj)
			 (add-decl nj)
			 (assert (or (null *insert-add-decl*)
				     (memq nj (all-decls (current-theory)))))
			 (setf (module nj)
			       (if (fully-instantiated? thname)
				   (current-theory)
				   (module j)))
			 (setf (generated-by nj) (or (generated-by j) j))
			 (assert (eq (module nj) (current-theory))))
		       nj)))))
      j))

(defun free-params-theories (jdecl)
  (if (eq (free-parameter-theories jdecl) 'unbound)
      (setf (free-parameter-theories jdecl)
	    (delete-duplicates (mapcar #'module (free-params jdecl))
			       :test #'eq))
      (free-parameter-theories jdecl)))

(defun compatible-application-judgement-args (judgement-argtypes argtypes
								 domain-types)
  (every #'(lambda (j-argtype argtypes domtypes)
	     (every #'(lambda (jty atypes dty)
			(some #'(lambda (aty)
				  (subtype-wrt? aty jty dty))
			      atypes))
		    j-argtype argtypes domtypes))
	 judgement-argtypes argtypes domain-types))

;; Returns true when type1 is a subtype of type2, given that it must be
;; of type reltype, e.g., (subtype-wrt? rat nzrat nzreal) is true.
;; Another way to put this is that type1 intersected with reltype is a
;; subtype of type2.  The typical use is for things like (a / b), where
;; a, b: rat We would like to use the judgement rat_div_nzrat_is_rat,
;; and simply get the TCC "b /= 0", rather than disallow all judgements
;; and get the TCC "rational_pred(b) AND b /= 0".
(defun subtype-wrt? (type1 type2 reltype &optional bindings)
  (subtype-wrt?* type1 type2 reltype bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 type-expr) reltype bindings)
  (declare (ignore reltype bindings))
  (subtype-of? te1 te2))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 subtype) (reltype subtype)
			  bindings)
  (or (subtype-of? te1 te2)
      (and (same-predicate? te2 reltype bindings)
	   (subtype-wrt?* te1 (supertype te2) (supertype reltype) bindings))))

(defmethod subtype-wrt?* ((te1 dep-binding) (te2 type-expr) reltype bindings)
  (subtype-wrt?* (type te1) te2 reltype bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 dep-binding) reltype bindings)
  (subtype-wrt?* te1 (type te2) reltype bindings))

(defmethod subtype-wrt?* ((te1 dep-binding) (te2 dep-binding) reltype bindings)
  (subtype-wrt?* (type te1) (type te2) reltype bindings))

(defmethod subtype-wrt?* ((te1 type-expr) (te2 type-expr) (reltype dep-binding)
			  bindings)
  (subtype-wrt?* te1 te2 (type reltype) bindings))

(defmethod subtype-wrt?* ((te1 funtype) (te2 funtype) reltype bindings)
  (and (subtype-wrt?* (domain te1) (domain te2) (domain reltype) bindings)
       (subtype-wrt?* (range te1) (range te2) (range reltype)
		      (if (and (dep-binding? (domain te2))
			       (dep-binding? (domain reltype)))
			  (acons (domain te2) (domain reltype) bindings)
			  bindings))))

(defmethod subtype-wrt?* ((te1 tupletype) (te2 tupletype) reltype bindings)
  (subtype-wrt?-list (types te1) (types te2) (types reltype) bindings))

(defmethod subtype-wrt?* ((te1 cotupletype) (te2 cotupletype) reltype bindings)
  (subtype-wrt?-list (types te1) (types te2) (types reltype) bindings))

(defun subtype-wrt?-list (types1 types2 reltypes bindings)
  (or (null types1)
      (and (subtype-wrt?* (car types1) (car types2) (car reltypes) bindings)
	   (subtype-wrt?-list (cdr types1) (cdr types2) (cdr reltypes)
			      (if (and (dep-binding? (car types2))
				       (dep-binding? (car reltypes)))
				  (acons (car types2) (car reltypes) bindings)
				  bindings)))))

(defmethod subtype-wrt?* ((te1 recordtype) (te2 recordtype) reltype bindings)
  (subtype-wrt?-fields (fields te1) (fields te2) (fields reltype) bindings))

(defmethod subtype-wrt?* ((te1 recordtype) (te2 recordtype)
			  (reltype dep-binding) bindings)
  (subtype-wrt?* te1 te2 (type reltype) bindings))

(defun subtype-wrt?-fields (flds1 flds2 relflds bindings)
  (or (null flds1)
      (and (eq (id (car flds1)) (id (car flds2)))
	   (eq (id (car flds1)) (id (car relflds)))
	   (subtype-wrt?* (type (car flds1)) (type (car flds2))
			  (type (car relflds)) bindings)
	   (subtype-wrt?-fields (cdr flds1) (cdr flds2) (cdr relflds)
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
    (delete-duplicates preds :test #'tc-eq)))

(defmethod type-constraints* ((list cons) ex preds all?)
  (let ((car-preds (type-constraints* (car list) ex nil all?)))
    (type-constraints* (cdr list) ex (nconc car-preds preds) all?)))

(defmethod type-constraints* ((te subtype) ex preds all?)
  (cond ((or (member te *subtypes-seen* :test #'tc-eq)
	     (and (not all?)
		  preds
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

(defmethod type-constraints* ((te dep-binding) ex preds all?)
  (type-constraints* (type te) ex preds all?))

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
  (unless (or (member te *subtypes-seen* :test #'tc-eq)
	      (and (not all?)
		   preds
		   (ignored-type-constraint te)))
    (push te *subtypes-seen*)
    (let ((pred (predicate te)))
      (type-predicates* (supertype te) (cons pred preds) all?))))

(defmethod type-predicates* ((te dep-binding) preds all?)
  (type-predicates* (type te) preds all?))

(defmethod type-predicates* (te preds all?)
  (declare (ignore te all?))
  (nreverse preds))

(defmethod argument-application-number ((ex application) &optional (num 0))
  (argument-application-number (operator ex) (1+ num)))

(defmethod argument-application-number ((ex expr) &optional (num 0))
  num)


(defun set-prelude-context-judgements (judgements)
  (make-instance 'judgements
    'number-judgements-alist
    (set-prelude-number-judgements (number-judgements-alist judgements))
    'name-judgements-alist
    (set-prelude-name-judgements (name-judgements-alist judgements))
    'application-judgements-alist
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
  (if (fully-instantiated? (minimal-judgements (cdr elt)))
      elt
      (cons (car elt)
	    (make-instance 'name-judgements
	      'minimal-judgements nil
	      'generic-judgements (append (minimal-judgements (cdr elt))
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
  (let ((nvect (make-array (length vect))))
    (dotimes (i (length vect))
      (let ((ajs (svref vect i)))
	(when ajs
	  (setf (svref nvect i)
		(if (fully-instantiated? (judgements-graph ajs))
		    ajs
		    (let ((ngenerics (copy-judgements-graph-to-generics
				      (judgements-graph ajs)
				      (generic-judgements ajs))))
		      (make-instance 'application-judgements
			'generic-judgements ngenerics
			'judgements-graph nil)))))))
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
		   (eq from-min to-min)
		   (fully-instantiated? to-min)
		   (eq from-gen to-gen))
	(multiple-value-bind (new-min new-gen)
	    (merge-name-judgements* (append from-min from-gen) to-min to-gen)
	  (unless (and (eq new-min to-min)
		       (eq new-gen to-gen))
	    (setq to-alist
		  (acons decl
			 (make-instance 'name-judgements
			   'minimal-judgements new-min
			   'generic-judgements new-gen)
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
			to-gen))))))
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
      (unless (memq from-entry prelude-alist)
	(let* ((decl (car from-entry))
	       (from-vector (cdr from-entry))
	       (to-entry (assq decl to-alist))
	       (to-vector (cdr to-entry)))
	  (assert (or (null to-vector) (vectorp to-vector)))
	  ;; Note: from-vector and to-vector may be eq
	  (if (null to-vector)
	      (if (or (formals-sans-usings theory)
		      (mappings theoryname))
		  (setq to-alist
			(acons decl
			       (subst-appl-judgements-vector
				from-vector theory theoryname)
			       to-alist))
		  (setq to-alist
			(acons decl from-vector to-alist)))
	      (let ((new-vector
		     (merge-appl-judgement-vectors from-vector to-vector
						   theory theoryname)))
		(assert (vectorp new-vector))
		(unless (eq new-vector to-vector)
		  (assert (not (eq new-vector from-vector)))
		  (setq to-alist
			(acons decl new-vector
			       (remove* from-entry to-alist))))))))))
  to-alist)

(defun subst-appl-judgements-vector (vector theory theoryname)
  (let ((new-elts nil))
    (dotimes (i (length vector))
      (let ((elt (aref vector i)))
	(when elt
	  (let ((sj (subst-appl-judgements elt theory theoryname)))
	    (unless (eq sj elt)
	      (push (cons i sj) new-elts))))))
;;     (when new-elts
;;       (break "new-elts in subst-appl-judgements"))
    (if new-elts
	(let ((new-vector (make-array (length vector))))
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
					   (length to-vector)))))
	  ;;(break "new-elts in merge-appl-judgement-vectors")
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
;;     (unless (or (null (generic-judgements to-entry))
;; 		(eq gen-jdecls (generic-judgements to-entry)))
;;       (break "Different gen-jdecls in merge-appl-judgements-entries"))
;;     (unless (eq graph (judgements-graph to-entry))
;;       (break "Different graph in merge-appl-judgements-entries"))
    (lcopy to-entry
      'generic-judgements gen-jdecls
      'judgements-graph graph)))

(defun merge-appl-judgements-entries* (from-gens from-graph to-gens
						 to-graph theory theoryname)
  ;; Note: from-graph and to-graph may be eq, from-gens and to-gens may be eq
  (multiple-value-bind (new-to-gens new-to-graph)
      (merge-appl-judgement-graphs from-graph to-graph to-gens
				   theory theoryname)
    ;;(assert (every #'(lambda (x) (every #'judgement? x)) new-to-graph))
    (assert (every #'(lambda (g) (fully-instantiated? (car g))) new-to-graph))
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
	 (values to-gens from-graph))
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
      
(defun merge-appl-judgement-graphs* (from-graph to-graph to-gens
						theory theoryname)
  ;; Note: from-graph and to-graph may be eq,
  ;; and to-gens should not be destructively modified
  (if (null from-graph)
      (values to-gens to-graph)
      (let ((from-jdecl (caar from-graph)))
	(if (or (assq from-jdecl to-graph)
		(assoc from-jdecl to-graph :test #'judgement-eq))
	    (merge-appl-judgement-graphs*
	     (cdr from-graph) to-graph to-gens theory theoryname)
	    (if (fully-instantiated? from-jdecl)
		(merge-appl-judgement-graphs*
		 (cdr from-graph) 
		 (add-application-judgement from-jdecl to-graph)
		 to-gens theory theoryname)
		(let ((subst-from-jdecl (subst-params-decl from-jdecl
							   theoryname theory)))
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
    'number-judgements-alist (number-judgements-alist from)
    'name-judgements-alist (name-judgements-alist from)
    'application-judgements-alist (application-judgements-alist from)))

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
;	   'argtype-hash (copy (argtype-hash appl-judgement))
	   'generic-judgements (copy-list
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
  (let ((judgements-copied? nil)
	(jtypes (judgement-types expr)))
    (when jtypes
      (let ((jthash (copy (judgement-types-hash (judgements context)))))
	(setf (gethash name jthash) jtypes)
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
			'application-judgements-alist capplalist)))))))))

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
				elt
				(merge-known-subtypes-elts
				 cur-elt th-subtype-elt))))
	      (remove-compatible-subtype-of-hash-entries (car new-elt))
	      (setf (current-known-subtypes)
		    (cons new-elt
			  (remove cur-elt (current-known-subtypes)))))))))))

(defun merge-known-subtypes-elts (cur-elt new-elt)
  (merge-known-subtypes-elts* (reverse new-elt) (cdr cur-elt)))

(defun merge-known-subtypes-elts* (new-elts elts)
  (if (null new-elts)
      elts
      (merge-known-subtypes-elts*
       (cdr new-elts)
       (pushnew (car new-elts) elts :test #'tc-eq))))

(defun known-subtype-of? (t1 t2)
  (let ((it (cons t1 t2)))
    (unless (member it *subtypes-seen* :test #'tc-eq)
      (let ((*subtypes-seen* (cons it *subtypes-seen*)))
	(check-known-subtypes t1 t2)))))

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
    (let* ((frees (when (fully-instantiated? tt1)
		    (mapcar #'list
		      (remove-if #'(lambda (x)
				     (eq (module x) (current-theory)))
			(free-params (car ksubtypes))))))
	   (bindings (when frees (tc-match tt1 (car ksubtypes) frees))))
      (multiple-value-bind (thinst theory)
	  (subst-theory-inst-from-free-params bindings)
	(let ((kt (if frees
		      (when thinst
			(subst-mod-params (car ksubtypes) thinst theory))
		      (car ksubtypes))))
	  (when kt
	    (let ((subst (when (freevars kt) (simple-match kt tt1))))
	      (when (if subst
			(not (eq subst 'fail))
			(tc-eq tt1 kt))
		(some #'(lambda (ks)
			  (subtype-of*?
			   (substit (if thinst
					(subst-mod-params ks thinst theory)
					ks)
			     subst)
			   tt2))
		      (cdr ksubtypes))))))))))

(defun subst-theory-inst-from-free-params (bindings)
  (when (and bindings
	     (every #'cdr bindings))
    (let* ((th (module (caar bindings)))
	   (nbindings (mapcar #'(lambda (elt)
				  (cons (car elt)
					(if (actual? (cdr elt))
					    (cdr elt)
					    (mk-actual (cdr elt)))))
			bindings))
	   (thinst (mk-modname (id th)
		     (mapcar #'(lambda (fp)
				 (or (cdr (assq fp nbindings))
				     (mk-res-actual
				      (mk-name-expr (id fp)
					nil nil
					(make-resolution fp (mk-modname (id th))))
				      th)))
		       (formals-sans-usings th)))))
      (when (fully-instantiated? thinst)
	(values thinst th)))))
    

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
      (simple-match* (actuals ex) (actuals inst) bindings subst)
      'fail))

(defmethod simple-match* ((ex actual) (inst actual) bindings subst)
  (if (type-value ex)
      (if (type-value inst)
	  (simple-match* (type-value ex) (type-value inst) bindings subst)
	  'fail)
      (if (type-value inst)
	  'fail
	  (simple-match* (expr ex) (expr inst) bindings subst))))

(defmethod simple-match* (ex inst bindings subst)
  (declare (ignore ex inst bindings subst))
  'fail)

(defmethod judgements-all-visible? ((ctx context))
  (let* ((*current-context* ctx))
    (judgements-all-visible? (judgements ctx))))

(defmethod judgements-all-visible? ((js judgements))
  (and (every #'(lambda (nj) (every #'judgement-visible? (cdr nj)))
	      (number-judgements-alist js))
       (every #'(lambda (nj)
		  (and (every #'judgement-visible?
			      (minimal-judgements (cdr nj)))
		       (every #'judgement-visible?
			      (generic-judgements (cdr nj)))))
	      (name-judgements-alist js))
       (every
	#'(lambda (ajs)
	    (every
	     #'(lambda (aj)
		 (or (null aj)
		     (and (every #'(lambda (jg)
				     (every #'judgement-visible? jg))
				 (judgements-graph aj))
			  (every #'judgement-visible?
				 (generic-judgements aj)))))
		   (cdr ajs)))
	(application-judgements-alist js))))

(defun judgement-visible? (j)
  (or *adt*
      (eq (module j) (current-theory))
      (from-prelude? (module j))
      (get-lhash (module j) (current-using-hash))
      (and (importing? (current-declaration))
	   
	   (eq (id (module j)) (id (theory-name (current-declaration)))))
      (break)))
