;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; judgements.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 22:40:53 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 17:40:56 1998
;; Update Count    : 7
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;;; The context has a judgements slot, which consists of a judgements
;;; instance, which in turn has slots for number-judgements,
;;; name-judgements and application-judgements.
;;; number-judgements:
;;;   eql hashtable on numbers; returns a list of judgements
;;; name-judgements:
;;;   eq hashtable on declarations; what it returns depends on whether the
;;;   name has theory parameters or not:
;;;   If not:
;;;     returns a list of judgements
;;;   Otherwise returns a list:
;;;     1. car is a list of generic judgements
;;;     2. cdr is an assoc list of theory instances to judgements
;;; application-judgements:
;;;   eq hashtable on declarations; returns a list with three elements:
;;;     1. a hashtable of applications to judgements
;;;     2. a hashtable of argument type lists to judgements
;;;     3. If no theory params, a list of judgements
;;;        else a list whose car is a list of generic judgements and
;;;             cdr is an assoc list of theory instances to judgements

;;; There are three places where judgement declarations are added to the
;;; context:
;;;   1. when typechecking a judgement declaration: add-judgement-decl
;;;   2. when importing a theory that has judgements: merge-judgements
;;;   3. when building up a context: merge-judgements

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

(defun add-appl-judgement-link (new-node nodes lnodes)
  (if (null nodes)
      lnodes
      (let ((lt? (and (not (memq (car nodes) lnodes))
		      (judgement-lt new-node (car nodes)))))
	(when lt?
	  (pushnew (car nodes) (parents new-node) :test #'eq))
	(add-appl-judgement-link
	 new-node (cdr nodes)
	 (if lt?
	     (collect-judgement-predecessors (car nodes) lnodes)
	     (if (memq (car nodes) lnodes)
		 lnodes
		 (add-appl-judgement-link
		  new-node
		  (parents (car nodes))
		  (cons (car nodes) lnodes))))))))

(defun collect-judgement-predecessors (node &optional preds)
  (collect-judgement-predecessors* (parents node) preds))

(defun collect-judgement-predecessors* (nodes preds)
  (if (null nodes)
      preds
      (collect-judgement-predecessors*
       (cdr nodes)
       (if (memq (car nodes) preds)
	   preds
	   (collect-judgement-predecessors*
	    (parents (car nodes))
	    (cons (car nodes) preds))))))
	   

(defun collect-immediate-children (jdecl graph &optional children ignore)
  (cond ((null graph)
	 (nreverse children))
	((or (memq (caar graph) ignore)
	     (not (judgement-lt (caar graph) jdecl))
	     (some #'(lambda (jd) (judgement-lt jd jdecl))
		   (cdar graph)))
	 (collect-immediate-children jdecl (cdr graph) children ignore))
	(t (collect-immediate-children
	    jdecl (cdr graph)
	    (cons (caar graph) children)
	    (append (cdar graph) ignore)))))

(defun collect-immediate-parents (jdecl graph &optional parents ignore)
  (cond ((null graph)
	 (nreverse parents))
	((or (memq (caar graph) ignore)
	     (not (judgement-lt jdecl (caar graph))))
	 (collect-immediate-parents jdecl (cdr graph) parents ignore))
	(t (collect-immediate-parents
	    jdecl (cdr graph)
	    (cons (caar graph) parents)
	    (append (cdar graph) ignore)))))

(defmethod judgement-eq ((d1 application-judgement) (d2 application-judgement))
  (tc-eq (judgement-type d1) (judgement-type d2)))

(defmethod judgement-lt ((d1 number-judgement) (d2 number-judgement))
  (and (not (tc-eq (type d1) (type d2)))
       (subtype-of? (type d1) (type d2))))

(defmethod judgement-lt ((d1 name-judgement) (d2 name-judgement))
  (and (not (tc-eq (type d1) (type d2)))
       (subtype-of? (type d1) (type d2))))

(defmethod judgement-lt ((d1 application-judgement) (d2 application-judgement))
  (judgement-lt (judgement-type d1) (judgement-type d2)))

(defmethod judgement-lt ((t1 funtype) (t2 funtype))
  (and (not (tc-eq t1 t2))
       (subtype-of? (domain t1) (domain t2))
       (subtype-of? (range t1) (range t2))))

(defmethod judgement-subsumes ((d1 application-judgement)
			       (d2 application-judgement))
  (judgement-subsumes (judgement-type d1) (judgement-type d2)))

(defmethod judgement-subsumes ((t1 funtype) (t2 funtype))
  (and (subtype-of? (domain t2) (domain t1))
       (subtype-of? (range t1) (range t2))))
	

;;; Accessors and update functions for the above

;(defun number-judgements (number)
;  (gethash number
;	   (number-judgements-hash (judgements *current-context*))))

;(defsetf number-judgements (number) (judgement-decl)
;  `(pushnew ,judgement-decl
;	    (gethash ,number
;		     (number-judgements-hash (judgements *current-context*)))
;	    :key #'type :test #'tc-eq))

;(defun name-judgements (name)
;  (gethash (declaration name)
;	   (name-judgements-hash (judgements *current-context*))))

;(defsetf name-judgements (name) (entry)
;  `(pushnew ,entry
;	    (gethash ,(declaration name)
;		     (name-judgements-hash (judgements *current-context*)))
;	    :key #'type :test #'same-name-judgement-entry))

(defun application-judgements (application)
  (let ((op (operator* application)))
    (when (typep op 'name-expr)
      (let ((currynum (argument-application-number application)))
	(aref (gethash (declaration op)
			(application-judgements-hash
			 (judgements *current-context*)))
	       (1- currynum))))))

;(defsetf application-judgements (application) (entry)
;  `(let ((op (operator* application)))
;     (when (typep op 'name-expr)
;       (pushnew ,entry
;		(gethash ,(declaration op)
;			 (application-judgements-hash
;			  (judgements *current-context*)))
;		:key #'type :test #'same-application-judgement-entry))))


;;; add-judgement-decl (subtype-judgement) is invoked from
;;; typecheck* (subtype-judgement)

(defmethod add-judgement-decl ((decl subtype-judgement))
  (add-to-known-subtypes (subtype decl) (type decl)))

;;; Invoked from typecheck* (number-judgement)

(defmethod add-judgement-decl ((decl number-judgement))
  (setf (number-judgements (number (number decl))) decl))

;;; Invoked from typecheck* (name-judgement)

(defmethod add-judgement-decl ((jdecl name-judgement))
  (let* ((decl (declaration (name jdecl)))
	 (simple? (or (eq (module decl) *current-theory*)
		      (null (formals-sans-usings (module decl))))))
    (if simple?
	;; just add the declaration in this case
	(pushnew decl (name-judgements decl) :key #'type :test #'tc-eq)
	;; entry is a list whose car is a list of generic judgement decls,
	;; cdr is an assoc-list that maps theory instances to judgements.
	(let ((entry (name-judgements decl)))
	  (if entry
	      (let* ((thinst (module-instance (name jdecl)))
		     (thentry (assoc thinst (cdr entry) :teset #'tc-eq)))
		(if thentry
		    (pushnew jdecl (cdr thentry) :key #'type :test #'tc-eq)
		    (push (list thinst jdecl) (cdr entry))))
	      (push (list nil (list thinst jdecl)) (name-judgements decl)))))))

(defmethod add-judgement-decl (decl)
  nil)

;;; Invoked from typecheck* (application-judgement)

(defmethod add-judgement-decl ((jdecl application-judgement))
  (let* ((decl (declaration (name jdecl)))
	 (simple? (or (eq (module decl) *current-theory*)
		      (null (formals-sans-usings (module decl)))
		      (fully-instantiated? jdecl)))
	 (currynum (length (formals jdecl)))
	 (applhash (application-judgements-hash
		    (judgements *current-context*))))
    (if simple?
	;; Add to the tree pointed to by the vector
	(let ((vector (gethash decl applhash)))
	  (cond ((null vector)
		 (setq vector (make-array currynum :adjustable t
					  :initial-element nil))
		 (setf (gethash decl applhash) vector))
		((> currynum (length vector))
		 (adjust-array vector currynum :initial-element nil)))
	  (let ((entry (aref vector (1- currynum))))
	    (cond ((null entry)
		   (setf (aref vector (1- currynum))
			 (make-instance 'application-judgements
			   'judgements-graph (list (list jdecl))
			   'argtype-hash (when (and (no-dep-bindings
						     (judgement-type jdecl))
						    (no-dep-bindings
						     (type decl)))
					   (make-hash-table
					    :hash-function 'pvs-sxhash
					    :test 'tc-eq)))))
		  (t (add-judgement-decl-to-graph jdecl entry)
		     (when (and (argtype-hash entry)
				(no-dep-bindings (judgement-type jdecl)))
		       (clrhash (argtype-hash entry)))))))
	;; Not simple; the decl has theory parameters
	(break "add-application-judgement"))))

(defun add-judgement-decl-to-graph (jdecl entry)
  (unless (some-judgement-subsumes jdecl (judgements-graph entry))
    (clrhash (judgement-types-hash (judgements *current-context*)))
    (setf (judgements-graph entry)
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

(defun some-judgement-subsumes (jdecl graph)
  (let ((sjdecl (find-if #'(lambda (adjlist)
			     (judgement-subsumes (car adjlist) jdecl))
		  graph)))
    (when sjdecl
      (pvs-warning
	  "Judgement ~a is not needed; it is subsumed by ~a"
	(id jdecl) (id sjdecl))
      t)))

(defun show-judgements-graph (decl)
  (let ((appl-entry
	 (gethash decl (application-judgements-hash
			(judgements *current-context*)))))
    (if (null appl-entry)
	(format t "~%No application judgements for ~a" (id decl))
	(show-judgements-graph* (judgements-graph appl-entry)))))

(defun show-judgements-graph* (nodes)
  (format t "~{~%~a~}" nodes))


;;; Called from add-to-using -> update-current-context

(defun update-judgements-of-current-context (theory theoryname)
  (merge-imported-judgements (judgements (saved-context theory))
			     theory theoryname))

;;; judgement-types

(defun judgement-types+ (expr)
  (or (judgement-types expr)
      (list (type expr))))

(defmethod judgement-types ((ex expr))
  (let ((jhash (judgement-types-hash (judgements *current-context*))))
    (multiple-value-bind (jtypes there?)
	(gethash ex jhash)
      (if there?
	  jtypes
	  (setf (gethash ex jhash)
		(judgement-types* ex))))))

(defmethod judgement-types ((ex tuple-expr))
  nil)

(defmethod judgement-types ((ex record-expr))
  nil)

(defmethod judgement-types ((ex binding-expr))
  nil)


(defmethod judgement-types* ((ex number-expr))
  (append (gethash (number ex)
		   (number-judgements-hash (judgements *current-context*)))
	  (list (available-numeric-type (number ex)))))

(defmethod judgement-types* ((ex name-expr))
  (let* ((decl (declaration ex))
	 (entry (gethash decl
			 (name-judgements-hash
			  (judgements *current-context*)))))
    (when entry
      (if (simple-parameters? decl)
	  entry
	  (break)))))

(defmethod judgement-types* ((ex application))
  (let* ((op (operator* ex)))
    (when (typep op 'name-expr)
      (let* ((decl (declaration op))
	     (vector (gethash decl
			      (application-judgements-hash
			       (judgements *current-context*)))))
	(when vector
	  (let* ((currynum (argument-application-number ex))
		 (entry (when (<= currynum (length vector))
			  (aref vector (1- currynum)))))
	    (when entry
	      (let ((argtypes (judgement-arg-types ex)))
		(multiple-value-bind (jatypes there?)
		    (when (argtype-hash entry)
		      (gethash argtypes (argtype-hash entry)))
		  (if there?
		      jatypes
		      (let* ((gtypes (compute-application-judgement-types
				      ex
				      (judgements-graph entry)))
			     (jtypes (generic-application-judgement-types
				      ex (generic-judgements entry) gtypes)))
			(when (argtype-hash entry)
			  (setf (gethash argtypes (argtype-hash entry)) jtypes))
			jtypes)))))))))))

(defun generic-application-judgement-types (ex gen-judgements jtypes)
  (if (null gen-judgements)
      jtypes
      (break "generic-application-judgement-types")))

(defmethod no-dep-bindings ((te funtype))
  (no-dep-bindings (domain te)))

(defmethod no-dep-bindings ((te tupletype))
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
     (mapcar #'judgement-types* args-list)
     (domain* (type (operator* ex)))
     graph)))

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
	 (if range
	     (pushnew range jtypes :test #'tc-eq)
	     jtypes)
	 (if (or range excluded?)
	     (union (car graph) exclude)
	     exclude)))))

(defun compute-appl-judgement-types* (arguments argtypes rdomains jdecl)
  (let* ((jtype (judgement-type jdecl))
	 (domains (domain* jtype))
	 (range (range* jtype)))
    (when (length= argtypes domains)
      (assert (length= argtypes rdomains))
      (assert (length= argtypes arguments))
      (compute-appl-judgement-range-type
       arguments argtypes rdomains domains range))))

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
  (if (null argtypes)
      (subtype-wrt? (type argument) jdomain rdomain)
      (judgement-arguments-match*? argtypes rdomain jdomain)))

(defun judgement-arguments-match*? (argtypes rdomain jdomain)
  (if (listp argtypes)
      (judgement-list-arguments-match? argtypes rdomain jdomain)
      (judgement-vector-arguments-match?
       argtypes (types rdomain) (types jdomain) 0)))

(defmethod types ((te dep-binding))
  (types (type te)))

(defun judgement-list-arguments-match? (argtypes rdomain jdomain)
  (when argtypes
    (or (subtype-wrt? rdomain (car argtypes) jdomain)
	(judgement-list-arguments-match? (cdr argtypes) rdomain jdomain))))

(defun judgement-vector-arguments-match? (argtypes rdomain jdomain num)
  (or (>= num (length argtypes))
      (and (judgement-vector-arguments-match*?
	    (aref argtypes num) (car rdomain) (car jdomain))
	   (judgement-vector-arguments-match?
	    argtypes (cdr rdomain) (cdr jdomain) (1+ num)))))

(defun judgement-vector-arguments-match*? (argtypes rtype jtype)
  (when argtypes
    (or (subtype-wrt? (car argtypes) jtype rtype)
	(judgement-vector-arguments-match*? (cdr argtypes) rtype jtype))))

(defmethod judgement-types* ((ex field-application))
  (let ((atypes (judgement-types* (argument ex))))
    (when atypes
      (field-application-types atypes ex))))

(defmethod judgement-types* ((ex projection-application))
  (let ((atypes (judgement-types* (argument ex))))
    (when atypes
      (if (vectorp atypes)
	  (aref atypes (1- (index ex)))
	  (projection-application-types atypes ex)))))

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
	 (jtypes (mapcar #'judgement-types* exprs)))
    (unless (every #'null jtypes)
      (let* ((len (length exprs))
	     (vec (make-array len)))
	(dotimes (i len)
	  (setf (aref vec i)
		(or (nth i jtypes)
		    (list (type (nth i exprs))))))
	vec))))

(defmethod judgement-types* ((ex quant-expr))
  nil)

(defmethod judgement-types* ((ex lambda-expr))
  (let ((jtypes (judgement-types* (expression ex))))
    (when jtypes
      (let ((dom (domain (type ex))))
	(mapcar #'(lambda (jty) (mk-funtype dom jty)) jtypes)))))

(defmethod judgement-types* ((ex cases-expr))
  nil)

(defmethod subst-params-decl ((j subtype-judgement) modinst)
  (let* ((gj (or (generated-by j) j))
	 (nj (lcopy gj
	       'declared-type (subst-mod-params (declared-type gj) modinst)
	       'type (subst-mod-params (type gj) modinst)
	       'name (subst-mod-params (subtype gj) modinst)
	       'declared-subtype (subst-mod-params (declared-subtype gj)
						     modinst))))
    (unless (eq gj nj)
      (setf (generated-by nj) gj))
    nj))

(defmethod subst-params-decl ((j number-judgement) modinst)
  (let* ((gj (or (generated-by j) j))
	 (nj (lcopy gj
	       'declared-type (subst-mod-params (declared-type gj) modinst)
	       'type (subst-mod-params (type gj) modinst))))
    (unless (eq gj nj)
      (setf (generated-by nj) gj))
    nj))

(defmethod subst-params-decl ((j name-judgement) modinst)
  (let* ((gj (or (generated-by j) j))
	 (nj (lcopy gj
	       'declared-type (subst-mod-params (declared-type gj) modinst)
	       'type (subst-mod-params (type gj) modinst)
	       'name (subst-mod-params (name gj) modinst))))
    (unless (eq gj nj)
      (setf (generated-by nj) gj))
    nj))

(defmethod subst-params-decl ((j application-judgement) modinst)
  (let* ((gj (or (generated-by j) j))
	 (nj (lcopy gj
	       'declared-type (subst-mod-params (declared-type gj) modinst)
	       'type (subst-mod-params (type gj) modinst)
	       'name (subst-mod-params (name gj) modinst)
	       'formals (subst-mod-params (formals gj) modinst)
	       'formal-types (subst-mod-params (formal-types gj) modinst))))
    (unless (eq gj nj)
      (setf (generated-by nj) gj))
    nj))



(defun compatible-application-judgement-args (judgement-argtypes argtypes
								 domain-types)
  (every #'(lambda (j-argtype argtypes domtypes)
	     (every #'(lambda (jty atypes dty)
			(some #'(lambda (aty)
				  (subtype-wrt? aty jty dty))
			      atypes))
		    j-argtype argtypes domtypes))
	 judgement-argtypes argtypes domain-types))

(defun subtype-wrt? (type1 type2 reltype)
  ;; returns true when type1 is a subtype of type2, given that it must be
  ;; of type reltype, e.g., (subtype-wrt? rat nzrat nzreal) is true.
  (or (subtype-of? type1 type2)
      (and (same-predicate? type2 reltype)
	   (subtype-of? type1 (supertype type2)))))

(defmethod same-predicate? ((t1 subtype) (t2 subtype))
  (same-predicate? (predicate t1) (predicate t2)))

(defmethod same-predicate? ((p1 lambda-expr) (p2 lambda-expr))
  (with-slots ((ex1 expression) (b1 bindings)) p1
    (with-slots ((ex2 expression) (b2 bindings)) p2
      (tc-eq-with-bindings ex1 ex2 (acons (car b1) (car b2) nil)))))

(defmethod same-predicate? (p1 p2)
  (tc-eq p1 p2))

(defun minimal-types (types &optional mintypes)
  (cond ((null types)
	 mintypes)
	((or (some #'(lambda (ty) (subtype-of? ty (car types))) (cdr types))
	     (some #'(lambda (ty) (subtype-of? ty (car types))) mintypes))
	 (minimal-types (cdr types) mintypes))
	(t (minimal-types (cdr types) (cons (car types) mintypes)))))

(defun judgement-argtypes (judgement)
  (or (formal-types judgement)
      (setf (formal-types judgement)
	    (judgement-argtypes* (formals judgement)))))

(defun judgement-argtypes* (formals-list &optional argtypes bindings)
  (if (null formals-list)
      (nreverse argtypes)
      (multiple-value-bind (nargs nbindings)
	  (judgement-argtypes** (car formals-list) (cdr formals-list) bindings)
	(judgement-argtypes*
	 (cdr formals-list)
	 (cons nargs argtypes)
	 nbindings))))

(defun judgement-argtypes** (formals formals-list bindings &optional argtypes)
  (if (null formals)
      (values (nreverse argtypes) bindings)
      (let ((argtype (judgement-argtype formals formals-list bindings)))
	(judgement-argtypes**
	 (cdr formals)
	 formals-list
	 (if (typep argtype 'dep-binding)
	     (acons (car formals) argtype bindings)
	     bindings)
	 (cons argtype argtypes)))))

(defun judgement-argtype (formals formals-list bindings)
  (if (or (some #'(lambda (fm)
		    (member (car formals) (freevars fm)
			    :test #'same-declaration))
		(cdr formals))
	  (some #'(lambda (fmlist)
		    (some #'(lambda (fm)
			      (member (car formals) (freevars fm)
				      :test #'same-declaration))
			  fmlist))
		formals-list))
      (mk-dep-binding (id (car formals))
		      (substit (type (car formals)) bindings)
		      (substit (declared-type (car formals)) bindings))
      (substit (type (car formals)) bindings)))

(defmethod all-domain-types ((ex application) &optional domtypes)
  (all-domain-types
   (operator ex)
   (cons (domain-types (type (operator ex))) domtypes)))

(defmethod all-domain-types (ex &optional domtypes)
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
  (nreverse argtypes))

(defun op-judgement-types (res)
  (get-judgements res))

(defvar *subtypes-seen* nil)

(defun type-predicates (ex)
  (let* ((jtypes (judgement-types ex))
	 (*subtypes-seen* nil)
	 (preds (collect-type-predicates (or jtypes (type ex)) ex nil)))
    preds))

(defmethod collect-type-predicates ((list cons) ex preds)
  (let ((car-preds (collect-type-predicates (car list) ex nil)))
    (collect-type-predicates (cdr list) ex (nconc car-preds preds))))

(defmethod collect-type-predicates ((te subtype) ex preds)
  (unless (member te *subtypes-seen* :test #'tc-eq)
    (push te *subtypes-seen*)
    (let ((pred (make!-reduced-application (predicate te) ex)))
      (collect-type-predicates (supertype te)
			       ex
			       (cons pred preds)))))

(defmethod make!-reduced-application ((op lambda-expr) (arg tuple-expr))
  (if (singleton? (bindings op))
      (call-next-method)
      (substit (expression op)
	(pairlis (bindings op) (exprs arg)))))

(defmethod make!-reduced-application ((op lambda-expr) arg)
  (if (singleton? (bindings op))
      (substit (expression op)
	(acons (car (bindings op)) arg nil))
      (call-next-method)))

(defmethod make!-reduced-application (op arg)
  (make!-application op arg))

(defmethod collect-type-predicates (te ex preds)
  preds)


(defmethod argument-application-number ((ex application) &optional (num 0))
  (argument-application-number (operator ex) (1+ num)))

(defmethod argument-application-number ((ex expr) &optional (num 0))
  num)


;;; Merging judgement structures; needed when typechecking an IMPORTING

(defmethod merge-imported-judgements (from-judgements theory theoryname)
  (let ((to-judgements (judgements *current-context*))
	(*judgements-added* nil))
    (merge-number-judgements (number-judgements-hash from-judgements)
			     (number-judgements-hash to-judgements)
			     theory theoryname)
    (merge-name-judgements (name-judgements-hash from-judgements)
			   (name-judgements-hash to-judgements)
			   theory theoryname)
    (merge-application-judgements (application-judgements-hash from-judgements)
				  (application-judgements-hash to-judgements)
				  theory theoryname)
    (when *judgements-added*
      (clrhash (judgement-types-hash to-judgements)))))

(defun merge-number-judgements (from-hash to-hash theory theoryname)
  (unless (zerop (hash-table-count from-hash))
    (maphash #'(lambda (num jdecls)
		 (let* ((to-judgements (gethash num to-hash))
			(sjdecls (minimal-judgement-decls
				  (subst-params-decls jdecls theory theoryname)
				  to-judgements)))
		   (unless (equal sjdecls to-judgements)
		     (setf (gethash num to-hash) sjdecls)
		     (setf *judgements-added* t))))
	     from-hash)))

(defun merge-name-judgements (from-hash to-hash theory theoryname)
  (unless (zerop (hash-table-count from-hash))
    (maphash #'(lambda (decl jdecls)
		 (let* ((to-judgements (gethash decl to-hash))
			(sjdecls (minimal-judgement-decls
				  (subst-params-decls jdecls theory theoryname)
				  to-judgements)))
		   (unless (equal sjdecls to-judgements)
		     (setf (gethash decl to-hash) sjdecls)
		     (setf *judgements-added* t))))
	     from-hash)))

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

(defun merge-application-judgements (from-hash to-hash theory theoryname)
  (unless (zerop (hash-table-count from-hash))
    (maphash
     #'(lambda (decl from-vector)
	 (let ((to-vector (gethash decl to-hash)))
	   (setf (gethash decl to-hash)
		 (if to-vector
		     (merge-appl-judgement-vectors decl from-vector to-vector
						   theory theoryname)
		     (copy-appl-judgement-vectors decl from-vector
						  theory theoryname)))))
     from-hash)))

(defun merge-appl-judgement-vectors (decl from-vector to-vector
					  theory theoryname)
  (when (< (length to-vector) (length from-vector))
    (setq to-vector (resize-array to-vector (length from-vector))))
  (dotimes (i (length from-vector))
    (when (aref from-vector i)
      (cond ((aref to-vector i)
	     (merge-appl-judgements-entries
	      decl (aref from-vector i) (svref to-vector i)
	      theory theoryname))
	    (t (setf (aref to-vector i)
		     (copy-application-judgements (aref from-vector i)
						  theory theoryname))
	       (setq *judgements-added* t)))))
  to-vector)

;;; Entry here is an instance of application-judgements

(defun merge-appl-judgements-entries (decl from-entry to-entry
					   theory theoryname)
  (unless (equal (judgements-graph from-entry)
		 (judgements-graph to-entry))
    (merge-judgement-graphs decl
			    to-entry
			    (judgements-graph from-entry)
			    (judgements-graph to-entry)
			    theory theoryname)))

(defun merge-judgement-graphs (decl to-entry from-graph to-graph
				    theory theoryname)
  
  (let* ((jdecl (judgement-decl (car from-nodes)))
	 (sjdecl (if (eq (module jdecl) theory)
		     (subst-params-decl jdecl theory theoryname)
		     jdecl))
	 (simple? (or (eq (module decl) *current-theory*)
		      (null (formals-sans-usings (module decl)))
		      (fully-instantiated? (judgement-type sjdecl)))))
    (if simple?
	(add-judgement-decl-to-graph sjdecl to-entry)
	(unless (judgement-uninstantiable? decl sjdecl)
	  (push sjdecl (generic-judgements to-entry))))))

(defun judgement-uninstantiable? (decl jdecl)
  nil)
  

;;; Copying judgement structures

(defmethod copy-judgements ((from context))
  (copy-judgements (judgements from)))

(defmethod copy-judgements ((from judgements))
  (make-instance 'judgements
    'number-judgements-hash (copy-number-judgements-hash
			     (number-judgements-hash from))
    'name-judgements-hash (copy-name-judgements-hash
			   (name-judgements-hash from))
    'application-judgements-hash (copy-application-judgements-hash
				  (application-judgements-hash from))))

(defmethod copy-number-judgements-hash (number-hash)
  (let ((new-hash (make-hash-table :test 'eql)))
    (maphash #'(lambda (num judgements)
		 (setf (gethash num new-hash) (copy-list judgements)))
	     number-hash)
    new-hash))

(defmethod copy-name-judgements-hash (name-hash)
  (let ((new-hash (make-hash-table :test 'eq)))
    (maphash #'(lambda (decl judgements)
		 (setf (gethash decl new-hash) (copy-list judgements)))
	     name-hash)
    new-hash))

(defun copy-application-judgements-hash (appl-hash)
  (let ((newhash (make-hash-table :test 'eq)))
    (maphash #'(lambda (decl vector)
		 (setf (gethash decl newhash)
		       (copy-appl-judgement-vectors decl vector)))
	     appl-hash)
    newhash))

(defun copy-appl-judgement-vectors (decl from-vector)
  (map 'simple-vector
       #'(lambda (x)
	   (when x
	     (copy-application-judgements x)))
       from-vector))

(defun copy-application-judgements (appl-judgement)
  (make-instance 'application-judgements
    'argtype-hash (copy (argtype-hash appl-judgement))
    'judgements-graph (copy-judgements-graph
		       (judgements-graph appl-judgement))))

(defun copy-judgements-graph (graph)
  (copy-tree graph))
