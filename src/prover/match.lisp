;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match.lisp -- 
;; Author          : N. Shankar
;; Created On      : Sat May 23 22:44:44 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:44:36 1998
;; Update Count    : 2
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;NSH(5.10.94) NOte that match is used by inst?, rewrite, and replace.
;;It has to be used by replace so that a rewrite occurs when a match
;;is found, and match does some fancy things so that replace cannot
;;use tc-eq.  I need to look at the no-freevars? test in match*:around
;;(type-expr) and see if it violates this condition.  I also need to run
;;some tests to see if there is no significant loss of efficiency by
;;looking inside types for matches.

(defun merge-subst (given hashed)
  (if (every #'(lambda (x)
		  (let ((y (assoc (car x) given :test #'same-declaration)))
		    (if y (tc-eq (cdr x)(cdr y)) T)))
	      hashed)
      (append hashed
	      (set-difference
	       given hashed
	       :key #'car
	       :test #'same-declaration))
      'FAIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(10.16.95): augmented match with the ability to postponed
;;failed matches on actuals in case these matches can be obtained
;;through other sources.  A typical example is (bv1 o bv2) ^ (i, j)
;;where the first arg of ^ has type bitvector(m + n) but the pattern
;;has 47 + 1.  Since the caret is the operator, it is processes first
;;and the match fails.  The postponement of matches should not loop
;;because the postponed matches are not on actuals but on the expr
;;or type part of the actual.

(defun match-remaining-actuals (stack subst)
  (if (null stack)
      subst
      (let* ((stack1 (car stack))
	     (expr (car stack1))
	     (instance (cadr stack1))
	     (bind-alist (caddr stack1))
	     (result (match* expr instance
			     bind-alist subst)))
	(if (eq result 'fail)
	    'fail
	    (match-remaining-actuals (cdr stack) result)))))

(defun match (expr instance bind-alist subst)
  (let* ((*match-cache* (or *match-cache* ;;for initialization
			                   ;;not shadowing
			    (make-hash-table :test #'eq)))
	 (hashed-table-expr
	  (unless *no-bound-variables-in-match* ;;NSH(10.19.95)
	    (gethash expr *match-cache*)))  ;;should not cache or lookup
	 (hashed-value (when hashed-table-expr
			 (gethash instance hashed-table-expr)))
	 (hashed-result (when hashed-value (car hashed-value)))
	 (hashed-modsubst (when hashed-value (cdr hashed-value)))
	 (*dont-cache-match?* nil)
	 (*remaining-actuals-matches* nil))
    (cond ((and hashed-value
		(eq hashed-result 'FAIL))
	   'FAIL)
	  (hashed-value (setq *modsubst* hashed-modsubst)
			(if subst
			    (merge-subst subst hashed-result)
			    hashed-result))
	  (t (let* ((frees (freevars expr)) ;just to set no-freevars?
		    (res (match* expr instance bind-alist subst))
		    (result
		     (if (or (eq res 'fail)
			     (not (subsetp frees res :test
					   #'(lambda (x y) ;;nsh(7.12.95)
					       ;;was tc-eq.
					       (same-declaration x (car y))))))
			 'fail
			 (match-remaining-actuals
			  *remaining-actuals-matches* res))))
	       (when (and (null subst)
			  (null *dont-cache-match?*)
			  (null *no-bound-variables-in-match*))
		 (if hashed-table-expr
		   (setf (gethash instance hashed-table-expr)
			 (cons result *modsubst*))
		   (setf (gethash expr *match-cache*)
			 (let ((hash (make-hash-table :test #'eq)))
			   (setf (gethash instance hash)
				 (cons result *modsubst*))
			   hash))))
	       result)))))
				

(defmethod match* :around ((texpr type-expr)(instance type-expr)
			  bind-alist subst)
	   (if (eq subst 'fail) 'fail
	       (if (and (eq texpr instance)(null (freevars texpr)))
		   subst
		   (with-slots ((print-texpr print-type)) texpr
		     (with-slots ((print-instance print-type)) instance
		       (if (and print-texpr
				print-instance
				(type-name? print-texpr)
				(type-name? print-instance)
				(same-declaration print-texpr
					      print-instance))
			   (match* print-texpr
				   print-instance
				   bind-alist subst)
			   (call-next-method)))))))

(defmethod match* ((texpr type-name) (instance type-expr) bind-alist subst)
  (if (and (consp *modsubst*)
	   (assoc texpr *modsubst* :test #'same-declaration))
      (let ((newmodsubst (tc-unify instance texpr *modsubst*)))
	(cond (newmodsubst
	       (setq *modsubst* newmodsubst)
	       subst)
	      (t 'fail)))
      (if (type-name? instance)
	  (with-slots ((id1 id)(act1 actuals)) texpr
	    (with-slots ((id2 id)(act2 actuals)) instance
	      (if (eq id1 id2)
		  (match* (resolution texpr)(resolution instance)
			  bind-alist
			  subst)
		  'fail)))
	  'fail)))

(defmethod match* ((expr resolution)(instance resolution) bind-alist subst)
;;  (break)
  (if (and (eq expr instance)(null (freevars expr))) subst
  (with-slots ((d1 declaration) (m1 module-instance)) expr
    (with-slots ((d2 declaration) (m2 module-instance)) instance
      (if (eq d1 d2)
	  (match* m1 m2 bind-alist
	     subst)
      'fail)))))

;;NSH(7.14.94) need to be careful with tc-unify-acts since it uses assoc
;;by symbol rather than same-declaration.  Either I check that the modname
;;is the one that is currently being instantiated or Sam needs to change
;;the assoc to use same-declaration.
;;; SO 8/17/94 - changed tc-unify-acts to tc-match-acts
(defmethod match* ((expr modname)(instance modname) bind-alist subst)
;;  (break "match:modname")
  (if (and (eq expr instance)(null (freevars expr))) subst
  (with-slots ((id1 id)(act1 actuals)) expr
    (with-slots ((id2 id)(act2 actuals)) instance
      (if (eq id1 id2)
	  (if act1 
	      (match* act1 act2 bind-alist subst)
	      (if act2 
		  (let ((formals (formals (get-theory id1))))
		    (if formals
			(let* ((*tc-match-exact* t)
			       (newmodsubst
				(and (consp *modsubst*)
				     (or (tc-match-acts
					  act2 formals *modsubst*)
					 (tc-match-acts
					  act2 formals
					  (mapcar #'(lambda (s)
						      (list (car s)))
					    *modsubst*))))))
			  (cond (newmodsubst
				 (setq *modsubst* newmodsubst)
				 subst)
				(t 'fail)))
			'fail))
		  subst))
	  'fail)))))


(defmethod match* ((expr actual)(instance actual) bind-alist subst)
  (if (and (eq expr instance)(null (freevars expr))) subst
  (with-slots ((tv1 type-value) (ex1 expr)) expr
    (with-slots ((tv2 type-value) (ex2 expr)) instance
      (if tv1 (if tv2
		  (let ((res (match* tv1 tv2 bind-alist subst)))
		    (cond ((and (eq res 'fail)
				(freevars tv1))
			   (push (list tv1 tv2 bind-alist)
				 *remaining-actuals-matches*)
			   subst)
			  (t res)))
		  'fail)
	  (if tv2 'fail
	      (let ((res (match* ex1 ex2 bind-alist subst)))
		(cond ((and (eq res 'fail)
			    (freevars ex1))
		       (push (list ex1 ex2 bind-alist)
				 *remaining-actuals-matches*)
		       subst)
		      (t res)))
			 ))))))

(defmethod match* ((expr subtype)(instance subtype) bind-alist subst)
  (with-slots ((st1 supertype) (p1 predicate)) expr
    (with-slots ((st2 supertype) (p2 predicate)) instance
      (match* st1 st2 bind-alist (match* p1 p2 bind-alist subst)))))

(defmethod match* ((expr funtype)(instance funtype) bind-alist subst)
  (with-slots ((d1 domain) (r1 range)) expr
    (with-slots ((d2 domain) (r2 range))  instance
      (match* (if (consp d1)(append d1 (list r1)) (list d1 r1))
	      (if (consp d2)(append d2 (list r2)) (list d2 r2))
	      bind-alist subst))))

(defmethod match* ((expr tupletype)(instance tupletype) bind-alist subst)
  (with-slots ((ty1 types)) expr
    (with-slots ((ty2 types)) instance
      (match* ty1 ty2 bind-alist subst))))

(defmethod match* ((expr cons)(instance tupletype) bind-alist subst)
  (with-slots ((ty2 types)) instance
      (match* expr ty2 bind-alist subst)))

(defmethod match* ((expr tupletype)(instance cons) bind-alist subst)
  (with-slots ((ty1 types)) expr
      (match* ty1 instance bind-alist subst)))

(defmethod match* ((expr recordtype) (instance recordtype) bind-alist
		  subst)
  (with-slots ((fields1 fields)) expr
    (with-slots ((fields2 fields)) instance
      (match* fields1 fields2 bind-alist subst))))

(defmethod match* ((expr field-decl)(instance field-decl) bind-alist
		   subst)
  (with-slots ((id1 id)(ty1 type)) expr
    (with-slots ((id2 id)(ty2 type)) instance
      (if (eq id1 id2)
	  (match* ty1 ty2 bind-alist subst)
	  'fail))))

(defmethod match* ((expr dep-binding)(instance dep-binding) bind-alist
		   subst)
  (with-slots ((ty1 type)) expr
    (with-slots ((ty2 type)) instance
      (match* ty1 ty2 bind-alist subst))))

(defmethod match* ((expr type-expr)(instance type-expr) bind-alist subst)
  (declare (ignore bind-alist))
  (if (tc-eq expr instance)
      subst
      'fail))

(defmethod match* ((expr type-expr)(instance dep-binding) bind-alist subst)
  (match* expr (type instance) bind-alist subst))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arith-match (n1 n2);;for matching arithops(9/28).
  (and (eq (id n1) (id n2))
       (member (id n1) '(+ - * /))
       (member (id (module-instance n2))
	       (member (id (module-instance n1))
		       '(|naturalnumbers| |integers|
			 |rationals| |reals| |numbers|)))))
;;T here caused Allegro compiler bug.

(defmethod match* :around
  ((expr expr)(instance expr) bind-alist subst)
  (if (eq subst 'fail) 'fail
      (if (and (null (freevars expr))(tc-eq expr instance))
	  subst
      (let ((ans (call-next-method)))
	(if (and (eq ans 'fail);;(null bind-alist)
		 (not (bind-decl? expr));;NSH(12.1.94)
		 (type expr) (type instance)
		 ;;NSH(9.19.97)fixes Wilding's rewriting inefficiency
		 ;;by avoiding assert-test0 on numbers
		 (not (and (number-expr? expr)
			   (number-expr? instance)))
		 (tc-eq (find-supertype (type expr)) *number*)
		 (tc-eq (find-supertype (type instance)) *number*))
	    (let* (
		   ;;NSH(9.19.94) not needed anymore.
;		   (*bound-variables*
;		    (nconc (loop for (x . y) in bind-alist
;				 collect y)
;			   *bound-variables*))  ;;;nconc (list x y)))
		   (*keep-unbound* *bound-variables*)
		   (subst-bind-alist
		    (loop for x in bind-alist
			  collect
			  (if (consp (cdr x))
			      (cons (car x)
				    (make!-tuple-expr
				     (mapcar #'(lambda (z)
						 (make!-name-expr
						  (id z) nil nil
						  (make-resolution z
						    nil (type z))))
					     (cdr x))))
			      x)))
		   (substituted-expr
		    (substit expr (append subst-bind-alist subst)))
		   (lhs (if (eq expr substituted-expr)
			    expr
			    (beta-reduce substituted-expr)))
		   (equality (make!-equation lhs instance))
;;NSH(11.18.96): switching back to using assert-test0 over
;;tc-eq-norm-addition. 
		   (result
		    (if (subsetp (freevars lhs) *bound-variables* :test
				 #'tc-eq)
			(assert-test0;;pseudo-normalize would do
			 ;;too much work.  
			 equality)
			'fail))
; 		   (result
;		    (and (subsetp (freevars lhs) *bound-variables* :test
;				 #'tc-eq)
;			 (tc-eq (norm-addition lhs)(norm-addition instance))))
		   ) ;;(when (not (eq (eq result 'TRUE) result2))(break "match :around"))
	      (if  (true-p result)  subst ;;NSH(4.10.97) was 'fail
		  (multiple-value-bind
		      (sig lhs-terms rhs-terms)
		      (light-cancel-terms (addends lhs)
					  (addends instance))
		    (if (and (null lhs-terms)(null rhs-terms))
			subst
			(if (eq sig '?)
			    (match* (make-sum lhs-terms
					     (compatible-type (type lhs)
							      *integer*))
				   (make-sum rhs-terms
					     (compatible-type (type instance)
							      *integer*))
				   bind-alist
				   subst)
		    'fail)))
		  ))
	    ans)))))

(defun light-cancel-terms (lhs-terms rhs-terms)
  (light-cancel-terms* lhs-terms rhs-terms NIL 'X))

(defun light-cancel-terms* (lhs* rhs* lhs-accum sig)
  (cond ((null lhs*)(values sig (nreverse lhs-accum)
			    rhs*))
	(t (let ((rhs-match
		  (find (car lhs*) rhs*
			:test #'tc-eq
			)))
	     (if rhs-match
		 (light-cancel-terms* (cdr lhs*)
				  (remove rhs-match rhs*
					  :count 1);;NSH(2.21.97) 
				  lhs-accum '?)
		 (light-cancel-terms* (cdr lhs*)
				  rhs*
				  (cons (car lhs*) lhs-accum)
				  sig))))))


;		 (let* ((alists (alists *ps*))
;			(usealist (usealist alists))
;			(sigalist (sigalist alists))
;			(findalist (findalist alists))
;			(typealist (typealist alists))
;			(result (assert-test
;				 (make-equality
;				  (substit expr subst)
;				  instance))))
;		   (if (eq result 'TRUE) subst 'fail))
;		 ans)))
				      
		   
		   
(defmethod match* ((lhs expr) (instance coercion) bind-alist subst)
  (match* lhs (args1 instance) bind-alist subst))

(defmethod match* ((lhs coercion) (instance expr) bind-alist subst)
  (match* (args1 lhs)  instance bind-alist subst))

(defmethod match* ((lhs name-expr) (instance coercion) bind-alist subst)
  (match* lhs (args1 instance) bind-alist subst))

(defmethod match* ((lhs field-name-expr) (instance field-name-expr)
		   bind-alist subst)
  (with-slots ((id1 id)(ty1 type)) lhs
    (with-slots ((id2 id)(ty2 type)) instance
      (if (eq id1 id2)
	  (match* ty1 ty2 bind-alist subst)
	  ;(if (tc-eq ty1 ty2 bind-alist) subst 'fail)
	  'fail))))

(defmethod match* ((lhs projection-expr)(instance projection-expr)
		   bind-alist subst)
  (with-slots ((id1 id)(ty1 type)) lhs
    (with-slots ((id2 id)(ty2 type)) instance
      (if (eq id1 id2)
	  (match* ty1 ty2 bind-alist subst)
	  ;(if (tc-eq ty1 ty2 bind-alist) subst 'fail)
	  'fail))))


;;NSH(7.13.94): Several changes: since match* is used by replace, it
;;should do an exact check on the type of non-free variables but
;;only require a compatibility check on the type of freevar/instance
;;pairs.  It should however try to match the type of a freevar in case
;;there is  another freevar within it.   This is still unfinished because
;;of the tc-eq test for subst-terms and the set-type not making sense.
;;Need to also make sure that type of constants are not matched since any
;;free variables must already occur in the actuals.

(defun same-declaration-binding (x binding)
  (if (consp binding)
      (every #'(lambda (y)(same-declaration x y))
	     binding)
      (same-declaration x binding)))

(defmethod match* ((lhs name-expr) (instance expr) bind-alist subst)
  (cond					;((eq subst 'fail) 'fail)
   ((variable? lhs)
    (let ((bind-entry (assoc-decl lhs bind-alist))
	  (instance-bindings
	   (loop for (nil . y) in bind-alist
		 append (if (consp y) y (list y))))
	  (subst-entry (or (assoc lhs subst :test #'same-declaration)
			   (assoc lhs subst
				  :test
				  #'(lambda (x y)
				      (and (null (declaration y))
					   (same-id x y)))))))
      (cond
       (bind-entry  (if (and (variable? instance)
			     (binding? (cdr bind-entry)) 
			     (same-declaration instance
					       (cdr bind-entry)))
			subst
			(if (and (tuple-expr? instance)
				 (consp (cdr bind-entry))
				 (eql (length (exprs instance))
				      (length (cdr bind-entry)))
				 (loop for 
				       inst in (exprs instance)
				       as bdecl in (cdr bind-entry)
				       always
				       (same-declaration inst bdecl)))
			    subst
			    'fail)))
       (subst-entry
	(let* ((subst-term (cdr subst-entry))
	       (newmodsubst
		(tc-eq-or-unify* (substit (type lhs) subst)
				 (if (type subst-term)
				     (list (type subst-term))
				     (types subst-term))
				 *modsubst*)))
	  (cond (newmodsubst
		 (let ((newsubst-term (if (type subst-term)
					  subst-term
					  (copy-all subst-term))))
		   (unless (type newsubst-term)
		     (set-type newsubst-term (type instance)
			       *current-context*))
		   (if (tc-eq newsubst-term instance)
		       (let* ((subst
			       (if (type subst-term)
				   subst
				   (acons (declaration lhs)
					  newsubst-term
					  (remove subst-entry subst))))
			      (typesubst (match* (type lhs)
						 (type instance)
						 bind-alist
						 subst)))
			 (setq *modsubst* newmodsubst)
			 (if (eq typesubst 'fail)
			     subst
			     typesubst))
		       'fail)))
		(t (let ((typesubst (match* (type lhs)(type instance)
					    bind-alist subst)))
		     (if (eq typesubst 'fail)
			 'fail
			 typesubst))))))
       (;;(and (null bind-entry)(null subst-entry) ;(variable? lhs)
	(let ((freevars (freevars instance)))
	  (assert (subsetp instance-bindings *bound-variables*
			   :test #'same-declaration))
	  (and (null (intersection freevars instance-bindings
				   :test #'same-declaration))
	       (or (null *no-bound-variables-in-match*)
		   (null (intersection freevars *bound-variables*
				       :test #'same-declaration)))))
	(let* ((stype (substit (type lhs) subst))
	       (newmodsubst (tc-eq-or-unify stype (type instance) *modsubst*))
	       (newsubst (acons (declaration lhs) instance subst)))
	  ;;(break "match:name-expr")
	  (cond (newmodsubst
		 (setq *modsubst* newmodsubst)
		 (let ((typesubst (match* (type lhs)(type instance)
					  bind-alist newsubst)))
		   (if (eq typesubst 'fail)
		       newsubst
		       typesubst)))
		(t (let ((typesubst (match* (type lhs)(type instance)
					    bind-alist newsubst)))
		     (if (eq typesubst 'fail)
			 ;; SO - this was just fail, but types are not
			 ;; necessarily tc-eq; for example, matching
			 ;; an integer variable against the number 1
			 (if (compatible? (type lhs) (type instance))
			     newsubst
			     'fail)
			 typesubst))))))
       (t 'fail))))
   ((and (typep instance 'name-expr)
	 (tc-eq lhs instance))
    subst)
   ((and (typep instance 'name-expr)
	 (same-id lhs instance))
    (match* (resolution lhs)(resolution instance) bind-alist subst))
   ((and (consp *modsubst*)
	 (assoc-decl lhs *modsubst*))
    (let ((newmodsubst (tc-unify instance lhs *modsubst*)))
      (cond (newmodsubst
	     (setq *modsubst* newmodsubst)
	     subst)
	    (t 'fail))))
   (t 'fail)))

;;NSH: replaced by match*(resolution) above.	     
;		  (eq (declaration lhs)(declaration instance))))
;	 (let ((modsubst
;		(tc-eq-or-unify (type lhs)(type instance) *modsubst*)))
;	   (cond (modsubst (setq *modsubst* modsubst) subst)
;		 (t 'fail))))
;	 (t 'fail)))

;;; SO 8/17/94 - uses the new form of application
(defmethod match* ((lhs application)(instance application) bind-alist subst)
  (with-slots ((op1 operator)(lhs-args argument)) lhs
    (with-slots ((op2 operator)(rhs-args argument)) instance
      (cond				;((eq subst 'fail) 'fail)
       ((higher-order-pattern? lhs bind-alist)
	(match-ho-pattern lhs instance bind-alist subst))
       (t (let* ((opresult (match-ops op1 op2 bind-alist subst))
		 (result (match* lhs-args rhs-args bind-alist opresult)))
	    (if (eq result 'fail)
		(call-next-method)
		result)))))))

(defmethod match-ops ((op1 constructor-name-expr) (op2 constructor-name-expr)
		      bind-alist subst)
  (match-adt-ops op1 op2 bind-alist subst))

(defun match-adt-ops (op1 op2 bind-alist subst)
  (if (eq (id op1) (id op2))
      (let ((adt1 (adt op1))
	     (adt2 (adt op2)))
	 (if (eq (adt adt1) (adt adt2))
	      (match-adt-actuals (actuals (module-instance
					   (resolution (or (print-type adt1)
							   adt1))))
				 (actuals (module-instance
					   (resolution (or (print-type adt2)
							   adt2))))
				 bind-alist
				 subst
				 (formals-sans-usings (adt adt1))
				 (positive-types (adt adt1)))
	      'fail))
      'fail))

(defmethod match-ops ((op1 expr) (op2 expr) bind-alist subst)
  (match* op1 op2 bind-alist subst))

(defun match-adt-actuals (acts1 acts2 bind-alist substs formals postypes)
  (cond ((null acts1)
	 substs)
	((eq substs 'fail)
	 'fail)
	(t (match-adt-actuals
	    (cdr acts1) (cdr acts2)
	    bind-alist
	    (cond ((member (car formals) postypes :test #'same-id)
		   (let ((asubsts (match* (type-value (car acts1))
					  (type-value (car acts2))
					  bind-alist substs)))
		     (if (eq asubsts 'fail)
			 (match* (find-supertype (type-value (car acts1)))
				 (find-supertype (type-value (car acts2)))
				 bind-alist substs)
			 asubsts)))
		  ((tc-eq (car acts1) (car acts2))
		   substs)
		  (t 'fail))
	    formals
	    postypes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(10.18.94): projection/field applications

(defmethod match* ((lhs field-application)(rhs field-application)
		   bind-alist subst)
  (with-slots ((id1 id)(arg1 argument)) lhs
    (with-slots ((id2 id)(arg2 argument)) rhs
      (if (eq id1 id2)
	  (match* arg1 arg2 bind-alist subst)
	  'fail))))

(defmethod match* ((lhs projection-application)(rhs projection-application)
		   bind-alist subst)
  (with-slots ((ind1 index)(arg1 argument)) lhs
    (with-slots ((ind2 index)(arg2 argument)) rhs
      (if (eql ind1 ind2)
	  (match* arg1 arg2 bind-alist subst)
	  'fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(4.28.94) : code for higher-order matching.

(defun check-ho-pattern-args-list* (args-list bind-alist)
  (every #'(lambda (args) (check-ho-pattern-args args bind-alist))
	 args-list))

;; I changed the following function to add the name-expr? test...
;; it was giving an error trying to find declarations of things
;; that didn't have them.  (I already changed match.lisp.)  -- crw
(defun check-ho-pattern-args (args bind-alist)
  (every #'(lambda (x) (and (name-expr? x)
			    (assoc-decl x bind-alist)))
	 args))

(defun check-ho-pattern-args-list (args-list bind-alist)
  (and (check-ho-pattern-args-list* args-list bind-alist)
       (not (duplicates? (apply #'append args-list)))))

(defun higher-order-pattern? (lhs bind-alist)
  ;;assuming that lhs is an application.
  (let ((op* (operator* lhs))
	(args* (arguments* lhs)))
    (and (or (and (variable? op*) ;;op* must be free var.
		  (null (assoc-decl op* bind-alist)))
	     (lambda-expr? op*)) ;;or lambda-expr.  
	 (check-ho-pattern-args-list args* bind-alist))))

(defun match-ho-pattern (lhs instance bind-alist subst)
  ;;assuming that lhs is a higher-order pattern.
  (let* ((op* (operator* lhs))
	 (args* (arguments* lhs))
	 (rhs-args* (map-args*-with-bind-alist args* bind-alist))
	 (instance-op* (operator* instance))
	 (instance-args* (arguments* instance))
	 (rhs-for-op*
	  (if (and (application? instance)
		   (eql (length rhs-args*)(length instance-args*))
		   (every #'(lambda (x)
			      (every #'variable? x))
			  instance-args*);;NSH(1.5.95)
		   (every #'(lambda (x y)
			      (and (eql (length x)(length y))
				   (every #'same-declaration x y)))
			  rhs-args* instance-args*)
		   (tc-eq-or-unify (type op*) ;;NSH(5.29.96) was tc-eq
				   (type instance-op*)
				   *modsubst*))
	      instance-op*
	  (make-lambda-expr-iter rhs-args* instance))))
    (match* op* rhs-for-op* bind-alist subst)))

(defun make-lambda-expr-iter (args* instance)
  (if (null args*) instance
      (make-lambda-expr-iter (cdr args*)
			     (make!-lambda-expr (car args*)
			       instance))))
     
(defun map-args*-with-bind-alist (args*  bind-alist)
  ;;assuming higher-order-pattern? succeeded
  ;;and each x is bound in bind-alist.  
  (loop for args in args* collect
	(loop for x in args
	      append (let ((y (cdr (assoc-decl x bind-alist))))
		       (if (consp y) y (list y))))))


(defmethod match* ((lhs application)(instance expr) bind-alist subst)
    (cond ;((eq subst 'fail) 'fail)
        ((higher-order-pattern? lhs bind-alist)
	 (match-ho-pattern lhs instance bind-alist subst))
	((and (tc-eq (find-supertype (type lhs)) *number*)
	      (tc-eq (find-supertype (type instance)) *number*))
	 (if (is-addition? lhs)
	     (if (number-expr? (args1 lhs))
		 (let ((diff (make-assert-expr
			      (make-difference instance (args1 lhs)
					       (type (args2 lhs))))))
		   (if diff
		       (match* (args2 lhs) diff
			      bind-alist subst)
		       'fail))
		 (if (number-expr? (args2 lhs))
		     (let ((diff (make-assert-expr
				  (make-difference instance (args2 lhs)
						   (type (args1 lhs))))))
		       (if diff (match* (args1 lhs) diff
				       bind-alist subst)
			   'fail))
		     'fail))
	     (if (is-subtraction? lhs)
		 (if (number-expr? (args2 lhs))
		     (let ((sum (make-assert-expr
				 (make-sum (list instance (args2 lhs))
					   (type (args1 lhs))))))
		       (if sum (match* (args1 lhs) sum
				      bind-alist subst)
			   'fail))
		     'fail)
		 'fail)))
	(t 'fail)))
    
(defmethod match* ((lhs branch)(instance branch) bind-alist subst)
  (match* (else-part lhs)(else-part instance) bind-alist
	 (match* (then-part lhs)(then-part instance) bind-alist
		 (match* (condition lhs)(condition instance)
			 bind-alist subst))))

(defmethod match* ((lhs cases-expr)(instance cases-expr) bind-alist subst)
  (with-slots ((lhs-expr expression)
	       (lhs-selections selections)
	       (lhs-else else-part))
      lhs
    (with-slots ((rhs-expr expression)
		 (rhs-selections selections)
		 (rhs-else else-part))
	instance
	(let ((expression-match
	       (match* lhs-expr rhs-expr
		       bind-alist subst)))
	  (if (eq expression-match 'fail) 'fail
	      (let ((sel-match
		     (match-selections lhs-selections
				       rhs-selections
				       bind-alist expression-match)))
		(if (eq sel-match 'fail) 'fail
		    (match* lhs-else rhs-else
			    bind-alist sel-match))))))))

(defun match-selections (lhs-selections instance-selections
					bind-alist subst)
  (cond ((null lhs-selections)
	 (if (null instance-selections) subst 'fail))
	(t (let* ((lhs (car lhs-selections))
		  (instance (find lhs instance-selections
				  :test #'(lambda (x y)
					    (exequal (constructor x)
						     (constructor y))))))
	     (if (null instance) 'fail
		 (match-selections (cdr lhs-selections)
				   (remove lhs instance-selections
					   :test #'(lambda (x y)
					    (exequal (constructor x)
						     (constructor y))))
				   bind-alist
				   (let ((*bound-variables*
					  (append (args instance)
						  *bound-variables*)))
				     (match* (expression lhs)
					     (expression instance)
					     (nconc (pairlis (args lhs)
							     (args instance))
						    bind-alist)
					     subst))))))))
							   
			
(defun match-bindings (lhs instance bind-alist subst)
  (if (null lhs)
      (if (null instance) subst
	  'fail)
      (if (null instance) 'fail
	  (if (and (singleton? lhs)(not (singleton? instance)))
	      (match* (type (car lhs))
		      (make-tupletype (mapcar #'type instance))
		      bind-alist subst)
	      (let ((res (match* (car lhs)(car instance)
				 bind-alist subst))
		    (*bound-variables* (cons (car instance)
					     *bound-variables*)))
		(match* (cdr lhs)(cdr instance)
			(cons (cons (car lhs)(car instance)) bind-alist)
			res))))))

(defmethod match* ((lhs bind-decl)(rhs bind-decl) bind-alist subst)
  (match* (type lhs)(type rhs) bind-alist subst))

(defun pair-boundvars (list1 list2)
  (if (or (null list1)(null list2))
      nil
      (if (and (singleton? list1)(not (singleton? list2)))
	  (list (cons (car list1) list2))
	  (cons (cons (car list1)(car list2))
		(pair-boundvars (cdr list1)(cdr list2))))))
      
(defmethod match* ((lhs binding-expr)(instance binding-expr) bind-alist
		   subst)
  (with-slots ((bind1 bindings)(expr1 expression))
      lhs
    (with-slots ((bind2 bindings)(expr2 expression))
	instance
      (cond				;((eq subst 'fail) 'fail)
       ( (same-binding-op? lhs instance);;(break "match-bind")
	     ;(equal (length bind1)(length bind2)))
	;;the above is not enough, the type must match as well
	;;do this later...(nsh:5/25)
	(let ((*bound-variables* 
	       (append bind2
		       *bound-variables*)))   
	  (match* expr1 expr2
		  (nconc (pair-boundvars bind1 bind2)
			 bind-alist)
		  (match-bindings bind1 bind2 bind-alist subst))))
       (t 'fail)))))

(defmethod match* ((lhs record-expr)(instance record-expr)
		  bind-alist subst)
  (with-slots ((lhs-assigns assignments)) lhs
    (with-slots ((rhs-assigns assignments)) instance
      (cond				;((eq subst 'fail) 'fail)
       ((check-ids (loop for x in lhs-assigns
			 collect (caar (arguments x)))
		   (loop for x in rhs-assigns
			 collect (caar (arguments x))))
	(match* (loop for x in lhs-assigns
		      collect (expression x))
		(loop for x in lhs-assigns
		      collect (expression
			       (find x rhs-assigns
				     :test #'(lambda (x y)
					       (same-id (caar (arguments x))
							(caar (arguments y)))))
))
		bind-alist subst))
       (t 'fail)))))

(defmethod match* ((lhs tuple-expr)(instance tuple-expr)
		  bind-alist subst)
  (with-slots ((lhs-exprs exprs)) lhs
    (with-slots ((rhs-exprs exprs)) instance
      (match* lhs-exprs rhs-exprs
	      bind-alist subst))))

(defmethod match* ((lhs update-expr)(instance update-expr)
		   bind-alist subst)
  (with-slots ((lhs-assigns assignments)(lhs-expr expression)) lhs
    (with-slots ((rhs-assigns assignments)(rhs-expr expression)) instance
      (match* lhs-assigns rhs-assigns
	      bind-alist
	      (match* lhs-expr rhs-expr
		      bind-alist subst)))))

(defmethod match* ((lhs assignment)(instance assignment)
		   bind-alist subst)
  (with-slots ((lhs-args arguments)(lhs-expr expression)) lhs
    (with-slots ((rhs-args arguments)(rhs-expr expression)) instance
      (match* lhs-expr rhs-expr
	      bind-alist
	      (match* lhs-args rhs-args
		      bind-alist subst)))))

(defmethod match* ((lhs field-assignment-arg) (instance field-assignment-arg)
		   bind-alist subst)
  (declare (ignore bind-alist))
  (if (eq (id lhs) (id instance))
      subst
      'fail))

(defmethod match* ((lhs field-assignment-arg) instance bind-alist subst)
  (declare (ignore instance bind-alist subst))
  'fail)

(defmethod match* ((lhs name-expr) (instance field-assignment-arg)
		   bind-alist subst)
  (declare (ignore bind-alist subst))
  'fail)

(defmethod match* ((lhs application) (instance field-assignment-arg)
		   bind-alist subst)
  (declare (ignore bind-alist subst))
  'fail)

(defmethod match* ((lhs list) (instance list) bind-alist subst)
  (cond					;((eq subst 'fail) 'fail)
   ((and (null lhs)(null instance)) subst)
   ((= (length lhs)(length instance))
    (if (dep-binding? (car lhs))
	(if (dep-binding? (car instance))
	    (let ((car-result (match* (car lhs);;NSH(7.26.94)
				      (car instance)
				      bind-alist
				      subst))
		  (*bound-variables* (cons (car instance)
					   *bound-variables*)))
	      (match* (cdr lhs)(cdr instance)
		      (cons (cons (car lhs)(car instance))
			    bind-alist)
		      car-result))
	    'fail)
	(if (dep-binding? (car instance))
	    'fail
	    (match* (cdr lhs)(cdr instance) bind-alist
		    (match* (car lhs)(car instance) bind-alist subst)))))
   (t 'fail)))

(defmethod match* ((lhs expr) (instance expr) bind-alist subst)
  (declare (ignore bind-alist))
  (if (tc-eq lhs instance)
      subst
      'fail))
