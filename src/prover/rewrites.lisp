;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewrites.lisp -- 
;; Author          : N. Shankar
;; Created On      : Sat May 23 10:40:02 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 03:05:24 1998
;; Update Count    : 2
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The rulefun rewrite finds the matching instantiations automatically.

(defun rewrite-step (lemma fnums &optional subst target-fnums
			       dir order)
  (let* ((lemmaname-expr (pc-parse lemma 'name))
	 (resolutions (formula-or-definition-resolutions lemmaname-expr))
	 (sforms (select-seq (s-forms *goal*)
			     (if (numberp fnums) (list fnums)
				 fnums))))
    (cond ((null resolutions)
	   (error-format-if "~%No resolution for ~a" lemma)
	   '(skip))
	  ((or (not (listp subst))
	       (oddp (length subst)))
	   (error-format-if "~%Substitution ~a must be an even length list."
		      subst)
	   '(skip))
	  ((null sforms)
	   (error-format-if "~%No sequent formulas for ~a" fnums)
	   '(skip))
	  (t (let ((in-subst
		    (loop for (x . y) in (make-alist subst)
			  collect
			  (cons (pc-parse x 'name)
				(pc-parse y 'expr)))))
	       (search-and-rewrite lemmaname-expr resolutions sforms
				   in-subst
				   *current-context*
				   target-fnums dir order))))))



(defun search-and-rewrite (name-expr resolutions sforms &optional
				     in-subst context in-sformnums dir order)
  (cond ((null resolutions)
	 (error-format-if "~%No matching instance for ~a found." name-expr)
	 '(skip))
	(t (let* ((res (car resolutions))
		  (mod-inst (module-instance res))
		  (forms (create-formulas res context))
		  (rule (search-and-rewrite* name-expr res mod-inst
					      forms sforms
					     in-subst context
					     in-sformnums dir order)))
	     (cond ((null rule)
		    (search-and-rewrite name-expr (cdr resolutions) sforms
					in-subst context in-sformnums
					dir order))
		   (t rule))))))

(defun tc-alist (in-alist &optional out-alist (tccs 'ALL))
  (cond ((null in-alist)
	 out-alist)
	(t (tc-alist (cdr in-alist)
		     (let* ((var (caar in-alist))
			    (term (cdar in-alist))
			    (expected (when (slot-exists-p var 'type)
					(substit (type var) out-alist))))
		       (if (set-difference (freevars expected)
					   *bound-variables*
					   :test #'same-declaration)
			   (type-error expected "Free variables in type ~a" expected)
			   (cons
			    (cons var
				  (typecheck
				   term
				   :expected
				   expected
				   :tccs tccs ;;NSH(10.20.94)
				   :context *current-context*))
			    out-alist)))
		     tccs))))

(defun check-modsubst (modsubst)
  (cond ((or (null modsubst)(eq modsubst T))
	 modsubst)
	((loop for (nil . y) in modsubst
	       thereis (null y))
	 nil)
	((loop for (x . nil) in modsubst
	       thereis (not (eq (module x)
				(module (caar modsubst)))))
	 nil)
	(t (let* ((module (module (caar modsubst)))
		  (formals (formals-sans-usings module)))
	     (when (subsetp formals modsubst
			    :test #'(lambda (x y) (tc-eq x (car y))))
	       modsubst)))))

(defun search-and-rewrite* (name-expr res mod-inst 
				      forms sforms
				      in-subst context in-sformnums dir order)
  (when (and forms sforms)
    (let* ((form (car forms));;it's always a universal closure
	   (outervars (substitutable-vars form))
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
		    collect (cons x (cdr (assoc x in-subst
						:test #'same-id))))))
	   (in-subst (if check
			 (let ((*tccforms* *tccforms*));;protecting
					;NSH(11/17/93: too strong a check
					;(tc-alist temp-subst)
			   (loop for (nil . y) in temp-subst
				 do (typecheck y))
			   temp-subst)
			 in-subst))
	   (res-params (external-free-params res))
	   (*modsubst* (if res-params
			   (mapcar #'list res-params)
			   T)));;(break "search-rw*")
      (multiple-value-bind
	  (subst modsubst)
	  (if (not check) 'fail
	      (let ((lhs (split-rewrite (car forms) subvars dir)))
		(find-match lhs
			    (formula (car sforms)) nil
			    in-subst order)));;NSH(10.10.94)
	;;dir->order.
	(cond ((or (eq subst 'fail)
		   (null (check-modsubst modsubst)))
	       (or (search-and-rewrite* name-expr res mod-inst 
					forms (cdr sforms)
					in-subst context
					in-sformnums dir order)
		   (search-and-rewrite* name-expr res mod-inst 
					(cdr forms) sforms
					in-subst context
					in-sformnums dir order)))
	      (t (let* ((modinst
			 (unless (eq modsubst T)
			   (let* ((module (module (caar modsubst)))
				  (mod-id (id module))
				  (formals (formals-sans-usings module))
				  (alist
				   (mapcar #'(lambda (fml)
					       (assoc fml modsubst))
				     formals)))
			     (make-instance 'modname
			       'id mod-id 
			       'actuals (mapcar #'(lambda (x) (mk-actual (cdr x))) alist)))))
			(newres
			 (if (eq modsubst T)
			     res
			     (subst-mod-params res modinst)))
			(full-name-expr
			 (copy name-expr
			   'resolutions (list newres)
			   'actuals (actuals (module-instance newres)))))
		   (format-if "~%Found matching substitution:")
		   (loop for (x . y) in subst
			 do (format-if "~%~a gets ~a," x y))
		   `(rewrite-lemma$ ,full-name-expr
				    ,(flatten-sub subst)
				    ,in-sformnums
				    ,dir))))))))
					
(defun get-formulas (name context)
  (create-formulas name context))



(defun flatten-and-unparse-sub (subst)
  (cond ((null subst) nil)
	(t (cons (unparse (caar subst) :string T)
		 (cons (unparse (cdar subst) :string T)
		       (flatten-and-unparse-sub (cdr subst)))))))

(defun flatten-sub (subst)
  (cond ((null subst) nil)
	(t (cons  (if (slot-exists-p (caar subst) 'id)
		      (id (caar subst))
		      (caar subst))
		 (cons  (cdar subst)
		       (flatten-sub (cdr subst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-match (lhs expr bind-alist subst)
  (let* ((*modsubst* *modsubst*)
	 (*no-bound-variables-in-match* T)
;	 (new-subst
;	  (loop for (x . y) in subst
;		collect
;		(cons x
;		      (typecheck
;		       (pc-parse y 'expr)
;		       :expected (when (slot-exists-p x 'type)
;				   (type x))
;		       :context *current-context*))))
	 (result (match lhs expr bind-alist subst)))
    (values result *modsubst*)))

(defmacro some-find-match (try1 try2)
  `(multiple-value-bind (insub inmodsubst)
       ,try1
     (if (eq insub 'fail)
	 ,try2
	 (values insub inmodsubst))))

(defmethod find-match-arith-polarity
    (lhs-template (expr list)
		  bind-alist subst order polarity)
  (if (consp expr)
      (some-find-match
       (find-match-arith-polarity lhs-template
				  (car expr) bind-alist subst
				  order polarity)
       (find-match-arith-polarity lhs-template
				  (cdr expr) bind-alist subst
				  order polarity))
      'FAIL))

(defmethod find-match-arith-polarity
    (lhs-template (expr T)
		  bind-alist subst order polarity)
  (find-out-match-polarity lhs-template expr bind-alist
			   subst order 'FAIL nil polarity))

(defmethod find-match-arith-polarity
    (lhs-template (expr application)
		  bind-alist subst order polarity)
  (let ((op (operator expr)))
    (multiple-value-bind (insub inmodsubst)
	(if (and (name-expr? op)
		   (interpreted? op))
	  (cond
	   ((memq (id (operator expr)) '(< <=))
	    (some-find-match
	     (find-match-arith-polarity
	      lhs-template
	     (args1 expr) bind-alist subst order
	     (arith-polarity polarity 'less))
	     (find-match-arith-polarity
	      lhs-template
	     (args2 expr) bind-alist subst order
	     (arith-polarity polarity 'more))))
	   ((memq (id (operator expr)) '(> >=))
	    (some-find-match
	     (find-match-arith-polarity
	      lhs-template
	     (args1 expr) bind-alist subst order
	     (arith-polarity polarity 'more))
	     (find-match-arith-polarity
	      lhs-template
	     (args1 expr) bind-alist subst order
	     (arith-polarity polarity 'less))))
	   ((is-plus? op)
	    (find-match-arith-polarity
	     lhs-template
	     (arguments expr) bind-alist subst order polarity))
	   ((is-sub-minus? op)
	    (some-find-match
	     (find-match-arith-polarity
	      lhs-template
	     (args1 expr) bind-alist subst order polarity)
	     (find-match-arith-polarity
	      lhs-template
	      (args2 expr) bind-alist subst order (toggle polarity))))
	   ((is-minus? op)
	    (find-match-arith-polarity
	     lhs-template
	     (argument op) bind-alist subst order (toggle polarity)))
	   (t 'FAIL))
	'FAIL)  
    (find-out-match-polarity lhs-template
			     expr bind-alist subst
			     order insub inmodsubst polarity))))

(defmethod find-match-polarity
    (lhs-template (expr application) bind-alist subst order polarity)
  (if polarity ;;i.e., if it's positive/negative
      (multiple-value-bind (insub inmodsubst)
	  (cond ((implication? expr)
		 (multiple-value-bind (insub inmodsubst)
		     (find-match-polarity lhs-template
					  (args2 expr) bind-alist
					  subst order
				      
					  (toggle polarity)
					  )
		   (if (eq insub 'fail)
		       (find-match-polarity
			lhs-template
			(args1 expr)
			bind-alist subst
			order polarity)
		       (values insub inmodsubst))))
		((negation? expr)
		 (find-match-polarity lhs-template
				      (args1 expr) bind-alist
				      subst order
				      (toggle polarity)))
		((or (conjunction? expr)(disjunction? expr))
		 (find-match-polarity lhs-template
				      (arguments expr) bind-alist
				      subst order 
				      polarity))
		(t (find-match-arith-polarity lhs-template
					      expr
					      bind-alist
					      subst
					      order
					      polarity))) ;;no point going further - no polarity
	(find-out-match-polarity lhs-template expr bind-alist
				 subst order insub inmodsubst
				 polarity))
      (find-match (template-expression lhs-template) expr
		  bind-alist subst order)))

(defmethod find-match-polarity
    (lhs-template (expr binding-expr) bind-alist subst order polarity)
  (if polarity
      (multiple-value-bind (insub inmodsubst)
	  (let ((*bound-variables* 
		 (append (bindings expr) *bound-variables*)))
	    (find-match-polarity lhs-template (expression expr)
				 bind-alist
				 subst order polarity))
	(find-out-match-polarity lhs-template expr bind-alist subst
				 order insub inmodsubst polarity))
      (find-match (template-expression lhs-template) bind-alist subst
		  order)))

(defmethod find-match-polarity (lhs-template (expr list) bind-alist subst
				    order polarity)
  (if polarity
      (cond ((null expr) 'fail)
	    (t (multiple-value-bind (insub inmodsubst)
		   (find-match-polarity lhs-template (car expr)
					bind-alist
					subst order polarity)
		 (if (eq insub 'fail)
		     (find-match-polarity lhs-template (cdr expr) bind-alist
					  subst order polarity)
		     (values insub inmodsubst)))))))

;;;if it isn't an application, list or binding, then the polarit
;;;doesn't count, and find-match can be called.
(defmethod find-match-polarity (lhs-template (expr T) bind-alist subst
					     order polarity)
  (declare (ignore polarity))
  (find-match (template-expression lhs-template) expr bind-alist subst order))

(defun find-out-match-polarity (lhs-template expr bind-alist subst order
				    insub inmodsubst polarity)
  (if (and (eq insub 'fail)
	   (not (eq (template-polarity lhs-template)
		    polarity)))
      (call-match (template-expression lhs-template)
		  expr bind-alist subst)
      (if (and (eq order 'out)  ;;NSH(9/92)order does not support
	                        ;;outside-in rewriting.
	       (not (eq (template-polarity lhs-template)
			polarity)))    
	  (multiple-value-bind (topsub topmodsubst)
	      (call-match (template-expression lhs-template) expr bind-alist subst)
	      (if (eq topsub 'fail)
		  (values insub inmodsubst)
		  (values topsub topmodsubst)))
	  (values insub inmodsubst))))

(defun find-out-all-matches-polarity (lhs-template expr bind-alist subst
					  accum polarity)
  (if (and polarity
	   (not (eq (template-polarity lhs-template)
		    polarity)))
      (multiple-value-bind (sub modsub)
	  (call-match (template-expression lhs-template) expr bind-alist subst)
	(if (not (eq sub 'fail))
	    (cons (cons sub modsub) accum)
	    accum))
      accum))

(defmethod find-all-matches-arith-polarity
    (lhs-template (expr list)
		  bind-alist subst accum polarity)
  (if (consp expr)
      (find-all-matches-arith-polarity
       lhs-template (car expr)
       bind-alist subst
       (find-all-matches-arith-polarity lhs-template (cdr expr)
					bind-alist subst accum polarity)
       polarity)
      accum))

(defmethod find-all-matches-arith-polarity
    (lhs-template (expr T)
		  bind-alist subst accum polarity)
  (find-out-all-matches-polarity lhs-template expr bind-alist subst
				 accum polarity))

(defmethod find-all-matches-arith-polarity
    (lhs-template (expr application)
		  bind-alist subst accum polarity)
  (let* ((op (operator expr))
	 (arg-matches 
	  (when (and (name-expr? op)
		     (interpreted? op))
	    (cond
	     ((memq (id (operator expr)) '(< <=))
	      (find-all-matches-arith-polarity
	       lhs-template
	       (args1 expr) bind-alist subst 
	       (find-all-matches-arith-polarity
		lhs-template
		(args2 expr) bind-alist subst
		accum
		(arith-polarity polarity 'more))
	       (arith-polarity polarity 'less)))
	     ((memq (id (operator expr)) '(> >=))
	      (find-all-matches-arith-polarity
	       lhs-template
	       (args1 expr) bind-alist subst
	       (find-all-matches-arith-polarity
		lhs-template
		(args2 expr)
		bind-alist
		subst
		accum
		(arith-polarity polarity 'less))
	       (arith-polarity polarity 'more)))
	     ((is-plus? op)
	      (find-all-matches-arith-polarity
	       lhs-template
	       (args1 expr) bind-alist subst
	       (find-all-matches-arith-polarity
		lhs-template
		(args2 expr) bind-alist subst accum polarity)
	       polarity))
	     ((is-sub-minus? op)
	      (find-all-matches-arith-polarity
	       lhs-template
	       (args1 expr) bind-alist subst
	       (find-all-matches-arith-polarity
		lhs-template
		(args2 expr) bind-alist subst
		 accum (toggle polarity))
	       polarity))
	     ((is-minus? op)
	      (find-all-matches-arith-polarity
	       lhs-template
	       (argument op) bind-alist subst
	        accum (toggle polarity)))
	     (t nil)))))
    (find-out-all-matches-polarity lhs-template
				   expr bind-alist subst
				   arg-matches polarity)))

(defmethod find-all-matches-polarity (lhs-template (expr application)
					  bind-alist subst accum polarity)
  (if polarity ;;i.e., if it's positive/negative
      (let ((newaccum
	     (cond ((implication? expr)
		    (find-all-matches-polarity lhs-template
					       (args2 expr) bind-alist
					       subst 
					       (find-all-matches-polarity
						lhs-template
						(args1 expr)
						bind-alist subst
						accum polarity)
					       (toggle polarity)
					       ))
		   ((negation? expr)
		    (find-all-matches-polarity lhs-template
					       (args1 expr) bind-alist
					       subst accum
					       (toggle polarity)))
		   ((or (conjunction? expr)(disjunction? expr))
		    (find-all-matches-polarity lhs-template
					       (arguments expr) bind-alist
					       subst accum 
					       polarity))
		   (t (find-all-matches-arith-polarity
		       lhs-template expr bind-alist subst
		       accum polarity)))))
	(find-out-all-matches-polarity lhs-template expr bind-alist
				 subst
				 newaccum
				 polarity))
    (find-out-all-matches lhs-template expr bind-alist subst
		    (find-all-matches-polarity
		     lhs-template
		     (cons (operator expr)(arguments expr))
		     bind-alist subst accum polarity))))

(defmethod find-all-matches-polarity (lhs (expr binding-expr) bind-alist
					  subst accum polarity)
      
  (find-out-all-matches-polarity
   lhs expr bind-alist subst
   (let ((*bound-variables*
	  (append (bindings expr)
		  *bound-variables*)))
     (find-all-matches-polarity lhs (expression expr)
				bind-alist
				subst accum polarity))
   polarity))

(defmethod find-all-matches-polarity (lhs (expr list) bind-alist subst
					  accum polarity)
  (if (null expr)
      accum
      (find-all-matches-polarity
       lhs (car expr) bind-alist subst
       (find-all-matches-polarity lhs (cdr expr) bind-alist subst accum
				  polarity)
       polarity)))

(defmethod find-all-matches-polarity (lhs-template (expr T) bind-alist subst
						   accum polarity)
  (find-out-all-matches-polarity lhs-template expr bind-alist subst
				 accum polarity))

(defmethod find-match (lhs (expr  name-expr) bind-alist subst order)
  (declare (ignore order))
  (call-match lhs expr bind-alist subst))

(defmethod find-match (lhs (expr number-expr) bind-alist subst order)
  (declare (ignore order))
  (call-match lhs expr bind-alist subst))

(defun find-out-match (lhs expr bind-alist subst order
			   insub inmodsubst)
  (if (eq insub 'fail)
	(call-match lhs expr bind-alist subst)
	(if (eq order 'out)  ;;NSH(9/92)order does not support
	                     ;;outside-in rewriting.
	    (multiple-value-bind (topsub topmodsubst)
		(call-match lhs expr bind-alist subst)
	      (if (eq topsub 'fail)
		  (values insub inmodsubst)
		  (values topsub topmodsubst)))
	    (values insub inmodsubst))))

(defmethod find-match (lhs (expr application) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (cons (operator expr)(arguments expr))
		  bind-alist subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr projection-application) bind-alist 
			   subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (argument expr) bind-alist subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr field-application) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (argument expr) bind-alist subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))
  

(defmethod find-match (lhs (expr cases-expr) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (cons (expression expr)(selections expr))
		  bind-alist subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr selection) bind-alist subst order)
  (let ((*bound-variables* (append (args expr) *bound-variables*)))
    ;;NSH(8.4.94)
    (find-match lhs (expression expr)
		       bind-alist
		subst order)))

(defmethod find-match (lhs (expr binding-expr) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
     (let ((*bound-variables* 
	    (append (bindings expr) *bound-variables*)))
      (find-match lhs (expression expr)
			     bind-alist
			   subst order))
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr record-expr) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (loop for x in (assignments expr)
			    collect (expression  x))
		  bind-alist
		  subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr tuple-expr) bind-alist subst order)
  (multiple-value-bind (insub inmodsubst)
      (find-match lhs (exprs expr)
		  bind-alist
		  subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))
  

(defmethod find-match (lhs (expr update-expr) bind-alist subst order)
    (multiple-value-bind (insub inmodsubst)
      (find-match lhs (cons (expression expr) (assignments expr))
		  bind-alist
		  subst order)
    (find-out-match lhs expr bind-alist subst order insub inmodsubst)))

(defmethod find-match (lhs (expr assignment) bind-alist subst order)
  (find-match lhs (append (arguments expr)(list (expression expr)))
	      bind-alist subst order))

(defmethod find-match (lhs (expr list) bind-alist subst order)
  (cond ((null expr) 'fail)
	(t (multiple-value-bind (insub inmodsubst)
	       (find-match lhs (car expr)
			   bind-alist
			   subst order)
	     (if (eq insub 'fail)
		 (find-match lhs (cdr expr) bind-alist subst order)
		 (values insub inmodsubst))))))

(defmethod find-match (lhs (expr expr) bind-alist subst order)
  (declare (ignore lhs bind-alist subst order))
  'fail)  ;;multiple values not needed here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;find all matches (NSH 9.20.93).

(defmethod find-all-matches (lhs (expr  name-expr) bind-alist subst
			   accum)
  (multiple-value-bind (sub modsub)
      (call-match lhs expr bind-alist subst)
    (if (not (eq sub 'fail))
	(cons (cons sub modsub) accum)
	accum)))

(defmethod find-all-matches (lhs (expr number-expr) bind-alist subst accum)
  (multiple-value-bind (sub modsub)
      (call-match lhs expr bind-alist subst)
    (if (not (eq sub 'fail))
	(cons (cons sub modsub) accum)
	accum)))

(defun find-out-all-matches (lhs expr bind-alist subst accum)
  (multiple-value-bind (sub modsub)
      (call-match lhs expr bind-alist subst)
    (if (not (eq sub 'fail))
	(cons (cons sub modsub) accum)
	accum)))

(defmethod find-all-matches (lhs (expr application) bind-alist subst accum)
    (find-out-all-matches lhs expr bind-alist subst
		    (find-all-matches lhs (cons (operator expr)(arguments expr))
				      bind-alist subst accum)))

;;NSH(10.18.94)
(defmethod find-all-matches (lhs (expr projection-application)
				 bind-alist subst accum)
  (find-out-all-matches lhs expr bind-alist subst
			(find-all-matches lhs (argument expr)
					  bind-alist subst accum)))


(defmethod find-all-matches (lhs (expr field-application)
				 bind-alist subst accum)
  (find-out-all-matches lhs expr bind-alist subst
			(find-all-matches lhs (argument expr)
					  bind-alist subst accum)))


(defmethod find-all-matches (lhs (expr cases-expr) bind-alist subst accum)
  
    (find-out-all-matches lhs expr bind-alist subst
		    (find-all-matches lhs (cons (expression expr)
						(selections expr))
				      bind-alist subst accum)))

(defmethod find-all-matches (lhs (expr selection) bind-alist subst accum)
  (let ((*bound-variables* (append (args expr) *bound-variables*)))
    ;;NSH(8.4.94) 
    (find-all-matches lhs (expression expr)
		      bind-alist
		      subst accum)))

(defmethod find-all-matches (lhs (expr binding-expr) bind-alist
				 subst accum)
      
    (find-out-all-matches
     lhs expr bind-alist subst
     (let ((*bound-variables*
	    (append (bindings expr)
		    *bound-variables*)))
       (find-all-matches lhs (expression expr)
			 bind-alist
			 subst accum))))

(defmethod find-all-matches (lhs (expr record-expr) bind-alist subst accum)
  
    (find-out-all-matches lhs expr bind-alist subst
		    (find-all-matches lhs (loop for x in (assignments expr)
						collect (expression  x))
				      bind-alist
				      subst accum)))

(defmethod find-all-matches (lhs (expr tuple-expr) bind-alist subst accum)
  
    (find-out-all-matches lhs expr bind-alist subst
		    (find-all-matches lhs (exprs expr)
				      bind-alist
				      subst accum)))
  

(defmethod find-all-matches (lhs (expr update-expr) bind-alist subst accum)
  
    (find-out-all-matches lhs expr bind-alist subst
		    (find-all-matches lhs
				      (cons (expression expr)
					    (loop for x in (assignments expr)
						  collect (expression x)))
				      bind-alist
				      subst accum)))




;(NSH:8-21-91) subsumed by application
;(defmethod find-all-matches (lhs (expr if-expr) bind-alist)
;  (let ((topsub (match lhs expr bind-alist nil)))
;    (if (eq topsub 'fail)
;	(let ((condsub (find-all-matches lhs (condition expr) bind-alist)))
;	  (if (eq condsub 'fail)
;	      (let ((thensub (find-all-matches lhs (then-part expr) bind-alist)))
;		(if (eq thensub 'fail)
;		    (find-all-matches lhs (else-part expr) bind-alist)
;		    thensub))
;	      condsub))
;	topsub)))

(defmethod find-all-matches (lhs (expr list) bind-alist subst accum)
  (cond ((null expr) accum)
	(t (find-all-matches lhs (car expr)
			   bind-alist
			   subst
			   (find-all-matches lhs (cdr expr)
					     bind-alist subst accum)))))


(defmethod find-all-matches (lhs (expr expr) bind-alist subst accum)
  (declare (ignore lhs bind-alist subst ))
  accum)  ;;multiple values not needed here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-rewrite (form &optional given-vars dir forall-vars)
  (if (forall? form)
      (let ((forall-vars (append (relevant-bindings form) forall-vars)))
	(multiple-value-bind
	    (lhs rhs hyp)
	    (split-rewrite (expression form) given-vars dir
			 forall-vars)
	(if (subsetp forall-vars;;NSH(5.21.96)
		     (append given-vars (freevars lhs))
		     :test #'same-declaration)
	    (values lhs rhs hyp)
	    (values form *true* nil))))
      (multiple-value-bind
	  (lhs rhs hyp)
	  (split-rewrite* form given-vars dir)
	(if (subsetp forall-vars;;NSH(5.21.96)
		     (append given-vars (freevars lhs))
		     :test #'same-declaration)
	    (values lhs rhs hyp)
	    (values form *true* nil)))))

;;; Relevant bindings returns those bindings that actually appear in the
;;; forall expression, in other relevant bindings, or that may have empty
;;; types.  Bindings that aren't relevant can safely be dropped from the
;;; expression.

(defun relevant-bindings (forall-expr)
  (relevant-bindings* (reverse (bindings forall-expr))
		      (freevars (expression forall-expr))))

(defun relevant-bindings* (bindings freevars &optional rbindings)
  (if (null bindings)
      rbindings
      (relevant-bindings*
       (cdr bindings)
       freevars
       (if (or (not (nonempty? (type (car bindings))))
	       (member (car bindings) freevars
		       :test #'same-declaration)
	       (member (car bindings) rbindings
		       :test #'(lambda (x y)
				 (member x (freevars y)
					 :test #'same-declaration))))
	   (cons (car bindings) rbindings)
	   rbindings))))

;;; SO 8/17/94 - changed mk-conjunction to make-conjunction
(defun split-rewrite* (form vars dir)
  (cond ((or (equation? form)(iff? form))
	 ;;checks if rhs freevars are in given + lhs freevars.
	 (let ((lhs (if (eq dir 'RL)(args2 form)(args1 form)))
	       (rhs (if (eq dir 'RL)(args1 form)(args2 form))))
	   (if
	     (subsetp (freevars rhs)
		      (append vars
			      (freevars lhs))
		      :test #'same-declaration);;NSH(11.2.94)
	     (values lhs rhs nil)
	     (values form *true* nil))))
	((implication? form)
	 (multiple-value-bind (lhs rhs hyp)
	     (split-rewrite* (args2 form) vars dir)
	   (values lhs rhs
		   (if (null hyp) (args1 form)
		       (make-conjunction (list (args1 form) hyp))))))
	((negation? form)
	 (values (args1 form) *false* nil))
	(t (values form *true* nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tc-eq-or-unify (lhs rhs modsubst)
  (if (eq modsubst T)
      (strict-compatible? lhs rhs)
      (tc-unify rhs lhs modsubst)))

(defun tc-eq-or-unify* (lhs rhs-list modsubst)
  (cond ((null rhs-list) nil)
	(t (let ((newmodsubst
		  (tc-eq-or-unify lhs (car rhs-list) modsubst)))
	     (if newmodsubst (values newmodsubst (car rhs-list))
		 (tc-eq-or-unify* lhs (cdr rhs-list) modsubst))))))

	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(addrule 'cache (names) () (cache-formulas names)
;	 "(cache <name/names>) : Creates and saves formulas for speedier retrieval.")

(defun cache-formulas (names)
;;Caches formula and returns current goal as subgoal so that the step
;;gets recorded in the justification.  
  #'(lambda (ps)
      (cond ((consp names)
	     (loop for name in names
		   do (cache-formula name *current-context*)))
	    (t (cache-formula names *current-context*)))
      (values '? (list (current-goal ps)))))

(defun cache-formula (symbol context)
  (let* ((name (pc-parse symbol 'name))
	 (formula-resolutions (resolve name 'formula nil context))
	 (constant-resolutions
	  (loop for res in (resolve name 'expr nil context)
		when   (definition
			   (declaration res))
		collect res)))
    (loop for res in formula-resolutions
	  when (not (get-cache res *cache-formula*))
	  do (let ((fmlas (create-formulas res context)))
	       (when fmlas
		 (add-symbol-entry (declaration res)
				   (cons (cons res fmlas)
					 (gethash (declaration res)
						  *cache-formula*))
				   *cache-formula*))))
    (loop for res in constant-resolutions
	  when (not (get-cache res *cache-formula*))
	  do (let ((fmlas (create-formulas res context)))
	       (when fmlas
		 (add-symbol-entry (declaration res)
				   (cons (cons res fmlas)
					 (gethash (declaration res)
						  *cache-formula*))
				   *cache-formula*))))
    *cache-formula*))

(defun get-cache (res cache)
  (assoc res (gethash (declaration res) cache)
	 :test #'tc-eq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-names (id expr)
  (let ((names nil))
    (mapobject #'(lambda (ex)
		   (when (and (typep ex 'name-expr)
			      (eq (id ex) id))
		     (pushnew ex names :test #'tc-eq)))
	       expr)
    names))
