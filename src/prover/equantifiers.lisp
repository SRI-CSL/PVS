;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equantifiers.lisp -- Quantifier rules
;; Author          : Natarajan Shankar
;; Created On      : Fri Oct  8 12:57:34 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:23:17 1998
;; Update Count    : 34
;; Status          : Beta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;resolution for name is not copied in classes.lisp
(defun copy-name-expr (name-expr new-id)
  (lcopy name-expr
    'id new-id
    'resolutions (resolutions name-expr)))
						   
(defun declared-type-module-instance (mi)
  (let ((nacts (mapcar #'(lambda (act)
			   (if (type-value act)
			       (if (name-expr? (expr act))
				   (copy act
				     'type-value
				     (mk-type-name (expr act)))
				   (copy act
				     'type-value
				     (expr act)))
			       act))
		 (actuals mi))))
    (if (equal nacts (actuals mi))
	mi
	(copy mi 'actuals nacts))))

(defun gen-symbol (name  char counter)    
  (let* ((string (string  (op-to-id  name)))
	 (pos (position-if #'(lambda (x)(member x (list char #\! #\_)))
			   string :from-end T));;NSH(4-11-94) $ to _.
	 ;;(1/21/91) Changed
	 (prefix (subseq string 0 pos))
	 (suffix (when pos (subseq string (1+ pos))))
	 (prestring (if (and pos (every #'digit-char-p suffix))
			prefix
			string)))
    (format nil "~a~a~a" prestring char (funcall counter))))

(defun symbol-index (name)
  (let* ((name  (intern  (string  (op-to-id  name))))
	 (string (format nil "~a" name))
	 (pos (position-if #'(lambda (x)(member x (list  #\! #\$))) string))
         (index (when pos (parse-integer string :start (1+ pos)
					 :junk-allowed T))))
    index))

(defun symbol-prefix (id)
  (let* ((string (string (op-to-id id)))
	 (pos (position #\_ string :from-end T))
	 (prefix (if pos (subseq string 0 pos)
		     string))
	 (index (when pos  ;;NSH(9.20.95)
		  (parse-integer string :start (1+ pos) :junk-allowed T))))
    (if index (intern prefix) id)))
	

(defun pvs-gentemp (string &optional (count 0))
  (let ((next (format nil "~a~a" string count)))
    (if (find-symbol next)
	(pvs-gentemp string (1+ count))
	(intern next))))

(defun new-boundvar-id (id)
  (let* ((string (string (op-to-id id)))
	 (pos (position #\_ string :from-end T))
	 (prefix (if pos (subseq string 0 pos)
		     string))
	 (suffix (if pos (subseq string (1+ pos) ) "")))
    (if (every #'digit-char-p suffix)
	(gentemp (concatenate 'string  prefix "_"))
	(gentemp  (concatenate 'string string "_")))))
         

(defun new-symbol (name counter)
  (intern (gen-symbol name #\$ counter)))

(defun new-sko-symbol (name context &optional counter symbols &key keep-underscore?)
  (unless counter (newcounter *skofun-counter*))
  (let* ((symb (if keep-underscore?
		   (concatenate 'string
		     (string name)
		     "!"
		     (princ-to-string (funcall *skofun-counter*)))
		   (gen-symbol name #\! *skofun-counter*)))
	 (isymb (intern symb)))
    (if (or (declared? isymb context)
	    (member symb symbols :test #'same-id))
	(new-sko-symbol name context *skofun-counter* symbols
			:keep-underscore? keep-underscore?)
	symb)))

(defun new-sko-symbol-list (names context &optional counter symbols
				  &key keep-underscore?) 
  (cond ((null names) (nreverse symbols))
	(t (let ((symb1 (new-sko-symbol (car names)
					context counter symbols
					:keep-underscore? keep-underscore?)))
	     (new-sko-symbol-list (cdr names) context counter
				  (cons symb1 symbols)
				  :keep-underscore?
				  keep-underscore?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-skofun (boundvar governing-vars context)
  (let* ((id (new-symbol (format nil "~a~a"  "#" (id boundvar))
			 *skofun-counter*))
	 (type (if (null governing-vars)
		   (type boundvar)
		 (make-instance 'funtype
				'domain (loop for x in governing-vars
					      collect (type x))
				'range (type boundvar))))
	 (const-decl (make-instance 'const-decl
				  'id id
				  'module (module context)
				  'type type)))
    (add-decl const-decl  context)
    (typecheck (pc-parse id 'expr)
	       :expected type
	       :context context)))

;  (make-instance 'name-expr
;    'type (cond ((null governing-vars)
;		 (type boundvar))
;		(t (make-instance 'funtype
;		     'domain (loop for x in governing-vars collect (type x))
;		     'range (type boundvar))))
;    'name (make-instance 'name
;	    'id (new-symbol (format nil "~a~a"  "#" (id (name boundvar)))
;			    *skofun-counter*))))
;;(NSH4-19: make-skoterm and make-skofun not used any more.
;(defun make-skoterm (skofun governing-vars context)
;  (if (null governing-vars) skofun
;    (let ((skoterm (make-instance 'application
;				  'operator skofun
;				  'arguments governing-vars)))
;      (typecheck skoterm
;		 :expected (range (type skofun))
;		 :context context)
;      skoterm)))

;(defun make-skovar (boundvar context)
;  (let* ((id (new-symbol (format nil "~a~a"  "?" (id boundvar))
;			 *skovar-counter*))
;	 (var-decl (make-instance 'var-decl
;				  'id id
;				  'module (module context)
;				  'type (type boundvar))))
;    (add-decl var-decl context)
;    (typecheck (pc-parse id 'expr) :context context)))
				  
;  (make-instance 'name-expr
;    'type (type boundvar)
;    'name (make-instance 'name
;	    'id (new-symbol (format nil "~a~a"  "?" (id (name boundvar)))
;			    *skovar-counter*))))

(defun declared? (id context)
  (or (not (every #'var-decl? (gethash id (local-decls context))))
       (not (null (gethash id (local-proof-decls context))))))

(defun one-to-one (alist)
  (if (consp alist)
      (if (consp (car alist))
	  (and (null (assoc (caar alist) (cdr alist)))
	       (not (member (cdar alist)(cdr alist)
			    :test #'(lambda (x y)(if (consp y)
						     (equal x (cdr y))
						     NIL))))
	       (one-to-one (cdr alist)))
	  (one-to-one (cdr alist)))
      T))

(defun makeskoconst (id type context)
  (put-decl (make-instance
	     'skolem-const-decl
	     'id (id id)
	     'type type
	     'module (module context))
	    (local-proof-decls context))
  (typecheck
   id
   :expected type 
   :context context))

(defmethod quantifier-step ((expr quant-expr)  context sign terms ps)
  (let* ((preterms (if (listp terms) terms (list terms)))
	 (terms (loop for x in preterms
		      collect (pc-parse x 'expr)))
	 (boundvars (bindings expr))
	 (sub-boundvars (loop for x in boundvars
			      as y in terms
			      when (not (and (typep y 'name-expr)
					     (eq (id y) '_)))
			      collect x))
	 (id-boundvars  (loop for x in boundvars
			      as y in terms
			      when (and (typep y 'name-expr)
					(eq (id y) '_))
			      collect x))
	 (sub-freevars (loop for x in sub-boundvars
			     append (freevars x)))
	 (overlap (intersection sub-freevars id-boundvars
				:test #'same-declaration))
	 (subterms (loop for y in terms
			 when (not (and (typep y 'name-expr)
					 (eq (id y) '_)))
			 collect y))
	 (qalist (unless overlap 
		   (make-quant-alist sub-boundvars subterms)))
	 (freevars (unless (stringp qalist) ;; Error message
		     (loop for (x . y) in qalist
			   append (freevars y)))));;(break "quantifier-step")
    (cond ((stringp qalist)
	   (format-if qalist)
	   expr)
	  (freevars
	   (format-if "~%The supplied terms should not contain free variables.
The following irrelevant free variables occur in the terms: ~a"
		      freevars)
	   expr)
	  (overlap
	   (format-if
	    "~%The types of the substituted variables contain free occurrences
of the following quantified variables: ~a.
Please provide substitutions for these variables." overlap)
	   expr) 
	  (t (let* ((newexpr
		     (if id-boundvars
			 (if (eql (length id-boundvars)
				  (length boundvars))
			     expr
			     (lcopy expr
			       'bindings id-boundvars))
			 (expression expr)))
		    (qbody (if (eq newexpr expr) expr
			       (substit newexpr qalist))))
	       (unless (exequal qbody expr)
		 (push-references-list qalist *dependent-decls*))
	       qbody)))))
		     

(defun make-quant-alist (boundvars terms &optional alist)
  (if (null boundvars)
      (nreverse alist)
      (multiple-value-bind (newterm error)
	  ;; Make sure we catch any type-errors
	  (with-no-type-errors
	   (internal-pc-typecheck (car terms)
	     :tccs 'ALL
	     :expected (substit (type (car boundvars)) alist)))
	(if newterm
	    (make-quant-alist (cdr boundvars)
			      (cdr terms)
			      (acons (car boundvars) newterm alist))
	    error))))

(defmethod skolemize-step ((expr quant-expr)  context sign terms)
  (let* ((preterms (if (listp terms) terms (list terms)))
	 (terms (loop for x in preterms
		      collect (pc-parse x 'expr)))
	 (boundvars (bindings expr))
	 (sub-boundvars (loop for x in boundvars
			      as y in terms
			      when (not (and (typep y 'name-expr)
					     (eq (id y) '_)))
			      collect x))
	 (id-boundvars  (loop for x in boundvars
			      as y in terms
			      when (and (typep y 'name-expr)
					(eq (id y) '_))
			      collect x))
	 (sub-freevars (loop for x in sub-boundvars
			     append (freevars x)))
	 (overlap (intersection sub-freevars id-boundvars
				:test #'same-declaration))
	 (subterms (loop for y in terms
			 when (not (and (typep y 'name-expr)
					(eq (id y) '_)))
			 collect y))
	 (check (loop for  y in subterms
		      when  (or (not (typep y 'name-expr))
				(declared? (id y) context)
				(not (every #'(lambda (r)
						(typep (declaration r)
						       'var-decl))
					    (resolve y 'expr nil context))))
		      collect y))
	 (*current-context* context))
    (cond (check
	   (format-if "~%The supplied skolem constants must all be new names.
The following are either not names or are previously declared: ~a" check)
	   expr)
	  ((duplicates? subterms :test #'same-id)
	   (format-if "~%Duplicate use of skolem constants.")
	   expr)
	  (overlap
	   (format-if
	    "~%The types of the skolemized variables contain free occurrences
of the following quantified variables: ~a.
Please provide skolem constants for these variables." overlap)
	   expr)
	  (t (let ((newexpr (if id-boundvars
				(if (eql (length id-boundvars)
					 (length boundvars))
				    expr
				    (lcopy expr
				      'bindings id-boundvars))
				(expression expr))))
	       (if (eq newexpr expr) expr
		   (substit newexpr
		     (make-skolem-alist sub-boundvars
					subterms context))))))))


(defun make-skolem-alist (boundvars skoconstants context &optional alist)
  (cond ((null boundvars)
	 (loop for (x . y) in alist
	       do (record-type-constraints y))
	 (nreverse alist))
	(t (let ((binding (cons (car boundvars)
				(makeskoconst (car skoconstants)
					      (substit (type (car boundvars))
						alist)
					      context))))
	     (make-skolem-alist (cdr boundvars)(cdr skoconstants)
				context
				(cons binding alist))))))

;(defun quant-rule (sformnum substs)
;  (make-instance 'rule
;    'rule-part (quant-rule-fun sformnum substs)
;    'rule-input `(quant ,sformnum ,substs)))

(defun quant-rule-fun ( sformnum  terms &optional copy?)
  #'(lambda (ps)
      (let* ((terms (if (listp terms) terms (list terms)))
	     (sformnum (find-quant sformnum terms ps)))
	(quant-step sformnum ps terms copy?))))

(defun find-quant (sformnum terms ps)
  (find-sform (s-forms (current-goal ps)) sformnum
	      #'(lambda (sform)
		  (or (and (exists-expr? (formula sform))
			   (eql (length (bindings (formula sform)))
				(length terms)))
		      (and (not-expr? (formula sform))
			   (forall-expr?
			    (args1 (formula sform)))
			   (eql (length (bindings
					 (args1 (formula sform))))
				(length terms)))))))

(defun skolem-rule-fun (&optional sformnum  terms)
  #'(lambda (ps)
	(skolem-step sformnum ps terms)))

(defun make-alist (substs)
  (if (and (consp substs)(consp (cdr substs)))
      (cons (cons (car substs)(cadr substs))
	    (make-alist (cddr substs)))
    NIL))

(defun quant-step (sformnum ps &optional terms copy?)
  (cond ((or (null sformnum)(null terms))
	 (format-if "~%No suitable (+ve EXISTS/-ve FORALL) quantified formula found.")
	 (values 'X nil nil))
	(t (let ((*tccforms* NIL)
		 (*dependent-decls* NIL))
	     (multiple-value-bind (signal subgoal)
		 (sequent-reduce
		  (current-goal ps)
		  #'(lambda (sform)
		      (quant-step-sform ps sform
					terms
					copy?))  
		  (list sformnum))
	       (let ((qsforms (select-seq (s-forms (current-goal ps))
					  (list sformnum)))
		     (other-sforms (delete-seq (s-forms (current-goal ps))
					       (list sformnum))))
		 (when (and (eq signal '?) (not copy?))
		   (loop for sf in qsforms
			 do (pushnew sf (hidden-s-forms subgoal)
				     :test #'tc-eq)))
		 (let* ((*tccforms* (remove-duplicates *tccforms*
				      :test #'tc-eq
				      :key #'tccinfo-formula))
			(tccforms (assert-tccforms *tccforms* ps))
			(tcc-subgoals
			 (loop for tccinfo in tccforms
			       collect
			       (let ((newgoal
				      (change-class
				       (copy subgoal
					 's-forms
					 (cons (make-instance 's-formula
						 'formula
						 (tccinfo-formula tccinfo))
					       other-sforms))
				       'tcc-sequent))
				     (references NIL))
				 (setf (tcc newgoal)(tccinfo-formula tccinfo)
				       (reason newgoal)(tccinfo-reason tccinfo)
				       (expr newgoal)(tccinfo-expr tccinfo)
				       (kind newgoal)(tccinfo-kind tccinfo)
				       (type newgoal)(tccinfo-type tccinfo))
				 (push-references-list
					(tccinfo-formula tccinfo)
					references)
				 (list newgoal
				       'dependent-decls
				       references
				       ))
				 ))
			(false-tccforms
			 (loop for x in tccforms as y in *tccforms*
			       when (tc-eq (tccinfo-formula x)
					   *false*)
			       collect
			       (list (tccinfo-kind y)
				     (tccinfo-formula y)
				     (tccinfo-expr y)
				     (tccinfo-type y))))
			)
		   (cond (false-tccforms
			  (format T
			    "~%False TCC(s):~
 ~{~%~{ ~a TCC: ~a when typechecking ~a : ~a~}~}~
~%when formula ~a is instantiated with ~a"
			    false-tccforms sformnum terms)
			  (values 'X nil nil))
			       
		   ;;(push-references-list *tccforms* dependent-decls*)
			 (t (values signal (cons (list subgoal
						       'dependent-decls
						       *dependent-decls*)
						 tcc-subgoals)))))))))))

(defun quant-step-sform (ps sform terms copy?)
  (let* ((fmla (formula sform))
	 (sign (not (not-expr? fmla)))
	 (body (if sign fmla (args1 fmla)))
	 (terms (if (listp terms) terms (list terms)))
	 (instantiable?
	  (or (and sign (typep body 'exists-expr))
	      (and (not sign)(typep body 'forall-expr))))
	 (length-check (and instantiable?
			    (eql (length (bindings body))
				 (length terms))))
	 (qbody (if length-check
		    (quantifier-step body *current-context* sign
				   terms ps)
		    body)))
    (if (not instantiable?)
	(format-if "~%Formula ~a is not instantiable." body)
	(when (not length-check)
	  (format-if "Expecting ~a terms, but ~a terms provided."
		      (length (bindings body)) (length terms))))
    (if (exequal qbody body) (values 'X sform)
	(values '? (if copy?
		       (list sform
			     (copy sform 'formula
				   (if sign qbody
				       (negate qbody))))
		       (copy sform 'formula
			 (if sign qbody
			     (negate qbody))))))))

(defun skolem-step-sform (ps sform  new-context &optional terms)
  (let* ((fmla (formula sform))
	 (sign (not (not-expr? fmla)))
	 (body (if sign fmla (args1 fmla)))
	 (skolemizable? (or (and sign (forall-expr? body))
			   (and (not sign)(exists-expr? body))))
	 (length-check (and skolemizable?
			    (eql (length (bindings body))
				 (length terms))))
	 (skobody (if length-check
		      (skolemize-step body new-context sign
				   terms)
		      body)))
    (if (not skolemizable?)
	(format-if "~%Formula~%~a~% is not skolemizable." body)
	(when (not length-check)
	  (format-if "Expecting ~a skolem constant(s), but ~a supplied."
		     (length (bindings body)) (length terms))))
    (if (exequal skobody body) (values 'X sform)
	(values '? 
		(copy sform 'formula
		      (if sign skobody
			  (negate skobody)))))))

#+lucid
(defmethod copy ((ht hash-table) &rest args)
  (let ((new-ht (make-hash-table :test (hash-table-test ht))))
    (maphash #'(lambda (id data)
		 (setf (gethash id new-ht) data))
	     ht)
    new-ht))

#-lucid
(defmethod copy (obj &rest args)
  (if (typep obj 'hash-table)
      (let ((new-ht (make-hash-table :test (hash-table-test obj))))
	(maphash #'(lambda (id data)
		     (setf (gethash id new-ht) data))
		 obj)
	new-ht)
      (error "copy called for unknown type: ~a" (type-of obj))))


(defun find-all-sformnums (sforms sformnums pred
				  &optional (pos 1)(neg -1)(acc nil))
  (cond ((null sforms) (nreverse acc))
	(t (let* ((sign (not (not-expr? (formula (car sforms)))))
		  (newpos (if sign (1+ pos) pos))
		  (newneg (if sign neg (1- neg)))
		  (newacc (if (and (in-sformnums?
				    (car sforms) pos neg sformnums)
				   (funcall pred (formula (car sforms))))
			      (cons (if sign pos neg) acc)
			      acc)))  ;;(break "find-all")
		 (find-all-sformnums (cdr sforms) sformnums pred
				     newpos newneg newacc)))))
				     
		 

	      

(defun find-sform (sforms sformnum &optional (pred #'(lambda (x) T)))
  (find-sform* sforms sformnum pred
		      1 -1))

(defun find-sform* (sforms sformnum pred pos neg)
  (cond ((null sforms) nil)
	((not-expr? (formula (car sforms)))
	 (if (and (or (memq sformnum '(* -))
		      (equal sformnum neg))
		  (funcall pred  (car sforms)))
	     neg
	     (find-sform* (cdr sforms) sformnum pred pos (1- neg))))
	(t (if (and (or (memq sformnum '(* +))
			(equal sformnum pos))
		  (funcall pred (car sforms)))
	     pos
	     (find-sform* (cdr sforms) sformnum pred (1+ pos) neg)))))

(defmethod tc-eq* ((x s-formula)(y s-formula) bindings)
  (tc-eq* (formula x)(formula y) bindings))
	

(defun quant-step (sformnum ps &optional terms copy?)
  (cond ((or (null sformnum)(null terms))
	 (format-if "~%No suitable (+ve EXISTS/-ve FORALL) quantified formula found.")
	 (values 'X nil nil))
	(t (let ((*tccforms* NIL)
		 (*dependent-decls* NIL))
	     (multiple-value-bind (signal subgoal)
		 (sequent-reduce
		  (current-goal ps)
		  #'(lambda (sform)
		      (quant-step-sform ps sform
					terms
					copy?))  
		  (list sformnum))
	       (let ((qsforms (select-seq (s-forms (current-goal ps))
					  (list sformnum)))
		     (other-sforms (delete-seq (s-forms (current-goal ps))
					       (list sformnum))))
		 (when (and (eq signal '?) (not copy?))
		   (loop for sf in qsforms
			 do (pushnew sf (hidden-s-forms subgoal)
				     :test #'tc-eq)))
		 (let* ((*tccforms* (remove-duplicates *tccforms*
				      :test #'tc-eq
				      :key #'tccinfo-formula))
			(tccforms (assert-tccforms *tccforms* ps))
			(tcc-subgoals
			 (loop for tccinfo in tccforms
			       collect
			       (let ((newgoal
				      (change-class
				       (copy subgoal
					 's-forms
					 (cons (make-instance 's-formula
						 'formula
						 (tccinfo-formula tccinfo))
					       other-sforms))
				       'tcc-sequent))
				     (references NIL))
				 (setf (tcc newgoal)(tccinfo-formula tccinfo)
				       (reason newgoal)(tccinfo-reason tccinfo)
				       (expr newgoal)(tccinfo-expr tccinfo)
				       (kind newgoal)(tccinfo-kind tccinfo)
				       (type newgoal)(tccinfo-type tccinfo))
				 (list newgoal
				       'dependent-decls
				       (push-references-list
					(tccinfo-formula tccinfo)
					references)))
				 ))
			);(break "quant-step")
		   ;;(push-references-list *tccforms* dependent-decls*)
		   (values signal (cons (list subgoal
					      'dependent-decls
					      *dependent-decls*)
					tcc-subgoals)))))))))

(defun skolem-step (sformnum ps &optional terms copy?)
  (let* ((*assert-typepreds* nil)
	 (*subtype-hash* (copy (subtype-hash ps)))
	 (alists (alists ps))
	 (findalist (dpinfo-findalist alists))
	 (usealist (dpinfo-usealist alists))
	 (sigalist (dpinfo-sigalist alists))
	 (new-context (copy *current-context*
			   'local-proof-decls
			   (copy (local-proof-decls *current-context*))))
	(terms (if (consp terms) terms (list terms)))
	(sformnum (find-sform (s-forms (current-goal ps)) sformnum
			      #'(lambda (sform)
				   (or (and (forall-expr? (formula sform))
					    (eql (length (bindings
							  (formula sform)))
						 (length terms)))
				       (and (not-expr? (formula sform))
					    (exists-expr?
					     (args1 (formula sform)))
					    (eql (length (bindings
							  (args1
							   (formula sform))))
						 (length terms))))))))
    (cond ((null sformnum)
	   (format-if "~%No suitable (+ve FORALL/-ve EXISTS) quantified expression found.")
	   (values 'X nil nil))
	  (t (multiple-value-bind (signal subgoal)
		 (sequent-reduce (current-goal ps)
				 #'(lambda (sform)
				     (skolem-step-sform ps sform
							new-context
							terms))
				 (list sformnum))
	       (if (eq signal 'X)(values 'X nil nil)
		   (if (some #'(lambda (fmla)
			   (let* ((sign (not (not-expr? fmla)))
				  (body (if sign
					    fmla
					    (args1 fmla)))
				  (translated-body
				   (top-translate-to-prove
				    body))
				  (translated-fmla
				   (if sign translated-body
				       (list 'NOT
					     translated-body))))
			   (and (not (connective-occurs? body))
				(let ((res (catch 'context
					     (call-process translated-fmla))))
				  (when (consp res)
				    (loop for x in res
					  do (push x *process-output*)))
				  (eq res 'FALSE)))))
		       *assert-typepreds*)
		 (values '! sformnum)  ; SO - changed from sform
		 (values signal (list
				   (cons subgoal
					(list 'context new-context
					      'subtype-hash *subtype-hash*
					      'alists
					      (make-instance 'dpinfo
						'dpinfo-sigalist sigalist
						'dpinfo-findalist findalist
						'dpinfo-usealist usealist))))))))))))
		      
;  (let* ((goalsequent (current-goal ps))
;	 (selected-sforms (select-seq (s-forms goalsequent)(list sformnum)))
;	 (new-sforms (delete-seq (s-forms goalsequent)(list sformnum)))
;	 (newcontext (copy *current-context*)))
;    (if (or (null selected-sforms)
;	    (not (or  (typep (formula (car selected-sforms)) 'quant-expr)
;		      (and (not-expr? (formula (car selected-sforms)))
;			   (typep (fstarg (formula (car selected-sforms)))
;				  'quant-expr)))))
;	(values 'X nil nil) ;;Rule inapplicable.
;	(let* ((sel-sform (car selected-sforms))
;	       (governing-vars (variables sel-sform))
;	       (formula (formula sel-sform))
;	       (alist (make-alist substs)))
;	  (multiple-value-bind
;	      (new-formula new-vars)
;	      (if (not-expr? formula)
;		  (skolem-step (fstarg formula)
;			     governing-vars
;			     newcontext
;			     NIL alist)
;		(skolem-step formula governing-vars newcontext T alist))
;	      ;;(break)
;	    (values '?
;		    (list (copy goalsequent
;				's-forms
;				(cons (make-instance 's-formula
;					'formula
;					(if (not-expr? formula)
;					    (make-negation new-formula)
;					    new-formula)
;					'variables new-vars)
;				      new-sforms)))
;		    (list 'context newcontext)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun find-!quant (sformnum ps)
  (find-sform (s-forms (current-goal ps)) sformnum
	      #'(lambda (sform)
		  (or (forall-expr? (formula sform))
		      (and (not-expr? (formula sform))
			   (exists-expr?
			    (args1 (formula sform))))))))

(defun fill-up-terms (sformnum terms ps &optional keep-underscore?)
  (let* ((sforms (select-seq (s-forms (current-goal ps)) (list sformnum)))
	 (sform (when sforms (car sforms)))
	 (fmla (when sforms (formula sform)))
	 (boundvars (when sforms
		      (if (forall-expr? fmla)
			  (bindings fmla)
			  (bindings (args1 fmla))))))
    (if (< (length terms) (length boundvars))
	(let* ((extra-boundvars
		(if terms (nthcdr (1- (length terms)) boundvars)
		    boundvars))
	       (skonames (new-sko-symbol-list
			  (mapcar #'id extra-boundvars)
			  *current-context*
			  nil nil
			  :keep-underscore? keep-underscore?)))
	  (append terms skonames))
	terms)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-or-best? (if-match)(memq if-match '(all best)))

(defun all-or-best-or-first*? (if-match)
  (memq if-match '(all best first*)))

(defun all-or-first*? (if-match)
  (memq if-match '(all first*)))

(defun make-inst?-rule (fnum newterms copy? if-match)
  (if (all-or-first*? if-match)
      (if newterms
	  (if (cdr newterms)
	      `(then ,(make-copy-quant-rule fnum (car newterms))
		,(make-inst?-rule fnum (cdr newterms) copy? if-match))
	      (if copy?
		  (make-copy-quant-rule fnum (car newterms))
		  (make-quant-rule fnum (car newterms))))
	  '(skip))
      (if copy?
	  (make-copy-quant-rule fnum newterms)
	  (make-quant-rule fnum newterms))))

(defun  make-copy-quant-rule (fnum newterms)
  (let* ((badterm (find-if #'(lambda (nt)
			       (member "_" nt))
		    newterms))
	 (nterms (ldiff newterms (cdr (memq badterm newterms)))))
    (cons 'then*
	  (cons `(instantiate ,fnum  ;;NSH(10.4.96)instantiate-one
			              ;;is not well-behaved; causes
			              ;;grind to fail.  
			      ,(car nterms) T)
		(loop for qterms in (cdr nterms)
		      collect `(instantiate  ;;NSH(4.2.95)was quant
				,(if (< fnum 0)
				     (1- fnum)
				     (1+ fnum))
				,qterms))))))

(defun make-quant-rule (fnum newterms)
  (let* ((badterm (find-if #'(lambda (nt)
			       (member "_" nt))
		    newterms))
	 (nterms (ldiff newterms (cdr (memq badterm newterms)))))
    (cons 'then*
	  (loop for qterms in nterms
		collect `(instantiate ,fnum ,qterms)))))


;      (let ((formula (when sforms
;		       (if (not-expr? (formula (car sforms)))
;			   (args1 (formula (car sforms)))
;			   (formula (car sforms))))))
;    (if (and sforms
;	     (or (forall-expr? formula)
;		 (exists-expr? formula))
;	     (equal (length (bindings formula))
;		    (length terms)))
;	(quant sformnum terms copy?)))


;;find-?quant finds quantified formula where outer variables contain
;;those in subst.
(defun find-?quant (sformnum subst ps)
  (let* ((subalist (make-alist subst))
	(subnames (loop for (x . y) in subalist
			collect (pc-parse x 'expr)))
	(badnames (loop for x in subnames
			when (not (typep x 'name-expr))
			collect x)))
    (cond (badnames
	   (format-if "~%Substitution ~a is ill-formed" subst)
	   nil)
	  (t (find-sform (s-forms (current-goal ps)) sformnum
	      #'(lambda (sform)
		  (or (and (exists-expr? (formula sform))
			   (subsetp subnames
				    (quant-bndvars*
				     (formula sform) T)
				    :test #'(lambda (x y)
					      (format-equal (id x)(id y)))))
		      (and (not-expr? (formula sform))
			   (forall-expr?
			    (args1 (formula sform)))
			   (subsetp subnames
				    (quant-bndvars*
				     (args1 (formula sform)) nil)
				    :test #'(lambda (x y)
					      (format-equal
					       (id x)
					       (id y))))))))))))

(defun quant-body* (fmla sign)
  (if sign
      (if (exists-expr? fmla)
	  (quant-body* (expression fmla) sign)
	  fmla)
      (if (forall-expr? fmla)
	  (quant-body* (expression fmla) sign)
	  fmla)))

(defun quant-bndvars* (fmla sign)
  (if sign
      (when (exists-expr? fmla)
	(append (bindings fmla)
		(quant-bndvars* (expression fmla) sign)))
      (when (forall-expr? fmla)
	(append (bindings fmla)
		(quant-bndvars* (expression fmla) sign)))))

(defun order-subst (subst boundvars)
  (if (null boundvars)
      nil
      (let ((entry (assoc (car boundvars) subst
			  :key #'declaration)))
	(if entry
	    (cons entry (order-subst subst (cdr boundvars)))
	    (order-subst subst (cdr boundvars))))))

(defun filter-subst (fmla subst sign boundvars if-match)
  (if (all-or-first*? if-match)
      (rem-dups
	  subst
	:test #'tc-eq
	:key #'(lambda (sub) (quant-subs* fmla sub sign nil)))
      (if (eq if-match 'best)
	  (let* ((all-subst
		  (rem-dups
		      subst 
		    :test #'tc-eq
		    :key #'(lambda (sub) (quant-subs* fmla sub sign nil))))
		 (tcc-all-subst
		  (loop for sub in all-subst
			collect
			(let ((*tccforms* nil))
			  (tc-alist (order-subst (cdr sub) boundvars))
			  (cons (length *tccforms*)
				sub))))
		 (tccnums (mapcar #'car tcc-all-subst))
		 (min-tccnums (if tccnums (apply #'min tccnums) 0))
		 (best-sub (loop for (x . y) in tcc-all-subst
				thereis (when (eq x min-tccnums)
					  y))))
	    best-sub)
	  subst)))

(defun quant-subs* (fmla subst sign if-match)
  (if (all-or-first*? if-match)
      (loop for sub in subst
	    collect (quant-subs* fmla sub sign nil))
      (if sign
	  (if (exists-expr? fmla)
	      (cons (loop for var in (bindings fmla)
			  collect
			  (let ((entry
				 (assoc var (cdr subst) :key #'declaration)))
			    (if entry (cdr entry)
				"_")))
		    (quant-subs* (expression fmla) subst sign if-match))
	      nil)
	  (if (forall-expr? fmla)
	      (cons (loop for var in (bindings fmla)
			  collect
			  (let ((entry
				 (assoc var (cdr subst) :key #'declaration)))
			    (if entry (cdr entry)
				"_")))
		    (quant-subs* (expression fmla) subst sign if-match))
	      nil))))
	       
 
(defun find-quant-terms (sforms subst where if-match
				polarity?
				ps)
  (cond ((null sforms)
	 (format-if "~%Couldn't find a suitable quantified formula.")
	 NIL)
	(t
	 (let* ((alists (alists ps))
		(sigalist (dpinfo-sigalist  alists))
		(findalist (dpinfo-findalist  alists))
		(usealist (dpinfo-usealist alists)))
	   (find-quant-terms* sforms subst where if-match polarity? ps)))))

(defun forall-sform?  (sform)
  (let ((formula  (formula sform)))
    (if (not-expr? formula)
	(exists-expr? (args1 formula))
	(forall-expr? formula))))

(defun exists-sform? (sform)
  (let ((formula  (formula sform)))
    (if (not-expr? formula)
	(forall-expr? (args1 formula))
	(exists-expr? formula))))

(defun find-quant-terms* (sforms subst where if-match polarity? ps)
    (cond ((null sforms)
	   (format-if "~%Couldn't find a suitable instantiation for any
quantified  formula.  Please provide partial instantiation.")
	   nil)
	  ((or (not (listp subst))
	       (oddp (length subst)))
	   (format-if "~%Given substitution ~a
is not of the form: (<var> <term>...)" subst)
	   nil)
	  ((not (quant-expr? (if (not-expr? (formula (car sforms)))
				 (args1 (formula (car sforms)))
				 (formula (car sforms)))))
	   (find-quant-terms* (cdr sforms) subst where if-match
			      polarity? ps))
	  (t 
	   (let* 
	       ((sform  (car sforms))
		(formula  (formula sform))
		(sign (not (not-expr? formula)))
		(fmla (quant-body* (if sign formula (args1 formula))
				   sign))
		(boundvars (quant-bndvars* (if sign formula (args1 formula))
					   sign))
		(pre-alist (make-alist subst))
		(pre-alist (loop for (x . y) in pre-alist
				   collect
				   (cons (pc-parse x 'name) y)))
		(bad-subst (loop for (x . y) in pre-alist
				   thereis
				   (when (not (member (id x)
						 boundvars
						 :test
						 #'(lambda (u v)
						     (format-equal
						      u (id v)))))
				     x))))
	     (cond (bad-subst
		    (find-quant-terms* (cdr sforms) subst where
				       if-match polarity? ps))
		   (t (let ((sub
			     (find-quant-subst (car sforms) pre-alist
					       boundvars sign fmla
					       where if-match
					       polarity? ps)))
			(if sub
			    (cons (find-sform (s-forms
					       (current-goal *ps*))
					      '*
					      #'(lambda (x)
						  (eq x (car sforms))))
				  sub)
			    (find-quant-terms* (cdr sforms) subst
					       where if-match
					       polarity? ps)))))
	     ))))


(defun find-quant-subst (sform pre-alist boundvars sign fmla
			       where if-match polarity? ps)
  (let* ((alist
	  (loop for (x . y) in pre-alist
		collect
		(let*
		    ((v (find (id x)
			      boundvars
			      :test
			      #'(lambda (u v)
				  (format-equal
				   u (id v)))))
		     (term (pc-parse y 'expr)))
		  (cons v term))))
	 (alist (tc-alist alist))
	 (*all-boundvars* boundvars) ;;NSH(11.6.95) for use in template?
	 (rest-boundvars
	  (loop for v in boundvars
		when (not (assoc v alist))
		collect v))
	 ;;(freevars (freevars formula))
	 ;;	 (allvars (append freevars boundvars))
	 (other-sforms 
	  (gather-seq (s-forms (current-goal ps))
		      where
		      nil
		      #'(lambda (x) (not (eq x sform)))
			  ;;NSH(11.6.94): to allow self-instantiation
		          ;;of quantified formulas (not (eq x sform))
		      ))
	 (all-sforms (gather-seq (s-forms (current-goal ps))
		      where
		      nil))
	 (all-fmlas
	  (if (memq sform all-sforms)
	      (mapcar #'formula (append other-sforms (list sform)))
	      (mapcar #'formula other-sforms))))
    (find-quant-subst* sform sign fmla
		       all-fmlas alist rest-boundvars if-match
		       polarity?
		       (length rest-boundvars))))

(defun top-find-match-list (template fmlas alist if-match polarity polarity?)
  (if polarity?
      (find-match-list-with-polarity template fmlas
				     alist if-match polarity)
      (find-match-list template fmlas alist if-match)))

(defun top-find-match-list-templates
  (templates fmlas alist if-match polarity polarity?)
  (if (consp templates)
      (let* ((temp (car templates))
	     (result
	      (top-find-match-list temp
				  fmlas
				  alist
				  if-match 'positive polarity?)))
	(if (all-or-best-or-first*? if-match)
	    (nconc (mapcar #'(lambda (x) (cons temp x))
		     result)
		   (top-find-match-list-templates
		    (cdr templates) fmlas alist if-match
		    polarity polarity?))
	    (if result
		(cons temp result)
		(top-find-match-list-templates
		    (cdr templates) fmlas alist if-match
		    polarity polarity?))))
      nil))

(defun find-quant-subst* (sform sign fmla
				other-fmlas alist rest-boundvars if-match
				polarity? n)
  (let* ((polarity (if sign 'positive 'negative))
	 (templates (find-templates fmla rest-boundvars n nil polarity
				    polarity?))
	 ;;(freevars-sub (mapcar #'(lambda (x) (cons x x)) freevars))
	 (*modsubst* T)
	 (temp-subst (top-find-match-list-templates
		      templates other-fmlas alist if-match
		      'positive polarity?))
	 (subst (filter-subst (if sign
				(formula sform)
				(args1 (formula sform)))
			    (if temp-subst temp-subst
				(when alist
				  (if (all-or-best? if-match)
				      (list (cons NIL alist))
				      (cons NIL alist)))) ;for null template
			    sign rest-boundvars if-match))
	 (subs (quant-subs* (if sign
				(formula sform)
				(args1 (formula sform)))
			    (if subst subst
				(when alist (if (all-or-best? if-match)
				      (list (cons NIL alist))
				      (cons NIL alist))))
			    sign if-match))) 
    (cond ((and (null subst) (or if-match (null alist)(null subs)))
	   (if  (and (> n 1) ;;NSH(5.23.95) used to be n>2.
		     (not (eq if-match 'all)))
	       (find-quant-subst* sform sign fmla other-fmlas
				   alist rest-boundvars
				  if-match polarity? (1- n))
	   ;(format-if "~%Couldn't find a matching substitution.")
	   nil))
	  ((and (null subst) subs)
	   (format-if "~%Using supplied substitutions")
	   subs)
	  (t (cond ((all-or-first*? if-match)
		    (loop for sub in subst as index from 1
			  do (format-if "~%Found substitution ~a:"
					index)
			     (loop for (x . y) in (cdr sub)
				   do (format-if "~%~a gets ~a," x y))
			     (format-if "~%Using template: ~a" (car sub))))
		   (t (format-if "~%Found substitution:")
		      (loop for (x . y) in (cdr subst)
			    do (format-if "~%~a gets ~a," x y))
		      (format-if "~%Using template: ~a" (car subst))))
	     subs))))

(defun find-match-list-with-polarity
  (template fmlas subst if-match polarity)
  (when fmlas
    (let ((match-list1
	   (if (all-or-best-or-first*? if-match)
	       (mapcar #'car
		 (find-all-matches-polarity
		  template (car fmlas)
		  nil subst nil polarity))
	       (multiple-value-bind (sub modsub)
		   (find-match-polarity template (car fmlas) nil subst nil
					polarity)
		 (if (eq sub 'fail) nil sub)))))
      (if (all-or-best-or-first*? if-match)
	  (nconc match-list1
		 (find-match-list-with-polarity template (cdr fmlas) subst if-match
						polarity))
	  (if match-list1
	      match-list1
	      (find-match-list-with-polarity template (cdr fmlas) subst if-match
					     polarity))))))

(defun find-match-list (template fmlas subst if-match)
  (when fmlas
    (let ((match-list1
	   (if (all-or-best-or-first*? if-match)
	       (mapcar #'car
		       (find-all-matches template (car fmlas)
					 nil subst nil))
	       (multiple-value-bind (sub modsub)
		   (find-match template (car fmlas) nil subst nil)
		 (if (eq sub 'fail) nil sub)))))
      (if (all-or-best-or-first*? if-match)
	  (nconc match-list1
		 (find-match-list template (cdr fmlas) subst if-match))
	  (if match-list1
	      match-list1
	      (find-match-list template (cdr fmlas) subst if-match))))))

(defun template? (fmla boundvars)
  (let ((freevars (freevars fmla)))
    (and (subsetp freevars *all-boundvars* :test #'same-declaration)
	 (eql (length (intersection freevars boundvars
				    :key #'declaration))
	      *template-num*))))

(defun find-templates (expr boundvars n &optional accum polarity polarity?)
  (let* ((*template-num* n)
	 (templates
	  (if polarity?
	      (rem-dups
	       (find-templates-with-polarity
		expr boundvars polarity)
	       :test #'tc-eq
	       :key #'template-expression)
	      (rem-dups (find-templates* expr boundvars accum)
			:test #'tc-eq))))
    (if (and (eq n 1)(null polarity?))
	(let* ((nonvars (remove-if #'variable? templates))
	       (vars (remove-if #'(lambda (x) (not (variable? x)))
			  templates))
	       (boundvars-not-in-vars ;;NSH(10.21.96) non-occurring boundvars
		(loop for x in boundvars
		      when (not (member x vars :test #'same-declaration))
		      collect (change-class
			       (copy x 'kind 'VARIABLE)
			       'name-expr))))
	(nconc nonvars vars
	       boundvars-not-in-vars
	       ))
	templates)))

(defun subset-expr-freevars-against-arg-templates
  (expr-freevars arg-template)
  (subsetp expr-freevars
	   (freevars arg-template)
	   :test #'same-declaration))

(defun subsumed-expr-against-arg-templates
  (expr arg-templates)
  (member (freevars expr)
	  arg-templates
	  :test #'subset-expr-freevars-against-arg-templates))

(defun connective-expr? (expr)
  (or ;;NSH(4.8.96) (negation? expr)
	(implication? expr)(conjunction? expr)
	(disjunction? expr)(ifff? expr)))

(defun toggle (polarity)
  (if (eq polarity 'positive) 'negative
      (if (eq polarity 'negative) 'positive
	  (if (eq polarity 'less) 'more
	      (if (eq polarity 'more) 'less
		  polarity)))))

(defstruct template ;;NSH(3.10.97) find-templates returns templates
  expression        ;;with polarity instead of expressions. 
  polarity)

(defmethod freevars* ((expr template) frees)
  (freevars* (template-expression expr) frees))

(defun mk-template (expr polarity)
  (make-template :expression expr :polarity polarity))

(defun arith-polarity? (polarity)
  (memq polarity '(less more)))

(defun arith-polarity (bpolarity apolarity)
  (if (eq bpolarity 'positive)
      apolarity
      (if (eq bpolarity 'negative)
	  (toggle apolarity)
	  nil)))

(defmethod find-templates-with-arithmetic-polarity
  ((expr application) boundvars polarity accum)
  (let* ((op (operator expr))
	 (arg-templates 
	  (when (and (name-expr? op)
		     (interpreted? op))
	    (cond
	     ((memq (id (operator expr)) '(< <=))
	      (find-templates-with-arithmetic-polarity
	       (args1 expr) boundvars
	       (arith-polarity polarity 'less)
	       (find-templates-with-polarity (args2 expr)
					     boundvars
					     (arith-polarity polarity 'more)
					     accum)))
	     ((memq (id (operator expr)) '(> >=))
	      (find-templates-with-arithmetic-polarity
	       (args1 expr) boundvars
	       (arith-polarity polarity 'more)
	       (find-templates-with-polarity (args2 expr)
					     boundvars
					     (arith-polarity polarity 'less)
					     accum)))
	     ((is-plus? op)
	      (find-templates-with-arithmetic-polarity
	       (args1 expr) boundvars polarity
	       (find-templates-with-arithmetic-polarity
		(args2 expr) boundvars polarity accum)))
	     ((is-sub-minus? op)
	      (find-templates-with-arithmetic-polarity
	       (args1 expr)
	       boundvars
	       polarity
	       (find-templates-with-arithmetic-polarity
		(args2 expr)
		boundvars (toggle polarity) accum)))
	     ((is-minus? op)
	      (find-templates-with-arithmetic-polarity
	       (argument op)
	       boundvars (toggle polarity) accum))
	     (t nil)))))
    (if (template? expr boundvars)
	(cons (mk-template expr polarity)
	      arg-templates)
	arg-templates)))

(defmethod find-templates-with-arithmetic-polarity
    ((expr list)  boundvars polarity accum)
  (if (consp expr)
      (find-templates-with-arithmetic-polarity
       (car expr)
       boundvars polarity
       (find-templates-with-arithmetic-polarity
	(cdr expr) boundvars polarity accum))
      accum))

(defmethod find-templates-with-arithmetic-polarity
    ((expr T)  boundvars polarity accum)
  (if (template? expr boundvars)
      (cons (mk-template expr polarity) accum)
      accum))

(defmethod find-templates-with-polarity
    ((expr application) boundvars
      polarity &optional accum)
  (let ((argument-templates
	 (cond ((implication? expr)
		(find-templates-with-polarity
		 (args2 expr) boundvars polarity
		 (find-templates-with-polarity (args1 expr)
				  boundvars (toggle polarity))
		 ))
	       ((negation? expr)
		(find-templates-with-polarity (args1 expr) boundvars
				(toggle polarity)))
	       ((or (conjunction? expr)(disjunction? expr))
		(find-templates-with-polarity (arguments expr) boundvars
				 polarity))
	       (t
		(find-templates-with-arithmetic-polarity
		 expr boundvars polarity nil)))))
    (if (template? expr boundvars)
	(if (and (connective-expr? expr)
		 (subsumed-expr-against-arg-templates expr argument-templates))
	    (append argument-templates accum)
	    (cons (mk-template expr polarity)
		  (append argument-templates accum)))
	(append argument-templates accum))))

(defmethod find-templates-with-polarity ((expr binding-expr) boundvars
			    polarity &optional accum)
  (let* ((argument-templates
	  (find-templates-with-polarity  (expression expr) boundvars polarity))
	 (argument-templates
	  (loop for fmla in argument-templates
		when (null (intersection (freevars fmla)
					 (bindings expr)
					 :key #'declaration))
		collect fmla)))
    (if (template? expr boundvars)
	(cons (mk-template expr polarity)
	      (append argument-templates accum))
	(append argument-templates accum))))

(defmethod find-templates-with-polarity ((expr list) boundvars
			    polarity &optional accum)
  (cond ((null expr) accum)
	(t (find-templates-with-polarity (car expr) boundvars polarity
			   (find-templates-with-polarity (cdr expr) boundvars
					    polarity accum)))))

(defmethod find-templates-with-polarity ((expr name-expr) boundvars
					 polarity &optional accum)
  (if (and (variable? expr)
	   (subsetp boundvars
	       (list expr)
	       :key #'declaration))
      (cons (mk-template expr polarity) accum)
      accum))

(defmethod find-templates-with-polarity ((expr T) boundvars
					 polarity &optional accum) 
  accum)

(defun rem-dups* (list accum &key (test #'eql) (key #'identity))
  ;;NSH(10.2.95) needed to preserve order
  (if (null list)(nreverse accum)
      (rem-dups* (delete (funcall key (car list)) (cdr list) :test test :key key)
		(cons (car list) accum)
		:test test :key key)))

(defun rem-dups (list &key (test #'eql) (key #'identity))
  (rem-dups* list nil :test test :key key))


(defmethod find-templates* ((expr application) boundvars &optional accum)
  (let ((argument-templates
	 (if (implication? expr)
	     (find-templates* (args2 expr) boundvars
			     (find-templates* (args1 expr) boundvars))
	     (find-templates* (cons (operator expr)
			       (arguments expr))
			 boundvars))))
    (if (template? expr boundvars)
	(if (and (connective-expr? expr)
		 (subsumed-expr-against-arg-templates expr argument-templates))
	    (append argument-templates accum)
	    (cons expr (append argument-templates accum)))
	(append argument-templates accum))))

(defmethod find-templates* ((expr field-application) boundvars &optional accum)
  (find-templates* (argument expr) boundvars accum))

(defmethod find-templates* ((expr projection-application) boundvars
			    &optional accum) 
  (find-templates* (argument expr) boundvars accum)) 


(defmethod find-templates* ((expr record-expr) boundvars &optional accum)
  (let ((argument-templates
	 (find-templates* (assignments expr) boundvars)))
    (if (null argument-templates)
	(if (template? expr boundvars)
	    (cons expr accum)
	    accum)
	(cons expr (append argument-templates accum)))))

(defmethod find-templates* ((expr assignment) boundvars &optional
			    accum)
  (find-templates* (expression expr)
		  boundvars
		  (find-templates* (arguments expr)
				   boundvars accum)))

(defmethod find-templates* ((expr tuple-expr) boundvars &optional accum)
  (let ((argument-templates
	 (find-templates* (exprs expr) boundvars)))
    (if (null argument-templates)
	(if (template? expr boundvars)
	    (cons expr accum)
	    accum)
	(cons expr (append argument-templates accum)))))

(defmethod find-templates* ((expr cases-expr) boundvars &optional accum)
  (let ((argument-templates
	 (find-templates* (cons (expression expr)
			       (append (mapcar #'expression
					       (selections expr))
				       (else-part expr)))
				boundvars)))
    (if (null argument-templates)
	(if (template? expr boundvars)
	    (cons expr accum)
	    accum)
	(cons expr (append argument-templates accum)))))

(defmethod find-templates* ((expr binding-expr) boundvars &optional accum)
  (let* ((argument-templates
	  (find-templates*  (expression expr) boundvars))
	 (argument-templates
	  (loop for fmla in argument-templates
		when (null (intersection (freevars fmla)
					 (bindings expr)
					 :key #'declaration))
		collect fmla)))
    (if (template? expr boundvars)
	(cons expr (append argument-templates accum))
	(append argument-templates accum))))


(defmethod find-templates* ((expr update-expr) boundvars &optional accum)
  (let ((argument-templates
	 (find-templates*  (cons (expression expr)
				(loop for asgn in (assignments expr)
				      nconc (list (arguments asgn)
						  (expression asgn))))
				boundvars)))
	(if (null argument-templates)
	    (if (template? expr boundvars)
		(cons expr accum)
		accum)
	    (cons expr (append argument-templates accum)))))

(defmethod find-templates* ((expr list) boundvars &optional accum)
  (cond ((null expr) accum)
	(t (find-templates* (car expr) boundvars 
			   (find-templates* (cdr expr) boundvars accum)))))

(defmethod find-templates* ((expr name-expr) boundvars &optional accum)
  (if (and (variable? expr)
	   (subsetp boundvars
	       (list expr)
	       :key #'declaration))
      (cons expr accum)
      accum))

(defmethod find-templates* ((expr T) boundvars &optional accum)
  accum)



;(defmethod find-templates ((expr record-expr) boundvars &optional accum)
;  (let ((argument-templates
;	 (find-templates (assignments expr) boundvars)))
;    (if (null argument-templates)
;	(if (subsetp boundvars
;			(freevars expr)
;			:key #'declaration)
;	       (cons expr accum)
;	       accum)
;	(append argument-templates accum))))
;
;(defmethod find-templates ((expr tuple-expr) boundvars &optional accum)
;  (let ((argument-templates
;	 (find-templates (exprs expr) boundvars)))
;    (if (null argument-templates)
;	(if (subsetp boundvars
;			(freevars expr)
;			:key #'declaration)
;	       (cons expr accum)
;	       accum)
;	(append argument-templates accum))))
;
;(defmethod find-templates ((expr cases-expr) boundvars &optional accum)
;  (let ((argument-templates
;	 (find-templates (cons (expression expr)
;			       (append (mapcar #'expression
;					       (selections expr))
;				       (else-part expr)))
;				boundvars)))
;    (if (null argument-templates)
;	(if (subsetp boundvars
;			(freevars expr)
;			:key #'declaration)
;	       (cons expr accum)
;	       accum)
;	(append argument-templates accum))))
;
;(defmethod find-templates ((expr binding-expr) boundvars &optional accum)
;  (let ((argument-templates
;	 (find-templates  (expression expr) boundvars)))
;	(if (null argument-templates)
;	    (if (subsetp boundvars
;			 (freevars expr)
;			 :key #'declaration)
;		(cons expr accum)
;		accum)
;	    (append argument-templates accum))))
;
;(defmethod find-templates ((expr update-expr) boundvars &optional accum)
;  (let ((argument-templates
;	 (find-templates  (cons (expression expr)
;				(loop for asgn in (assignments expr)
;				      nconc (list (arguments asgn)
;						  (expression asgn))))
;				boundvars)))
;	(if (null argument-templates)
;	    (if (subsetp boundvars
;			 (freevars expr)
;			 :key #'declaration)
;		(cons expr accum)
;		accum)
;	    (append argument-templates accum))))
;
;(defmethod find-templates ((expr list) boundvars &optional accum)
;  (cond ((null expr) accum)
;	(t (find-templates (car expr) boundvars
;			   (find-templates (cdr expr) boundvars accum)))))
;
;;(defmethod find-templates ((expr name-expr) boundvars &optional accum)
;;  accum)
;
;;(defmethod find-templates ((expr expr) boundvars &optional accum)
;;  accum)
;
;(defmethod find-templates ((expr T) boundvars &optional accum)
;  accum)


;(defun find-templates (fmla  boundvars &optional accum)
;  (cond ((null fmla) accum)
;	((consp fmla)
;	 (find-templates (car fmla) boundvars
;			 (find-templates (cdr fmla) boundvars accum)))
;	((or (not-expr? fmla)
;	     (and-expr? fmla)
;	     (or-expr? fmla)
;	     (iff-expr? fmla))
;	 (find-templates (arguments fmla) boundvars accum))
;	((or (implies-expr? fmla)
;	     (branch? fmla))
;	 (find-templates (reverse (arguments fmla)) boundvars accum))
;	((or (equality? fmla)(inequality? fmla))
;	 (append (when (subsetp boundvars
;			      (freevars fmla)
;			      :key #'declaration)
;		   (list fmla))
;	 (find-templates (arguments fmla) boundvars  accum)))
;	(t (if (subsetp boundvars
;			(freevars fmla)
;			:key #'declaration)
;	       (cons fmla accum)
;	       accum))))

			 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun reveal-step (fnums)
  #'(lambda (ps)
      (let* ((sequent (current-goal ps))
	     (fnums (loop for fn in fnums
		    append (if (consp fn) fn (list fn))))
	     (hforms (select-seq (hidden-s-forms sequent) fnums)))
	(cond ((null hforms)
	       (values 'X nil))
	      (t 
	       (values '?
		       (list
			(copy sequent
			     's-forms (append hforms
					     (s-forms sequent))))))))))




(defun hide-step (sformnums)
  #'(lambda (ps)
      (let* ((sformnums
	      (loop for sf in sformnums
		    append (if (consp sf) sf (list sf))))
	     (sforms (select-seq (s-forms (current-goal ps))
				sformnums))
	     (remaining-sforms (delete-seq (s-forms (current-goal ps))
				sformnums))
	     (hforms (appendnew sforms
				(hidden-s-forms (current-goal ps))
				:test
				#'(lambda (x y)
				    (tc-eq (formula x)(formula y))))))
	(cond ((null sforms)(values 'X NIL))
	      (t (values '?
			 (list (copy (current-goal ps)
				     's-forms remaining-sforms
				     'hidden-s-forms hforms))))))))

(defun appendnew (list1 list2 &key test)
  (if list1
      (let ((rest (appendnew (cdr list1) list2 :test test)))
	(if (member (car list1) rest
		    :test test)
	    rest
	    (cons (car list1) rest)))
      list2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;substitution rule (1-7-91):
;; add-subst adds a substitution pair to the alist, the variable
;;part of the substitution must be a Skolem var.
;; apply-subst applies a substitution to the current goal and 
;;substitution.
;; apply-subst* applies all the substitutions.
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;add-subst should mainly check that there is no circularity in the 
;;;substitution
;	
;(addrule 'add-subst (varname term) nil (add-subst-rule varname term)
;	 "(add-subst <var> <term>): Adds the substitution to the
;current substitution alist.  See apply-subst.")
;
;(defun add-subst-rule (varname term)
;  (make-instance 'rule
;    'rule-part (add-subst-fun varname term)
;    'rule-input `(add-subst ,varname ,term)))
;
;(defun add-subst-fun (varname term)
;  #'(lambda (ps) (add-subst-step varname term ps)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-equality (lhs rhs)
  (make-equation lhs rhs))

;(defun add-subst-step (varname term ps)
;  (let ((var-rep (typecheck (pc-parse varname 'expr)
;			    :context *current-context*)))
;    ;;How should typecheck errors be handled here?
;    ;;(break)
;  (cond ((or (null var-rep)
;	     (not (skovar? var-rep)))
;	 (format t "~%~a is not a good Skolem variable.~%" varname)
;	 (values 'X nil nil)) ;;var not a Herbrand variable.
;	(t (let* ((term-rep (parse :string 
;				      (format nil "~a" term)
;				      :nt 'expr))
;		  (typed-term (typecheck term-rep
;				      :context *current-context*
;				      :expected (type var-rep))))
;	     (cond ((null typed-term) (values 'X nil nil))
;		   ((sub-occursin var-rep typed-term (substitution ps))
;		    (format t "~%Substitution violates occurs check.")
;		    (values 'X nil nil))
;		   (t
;		    (multiple-value-bind
;		     (signal subgoals subst updates)
;		     (do-assert (negate       ;;assert as an assumption
;				 (make-equality var-rep typed-term)) ps)
;		     (if (eq signal '!)
;			 (values '! nil
;				 (cons (cons var-rep typed-term)
;				       (substitution ps))
;				 updates)
;		       (values '? (list (current-goal ps))
;			       (cons (cons var-rep typed-term)
;				     (substitution ps))
;			       updates))))))))))

;(defun skovar? (x)    
;  (char= (char (string (id x)) 0) #\?))
;
;
;(defmethod sub-occursin (varname (term name-expr) subst)
;  (cond ((exequal varname term) t)
;	((not (skovar? term)) NIL)
;	(t (let ((pair (assoc term subst :test #'exequal)))
;	     (if (null pair) NIL
;		 (sub-occursin varname (cdr pair) subst))))))
;
;(defmethod sub-occursin (varname (term application) subst)
;  (or (sub-occursin varname (operator term) subst)
;      (loop for arg in (arguments term)
;	    thereis (sub-occursin varname arg subst))))
;
;(defmethod sub-occursin (varname (term binding-expr) subst)
;  (sub-occursin varname (expression term) subst))
;
;
;(defmethod sub-occursin (varname (term expr) subst)
;  NIL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;apply-subst
;
;	
;(addrule 'apply-subst (varname) nil (apply-subst-rule varname)
;	 "(apply-subst <var>): Substitutes the binding for variable in the sequent.")
;
;(defun apply-subst-rule (varname )
;  (make-instance 'rule
;    'rule-part (apply-subst-fun varname)
;    'rule-input `(apply-subst ,varname)))
;
;(defun apply-subst-fun (varname )
;  #'(lambda (ps) (apply-subst-step varname ps)))
;
;(defun apply-subst-step (varname  ps)
;  (let ((pair (assoc varname (substitution ps)
;		     :test #'(lambda (x y)(eq x (id (name y)))))))
;    (cond ((null pair)
;	   (format t "~%No such substitution. ~%")
;	   (values 'X nil nil))
;	  (t (let* ((alist (list pair))
;		    (new-s-forms (loop for sform in (s-forms
;						     (current-goal ps))
;				       collect
;				       (copy sform 'formula
;					     (substit (formula sform)
;						      alist))))
;		    (new-subst (loop for sub in (substitution ps)
;				     collect (cons (car sub)
;						   (substit (cdr sub) alist)))))
;	   (values '?
;		     (list
;		      (copy (current-goal ps)
;			    's-forms
;			    new-s-forms))
;		     new-subst))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;useful operations (courtesy Sam).
(defun args1 (expr)
  (car (arguments expr)))

(defun args2 (expr)
  (cadr (arguments expr)))

(defmethod constant? ((expr name-expr))
  (eq (kind expr) 'CONSTANT))
;   (or (typep (declaration expr) 'const-decl)
;       (typep (declaration expr) 'def-decl))
  ;;(NSH:4-5-91) incorrect (not (typep (type expr) 'funtype))

(defmethod constant? ((expr projection-expr))
  T)

(defmethod constant? ((expr T))
  NIL)

(defun variable? (expr)
  (and (typep expr 'name-expr)
       (eq (kind expr) 'variable)))

;(defmethod declaration ((expr name-expr))
;  (declaration (car (resolutions (name expr)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
